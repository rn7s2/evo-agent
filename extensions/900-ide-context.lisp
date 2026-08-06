;;;; 900-ide-context.lisp — ambient editor context from a companion IDE plugin.
;;;;
;;;; The IDE (see the evo-vscode extension) writes a small JSON file whenever
;;;; editor focus or the text selection changes, and exports its path to the
;;;; terminal as EVO_IDE_CONTEXT.  This extension consumes it:
;;;;
;;;;   * on submit, the focused file and any selected text are journaled as a
;;;;     :custom-message immediately before the user's message, so the model
;;;;     can resolve "this file" / "the selection" without a tool call;
;;;;   * the status line grows a "⧉ N lines selected" segment while a
;;;;     selection exists.
;;;;
;;;; Transport is a file, not a socket: evo needs this state at exactly two
;;;; moments and initiates both, so there is nothing to push.  No port, no
;;;; token, no reconnect — and a hand-written JSON file is the whole test rig.
;;;;
;;;; Without EVO_IDE_CONTEXT in the environment nothing is installed: no
;;;; wrappers, no thread, no behaviour change.

(in-package :evo.user)

(defparameter *ide-context-env-var* "EVO_IDE_CONTEXT"
  "Environment variable holding the path of the IDE state file.")

(defparameter *ide-context-max-age-seconds* (* 4 60 60)
  "Ignore the state file if the IDE has not touched it in this long.  A
terminal outliving its IDE should not keep prefixing prompts with the file
somebody was looking at yesterday.")

(defparameter *ide-context-max-lines* 100
  "Selected text beyond this many lines is elided in the injected block.")

(defparameter *ide-context-max-chars* 4000
  "Selected text beyond this many characters is elided in the injected block.")

(defparameter *ide-context-poll-seconds* 0.25
  "How often the status poller stats the state file.")

(defvar *ide-context-lock* (bt:make-lock "ide-context"))

(defvar *ide-context-cache* nil
  "Plist (:mtime :size :state) — the last parse, reused until the file moves.")

(defvar *ide-context-last-injected* nil
  "Text of the last injected block; identical state is not injected twice.")

(defvar *ide-context-original-status-line* nil
  "EVO.TUI::STATUS-LINE before this extension wrapped it.")

(defvar *ide-context-original-submit* nil
  "EVO.TUI::SUBMIT-TO-AGENT before this extension wrapped it.")

(defvar *ide-context-poller-generation* 0)
(defvar *ide-context-poller-thread* nil)

;;; Reading the state file.

(defun ide-context-path ()
  "Path of the IDE state file, or NIL when the feature is not active."
  (let ((value (evo.util:getenv *ide-context-env-var*)))
    (and (stringp value) (plusp (length value)) value)))

(defun ide-context-parse (path)
  "Parse PATH as the IDE state JSON.  Returns a plist, or NIL on any problem —
a half-written or malformed file must never break a turn."
  (ignore-errors
    (let ((parsed (com.inuoe.jzon:parse (evo.util:read-file-string path))))
      (and (hash-table-p parsed)
           (evo.provider::json->sexpr parsed)))))

(defun ide-context-fresh-p (mtime)
  (and mtime (<= (- (get-universal-time) mtime) *ide-context-max-age-seconds*)))

(defun ide-context-state ()
  "Current IDE state plist, or NIL.  Cheap: stats the file and re-reads only
when it has changed.  Stale files (see *ide-context-max-age-seconds*) read as
absent."
  (let ((path (ide-context-path)))
    (when path
      (let* ((truename (ignore-errors (probe-file path)))
             (mtime (and truename (ignore-errors (file-write-date truename))))
             (size (and truename
                        (ignore-errors
                          (with-open-file (in truename :element-type '(unsigned-byte 8))
                            (file-length in))))))
        (cond
          ((not (ide-context-fresh-p mtime))
           (bt:with-lock-held (*ide-context-lock*) (setf *ide-context-cache* nil))
           nil)
          (t
           (bt:with-lock-held (*ide-context-lock*)
             (let ((cache *ide-context-cache*))
               (if (and cache
                        (eql mtime (evo.util:pget cache :mtime))
                        (eql size (evo.util:pget cache :size)))
                   (evo.util:pget cache :state)
                   (let ((state (ide-context-parse truename)))
                     (setf *ide-context-cache*
                           (list :mtime mtime :size size :state state))
                     state))))))))))

;;; Rendering the injected block.

(defun ide-context-alive-p (pid)
  "True unless PID is known to be gone.  One fork per submitted prompt, and
only there — the status poller never runs this."
  (or (not (integerp pid))
      (ignore-errors
        (zerop (nth-value 2 (uiop:run-program (list "kill" "-0" (princ-to-string pid))
                                              :ignore-error-status t
                                              :output nil :error-output nil))))))

(defun ide-context-display-path (file)
  "FILE relative to the working directory when it is inside it, else as the
IDE sees it.  Both sides are resolved through TRUENAME first: an editor
reporting /tmp/x and a shell sitting in /private/tmp/x are the same directory,
and an unresolved comparison would print absolute paths forever on macOS."
  (let* ((cwd (or (ignore-errors (namestring (truename (uiop:getcwd))))
                  (ignore-errors (namestring (uiop:getcwd)))))
         (real (or (ignore-errors (namestring (truename file))) file)))
    (if (and cwd real (evo.util:string-prefix-p cwd real)
             (> (length real) (length cwd)))
        (subseq real (length cwd))
        file)))

(defun ide-context-elide (text)
  "Cap TEXT at *ide-context-max-lines* / *ide-context-max-chars*.
Returns (values text elided-p)."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (elided-p nil))
    (when (> (length lines) *ide-context-max-lines*)
      (setf lines (subseq lines 0 *ide-context-max-lines*)
            elided-p t))
    (let ((joined (evo.util:string-join (string #\Newline) lines)))
      (when (> (length joined) *ide-context-max-chars*)
        (setf joined (subseq joined 0 *ide-context-max-chars*)
              elided-p t))
      (values joined elided-p))))

(defun ide-context-fence (text)
  "A backtick fence longer than any run of backticks inside TEXT."
  (let ((longest 0)
        (run 0))
    (loop for ch across text
          do (if (char= ch #\`)
                 (setf run (1+ run) longest (max longest run))
                 (setf run 0)))
    (make-string (max 3 (1+ longest)) :initial-element #\`)))

(defun ide-context-selection-lines (state)
  "Number of lines in the current selection, or NIL when nothing is selected."
  (let ((selection (evo.util:pget state :selection)))
    (and selection (evo.util:pget selection :line-count))))

(defun ide-context-block (state)
  "The text injected ahead of a user message, or NIL when there is nothing
worth saying."
  (let ((file (evo.util:pget state :file)))
    (when (stringp file)
      (let* ((selection (evo.util:pget state :selection))
             (text (evo.util:pget state :selected-text))
             (display (ide-context-display-path file)))
        (with-output-to-string (out)
          (format out "<ide-context>~%")
          (format out "focused file: ~a~@[ (unsaved changes)~]~%"
                  display (evo.util:pget state :dirty))
          (when selection
            (format out "selection: lines ~a-~a (~a line~:p)~%"
                    (evo.util:pget selection :start-line)
                    (evo.util:pget selection :end-line)
                    (or (evo.util:pget selection :line-count) 0)))
          (when (and selection (stringp text) (plusp (length text)))
            (multiple-value-bind (body elided-p) (ide-context-elide text)
              (let ((fence (ide-context-fence body)))
                (format out "~a~@[~a~]~%~a~%~a~%"
                        fence (evo.util:pget state :language-id) body fence)
                (when (or elided-p (evo.util:pget state :selected-text-truncated))
                  (format out "(selection truncated for length; read the file for the rest)~%")))))
          (format out "Ambient editor state, not necessarily the task: use it to resolve~%")
          (format out "\"this file\", \"the selection\", \"here\"; ignore it when the request~%")
          (format out "stands on its own.~%")
          (format out "</ide-context>"))))))

;;; The two integration points.

(defun ide-context-inject (agent)
  "Journal the current IDE context as a :custom-message ahead of the user's
message.  Journaled rather than projected on the fly so history stays
append-only: the block the model saw for a message never changes, which keeps
the provider prompt cache intact across a turn's tool calls."
  (ignore-errors
    (let ((state (ide-context-state)))
      (when (and state (ide-context-alive-p (evo.util:pget state :pid)))
        (let ((text (ide-context-block state)))
          (when (and text (not (equal text *ide-context-last-injected*)))
            (evo:inject-context text :key "ide-context" :agent agent)
            (setf *ide-context-last-injected* text)))))))

(defun ide-context-submit-wrapper (tui text &rest args)
  ;; &rest, not a fixed arity: this wraps a kernel function, and a wrapper
  ;; that pins its signature breaks the moment the kernel grows an argument
  ;; (submit-to-agent gained attached images).  Pass whatever came in.
  (ide-context-inject (evo.tui::tui-agent tui))
  (apply *ide-context-original-submit* tui text args))

(defun ide-context-label ()
  "Status segment: shown only while a selection exists."
  (let* ((state (ignore-errors (ide-context-state)))
         (lines (and state (ide-context-selection-lines state))))
    (when (and (integerp lines) (plusp lines))
      (evo.tui::dim (format nil " · ⧉ ~a line~:p selected" lines)))))

(defun ide-context-status-line-wrapper (tui)
  (let ((base (if *ide-context-original-status-line*
                  (funcall *ide-context-original-status-line* tui)
                  ""))
        (label (ignore-errors (ide-context-label))))
    (if label (concatenate 'string base label) base)))

;;; Installation.  Kernel packages are locked; unlock around the single
;;; fdefinition change, exactly as any other userspace patch does.

(defun ide-context-patch (symbol saved-slot new-function)
  (let ((pkg (symbol-package symbol)))
    (unless (symbol-value saved-slot)
      (setf (symbol-value saved-slot) (symbol-function symbol)))
    (evo.port:unlock-package pkg)
    (unwind-protect
         (setf (symbol-function symbol) new-function)
      (evo.port:lock-package pkg))))

(defun ide-context-install-wrappers ()
  (ide-context-patch 'evo.tui::status-line '*ide-context-original-status-line*
                     #'ide-context-status-line-wrapper)
  (ide-context-patch 'evo.tui::submit-to-agent '*ide-context-original-submit*
                     #'ide-context-submit-wrapper))

(defun ide-context-generation-current-p (generation)
  (= generation *ide-context-poller-generation*))

(defun ide-context-poller-loop (generation)
  "Repaint the status line when the selection changes.  The TUI only repaints
when marked dirty, so somebody has to notice; a stat every quarter second is
cheaper than a socket."
  (let ((last nil))
    (loop while (ide-context-generation-current-p generation)
          do (let ((label (ignore-errors (ide-context-label)))
                   (tui (ignore-errors evo.tui::*tui*)))
               (unless (equal label last)
                 (setf last label)
                 (when tui (ignore-errors (setf (evo.tui::tui-dirty tui) t))))
               (sleep *ide-context-poll-seconds*)))))

(defun ide-context-start-poller ()
  (incf *ide-context-poller-generation*)
  (let ((generation *ide-context-poller-generation*))
    (setf *ide-context-poller-thread*
          (bt:make-thread (lambda () (ide-context-poller-loop generation))
                          :name "evo-ide-context"))))

(when (ide-context-path)
  (ide-context-install-wrappers)
  (ide-context-start-poller))
