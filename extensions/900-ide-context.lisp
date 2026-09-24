;;;; 900-ide-context.lisp — ambient editor context from a companion IDE plugin.
;;;;
;;;; The IDE (see the evo-vscode extension) writes a small JSON file whenever
;;;; editor focus or the text selection changes, and exports its path to the
;;;; terminal as EVO_IDE_CONTEXT.  This extension consumes it:
;;;;
;;;;   * whenever something the user said is journaled (the :user-message
;;;;     event), the focused file and any selected text are journaled as a
;;;;     :custom-message immediately before it, so the model can resolve "this
;;;;     file" / "the selection" without a tool call;
;;;;   * the status line grows a "⧉ N lines selected" segment while a
;;;;     selection exists.
;;;;
;;;; Transport is a file, not a socket: evo needs this state at exactly two
;;;; moments and initiates both, so there is nothing to push.  No port, no
;;;; token, no reconnect — and a hand-written JSON file is the whole test rig.
;;;;
;;;; Without EVO_IDE_CONTEXT in the environment nothing is installed: no
;;;; hook, no thread, no behaviour change.

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

(defvar *ide-context-poller-stop* nil
  "Set by the tracked task's stop function; the poller loop watches it.")

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
           (evo:json->sexpr parsed)))))

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
  "True unless PID is known to be *gone*.  One fork per submitted prompt,
and only there — the status poller never runs this.  Fail-open on purpose:
where the probe itself cannot run (no kill(1), i.e. Windows) the editor's
own liveness is unknown, and dropping its context on an unanswerable
question is the worse of the two mistakes."
  (or (not (integerp pid))
      (evo.port:windows-p)
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
the provider prompt cache intact across a turn's tool calls.

Called from :user-message, which the kernel announces on the run's own thread
at the turn boundary, just before it journals what the user said — so the
block lands immediately ahead of the message, never inside a turn in flight."
  (ignore-errors
    (let ((state (ide-context-state)))
      (when (and state (ide-context-alive-p (evo.util:pget state :pid)))
        (let ((text (ide-context-block state)))
          (when (and text (not (equal text *ide-context-last-injected*)))
            (evo:inject-context text :key "ide-context" :agent agent)
            (setf *ide-context-last-injected* text)))))))

(defun ide-context-user-message (payload)
  ":user-message — the user said something; put the editor's context first."
  (ide-context-inject (evo.util:pget payload :agent)))

(defun ide-context-label (&optional tui)
  "Status segment text: shown only while a selection exists.  No separator of
its own — EVO.TUI:ADD-STATUS-SEGMENT's renderer owns the punctuation between
segments, so a segment that renders nothing leaves no dangling \" · \"."
  (declare (ignore tui))
  (let* ((state (ignore-errors (ide-context-state)))
         (lines (and state (ide-context-selection-lines state))))
    (when (and (integerp lines) (plusp lines))
      (evo.tui:dim (format nil "⧉ ~a line~:p selected" lines)))))

;;; Installation: a hook and a status segment, both registrations the kernel
;;; sees — so a reload withdraws them with this file's generation.

(defun ide-context-install ()
  ;; NAMEd: this file is re-loaded on every /reload and on :load replay.
  (evo:on :user-message #'ide-context-user-message :name :ide-context)
  ;; The status line is a registry, not a function to wrap: several parties
  ;; want a piece of that line and only the renderer can see them all at once.
  ;; Order 600 puts this just inboard of the core segments (100-400).
  (evo.tui:add-status-segment :ide-selection #'ide-context-label
                              :side :left :order 600))

(defun ide-context-poller-loop ()
  "Ask the TUI to repaint when the selection changes.  The TUI only repaints
when marked dirty, so somebody has to notice; a stat every quarter second is
cheaper than a socket.

The repaint is REQUESTED through the event queue rather than by setting
TUI-DIRTY here: every TUI slot belongs to the TUI thread, and a poller writing
one directly is the same cross-thread mutation this design removed everywhere
else."
  (let ((last nil))
    (loop until *ide-context-poller-stop*
          do (let ((label (ignore-errors (ide-context-label))))
               (unless (equal label last)
                 (setf last label)
                 (ignore-errors (evo.tui:request-repaint)))
               (sleep *ide-context-poll-seconds*)))))

(defun ide-context-start-poller ()
  ;; Tracked: on /reload the task is stopped and JOINED before the next
  ;; generation loads, so two pollers can never run at once.
  (setf *ide-context-poller-stop* nil)
  (evo:spawn-task :name :ide-context-poller
                  :run #'ide-context-poller-loop
                  :stop (lambda () (setf *ide-context-poller-stop* t))))

;; Everything installed here — the hook, the status segment, the poller — is
;; a registration the kernel tracks, so a reload withdraws all of it with this
;; file's generation and there is nothing to undo by hand.
(when (ide-context-path)
  (ide-context-install)
  (ide-context-start-poller))
