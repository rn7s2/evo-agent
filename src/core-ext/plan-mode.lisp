;;;; plan-mode.lisp — plan/auto modes, a core extension.
;;;;
;;;; A mode is journal state, never an in-memory flag: the current mode is
;;;; the fold of :custom "mode" entries, so it survives restart and
;;;; compaction untouched and every frontend reads the same value.
;;;;
;;;; Entering plan mode does three things through the public API — journal
;;;; the mode, gate the tool set, inject the plan instructions as a keyed
;;;; :custom-message — and two hooks registered here back that up:
;;;;
;;;;   :tool-call        the enforcement gate.  A :tools-change only lands at
;;;;                     the next save point, and the model can name a tool
;;;;                     that is not in its schema list, so policy is decided
;;;;                     here, per call, against live journal state.
;;;;   :transform-context filters the injected instructions back out of
;;;;                     context once the mode is off — the instructions are
;;;;                     never edited out of the journal, only out of the
;;;;                     projection sent to the model.
;;;;
;;;; Core extension in the literal sense: bundled and always on, but built on
;;;; nothing but the EVO public API — a userspace extension could have
;;;; written it, and can extend it (push onto *plan-tools*, rebind the bash
;;;; allowlist) without touching the kernel.

(in-package :evo.plan)

(defparameter *modes*
  '(("auto" . "full permissions (default)")
    ("plan" . "read-only: explore and present a plan"))
  "Mode name -> one-line description.  The frontends' mode picker renders
this; adding a mode here is not enough — policy lives in SET-MODE.")

(defparameter *default-mode* "auto"
  "Mode of a session that never switched: fully permissive.")

(defparameter *plan-tools* '("read" "bash" "get_goal" "todo")
  "Tools available while planning.  One list, two uses: the gated tool set
the model is offered, and the allowlist the :tool-call gate enforces —
they cannot drift apart.  Extensions may push read-only tools onto it.")

(defparameter *plan-bash-allowlist*
  '("ls" "cat" "head" "tail" "grep" "rg" "find" "wc" "pwd" "git" "file" "du"
    "stat" "which" "tree" "echo" "sort" "uniq" "diff" "awk" "sed" "basename"
    "dirname" "realpath" "env" "date")
  "Command heads allowed while planning.  Every chained segment of a bash
command must have an allowlisted head, so `git log | head` passes and
`git status && rm -rf x` does not.")

(defparameter *plan-instructions*
  "PLAN MODE is on. Do not modify anything: no writing or editing files, no
state-changing shell commands. Explore the code, then produce a concrete
step-by-step plan and present it to the user. When the user is satisfied
they will switch you back to auto mode (shift+tab or /permission) to execute.")

(defparameter *instruction-key* "plan-mode"
  "Key on the injected :custom-message, so the context filter can find it.")

;;; Mode state.

(defun mode-name (name)
  "NAME normalized to a known mode name, or NIL."
  (car (assoc (string name) *modes* :test #'string-equal)))

(defun current-mode (&optional (agent evo:*agent*))
  (or (and agent (evo:custom-state "mode" agent)) *default-mode*))

(defun plan-mode-p (&optional (agent evo:*agent*))
  (equal (current-mode agent) "plan"))

(defun set-mode (mode &optional (agent evo:*agent*))
  "Switch AGENT to MODE (\"auto\" or \"plan\") and apply its policy.
Returns the normalized mode name when it changed, NIL when already there —
frontends announce on a non-NIL return.  Signals on an unknown mode."
  (let ((mode (or (mode-name mode)
                  (error "Unknown mode ~s (known: ~{~a~^ ~})"
                         mode (mapcar #'car *modes*)))))
    (unless (equal mode (current-mode agent))
      (evo:set-custom-state "mode" mode agent)
      (cond ((equal mode "plan")
             (evo:set-active-tools agent *plan-tools*)
             (evo:inject-context *plan-instructions*
                                 :key *instruction-key* :agent agent))
            (t
             (evo:set-active-tools agent nil)))   ; nil = full tool set
      mode)))

;;; Bash gating.  First-word allowlisting is only as good as its reading of
;;; the shell: `git status && rm -rf x` starts with an allowed word, and
;;; `grep -n "a || b" src` contains an operator that is only text.  So the
;;; command is scanned once, quote-aware, and every chained segment is
;;; judged on its own head.

(defun scan-shell-command (command)
  "Walk COMMAND once, tracking quotes.  Returns (values SEGMENTS
SUBSTITUTION-P REDIRECT-P): SEGMENTS are the pieces split at unquoted
; | || && and newlines, SUBSTITUTION-P is true if it embeds $(...) or
backticks, REDIRECT-P is true if it redirects output to a file (an fd dup
like 2>&1 is not a write)."
  (let ((segments nil) (start 0) (substitution-p nil) (redirect-p nil)
        (quote-char nil) (i 0) (n (length command)))
    (labels ((peek (k) (let ((j (+ i k))) (and (< j n) (char command j))))
             (cut (end next)
               (push (subseq command start end) segments)
               (setf start next)))
      (loop while (< i n)
            do (let ((c (char command i)))
                 (cond
                   ;; Single quotes are opaque: nothing inside is special.
                   ((eql quote-char #\')
                    (when (char= c #\') (setf quote-char nil))
                    (incf i))
                   ;; Double quotes still interpolate $(...) and backticks.
                   ((eql quote-char #\")
                    (cond ((char= c #\\) (incf i 2))
                          ((char= c #\") (setf quote-char nil) (incf i))
                          ((and (char= c #\$) (eql (peek 1) #\()) (setf substitution-p t) (incf i 2))
                          ((char= c #\`) (setf substitution-p t) (incf i))
                          (t (incf i))))
                   ((char= c #\\) (incf i 2))
                   ((or (char= c #\') (char= c #\")) (setf quote-char c) (incf i))
                   ((and (char= c #\$) (eql (peek 1) #\()) (setf substitution-p t) (incf i 2))
                   ((char= c #\`) (setf substitution-p t) (incf i))
                   ((char= c #\>)
                    (let ((j (1+ i)))
                      (when (eql (peek 1) #\>) (incf j))
                      (loop while (and (< j n) (member (char command j) '(#\Space #\Tab)))
                            do (incf j))
                      (unless (and (< j n) (char= (char command j) #\&))
                        (setf redirect-p t))
                      (setf i j)))
                   ((and (char= c #\&) (eql (peek 1) #\&)) (cut i (+ i 2)) (incf i 2))
                   ((and (char= c #\|) (eql (peek 1) #\|)) (cut i (+ i 2)) (incf i 2))
                   ((member c '(#\| #\; #\Newline)) (cut i (1+ i)) (incf i))
                   (t (incf i)))))
      (push (subseq command start) segments)
      (values (nreverse segments) substitution-p redirect-p))))

(defun segment-head (segment)
  "First word of SEGMENT, or \"\" when it is blank."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) segment))
         (end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                               trimmed)
                  (length trimmed))))
    (subseq trimmed 0 end)))

(defun bash-block-reason (command)
  "NIL if COMMAND may run while planning, else why it may not."
  (multiple-value-bind (segments substitution-p redirect-p)
      (scan-shell-command (or command ""))
    (cond
      (substitution-p
       "command substitution ($(...) or backticks) can run anything")
      (redirect-p
       "output redirection writes files")
      (t
       (loop for segment in segments
             for head = (segment-head segment)
             unless (or (zerop (length head))
                        (member head *plan-bash-allowlist* :test #'equal))
               return (format nil "bash is allowlisted (~{~a~^ ~}); '~a' is not"
                              *plan-bash-allowlist* head))))))

;;; The hooks.

(defun gate-tool-call (call)
  "The :tool-call hook.  While planning, only *plan-tools* run, and bash
only for read-only commands."
  (when (plan-mode-p)
    (let ((name (pget call :name)))
      (cond
        ((not (member name *plan-tools* :test #'equal))
         (list :block t
               :reason (format nil "plan mode is read-only: ~a is not available (~{~a~^ ~} are). Present a plan; the user switches back with shift+tab or /permission to execute it."
                               name *plan-tools*)))
        ((equal name "bash")
         (let ((reason (bash-block-reason (pget (pget call :arguments) :command))))
           (when reason
             (list :block t :reason (format nil "plan mode: ~a" reason)))))))))

(defun filter-plan-instructions (messages)
  "The :transform-context hook.  Plan instructions vanish from the
projection whenever the mode is off; the journal keeps them."
  (if (plan-mode-p)
      messages
      (remove-if (lambda (m)
                   (equal (pget (pget m :meta) :key) *instruction-key*))
                 messages)))

(defvar *hooks-installed* nil
  "Guard: this file is loaded once into the image, but a reload during
development must not stack a second copy of each hook.")

(defun install-hooks ()
  (unless *hooks-installed*
    ;; Late binding through the symbols, so redefining a gate takes effect.
    (evo:on :tool-call (lambda (call) (gate-tool-call call)))
    (evo:on :transform-context (lambda (messages) (filter-plan-instructions messages)))
    (setf *hooks-installed* t)))

(install-hooks)
