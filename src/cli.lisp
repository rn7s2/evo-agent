;;;; cli.lisp — the evo CLI (non-TUI modes).
;;;;
;;;; The MVP ships the two scriptable frontends the design wants from day
;;;; one: print mode (`evo -p "prompt"`) and event-stream mode (`--events`,
;;;; line-delimited sexprs on stdout).  The TUI is the interactive frontend.
;;;;
;;;; stdout carries assistant text (print mode) or event sexprs (event mode);
;;;; the tool/turn trace goes to stderr.

(in-package :evo.cli)

(defparameter *usage*
  "evo — a goal-oriented, self-evolving agent

Usage:
  evo                            interactive TUI (on a terminal)
  evo -p \"prompt\"              run one task in print mode, stream text to stdout
  evo --goal \"objective\" [-p \"first prompt\"]
                                 create a goal and run until complete/blocked/budget
  evo --resume [path] [-p ...]   resume a session (default: latest for this cwd);
                                 with an active goal and no -p, continues the goal
  evo --events ...               emit line-delimited sexpr events instead of text
  evo --list-sessions            list sessions for this cwd
  evo --model <id>               model id (default from settings, else claude-sonnet-5)
  evo --thinking <level>         off|low|medium|high|xhigh (default medium)
  evo --no-userspace             boot without extensions (quarantine mode)
  evo --no-supervisor            run the session in-process, no crash-restart parent
  evo --help

evo supervises itself: crashes and hangs restart the session with --resume;
a goal that was active picks itself back up.  Exit codes: 0 done, 1 error,
2 goal blocked, 3 budget-limited, 64 usage error.

Settings: ~/.evo/settings.sexp and <cwd>/.evo/settings.sexp (project wins), e.g.
  (:model \"ark-deepseek-v4-pro\" :thinking :xhigh
   :providers (:anthropic (:base-url \"http://127.0.0.1:8787\" :api-key \"sk-...\")))")

(defun parse-args (argv)
  "Parse ARGV into a plist.  Signals on unknown flags."
  (let ((opts nil))
    (loop while argv
          for arg = (pop argv)
          do (cond
               ((member arg '("-p" "--print") :test #'string=)
                (setf (getf opts :prompt) (or (pop argv) (error "-p needs a prompt"))))
               ((string= arg "--goal")
                (setf (getf opts :goal) (or (pop argv) (error "--goal needs an objective"))))
               ((string= arg "--resume")
                (setf (getf opts :resume)
                      (if (and argv (not (evo.util:string-prefix-p "-" (first argv))))
                          (pop argv)
                          :latest)))
               ((string= arg "--events") (setf (getf opts :events) t))
               ((string= arg "--list-sessions") (setf (getf opts :list-sessions) t))
               ((string= arg "--model")
                (setf (getf opts :model) (or (pop argv) (error "--model needs an id"))))
               ((string= arg "--thinking")
                (setf (getf opts :thinking)
                      (intern (string-upcase (or (pop argv) (error "--thinking needs a level")))
                              :keyword)))
               ((string= arg "--no-userspace") (setf (getf opts :no-userspace) t))
               ((string= arg "--no-supervisor") (setf (getf opts :no-supervisor) t))
               ((member arg '("-h" "--help") :test #'string=) (setf (getf opts :help) t))
               ((string= arg "--version") (setf (getf opts :version) t))
               (t (error "Unknown argument: ~a (try --help)" arg))))
    opts))

;;; Print-mode rendering.

(defvar *printed-text-p* nil)

(defun print-mode-event-handler (event)
  (let ((type (pget event :type)))
    (case type
      (:text-delta
       (write-string (pget event :text) *standard-output*)
       (setf *printed-text-p* t)
       (force-output *standard-output*))
      (:thinking-delta
       (when (getenv "EVO_VERBOSE")
         (write-string (pget event :text) *error-output*)
         (force-output *error-output*)))
      (:tool-call-start
       (when *printed-text-p*
         (terpri *standard-output*) (force-output *standard-output*)
         (setf *printed-text-p* nil))
       (format *error-output* "~&⏺ ~a~%" (pget event :name))
       (force-output *error-output*))
      (:tool-result
       (when (pget event :is-error)
         (format *error-output* "~&  ✗ ~a~%" (pget event :content))
         (force-output *error-output*)))
      (:message-end
       (when *printed-text-p*
         (terpri *standard-output*) (force-output *standard-output*)
         (setf *printed-text-p* nil))
       (let ((err (pget event :error)))
         (when err
           (format *error-output* "~&✗ provider error: ~a~%" err)
           (force-output *error-output*)))))))

(defun event-mode-handler (event)
  (handler-case
      (write-sexpr-line event *standard-output*)
    (error ()
      ;; An event containing a non-journal-safe value must not kill the run.
      (format *standard-output* "(:type :unprintable-event)~%")))
  (force-output *standard-output*))

(defun cmd-list-sessions ()
  (let ((sessions (list-sessions)))
    (if (null sessions)
        (format t "No sessions for ~a~%" (namestring (uiop:getcwd)))
        (dolist (s sessions)
          (format t "~a  ~a~%" (pget s :timestamp) (pget s :path))))))

(defun resolve-journal (opts)
  "Open or create the session journal per OPTS."
  (let ((resume (getf opts :resume)))
    (cond
      ((null resume) (make-session-journal))
      ((eq resume :latest)
       (let ((path (latest-session)))
         (unless path (error "No sessions to resume for ~a" (namestring (uiop:getcwd))))
         (open-journal path)))
      (t (open-journal resume)))))

(define-condition usage-error (error)
  ((text :initarg :text :reader usage-error-text))
  (:report (lambda (c s) (format s "~a" (usage-error-text c)))))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  "Exit codes are supervisor protocol: 0 done, 1 error (restart-eligible),
2 goal blocked, 3 budget-limited, 64 usage error (never restart)."
  (let ((opts (handler-case (parse-args argv)
                (error (e)
                  (format *error-output* "evo: ~a~%" e)
                  (return-from main 64)))))
    (handler-case
        (cond
          ((getf opts :help) (write-line *usage*) 0)
          ((getf opts :version) (write-line "evo 0.1.0") 0)
          ((getf opts :list-sessions) (load-settings) (cmd-list-sessions) 0)
          ;; One binary, two roles: the plain invocation is the
          ;; supervisor parent; it re-spawns this same binary as the child.
          ((supervised-run-p opts) (supervise argv))
          (t (run-cli opts)))
      (usage-error (e)
        (format *error-output* "evo: ~a~%" e)
        64)
      (error (e)
        (format *error-output* "evo: ~a~%" e)
        1))))

(defun setup-agent (opts &key events-cb)
  "Shared session bring-up for every frontend.  Returns (values agent resumed-p)."
  (load-settings)
  (let* ((journal (resolve-journal opts))
         (resumed-p (journal-started-p journal))
         (agent (make-agent
                 :journal journal
                 :events-cb events-cb
                 :model-override (getf opts :model)
                 :thinking-override (getf opts :thinking))))
    (setf evo:*agent* agent)
    (evo.kernel:lock-kernel-packages)
    ;; Userspace: boot extension dirs, then replay the session's :load entries.
    (unless (getf opts :no-userspace)
      (let ((evo.kernel::*current-journal* journal))
        (boot-extensions :journal (and (not resumed-p) journal))
        (when resumed-p
          (replay-loads (fold-state journal)))))
    (run-hooks :session-start (list :agent agent :resumed resumed-p))
    ;; Journal explicit model/thinking choices so resume preserves them.
    (when (getf opts :model)
      (append-entry journal (list :type :model-change
                                  :provider :anthropic :model (getf opts :model))))
    (when (getf opts :thinking)
      (append-entry journal (list :type :thinking-change
                                  :thinking (getf opts :thinking))))
    (when (getf opts :goal)
      (evo.kernel:create-goal-entry agent (getf opts :goal)))
    (values agent resumed-p)))

(defun tty-p ()
  (and (plusp (sb-unix:unix-isatty 0))
       (plusp (sb-unix:unix-isatty 1))))

(defun run-cli (opts)
  (if (and (tty-p)
           (not (getf opts :prompt))
           (not (getf opts :events)))
      ;; Interactive: the tui core extension.
      (multiple-value-bind (agent resumed-p) (setup-agent opts)
        (evo.tui:start-tui agent :resumed-p resumed-p))
      (run-headless opts)))

(defun run-headless (opts)
  (multiple-value-bind (agent resumed-p)
      (setup-agent opts :events-cb (if (getf opts :events)
                                       #'event-mode-handler
                                       #'print-mode-event-handler))
    (declare (ignore resumed-p))
    (let ((journal (agent-journal agent)))
      ;; Seed the run.
      (let ((prompt (getf opts :prompt))
            (goal (evo.kernel:current-goal agent)))
        (cond
          (prompt (queue-steering agent prompt))
          ((and goal (eq (pget goal :status) :active))
           (queue-steering agent (evo.kernel:goal-continuation-for agent goal)))
          (t (error 'usage-error :text "Nothing to do headless: give -p \"prompt\", --goal, or --resume a session with an active goal"))))
      (let* ((outcome (run-until-settled agent))
             (goal (evo.kernel:current-goal agent)))
        (when (journal-started-p journal)
          (format *error-output* "~&session: ~a~%" (namestring (journal-path journal))))
        (when goal
          (format *error-output* "goal ~a: ~a~%"
                  (pget goal :goal-id) (string-downcase (pget goal :status))))
        ;; Exit codes are supervisor protocol: 0 done, 1 error
        ;; (restart-eligible), 2 blocked by model, 3 budget-limited —
        ;; 2 and 3 need a human, the supervisor must NOT restart them.
        (cond ((and goal (eq (pget goal :status) :complete)) 0)
              ((and goal (eq (pget goal :status) :blocked))
               (if (equal (pget goal :blocked-reason) "turn-error") 1 2))
              ((and goal (eq (pget goal :status) :budget-limited)) 3)
              ((eq outcome :stop) 0)
              (t 1))))))
