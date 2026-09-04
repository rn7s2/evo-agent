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
  evo --image <path> -p ...      attach an image to the first prompt (repeatable)
  evo --goal \"objective\" [-p \"first prompt\"]
                                 create a goal and run until complete or budget
  evo --resume [path] [-p ...]   resume a session (default: the one most recently
                                 worked in, for this cwd);
                                 with an active goal and no -p, continues the goal
  evo --events ...               emit line-delimited sexpr events instead of text
  evo --list-sessions            list sessions for this cwd, last worked in first
  evo --model <id>               model id (default: the :model setting from init.lisp)
  evo --thinking <level>         low|medium|high|xhigh|max (default medium)
  evo --no-userspace             boot without init.lisp, post-init.lisp, or extensions (quarantine mode)
  evo --no-supervisor            run the session in-process, no crash-restart parent
  evo --help

evo supervises itself: crashes and hangs restart the session with --resume;
a goal that was active picks itself back up.  Exit codes: 0 done, 1 error,
2 goal paused, 3 budget-limited, 64 usage error.

Config: ~/.evo/init.lisp, then <cwd>/.evo/init.lisp, then extensions, then
~/.evo/post-init.lisp, then <cwd>/.evo/post-init.lisp (Lisp, evaluated in order;
later calls override).  post-init.lisp runs after extensions, so it can reference
models registered by extensions.  evo ships no built-in model table, e.g.
  (evo:register-model \"claude-opus-5\"
    :provider :anthropic
    :context-window 1000000 :max-output 128000
    :effort t :thinking-mode :adaptive)
  (evo:set-setting :model \"claude-opus-5\")")

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
               ((string= arg "--image")
                (setf (getf opts :images)
                      (append (getf opts :images)
                              (list (or (pop argv) (error "--image needs a path"))))))
               ((string= arg "--model")
                (setf (getf opts :model) (or (pop argv) (error "--model needs an id"))))
               ((string= arg "--thinking")
                (let ((level (intern (string-upcase
                                      (or (pop argv) (error "--thinking needs a level")))
                                     :keyword)))
                  (unless (member level +effort-levels+)
                    (error "--thinking must be one of low|medium|high|xhigh|max"))
                  (setf (getf opts :thinking) level)))
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
       (let ((args (evo.kernel:tool-call-display-arguments
                    (pget event :name) (pget event :arguments)
                    (pget event :arguments-json))))
         (if args
             ;; One bounded line: a write call carries whole files in :content.
             (format *error-output* "~&⏺ ~a ~a~%" (pget event :name)
                     (evo.util:truncate-string
                      (substitute #\Space #\Newline
                                  (if (stringp args) args (format nil "~s" args)))
                      200 "…"))
             (format *error-output* "~&⏺ ~a~%" (pget event :name))))
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

(define-condition usage-error (error)
  ((text :initarg :text :reader usage-error-text))
  (:report (lambda (c s) (format s "~a" (usage-error-text c)))))

(defun resolve-journal (opts)
  "Open or create the session journal per OPTS."
  (let ((resume (getf opts :resume)))
    (cond
      ((null resume) (make-session-journal))
      ((eq resume :latest)
       (let ((path (latest-session)))
         ;; A usage error, not a crash: exit 64 is the code the supervisor
         ;; never restarts, and restarting cannot conjure a session.
         (unless path
           (error 'usage-error
                  :text (format nil "No sessions to resume for ~a"
                                (namestring (uiop:getcwd)))))
         (open-journal path)))
      (t (open-journal resume)))))

(defun main (&optional (argv (evo.port:argv)))
  "Exit codes are supervisor protocol: 0 done, 1 error (restart-eligible),
2 goal paused, 3 budget-limited, 64 usage error (never restart)."
  (let ((opts (handler-case (parse-args argv)
                (error (e)
                  (format *error-output* "evo: ~a~%" e)
                  (return-from main 64)))))
    (handler-case
        (cond
          ((getf opts :help) (write-line *usage*) 0)
          ((getf opts :version) (write-line "evo 0.1.0") 0)
          ((getf opts :list-sessions) (cmd-list-sessions) 0)
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

(defun toplevel ()
  "Entry point of the built binary (SBCL image toplevel / ECL epilogue):
fresh id entropy, debugger off, in-image compiler on, exit code from MAIN.

RESEED-IDS comes first and must stay there: the saved image carries the
random state it was built with, so every id minted before this call would
repeat across processes."
  (reseed-ids)
  (evo.port:disable-debugger)
  (evo.port:ensure-in-image-compiler)
  (evo.port:exit-lisp (main)))

(defun no-model-message (opts)
  (format nil (cat "No model is configured. evo ships no built-in model table: create~%"
                   "~a~%(or <project>/.evo/init.lisp) and register the models you use, then pick a default:~%~%  "
                   "(evo:register-model \"claude-opus-5\"~%    "
                   ":provider :anthropic~%    "
                   ":context-window 1000000 :max-output 128000~%    "
                   ":effort t :thinking-mode :adaptive)~%  "
                   "(evo:set-setting :model \"claude-opus-5\")~%~%"
                   "A commented sample is at docs/examples/init.lisp (installed to~%"
                   "~a by `make install-home`)."
                   "~@[~%~%~a~]")
          (namestring (merge-pathnames "init.lisp" (evo.util:evo-home)))
          (namestring (merge-pathnames "docs/examples/init.lisp" (evo.util:evo-home)))
          (and (getf opts :no-userspace)
               "Note: --no-userspace skips init files, so no models are registered in this mode.")))

(defun preflight-model (agent journal opts)
  "Headless-only model check, raised as usage-error: exit 64 is the one
code the supervisor never restarts, so a config problem cannot enter the
restart/quarantine loop.  Runs after SETUP-AGENT has journaled a --model
choice, so it validates the id the first turn will actually use.  The TUI
never preflights — it gates at run start instead (CHECK-MODEL-READY),
where /model and /reload can fix the registry in place."
  (let* ((state (fold-state journal))
         (id (or (evo.journal:state-model state)
                 (evo.kernel:agent-model-override agent)
                 (setting :model))))
    (unless id
      (error 'usage-error :text (no-model-message opts)))
    (handler-case (find-model id (evo.kernel:effective-model-provider state id))
      (error (e)
        (error 'usage-error
               :text (format nil "~a~@[~%~%~a~]" e
                             (and (getf opts :no-userspace)
                                  "Note: --no-userspace skips init files, so no models are registered in this mode.")))))))

(defun setup-agent (opts &key events-cb)
  "Shared session bring-up for every frontend.  Returns (values agent resumed-p)."
  (let* ((journal (resolve-journal opts))
         (resumed-p (journal-started-p journal))
         (agent (make-agent
                 :journal journal
                 :events-cb events-cb
                 :model-override (getf opts :model)
                 :thinking-override (getf opts :thinking))))
    (setf evo:*agent* agent)
    (evo.kernel:lock-kernel-packages)
    ;; Userspace: init files (config), extension dirs, then replay the
    ;; session's :load entries.
    (if (getf opts :no-userspace)
        (progn                        ; kernel registries still need seeding
          (evo.util:reset-settings)
          (evo.provider:reset-user-registries))
        (let ((evo.kernel::*current-journal* journal))
          (evo.kernel:boot-userspace :journal (and (not resumed-p) journal))
          (when resumed-p
            (replay-loads (fold-state journal)))))
    (run-hooks :session-start (list :agent agent :resumed resumed-p))
    ;; Journal explicit model/thinking choices so resume preserves them.
    (when (getf opts :model)
      (append-entry journal (list :type :model-change :model (getf opts :model))))
    (when (getf opts :thinking)
      (append-entry journal (list :type :thinking-change
                                  :thinking (getf opts :thinking))))
    (when (getf opts :goal)
      (evo.kernel:create-goal-entry agent (getf opts :goal)))
    (values agent resumed-p)))

(defun tty-p ()
  (evo.port:tty-p))

(defun run-cli (opts)
  (if (and (tty-p)
           (not (getf opts :prompt))
           (not (getf opts :events)))
      ;; Interactive: the tui core extension.
      (multiple-value-bind (agent resumed-p) (setup-agent opts)
        (evo.tui:start-tui agent :resumed-p resumed-p))
      (run-headless opts)))

(defun headless-images (opts)
  "Resolve --image paths to :image content blocks.  A bad path is a usage
error: headless has no editor to correct it in, and silently running the
prompt without the image the user asked for is worse than not running."
  (loop for path in (getf opts :images)
        collect (multiple-value-bind (block reason) (evo.media:attach-image-file path)
                  (or block (error 'usage-error :text (format nil "--image: ~a" reason))))))

(defun run-headless (opts)
  (multiple-value-bind (agent resumed-p)
      (setup-agent opts :events-cb (if (getf opts :events)
                                       #'event-mode-handler
                                       #'print-mode-event-handler))
    (declare (ignore resumed-p))
    (let ((journal (agent-journal agent)))
      ;; Fail eagerly: headless has no /model recovery, and nothing may
      ;; enter the journal before the model is known to resolve.
      (preflight-model agent journal opts)
      ;; Seed the run.
      (let ((prompt (getf opts :prompt))
            (images (headless-images opts))
            (goal (evo.kernel:current-goal agent)))
        (cond
          (prompt (queue-steering agent prompt :images images))
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
        ;; (restart-eligible), 2 paused, 3 budget-limited — 2 and 3 need a
        ;; human, the supervisor must NOT restart them.  A turn error leaves
        ;; the goal active and lands on the (t 1) branch, so the
        ;; supervisor's --resume restart picks the goal back up.
        (cond ((and goal (eq (pget goal :status) :complete)) 0)
              ((and goal (eq (pget goal :status) :paused)) 2)
              ((and goal (eq (pget goal :status) :budget-limited)) 3)
              ((eq outcome :stop) 0)
              (t 1))))))
