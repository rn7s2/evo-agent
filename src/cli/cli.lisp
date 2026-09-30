;;;; cli.lisp — the evo CLI: argument parsing, session bring-up, and the
;;;; entry point that composes the core with its frontends.
;;;;
;;;; Two frontends are scriptable and live here: print mode (`evo -p
;;;; "prompt"`) and event-stream mode (`--events`, line-delimited sexprs on
;;;; stdout).  The TUI is the interactive frontend.
;;;;
;;;; stdout carries assistant text (print mode) or event sexprs (event mode);
;;;; the tool/turn trace goes to stderr.

(in-package :evo.cli)

(defparameter *usage*
  "evo-agent — a goal-oriented, self-evolving agent

Usage:
  evo-agent                              interactive TUI (on a terminal)
  evo-agent -p \"prompt\"                  run one task in print mode, stream text to stdout
  evo-agent --image <path> -p ...        attach an image to the first prompt (repeatable)
  evo-agent --goal \"objective\" [-p \"first prompt\"]
                                         create a goal and run until complete or budget
  evo-agent --resume [path] [-p ...]     resume a session (default: the one most recently
                                         worked in, for this cwd);
                                         with an active goal and no -p, continues the goal
  evo-agent --events ...                 emit line-delimited sexpr events instead of text
  evo-agent --list-sessions              list sessions for this cwd, last worked in first
  evo-agent --model <id>[@<provider>]    model id (default: the :model setting from init.lisp);
                                         @provider picks one of several registrations
  evo-agent --thinking <level>           low|medium|high|xhigh|max (default medium)
  evo-agent --no-userspace               boot without init.lisp, post-init.lisp, or extensions (quarantine mode)
  evo-agent --no-supervisor              run the session in-process, no crash-restart parent
  evo-agent serve [options]              headless session controlled over HTTP (docs/serve.md)
      --host <addr>                      address to bind (default 127.0.0.1)
      --port <n>                         port to bind (default 8421; 0 picks a free one)
      --ready-file <path>                publish {port, url, token, epoch, session} here,
                                         mode 0600, atomically, rewritten after every restart
      --watch-stdin                      EOF on stdin means the driver is gone: shut down
      --as-lane                          write the session as a lane's, not a person's
      --no-http-eval                     do not offer the eval op (eval is RCE)
      --as-lane                          this serve is a swarm's lane: its
                                         journal says the program is lane
      --allow-remote                     permit a non-loopback --host
      --resume [path] --model <id> --thinking <level> --no-userspace  as above
  evo-agent catalog --json [--no-userspace]
                                         print what this session can use (models, providers,
                                         tools, commands, skills, languages) and exit
  evo-agent --help | --version

evo-agent supervises itself: crashes and hangs restart the session with
--resume, resuming the session it was on; a goal that was active picks itself
back up.  Exit codes: 0 done, 1 error, 2 goal paused, 3 budget-limited,
64 usage error.

Config: ~/.evo/init.lisp, then <cwd>/.evo/init.lisp, then extensions, then
~/.evo/post-init.lisp, then <cwd>/.evo/post-init.lisp (Lisp, evaluated in order;
later calls override).  post-init.lisp runs after extensions, so it can reference
models registered by extensions.  evo-agent ships no built-in model table, e.g.
  (evo:register-model \"claude-opus-5\"
    :provider :anthropic
    :context-window 1000000 :max-output 128000
    :effort t :thinking-mode :adaptive)
  (evo:set-setting :model \"claude-opus-5\")")

(defparameter *serve-default-port* 8421)

(defun parse-port (text)
  (let ((n (and text (ignore-errors (parse-integer text)))))
    (unless (and n (<= 0 n 65535))
      (error "--port needs a number from 0 to 65535"))
    n))

(defun parse-args (argv)
  "Parse ARGV into a plist.  Signals on unknown flags.  A leading `serve` or
`catalog` selects that subcommand (:serve / :catalog) and admits its own
flags."
  (let ((opts nil))
    (when (and argv (member (first argv) '("serve" "catalog") :test #'string=))
      (setf (getf opts (if (string= (pop argv) "serve") :serve :catalog)) t))
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
               ((string= arg "--json") (setf (getf opts :json) t))
               ((string= arg "--list-sessions") (setf (getf opts :list-sessions) t))
               ((and (getf opts :sessions) (string= arg "--json"))
                (setf (getf opts :json) t))
               ((and (getf opts :sessions) (string= arg "--all"))
                (setf (getf opts :all) t))
               ((and (getf opts :sessions) (string= arg "--rescan"))
                (setf (getf opts :rescan) t))
               ((and (getf opts :sessions) (string= arg "--cwd"))
                (setf (getf opts :cwd) (or (pop argv) (error "--cwd needs a directory"))))
               ((and (getf opts :sessions) (string= arg "--program"))
                (setf (getf opts :program) (or (pop argv) (error "--program needs a name"))))
               ((string= arg "--image")
                (setf (getf opts :images)
                      (append (getf opts :images)
                              (list (or (pop argv) (error "--image needs a path"))))))
               ((string= arg "--model")
                ;; SET-MODEL-OPT returns the plist: SETF GETF on a key that is
                ;; not there yet prepends, and a prepend cannot be seen by the
                ;; caller unless it takes the result back.
                (setf opts (set-model-opt opts (or (pop argv) (error "--model needs an id")))))
               ((string= arg "--thinking")
                (let ((level (intern (string-upcase
                                      (or (pop argv) (error "--thinking needs a level")))
                                     :keyword)))
                  (unless (member level +effort-levels+)
                    (error "--thinking must be one of low|medium|high|xhigh|max"))
                  (setf (getf opts :thinking) level)))
               ((string= arg "--no-userspace") (setf (getf opts :no-userspace) t))
               ((string= arg "--no-supervisor") (setf (getf opts :no-supervisor) t))
               ((and (getf opts :serve) (string= arg "--host"))
                (setf (getf opts :host) (or (pop argv) (error "--host needs an address"))))
               ((and (getf opts :serve) (string= arg "--port"))
                (setf (getf opts :port) (parse-port (pop argv))))
               ((and (getf opts :serve) (string= arg "--ready-file"))
                (setf (getf opts :ready-file) (or (pop argv) (error "--ready-file needs a path"))))
               ((and (getf opts :serve) (string= arg "--watch-stdin"))
                (setf (getf opts :watch-stdin) t))
               ((and (getf opts :serve) (string= arg "--as-lane"))
                ;; A lane: a serve process a swarm's coordinator owns.  Its
                ;; journal says so (CONTRACT §3), so a session list can tell a
                ;; lane from a coordinator without opening the file.
                (setf (getf opts :as-lane) t))
               ((and (getf opts :serve) (string= arg "--no-http-eval"))
                (setf (getf opts :no-http-eval) t))
               ;; A lane is an agent serving one session for a coordinator
               ;; that supervises it (CONTRACT §9.2): the session it writes
               ;; belongs to "lane", so a session list can tell a lane's
               ;; journals from a person's.
               ((and (getf opts :serve) (string= arg "--as-lane"))
                (setf (getf opts :program-name) "lane"))
               ((and (getf opts :serve) (string= arg "--allow-remote"))
                (setf (getf opts :allow-remote) t))
               ((member arg '("-h" "--help") :test #'string=) (setf (getf opts :help) t))
               ((string= arg "--version") (setf (getf opts :version) t))
               (t (error "Unknown argument: ~a (try --help)" arg))))
    (when (getf opts :sessions)
      ;; An offline CLI: it prints one JSON document and exits, so nothing
      ;; that would open a session belongs on the same command line.
      (unless (getf opts :json)
        (error "sessions needs --json — it prints the index as one document"))
      (dolist (flag '((:prompt . "-p") (:events . "--events") (:images . "--image")
                      (:serve . "serve") (:goal . "--goal")
                      (:list-sessions . "--list-sessions")))
        (when (getf opts (car flag))
          (error "~a does not combine with sessions" (cdr flag)))))
    (when (getf opts :serve)
      (unless (getf opts :port)
        (setf (getf opts :port) *serve-default-port*)))
    (when (or (getf opts :serve) (getf opts :catalog))
      ;; serve is driven over HTTP and catalog prints one document: a prompt,
      ;; an event stream on stdout or a goal on the command line would be a
      ;; second, competing driver.
      (dolist (flag '((:prompt . "-p") (:events . "--events") (:images . "--image")
                      (:goal . "--goal") (:list-sessions . "--list-sessions")))
        (when (getf opts (car flag))
          (error "~a does not combine with ~(~a~)"
                 (cdr flag) (if (getf opts :serve) "serve" "catalog")))))
    opts))

(defun set-model-opt (opts text)
  "OPTS with its model set from TEXT — ID, or ID@PROVIDER.  The provider is
only there when the id is registered under several (or when the bare id's
first registration is not the wanted one), so a plain id keeps working
unchanged.  Returns the plist: the caller must take it back, because adding a
key prepends a cons the caller's own variable does not see."
  (multiple-value-bind (id provider) (split-model-ref text)
    (setf (getf opts :model) id)
    (when provider (setf (getf opts :model-provider) provider))
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

(defun cmd-sessions (opts)
  "`evo-agent sessions --json` (CONTRACT §2): the session index as one JSON
document, newest first.  Reads ~/.evo/sessions/index.jsonl — scanning the
journals when it is missing, or when --rescan asks for it.  Never boots a
session, never opens a listener."
  (let ((sessions (session-list :rescan (getf opts :rescan)
                                :cwd (and (getf opts :cwd)
                                          (namestring (uiop:ensure-directory-pathname
                                                       (getf opts :cwd))))
                                :program (getf opts :program)
                                :all (getf opts :all))))
    (write-line (evo.journal:json-encode
                 (list :sessions (coerce sessions 'vector))))
    (force-output *standard-output*)
    0))

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
  "Open or create the session journal per OPTS.  A new session records which
program opened it (`evo-agent` or `evo-swarm`, the name this process runs
under); a resumed session that predates the field is stamped with the same, so
the session list can say whose sessions these are."
  (let ((resume (getf opts :resume))
        (program (or (getf opts :program-name)
                     (and (getf opts :as-lane) "lane")
                     evo.port:*program-name* "evo-agent")))
    (flet ((stamped (journal)
             (unless (pget (journal-header journal) :program)
               (set-session-header journal :program program))
             journal))
      (cond
        ((null resume) (make-session-journal (uiop:getcwd) :program program))
        ((eq resume :latest)
         (let ((path (latest-session)))
           ;; A usage error, not a crash: exit 64 is the code the supervisor
           ;; never restarts, and restarting cannot conjure a session.
           (unless path
             (error 'usage-error
                    :text (format nil "No sessions to resume for ~a"
                                  (namestring (uiop:getcwd)))))
           (stamped (open-journal path))))
        (t (stamped (open-journal resume)))))))

(defun main (&optional (argv (evo.port:argv)))
  "Exit codes are supervisor protocol: 0 done, 1 error (restart-eligible),
2 goal paused, 3 budget-limited, 64 usage error (never restart)."
  (setf evo.port:*program-name* "evo-agent")
  (let ((opts (handler-case (parse-args argv)
                (error (e)
                  (format *error-output* "~a: ~a~%" evo.port:*program-name* e)
                  (return-from main 64)))))
    (handler-case
        (cond
          ((getf opts :help) (write-line *usage*) 0)
          ((getf opts :version) (write-line "evo-agent 0.1.0") 0)
          ((getf opts :sessions) (cmd-sessions opts))
          ((getf opts :list-sessions) (cmd-list-sessions) 0)
          ((getf opts :catalog) (cmd-catalog opts))
          ;; One binary, two roles: the plain invocation is the
          ;; supervisor parent; it re-spawns this same binary as the child.
          ((supervised-run-p opts)
           ;; A serve token is minted once per launch, here in the parent,
           ;; so a restarted child keeps the one clients already hold.
           (when (getf opts :serve)
             (check-serve-ready opts)
             (evo.port:setenv "EVO_SERVE_TOKEN" (evo.serve:resolve-token)))
           (supervise argv))
          (t (run-cli opts)))
      (usage-error (e)
        (format *error-output* "~a: ~a~%" evo.port:*program-name* e)
        64)
      (error (e)
        (format *error-output* "~a: ~a~%" evo.port:*program-name* e)
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
  (format nil (cat "No model is configured. evo-agent ships no built-in model table: create~%"
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

(defun split-env-fields (text)
  "TEXT split on `;'."
  (loop with start = 0
        for end = (position #\; text :start start)
        collect (subseq text start end)
        while end
        do (setf start (1+ end))))

(defun parse-recovery-env (&optional (text (getenv "EVO_RECOVERY")))
  "Parse the supervisor's EVO_RECOVERY line into the plist the `:recover'
journal entry carries, or NIL when this boot was not a supervisor restart:
\"status=signaled;code=9;attempt=2;duration=640;reason=hang too long\".

The environment is the only carrier for these facts — the parent witnessed
the death, and the child that replaces the dead one has no other way to
learn how it ended (see EVO.CLI:SUPERVISE)."
  (when (and text (plusp (length text)))
    (loop for field in (split-env-fields text)
          for eq = (position #\= field)
          when eq
            append (let ((key (subseq field 0 eq))
                         (value (subseq field (1+ eq))))
                     (cond
                       ((string= key "status")
                        (cond ((string= value "signaled") '(:status :signaled))
                              ((string= value "exited") '(:status :exited))))
                       ((string= key "code")
                        (let ((n (parse-integer value :junk-allowed t)))
                          (and n (list :code n))))
                       ((string= key "attempt")
                        (let ((n (parse-integer value :junk-allowed t)))
                          (and n (list :attempt n))))
                       ((string= key "duration")
                        (let ((n (parse-integer value :junk-allowed t)))
                          (and n (list :duration n))))
                       ((string= key "reason")
                        (and (plusp (length value)) (list :reason value))))))))

(defun consume-recovery (agent resumed-p)
  "Journal a recovery the supervisor left in the environment, then clear the
variable so nothing this session spawns reads it as its own.  RESUMED-P is
whether AGENT opened an existing session: a fresh start has nothing to
recover, so the facts are dropped rather than journalled.  Returns what was
consumed, or NIL."
  (let ((recovery (parse-recovery-env)))
    (when (and resumed-p recovery)
      (evo.kernel:record-recovery agent recovery))
    (when recovery
      (evo.port:setenv "EVO_RECOVERY" ""))
    recovery))

(defun setup-agent (opts &key events-cb frontend)
  "Shared session bring-up for every frontend.  Returns (values agent resumed-p).
FRONTEND is the object answering the core's frontend protocol, bound before
anything boots so an extension deciding at load time sees it."
  (setf *frontend* frontend)
  (let* ((journal (resolve-journal opts))
         (resumed-p (journal-started-p journal))
         (agent (make-agent
                 :journal journal
                 :events-cb events-cb
                 :model-override (getf opts :model)
                 :thinking-override (getf opts :thinking))))
    (setf evo:*agent* agent)
    ;; The core locks its own packages; these two are the frontends this
    ;; binary composes it with.
    (lock-kernel-packages :evo.cli :evo.tui :evo.serve)
    ;; A supervisor restart leaves the dead child's exit facts in the
    ;; environment; journal them first, before the userspace build, whose
    ;; failure must not erase why this process is here.
    (consume-recovery agent resumed-p)
    ;; Userspace: init files (config), extension dirs, then replay the
    ;; session's :load entries.
    (boot-session agent :resumed-p resumed-p
                        :no-userspace (getf opts :no-userspace))
    ;; Journal explicit model/thinking choices so resume preserves them.
    (when (getf opts :model)
      (set-session-model agent (getf opts :model) (getf opts :model-provider)))
    (when (getf opts :thinking)
      (set-session-thinking agent (getf opts :thinking)))
    (when (getf opts :goal)
      (evo.kernel:create-goal-entry agent (getf opts :goal)))
    (values agent resumed-p)))

(defun cmd-catalog (opts)
  "Print what this session can use — the same document GET /catalog answers —
and exit.  No listener: the userspace is booted in-process, which is the
whole point (see docs/serve.md \"Starting it\")."
  (multiple-value-bind (agent resumed-p) (setup-agent opts)
    (declare (ignore resumed-p))
    (format t "~a~%" (evo.serve:encode-json (evo.serve:catalog-plist agent)))
    0))

(defun tty-p ()
  (evo.port:tty-p))

(defun run-cli (opts)
  (cond
    ((getf opts :serve) (run-serve opts))
    ((and (tty-p)
          (not (getf opts :prompt))
          (not (getf opts :events)))
     ;; Interactive: the tui core extension.
     (multiple-value-bind (agent resumed-p)
         (setup-agent opts :frontend (make-instance 'evo.tui:tui-frontend))
       (evo.tui:start-tui agent :resumed-p resumed-p)))
    (t (run-headless opts))))

(defun check-serve-ready (opts)
  "serve needs somewhere for its URL and token to reach a client: a
--ready-file to publish them to, or EVO_SERVE_TOKEN naming the token itself.
With neither, the random token would lock everybody out, so refuse up front
(exit 64)."
  (unless (or (getf opts :ready-file)
              (plusp (length (getenv "EVO_SERVE_TOKEN"))))
    (error 'usage-error
           :text "serve needs a way to hand over its URL and token: --ready-file <path> (written mode 0600, atomically), or set EVO_SERVE_TOKEN")))

(defun run-serve (opts)
  "The HTTP frontend: validate the bind, bring the session up with the server
as its frontend, then serve until POST /shutdown (or end of file on stdin)."
  (check-serve-ready opts)
  (let ((host (or (getf opts :host) "127.0.0.1")))
    (unless (or (evo.serve:loopback-host-p host) (getf opts :allow-remote))
      (error 'usage-error
             :text (format nil "--host ~a is not a loopback address; pass --allow-remote to expose the session (eval over HTTP is remote code execution — the token is the only gate)"
                           host)))
    (let ((server (evo.serve:make-server :host host
                                         :port (getf opts :port)
                                         :token (evo.serve:resolve-token)
                                         :ready-file (getf opts :ready-file)
                                         :watch-stdin (getf opts :watch-stdin))))
      (multiple-value-bind (agent resumed-p) (setup-agent opts :frontend server)
        (evo.serve:serve server agent :resumed-p resumed-p)))))

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
          (prompt (queue-steering agent prompt :images images :from-user t))
          ((and goal (eq (pget goal :status) :active))
           (queue-steering agent (evo.kernel:goal-continuation-for agent goal)
                           :origin (goal-origin goal :continue)))
          (t (error 'usage-error :text "Nothing to do headless: give -p \"prompt\", --goal, or --resume a session with an active goal"))))
      (let* ((outcome (unwind-protect (run-until-settled agent)
                        ;; The session ends here, whatever the outcome.
                        (end-session agent)))
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
