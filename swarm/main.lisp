;;;; main.lisp — the evo-swarm program: arguments, supervision, bring-up.
;;;;
;;;; evo-swarm invoked plainly is its own supervisor, exactly as evo is (the
;;;; same supervisor, evo.cli:supervise): the coordinator runs as its child
;;;; and a crash restarts it with --resume — and a resumed coordinator
;;;; restores its lanes from its journal.  Its lanes watch its pid, so lanes
;;;; never outlive the coordinator that drove them.
;;;;
;;;; The coordinator has two frontends, and the command line picks one: the
;;;; TUI plainly, or `serve` — the same session, controlled over HTTP.  A VIEW
;;;; (view.lisp) is what the swarm needs either way: somewhere for its notices
;;;; to go, and the call that runs the session until it quits.  Nothing here
;;;; names a terminal or a socket beyond choosing the view.

(in-package :evo.swarm)

(defparameter *usage*
  "evo-swarm — one coordinator agent, a pool of worker lanes

Usage:
  evo-swarm                      start a swarm here: the coordinator in this
                                 terminal, lanes as `evo-agent serve` processes
  evo-swarm serve [options]      headless: the coordinator is itself a serve
                                 session, driven over HTTP
      --host <addr>              address to bind (default 127.0.0.1)
      --port <n>                 port to bind (default 8421; 0 picks a free one)
      --token-file <path>        write the bearer token here (mode 0600)
      --allow-remote             permit a non-loopback --host
  evo-swarm --workers <n>        how many lanes (default: the :swarm-workers
                                 setting, else 6)
  evo-swarm --resume [path]      resume the coordinator session (default: the
                                 last one here) and restore its lanes
  evo-swarm --model <id>         the coordinator's model (lanes default to it)
  evo-swarm --thinking <level>   low|medium|high|xhigh|max
  evo-swarm --evo <path>         the evo-agent binary lanes run (default:
                                 EVO_BINARY, then the one beside evo-swarm,
                                 then PATH)
  evo-swarm --no-userspace       no init files, extensions or swarm.lisp
  evo-swarm --no-supervisor      run the coordinator in-process
  evo-swarm --help | --version

serve takes the agent's serve flags and the swarm's own together.  Its bearer
token is minted once per launch, in the supervisor parent, so a restarted
coordinator keeps the one clients already hold.

Config: init.lisp, extensions and post-init.lisp as for evo, then
~/.evo/swarm.lisp and <cwd>/.evo/swarm.lisp: the coordinator's models and
settings for the swarm, lane count, tool limits, prompt notes, and
(evo.swarm:in-lanes ...) — code every lane evaluates.  See docs/swarm.md.")

(defun parse-args (argv)
  (let ((opts nil))
    (when (equal (first argv) "serve")
      (pop argv)
      (setf (getf opts :serve) t))
    (loop while argv
          for arg = (pop argv)
          do (cond
               ((string= arg "--workers")
                (let ((n (ignore-errors (parse-integer (or (pop argv) "")))))
                  (unless (and n (<= 1 n 64))
                    (error 'evo.cli:usage-error :text "--workers needs a number from 1 to 64"))
                  (setf (getf opts :workers) n)))
               ((string= arg "--resume")
                (setf (getf opts :resume)
                      (if (and argv (not (string-prefix-p "-" (first argv))))
                          (pop argv)
                          :latest)))
               ((string= arg "--model")
                (setf (getf opts :model) (or (pop argv) (error 'evo.cli:usage-error :text "--model needs an id"))))
               ((string= arg "--thinking")
                (let ((level (intern (string-upcase (or (pop argv) "")) :keyword)))
                  (unless (member level +effort-levels+)
                    (error 'evo.cli:usage-error :text "--thinking must be one of low|medium|high|xhigh|max"))
                  (setf (getf opts :thinking) level)))
               ((string= arg "--evo")
                (setf (getf opts :evo) (or (pop argv) (error 'evo.cli:usage-error :text "--evo needs a path"))))
               ((string= arg "--no-userspace") (setf (getf opts :no-userspace) t))
               ((string= arg "--no-supervisor") (setf (getf opts :no-supervisor) t))
               ;; serve's own flags, exactly as the agent's serve takes them
               ;; — a swarm is one session, and this is the flag set that
               ;; opens its door.  Without the subcommand they are unknown, so
               ;; a typo cannot silently start something headless.
               ((and (getf opts :serve) (string= arg "--host"))
                (setf (getf opts :host)
                      (or (pop argv) (error 'evo.cli:usage-error :text "--host needs an address"))))
               ((and (getf opts :serve) (string= arg "--port"))
                (setf (getf opts :port) (evo.cli:parse-port (pop argv))))
               ((and (getf opts :serve) (string= arg "--token-file"))
                (setf (getf opts :token-file)
                      (or (pop argv) (error 'evo.cli:usage-error :text "--token-file needs a path"))))
               ((and (getf opts :serve) (string= arg "--allow-remote"))
                (setf (getf opts :allow-remote) t))
               ((member arg '("-h" "--help") :test #'string=) (setf (getf opts :help) t))
               ((string= arg "--version") (setf (getf opts :version) t))
               (t (error 'evo.cli:usage-error
                         :text (format nil "Unknown argument: ~a (try --help)" arg)))))
    (when (getf opts :serve)
      (unless (getf opts :port)
        (setf (getf opts :port) evo.cli:*serve-default-port*)))
    opts))

(defun restart-argv (argv)
  "A restarted coordinator: its own flags minus the session ones, plus
--resume when it has a session to resume (its journal records the lanes).

serve's flags are kept — where it listens and where its token goes — because
the child that comes back must open the same door for the clients that hold
its token.  --model and --thinking are not: the journal already carries the
session's, and re-passing them would override a /model switch made since."
  (let* ((serve (equal (first argv) "serve"))
         (args (if serve (rest argv) argv)))
    (append (when serve '("serve"))
            (loop while args
                  for arg = (pop args)
                  when (member arg '("--workers" "--evo") :test #'equal)
                    append (list arg (pop args))
                  when (member arg '("--no-userspace") :test #'equal)
                    collect arg
                  when (member arg '("--resume" "--model" "--thinking") :test #'equal)
                    do (when (and args (not (string-prefix-p "-" (first args)))) (pop args)))
            ;; evo serve's own restart flags: --host, --port, --token-file,
            ;; --allow-remote.  --no-userspace is already kept above, so the
            ;; copy serve-restart-flags also returns is dropped.
            (when serve
              (remove "--no-userspace"
                      (evo.cli:serve-restart-flags (rest argv))
                      :test #'equal))
            (when (latest-session) '("--resume")))))

(defun find-evo-binary (opts)
  "The evo-agent binary lanes run.  A lane is the agent alone — `evo-agent
serve` — never this program: the `evo` beside evo-swarm is the swarm itself,
and a lane running it would only spawn swarms of its own."
  (let* ((suffix (if (evo.port:windows-p) ".exe" ""))
         (beside (merge-pathnames (format nil "evo-agent~a" suffix)
                                  (uiop:pathname-directory-pathname
                                   (evo.port:runtime-pathname))))
         (candidates (list (getf opts :evo)
                           (getenv "EVO_BINARY")
                           (and (probe-file beside) (namestring beside))
                           (let ((p (evo.port:program-in-path "evo-agent")))
                             (and p (namestring p))))))
    (or (find-if (lambda (c) (and c (probe-file c))) candidates)
        (error 'evo.cli:usage-error
               :text "cannot find the evo-agent binary lanes run: put it beside evo-swarm, on PATH, or name it with --evo / EVO_BINARY"))))

(defun check-serve-host (host opts)
  "A served coordinator binds loopback unless --allow-remote says otherwise:
eval over HTTP is remote code execution, and the token is the only gate.  The
same rule, and the same words, as the agent's serve."
  (unless (or (evo.serve:loopback-host-p host) (getf opts :allow-remote))
    (error 'evo.cli:usage-error
           :text (format nil "--host ~a is not a loopback address; pass --allow-remote to expose the swarm (eval over HTTP is remote code execution — the token is the only gate)"
                         host)))
  host)

(defun coordinator-view (opts)
  "The view the coordinator runs under: the TUI in a terminal, or serve —
the swarm's own headless view — when asked for one (:serve)."
  (if (getf opts :serve)
      (let ((host (or (getf opts :host) "127.0.0.1")))
        (evo.cli:check-serve-token opts)
        (check-serve-host host opts)
        (let ((server (evo.serve:make-server
                       :host host
                       :port (getf opts :port)
                       :token (evo.serve:resolve-token)
                       :token-file (getf opts :token-file)
                       :identity *swarm-identity*)))
          ;; The swarm's own read-only API, on the same server: GET /lanes and
          ;; a lane's transcript and events.  Never on a lane's serve.
          (register-swarm-routes)
          (make-instance 'serve-view :server server)))
      (progn
        (unless (evo.port:tty-p)
          (error 'evo.cli:usage-error
                 :text "evo-swarm's coordinator is a TUI: run it in a terminal, or `evo-swarm serve` for a headless swarm driven over HTTP"))
        (make-instance 'tui-view))))

(defun install-coordinator (view)
  "Install the swarm tools, commands and prompt note for either frontend; only
the TUI gets a status-line segment."
  (register-swarm-tools)
  (pushnew 'load-swarm-config *post-init-hooks*)
  (register-swarm-commands)
  (when (typep view 'tui-view)
    (install-tui-observation))
  (evo:register-prompt-note "swarm-coordinator"
                            (lambda (pack) (declare (ignore pack)) (coordinator-note))))

(defun run-swarm (opts)
  (let* ((view (coordinator-view opts))
         (evo-binary (find-evo-binary opts))
         (frontend (if (getf opts :serve)
                       (serve-view-server view)
                       (make-instance 'evo.tui:tui-frontend))))
    (install-coordinator view)
    (multiple-value-bind (agent resumed-p)
        (evo.cli:setup-agent opts :frontend frontend)
      (apply-coordinator-tools agent)
      (let* ((record (and resumed-p (evo:custom-state "swarm" agent)))
             (workers (or (getf opts :workers)
                          (let ((n (setting :swarm-workers))) (and (integerp n) n))
                          6)))
        (setf *swarm* (make-swarm :agent agent :workers workers
                                  :evo-binary evo-binary :record record
                                  :view view))
        (ensure-directories-exist (swarm-dir *swarm*))
        (record-swarm)
        ;; A journal switch (/new, /fork, /resume) takes the swarm along: the
        ;; new session records the lanes too.
        (evo:on :session-start (lambda (event)
                                 (declare (ignore event))
                                 (record-swarm)
                                 (apply-coordinator-tools agent :new-session t))
                :name :evo-swarm-record)
        ;; Quitting stops every lane — before the coordinator's own task
        ;; stops, as :session-end promises.
        (evo:on :session-end (lambda (event) (declare (ignore event)) (stop-swarm))
                :name :evo-swarm-stop)
        (start-lanes *swarm* :resume (and record t))
        (unwind-protect (swarm-run agent resumed-p)
          (stop-swarm))))))

(defun main (&optional (argv (evo.port:argv)))
  "Exit codes as evo's: 0 done, 1 error, 64 usage error.

A command line that does not parse is a usage error like any other, whatever
condition it raised — 64 is the code the supervisor never restarts, and a
mistyped flag cannot be fixed by trying it again."
  ;; The shared layers (supervise, boot, serve) name this program in their
  ;; messages; the swarm is evo-swarm, and its lanes say evo-agent.
  (setf evo.port:*program-name* "evo-swarm")
  (let ((opts (handler-case (parse-args argv)
                (error (e)
                  (format *error-output* "evo-swarm: ~a~%" e)
                  (return-from main 64)))))
    (handler-case
        (cond
          ((getf opts :help) (write-line *usage*) 0)
          ((getf opts :version) (write-line "evo-swarm 0.1.0") 0)
          ((evo.cli:supervised-run-p opts)
           ;; A serve token is minted once per launch, here in the parent, so a
           ;; restarted child keeps the one clients already hold.
           (when (getf opts :serve)
             (evo.cli:check-serve-token opts)
             (evo.port:setenv "EVO_SERVE_TOKEN" (evo.serve:resolve-token)))
           (evo.cli:supervise argv :restart-argv #'restart-argv))
          (t (run-swarm opts)))
      (evo.cli:usage-error (e)
        (format *error-output* "evo-swarm: ~a~%" e)
        64)
      (error (e)
        (format *error-output* "evo-swarm: ~a~%" e)
        1))))

(defun toplevel ()
  "Entry point of the built binary, as evo.cli:toplevel is evo's."
  (reseed-ids)
  (evo.port:disable-debugger)
  (evo.port:ensure-in-image-compiler)
  (evo.port:exit-lisp (main)))
