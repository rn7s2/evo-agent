;;;; main.lisp — the evo-swarm program: arguments, supervision, bring-up.
;;;;
;;;; evo-swarm invoked plainly is its own supervisor, exactly as evo is (the
;;;; same supervisor, evo.cli:supervise): the coordinator runs as its child
;;;; and a crash restarts it resuming its exact session (CONTRACT §8) — and a
;;;; resumed coordinator restores its swarm from its journal.
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
      --ready-file <path>        write the ready file (port, token, session) here
      --watch-stdin              shut down when stdin reaches EOF
      --allow-remote             permit a non-loopback --host
  evo-swarm --workers <n>        how many lanes (default: the :swarm-workers
                                 setting, else 6)
  evo-swarm --resume [path]      resume the coordinator session (default: the
                                 last one here) and restore its lanes
  evo-swarm --model <id[@provider]>        the coordinator's model
  evo-swarm --thinking <level>   low|medium|high|xhigh|max
  evo-swarm --lane-model <id[@provider]>   the lanes' model (default: the
                                 coordinator's)
  evo-swarm --lane-thinking <l>  the lanes' thinking level
  evo-swarm --evo <path>         the evo-agent binary lanes run (default:
                                 EVO_BINARY, then the one beside evo-swarm,
                                 then PATH)
  evo-swarm --no-userspace       no init files, extensions or swarm.lisp
  evo-swarm --no-supervisor      run the coordinator in-process
  evo-swarm catalog --json       what a swarm launched from here could use,
                                 without launching one
  evo-swarm check --json         whether that launch would work; exit 1 if not
  evo-swarm --help | --version

serve takes the agent's serve flags and the swarm's own together.  Its port
and token are minted once per launch, in the supervising parent, and written
to the ready file — a restarted coordinator opens the same door for the
clients that already hold them.

Config: init.lisp, extensions and post-init.lisp as for evo, then
~/.evo/swarm.lisp and <cwd>/.evo/swarm.lisp: the coordinator's models and
settings for the swarm, lane count, tool limits, prompt notes, and
(evo.swarm:in-lanes ...) — code every lane evaluates.  See docs/swarm.md.")

(defun check-think-level (text what)
  (let ((level (intern (string-upcase (or text "")) :keyword)))
    (unless (member level +effort-levels+)
      (error 'evo.cli:usage-error
             :text (format nil "~a must be one of low|medium|high|xhigh|max" what)))
    level))

(defun parse-args (argv)
  (let ((opts nil))
    ;; The subcommands.  `serve` opens a door; `catalog` and `check` answer a
    ;; question and exit — the GUI asks them before it starts anything.
    (cond ((equal (first argv) "serve")
           (pop argv)
           (setf (getf opts :serve) t))
          ((member (first argv) '("catalog" "check") :test #'equal)
           (setf (getf opts (intern (string-upcase (pop argv)) :keyword)) t)))
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
                ;; ID[@PROVIDER], split once: the provider picks between
                ;; several registrations of the same id (CONTRACT §1).
                (setf opts (evo.cli:set-model-opt
                            opts
                            (or (pop argv)
                                (error 'evo.cli:usage-error
                                       :text "--model needs an id")))))
               ((string= arg "--lane-model")
                (setf (getf opts :lane-model)
                      (or (pop argv)
                          (error 'evo.cli:usage-error :text "--lane-model needs an id"))))
               ((string= arg "--lane-thinking")
                (setf (getf opts :lane-thinking) (check-think-level (pop argv) "--lane-thinking")))
               ((string= arg "--thinking")
                (setf (getf opts :thinking) (check-think-level (pop argv) "--thinking")))
               ((string= arg "--evo")
                (setf (getf opts :evo) (or (pop argv) (error 'evo.cli:usage-error :text "--evo needs a path"))))
               ((string= arg "--no-userspace") (setf (getf opts :no-userspace) t))
               ((string= arg "--no-supervisor") (setf (getf opts :no-supervisor) t))
               ((string= arg "--json") (setf (getf opts :json) t))
               ;; serve's own flags, exactly as the agent's serve takes them
               ;; — a swarm is one session, and this is the flag set that
               ;; opens its door.  Without the subcommand they are unknown, so
               ;; a typo cannot silently start something headless.
               ((and (getf opts :serve) (string= arg "--host"))
                (setf (getf opts :host)
                      (or (pop argv) (error 'evo.cli:usage-error :text "--host needs an address"))))
               ((and (getf opts :serve) (string= arg "--port"))
                (setf (getf opts :port) (evo.cli:parse-port (pop argv))))
               ((and (getf opts :serve) (string= arg "--ready-file"))
                (setf (getf opts :ready-file)
                      (or (pop argv) (error 'evo.cli:usage-error :text "--ready-file needs a path"))))
               ((and (getf opts :serve) (string= arg "--watch-stdin"))
                (setf (getf opts :watch-stdin) t))
               ;; catalog/check always answer JSON; --json says so out loud,
               ;; as the agent's catalog does.
               ((and (or (getf opts :catalog) (getf opts :check)) (string= arg "--json"))
                (setf (getf opts :json) t))
               ((and (getf opts :serve) (string= arg "--allow-remote"))
                (setf (getf opts :allow-remote) t))
               ((member arg '("-h" "--help") :test #'string=) (setf (getf opts :help) t))
               ((string= arg "--version") (setf (getf opts :version) t))
               (t (error 'evo.cli:usage-error
                         :text (format nil "Unknown argument: ~a (try --help)" arg)))))
    (when (getf opts :serve)
      (unless (getf opts :port)
        (setf (getf opts :port) evo.cli:*serve-default-port*)))
    (when (getf opts :lane-model)
      ;; ID[@PROVIDER], split once here: the record keeps both halves, and
      ;; CONTRACT §4.3's config wants them as an object.
      (multiple-value-bind (id provider) (split-model-ref (getf opts :lane-model))
        (setf (getf opts :lane-model) id)
        (when provider (setf (getf opts :lane-provider) provider))))
    opts))

(defun restart-argv (argv)
  "A restarted coordinator: the flags that describe the swarm and the server,
plus --resume naming the exact session it was on (CONTRACT §8) and the port it
bound.

serve's flags are kept — where it listens and where its ready file goes —
because the child that comes back must open the same door for the clients
that hold its URL and token.  --model and --thinking are not: the journal
already carries the session's, and re-passing them would override a /model
switch made since.  --lane-model and --lane-thinking are kept: they describe
the swarm, not the session, and an unchanged replay only re-records them."
  (let* ((serve (equal (first argv) "serve"))
         (args (if serve (rest argv) argv))
         (kept (loop while args
                     for arg = (pop args)
                     when (member arg '("--workers" "--evo" "--lane-model" "--lane-thinking")
                                 :test #'equal)
                       append (if args (list arg (pop args)) (list arg))
                     when (equal arg "--no-userspace")
                       collect arg
                     when (member arg '("--resume" "--model" "--thinking") :test #'equal)
                       do (when (and args (not (string-prefix-p "-" (first args))))
                            (pop args)))))
    (append (when serve '("serve"))
            kept
            (when serve
              ;; --host, --port (the one it bound), --ready-file,
              ;; --allow-remote, --watch-stdin.  --no-userspace is already
              ;; kept above, so the copy the shared layer also returns is
              ;; dropped.
              (loop for fragment in (evo.cli:pin-bound-port
                                     (evo.cli:serve-restart-flags (rest argv)))
                    unless (equal fragment "--no-userspace")
                      collect fragment))
            ;; The exact session path the child reported; never a bare
            ;; --resume (which means "newest in this folder").
            (evo.cli:exact-session-args))))

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
        (check-serve-host host opts)
        (let ((server (evo.serve:make-server
                       :host host
                       :port (getf opts :port)
                       :ready-file (getf opts :ready-file)
                       :watch-stdin (getf opts :watch-stdin)
                       :identity *swarm-identity*)))
          (make-instance 'serve-view :server server)))
      (progn
        (unless (evo.port:tty-p)
          (error 'evo.cli:usage-error
                 :text "evo-swarm's coordinator is a TUI: run it in a terminal, or `evo-swarm serve` for a headless swarm driven over HTTP"))
        (make-instance 'tui-view))))

(defun coordinator-busy-changed (agent busy)
  "The coordinator started or finished a task (:busy / :idle): `waiting` turns
on and off, so every client's swarm status stays true (§4.3)."
  (let ((swarm (and *swarm* (eq agent (swarm-agent *swarm*)) *swarm*)))
    (when swarm
      (bt:with-lock-held ((swarm-lock swarm))
        (setf (swarm-coordinator-busy swarm) busy))
      (publish-swarm-state swarm))))

(defun install-coordinator (view)
  "Install the swarm tools, commands, holds and prompt note for either
frontend; only the TUI gets a status-line segment."
  (register-swarm-tools)
  (register-swarm-holds)
  (pushnew 'load-swarm-config *post-init-hooks*)
  (register-swarm-commands)
  (evo:on :busy (lambda (event) (coordinator-busy-changed (getf event :agent) t))
          :name :evo-swarm-busy)
  (evo:on :idle (lambda (event) (coordinator-busy-changed (getf event :agent) nil))
          :name :evo-swarm-idle)
  (when (typep view 'tui-view)
    (install-tui-observation))
  (evo:register-prompt-note "swarm-coordinator"
                            (lambda (pack) (declare (ignore pack)) (coordinator-note))))

(defun run-swarm (opts)
  (let* ((view (coordinator-view opts))
         (evo-binary (find-evo-binary opts))
         (server (and (typep view 'serve-view) (serve-view-server view)))
         (frontend (if server server (make-instance 'evo.tui:tui-frontend))))
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
                                  :view view :server server))
        ;; The flags override what a resumed record restored, and are
        ;; re-recorded: lane configuration is swarm data, not a user file.
        (when (getf opts :lane-model)
          (setf (swarm-lane-model *swarm*) (getf opts :lane-model)
                (swarm-lane-provider *swarm*) (getf opts :lane-provider)))
        (when (getf opts :lane-thinking)
          (setf (swarm-lane-thinking *swarm*) (getf opts :lane-thinking)))
        (ensure-directories-exist (swarm-dir *swarm*))
        (record-swarm)
        (when server (register-swarm-topics server *swarm*))
        ;; A journal switch (/new, /fork, /resume): a resumed session gets
        ;; its own recorded swarm back; any other takes the running one along.
        (evo:on :session-start (lambda (event)
                                 (declare (ignore event))
                                 (adopt-session-swarm agent)
                                 (apply-coordinator-tools agent :new-session t))
                :name :evo-swarm-record)
        ;; Quitting stops every lane — before the coordinator's own task
        ;; stops, as :session-end promises.
        (evo:on :session-end (lambda (event) (declare (ignore event)) (stop-swarm))
                :name :evo-swarm-stop)
        (publish-swarm-state *swarm* t)
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
          ((getf opts :catalog) (cmd-catalog opts))
          ((getf opts :check) (cmd-check opts))
          ((evo.cli:supervised-run-p opts)
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
