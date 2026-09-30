;;;; supervisor.lisp — the supervisor, in-binary.
;;;;
;;;; One binary total: invoked normally, evo IS the tiny supervisor parent —
;;;; it re-spawns itself (evo.port:runtime-pathname) as the supervised child
;;;; with EVO_SUPERVISED_CHILD=1 and inherited stdio (the TTY passes
;;;; straight through), then monitors process exit and the heartbeat file,
;;;; restarts with --resume on crashes and hangs, and quarantines repeated
;;;; boot failures with --no-userspace.
;;;;
;;;; Exit-code protocol (child -> parent):
;;;;   0 done · 1 error (restart-eligible) · 2 goal paused ·
;;;;   3 budget-limited · 64 usage error — 0/2/3/64 stop, everything else
;;;;   (including signals) restarts.

(in-package :evo.cli)

(defun supervisor-setting (env-var default)
  (let ((value (getenv env-var)))
    (or (and value (parse-integer value :junk-allowed t)) default)))

(defun heartbeat-age (path start-time)
  "Seconds since the child last touched PATH (or since START-TIME if never)."
  (- (get-universal-time)
     (or (ignore-errors (file-write-date path)) start-time)))

(defparameter *serve-restart-flags* '("--host" "--port" "--ready-file")
  "serve flags a restarted child keeps, each with its value.")

(defparameter *serve-restart-switches*
  '("--allow-remote" "--no-userspace" "--watch-stdin" "--no-http-eval"
    "--as-lane")
  "serve flags a restarted child keeps, each with no value.")

(defun serve-restart-flags (argv)
  "The serve flags in ARGV that describe the server rather than the session:
where it listens, where its ready file goes, and whether it follows stdin.
--model, --thinking and --resume are session state, which the journal already
carries — re-passing --model would re-journal the launch model over a /model
switch made since."
  (loop while argv
        for arg = (pop argv)
        when (member arg *serve-restart-flags* :test #'equal)
          append (list arg (pop argv))
        when (member arg *serve-restart-switches* :test #'equal)
          collect arg))

(defun pin-bound-port (args)
  "ARGS with --port's value replaced by the port the child actually bound —
the one it reported to the supervisor's state directory.  `--port 0` is a
request, not a port: a restart that passed it on would come back on a port no
client knows.  ARGS is returned unchanged when the child never reported one."
  (let ((bound (supervisor-bound-port)))
    (if (null bound)
        args
        (loop with out = nil
              with argv = (copy-list args)
              while argv
              for arg = (pop argv)
              do (if (equal arg "--port")
                     (progn (when argv (pop argv))
                            (setf out (append out (list arg (princ-to-string bound)))))
                     (setf out (append out (list arg))))
              finally (return out)))))

(defun exact-session-args ()
  "The arguments that make a restart resume *this* session: `--resume <the
path the child reported>`.  Empty when it reported none, or reported one it
never wrote to disk — a child that died before journalling has nothing to
resume, and passing --resume would only turn one failure into \"No sessions
to resume\", once per attempt.

This is the shared half of every restart path (evo-agent's RESTART-ARGV and
evo-swarm's override of it): a bare `--resume` means \"the newest session in
this directory\", which is a different session the moment two evos — or two
swarm tabs — share a folder."
  (let ((path (supervisor-current-session)))
    (when path (list "--resume" path))))

(defun restart-argv (argv)
  "Arguments for a restarted child.

The session it resumes is the one it was on (EXACT-SESSION-ARGS) and the port
it bound is the one it gets back (PIN-BOUND-PORT): `--port 0` is a request,
not a port."
  (append (when (equal (first argv) "serve")
            (cons "serve" (pin-bound-port (serve-restart-flags (rest argv)))))
          (exact-session-args)
          (when (member "--events" argv :test #'equal) '("--events"))))

(defun ready-file-path (argv)
  "The --ready-file PATH in ARGV, or NIL."
  (loop for (arg value) on argv
        when (equal arg "--ready-file") return value))

(defun supervisor-state-directory* ()
  "This supervisor's own state directory: one per launch, created if need be,
named in the environment for the child.  A supervisor started by another
supervisor would inherit one; it makes its own, so `current-session` always
belongs to the child it is about to spawn."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "evo-supervisor-~a/" (gen-id))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist (merge-pathnames "x" dir))
    (evo.port:setenv "EVO_SUPERVISOR_STATE_DIR" (namestring dir))
    dir))

(defun spawn-child (args heartbeat-file &optional recovery restarts)
  (evo.port:launch-child
   (namestring (evo.port:runtime-pathname)) args
   :environment (append (list "EVO_SUPERVISED_CHILD=1"
                              (format nil "EVO_HEARTBEAT_FILE=~a" heartbeat-file)
                              (format nil "EVO_SUPERVISOR_PID=~d" (evo.port:getpid))
                              (format nil "EVO_SUPERVISOR_RESTARTS=~d" (or restarts 0)))
                        (when recovery
                          (list (format nil "EVO_RECOVERY=~a" recovery)))
                        (remove-if (lambda (e)
                                     (or (string-prefix-p "EVO_SUPERVISED_CHILD=" e)
                                         (string-prefix-p "EVO_HEARTBEAT_FILE=" e)
                                         (string-prefix-p "EVO_RECOVERY=" e)
                                         (string-prefix-p "EVO_SUPERVISOR_PID=" e)
                                         (string-prefix-p "EVO_SUPERVISOR_RESTARTS=" e)))
                                   (evo.port:environ)))))

(defun reset-tty ()
  "Best-effort cooked-mode restore.  A child that dies abnormally can die
inside the TUI's raw mode; without this the next child's terminal snapshot
captures raw as the state to \"restore\", and the user's shell inherits a
raw terminal when the supervisor finally exits."
  (ignore-errors (evo.port:terminal-sane)))

(defun monitor-child (process heartbeat-file start-time hang-timeout)
  "Poll until PROCESS exits; kill -9 on a stale heartbeat.
Returns (values STATUS CODE HUNG-P): STATUS is :exited or :signaled, CODE the
exit code or the signal number, HUNG-P true when the kill was the watchdog's."
  (let ((hung nil))
    (loop while (evo.port:process-alive-p process)
          do (sleep 2)
             ;; Only judge staleness once the child has had time to boot.
             (when (and (> (- (get-universal-time) start-time) 30)
                        (> (heartbeat-age heartbeat-file start-time) hang-timeout))
               (format *error-output* "~&~a: heartbeat stale — killing hung child~%"
                       evo.port:*program-name*)
               (setf hung t)
               (ignore-errors (evo.port:process-kill process))))
    (multiple-value-bind (status code) (evo.port:process-wait process)
      (values status code hung))))

(defun recovery-env-string (status code attempt duration reason)
  "The supervisor's account of the child that just died, for the child that
replaces it — one environment value, e.g.
`status=signaled;code=9;attempt=2;duration=640;reason=hang too long'.
REASON is the supervisor's own words or NIL when it has none."
  (format nil "status=~(~a~);code=~a;attempt=~d;duration=~d~@[;reason=~a~]"
          status code attempt duration reason))

(defun supervise (argv &key (restart-argv #'restart-argv))
  "The supervisor loop.  Returns the final exit code.  RESTART-ARGV maps the
original ARGV to a restarted child's arguments; another program built on this
supervisor (evo-swarm) passes its own.

The supervisor's state directory outlives every child, so the child of the
moment can report what only it knows — which session it is on, and the port it
bound — and the restart can name both exactly."
  (let ((hang-timeout (supervisor-setting "EVO_HANG_TIMEOUT" 600))
        (boot-grace (supervisor-setting "EVO_BOOT_GRACE" 20))
        (max-boot-failures (supervisor-setting "EVO_MAX_BOOT_FAILURES" 3))
        (max-restarts (supervisor-setting "EVO_SUPERVISOR_MAX_RESTARTS" 50))
        (restarts 0) (boot-failures 0) (quarantined nil) (extra nil)
        (recovery nil)                  ; how the child before this one died
        (first t))
    (supervisor-state-directory*)
    (unwind-protect
    (loop
      (let* ((heartbeat (merge-pathnames
                         (format nil "evo-heartbeat-~a" (gen-id))
                         (uiop:temporary-directory)))
             (start (get-universal-time))
             (args (append (if first argv (funcall restart-argv argv)) extra)))
        (unless first
          ;; Name the actual arguments: "restarting with --resume" while
          ;; quietly restarting without it is how a supervisor lies.
          (format *error-output* "~&~a: restarting~{ ~a~} (attempt ~d)~%"
                  evo.port:*program-name* args restarts))
        (multiple-value-bind (status code hung)
            (monitor-child (spawn-child args heartbeat recovery restarts)
                           heartbeat start hang-timeout)
          (ignore-errors (delete-file heartbeat))
          (let ((outcome (if (eq status :signaled) :crashed code)))
            ;; Clean exits (0/2/3/64) ran their own terminal teardown; the
            ;; abnormal ones may have died in raw mode.
            (when (or hung (not (member outcome '(0 2 3 64))))
              (reset-tty))
            (let ((duration (- (get-universal-time) start)))
              (setf first nil)
              (case outcome
                (0 (return 0))
                (2 (format *error-output* "~&~a: goal paused — human needed~%"
                           evo.port:*program-name*)
                   (return 2))
                (3 (format *error-output* "~&~a: goal budget-limited — human needed~%"
                           evo.port:*program-name*)
                   (return 3))
                (64 (return 64)))       ; usage error: restarting won't help
              (when hung
                (format *error-output* "~&~a: child was hung (killed)~%"
                        evo.port:*program-name*))
              ;; Boot-failure quarantine.
              (if (< duration boot-grace)
                  (incf boot-failures)
                  (setf boot-failures 0))
              (when (>= boot-failures max-boot-failures)
                (when quarantined
                  (format *error-output* "~&~a: quarantined boot is failing too — giving up. Fix or remove the offending source file (see ':load replay' lines above).~%"
                          evo.port:*program-name*)
                  (return 1))
                (format *error-output* "~&~a: boot failed ~d times fast — QUARANTINE: retrying with --no-userspace~%"
                        evo.port:*program-name*
                        boot-failures)
                (setf extra '("--no-userspace") quarantined t boot-failures 0))
              (incf restarts)
              (when (>= restarts max-restarts)
                (format *error-output* "~&~a: restart budget exhausted~%"
                        evo.port:*program-name*)
                (return 1))
              ;; Hand the next child what only this process knows: how the
              ;; one it replaces ended.  It journals that; no child can
              ;; observe its predecessor's death otherwise.
              (setf recovery (recovery-env-string status code restarts duration
                                                  (and hung "hang too long")))
              (sleep 2)))))
      ;; Nothing is left to restart: forget what the dead children reported,
      ;; and take the ready file with it — a client watching that path must
      ;; not keep trusting a pid that will never come back.
      (ignore-errors (evo.kernel:delete-supervisor-state))
      (let ((ready (ready-file-path argv)))
        (when ready (ignore-errors (delete-file ready))))))))

(defun supervised-run-p (opts)
  "Should this invocation run the supervisor parent instead of a session?"
  (not (or (getenv "EVO_SUPERVISED_CHILD")
           (getenv "EVO_NO_SUPERVISOR")
           (getf opts :no-supervisor)
           (getf opts :catalog)
           (getf opts :check)
           (getf opts :help) (getf opts :version) (getf opts :list-sessions))))
