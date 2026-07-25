;;;; supervisor.lisp — the supervisor, in-binary.
;;;;
;;;; One binary total: invoked normally, evo IS the tiny supervisor parent —
;;;; it re-spawns itself (sb-ext:*runtime-pathname*) as the supervised child
;;;; with EVO_SUPERVISED_CHILD=1 and inherited stdio (the TTY passes
;;;; straight through), then monitors process exit and the heartbeat file,
;;;; restarts with --resume on crashes and hangs, and quarantines repeated
;;;; boot failures with --no-userspace.
;;;;
;;;; Exit-code protocol (child -> parent):
;;;;   0 done · 1 error (restart-eligible) · 2 goal blocked ·
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

(defun spawn-child (args heartbeat-file)
  (sb-ext:run-program
   (namestring sb-ext:*runtime-pathname*) args
   :input t :output t :error t :wait nil
   :environment (list* "EVO_SUPERVISED_CHILD=1"
                       (format nil "EVO_HEARTBEAT_FILE=~a" heartbeat-file)
                       (remove-if (lambda (e)
                                    (or (string-prefix-p "EVO_SUPERVISED_CHILD=" e)
                                        (string-prefix-p "EVO_HEARTBEAT_FILE=" e)))
                                  (sb-ext:posix-environ)))))

(defun monitor-child (process heartbeat-file start-time hang-timeout)
  "Poll until PROCESS exits; kill -9 on a stale heartbeat.
Returns (values exit-code hung-p)."
  (let ((hung nil))
    (loop while (sb-ext:process-alive-p process)
          do (sleep 2)
             ;; Only judge staleness once the child has had time to boot.
             (when (and (> (- (get-universal-time) start-time) 30)
                        (> (heartbeat-age heartbeat-file start-time) hang-timeout))
               (format *error-output* "~&evo: heartbeat stale — killing hung child~%")
               (setf hung t)
               (ignore-errors (sb-ext:process-kill process sb-unix:sigkill))))
    (sb-ext:process-wait process)
    (values (if (eq (sb-ext:process-status process) :signaled)
                :crashed
                (sb-ext:process-exit-code process))
            hung)))

(defun supervise (argv)
  "The supervisor loop.  Returns the final exit code."
  (let ((hang-timeout (supervisor-setting "EVO_HANG_TIMEOUT" 600))
        (boot-grace (supervisor-setting "EVO_BOOT_GRACE" 20))
        (max-boot-failures (supervisor-setting "EVO_MAX_BOOT_FAILURES" 3))
        (max-restarts (supervisor-setting "EVO_SUPERVISOR_MAX_RESTARTS" 50))
        (restart-args (append '("--resume")
                              (when (member "--events" argv :test #'equal)
                                '("--events"))))
        (restarts 0) (boot-failures 0) (quarantined nil) (extra nil)
        (first t))
    (loop
      (let* ((heartbeat (merge-pathnames
                         (format nil "evo-heartbeat-~a" (gen-id))
                         (uiop:temporary-directory)))
             (start (get-universal-time))
             (args (append (if first argv restart-args) extra)))
        (unless first
          (format *error-output* "~&evo: restarting with --resume (attempt ~d)~%"
                  restarts))
        (multiple-value-bind (code hung)
            (monitor-child (spawn-child args heartbeat) heartbeat start hang-timeout)
          (ignore-errors (delete-file heartbeat))
          (let ((duration (- (get-universal-time) start)))
            (setf first nil)
            (case code
              (0 (return 0))
              (2 (format *error-output* "~&evo: goal blocked by the model — human needed~%")
                 (return 2))
              (3 (format *error-output* "~&evo: goal budget-limited — human needed~%")
                 (return 3))
              (64 (return 64)))         ; usage error: restarting won't help
            (when hung
              (format *error-output* "~&evo: child was hung (killed)~%"))
            ;; Boot-failure quarantine.
            (if (< duration boot-grace)
                (incf boot-failures)
                (setf boot-failures 0))
            (when (>= boot-failures max-boot-failures)
              (when quarantined
                (format *error-output* "~&evo: quarantined boot is failing too — giving up. Fix or remove the offending source file (see ':load replay' lines above).~%")
                (return 1))
              (format *error-output* "~&evo: boot failed ~d times fast — QUARANTINE: retrying with --no-userspace~%"
                      boot-failures)
              (setf extra '("--no-userspace") quarantined t boot-failures 0))
            (incf restarts)
            (when (>= restarts max-restarts)
              (format *error-output* "~&evo: restart budget exhausted~%")
              (return 1))
            (sleep 2)))))))

(defun supervised-run-p (opts)
  "Should this invocation run the supervisor parent instead of a session?"
  (not (or (getenv "EVO_SUPERVISED_CHILD")
           (getenv "EVO_NO_SUPERVISOR")
           (getf opts :no-supervisor)
           (getf opts :help) (getf opts :version) (getf opts :list-sessions))))
