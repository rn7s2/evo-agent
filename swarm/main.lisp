;;;; main.lisp — the evo-swarm program: arguments, supervision, bring-up.
;;;;
;;;; evo-swarm invoked plainly is its own supervisor, exactly as evo is (the
;;;; same supervisor, evo.cli:supervise): the coordinator runs as its child
;;;; and a crash restarts it with --resume — and a resumed coordinator
;;;; restores its lanes from its journal.  Its lanes watch its pid, so lanes
;;;; never outlive the coordinator that drove them.

(in-package :evo.swarm)

(defparameter *usage*
  "evo-swarm — one coordinator agent, a pool of worker lanes

Usage:
  evo-swarm                      start a swarm here: the coordinator in this
                                 terminal, lanes as `evo serve` processes
  evo-swarm --workers <n>        how many lanes (default: the :swarm-workers
                                 setting, else 6)
  evo-swarm --resume [path]      resume the coordinator session (default: the
                                 last one here) and restore its lanes
  evo-swarm --model <id>         the coordinator's model (lanes default to it)
  evo-swarm --thinking <level>   low|medium|high|xhigh|max
  evo-swarm --evo <path>         the evo binary lanes run (default: EVO_BINARY,
                                 then the one beside evo-swarm, then PATH)
  evo-swarm --no-userspace       no init files, extensions or swarm.lisp
  evo-swarm --no-supervisor      run the coordinator in-process
  evo-swarm --help | --version

Config: init.lisp, extensions and post-init.lisp as for evo, then
~/.evo/swarm.lisp and <cwd>/.evo/swarm.lisp: the coordinator's models and
settings for the swarm, lane count, tool limits, prompt notes, and
(evo.swarm:in-lanes ...) — code every lane evaluates.  See docs/swarm.md.")

(defun parse-args (argv)
  (let ((opts nil))
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
               ((member arg '("-h" "--help") :test #'string=) (setf (getf opts :help) t))
               ((string= arg "--version") (setf (getf opts :version) t))
               (t (error 'evo.cli:usage-error
                         :text (format nil "Unknown argument: ~a (try --help)" arg)))))
    opts))

(defun restart-argv (argv)
  "A restarted coordinator: its own flags minus the session ones, plus
--resume when it has a session to resume (its journal records the lanes)."
  (append (loop while argv
                for arg = (pop argv)
                when (member arg '("--workers" "--evo") :test #'equal)
                  append (list arg (pop argv))
                when (member arg '("--no-userspace") :test #'equal)
                  collect arg
                when (member arg '("--resume" "--model" "--thinking") :test #'equal)
                  do (when (and argv (not (string-prefix-p "-" (first argv)))) (pop argv)))
          (when (latest-session) '("--resume"))))

(defun find-evo-binary (opts)
  "The evo binary lanes run."
  (let* ((suffix (if (evo.port:windows-p) ".exe" ""))
         (beside (merge-pathnames (format nil "evo~a" suffix)
                                  (uiop:pathname-directory-pathname
                                   (evo.port:runtime-pathname))))
         (candidates (list (getf opts :evo)
                           (getenv "EVO_BINARY")
                           (and (probe-file beside) (namestring beside))
                           (let ((p (evo.port:program-in-path "evo")))
                             (and p (namestring p))))))
    (or (find-if (lambda (c) (and c (probe-file c))) candidates)
        (error 'evo.cli:usage-error
               :text "cannot find the evo binary lanes run: put it beside evo-swarm, on PATH, or name it with --evo / EVO_BINARY"))))

(defun install-coordinator ()
  "What the coordinator has beyond an evo session: the swarm tools, the
lane commands and status segment, the coordinator note.  Installed before the
session boots, so /reload keeps them (they are the base, not an extension)."
  (register-swarm-tools)
  (pushnew 'load-swarm-config *post-init-hooks*)
  (install-tui-observation)
  (evo:register-prompt-note "swarm-coordinator"
                            (lambda (pack) (declare (ignore pack)) (coordinator-note))))

(defun run-swarm (opts)
  (unless (evo.port:tty-p)
    (error 'evo.cli:usage-error
           :text "evo-swarm needs a terminal: the coordinator is a TUI (lanes are driven over HTTP; for a headless single agent use `evo serve`)"))
  (let ((evo-binary (find-evo-binary opts)))
    (install-coordinator)
    (multiple-value-bind (agent resumed-p)
        (evo.cli:setup-agent opts :frontend (make-instance 'evo.tui:tui-frontend))
      (apply-coordinator-tools agent)
      (let* ((record (and resumed-p (evo:custom-state "swarm" agent)))
             (workers (or (getf opts :workers)
                          (let ((n (setting :swarm-workers))) (and (integerp n) n))
                          6)))
        (setf *swarm* (make-swarm :agent agent :workers workers
                                  :evo-binary evo-binary :record record))
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
        (unwind-protect (evo.tui:start-tui agent :resumed-p resumed-p)
          (stop-swarm))))))

(defun main (&optional (argv (evo.port:argv)))
  "Exit codes as evo's: 0 done, 1 error, 64 usage error."
  (handler-case
      (let ((opts (parse-args argv)))
        (cond
          ((getf opts :help) (write-line *usage*) 0)
          ((getf opts :version) (write-line "evo-swarm 0.1.0") 0)
          ((evo.cli:supervised-run-p opts)
           (evo.cli:supervise argv :restart-argv #'restart-argv))
          (t (run-swarm opts))))
    (evo.cli:usage-error (e)
      (format *error-output* "evo-swarm: ~a~%" e)
      64)
    (error (e)
      (format *error-output* "evo-swarm: ~a~%" e)
      1)))

(defun toplevel ()
  "Entry point of the built binary, as evo.cli:toplevel is evo's."
  (reseed-ids)
  (evo.port:disable-debugger)
  (evo.port:ensure-in-image-compiler)
  (evo.port:exit-lisp (main)))
