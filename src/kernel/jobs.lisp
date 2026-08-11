;;;; jobs.lisp — background jobs: detach-on-ceiling + the `wait` tool.
;;;;
;;;; A bash command that passes its timeout is not killed: it keeps running,
;;;; is entered here as a numbered job, and bash returns a handle instead of
;;;; blocking.  `wait` then blocks up to its own ceiling and returns the
;;;; instant the job finishes — so the model never has to GUESS a duration and
;;;; sleep for it.  Jobs are in-memory only and are killed when evo exits
;;;; (EVO.PORT exit hook), so none ever outlives its evo: there is nothing to
;;;; reap across a restart and nothing to journal.

(in-package :evo.kernel)

;;; Shared with bash (builtin-tools.lisp loads after this file): the timeout is
;;; now a YIELD ceiling — the most seconds to block before the command goes to
;;; the background — not a kill deadline.
(defparameter *bash-default-timeout* 120)
(defparameter *bash-max-timeout* 600)

(defparameter *job-output-budget* 40000
  "Most characters of a job's fresh output `wait` returns in one call; a longer
slice is tail-truncated (the end of a log is the useful part).")

(defstruct job
  id            ; small integer, 1..N per evo process
  command       ; the shell command string, for display
  pid           ; raw pid captured at detach — the reaper kills by this, never
                ; touching the process handle (safe off the worker thread)
  process       ; the port process handle — waited/polled only on the tool path
  out-file      ; combined stdout/stderr, still being written
  script        ; scratch shell script to delete at retire, or NIL
  (chars-read 0); how much of OUT-FILE `wait` has already returned
  start-time    ; universal time at detach, for the status-line elapsed clock
  (status :running))

(defvar *jobs* nil
  "Alist (id . job) of live background jobs, newest first.  Guarded by *JOBS-LOCK*.")
(defvar *jobs-lock* (bt:make-lock "evo-jobs"))
(defvar *job-counter* 0)

(defun register-job (&key command pid process out-file script)
  "Enter a detached process as a job and return its id."
  (bt:with-lock-held (*jobs-lock*)
    (let* ((id (incf *job-counter*))
           (job (make-job :id id :command command :pid pid :process process
                          :out-file out-file :script script
                          :start-time (get-universal-time))))
      (push (cons id job) *jobs*)
      id)))

(defun find-job (id)
  (bt:with-lock-held (*jobs-lock*)
    (cdr (assoc id *jobs*))))

(defun retire-job (id)
  "Drop job ID from the registry and delete its scratch files.  The process is
assumed already dead (finished or killed by the caller)."
  (let ((job (bt:with-lock-held (*jobs-lock*)
               (prog1 (cdr (assoc id *jobs*))
                 (setf *jobs* (remove id *jobs* :key #'car))))))
    (when job
      (ignore-errors (when (job-out-file job) (delete-file (job-out-file job))))
      (ignore-errors (when (job-script job) (delete-file (job-script job)))))
    job))

(defun job-new-output (job)
  "The part of JOB's output not yet returned, tail-truncated to the budget.
Advances the read cursor to end-of-file."
  (let* ((all (normalize-newlines
               (or (ignore-errors (read-file-string (job-out-file job))) "")))
         (start (min (job-chars-read job) (length all)))
         (fresh (subseq all start)))
    (setf (job-chars-read job) (length all))
    (if (> (length fresh) *job-output-budget*)
        (format nil "[earlier output truncated]~%~a"
                (subseq fresh (- (length fresh) *job-output-budget*)))
        fresh)))

(defun detach-as-job (&key command process out-file script)
  "Register a still-running PROCESS as a background job.  Returns its id.
Called by bash when a command reaches its yield ceiling."
  (register-job :command command
                :pid (ignore-errors (evo.port:process-pid process))
                :process process :out-file out-file :script script))

(defun short-command (command &optional (limit 40))
  "A one-line, length-capped rendering of COMMAND for notices and the status line."
  (let* ((line (or (first (uiop:split-string (or command "")
                                             :separator '(#\Newline)))
                   ""))
         (trimmed (string-trim '(#\Space #\Tab) line)))
    (cond ((zerop (length trimmed)) nil)  ; NIL so ~@[ elides the ": " entirely
          ((> (length trimmed) limit)
           (concatenate 'string (subseq trimmed 0 (1- limit)) "…"))
          (t trimmed))))

(defun still-running-note (id command new-output)
  "The message bash / wait return while a job keeps running."
  (format nil
          (cat "Still running in the background as job ~d~@[: ~a~].~%"
               "Use `wait` with job_id ~d to collect more output, keep waiting, "
               "or kill it.  Background jobs are killed when evo exits.~%"
               "--- output so far ---~%~a")
          id (short-command command) id
          (if (plusp (length new-output)) new-output "(no output yet)")))

;;; The exit hook: kill every live job by pid.  Pure kill(2)/pgrep syscalls
;;; (no process handle touched), so it is safe on the shutdown thread.
(defun reap-all-jobs ()
  (dolist (entry (bt:with-lock-held (*jobs-lock*) (copy-list *jobs*)))
    (let ((job (cdr entry)))
      (ignore-errors (evo.port:reap-pid-tree (job-pid job)))
      (ignore-errors (when (job-out-file job) (delete-file (job-out-file job))))
      (ignore-errors (when (job-script job) (delete-file (job-script job))))))
  (bt:with-lock-held (*jobs-lock*) (setf *jobs* nil)))

(defun running-jobs-summary ()
  "For the TUI status segment.  NIL when no jobs run, else a plist
(:count N :command STRING :since UNIVERSAL-TIME) describing the oldest job."
  (bt:with-lock-held (*jobs-lock*)
    (when *jobs*
      (let ((oldest (cdr (first (last *jobs*)))))  ; *jobs* is newest-first
        (list :count (length *jobs*)
              :command (job-command oldest)
              :since (job-start-time oldest))))))

;;; The `wait` tool.

(defun tool-wait (args)
  (let* ((id (pget args :job-id))
         (timeout (min *bash-max-timeout*
                       (max 0 (or (pget args :timeout) *bash-default-timeout*))))
         (kill (pget args :kill))
         (job (and (integerp id) (find-job id)))
         (agent *executing-agent*))
    (unless (integerp id)
      (error "wait needs a job_id (the integer bash returned when a command went to the background)"))
    (unless job
      (error "no such job ~a — it either finished already or did not survive an evo restart" id))
    (when kill
      (evo.port:reap-pid-tree (job-pid job))
      (ignore-errors (evo.port:process-wait (job-process job)))
      (let ((out (job-new-output job)))
        (retire-job id)
        (return-from tool-wait
          (values (format nil "Job ~d killed.~@[~%~a~]"
                          id (and (plusp (length out)) out))
                  (list :job-id id :killed t)))))
    ;; Poll until the job exits, the ceiling elapses, or the user aborts.
    (loop with deadline = (+ (get-internal-real-time)
                             (* timeout internal-time-units-per-second))
          while (evo.port:process-alive-p (job-process job))
          do (when (and agent (agent-abort-flag agent))
               ;; Abort ends the turn but does NOT kill a backgrounded job.
               (error "Wait interrupted; job ~d is still running in the background." id))
             (when (> (get-internal-real-time) deadline)
               (return-from tool-wait
                 (values (still-running-note id (job-command job)
                                             (job-new-output job))
                         (list :job-id id :running t))))
             (sleep 0.05))
    ;; Finished on its own.
    (let ((code (nth-value 1 (evo.port:process-wait (job-process job))))
          (out (job-new-output job)))
      (retire-job id)
      (values (format nil "Job ~d finished.~%~a~@[~%(exit code ~d)~]"
                      id (if (plusp (length out)) out "(no new output)")
                      (and (integerp code) (/= code 0) code))
              (list :job-id id :exit-code code)))))

(defun register-jobs-tools ()
  (register-tool*
   :name "wait"
   :description
   (cat "Wait on a background job — the numbered handle bash returns when a "
        "command passes its timeout and keeps running.  Blocks up to `timeout` "
        "seconds and returns THE INSTANT the job finishes (with its exit code), "
        "or a fresh slice of output if it is still going.  Set kill to stop it "
        "now.  Do not sleep-and-poll; let wait block.  Jobs are killed when evo "
        "exits and do not survive a restart.")
   :schema '(:object
             (:job-id :type :integer
              :description "Job id bash returned when the command moved to the background.")
             (:timeout :type :integer :optional t
              :description "Max seconds to block before yielding (default 120, max 600); 0 polls and returns at once.")
             (:kill :type :boolean :optional t
              :description "Terminate the job now and return its final output."))
   :execute #'tool-wait))

(register-jobs-tools)
(evo.port:add-exit-hook 'reap-all-jobs)
