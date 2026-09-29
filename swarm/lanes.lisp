;;;; lanes.lisp — lanes: launched, initialized, watched, restarted, stopped.
;;;;
;;;; A lane is `evo-agent serve --no-userspace` invoked plainly, so evo's own
;;;; supervisor runs it (design.md §15): a crash or a hang restarts it with
;;;; --resume, which finds the lane's own session because EVO_SESSIONS_DIR
;;;; points it at the lane's directory — the same variable keeps lane
;;;; journals out of the coordinator's /resume list.  EVO_SERVE_WATCH_PID
;;;; names this process, so lanes whose coordinator died shut themselves down.
;;;;
;;;; Ownership (design.md §6): each lane has one subscriber thread, which reads
;;;; its events and owns its reconnects and re-initializations; the
;;;; coordinator's tools act on a lane through HTTP from the run thread; the
;;;; TUI only reads.  Every lane slot is written under the swarm lock.

(in-package :evo.swarm)

;;; Telling the coordinator.

(defun tell-coordinator (text &key (style :notice))
  "Give the coordinator TEXT as input: queued to its next turn boundary when
it is working, starting a run when it is idle — and shown to the human."
  (let ((agent (and *swarm* (swarm-agent *swarm*))))
    (when agent
      (evo:steer text agent)
      (swarm-say text :style style)
      (evo:request-run))))

;;; Holding the coordinator's goal.

(defun lanes-busy-p (&optional (swarm *swarm*))
  "True while some lane of SWARM is working or compacting."
  (and swarm
       (bt:with-lock-held ((swarm-lock swarm))
         (some (lambda (lane) (member (lane-state lane) '(:working :compacting)))
               (swarm-lanes swarm)))))

(defun hold-goal-while-lanes-work (agent goal)
  "A goal hold (evo.kernel:*goal-hold-predicates*): the coordinator's active
goal is not re-steered while a lane works.  Ending its turn then is waiting,
not idling, and the lane's report or finished run is what wakes it
(TELL-COORDINATOR).  With every lane idle the goal re-steers as usual."
  (declare (ignore goal))
  (and *swarm* (eq agent (swarm-agent *swarm*)) (lanes-busy-p)))

;;; The journal record: what `evo-swarm --resume` restores.

(defun swarm-record ()
  (with-swarm-lock ()
    (list :id (swarm-id *swarm*)
          :dir (namestring (swarm-dir *swarm*))
          :workers (swarm-workers *swarm*)
          :lanes (coerce
                  (loop for lane in (swarm-lanes *swarm*)
                        collect (list :n (lane-n lane)
                                      :cwd (namestring (lane-cwd lane))
                                      :worktree (lane-worktree lane)
                                      :branch (lane-branch lane)
                                      :task (lane-task lane)
                                      :extra-forms (coerce (lane-extra-forms lane) 'vector)))
                  'vector))))

(defun record-swarm ()
  "Journal the swarm's shape on the coordinator's session (a :custom entry,
invisible to the model): lanes, their worktrees, the code evaluated into
them.  Called whenever that shape changes."
  (when (and *swarm* (swarm-agent *swarm*))
    (ignore-errors (evo:set-custom-state "swarm" (swarm-record) (swarm-agent *swarm*)))))

;;; Launching.

(defparameter *lane-boot-seconds* 90
  "How long a lane may take to answer /health after launch.")

(defparameter *stripped-environment*
  '("EVO_SUPERVISED_CHILD=" "EVO_HEARTBEAT_FILE=" "EVO_NO_SUPERVISOR="
    "EVO_SERVE_TOKEN=" "EVO_SESSIONS_DIR=" "EVO_SERVE_WATCH_PID=")
  "Variables of this process a lane must not inherit: they describe the
coordinator's own supervision and session, not the lane's.")

(defun free-port ()
  "A port nothing listens on right now."
  (let ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect (usocket:get-local-port socket)
      (usocket:socket-close socket))))

(defun lane-environment (lane)
  (append (list (format nil "EVO_SERVE_TOKEN=~a" (lane-token lane))
                (format nil "EVO_SESSIONS_DIR=~a"
                        (namestring (merge-pathnames "sessions/" (lane-dir lane))))
                (format nil "EVO_SERVE_WATCH_PID=~d" (evo.port:getpid)))
          (lane-secret-environment)
          (remove-if (lambda (entry)
                       (some (lambda (prefix) (string-prefix-p prefix entry))
                             *stripped-environment*))
                     (evo.port:environ))))

(defun lane-has-session-p (lane)
  (directory (merge-pathnames "sessions/*.sexp" (lane-dir lane))))

(defun launch-lane (lane &key resume)
  "Start LANE's process: `evo-agent serve --no-userspace` on its port, supervised,
detached from our terminal, logging to its directory.  RESUME continues its
session when it has one."
  (ensure-directories-exist (merge-pathnames "sessions/" (lane-dir lane)))
  (write-file-string (merge-pathnames "url" (lane-dir lane))
                     (format nil "http://127.0.0.1:~d~%" (lane-port lane)))
  (let ((process (evo.port:launch-child
                  (namestring (swarm-evo-binary *swarm*))
                  (append (list "serve" "--no-userspace"
                                "--port" (princ-to-string (lane-port lane))
                                "--token-file" (namestring (merge-pathnames "token" (lane-dir lane))))
                          (when (and resume (lane-has-session-p lane)) '("--resume")))
                  :input nil
                  :output (merge-pathnames "lane.log" (lane-dir lane))
                  :error-output :output
                  :environment (lane-environment lane)
                  :directory (lane-cwd lane)
                  ;; Never share the coordinator's terminal: a lane's
                  ;; supervisor resets the tty it has after a crash.
                  :new-session t)))
    (with-swarm-lock ()
      (setf (lane-process lane) process
            (lane-state lane) :starting
            (lane-pid lane) nil
            (lane-cursor lane) 0))
    (maybe-publish-lane-state lane)
    process))

(defun wait-for-health (lane &key (seconds *lane-boot-seconds*))
  "LANE's /health once it answers, or NIL after SECONDS or if its process
exits first."
  (loop repeat (* seconds 5)
        for health = (lane-health lane)
        when health return health
        do (let ((process (lane-process lane)))
             (when (and process (not (evo.port:process-alive-p process)))
               (return nil)))
           (sleep 0.2)))

(defun initialize-lane (lane)
  "Evaluate the baseline in LANE, then the code the coordinator has
evaluated into it before (so a restart gets back what it had).  One form per
eval, as LOAD would: the lane reads each form only after the one before it
ran, so an IN-LANES form can name a package an earlier one loaded."
  (dolist (form (baseline-forms lane *swarm*))
    (lane-eval lane (forms->code (list form))))
  (dolist (extra (with-swarm-lock () (copy-list (lane-extra-forms lane))))
    (handler-case (lane-eval lane extra)
      (lane-error (e)
        (swarm-say (format nil "lane ~d: replaying an eval failed: ~a"
                           (lane-n lane) (lane-error-text e))
                   :style :error)))))

(defun sync-lane-state (lane &key (announce t))
  "Read LANE's /state into its status, and cache its goal.  ANNOUNCE NIL
refreshes without publishing a `lane-state` event: what bringing a lane up
does, so the machine-readable stream tells a lane's work cycle (starting,
working, idle) rather than an idle event for a lane never given work."
  (multiple-value-bind (status state) (ignore-errors (lane-get lane "/state"))
    (when (eql status 200)
      (with-swarm-lock ()
        (setf (lane-state lane)
              (let ((s (getf state :status)))
                (cond ((equal s "running") :working)
                      ((equal s "compacting") :compacting)
                      (t :idle)))))
      (note-lane-goal lane (let ((goal (getf state :goal)))
                             (and (listp goal) goal)))
      (when announce (maybe-publish-lane-state lane)))))

(defun bring-up-lane (lane &key resume)
  "Launch LANE, wait for it, initialize it, and start reading its events.
Returns T when it came up."
  (launch-lane lane :resume resume)
  (let ((health (wait-for-health lane)))
    (cond
      ((null health)
       (with-swarm-lock () (setf (lane-state lane) :down))
       (maybe-publish-lane-state lane)
       (tell-coordinator (format nil "[lane ~d] failed to start — see ~a"
                                 (lane-n lane)
                                 (namestring (merge-pathnames "lane.log" (lane-dir lane))))
                         :style :error)
       nil)
      (t
       (with-swarm-lock () (setf (lane-pid lane) (getf health :pid)))
       (handler-case (initialize-lane lane)
         (lane-error (e)
           (tell-coordinator (format nil "[lane ~d] initialization failed: ~a"
                                     (lane-n lane) (lane-error-text e))
                             :style :error)))
       ;; Subscribe from here on: what the lane said before it was
       ;; initialized (its model gate complaining that no model exists yet)
       ;; is not news for the coordinator.
       (let ((now (lane-health lane)))
         (with-swarm-lock () (setf (lane-cursor lane) (or (getf now :cursor) 0))))
       (sync-lane-state lane :announce nil)
       (start-subscriber lane)
       (swarm-repaint)
       t))))

;;; Events.

(defun report-text (lane event)
  (format nil "[lane ~d report] done: ~a~@[~%evidence: ~a~]~@[~%next: ~a~]~@[~%blocked: ~a~]~@[~%requests: ~a~]~@[~%goal: ~a~]"
          (lane-n lane)
          (or (getf event :done) "")
          (getf event :evidence) (getf event :next)
          (getf event :blocked) (getf event :requests)
          (getf event :goal)))

(defun goal-disposition (status)
  "What a settled lane's goal STATUS tells the coordinator, or NIL for no
goal.  A lane's goal re-steers it for as long as it is active, so a lane that
settles with its goal still active is not done: it errored or was stopped."
  (cond ((null status) nil)
        ((equal status "active") "active, but the lane is idle until steered")
        ((equal status "budget-limited") "budget-limited (out of tokens)")
        (t status)))

(defun run-ended-text (lane event)
  (format nil "[lane ~d] run ended (~a)~@[ — goal: ~a~]~@[ — task: ~a~]"
          (lane-n lane) (getf event :outcome)
          (goal-disposition (getf event :goal))
          (with-swarm-lock ()
            (and (lane-task lane)
                 (truncate-string (lane-task lane) 80 "…")))))

(defun watch-output (lane text)
  "Show TEXT from a watched lane in the scrollback, whole lines only."
  (let ((lines nil))
    (with-swarm-lock ()
      (let ((all (concatenate 'string (lane-partial lane) text)))
        (loop for pos = (position #\Newline all)
              while pos
              do (push (subseq all 0 pos) lines)
                 (setf all (subseq all (1+ pos))))
        (setf (lane-partial lane) all)))
    (dolist (line (nreverse lines))
      (swarm-say (format nil "  [lane ~d] ~a" (lane-n lane) line)))))

(defun flush-watch (lane)
  (let ((rest (with-swarm-lock () (shiftf (lane-partial lane) ""))))
    (when (plusp (length rest))
      (swarm-say (format nil "  [lane ~d] ~a" (lane-n lane) rest)))))

(defun handle-lane-event (lane type event)
  "One event from LANE: keep its status current, and turn what the
coordinator must hear — reports, finished runs, errors — into its input."
  (when (member type '("task-start" "settled") :test #'string=)
    (swarm-repaint))
  (let ((now (get-universal-time))
        (watched (with-swarm-lock () (lane-watched lane))))
    (cond
      ((string= type "task-start")
       (with-swarm-lock ()
         (setf (lane-state lane) (if (equal (getf event :kind) "compact") :compacting :working)
               (lane-task-started lane) now
               (lane-step-started lane) now))
       (maybe-publish-lane-state lane))
      ((member type '("turn-start" "compaction-start" "compaction-end") :test #'string=)
       (with-swarm-lock () (setf (lane-step-started lane) now)))
      ((string= type "report")
       (with-swarm-lock () (push event (lane-reports lane)))
       (note-lane-goal-status lane (getf event :goal))
       (maybe-publish-lane-state lane)
       (tell-coordinator (report-text lane event)))
      ((string= type "task-end")
       (let ((error (getf event :error)))
         (when error
           (tell-coordinator (format nil "[lane ~d] error: ~a" (lane-n lane) error)
                             :style :error))))
      ((string= type "settled")
       (with-swarm-lock () (setf (lane-state lane) :idle))
       (note-lane-goal-status lane (getf event :goal))
       (maybe-publish-lane-state lane)
       (when watched (flush-watch lane))
       (tell-coordinator (run-ended-text lane event)))
      ((and (string= type "output") (equal (getf event :style) "error"))
       (tell-coordinator (format nil "[lane ~d] ~a" (lane-n lane) (getf event :text))
                         :style :error))
      (watched
       (cond ((string= type "text-delta") (watch-output lane (getf event :text)))
             ((string= type "tool-call-start")
              (flush-watch lane)
              (swarm-say
               (format nil "  [lane ~d] ~a" (lane-n lane)
                       (evo.command:format-tool-call-plain (getf event :name)
                                                           (getf event :arguments)))))
             ((string= type "message-end") (flush-watch lane)))))))

(defun lane-stopping-p (lane)
  (with-swarm-lock () (or (lane-stopping lane) (swarm-stopping *swarm*))))

(defun read-lane-events (lane)
  "Read LANE's events until its stream ends."
  (handler-case
      (let ((stream (open-event-stream lane)))
        (unwind-protect
             (read-sse-events
              stream
              (lambda (id type data)
                (when id (with-swarm-lock () (setf (lane-cursor lane) id)))
                (let ((event (ignore-errors (evo.serve:decode-json data))))
                  (when (and type (listp event))
                    (handler-case (handle-lane-event lane type event)
                      (error (e)
                        (swarm-say (format nil "lane ~d: event error: ~a"
                                           (lane-n lane) e)
                                   :style :error)))))))
          (ignore-errors (close stream))))
    (error () nil)))

(defun recover-lane (lane)
  "LANE's stream ended while nobody was stopping it: wait for it to answer
again.  The same pid is a dropped connection; a new one is a crash its
supervisor already restarted, and that lane has lost what was evaluated into
it — re-initialize it and tell the coordinator.  A lane whose supervisor
itself exited is down.  Returns NIL when the lane is gone for good."
  (with-swarm-lock () (setf (lane-state lane) :down))
  (maybe-publish-lane-state lane)
  (loop
    (when (lane-stopping-p lane) (return nil))
    (let ((process (lane-process lane)))
      (when (and process (not (evo.port:process-alive-p process)))
        (tell-coordinator (format nil "[lane ~d] is down: its process exited. restart_lane brings it back."
                                  (lane-n lane))
                          :style :error)
        (return nil)))
    (let ((health (lane-health lane)))
      (when health
        (let ((old (with-swarm-lock () (lane-pid lane)))
              (new (getf health :pid)))
          (if (eql old new)
              (sync-lane-state lane)
              (progn
                (with-swarm-lock ()
                  (setf (lane-pid lane) new
                        (lane-cursor lane) 0)
                  (incf (lane-restarts lane)))
                (handler-case (initialize-lane lane)
                  (lane-error () nil))
                (sync-lane-state lane)
                (tell-coordinator
                 (format nil "[lane ~d] crashed and was restarted by its supervisor (pid ~a → ~a); its session was resumed and it was re-initialized.~@[ The task in flight (~a) may need re-delegating.~]"
                         (lane-n lane) old new
                         (with-swarm-lock () (lane-task lane)))
                 :style :error)))
          (return t))))
    (sleep 0.5)))

(defun subscriber-loop (lane)
  (loop
    (read-lane-events lane)
    (when (lane-stopping-p lane) (return))
    (unless (recover-lane lane) (return))))

(defun start-subscriber (lane)
  (let ((thread (bt:make-thread (lambda () (subscriber-loop lane))
                                :name (format nil "evo-swarm-lane-~d" (lane-n lane)))))
    (with-swarm-lock () (setf (lane-subscriber lane) thread))
    thread))

;;; Stopping and restarting.

(defun stop-lane (lane &key (seconds 15))
  "Shut LANE down cleanly (POST /shutdown), reap its process — killing it
after SECONDS — and let its subscriber see the stream end."
  (with-swarm-lock () (setf (lane-stopping lane) t))
  (ignore-errors (lane-post lane "/shutdown" :timeout 10))
  (let ((process (lane-process lane)))
    (when process
      (loop repeat (* seconds 10)
            while (evo.port:process-alive-p process)
            do (sleep 0.1))
      (when (evo.port:process-alive-p process)
        (ignore-errors (evo.port:process-kill-tree process)))
      (ignore-errors (evo.port:process-wait process))))
  (let ((thread (with-swarm-lock () (lane-subscriber lane))))
    (when (and thread (not (eq thread (bt:current-thread))))
      (loop repeat 50 while (bt:thread-alive-p thread) do (sleep 0.1))
      (when (bt:thread-alive-p thread)
        (ignore-errors (bt:destroy-thread thread)))))
  (with-swarm-lock ()
    (setf (lane-state lane) :stopped
          (lane-subscriber lane) nil))
  (maybe-publish-lane-state lane))

(defun restart-lane (lane &key fresh)
  "Stop LANE and bring it up again: its session resumed, or with FRESH a new
session (and the code evaluated into it forgotten).  Picks up a changed cwd
(a worktree given or taken away)."
  (stop-lane lane)
  (with-swarm-lock ()
    (setf (lane-stopping lane) nil)
    (when fresh
      (setf (lane-extra-forms lane) nil
            (lane-task lane) nil
            (lane-reports lane) nil)))
  (when fresh
    ;; A fresh session is a fresh sessions directory: --resume must not
    ;; find the old one on a later crash.
    (let ((sessions (merge-pathnames "sessions/" (lane-dir lane))))
      (dolist (file (directory (merge-pathnames "*.sexp" sessions)))
        (rename-file file (make-pathname :type "sexp-retired" :defaults file)))))
  (record-swarm)
  (bring-up-lane lane :resume (not fresh)))

;;; The swarm.

(defun swarm-home () (merge-pathnames "swarm/" (evo-home)))

(defun make-lane-for (swarm n &key cwd worktree branch task extra-forms)
  (%make-lane :n n :port (free-port)
              :token (fresh-token)
              :dir (merge-pathnames (format nil "lane-~d/" n) (swarm-dir swarm))
              :cwd (or cwd (swarm-cwd swarm))
              :worktree worktree :branch branch :task task
              :extra-forms (coerce (or extra-forms #()) 'list)))

(defun make-swarm (&key agent workers evo-binary record view)
  "A swarm for AGENT: from RECORD (a resumed coordinator journal's) when
given, else a new one of WORKERS lanes.  VIEW is how its coordinator is shown
and run (view.lisp)."
  (let* ((id (or (getf record :id) (format nil "~a-~a" (session-file-stamp) (gen-id 4))))
         (swarm (%make-swarm :id id
                             :dir (uiop:ensure-directory-pathname
                                   (or (getf record :dir)
                                       (merge-pathnames (format nil "~a/" id) (swarm-home))))
                             :cwd (uiop:getcwd)
                             :workers (if record (length (getf record :lanes)) workers)
                             :evo-binary evo-binary
                             :view view
                             :agent agent)))
    (setf (swarm-lanes swarm)
          (if record
              (loop for r across (getf record :lanes)
                    collect (make-lane-for swarm (getf r :n)
                                           :cwd (uiop:ensure-directory-pathname (getf r :cwd))
                                           :worktree (getf r :worktree)
                                           :branch (getf r :branch)
                                           :task (getf r :task)
                                           :extra-forms (getf r :extra-forms)))
              (loop for n from 1 to workers collect (make-lane-for swarm n))))
    swarm))

(defun session-file-stamp ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" year month day hour min sec)))

(defun start-lanes (swarm &key resume)
  "Bring every lane up in parallel, each on its own thread; the TUI does not
wait for them."
  (dolist (lane (swarm-lanes swarm))
    (let ((lane lane))
      (bt:make-thread (lambda ()
                        (handler-case (bring-up-lane lane :resume resume)
                          (error (e)
                            (with-swarm-lock () (setf (lane-state lane) :down))
                            (maybe-publish-lane-state lane)
                            (swarm-say (format nil "lane ~d: ~a" (lane-n lane) e)
                                       :style :error))))
                      :name (format nil "evo-swarm-start-~d" (lane-n lane))))))

(defun stop-swarm (&optional (swarm *swarm*))
  "Stop every lane, in parallel, and wait for them all."
  (when swarm
    (bt:with-lock-held ((swarm-lock swarm)) (setf (swarm-stopping swarm) t))
    (let ((threads (loop for lane in (swarm-lanes swarm)
                         collect (let ((lane lane))
                                   (bt:make-thread (lambda () (stop-lane lane))
                                                   :name "evo-swarm-stop")))))
      (dolist (thread threads) (ignore-errors (bt:join-thread thread))))))
