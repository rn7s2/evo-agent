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

(defun tell-coordinator (text &key origin (style :notice))
  "Give the coordinator TEXT as input: queued to its next turn boundary when
it is working, starting a run when it is idle — and shown to the human.

ORIGIN is the structured record of what was injected (`:lane-report` or
`:lane-event`, CONTRACT §3), journaled beside the message.  TEXT stays exactly
what it was: the model still reads a `[lane 3 report] …` line, and a frontend
that has the origin does not have to parse it back out."
  (let ((agent (and *swarm* (swarm-agent *swarm*))))
    (when agent
      (evo:steer text agent nil :origin origin)
      (swarm-say text :style style)
      (evo:request-run))))

(defun lane-report-origin (lane event)
  "A lane's report as structure: what the coordinator used to have to re-parse
out of the English line (B1)."
  (list* :kind :lane-report :lane (lane-n lane)
         :done (getf event :done)
         (append (when (getf event :evidence) (list :evidence (getf event :evidence)))
                 (when (getf event :next) (list :next (getf event :next)))
                 (when (getf event :blocked) (list :blocked (getf event :blocked)))
                 (when (getf event :requests) (list :requests (getf event :requests)))
                 (when (getf event :goal)
                   (list :goal (intern (string-upcase (getf event :goal)) :keyword))))))

(defun lane-event-origin (lane event &key detail severity outcome goal-status)
  "One lane transition as structure — the machine-readable half of the line
TELL-COORDINATOR carries.  EVENT is one of :RUN-ENDED :ERROR :FAILED-TO-START
:INIT-FAILED :CRASHED :DOWN :RESTARTED."
  (list :kind :lane-event :lane (lane-n lane) :event event
        :severity (or severity :info)
        :outcome outcome
        :goal-status (and goal-status (intern (string-upcase goal-status) :keyword))
        :detail detail))

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
          ;; The lane configuration is swarm data, not a user file: recorded
          ;; here so a resumed swarm runs its lanes the same way.
          :lane-model (swarm-lane-model *swarm*)
          :lane-provider (swarm-lane-provider *swarm*)
          :lane-thinking (swarm-lane-thinking *swarm*)
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
them.  Called whenever that shape changes.  The session header names the
swarm too, so a session list can say which swarm a session drove without
opening the file."
  (when (and *swarm* (swarm-agent *swarm*))
    (let ((agent (swarm-agent *swarm*)))
      (ignore-errors (evo:set-custom-state "swarm" (swarm-record) agent))
      (ignore-errors (evo.journal:set-session-header (evo.kernel:agent-journal agent)
                                                    :program "evo-swarm"
                                                    :swarm-id (swarm-id *swarm*))))))

;;; Launching.

(defparameter *lane-boot-seconds* 90
  "How long a lane may take to answer /health after launch.")

(defparameter *stripped-environment*
  '("EVO_SUPERVISED_CHILD=" "EVO_HEARTBEAT_FILE=" "EVO_NO_SUPERVISOR="
    "EVO_SERVE_TOKEN=" "EVO_SESSIONS_DIR=" "EVO_SERVE_WATCH_PID="
    "EVO_SUPERVISOR_STATE_DIR=" "EVO_SUPERVISOR_PID=" "EVO_SUPERVISOR_RESTARTS="
    "EVO_RECOVERY=")
  "Variables of this process a lane must not inherit: they describe the
coordinator's own supervision and session, not the lane's.  A lane is its own
supervised process, so it makes its own supervisor state directory — an
inherited one would have it report its session to the coordinator's
supervisor.")

(defun lane-environment (lane)
  (append (list (format nil "EVO_SESSIONS_DIR=~a"
                        (namestring (merge-pathnames "sessions/" (lane-dir lane)))))
          (lane-secret-environment)
          (remove-if (lambda (entry)
                       (some (lambda (prefix) (string-prefix-p prefix entry))
                             *stripped-environment*))
                     (evo.port:environ))))

(defun lane-has-session-p (lane)
  (directory (merge-pathnames "sessions/*.sexp" (lane-dir lane))))

(defun lane-ready-file (lane)
  "Where LANE publishes its URL, token, epoch, pid and session (see
docs/serve.md): <lane dir>/ready.json."
  (merge-pathnames "ready.json" (lane-dir lane)))

(defun lane-ready-plist (lane)
  "LANE's ready file as a plist, or NIL when it is not there yet."
  (let ((path (lane-ready-file lane)))
    (when (probe-file path)
      (ignore-errors (evo.serve:decode-json (read-file-string path))))))

(defun lane-resume-path (lane)
  "The session LANE was on, from the ready file it published last, or NIL.
An exact path — a lane is resumed as the session it was, not as \"whatever is
newest in this directory\" (which would be a different session the moment
another lane shares the directory)."
  (let* ((ready (lane-ready-plist lane))
         (path (getf (getf ready :session) :path)))
    (and path (probe-file path) path)))

(defun launch-lane (lane &key resume)
  "Start LANE's process: `evo-agent serve --no-userspace` with its own ready
file, supervised, detached from our terminal, logging to its directory.  Its
stdin is a pipe this process holds: closing it is how a lane is told its
coordinator is gone (--watch-stdin).  RESUME continues its session when it has
one."
  (ensure-directories-exist (merge-pathnames "sessions/" (lane-dir lane)))
  (let* ((path (and resume (or (lane-resume-path lane)
                               ;; A lane directory from before ready files
                               ;; existed: its own sessions directory still
                               ;; makes a bare --resume unambiguous.
                               (and (lane-has-session-p lane) :latest))))
         (ready-file (namestring (lane-ready-file lane))))
    ;; A ready file from the life before this one says nothing about this
    ;; one: whoever starts it publishes its own.
    (ignore-errors (delete-file ready-file))
    (when (lane-stopping-p lane)
      (return-from launch-lane nil))
    (multiple-value-bind (process input)
        (evo.port:launch-child-piped
         (namestring (swarm-evo-binary *swarm*))
         (append (list "serve" "--no-userspace" "--port" "0"
                       "--ready-file" ready-file)
                 (when input '("--watch-stdin"))
                 (when path
                   (if (eq path :latest) '("--resume") (list "--resume" path))))
         :output (merge-pathnames "lane.log" (lane-dir lane))
         :error-output :output
         :environment (lane-environment lane)
         :directory (lane-cwd lane)
         ;; Never share the coordinator's terminal: a lane's supervisor
         ;; resets the tty it has after a crash.
         :new-session t)
      ;; A stop that came while we launched saw no process to reap: reap it
      ;; here, or it would outlive the swarm that dropped it.
      (when (with-swarm-lock ()
              (setf (lane-process lane) process
                    (lane-input lane) input)
              (cond ((or (lane-stopping lane) (swarm-stopping *swarm*)) t)
                    (t (setf (lane-state lane) :starting
                             (lane-pid lane) nil
                             (lane-ready lane) nil
                             (lane-cursor lane) 0)
                       nil)))
        (close-lane-input lane)
        (ignore-errors (evo.port:process-kill-tree process))
        (ignore-errors (evo.port:process-wait process))
        (return-from launch-lane nil))
      (maybe-publish-lane-state lane)
      process)))

(defun close-lane-input (lane)
  "Let go of LANE's stdin.  A lane is watching it (--watch-stdin), so this is
the end-of-file that tells it to stop — the one signal that survives the
coordinator dying, SIGKILL and all."
  (let ((input (with-swarm-lock () (shiftf (lane-input lane) nil))))
    (when input (ignore-errors (close input)))
    (not (null input))))

(defun wait-for-ready (lane &key (seconds *lane-boot-seconds*))
  "Read LANE's ready file once it publishes one, and keep it on the lane: the
URL, token, epoch and pid a client needs, and the session behind them (see
docs/serve.md \"Starting it\").  NIL after SECONDS, or as soon as its process
exits — a lane that never becomes ready is a failure, not a wait."
  (loop repeat (* seconds 5)
        do (let ((ready (lane-ready-plist lane)))
             (when (and ready (getf ready :port))
               (with-swarm-lock ()
                 (setf (lane-ready lane) ready
                       (lane-port lane) (getf ready :port)
                       (lane-token lane) (getf ready :token)
                       (lane-pid lane) (getf ready :pid)))
               (return ready)))
           (let ((process (lane-process lane)))
             (when (and process (not (evo.port:process-alive-p process)))
               (return nil)))
           (sleep 0.2)))

(defun refresh-lane-ready (lane)
  "Re-read LANE's ready file; T when the lane is a different life than the one
we were talking to (its epoch changed).  Every restart publishes a new one,
with the port, token, pid and epoch of the process that is serving now."
  (let ((ready (lane-ready-plist lane)))
    (when (and ready (getf ready :port))
      (with-swarm-lock ()
        (let ((changed (not (equal (getf ready :epoch)
                                   (getf (lane-ready lane) :epoch)))))
          (setf (lane-ready lane) ready
                (lane-port lane) (getf ready :port)
                (lane-token lane) (getf ready :token)
                (lane-pid lane) (getf ready :pid))
          changed)))))

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
  "Launch LANE, wait for its ready file, initialize it, and start reading its
events.  Returns T when it came up.  A lane stopped while it came up (the
swarm quitting, or switched away from by /resume) stops coming up, quietly."
  (unless (launch-lane lane :resume resume)
    (return-from bring-up-lane nil))
  (let ((ready (wait-for-ready lane)))
    (cond
      ((lane-stopping-p lane) nil)
      ((null ready)
       (with-swarm-lock () (setf (lane-state lane) :down))
       (maybe-publish-lane-state lane)
       (tell-coordinator (format nil "[lane ~d] failed to start — see ~a"
                                 (lane-n lane)
                                 (namestring (merge-pathnames "lane.log" (lane-dir lane))))
                         :style :error
                         :origin (lane-event-origin
                                  lane :failed-to-start
                                  :severity :error
                                  :detail (namestring (merge-pathnames "lane.log" (lane-dir lane)))))
       nil)
      (t
       (handler-case (initialize-lane lane)
         (lane-error (e)
           (tell-coordinator (format nil "[lane ~d] initialization failed: ~a"
                                     (lane-n lane) (lane-error-text e))
                             :style :error
                             :origin (lane-event-origin lane :init-failed
                                                        :severity :error
                                                        :detail (lane-error-text e)))))
       ;; Subscribe from here on: what the lane said before it was
       ;; initialized (its model gate complaining that no model exists yet)
       ;; is not news for the coordinator.
       (when (lane-stopping-p lane)
         (return-from bring-up-lane nil))
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
       (tell-coordinator (report-text lane event)
                         :origin (lane-report-origin lane event)))
      ((string= type "task-end")
       (let ((error (getf event :error)))
         (when error
           (tell-coordinator (format nil "[lane ~d] error: ~a" (lane-n lane) error)
                             :style :error
                             :origin (lane-event-origin lane :error
                                                        :severity :error
                                                        :detail (format nil "~a" error))))))
      ((string= type "settled")
       (with-swarm-lock () (setf (lane-state lane) :idle))
       (note-lane-goal-status lane (getf event :goal))
       (maybe-publish-lane-state lane)
       (when watched (flush-watch lane))
       (tell-coordinator (run-ended-text lane event)
                         :origin (lane-event-origin lane :run-ended
                                                    :outcome (getf event :outcome)
                                                    :goal-status (getf event :goal))))
      ((and (string= type "output") (equal (getf event :style) "error"))
       (tell-coordinator (format nil "[lane ~d] ~a" (lane-n lane) (getf event :text))
                         :style :error
                         :origin (lane-event-origin lane :error
                                                    :severity :error
                                                    :detail (getf event :text))))
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
again.  A dropped connection leaves it the same process; a new epoch in its
ready file is a crash its own supervisor restarted, and that lane has lost
what was evaluated into it — re-initialize it and tell the coordinator.  A
lane whose supervisor itself exited is down.  Returns NIL when the lane is
gone for good."
  (with-swarm-lock () (setf (lane-state lane) :down))
  (maybe-publish-lane-state lane)
  (loop
    (when (lane-stopping-p lane) (return nil))
    (let ((process (lane-process lane)))
      (when (and process (not (evo.port:process-alive-p process)))
        (tell-coordinator (format nil "[lane ~d] is down: its process exited. restart_lane brings it back."
                                  (lane-n lane))
                          :style :error
                          :origin (lane-event-origin lane :down :severity :error))
        (return nil)))
    (when (refresh-lane-ready lane)
      (with-swarm-lock ()
        (setf (lane-cursor lane) 0)
        (incf (lane-restarts lane)))
      (handler-case (initialize-lane lane)
        (lane-error () nil))
      (sync-lane-state lane)
      (tell-coordinator
       (format nil "[lane ~d] crashed and was restarted by its supervisor (epoch ~a); its session was resumed and it was re-initialized.~@[ The task in flight (~a) may need re-delegating.~]"
               (lane-n lane) (getf (lane-ready lane) :epoch)
               (with-swarm-lock () (lane-task lane)))
       :style :error)
      (return t))
    (when (and (lane-health lane) (lane-ready lane))
      (sync-lane-state lane)
      (return t))
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
  "Shut LANE down: close the pipe it watches and ask it over HTTP — either one
is a clean stop, and the pipe is the one that works even when its HTTP layer
is wedged — then reap its process (killing it after SECONDS) and let its
subscriber see the stream end."
  (with-swarm-lock () (setf (lane-stopping lane) t))
  (close-lane-input lane)
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
  (%make-lane :n n
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
                             :agent agent
                             ;; Recorded lane configuration, restored: a
                             ;; resumed swarm runs its lanes as it did.
                             :lane-model (getf record :lane-model)
                             :lane-provider (getf record :lane-provider)
                             :lane-thinking (getf record :lane-thinking))))
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

(defun adopt-session-swarm (agent)
  "Called when AGENT's coordinator switched journals (/new, /fork, /resume).
A resumed session that records a different swarm gets that swarm back: the
running lanes stop (their sessions stay in their own swarm directory, which
the session just left still records) and the recorded lanes come up, each
resuming its own session.  Any other session — a new one, a fork, one with
no swarm — takes the running lanes along.  Either way the session then
records the swarm it has."
  (let ((record (evo:custom-state "swarm" agent))
        (old *swarm*))
    (when (and old record (getf record :id)
               (not (equal (getf record :id) (swarm-id old))))
      (swarm-say (format nil "switching to this session's swarm ~a: stopping the current lanes…"
                         (getf record :id)))
      (stop-swarm old)
      (setf *swarm* (make-swarm :agent agent :record record
                                :workers (swarm-workers old)
                                :evo-binary (swarm-evo-binary old)
                                :view (swarm-view old)))
      (ensure-directories-exist (swarm-dir *swarm*))
      (start-lanes *swarm* :resume t)
      (swarm-repaint))
    (record-swarm)))

(defun stop-swarm (&optional (swarm *swarm*))
  "Stop every lane, in parallel, and wait for them all."
  (when swarm
    (bt:with-lock-held ((swarm-lock swarm)) (setf (swarm-stopping swarm) t))
    (let ((threads (loop for lane in (swarm-lanes swarm)
                         collect (let ((lane lane))
                                   (bt:make-thread (lambda () (stop-lane lane))
                                                   :name "evo-swarm-stop")))))
      (dolist (thread threads) (ignore-errors (bt:join-thread thread))))))
