;;;; lanes.lisp — lanes: launched, mirrored, restarted, stopped.
;;;;
;;;; A lane is `evo-agent serve --no-supervisor` on loopback, with its own
;;;; ready file under the swarm's directory and stdin a pipe this process
;;;; holds (CONTRACT §1, §6): --watch-stdin sees EOF when the coordinator is
;;;; gone, which is how a lane can never outlive it.  Its session directory is
;;;; its own (EVO_SESSIONS_DIR), so lane journals stay out of the
;;;; coordinator's /resume list.
;;;;
;;;; One crash rule (design §7.2): the coordinator owns its lanes, so a lane
;;;; whose process exited is restarted *here*, resuming its exact session path,
;;;; and never by a supervisor of its own.  The lane's epoch changes, its
;;;; mirror emits topic.reset `lane:N`, and the coordinator's input gets a
;;;; lane-event item — with no pid comparison anywhere (the epoch is the
;;;; identity).
;;;;
;;;; Ownership (design.md §6): each lane has one thread, which owns its
;;;; process, its mirror and its reconnects; the coordinator's tools act on a
;;;; lane through /ops from the run thread; the TUI only reads.  Every lane slot
;;;; is written under the swarm lock.

(in-package :evo.swarm)

;;; Holding the coordinator's goal (and saying why it is waiting).

(defun hold-goal-while-lanes-work (agent goal)
  "A goal hold (evo.kernel:*goal-hold-predicates*): the coordinator's active
goal is not re-steered while a lane works.  Ending its turn then is waiting,
not idling, and the lane's report or finished run is what wakes it
(TELL-COORDINATOR).  With every lane idle the goal re-steers as usual."
  (declare (ignore goal))
  (and *swarm* (eq agent (swarm-agent *swarm*)) (lanes-busy-p)))

(defun register-swarm-holds ()
  "The swarm's holds, on both mechanisms: the kernel's goal hold (the
coordinator is not re-steered while lanes work) and the core hold hook the
VIEW reads to report session status `waiting` (CONTRACT §4.2).  Registered as
named hooks so a /reload does not double-register them."
  (pushnew 'hold-goal-while-lanes-work *goal-hold-predicates*)
  (evo:register-hold-predicate #'coordinator-hold-reason))

;;; The journal record: what `evo-swarm --resume` restores.

(defun swarm-record ()
  (with-swarm-lock ()
    (list :id (swarm-id *swarm*)
          :dir (namestring (swarm-dir *swarm*))
          :workers (swarm-workers *swarm*)
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
them, and the lane model configuration (CONTRACT §1).  Called whenever that
shape changes.  The session header names the swarm too (CONTRACT §3), so a
session list can say which swarm a session drove without opening the file."
  (when (and *swarm* (swarm-agent *swarm*))
    (let ((agent (swarm-agent *swarm*)))
      (ignore-errors (evo:set-custom-state "swarm" (swarm-record) agent))
      (ignore-errors (evo.journal:set-session-header (evo.kernel:agent-journal agent)
                                                    :program "evo-swarm"
                                                    :swarm-id (swarm-id *swarm*))))))

;;; Launching.

(defparameter *lane-boot-seconds* 90
  "How long a lane may take to write its ready file after launch.")

(defparameter *stripped-environment*
  '("EVO_SUPERVISED_CHILD=" "EVO_HEARTBEAT_FILE=" "EVO_NO_SUPERVISOR="
    "EVO_SERVE_TOKEN=" "EVO_SESSIONS_DIR=" "EVO_SERVE_WATCH_PID="
    "EVO_SUPERVISOR_STATE_DIR=" "EVO_SUPERVISOR_PID=" "EVO_SUPERVISOR_RESTARTS="
    "EVO_RECOVERY=")
  "Variables of this process a lane must not inherit: they describe the
coordinator's own supervision and session, not the lane's.  A lane is launched
with --no-supervisor, but an inherited supervisor state directory would still
have it report to the coordinator's supervisor.")

(defun lane-environment (lane)
  "The lane's environment: its own session directory, the providers' keys (by
variable only), and this process's environment minus what describes the
coordinator's own supervision."
  (append (list (format nil "EVO_SESSIONS_DIR=~a"
                        (namestring (merge-pathnames "sessions/" (lane-dir lane)))))
          (lane-secret-environment)
          (remove-if (lambda (entry)
                       (some (lambda (prefix) (string-prefix-p prefix entry))
                             *stripped-environment*))
                     (evo.port:environ))))

(defun lane-log-path (lane)
  (merge-pathnames "lane.log" (lane-dir lane)))

(defun lane-has-session-p (lane)
  (directory (merge-pathnames "sessions/*.sexp" (lane-dir lane))))

(defun lane-launch-args (lane &key resume)
  "How a lane is launched: no supervisor of its own (the coordinator restarts
it, design §7.2), a port of its own choosing announced in its ready file, and
stdin a pipe we hold (EOF = the coordinator is gone).  With RESUME it continues
its exact session (CONTRACT §8), never a bare --resume."
  (append (list "serve" "--no-userspace" "--no-supervisor"
                "--port" "0"
                "--ready-file" (namestring (ready-file-path lane))
                "--watch-stdin")
          ;; The exact session it was on, never "whatever is newest in this
          ;; directory" (CONTRACT §8).  A lane directory from before ready
          ;; files existed has a session but no path: its own sessions
          ;; directory still makes a bare --resume unambiguous.
          (when resume
            (let ((session (lane-session-path lane)))
              (cond ((and session (probe-file session))
                     (list "--resume" (namestring session)))
                    ((lane-has-session-p lane) '("--resume")))))))

(defun launch-lane (lane &key resume)
  "Start LANE's process, detached from our terminal, logging to its directory.
RESUME continues the exact session its ready file named.  Returns the process,
or NIL when the lane is being stopped."
  (ensure-directories-exist (merge-pathnames "sessions/" (lane-dir lane)))
  (when (lane-stopping-p lane)
    (return-from launch-lane nil))
  ;; A ready file left by the process that died would be read as this
  ;; launch's: it is written once we are listening, so it must not survive.
  (ignore-errors (delete-file (ready-file-path lane)))
  (let ((args (lane-launch-args lane :resume resume))
        (process nil) (stdin nil))
    ;; Piped, not inherited: the write end is how the coordinator holds the
    ;; lane, and closing it is the end of file --watch-stdin stops on.
    (setf (values process stdin)
          (evo.port:launch-child-piped (namestring (swarm-evo-binary *swarm*)) args
                                       :output (lane-log-path lane)
                                       :error-output :output
                                       :environment (lane-environment lane)
                                       :directory (lane-cwd lane)
                                       :new-session t))
    (when (with-swarm-lock ()
            (setf (lane-process lane) process
                  (lane-stdin lane) stdin)
            (cond ((or (lane-stopping lane) (swarm-stopping *swarm*)) t)
                  (t (setf (lane-state lane) :starting
                           (lane-ready lane) nil)
                     nil)))
      (ignore-errors (evo.port:process-kill-tree process))
      (ignore-errors (evo.port:process-wait process))
      (return-from launch-lane nil))
    (swarm-lane-changed)
    process))

(defun lane-stopping-p (lane)
  "True while the swarm itself is stopping LANE (or going away)."
  (with-swarm-lock () (or (lane-stopping lane) (swarm-stopping *swarm*))))

(defun lane-process-alive-p (lane)
  (let ((process (lane-process lane)))
    (and process (evo.port:process-alive-p process))))

(defun wait-for-ready (lane &key (seconds *lane-boot-seconds*) (epoch nil))
  "Wait for LANE's ready file to name a process that is listening.  EPOCH, when
given, must differ (a restart, whose predecessor's file is not news).  Returns
the ready plist, or NIL when the deadline passes or the process exits."
  (loop repeat (* seconds 5)
        for ready = (lane-refresh-ready lane)
        when (and ready (not (equal (getf ready :epoch) epoch)))
          return ready
        unless (lane-process-alive-p lane)
          return nil
        do (sleep 0.2)))

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
  "Evaluate the baseline in LANE, then the code the coordinator has evaluated
into it before (so a restart gets back what it had).  One form per eval, as
LOAD would: the lane reads each form only after the one before it ran, so an
IN-LANES form can name a package an earlier one loaded."
  (dolist (form (baseline-forms lane *swarm*))
    (lane-op lane "eval" (list :code (forms->code (list form)))))
  (dolist (extra (with-swarm-lock () (copy-list (lane-extra-forms lane))))
    (handler-case (lane-op lane "eval" (list :code extra))
      (lane-error (e)
        (swarm-say (format nil "lane ~d: replaying an eval failed: ~a"
                           (lane-n lane) (lane-error-text e))
                   :style :error)))))

(defun bring-up-lane (lane &key resume)
  "Launch LANE, wait for it, initialize it, and take its first snapshot.
Returns T when it came up.  A lane stopped while it came up (the swarm
quitting, or switched away from by /resume) stops coming up, quietly."
  (unless (launch-lane lane :resume resume)
    (return-from bring-up-lane nil))
  (let ((ready (wait-for-ready lane :epoch (and resume (lane-epoch lane)))))
    (cond
      ((lane-stopping-p lane) nil)
      ((null ready)
       (with-swarm-lock () (setf (lane-state lane) :down))
       (swarm-lane-changed)
       (tell-lane-event lane :failed-to-start :severity :error
                        :detail (format nil "see ~a" (namestring (lane-log-path lane))))
       nil)
      (t
       (with-swarm-lock ()
         (setf (lane-state lane) :starting
               (lane-epoch lane) (getf ready :epoch)
               (lane-session-path lane) (getf (getf ready :session) :path)))
       (handler-case (initialize-lane lane)
         (lane-error (e)
           (tell-lane-event lane :init-failed :severity :error
                            :detail (lane-error-text e))))
       (when (lane-stopping-p lane)
         (return-from bring-up-lane nil))
       ;; The lane's own view is authoritative for what it shows; the first
       ;; snapshot is what makes it `starting → idle` (CONTRACT §4.3).
       (ignore-errors (mirror-load (lane-mirror lane)))
       (mirror-note-lane-state (lane-mirror lane))
       (swarm-lane-changed)
       t))))

(defun clear-lane-ready (lane)
  "Forget the lane's ready file, so nothing reads a dead process's address."
  (ignore-errors (delete-file (ready-file-path lane)))
  (with-swarm-lock () (setf (lane-ready lane) nil)))

;;; The lane's one thread: its process, its mirror, its reconnects.

(defun lane-frame (lane id type data)
  "One frame of LANE's op stream.  TYPE is serve's `op`; ID is the cursor the
frame carries, which may only advance once the op is applied."
  (declare (ignore type))
  (let ((op (ignore-errors (evo.serve:decode-json data))))
    (when (listp op)
      (let ((name (getf op :op)))
        (cond
          ((equal name "hello")
           (setf (mirror-cursor (lane-mirror lane))
                              (format nil "~a.~a" (getf op :epoch) (getf op :seq))))
          (name
           (let ((result (handler-case (mirror-apply (lane-mirror lane) op)
                           (error (e)
                             (swarm-say (format nil "lane ~d: bad op (~a): ~a"
                                                (lane-n lane) name e)
                                        :style :error)
                             :rebuilt))))
             ;; A rebuild took its own snapshot at a cursor of its own; the
             ;; frame's id is older and would replay what it already holds.
             (unless (eq result :rebuilt)
               (when (stringp id) (setf (mirror-cursor (lane-mirror lane)) id))))))))))

(defun follow-lane (lane)
  "Follow LANE's stream from the mirror's cursor until it ends."
  (let* ((mirror (lane-mirror lane))
         (cursor (or (mirror-cursor mirror) (mirror-load mirror))))
    (let ((stream (lane-open-stream lane :topics *lane-topic* :since cursor)))
      (unwind-protect
           (read-sse-events stream (lambda (id type data) (lane-frame lane id type data)))
        (ignore-errors (close stream))))))

(defun lane-recover (lane)
  "LANE's stream ended while nobody was stopping it.  The same process (the
epoch in its ready file is the one we had) is a dropped connection: follow it
again from the cursor.  A process that exited is restarted here, with its
session resumed — the epoch changes, the mirror emits topic.reset `lane:N`, and
the coordinator hears a lane-event item (design §7.2).  Returns T when the lane
is following again, NIL when it is finished."
  (let ((epoch (lane-epoch lane)))
    (with-swarm-lock () (setf (lane-state lane) :down))
    (swarm-lane-changed)
    (loop
      (when (lane-stopping-p lane) (return nil))
      (cond
        ((lane-process-alive-p lane)
         (let ((ready (lane-refresh-ready lane)))
           (cond
             ((null ready) (sleep 0.2))
             ((equal (getf ready :epoch) epoch)
              ;; The same process: nothing was lost but the connection.
              (with-swarm-lock () (setf (lane-state lane) :idle))
              (mirror-note-lane-state (lane-mirror lane))
              (swarm-lane-changed)
              (return t))
             (t
              ;; A new process with the same lane number: it restarted under
              ;; us (a crash its ready file survived).
              (lane-restarted lane ready)
              (return t)))))
        (t (return (restart-crashed-lane lane)))))))

(defun lane-restarted (lane ready)
  "Adopt READY, the ready file of a LANE process that is not the one we knew:
its session is the one it resumed, its items are the ones it has now."
  (with-swarm-lock ()
    (setf (lane-epoch lane) (getf ready :epoch)
          (lane-session-path lane) (getf (getf ready :session) :path)
          (lane-state lane) :idle))
  (incf-lane-restarts lane)
  (mirror-rebuild (lane-mirror lane) "lane_restarted")
  (mirror-note-lane-state (lane-mirror lane))
  (swarm-lane-changed)
  (tell-lane-event lane :restarted :severity :warn
                   :detail "its session was resumed and it was re-initialized"))

(defun incf-lane-restarts (lane)
  (with-swarm-lock () (incf (lane-restarts lane))))

(defun restart-crashed-lane (lane)
  "Restart a LANE whose process exited, here — the coordinator owns its lanes,
so there is no per-lane supervisor to wait for (design §7.2).  Its exact
session path is resumed.  Returns T when it came back; the caller is already
the lane's own thread, so no new one is started."
  (let ((log (namestring (lane-log-path lane))))
    (clear-lane-ready lane)
    (incf-lane-restarts lane)
    (tell-lane-event lane :crashed :severity :error
                     :detail (format nil "its process exited; restarting it from ~a" log))
    (if (bring-up-lane lane :resume t)
        (progn (tell-lane-event lane :restarted :severity :warn
                                :detail "its session was resumed; the task in flight may need re-delegating")
               t)
        (progn (tell-lane-event lane :down :severity :error
                                :detail (format nil "it did not come back — see ~a" log))
               nil))))

(defun lane-thread-body (lane)
  "Own LANE's stream for the swarm's life.  The process is already up (the
caller brought it up): follow its ops, and when the stream ends decide between
following the same process and restarting it."
  (loop
    (handler-case (follow-lane lane)
      (error (e)
        (swarm-say (format nil "lane ~d: ~a" (lane-n lane) e) :style :error)))
    (when (lane-stopping-p lane) (return))
    (unless (lane-recover lane) (return))))

(defun start-lane-thread (lane)
  "Give LANE its one thread, for a lane already brought up (a restart, or the
swarm's first launch)."
  (let ((thread (bt:make-thread (lambda () (lane-thread-body lane))
                                :name (format nil "evo-swarm-mirror-~d" (lane-n lane)))))
    (with-swarm-lock () (setf (lane-subscriber lane) thread))
    thread))

;;; Stopping and restarting.

(defun stop-lane (lane &key (seconds 15))
  "Shut LANE down: close the pipe that holds it (EOF is its shutdown, CONTRACT
§1), wait for the process — killing it after SECONDS — and let its mirror see
the stream end."
  (with-swarm-lock () (setf (lane-stopping lane) t))
  (let ((stdin (lane-stdin lane)))
    (when stdin (ignore-errors (close stdin))))
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
          (lane-subscriber lane) nil
          (lane-stdin lane) nil))
  (clear-lane-ready lane)
  (swarm-lane-changed))

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
            (lane-reports lane) 0
            (lane-session-path lane) nil)))
  (when fresh
    ;; A fresh session is a fresh sessions directory: --resume must not find
    ;; the old one on a later crash.
    (let ((sessions (merge-pathnames "sessions/" (lane-dir lane))))
      (dolist (file (directory (merge-pathnames "*.sexp" sessions)))
        (rename-file file (make-pathname :type "sexp-retired" :defaults file)))))
  (record-swarm)
  (when (bring-up-lane lane :resume (not fresh))
    (start-lane-thread lane)
    t))

;;; The swarm.

(defun swarm-home () (merge-pathnames "swarm/" (evo-home)))

(defun make-lane-for (swarm n &key cwd worktree branch task extra-forms)
  (let ((lane (%make-lane :n n
                          :dir (merge-pathnames (format nil "lane-~d/" n) (swarm-dir swarm))
                          :cwd (or cwd (swarm-cwd swarm))
                          :worktree worktree :branch branch :task task
                          :extra-forms (coerce (or extra-forms #()) 'list))))
    (setf (lane-mirror lane) (make-mirror lane))
    lane))

(defun make-swarm (&key agent workers evo-binary record view server)
  "A swarm for AGENT: from RECORD (a resumed coordinator journal's) when
given, else a new one of WORKERS lanes.  VIEW is how its coordinator is shown
and run (view.lisp); SERVER is its op log, when the coordinator is served."
  (let* ((id (or (getf record :id) (format nil "~a-~a" (session-file-stamp) (gen-id 4))))
         (swarm (%make-swarm :id id
                             :dir (uiop:ensure-directory-pathname
                                   (or (getf record :dir)
                                       (merge-pathnames (format nil "~a/" id) (swarm-home))))
                             :cwd (uiop:getcwd)
                             :workers (if record (length (getf record :lanes)) workers)
                             :evo-binary evo-binary
                             :view view
                             :server server
                             :agent agent
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
  "Bring every lane up in parallel, each on its own thread; the coordinator
does not wait for them."
  (dolist (lane (swarm-lanes swarm))
    (let ((lane lane))
      (bt:make-thread (lambda ()
                        (handler-case
                            (when (bring-up-lane lane :resume resume)
                              (lane-thread-body lane))
                          (error (e)
                            (with-swarm-lock () (setf (lane-state lane) :down))
                            (swarm-lane-changed)
                            (swarm-say (format nil "lane ~d: ~a" (lane-n lane) e)
                                       :style :error))))
                      :name (format nil "evo-swarm-start-~d" (lane-n lane))))))

(defun adopt-session-swarm (agent)
  "Called when AGENT's coordinator switched journals (/new, /fork, /resume).
A resumed session that records a different swarm gets that swarm back: the
running lanes stop, the recorded lanes come up — each resuming its own session
— and every client is told to re-snapshot the swarm and every lane (CONTRACT
§7.3).  Any other session — a new one, a fork, one with no swarm — takes the
running lanes along.  Either way the session then records the swarm it has."
  (let ((record (evo:custom-state "swarm" agent))
        (old *swarm*))
    (when (and old record (getf record :id)
               (not (equal (getf record :id) (swarm-id old))))
      (swarm-say (format nil "switching to this session's swarm ~a: stopping the current lanes…"
                         (getf record :id)))
      (stop-swarm old)
      (publish-swarm-switch old)
      (setf *swarm* (make-swarm :agent agent :record record
                                :workers (swarm-workers old)
                                :evo-binary (swarm-evo-binary old)
                                :view (swarm-view old)
                                :server (swarm-server old)))
      (ensure-directories-exist (swarm-dir *swarm*))
      (register-swarm-topics (swarm-server *swarm*) *swarm*)
      (start-lanes *swarm* :resume t)
      (swarm-lane-changed))
    (record-swarm)))

(defun publish-swarm-switch (swarm)
  "A different swarm is taking over: the swarm topic and every lane topic are
not the client's any more (§7.3)."
  (publish-op (list :op "topic.reset" :topic "swarm" :reason "swarm_switched"))
  (dolist (lane (swarm-lanes swarm))
    (publish-op (list :op "topic.reset" :topic (lane-topic lane) :reason "swarm_switched"))))

(defun stop-swarm (&optional (swarm *swarm*))
  "Stop every lane, in parallel, and wait for them all."
  (when swarm
    (bt:with-lock-held ((swarm-lock swarm)) (setf (swarm-stopping swarm) t))
    (let ((threads (loop for lane in (swarm-lanes swarm)
                         collect (let ((lane lane))
                                   (bt:make-thread (lambda () (stop-lane lane))
                                                   :name "evo-swarm-stop")))))
      (dolist (thread threads) (ignore-errors (bt:join-thread thread))))))
