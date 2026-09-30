;;;; server.lisp — `evo serve`: one session, controlled over HTTP.
;;;;
;;;; Threads, and who owns what:
;;;;
;;;;   session thread  the process's main thread.  Owns the task (the one run
;;;;                   or compaction), the agent's mailbox writes that start
;;;;                   work, every journal switch, every operation.  It blocks
;;;;                   on a condition variable until a message arrives;
;;;;                   nothing else touches what it owns.
;;;;   listener        accepts connections, starts one thread per connection.
;;;;   connection      checks the token, reads one request and answers it: a
;;;;                   snapshot from the published view, or an op queued for
;;;;                   the session thread — or, for /stream, a thread that
;;;;                   writes the op log from its own cursor.
;;;;   op flusher      writes out coalesced item.append ops when their 50 ms
;;;;                   window closes.
;;;;   run worker      the task's thread.  Its events go to the view; its
;;;;                   completion goes to the inbox.
;;;;
;;;; Every operation runs on the session thread, in arrival order, exactly as
;;;; the TUI runs typed commands on its own thread — and one that needs a
;;;; quiet session and finds a busy one is refused with `busy`, never raced.
;;;;
;;;; Reads never queue behind a run: /health answers from its own slot,
;;;; /snapshot and /items read the published view under a lock, /stream reads
;;;; the op log.  Nothing in this server polls with a sleep.

(in-package :evo.serve)

;; The route table lives with the routes (routes.lisp), which loads after this
;; file; SERVER-ROUTES reads it at dispatch time.
(declaim (special *routes* *routes-lock*))

;;; Errors: an op's failure is data in a 200 reply (CONTRACT §5.5), never an
;;; HTTP status, so a client branches on the code alone.

(define-condition op-error (error)
  ((code :initarg :code :reader op-error-code)
   (message :initarg :message :reader op-error-message)
   (detail :initarg :detail :initform nil :reader op-error-detail))
  (:report (lambda (c s) (format s "~a: ~a" (op-error-code c) (op-error-message c)))))

(defun op-fail (code control &rest args)
  "Refuse the running op with CODE and a fixed MESSAGE.  Messages never quote
a value the caller supplied: an error cannot leak a secret that way."
  (error 'op-error :code code :message (apply #'format nil control args)))

;;; Tokens and binding.

(defun resolve-token ()
  "The bearer token: EVO_SERVE_TOKEN when set (a supervisor restart, or a
caller that chose its own), else 32 fresh random bytes as hex."
  (let ((env (getenv "EVO_SERVE_TOKEN")))
    (if (plusp (length env))
        env
        (format nil "~(~{~2,'0x~}~)" (coerce (evo.port:random-octets 32) 'list)))))

(defun loopback-host-p (host)
  "True when HOST names this machine only."
  (let ((host (string-downcase (string-trim "[]" host))))
    (or (string= host "localhost")
        (string= host "::1")
        (string= host "0:0:0:0:0:0:0:1")
        (string-prefix-p "127." host))))

(defun token-equal-p (given expected)
  "Compare without an early exit, so the time taken does not say how much of
a guess was right."
  (and (stringp given)
       (= (length given) (length expected))
       (zerop (loop for a across given
                    for b across expected
                    sum (logxor (char-code a) (char-code b))))))

;;; The server.

(defstruct (task (:constructor make-task (&key id kind)))
  "The session's one task (a run or a compaction).  Owned by the session
thread; forgotten only after its thread is joined.  Times are epoch
milliseconds, as the protocol reports them."
  id kind thread (started (op-now-ms)) (step-started nil))

(defparameter *identity* (list :name "evo-agent" :version "0.1.0" :features nil)
  "Who this program is, as GET /health and GET /catalog report it.  The
default names the agent, the base program; a program built on this server sets
it — or passes :IDENTITY to MAKE-SERVER — so a client can tell which program
answered (evo-swarm names itself \"evo-swarm\").")

(defstruct (server (:constructor %make-server))
  host port token
  ;; Who serves this session, and the extra routes it serves.  NIL means only
  ;; the program-wide defaults — *IDENTITY* and *ROUTES*.
  identity-override routes-override
  agent
  ;; The op log: the protocol's one source of ordering (oplog.lisp).
  (oplog (make-op-log))
  ;; Topic providers, by name ("session", "swarm", "lane:1").
  (topics (make-hash-table :test #'equal))
  (topics-lock (bt:make-lock "serve-topics"))
  ;; Messages to the session thread.  STOPPING is written once, under the
  ;; same lock, when the server shuts down; the condition variable is what
  ;; the session thread waits on (never a sleep).
  (inbox nil) (inbox-lock (bt:make-lock "serve-inbox"))
  (inbox-cv (bt:make-condition-variable :name "serve-inbox"))
  (stopping nil)
  ;; Session-thread state.
  task quit (loop-tick (op-now-ms))
  ;; Queued input serve minted an id for: id -> (:text … :queue …).  The
  ;; kernel's own ids replace this the day CANCEL-QUEUED lands (CONTRACT §3).
  (queued (make-hash-table :test #'equal)) (queued-lock (bt:make-lock "serve-queued"))
  ;; Idempotency: the last 256 op replies by rid (CONTRACT §5.5).
  (rid-lock (bt:make-lock "serve-rid"))
  (rid-replies (make-hash-table :test #'equal))
  (rid-order nil)
  ;; Capabilities and hooks a program on top of this server fills.
  (eval-enabled t)
  ;; (lambda (server scope lane) -> list of topic names) — evo-swarm's swarm
  ;; and lane interrupt scopes; INTERRUPT-SCOPE's default only knows :session.
  interrupt-hook
  ;; (lambda (server)) — run on shutdown, before the session ends (a swarm
  ;; stops its lanes and supervisor here).
  shutdown-hook
  ;; Launch plumbing (CONTRACT §1): where the ready file goes, and whether
  ;; stdin EOF means the parent is gone.
  ready-file watch-stdin
  (started-at (op-now-ms))
  flusher-thread
  ;; Listener state: the socket, its thread, and the connection threads it
  ;; started (guarded by CONNECTIONS-LOCK).
  listener listener-thread
  (connections nil) (connections-lock (bt:make-lock "serve-connections")))

(defun make-server (&key (host "127.0.0.1") (port 8421) token ready-file
                         (watch-stdin nil) identity routes (eval-enabled t))
  "A server.  IDENTITY says who this program is (see *IDENTITY*); ROUTES are
additional routes considered before the built-ins.  READY-FILE is where the
serving process publishes its port and token (CONTRACT §1); WATCH-STDIN makes
EOF on stdin a clean shutdown.  EVAL-ENABLED is the --no-http-eval gate."
  (%make-server :host host :port port :token (or token (resolve-token))
                :identity-override identity
                :routes-override routes
                :ready-file ready-file
                :watch-stdin watch-stdin
                :eval-enabled eval-enabled))

(defun server-identity (server)
  "Who the program serving this session says it is: the server's own identity,
or the program-wide *IDENTITY*."
  (or (server-identity-override server) *identity*))

(defun server-program (server)
  (or (getf (server-identity server) :name) evo.port:*program-name*))

(defun server-version (server)
  (or (getf (server-identity server) :version) "0.1.0"))

(defun server-epoch (server) (op-log-epoch (server-oplog server)))
(defun server-seq (server) (op-log-last-seq (server-oplog server)))
(defun server-cursor (server) (format-cursor (server-oplog server)))

(defun server-routes (server)
  "A snapshot of the server's additional routes followed by the program-wide
defaults.  A same path/method in the server's routes overrides the default."
  (bt:with-lock-held (*routes-lock*)
    (let ((extra (server-routes-override server)))
      (if extra
          (append (copy-list extra) (copy-list *routes*))
          (copy-list *routes*)))))

(defun stopping-p (server)
  (bt:with-lock-held ((server-inbox-lock server))
    (server-stopping server)))

;;; The inbox: messages for the session thread, and the condition variable
;;; that replaces the 20 ms sleep it used to poll with.

(defun post (server message)
  "Queue MESSAGE for the session thread and wake it.  Any thread.  NIL once
stopping."
  (bt:with-lock-held ((server-inbox-lock server))
    (unless (server-stopping server)
      (setf (server-inbox server) (append (server-inbox server) (list message)))
      (bt:condition-notify (server-inbox-cv server))
      t)))

(defun drain-inbox (server)
  (bt:with-lock-held ((server-inbox-lock server))
    (shiftf (server-inbox server) nil)))

(defun wait-for-inbox (server timeout)
  "Block until the session thread has something to do.  TIMEOUT is the
heartbeat tick — it is not a poll of anything: the loop's work is announced by
POST, and the tick only keeps the supervisor's staleness watchdog honest."
  (bt:with-lock-held ((server-inbox-lock server))
    (unless (or (server-inbox server) (server-stopping server))
      (bt:condition-wait (server-inbox-cv server) (server-inbox-lock server)
                         :timeout timeout))))

(defun wake-session (server)
  (bt:with-lock-held ((server-inbox-lock server))
    (bt:condition-notify (server-inbox-cv server))))

;;; Calls into the session thread.

(defstruct (promise (:constructor make-promise ()))
  (lock (bt:make-lock "serve-promise"))
  (cv (bt:make-condition-variable :name "serve-promise"))
  done value condition)

(defun fulfill (promise value &optional condition)
  (bt:with-lock-held ((promise-lock promise))
    (setf (promise-value promise) value
          (promise-condition promise) condition
          (promise-done promise) t)
    (bt:condition-notify (promise-cv promise))))

(defparameter *call-timeout* 600
  "Seconds a caller waits for the session thread before giving up.  Long
enough for any op that is not itself a turn; a wedged session thread answers
503 rather than hanging a connection for ever.")

(defun call-on-session (server fn)
  "Run FN on the session thread and return its value here.  A condition FN
signals is re-signalled in the caller.  503 once the server is stopping."
  (let ((promise (make-promise)))
    (unless (post server (list :call fn promise))
      (http-fail 503 "server is shutting down"))
    (bt:with-lock-held ((promise-lock promise))
      (loop until (or (promise-done promise)
                      (stopping-p server))
            do (bt:condition-wait (promise-cv promise) (promise-lock promise)
                                  :timeout *call-timeout*)
               (unless (promise-done promise)
                 (http-fail 503 "the session thread did not answer")))
      (when (promise-condition promise)
        (error (promise-condition promise)))
      (promise-value promise))))

;;; Idempotency: a retried rid gets the reply it already got, and nothing else
;;; happens.

(defparameter *rid-cache-size* 256)

(defun rid-lookup (server rid)
  (bt:with-lock-held ((server-rid-lock server))
    (gethash rid (server-rid-replies server))))

(defun rid-remember (server rid reply)
  (bt:with-lock-held ((server-rid-lock server))
    (unless (gethash rid (server-rid-replies server))
      (push rid (server-rid-order server)))
    (setf (gethash rid (server-rid-replies server)) reply)
    (loop while (> (length (server-rid-order server)) *rid-cache-size*)
          for oldest = (car (last (server-rid-order server)))
          do (remhash oldest (server-rid-replies server))
             (setf (server-rid-order server)
                   (butlast (server-rid-order server))))))

;;; The reply a command builds while it runs on the session thread.

(defstruct reply
  (status 200) (output nil) (data nil) (error nil) (choices nil))

(defvar *reply* nil
  "The reply of the command running on the session thread, or NIL — output
produced outside a command (a task finishing) goes to the view only.")

(defun reply-add-data (key value)
  (when *reply*
    (setf (getf (reply-data *reply*) key) value)))

;;; The server as the command layer's host.

(defmethod evo.command:host-agent ((server server)) (server-agent server))
(defmethod evo.command:host-running-p ((server server)) (and (server-task server) t))
(defmethod evo.command:host-start-run ((server server)) (start-run server))
(defmethod evo.command:host-start-compact ((server server) hint)
  (start-compact server hint))

(defmethod evo.command:host-say ((server server) text &optional (style :plain))
  (when *reply*
    (push (list :style style :text text) (reply-output *reply*)))
  (server-notice server text :source :command :severity (case style
                                                          (:error :error)
                                                          (:notice :info)
                                                          (t :info))))

(defmethod evo.command:host-choose ((server server) title items action &key (index 0))
  "No picker over HTTP: the choices come back as data, and the same command
takes one as its argument."
  (declare (ignore action))
  (let ((rows (loop for item in items
                    collect (if (consp (cdr item))
                                (list :label (first item) :value (second item)
                                      :description (third item))
                                (list :label (car item) :value (cdr item))))))
    (when *reply*
      (setf (reply-choices *reply*) (list :title title :index index :items rows)))
    (evo.command:host-notice server
                             (format nil "~a~%~{  ~a~%~}" title
                                     (loop for row in rows
                                           collect (format nil "~a~@[  ~a~]"
                                                           (getf row :label)
                                                           (getf row :description)))))))

(defmethod evo.command:host-set-draft ((server server) text)
  ;; The layer reports the text as :draft data; nothing to paint here.
  (declare (ignore text))
  nil)

(defmethod evo.command:host-session-switched ((server server))
  "A journal switch is not an append: the topic is reset and the client
re-reads its snapshot (CONTRACT §5.3)."
  (topic-reset server "session" "session_switched"))

(defmethod evo.command:host-command-context ((server server)) (list :server server))
(defmethod evo.command:host-interrupt-hint ((server server)) "interrupt it first")
(defmethod evo.command:host-data ((server server) key value) (reply-add-data key value))

(defmethod evo.command:host-command-failed ((server server) name condition)
  (when *reply*
    (setf (reply-status *reply*) 422
          (reply-error *reply*) (format nil "/~a: ~a" name condition)))
  (evo.command:host-notice server (format nil "✗ /~a: ~a" name condition) :severity :error))

(defmethod evo.command:host-refresh ((server server))
  "The fold changed under the session.  Most changes are journal appends, and
the view hears those itself; a leaf that moved appends nothing and needs a
rebuild, which is what the sync is for."
  (sync-session-topic server))

;;; The server as the session's frontend.

(defmethod frontend-interactive-p ((server server)) nil)

(defmethod frontend-request-run ((server server) &key text)
  (post server (list :run-requested text)))

;;; Notices.  A notice is an item in the view (CONTRACT §4.1), not a line in a
;;; log a client would have to parse: durable notices are journaled by the
;;; kernel, and every notice is published to the view.

(defun server-notice (server text &key (severity :info) (source :serve) durable (data nil))
  (when (and (stringp text) (plusp (length text)))
    (topic-notice server (or (topic-name-for-notice server) "session")
                  text :severity severity :source source :durable durable :data data)))

;;; Tasks (session thread only).

(defun model-ready-p (server)
  "The model gate, as the TUI has it: T when the effective model resolves;
otherwise say why, and leave queued input queued."
  (let ((agent (server-agent server)))
    (handler-case (progn (effective-model (fold-state (agent-journal agent)) agent) t)
      (error (e)
        (evo.command:host-say server (format nil "✗ ~a" e) :error)
        (evo.command:host-say server "recover with /model <id> (a registered model)" :dim)
        (when (steering-pending-p agent)
          (evo.command:host-notice server "input stays queued — it runs once the model resolves"))
        nil))))

(defun spawn-task (server kind body)
  "Publish a new task of KIND and start its thread running BODY, a function
of no arguments returning (values outcome text).  :WORKER-DONE always
arrives, whatever BODY does."
  (let* ((agent (server-agent server))
         (task (make-task :id (format nil "task_~a" (gen-id 4)) :kind kind))
         (id (task-id task)))
    (reset-agent-run-control agent)
    (setf (server-task server) task)
    (setf (task-thread task)
          (bt:make-thread
           (lambda ()
             (let ((outcome :error) (text nil))
               (unwind-protect
                    (handler-case (multiple-value-setq (outcome text) (funcall body))
                      (serious-condition (e)
                        (setf outcome :error text (format nil "~a" e))))
                 (post server (list :worker-done id outcome text)))))
           :name (format nil "evo-~(~a~)" kind)))
    task))

(defun start-run (server)
  "Start a run for the queued input unless a task is already running."
  (unless (server-task server)
    (when (model-ready-p server)
      (let ((agent (server-agent server)))
        (spawn-task server :run (lambda () (run-until-settled agent)))))))

(defun start-compact (server hint)
  (unless (server-task server)
    (when (model-ready-p server)
      (let ((agent (server-agent server)))
        (spawn-task server :compact
                    (lambda ()
                      (emit-event agent :type :compaction-start)
                      (unwind-protect
                           (handler-case
                               (progn (compact-now agent :hint hint :manual t)
                                      (if (agent-abort-flag agent) :aborted :stop))
                             (error (e)
                               (if (agent-abort-flag agent)
                                   :aborted
                                   (values :error (format nil "~a" e)))))
                        (emit-event agent :type :compaction-end))))))))

(defun finish-task (server id outcome text)
  "A task's thread is done: join it, forget the task, report, and start the
next run if input queued up meanwhile."
  (let ((task (server-task server))
        (agent (server-agent server)))
    (when (and task (equal id (task-id task)))
      (ignore-errors (bt:join-thread (task-thread task)))
      ;; Nobody owns the mailbox now: an :abort the run never consumed would
      ;; read as pending work and wedge quiescence.
      (reset-agent-run-control agent)
      (setf (server-task server) nil)
      (when (eq (task-kind task) :compact)
        (case outcome
          (:stop (evo.command:host-notice server "✓ compacted"
                                          :durable t :data (list :source :serve)))
          (:aborted (evo.command:host-notice server "✗ compact interrupted" :severity :warn))
          (t (evo.command:host-notice server (format nil "✗ compact: ~a" text)
                                      :severity :error
                                      :durable t :data (list :source :serve)))))
      (when (and (eq (task-kind task) :run) text)
        (evo.command:host-say server (format nil "✗ internal error in run: ~a" text) :error))
      (let ((goal (current-goal agent)))
        (when (and goal (member (pget goal :status) '(:complete :budget-limited :paused)))
          (evo.command:host-notice server (format nil "◆ goal ~a: ~(~a~)"
                                                  (pget goal :goal-id) (pget goal :status))
                                   :durable t :data (list :source :goal))))
      (when (and (steering-pending-p agent) (not (server-quit server)))
        (start-run server)))))

;;; The session thread.

(defun handle-message (server message)
  (destructuring-bind (kind &rest args) message
    (ecase kind
      (:call
       (destructuring-bind (fn promise) args
         (handler-case (fulfill promise (funcall fn))
           (serious-condition (e) (fulfill promise nil e)))))
      (:worker-done (apply #'finish-task server args))
      (:step
       (let ((task (server-task server)))
         (when task (setf (task-step-started task) (op-now-ms)))))
      (:run-requested
       (let ((text (first args)))
         (when text
           (log-user-input server text))
         (start-run server)))
      (:stdin-eof
       (server-notice server "stdin closed — shutting down" :severity :info
                                                        :source :serve)
       (setf (server-quit server) t)))))

(defun session-loop (server)
  (loop until (server-quit server)
        do (heartbeat-touch)
           (setf (server-loop-tick server) (op-now-ms))
           (dolist (message (drain-inbox server))
             (handler-case (handle-message server message)
               (serious-condition (e)
                 (ignore-errors
                   (say-event server (format nil "✗ serve error: ~a" e) :error)))))
           ;; Sleep until POST announces work; the timeout is only the
           ;; heartbeat tick.  Without this wait the loop spins a core.
           (unless (server-quit server)
             (wait-for-inbox server 1))))

(defun events-callback (server)
  "The agent's events callback: every kernel event into the view, on the
worker's own thread; a step boundary also tells the session thread, which
owns the task's step clock."
  (lambda (event)
    (topic-on-event server event)
    (when (member (getf event :type) '(:turn-start :compaction-start :compaction-end))
      (post server (list :step)))))

(defun say-event (server text style)
  "A line the session says outside any command: a notice in the view."
  (server-notice server text :severity (if (eq style :error) :error :info)
                             :source :serve))

;;; Serving.

(defun write-json (stream status body)
  (write-response stream status (encode-json body)))

(defun write-error (stream status text)
  (write-json stream status (list :ok 'false :error text)))

(defun register-connection (server thread)
  (bt:with-lock-held ((server-connections-lock server))
    (push thread (server-connections server))
    (setf (server-connections server)
          (remove-if-not #'bt:thread-alive-p (server-connections server)))))

(defun handle-connection (server socket)
  (let ((stream (usocket:socket-stream socket)))
    (unwind-protect
         ;; A peer that connects and says nothing does not get a thread for
         ;; life.
         (when (usocket:wait-for-input socket :timeout 30 :ready-only t)
           (handler-case
               (let ((request (read-request stream)))
                 (when request (respond server request stream)))
             (http-error (e)
               (ignore-errors (write-error stream (http-error-status e) (http-error-text e))))
             (stream-error () nil)
             (error (e)
               (ignore-errors (write-error stream 500 (format nil "~a" e))))))
      (ignore-errors (finish-output stream))
      (ignore-errors (usocket:socket-close socket)))))

(defun accept-loop (server)
  (let ((listener (server-listener server)))
    (loop until (stopping-p server)
          do (handler-case
                 (when (usocket:wait-for-input listener :timeout 0.5 :ready-only t)
                   (let ((socket (usocket:socket-accept
                                  listener :element-type '(unsigned-byte 8))))
                     (register-connection
                      server
                      (bt:make-thread (lambda () (handle-connection server socket))
                                      :name "evo-serve-connection"))))
               (error (e)
                 (unless (stopping-p server)
                   (ignore-errors
                     (say-event server (format nil "✗ accept: ~a" e) :error))))))))

(defun open-listener (server)
  "Bind the listening socket; record the port actually bound (--port 0)."
  (let ((listener (usocket:socket-listen (server-host server) (server-port server)
                                         :reuse-address t :backlog 32
                                         :element-type '(unsigned-byte 8))))
    (setf (server-listener server) listener
          (server-port server) (usocket:get-local-port listener))
    listener))

(defun announce-startup (server resumed-p)
  "What the TUI does when it comes up: an active goal picks itself back up,
and a model that does not resolve is said now rather than at the first op."
  (let* ((agent (server-agent server))
         (goal (current-goal agent)))
    (cond ((and goal (eq (pget goal :status) :active))
           (queue-steering agent (goal-continuation-for agent goal)
                           :origin (goal-origin goal :continue))
           (start-run server))
          (t (model-ready-p server)))
    (server-notice server (if resumed-p "session resumed" "session ready")
                   :severity :info :source :serve)))

;;; The ready file (CONTRACT §1): the one place a client learns the port and
;;; the token, written atomically so a reader never sees half of it.

(defun ready-file-body (server)
  "What the ready file says.  SUPERVISOR_PID and RESTARTS come from the
supervisor's environment, and are absent when this process is its own
parent."
  (let* ((agent (server-agent server))
         (journal (and agent (agent-journal agent))))
    (list :epoch (server-epoch server)
          :pid (evo.port:getpid)
          :supervisor-pid (ignore-errors
                            (parse-integer (getenv "EVO_SUPERVISOR_PID")))
          :port (server-port server)
          :url (format nil "http://~a:~d/" (server-host server) (server-port server))
          :token (server-token server)
          :session (and journal
                        (list :id (pget (journal-header journal) :id)
                              :path (namestring (journal-path journal))))
          :program (server-program server)
          :version (server-version server)
          :restarts (or (ignore-errors
                          (parse-integer (getenv "EVO_RESTARTS") :junk-allowed t))
                        0))))

(defun write-ready-file (server)
  "Write the ready file atomically: a temp file in the same directory, mode
0600, then a rename — which is atomic, so a reader either sees the old file
or the complete new one."
  (let* ((path (merge-pathnames (server-ready-file server)))
         (tmp (make-pathname :name (format nil ".~a.tmp~a" (pathname-name path)
                                           (gen-id 4))
                             :defaults path)))
    (ensure-directories-exist path)
    (evo.port:write-private-file tmp (encode-json (ready-file-body server)))
    (rename-file tmp path)
    path))

(defun delete-ready-file (server)
  (when (server-ready-file server)
    (ignore-errors (delete-file (merge-pathnames (server-ready-file server))))))

;;; stdin: EOF means the parent that started us is gone (CONTRACT §1).

(defun watch-stdin-loop (server)
  (loop for char = (read-char *standard-input* nil :eof)
        until (eq char :eof))
  (post server (list :stdin-eof)))

;;; The op flusher: one thread, which waits for the next coalescing deadline
;;; instead of polling for it.

(defun op-flusher-loop (server)
  (let ((log (server-oplog server)))
    (loop until (stopping-p server)
          do (bt:with-lock-held ((op-log-lock log))
               (when (op-log-flush-expired log)
                 (bt:condition-notify (op-log-cv log)))
               (bt:condition-wait (op-log-cv log) (op-log-lock log)
                                  :timeout (op-log-wait-seconds log))))))

(defun shutdown-task (server &key (seconds 5))
  "Abort the task and reap it, draining the inbox for its :worker-done; any
call still waiting is answered 503.  T once no task is left."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (when (server-task server)
      (request-abort (server-agent server)))
    (loop while (and (server-task server)
                     (< (get-internal-real-time) deadline))
          do (dolist (message (drain-inbox server))
               (case (first message)
                 (:worker-done (ignore-errors (apply #'finish-task server (rest message))))
                 (:call (fulfill (third message) nil
                                 (make-condition 'http-error :status 503
                                                             :text "server is shutting down")))))
             (sleep 0.02))
    (not (server-task server))))

(defun close-connection-threads (server)
  "Streams see STOPPING, flush what is left and close; give them a moment."
  (loop repeat 50
        while (bt:with-lock-held ((server-connections-lock server))
                (some #'bt:thread-alive-p (server-connections server)))
        do (sleep 0.02)))

(defun stop-listening (server)
  (bt:with-lock-held ((server-inbox-lock server))
    (setf (server-stopping server) t))
  ;; Nothing will run what is still queued: answer every waiting call, wake
  ;; the session thread and every op waiter.
  (dolist (message (drain-inbox server))
    (when (eq (first message) :call)
      (fulfill (third message) nil
               (make-condition 'http-error :status 503 :text "server is shutting down"))))
  (wake-session server)
  (op-log-wake (server-oplog server))
  (ignore-errors (usocket:socket-close (server-listener server)))
  (when (server-listener-thread server)
    (ignore-errors (bt:join-thread (server-listener-thread server))))
  (when (server-flusher-thread server)
    (ignore-errors (bt:join-thread (server-flusher-thread server))))
  (close-connection-threads server)
  (delete-ready-file server))

(defun serve (server agent &key resumed-p)
  "Serve AGENT's session over HTTP until POST /ops server.shutdown — or, with
--watch-stdin, until stdin closes.  Returns the exit code: 0 after a clean
shutdown, 64 when the address cannot be bound (a usage error — restarting
would not free the port)."
  (setf (server-agent server) agent
        (agent-events-cb agent) (events-callback server))
  (handler-case (open-listener server)
    (error (e)
      (format *error-output* "~&~a serve: cannot listen on ~a:~a — ~a~%"
              (server-program server) (server-host server) (server-port server) e)
      (return-from serve 64)))
  (install-session-topic server agent)
  (when (server-ready-file server) (write-ready-file server))
  (setf (server-listener-thread server)
        (bt:make-thread (lambda () (accept-loop server)) :name "evo-serve-listener")
        (server-flusher-thread server)
        (bt:make-thread (lambda () (op-flusher-loop server)) :name "evo-serve-flusher"))
  (when (server-watch-stdin server)
    (bt:make-thread (lambda () (watch-stdin-loop server)) :name "evo-serve-stdin"))
  (format t "~&~a serve: listening on http://~a:~d/~@[ (ready file ~a)~]~%"
          (server-program server) (server-host server) (server-port server)
          (server-ready-file server))
  (finish-output)
  (unwind-protect
       (progn
         (announce-startup server resumed-p)
         (session-loop server))
    ;; The session is going away: extensions first, while it still owns what
    ;; they hold; then whatever the program on top of this server owns (a
    ;; swarm's lanes); then the task; then the doors.
    (when (server-shutdown-hook server)
      (ignore-errors (funcall (server-shutdown-hook server) server)))
    (ignore-errors (end-session agent))
    (ignore-errors (shutdown-task server))
    (stop-listening server))
  0)
