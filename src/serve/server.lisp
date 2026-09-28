;;;; server.lisp — `evo serve`: one session, controlled over HTTP.
;;;;
;;;; Threads, and who owns what (design.md §6, §16.2):
;;;;
;;;;   session thread  the process's main thread.  Owns the task (the one run
;;;;                   or compaction), the agent's mailbox writes that start
;;;;                   work, every journal switch, every command.  It drains
;;;;                   an inbox; nothing else touches what it owns.
;;;;   listener        accepts connections, starts one thread per connection.
;;;;   connection      parses a request, checks the token, and hands the work
;;;;                   to the session thread as a closure in the inbox, then
;;;;                   waits for the answer — or streams the event log.
;;;;   run worker      the task's thread, as in the TUI: run-until-settled or
;;;;                   a manual compaction.  Its events go to the event log;
;;;;                   its completion goes to the inbox.
;;;;
;;;; So every command runs on the session thread, in arrival order, exactly as
;;;; the TUI runs typed commands on its own thread — and a command that needs a
;;;; quiet session and finds a busy one is refused with 409, never raced.

(in-package :evo.serve)

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
  "The session's one task (see the TUI's TUI-TASK): a run or a compaction.
Owned by the session thread; forgotten only after its thread is joined."
  id kind thread (started (get-universal-time)) (step-started nil))

(defparameter *identity* (list :name "evo" :version "0.1.0" :features nil)
  "Who this program is, as GET /health reports it: :NAME and :VERSION strings
and :FEATURES, a list of capability names a client may negotiate on.  The
default names this program; a program built on this server sets it — or passes
:IDENTITY to MAKE-SERVER — so a client can tell which program answered.")

(defstruct (server (:constructor %make-server))
  host port token token-file
  ;; Who serves this session, and the extra routes it serves.  NIL means only
  ;; the program-wide defaults — *IDENTITY* and *ROUTES*.  Extra routes are
  ;; considered before the defaults, so one server can extend (or deliberately
  ;; override) the protocol without losing its built-ins.
  identity-override routes-override
  agent
  (log (make-event-log))
  ;; Messages to the session thread, guarded by INBOX-LOCK.  STOPPING is
  ;; written once, under the same lock, when the session thread shuts down.
  (inbox nil) (inbox-lock (bt:make-lock "serve-inbox")) (stopping nil)
  ;; Session-thread state.
  task (quit nil)
  ;; Listener state: the socket, its thread, and the connection threads it
  ;; started (guarded by CONNECTIONS-LOCK).
  listener listener-thread
  (connections nil) (connections-lock (bt:make-lock "serve-connections"))
  (wrote-token-file nil))

;; The default route table lives with the routes (routes.lisp), which loads
;; after this file; SERVER-ROUTES reads it at dispatch time.
(declaim (special *routes*))

(defvar *routes-lock* (bt:make-lock "serve-routes")
  "Guards replacement and snapshots of the program-wide route table.")

(defun make-server (&key (host "127.0.0.1") (port 8421) token token-file
                         identity routes)
  "A server.  IDENTITY says who this program is (see *IDENTITY*); ROUTES are
additional routes considered before the built-ins (see *ROUTES*, ADD-ROUTE).
Program-wide routes added later are still seen."
  (%make-server :host host :port port :token (or token (resolve-token))
                :identity-override identity
                :routes-override routes
                :token-file token-file))

(defun server-identity (server)
  "Who the program serving this session says it is: the server's own identity,
or the program-wide *IDENTITY*."
  (or (server-identity-override server) *identity*))

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

(defun post (server message)
  "Queue MESSAGE for the session thread.  Any thread.  NIL once stopping."
  (bt:with-lock-held ((server-inbox-lock server))
    (unless (server-stopping server)
      (setf (server-inbox server) (append (server-inbox server) (list message)))
      t)))

(defun drain-inbox (server)
  (bt:with-lock-held ((server-inbox-lock server))
    (shiftf (server-inbox server) nil)))

(defun say-event (server text style)
  (publish (server-log server) (list :type :output :style style :text text)))

(defun server-publish (server event)
  "Append EVENT — a plist with :type, like the kernel's own events — to this
server's event log.  Every /events stream (and every client that reconnects
with Last-Event-ID) sees it exactly as it sees a session event: numbered, once,
in order.  Any thread.  Returns the event's id."
  (publish (server-log server) event))

(defun server-cursor (server)
  "The id of the newest event in this server's log: the cursor a client resumes
from — GET /events?since=<cursor> — and what a reply that starts work should
hand back with it."
  (last-event-id (server-log server)))

;;; Calls into the session thread.

(defstruct (promise (:constructor make-promise ()))
  (lock (bt:make-lock "serve-promise")) done value condition)

(defun fulfill (promise value &optional condition)
  (bt:with-lock-held ((promise-lock promise))
    (setf (promise-value promise) value
          (promise-condition promise) condition
          (promise-done promise) t)))

(defun call-on-session (server fn)
  "Run FN on the session thread and return its value here.  A condition FN
signals is re-signalled in the caller.  503 once the server is stopping."
  (let ((promise (make-promise)))
    (unless (post server (list :call fn promise))
      (http-fail 503 "server is shutting down"))
    (loop
      (bt:with-lock-held ((promise-lock promise))
        (when (promise-done promise)
          (when (promise-condition promise)
            (error (promise-condition promise)))
          (return (promise-value promise))))
      (sleep 0.005))))

;;; The reply a command builds while it runs on the session thread.

(defstruct reply
  (status 200) (output nil) (data nil) (error nil) (choices nil))

(defvar *reply* nil
  "The reply of the command running on the session thread, or NIL — output
produced outside a command (a task finishing) goes to the event log only.")

(defun reply-add-data (key value)
  (when *reply*
    (setf (getf (reply-data *reply*) key) value)))

(defun task-plist (task)
  (when task
    (let ((now (get-universal-time)))
      (list :id (task-id task) :kind (task-kind task)
            :started (task-started task)
            :age (- now (task-started task))
            ;; The step clock (design.md §16): the current turn or compaction,
            ;; not the whole task — how a slow step is told from a wedged one.
            :step-age (- now (or (task-step-started task) (task-started task)))))))

;;; The server as the command layer's host.

(defmethod evo.command:host-agent ((server server)) (server-agent server))
(defmethod evo.command:host-running-p ((server server)) (and (server-task server) t))
(defmethod evo.command:host-start-run ((server server)) (start-run server))
(defmethod evo.command:host-start-compact ((server server) hint)
  (start-compact server hint))

(defmethod evo.command:host-say ((server server) text &optional (style :plain))
  (when *reply*
    (push (list :style style :text text) (reply-output *reply*)))
  (say-event server text style))

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
    (evo.command:host-say server
                          (format nil "~a~%~{  ~a~%~}" title
                                  (loop for row in rows
                                        collect (format nil "~a~@[  ~a~]"
                                                        (getf row :label)
                                                        (getf row :description))))
                          :plain)))

(defmethod evo.command:host-set-draft ((server server) text)
  ;; The layer reports the text as :draft data; nothing to paint here.
  (declare (ignore text))
  nil)

(defmethod evo.command:host-session-switched ((server server))
  (publish (server-log server)
           (list :type :session-switched
                 :session (namestring (journal-path (agent-journal (server-agent server)))))))

(defmethod evo.command:host-command-context ((server server)) (list :server server))
(defmethod evo.command:host-interrupt-hint ((server server)) "POST /interrupt")
(defmethod evo.command:host-data ((server server) key value) (reply-add-data key value))

(defmethod evo.command:host-command-failed ((server server) name condition)
  (when *reply*
    (setf (reply-status *reply*) 422
          (reply-error *reply*) (format nil "/~a: ~a" name condition)))
  (evo.command:host-say server (format nil "✗ /~a: ~a" name condition) :error))

;;; The server as the session's frontend.

(defmethod frontend-interactive-p ((server server)) nil)

(defmethod frontend-request-run ((server server) &key text)
  (post server (list :run-requested text)))

;;; Tasks (session thread only).

(defun model-ready-p (server)
  "The model gate, as the TUI has it: T when the effective model resolves;
otherwise say why and how to recover, and leave queued input queued."
  (let ((agent (server-agent server)))
    (handler-case (progn (effective-model (fold-state (agent-journal agent)) agent) t)
      (error (e)
        (evo.command:host-say server (format nil "✗ ~a" e) :error)
        (evo.command:host-say server "recover with /model <id> (a registered model) or register one: POST /eval (evo:register-model ...)" :dim)
        (when (steering-pending-p agent)
          (evo.command:host-say server "input stays queued — it runs once the model resolves" :dim))
        nil))))

(defun spawn-task (server kind body)
  "Publish a new task of KIND and start its thread running BODY, a function
of no arguments returning (values outcome text).  :WORKER-DONE always
arrives, whatever BODY does."
  (let* ((agent (server-agent server))
         (task (make-task :id (gen-id) :kind kind))
         (id (task-id task)))
    (reset-agent-run-control agent)
    (setf (server-task server) task)
    (publish (server-log server) (list :type :task-start :task-id id :kind kind))
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
  "Start a run for the queued steering unless a task is already running."
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
                               (progn (compact-now agent :hint hint)
                                      (if (agent-abort-flag agent) :aborted :stop))
                             (error (e)
                               (if (agent-abort-flag agent)
                                   :aborted
                                   (values :error (format nil "~a" e)))))
                        (emit-event agent :type :compaction-end))))))))

(defun finish-task (server id outcome text)
  "A task's thread is done: join it, forget the task, report, and start the
next run if input queued up meanwhile — or announce the session settled."
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
          (:stop (evo.command:host-say server "✓ compacted" :success))
          (:aborted (evo.command:host-say server "✗ compact interrupted" :dim))
          (t (evo.command:host-say server (format nil "✗ compact: ~a" text) :error))))
      (when (and (eq (task-kind task) :run) text)
        (evo.command:host-say server (format nil "✗ internal error in run: ~a" text) :error))
      (publish (server-log server)
               (list :type :task-end :task-id id :kind (task-kind task)
                     :outcome outcome :error text))
      (let ((goal (current-goal agent)))
        (when (and goal (member (pget goal :status) '(:complete :budget-limited :paused)))
          (evo.command:host-say server (format nil "◆ goal ~a: ~(~a~)"
                                               (pget goal :goal-id) (pget goal :status))
                                :notice)))
      (when (and (steering-pending-p agent) (not (server-quit server)))
        (start-run server))
      (unless (server-task server)
        ;; The goal's status rides along: a settled lane whose goal is still
        ;; :active is not done — it errored or was stopped, and stays idle.
        (let ((goal (current-goal agent)))
          (publish (server-log server)
                   (list :type :settled :outcome outcome
                         :goal (and goal (pget goal :status)))))))))

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
         (when task (setf (task-step-started task) (get-universal-time)))))
      (:run-requested
       (let ((text (first args)))
         (when text
           (publish (server-log server) (list :type :user-input :text text)))
         (start-run server))))))

(defun events-callback (server)
  "The agent's events callback: every kernel event into the log, on the
worker's own thread; a step boundary also tells the session thread, which
owns the task's step clock."
  (let ((log (server-log server)))
    (lambda (event)
      (publish log event)
      (when (member (getf event :type) '(:turn-start :compaction-start :compaction-end))
        (post server (list :step))))))

(defparameter *watch-interval* 2
  "Seconds between checks that the watched process (EVO_SERVE_WATCH_PID) is
still alive.")

(defun watched-pid ()
  "The pid in EVO_SERVE_WATCH_PID, or NIL.  A process that started this
server to drive it — an evo-swarm coordinator — names itself here, so a
server whose driver died shuts itself down instead of idling forever."
  (let ((text (getenv "EVO_SERVE_WATCH_PID")))
    (and (plusp (length text)) (ignore-errors (parse-integer text)))))

(defun session-loop (server)
  (loop with watched = (watched-pid)
        with next-watch = (+ (get-universal-time) *watch-interval*)
        until (server-quit server)
        do (heartbeat-touch)
           (when (and watched (>= (get-universal-time) next-watch))
             (setf next-watch (+ (get-universal-time) *watch-interval*))
             (unless (evo.port:pid-alive-p watched)
               (say-event server (format nil "the driving process ~d is gone — shutting down"
                                         watched)
                          :dim)
               (setf (server-quit server) t)))
           (dolist (message (drain-inbox server))
             (handler-case (handle-message server message)
               (serious-condition (e)
                 (ignore-errors
                   (say-event server (format nil "✗ serve error: ~a" e) :error)))))
           (sleep 0.02)))

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
                   (let ((socket (usocket:socket-accept listener
                                                        :element-type '(unsigned-byte 8))))
                     (register-connection
                      server
                      (bt:make-thread (lambda () (handle-connection server socket))
                                      :name "evo-serve-connection"))))
               (error (e)
                 (unless (stopping-p server)
                   (ignore-errors
                     (say-event server (format nil "✗ accept: ~a" e) :error))
                   (sleep 0.1)))))))

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
and a model that does not resolve is said now rather than at the first prompt."
  (let* ((agent (server-agent server))
         (goal (current-goal agent)))
    (cond ((and goal (eq (pget goal :status) :active))
           (queue-steering agent (goal-continuation-for agent goal))
           (start-run server))
          (t (model-ready-p server))))
  (publish (server-log server)
           (list :type :ready :resumed (and resumed-p t))))

(defun stop-listening (server)
  (bt:with-lock-held ((server-inbox-lock server))
    (setf (server-stopping server) t))
  ;; Nothing will run what is still queued: answer every waiting call.
  (dolist (message (drain-inbox server))
    (when (eq (first message) :call)
      (fulfill (third message) nil
               (make-condition 'http-error :status 503 :text "server is shutting down"))))
  (ignore-errors (usocket:socket-close (server-listener server)))
  (when (server-listener-thread server)
    (ignore-errors (bt:join-thread (server-listener-thread server))))
  ;; Streams see STOPPING, flush what is left and close; give them a moment.
  (loop repeat 100
        while (bt:with-lock-held ((server-connections-lock server))
                (some #'bt:thread-alive-p (server-connections server)))
        do (sleep 0.02)))

(defun serve (server agent &key resumed-p)
  "Serve AGENT's session over HTTP until POST /shutdown.  Returns the exit
code: 0 after a clean shutdown, 64 when the address cannot be bound (a
usage error — restarting would not free the port)."
  (setf (server-agent server) agent
        (agent-events-cb agent) (events-callback server))
  (handler-case (open-listener server)
    (error (e)
      (format *error-output* "~&evo serve: cannot listen on ~a:~a — ~a~%"
              (server-host server) (server-port server) e)
      (return-from serve 64)))
  (when (server-token-file server)
    (evo.port:write-private-file (server-token-file server) (server-token server))
    (setf (server-wrote-token-file server) t))
  (publish (server-log server)
           (list :type :hello :pid (evo.port:getpid) :port (server-port server)
                 :session (namestring (journal-path (agent-journal agent)))))
  (setf (server-listener-thread server)
        (bt:make-thread (lambda () (accept-loop server)) :name "evo-serve-listener"))
  (format t "~&evo serve: listening on http://~a:~d/~@[ (token in ~a)~]~%"
          (server-host server) (server-port server) (server-token-file server))
  (finish-output)
  (unwind-protect
       (progn
         (announce-startup server resumed-p)
         (session-loop server))
    ;; The session is going away: extensions first, while it still owns what
    ;; they hold; then the task; then the doors.
    (publish (server-log server) (list :type :shutdown))
    (ignore-errors (end-session agent))
    (ignore-errors (shutdown-task server))
    (publish (server-log server) (list :type :bye))
    (stop-listening server)
    (when (server-wrote-token-file server)
      (ignore-errors (delete-file (server-token-file server)))))
  0)
