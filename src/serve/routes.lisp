;;;; routes.lisp — the HTTP surface: what each request does.
;;;;
;;;; Reads answer from the published state (a snapshot, an item page, the
;;;; catalog) and never queue behind a run; /stream is the op log; POST /ops
;;;; is the only write, and it always answers 200 with a code inside
;;;; (CONTRACT §5).  The old surface — /prompt, /steer, /command, /events,
;;;; streamed replies, `settled` — is deleted, not deprecated.

(in-package :evo.serve)

(defstruct (route (:constructor make-route (path method handler &key prefix)))
  "One entry in a route table.  PATH is matched exactly, or — when it is a
prefix route — as a prefix: \"/items/\" also takes \"/items/e_1\", and what
followed the prefix is bound to *ROUTE-TAIL* for the handler."
  path method handler (prefix nil))

(defun route-prefix-p (route)
  "T when ROUTE takes every path under its pattern rather than the whole path."
  (route-prefix route))

(defvar *route-tail* nil
  "What followed a prefix route's pattern in the request path: \"/items/e_1\"
through the route \"/items/\" is \"e_1\".  NIL outside a prefix route's
handler.")

(defparameter *routes*
  (list (make-route "/health" :get 'handle-health)
        (make-route "/snapshot" :get 'handle-snapshot)
        (make-route "/stream" :get 'handle-stream)
        (make-route "/items" :get 'handle-items)
        (make-route "/items/" :get 'handle-item :prefix t)
        (make-route "/media/" :get 'handle-media :prefix t)
        (make-route "/ops" :post 'handle-ops)
        (make-route "/catalog" :get 'handle-catalog)
        (make-route "/sessions" :get 'handle-sessions)
        (make-route "/debug/context" :get 'handle-debug-context)
        (make-route "/debug/journal" :get 'handle-debug-journal))
  "The program-wide route table: PATH, METHOD, handler (a function of SERVER
REQUEST BODY STREAM).  Every server dispatches through it, after any routes
passed to MAKE-SERVER :ROUTES; a program adds routes with ADD-ROUTE.")

(defvar *routes-lock* (bt:make-lock "serve-routes")
  "Guards replacement and snapshots of the program-wide route table.")

(defun add-route (path method handler &key prefix)
  "Add a route to the program-wide table.  METHOD (:GET or :POST) on PATH runs
HANDLER, a function designator of SERVER REQUEST BODY STREAM.  PREFIX makes it
a prefix route — \"/widgets/\" also takes \"/widgets/3\" — and its handler
reads the rest of the path from *ROUTE-TAIL*.  Re-adding a PATH and METHOD
replaces it.  Registration is safe while servers are running."
  (let ((route (make-route (string-right-trim "/" (string path)) method handler
                           :prefix prefix)))
    (bt:with-lock-held (*routes-lock*)
      (setf *routes*
            (cons route (remove-if (lambda (other)
                                     (and (string= (route-path other) (route-path route))
                                          (eq (route-method other) (route-method route))))
                                   *routes*))))
    route))

(defun route-match (route path)
  "Whether ROUTE takes PATH: (values T TAIL) — TAIL what followed a prefix
route's pattern, NIL for an exact one — or NIL.  A trailing slash is the same
path on either side."
  (let ((pattern (string-right-trim "/" (route-path route)))
        (path (string-right-trim "/" (string path))))
    (if (route-prefix-p route)
        (cond ((string= pattern path) (values t nil))
              ((and (plusp (length pattern))
                    (string-prefix-p (concatenate 'string pattern "/") path))
               (values t (subseq path (1+ (length pattern)))))
              (t nil))
        (when (string= pattern path) (values t nil)))))

(defun route-dispatch-order (routes)
  "ROUTES as dispatch tries them: exact matches in table order, then prefix
matches longest pattern first, so the most specific route wins."
  (append (remove-if #'route-prefix-p routes)
          (stable-sort (copy-list (remove-if-not #'route-prefix-p routes))
                       #'> :key (lambda (route) (length (route-path route))))))

(defun route-request (method path &optional (routes *routes*))
  "The handler for METHOD and PATH in ROUTES: (values HANDLER NIL TAIL), or
(values NIL STATUS NIL) — 404 for an unknown path, 405 for a known path and
the wrong method."
  (let ((matched nil))
    (dolist (route (route-dispatch-order routes))
      (multiple-value-bind (hit tail) (route-match route path)
        (when hit
          (setf matched t)
          (when (string-equal method (symbol-name (route-method route)))
            (return-from route-request (values (route-handler route) nil tail))))))
    (values nil (if matched 405 404) nil)))

(defun authorized-p (server request)
  (let ((header (request-header request "authorization")))
    (and header
         (string-prefix-p "Bearer " header)
         (token-equal-p (string-trim " " (subseq header 7)) (server-token server)))))

(defun request-json (request)
  "The request body as a plist; NIL for an empty body.  400 on bad JSON or a
body that is not an object."
  (let ((body (string-trim '(#\Space #\Tab #\Newline #\Return) (request-body request))))
    (when (plusp (length body))
      (let ((value (handler-case (decode-json body)
                     ;; A document the parser refuses for its size is not a
                     ;; malformed one: say which it is, so a client sending a
                     ;; picture it is allowed to send is not told its JSON is
                     ;; wrong (413 Content Too Large).
                     (com.inuoe.jzon:json-parse-limit-error ()
                       (http-fail 413 "request body is over the JSON parser's limit"))
                     (error () (http-fail 400 "body is not valid JSON")))))
        (unless (listp value)
          (http-fail 400 "body must be a JSON object"))
        value))))

(defun respond (server request stream)
  (unless (authorized-p server request)
    (return-from respond
      (write-response stream 401 (encode-json (list :ok 'false :error "missing or wrong bearer token"))
                      :extra-headers (list (cons "WWW-Authenticate"
                                                 (format nil "Bearer realm=~s"
                                                         (server-program server)))))))
  (when (and (eq (request-method request) :POST) (stopping-p server))
    (return-from respond (write-error stream 503 "server is shutting down")))
  (multiple-value-bind (handler status tail)
      (route-request (request-method request) (request-path request)
                     (server-routes server))
    (if handler
        (let ((*route-tail* tail))
          (funcall handler server request (request-json request) stream))
        (write-error stream status (if (= status 404) "no such endpoint" "method not allowed")))))

;;; Query parameters.

(defun query-integer (request name &key required default (min 0) max)
  (let ((text (request-query-param request name)))
    (cond
      ((null text)
       (when required (http-fail 400 "missing required query parameter"))
       default)
      (t (let ((n (ignore-errors (parse-integer text))))
           (unless (and n (>= n min) (or (null max) (<= n max)))
             (http-fail 400 "~(~a~) is out of range" name))
           n)))))

(defun query-topic (request)
  (or (request-query-param request "topic") "session"))

(defun query-topic-provider (server request)
  (let* ((name (query-topic request))
         (provider (topic-provider server name)))
    (unless provider (http-fail 404 "no such topic"))
    (values provider name)))

(defun wire-has-more (snapshot)
  "SNAPSHOT with its HAS-MORE key made a boolean."
  (setf (getf snapshot :has-more) (wire-boolean (getf snapshot :has-more)))
  snapshot)

;;; Reads.

(defun handle-health (server request body stream)
  "Report who this is and how it is doing — never by asking the session
thread, which may be inside a long operation (CONTRACT §5.1)."
  (declare (ignore request body))
  (write-json stream 200
              (list :ok t
                    :program (server-program server)
                    :version (server-version server)
                    :epoch (server-epoch server)
                    :pid (evo.port:getpid)
                    :supervisor-pid (ignore-errors
                                      (parse-integer (getenv "EVO_SUPERVISOR_PID")))
                    :restarts (or (ignore-errors
                                    (parse-integer (getenv "EVO_RESTARTS")
                                                   :junk-allowed t))
                                  0)
                    :started-at (server-started-at server)
                    ;; How long the session thread has been inside one
                    ;; operation: a wedged session shows up here, and nowhere
                    ;; else in /health.
                    :session-loop-age-ms (- (op-now-ms) (server-loop-tick server)))))

(defun handle-snapshot (server request body stream)
  "Every requested topic's state and newest items as of one seq (CONTRACT
§5.2).  `lane:*` means every lane."
  (declare (ignore body))
  (let* ((spec (or (request-query-param request "topics")
                   (format nil "~{~a~^,~}" (topic-provider-names server))))
         (names (expand-topic-names server spec))
         (items (query-integer request "items" :default 200 :min 0 :max 5000)))
    (multiple-value-bind (epoch seq snaps) (topics-snapshot server names items)
      (write-json stream 200
                  (list :epoch epoch :seq seq
                        :topics (json-object-value
                                 (loop for (name . snap) in snaps
                                       collect (cons name (wire-has-more snap)))))))))

(defun handle-items (server request body stream)
  "Older items of one topic, newest first among themselves (CONTRACT §5.4)."
  (declare (ignore body))
  (multiple-value-bind (provider name) (query-topic-provider server request)
    (let ((before (request-query-param request "before"))
          (limit (query-integer request "limit" :default 100 :min 1 :max 1000)))
      (multiple-value-bind (items more) (topic-items-before provider before limit)
        (write-json stream 200 (list :items items :has-more (wire-boolean more)
                                     :topic name))))))

(defun handle-item (server request body stream)
  "One item, whole — the thinking and the tool result as they were, not as the
snapshot bounded them."
  (declare (ignore body))
  (multiple-value-bind (provider name) (query-topic-provider server request)
    (let ((item (topic-item provider (or *route-tail* ""))))
      (unless item (http-fail 404 "no such item"))
      (write-json stream 200 (list :item item :topic name)))))

(defun handle-media (server request body stream)
  "The bytes of image N of an item, with its own media type."
  (declare (ignore body))
  (multiple-value-bind (provider name) (query-topic-provider server request)
    (declare (ignore name))
    (let* ((tail (or *route-tail* ""))
           (slash (position #\/ tail))
           (id (and slash (subseq tail 0 slash)))
           (n (and slash (ignore-errors (parse-integer tail :start (1+ slash))))))
      (unless (and id n (plusp (length id)))
        (http-fail 404 "no such media"))
      (multiple-value-bind (octets media-type) (topic-media provider id n)
        (unless octets (http-fail 404 "no such media"))
        (write-response-octets stream 200 octets (or media-type "application/octet-stream"))))))

(defun handle-catalog (server request body stream)
  (declare (ignore request body))
  (write-json stream 200 (catalog-for-server server (server-agent server))))

(defun handle-sessions (server request body stream)
  "The sessions on disk, newest first — the CLI's body and the GUI's resume
list (CONTRACT §2)."
  (declare (ignore server body))
  (let ((scope (or (request-query-param request "scope") "cwd")))
    (write-json stream 200
                (sessions-body :all (equal scope "all")
                               :cwd (or (request-query-param request "cwd")
                                        (uiop:getcwd))
                               :program (request-query-param request "program")))))

;;; Debug reads: what the model sees, and the raw journal.  For tools and
;;; tests, never for rendering.

(defun handle-debug-context (server request body stream)
  (declare (ignore request body))
  (write-json stream 200
              (call-on-session
               server
               (lambda ()
                 (let* ((agent (server-agent server))
                        (state (fold-state (agent-journal agent))))
                   (list :messages (coerce (state-messages state) 'vector)
                         :tokens (ignore-errors (estimate-context-tokens
                                                 (state-messages state)))))))))

(defun handle-debug-journal (server request body stream)
  (declare (ignore request body))
  (write-json stream 200
              (call-on-session
               server
               (lambda ()
                 (let* ((journal (agent-journal (server-agent server)))
                        (path (and (journal-leaf-id journal) (entry-path journal))))
                   (list :path (namestring (journal-path journal))
                         :header (journal-header journal)
                         :leaf (journal-leaf-id journal)
                         :entries (coerce path 'vector)))))))

;;; POST /ops.

(defun handle-ops (server request body stream)
  "Run one operation.  A malformed envelope is 400; everything the operation
itself decides is a 200 with `ok` and either `result` or `error` (CONTRACT
§5.5)."
  (declare (ignore request))
  (unless (listp body)
    (http-fail 400 "body must be a JSON object"))
  (let ((rid (getf body :rid))
        (op (getf body :op))
        (args (getf body :args)))
    (unless (and (stringp rid) (plusp (length rid)))
      (http-fail 400 "rid is required"))
    (unless (and (stringp op) (plusp (length op)))
      (http-fail 400 "op is required"))
    (write-json stream 200 (answer-op server rid op args))))

;;; The stream.

(defparameter *sse-ping-seconds* 15
  "How long a stream waits for an op before saying `: ping`.  The ping is a
comment a client ignores, and the write is also how a vanished peer is
noticed — never a poll of anything.")

(defun stream-patterns (spec)
  "The topics a stream asked for, as patterns: \"lane:*\" stays a pattern, so
a lane that appears later is still subscribed."
  (when (and spec (plusp (length spec)))
    (remove-if (lambda (name) (zerop (length name)))
               (mapcar (lambda (part) (string-trim '(#\Space #\Tab) part))
                       (uiop:split-string spec :separator ",")))))

(defun stream-wants-p (patterns topic)
  (or (null patterns)
      (some (lambda (pattern) (topic-name-matches-p pattern topic)) patterns)))

(defun write-sse-reset (stream reason)
  "Tell the client its cursor cannot be continued: it re-snapshots everything
and reconnects with the cursor from that snapshot (CONTRACT §5.3)."
  (write-sse-event stream "op"
                   (encode-json (list :op "stream.reset" :reason reason))))

(defun handle-stream (server request body stream)
  "Server-Sent Events of ops.  The first frame is always `hello`, so a client
knows the epoch it is reading and where the stream starts."
  (declare (ignore body))
  (let* ((log (server-oplog server))
         (patterns (stream-patterns (request-query-param request "topics")))
         (since-text (request-query-param request "since")))
    (multiple-value-bind (epoch seq) (op-log-cursor log)
      (let ((start seq) (reset nil))
        (when since-text
          (multiple-value-bind (since-epoch since)
              (parse-cursor since-text)
            (cond
              ((null since-epoch) (setf reset "cursor_unknown"))
              ((not (equal since-epoch epoch)) (setf reset "restarted"))
              ((> since seq) (setf reset "cursor_unknown"))
              ((< since (1- (op-log-oldest-seq log))) (setf reset "cursor_too_old"))
              (t (setf start since)))))
        (write-sse-head stream)
        (write-sse-event stream "op"
                         (encode-json (list :op "hello" :epoch epoch :seq start))
                         :id (format nil "~a.~d" epoch start))
        (if reset
            (progn (write-sse-reset stream reset)
                   (finish-output stream))
            (handler-case (stream-ops server stream patterns start)
              (error () nil)))
        (finish-output stream)))))

(defun stream-ops (server stream patterns since)
  "Write ops after SINCE until the client goes or the server stops.  Waiting is
on the op log's condition variable; the only timeout is the ping, and a client
that falls out of retention is told to reconnect rather than left waiting."
  (let ((log (server-oplog server))
        (epoch (server-epoch server)))
    (loop with cursor = since
          do (multiple-value-bind (ops missed unknown)
                 (op-log-wait log cursor *sse-ping-seconds*)
               (cond
                 ((or missed unknown)
                  (write-sse-reset stream (if unknown "cursor_unknown" "cursor_too_old"))
                  (return))
                 (t
                  (dolist (op ops)
                    (destructuring-bind (seq json topic) op
                      (setf cursor seq)
                      (when (stream-wants-p patterns topic)
                        (write-sse-event stream "op" json
                                         :id (format nil "~a.~d" epoch seq)))))
                  (when (null ops) (write-sse-comment stream "ping"))
                  (when (stopping-p server) (return))))))))
