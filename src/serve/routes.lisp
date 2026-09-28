;;;; routes.lisp — the HTTP surface: what each request does.
;;;;
;;;; GETs read state; POSTs are commands.  Every request carries the bearer
;;;; token.  A command runs on the session thread (CALL-ON-SESSION) inside a
;;;; fresh REPLY, and answers with it as JSON — or, when the client asks for a
;;;; stream (`"stream": true` in the body, or `Accept: text/event-stream`), as
;;;; an SSE stream: the reply first (event `result`), then every event from the
;;;; moment the command ran until the session settles.  docs/serve.md is the
;;;; reference for all of it.

(in-package :evo.serve)

(defparameter *routes*
  '(("/health" :get handle-health)
    ("/state" :get handle-state)
    ("/transcript" :get handle-transcript)
    ("/journal" :get handle-journal)
    ("/lore" :get handle-lore)
    ("/sessions" :get handle-sessions)
    ("/registry" :get handle-registry)
    ("/events" :get handle-events)
    ("/prompt" :post handle-prompt)
    ("/steer" :post handle-steer)
    ("/follow-up" :post handle-follow-up)
    ("/interrupt" :post handle-interrupt)
    ("/command" :post handle-command)
    ("/eval" :post handle-evaluate)
    ("/load-extension" :post handle-load-extension)
    ("/shutdown" :post handle-shutdown))
  "PATH, METHOD, handler name (a function of SERVER REQUEST BODY STREAM).")

(defun route-request (method path)
  "The handler for METHOD and PATH: (values HANDLER NIL), or (values NIL
STATUS) — 404 for an unknown path, 405 for a known path and the wrong method."
  (let ((route (assoc (string-right-trim "/" path) *routes* :test #'string=)))
    (cond ((null route) (values nil 404))
          ((string-equal method (symbol-name (second route)))
           (values (third route) nil))
          (t (values nil 405)))))

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
                     (error () (http-fail 400 "body is not valid JSON")))))
        (unless (listp value)
          (http-fail 400 "body must be a JSON object"))
        value))))

(defun respond (server request stream)
  (unless (authorized-p server request)
    (return-from respond
      (write-response stream 401 (encode-json (list :ok 'false :error "missing or wrong bearer token"))
                      :extra-headers '(("WWW-Authenticate" . "Bearer realm=\"evo\"")))))
  (multiple-value-bind (handler status) (route-request (request-method request)
                                                       (request-path request))
    (if handler
        (funcall handler server request (request-json request) stream)
        (write-error stream status (if (= status 404) "no such endpoint" "method not allowed")))))

(defun body-string (body key &key required)
  (let ((value (getf body key)))
    (cond ((stringp value) value)
          ((null value) (when required (http-fail 400 "missing \"~(~a~)\"" key)))
          (t (http-fail 400 "\"~(~a~)\" must be a string" key)))))

;;; Commands: run on the session thread, answer with the reply.

(defun stream-requested-p (request body)
  (or (eq (getf body :stream) t)
      (search "text/event-stream" (or (request-header request "accept") ""))))

(defun reply-body (reply cursor server)
  (list :ok (if (< (reply-status reply) 300) t 'false)
        :status (reply-status reply)
        :error (reply-error reply)
        :output (coerce (reverse (reply-output reply)) 'vector)
        :data (reply-data reply)
        :choices (reply-choices reply)
        :cursor cursor
        :task (task-plist (server-task server))))

(defun run-command (server fn)
  "Run FN on the session thread inside a fresh reply.  Returns (
STATUS BODY RUNNING-P) as a list.  A refusal becomes its HTTP status."
  (call-on-session
   server
   (lambda ()
     (let* ((*reply* (make-reply))
            (cursor (last-event-id (server-log server))))
       (handler-case (funcall fn)
         (evo.command:command-refused (c)
           (setf (reply-status *reply*)
                 (ecase (evo.command:command-refused-kind c)
                   (:conflict 409) (:invalid 400) (:not-found 404))
                 (reply-error *reply*) (evo.command:command-refused-text c))
           (say-event server (evo.command:command-refused-text c) :dim)))
       (list (reply-status *reply*)
             (reply-body *reply* cursor server)
             (and (server-task server) t))))))

(defun answer-command (server request body stream fn)
  "Run FN as a command and answer: JSON, or SSE through the run it caused."
  (destructuring-bind (status reply running) (run-command server fn)
    (if (stream-requested-p request body)
        (progn
          (write-sse-head stream)
          (write-sse-event stream "result" (encode-json reply))
          (when (and running (< status 300))
            (stream-events server stream (getf reply :cursor) :until-settled t)))
        (write-json stream status reply))))

(defvar *server* nil "The server whose command is running (session thread).")

(defun say (text &optional (style :plain))
  (evo.command:host-say *server* text style))

(defmacro define-command-route (name (server body) &body forms)
  "A POST handler whose FORMS run on the session thread as a command."
  (let ((request (gensym "REQUEST")) (stream (gensym "STREAM")))
    `(defun ,name (,server ,request ,body ,stream)
       (declare (ignorable ,body))
       (answer-command ,server ,request ,body ,stream
                       (lambda () (let ((*server* ,server)) ,@forms))))))

(defun request-images (body)
  "The :images of BODY as :image blocks: each {\"path\": ...} read from disk,
or {\"data\": base64, \"media_type\": ...} taken by value.  400 on a bad one —
running the prompt without the picture it asked about is worse than not."
  (let ((images (getf body :images)))
    (unless (or (null images) (vectorp images) (listp images))
      (http-fail 400 "\"images\" must be an array"))
    (loop for image across (coerce (or images #()) 'vector)
          collect (cond
                    ((and (listp image) (stringp (getf image :path)))
                     (multiple-value-bind (block reason)
                         (evo.media:attach-image-file (getf image :path))
                       (or block (http-fail 400 "image: ~a" reason))))
                    ((and (listp image) (stringp (getf image :data)))
                     (let* ((octets (handler-case (base64->octets (getf image :data))
                                      (error () (http-fail 400 "image data is not base64"))))
                            (sniffed (evo.media:sniff-media-type octets)))
                       (unless sniffed
                         (http-fail 400 "image data is not a png, jpeg, gif or webp"))
                       (evo.media:make-image-block
                        :data (getf image :data) :media-type sniffed
                        :name (or (getf image :name) "image")
                        :bytes (length octets) :source "http")))
                    (t (http-fail 400 "each image needs \"path\" or \"data\""))))))

(defun require-text-or-images (text images)
  (unless (or (plusp (length (or text ""))) images)
    (http-fail 400 "nothing to send: give \"text\" and/or \"images\"")))

(defun handle-prompt (server request body stream)
  "The user's turn, as typed into the TUI's editor and sent: it starts a run,
or — while one runs — lands at its next turn boundary."
  (let ((text (body-string body :text))
        (images (request-images body)))
    (require-text-or-images text images)
    (answer-command server request body stream
                    (lambda ()
                      (let ((*server* server))
                        (evo.command:host-submit server (or text "") images)
                        (reply-add-data :queued t))))))

(defun handle-steer (server request body stream)
  "Mid-run input: lands at the running task's next turn boundary.  409 when
nothing runs — POST /prompt is how a run starts."
  (let ((text (body-string body :text))
        (images (request-images body)))
    (require-text-or-images text images)
    (answer-command server request body stream
                    (lambda ()
                      (unless (server-task server)
                        (evo.command:refuse :conflict "no run to steer — POST /prompt starts one"))
                      (queue-steering (server-agent server) (or text "") :images images
                                                                        :from-user t)
                      (reply-add-data :queued t)))))

(defun handle-follow-up (server request body stream)
  "Input for after the run settles (the kernel's follow-up queue).  With no
run going it is simply the next prompt."
  (let ((text (body-string body :text :required t)))
    (answer-command server request body stream
                    (lambda ()
                      (if (server-task server)
                          (queue-followup (server-agent server) text)
                          (evo.command:host-submit server text))
                      (reply-add-data :queued t)))))

(define-command-route handle-interrupt (server body)
  (let ((task (server-task server)))
    (reply-add-data :interrupted (and task t))
    (if task
        (progn (request-abort (server-agent server))
               (say "✗ interrupting…" :dim))
        (say "nothing to interrupt" :dim))))

(defun command-text (body)
  (let ((text (body-string body :text))
        (name (body-string body :name))
        (args (body-string body :args)))
    (cond (text (if (string-prefix-p "/" (string-left-trim " " text))
                    text
                    (concatenate 'string "/" text)))
          (name (format nil "/~a~@[ ~a~]" (string-left-trim "/" name) args))
          (t (http-fail 400 "give \"text\" (\"/goal ...\") or \"name\" and \"args\"")))))

(defun handle-command (server request body stream)
  "A slash command, resolved exactly as the TUI resolves one."
  (let ((text (command-text body)))
    (answer-command server request body stream
                    (lambda ()
                      (let ((*server* server))
                        (unless (evo.command:dispatch-command server text)
                          (evo.command:refuse :not-found "unknown command /~a"
                                              (evo.command:parse-command text))))))))

(define-command-route handle-evaluate (server body)
  ;; `form`: exactly one sexpr, as /eval.  `code`: a body, as the eval tool.
  (let ((form-text (body-string body :form))
        (code (body-string body :code)))
    (multiple-value-bind (form reason)
        (cond (form-text (evo.eval:single-form form-text))
              (code (handler-case
                        (let ((forms (evo.eval::read-forms code)))
                          (if forms
                              (values (if (rest forms) (cons 'progn forms) (first forms)) nil)
                              (values nil "nothing to evaluate")))
                      (serious-condition (e) (values nil (format nil "unreadable code — ~a" e)))))
              (t (values nil "give \"form\" (one sexpr) or \"code\" (a body)")))
      (when reason (evo.command:refuse :invalid "~a" reason))
      (multiple-value-bind (values output condition) (evo.eval:eval-form form)
        (reply-add-data :values (mapcar #'evo.eval::print-value values))
        (reply-add-data :output output)
        (reply-add-data :result (evo.eval::format-result values output condition))
        (when condition
          (setf (reply-status *reply*) 422
                (reply-error *reply*) (format nil "~(~a~): ~a" (type-of condition) condition)))))))

(define-command-route handle-load-extension (server body)
  (let ((path (body-string body :path :required t)))
    (handler-case
        (let ((loaded (evo:load-extension path)))
          (reply-add-data :path (namestring loaded))
          (say (format nil "loaded ~a" (namestring loaded)) :dim))
      (error (e)
        (setf (reply-status *reply*) 422
              (reply-error *reply*) (format nil "~a" e))))))

(define-command-route handle-shutdown (server body)
  (setf (server-quit server) t)
  (say "shutting down" :dim))

;;; Reads.

(defun answer-read (server stream fn)
  (write-json stream 200 (call-on-session server fn)))

(defun handle-health (server request body stream)
  (declare (ignore request body))
  (write-json stream 200 (list :ok t :pid (evo.port:getpid)
                               :cursor (last-event-id (server-log server)))))

(defun handle-state (server request body stream)
  (declare (ignore request body))
  (answer-read server stream
               (lambda ()
                 (let* ((task (server-task server))
                        (agent (server-agent server)))
                   (list* :status (cond ((null task) :idle)
                                        ((eq (task-kind task) :compact) :compacting)
                                        (t :running))
                          :task (task-plist task)
                          :turn (evo.kernel::agent-turn-index agent)
                          :cursor (last-event-id (server-log server))
                          (evo.command:session-summary agent))))))

(defun strip-image-data (value)
  "VALUE with every image block's base64 :data dropped — the transcript names
its pictures; POST /eval can fetch one if it is really wanted."
  (cond ((and (plist-p value) (eq (getf value :type) :image) (getf value :data))
         (let ((copy (copy-list value)))
           (remf copy :data)
           (append copy (list :data-omitted t))))
        ((plist-p value)
         (loop for (k v) on value by #'cddr append (list k (strip-image-data v))))
        ((listp value) (mapcar #'strip-image-data value))
        ((and (vectorp value) (not (stringp value))) (map 'vector #'strip-image-data value))
        (t value)))

(defun query-limit (request)
  (let ((text (request-query-param request "limit")))
    (and text (or (ignore-errors (parse-integer text))
                  (http-fail 400 "limit must be an integer")))))

(defun handle-transcript (server request body stream)
  "The LLM context the next turn sends: the fold's messages."
  (declare (ignore body))
  (let ((limit (query-limit request)))
    (answer-read server stream
                 (lambda ()
                   (let ((messages (state-messages
                                    (fold-state (agent-journal (server-agent server))))))
                     (list :messages
                           (coerce (strip-image-data
                                    (if limit (last messages limit) messages))
                                   'vector)))))))

(defun handle-journal (server request body stream)
  "The journal: its header and the entries on the root→leaf path."
  (declare (ignore body))
  (let ((limit (query-limit request)))
    (answer-read server stream
                 (lambda ()
                   (let* ((journal (agent-journal (server-agent server)))
                          (path (and (journal-leaf-id journal) (entry-path journal))))
                     (list :path (namestring (journal-path journal))
                           :header (journal-header journal)
                           :leaf (journal-leaf-id journal)
                           :entries (coerce (strip-image-data
                                             (if limit (last path limit) path))
                                            'vector)))))))

(defun handle-lore (server request body stream)
  (declare (ignore request body))
  (answer-read server stream
               (lambda ()
                 (list :entries
                       (coerce (all-lore-entries
                                :state (fold-state (agent-journal (server-agent server))))
                               'vector)))))

(defun handle-sessions (server request body stream)
  "This directory's sessions, last worked in first — the /resume list; `n` is
what /resume <n> takes."
  (declare (ignore request body))
  (answer-read server stream
               (lambda ()
                 (let ((sessions (list-sessions)))
                   (list :current (namestring (journal-path (agent-journal (server-agent server))))
                         :sessions
                         (coerce (loop for s in sessions
                                       for (label path summary)
                                         in (evo.command:resume-select-items sessions)
                                       for n from 1
                                       collect (list :n n :path path :label label
                                                     :timestamp (getf s :timestamp)
                                                     :summary summary))
                                 'vector))))))

(defparameter *secret-words* '("key" "token" "secret" "password" "credential" "auth")
  "A setting or provider field whose name holds one of these is not shown.")

(defun secret-name-p (keyword)
  (let ((name (string-downcase (symbol-name keyword))))
    (some (lambda (word) (search word name)) *secret-words*)))

(defun without-secrets (plist)
  (loop for (k v) on plist by #'cddr
        unless (secret-name-p k) append (list k v)))

(defun registry-snapshot (agent)
  "What this session can use — models, providers, APIs, tools, commands,
skills, templates, languages, settings — with every secret left out.  A
provider says whether it HAS a key, never the key; the name of the variable
it reads one from is not secret and stays."
  (list :models (coerce (loop for m in (all-models)
                              collect (let ((copy (copy-list m)))
                                        (when (listp (getf copy :effort))
                                          (setf (getf copy :effort)
                                                (coerce (getf copy :effort) 'vector)))
                                        (without-secrets copy)))
                        'vector)
        :providers (coerce (loop for key in (provider-keys)
                                 for reg = (provider-registration key)
                                 collect (list :key key
                                               :base-url (getf reg :base-url)
                                               :api-key-env (getf reg :api-key-env)
                                               :has-api-key
                                               (and (or (plusp (length (or (getf reg :api-key) "")))
                                                        (plusp (length (or (and (getf reg :api-key-env)
                                                                                (getenv (getf reg :api-key-env)))
                                                                           ""))))
                                                    t)))
                           'vector)
        :apis (coerce (api-keys) 'vector)
        :tools (coerce (loop for name in (all-tool-names)
                             for tool = (find-tool name)
                             collect (list :name name
                                           :description (and tool (tool-description tool))))
                       'vector)
        :active-tools (coerce (mapcar #'tool-name
                                      (active-tools (fold-state (agent-journal agent))))
                              'vector)
        :commands (coerce (loop for (name . description) in (evo.command:command-catalog)
                                collect (list :name name :description description))
                          'vector)
        :skills (coerce (loop for s in (available-skills)
                              collect (list :name (getf s :name)
                                            :description (getf s :description)
                                            :path (princ-to-string (getf s :path))))
                        'vector)
        :templates (coerce (evo.command:template-names) 'vector)
        :languages (coerce (loop for p in (all-prompt-languages)
                                 collect (list :code (getf p :code) :name (getf p :name)
                                               :native (getf p :native)))
                           'vector)
        :settings (without-secrets (copy-list *settings*))))

(defun handle-registry (server request body stream)
  (declare (ignore request body))
  (answer-read server stream
               (lambda () (registry-snapshot (server-agent server)))))

;;; The event stream.

(defparameter *keepalive-seconds* 15)

(defun stream-events (server stream cursor &key until-settled)
  "Send the log's events after CURSOR to STREAM as SSE, and keep sending as
they arrive.  UNTIL-SETTLED ends the stream at the first `settled` event (the
session idle again); otherwise it runs until the client goes or the server
stops.  A cursor that fell out of the log gets a `gap` event first."
  (let ((log (server-log server))
        (last-write (get-universal-time)))
    (handler-case
        (loop
          (let ((stopping (stopping-p server)))
            (multiple-value-bind (events missed-from) (events-after log cursor)
              (when missed-from
                (write-sse-event stream "gap"
                                 (encode-json (list :type :gap :from missed-from
                                                    :to (1- (first (first events))))))
                (setf last-write (get-universal-time)))
              (dolist (event events)
                (destructuring-bind (id type json) event
                  (write-sse-event stream type json :id id)
                  (setf cursor id last-write (get-universal-time))
                  (when (and until-settled (string= type "settled"))
                    (return-from stream-events cursor))))
              (when (and stopping (null events))
                (return-from stream-events cursor))
              (when (> (- (get-universal-time) last-write) *keepalive-seconds*)
                (write-sse-comment stream "keepalive")
                (setf last-write (get-universal-time)))
              (unless events (sleep 0.05)))))
      ;; The client went away: that ends its stream, nothing else.
      (error () cursor))))

(defun handle-events (server request body stream)
  "GET /events: every session event as SSE.  Starts after Last-Event-ID (a
reconnect) or ?since=N; with neither, from now on — ?since=0 replays what the
log still holds."
  (declare (ignore body))
  (let* ((resume (or (request-header request "last-event-id")
                     (request-query-param request "since")))
         (cursor (if resume
                     (or (ignore-errors (parse-integer resume))
                         (http-fail 400 "Last-Event-ID / since must be an integer"))
                     (last-event-id (server-log server)))))
    (write-sse-head stream)
    (write-sse-comment stream (format nil "evo events after ~d" cursor))
    (stream-events server stream cursor)))
