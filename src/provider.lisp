;;;; provider.lisp — the Anthropic Messages adapter, plus the provider
;;;; infrastructure both adapters share (JSON bridge, handoff pass, HTTP +
;;;; retry, dispatch).  The OpenAI Responses adapter is provider-openai.lisp.
;;;;
;;;; One unified message model; stateless replay (full history each request);
;;;; hand-rolled SSE; errors are data — this layer never signals into the
;;;; loop: failures become an assistant message with :stop-reason :error (or
;;;; :aborted) plus :error-message.

(in-package :evo.provider)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (evo.port:add-package-local-nickname :jzon :com.inuoe.jzon :evo.provider))

(define-condition provider-error (error)
  ((message :initarg :message :reader provider-error-message)
   (retryable :initarg :retryable :initform nil :reader provider-error-retryable-p)
   (retry-after :initarg :retry-after :initform nil :reader provider-error-retry-after))
  (:report (lambda (c s) (format s "~a" (provider-error-message c)))))

;;; JSON <-> sexpr bridging.
;;;
;;; Rule: JSON objects are plists (keyword keys, `_` <-> `-`), JSON arrays are
;;; CL vectors — never lists — so the two are unambiguous in the journal.

(defun key->keyword (string)
  (intern (substitute #\- #\_ (string-upcase string)) :keyword))

(defun keyword->key (keyword)
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun json->sexpr (value)
  (typecase value
    (hash-table (loop for k being the hash-keys of value using (hash-value v)
                      append (list (key->keyword k) (json->sexpr v))))
    (string value)
    (integer value)
    (double-float value)
    (real value)
    (vector (map 'vector #'json->sexpr value))
    (symbol (cond ((eq value t) t) (t nil)))  ; false/null -> nil
    (t nil)))

(defun sexpr->json (value)
  (typecase value
    (cons (let ((h (make-hash-table :test #'equal)))
            (loop for (k v) on value by #'cddr
                  do (setf (gethash (keyword->key k) h) (sexpr->json v)))
            h))
    (string value)
    (number value)
    (keyword (string-downcase (symbol-name value)))
    (vector (map 'vector #'sexpr->json value))
    (t value)))                         ; t/nil pass through as true/false

(defun jobj (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun jget (obj &rest keys)
  (let ((v obj))
    (dolist (k keys v)
      (setf v (and (hash-table-p v) (gethash k v))))))

;;; Handoff pass: run at request build time.
;;;  - errored/aborted assistant turns are elided
;;;  - same-model thinking replays verbatim; cross-model thinking is dropped
;;;  - orphaned tool calls get synthetic error results

(defun message-role (m) (pget m :role))
(defun message-content (m) (pget m :content))
(defun message-stop-reason (m) (pget m :stop-reason))
(defun message-usage (m) (pget m :usage))

(defun handoff-pass (messages target-model-id)
  (let* ((live (remove-if (lambda (m)
                            (and (eq (message-role m) :assistant)
                                 (member (message-stop-reason m) '(:error :aborted))))
                          messages))
         (result-ids (loop for m in live
                           when (eq (message-role m) :tool-result)
                             collect (pget m :tool-call-id))))
    (loop for m in live
          append
          (case (message-role m)
            (:assistant
             (let* ((same-model (equal (pget m :model) target-model-id))
                    (content (remove-if
                              (lambda (block)
                                (and (eq (pget block :type) :thinking)
                                     (not same-model)))
                              (message-content m)))
                    (orphans (loop for block in content
                                   when (and (eq (pget block :type) :tool-call)
                                             (not (member (pget block :id) result-ids
                                                          :test #'equal)))
                                     collect block)))
               (if (null content)
                   nil
                   (cons (pput m :content content)
                         (loop for call in orphans
                               collect (list :role :tool-result
                                             :tool-call-id (pget call :id)
                                             :tool-name (pget call :name)
                                             :is-error t
                                             :content (list (list :type :text
                                                                  :text "Tool execution was interrupted before completion; the call did not run. Re-issue it if still needed."))))))))
            (t (list m))))))

;;; Request building.

(defun content-block->json (block)
  (case (pget block :type)
    (:text (jobj "type" "text" "text" (pget block :text)))
    (:thinking (jobj "type" "thinking"
                     "thinking" (pget block :thinking)
                     "signature" (or (pget block :signature) "")))
    (:tool-call (jobj "type" "tool_use"
                      "id" (pget block :id)
                      "name" (pget block :name)
                      "input" (let ((args (pget block :arguments)))
                                (if args (sexpr->json args) (jobj)))))
    (:image (jobj "type" "text" "text" "[image omitted]"))
    (t (error 'provider-error :message (format nil "Unknown content block type ~s" (pget block :type))))))

(defun tool-result->json-block (m)
  (jobj "type" "tool_result"
        "tool_use_id" (pget m :tool-call-id)
        "is_error" (if (pget m :is-error) t nil)
        "content" (map 'vector #'content-block->json (message-content m))))

(defun messages->json (messages)
  "Convert unified messages to Anthropic wire messages.
Consecutive user/tool-result messages merge into a single user message."
  (let ((out nil))     ; list of (role . blocks-list), reversed
    (dolist (m messages)
      (ecase (message-role m)
        (:assistant
         (push (cons "assistant" (map 'list #'content-block->json (message-content m)))
               out))
        ((:user :tool-result)
         (let ((blocks (if (eq (message-role m) :tool-result)
                           (list (tool-result->json-block m))
                           (map 'list #'content-block->json (message-content m)))))
           (if (and out (equal (caar out) "user"))
               (setf (cdr (car out)) (append (cdr (car out)) blocks))
               (push (cons "user" blocks) out))))))
    (map 'vector
         (lambda (pair)
           (jobj "role" (car pair) "content" (coerce (cdr pair) 'vector)))
         (nreverse out))))

(defun add-cache-control (json-messages)
  "Mark the last block of the last message as a cache breakpoint."
  (let ((last-msg (and (plusp (length json-messages))
                       (aref json-messages (1- (length json-messages))))))
    (when last-msg
      (let ((content (gethash "content" last-msg)))
        (when (plusp (length content))
          (setf (gethash "cache_control" (aref content (1- (length content))))
                (jobj "type" "ephemeral"))))))
  json-messages)

(defun tools->json (tools)
  "TOOLS: list of (:name s :description s :input-schema hash-table)."
  (when tools
    (let ((v (map 'vector
                  (lambda (tl)
                    (jobj "name" (pget tl :name)
                          "description" (pget tl :description)
                          "input_schema" (pget tl :input-schema)))
                  tools)))
      (setf (gethash "cache_control" (aref v (1- (length v)))) (jobj "type" "ephemeral"))
      v)))

(defun build-request-json (&key model system messages tools thinking-level)
  (let* ((model-id (pget model :id))
         (budget (and (pget model :thinking) (thinking-budget thinking-level)))
         (req (jobj "model" model-id
                    "max_tokens" (model-max-output model)
                    "stream" t
                    "messages" (add-cache-control
                                (messages->json (handoff-pass messages model-id))))))
    (when system
      (setf (gethash "system" req)
            (vector (let ((b (jobj "type" "text" "text" system)))
                      (setf (gethash "cache_control" b) (jobj "type" "ephemeral"))
                      b))))
    (let ((jt (tools->json tools)))
      (when jt (setf (gethash "tools" req) jt)))
    (when budget
      (setf (gethash "thinking" req)
            (jobj "type" "enabled" "budget_tokens" budget)))
    (jzon:stringify req)))

;;; SSE parsing: hand-rolled; a stream ending without message_stop is a
;;; retryable error.  Tool arguments accumulate as partial JSON and are parsed
;;; at content_block_stop.

(defstruct sse-block type text thinking signature id name (input-json ""))

(defun parse-sse-stream (char-stream &key on-event abort-flag)
  "Parse an Anthropic Messages SSE stream.  Returns a plist:
 (:content blocks :stop-reason kw :usage plist :model str :stopped-p bool
  :aborted-p bool :error-message str-or-nil)"
  (let ((blocks (make-hash-table))     ; index -> sse-block
        (max-index -1)
        (raw-stop nil) (model nil) (stopped-p nil) (error-message nil)
        (in-tokens 0) (out-tokens 0) (cache-read 0) (cache-write 0)
        (event-type nil) (data-lines nil))
    (labels ((emit (&rest ev) (when on-event (funcall on-event ev)))
             (finish-block (idx)
               (declare (ignore idx)))
             (dispatch ()
               (when data-lines
                 (let* ((data (string-join (string #\Newline) (nreverse data-lines)))
                        (obj (ignore-errors (jzon:parse data)))
                        (type (or event-type (and obj (jget obj "type")))))
                   (setf event-type nil data-lines nil)
                   (when obj
                     (cond
                       ((equal type "message_start")
                        (let ((usage (jget obj "message" "usage")))
                          (when usage
                            (setf in-tokens (or (jget usage "input_tokens") 0)
                                  cache-read (or (jget usage "cache_read_input_tokens") 0)
                                  cache-write (or (jget usage "cache_creation_input_tokens") 0))))
                        (setf model (jget obj "message" "model"))
                        (emit :type :message-start))
                       ((equal type "content_block_start")
                        (let* ((idx (jget obj "index"))
                               (cb (jget obj "content_block"))
                               (cbtype (jget cb "type"))
                               (block (make-sse-block
                                       :type (cond ((equal cbtype "text") :text)
                                                   ((equal cbtype "thinking") :thinking)
                                                   ((equal cbtype "tool_use") :tool-call)
                                                   (t :unknown))
                                       :text "" :thinking ""
                                       :signature (or (jget cb "signature") "")
                                       :id (jget cb "id")
                                       :name (jget cb "name"))))
                          (setf (gethash idx blocks) block
                                max-index (max max-index idx))
                          (when (eq (sse-block-type block) :tool-call)
                            (emit :type :tool-call-start :name (sse-block-name block)
                                  :id (sse-block-id block)))))
                       ((equal type "content_block_delta")
                        (let* ((idx (jget obj "index"))
                               (block (gethash idx blocks))
                               (delta (jget obj "delta"))
                               (dtype (jget delta "type")))
                          (when block
                            (cond
                              ((equal dtype "text_delta")
                               (let ((s (jget delta "text")))
                                 (setf (sse-block-text block)
                                       (concatenate 'string (sse-block-text block) s))
                                 (emit :type :text-delta :text s)))
                              ((equal dtype "thinking_delta")
                               (let ((s (jget delta "thinking")))
                                 (setf (sse-block-thinking block)
                                       (concatenate 'string (sse-block-thinking block) s))
                                 (emit :type :thinking-delta :text s)))
                              ((equal dtype "signature_delta")
                               ;; chunked; must append
                               (setf (sse-block-signature block)
                                     (concatenate 'string (sse-block-signature block)
                                                  (or (jget delta "signature") ""))))
                              ((equal dtype "input_json_delta")
                               (setf (sse-block-input-json block)
                                     (concatenate 'string (sse-block-input-json block)
                                                  (or (jget delta "partial_json") ""))))))))
                       ((equal type "content_block_stop")
                        (finish-block (jget obj "index")))
                       ((equal type "message_delta")
                        (let ((sr (jget obj "delta" "stop_reason"))
                              (usage (jget obj "usage")))
                          (when (stringp sr) (setf raw-stop sr))
                          (when usage
                            ;; Some backends only report full usage here.
                            (setf out-tokens (or (jget usage "output_tokens") out-tokens))
                            (let ((in (jget usage "input_tokens")))
                              (when (and (integerp in) (plusp in)) (setf in-tokens in)))
                            (let ((cr (jget usage "cache_read_input_tokens")))
                              (when (integerp cr) (setf cache-read (max cache-read cr))))
                            (let ((cw (jget usage "cache_creation_input_tokens")))
                              (when (integerp cw) (setf cache-write (max cache-write cw)))))))
                       ((equal type "message_stop")
                        (setf stopped-p t))
                       ((equal type "error")
                        (setf error-message
                              (format nil "~a: ~a"
                                      (or (jget obj "error" "type") "error")
                                      (or (jget obj "error" "message") data))))
                       (t nil)))))))
      (loop for line = (read-line char-stream nil :eof)
            until (or (eq line :eof) stopped-p error-message)
            do (when (and abort-flag (funcall abort-flag))
                 (return-from parse-sse-stream
                   (list :aborted-p t :content nil :stop-reason :aborted
                         :usage (list :input in-tokens :output out-tokens
                                      :cache-read cache-read :cache-write cache-write))))
               (let ((line (string-right-trim '(#\Return) line)))
                 (cond ((zerop (length line)) (dispatch))
                       ((string-prefix-p "event:" line)
                        (setf event-type (string-trim " " (subseq line 6))))
                       ((string-prefix-p "data:" line)
                        (push (string-trim " " (subseq line 5)) data-lines))
                       (t nil))))
      (dispatch)
      ;; Materialize blocks in index order.
      (let ((content
              (loop for i from 0 to max-index
                    for block = (gethash i blocks)
                    when block
                      collect (ecase (sse-block-type block)
                                (:text (list :type :text :text (sse-block-text block)))
                                (:thinking (list :type :thinking
                                                 :thinking (sse-block-thinking block)
                                                 :signature (sse-block-signature block)))
                                (:tool-call
                                 (let* ((raw (sse-block-input-json block))
                                        (args (cond ((zerop (length raw)) nil)
                                                    (t (handler-case
                                                           (json->sexpr (jzon:parse raw))
                                                         (error () :parse-error))))))
                                   (append (list :type :tool-call
                                                 :id (sse-block-id block)
                                                 :name (sse-block-name block))
                                           (if (eq args :parse-error)
                                               (list :arguments nil :arguments-error
                                                     (truncate-string raw 2000))
                                               (list :arguments args)))))
                                (:unknown (list :type :text :text ""))))))
        (list :content content
              :model model
              :stopped-p stopped-p
              :error-message error-message
              :stop-reason (normalize-stop-reason raw-stop content)
              :usage (list :input in-tokens :output out-tokens
                           :cache-read cache-read :cache-write cache-write))))))

(defun normalize-stop-reason (raw content)
  "Normalize to :stop :length :tool-use :error :aborted.  Tool-use presence in
content wins (some proxies report end_turn alongside tool_use blocks)."
  (cond ((find :tool-call content :key (lambda (b) (pget b :type))) :tool-use)
        ((null raw) :stop)
        ((equal raw "end_turn") :stop)
        ((equal raw "stop_sequence") :stop)
        ((equal raw "pause_turn") :stop)
        ((equal raw "max_tokens") :length)
        ((equal raw "tool_use") :tool-use)
        ((equal raw "refusal") :error)
        (t (error 'provider-error
                  :message (format nil "Unknown stop reason ~s" raw)))))

;;; Cost accounting (rationals; per-MTok prices from the model table).

(defun compute-cost (model usage)
  (let ((cost (pget model :cost)))
    (/ (+ (* (pget usage :input 0) (pget cost :input 0))
          (* (pget usage :output 0) (pget cost :output 0))
          (* (pget usage :cache-read 0) (pget cost :cache-read 0))
          (* (pget usage :cache-write 0) (pget cost :cache-write 0)))
       1000000)))

(defun message-cost (message)
  (pget (message-usage message) :cost-usd 0))

(defun usage-total-tokens (usage)
  (+ (pget usage :input 0) (pget usage :output 0)
     (pget usage :cache-read 0) (pget usage :cache-write 0)))

;;; HTTP + retry: in-request retry honoring retry-after, exponential
;;; backoff with jitter; classification on HTTP status, not string matching.

(defun provider-config (provider-key)
  (let ((conf (pget (setting :providers) provider-key)))
    (ecase provider-key
      (:anthropic
       (list :base-url (or (pget conf :base-url) "https://api.anthropic.com")
             :api-key (or (pget conf :api-key)
                          (getenv (or (pget conf :api-key-env) "ANTHROPIC_API_KEY"))
                          "")))
      (:openai
       (list :base-url (or (pget conf :base-url) "https://api.openai.com")
             :api-key (or (pget conf :api-key)
                          (getenv (or (pget conf :api-key-env) "OPENAI_API_KEY"))
                          ""))))))

(defun url-host (url)
  (let* ((start (let ((p (search "://" url))) (if p (+ p 3) 0)))
         (end (or (position-if (lambda (c) (member c '(#\/ #\: #\?))) url
                               :start start)
                  (length url))))
    (subseq url start end)))

(defun proxy-bypass-p (host)
  "True when HOST must skip the proxy: loopback always, plus no_proxy /
NO_PROXY entries (comma-separated; an entry matches itself and its
subdomains; * matches everything)."
  (flet ((suffix-p (suffix s)
           (let ((n (- (length s) (length suffix))))
             (and (>= n 0) (string-equal suffix s :start2 n)))))
    (or (member host '("localhost" "127.0.0.1" "::1") :test #'string-equal)
        (let ((no-proxy (or (getenv "no_proxy") (getenv "NO_PROXY"))))
          (and no-proxy
               (loop for start = 0 then (1+ end)
                     for end = (or (position #\, no-proxy :start start)
                                   (length no-proxy))
                     for entry = (string-trim " " (subseq no-proxy start end))
                     thereis (and (plusp (length entry))
                                  (or (string= entry "*")
                                      (string-equal entry host)
                                      (suffix-p (if (char= (char entry 0) #\.)
                                                    entry
                                                    (concatenate 'string "." entry))
                                                host)))
                     until (= end (length no-proxy))))))))

(defun env-proxy (url)
  "Proxy for URL from the environment.  Passed explicitly on every
request: dexador's *default-proxy* only reads the UPPERCASE env vars, and
via a defvar evaluated at image build time — so in the shipped binary it
is stale on top of missing the lowercase Unix convention."
  (flet ((nonempty (name)
           (let ((v (getenv name)))
             (and v (plusp (length v)) v))))
    (let ((proxy (or (nonempty "HTTPS_PROXY") (nonempty "https_proxy")
                     (nonempty "HTTP_PROXY") (nonempty "http_proxy"))))
      (and proxy
           (not (proxy-bypass-p (url-host url)))
           proxy))))

(defun retryable-status-p (status)
  (or (member status '(408 409 429 425 529))
      (and status (<= 500 status 599))))

(defun retry-delay (attempt retry-after)
  "Seconds to sleep before retry ATTEMPT (0-based).  NIL = don't retry."
  (cond ((and retry-after (> retry-after 60)) nil) ; refuse silently-long delays
        (retry-after retry-after)
        (t (min 30 (+ (expt 2 attempt) (random 1.0))))))

(defun parse-retry-after (headers)
  (let ((v (and (hash-table-p headers) (gethash "retry-after" headers))))
    (typecase v
      (string (ignore-errors (parse-integer v :junk-allowed t)))
      (number v)
      (t nil))))

(defparameter *max-attempts* 4)

(defun call-provider (&key model system messages tools thinking-level cache-key
                           on-event abort-flag)
  "Make one streamed request.  ALWAYS returns an assistant message plist —
errors come back as data with :stop-reason :error/:aborted, never signals.
Dispatches on the model's :api tag.  CACHE-KEY (the session id) becomes
prompt_cache_key on OpenAI; Anthropic uses cache_control breakpoints instead."
  (let* ((model (if (consp model) model (find-model model)))
         (config (provider-config (pget model :provider)))
         (base-url (string-right-trim "/" (pget config :base-url)))
         (last-error nil))
    (multiple-value-bind (url headers body parser)
        (ecase (pget model :api)
          (:anthropic-messages
           (values (concatenate 'string base-url "/v1/messages")
                   `(("content-type" . "application/json")
                     ("x-api-key" . ,(pget config :api-key))
                     ("anthropic-version" . "2023-06-01"))
                   (build-request-json :model model :system system :messages messages
                                       :tools tools :thinking-level thinking-level)
                   #'parse-sse-stream))
          (:openai-responses
           ;; Late-bound: the builder/parser live in provider-openai.lisp,
           ;; which loads after this file.
           (values (concatenate 'string base-url "/v1/responses")
                   `(("content-type" . "application/json")
                     ("authorization" . ,(concatenate 'string "Bearer "
                                                      (pget config :api-key))))
                   (funcall 'build-responses-request-json
                            :model model :system system :messages messages
                            :tools tools :thinking-level thinking-level
                            :cache-key cache-key)
                   'parse-responses-sse-stream)))
      (flet ((finish (result)
               (let* ((usage (pget result :usage))
                      (usage (pput usage :cost-usd (compute-cost model usage))))
                 (append (list :role :assistant
                               :api (pget model :api)
                               :provider (pget model :provider)
                               :model (pget model :id)
                               :stop-reason (pget result :stop-reason)
                               :usage usage
                               :content (pget result :content))
                         (when (pget result :error-message)
                           (list :error-message (pget result :error-message))))))
             (error-message (text retryable)
               (list :role :assistant
                     :api (pget model :api) :provider (pget model :provider)
                     :model (pget model :id)
                     :stop-reason :error :error-message text
                     :retryable (and retryable t)
                     :usage (list :input 0 :output 0 :cache-read 0 :cache-write 0 :cost-usd 0)
                     :content nil)))
        (dotimes (attempt *max-attempts*
                          (error-message (format nil "Provider request failed after ~d attempts: ~a"
                                                 *max-attempts* last-error)
                                         t))
          (let ((outcome
                  (handler-case
                      (let ((stream (apply #'dex:post url
                                           :headers headers :content body
                                           :want-stream t :force-binary t
                                           :keep-alive nil
                                           :connect-timeout 15
                                           :read-timeout 600
                                           (let ((proxy (env-proxy url)))
                                             (when proxy (list :proxy proxy))))))
                        (unwind-protect
                             (let* ((chars (flexi-streams:make-flexi-stream
                                            stream :external-format :utf-8))
                                    (result (funcall parser chars :on-event on-event
                                                                  :abort-flag abort-flag)))
                               (cond
                                 ((pget result :aborted-p)
                                  (list :done (error-message "Aborted by user" nil)))
                                 ((pget result :error-message)
                                  (list :retry (pget result :error-message) nil))
                                 ((not (pget result :stopped-p))
                                  (list :retry "Stream ended without a terminal event (truncated response)" nil))
                                 (t (list :done (finish result)))))
                          (ignore-errors (close stream))))
                    (dexador.error:http-request-failed (e)
                      (let* ((status (dexador.error:response-status e))
                             (body (ignore-errors
                                    (let ((b (dexador.error:response-body e)))
                                      (if (streamp b)
                                          (ignore-errors
                                           (let ((fb (flexi-streams:make-flexi-stream
                                                      b :external-format :utf-8)))
                                             (with-output-to-string (s)
                                               (loop for line = (read-line fb nil)
                                                     while line do (write-line line s)))))
                                          (format nil "~a" b)))))
                             (text (format nil "HTTP ~a: ~a" status
                                           (truncate-string (or body "") 2000))))
                        (if (retryable-status-p status)
                            (list :retry text (parse-retry-after (dexador.error:response-headers e)))
                            (list :done (error-message text nil)))))
                    (provider-error (e)
                      (list :done (error-message (provider-error-message e) nil)))
                    (error (e)
                      (list :retry (format nil "~a: ~a" (type-of e) e) nil)))))
            (ecase (first outcome)
              (:done (return (second outcome)))
              (:retry
               (setf last-error (second outcome))
               (when (and abort-flag (funcall abort-flag))
                 (return (error-message "Aborted by user" nil)))
               (let ((delay (retry-delay attempt (third outcome))))
                 (cond ((null delay)
                        (return (error-message last-error t)))
                       ((< attempt (1- *max-attempts*))
                        (sleep delay))))))))))))
