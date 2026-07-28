;;;; core.lisp — provider infrastructure shared by all APIs: the JSON
;;;; bridge, the handoff pass, the SSE transport loop, HTTP + retry, and
;;;; API dispatch in call-provider.  The wire adapters live in
;;;; anthropic.lisp and openai.lisp (protocol: api.lisp).
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

(defun usage-total-tokens (usage)
  (+ (pget usage :input 0) (pget usage :output 0)
     (pget usage :cache-read 0) (pget usage :cache-write 0)))

;;; SSE transport: the framing loop shared by every SSE-based API.

(defun map-sse-events (char-stream dispatch &key abort-flag)
  "Drive the SSE framing loop: event:/data: accumulation, CR trimming,
dispatch on blank lines (multi-line data joined with newlines), final
flush at EOF.  DISPATCH is called as (event-type-or-nil data-string);
return :stop to end the stream.  Returns :aborted when ABORT-FLAG fires,
else :done."
  (let ((event-type nil) (data-lines nil))
    (flet ((flush ()
             (when data-lines
               (let ((type event-type)
                     (data (string-join (string #\Newline) (nreverse data-lines))))
                 (setf event-type nil data-lines nil)
                 (funcall dispatch type data)))))
      (loop for line = (read-line char-stream nil :eof)
            do (when (and abort-flag (funcall abort-flag))
                 (return :aborted))
               (when (eq line :eof)
                 (flush)
                 (return :done))
               (let ((line (string-right-trim '(#\Return) line)))
                 (cond ((zerop (length line))
                        (when (eq (flush) :stop) (return :done)))
                       ((string-prefix-p "event:" line)
                        (setf event-type (string-trim " " (subseq line 6))))
                       ((string-prefix-p "data:" line)
                        (push (string-trim " " (subseq line 5)) data-lines))
                       (t nil)))))))

;;; HTTP + retry: in-request retry honoring retry-after, exponential
;;; backoff with jitter; classification on HTTP status, not string matching.

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

;;; Default transport: streamed SSE POST over dexador, parsed by the API's
;;; parse-stream.  Lives here (not api.lisp) because it owns the HTTP/proxy
;;; helpers above.

(defmethod perform-request ((api provider-api) url headers body
                            &key on-event abort-flag abort-cleanup &allow-other-keys)
  (let ((stream nil)
        (done nil)
        (cancelled nil)
        (result nil)
        (condition nil))
    (labels ((aborted-result ()
               (list :aborted-p t :content nil :stop-reason :aborted
                     :usage (list :input 0 :output 0
                                  :cache-read 0 :cache-write 0)))
             (aborted-p ()
               (or cancelled (and abort-flag (funcall abort-flag))))
             (abort-stream ()
               (when stream
                 (ignore-errors (close stream :abort t))))
             (emit (event)
               (unless (aborted-p)
                 (when on-event (funcall on-event event))))
             (request-body ()
               (unwind-protect
                    (handler-case
                        (setf result
                              (progn
                                (when (aborted-p)
                                  (return-from request-body (setf result (aborted-result))))
                                (setf stream (apply #'dex:post url
                                                    :headers headers :content body
                                                    :want-stream t :force-binary t
                                                    :keep-alive nil
                                                    :connect-timeout 15
                                                    :read-timeout 600
                                                    (let ((proxy (env-proxy url)))
                                                      (when proxy (list :proxy proxy)))))
                                (if (aborted-p)
                                    (aborted-result)
                                    (parse-stream api
                                                  (flexi-streams:make-flexi-stream
                                                   stream :external-format :utf-8)
                                                  :on-event #'emit
                                                  :abort-flag #'aborted-p))))
                      (serious-condition (e)
                        (setf condition e)))
                 (ignore-errors (close stream))
                 (setf done t))))
      (let* ((unregister (and abort-cleanup (funcall abort-cleanup #'abort-stream)))
             (thread (bt:make-thread #'request-body :name "evo-provider-request")))
        (unwind-protect
             (loop
               (cond (done
                      (when condition (error condition))
                      (return result))
                     ((and abort-flag (funcall abort-flag))
                      (abort-stream)
                      (setf cancelled t)
                      (return (aborted-result))))
               (sleep 0.02))
          (when unregister (funcall unregister))
          (unless cancelled (ignore-errors (bt:join-thread thread))))))))

(defun abortible-sleep (seconds abort-flag)
  "Sleep up to SECONDS, waking quickly when ABORT-FLAG becomes true."
  (loop with deadline = (+ (get-internal-real-time)
                           (* seconds internal-time-units-per-second))
        until (or (and abort-flag (funcall abort-flag))
                  (>= (get-internal-real-time) deadline))
        do (sleep (min 0.05 (/ (max 0 (- deadline (get-internal-real-time)))
                               internal-time-units-per-second)))))

(defun call-provider (&key model system messages tools thinking-level cache-key
                           on-event abort-flag abort-cleanup)
  "Make one streamed request.  ALWAYS returns an assistant message plist —
errors come back as data with :stop-reason :error/:aborted, never signals.
The model's :api tag names the wire API; its :provider names the endpoint
config.  CACHE-KEY (the session id) becomes prompt_cache_key on OpenAI;
Anthropic uses cache_control breakpoints instead."
  (let* ((model (if (consp model) model (find-model model)))
         (api (find-api (pget model :api)))
         (config (provider-config (pget model :provider)))
         (url (concatenate 'string
                           (string-right-trim "/" (pget config :base-url))
                           (endpoint-path api)))
         (headers (cons '("content-type" . "application/json")
                        (auth-headers api config)))
         (body (build-request api :model model :system system
                              :messages messages :tools tools
                              :thinking-level thinking-level
                              :cache-key cache-key))
         (last-error nil))
    (flet ((finish (result)
             (let ((usage (pget result :usage)))
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
                   :usage (list :input 0 :output 0 :cache-read 0 :cache-write 0)
                   :content nil))
           (aborted-message (&optional usage)
             (list :role :assistant
                   :api (pget model :api) :provider (pget model :provider)
                   :model (pget model :id)
                   :stop-reason :aborted
                   :usage (or usage (list :input 0 :output 0
                                          :cache-read 0 :cache-write 0))
                   :content nil)))
      (dotimes (attempt *max-attempts*
                        (error-message (format nil "Provider request failed after ~d attempts: ~a"
                                               *max-attempts* last-error)
                                       t))
        (let ((outcome
                (handler-case
                    (let ((result (perform-request api url headers body
                                                   :on-event on-event
                                                   :abort-flag abort-flag
                                                   :abort-cleanup abort-cleanup)))
                      (cond
                        ((pget result :aborted-p)
                         (list :done (aborted-message (pget result :usage))))
                        ((pget result :error-message)
                         (list :retry (pget result :error-message) nil))
                        ((not (pget result :stopped-p))
                         (list :retry "Stream ended without a terminal event (truncated response)" nil))
                        (t (list :done (finish result)))))
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
               (return (aborted-message)))
             (let ((delay (retry-delay attempt (third outcome))))
               (cond ((null delay)
                      (return (error-message last-error t)))
                     ((< attempt (1- *max-attempts*))
                      (abortible-sleep delay abort-flag)
                      (when (and abort-flag (funcall abort-flag))
                        (return (aborted-message)))))))))))))
