;;;; openai.lisp — the OpenAI Responses API.
;;;;
;;;; Same contract as the Anthropic adapter (see api.lisp): stateless
;;;; replay (`store` false, no previous_response_id — the whole history is
;;;; `input` items each request), hand-rolled SSE, errors are data.
;;;; Reasoning replays as the whole reasoning item (with encrypted_content)
;;;; kept on the :thinking block as :item; assistant text and tool-call
;;;; blocks keep their output-item ids as :item-id.  Item ids replay only
;;;; against the same model — the server validates fc_*<->rs_* same-response
;;;; pairing, and a model switch would trip it.

(in-package :evo.provider)

(defclass openai-responses-api (provider-api) ())

(defmethod endpoint-path ((api openai-responses-api))
  "/v1/responses")

(defmethod auth-headers ((api openai-responses-api) config)
  `(("authorization" . ,(concatenate 'string "Bearer " (pget config :api-key)))))

(defmethod default-provider-key ((api openai-responses-api)) :openai)
(defmethod default-base-url ((api openai-responses-api)) "https://api.openai.com")
(defmethod default-api-key-env ((api openai-responses-api)) "OPENAI_API_KEY")

;;; Thinking levels -> reasoning effort.  NIL = reasoning off (the adapter
;;; then sends an explicit effort "none").

(defun reasoning-effort (level)
  (case level
    ((nil :off) nil)
    (:low "low")
    (:medium "medium")
    (:high "high")
    (:xhigh "xhigh")
    (t nil)))

(defmethod thinking-param ((api openai-responses-api) level)
  (reasoning-effort level))

;;; Request building.
;;;
;;; History -> `input` items.  One unified message can fan out to several
;;; items (reasoning + message + function_call all live inside one
;;; assistant message here); tool results become function_call_output.

(defun user-block->input-json (block)
  (case (pget block :type)
    (:text (jobj "type" "input_text" "text" (pget block :text)))
    (:image (jobj "type" "input_text" "text" "[image omitted]"))
    (t (error 'provider-error
              :message (format nil "Unknown user content block type ~s"
                               (pget block :type))))))

(defun tool-result->output-string (m)
  "The Responses API has no is_error flag and rejects empty output;
the error signal rides in the text itself."
  (let ((text (string-join
               (string #\Newline)
               (loop for b in (message-content m)
                     when (eq (pget b :type) :text)
                       collect (pget b :text)))))
    (if (zerop (length text)) "(no tool output)" text)))

(defun assistant-message->items (m target-model-id)
  (let ((same-model (equal (pget m :model) target-model-id)))
    (loop for block in (message-content m)
          append
          (case (pget block :type)
            (:thinking
             ;; Same-model reasoning replays verbatim as the stored item;
             ;; cross-model thinking was already dropped by handoff-pass.
             (let ((item (pget block :item)))
               (when item (list (sexpr->json item)))))
            (:text
             (let ((item (jobj "type" "message" "role" "assistant"
                               "content" (vector (jobj "type" "output_text"
                                                       "text" (pget block :text)))))
                   (id (pget block :item-id)))
               (when (and same-model id)
                 (setf (gethash "id" item) id))
               (list item)))
            (:tool-call
             (let ((item (jobj "type" "function_call"
                               "call_id" (pget block :id)
                               "name" (pget block :name)
                               "arguments"
                               (let ((args (pget block :arguments)))
                                 (if args
                                     (jzon:stringify (sexpr->json args))
                                     "{}"))))
                   (id (pget block :item-id)))
               ;; Only fc_* ids pair-validate; anything else is replayed
               ;; id-less and the server synthesizes one.
               (when (and same-model id (string-prefix-p "fc_" id))
                 (setf (gethash "id" item) id))
               (list item)))
            (:image nil)
            (t (error 'provider-error
                      :message (format nil "Unknown content block type ~s"
                                       (pget block :type))))))))

(defun messages->input-items (messages target-model-id)
  (let ((items nil))
    (dolist (m messages)
      (ecase (message-role m)
        (:user
         (push (jobj "type" "message" "role" "user"
                     "content" (map 'vector #'user-block->input-json
                                    (message-content m)))
               items))
        (:assistant
         (dolist (item (assistant-message->items m target-model-id))
           (push item items)))
        (:tool-result
         (push (jobj "type" "function_call_output"
                     "call_id" (pget m :tool-call-id)
                     "output" (tool-result->output-string m))
               items))))
    (coerce (nreverse items) 'vector)))

(defun tools->responses-json (tools)
  "Responses function tools are flat: name/description/parameters at top
level, not nested under \"function\"."
  (when tools
    (map 'vector
         (lambda (tl)
           (jobj "type" "function"
                 "name" (pget tl :name)
                 "description" (pget tl :description)
                 "parameters" (pget tl :input-schema)))
         tools)))

(defun build-responses-request-json (&key model system messages tools
                                          thinking-level cache-key)
  (let* ((model-id (pget model :id))
         (effort (and (pget model :thinking) (reasoning-effort thinking-level)))
         (req (jobj "model" model-id
                    "stream" t
                    "store" nil
                    "max_output_tokens" (model-max-output model)
                    "input" (messages->input-items
                             (handoff-pass messages model-id) model-id))))
    (when system
      (setf (gethash "instructions" req) system))
    (let ((jt (tools->responses-json tools)))
      (when jt (setf (gethash "tools" req) jt)))
    (cond (effort
           ;; encrypted_content must be requested explicitly or stateless
           ;; replay of reasoning is impossible.
           (setf (gethash "reasoning" req) (jobj "effort" effort "summary" "auto")
                 (gethash "include" req) (vector "reasoning.encrypted_content")))
          ((pget model :thinking)
           ;; Reasoning model with thinking off: say so explicitly rather
           ;; than inherit the server-side default.
           (setf (gethash "reasoning" req) (jobj "effort" "none"))))
    (when cache-key
      (setf (gethash "prompt_cache_key" req) cache-key))
    (jzon:stringify req)))

(defmethod build-request ((api openai-responses-api)
                          &key model system messages tools thinking-level cache-key)
  (build-responses-request-json :model model :system system :messages messages
                                :tools tools :thinking-level thinking-level
                                :cache-key cache-key))

;;; SSE parsing.  Output items accumulate keyed by output_index; a stream
;;; ending without a terminal response.* event is a retryable error.
;;; Tool arguments accumulate as partial JSON deltas, with the complete
;;; string from *.arguments.done / output_item.done taking precedence.

(defstruct oai-item type text (summary "") item call-id item-id name (args-json ""))

(defun oai-usage (resp)
  "Responses usage counts cached (and explicit-cache-write) tokens inside
input_tokens; unbundle them so each bucket is attributed separately."
  (let* ((usage (jget resp "usage"))
         (input (or (jget usage "input_tokens") 0))
         (cached (or (jget usage "input_tokens_details" "cached_tokens") 0))
         (written (or (jget usage "input_tokens_details" "cache_write_tokens") 0)))
    (when usage
      (list :input (max 0 (- input cached written))
            :output (or (jget usage "output_tokens") 0)
            :cache-read cached
            :cache-write written))))

(defun summary-item-text (item)
  "Join a reasoning item's summary (and raw reasoning content, if the
model exposes it) into display text."
  (let ((parts (append (coerce (or (jget item "summary") #()) 'list)
                       (coerce (or (jget item "content") #()) 'list))))
    (string-join (format nil "~2%")
                 (loop for p in parts
                       for text = (jget p "text")
                       when (and (stringp text) (plusp (length text)))
                         collect text))))

(defun message-item-text (item)
  (let ((parts (coerce (or (jget item "content") #()) 'list)))
    (apply #'concatenate 'string
           (loop for p in parts
                 collect (or (jget p "text") (jget p "refusal") "")))))

(defun parse-responses-sse-stream (char-stream &key on-event abort-flag)
  "Parse an OpenAI Responses SSE stream into the adapter result plist."
  (let ((items (make-hash-table))       ; output_index -> oai-item
        (max-index -1)
        (status nil) (incomplete-reason nil) (model nil)
        (stopped-p nil) (error-message nil)
        (usage nil))
    (labels ((emit (&rest ev) (when on-event (funcall on-event ev)))
             (item-at (obj)
               (let ((idx (jget obj "output_index")))
                 (and (integerp idx) (gethash idx items))))
             (terminal (obj new-status)
               (let ((resp (jget obj "response")))
                 (setf status new-status
                       stopped-p t
                       model (or (jget resp "model") model)
                       usage (or (oai-usage resp) usage)
                       incomplete-reason (jget resp "incomplete_details" "reason"))))
             (handle (event-type data)
               (let* ((obj (ignore-errors (jzon:parse data)))
                      (type (or event-type (and obj (jget obj "type")))))
                 (when obj
                   (cond
                     ((equal type "response.created")
                      (setf model (or (jget obj "response" "model") model))
                      (emit :type :message-start))
                     ((equal type "response.output_item.added")
                      (let* ((idx (jget obj "output_index"))
                             (jitem (jget obj "item"))
                             (itype (jget jitem "type"))
                             (item (make-oai-item
                                    :type (cond ((equal itype "message") :text)
                                                ((equal itype "reasoning") :thinking)
                                                ((equal itype "function_call") :tool-call)
                                                (t :unknown))
                                    :text ""
                                    :call-id (jget jitem "call_id")
                                    :item-id (jget jitem "id")
                                    :name (jget jitem "name"))))
                        (when (integerp idx)
                          (setf (gethash idx items) item
                                max-index (max max-index idx)))))
                     ((or (equal type "response.output_text.delta")
                          (equal type "response.refusal.delta"))
                      (let ((item (item-at obj))
                            (s (or (jget obj "delta") "")))
                        (when item
                          (setf (oai-item-text item)
                                (concatenate 'string (oai-item-text item) s))
                          (emit :type :text-delta :text s))))
                     ((or (equal type "response.reasoning_summary_text.delta")
                          (equal type "response.reasoning_text.delta"))
                      (let ((item (item-at obj))
                            (s (or (jget obj "delta") "")))
                        (when item
                          (setf (oai-item-summary item)
                                (concatenate 'string (oai-item-summary item) s))
                          (emit :type :thinking-delta :text s))))
                     ((equal type "response.reasoning_summary_part.done")
                      ;; Separate summary parts; a trailing separator is
                      ;; trimmed at materialization.
                      (let ((item (item-at obj)))
                        (when item
                          (setf (oai-item-summary item)
                                (concatenate 'string (oai-item-summary item)
                                             (format nil "~2%")))
                          (emit :type :thinking-delta :text (format nil "~2%")))))
                     ((equal type "response.function_call_arguments.delta")
                      (let ((item (item-at obj)))
                        (when item
                          (setf (oai-item-args-json item)
                                (concatenate 'string (oai-item-args-json item)
                                             (or (jget obj "delta") ""))))))
                     ((equal type "response.function_call_arguments.done")
                      (let ((item (item-at obj))
                            (args (jget obj "arguments")))
                        (when (and item (stringp args))
                          (setf (oai-item-args-json item) args))))
                     ((equal type "response.output_item.done")
                      (let* ((idx (jget obj "output_index"))
                             (jitem (jget obj "item"))
                             (item (or (and (integerp idx) (gethash idx items))
                                       ;; done without added still counts
                                       (let ((new (make-oai-item :type :unknown :text "")))
                                         (when (integerp idx)
                                           (setf (gethash idx items) new
                                                 max-index (max max-index idx)))
                                         new))))
                        (when (eq (oai-item-type item) :unknown)
                          (setf (oai-item-type item)
                                (let ((itype (jget jitem "type")))
                                  (cond ((equal itype "message") :text)
                                        ((equal itype "reasoning") :thinking)
                                        ((equal itype "function_call") :tool-call)
                                        (t :unknown)))))
                        (setf (oai-item-item-id item)
                              (or (jget jitem "id") (oai-item-item-id item)))
                        (case (oai-item-type item)
                          (:thinking
                           (setf (oai-item-item item) (json->sexpr jitem))
                           (when (zerop (length (oai-item-summary item)))
                             (setf (oai-item-summary item) (summary-item-text jitem))))
                          (:text
                           (let ((text (message-item-text jitem)))
                             (when (plusp (length text))
                               (setf (oai-item-text item) text))))
                          (:tool-call
                           (let ((args (jget jitem "arguments")))
                             (when (and (stringp args) (plusp (length args)))
                               (setf (oai-item-args-json item) args)))
                           (setf (oai-item-call-id item)
                                 (or (jget jitem "call_id") (oai-item-call-id item))
                                 (oai-item-name item)
                                 (or (jget jitem "name") (oai-item-name item)))))))
                     ((equal type "response.completed") (terminal obj "completed"))
                     ((equal type "response.incomplete") (terminal obj "incomplete"))
                     ((equal type "response.failed")
                      (let ((err (jget obj "response" "error")))
                        (setf error-message
                              (format nil "~a: ~a"
                                      (or (jget err "code") "response.failed")
                                      (or (jget err "message") data)))))
                     ((equal type "error")
                      (setf error-message
                            (format nil "~a: ~a"
                                    (or (jget obj "code") "error")
                                    (or (jget obj "message") data))))
                     (t nil)))
                 (when (or stopped-p error-message) :stop))))
      (when (eq (map-sse-events char-stream #'handle :abort-flag abort-flag)
                :aborted)
        (return-from parse-responses-sse-stream
          (list :aborted-p t :content nil :stop-reason :aborted
                :usage (or usage (list :input 0 :output 0
                                       :cache-read 0 :cache-write 0)))))
      ;; Materialize items in output order.
      (let ((content
              (loop for i from 0 to max-index
                    for item = (gethash i items)
                    when item
                      append (case (oai-item-type item)
                               (:text
                                (list (append
                                       (list :type :text :text (oai-item-text item))
                                       (when (oai-item-item-id item)
                                         (list :item-id (oai-item-item-id item))))))
                               (:thinking
                                (when (oai-item-item item)
                                  (list (list :type :thinking
                                              :thinking (string-right-trim
                                                         '(#\Newline #\Space)
                                                         (oai-item-summary item))
                                              :item (oai-item-item item)))))
                               (:tool-call
                                (let* ((raw (oai-item-args-json item))
                                       (args (cond ((zerop (length raw)) nil)
                                                   (t (handler-case
                                                          (json->sexpr (jzon:parse raw))
                                                        (error () :parse-error))))))
                                  (list (append
                                         (list :type :tool-call
                                               :id (oai-item-call-id item)
                                               :name (oai-item-name item))
                                         (when (oai-item-item-id item)
                                           (list :item-id (oai-item-item-id item)))
                                         (if (eq args :parse-error)
                                             (list :arguments nil :arguments-error
                                                   (truncate-string raw 2000))
                                             (list :arguments args))))))
                               (t nil)))))
        (list :content content
              :model model
              :stopped-p stopped-p
              :error-message error-message
              :stop-reason (normalize-responses-stop-reason
                            status incomplete-reason content)
              :usage (or usage (list :input 0 :output 0
                                     :cache-read 0 :cache-write 0)))))))

(defun normalize-responses-stop-reason (status incomplete-reason content)
  "The Responses API has no tool-use status; infer it from content.
Unknown statuses and incomplete reasons are loud errors (design rule)."
  (cond ((find :tool-call content :key (lambda (b) (pget b :type))) :tool-use)
        ((null status) :stop)               ; non-terminal stream; caller retries
        ((equal status "completed") :stop)
        ((equal status "incomplete")
         (cond ((equal incomplete-reason "max_output_tokens") :length)
               ((null incomplete-reason) :length)
               (t (error 'provider-error
                         :message (format nil "Unknown incomplete reason ~s"
                                          incomplete-reason)))))
        (t (error 'provider-error
                  :message (format nil "Unknown response status ~s" status)))))

(defmethod parse-stream ((api openai-responses-api) char-stream
                         &key on-event abort-flag)
  (parse-responses-sse-stream char-stream :on-event on-event :abort-flag abort-flag))

(register-api :openai-responses (make-instance 'openai-responses-api))
