;;;; Native OpenAI Responses protocol for modern reasoning models.
;;;; Full-history replay, store=false. See docs/responses.md for the contract.

(in-package :evo.provider)

(defclass openai-responses-api (provider-api) ())
(defmethod endpoint-path ((api openai-responses-api)) "/responses")
(defmethod default-provider-key ((api openai-responses-api)) :openai)
(defmethod default-base-url ((api openai-responses-api)) "https://api.openai.com/v1")
(defmethod default-api-key-env ((api openai-responses-api)) "OPENAI_API_KEY")
(defmethod auth-headers ((api openai-responses-api) config)
  (list (cons "Authorization" (cat "Bearer " (pget config :api-key)))))

(defun responses-input-part (block)
  (case (pget block :type)
    (:text (jobj "type" "input_text" "text" (pget block :text)))
    (:image
     (let ((url (or (pget block :url)
                    (when (pget block :data)
                      (format nil "data:~a;base64,~a"
                              (pget block :media-type "image/png") (pget block :data))))))
       (if url
           (jobj "type" "input_image" "image_url" url
                 "detail" (pget block :detail "auto"))
           (responses-input-part (image-placeholder-block block)))))
    (:file
     (let ((part (jobj "type" "input_file")))
       (loop for (key wire) on '(:file-id "file_id" :file-data "file_data"
                                 :file-url "file_url" :filename "filename") by #'cddr
             when (pget block key) do (setf (gethash wire part) (pget block key)))
       part))
    (t (error 'provider-error :message
              (format nil "Unsupported Responses input block ~s" (pget block :type))))))

(defun responses-input (messages model)
  (let (items)
    (dolist (m (handoff-pass messages (pget model :id) :vision (model-vision-p model)
                            :api :openai-responses :provider (pget model :provider)))
      (case (message-role m)
        ((:user :system :developer)
         (push (jobj "role" (string-downcase (symbol-name (message-role m)))
                     "content" (map 'vector #'responses-input-part (message-content m))) items))
        (:tool-result
         (push (jobj "type" "function_call_output" "call_id" (pget m :tool-call-id)
                     "output" (map 'vector #'responses-input-part (message-content m))) items))
        (:assistant
         (dolist (block (message-content m))
           (let ((raw (and (equal (pget m :model) (pget model :id))
                           (eq (pget m :api) :openai-responses)
                           (equal (pget m :provider) (pget model :provider))
                           (pget block :responses-item-json))))
             (cond
               (raw (push (evo.util:parse-json raw) items))
               ((eq (pget block :type) :tool-call)
                (push (jobj "type" "function_call" "call_id" (pget block :id)
                            "name" (pget block :name)
                            "arguments" (or (pget block :arguments-json)
                                            (jzon:stringify
                                             (or (and (pget block :arguments)
                                                      (sexpr->json (pget block :arguments)))
                                                 (jobj))))) items))
               ((eq (pget block :type) :text)
                (when (plusp (length (pget block :text "")))
                  (push (jobj "role" "assistant" "content" (pget block :text)) items)))
               ;; Foreign thinking is opaque to this API; never translate it.
               ((eq (pget block :type) :thinking) nil)
               (t (error 'provider-error :message "Unsupported Responses assistant block"))))))
        (t (error 'provider-error :message "Unsupported Responses message role"))))
    (coerce (nreverse items) 'vector)))

(defun responses-options-json (options)
  "JSON text preserves arbitrary schema keys and JSON null/false exactly."
  (let ((obj (cond ((null options) (jobj))
                   ((stringp options) (evo.util:parse-json options))
                   ((hash-table-p options) options)
                   (t (error "Responses options must be a JSON object string or hash table")))))
    (unless (hash-table-p obj) (error "Responses options must be a JSON object"))
    obj))

(defmethod build-request ((api openai-responses-api)
                          &key model system messages tools thinking-level)
  (let* ((options (responses-options-json (pget model :responses-options)))
         (req (jobj "model" (pget model :id) "stream" t "store" nil
                    "max_output_tokens" (model-max-output model)
                    "input" (responses-input messages model)))
         (reasoning (jobj))
         (effort (clamp-effort thinking-level (model-effort model))))
    ;; Options expose protocol controls without replacing the agent's history,
    ;; transport or execution ownership. Stateful/background transports need a
    ;; different adapter contract and must not silently appear to work here.
    (maphash (lambda (key value)
               (unless (member key '("reasoning" "text" "tool_choice" "parallel_tool_calls"
                                     "include" "metadata" "service_tier" "prompt_cache_key"
                                     "prompt_cache_retention" "safety_identifier" "max_tool_calls"
                                     "truncation" "tools") :test #'equal)
                 (error 'provider-error :message (format nil "Unsupported Responses option ~s" key)))
               (setf (gethash key req) value)) options)
    (when system (setf (gethash "instructions" req) system))
    (unless (gethash "reasoning" options)
      (setf (gethash "summary" reasoning) "auto"))
    (when (gethash "reasoning" options)
      (unless (hash-table-p (gethash "reasoning" options))
        (error 'provider-error :message "Responses reasoning must be an object"))
      (maphash (lambda (k v) (setf (gethash k reasoning) v)) (gethash "reasoning" options)))
    (when effort
      (setf (gethash "effort" reasoning) (string-downcase (symbol-name effort))))
    (setf (gethash "reasoning" req) reasoning)
    (let ((functions
            (map 'vector
                 (lambda (tool)
                   (jobj "type" "function" "name" (pget tool :name)
                         "description" (pget tool :description "")
                         "parameters" (pget tool :input-schema)
                         ;; evo tools have genuinely optional properties.
                         "strict" (and (pget tool :strict) t))) tools)))
      (when (or tools (gethash "tools" options))
        (setf (gethash "tools" req)
              (concatenate 'vector functions (or (gethash "tools" options) #())))))
    (jzon:stringify req)))

(defun responses-item-block (item)
  "Keep each output item intact in a journal-readable string. Display and
execution use the unified fields, replay uses the original wire object."
  (let* ((type (jget item "type"))
         (raw (jzon:stringify item))
         (block
           (cond
             ((equal type "message")
              (list :type :text :text
                    (with-output-to-string (out)
                      (loop for part across (or (jget item "content") #())
                            do (cond ((equal (jget part "type") "output_text")
                                      (write-string (or (jget part "text") "") out))
                                     ((equal (jget part "type") "refusal")
                                      (write-string (or (jget part "refusal") "") out))
                                     (t (error 'provider-error :message
                                               "Unsupported Responses message content")))))))
             ((equal type "reasoning")
              (list :type :thinking :thinking
                    (with-output-to-string (out)
                      (loop for part across (or (jget item "summary") #())
                            for first = t then nil
                            do (unless first (terpri out))
                               (write-string (or (jget part "text") "") out)))
                    :signature ""))
             ((equal type "function_call")
              (let* ((args (or (jget item "arguments") ""))
                     (parsed (handler-case (evo.util:parse-json args) (error () nil))))
                (append (list :type :tool-call :id (jget item "call_id")
                              :name (jget item "name") :arguments-json args)
                        (if (hash-table-p parsed)
                            (list :arguments (json->sexpr parsed))
                            (list :arguments nil :arguments-error (truncate-string args 2000))))))
             ;; Hosted tools execute remotely. Preserve their items for replay
             ;; without dispatching them to evo's local tool registry.
             ((member type '("web_search_call" "file_search_call" "code_interpreter_call"
                             "image_generation_call" "mcp_call" "mcp_list_tools") :test #'equal)
              (list :type :text :text ""))
             (t (error 'provider-error :message
                       (format nil "Unsupported Responses output item ~s" type))))))
    (append block (list :responses-item-json raw))))

(defun responses-result (response &optional event-type)
  (let* ((status (jget response "status"))
         (usage (jget response "usage"))
         (cached (or (jget usage "input_tokens_details" "cached_tokens") 0))
         (output (jget response "output"))
         (reason (jget response "incomplete_details" "reason")))
    (unless (member status '("completed" "incomplete" "failed" "cancelled") :test #'equal)
      (error 'provider-error :message (format nil "Unknown Responses terminal status ~s" status)))
    (when (and event-type (not (equal event-type (cat "response." status))))
      (error 'provider-error :message "Responses terminal event/status mismatch"))
    (when (member status '("failed" "cancelled") :test #'equal)
      (error 'provider-error :message
             (format nil "Responses ~a: ~a" status
                     (or (jget response "error" "message") "no error details"))))
    (unless (and (vectorp output) (not (stringp output)))
      (error 'provider-error :message "Responses terminal output is missing"))
    (let* ((content (map 'list #'responses-item-block output))
           (stop (cond ((equal status "incomplete")
                        (cond ((equal reason "max_output_tokens") :length)
                              ((equal reason "content_filter") :error)
                              (t (error 'provider-error :message
                                        (format nil "Unknown Responses incomplete reason ~s" reason)))))
                       ((find :tool-call content :key (lambda (b) (pget b :type))) :tool-use)
                       (t :stop))))
      ;; Incomplete arguments must never be executed by the agent loop.
      (when (equal status "incomplete")
        (setf content (remove :tool-call content :key (lambda (b) (pget b :type)))))
      (list :content content :model (jget response "model") :stopped-p t
            :stop-reason stop
            :usage (list :input (max 0 (- (or (jget usage "input_tokens") 0) cached))
                         :output (or (jget usage "output_tokens") 0)
                         :cache-read cached :cache-write 0
                         :reasoning (or (jget usage "output_tokens_details" "reasoning_tokens") 0))))))

(defmethod parse-stream ((api openai-responses-api) char-stream &key on-event abort-flag)
  (let ((result nil))
    (labels ((emit (type text)
               (when on-event (funcall on-event (list :type type :text text))))
             (dispatch (event data)
               (unless (or (equal data "[DONE]") (equal event "ping"))
                 (let* ((obj (handler-case (evo.util:parse-json data)
                               (error () (error 'provider-error :message "Malformed Responses SSE JSON"))))
                        (type (or (jget obj "type") event)))
                   (cond
                     ((equal type "response.created")
                      (when on-event (funcall on-event (list :type :message-start))))
                     ((member type '("response.output_text.delta" "response.refusal.delta") :test #'equal)
                      (emit :text-delta (or (jget obj "delta") "")))
                     ((equal type "response.reasoning_summary_text.delta")
                      (emit :thinking-delta (or (jget obj "delta") "")))
                     ((member type '("response.completed" "response.incomplete"
                                     "response.failed" "response.cancelled") :test #'equal)
                      ;; Terminal output is authoritative: it includes complete
                      ;; tool arguments, annotations, phases and encrypted state.
                      (setf result (responses-result (jget obj "response") type))
                      :stop)
                     ((equal type "error")
                      (error 'provider-error :message
                             (format nil "Responses error: ~a" (or (jget obj "message")
                                                                  (jget obj "error" "message")
                                                                  "unknown error")))))))))
      (if (eq (map-sse-events char-stream #'dispatch :abort-flag abort-flag) :aborted)
          (list :aborted-p t :stop-reason :aborted :content nil)
          (or result (list :stopped-p nil :content nil :stop-reason :error))))))

(register-api :openai-responses (make-instance 'openai-responses-api))
