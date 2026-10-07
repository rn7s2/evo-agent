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

(defun responses-tool-output (content)
  "Plain text tool results use the compact string wire shape; results carrying
images or files use structured content items."
  (if (every (lambda (block) (eq (pget block :type) :text)) content)
      (with-output-to-string (out)
        (dolist (block content) (write-string (pget block :text "") out)))
      (map 'vector #'responses-input-part content)))

(defun responses-input (messages model)
  (let (items)
    (dolist (m (handoff-pass messages (pget model :id) :vision (model-vision-p model)
                            :api (pget model :api) :provider (pget model :provider)))
      (case (message-role m)
        ((:user :system :developer)
         (push (jobj "role" (string-downcase (symbol-name (message-role m)))
                     "content" (map 'vector #'responses-input-part (message-content m))) items))
        (:tool-result
         (let ((content (message-content m)))
           (push (jobj "type" "function_call_output" "call_id" (pget m :tool-call-id)
                       "output" (responses-tool-output content)) items)))
        (:assistant
         (dolist (block (message-content m))
           (let ((raw (and (equal (pget m :model) (pget model :id))
                           (equal (pget m :api) (pget model :api))
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
    ;; Stateless reasoning replay needs the opaque payload.  Preserve every
    ;; explicit include while making the replay token part of the base contract.
    (let ((include (or (gethash "include" req) #())))
      (unless (find "reasoning.encrypted_content" include :test #'equal)
        (setf (gethash "include" req)
              (concatenate 'vector include #("reasoning.encrypted_content")))))
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
         (reason (jget response "incomplete_details" "reason"))
         (output-tokens (or (jget usage "output_tokens") 0))
         (reasoning-tokens (or (jget usage "output_tokens_details" "reasoning_tokens") 0)))
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
      (when (and (equal status "completed")
                 (> output-tokens reasoning-tokens)
                 (null content))
        (error 'provider-error :message
               "Responses completed with visible output tokens but no assistant output items"))
      (list :content content :model (jget response "model") :stopped-p t
            :stop-reason stop
            :usage (list :input (max 0 (- (or (jget usage "input_tokens") 0) cached))
                         :output (or (jget usage "output_tokens") 0)
                         :cache-read cached :cache-write 0
                         :reasoning (or (jget usage "output_tokens_details" "reasoning_tokens") 0))))))

(defmethod parse-stream ((api openai-responses-api) char-stream &key on-event abort-flag)
  (let ((result nil)
        ;; Responses Lite may leave the terminal response.output empty.  Its
        ;; output_item.done events are then the only authoritative copy of
        ;; messages and function calls, so retain them until the terminal event.
        (items nil)
        (text (make-hash-table :test #'equal))
        (refusals (make-hash-table :test #'equal))
        (arguments (make-hash-table :test #'equal)))
    (labels ((emit (type value)
               (when on-event (funcall on-event (list :type type :text value))))
             (event-key (obj)
               (or (jget obj "item_id") (jget obj "call_id")
                   (let ((item (jget obj "item")))
                     (and item (or (jget item "id") (jget item "call_id"))))
                   (jget obj "output_index") 0))
             (append-fragment (table key fragment)
               (when fragment
                 (setf (gethash key table)
                       (cat (or (gethash key table) "") fragment))))
             (set-fragment (table key value)
               (when value (setf (gethash key table) value)))
             (remember-item (obj)
               (let ((item (jget obj "item")))
                 (when item
                   (setf items (nconc items (list (cons (event-key obj) item)))))))
             (completed-output (&key include-fragments)
               (let ((seen (make-hash-table :test #'equal)) output)
                 (dolist (pair items)
                   (let* ((key (car pair))
                          (item (cdr pair))
                          (type (jget item "type"))
                          (output-text (gethash key text))
                          (refusal (gethash key refusals))
                          (args (gethash key arguments)))
                     (setf (gethash key seen) t)
                     (cond
                       ((and (equal type "function_call") args
                             (zerop (length (or (jget item "arguments") ""))))
                        (setf (gethash "arguments" item) args))
                       ((and (equal type "message")
                             (let ((content (jget item "content")))
                               (or (not (vectorp content)) (zerop (length content))))
                             (or output-text refusal))
                        (setf (gethash "content" item)
                              (vector (if output-text
                                          (jobj "type" "output_text" "text" output-text)
                                          (jobj "type" "refusal" "refusal" refusal))))))
                     (push item output)))
                 (when include-fragments
                   (let (keys)
                     (dolist (table (list text refusals))
                       (maphash (lambda (key value)
                                  (declare (ignore value))
                                  (pushnew key keys :test #'equal)) table))
                     (dolist (key keys)
                       (unless (gethash key seen)
                         (let ((output-text (gethash key text))
                               (refusal (gethash key refusals)))
                           (when (or output-text refusal)
                             (push (jobj "type" "message" "role" "assistant"
                                         "status" "completed"
                                         "content" (vector
                                                    (if output-text
                                                        (jobj "type" "output_text"
                                                              "text" output-text)
                                                        (jobj "type" "refusal"
                                                              "refusal" refusal))))
                                   output)))))))
                 (coerce (nreverse output) 'vector)))
             (item-key (item)
               (or (jget item "id") (jget item "call_id")))
             (merge-output (remembered terminal)
               "Merge Lite's per-item stream with a terminal snapshot.  The
snapshot wins for an id it repeats; remembered event order is retained."
               (let ((remaining (coerce terminal 'list)) output)
                 (loop for item across remembered
                       for id = (item-key item)
                       for final = (and id (find id remaining
                                                 :key #'item-key
                                                 :test #'equal))
                       do (push (or final item) output)
                          (when final (setf remaining (delete final remaining :count 1))))
                 (coerce (nconc (nreverse output) remaining) 'vector)))
             (terminal-result (obj type)
               (let* ((response (jget obj "response"))
                      (output (and response (jget response "output"))))
                 (unless (hash-table-p response)
                   (error 'provider-error :message "Responses terminal event is missing response"))
                 ;; Responses Lite terminal objects carry completion metadata but
                 ;; need not repeat the public API's status field.
                 (unless (jget response "status")
                   (setf (gethash "status" response)
                         (cond ((equal type "response.completed") "completed")
                               ((equal type "response.incomplete") "incomplete")
                               ((equal type "response.failed") "failed")
                               ((equal type "response.cancelled") "cancelled"))))
                 ;; The public API normally repeats output items in the terminal;
                 ;; Codex's Lite route does not.  Merge both channels, with a
                 ;; terminal copy winning when it repeats the same item id.
                 (let ((terminal (if (and (vectorp output) (not (stringp output)))
                                     output #())))
                   (let ((remembered (completed-output :include-fragments
                                                      (zerop (length terminal)))))
                     (setf (gethash "output" response)
                           (if (plusp (length remembered))
                               (merge-output remembered terminal)
                               terminal))))
                 (responses-result response type)))
             (dispatch (event data)
               (unless (or (equal data "[DONE]") (equal event "ping"))
                 (let* ((obj (handler-case (evo.util:parse-json data)
                               (error () (error 'provider-error :message "Malformed Responses SSE JSON"))))
                        (type (or (jget obj "type") event))
                        (key (event-key obj)))
                   (cond
                     ((equal type "response.created")
                      (when on-event (funcall on-event (list :type :message-start))))
                     ((equal type "response.output_text.delta")
                      (append-fragment text key (jget obj "delta"))
                      (emit :text-delta (or (jget obj "delta") "")))
                     ((equal type "response.refusal.delta")
                      (append-fragment refusals key (jget obj "delta"))
                      (emit :text-delta (or (jget obj "delta") "")))
                     ((equal type "response.output_text.done")
                      (set-fragment text key (jget obj "text")))
                     ((equal type "response.refusal.done")
                      (set-fragment refusals key (jget obj "refusal")))
                     ((equal type "response.content_part.done")
                      (let ((part (jget obj "part")))
                        (cond ((equal (jget part "type") "output_text")
                               (set-fragment text key (jget part "text")))
                              ((equal (jget part "type") "refusal")
                               (set-fragment refusals key (jget part "refusal"))))))
                     ((equal type "response.reasoning_summary_text.delta")
                      (emit :thinking-delta (or (jget obj "delta") "")))
                     ((equal type "response.output_item.added") nil)
                     ((equal type "response.output_item.done")
                      ;; Only a done item is safe to journal or execute.  An added
                      ;; function call can still have incomplete arguments.
                      (remember-item obj))
                     ((equal type "response.function_call_arguments.delta")
                      (append-fragment arguments key (jget obj "delta")))
                     ((equal type "response.function_call_arguments.done")
                      (set-fragment arguments key (jget obj "arguments")))
                     ((member type '("response.completed" "response.incomplete"
                                     "response.failed" "response.cancelled") :test #'equal)
                      (setf result (terminal-result obj type))
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
