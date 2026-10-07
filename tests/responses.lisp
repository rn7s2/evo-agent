;;;; Responses protocol fixtures. Loaded by the main unit runner.
(in-package :evo.tests)

(defun responses-fixture-model (&optional options)
  (list :id "gpt-6-astra" :provider :openai :api :openai-responses
        :context-window 100000 :max-output 8192 :effort '(:low :medium :high :xhigh :max)
        :responses-options options))

(defun responses-fixture-parse (events &key on-event abort-flag)
  (with-input-from-string
      (in (with-output-to-string (s)
            (dolist (event events)
              ;; No event: header, as allowed by the SSE protocol.
              (format s "data: ~a~%~%" event))))
    (parse-stream (find-api :openai-responses) in :on-event on-event :abort-flag abort-flag)))

(defun test-responses ()
  (let* ((api (find-api :openai-responses))
         (model (responses-fixture-model
                 "{\"text\":{\"format\":{\"type\":\"json_schema\",\"name\":\"result\",\"strict\":true,\"schema\":{\"type\":\"object\",\"properties\":{\"CamelCase\":{\"type\":\"string\"}},\"required\":[\"CamelCase\"],\"additionalProperties\":false}}},\"reasoning\":{\"mode\":\"pro\",\"context\":\"all_turns\"},\"parallel_tool_calls\":false}"))
         (schema (parse-json "{\"type\":\"object\",\"properties\":{\"FileName\":{\"type\":\"string\"}}}"))
         (request (parse-json
                   (build-request api :model model :system "Be helpful." :thinking-level :xhigh
                                  :messages '((:role :user :content ((:type :text :text "hello")
                                                                    (:type :image :data "YWJj" :media-type "image/png")
                                                                    (:type :file :file-id "file_1"))))
                                  :tools (list (list :name "read" :description "Read file" :input-schema schema))))))
    (check "responses endpoint and bearer auth"
           (and (equal "/responses" (endpoint-path api))
                (equal '(("Authorization" . "Bearer test")) (auth-headers api '(:api-key "test")))))
    (check "responses defaults survive registry reset"
           (let ((evo.provider::*providers* nil) (evo.provider::*models* nil))
             (reset-user-registries)
             (equal "https://api.openai.com/v1" (pget (provider-registration :openai) :base-url))))
    (check "responses stateless streaming request"
           (and (eq t (gethash "stream" request)) (null (gethash "store" request))
                (= 8192 (gethash "max_output_tokens" request))
                (equal "Be helpful." (gethash "instructions" request))
                (null (gethash "messages" request))))
    (check "responses modern reasoning controls"
           (and (equal "xhigh" (evo.provider::jget request "reasoning" "effort"))
                (equal "pro" (evo.provider::jget request "reasoning" "mode"))
                (equal "all_turns" (evo.provider::jget request "reasoning" "context"))))
    (check "responses schemas retain case and false"
           (and (evo.provider::jget request "text" "format" "schema" "properties" "CamelCase")
                (null (gethash "parallel_tool_calls" request))
                (evo.provider::jget (aref (gethash "tools" request) 0) "parameters" "properties" "FileName")
                (null (gethash "strict" (aref (gethash "tools" request) 0)))))
    (let ((parts (gethash "content" (aref (gethash "input" request) 0))))
      (check "responses images and file inputs"
             (and (equal "input_image" (gethash "type" (aref parts 1)))
                  (equal "data:image/png;base64,YWJj" (gethash "image_url" (aref parts 1)))
                  (equal "file_1" (gethash "file_id" (aref parts 2))))))
    (check-signals "responses rejects stateful override"
                   (build-request api :model (responses-fixture-model "{\"previous_response_id\":\"resp_1\"}")))
    (let ((evo.provider::*models* nil))
      (register-model* "gpt-5.6-sol" :provider :openai :api :openai-responses
                       :context-window 100000 :max-output 8192 :effort t
                       :responses-options "{\"text\":{\"verbosity\":\"low\"}}")
      (check "responses options survive model registration"
             (equal "{\"text\":{\"verbosity\":\"low\"}}"
                    (pget (find-model "gpt-5.6-sol") :responses-options)))))
  (let* ((events nil)
         (terminal "{\"type\":\"response.completed\",\"response\":{\"status\":\"completed\",\"model\":\"gpt-6-astra\",\"usage\":{\"input_tokens\":100,\"output_tokens\":30,\"input_tokens_details\":{\"cached_tokens\":80},\"output_tokens_details\":{\"reasoning_tokens\":20}},\"output\":[{\"type\":\"reasoning\",\"id\":\"rs_1\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"Plan\"}],\"encrypted_content\":\"opaque\"},{\"type\":\"message\",\"id\":\"msg_1\",\"role\":\"assistant\",\"status\":\"completed\",\"phase\":\"commentary\",\"content\":[{\"type\":\"output_text\",\"text\":\"Reading\",\"annotations\":[]}]},{\"type\":\"function_call\",\"id\":\"fc_1\",\"call_id\":\"call_1\",\"name\":\"read\",\"arguments\":\"{\\\"FileName\\\":\\\"a\\\"}\"},{\"type\":\"function_call\",\"id\":\"fc_2\",\"call_id\":\"call_2\",\"name\":\"read\",\"arguments\":\"{\\\"FileName\\\":\\\"b\\\"}\"}]}}")
         (result (responses-fixture-parse
                  (list "{\"type\":\"response.created\"}"
                        "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"Plan\"}"
                        "{\"type\":\"response.output_text.delta\",\"delta\":\"Reading\"}"
                        "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":3,\"delta\":\"{\"}"
                        "{\"type\":\"response.function_call_arguments.delta\",\"output_index\":2,\"delta\":\"{\"}"
                        terminal)
                  :on-event (lambda (ev) (push ev events))))
         (content (pget result :content))
         (history (list (list :role :assistant :api :openai-responses :provider :openai
                              :model "gpt-6-astra" :content content))))
    (check "responses streaming events"
           (equal '(:message-start :thinking-delta :text-delta)
                  (mapcar (lambda (ev) (pget ev :type)) (reverse events))))
    (check "responses parallel calls ordered by terminal output"
           (and (eq :tool-use (pget result :stop-reason))
                (equal '("call_1" "call_2") (mapcar (lambda (b) (pget b :id)) (cddr content)))
                (equal "{\"FileName\":\"a\"}" (pget (third content) :arguments-json))))
    (check "responses cached and reasoning tokens not double counted"
           (and (= 130 (usage-total-tokens (pget result :usage)))
                (= 20 (pget (pget result :usage) :input))
                (= 20 (pget (pget result :usage) :reasoning))))
    (let* ((request (parse-json (build-request (find-api :openai-responses)
                                              :model (responses-fixture-model) :messages history)))
           (input (gethash "input" request)))
      (check "responses exact reasoning and phase replay"
             (and (equal "opaque" (gethash "encrypted_content" (aref input 0)))
                  (equal "commentary" (gethash "phase" (aref input 1)))
                  (equal "{\"FileName\":\"a\"}" (gethash "arguments" (aref input 2)))))
      (check "responses orphan tool results reference call_id not item id"
             (and (= 6 (length input))
                  (equal "call_1" (gethash "call_id" (aref input 4)))
                  (equal "function_call_output" (gethash "type" (aref input 4))))))
    (let* ((other (pput (responses-fixture-model) :provider :other))
           (request (parse-json (build-request (find-api :openai-responses) :model other :messages history))))
      (check "responses foreign provider drops opaque state"
             (not (search "opaque" (com.inuoe.jzon:stringify request)))))
    (let ((request (build-request (find-api :anthropic-messages)
                                  :model (responses-fixture-model) :messages history)))
      (check "responses reasoning does not leak into Messages protocol"
             (not (search "\"signature\"" request))))
    (let* ((journal (with-standard-io-syntax (write-to-string history)))
           (restored (with-standard-io-syntax (read-from-string journal))))
      (check "responses replay metadata survives journal round trip" (equalp history restored))))
  (dolist (case '(("completed" nil :stop) ("incomplete" "max_output_tokens" :length)
                  ("incomplete" "content_filter" :error)))
    (let* ((response (evo.provider::jobj "status" (first case) "output" #()
                                        "incomplete_details" (evo.provider::jobj "reason" (second case))))
           (result (evo.provider::responses-result response)))
      (check (format nil "responses terminal ~s" case) (eq (third case) (pget result :stop-reason)))))
  (let ((result (responses-fixture-parse
                 '("{\"type\":\"response.completed\",\"response\":{\"status\":\"completed\",\"output\":[{\"type\":\"message\",\"content\":[{\"type\":\"refusal\",\"refusal\":\"Cannot help\"}]}]}}"))))
    (check "responses refusal preserved" (equal "Cannot help" (pget (first (pget result :content)) :text))))
  (check "responses EOF is not success" (null (pget (responses-fixture-parse '("[DONE]")) :stopped-p)))
  (check "responses cancellation" (pget (responses-fixture-parse '("{}") :abort-flag (constantly t)) :aborted-p))
  (dolist (bad '("{bad" "{\"type\":\"error\",\"message\":\"invalid\"}"
                 "{\"type\":\"response.failed\",\"response\":{\"status\":\"failed\",\"error\":{\"message\":\"failed\"}}}"
                 "{\"type\":\"response.completed\",\"response\":{\"status\":\"queued\",\"output\":[]}}"))
    (check-signals "responses malformed or failed stream" (responses-fixture-parse (list bad))))
  (let ((block (evo.provider::responses-item-block
                (parse-json "{\"type\":\"function_call\",\"call_id\":\"call_1\",\"name\":\"read\",\"arguments\":\"{broken\"}"))))
    (check "responses malformed tool args handled by tool error path" (pget block :arguments-error)))
  (let ((result (evo.provider::responses-result
                 (parse-json "{\"status\":\"incomplete\",\"incomplete_details\":{\"reason\":\"max_output_tokens\"},\"output\":[{\"type\":\"function_call\",\"call_id\":\"call_1\",\"name\":\"read\",\"arguments\":\"{\"}]}"))))
    (check "responses incomplete calls cannot execute"
           (and (eq :length (pget result :stop-reason)) (null (pget result :content))))))

(defun test-responses-extra ()
  (let* ((api (find-api :openai-responses))
         (messages '((:role :tool-result :tool-call-id "call_image" :is-error t
                      :content ((:type :text :text "Image result")
                                (:type :image :data "YWJj" :media-type "image/png")))))
         (request (parse-json (build-request api :model (responses-fixture-model)
                                             :messages messages)))
         (item (aref (gethash "input" request) 0)))
    (check "responses text tool output uses the string wire shape"
           (equal "ok"
                  (evo.provider::responses-tool-output
                   '((:type :text :text "ok")))))
    (check "responses multimodal tool output"
           (and (equal "call_image" (gethash "call_id" item))
                (equal "input_image" (gethash "type" (aref (gethash "output" item) 1)))))
    (let* ((request (parse-json (build-request api :model (pput (responses-fixture-model) :vision nil)
                                              :messages messages)))
           (output (gethash "output" (aref (gethash "input" request) 0))))
      (check "responses blind model degrades tool-result images"
             (and (stringp output) (search "no vision" output))))
    (check "responses default summary"
           (equal "auto" (evo.provider::jget
                          (parse-json (build-request api :model (responses-fixture-model)))
                          "reasoning" "summary")))
    (check "responses requests encrypted reasoning for stateless replay"
           (equal '("reasoning.encrypted_content")
                  (coerce (gethash "include"
                                   (parse-json (build-request api :model (responses-fixture-model))))
                          'list)))
    (check "responses summary can be omitted explicitly"
           (null (evo.provider::jget
                  (parse-json (build-request api :model (responses-fixture-model "{\"reasoning\":{}}")))
                  "reasoning" "summary")))
    (let* ((request (parse-json
                     (build-request api :model (responses-fixture-model
                                                "{\"tools\":[{\"type\":\"web_search\"}],\"tool_choice\":\"auto\",\"include\":[\"web_search_call.action.sources\"],\"prompt_cache_key\":\"test\"}"))))
           (tools (gethash "tools" request)))
      (check "responses hosted tools and cache controls"
             (and (equal "web_search" (gethash "type" (aref tools 0)))
                  (equal "test" (gethash "prompt_cache_key" request))
                  (equal '("web_search_call.action.sources" "reasoning.encrypted_content")
                         (coerce (gethash "include" request) 'list))))))
  (let* ((item (parse-json "{\"type\":\"web_search_call\",\"id\":\"ws_1\",\"status\":\"completed\",\"action\":{\"type\":\"search\",\"query\":\"test\"}}"))
         (block (evo.provider::responses-item-block item)))
    (check "responses hosted calls are not local executable calls"
           (and (eq :text (pget block :type))
                (equal "ws_1" (gethash "id" (parse-json (pget block :responses-item-json)))))))
  (check "responses compaction counts each output message item"
         (= 3 (evo.kernel::count-message-items
               '(:role :assistant :api :openai-responses
                 :content ((:type :text :text "first") (:type :text :text "second")
                           (:type :thinking :thinking "plan"))))))
  (check-signals "responses unknown output item is explicit"
                 (evo.provider::responses-item-block (parse-json "{\"type\":\"new_client_tool_call\"}")))
  (check-signals "responses incomplete reason is explicit"
                 (evo.provider::responses-result
                  (parse-json "{\"status\":\"incomplete\",\"output\":[],\"incomplete_details\":{\"reason\":\"new_reason\"}}")))
  (check-signals "responses terminal status and event must match"
                 (responses-fixture-parse
                  '("{\"type\":\"response.completed\",\"response\":{\"status\":\"incomplete\",\"output\":[]}}"))))
