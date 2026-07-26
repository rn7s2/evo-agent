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
    (check "oai sse tool-call-start event carries call_id"
           (let ((ev (find :tool-call-start events
                           :key (lambda (e) (pget e :type)))))
             (equal (pget ev :id) "call_1"))))
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
    (evo.tui::history-remember tui "/plan")
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
                 (equal (evo.journal:state-tools state) evo.tui::*plan-mode-tools*))
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
        ;; completion candidates include the new commands
        (let ((commands (evo.tui::all-commands)))
          (check "mode is a completion candidate"
                 (assoc "mode" commands :test #'string=))
          (check "exit is a completion candidate"
                 (assoc "exit" commands :test #'string=)))))))

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
                (equal (cdr (assoc "description" front :test #'equal)) "a demo skill")))))

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
           (let ((lore (all-lore)))
             (check "lore round trip"
                    (equal lore '("always run tests" "prefer rg over grep"))))
           (let ((prompt (build-system-prompt nil :lore (all-lore))))
             (check "lore injected into prompt"
                    (search "prefer rg over grep" prompt))))
      (evo.port:setenv "EVO_HOME"
                       (namestring (uiop:ensure-directory-pathname
                                    (format nil "~a/evo-unit-home" (or (uiop:getenv "TMPDIR") "/tmp"))))))))

;;; Plan-mode extension (seed corpus): the :tool-call gate and the
;;; transform-context filter, exercised directly at the hook level.

(defun test-plan-mode ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-plan-~a/" (uiop:getenv "TMPDIR") (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (evo:*agent* agent))
    (evo.kernel:load-extension*
     (merge-pathnames "extensions/plan-mode.lisp" (uiop:getcwd))
     :record nil)
    ;; auto (default): write passes, context untouched.
    (multiple-value-bind (args blocked-p)
        (evo.kernel::intercept-tool-call "write" '(:path "x" :content "y"))
      (declare (ignore args))
      (check "auto mode allows write" (not blocked-p)))
    ;; plan: write blocked, bash allowlisted.
    (evo:set-custom-state "mode" "plan" agent)
    (multiple-value-bind (args blocked-p reason)
        (evo.kernel::intercept-tool-call "write" '(:path "x" :content "y"))
      (declare (ignore args))
      (check "plan mode blocks write" blocked-p)
      (check "plan mode reason" (search "plan mode" reason)))
    (multiple-value-bind (args blocked-p)
        (evo.kernel::intercept-tool-call "bash" '(:command "git status"))
      (declare (ignore args))
      (check "plan mode allows git status" (not blocked-p)))
    (multiple-value-bind (args blocked-p)
        (evo.kernel::intercept-tool-call "bash" '(:command "rm -rf build"))
      (declare (ignore args))
      (check "plan mode blocks rm" blocked-p))
    ;; transform-context: plan-mode injections filtered out when mode is off.
    (let ((messages (list '(:role :user :content ((:type :text :text "hi")))
                          '(:role :user :meta (:key "plan-mode")
                            :content ((:type :text :text "PLAN MODE"))))))
      (flet ((run-transforms (msgs)
               (dolist (hook (gethash :transform-context evo.kernel::*event-hooks*) msgs)
                 (setf msgs (funcall hook msgs)))))
        (check "plan messages kept while planning"
               (= 2 (length (run-transforms messages))))
        (evo:set-custom-state "mode" "auto" agent)
        (check "plan messages filtered in auto"
               (= 1 (length (run-transforms messages))))))))

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
    (test-render-anchor)
    (test-input-history)
    (test-mode-switching)
    (test-goal-budget)
    (test-templates)
    (test-compaction)
    (test-lore)
    (test-plan-mode)
    (format t "~%~d passed, ~d failed~%" *pass* *fail*)
    (if (zerop *fail*) 0 1)))
