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

(defun tmp-dir ()
  "Scratch directory for test fixtures, as a string with no trailing
separator.  UIOP:TEMPORARY-DIRECTORY is the portable answer: it honours
TMPDIR/TMP where set and falls back to /tmp, and on Windows it is the real
per-user temp directory.  (The old hardcoded \"/tmp\" fallback only worked on
Windows when C:\\tmp happened to exist — a fresh box has no /tmp there, and
every fixture write would fail.)"
  (string-right-trim "/\\" (namestring (uiop:temporary-directory))))

(defun stub-emit-program (line)
  "A runnable stub program that prints LINE to stdout and exits 0, ignoring
its arguments.  Stands in for an external tool whose contract is \"argv in,
one line out\" — a clipboard helper, PowerShell.  Portable: a .cmd on
Windows (SBCL's RUN-PROGRAM launches .cmd directly), a chmod+x /bin/sh
script elsewhere.  Returns its path."
  (if (evo.port:windows-p)
      (let ((path (format nil "~a/evo-stub-~a.cmd" (tmp-dir) (gen-id 8))))
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :external-format :latin-1)
          ;; .cmd wants CR-LF; @echo off keeps the command itself off stdout.
          (format out "@echo off~c~%echo ~a~c~%" #\Return line #\Return))
        path)
      (let ((path (format nil "~a/evo-stub-~a" (tmp-dir) (gen-id 8))))
        (with-open-file (out path :direction :output :if-exists :supersede)
          (format out "#!/bin/sh~%echo '~a'~%" line))
        (uiop:run-program (list "/bin/chmod" "+x" path) :ignore-error-status t)
        path)))

(defun stub-copy-program ()
  "A runnable stub program that copies its first argument to its second — a
fake image downscaler / converter, exercising the RUN-CHILD path with a real
external program.  Portable: a .cmd using COPY on Windows, /bin/cp elsewhere.
Returns the program spec RUN-CHILD wants (an absolute path, or \"cp\").

The Windows body normalizes the arguments to backslashes first: SBCL hands
COPY the forward-slash namestrings it produces, and cmd's COPY builtin reports
\"cannot find the file\" on those."
  (if (evo.port:windows-p)
      (let ((path (format nil "~a/evo-copy-~a.cmd" (tmp-dir) (gen-id 8))))
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :external-format :latin-1)
          ;; Explicit CR-LF, no ~<newline> FORMAT continuation (banned tree-wide).
          (dolist (line '("@echo off"
                          "setlocal"
                          "set \"src=%~1\""
                          "set \"dst=%~2\""
                          "set \"src=%src:/=\\%\""
                          "set \"dst=%dst:/=\\%\""
                          "copy /y \"%src%\" \"%dst%\" >nul"
                          "exit /b %ERRORLEVEL%"))
            (write-string line out)
            (write-char #\Return out)
            (write-char #\Newline out)))
        path)
      "cp"))

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
               (format nil "~a/evo-test-~a/" (tmp-dir) (gen-id))))
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
      (check "model fold: no provider for a bare-id entry"
             (null (state-model-provider state)))
      (check "tools fold" (equal (state-tools state) '("bash"))))
    (append-entry journal '(:type :model-change :model "m3" :provider :proxy-co))
    (let ((state (fold-state journal)))
      (check "model fold: provider-journaled model" (equal (state-model state) "m3"))
      (check "model fold: provider folds through"
             (equal (state-model-provider state) :proxy-co)))))

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
  (format nil (cat "event: message_start~%data: {\"type\":\"message_start\",\"message\":{\"model\":\"m\",\"usage\":{\"input_tokens\":7}}}~%~%"
                   "event: content_block_start~%data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}~%~%"
                   "event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"hm\"}}~%~%"
                   "event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"c2ln\"}}~%~%"
                   "event: content_block_stop~%data: {\"type\":\"content_block_stop\",\"index\":0}~%~%"
                   "event: content_block_start~%data: {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tc1\",\"name\":\"bash\"}}~%~%"
                   "event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"comm\"}}~%~%"
                   "event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"and\\\": \\\"ls\\\"}\"}}~%~%"
                   "event: content_block_stop~%data: {\"type\":\"content_block_stop\",\"index\":1}~%~%"
                   "event: message_delta~%data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":42}}~%~%"
                   "event: message_stop~%data: {\"type\":\"message_stop\"}~%~%")))

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

;;; Wire message shaping — MESSAGES->JSON.
;;;
;;; The journal writes one entry per tool result and one per thing the user
;;; said; the wire wants a turn per role.  Merging them is required (parallel
;;; tool calls answer in ONE user message), and mixing them is refused (Kimi
;;; Code answers 400 when the message answering a tool call also carries
;;; text, which is what a user typing while a tool ran used to produce).

(defun wire-messages (messages)
  "MESSAGES->JSON as a list of (role . block-type-list)."
  (loop for m across (evo.provider::messages->json messages)
        collect (cons (gethash "role" m)
                      (loop for b across (gethash "content" m)
                            collect (gethash "type" b)))))

(defun test-wire-message-shaping ()
  (let ((assistant '(:role :assistant :model "m" :stop-reason :tool-use
                     :usage (:input 1 :output 1 :cache-read 0 :cache-write 0)
                     :content ((:type :tool-call :id "a" :name "bash" :arguments nil))))
        (result-a '(:role :tool-result :tool-call-id "a" :tool-name "bash"
                    :is-error nil :content ((:type :text :text "ok"))))
        (result-b '(:role :tool-result :tool-call-id "b" :tool-name "bash"
                    :is-error nil :content ((:type :text :text "ok"))))
        (said '(:role :user :content ((:type :text :text "继续"))))
        (said-again '(:role :user :content ((:type :text :text "继续")))))
    ;; The reported break: a tool result followed by everything the user
    ;; typed while it ran, merged into one user message with the result.
    (check "a tool result never shares a message with text"
           (equal '(("assistant" "tool_use")
                    ("user" "tool_result")
                    ("user" "text" "text"))
                  (wire-messages (list assistant result-a said said-again))))
    ;; Parallel tool calls still answer in ONE message, or a tool_use is left
    ;; unanswered.
    (check "tool results merge with each other"
           (equal '(("assistant" "tool_use")
                    ("user" "tool_result" "tool_result"))
                  (wire-messages (list assistant result-a result-b))))
    (check "plain user turns still merge"
           (equal '(("user" "text" "text"))
                  (wire-messages (list said said-again))))
    ;; Results lead their run whatever order the journal holds: the answer to
    ;; a tool call lands in the message directly after it.
    (check "results lead the run they belong to"
           (equal '(("assistant" "tool_use")
                    ("user" "tool_result")
                    ("user" "text"))
                  (wire-messages (list assistant said result-a))))
    (check "a new assistant turn closes the run"
           (equal '(("assistant" "tool_use")
                    ("user" "tool_result")
                    ("assistant" "tool_use")
                    ("user" "tool_result"))
                  (wire-messages (list assistant result-a assistant result-b))))))

;;; Kimi Code provider — extensions/020-kimi-provider.lisp
;;;
;;; The extension is configuration, not a wire protocol: Kimi Code's
;;; Anthropic-compatible endpoint is driven by the kernel's own
;;; :anthropic-messages adapter.  So what is tested here is the config —
;;; endpoint, both K3 ids, and the one thing K3 needs that stock Anthropic
;;; models do not: effort as the only thinking dial, with no `thinking`
;;; object on the wire (a budget is ignored there, and a disabled thinking
;;; object routes the request to an older model).

(defun test-kimi-provider ()
  (let ((saved-models evo.provider::*models*)
        (saved-providers (copy-alist evo.provider::*providers*))
        (env-names '("KIMI_API_KEY" "KIMI_BASE_URL"))
        (key "sk-kimi-test-key-0123456789"))
    (let ((saved-env (mapcar (lambda (name) (cons name (getenv name))) env-names)))
      (unwind-protect
           (flet ((load-extension ()
                    (load (merge-pathnames "extensions/020-kimi-provider.lisp"
                                           (uiop:getcwd))
                          :verbose nil :print nil))
                  (forget-provider ()
                    (setf evo.provider::*providers*
                          (remove :kimi evo.provider::*providers* :key #'car))))
             (dolist (name env-names) (evo.port:setenv name ""))
             (evo.port:setenv "KIMI_API_KEY" key)
             (load-extension)
             (let ((config (provider-config :kimi)))
               (check "kimi provider base url"
                      (equal (pget config :base-url) "https://api.kimi.com/coding"))
               (check "kimi provider key from KIMI_API_KEY"
                      (equal (pget config :api-key) key)))
             (let ((k3 (find-model "k3" :kimi))
                   (k3-256k (find-model "k3-256k" :kimi)))
               (check "kimi models speak the kernel's anthropic adapter"
                      (and (eq (pget k3 :api) :anthropic-messages)
                           (eq (pget k3-256k :api) :anthropic-messages)))
               (check "kimi context windows"
                      (and (= (model-context-window k3) 1048576)
                           (= (model-context-window k3-256k) 262144)))
               (check "kimi max output" (= (model-max-output k3) 131072))
               (check "kimi models see"
                      (and (model-vision-p k3) (model-vision-p k3-256k)))
               ;; K3's official rungs, exactly: evo clamps an off-ladder
               ;; level down itself, instead of letting the endpoint round
               ;; medium up to high and xhigh up to max.
               (check "kimi effort ladder is the official three rungs"
                      (equal (model-effort k3) '(:low :high :max)))
               (check "kimi thinking mode is effort-only"
                      (and (eq (model-thinking-mode k3) :effort-only)
                           (eq (model-thinking-mode k3-256k) :effort-only)))
               ;; Request shape: effort dial, and no thinking object at all.
               (let* ((raw (evo.provider::build-request-json
                            :model k3-256k :system "sys"
                            :messages '((:role :user
                                         :content ((:type :text :text "go"))))
                            :thinking-level :max))
                      (req (com.inuoe.jzon:parse raw)))
                 (check "kimi req model" (equal (evo.provider::jget req "model")
                                                "k3-256k"))
                 (check "kimi req sends an official rung verbatim"
                        (equal (evo.provider::jget req "output_config" "effort")
                               "max"))
                 (check "kimi req sends no thinking object"
                        (not (search "\"thinking\"" raw)))
                 (check "kimi req max_tokens" (= (evo.provider::jget req "max_tokens")
                                                 131072)))
               ;; Off-ladder rungs clamp DOWN to an official one on the way
               ;; out — never up: a request must not spend more than asked.
               (flet ((effort-at (level)
                        (evo.provider::jget
                         (com.inuoe.jzon:parse
                          (evo.provider::build-request-json
                           :model k3 :system nil
                           :messages '((:role :user
                                        :content ((:type :text :text "go"))))
                           :thinking-level level))
                         "output_config" "effort")))
                 (check "kimi req clamps medium down to low"
                        (equal (effort-at :medium) "low"))
                 (check "kimi req clamps xhigh down to high"
                        (equal (effort-at :xhigh) "high")))
               ;; Both spellings of the base URL work: the adapter supplies
               ;; /v1/messages, so a configured /v1 would double it.
               (check "kimi base url normalizes a trailing /v1"
                      (equal (funcall (symbol-function
                                       (find-symbol "KIMI--NORMALIZE-BASE-URL"
                                                    :evo.user))
                                      "https://api.kimi.com/coding/v1")
                             "https://api.kimi.com/coding"))
               ;; End to end: model -> endpoint url + x-api-key auth.
               (let ((saved-post (symbol-function 'dex:post))
                     (sse (format nil (cat "event: message_start~%data: {\"type\":\"message_start\",\"message\":{\"model\":\"k3-256k\",\"usage\":{\"input_tokens\":10,\"output_tokens\":0}}}~%~%"
                                           "event: content_block_start~%data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}~%~%"
                                           "event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}~%~%"
                                           "event: message_delta~%data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":2}}~%~%"
                                           "event: message_stop~%data: {\"type\":\"message_stop\"}~%~%")))
                     (seen nil))
                 (unwind-protect
                      (progn
                        (setf (symbol-function 'dex:post)
                              (lambda (url &rest args)
                                (setf seen (list :url url :args args))
                                (flexi-streams:make-in-memory-input-stream
                                 (flexi-streams:string-to-octets
                                  sse :external-format :utf-8))))
                        (let ((message (call-provider
                                        :model k3-256k :system "sys"
                                        :messages '((:role :user
                                                     :content ((:type :text :text "go"))))
                                        :thinking-level :high)))
                          (check "kimi call: endpoint url"
                                 (equal (pget seen :url)
                                        "https://api.kimi.com/coding/v1/messages"))
                          (check "kimi call: x-api-key auth"
                                 (equal (cdr (assoc "x-api-key"
                                                    (pget (pget seen :args) :headers)
                                                    :test #'equal))
                                        key))
                          (check "kimi call: assistant message"
                                 (and (eq (pget message :role) :assistant)
                                      (eq (pget message :provider) :kimi)
                                      (equal (pget message :model) "k3-256k")))))
                   (setf (symbol-function 'dex:post) saved-post))))
             ;; Config beats the environment.  init.lisp is evaluated before
             ;; extensions load and register-provider merges with the later
             ;; call winning, so the extension has to fill in only what config
             ;; left out — otherwise a key or endpoint written in init.lisp
             ;; would be silently undone by the extension that follows it.
             (forget-provider)
             (register-provider* :kimi :base-url "https://kimi.example/proxy"
                                       :api-key "sk-from-init")
             (load-extension)
             (let ((config (provider-config :kimi)))
               (check "kimi: a base url from init.lisp survives the extension"
                      (equal (pget config :base-url) "https://kimi.example/proxy"))
               (check "kimi: a key from init.lisp beats the env"
                      (equal (pget config :api-key) "sk-from-init")))
             ;; With nothing configured, KIMI_BASE_URL fills the gap.
             (forget-provider)
             (evo.port:setenv "KIMI_BASE_URL" "https://kimi.example/other/v1")
             (load-extension)
             (check "kimi: KIMI_BASE_URL used when config is silent"
                    (equal (pget (provider-config :kimi) :base-url)
                           "https://kimi.example/other")))
        (dolist (pair saved-env)
          (evo.port:setenv (car pair) (or (cdr pair) "")))
        (setf evo.provider::*models* saved-models
              evo.provider::*providers* saved-providers)))))

;;; Timeouts and proxy env detection

(defun test-port-timeout ()
  (check "portable timeout returns completed value"
         (= (evo.port:call-with-timeout 1 (lambda () 42)) 42))
  (check "portable timeout signals timeout-error"
         (handler-case
             (progn
               (evo.port:call-with-timeout 0.01 (lambda () (sleep 1)))
               nil)
           (evo.port:timeout-error () t))))

(defun test-env-proxy ()
  (let ((saved (mapcar (lambda (v) (cons v (getenv v)))
                       '("HTTPS_PROXY" "https_proxy" "HTTP_PROXY" "http_proxy"
                         "NO_PROXY" "no_proxy")))
        (url "https://api.anthropic.com/v1/messages"))
    (unwind-protect
         (progn
           (dolist (pair saved) (evo.port:setenv (car pair) ""))
           (check "no proxy env" (null (evo.util:env-proxy url)))
           (evo.port:setenv "http_proxy" "http://lower:3128")
           (check "lowercase http_proxy detected"
                  (equal (evo.util:env-proxy url) "http://lower:3128"))
           (evo.port:setenv "https_proxy" "http://lowers:3128")
           (check "lowercase https_proxy preferred"
                  (equal (evo.util:env-proxy url) "http://lowers:3128"))
           (evo.port:setenv "HTTPS_PROXY" "http://upper:3128")
           (check "uppercase still wins"
                  (equal (evo.util:env-proxy url) "http://upper:3128"))
           (check "loopback bypasses proxy"
                  (null (evo.util:env-proxy "http://127.0.0.1:8787/v1/messages")))
           (check "localhost bypasses proxy"
                  (null (evo.util:env-proxy "http://localhost:8787/v1/messages")))
           (evo.port:setenv "no_proxy" "example.com, anthropic.com")
           (check "no_proxy suffix match bypasses"
                  (null (evo.util:env-proxy url)))
           (evo.port:setenv "no_proxy" "example.com")
           (check "no_proxy non-match still proxies"
                  (equal (evo.util:env-proxy url) "http://upper:3128"))
           (evo.port:setenv "no_proxy" "*")
           (check "no_proxy star bypasses everything"
                  (null (evo.util:env-proxy url))))
      (dolist (pair saved)
        (evo.port:setenv (car pair) (or (cdr pair) ""))))))

(defun test-claude-oauth-proxy-guards ()
  (let* ((env-names '("HTTPS_PROXY" "https_proxy" "HTTP_PROXY" "http_proxy"
                      "NO_PROXY" "no_proxy" "CLAUDE_OAUTH_ACCESS_TOKEN"))
         (saved-env (mapcar (lambda (name) (cons name (getenv name))) env-names))
         (saved-get (symbol-function 'dex:get))
         (saved-post (symbol-function 'dex:post))
         (saved-models evo.provider::*models*)
         (saved-providers (copy-alist evo.provider::*providers*))
         (calls nil)
         (proxy "http://lowercase-proxy:3128"))
    (unwind-protect
         (progn
           (dolist (name env-names) (evo.port:setenv name ""))
           (evo.port:setenv "https_proxy" proxy)
           (evo.port:setenv "CLAUDE_OAUTH_ACCESS_TOKEN" "test-access-token")
           (setf (symbol-function 'dex:get)
                 (lambda (url &rest args)
                   (push (list :get url args) calls)
                   "{\"data\":[]}"))
           (setf (symbol-function 'dex:post)
                 (lambda (url &rest args)
                   (push (list :post url args) calls)
                   "{\"access_token\":\"access\",\"refresh_token\":\"refresh\",\"expires_in\":3600}"))
           (load (merge-pathnames "extensions/020-claude-oauth-provider.lisp"
                                  (uiop:getcwd))
                 :verbose nil :print nil)
           (let ((sha256 (symbol-function (find-symbol "SHA256-HEX" :evo.user))))
             (check "claude oauth SHA256 hashes an empty string without a shell"
                    (string= (funcall sha256 "")
                             "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
             (check "claude oauth SHA256 hashes a multi-block input"
                    (string= (funcall sha256 (make-string 100 :initial-element #\a))
                             "2816597888e4a0d3a36b82b83316ab32680eb8f00f8cd3b904d681246d285a0e"))
             (check "claude oauth SHA256 hashes UTF-8 input"
                    (string= (funcall sha256 "你好")
                             "670d9743542cae3ea7ebe36af56bd53648b0a1126162e78d81a32934a711302e")))
           (setf calls nil)
           (funcall (symbol-function
                     (find-symbol "CLAUDE-OAUTH--EXCHANGE-CODE" :evo.user))
                    "code" "verifier" "http://localhost/callback" "state")
           (funcall (symbol-function
                     (find-symbol "CLAUDE-OAUTH--REFRESH-TOKEN" :evo.user))
                    "refresh")
           (check "claude oauth guards all outbound requests"
                  (= (length calls) 2))
           (check "claude oauth uses lowercase environment proxy"
                  (every (lambda (call)
                           (equal (getf (third call) :proxy) proxy))
                         calls)))
      (setf (symbol-function 'dex:get) saved-get
            (symbol-function 'dex:post) saved-post)
      (setf evo.provider::*models* saved-models
            evo.provider::*providers* saved-providers)
      (dolist (pair saved-env)
        (evo.port:setenv (car pair) (or (cdr pair) ""))))))

(defun test-claude-oauth-auto-refresh ()
  ;; Test ensure-valid-token: fresh token → no-op, expired token → refresh.
  (let* ((env-names '("HTTPS_PROXY" "https_proxy" "HTTP_PROXY" "http_proxy"
                      "NO_PROXY" "no_proxy" "CLAUDE_OAUTH_ACCESS_TOKEN"
                      "CLAUDE_OAUTH_REFRESH_TOKEN"))
         (saved-env (mapcar (lambda (name) (cons name (getenv name))) env-names))
         (saved-post (symbol-function 'dex:post))
         (saved-get (symbol-function 'dex:get))
         (saved-models evo.provider::*models*)
         (saved-providers (copy-alist evo.provider::*providers*))
         (refresh-calls nil)
         (token-dir (merge-pathnames "claude-oauth/"
                                     (or (getenv "EVO_HOME")
                                         (merge-pathnames ".evo/" (user-homedir-pathname)))))
         (token-file (merge-pathnames "token.sexp" token-dir)))
    (unwind-protect
         (progn
           ;; Clean environment and token file.
           (dolist (name env-names) (evo.port:setenv name ""))
           (when (probe-file token-file) (delete-file token-file))
           ;; Mock dex:get (belt and braces: the extension makes no load-time
           ;; requests any more) and dex:post (refresh).
           (setf (symbol-function 'dex:get)
                 (lambda (url &rest args)
                   (declare (ignore url args))
                   "{\"data\":[]}"))
           (setf (symbol-function 'dex:post)
                 (lambda (url &rest args)
                   (push (list url args) refresh-calls)
                   "{\"access_token\":\"refreshed-at\",\"refresh_token\":\"refreshed-rt\",\"expires_in\":3600}"))
           (load (merge-pathnames "extensions/020-claude-oauth-provider.lisp"
                                  (uiop:getcwd))
                 :verbose nil :print nil)
           (let* ((now (funcall (find-symbol "CLAUDE-OAUTH--NOW-MS" :evo.user)))
                  (refresh-fn (symbol-function
                               (find-symbol "CLAUDE-OAUTH--ENSURE-VALID-TOKEN" :evo.user)))
                  (write-tokens-fn (symbol-function
                                    (find-symbol "CLAUDE-OAUTH--WRITE-TOKENS" :evo.user)))
                  (read-tokens-fn (symbol-function
                                   (find-symbol "CLAUDE-OAUTH--READ-TOKENS" :evo.user))))
             ;; 1. No stored file — should return NIL, no refresh.
             (setf refresh-calls nil)
             (check "auto-refresh: no stored file → nil"
                    (null (funcall refresh-fn)))
             (check "auto-refresh: no stored file → no refresh call"
                    (null refresh-calls))
             ;; 2. Fresh token — should return the stored token, no refresh.
             (funcall write-tokens-fn "sk-ant-oat-fresh" "rt-fresh"
                      (+ now 3600000) (+ now 2592000000))
             (setf refresh-calls nil)
             (check "auto-refresh: fresh token → no refresh"
                    (string= (funcall refresh-fn) "sk-ant-oat-fresh"))
             (check "auto-refresh: fresh token → no dex:post"
                    (null refresh-calls))
             ;; 3. Expired token (0s remaining) — should refresh and return new token.
             (funcall write-tokens-fn "sk-ant-oat-expired" "rt-expired"
                      now (+ now 2592000000))
             (setf refresh-calls nil)
             (check "auto-refresh: expired token → refreshed"
                    (string= (funcall refresh-fn) "refreshed-at"))
             (check "auto-refresh: expired token → called dex:post"
                    (and refresh-calls (= (length refresh-calls) 1)))
             ;; Verify the stored file was updated.
             (let ((updated (funcall read-tokens-fn)))
               (check "auto-refresh: stored token updated"
                      (string= (getf updated :access-token) "refreshed-at"))
               (check "auto-refresh: stored refresh token updated"
                      (string= (getf updated :refresh-token) "refreshed-rt")))
             ;; 4. Expired token can use refresh token from the environment.
             (evo.port:setenv "CLAUDE_OAUTH_REFRESH_TOKEN" "rt-from-env")
             (funcall write-tokens-fn "sk-ant-oat-expired-env" nil
                      now nil)
             (setf refresh-calls nil)
             (check "auto-refresh: env refresh token → refreshed"
                    (string= (funcall refresh-fn) "refreshed-at"))
             (check "auto-refresh: env refresh token used"
                    (and refresh-calls
                         (search "\"refresh_token\":\"rt-from-env\""
                                 (getf (second (first refresh-calls)) :content))))
             (evo.port:setenv "CLAUDE_OAUTH_REFRESH_TOKEN" "")
             ;; 5. Refresh token expired — should signal provider-error.
             (funcall write-tokens-fn "sk-ant-oat-expired2" "rt-expired2"
                      (- now 1000) (- now 1000))
             (setf refresh-calls nil)
             (check "auto-refresh: expired refresh token → error"
                    (handler-case
                        (progn (funcall refresh-fn) nil)
                      (provider-error (e)
                        (and (search "refresh token expired"
                                     (funcall (read-from-string "evo.provider::provider-error-message") e)
                                     :test #'char-equal)
                             t))))
             ;; 6. Auto-refresh disabled — should return stored token without refresh.
             (setf (symbol-value
                    (find-symbol "*CLAUDE-OAUTH-AUTO-REFRESH*" :evo.user)) nil)
             (funcall write-tokens-fn "sk-ant-oat-stale" "rt-stale"
                      (- now 1000) (+ now 2592000000))
             (setf refresh-calls nil)
             (check "auto-refresh: disabled → no refresh"
                    (string= (funcall refresh-fn) "sk-ant-oat-stale"))
             (check "auto-refresh: disabled → no dex:post"
                    (null refresh-calls))
             ;; Re-enable auto-refresh for the next test run.
             (setf (symbol-value
                    (find-symbol "*CLAUDE-OAUTH-AUTO-REFRESH*" :evo.user)) t)))
      (setf (symbol-function 'dex:post) saved-post
            (symbol-function 'dex:get) saved-get)
      (setf evo.provider::*models* saved-models
            evo.provider::*providers* saved-providers)
      (dolist (pair saved-env)
        (evo.port:setenv (car pair) (or (cdr pair) "")))
      (when (probe-file token-file) (delete-file token-file)))))

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
  ;; "Too big to read in the editbox" is measured in both directions: one
  ;; enormous line is as unreadable as a dozen short ones, and either would
  ;; push the region past the bottom of the screen.
  (let ((eb (evo.tui::make-edit-buffer))
        (wide (make-string 1500 :initial-element #\x)))
    (evo.tui::eb-paste eb wide)
    (check "a single huge line collapses too"
           (search "[paste #1: 1500 chars]" (evo.tui::eb-text eb)))
    (check "huge line submits in full" (equal (evo.tui::eb-submit-text eb) wide)))
  (let ((eb (evo.tui::make-edit-buffer)))
    (evo.tui::eb-paste eb (make-string 900 :initial-element #\x))
    (check "a merely long line stays text"
           (= 900 (length (evo.tui::eb-text eb)))))
  ;; Wrapping math.
  (let ((eb (evo.tui::make-edit-buffer)))
    (evo.tui::eb-insert-text eb "0123456789")
    (multiple-value-bind (rows crow ccol) (evo.tui::eb-display-rows eb 4)
      (check "wrap rows" (equal rows '("0123" "4567" "89")))
      (check "wrap cursor" (and (= crow 2) (= ccol 2)))))
  ;; Viewport: the editor scrolls inside its budget instead of painting a
  ;; region taller than the screen (which strands rows in scrollback).
  (let ((rows '("r0" "r1" "r2" "r3" "r4" "r5")))
    (multiple-value-bind (shown crow) (evo.tui::editor-viewport rows 2 10)
      (check "viewport: everything fits, nothing changes"
             (and (equal shown rows) (= crow 2))))
    (multiple-value-bind (shown crow) (evo.tui::editor-viewport rows 1 4)
      (check "viewport: cursor in the head keeps the head" (equal (first shown) "r0"))
      (check "viewport: hidden tail is announced"
             (search "3 more lines below" (car (last shown))))
      (check "viewport: budget respected" (= 4 (length shown)))
      (check "viewport: cursor row unmoved" (= crow 1)))
    (multiple-value-bind (shown crow) (evo.tui::editor-viewport rows 5 4)
      (check "viewport: cursor past the window scrolls it"
             (equal (last shown) '("r5")))
      (check "viewport: hidden head is announced"
             (search "3 more lines above" (first shown)))
      (check "viewport: scrolled budget respected" (= 4 (length shown)))
      (check "viewport: cursor lands on the last row" (= crow 3)))
    (multiple-value-bind (shown crow) (evo.tui::editor-viewport rows 4 1)
      (check "viewport: no room for a marker shows the cursor's row"
             (and (equal shown '("r4")) (zerop crow))))))

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
  ;; TERM-SETUP asks for modifyOtherKeys, so every key it sends back must
  ;; decode — decoding only Enter and Tab silently killed ctrl+v (image
  ;; paste) and ctrl+c on the terminals that honour the request.
  (check "modifyOtherKeys ctrl-v"        ; ESC [ 27;5;118 ~
         (equal (feed-bytes '(27 91 50 55 59 53 59 49 49 56 126)) '((:ctrl #\v))))
  (check "modifyOtherKeys ctrl-c"        ; ESC [ 27;5;99 ~
         (equal (feed-bytes '(27 91 50 55 59 53 59 57 57 126)) '((:ctrl #\c))))
  (check "modifyOtherKeys ctrl-shift-V folds to ctrl-v"
         (equal (feed-bytes '(27 91 50 55 59 54 59 56 54 126)) '((:ctrl #\v))))
  (check "modifyOtherKeys plain char"    ; ESC [ 27;1;97 ~
         (equal (feed-bytes '(27 91 50 55 59 49 59 57 55 126)) '((:char #\a))))
  ;; cmd+v, when a terminal reports super instead of eating the key: the
  ;; paste gesture, and only that one — cmd+c must NOT become ctrl+c (quit).
  (check "kitty cmd+v is the paste gesture"
         (equal (feed-bytes '(27 91 49 49 56 59 57 117)) '((:super #\v))))
  (check "kitty cmd+c is dropped"
         (null (feed-bytes '(27 91 57 57 59 57 117))))
  ;; ctrl+alt+v: the second door to the clipboard, for terminals whose own
  ;; paste binding eats plain ctrl+v (VS Code on Linux/Windows, Windows
  ;; Terminal under WSL).  It has to survive all three encodings, and the
  ;; legacy one — ESC then the control byte — used to be dropped as an
  ;; "unknown alt-key", so the fallback shortcut failed exactly where the
  ;; shortcut it falls back from was already failing.
  (check "ctrl+alt+v legacy (ESC + control byte)"
         (equal (feed-bytes '(27 22)) '((:ctrl #\v))))
  (check "ctrl+alt+v kitty csi-u"        ; ESC [ 118;7 u  (mod 1+2+4)
         (equal (feed-bytes '(27 91 49 49 56 59 55 117)) '((:ctrl #\v))))
  (check "ctrl+alt+v modifyOtherKeys"    ; ESC [ 27;7;118 ~
         (equal (feed-bytes '(27 91 50 55 59 55 59 49 49 56 126)) '((:ctrl #\v))))
  (check "alt+letter is still dropped" (null (feed-bytes '(27 118))))
  (check "alt-enter fallback" (equal (feed-bytes '(27 13)) '(:newline)))
  (check "ctrl-c" (equal (feed-bytes '(3)) '((:ctrl #\c))))
  (check "bracketed paste"
         (equal (feed-bytes (append '(27 91 50 48 48 126)
                                    (map 'list #'char-code "x")
                                    '(27 91 50 48 49 126)))
                '((:paste "x"))))
  ;; The payload crosses the parser untouched: normalizing it is HANDLE-PASTE's
  ;; job and hers alone, so a bracketed paste and a burst cannot drift apart.
  (check "bracketed paste payload is handed over raw"
         (equal (feed-bytes (append '(27 91 50 48 48 126)
                                    (map 'list #'char-code
                                         (format nil "a~cb" #\Return))
                                    '(27 91 50 48 49 126)))
                (list (list :paste (format nil "a~cb" #\Return)))))
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
    (check "split utf8 completes" (equal (evo.tui::parse-keys state) '((:char #\é)))))
  ;; Asking for enhanced key reporting is only graceful where the answer
  ;; can be understood: a terminal that echoes the request instead of
  ;; honouring it would litter the transcript, and an emulator that claims
  ;; a protocol then mangles it needs a way out that is not "stop using
  ;; evo".  TERM-TEARDOWN pops only what TERM-SETUP pushed.
  (check "key protocol: asked for on a real terminal"
         (evo.tui::key-enhancement-wanted-p nil "xterm-256color"))
  (check "key protocol: not on a dumb terminal"
         (null (evo.tui::key-enhancement-wanted-p nil "dumb")))
  (check "key protocol: not without a TERM at all"
         (null (evo.tui::key-enhancement-wanted-p nil "")))
  (check "key protocol: EVO_KEY_ENHANCEMENT=0 is the escape hatch"
         (null (evo.tui::key-enhancement-wanted-p "0" "xterm-256color")))
  (check "key protocol: EVO_KEY_ENHANCEMENT=1 overrides a dumb TERM"
         (evo.tui::key-enhancement-wanted-p "1" "dumb")))

;;; Pasting
;;;
;;; Two shapes reach the editor and both have to arrive intact: a bracketed
;;; paste (payload handed over as data) and a paste from a terminal that
;;; brackets nothing, which is just the clipboard typed at us at machine
;;; speed.  The bug that motivated all of this: line breaks inside a paste
;;; are usually CR — xterm.js (VS Code, Cursor) rewrites every newline in
;;; the clipboard to CR, and raw mode does no CR->LF translation — and evo
;;; dropped them, welding every pasted line into one.

(defun paste-events (tui &rest byte-batches)
  "BYTE-BATCHES, one per poll tick 20ms apart, through the exact path TICK
takes: parse, then fold pasted batches.  A quiet tick is appended, standing
for the moment the user stops feeding the terminal.  A batch given as
(:after MS . BYTES) arrives that many milliseconds after the previous one —
that is how a human's next keystroke differs from a paste's last chunk."
  (loop with clock = 0
        for batch in (append byte-batches (list nil))
        for gap = (if (and (consp batch) (eq (first batch) :after)) (second batch) 20)
        for bytes = (if (and (consp batch) (eq (first batch) :after)) (cddr batch) batch)
        append (progn
                 (incf clock gap)
                 (evo.tui::in-push-bytes (evo.tui::tui-input tui)
                                         (coerce bytes 'vector))
                 (evo.tui::tick-key-events
                  tui (evo.tui::parse-keys (evo.tui::tui-input tui)) clock))))

(defun bytes-of (string)
  (coerce (flexi-streams:string-to-octets string :external-format :utf-8) 'list))

(defun bracketed (string)
  (append '(27 91 50 48 48 126) (bytes-of string) '(27 91 50 48 49 126)))

(defun test-paste ()
  ;; 1. Normalization: line breaks in every spelling terminals use, and
  ;; nothing that could steer the terminal on the next repaint.
  (flet ((norm (text) (evo.tui::normalize-paste text)))
    (check "normalize: CR is a line break, not a dropped byte"
           (equal (norm (format nil "one~ctwo" #\Return)) (format nil "one~%two")))
    (check "normalize: CRLF is one line break"
           (equal (norm (format nil "one~c~ctwo" #\Return #\Newline))
                  (format nil "one~%two")))
    (check "normalize: LF passes through"
           (equal (norm (format nil "one~%two")) (format nil "one~%two")))
    (check "normalize: blank lines survive"
           (equal (norm (format nil "a~c~cb" #\Return #\Return))
                  (format nil "a~%~%b")))
    (check "normalize: indentation survives"
           (equal (norm (format nil "def f():~c    return 1" #\Return))
                  (format nil "def f():~%    return 1")))
    (check "normalize: tabs survive"
           (equal (norm (format nil "a~cb" #\Tab)) (format nil "a~cb" #\Tab)))
    ;; A stray ESC would be painted straight back at the terminal by the next
    ;; repaint (SANITIZE-LINE keeps ESC) and swallow the text after it.
    (check "normalize: ANSI sequences are dropped whole"
           (equal (norm (format nil "a~c[31mred~c[0m" #\Escape #\Escape)) "ared"))
    (check "normalize: other control bytes are dropped"
           (equal (norm (format nil "a~cb" (code-char 7))) "ab")))
  ;; 2. A bracketed paste: normalized at the door, then the editor.
  (let ((tui (evo.tui::make-tui)))
    (evo.tui::handle-paste tui (format nil "alpha~cbravo" #\Return))
    (check "bracketed CR paste keeps its lines"
           (equal (evo.tui::eb-text (evo.tui::tui-editor tui))
                  (format nil "alpha~%bravo"))))
  (let ((tui (evo.tui::make-tui)))
    (evo.tui::handle-paste tui (format nil "l1~cl2~cl3~cl4~cl5"
                                       #\Return #\Return #\Return #\Return))
    (check "bracketed CR paste collapses like any other"
           (search "[paste #1: 5 lines]"
                   (evo.tui::eb-text (evo.tui::tui-editor tui))))
    (check "collapsed CR paste submits with real newlines"
           (equal (evo.tui::eb-submit-text (evo.tui::tui-editor tui))
                  (format nil "l1~%l2~%l3~%l4~%l5"))))
  ;; 3. Unbracketed paste: the clipboard typed at us.  Every line break in
  ;; it is the byte Enter sends, so without burst detection the first line
  ;; is submitted as a prompt and the rest race in behind it.
  (let ((tui (evo.tui::make-tui)))
    (check "unbracketed multi-line paste is one paste, and no submit"
           (equal (paste-events tui (bytes-of (format nil "one~ctwo~cthree"
                                                      #\Return #\Return)))
                  (list (list :paste (format nil "one~%two~%three"))))))
  (let ((tui (evo.tui::make-tui)))
    (check "a paste split across ticks stays one paste"
           (equal (paste-events tui
                                (bytes-of (format nil "one~ctw" #\Return))
                                (bytes-of (format nil "o~cthree" #\Return))
                                (bytes-of "!"))
                  (list (list :paste (format nil "one~%two~%three!"))))))
  ;; A line break at the very end is ambiguous: it is either the paste's
  ;; last newline or the user's Enter.  Held for a tick, then released as
  ;; Enter — which is what keeps `tmux send-keys "/help\r"` and every
  ;; scripted driver working.
  (let ((tui (evo.tui::make-tui)))
    (check "a trailing break is the user's Enter"
           (equal (paste-events tui (bytes-of (format nil "one~ctwo~c"
                                                      #\Return #\Return)))
                  (list (list :paste (format nil "one~%two")) :enter))))
  (let ((tui (evo.tui::make-tui)))
    (check "a held break followed by more text was interior after all"
           (equal (paste-events tui
                                (bytes-of (format nil "one~ctwo~c" #\Return #\Return))
                                (bytes-of "three"))
                  (list (list :paste (format nil "one~%two~%three"))))))
  ;; Typing is not pasting: one or two characters per poll batch is a human.
  (let ((tui (evo.tui::make-tui)))
    (check "typing passes through untouched"
           (equal (paste-events tui '(104) '(105) '(13))
                  (list '(:char #\h) '(:char #\i) :enter))))
  (let ((tui (evo.tui::make-tui)))
    (check "a burst of control keys is not a paste"
           (equal (paste-events tui '(27 91 65 27 91 66 27 91 65))
                  (list :up :down :up))))
  ;; Anything that is not text closes the burst, in order.
  (let ((tui (evo.tui::make-tui)))
    (check "a keystroke after a burst ends it, text first"
           (equal (paste-events tui (bytes-of "abcd") '(27 91 68))
                  (list (list :paste "abcd") :left))))
  ;; The burst is not a mode: what a human types after a paste is typing,
  ;; and a human is at least a tenth of a second away.
  (let ((tui (evo.tui::make-tui)))
    (check "typing right after a paste is typing"
           (equal (paste-events tui (bytes-of "abcd") '(:after 400 101) '(:after 400 102))
                  (list (list :paste "abcd") '(:char #\e) '(:char #\f)))))
  ;; ...while the paste's own last chunk, still arriving, belongs to it.
  (let ((tui (evo.tui::make-tui)))
    (check "a short trailing chunk still belongs to the paste"
           (equal (paste-events tui (bytes-of "abcd") (bytes-of "ef"))
                  (list (list :paste "abcdef")))))
  ;; A bracketed paste is a whole event; it must not be re-read as a burst.
  (let ((tui (evo.tui::make-tui)))
    (check "a bracketed paste stays one paste"
           (equal (paste-events tui (bracketed (format nil "a~cb" #\Return)))
                  (list (list :paste (format nil "a~cb" #\Return))))))
  ;; EVO_PASTE_BURST=0: the raw key stream, for input that arrives fast and
  ;; is not a paste.
  (let ((tui (evo.tui::make-tui)))
    (setf (evo.tui::pb-enabled (evo.tui::tui-burst tui)) nil)
    (check "burst detection can be turned off"
           (equal (paste-events tui (bytes-of "ab") '(13))
                  (list '(:char #\a) '(:char #\b) :enter))))
  (check "EVO_PASTE_BURST=0 is the escape hatch"
         (null (evo.tui::paste-burst-wanted-p "0")))
  (check "burst detection is on by default"
         (evo.tui::paste-burst-wanted-p nil))
  ;; A select popup has nothing to paste into: it takes keys raw, and any
  ;; burst in flight is closed out rather than left to reappear later.
  (let ((tui (evo.tui::make-tui)))
    (evo.tui::in-push-bytes (evo.tui::tui-input tui) (coerce (bytes-of "abcd") 'vector))
    (evo.tui::tick-key-events tui (evo.tui::parse-keys (evo.tui::tui-input tui)))
    (setf (evo.tui::tui-mode tui) :select)
    (check "entering a popup releases the burst in flight"
           (equal (evo.tui::tick-key-events tui '(:down))
                  (list (list :paste "abcd") :down))))
  ;; End to end, the way it actually happens: a paste too big for the
  ;; editbox, typed at a terminal that brackets nothing, lands as one
  ;; placeholder and submits in full.
  (let* ((tui (evo.tui::make-tui))
         (text (format nil "def f():~c    return 1~c~cf()" #\Return #\Return #\Return))
         (events (paste-events tui (bytes-of text))))
    (dolist (event events) (evo.tui::handle-key-edit tui event))
    (check "unbracketed paste reaches the editor whole"
           (equal (evo.tui::eb-submit-text (evo.tui::tui-editor tui))
                  (format nil "def f():~%    return 1~%~%f()")))
    (check "unbracketed paste collapsed to a token"
           (search "[paste #1: 4 lines]"
                   (evo.tui::eb-text (evo.tui::tui-editor tui))))))

;;; Status-line segment registry
;;;
;;; The regression behind this: two extensions each wrapped EVO.TUI::STATUS-LINE
;;; and appended to the previous one's string.  The inner wrapper padded to the
;;; full terminal width to right-align itself, so everything the outer one
;;; appended landed past the right edge and DRAW-REGION truncated it away — the
;;; segment was computed correctly every frame and thrown away every frame.

(defun test-status-segments ()
  ;; Core segments are registered at load time, like anybody else's.
  (let ((core (mapcar #'evo.tui::status-segment-name (evo.tui::status-segments :left))))
    (check "core model segment registered" (member :model core))
    (check "core context segment registered" (member :context core)))
  ;; A fresh dynamic binding isolates the rest from the core registrations.
  (let ((evo.tui::*status-segments* nil)
        (evo.tui::*cols* 41)                    ; renderer width = 40
        (tui (evo.tui::make-tui)))
    (flet ((seg (text) (lambda (tui) (declare (ignore tui)) text)))
      (evo.tui:add-status-segment :l1 (seg "L1") :side :left :order 100)
      (evo.tui:add-status-segment :l2 (seg "L2") :side :left :order 200)
      (evo.tui:add-status-segment :r1 (seg "R1") :side :right :order 100)
      (evo.tui:add-status-segment :r2 (seg "R2") :side :right :order 200)
      (let ((line (evo.tui::status-line tui)))
        (check "left: ascending order runs left to right"
               (< (search "L1" line) (search "L2" line)))
        (check "right: ascending order counts inward from the right edge"
               (< (search "R2" line) (search "R1" line)))
        (check "right group sits flush against the right edge"
               (= (evo.tui::visible-length line) 40))
        (check "a right-aligned segment no longer eats the left ones"
               (and (search "L1" line) (search "L2" line)))
        (check "same-side neighbours are separated by the renderer"
               (search " · " line)))
      ;; Registering a name again replaces it: extension reloads are idempotent.
      (evo.tui:add-status-segment :l2 (seg "L2-NEW") :side :left :order 200)
      (let ((line (evo.tui::status-line tui)))
        (check "re-registering a name replaces it" (search "L2-NEW" line))
        (check "re-registering does not duplicate"
               (= 4 (length (evo.tui::status-segments)))))
      ;; A segment rendering nothing leaves no dangling separator or padding.
      (evo.tui::remove-status-segment :l2)
      (evo.tui::remove-status-segment :r1)
      (evo.tui::remove-status-segment :r2)
      (evo.tui:add-status-segment :quiet (seg nil) :side :left :order 300)
      (check "an empty segment contributes nothing at all"
             (string= "L1" (evo.tui::status-line tui)))
      ;; A signalling segment is skipped, not fatal to the whole line.
      (evo.tui:add-status-segment :boom (lambda (tui) (declare (ignore tui))
                                          (error "segment blew up"))
                                  :side :left :order 350)
      (check "a signalling segment is skipped, the line survives"
             (string= "L1" (evo.tui::status-line tui))))
    ;; Overflow: drop from the middle outward, so the segments nearest each
    ;; edge are the last to go.
    (let ((evo.tui::*status-segments* nil)
          (evo.tui::*cols* 21))                 ; renderer width = 20
      (flet ((seg (text) (lambda (tui) (declare (ignore tui)) text)))
        (evo.tui:add-status-segment :l1 (seg "LEFT-ONE") :side :left :order 100)
        (evo.tui:add-status-segment :l2 (seg "LEFT-TWO") :side :left :order 200)
        (evo.tui:add-status-segment :r1 (seg "RIGHT-ONE") :side :right :order 100)
        (evo.tui:add-status-segment :r2 (seg "RIGHT-TWO") :side :right :order 200)
        (let ((line (evo.tui::status-line tui)))
          (check "overflow keeps the segment nearest the left edge"
                 (search "LEFT-ONE" line))
          (check "overflow keeps the segment nearest the right edge"
                 (search "RIGHT-ONE" line))
          (check "overflow drops the inboard left segment"
                 (null (search "LEFT-TWO" line)))
          (check "overflow drops the inboard right segment"
                 (null (search "RIGHT-TWO" line)))
          (check "the fitted line never paints past the width"
                 (<= (evo.tui::visible-length line) 20)))))
    ;; One segment that cannot fit is truncated, not blanked.
    (let ((evo.tui::*status-segments* nil)
          (evo.tui::*cols* 6))                  ; renderer width = 10 (floor)
      (evo.tui:add-status-segment :only (lambda (tui) (declare (ignore tui))
                                          "ABCDEFGHIJKLMNOP")
                                  :side :left :order 100)
      (let ((line (evo.tui::status-line tui)))
        (check "an unfittable lone segment is truncated, not dropped"
               (and (search "ABC" line) (<= (evo.tui::visible-length line) 10)))))
    (check "SIDE is validated"
           (handler-case (progn (evo.tui:add-status-segment :bad (lambda (tui) tui)
                                                           :side :middle)
                                nil)
             (error () t)))))

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
      ;; Activity animation: rotating slash working/compacting, pulsing star thinking,
      ;; static idle glyph.
      (check "idle glyph" (search "○ idle" (evo.tui::activity-line tui)))
      (setf (evo.tui::tui-task tui)
            (evo.tui::make-tui-task :id "probe-run" :kind :run))
      (check "working slash frame 0" (search "| working" (evo.tui::activity-line tui)))
      (incf (evo.tui::tui-spinner tui))
      (check "working slash rotates" (search "/ working" (evo.tui::activity-line tui)))
      (setf (evo.tui::tui-task-kind (evo.tui::tui-task tui)) :compact)
      (check "compacting slash rotates"
             (search "/ compacting..." (evo.tui::activity-line tui)))
      (setf (evo.tui::tui-task-kind (evo.tui::tui-task tui)) :run
            (evo.tui::tui-thinking-tail tui) "hm")
      (check "thinking pulse frame" (search "✳ thinking · hm" (evo.tui::activity-line tui)))
      (setf (evo.tui::tui-thinking-tail tui) "")
      ;; Activity + todo sections + editbox rules.
      (setf (evo.tui::tui-todos tui) (vector '(:status "pending" :text "x")))
      (let ((lines (evo.tui::compose-region tui)))
        (check "separators for activity/todo/editbox"
               (= 4 (count-if (lambda (l) (search "─" l)) lines))))
      ;; A long list hides its completed prefix: the done items at the top
      ;; collapse into a "+N more" and the panel shows from the first
      ;; unfinished item down; a tail still too long hides behind the bottom
      ;; "+N more" as before.
      (flet ((panel (todos)
               (setf (evo.tui::tui-todos tui) todos)
               (evo.tui::compose-region tui))
             ;; The item numbers, in the order the panel displays them, so we
             ;; can assert the list reads top-down and never reversed.
             (item-order (lines)
               (loop for l in lines
                     for pos = (search "item-" l)
                     when pos
                     collect (parse-integer l :start (+ pos 5) :junk-allowed t))))
        (let ((lines (panel (coerce
                             (loop for i from 0 below 12
                                   collect (list :text (format nil "item-~d" i)
                                                 :status (if (< i 3) :done :pending)))
                             'vector))))
          (check "done prefix hides behind a top +N more"
                 (some (lambda (l) (search "+3 more" l)) lines))
          (check "the first unfinished item leads the visible list"
                 (and (some (lambda (l) (search "item-3" l)) lines)
                      (notany (lambda (l) (search "item-0" l)) lines)))
          (check "the overflowing tail still hides behind a bottom +N more"
                 (some (lambda (l) (search "+2 more" l)) lines))
          (check "both markers keep the panel at limit+1 rows"
                 (= 9 (count-if (lambda (l) (or (search "item-" l)
                                                (search "+" l)))
                                lines)))
          (check "the visible items read top-down, not reversed"
                 (equal (item-order lines) '(3 4 5 6 7 8 9))))
        (let ((lines (panel (coerce
                             (loop for i from 0 below 10
                                   collect (list :text (format nil "item-~d" i)
                                                 :status (if (< i 3) :done :pending)))
                             'vector))))
          (check "a top marker alone when the rest fits"
                 (and (some (lambda (l) (search "+3 more" l)) lines)
                      (some (lambda (l) (search "item-9" l)) lines)
                      (= 1 (count-if (lambda (l) (search "+" l)) lines)))))
        ;; Almost everything done, one item left: hiding the whole done
        ;; prefix would leave a single row under a "+9 more" — the panel must
        ;; instead fill up, revealing recent done items above the last one.
        (let ((lines (panel (coerce
                             (loop for i from 0 below 10
                                   collect (list :text (format nil "item-~d" i)
                                                 :status (if (< i 9) :done
                                                             :in-progress)))
                             'vector))))
          (check "an almost-done list fills the panel, not one lonely row"
                 (and (some (lambda (l) (search "+3 more" l)) lines)
                      (some (lambda (l) (search "item-3" l)) lines) ; a done item
                      (some (lambda (l) (search "item-9" l)) lines) ; the last one
                      (notany (lambda (l) (search "item-2" l)) lines)
                      (= 1 (count-if (lambda (l) (search "+" l)) lines))))
          (check "the filled panel reads top-down and holds the last one"
                 (equal (item-order lines) '(3 4 5 6 7 8 9))))
        (let ((lines (panel (coerce
                             (loop for i from 0 below 10
                                   collect (list :text (format nil "item-~d" i)
                                                 :status :pending))
                             'vector))))
          (check "without a done prefix the list shows from the top"
                 (and (some (lambda (l) (search "item-0" l)) lines)
                      (some (lambda (l) (search "+2 more" l)) lines))))
        (let ((lines (panel (coerce
                             (loop for i from 0 below 12
                                   collect (list :text (format nil "item-~d" i)
                                                 :status :done))
                             'vector))))
          ;; Nothing unfinished to anchor on: show the finish line, not the
          ;; stale head — the last items under a top "+N more" (mirror of the
          ;; almost-done fill above).
          (check "an all-done list anchors at the bottom, showing the finish line"
                 (and (some (lambda (l) (search "item-11" l)) lines) ; the last one
                      (some (lambda (l) (search "+5 more" l)) lines)
                      (notany (lambda (l) (search "item-0" l)) lines)
                      (= 1 (count-if (lambda (l) (search "+" l)) lines))))
          (check "the all-done panel reads top-down"
                 (equal (item-order lines) '(5 6 7 8 9 10 11))))
        (let ((lines (panel (coerce
                             (loop for i from 0 below 6
                                   collect (list :text (format nil "item-~d" i)
                                                 :status (if (< i 2) :done :pending)))
                             'vector))))
          (check "a short list never hides anything"
                 (and (some (lambda (l) (search "item-0" l)) lines)
                      (some (lambda (l) (search "item-5" l)) lines)
                      (notany (lambda (l) (search "+" l)) lines)))
          (check "a short list stays in order, not reversed"
                 (equal (item-order lines) '(0 1 2 3 4 5))))))
    ;; The region is painted with relative cursor movement, so it must never
    ;; be taller than the screen: one frame taller than the terminal scrolls
    ;; its own top away and every later frame then clears the wrong rows and
    ;; strands copies in scrollback.  A paste is what makes the editor big
    ;; enough to matter, so the editor is budgeted and the whole region is
    ;; clamped.
    (let ((evo.tui::*rows* 12)
          (tui (evo.tui::make-tui)))
      (setf (evo.tui::tui-model-label tui) "m"
            (evo.tui::tui-thinking-label tui) "medium")
      (evo.tui::eb-paste (evo.tui::tui-editor tui)
                         (format nil "~a~%~a~%~a"
                                 (make-string 170 :initial-element #\a)
                                 (make-string 170 :initial-element #\b)
                                 (make-string 170 :initial-element #\c)))
      (multiple-value-bind (lines crow) (evo.tui::compose-region tui)
        (check "a big paste cannot outgrow the screen"
               (<= (length lines) evo.tui::*rows*))
        (check "the cursor stays inside the region"
               (< -1 crow (length lines)))
        (check "the editor says it is scrolled"
               (some (lambda (l) (search "more lines above" l)) lines)))
      ;; Same guarantee with a full todo panel and a long streaming tail
      ;; under it — nothing in the region may push it past the screen.
      (setf (evo.tui::tui-task tui)
            (evo.tui::make-tui-task :id "region-probe" :kind :run)
            (evo.tui::tui-partial tui) (make-string 900 :initial-element #\z)
            (evo.tui::tui-todo-visible tui) t
            (evo.tui::tui-todos tui)
            (coerce (loop repeat 10 collect '(:status "pending" :text "item")) 'vector))
      (multiple-value-bind (lines crow) (evo.tui::compose-region tui)
        (check "a full region still fits the screen"
               (<= (length lines) evo.tui::*rows*))
        (check "the cursor still stays inside the region"
               (< -1 crow (length lines)))))
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
      (evo.tui::complete-at-point tui)
      (check "unique completion + space"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui)) "/export "))
      ;; Tab in plain text stays a literal tab.
      (evo.tui::eb-clear (evo.tui::tui-editor tui))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "x")
      (evo.tui::complete-at-point tui)
      (check "literal tab outside command"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui))
                    (format nil "x~c" #\Tab))))
    ;; Live completion popup: typing a /command word shows bounded
    ;; suggestions under the editor, alongside the input.
    (let ((tui (evo.tui::make-tui)))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/t")
      (multiple-value-bind (prefix matches kind) (evo.tui::completion-context tui)
        (check "popup active on /prefix" (equal prefix "t"))
        (check "popup knows it is completing a command" (eq :command kind))
        (check "popup filters by prefix"
               (and (<= 3 (length matches))
                    (every (lambda (m) (evo.util:string-prefix-p "t" (car m)))
                           matches)))
        ;; help text: dim, in an aligned column
        (let ((rows (evo.tui::completion-rows tui matches kind)))
          (flet ((desc-col (row marker)
                   (let ((pos (search marker row)))
                     (and pos (evo.tui::visible-length (subseq row 0 pos))))))
            (check "popup help text rendered dim"
                   (search (format nil "~c[2m" #\Escape)
                           (find-if (lambda (r) (search "toggle the todo" r)) rows)))
            (check "popup help text aligned"
                   (equal (desc-col (find-if (lambda (r) (search "low·medium" r)) rows)
                                    "low·medium")
                          (desc-col (find-if (lambda (r) (search "toggle the todo" r)) rows)
                                    "toggle the todo"))))))
      (let ((lines (evo.tui::compose-region tui)))
        (check "popup rendered alongside input"
               (and (find-if (lambda (l) (search "❯" l)) lines)
                    ;; /t candidates sort alphabetically: theme < thinking < todo < tree
                    (find-if (lambda (l) (search "● /theme" l)) lines)
                    (find-if (lambda (l) (search "  /todo" l)) lines))))
      (evo.tui::edit-down tui)            ; selection moves in the popup...
      (evo.tui::complete-at-point tui)     ; ...and tab accepts it
      (check "tab accepts highlighted"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui)) "/thinking "))
      ;; enter on a partial command word completes instead of submitting
      (evo.tui::eb-clear (evo.tui::tui-editor tui))
      (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/mod")
      (evo.tui::submit tui)
      (check "enter completes /mod to /model"
             (equal (evo.tui::eb-text (evo.tui::tui-editor tui)) "/model "))
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
      (multiple-value-bind (prefix matches kind) (evo.tui::completion-context tui)
        (check "bare slash lists all commands"
               (and (equal prefix "") (> (length matches) 7)))
        (let ((rows (evo.tui::completion-rows tui matches kind)))
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
               (format nil "~a/evo-resume-~a/" (tmp-dir) (gen-id))))
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

;;; Soft-wrap of the still-streaming preview line: a long paragraph must
;;; render across several one-terminal-row lines (not a single truncated
;;; row), each within the width, and content must be preserved in order.

(defun test-wrap-visible ()
  ;; Plain ascii wraps by cell; every row within width; text preserved.
  (let ((rows (evo.tui::wrap-visible "abcdefghij" 4)))
    (check "wrap splits into rows" (equal rows '("abcd" "efgh" "ij")))
    (check "wrap rows within width"
           (every (lambda (r) (<= (evo.tui::visible-length r) 4)) rows)))
  ;; Wide chars never overflow: a 3-col width fits one 中 per row.
  (let ((rows (evo.tui::wrap-visible (make-string 3 :initial-element #\中) 3)))
    (check "wide wrap one per row" (= 3 (length rows)))
    (check "wide wrap within width"
           (every (lambda (r) (<= (evo.tui::visible-length r) 3)) rows)))
  ;; SGR escapes don't count toward width and are never split.
  (let* ((styled (concatenate 'string (evo.tui::bold "abcd") "efgh"))
         (rows (evo.tui::wrap-visible styled 4)))
    (check "styled wraps by visible width"
           (every (lambda (r) (<= (evo.tui::visible-length r) 4)) rows))
    (check "styled preserves sgr"
           (search (format nil "~c[1m" #\Escape) (first rows))))
  ;; Short input is a single row; empty input still yields one (empty) row.
  (check "short stays one row" (equal '("hi") (evo.tui::wrap-visible "hi" 40)))
  (check "empty yields one row" (equal '("") (evo.tui::wrap-visible "" 40)))
  ;; Char-based wrap is stable: a prefix's break points don't move when more
  ;; text is appended (so the streaming preview doesn't jump as it grows).
  (let ((a (evo.tui::wrap-visible "abcdef" 4))
        (b (evo.tui::wrap-visible "abcdefghij" 4)))
    (check "wrap prefix stable" (equal (first a) (first b)))))

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

;;; LaTeX math seam (core; the rasterizer lives in an extension).  This
;;; covers the grammar and placement the TUI does with whatever a renderer
;;; returns — a stub renderer stands in for the LaTeX->image extension.

(defun test-math ()
  ;; grammar: MD-SPLIT-MATH carves out $…$, $$…$$, \(…\), \[…\]
  (check "inline $..$ splits"
         (equal (evo.tui::md-split-math "a $x^2$ b")
                '((:text "a ") (:math "x^2" nil) (:text " b"))))
  (check "display $$..$$ splits"
         (equal (evo.tui::md-split-math "p $$E=mc^2$$ q")
                '((:text "p ") (:math "E=mc^2" t) (:text " q"))))
  (check "\\(..\\) inline / \\[..\\] display"
         (and (equal (evo.tui::md-split-math "\\(a+b\\)") '((:math "a+b" nil)))
              (equal (evo.tui::md-split-math "\\[a+b\\]") '((:math "a+b" t)))))
  (check "dollar inside a code span stays prose"
         (equal (mapcar #'first (evo.tui::md-split-math "cost `$5` and $y$"))
                '(:text :math)))
  (check "escaped \\$ stays prose"
         (equal (evo.tui::md-split-math "price \\$5 only")
                '((:text "price \\$5 only"))))
  (check "currency $5 and $10 stays prose"
         (equal (evo.tui::md-split-math "it is $5 and $10 today")
                '((:text "it is $5 and $10 today"))))
  ;; defaults: aligned/bottom, so inline prose stays one selectable line
  (check "default inline mode is aligned, bottom"
         (and (eq :aligned (evo.tui::math-inline-mode))
              (eq :bottom (evo.tui::math-inline-valign))))
  ;; placement: a stub renderer stands in for the extension
  (let ((evo.tui:*math-enabled* t)
        (evo.tui:*math-renderer*
          (lambda (latex disp) (format nil "<IMG:~a:~a>" (if disp "D" "I") latex))))
    (evo.util:set-setting :math-inline-mode :break)   ; test :break explicitly
    ;; a placed image ends its physical line (break before trailing text), so
    ;; images never stair-step "lower and lower" down a wrapped line
    (check "placed image breaks the line before trailing text"
           (equal (evo.tui::md-render-line "see $x^2$ end" (evo.tui::make-md))
                  (format nil "see <IMG:I:x^2>~% end")))
    (check "two inline formulas each end their own line"
           (equal (evo.tui::md-render-line "a $x$ b $y$ c" (evo.tui::make-md))
                  (format nil "a <IMG:I:x>~% b <IMG:I:y>~% c")))
    ;; multi-line $$ block: interior lines suppressed (NIL), one image at close
    (let ((md (evo.tui::make-md)))
      (check "bare $$ opens a suppressed block"
             (null (evo.tui::md-render-line "$$" md)))
      (check "block interior suppressed"
             (and (null (evo.tui::md-render-line "a+b" md))
                  (null (evo.tui::md-render-line "=c" md))))
      (check "closing $$ emits the whole formula as one image"
             (equal (evo.tui::md-render-line "$$" md)
                    (format nil "<IMG:D:a+b~%=c>"))))
    ;; a multi-row display block reserves its rows and lands on the foot
    (let ((md2 (evo.tui::make-md)) (nl (string #\Newline))
          (evo.tui:*math-renderer*
            (lambda (l d) (declare (ignore l d)) (values "<D>" 3 1 5))))
      (evo.tui::md-render-line "$$" md2)
      (evo.tui::md-render-line "z" md2)
      (check "multi-row display block reserves its rows"
             (equal (evo.tui::md-render-line "$$" md2)
                    (concatenate 'string nl nl (evo.tui::cursor-up 2)
                                 "<D>" (evo.tui::cursor-down 2)))))
    ;; the live preview must show source, never an image (region-safe)
    (check "preview renders math as source"
           (equal (evo.tui::md-render-preview "see $x^2$ end" (evo.tui::make-md))
                  "see $x^2$ end")))
  ;; a renderer that declines (NIL) falls back to the LaTeX source
  (let ((evo.tui:*math-enabled* t)
        (evo.tui:*math-renderer* (lambda (l d) (declare (ignore l d)) nil)))
    (check "declined render falls back to source"
           (equal (evo.tui::md-render-line "x $a+b$ y" (evo.tui::make-md))
                  "x $a+b$ y")))
  ;; :aligned mode — prose stays one selectable line; each image is drawn ABOVE
  ;; the baseline between a cursor save/restore, so placement never depends on
  ;; how the terminal moves the cursor for an image.  These stubs report only a
  ;; height, so the baseline comes from :MATH-INLINE-VALIGN and there is no COLS
  ;; step.
  (let ((evo.tui:*math-enabled* t)
        (evo.tui:*math-renderer*
          (lambda (latex disp) (declare (ignore disp))
            (values (format nil "<IMG:~a>" latex) 3)))
        (saved (evo.util:setting :math-inline-mode :break)))
    (unwind-protect
         (let ((nl (string #\Newline))
               (u1 (evo.tui::cursor-up 1)) (u2 (evo.tui::cursor-up 2))
               (d1 (evo.tui::cursor-down 1))
               (sv (evo.tui::save-cursor)) (rs (evo.tui::restore-cursor)))
           (evo.util:set-setting :math-inline-mode :aligned)
           (evo.util:set-setting :math-inline-valign :center)
           ;; h=3, center -> baseline row 1 (1 above, 1 below): reserve 2 rows,
           ;; rise to the baseline, draw the image one row up (save/restore),
           ;; prose stays on the baseline, drop to the foot.
           (check "aligned centers the image on the text baseline"
                  (equal (evo.tui::md-render-line "x $a$ y" (evo.tui::make-md))
                         (concatenate 'string nl nl u1 "x " sv u1 "<IMG:a>" rs " y" d1)))
           (evo.util:set-setting :math-inline-valign :top)
           ;; top -> baseline is the image's first row (0 above, 2 below).
           (check "aligned :top hangs the image from the baseline row"
                  (equal (evo.tui::md-render-line "$a$" (evo.tui::make-md))
                         (concatenate 'string nl nl u2 sv "<IMG:a>" rs
                                      (evo.tui::cursor-down 2)))))
      (evo.util:set-setting :math-inline-mode saved)
      (evo.util:set-setting :math-inline-valign :center)))
  ;; MIXED heights with baselines + widths reported (the real renderer's path):
  ;; a 2-row and a 3-row formula line up on ONE baseline, and the cursor is
  ;; stepped past each by its own COLS.
  (let ((evo.tui:*math-enabled* t)
        (evo.tui:*math-renderer*
          (lambda (latex disp) (declare (ignore disp))
            ;; (escape total ascent cols)
            (if (string= latex "a")
                (values "<a>" 2 1 2)
                (values "<b>" 3 2 4))))
        (saved (evo.util:setting :math-inline-mode :aligned)))
    (unwind-protect
         (let ((nl (string #\Newline))
               (u1 (evo.tui::cursor-up 1)) (u2 (evo.tui::cursor-up 2))
               (r2 (evo.tui::cursor-right 2)) (r4 (evo.tui::cursor-right 4))
               (sv (evo.tui::save-cursor)) (rs (evo.tui::restore-cursor)))
           (evo.util:set-setting :math-inline-mode :aligned)
           ;; above = max ascent = 2, below = 0 -> h=3, baseline is the foot row.
           (check "aligned lines mixed-height formulas on one baseline"
                  (equal (evo.tui::md-render-line "x $a$ $b$ y" (evo.tui::make-md))
                         (concatenate 'string nl nl
                                      "x " sv u1 "<a>" rs r2
                                      " " sv u2 "<b>" rs r4 " y"))))
      (evo.util:set-setting :math-inline-mode saved)
      (evo.util:set-setting :math-inline-valign :center)))
  ;; :SELF advance — the escape itself steps the cursor past the image (the
  ;; terminal's own exact column count), landing on the image's bottom row:
  ;; no save/restore, no COLS step, only vertical correction.
  (let ((evo.tui:*math-enabled* t)
        (evo.tui:*math-renderer*
          (lambda (latex disp) (declare (ignore disp))
            (values (format nil "<~a>" latex) 3 1 4 :self)))
        (saved (evo.util:setting :math-inline-mode :aligned)))
    (unwind-protect
         (let ((nl (string #\Newline))
               (u1 (evo.tui::cursor-up 1)) (d1 (evo.tui::cursor-down 1)))
           (evo.util:set-setting :math-inline-mode :aligned)
           ;; total 3, ascent 1 -> descent 1; block: 1 above, baseline, 1 below.
           (check ":self advance corrects vertically only"
                  (equal (evo.tui::md-render-line "x $a$ y" (evo.tui::make-md))
                         (concatenate 'string nl nl u1 "x " u1 "<a>" u1 " y" d1))))
      (evo.util:set-setting :math-inline-mode saved)))
  ;; width-aware wrap: an image that would overflow the terminal width starts
  ;; a new sub-line (with its own baseline block) instead of letting the
  ;; terminal hard-wrap mid-image and wreck the reserved-row geometry.
  (let ((evo.tui:*math-enabled* t)
        (evo.tui::*cols* 20)
        (evo.tui:*math-renderer*
          (lambda (latex disp) (declare (ignore disp))
            (values (format nil "<~a>" latex) 1 0 10)))
        (saved (evo.util:setting :math-inline-mode :aligned)))
    (unwind-protect
         (let ((nl (string #\Newline))
               (sv (evo.tui::save-cursor)) (rs (evo.tui::restore-cursor))
               (r10 (evo.tui::cursor-right 10)))
           (evo.util:set-setting :math-inline-mode :aligned)
           ;; image budget = 10 + 1 + ceil(10/8) = 13; 11 prose cols + 13 > 20.
           (check "overflowing formula wraps to its own sub-line"
                  (equal (evo.tui::md-render-line "0123456789 $a$" (evo.tui::make-md))
                         (concatenate 'string "0123456789 " nl sv "<a>" rs r10)))
           ;; prose itself splits at a cell boundary when it overflows.
           (check "prose splits at the width boundary"
                  (equal (evo.tui::md-render-line
                          "abcdefghijklmnopqrs $a$ tail" (evo.tui::make-md))
                         (concatenate 'string "abcdefghijklmnopqrs " nl
                                      sv "<a>" rs r10 " tail"))))
      (evo.util:set-setting :math-inline-mode saved)))
  ;; default policy: the bundled renderer is installed only when a caller opts in.
  (let ((saved-default (uiop:getenv "EVO_MATH_DEFAULT"))
        (saved-webview (uiop:getenv "EVO_WEBVIEW"))
        (saved-setting (evo.util:setting :math :unset))
        (saved-renderer evo.tui:*math-renderer*)
        (saved-enabled evo.tui:*math-enabled*)
        (saved-notes evo.kernel::*prompt-notes*))
    (unwind-protect
         (progn
           (evo.port:setenv "EVO_MATH_DEFAULT" "")
           (evo.port:setenv "EVO_WEBVIEW" "")
           (remf evo.util:*settings* :math)
           (setf evo.tui:*math-renderer* nil
                 evo.tui:*math-enabled* nil)
           (load (merge-pathnames "extensions/300-latex-math.lisp" (uiop:getcwd))
                 :verbose nil :print nil)
           (let ((math-on-p (uiop:find-symbol* :math-on-p :evo.user))
                 (sync (uiop:find-symbol* :math-sync-prompt-note :evo.user)))
             (check "latex-math extension is off by default"
                    (not (funcall math-on-p)))
             (check "latex-math does not install renderer by default"
                    (null evo.tui:*math-enabled*))
             (evo.port:setenv "EVO_WEBVIEW" "1")
             (remf evo.util:*settings* :math)
             (check "latex-math defaults on in VS Code Math Mode"
                    (funcall math-on-p))
             (evo.util:set-setting :math nil)
             (check "explicit :math nil overrides VS Code Math Mode default"
                    (not (funcall math-on-p)))
             (evo.port:setenv "EVO_WEBVIEW" "")
             (evo.port:setenv "EVO_MATH_DEFAULT" "1")
             (remf evo.util:*settings* :math)
             (check "EVO_MATH_DEFAULT opts latex-math in"
                    (funcall math-on-p))
             (evo.util:set-setting :math t)
             (funcall sync)
             (check "latex-math prompt note follows explicit opt-in"
                    (or (not (evo.user::latex-toolchain-ready-p))
                        (cdr (assoc "latex-math" evo.kernel::*prompt-notes*
                                    :test #'equal))))))
      (setf evo.tui:*math-renderer* saved-renderer
            evo.tui:*math-enabled* saved-enabled
            evo.kernel::*prompt-notes* saved-notes)
      (if (eq saved-setting :unset)
          (remf evo.util:*settings* :math)
          (evo.util:set-setting :math saved-setting))
      (evo.port:setenv "EVO_MATH_DEFAULT" (or saved-default ""))
      (evo.port:setenv "EVO_WEBVIEW" (or saved-webview ""))))

  ;; disabled: byte-for-byte the old behaviour
  (let ((evo.tui:*math-enabled* nil))
    (check "math off leaves $x$ untouched"
           (equal (evo.tui::md-render-line "a $x$ b" (evo.tui::make-md))
                  "a $x$ b"))))

;;; Prose-styler seam (core; the bionic reader lives in an extension).  The
;;; inline renderer routes plain prose runs through *PROSE-STYLER*; code spans,
;;; link URLs, **strong** and headings must NOT reach it, and with no styler
;;; installed the renderer is byte-for-byte what it was.

(defun test-prose-styler ()
  ;; Off by default: identity, byte-for-byte.
  (check "no styler: plain prose identity"
         (equal "hello there" (evo.tui::md-inline "hello there")))
  (check "no styler: unmatched marker still literal"
         (equal "2 * 3 * 4" (evo.tui::md-inline "2 * 3 * 4")))
  ;; A stub styler brackets every prose run it is given.
  (let ((evo.tui:*prose-styler* (lambda (s) (concatenate 'string "«" s "»"))))
    (check "prose run reaches the styler"
           (search "«hi »" (evo.tui::md-inline "hi **b**")))
    (check "strong text does NOT reach the styler"
           (not (search "«b»" (evo.tui::md-inline "hi **b**"))))
    (check "code span content does NOT reach the styler"
           (not (find #\« (evo.tui::md-inline "`x`"))))
    (check "link URL does NOT reach the styler"
           (not (search "«u»" (evo.tui::md-inline "[t](u)"))))
    (check "emphasised prose still reaches the styler"
           (search "«c»" (evo.tui::md-inline "*c*")))
    (check "list item text reaches the styler"
           (search "«" (evo.tui::md-render-line "- item" (evo.tui::make-md))))
    ;; A heading is already fully bold: the styler is suppressed there.
    (check "heading suppresses the styler"
           (not (find #\« (evo.tui::md-render-line "## Title" (evo.tui::make-md)))))
    ;; A signalling styler falls back to the source run, never crashes.
    (let ((evo.tui:*prose-styler* (lambda (s) (declare (ignore s)) (error "boom"))))
      (check "signalling styler falls back to source"
             (equal "hello" (evo.tui::md-inline "hello"))))))

;;; Bionic reader extension (extensions/350-bionic-reader.lisp): no toolchain,
;;; so the real extension loads and runs here.  Covers the ASCII-only rule
;;; (English words bolded; anything non-ASCII left untouched) and the command.

(defun test-bionic ()
  (let ((bon (format nil "~c[1m" #\Escape))
        (boff (format nil "~c[22m" #\Escape))
        (saved-styler evo.tui:*prose-styler*)
        (saved-on (evo.util:setting :bionic t))
        (saved-fix (evo.util:setting :bionic-fixation 0.5)))
    (unwind-protect
         (progn
           (load (merge-pathnames "extensions/350-bionic-reader.lisp"
                                  (uiop:getcwd))
                 :verbose nil :print nil)
           (evo.util:set-setting :bionic-fixation 0.5)
           (let ((bionic (uiop:find-symbol* :bionic-transform :evo.user))
                 (cmd (uiop:find-symbol* :bionic-command :evo.user)))
             ;; ASCII words: the leading ceil(n·0.5) letters bold, ≥1, ≤n.
             (check "bionic bolds an ascii word's prefix"
                    (equal (funcall bionic "hello")
                           (concatenate 'string bon "hel" boff "lo")))
             (check "bionic bolds a whole one-letter word"
                    (equal (funcall bionic "a")
                           (concatenate 'string bon "a" boff)))
             (check "bionic keeps punctuation and spacing"
                    (equal (funcall bionic "hi, ok")
                           (concatenate 'string bon "h" boff "i, " bon "o" boff "k")))
             ;; Non-English: untouched, byte-for-byte.
             (let ((accented (format nil "caf~c" (code-char 233))))     ; café
               (check "bionic leaves an accented word untouched"
                      (equal (funcall bionic accented) accented)))
             (let ((cjk (coerce (list (code-char #x65E5) (code-char #x672C)
                                      (code-char #x8A9E))
                                'string)))                              ; 日本語
               (check "bionic leaves a CJK word untouched"
                      (equal (funcall bionic cjk) cjk)))
             (let ((glued (coerce (list (code-char #x65E5) #\w #\o #\r #\d)
                                  'string)))                           ; 日word
               (check "bionic leaves ascii glued to non-ascii untouched"
                      (equal (funcall bionic glued) glued)))
             (check "bionic still bolds an english word beside foreign text"
                    (search (concatenate 'string bon "wo" boff "rd")
                            (funcall bionic
                                     (coerce (list (code-char #x65E5) #\Space
                                                   #\w #\o #\r #\d)
                                             'string))))               ; 日 word
             ;; The command drives the seam and the settings.
             (check "/bionic off removes the styler"
                    (progn (funcall cmd '(:args "off"))
                           (null evo.tui:*prose-styler*)))
             (check "/bionic on installs the styler"
                    (progn (funcall cmd '(:args "on"))
                           (and evo.tui:*prose-styler* t)))
             (check "/bionic status reports state"
                    (search "bionic on" (funcall cmd '(:args "status"))))
             (check "/bionic fixation sets the fraction"
                    (progn (funcall cmd '(:args "fixation 0.75"))
                           (< (abs (- 0.75 (evo.util:setting :bionic-fixation 0)))
                              1d-4)))
             ;; End to end through the renderer: prose gets bolded once on.
             (check "seam applies bionic to rendered prose"
                    (search bon (evo.tui::md-render-line "plain words"
                                                         (evo.tui::make-md))))))
      (evo.tui:register-prose-styler saved-styler)
      (evo.util:set-setting :bionic saved-on)
      (evo.util:set-setting :bionic-fixation saved-fix))))

;;; Light/dark theme: semantic colours resolve through the :theme setting.

(defun test-theme ()
  (let ((saved (evo.util:setting :theme :dark)))
    (unwind-protect
         (progn
           ;; DARK (default) is byte-for-byte the historical ANSI palette.
           (evo.util:set-setting :theme :dark)
           (check "dark theme is the default" (eq :dark (evo.tui::current-theme)))
           (check "dark dim = ANSI faint"
                  (equal (evo.tui::dim "x")
                         (format nil "~c[2mx~c[0m" #\Escape #\Escape)))
           (check "dark cyan = ANSI 36"
                  (equal (evo.tui::cyan "x")
                         (format nil "~c[36mx~c[0m" #\Escape #\Escape)))
           (check "dark code role = ANSI 33"
                  (equal (evo.tui::sgr-role :code) (format nil "~c[33m" #\Escape)))
           ;; LIGHT recolours to 24-bit where the terminal has truecolor.
           (let ((evo.tui::*truecolor* t))
             (evo.util:set-setting :theme :light)
             (check "light theme selected" (eq :light (evo.tui::current-theme)))
             (check "light accent is 24-bit and legible on white"
                    (search "38;2;0;100;112" (evo.tui::cyan "x")))
             (check "light muted is a solid ~7:1 grey, not soft mid-grey"
                  (search "38;2;88;88;88" (evo.tui::dim "x")))
           (check "light muted is no longer plain faint"
                    (not (search (format nil "~c[2m" #\Escape) (evo.tui::dim "x"))))
             (check "light code role differs from dark"
                    (not (equal (evo.tui::sgr-role :code)
                                (format nil "~c[33m" #\Escape)))))
           ;; Without truecolor, light falls back to the ANSI palette.
           (let ((evo.tui::*truecolor* nil))
             (evo.util:set-setting :theme :light)
             (check "no-truecolor light falls back to ANSI"
                    (equal (evo.tui::cyan "x")
                           (format nil "~c[36mx~c[0m" #\Escape #\Escape)))))
      (evo.util:set-setting :theme saved))))

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
    ;; Everything submitted is recalled, /commands included — whole,
    ;; partial, with arguments or without.
    (dolist (text '("//not a command" "/todo" "/goal ship it" "/eval (+ 1 2)"
                    "/eval (evo:all-too" "/tod" "/"))
      (evo.tui::history-remember tui text)
      (check (format nil "remembered ~a" text)
             (equal text (first (evo.tui::tui-history tui)))))
    (evo.tui::history-remember tui "/")
    (check "consecutive duplicate still collapses"
           (equal '("/" "/tod") (subseq (evo.tui::tui-history tui) 0 2)))
    ;; Browsing walks all of it, and no popup ever takes over — not on a
    ;; bare slash, not mid-symbol, not on a partial command word.
    (evo.tui::eb-clear eb)
    (dolist (expected '("/" "/tod" "/eval (evo:all-too" "/eval (+ 1 2)"
                        "/goal ship it" "/todo" "//not a command"
                        "second message" "first message"))
      (evo.tui::edit-up tui)
      (check (format nil "up recalls ~a" expected)
             (equal (evo.tui::eb-text eb) expected))
      (check (format nil "no popup on recalled ~a" expected)
             (null (evo.tui::completion-context tui))))
    ;; Editing a recalled entry makes it new input again: suggestions return.
    (evo.tui::history-recall tui "/tod")
    (check "recalled partial command word stays quiet"
           (null (evo.tui::completion-context tui)))
    (evo.tui::eb-backspace eb)
    (check "editing it brings suggestions back"
           (evo.tui::completion-context tui))
    (evo.tui::eb-insert-char eb #\d)
    (check "editing back to exactly the recalled text is quiet again"
           (null (evo.tui::completion-context tui)))
    ;; Tab asks for completion outright, whatever put the text there.
    (evo.tui::complete-at-point tui)
    (check "tab completes recalled content"
           (equal "/todo " (evo.tui::eb-text eb)))))

;;; /eval completion: the popup completes the image's own functions and
;;; variables inside an /eval invocation.

(defun test-eval-completion ()
  ;; What the cursor is on, and where the token starts.
  (let* ((tui (evo.tui::make-tui))
         (eb (evo.tui::tui-editor tui)))
    (evo.tui::eb-insert-text eb "/eval (evo:all-too")
    (multiple-value-bind (prefix kind) (evo.tui::completion-target eb)
      (check "eval content completes symbols" (eq :symbol kind))
      (check "token stops at the open paren" (equal "evo:all-too" prefix)))
    (multiple-value-bind (token start) (evo.tui::symbol-token-at-cursor eb)
      (check "token start is where it began" (= start 7))
      (check "token is what was typed" (equal "evo:all-too" token)))
    ;; Tab splices the completion in place, leaving the rest of the form.
    (evo.tui::eb-insert-text eb ")")
    (evo.tui::eb-move eb :left)
    (evo.tui::complete-at-point tui)
    (check "tab replaces just the token"
           (equal "/eval (evo:all-tools)" (evo.tui::eb-text eb)))
    (check "cursor lands after the completion"
           (= (evo.tui::eb-col eb) (1- (length (evo.tui::eb-text eb))))))
  ;; The command word itself is never completed over.
  (let* ((tui (evo.tui::make-tui))
         (eb (evo.tui::tui-editor tui)))
    (evo.tui::eb-insert-text eb "/eval (car x)")
    (evo.tui::eb-move eb :home)
    (dotimes (i 3) (evo.tui::eb-move eb :right))
    (check "no symbol completion inside the command word"
           (null (evo.tui::symbol-token-at-cursor eb)))
    (evo.tui::eb-move eb :end)
    (check "no popup after a closing paren"
           (null (evo.tui::completion-context tui))))
  ;; Rendering: symbols show as typed, commands keep their slash.
  (let* ((tui (evo.tui::make-tui))
         (eb (evo.tui::tui-editor tui)))
    (evo.tui::eb-insert-text eb "/eval (all-tool")
    (multiple-value-bind (prefix matches kind) (evo.tui::completion-context tui)
      (check "popup opens on an eval symbol" (equal "all-tool" prefix))
      (check "popup knows it is completing a symbol" (eq :symbol kind))
      (let ((rows (evo.tui::completion-rows tui matches kind)))
        (check "symbol rows carry no slash"
               (and (find-if (lambda (r) (search "all-tools" r)) rows)
                    (notany (lambda (r) (search "/all-tools" r)) rows)))))
    ;; Non-eval commands keep completing command names, not symbols.
    (evo.tui::eb-set-text eb "/lore car")
    (check "other commands do not complete symbols"
           (null (evo.tui::completion-target eb))))
  ;; A whole symbol name completes itself out of existence: nothing left to
  ;; choose, so no popup to capture up/down, and tab has nothing to say.
  (let* ((tui (evo.tui::make-tui))
         (eb (evo.tui::tui-editor tui)))
    (evo.tui::eb-insert-text eb "/eval (mapca")
    (check "popup while the name is partial" (evo.tui::completion-context tui))
    (evo.tui::eb-insert-char eb #\r)
    (check "popup gone once the name is whole"
           (null (evo.tui::completion-context tui)))
    (evo.tui::complete-at-point tui)
    (check "tab on a whole name leaves the buffer alone"
           (equal "/eval (mapcar" (evo.tui::eb-text eb)))
    ;; A name that is still a prefix of others keeps its popup.
    (evo.tui::eb-set-text eb "/eval (list")
    (check "a whole name that others extend keeps its popup"
           (evo.tui::completion-context tui))))

(defun completion-names (token)
  (mapcar #'car (evo.eval:completions-for token)))

(defun test-eval-completion-source ()
  ;; Unqualified: everything reachable while evaluating.
  (let ((names (completion-names "all-too")))
    (check "completes an evo function reachable unqualified"
           (member "all-tools" names :test #'string=)))
  (check "completes a CL function"
         (member "mapcar" (completion-names "mapca") :test #'string=))
  (check "completes a variable"
         (member "*package*" (completion-names "*packa") :test #'string=))
  (check "reports what a candidate is"
         (equal "function" (cdr (assoc "mapcar" (evo.eval:completions-for "mapca")
                                       :test #'string=))))
  (check "macros are named as macros"
         (search "macro" (cdr (assoc "defun" (evo.eval:completions-for "defu")
                                     :test #'string=))))
  ;; Only what can be called or read: an interned symbol that is neither is
  ;; not a completion candidate.
  (intern "EVAL-COMPLETION-BLANK" :evo.user)
  (check "a symbol that is neither function nor variable is not offered"
         (null (completion-names "eval-completion-blan")))
  (check "the same name completes once it names something"
         (progn (eval `(defparameter ,(intern "EVAL-COMPLETION-BLANK" :evo.user) 1))
                (member "eval-completion-blank" (completion-names "eval-completion-blan")
                        :test #'string=)))
  ;; Qualified: exports through one colon, the package's own through two.
  (check "single colon completes exported symbols"
         (member "evo:register-tool" (completion-names "evo:regis") :test #'string=))
  (check "single colon does not reach unexported symbols"
         (null (completion-names "evo.kernel:drain-steer")))
  (check "double colon reaches the package's own symbols"
         (member "evo.kernel::drain-steering" (completion-names "evo.kernel::drain-steer")
                 :test #'string=))
  (check "an unknown package completes nothing"
         (null (completion-names "no-such-package:x")))
  (check "keywords complete after a colon"
         (member ":test" (completion-names ":tes") :test #'string=))
  ;; Package names themselves, so the next keystroke lands qualified.
  (check "package names complete with their colon"
         (member "evo.kernel:" (completion-names "evo.kern") :test #'string=))
  ;; Nothing to filter on means no popup at all, rather than the whole image.
  (check "an empty token offers nothing" (null (completion-names "")))
  (check "a bare colon offers nothing" (null (completion-names ":")))
  ;; Case-insensitive in, canonical lowercase out.
  (check "matching ignores case"
         (member "mapcar" (completion-names "MAPCA") :test #'string=))
  ;; Sorted and unique — the popup shows a window of this list.
  (let ((names (completion-names "ma")))
    (check "candidates are sorted" (equal names (sort (copy-list names) #'string<)))
    (check "candidates are unique"
           (= (length names) (length (remove-duplicates names :test #'string=))))))

;;; Goal budgets (default: no limit)

(defun string-tree-search (form predicate)
  "Does any string inside FORM satisfy PREDICATE?"
  (typecase form
    (string (funcall predicate form))
    (cons (or (string-tree-search (car form) predicate)
              (string-tree-search (cdr form) predicate)))
    (vector (some (lambda (x) (string-tree-search x predicate)) (coerce form 'list)))
    (t nil)))

(defun format-continuation-p (string)
  "Does STRING use the ~<newline> FORMAT continuation — the one construct
whose validity depends on the file's line endings?  (An even run of tildes
before the newline is an escaped ~, not a directive.)

CR counts as well as LF: in a CR-LF file the continuation reads ~ CR LF, and
a lint that only looked for ~ LF would be blind on exactly the files that
have the problem."
  (loop for i from 0 below (1- (length string))
        thereis (and (char= (char string i) #\~)
                     (member (char string (1+ i)) '(#\Newline #\Return))
                     (oddp (loop for k downfrom i
                                 while (and (>= k 0) (char= (char string k) #\~))
                                 count t)))))

(defun format-continuation-offenders ()
  "(values OFFENDERS SCANNED UNREADABLE) over the Lisp sources in CWD.
Uses the reader rather than a regexp, so comments, character literals and
escapes are somebody else's problem.  Symbols land in a throwaway package.
A file the reader chokes on is reported as UNREADABLE, not quietly passed:
a lint that can go blind without saying so is worse than none."
  (let ((files (remove-if
                ;; The Windows console instruments (tests/windows-*.lisp) are
                ;; standalone SBCL-only diagnostics that call sb-alien/sb-sys
                ;; directly — they cannot even be READ on ECL (no such
                ;; packages), and being run by `sbcl --script` / `make.ps1
                ;; console-test` they are not portable suite source.  The
                ;; portability lint is for the tree that must read everywhere.
                (lambda (f)
                  (let ((name (file-namestring f)))
                    (and (>= (length name) 8)
                         (string= "windows-" (subseq name 0 8)))))
                (loop for pattern in '("src/*.lisp" "src/*/*.lisp"
                                       "extensions/*.lisp" "tests/*.lisp")
                      append (directory (merge-pathnames pattern (uiop:getcwd))))))
        (offenders nil) (unreadable nil) (scanned 0)
        (scratch (or (find-package :evo.tests.scan)
                     (make-package :evo.tests.scan :use '(:cl :evo)))))
    (dolist (file files (values (nreverse offenders) scanned (nreverse unreadable)))
      (handler-case
          (with-open-file (in file :external-format :utf-8)
            (let ((*package* scratch) (*read-eval* nil))
              (loop for form = (read in nil :eof)
                    until (eq form :eof)
                    ;; Honour IN-PACKAGE as the compiler does, or a file that
                    ;; leans on a package-local nickname (jzon:) won't read.
                    when (and (consp form) (eq (car form) 'in-package)
                              (find-package (second form)))
                      do (setf *package* (find-package (second form)))
                    when (string-tree-search form #'format-continuation-p)
                      do (pushnew (file-namestring file) offenders :test #'equal)))
            (incf scanned))
        (error (e) (push (cons (file-namestring file) (princ-to-string e))
                         unreadable))))))

(defun crlf-fixture (text)
  "Write TEXT to a scratch file with CR-LF line endings; return the path."
  (let ((path (format nil "~a/evo-crlf-~a.txt" (tmp-dir) (gen-id))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string (crlf-newlines text) out))
    path))

(defun test-line-endings ()
  ;; CAT: the replacement for the ~<newline> FORMAT continuation, which is
  ;; the one construct whose meaning depends on a file's line endings.
  (check "cat folds constants" (equal (cat "ab" "cd" "ef") "abcdef"))
  (check "cat expands to a literal, so FORMAT still sees a constant"
         (stringp (macroexpand-1 '(cat "a" "b"))))
  (check "the continuation lint can see one"
         (format-continuation-p (format nil "a~~~%b")))
  (check "the continuation lint ignores an escaped tilde"
         (not (format-continuation-p (format nil "a~~~~~%b"))))
  (check "the continuation lint sees the CR-LF spelling too"
         (format-continuation-p (format nil "a~~~c~%b" #\Return)))
  (multiple-value-bind (offenders scanned unreadable)
      (format-continuation-offenders)
    (check "the suite actually scanned the tree" (> scanned 30))
    (check "every source file was readable by the lint" (null unreadable))
    (check "no source file ends a string with the ~<newline> continuation"
           (null offenders)))
  ;; Normalization: foreign text arrives with whatever line endings it likes.
  (check "normalize CR-LF" (equal (normalize-newlines (format nil "a~c~%b" #\Return))
                                  (format nil "a~%b")))
  (check "normalize lone CR" (equal (normalize-newlines (format nil "a~cb" #\Return))
                                    (format nil "a~%b")))
  (check "normalize leaves LF text untouched"
         (let ((s (format nil "a~%b"))) (eq (normalize-newlines s) s)))
  (check "crlf-newlines roundtrips"
         (equal (normalize-newlines (crlf-newlines (format nil "a~%b~%")))
                (format nil "a~%b~%")))
  ;; The read tool shows LF whatever the file uses.
  (let ((path (crlf-fixture (format nil "alpha~%beta~%gamma~%"))))
    (let ((shown (evo.kernel::tool-read (list :path path))))
      (check "read tool hands the agent no CR" (null (find #\Return shown)))
      (check "read tool counts lines, not CR-LF halves"
             (search (format nil "     3~cgamma" #\Tab) shown)))
    ;; ... and an edit written with LF still matches, without converting the
    ;; file: the rest of its CR-LF endings survive.
    (evo.kernel::tool-edit (list :path path :old-string (format nil "alpha~%beta")
                     :new-string (format nil "ALPHA~%BETA")))
    (let ((raw (read-file-string path)))
      (check "edit with LF matched a CR-LF file"
             (search "ALPHA" raw))
      (check "edit kept the file's CR-LF endings"
             (= 3 (count #\Return raw)))
      (check "edit did not leave a bare LF behind"
             (= (count #\Return raw) (count #\Newline raw))))
    ;; A whole-file rewrite keeps the file's convention too — otherwise
    ;; every line of a Windows file shows up as changed.
    (evo.kernel::tool-write (list :path path
                                  :content (format nil "one~%two~%three~%")))
    (let ((raw (read-file-string path)))
      (check "write kept the file's CR-LF endings" (= 3 (count #\Return raw)))
      (check "write did not double any endings"
             (= (count #\Return raw) (count #\Newline raw))))
    (ignore-errors (delete-file path)))
  (let ((fresh (format nil "~a/evo-lf-~a.txt" (tmp-dir) (gen-id))))
    (evo.kernel::tool-write (list :path fresh :content (format nil "a~%b~%")))
    (check "a new file is written LF" (null (find #\Return (read-file-string fresh))))
    (ignore-errors (delete-file fresh)))
  ;; The prompt we send is the same text whatever the checkout looks like.
  (let ((prompt (build-system-prompt (list (find-tool "read")))))
    (check "system prompt carries no CR" (null (find #\Return prompt)))))

;;; Prompt notes: extension-contributed system-prompt additions, named so
;;; re-registration replaces (idempotent reloads) and NIL withdraws.

(defun test-prompt-notes ()
  (let ((saved evo.kernel::*prompt-notes*)
        (saved-languages evo.kernel::*prompt-languages*))
    (unwind-protect
         (progn
           (setf evo.kernel::*prompt-notes* nil)
           (evo:register-prompt-note "t-math" "Write formulas as LaTeX.")
           (check "registered note rides in the prompt"
                  (search "Write formulas as LaTeX."
                          (build-system-prompt nil)))
           (evo:register-prompt-note "t-math" "Prefer $...$ inline.")
           (let ((prompt (build-system-prompt nil)))
             (check "re-registration replaces, not accumulates"
                    (and (search "Prefer $...$ inline." prompt)
                         (not (search "Write formulas as LaTeX." prompt)))))
           (evo:register-prompt-note "t-math" nil)
           (check "nil text withdraws the note"
                  (not (search "Prefer $...$ inline."
                               (build-system-prompt nil))))
           ;; A note may be a function of the active language pack, so an
           ;; extension's guidance follows /lang instead of sitting in
           ;; English in the middle of a translated prompt.
           (evo:register-prompt-note
            "t-lang" (lambda (pack)
                       (when (equal (pget pack :code) "en")
                         (format nil "NOTE-FOR-~a" (pget pack :name)))))
           (check "a function note is called with the active language pack"
                  (search "NOTE-FOR-English" (build-system-prompt nil)))
           (evo.kernel:register-prompt-language "xx-note" :name "Notish"
                                               :sections (list :base "B"))
           (check "and returning nil drops it for that prompt"
                  (not (search "NOTE-FOR"
                               (build-system-prompt nil :language "xx-note"))))
           (evo:register-prompt-note "t-boom" (lambda (pack)
                                                (declare (ignore pack))
                                                (error "note is broken")))
           (check "a signalling note is skipped, not fatal to the prompt"
                  (handler-bind ((warning #'muffle-warning))
                    (search "NOTE-FOR-English" (build-system-prompt nil)))))
      (setf evo.kernel::*prompt-notes* saved
            evo.kernel::*prompt-languages* saved-languages))))

#| Prompt language packs: the prompt's own words are a registry, English is
just the pack that ships as a core extension, and what the user picked
(journaled) beats the :language setting beats the default. |#

(defun test-prompt-languages ()
  (let ((saved evo.kernel::*prompt-languages*)
        (saved-setting (setting :language)))
    (unwind-protect
         (progn
           (check "the English pack is registered by the core extension"
                  (equal "English" (pget (evo.kernel:find-prompt-language "en") :name)))
           (evo.kernel:register-prompt-language
            "xx-TEST" :name "Testish" :native "Tëstish"
            :response-language "Testish"
            :sections (list :base "BASE-IN-TESTISH" :tools-heading "## Tuulz"))
           (check "codes are case-insensitive"
                  (eq (evo.kernel:find-prompt-language "XX-test")
                      (evo.kernel:find-prompt-language "xx-test")))
           (let ((prompt (build-system-prompt nil :language "xx-TEST")))
             (check "the pack supplies the sections it translated"
                    (and (string-prefix-p "BASE-IN-TESTISH" prompt)
                         (search "## Tuulz" prompt)))
             (check "an untranslated section falls back to the default language"
                    (search "## Doing tasks" prompt))
             (check "the pack names the language to answer in"
                    (search "Always respond in Testish" prompt)))
           (let ((before (length evo.kernel::*prompt-languages*)))
             (evo.kernel:register-prompt-language
              "xx-TEST" :name "Testish" :sections (list :base "SECOND-BASE"))
             (check "re-registration replaces the pack, not stacks it"
                    (and (= before (length evo.kernel::*prompt-languages*))
                         (string-prefix-p "SECOND-BASE"
                                          (build-system-prompt nil :language "xx-test")))))
           (check "an unknown section key is refused at registration"
                  (nth-value 1 (ignore-errors
                                (evo.kernel:register-prompt-language
                                 "xx-bad" :sections (list :nonesuch "x")))))
           (check "an unregistered code is a response-language hint, not a pack"
                  (let ((prompt (build-system-prompt nil :language "Korean")))
                    (and (search "Always respond in Korean" prompt)
                         (search "## Doing tasks" prompt))))
           (check "no request means no language directive at all"
                  (not (search "## Language" (build-system-prompt nil :language nil))))
           ;; Precedence: journalled pick > :language setting > default.
           (let* ((dir (uiop:ensure-directory-pathname
                        (format nil "~a/evo-lang-~a/" (tmp-dir) (gen-id))))
                  (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
                  (agent (make-agent :journal journal)))
             (setf (setting :language) "xx-test")
             (check "the :language setting is the session default"
                    (equal "xx-test" (evo.kernel:language-request
                                      (fold-state journal))))
             (evo.kernel:set-prompt-language "en" agent)
             (check "a journalled pick outranks the setting"
                    (equal "en" (evo.kernel:language-request (fold-state journal))))
             ;; Nothing reaches disk before the first assistant message.
             (append-entry journal (list :type :message
                                         :message (list :role :assistant
                                                        :content (list (list :type :text
                                                                             :text "ok")))))
             (check "and it survives a reopen of the journal"
                    (equal "en" (evo.kernel:language-request
                                 (fold-state (open-journal (journal-path journal))))))
             (evo.kernel:set-prompt-language "xx-test")
             (check "without an agent the choice is just the setting"
                    (equal "xx-test" (setting :language)))))
      (setf evo.kernel::*prompt-languages* saved
            (setting :language) saved-setting))))

(defun test-goal-budget ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-goal-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal)))
    (create-goal-entry agent "do the thing")
    (let ((goal (current-goal agent)))
      (check "default budget is nil" (null (pget goal :token-budget)))
      (check "continuation says no limit"
             (search "no limit" (goal-continuation-message goal 1234)))
      (check "continuation with budget shows remaining"
             (search "(800 remaining)"
                     (goal-continuation-message (pput goal :token-budget 2034) 1234))))
    ;; The continuation nudges toward a done-when verifier only while none is
    ;; attached — once set, it must stop nagging.
    (let ((goal (current-goal agent)))
      (check "continuation nudges when no done-when"
             (search "No done-when verifier" (goal-continuation-message goal 10)))
      (check "continuation stops nudging once done-when set"
             (not (search "No done-when verifier"
                          (goal-continuation-message (pput goal :done-when "p") 10)))))))

(defvar *test-goal-done* nil
  "Flip switch read by the userspace done-when predicate in test-goal-tools.")

(defun test-goal-tools ()
  "update_goal: refine objective/done-when, human-only pause, resume, and
the guards + completion gating around all of it."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-goaltools-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (evo:*agent* agent))
    ;; A userspace verifier whose result this test controls.
    (eval `(defun ,(intern "TEST-GOAL-DONE-P" :evo.user) ()
             evo.tests::*test-goal-done*))
    (create-goal-entry agent "ship the feature")
    (let ((id (pget (current-goal agent) :goal-id)))
      ;; Refine objective text — same goal, new objective.
      (evo.kernel::tool-update-goal '(:objective "ship the feature and document it"))
      (check "objective refined"
             (equal (pget (current-goal agent) :objective)
                    "ship the feature and document it"))
      (check "refine keeps the same goal-id"
             (equal id (pget (current-goal agent) :goal-id))))
    ;; Empty update is refused.
    (check-signals "update_goal with no fields errors"
                   (evo.kernel::tool-update-goal '()))
    ;; Attach a done-when verifier after creation (goal set via /goal has none).
    (evo.kernel::tool-update-goal '(:done-when "test-goal-done-p"))
    (check "done-when attached to live goal"
           (equal (pget (current-goal agent) :done-when) "test-goal-done-p"))
    ;; Agent may send status="active" along with done_when on an active goal
    ;; (e.g. "attach verifier + reaffirm active"); must not error as resume.
    (let ((before (length (evo.journal::journal-entries (agent-journal agent)))))
      (evo.kernel::tool-update-goal '(:status "active" :done-when "test-goal-done-p"))
      (check "status=active+done_when on active goal refines instead of erroring"
             (equal (pget (current-goal agent) :status) :active))
      (check "status=active+done_when still appends a journal entry"
             (> (length (evo.journal::journal-entries (agent-journal agent))) before)))
    ;; Pausing is human-only: the tool rejects it with an explicit message
    ;; and the goal is untouched.
    (let ((msg (handler-case
                   (progn (evo.kernel::tool-update-goal '(:status "paused")) nil)
                 (error (e) (princ-to-string e)))))
      (check "update_goal status=paused signals" (not (null msg)))
      (check "pause rejection points at /goal pause"
             (and msg (not (null (search "/goal pause" msg))))))
    (check "goal still active after a rejected pause"
           (eq (pget (current-goal agent) :status) :active))
    ;; "blocked" is gone from the tool's vocabulary entirely.
    (check-signals "update_goal status=blocked rejected"
                   (evo.kernel::tool-update-goal '(:status "blocked" :reason "stuck")))
    ;; A human pause (what /goal pause journals) stops the idle loop...
    (evo.kernel:update-goal-entry agent (current-goal agent) :status :paused)
    (check "goal paused by the human"
           (eq (pget (current-goal agent) :status) :paused))
    ;; The idle-continuation hook must NOT re-steer a paused goal.
    (check "settled hook re-steers nothing while paused"
           (null (evo.kernel::goal-settled-hook agent :stop)))
    (check "no steering queued while paused"
           (not (evo.kernel:steering-pending-p agent)))
    ;; ...and the agent can still resume it from there.
    (evo.kernel::tool-update-goal '(:status "active"))
    (check "goal resumed to active" (eq (pget (current-goal agent) :status) :active))
    (check-signals "cannot resume an already-active goal"
                   (evo.kernel::tool-update-goal '(:status "active")))
    ;; A resumed active goal IS re-steered by the hook.
    (check "settled hook re-steers an active goal"
           (evo.kernel::goal-settled-hook agent :stop))
    (evo.kernel::drain-steering agent)   ; clear the queued continuation
    ;; Completion is gated by the verifier.
    (setf *test-goal-done* nil)
    (check-signals "complete rejected while done-when fails"
                   (evo.kernel::tool-update-goal '(:status "complete")))
    (check "goal stays active after a rejected completion"
           (eq (pget (current-goal agent) :status) :active))
    (setf *test-goal-done* t)
    (evo.kernel::tool-update-goal '(:status "complete"))
    (check "goal complete once done-when passes"
           (eq (pget (current-goal agent) :status) :complete))
    ;; A finished goal can no longer be refined.
    (check-signals "cannot refine a completed goal"
                   (evo.kernel::tool-update-goal '(:objective "too late")))))

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
  ;; Real skills in the wild write descriptions as YAML block scalars and
  ;; quoted strings; a line-based reader used to hand back ">-" or keep quotes.
  (let ((front (evo.kernel::parse-frontmatter
                (format nil "---~%name: rl-goal~%description: >-~%  Set a goal in the~%  workspace. Reads goals/.~%metadata:~%  loop: work-harness~%  step: 1-goal~%---~%body"))))
    (check "folded block scalar folds into one line"
           (equal (cdr (assoc "description" front :test #'equal))
                  "Set a goal in the workspace. Reads goals/."))
    (check "block scalar does not eat the next key"
           (equal (cdr (assoc "name" front :test #'equal)) "rl-goal"))
    (check "nested mapping stays under its own key"
           (and (equal (cdr (assoc "metadata" front :test #'equal))
                       "loop: work-harness step: 1-goal")
                (null (assoc "loop" front :test #'equal)))))
  (let ((front (evo.kernel::parse-frontmatter
                (format nil "---~%description: |~%  line one~%  line two~%name: lit~%---~%body"))))
    (check "literal block scalar keeps its line breaks"
           (equal (cdr (assoc "description" front :test #'equal))
                  (format nil "line one~%line two")))
    (check "literal block scalar stops at the next key"
           (equal (cdr (assoc "name" front :test #'equal)) "lit")))
  (let ((front (evo.kernel::parse-frontmatter
                (format nil "---~%name: \"quoted: name\"~%description: 'it''s fine'~%other: plain~%  continued~%---~%body"))))
    (check "double-quoted value loses its quotes"
           (equal (cdr (assoc "name" front :test #'equal)) "quoted: name"))
    (check "single-quoted value unescapes ''"
           (equal (cdr (assoc "description" front :test #'equal)) "it's fine"))
    (check "plain value wrapped over lines folds"
           (equal (cdr (assoc "other" front :test #'equal)) "plain continued")))
  (let* ((old-home (uiop:getenv "HOME"))
         (home (uiop:ensure-directory-pathname
                (format nil "~a/evo-agents-home-~a/" (tmp-dir) (gen-id))))
         (cwd (uiop:ensure-directory-pathname
               (format nil "~a/evo-agents-project-~a/" (tmp-dir) (gen-id))))
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

(defclass compact-fixture-api (provider-api) ())
(defvar *compact-fixture-mode* :success)
(defvar *compact-fixture-started* nil)
(defvar *compact-fixture-unblocked* nil)

(defmethod endpoint-path ((api compact-fixture-api))
  (declare (ignore api))
  "/fixture/compact")

(defmethod auth-headers ((api compact-fixture-api) config)
  (declare (ignore api config))
  nil)

(defmethod build-request ((api compact-fixture-api)
                          &key model system messages tools thinking-level)
  (declare (ignore api model system messages tools thinking-level))
  "{}")

(defmethod perform-request ((api compact-fixture-api) url headers body
                            &key on-event abort-flag abort-cleanup &allow-other-keys)
  (declare (ignore api url headers body on-event))
  (setf *compact-fixture-started* t)
  (ecase *compact-fixture-mode*
    (:success
     (list :content '((:type :text :text "tiny summary"))
           :stopped-p t :stop-reason :stop
           :usage '(:input 10 :output 2 :cache-read 0 :cache-write 0)))
    (:wait
     (let ((unregister (and abort-cleanup
                            (funcall abort-cleanup
                                     (lambda ()
                                       (setf *compact-fixture-unblocked* t))))))
       (unwind-protect
            (loop until *compact-fixture-unblocked*
                  do (when (and abort-flag (funcall abort-flag))
                       (setf *compact-fixture-unblocked* t))
                     (sleep 0.02))
         (when unregister (funcall unregister)))
       (list :aborted-p t :content nil :stop-reason :aborted
             :usage '(:input 0 :output 0 :cache-read 0 :cache-write 0))))))

(defun setup-compact-fixture-model ()
  (register-api :compact-fixture (make-instance 'compact-fixture-api))
  (register-provider* :compact-fixture :base-url "https://fixture.invalid")
  (register-model* "compact-fixture-model" :provider :compact-fixture
                   :api :compact-fixture :context-window 10000 :max-output 100))

(defun make-compact-fixture-tui (&key (old-chars 4000))
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-compact-tui-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent)))
    (append-entry journal '(:type :model-change :model "compact-fixture-model"))
    (append-entry journal `(:type :message
                            :message (:role :user
                                      :content ((:type :text :text ,(make-string old-chars :initial-element #\x))))))
    (append-entry journal '(:type :message
                            :message (:role :user
                                      :content ((:type :text :text "tail")))))
    ;; An assistant message with provider-reported usage in the retained tail.
    ;; Before the fix this usage anchored the estimate, so compact did not
    ;; lower the displayed token count.
    (append-entry journal '(:type :message
                            :message (:role :assistant
                                      :content ((:type :text :text "ok"))
                                      :stop-reason :stop
                                      :usage (:input 8000 :output 100 :cache-read 0 :cache-write 0))))
    (setf (agent-events-cb agent) (lambda (event) (evo.tui::push-event tui event)))
    (values tui agent journal)))

(defun wait-for-compact-worker (tui &key (seconds 2))
  (loop with deadline = (+ (get-internal-real-time)
                           (* seconds internal-time-units-per-second))
        while (and (evo.tui::tui-running tui)
                   (< (get-internal-real-time) deadline))
        do (dolist (event (evo.tui::drain-events tui))
             (evo.tui::handle-agent-event tui event))
           (sleep 0.02))
  (dolist (event (evo.tui::drain-events tui))
    (evo.tui::handle-agent-event tui event))
  (not (evo.tui::tui-running tui)))

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
               (format nil "~a/evo-test-~a/" (tmp-dir) (gen-id))))
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
  ;; Manual /compact runs on a worker: the UI immediately shows a rotating
  ;; compacting activity line, then refreshes context accounting after the
  ;; compaction entry lands.
  (let ((previous-mode *compact-fixture-mode*)
        (previous-keep evo.kernel::*compact-keep-recent-tokens*))
    (unwind-protect
         (progn
           (setf *compact-fixture-mode* :success
                 *compact-fixture-started* nil
                 *compact-fixture-unblocked* nil
                 evo.kernel::*compact-keep-recent-tokens* 1)
           (setup-compact-fixture-model)
           (multiple-value-bind (tui agent journal) (make-compact-fixture-tui)
             (declare (ignore agent))
             (evo.tui::refresh-goal tui)
             (let ((before (evo.tui::tui-context-tokens tui)))
               (with-output-to-string (fake-tty)
                 (let ((evo.tui::*tty-out* fake-tty)
                       (evo.tui::*region-height* 0)
                       (evo.tui::*region-cursor-row* 0))
                   (evo.tui::start-compact-worker tui "")
                   (check "manual compact starts worker instead of blocking UI"
                          (evo.tui::tui-running tui))
                   (check "manual compact provider request starts"
                          (loop repeat 100 until *compact-fixture-started*
                                do (sleep 0.01)
                                finally (return *compact-fixture-started*)))
                   (check "manual compact shows spinner activity"
                          (search "compacting..." (evo.tui::activity-line tui)))
                   (check "manual compact worker finishes" (wait-for-compact-worker tui))
                   (let ((after (evo.tui::tui-context-tokens tui)))
                     (check "manual compact appends compaction entry"
                            (find :compaction (entry-path journal)
                                  :key (lambda (e) (pget e :type))))
                     (check "manual compact refreshes context tokens"
                            (< after before))))))))
      (setf *compact-fixture-mode* previous-mode
            evo.kernel::*compact-keep-recent-tokens* previous-keep)))
  ;; Interrupting manual /compact sets the same abort flag used by model calls;
  ;; the provider cleanup runs and the worker comes back without appending a
  ;; compaction checkpoint.
  (let ((previous-mode *compact-fixture-mode*)
        (previous-keep evo.kernel::*compact-keep-recent-tokens*))
    (unwind-protect
         (progn
           (setf *compact-fixture-mode* :wait
                 *compact-fixture-started* nil
                 *compact-fixture-unblocked* nil
                 evo.kernel::*compact-keep-recent-tokens* 1)
           (setup-compact-fixture-model)
           (multiple-value-bind (tui agent journal) (make-compact-fixture-tui)
             (declare (ignore agent))
             (with-output-to-string (fake-tty)
               (let ((evo.tui::*tty-out* fake-tty)
                     (evo.tui::*region-height* 0)
                     (evo.tui::*region-cursor-row* 0))
                 (evo.tui::start-compact-worker tui "")
                 (loop repeat 100 until *compact-fixture-started* do (sleep 0.01))
                 (evo.tui::interrupt-run tui)
                 (check "manual compact interrupt runs abort cleanup"
                        (loop repeat 100 until *compact-fixture-unblocked*
                              do (sleep 0.01)
                              finally (return *compact-fixture-unblocked*)))
                 (check "manual compact interrupt finishes worker"
                        (wait-for-compact-worker tui))
                 (check "manual compact interrupt appends no checkpoint"
                        (not (find :compaction (entry-path journal)
                                   :key (lambda (e) (pget e :type))))))))
      (setf *compact-fixture-mode* previous-mode
            evo.kernel::*compact-keep-recent-tokens* previous-keep)))
  ;; Overflow classification.
  (check "overflow detected"
         (overflow-error-p '(:role :assistant :stop-reason :error
                             :error-message "HTTP 400: prompt is too long: 250000 tokens")))
  (check "overflow not confused with 500"
         (not (overflow-error-p '(:role :assistant :stop-reason :error
                                  :error-message "HTTP 500: boom"))))))

;;; Lore

(defun test-lore ()
  (let* ((home (uiop:ensure-directory-pathname
                (format nil "~a/evo-lore-~a/" (tmp-dir) (gen-id)))))
    (evo.port:setenv "EVO_HOME" (namestring home))
    (unwind-protect
         (progn
           (let ((id1 (add-lore "always run tests" :scope :global))
                 (id2 (add-lore "prefer rg over grep" :scope :global)))
             ;; :cwd is the scratch home, not the repo — a real .evo/lore.sexp
             ;; in the working tree must not leak into project scope here.
             (let ((lore (all-lore :cwd home)))
               (check "lore round trip"
                      (equal lore '("always run tests" "prefer rg over grep"))))
             (let ((entries (all-lore-entries :cwd home)))
               (check "lore entries carry ids"
                      (and (equal id1 (getf (first entries) :id))
                           (equal id2 (getf (second entries) :id))))
               (check "lore entries carry scope"
                      (every (lambda (e) (eq :global (getf e :scope))) entries)))
             (let ((prompt (build-system-prompt nil :lore (all-lore-entries :cwd home))))
               (check "lore injected into prompt with id"
                      (and (search "prefer rg over grep" prompt)
                           (search id2 prompt))))
             ;; Edit and remove by id (no agent -> file scopes only).
             (edit-lore id1 "always run all tests" :agent nil :cwd home)
             (check "lore edited by id"
                    (equal (all-lore :cwd home)
                           '("always run all tests" "prefer rg over grep")))
             (remove-lore id2 :agent nil :cwd home)
             (check "lore removed by id"
                    (equal (all-lore :cwd home) '("always run all tests")))
             (check "find-lore-scope locates surviving entry"
                    (eq :global (find-lore-scope id1 :cwd home)))
             (check "find-lore-scope nil for removed entry"
                    (null (find-lore-scope id2 :cwd home)))
             (check "lore tool registered" (find-tool "lore")))
           ;; Session-scoped lore rides the journal, not the files.
           (let* ((sdir (uiop:ensure-directory-pathname
                         (format nil "~a/evo-lore-sess-~a/" (tmp-dir) (gen-id))))
                  (journal (progn (ensure-directories-exist sdir)
                                  (make-session-journal sdir)))
                  (agent (make-agent :journal journal))
                  (sid (add-session-lore agent "stay in the sandbox")))
             (flet ((state () (fold-state (agent-journal agent))))
               (check "session lore appears in entries"
                      (let ((e (find :session (all-lore-entries :state (state) :cwd home)
                                     :key (lambda (x) (getf x :scope)))))
                        (and e (equal (getf e :id) sid)
                             (equal (getf e :text) "stay in the sandbox"))))
               (check "find-lore-scope locates session lore"
                      (eq :session (find-lore-scope sid :state (state) :cwd home)))
               (edit-lore sid "stay strictly in the sandbox" :agent agent :cwd home)
               (check "session lore edited by id"
                      (equal "stay strictly in the sandbox"
                             (getf (find :session (all-lore-entries :state (state) :cwd home)
                                         :key (lambda (x) (getf x :scope)))
                                   :text)))
               (remove-lore sid :agent agent :cwd home)
               (check "session lore removed by id"
                      (null (find :session (all-lore-entries :state (state) :cwd home)
                                  :key (lambda (x) (getf x :scope))))))))
      (evo.port:setenv "EVO_HOME"
                       (namestring (uiop:ensure-directory-pathname
                                    (format nil "~a/evo-unit-home" (tmp-dir))))))))

;;; /lore vs /global-lore must respect scope, the way /memory and
;;; /global-memory do — they used to both dump every scope, indistinguishably.

(defun test-lore-slash-commands ()
  (let* ((home (uiop:ensure-directory-pathname
                (format nil "~a/evo-lore-cmd-home-~a/" (tmp-dir) (gen-id))))
         (dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-lore-cmd-proj-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent)))
    (evo.port:setenv "EVO_HOME" (namestring home))
    (unwind-protect
         (progn
           (add-lore "always run the whole suite" :scope :global :cwd dir)
           (add-lore "this repo uses rebase merges" :scope :project :cwd dir)
           (add-session-lore agent "stay focused on the current bug")
           (let ((scrollback
                  (with-output-to-string (fake-tty)
                    (let ((evo.tui::*tty-out* fake-tty))
                      (evo.tui::lore-command tui "" :project :cwd dir)))))
             (check "/lore shows project lore"
                    (search "this repo uses rebase merges" scrollback))
             (check "/lore shows session lore"
                    (search "stay focused on the current bug" scrollback))
             (check "/lore does not show global lore"
                    (not (search "always run the whole suite" scrollback))))
           (let ((scrollback
                  (with-output-to-string (fake-tty)
                    (let ((evo.tui::*tty-out* fake-tty))
                      (evo.tui::lore-command tui "" :global :cwd dir)))))
             (check "/global-lore shows global lore"
                    (search "always run the whole suite" scrollback))
             (check "/global-lore does not show project lore"
                    (not (search "this repo uses rebase merges" scrollback)))
             (check "/global-lore does not show session lore"
                    (not (search "stay focused on the current bug" scrollback))))
           ;; Adding through each command must land in the right store too.
           (with-output-to-string (fake-tty)
             (let ((evo.tui::*tty-out* fake-tty))
               (evo.tui::lore-command tui "prefer small commits" :project :cwd dir)
               (evo.tui::lore-command tui "always use two-space indent" :global :cwd dir)))
           (check "/lore add lands in project scope"
                  (find "prefer small commits" (all-lore :cwd dir) :test #'equal))
           (check "/global-lore add lands in global scope"
                  (find "always use two-space indent" (all-lore-entries :cwd dir)
                        :key (lambda (e) (getf e :text)) :test #'equal))
           (check "/global-lore add does not land in project scope"
                  (not (find "always use two-space indent"
                             (evo.kernel::read-lore-file
                              (evo.kernel::lore-file :project dir))
                             :key (lambda (e) (getf e :text)) :test #'equal))))
      (evo.port:setenv "EVO_HOME"
                       (namestring (uiop:ensure-directory-pathname
                                    (format nil "~a/evo-unit-home" (tmp-dir))))))))

;;; Memory

(defun test-project-memory ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-memory-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal)))
    (check "project memory tool registered" (find-tool "project_memory"))
    (check "global memory tool registered" (find-tool "global_memory"))
    (check "project memory command registered" (gethash "memory" evo::*commands*))
    (check "global memory command registered" (gethash "global-memory" evo::*commands*))
    (check "project memory starts empty"
           (null (evo.memory:read-memories :cwd dir)))
    (evo.memory::perform-project-memory-action
     '(:action "add" :kind "decision"
       :text "Use structured project memory entries.")
     :cwd dir)
    (evo.memory::perform-project-memory-action
     '(:action "add" :kind "constraint"
       :text "Core extensions use the public API.")
     :cwd dir)
    (let* ((entries (evo.memory:read-memories :cwd dir))
           (decision (find :decision entries :key (lambda (entry) (pget entry :kind))))
           (constraint (find :constraint entries :key (lambda (entry) (pget entry :kind))))
           (decision-id (pget decision :id))
           (constraint-id (pget constraint :id)))
      (check "project memory persists structured entries"
             (and (= 2 (length entries))
                  (string-prefix-p "mem-" decision-id)
                  (pget decision :created-at)
                  (pget decision :updated-at)))
      (check "project memory query filters text"
             (let ((result (evo.memory::perform-project-memory-action
                            '(:action "query" :query "structured") :cwd dir)))
               (and (search decision-id result)
                    (not (search constraint-id result)))))
      (evo.memory::perform-project-memory-action
       (list :action "update" :id decision-id
             :text "Use structured entries so stale memories can be removed.")
       :cwd dir)
      (check "project memory updates by stable id"
             (equal "Use structured entries so stale memories can be removed."
                    (pget (find decision-id (evo.memory:read-memories :cwd dir)
                                :key (lambda (entry) (pget entry :id))
                                :test #'equal)
                          :text)))
      (check-signals "project memory rejects exact duplicates"
                     (evo.memory::perform-project-memory-action
                      '(:action "add" :kind "decision"
                        :text "Use structured entries so stale memories can be removed.")
                      :cwd dir))
      (check-signals "project memory rejects unknown kinds"
                     (evo.memory::perform-project-memory-action
                      '(:action "add" :kind "misc" :text "junk") :cwd dir))
      (let ((rendered (evo.memory:render-memories
                       (evo.memory:read-memories :cwd dir))))
        (check "project memory renders ordered sections"
               (and (search "## Constraints" rendered)
                    (search "## Decisions" rendered)
                    (< (search "## Constraints" rendered)
                       (search "## Decisions" rendered)))))
      (evo.memory::perform-project-memory-action
       (list :action "remove" :id constraint-id) :cwd dir)
      (check "project memory removes stale entries"
             (and (= 1 (length (evo.memory:read-memories :cwd dir)))
                  (null (find constraint-id (evo.memory:read-memories :cwd dir)
                              :key (lambda (entry) (pget entry :id))
                              :test #'equal))))
      (check "memory slash command shows current entries"
             (search decision-id
                     (evo.memory::memory-command
                      (list :agent agent :args "") :cwd dir)))
      (evo.memory::memory-command
       (list :agent agent :args "remember the release command") :cwd dir)
      (check "memory slash command steers the agent"
             (steering-pending-p agent))
      (evo.kernel::drain-steering agent)
      (check "memory slash command records user intention"
             (search "remember the release command"
                     (pget (first (message-content
                                   (first (state-messages (fold-state journal)))))
                           :text))))
    (let* ((session-journal (make-session-journal dir))
           (session-agent (make-agent :journal session-journal)))
      (evo.memory::inject-memory-scope
       (list :agent session-agent :resumed nil) :project dir)
      (let ((messages (state-messages (fold-state session-journal))))
        (check "fresh session injects project memory once"
               (and (= 1 (length messages))
                    (equal "project-memory" (pget (pget (first messages) :meta) :key))
                    (search "stale memories can be removed"
                            (pget (first (message-content (first messages))) :text)))))
      (evo.memory::inject-session-memory
       (list :agent session-agent :resumed t) :cwd dir)
      (check "resumed session does not reinject project memory"
             (= 1 (length (state-messages (fold-state session-journal))))))))

(defun test-global-memory ()
  (let* ((previous-home (or (uiop:getenv "EVO_HOME")
                            (namestring (evo-home))))
         (global-home (uiop:ensure-directory-pathname
                       (format nil "~a/evo-global-memory-~a/"
                               (tmp-dir) (gen-id))))
         (dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-global-memory-project-~a/"
                       (tmp-dir) (gen-id)))))
    (ensure-directories-exist dir)
    (evo.port:setenv "EVO_HOME" (namestring global-home))
    (unwind-protect
         (progn
           (evo.memory::perform-global-memory-action
            '(:action "add" :kind "convention"
              :text "Prefer concise technical responses.")
            :cwd dir)
           (evo.memory::perform-project-memory-action
            '(:action "add" :kind "fact"
              :text "This project uses session-level memory.")
            :cwd dir)
           (let ((global (evo.memory:read-memories :scope :global :cwd dir))
                 (project (evo.memory:read-memories :scope :project :cwd dir)))
             (check "global and project memory stores are isolated"
                    (and (= 1 (length global))
                         (= 1 (length project))
                         (search "concise" (pget (first global) :text))
                         (search "session-level" (pget (first project) :text))))
             (check "global memory uses evo home"
                    (equal (truename (evo.memory:memory-file :scope :global :cwd dir))
                           (truename (merge-pathnames "memory.sexp" global-home))))
             (check "global memory slash command shows current entries"
                    (search (pget (first global) :id)
                            (evo.memory::global-memory-command
                             (list :agent nil :args "") :cwd dir))))
           (let* ((journal (make-session-journal dir))
                  (agent (make-agent :journal journal)))
             (evo.memory::global-memory-command
              (list :agent agent :args "remember my preferred test style") :cwd dir)
             (check "global memory slash command steers to global tool"
                    (progn
                      (evo.kernel::drain-steering agent)
                      (search "`global_memory`"
                              (pget (first (message-content
                                            (first (state-messages
                                                    (fold-state journal)))))
                                    :text))))
             (let* ((session-journal (make-session-journal dir))
                    (session-agent (make-agent :journal session-journal)))
               (evo.memory::inject-session-memory
                (list :agent session-agent :resumed nil) :cwd dir)
               (let ((messages (state-messages (fold-state session-journal))))
                 (check "fresh session injects global then project memory"
                        (equal '("global-memory" "project-memory")
                               (mapcar (lambda (message)
                                         (pget (pget message :meta) :key))
                                       messages)))
                 (check "global memory snapshot has ordinary context"
                        (search "Prefer concise technical responses."
                                (pget (first (message-content (first messages)))
                                      :text))))
               (evo.memory::inject-session-memory
                (list :agent session-agent :resumed t) :cwd dir)
               (check "resume does not reinject either memory scope"
                      (= 2 (length (state-messages
                                    (fold-state session-journal))))))))
      (evo.port:setenv "EVO_HOME" previous-home))))

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
              (format nil "~a/evo-prompt-~a/" (tmp-dir) (gen-id)))))
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
      (check "system prompt directs explicit memory management"
             (and (search "requests to remember, refine, or forget" prompt)
                  (search "`project_memory`" prompt)
                  (search "`global_memory`" prompt)))
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
               (format nil "~a/evo-toolev-~a/" (tmp-dir) (gen-id))))
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

;;; Interrupts yield to active UI affordances, then unblock tool execution.

(defun test-interrupt ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-interrupt-~a/"
                       (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent)))
    (setf (evo.tui::tui-task tui)
          (evo.tui::make-tui-task :id "interrupt-probe" :kind :run))
    (evo.tui::eb-insert-text (evo.tui::tui-editor tui) "/t")
    (with-output-to-string (fake-tty)
      (let ((evo.tui::*tty-out* fake-tty)
            (evo.tui::*region-height* 0)
            (evo.tui::*region-cursor-row* 0))
        (evo.tui::handle-key-edit tui :escape)
        (check "esc while running hides popup before interrupting"
               (and (not (agent-abort-flag agent))
                    (equal (evo.tui::tui-complete-dismissed tui) "t")))
        (evo.tui::handle-key-edit tui :escape)
        (check "esc interrupts after popup is dismissed"
               (agent-abort-flag agent)))))
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-select-interrupt-~a/"
                       (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent)))
    (setf (evo.tui::tui-task tui)
          (evo.tui::make-tui-task :id "interrupt-probe" :kind :run))
    (evo.tui::enter-select tui "pick:" (list (cons "item" :item)) #'identity)
    (with-output-to-string (fake-tty)
      (let ((evo.tui::*tty-out* fake-tty)
            (evo.tui::*region-height* 0)
            (evo.tui::*region-cursor-row* 0))
        (evo.tui::handle-key-select tui :escape)))
    (check "esc while running cancels select before interrupting"
           (and (eq (evo.tui::tui-mode tui) :edit)
                (not (agent-abort-flag agent)))))
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-bash-abort-~a/"
                       (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (marker (merge-pathnames "started" dir))
         (aborted nil)
         (worker
           (bt:make-thread
            (lambda ()
              (let ((evo.kernel::*executing-agent* agent))
                (handler-case
                    (evo.kernel::tool-bash
                     (list :command
                           ;; Same shape either way: create the marker, then
                           ;; sleep long enough to be interrupted.  The shell
                           ;; is the platform's (PowerShell on Windows), so the
                           ;; command must be spelled for it.
                           #+evo-windows
                           (format nil "New-Item -ItemType File -Force -Path '~a' > $null; Start-Sleep 5"
                                   (namestring marker))
                           #-evo-windows
                           (format nil "touch ~s; sleep 5" (namestring marker))
                           :timeout 10))
                  (error (e)
                    (setf aborted (search "aborted" (format nil "~a" e)))))))
            :name "test-bash-abort")))
    ;; Wait long enough for the command to start: /bin/sh is instant, but the
    ;; Windows shell is PowerShell, whose cold start (script AMSI-scanned, .NET
    ;; JITed) can be several seconds — none of which is the abort latency this
    ;; test measures.
    (loop repeat (if (evo.port:windows-p) 3000 100)
          until (probe-file marker) do (sleep 0.01))
    (check "bash command starts before interrupt" (probe-file marker))
    (check "bash process remains owned by its execution thread"
           (bt:with-lock-held ((evo.kernel::agent-lock agent))
             (null (evo.kernel::agent-abort-cleanups agent))))
    ;; Time the abort itself — from the request, not from launch — so the
    ;; measurement isolates "did it interrupt the 5s sleep" from shell startup.
    (let ((abort-start (get-internal-real-time)))
      (request-abort agent)
      (bt:join-thread worker)
      (ignore-errors (delete-file marker))
      (let ((elapsed (/ (- (get-internal-real-time) abort-start)
                        internal-time-units-per-second)))
        ;; Comfortably under the 5s the command would have slept: the kill
        ;; landed instead of the sleep being waited out.  taskkill /T is a
        ;; touch slower than a SIGKILL, hence the wider Windows margin.
        (check "bash observes abort without waiting for command completion"
               (and aborted (< elapsed (if (evo.port:windows-p) 4 3/2))))))))

;;; Background jobs: a command past its timeout ceiling detaches instead of
;;; being killed, `wait` collects/kills it, and no job outlives its evo.

(defun test-jobs ()
  (flet ((sleepcmd (n)
           #+evo-windows (format nil "Start-Sleep ~d" n)
           #-evo-windows (format nil "sleep ~d" n)))
    ;; Reset the registry so counts and IDs are deterministic in isolation.
    (setf evo.kernel::*jobs* nil evo.kernel::*job-counter* 0)
    ;; Ceiling of 0s forces the still-running command to detach at once.
    (multiple-value-bind (note details)
        (evo.kernel::tool-bash (list :command (sleepcmd 2) :timeout 0))
      (check "bash detaches at the ceiling instead of killing"
             (getf details :running))
      (check "detach note names the job" (search "job" note))
      (let ((id (getf details :job-id)))
        (check "job id is a positive integer" (and (integerp id) (plusp id)))
        (check "job is registered" (evo.kernel::find-job id))
        (check "running-jobs-summary reports the live job"
               (let ((s (running-jobs-summary))) (and s (>= (getf s :count) 1))))
        (check "job status segment renders the ▷ marker"
               (let ((seg (evo.tui::jobs-status-segment)))
                 (and (stringp seg) (search "▷" seg))))
        ;; Wait returns the instant it finishes, with the exit code.
        (multiple-value-bind (wnote wdetails)
            (evo.kernel::tool-wait (list :job-id id :timeout 10))
          (check "wait reports the finished job's exit code"
                 (eql 0 (getf wdetails :exit-code)))
          (check "wait note says finished" (search "finished" wnote))
          (check "a finished job is retired from the registry"
                 (null (evo.kernel::find-job id)))
          (check "no jobs left running" (null (running-jobs-summary))))))
    ;; wait with :kill terminates a long job.
    (setf evo.kernel::*jobs* nil evo.kernel::*job-counter* 0)
    (multiple-value-bind (note details)
        (evo.kernel::tool-bash (list :command (sleepcmd 30) :timeout 0))
      (declare (ignore note))
      (let* ((id (getf details :job-id))
             (job (evo.kernel::find-job id)))
        (check "detached job records a raw pid for the reaper"
               (integerp (evo.kernel::job-pid job)))
        (multiple-value-bind (wnote wdetails)
            (evo.kernel::tool-wait (list :job-id id :kill t))
          (declare (ignore wnote))
          (check "wait kill is reported" (getf wdetails :killed))
          (check "a killed job is retired" (null (evo.kernel::find-job id))))))
    ;; wait on an unknown id is an error, not a crash.
    (check-signals "wait on an unknown job signals"
                   (evo.kernel::tool-wait (list :job-id 999999)))
    ;; job-new-output returns only the freshly appended slice, by offset.
    (let* ((dir (uiop:ensure-directory-pathname
                 (format nil "~a/evo-joboutput-~a/" (tmp-dir) (gen-id))))
           (f (progn (ensure-directories-exist dir)
                     (merge-pathnames "out" dir)))
           (job (evo.kernel::make-job :id 1 :command "x" :out-file f
                                      :chars-read 0
                                      :start-time (get-universal-time))))
      (with-open-file (o f :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
        (format o "hello~%"))
      (check "job-new-output reads the first slice"
             (search "hello" (evo.kernel::job-new-output job)))
      (check "job-new-output is empty when nothing was appended"
             (string= "" (evo.kernel::job-new-output job)))
      (with-open-file (o f :direction :output :if-exists :append
                           :if-does-not-exist :create)
        (format o "world~%"))
      (check "job-new-output returns only the appended part"
             (let ((out (evo.kernel::job-new-output job)))
               (and (search "world" out) (not (search "hello" out))))))
    ;; The exit reaper kills live jobs by pid and empties the registry.
    (setf evo.kernel::*jobs* nil evo.kernel::*job-counter* 0)
    (multiple-value-bind (note details)
        (evo.kernel::tool-bash (list :command (sleepcmd 30) :timeout 0))
      (declare (ignore note))
      (let* ((id (getf details :job-id))
             (proc (evo.kernel::job-process (evo.kernel::find-job id))))
        (evo.kernel::reap-all-jobs)
        (loop repeat 300 until (not (evo.port:process-alive-p proc))
              do (sleep 0.01))
        (check "reap-all-jobs kills the running process"
               (not (evo.port:process-alive-p proc)))
        (check "reap-all-jobs empties the registry" (null evo.kernel::*jobs*))))
    ;; Status segment is wired onto the inner right at order 200.
    (let ((seg (find :jobs (evo.tui::status-segments :right)
                     :key #'evo.tui::status-segment-name)))
      (check "jobs segment sits on the right" seg)
      (check "jobs segment order is 200 (inner, past model-load at 100)"
             (and seg (= 200 (evo.tui::status-segment-order seg)))))))

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

;;; The extension points plan mode used to be built on.  Plan mode is gone
;;; (one mode, auto, by design) but the seams it proved out are public API
;;; and stay: the :tool-call gate, set-active-tools, keyed inject-context
;;; plus the :transform-context filter, and custom journal state.  These
;;; tests drive them the way a userspace extension would.

(defun tool-call-blocked-p (name args)
  "Run the real :tool-call hook chain.  Returns (values blocked-p reason)."
  (multiple-value-bind (args blocked-p reason)
      (evo.kernel::intercept-tool-call name args)
    (declare (ignore args))
    (values (and blocked-p t) reason)))

(defun run-transform-hooks (messages)
  "Project MESSAGES through the registered :transform-context hooks."
  (dolist (hook (evo.kernel::event-hook-functions :transform-context) messages)
    (setf messages (funcall hook messages))))

(defmacro with-temp-hooks (&body body)
  "Run BODY with the hook table saved and restored, so a test can register
:tool-call/:transform-context hooks without leaking them into later tests."
  (let ((saved (gensym)))
    `(let ((,saved (let ((copy (make-hash-table)))
                     (maphash (lambda (k v) (setf (gethash k copy) v))
                              evo.kernel::*event-hooks*)
                     copy)))
       (unwind-protect (progn ,@body)
         (clrhash evo.kernel::*event-hooks*)
         (maphash (lambda (k v) (setf (gethash k evo.kernel::*event-hooks*) v))
                  ,saved)))))

(defun active-tool-names (state)
  "Names of the tools the model is actually offered for STATE."
  (mapcar #'evo.kernel:tool-name (evo.kernel:active-tools state)))

(defun test-no-modes ()
  "Only one mode by design: nothing journals a \"mode\", no gate is installed
out of the box, and no frontend surface offers a switch."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-nomode-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (evo:*agent* agent))
    ;; No mode state, and nothing gates a write out of the box.
    (check "no mode custom state" (null (evo.journal:custom-state (fold-state journal) "mode")))
    (check "write is not gated" (not (tool-call-blocked-p "write" '(:path "x" :content "y"))))
    (check "bash is not gated" (not (tool-call-blocked-p "bash" '(:command "rm -rf build"))))
    (check "full tool set by default"
           (equal (active-tool-names (fold-state journal)) (all-tool-names)))
    ;; The package is gone; no symbol of it survives to be called.
    (check "EVO.PLAN package is gone" (null (find-package :evo.plan)))
    ;; No frontend surface: no command, no completion candidate, no indicator.
    (let ((commands (evo.tui::all-commands)))
      (dolist (name '("permission" "mode" "plan" "auto"))
        (check (format nil "/~a is not a completion candidate" name)
               (not (assoc name commands :test #'string=)))))
    (let ((tui (evo.tui::make-tui :agent agent)))
      (dolist (name '("permission" "mode" "plan" "auto"))
        (check (format nil "/~a is not a builtin command" name)
               (not (evo.tui::builtin-command tui name ""))))
      ;; The status line leads with the model, and carries no mode indicator.
      (let ((line (evo.tui::status-line tui)))
        (check "status line has no mode indicator"
               (and (not (search "auto" line)) (not (search "plan" line))
                    (not (search "◆" line)) (not (search "◇" line))))
        (check "status line still reports context" (search "ctx" line)))
      ;; shift+tab still decodes as a key, it just no longer means anything.
      (with-output-to-string (fake-tty)
        (let ((evo.tui::*tty-out* fake-tty)
              (evo.tui::*region-height* 0)
              (evo.tui::*region-cursor-row* 0))
          (evo.tui::handle-key-edit tui :shift-tab)))
      (check "shift+tab journals no mode"
             (null (evo.journal:custom-state (fold-state journal) "mode"))))
    (check "plan mode source file is gone"
           (not (probe-file (merge-pathnames "src/core-ext/plan-mode.lisp" (uiop:getcwd)))))))

(defun test-tool-call-gate-extension-point ()
  "The :tool-call hook — the one interception point sandboxes and permission
gates build on.  A hook returning (:block t :reason ...) stops the call."
  (with-temp-hooks
    (check "no gate: write runs" (not (tool-call-blocked-p "write" '(:path "x"))))
    (evo:on :tool-call
            (lambda (call)
              (when (equal (pget call :name) "write")
                (list :block t :reason "read-only by policy"))))
    (multiple-value-bind (blocked-p reason) (tool-call-blocked-p "write" '(:path "x"))
      (check "gate blocks the named tool" blocked-p)
      (check "block reason is carried through" (search "read-only by policy" reason)))
    (check "gate leaves other tools alone"
           (not (tool-call-blocked-p "read" '(:path "x")))))
  (check "hook table restored after the test"
         (not (tool-call-blocked-p "write" '(:path "x")))))

(defun test-active-tools-extension-point ()
  "SET-ACTIVE-TOOLS gates the tool set the model is offered; NIL restores
the full set.  It is journal state (:tools-change), not an in-memory flag."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-tools-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal)))
    (check "full tool set by default"
           (equal (active-tool-names (fold-state journal)) (all-tool-names)))
    (evo:set-active-tools agent '("read" "bash"))
    (check "gated set folds out of the journal"
           (equal (state-tools (fold-state journal)) '("read" "bash")))
    (check "gated set is what the model is offered"
           (equal (active-tool-names (fold-state journal)) '("read" "bash")))
    (evo:set-active-tools agent nil)
    (check "nil restores the full tool set"
           (equal (active-tool-names (fold-state journal)) (all-tool-names)))))

(defun test-injected-context-extension-point ()
  "INJECT-CONTEXT with a :key, and the :transform-context hook that filters
by that key: the journal keeps the message forever, the projection sent to
the model is where it can be removed."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-inject-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (evo:*agent* agent))
    (evo:inject-context "SOME INSTRUCTIONS" :key "test-key" :agent agent)
    (check "injected message lands in the journal under its key"
           (find-if (lambda (m) (equal (pget (pget m :meta) :key) "test-key"))
                    (state-messages (fold-state journal))))
    (let ((messages (list '(:role :user :content ((:type :text :text "hi")))
                          (list :role :user :meta (list :key "test-key")
                                :content '((:type :text :text "SOME INSTRUCTIONS"))))))
      (with-temp-hooks
        (check "unfiltered projection carries both" (= 2 (length (run-transform-hooks messages))))
        (evo:on :transform-context
                (lambda (ms)
                  (remove-if (lambda (m) (equal (pget (pget m :meta) :key) "test-key")) ms)))
        (check "a transform-context hook filters by key"
               (= 1 (length (run-transform-hooks messages))))))
    (check "the journal still carries the filtered message"
           (find-if (lambda (m) (equal (pget (pget m :meta) :key) "test-key"))
                    (state-messages (fold-state journal))))))

(defun test-custom-state-extension-point ()
  "SET-CUSTOM-STATE / CUSTOM-STATE: extension state as a journal fold, so it
survives restart and compaction.  Plan mode kept its mode here; other
extensions still keep theirs."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-custom-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (evo:*agent* agent))
    (check "unset key is nil" (null (evo:custom-state "widget" agent)))
    (evo:set-custom-state "widget" "on" agent)
    (check "custom state round-trips" (equal "on" (evo:custom-state "widget" agent)))
    (check "custom state is a journal fold"
           (equal "on" (evo.journal:custom-state (fold-state journal) "widget")))
    (evo:set-custom-state "widget" "off" agent)
    (check "last write wins" (equal "off" (evo:custom-state "widget" agent)))))

;;; /eval

(defun eval-command-output (args)
  (evo.eval::eval-command (list :args args)))

(defun test-eval ()
  (check "eval command registered" (gethash "eval" evo::*commands*))
  ;; Arity: exactly one form, checked before anything runs.
  (check "single sexpr evaluates"
         (equal "⇒ 3" (eval-command-output "(+ 1 2)")))
  (check "leading and trailing whitespace is not a second form"
         (equal "⇒ 3" (eval-command-output (format nil "  (+ 1 2) ~%"))))
  (check "a trailing comment is not a second form"
         (equal "⇒ 3" (eval-command-output (format nil "(+ 1 2) ; sum~%"))))
  (let ((rejected (eval-command-output "(+ 1 2) (+ 3 4)")))
    (check "multiple sexprs are rejected" (search "expected exactly one" rejected))
    (check "multiple sexprs report the count" (search "2 forms" rejected))
    (check "multiple sexprs are pointed at progn" (search "(progn ...)" rejected)))
  (check "empty content is rejected with a usage line"
         (search "nothing to evaluate" (eval-command-output "   ")))
  (check "unreadable content is rejected with a reason"
         (search "unreadable sexpr" (eval-command-output "(+ 1 2")))
  ;; Rejection is decided at read time, so nothing in the input can run
  ;; before the arity check does.
  (let ((evo.user::*eval-canary* nil))
    (declare (special evo.user::*eval-canary*))
    (eval-command-output "#.(setf evo.user::*eval-canary* t) (+ 1 2)")
    (check "a rejected input never evaluates any of its forms"
           (null evo.user::*eval-canary*)))
  ;; The image, not a sandbox: what the session registered is reachable.
  (check "eval sees the live tool registry"
         (equal (format nil "⇒ ~a"
                        (let ((*print-case* :downcase))
                          (prin1-to-string (length (all-tool-names)))))
                (eval-command-output "(length (evo:all-tools))")))
  (check "eval sees userspace definitions from earlier evals"
         (progn (eval-command-output "(progn (defun eval-probe () :from-userspace)
                                             (eval-probe))")
                (equal "⇒ :from-userspace" (eval-command-output "(eval-probe)"))))
  (check "eval reads in the userspace package"
         (equal (format nil "⇒ ~s" (package-name (find-package :evo.user)))
                (eval-command-output "(package-name *package*)")))
  ;; Output is captured, never written at the frontend's terminal.
  (let ((result (eval-command-output "(progn (princ \"printed\") :done)")))
    (check "printed output is captured above the value"
           (equal (format nil "printed~%⇒ :done") result)))
  ;; A failing form is reported, not signalled at the caller.
  (let ((result (eval-command-output "(error \"boom\")")))
    (check "an error during evaluation is reported" (search "boom" result))
    (check "an error names its condition type" (search "simple-error" result)))
  (check "no values prints as no values"
         (equal "⇒ ; no values" (eval-command-output "(values)")))
  (check "multiple values print one per line"
         (equal (format nil "⇒ 1~%⇒ 2") (eval-command-output "(values 1 2)")))
  ;; Bounded printing: a live image holds unbounded and circular objects.
  (check "long results are elided, not dumped"
         (search "..." (eval-command-output "(make-list 5000)")))
  (check "circular results terminate"
         (let ((evo.eval::*print-length-limit* 10))
           (plusp (length (eval-command-output
                           "(let ((x (list 1 2))) (setf (cdr (last x)) x) x)"))))))

;;; The `eval` tool — the same evaluator, reached by the model.  It replaced
;;; load_extension in the tool set: the loader is now one form away, and the
;;; load it performs is still journaled, which is the only thing the removed
;;; tool did that bare evaluation does not.

(defun eval-tool-output (code)
  "Run the eval tool on CODE.  Returns (values CONTENT IS-ERROR)."
  (multiple-value-bind (content details is-error)
      (execute-tool (find-tool "eval") (list :code code))
    (declare (ignore details))
    (values content is-error)))

(defun test-eval-tool ()
  (check "eval is a tool" (find-tool "eval"))
  (check "load_extension is not a tool" (null (find-tool "load_extension")))
  (check "tool evaluates one form" (equal "⇒ 3" (eval-tool-output "(+ 1 2)")))
  ;; A body, unlike the command: defining a helper and calling it is one
  ;; thought, so it is one tool call.
  (check "tool evaluates a body and returns the last value"
         (equal "⇒ 7" (eval-tool-output
                       "(defun eval-tool-probe (x) (+ x 4)) (eval-tool-probe 3)")))
  (check "arithmetic is exact" (equal "⇒ 1/3" (eval-tool-output "(/ 1 3)")))
  (check "printed output comes back above the value"
         (equal (format nil "printed~%⇒ :done")
                (eval-tool-output "(princ \"printed\") :done")))
  ;; A condition is a FAILED tool call, not a success whose text starts with
  ;; a cross — the turn has to be able to tell them apart.
  (multiple-value-bind (content is-error) (eval-tool-output "(error \"boom\")")
    (check "a failing form fails the tool call" is-error)
    (check "the failure carries the condition text" (search "boom" content)))
  (multiple-value-bind (content is-error) (eval-tool-output "(+ 1 2")
    (check "unreadable code fails the tool call" is-error)
    (check "unreadable code says what was wrong" (search "unreadable code" content)))
  (multiple-value-bind (content is-error) (eval-tool-output "   ")
    (check "empty code fails the tool call" is-error)
    (check "empty code says what was missing"
           (search "nothing to evaluate" content)))
  ;; The durable half: evo:load-extension journals against the live agent, so
  ;; a file loaded from inside eval replays when the session resumes.
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-evaltool-~a/" (tmp-dir) (gen-id))))
         (file (progn (ensure-directories-exist dir)
                      (merge-pathnames "300-eval-tool-load.lisp" dir)))
         (journal (make-session-journal dir))
         (agent (make-agent :journal journal))
         (evo:*agent* agent))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string "(in-package :evo.user)
(defun eval-tool-loaded-p () t)" out))
    (eval-tool-output (format nil "(evo:load-extension ~s)" (namestring file)))
    (check "a file loaded through eval is really loaded"
           (let ((sym (find-symbol "EVAL-TOOL-LOADED-P" :evo.user)))
             (and sym (fboundp sym) (funcall sym))))
    (check "and its load is journaled for replay"
           (find (namestring (truename file))
                 (evo.journal:state-loads (fold-state journal))
                 :key (lambda (e) (pget e :path)) :test #'equal))))

;;; Model/provider registries + provider-API protocol

(defun test-registry ()
  (reset-user-registries)
  (register-model* "reg-a" :provider :anthropic :api :anthropic-messages
                   :context-window 1000 :max-output 100)
  (register-model* "reg-b" :provider :proxy-co
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
  ;; Same id under a second provider: a distinct, selectable model.
  (register-model* "reg-dupe" :provider :anthropic :api :anthropic-messages
                   :context-window 1000 :max-output 100)
  (register-model* "reg-dupe" :provider :proxy-co :api :anthropic-messages
                   :context-window 2000 :max-output 200)
  (check "same id, different provider: both registered"
         (equal '(:anthropic :proxy-co) (model-providers "reg-dupe")))
  (check "find-model disambiguates by provider"
         (and (= 1000 (pget (find-model "reg-dupe" :anthropic) :context-window))
              (= 2000 (pget (find-model "reg-dupe" :proxy-co) :context-window))))
  (check "bare id resolves to the first registration"
         (equal :anthropic (pget (find-model "reg-dupe") :provider)))
  (check "unknown provider for a known id names the registered ones"
         (handler-case (progn (find-model "reg-dupe" :nope) nil)
           (error (e) (let ((s (format nil "~a" e)))
                        (and (search "reg-dupe" s) (search "proxy-co" s))))))
  (register-model* "reg-dupe" :provider :proxy-co :api :anthropic-messages
                   :context-window 3000 :max-output 200)
  (check "re-register same (id, provider) replaces in place"
         (and (= 2 (length (model-providers "reg-dupe")))
              (= 3000 (pget (find-model "reg-dupe" :proxy-co) :context-window))))
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
                (pget (provider-config :anthropic) :base-url))))

(defun test-apis ()
  (check "find-api anthropic" (find-api :anthropic-messages))
  (check-signals "unknown api signals" (find-api :no-such-api))
  (check "endpoint path"
         (equal "/v1/messages" (endpoint-path (find-api :anthropic-messages)))))

;;; Extension-defined provider APIs (a new wire protocol from userspace).

;; A minimal API: implements the protocol, seeds nothing.  The common case
;; for an extension, and the one that used to break /reload.
(defclass bare-fixture-api (provider-api) ())
(defmethod endpoint-path ((api bare-fixture-api)) "/v1/bare")

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
                  "PERFORM-REQUEST" "MAP-SSE-EVENTS"
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
                   (:id "k3" :provider :kimi)
                   (:id "m" :provider :a)))
         (width (reduce #'max models
                        :key (lambda (m) (length (string (pget m :provider))))
                        :initial-value 0))
         (labels* (mapcar (lambda (m) (evo.tui::model-row-label m width)) models)))
    (check "provider leads the row"
           (evo.util:string-prefix-p "anthropic" (first labels*)))
    (check "provider is downcased"
           (evo.util:string-prefix-p "kimi" (second labels*)))
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

(defun test-same-id-multi-provider ()
  "The bug: registering one model id under several providers used to keep
only the last, so only one was selectable.  Identity is (id, provider);
selection journals the provider, and every resolution point honours it."
  (reset-user-registries)
  (reset-settings)
  (register-provider* :direct :base-url "https://direct.invalid")
  (register-provider* :proxy :base-url "https://proxy.invalid")
  (register-model* "shared-id" :provider :direct :api :anthropic-messages
                   :context-window 200000 :max-output 64000)
  (register-model* "shared-id" :provider :proxy :api :anthropic-messages
                   :context-window 500000 :max-output 32000)
  ;; 1. Both survive registration and both reach the picker.
  (check "picker lists every provider for the id"
         (equal '(:direct :proxy)
                (mapcar (lambda (m) (pget m :provider))
                        (remove-if-not (lambda (m) (equal "shared-id" (pget m :id)))
                                       (all-models)))))
  ;; 2. A journaled /model choice routes to that provider's endpoint.
  (let ((journal (make-session-journal "/tmp")))
    (append-entry journal '(:type :model-change :model "shared-id" :provider :proxy))
    (let* ((state (fold-state journal))
           (agent (make-agent :journal journal))
           (id (effective-model-id state agent))
           (model (find-model id (evo.kernel:effective-model-provider state id))))
      (check "journaled choice resolves to its provider"
             (equal :proxy (pget model :provider)))
      (check "journaled choice carries that provider's config"
             (equal 500000 (pget model :context-window)))
      (check "journaled choice hits that provider's endpoint"
             (equal "https://proxy.invalid"
                    (pget (provider-config (pget model :provider)) :base-url)))))
  ;; 3. No journaled provider: first registration wins, so a bare :model
  ;; setting still boots instead of erroring on the ambiguity.
  (let* ((journal (make-session-journal "/tmp"))
         (state (fold-state journal)))
    (set-setting :model "shared-id")
    (let* ((agent (make-agent :journal journal))
           (id (effective-model-id state agent))
           (model (find-model id (evo.kernel:effective-model-provider state id))))
      (check "bare default resolves to the first registration"
             (equal :direct (pget model :provider))))
    ;; 4. :model-provider names a different default without touching order.
    (set-setting :model-provider :proxy)
    (let* ((agent (make-agent :journal journal))
           (id (effective-model-id state agent))
           (model (find-model id (evo.kernel:effective-model-provider state id))))
      (check ":model-provider setting selects the default provider"
             (equal :proxy (pget model :provider))))
    ;; 5. A stale setting naming a provider that doesn't serve this id is
    ;; ignored rather than breaking an unrelated model.
    (register-model* "solo-id" :provider :direct :api :anthropic-messages
                     :context-window 1000 :max-output 100)
    (set-setting :model "solo-id")
    (let* ((agent (make-agent :journal journal))
           (id (effective-model-id state agent)))
      (check "stale :model-provider is ignored for an id it doesn't serve"
             (equal :direct (pget (find-model id (evo.kernel:effective-model-provider state id))
                                  :provider)))))
  (reset-settings)
  (reset-user-registries))

(defun register-fixture-models ()
  "The unit suite's stand-in for init.lisp: the model registry ships empty."
  (register-model* "claude-sonnet-5" :provider :anthropic
                   :context-window 1000000 :max-output 128000
                   :effort t :thinking-mode :adaptive)
  (register-model* "claude-opus-5" :provider :anthropic
                   :context-window 1000000 :max-output 128000
                   :effort t :thinking-mode :adaptive))

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
                  :context-window 200000 :max-output 64000
                  :thinking-mode :adaptive))
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
      (check "anth req adaptive thinking with summaries"
             (and (equal (jget "thinking" "type") "adaptive")
                  (equal (jget "thinking" "display") "summarized")
                  (null (jget "thinking" "budget_tokens"))))
      (check "anth req tools cached on last"
             (let ((tl (aref (jget "tools") 0)))
               (and (equal (evo.provider::jget tl "name") "bash")
                    (evo.provider::jget tl "cache_control"))))
      (let ((messages (jget "messages")))
        ;; The errored turn is elided; the tail is the tool result and what
        ;; the user said next, in a message each — see MESSAGES->JSON.
        (check "anth req errored turn elided, tail split by kind"
               (equal (map 'list (lambda (m) (evo.provider::jget m "role")) messages)
                      '("user" "assistant" "user" "user")))
        (let ((result-blocks (evo.provider::jget (aref messages 2) "content"))
              (said-blocks (evo.provider::jget (aref messages 3) "content")))
          (check "anth req answers the tool call with tool_result alone"
                 (and (= 1 (length result-blocks))
                      (equal (evo.provider::jget (aref result-blocks 0) "type")
                             "tool_result")))
          (check "anth req keeps what the user said in its own message"
                 (and (= 1 (length said-blocks))
                      (equal (evo.provider::jget (aref said-blocks 0) "type") "text")))
          (check "anth req cache breakpoint on last block"
                 (evo.provider::jget (aref said-blocks (1- (length said-blocks)))
                                     "cache_control")))))
    (let ((raw2 (build-request (find-api :anthropic-messages)
                               :model model :system nil :messages history
                               :tools nil :thinking-level :low)))
      (check "anth req no budget_tokens at any rung"
             (not (search "budget_tokens" raw2))))
    (check "anth req no output_config without :effort"
           (not (search "output_config" raw)))))

;;; output_config.effort: the level dial on models that have one.

(defun test-anthropic-effort ()
  (let* ((history (list '(:role :user :content ((:type :text :text "go")))))
         (adaptive '(:id "fixture-adaptive" :provider :anthropic
                     :api :anthropic-messages :context-window 200000
                     :max-output 64000 :thinking-mode :adaptive
                     :effort (:low :medium :high :xhigh :max)))
         (capped '(:id "fixture-capped" :provider :anthropic
                   :api :anthropic-messages :context-window 200000
                   :max-output 64000 :thinking-mode :adaptive
                   :effort (:low :medium :high)))
         ;; Effort is the whole dial on some endpoints (Kimi Code's K3):
         ;; a thinking object there routes the request to a different
         ;; model.  This is also the default mode, so DEFAULTED below —
         ;; a plist without :thinking-mode at all — must behave the same.
         (effort-only '(:id "fixture-effort-only" :provider :anthropic
                        :api :anthropic-messages :context-window 200000
                        :max-output 64000
                        :thinking-mode :effort-only
                        :effort (:low :medium :high :xhigh :max)))
         (defaulted '(:id "fixture-defaulted" :provider :anthropic
                      :api :anthropic-messages :context-window 200000
                      :max-output 64000
                      :effort (:low :medium :high :xhigh :max)))
         (req (lambda (model level)
                (com.inuoe.jzon:parse
                 (build-request (find-api :anthropic-messages)
                                :model model :system nil :messages history
                                :tools nil :thinking-level level)))))
    (flet ((effort (model level)
             (let ((r (funcall req model level)))
               (evo.provider::jget r "output_config" "effort")))
           (thinking (model level)
             (let ((r (funcall req model level)))
               (gethash "thinking" r))))
      (check "effort: level maps straight through"
             (and (equal "max" (effort adaptive :max))
                  (equal "xhigh" (effort adaptive :xhigh))
                  (equal "low" (effort adaptive :low))))
      (check "effort: clamped to the strongest supported level"
             (and (equal "high" (effort capped :max))
                  (equal "high" (effort capped :xhigh))
                  (equal "medium" (effort capped :medium))))
      (check "effort: every rung maps, none is off"
             (every (lambda (l) (effort adaptive l)) +effort-levels+))
      (check "adaptive: mode and summaries instead of a budget"
             (let ((th (thinking adaptive :high)))
               (and (equal "adaptive" (evo.provider::jget th "type"))
                    (equal "summarized" (evo.provider::jget th "display"))
                    (null (gethash "budget_tokens" th)))))
      (check "effort-only: effort dial, no thinking object"
             (and (equal "xhigh" (effort effort-only :xhigh))
                  (null (thinking effort-only :xhigh))
                  (null (thinking effort-only :low))))
      (check "effort-only is the default mode"
             (and (equal "high" (effort defaulted :high))
                  (null (thinking defaulted :high)))))))

;;; Model registry: effort declarations are validated and canonicalized.

(defun test-model-effort-registration ()
  (reset-user-registries)
  (register-model* "eff-all" :provider :anthropic :api :anthropic-messages
                   :context-window 1000 :max-output 100 :effort t
                   :thinking-mode :adaptive)
  (register-model* "eff-some" :provider :anthropic :api :anthropic-messages
                   :context-window 1000 :max-output 100 :effort '(:max :low))
  (register-model* "eff-none" :provider :anthropic :api :anthropic-messages
                   :context-window 1000 :max-output 100)
  (check "registry: :effort t expands to the full ladder"
         (equal '(:low :medium :high :xhigh :max) (model-effort (find-model "eff-all"))))
  (check "registry: subset kept in ladder order"
         (equal '(:low :max) (model-effort (find-model "eff-some"))))
  (check "registry: no effort by default"
         (null (model-effort (find-model "eff-none"))))
  (check "registry: thinking mode recorded, effort-only by default"
         (and (eq :adaptive (model-thinking-mode (find-model "eff-all")))
              (eq :effort-only (model-thinking-mode (find-model "eff-none")))))
  (check-signals "registry: bogus effort level rejected"
                 (register-model* "eff-bad" :provider :anthropic
                                  :api :anthropic-messages :context-window 1000
                                  :max-output 100 :effort '(:huge)))
  (check "registry: :effort-only is a thinking mode"
         (progn (register-model* "eff-only-mode" :provider :anthropic
                                 :api :anthropic-messages :context-window 1000
                                 :max-output 100 :thinking-mode :effort-only)
                (eq :effort-only (model-thinking-mode (find-model "eff-only-mode")))))
  (check-signals "registry: bogus thinking mode rejected"
                 (register-model* "eff-bad-mode" :provider :anthropic
                                  :api :anthropic-messages :context-window 1000
                                  :max-output 100 :thinking-mode :sometimes))
  ;; budget_tokens is a retired knob of retired models; the mode that sent
  ;; it is gone with them.
  (check-signals "registry: retired :extended mode rejected"
                 (register-model* "eff-retired-mode" :provider :anthropic
                                  :api :anthropic-messages :context-window 1000
                                  :max-output 100 :thinking-mode :extended))
  (reset-user-registries)
  (register-fixture-models))

;;; The retired :off rung.  Thinking is no longer switchable off, but old
;;; journals and old init.lisp files still say :off -- they must resume on
;;; the weakest live rung, not crash and not silently drop the dial.

(defun test-retired-off-level ()
  (reset-settings)
  (check "normalize: live rungs pass through"
         (equal +effort-levels+ (mapcar #'normalize-thinking-level +effort-levels+)))
  (check "normalize: :off folds onto the weakest live rung"
         (eq :low (normalize-thinking-level :off)))
  (check "normalize: nonsense is NIL, so callers fall through to their default"
         (and (null (normalize-thinking-level :sideways))
              (null (normalize-thinking-level nil))))
  (let ((journal (make-session-journal "/tmp")))
    (check "effective-thinking: default without a choice"
           (eq :medium (evo.kernel:effective-thinking (fold-state journal))))
    (set-setting :thinking :off)
    (check "effective-thinking: a stale :off setting still yields a live rung"
           (eq :low (evo.kernel:effective-thinking (fold-state journal))))
    (set-setting :thinking :high)
    (check "effective-thinking: the setting is honoured"
           (eq :high (evo.kernel:effective-thinking (fold-state journal))))
    ;; An old session that had thinking switched off mid-run.
    (append-entry journal '(:type :thinking-change :thinking :off))
    (check "effective-thinking: a journaled :off resumes on the weakest rung"
           (eq :low (evo.kernel:effective-thinking (fold-state journal))))
    (append-entry journal '(:type :thinking-change :thinking :xhigh))
    (check "effective-thinking: journal outranks the setting"
           (eq :xhigh (evo.kernel:effective-thinking (fold-state journal))))
    (check "effective-thinking: the --thinking flag outranks the setting"
           (eq :max (evo.kernel:effective-thinking
                     (fold-state (make-session-journal "/tmp")) :max))))
  (reset-settings))

;;; Images: base64, sniffing, attaching, the clipboard, editor tokens, and
;;; the wire formats.  Fixtures are real (if tiny) files, because sniffing
;;; and sizing are the whole point of the read path.

(defparameter *png-1x1-hex*
  (cat "89504e470d0a1a0a0000000d4948445200000001000000010802000000907753de00"
       "00000c49444154789c63b82322020002d401055970d3c20000000049454e44ae426082")
  "A valid 1x1 red PNG, 69 bytes.")

(defun hex->octets (hex)
  (let ((hex (remove-if (lambda (c) (member c '(#\Space #\Newline #\Tab #\~))) hex)))
    (coerce (loop for i from 0 below (length hex) by 2
                  collect (parse-integer hex :start i :end (+ i 2) :radix 16))
            '(vector (unsigned-byte 8)))))

(defun image-fixture (&key (name "shot.png") (hex *png-1x1-hex*))
  (let ((path (format nil "~a/evo-img-~a-~a" (tmp-dir) (gen-id 6) name)))
    (write-file-octets path (hex->octets hex))
    (probe-file path)))

(defun string->octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun test-base64 ()
  (flet ((b64 (s) (octets->base64 (string->octets s))))
    ;; RFC 4648 §10 test vectors: padding is where hand-rolled encoders die.
    (check "base64 rfc4648 vectors"
           (and (equal (b64 "") "")
                (equal (b64 "f") "Zg==")
                (equal (b64 "fo") "Zm8=")
                (equal (b64 "foo") "Zm9v")
                (equal (b64 "foob") "Zm9vYg==")
                (equal (b64 "fooba") "Zm9vYmE=")
                (equal (b64 "foobar") "Zm9vYmFy"))))
  (let ((bytes (coerce (loop for i from 0 below 256 collect i)
                       '(vector (unsigned-byte 8)))))
    (check "base64 round-trips every byte value"
           (equalp (base64->octets (octets->base64 bytes)) bytes)))
  (check "base64 decode ignores whitespace"
         (equalp (base64->octets (format nil "Zm9v~%YmFy")) (string->octets "foobar")))
  (check-signals "base64 decode rejects junk" (base64->octets "Zm9v!!")))

(defun test-image-media-types ()
  (check "sniff png" (equal (evo.media:sniff-media-type (hex->octets *png-1x1-hex*))
                            "image/png"))
  (check "sniff jpeg" (equal (evo.media:sniff-media-type (hex->octets "ffd8ffe000104a464946"))
                             "image/jpeg"))
  (check "sniff gif" (equal (evo.media:sniff-media-type (string->octets "GIF89a....."))
                            "image/gif"))
  (check "sniff webp" (equal (evo.media:sniff-media-type (string->octets "RIFF\0\0\0\0WEBPVP8 "))
                             "image/webp"))
  (check "sniff rejects non-images"
         (null (evo.media:sniff-media-type (string->octets "not an image at all"))))
  ;; The extension is a claim; the bytes are the fact.
  (let ((liar (image-fixture :name "notes.txt")))
    (check "media type comes from the bytes, not the extension"
           (equal (evo.media:file-media-type liar) "image/png"))))

(defun test-attach-image ()
  (let ((fixture (image-fixture)))
    (multiple-value-bind (block reason) (evo.media:attach-image-file fixture)
      (check "attach: no reason on success" (null reason))
      (check "attach: builds an :image block" (evo.media:image-block-p block))
      (check "attach: carries media type and size"
             (and (equal (pget block :media-type) "image/png")
                  (= (pget block :bytes) 69)))
      (check "attach: data is the file, base64-encoded"
             (equalp (base64->octets (pget block :data)) (read-file-octets fixture)))
      (check "attach: names the block after the file"
             (equal (pget block :name) (file-namestring fixture)))
      (check "attach: summary reads like a file listing"
             (search "png" (evo.media:image-summary block))))
    ;; file:// URLs and ~ are what a terminal drop and a typed path look like.
    (check "attach: accepts a file:// url"
           (evo.media:attach-image-file (format nil "file://~a" (namestring fixture))))
    ;; Failures are values, never signals: the caller is a keystroke handler.
    (check "attach: missing file reports why"
           (multiple-value-bind (block reason)
               (evo.media:attach-image-file "/nonexistent/evo/shot.png")
             (and (null block) (search "no such file" reason))))
    (let ((text (format nil "~a/evo-not-an-image-~a.png" (tmp-dir) (gen-id 6))))
      (write-file-string text "just text")
      (check "attach: non-image reports the supported formats"
             (multiple-value-bind (block reason) (evo.media:attach-image-file text)
               (and (null block) (search "png, jpeg, gif or webp" reason)))))
    ;; Over the cap with nothing to downscale with: fail loudly, with advice.
    (let ((evo.media:*max-image-bytes* 10)
          (evo.media:*downscalers* nil))
      (check "attach: oversized without a downscaler explains itself"
             (multiple-value-bind (block reason) (evo.media:attach-image-file fixture)
               (and (null block)
                    (search "over the" reason)
                    (search "imagemagick" reason)))))
    ;; Over the cap with a downscaler that helps: the small result is sent.
    (let* ((small (image-fixture))
           (evo.media:*max-image-bytes* 100)
           (evo.media:*downscalers*
             ;; A portable stand-in for sips/imagemagick: a real external
             ;; program (cp, or a COPY .cmd on Windows) that produces the
             ;; small output, exercising the RUN-CHILD path for real.
             (list (list :keep-format (stub-copy-program)
                         (lambda (in out dim)
                           (declare (ignore in dim))
                           (list (namestring small) out))))))
      ;; Make the original bigger than the cap by padding it with a comment
      ;; chunk-sized tail; sniffing only looks at the head.
      (let ((padded (format nil "~a/evo-img-big-~a.png" (tmp-dir) (gen-id 6))))
        (write-file-octets padded
                           (concatenate '(vector (unsigned-byte 8))
                                        (hex->octets *png-1x1-hex*)
                                        (make-array 200 :element-type '(unsigned-byte 8)
                                                        :initial-element 0)))
        (multiple-value-bind (block reason) (evo.media:attach-image-file padded)
          (check "attach: oversized image is downscaled, not rejected"
                 (and (null reason) (= (pget block :bytes) 69))))))))

(defun test-clipboard-image ()
  (let ((fixture (image-fixture))
        (seen-dir nil))
    (let ((evo.media:*clipboard-readers*
            (list (cons "fake pasteboard"
                        (lambda (dir)
                          (setf seen-dir dir)
                          (let ((path (merge-pathnames "clip.png" dir)))
                            (write-file-octets path (read-file-octets fixture))
                            path))))))
      (multiple-value-bind (block reason) (evo.media:clipboard-image)
        (check "clipboard: grabs an image block" (and (null reason)
                                                      (evo.media:image-block-p block)))
        (check "clipboard: names an anonymous grab after the clipboard"
               (equal (pget block :name) "clipboard.png"))
        (check "clipboard: records where it came from"
               (equal (pget block :source) "clipboard"))))
    (check "clipboard: scratch directory is cleaned up"
           (and seen-dir (not (probe-file seen-dir))))
    ;; A reader may point at a file the user owns (a file-manager copy):
    ;; that file must survive the grab.
    (let ((evo.media:*clipboard-readers*
            (list (cons "fake file reference" (lambda (dir) (declare (ignore dir)) fixture)))))
      (multiple-value-bind (block reason) (evo.media:clipboard-image)
        (check "clipboard: file reference keeps its own name"
               (and (null reason) (equal (pget block :name) (file-namestring fixture))))
        (check "clipboard: file reference is not deleted" (probe-file fixture))))
    ;; The Linux tools (wl-paste, xclip) are asked one MIME type at a time,
    ;; and a file manager's copy offers no image type at all — only
    ;; text/uri-list naming the file.  Without that fallback the gesture
    ;; that works from Finder finds nothing from Nautilus/Dolphin.  `printf`
    ;; stands in for the clipboard tool: same contract (argv in, bytes on
    ;; stdout), no X server required.  Guarded off Windows, which has no
    ;; printf and reaches its clipboard through WINDOWS-CLIPBOARD-IMAGE (below)
    ;; rather than this stdout-tool path.
    #-evo-windows
    (let* ((dir (uiop:ensure-directory-pathname
                 (format nil "~a/evo-uri-~a/" (tmp-dir) (gen-id 6))))
           (text-file (format nil "~a/evo-notes-~a.txt" (tmp-dir) (gen-id 6))))
      (ensure-directories-exist dir)
      (write-file-string text-file "not an image")
      (flet ((clipboard-offering (&rest lines)
               ;; A tool that has no image bytes, only this uri-list.
               (lambda (type)
                 (if (equal type "text/uri-list")
                     (list (format nil "~{~a~%~}" lines))
                     (list "")))))
        (check "clipboard: a file-manager copy (text/uri-list) attaches the file"
               (equal (evo.media::stdout-clipboard-reader
                       "printf"
                       (clipboard-offering (format nil "file://~a" (namestring fixture)))
                       dir)
                      fixture))
        (check "clipboard: uri-list skips comments and non-images"
               (equal (evo.media::stdout-clipboard-reader
                       "printf"
                       (clipboard-offering "# comment"
                                           (format nil "file://~a" text-file)
                                           (format nil "file://~a" (namestring fixture)))
                       dir)
                      fixture))
        (check "clipboard: a uri-list with no image is still nothing"
               (null (evo.media::stdout-clipboard-reader
                      "printf" (clipboard-offering (format nil "file://~a" text-file)) dir)))
        (check "clipboard: the pointed-at file is never consumed" (probe-file fixture)))
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))
    ;; Native Windows: evo asks PowerShell for the clipboard and gets a native
    ;; path back — `image:<temp.png>` for pixels PowerShell saved (a
    ;; screenshot), `path:<file>` for a file copied in Explorer.  The stub
    ;; speaks that same one-line contract, so WINDOWS-CLIPBOARD-IMAGE's own
    ;; move/keep/miss logic is what is under test.
    #+evo-windows
    (let* ((scratch (uiop:ensure-directory-pathname
                     (format nil "~a/evo-winclip-~a/" (tmp-dir) (gen-id 6))))
           (shot (format nil "~a/evo-winshot-~a.png" (tmp-dir) (gen-id 6))))
      (ensure-directories-exist scratch)
      ;; Pixels PowerShell saved to a temp file: ours to move into the scratch
      ;; dir the caller sweeps, and to remove from temp afterwards.
      (write-file-octets shot (read-file-octets fixture))
      (let ((evo.media::*wsl-powershell-programs*
              (list (stub-emit-program (format nil "image:~a" shot)))))
        (let ((got (evo.media::windows-clipboard-image scratch)))
          (check "windows clipboard: saved pixels land in the scratch dir"
                 (and got (evo.media::image-file-p got) (uiop:subpathp got scratch)))
          (check "windows clipboard: the temp file is moved out, not left behind"
                 (not (probe-file shot)))))
      ;; A file copied in Explorer: only pointed at, so used in place and never
      ;; deleted.
      (write-file-octets shot (read-file-octets fixture))
      (let ((evo.media::*wsl-powershell-programs*
              (list (stub-emit-program (format nil "path:~a" shot)))))
        (let ((got (evo.media::windows-clipboard-image scratch)))
          (check "windows clipboard: an Explorer file copy is used in place"
                 (equal got (probe-file shot)))
          (check "windows clipboard: the user's own file survives" (probe-file shot))))
      ;; A path to nothing is nothing — no attachment, no error.
      (let ((evo.media::*wsl-powershell-programs*
              (list (stub-emit-program
                     (format nil "path:~a/evo-gone-~a.png" (tmp-dir) (gen-id 6))))))
        (check "windows clipboard: a path that is not there is nothing"
               (null (evo.media::windows-clipboard-image scratch))))
      (ignore-errors (delete-file shot))
      (ignore-errors (uiop:delete-directory-tree scratch :validate t
                                                         :if-does-not-exist :ignore)))
    ;; A tool that is not installed is not an error: that is how the macOS
    ;; readers behave on Linux and the Linux readers on macOS.  (If xclip
    ;; happens to be installed here there is nothing to prove.)
    (check "clipboard: a missing clipboard tool is silent"
           (or (and (evo.port:program-in-path "xclip") t)
               (null (evo.media::x11-clipboard-image (tmp-dir)))))
    (let ((evo.media:*clipboard-readers* nil))
      (check "clipboard: no reader says so"
             (multiple-value-bind (block reason) (evo.media:clipboard-image)
               (and (null block) (search "no clipboard reader" reason)))))
    (let ((evo.media:*clipboard-readers*
            (list (cons "empty" (lambda (dir) (declare (ignore dir)) nil)))))
      (check "clipboard: an empty grab always says why"
             (multiple-value-bind (block reason) (evo.media:clipboard-image)
               (and (null block) (stringp reason) (plusp (length reason))))))
    ;; "No image on the clipboard" is a lie when nothing in the session can
    ;; read a clipboard at all: the screenshot IS on the user's clipboard,
    ;; and blaming the clipboard sends them looking in the wrong place.
    ;; This is the whole table, so the message names the missing piece.
    (flet ((gap (&rest args) (apply #'evo.media::clipboard-gap args))
           (have (&rest names)
             (lambda (program) (member program names :test #'equal))))
      (check "clipboard reason: macOS with osascript is a real empty clipboard"
             (null (gap :macos-p t :installed-p (have "osascript"))))
      (check "clipboard reason: wayland with no tool at all names the package"
             (search "wl-clipboard" (gap :wayland-p t :installed-p (have))))
      (check "clipboard reason: wayland with wl-paste is a real empty clipboard"
             (null (gap :wayland-p t :installed-p (have "wl-paste"))))
      ;; A Wayland desktop with only xclip is served by XWayland, and WSLg
      ;; bridges the Windows clipboard into the Linux tools: neither is a gap.
      (check "clipboard reason: wayland with only xclip is served by XWayland"
             (null (gap :wayland-p t :installed-p (have "xclip"))))
      (check "clipboard reason: X11 without xclip names the tool"
             (search "xclip" (gap :x11-p t :installed-p (have "wl-paste"))))
      (check "clipboard reason: WSL with no tool at all says the bridge is missing"
             (search "powershell" (gap :wsl-p t :installed-p (have))))
      (check "clipboard reason: WSL with WSLg's linux tools is no gap"
             (null (gap :wsl-p t :installed-p (have "wl-paste"))))
      (check "clipboard reason: WSL with powershell is a real empty clipboard"
             (null (gap :wsl-p t :installed-p (have "powershell.exe"))))
      (check "clipboard reason: no display at all is an ssh/tty session"
             (search "ssh" (gap :installed-p (have "xclip" "wl-paste")))))
    ;; WSL: the session is Linux but the clipboard is Windows', so evo asks
    ;; PowerShell for it.  `printf`-style stubbing is impossible here (the
    ;; reader picks a program off *WSL-POWERSHELL-PROGRAMS*), so the stub is
    ;; a real script with the real contract: argv in, one line out.  The
    ;; drive letter maps onto *WSL-MOUNT-ROOT*, which wsl.conf can move —
    ;; pointing it at a temp tree is exactly what a custom automount does.
    (let* ((root (format nil "~a/evo-wsl-~a" (tmp-dir) (gen-id 6)))
           (win-dir (format nil "~a/c/Temp" root))
           (win-file (format nil "~a/shot.png" win-dir))
           (stub (format nil "~a/evo-fake-powershell-~a" (tmp-dir) (gen-id 6))))
      (ensure-directories-exist (uiop:ensure-directory-pathname win-dir))
      (flet ((seed-clipboard-file ()
               (write-file-octets win-file (read-file-octets fixture)))
             (fake-powershell (line)
               ;; A real stub with the real contract (argv in, one line out),
               ;; runnable on this platform: a .cmd on Windows, a chmod+x
               ;; /bin/sh script elsewhere.  The WSL logic under test is pure
               ;; Lisp (path mapping via a bound *WSL-MOUNT-ROOT*), so it is
               ;; worth exercising even when the host is Windows itself.
               (setf stub (stub-emit-program line))
               stub))
        (let ((evo.media::*wsl-session* t)
              (evo.media::*wsl-mount-root* root)
              (evo.media::*wsl-path-program* nil)) ; no wslpath here: the fallback rule
          (check "wsl: a Windows path maps onto the mount root"
                 (equal (evo.media::windows->wsl-path "C:\\Temp\\shot.png")
                        (format nil "~a/c/Temp/shot.png" root)))
          (check "wsl: forward slashes and a lowercase drive map too"
                 (equal (evo.media::windows->wsl-path "d:/Users/a/x.png")
                        (format nil "~a/d/Users/a/x.png" root)))
          (check "wsl: a UNC path has no drive to mount"
                 (null (evo.media::windows->wsl-path "\\\\server\\share\\x.png")))
          ;; Pixels: PowerShell wrote them to the Windows temp directory,
          ;; so they are ours — copied into the scratch dir the caller
          ;; sweeps, and the Windows-side copy removed.
          (seed-clipboard-file)
          (let* ((dir (uiop:ensure-directory-pathname
                       (format nil "~a/evo-wsl-scratch-~a/" (tmp-dir) (gen-id 6))))
                 (evo.media::*wsl-powershell-programs*
                   (list (fake-powershell "image:C:\\Temp\\shot.png"))))
            (ensure-directories-exist dir)
            (let ((got (evo.media::wsl-clipboard-image dir)))
              (check "wsl: clipboard pixels come back as an image"
                     (and got (evo.media::image-file-p got)))
              (check "wsl: pixels land in the scratch dir, not Windows temp"
                     (and got (uiop:subpathp got dir)))
              (check "wsl: the Windows temp file is not left behind"
                     (not (probe-file win-file))))
            (ignore-errors (uiop:delete-directory-tree dir :validate t
                                                           :if-does-not-exist :ignore)))
          ;; A file copied in Explorer: the clipboard only points at it, so
          ;; it is used where it lies and never deleted.
          (seed-clipboard-file)
          (let* ((dir (uiop:ensure-directory-pathname
                       (format nil "~a/evo-wsl-scratch-~a/" (tmp-dir) (gen-id 6))))
                 (evo.media::*wsl-powershell-programs*
                   (list (fake-powershell "path:C:\\Temp\\shot.png"))))
            (ensure-directories-exist dir)
            (let ((got (evo.media::wsl-clipboard-image dir)))
              (check "wsl: an Explorer file copy attaches the file itself"
                     (equal got (probe-file win-file)))
              (check "wsl: the user's own file survives" (probe-file win-file)))
            (ignore-errors (uiop:delete-directory-tree dir :validate t
                                                           :if-does-not-exist :ignore)))
          ;; Nothing on the clipboard, and nothing pretending otherwise.
          (let ((evo.media::*wsl-powershell-programs*
                  (list (fake-powershell "path:C:\\Temp\\gone.png"))))
            (check "wsl: a path that is not there is not an attachment"
                   (null (evo.media::wsl-clipboard-image (tmp-dir)))))
          (let ((evo.media::*wsl-powershell-programs* (list "evo-no-such-powershell")))
            (check "wsl: no PowerShell on PATH is silent"
                   (null (evo.media::wsl-clipboard-image (tmp-dir))))))
        ;; Off WSL the reader must not even shell out.
        (let ((evo.media::*wsl-session* nil)
              (evo.media::*wsl-powershell-programs*
                (list (fake-powershell "image:C:\\Temp\\shot.png"))))
          (check "wsl: the reader stays out of the way elsewhere"
                 (null (evo.media::wsl-clipboard-image (tmp-dir)))))
        (ignore-errors (delete-file stub))
        (ignore-errors (uiop:delete-directory-tree
                        (uiop:ensure-directory-pathname root)
                        :validate t :if-does-not-exist :ignore))))))

(defun test-pasted-image-paths ()
  (let ((fixture (image-fixture))
        (spaced (image-fixture :name "a shot.png"))
        (text (format nil "~a/evo-plain-~a.txt" (tmp-dir) (gen-id 6))))
    (write-file-string text "hello")
    (check "paste: a bare image path attaches"
           (equal (evo.media:pasted-image-paths (namestring fixture)) (list fixture)))
    (check "paste: a quoted path with spaces attaches"
           (equal (evo.media:pasted-image-paths (format nil "'~a'" (namestring spaced)))
                  (list spaced)))
    ;; POSIX backslash-escaping of a space is a Unix convention; on Windows
    ;; the backslash is a path separator, so the native gesture is a
    ;; backslash-spelled path (an Explorer drag), which the Windows tokenizer
    ;; keeps intact where the POSIX splitter would eat it.
    #-evo-windows
    (check "paste: a backslash-escaped path attaches"
           (equal (evo.media:pasted-image-paths
                   (string-replace " " "\\ " (namestring spaced) :all t))
                  (list spaced)))
    #+evo-windows
    (check "paste: a native backslash Windows path attaches"
           (equal (evo.media:pasted-image-paths
                   (substitute #\\ #\/ (namestring fixture)))
                  (list fixture)))
    ;; Two space-free paths, one per line: the multi-token pass keeps the
    ;; backslashes and resolves each.  (A path with a space would have to be
    ;; quoted; that is the single-path case above.)
    #+evo-windows
    (let ((other (image-fixture)))
      (check "paste: several native backslash paths attach in order"
             (equal (evo.media:pasted-image-paths
                     (format nil "~a~%~a"
                             (substitute #\\ #\/ (namestring fixture))
                             (substitute #\\ #\/ (namestring other))))
                    (list fixture other))))
    #+evo-windows
    (check "paste: a native Windows path to nothing stays text"
           (null (evo.media:pasted-image-paths "C:\\evo-nope\\gone.png")))
    (check "paste: a file:// url attaches"
           (equal (evo.media:pasted-image-paths (format nil "file://~a" (namestring fixture)))
                  (list fixture)))
    (check "paste: several dropped files attach in order"
           (equal (evo.media:pasted-image-paths
                   (format nil "~a~%~a" (namestring fixture) (namestring fixture)))
                  (list fixture fixture)))
    ;; All-or-nothing: prose that merely mentions an image is prose.
    (check "paste: a sentence mentioning a path stays text"
           (null (evo.media:pasted-image-paths
                  (format nil "look at ~a please" (namestring fixture)))))
    (check "paste: a non-image path stays text"
           (null (evo.media:pasted-image-paths (namestring text))))
    (check "paste: ordinary text stays text"
           (null (evo.media:pasted-image-paths "just a sentence")))
    ;; WSL: Explorer and Windows Terminal hand out Windows spelling for a
    ;; filesystem evo sees mounted elsewhere, and POSIX quoting rules would
    ;; eat the backslashes (`C:\Users\a\x.png` -> `C:Usersax.png`) long
    ;; before anything got to look for the file.  Guarded off native Windows,
    ;; where WINDOWS-P shadows the WSL mount mapping in TOKEN->PATH (a Windows
    ;; path there names a real local file, not a mounted one) — the native
    ;; backslash cases are covered above instead.
    #-evo-windows
    (let* ((root (format nil "~a/evo-wslpaste-~a" (tmp-dir) (gen-id 6)))
           (win-dir (format nil "~a/c/Temp" root))
           (win-file (format nil "~a/shot.png" win-dir)))
      (ensure-directories-exist (uiop:ensure-directory-pathname win-dir))
      (write-file-octets win-file (read-file-octets fixture))
      (let ((evo.media::*wsl-mount-root* root)
            (evo.media::*wsl-path-program* nil))
        (let ((evo.media::*wsl-session* t))
          (check "paste: a dropped Windows path attaches under WSL"
                 (equal (evo.media:pasted-image-paths "C:\\Temp\\shot.png")
                        (list (probe-file win-file))))
          (check "paste: a quoted Windows path attaches too"
                 (equal (evo.media:pasted-image-paths "\"C:\\Temp\\shot.png\"")
                        (list (probe-file win-file))))
          (check "paste: a Windows path to nothing stays text"
                 (null (evo.media:pasted-image-paths "C:\\Temp\\gone.png"))))
        (let ((evo.media::*wsl-session* nil))
          (check "paste: a Windows path is just text off WSL"
                 (null (evo.media:pasted-image-paths "C:\\Temp\\shot.png")))))
      (ignore-errors (uiop:delete-directory-tree (uiop:ensure-directory-pathname root)
                                                 :validate t :if-does-not-exist :ignore))))
  (check "shell tokens: quotes and escapes"
         (equal (evo.media:split-shell-tokens "/tmp/a\\ b.png '/tmp/c d.png' \"/e f.png\"")
                (list "/tmp/a b.png" "/tmp/c d.png" "/e f.png"))))

(defun test-editor-attachments ()
  (let ((eb (evo.tui::make-edit-buffer))
        (one (evo.media:make-image-block :data "AA==" :media-type "image/png" :name "one.png"))
        (two (evo.media:make-image-block :data "BB==" :media-type "image/png" :name "two.png")))
    (evo.tui::eb-attach-image eb one)
    (evo.tui::eb-insert-text eb "what is this?")
    (evo.tui::eb-attach-image eb two)
    (check "editor: attachments show as tokens"
           (equal (evo.tui::eb-text eb) "[Image #1] what is this? [Image #2] "))
    (check "editor: submit yields both images"
           (equal (evo.tui::eb-submit-images eb) (list one two)))
    ;; The token is the handle: delete it and the image is not sent.
    (evo.tui::eb-set-text eb "only the second: [Image #2]")
    (check "editor: deleting a token drops its image"
           (equal (evo.tui::eb-submit-images eb) (list two)))
    ;; Order follows the text, not the order they were attached.
    (evo.tui::eb-set-text eb "[Image #2] then [Image #1]")
    (check "editor: images are ordered as they appear"
           (equal (evo.tui::eb-submit-images eb) (list two one)))
    (evo.tui::eb-clear eb)
    (check "editor: clearing the buffer drops attachments"
           (and (null (evo.tui::eb-attachments eb))
                (null (evo.tui::eb-submit-images eb))))))

(defun test-image-steering ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-imgsteer-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (fixture (image-fixture))
         (block (evo.media:attach-image-file fixture)))
    (queue-steering agent "what is in this screenshot?" :images (list block))
    (evo.kernel::drain-steering agent)
    (let* ((message (first (state-messages (fold-state journal))))
           (content (message-content message)))
      (check "steering: one user message carries text and image"
             (and (eq (pget message :role) :user) (= 2 (length content))))
      (check "steering: the image comes before the question"
             (and (eq (pget (first content) :type) :image)
                  (eq (pget (second content) :type) :text)))
      (check "steering: the image survives the journal round-trip"
             (equalp (base64->octets (pget (first content) :data))
                     (read-file-octets fixture))))
    ;; Text-only steering keeps its one-text-block shape.
    (queue-steering agent "plain")
    (evo.kernel::drain-steering agent)
    (check "steering: text-only messages are unchanged"
           (equal (message-content (car (last (state-messages (fold-state journal)))))
                  '((:type :text :text "plain"))))))

(defun test-image-wire ()
  (let* ((image (evo.media:make-image-block :data "QUJD" :media-type "image/png"
                                            :name "shot.png" :bytes 3))
         (history (list (list :role :user
                              :content (list image (list :type :text :text "what is this?")))))
         (seeing '(:id "fixture-vision" :provider :anthropic :api :anthropic-messages
                   :context-window 200000 :max-output 64000 :vision t))
         (blind (pput seeing :vision nil)))
    ;; Anthropic: a base64 source block.  No thinking level: these assert the
    ;; shape of the image block, and a request with no dial asked for is the
    ;; smallest one that shows it.
    (let* ((req (com.inuoe.jzon:parse
                 (build-request (find-api :anthropic-messages)
                                :model seeing :system "sys" :messages history
                                :tools nil :thinking-level nil)))
           (block (aref (evo.provider::jget (aref (evo.provider::jget req "messages") 0)
                                            "content")
                        0)))
      (check "anthropic: image goes as a base64 source"
             (and (equal (evo.provider::jget block "type") "image")
                  (equal (evo.provider::jget block "source" "type") "base64")
                  (equal (evo.provider::jget block "source" "media_type") "image/png")
                  (equal (evo.provider::jget block "source" "data") "QUJD"))))
    ;; A model without vision must not poison every later request with a 400.
    (let* ((req (com.inuoe.jzon:parse
                 (build-request (find-api :anthropic-messages)
                                :model blind :system "sys" :messages history
                                :tools nil :thinking-level nil)))
           (block (aref (evo.provider::jget (aref (evo.provider::jget req "messages") 0)
                                            "content")
                        0)))
      (check "anthropic: no vision degrades the image to text"
             (and (equal (evo.provider::jget block "type") "text")
                  (search "image not shown" (evo.provider::jget block "text"))
                  (search "shot.png" (evo.provider::jget block "text")))))
    ;; Request-size guard: keep the latest image and omit older image payloads
    ;; before the provider rejects the whole HTTP request as too large.
    (let* ((evo.provider::*max-request-image-data-chars* 7)
           (old (evo.media:make-image-block :data "OLDDATA" :media-type "image/png"
                                            :name "old.png" :bytes 7))
           (new (evo.media:make-image-block :data "NEWDATA" :media-type "image/png"
                                            :name "new.png" :bytes 7))
           (limited-history (list (list :role :user :content (list old))
                                  (list :role :user :content (list new))))
           (req (com.inuoe.jzon:stringify
                 (com.inuoe.jzon:parse
                  (build-request (find-api :anthropic-messages)
                                 :model seeing :system "sys"
                                 :messages limited-history :tools nil
                                 :thinking-level nil)))))
      (check "handoff: request-size guard keeps the newest image"
             (and (search "NEWDATA" req)
                  (not (search "OLDDATA" req))
                  (search "old.png" req)
                  (search "request size" req))))
    ;; Anthropic cache breakpoints should stop before image-bearing messages:
    ;; caching does not make the HTTP body smaller, and cached screenshot bytes
    ;; make the cache prefix unstable and expensive.
    (let* ((req (com.inuoe.jzon:parse
                 (build-request (find-api :anthropic-messages)
                                :model seeing :system "sys"
                                :messages (list (list :role :user
                                                       :content (list (list :type :text :text "before")
                                                                      image)))
                                :tools nil :thinking-level nil)))
           (content (evo.provider::jget (aref (evo.provider::jget req "messages") 0)
                                        "content"))
           (before (aref content 0))
           (image-json (aref content 1)))
      (check "anthropic: cache breakpoint stays before image suffix"
             (and (evo.provider::jget before "cache_control")
                  (null (evo.provider::jget image-json "cache_control")))))
    ;; The registry default: a model registered without the flag can see.
    (check "registry: vision defaults on, and :vision nil is honoured"
           (and (model-vision-p '(:id "x"))
                (model-vision-p (progn (register-model*
                                        "fixture-sees" :provider :anthropic
                                        :api :anthropic-messages :context-window 1000
                                        :max-output 100)
                                       (find-model "fixture-sees")))
                (not (model-vision-p (progn (register-model*
                                             "fixture-blind" :provider :anthropic
                                             :api :anthropic-messages :context-window 1000
                                             :max-output 100 :vision nil)
                                            (find-model "fixture-blind"))))))
    ;; Compaction accounting knows an image is not free.
    (check "compaction estimates images as a flat cost"
           (> (evo.kernel::estimate-message-tokens (first history)) 4000))
    (reset-user-registries)
    (register-fixture-models)))

(defun test-image-tui-paste ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-imgtui-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent))
         (fixture (image-fixture))
         (eb (evo.tui::tui-editor tui)))
    (with-output-to-string (fake-tty)
      (let ((evo.tui::*tty-out* fake-tty)
            (evo.tui::*region-height* 0)
            (evo.tui::*region-cursor-row* 0))
        ;; Dropping a file onto the terminal arrives as a paste of its path.
        (evo.tui::handle-paste tui (namestring fixture))
        (check "tui: pasting an image path attaches it"
               (and (= 1 (length (evo.tui::eb-attachments eb)))
                    (search "[Image #1]" (evo.tui::eb-text eb))))
        ;; Ordinary text still pastes as text.
        (evo.tui::handle-paste tui "just words")
        (check "tui: pasting text still types text"
               (and (= 1 (length (evo.tui::eb-attachments eb)))
                    (search "just words" (evo.tui::eb-text eb))))
        ;; ctrl+v with a clipboard that has an image.
        (let ((evo.media:*clipboard-readers*
                (list (cons "fake" (lambda (d)
                                     (let ((p (merge-pathnames "c.png" d)))
                                       (write-file-octets p (read-file-octets fixture))
                                       p))))))
          (evo.tui::handle-key-edit tui (list :ctrl #\v)))
        (check "tui: ctrl+v attaches the clipboard image"
               (= 2 (length (evo.tui::eb-attachments eb))))
        ;; cmd+v and right-click -> Paste: the terminal reads the clipboard
        ;; as text, finds none (it holds a screenshot) and pastes the empty
        ;; string.  That empty paste is the only trace of the gesture, so it
        ;; has to mean "look at the clipboard yourself".
        (let ((evo.media:*clipboard-readers*
                (list (cons "fake" (lambda (d)
                                     (let ((p (merge-pathnames "c2.png" d)))
                                       (write-file-octets p (read-file-octets fixture))
                                       p))))))
          (evo.tui::handle-paste tui ""))
        (check "tui: an empty paste attaches the clipboard image"
               (= 3 (length (evo.tui::eb-attachments eb))))
        ;; cmd+v reported as a real key (kitty super) is the same gesture.
        (let ((evo.media:*clipboard-readers*
                (list (cons "fake" (lambda (d)
                                     (let ((p (merge-pathnames "c3.png" d)))
                                       (write-file-octets p (read-file-octets fixture))
                                       p))))))
          (evo.tui::handle-key-edit tui (list :super #\v)))
        (check "tui: cmd+v attaches the clipboard image"
               (= 4 (length (evo.tui::eb-attachments eb))))
        ;; An empty paste with nothing on the clipboard is a message, not a
        ;; crash and not a stray character in the buffer.
        (let ((evo.media:*clipboard-readers* nil)
              (before (evo.tui::eb-text eb)))
          (evo.tui::handle-paste tui "")
          (check "tui: an empty paste with an empty clipboard changes nothing"
                 (and (= 4 (length (evo.tui::eb-attachments eb)))
                      (equal before (evo.tui::eb-text eb)))))
        ;; /image <path> is the same attach by another door.
        (evo.tui::image-command tui (namestring fixture))
        (check "tui: /image attaches by path"
               (= 5 (length (evo.tui::eb-attachments eb))))
        (check "tui: /image reports a bad path instead of erroring"
               (progn (evo.tui::image-command tui "/nonexistent/evo/x.png")
                      (= 5 (length (evo.tui::eb-attachments eb)))))))
    ;; The prompt block in scrollback lists what was actually sent.
    (let* ((evo.tui::*cols* 60)
           (block (evo.media:attach-image-file fixture))
           (rendered (evo.tui::user-prompt-block "[Image #1] what is this?" (list block))))
      (check "tui: the prompt block lists attached images"
             (and (search "Image #1" rendered)
                  (search (file-namestring fixture) rendered))))))

(defun test-image-read-tool ()
  "The read tool on an image: the picture comes back, not line noise."
  (let* ((fixture (image-fixture))
         (data (octets->base64 (read-file-octets fixture))))
    (register-model* "reads-images" :provider :anthropic :api :anthropic-messages
                     :context-window 200000 :max-output 8192)
    (register-model* "reads-nothing" :provider :anthropic :api :anthropic-messages
                     :context-window 200000 :max-output 8192 :vision nil)
    ;; A tool may answer with blocks instead of a string.
    (check "tool content: a string is one text block"
           (equal (evo.kernel::tool-content-blocks "hi")
                  '((:type :text :text "hi"))))
    (check "tool content: a lone block is wrapped"
           (equal (evo.kernel::tool-content-blocks '(:type :text :text "hi"))
                  '((:type :text :text "hi"))))
    (check "tool content: a list of blocks passes through"
           (= 2 (length (evo.kernel::tool-content-blocks
                         (list '(:type :text :text "hi")
                               (evo.media:make-image-block :data "QUJD"))))))
    ;; A userspace tool is agent-written: a block no adapter could send is
    ;; stringified here, not at request-build time where it would fail the
    ;; whole turn and name the adapter instead of the tool.
    (check "tool content: an unknown block becomes text"
           (let ((blocks (evo.kernel::tool-content-blocks '(:type :bogus :x 1))))
             (and (= 1 (length blocks))
                  (eq (pget (first blocks) :type) :text)
                  (search "BOGUS" (pget (first blocks) :text)))))
    (check "tool content: a nil inside a list is dropped"
           (= 1 (length (evo.kernel::tool-content-blocks
                         (list nil '(:type :text :text "hi"))))))
    ;; Truncation is a text budget; an image is not half-sendable.
    (let* ((evo.kernel::*max-tool-result-chars* 10)
           (blocks (evo.kernel::truncate-result-blocks
                    (list (list :type :text :text (make-string 500 :initial-element #\a))
                          (evo.media:make-image-block :data data :media-type "image/png"
                                                      :name "shot.png" :bytes 69)))))
      (check "tool result: text is truncated to the budget"
             (< (length (pget (first blocks) :text)) 200))
      (check "tool result: the image survives whole"
             (equal (pget (second blocks) :data) data)))
    (check "tool result: the host display names the image"
           (search "shot.png"
                   (evo.kernel::result-display-text
                    (list (evo.media:make-image-block :data data :media-type "image/png"
                                                      :name "shot.png" :bytes 69)))))
    (check "tool result: an image is priced into the context estimate"
           (> (evo.kernel::result-context-chars
               (list (evo.media:make-image-block :data data :bytes 69)))
              4000))
    ;; The tool call, end to end through the journal.
    (let* ((dir (uiop:ensure-directory-pathname
                 (format nil "~a/evo-imgread-~a/" (tmp-dir) (gen-id))))
           (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
           (events nil)
           (agent (make-agent :journal journal :model-override "reads-images"
                              :events-cb (lambda (ev) (push ev events)))))
      (evo.kernel::run-tool-call agent (list :name "read" :id "call_img"
                                             :arguments (list :path (namestring fixture))))
      (let* ((result (find :tool-result (reverse (state-messages (fold-state journal)))
                           :key #'message-role))
             (content (message-content result))
             (image (find :image content :key (lambda (b) (pget b :type)))))
        (check "read: an image file comes back as an image block"
               (and image (equal (pget image :data) data)
                    (equal (pget image :media-type) "image/png")))
        (check "read: the image is captioned, not silent"
               (search (file-namestring fixture)
                       (or (pget (first content) :text) "")))
        (check "read: reading an image is not an error"
               (not (pget result :is-error)))
        (check "read: the event prices the image into the context estimate"
               (> (pget (find :tool-result events :key (lambda (e) (pget e :type)))
                        :content-chars)
                  4000))
        ;; Anthropic sends it inside the tool_result; a blind model gets text.
        (let* ((history (list (list :role :assistant :model "reads-images"
                                    :content (list (list :type :tool-call :id "call_img"
                                                         :name "read"
                                                         :arguments (list :path "x.png"))))
                              result))
               (json (com.inuoe.jzon:stringify
                      (com.inuoe.jzon:parse
                       (build-request (find-api :anthropic-messages)
                                      :model (find-model "reads-images") :system "sys"
                                      :messages history :tools nil :thinking-level nil))))
               (blind (com.inuoe.jzon:stringify
                       (com.inuoe.jzon:parse
                        (build-request (find-api :anthropic-messages)
                                       :model (find-model "reads-nothing") :system "sys"
                                       :messages history :tools nil :thinking-level nil)))))
          (check "anthropic: the tool result carries the image"
                 (and (search "tool_result" json) (search data json)))
          (check "anthropic: a blind model gets a placeholder, not base64"
                 (and (not (search data blind)) (search "image not shown" blind)))))
      ;; A model that cannot see gets the truth instead of a megabyte of base64.
      (let* ((dir (uiop:ensure-directory-pathname
                   (format nil "~a/evo-imgblind-~a/" (tmp-dir) (gen-id))))
             (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
             (agent (make-agent :journal journal :model-override "reads-nothing")))
        (evo.kernel::run-tool-call agent (list :name "read" :id "call_blind"
                                               :arguments (list :path (namestring fixture))))
        (let ((result (find :tool-result (reverse (state-messages (fold-state journal)))
                            :key #'message-role)))
          (check "read: a blind model is refused, and sent no image"
                 (and (pget result :is-error)
                      (null (find :image (message-content result)
                                  :key (lambda (b) (pget b :type))))
                      (search "vision" (pget (first (message-content result)) :text)))))))
    ;; Text files are untouched by any of this.
    (let ((path (format nil "~a/evo-imgread-~a.txt" (tmp-dir) (gen-id 6))))
      (write-file-string path (format nil "alpha~%beta~%"))
      (check "read: a text file still reads as numbered lines"
             (let ((out (evo.kernel::tool-read (list :path path))))
               (and (stringp out) (search "alpha" out) (search "2" out)))))
    ;; And the agent is told which of the two worlds it is in.
    (check "prompt: a vision model is told it can see images"
           (search "Can see images: yes"
                   (build-system-prompt (list (find-tool "read")) :vision t)))
    (check "prompt: a blind model is told it cannot"
           (search "Can see images: no"
                   (build-system-prompt (list (find-tool "read")) :vision nil)))
    (check "prompt: the tool list says read takes images"
           (search "image" (build-system-prompt (list (find-tool "read")))))
    (reset-user-registries)
    (register-fixture-models)))

(defun test-image-export ()
  (let* ((dir (format nil "~a/evo-imgexp-~a" (tmp-dir) (gen-id 6)))
         (md (format nil "~a/transcript.md" dir))
         (fixture (image-fixture))
         (block (evo.media:attach-image-file fixture)))
    (ensure-directories-exist (uiop:ensure-directory-pathname dir))
    (let ((name (evo.tui::export-image block md 1)))
      (check "export: writes the image beside the markdown"
             (and (equal name "transcript-img1.png")
                  (equalp (read-file-octets (format nil "~a/~a" dir name))
                          (read-file-octets fixture)))))))

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
     "(evo:register-model \"init-b\" :provider :proxy-co
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

;;; Extension load order.  The file name is the entire ordering mechanism,
;;; hence the NNN- rank convention: these checks are what make it canonical
;;; rather than folklore.

(defun ranked-extension-name-p (name)
  (and (> (length name) 4)
       (every #'digit-char-p (subseq name 0 3))
       (char= (char name 3) #\-)))

(defun test-extension-load-order ()
  (let ((dir (merge-pathnames (format nil "ext-order-~a/" (gen-id))
                              (uiop:ensure-directory-pathname (tmp-dir)))))
    (ensure-directory dir)
    (unwind-protect
         (progn
           (dolist (name '("900-wrapper.lisp" "020-provider.lisp"
                           "100-tool.lisp" "unranked.lisp"))
             (write-file-string (merge-pathnames name dir) ";; probe"))
           (check "extensions load in file-name rank order"
                  (equal '("020-provider.lisp" "100-tool.lisp"
                           "900-wrapper.lisp" "unranked.lisp")
                         (mapcar #'file-namestring
                                 (evo.kernel::extension-files dir)))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))
  ;; Hooks fire in registration order, so load order orders them too — a
  ;; push would invert the ranks and make the numbering lie.
  (let ((evo.kernel::*event-hooks* (make-hash-table)))
    (evo.kernel:add-hook :order-probe (lambda (p) (declare (ignore p)) :first))
    (evo.kernel:add-hook :order-probe (lambda (p) (declare (ignore p)) :second))
    (check "hooks run in registration order"
           (equal '(:first :second) (evo.kernel:run-hooks :order-probe nil))))
  ;; Everything this repo ships carries a rank.
  (let ((vendored (append (directory (merge-pathnames "extensions/*.lisp"
                                                      (uiop:getcwd)))
                          (directory (merge-pathnames "extensions/examples/*.lisp"
                                                      (uiop:getcwd))))))
    (when vendored
      (check "vendored extensions are all ranked NNN-name.lisp"
             (every #'ranked-extension-name-p
                    (mapcar #'file-namestring vendored))))))

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
    (set-setting :model "claude-sonnet-5")
    (check "preflight passes with configured model"
           (progn (evo.cli::preflight-model agent journal nil) t))
    (set-setting :model "gone-model")
    (check-signals "preflight: unknown model id"
                   (evo.cli::preflight-model agent journal nil)))
  (reset-settings)
  (reset-user-registries)
  (register-fixture-models))

(defun test-proxy-plumbing ()
  "WITH-PROXY resolves the proxy once and publishes it for the WinHTTP shim,
which is the only way dexador's Windows backend can be made to use one: it
ignores :proxy and tells WinHTTP explicitly to go direct (see
ENSURE-WINHTTP-PROXY)."
  (let ((saved-http (getenv "http_proxy"))
        (saved-https (getenv "https_proxy"))
        (saved-no (getenv "no_proxy")))
    (unwind-protect
         (progn
           (evo.port:setenv "https_proxy" "http://127.0.0.1:10808")
           (evo.port:setenv "http_proxy" "")
           (evo.port:setenv "no_proxy" "internal.example")
           (with-proxy (proxy "https://api.anthropic.com/v1/messages")
             (check "with-proxy resolves the environment's proxy"
                    (equal proxy "http://127.0.0.1:10808"))
             (check "and publishes it where the shim reads it"
                    (equal *request-proxy* "http://127.0.0.1:10808")))
           (with-proxy (proxy "https://internal.example/v1")
             (check "no_proxy still wins" (null proxy))
             (check "and nothing is published for it" (null *request-proxy*)))
           (check "outside the form nothing is left behind" (null *request-proxy*))
           (check "the shim is a no-op off Windows"
                  (or (evo.port:windows-p) (progn (ensure-winhttp-proxy) t))))
      (evo.port:setenv "https_proxy" (or saved-https ""))
      (evo.port:setenv "http_proxy" (or saved-http ""))
      (evo.port:setenv "no_proxy" (or saved-no "")))))

(defun test-restart-and-resume ()
  "The supervisor's restart guess, and what a child does when the guess is
wrong.  Windows found this the hard way: one crash in the TUI turned into
five identical restarts, each reporting a different error than the real one."
  ;; The TUI's own streams: on Unix these stay plain descriptors 0 and 1.
  ;; (On Windows STD-DESCRIPTOR returns a GetStdHandle handle instead — a
  ;; literal 1 there is an invalid handle, not stdout.)
  #-evo-windows
  (progn
    (check "stdin is descriptor 0" (eql 0 (evo.port:std-descriptor :stdin)))
    (check "stdout is descriptor 1" (eql 1 (evo.port:std-descriptor :stdout))))
  (let ((saved (getenv "EVO_HOME"))
        (home (format nil "~a/evo-resume-~a/" (tmp-dir) (gen-id))))
    (unwind-protect
         (progn
           (evo.port:setenv "EVO_HOME" home)
           (ensure-directories-exist (sessions-directory))
           (check "nothing to resume: restart fresh, no --resume"
                  (null (evo.cli::restart-argv nil)))
           (check "--events survives a restart"
                  (equal '("--events") (evo.cli::restart-argv '("--events"))))
           (check "resume with nothing to resume is a usage error, not a crash"
                  (handler-case (progn (evo.cli::resolve-journal '(:resume :latest)) nil)
                    (evo.cli::usage-error () t)
                    (error () nil)))
           ;; A session reaches disk only once the model has answered — which
           ;; is exactly why an early crash leaves nothing to resume.
           (append-entry (make-session-journal)
                         '(:type :message :message (:role :assistant :content "hi")))
           (check "a session on disk: restart resumes it"
                  (equal '("--resume") (evo.cli::restart-argv nil)))
           (check "and --events still rides along"
                  (equal '("--resume" "--events") (evo.cli::restart-argv '("--events")))))
      (evo.port:setenv "EVO_HOME" (or saved "")))))

(defun test-parse-args ()
  (check "parse: thinking level keyword"
         (eq :high (getf (evo.cli::parse-args '("--thinking" "high")) :thinking)))
  (check-signals "parse: bogus thinking level"
                 (evo.cli::parse-args '("--thinking" "bogus")))
  (check-signals "parse: unknown flag" (evo.cli::parse-args '("--wat"))))

;;; Concurrency and ownership.
;;;
;;; These are the invariants the whole threading design rests on, so they are
;;; asserted rather than trusted: one owner per mutable object, no resource
;;; freed by a thread that does not own it, no task forgotten before it exits,
;;; no runtime generation observed half-built.

(defclass cancel-fixture-api (provider-api) ())
(defvar *cancel-fixture-entered* nil)
(defvar *cancel-fixture-exited* nil)
(defvar *cancel-fixture-closed-by* nil)

(defmethod endpoint-path ((api cancel-fixture-api)) "/fixture/cancel")
(defmethod auth-headers ((api cancel-fixture-api) config) (declare (ignore config)) nil)
(defmethod build-request ((api cancel-fixture-api) &key model system messages tools
                                                        thinking-level)
  (declare (ignore model system messages tools thinking-level))
  "{}")

;; A stream whose CLOSE records which thread closed it: that is the ownership
;; claim under test, not merely "it was closed".  Binary, because the transport
;; wraps the response in a flexi-stream, which reads bytes.
(defclass recording-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((open-p :initform t :accessor recording-stream-open-p)))

(defmethod stream-element-type ((s recording-stream)) '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((s recording-stream))
  ;; Never returns on its own: only cancellation ends this read.
  (loop (sleep 0.01)))

(defmethod close ((s recording-stream) &key abort)
  (declare (ignore abort))
  (setf *cancel-fixture-closed-by* (bt:current-thread)
        (recording-stream-open-p s) nil)
  t)

(defmethod parse-stream ((api cancel-fixture-api) char-stream &key on-event abort-flag)
  (declare (ignore on-event abort-flag))
  (setf *cancel-fixture-entered* t)
  (unwind-protect
       (read-char char-stream)          ; blocks until the owner is interrupted
    (setf *cancel-fixture-exited* t)))

(defun test-provider-request-ownership ()
  "Cancelling a request must stop it, close its stream ON THE OWNER THREAD, and
leave no thread behind.  The pre-fix transport closed the stream from the
caller's thread and skipped the join, so a cancelled request kept running."
  (let ((saved-post (symbol-function 'dex:post))
        (stream (make-instance 'recording-stream))
        (caller (bt:current-thread)))
    (unwind-protect
         (progn
           (setf *cancel-fixture-entered* nil
                 *cancel-fixture-exited* nil
                 *cancel-fixture-closed-by* nil)
           (setf (symbol-function 'dex:post)
                 (lambda (&rest args) (declare (ignore args)) stream))
           (let* ((api (make-instance 'cancel-fixture-api))
                  (started (get-internal-real-time))
                  (result (perform-request
                           api "https://fixture.invalid/v1" nil "{}"
                           :abort-flag
                           (lambda ()
                             ;; Abort once the request is genuinely streaming.
                             (and *cancel-fixture-entered*
                                  (> (/ (- (get-internal-real-time) started)
                                        internal-time-units-per-second)
                                     0.05)))))
                  (elapsed (/ (- (get-internal-real-time) started)
                              internal-time-units-per-second)))
             (check "cancelled request reports an aborted result"
                    (pget result :aborted-p))
             (check "cancellation does not wait out the stream" (< elapsed 3))
             ;; The join happens inside perform-request, so by the time it
             ;; returns the owner has already unwound.
             (check "request thread has exited when the call returns"
                    *cancel-fixture-exited*)
             (check "the stream is closed" (not (recording-stream-open-p stream)))
             (check "the stream is closed by its owner thread, not the caller"
                    (and *cancel-fixture-closed-by*
                         (not (eq *cancel-fixture-closed-by* caller))))
             (check "no provider request thread is left running"
                    (notany (lambda (th)
                              (and (bt:thread-alive-p th)
                                   (equal (bt:thread-name th) "evo-provider-request")))
                            (bt:all-threads)))))
      (setf (symbol-function 'dex:post) saved-post))))

(defun test-abort-is-a-message ()
  "REQUEST-ABORT must only post a message; the worker latches it and runs the
cleanups.  Previously the requesting thread ran them itself, which is what made
cross-thread teardown of worker-owned resources possible in the first place."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-abort-msg-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (ran-on nil))
    (evo.kernel::add-abort-cleanup agent (lambda () (setf ran-on (bt:current-thread))))
    (request-abort agent)
    (check "requesting an abort does not run cleanups on the caller's thread"
           (null ran-on))
    (check "the worker sees the abort at its next safe point"
           (agent-abort-flag agent))
    (check "and the cleanup ran on the thread that consumed it"
           (eq ran-on (bt:current-thread)))
    ;; Latched: a second look still reports aborted, and does not re-run.
    (setf ran-on nil)
    (check "abort stays latched for the rest of the run" (agent-abort-flag agent))
    (check "cleanups run once, not on every poll" (null ran-on))
    ;; A fresh run starts from a clean mailbox.
    (reset-agent-run-control agent)
    (check "a new run starts un-aborted" (not (agent-abort-flag agent)))))

(defun test-session-quiescence ()
  "A journal switch must not carry another session's queued input with it.
The model gate leaves steering queued while nothing runs, which used to look
idle enough to switch sessions under."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-quiesce-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent)))
    (check "an idle empty session is quiescent" (evo.tui::session-quiescent-p tui))
    (queue-steering agent "input typed against THIS session")
    (check "queued steering is pending work, even with no task running"
           (not (evo.tui::session-quiescent-p tui)))
    (with-output-to-string (fake-tty)
      (let ((evo.tui::*tty-out* fake-tty)
            (evo.tui::*region-height* 0)
            (evo.tui::*region-cursor-row* 0))
        (check "a session switch is refused while input is queued"
               (not (evo.tui::require-session-quiescent tui "/resume")))
        (check "switch-journal refuses rather than migrating the queue"
               (handler-case
                   (progn (evo.tui::switch-journal tui (make-session-journal dir)) nil)
                 (error () t)))
        (check "the input is still queued in its own session"
               (steering-pending-p agent))
        ;; Drained (as a real run would), the switch is allowed.
        (evo.kernel::drain-steering agent)
        (check "a drained session is quiescent again"
               (evo.tui::session-quiescent-p tui))
        (let ((run-id (evo.kernel::agent-run-id agent)))
          (setf (evo.kernel::agent-turn-index agent) 7
                (evo.kernel::agent-retry-count agent) 2)
          (evo.tui::switch-journal tui (make-session-journal dir))
          (check "switching resets session-owned execution state"
                 (and (not (equal run-id (evo.kernel::agent-run-id agent)))
                      (zerop (evo.kernel::agent-turn-index agent))
                      (zerop (evo.kernel::agent-retry-count agent)))))))))

(defun test-tui-task-ownership ()
  "Run state is one task object, and a stale completion event cannot clear a
newer task — the failure the separate running/worker/compacting booleans made
possible."
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-task-~a/" (tmp-dir) (gen-id))))
         (journal (progn (ensure-directories-exist dir) (make-session-journal dir)))
         (agent (make-agent :journal journal))
         (tui (evo.tui::make-tui :agent agent)))
    (check "no task means not running" (not (evo.tui::tui-running tui)))
    (setf (evo.tui::tui-task tui)
          (evo.tui::make-tui-task :id "task-2" :kind :run))
    (check "a live task means running" (evo.tui::tui-running tui))
    (check "a run task is not compacting" (not (evo.tui::tui-compacting tui)))
    (with-output-to-string (fake-tty)
      (let ((evo.tui::*tty-out* fake-tty)
            (evo.tui::*region-height* 0)
            (evo.tui::*region-cursor-row* 0))
        ;; A late :worker-done from a previous task must be ignored.
        (evo.tui::handle-agent-event tui '(:type :worker-done :task-id "task-1"
                                           :outcome :stop))
        (check "a stale worker-done cannot clear the current task"
               (evo.tui::tui-running tui))
        (evo.tui::handle-agent-event tui '(:type :worker-done :task-id "task-2"
                                           :outcome :stop))
        (check "the matching worker-done clears it"
               (not (evo.tui::tui-running tui)))
        ;; A run's automatic compaction: the activity display echoes the
        ;; worker's events, and dies with the task.
        (setf (evo.tui::tui-task tui)
              (evo.tui::make-tui-task :id "task-3" :kind :run))
        (evo.tui::handle-agent-event tui '(:type :compaction-start))
        (check "auto compaction shows as compacting"
               (search "compacting" (evo.tui::activity-line tui)))
        (evo.tui::handle-agent-event tui '(:type :compaction-end))
        (check "compaction end returns the display to working"
               (search "working" (evo.tui::activity-line tui)))
        ;; An abort the finished run never consumed is moot once its task is
        ;; joined; left queued it would wedge session-quiescence.
        (request-abort agent)
        (check "an unconsumed abort counts as pending mailbox work"
               (agent-pending-work-p agent))
        (evo.tui::handle-agent-event tui '(:type :worker-done :task-id "task-3"
                                           :outcome :aborted))
        (check "joining the task clears the abort it never consumed"
               (evo.tui::session-quiescent-p tui))))
    ;; Off-thread repaint requests go through the queue, not the slot.
    (setf (evo.tui::tui-dirty tui) nil)
    (evo.tui::request-repaint tui)
    (check "request-repaint does not touch TUI state directly"
           (not (evo.tui::tui-dirty tui)))
    (dolist (event (evo.tui::drain-events tui))
      (evo.tui::handle-agent-event tui event))
    (check "the TUI thread marks itself dirty when it drains the request"
           (evo.tui::tui-dirty tui))))

(defun test-extension-ownership ()
  "Hooks, tasks and patches belong to the extension generation that made them.
Reloading disposes the old generation instead of stacking another copy."
  (let ((evo.kernel::*event-hooks* (make-hash-table))
        (evo.kernel::*extension-disposers* nil)
        (evo.kernel::*extension-tasks* nil)
        (evo.kernel::*extension-generation* 10))
    (let ((owner-a (evo.kernel::%make-extension-owner :path "/x/900-p.lisp"
                                                     :generation 10)))
      ;; An anonymous hook is what accumulated before: assert the named one does
      ;; not, and that the anonymous one still appends (the API is explicit).
      (evo.kernel:add-hook :probe (lambda (p) (declare (ignore p)) :named)
                           :name :probe-hook :owner owner-a)
      (evo.kernel:add-hook :probe (lambda (p) (declare (ignore p)) :named-again)
                           :name :probe-hook :owner owner-a)
      (check "a named hook replaces its previous registration"
             (= 1 (length (gethash :probe evo.kernel::*event-hooks*))))
      (check "and the replacement is the live one"
             (equal '(:named-again) (evo.kernel:run-hooks :probe nil)))
      (evo.kernel:add-hook :probe (lambda (p) (declare (ignore p)) :anon)
                           :owner owner-a)
      (check "an anonymous hook appends"
             (= 2 (length (gethash :probe evo.kernel::*event-hooks*))))
      ;; A tracked task, and a disposer standing in for a function patch.
      (let* ((stop nil)
             (undone nil)
             (thread (let ((evo.kernel::*extension-owner* owner-a))
                       (evo:spawn-task :name :probe-task
                                       :run (lambda () (loop until stop
                                                             do (sleep 0.01)))
                                       :stop (lambda () (setf stop t))))))
        (let ((evo.kernel::*extension-owner* owner-a))
          (evo:on-unload (lambda () (setf undone t))))
        (check "the task is running" (bt:thread-alive-p thread))
        ;; A new generation arrives: dispose everything the old one owned.
        (incf evo.kernel::*extension-generation*)
        (evo.kernel:dispose-extension-owners
         :before evo.kernel::*extension-generation*)
        (check "disposal stops and joins the extension's task"
               (not (bt:thread-alive-p thread)))
        (check "disposal runs the extension's own undo" undone)
        (check "disposal withdraws the extension's hooks"
               (null (gethash :probe evo.kernel::*event-hooks*)))))
    ;; A task that ignores its stop is abandoned after a bounded wait — one
    ;; misbehaving extension must not be able to hang every future reload.
    (let* ((evo.kernel::*task-stop-seconds* 0.2)
           (owner-b (evo.kernel::%make-extension-owner
                     :path "/x/900-stubborn.lisp"
                     :generation evo.kernel::*extension-generation*)))
      (let ((evo.kernel::*extension-owner* owner-b))
        (evo:spawn-task :name :stubborn-task
                        :run (lambda () (sleep 3))    ; outlives the bound, then dies
                        :stop (lambda () nil)))       ; ignores its own stop
      (incf evo.kernel::*extension-generation*)
      (let ((started (get-internal-real-time)))
        (handler-bind ((warning #'muffle-warning))
          (evo.kernel:dispose-extension-owners
           :before evo.kernel::*extension-generation*))
        (check "a stubborn task is abandoned instead of hanging disposal"
               (< (/ (- (get-internal-real-time) started)
                     internal-time-units-per-second)
                  2))))))

(defparameter *catalog-good-init*
  "(evo:register-model \"catalog-good\" :provider :anthropic :api :anthropic-messages
   :context-window 1000 :max-output 100)
(evo:set-setting :model \"catalog-good\")")

(defun test-runtime-catalog-atomicity ()
  "A failed reload must leave the previous runtime intact, not a half-built
one — and must sweep the failed build's OWN registrations, or every retried
reload stacks another copy of whatever loaded before the failure.

The failure is real, not simulated: init files swallow ordinary errors (a
broken config may not take the session down), so the poison is a non-ERROR
serious condition in post-init, which unwinds BOOT-USERSPACE's own recovery
path after the probe extension has already loaded."
  (let* ((home (evo-home))
         (extdir (merge-pathnames "extensions/" home))
         (probe (merge-pathnames "500-atomicity-probe.lisp" extdir))
         (global-init (merge-pathnames "init.lisp" home))
         (global-post-init (merge-pathnames "post-init.lisp" home))
         (cwd (merge-pathnames (format nil "catalog-test-~a/" (gen-id)) home)))
    (ensure-directory extdir)
    (ensure-directory cwd)
    (flet ((live-tasks ()
             (count-if (lambda (th)
                         (and (bt:thread-alive-p th)
                              (equal (bt:thread-name th) "ATOMICITY-PROBE-TASK")))
                       (bt:all-threads)))
           (probe-hooks ()
             (length (gethash :atomicity-probe evo.kernel::*event-hooks*))))
      (unwind-protect
           (progn
             (write-file-string global-init *catalog-good-init*)
             (write-file-string
              probe
              "(in-package :evo.user)
(defvar *atomicity-probe-stop* nil)
(evo:on :atomicity-probe (lambda (ev) (declare (ignore ev)) nil)
        :name :atomicity-probe-hook)
(setf *atomicity-probe-stop* nil)
(evo:spawn-task :name :atomicity-probe-task
                :run (lambda () (loop until *atomicity-probe-stop* do (sleep 0.01)))
                :stop (lambda () (setf *atomicity-probe-stop* t)))")
             (evo.kernel:boot-userspace :cwd cwd)
             (sleep 0.1)
             (let ((generation evo.kernel::*extension-generation*))
               (check "the good generation is installed"
                      (and (equal "catalog-good" (setting :model))
                           (find "catalog-good" (all-models)
                                 :key (lambda (m) (pget m :id)) :test #'equal)))
               (check "its extension's hook and task are live"
                      (and (= 1 (probe-hooks)) (= 1 (live-tasks))))
               ;; Poison the build.  The broken init means the failing build
               ;; registers NO models and NO settings, so the assertions below
               ;; can only pass if the catalog restore really happened.
               (write-file-string global-init "(error \"broken init: registries stay empty\")")
               (write-file-string
                global-post-init
                "(define-condition atomicity-boom (serious-condition) ())
(error 'atomicity-boom)")
               (let ((*error-output* (make-broadcast-stream)))
                 (check "the poisoned build signals out of boot-userspace"
                        (handler-case
                            (progn (evo.kernel:boot-userspace :cwd cwd) nil)
                          (serious-condition () t))))
               (check "a failed build restores the previous model registry"
                      (find "catalog-good" (all-models)
                            :key (lambda (m) (pget m :id)) :test #'equal))
               (check "a failed build restores the previous settings"
                      (equal "catalog-good" (setting :model)))
               (check "a failed build does not advance the generation"
                      (= generation evo.kernel::*extension-generation*))
               (check "the failed build's own hooks are swept, not stranded"
                      (zerop (probe-hooks)))
               (check "the failed build's own task is stopped and joined"
                      (zerop (live-tasks)))
               ;; The normal repair: fix the files and reload.  Without the
               ;; failure-path sweep, the stranded registrations carry the
               ;; rolled-back counter's NEXT generation number, read as
               ;; current, and double up exactly here.
               (delete-file global-post-init)
               (write-file-string global-init *catalog-good-init*)
               (evo.kernel:boot-userspace :cwd cwd)
               (sleep 0.1)
               (check "the repaired reload installs exactly one hook"
                      (= 1 (probe-hooks)))
               (check "and exactly one task" (= 1 (live-tasks)))))
        ;; Stop the surviving task and dispose everything before leaving.
        (ignore-errors
          (let ((stop (find-symbol "*ATOMICITY-PROBE-STOP*" :evo.user)))
            (when stop (setf (symbol-value stop) t))))
        (ignore-errors (evo.kernel:dispose-extension-owners
                        :before (1+ evo.kernel::*extension-generation*)))
        (ignore-errors (delete-file probe))
        (ignore-errors (delete-file global-init))
        (ignore-errors (delete-file global-post-init))
        (reset-user-registries)
        (reset-settings)
        (register-fixture-models)))))

(defun test-reload-generation-ordering ()
  "Generations must not overlap.  A reloaded file reuses the same package and
the same globals, so if the outgoing generation were disposed AFTER the incoming
one loaded, the old stop function would write the very variable the new task
reads — and silently kill it.  Caught in exactly that form: one live poller
became zero after the first reload."
  (let* ((home (evo-home))
         (extdir (merge-pathnames "extensions/" home))
         (probe (merge-pathnames "500-reload-probe.lisp" extdir))
         (global-init (merge-pathnames "init.lisp" home))
         (cwd (merge-pathnames (format nil "reload-order-~a/" (gen-id)) home)))
    (ensure-directory extdir)
    (ensure-directory cwd)
    (unwind-protect
         (progn
           (write-file-string
            global-init
            "(evo:register-model \"reload-probe-model\" :provider :anthropic
   :api :anthropic-messages :context-window 1000 :max-output 100)")
           ;; The shared global is the point: both generations see this symbol.
           (write-file-string
            probe
            "(in-package :evo.user)
(defvar *reload-probe-stop* nil)
(defvar *reload-probe-unloads* 0)
(evo:on :session-start (lambda (ev) (declare (ignore ev)) nil)
        :name :reload-probe-hook)
(setf *reload-probe-stop* nil)
(evo:spawn-task :name :reload-probe-task
                :run (lambda () (loop until *reload-probe-stop* do (sleep 0.01)))
                :stop (lambda () (setf *reload-probe-stop* t)))
(evo:on-unload (lambda () (incf *reload-probe-unloads*)))")
           (flet ((live-tasks ()
                    (count-if (lambda (th)
                                (and (bt:thread-alive-p th)
                                     (equal (bt:thread-name th) "RELOAD-PROBE-TASK")))
                              (bt:all-threads)))
                  (hooks ()
                    (length (gethash :session-start evo.kernel::*event-hooks*))))
             (evo.kernel:boot-userspace :cwd cwd)
             (sleep 0.1)
             (let ((hooks-after-first (hooks)))
               (check "the extension's task is running after the first boot"
                      (= 1 (live-tasks)))
               (evo.kernel:boot-userspace :cwd cwd)
               (sleep 0.1)
               (check "a reload leaves exactly one live task, not zero and not two"
                      (= 1 (live-tasks)))
               (check "a reload does not accumulate hooks"
                      (= hooks-after-first (hooks)))
               (evo.kernel:boot-userspace :cwd cwd)
               (sleep 0.1)
               (check "still exactly one live task after a third boot"
                      (= 1 (live-tasks)))
               (check "each reload undid the previous generation once"
                      (eql 2 (symbol-value (find-symbol "*RELOAD-PROBE-UNLOADS*"
                                                        :evo.user)))))))
      ;; Stop the surviving task before leaving, so it does not outlive the test.
      (ignore-errors
        (let ((stop (find-symbol "*RELOAD-PROBE-STOP*" :evo.user)))
          (when stop (setf (symbol-value stop) t))))
      (ignore-errors (evo.kernel:dispose-extension-owners
                      :before (1+ evo.kernel::*extension-generation*)))
      (ignore-errors (delete-file probe))
      (ignore-errors (delete-file global-init))
      (reset-user-registries)
      (reset-settings)
      (register-fixture-models))))

(defun run-all ()
  (let ((*pass* 0) (*fail* 0))
    (test-sexpr-io)
    (test-journal)
    (test-schema)
    (test-registry)
    (test-apis)
    (test-extension-apis)
    (test-model-picker-labels)
    (test-same-id-multi-provider)
    (register-fixture-models)
    (test-sse)
    (test-sse-transport)
    (test-handoff)
    (test-wire-message-shaping)
    (test-anthropic-request)
    (test-anthropic-effort)
    (test-model-effort-registration)
    (test-retired-off-level)
    (test-kimi-provider)
    (test-port-timeout)
    (test-env-proxy)
    (test-claude-oauth-proxy-guards)
    (test-claude-oauth-auto-refresh)
    (test-init-files)
    (test-extension-load-order)
    (test-preflight)
    (test-proxy-plumbing)
    (test-restart-and-resume)
    (test-parse-args)
    (test-editor)
    (test-input)
    (test-paste)
    (test-status-segments)
    (test-tui-compose)
    (test-resume-picker)
    (test-render-anchor)
    (test-display-width)
    (test-wrap-visible)
    (test-markdown)
    (test-math)
    (test-prose-styler)
    (test-bionic)
    (test-theme)
    (test-user-prompt-block)
    (test-input-history)
    (test-line-endings)
    (test-prompt-notes)
    (test-prompt-languages)
    (test-goal-budget)
    (test-goal-tools)
    (test-templates)
    (test-compaction)
    (test-lore)
    (test-lore-slash-commands)
    (test-project-memory)
    (test-global-memory)
    (test-prompt-template)
    (test-tool-call-events)
    (test-interrupt)
    (test-jobs)
    (test-tool-call-display)
    (test-no-modes)
    (test-tool-call-gate-extension-point)
    (test-active-tools-extension-point)
    (test-injected-context-extension-point)
    (test-custom-state-extension-point)
    (test-eval)
    (test-eval-tool)
    (test-eval-completion)
    (test-eval-completion-source)
    (test-base64)
    (test-image-media-types)
    (test-attach-image)
    (test-clipboard-image)
    (test-pasted-image-paths)
    (test-editor-attachments)
    (test-image-steering)
    (test-image-wire)
    (test-image-tui-paste)
    (test-image-export)
    (test-image-read-tool)
    (test-provider-request-ownership)
    (test-abort-is-a-message)
    (test-session-quiescence)
    (test-tui-task-ownership)
    (test-extension-ownership)
    (test-runtime-catalog-atomicity)
    (test-reload-generation-ordering)
    (format t "~%~d passed, ~d failed~%" *pass* *fail*)
    (if (zerop *fail*) 0 1)))
