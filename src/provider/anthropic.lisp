;;;; anthropic.lisp — the Anthropic Messages API.
;;;;
;;;; Stateless replay (full history each request); prompt caching via
;;;; cache_control breakpoints (system prompt, last tool def, last user
;;;; message); same-model thinking replays verbatim with its signature.
;;;; See api.lisp for the adapter contract.

(in-package :evo.provider)

(defclass anthropic-messages-api (provider-api) ())

(defmethod endpoint-path ((api anthropic-messages-api))
  "/v1/messages")

(defmethod auth-headers ((api anthropic-messages-api) config)
  `(("x-api-key" . ,(pget config :api-key))
    ("anthropic-version" . "2023-06-01")))

(defmethod default-provider-key ((api anthropic-messages-api)) :anthropic)
(defmethod default-base-url ((api anthropic-messages-api)) "https://api.anthropic.com")
(defmethod default-api-key-env ((api anthropic-messages-api)) "ANTHROPIC_API_KEY")

;;; Thinking levels -> the wire.
;;;
;;; The one dial is `output_config.effort`, a request-level dial over the
;;; whole response (thinking, prose, and tool calls alike); the levels are
;;; exactly evo's ladder.  The supported Anthropic models — Sonnet 5,
;;; Opus 5, Fable 5 — all take it, alongside adaptive thinking
;;; (:thinking-mode :adaptive): the model decides when and how much to
;;; think, and evo asks for summarized thinking so there is something to
;;; display.  Extended thinking's `thinking.budget_tokens` is a retired
;;; knob of retired models: those three reject it outright, so it is gone
;;; from evo too.
;;;
;;; Third-party endpoints speaking this API vary, which is why the
;;; `thinking` object is per-model registry data (:thinking-mode) and worth
;;; measuring rather than assuming.  :effort-only — the default — sends no
;;; `thinking` object at all: effort is the whole dial, the smallest
;;; request every Messages-compatible endpoint accepts.  Kimi Code's K3 is
;;; one measured example — it always reasons, its documented dial is
;;; low/high/max effort, and a `thinking` of type disabled would route the
;;; request to an older model.

(defun effort-string (level supported)
  "Wire value for `output_config.effort`, or NIL when the model has no
effort parameter."
  (let ((clamped (clamp-effort level supported)))
    (when clamped (string-downcase (symbol-name clamped)))))

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
    (:image (let ((data (pget block :data)))
              (if data
                  (jobj "type" "image"
                        "source" (jobj "type" "base64"
                                       "media_type" (or (pget block :media-type) "image/png")
                                       "data" data))
                  ;; A vision-less model had its images degraded to text by
                  ;; the handoff pass; a data-less block reaching here is a
                  ;; bug elsewhere, and a placeholder beats a 400.
                  (content-block->json (image-placeholder-block block)))))
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

(defun json-tree-has-image-p (value)
  "Whether VALUE, already in provider JSON shape, contains an image block."
  (typecase value
    (hash-table
     (or (equal (gethash "type" value) "image")
         (loop for v being the hash-values of value
               thereis (json-tree-has-image-p v))))
    (vector
     (loop for v across value thereis (json-tree-has-image-p v)))
    (t nil)))

(defun add-cache-control (json-messages)
  "Mark a message cache breakpoint.

If the request contains actual image blocks, stop the message-level breakpoint
immediately before the first image-bearing content block.  Prompt caching does
not shrink the HTTP JSON body, and caching megabytes of screenshot pixels churns
the cache for little benefit.  System prompt and tool-schema breakpoints still
apply."
  (let ((candidate nil))
    (loop named scan
          for msg across json-messages
          for content = (gethash "content" msg)
          do (when content
               (loop for block across content
                     do (when (json-tree-has-image-p block)
                          (return-from scan))
                        (setf candidate block))))
    (when candidate
      (setf (gethash "cache_control" candidate) (jobj "type" "ephemeral"))))
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
         (effort (effort-string thinking-level (model-effort model)))
         (req (jobj "model" model-id
                    "max_tokens" (model-max-output model)
                    "stream" t
                    "messages" (add-cache-control
                                (messages->json
                                 (handoff-pass messages model-id
                                               :vision (model-vision-p model)))))))
    (when system
      (setf (gethash "system" req)
            (vector (let ((b (jobj "type" "text" "text" system)))
                      (setf (gethash "cache_control" b) (jobj "type" "ephemeral"))
                      b))))
    (let ((jt (tools->json tools)))
      (when jt (setf (gethash "tools" req) jt)))
    (when effort
      (setf (gethash "output_config" req) (jobj "effort" effort)))
    ;; Adaptive models decide when to think, so they take a mode rather than
    ;; a budget -- and `display` defaults to omitted there, which returns
    ;; signed but empty thinking blocks, so ask for summaries explicitly.
    ;; :effort-only sends no `thinking` object on purpose —
    ;; output_config.effort above is its whole dial.
    (when (eq (model-thinking-mode model) :adaptive)
      (setf (gethash "thinking" req)
            (jobj "type" "adaptive" "display" "summarized")))
    (jzon:stringify req)))

(defmethod build-request ((api anthropic-messages-api)
                          &key model system messages tools thinking-level)
  (build-request-json :model model :system system :messages messages
                      :tools tools :thinking-level thinking-level))

;;; SSE parsing: a stream ending without message_stop is a retryable error.
;;; Tool arguments accumulate as partial JSON and are parsed at
;;; content_block_stop.

(defstruct sse-block type text thinking signature id name (input-json ""))

(defun parse-sse-stream (char-stream &key on-event abort-flag)
  "Parse an Anthropic Messages SSE stream into the adapter result plist."
  (let ((blocks (make-hash-table))     ; index -> sse-block
        (max-index -1)
        (raw-stop nil) (model nil) (stopped-p nil) (error-message nil)
        (in-tokens 0) (out-tokens 0) (cache-read 0) (cache-write 0))
    (labels ((emit (&rest ev) (when on-event (funcall on-event ev)))
             (handle (event-type data)
               (let* ((obj (ignore-errors (jzon:parse data)))
                      (type (or event-type (and obj (jget obj "type")))))
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
                              max-index (max max-index idx))))
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
                     (t nil)))
                 (when (or stopped-p error-message) :stop))))
      (when (eq (map-sse-events char-stream #'handle :abort-flag abort-flag)
                :aborted)
        (return-from parse-sse-stream
          (list :aborted-p t :content nil :stop-reason :aborted
                :usage (list :input in-tokens :output out-tokens
                             :cache-read cache-read :cache-write cache-write))))
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

(defmethod parse-stream ((api anthropic-messages-api) char-stream
                         &key on-event abort-flag)
  (parse-sse-stream char-stream :on-event on-event :abort-flag abort-flag))

(register-api :anthropic-messages (make-instance 'anthropic-messages-api))
