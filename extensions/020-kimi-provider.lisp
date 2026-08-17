;;;; 020-kimi-provider.lisp — Moonshot AI's Kimi K3, over the OpenAI-compatible
;;;; chat-completions wire protocol.  Vendored user extension, installed by
;;;; `make install` (via install-home) into $(EVO_HOME)/extensions/ and loaded
;;;; automatically at startup.
;;;;
;;;; What this registers
;;;;   api      :kimi-chat-completions  — POST /v1/chat/completions (SSE)
;;;;   provider :moonshotai             — https://api.moonshot.ai
;;;;   model    kimi-k3                 — 1,048,576 ctx / 131,072 max output
;;;;
;;;; Only kimi-k3 is supported, by design.  K2.x speaks a different thinking
;;;; dial (the `thinking` object instead of top-level `reasoning_effort`) and
;;;; has no dynamic tool loading; supporting both from one adapter would mean
;;;; guessing per model id.  If you want a K2.x model, register it against an
;;;; adapter that speaks its dialect.
;;;;
;;;; The upstream config this implements, requirement by requirement:
;;;;   api "openai-completions"    — chat/completions, not /v1/responses; evo
;;;;                                 ships no chat-completions adapter, so the
;;;;                                 whole wire protocol lives here.  It is
;;;;                                 registered as :kimi-chat-completions
;;;;                                 rather than :openai-completions because it
;;;;                                 is not a general one: reasoning_content
;;;;                                 replay and dynamically loaded tools are
;;;;                                 Kimi's, and pointing another endpoint at
;;;;                                 this key would eventually 400.
;;;;   reasoning true              — :thinking t; K3 always thinks.
;;;;   input text + image          — :vision t; images ride as base64 data URLs.
;;;;   thinkingLevelMap            — THINKING-PARAM below is that table exactly
;;;;                                 (low/high/max map, the rest are unmapped);
;;;;                                 the request path additionally clamps an
;;;;                                 unmapped rung down evo's ladder.
;;;;   contextWindow / maxTokens   — 1048576 / 131072.
;;;;   cost                        — *KIMI-PRICING*, USD per 1M tokens, spent
;;;;                                 by /kimi:cost.  ¥20/¥2/¥100 on the .cn
;;;;                                 platform is the same ratio in CNY.
;;;;   supportsStore false         — no `store` field is sent (that is a
;;;;                                 Responses-API field anyway).
;;;;   supportsDeveloperRole false — the system prompt goes in a `system`
;;;;                                 message, never a `developer` one.
;;;;   supportsReasoningEffort true— top-level `reasoning_effort`.
;;;;   maxTokensField max_tokens   — *KIMI-MAX-TOKENS-FIELD*; the docs mark
;;;;                                 max_tokens deprecated in favour of
;;;;                                 max_completion_tokens (identical meaning),
;;;;                                 so switching is a one-line change.
;;;;   supportsStrictMode false    — tools are sent with "strict": false.  The
;;;;                                 server defaults it to TRUE, so opting out
;;;;                                 has to be explicit: evo tool schemas are
;;;;                                 hand-written per extension, not authored
;;;;                                 against Moonshot Flavored JSON Schema, and
;;;;                                 a schema quirk should not become a 400.
;;;;   thinkingFormat openai       — thinking streams as `reasoning_content`
;;;;                                 deltas beside `content` deltas.
;;;;   requiresReasoningContent... — every assistant turn replays its
;;;;                                 reasoning_content (Preserved Thinking is
;;;;                                 always on for K3 and the docs require the
;;;;                                 full assistant message to go back verbatim).
;;;;   deferredToolsMode kimi      — tools that appear mid-session are injected
;;;;                                 as a trailing {"role":"system","tools":[…]}
;;;;                                 message instead of being spliced into the
;;;;                                 top-level `tools` array, which would move
;;;;                                 the cached prefix.  See KIMI--SPLIT-TOOLS.
;;;;
;;;; Not sent, on purpose: temperature, top_p, n, presence_penalty,
;;;; frequency_penalty.  K3 fixes all five and rejects any other value.
;;;;
;;;; Environment variables:
;;;;   MOONSHOT_API_KEY / KIMI_API_KEY   — API key (Bearer token)
;;;;   MOONSHOT_BASE_URL / KIMI_BASE_URL — endpoint override, e.g.
;;;;       https://api.moonshot.cn for the China platform.  A trailing "/v1"
;;;;       is stripped: the SDK-style base URL and the host both work.
;;;;
;;;; Or write it in config instead, which wins over both — see
;;;; docs/examples/init.lisp:
;;;;
;;;;   (evo:register-provider :moonshotai :api-key "sk-...")
;;;;
;;;; init.lisp is evaluated before extensions load, so this extension fills in
;;;; only the fields config left out (KIMI--REGISTER-ENDPOINT).
;;;;
;;;; Keys are platform-scoped: a platform.kimi.com (.cn) key on api.moonshot.ai
;;;; is a 401, and vice versa.
;;;;
;;;; Reference: https://platform.kimi.com/docs/overview (API reference:
;;;; /docs/api/chat, /docs/guide/use-reasoning-effort, /docs/guide/
;;;; use-thinking-models, /docs/guide/use-dynamic-tool-loading).

(in-package :evo.user)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defparameter *kimi-provider-key* :moonshotai)
(defparameter *kimi-api-name* :kimi-chat-completions)
(defparameter *kimi-model-id* "kimi-k3")
(defparameter *kimi-default-base-url* "https://api.moonshot.ai")
(defparameter *kimi-api-key-env* "MOONSHOT_API_KEY")

(defparameter *kimi-context-window* 1048576)
(defparameter *kimi-max-output* 131072
  "Default max_tokens.  The server default for K3 is the same 131072; the
ceiling is 1048576.")

(defparameter *kimi-effort-levels* '(:low :high :max)
  "The rungs K3 accepts for reasoning_effort.  evo's ladder is
(:low :medium :high :xhigh :max); a request at a rung K3 does not have is
clamped down to the strongest one it does have.")

(defparameter *kimi-max-tokens-field* "max_tokens"
  "Wire name for the output-token cap.  \"max_tokens\" is what the provider
config asks for; the docs deprecate it in favour of the identically-behaved
\"max_completion_tokens\".")

(defparameter *kimi-strict-tools* nil
  "Value of each tool's `strict` field.  NIL turns off strict-schema decoding,
which the server would otherwise default to true.")

(defparameter *kimi-deferred-tools* t
  "When true, tools that show up after a session's first request are declared
in a trailing dynamic-tools system message instead of being added to the
top-level `tools` array.  Appending leaves the cached prefix untouched;
rewriting the tool array would invalidate it for the whole conversation.")

(defparameter *kimi-pricing* '(:input 3 :output 15 :cache-read 0.3 :cache-write 0)
  "USD per 1M tokens for kimi-k3 on api.moonshot.ai.  Cache writes are free —
K3's context cache is automatic, with no explicit write step.")

;;; ---------------------------------------------------------------------------
;;; Small helpers over kernel internals
;;;
;;; The JSON bridge (plists <-> hash-tables/vectors), the handoff pass and the
;;; effort clamp are EVO.PROVIDER internals rather than public API.  Reading
;;; them is what a package lock permits; wrapping them here keeps every such
;;; reference in one place, so a kernel rename breaks this section and not the
;;; whole adapter.
;;; ---------------------------------------------------------------------------

(defun kimi--pget (plist key &optional default)
  (evo.util:pget plist key default))

(defun kimi--jobj (&rest kvs)
  "JSON object from KVS (string key, value, ...)."
  (apply #'evo.provider::jobj kvs))

(defun kimi--jget (obj &rest keys)
  (apply #'evo.provider::jget obj keys))

;;; JSON null parses to a symbol, not NIL, so every read of an optional field
;;; is type-checked rather than trusted: chat-completions chunks are full of
;;; explicitly-null fields (finish_reason, usage, tool_call ids), and a null
;;; that slipped through as a value would surface as "NULL" in a prompt or as
;;; a type error deep in the parse.

(defun kimi--jstring (obj &rest keys)
  (let ((value (apply #'kimi--jget obj keys)))
    (and (stringp value) value)))

(defun kimi--jint (obj &rest keys)
  (let ((value (apply #'kimi--jget obj keys)))
    (if (realp value) value 0)))

(defun kimi--sexpr->json (value)
  (evo.provider::sexpr->json value))

(defun kimi--json->sexpr (value)
  (evo.provider::json->sexpr value))

(defun kimi--provider-entry ()
  "The raw registry plist for the provider, or NIL — what init.lisp already
said about this endpoint.  PROVIDER-CONFIG cannot answer this: it errors when
no base URL is set yet, and it resolves the key rather than reporting whether
one was written down."
  (cdr (assoc *kimi-provider-key* evo.provider::*providers*)))

(defun kimi--handoff (messages model)
  (evo.provider::handoff-pass messages (kimi--pget model :id)
                              :vision (evo.provider:model-vision-p model)))

(defun kimi--trim (value)
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun kimi--env (name)
  (kimi--trim (uiop:getenv name)))

(defun kimi--normalize-base-url (url)
  "Accept either the host (https://api.moonshot.ai) or the SDK-style base URL
(https://api.moonshot.ai/v1): ENDPOINT-PATH supplies the /v1 prefix, so a
configured one would double it."
  (when url
    (let ((url (string-right-trim "/" url)))
      (if (and (>= (length url) 3) (string= "/v1" url :start2 (- (length url) 3)))
          (string-right-trim "/" (subseq url 0 (- (length url) 3)))
          url))))

(defun kimi--base-url ()
  (or (kimi--normalize-base-url (or (kimi--env "KIMI_BASE_URL")
                                    (kimi--env "MOONSHOT_BASE_URL")))
      *kimi-default-base-url*))

(defun kimi--api-key ()
  (or (kimi--env *kimi-api-key-env*) (kimi--env "KIMI_API_KEY")))

;;; ---------------------------------------------------------------------------
;;; The API object
;;; ---------------------------------------------------------------------------

(defclass kimi-chat-completions-api (evo:provider-api) ()
  (:documentation "Moonshot AI chat-completions: OpenAI's wire shape plus
Kimi's documented extensions (reasoning_content, reasoning_effort,
cached_tokens, dynamically loaded tools)."))

(defmethod evo:endpoint-path ((api kimi-chat-completions-api))
  (declare (ignore api))
  "/v1/chat/completions")

(defmethod evo:auth-headers ((api kimi-chat-completions-api) config)
  (declare (ignore api))
  (let ((key (or (kimi--trim (kimi--pget config :api-key)) (kimi--api-key))))
    (unless key
      (error 'evo:provider-error
             :message (format nil (cat "No Moonshot API key: set ~a (or KIMI_API_KEY), "
                                       "or pass :api-key to (evo:register-provider ~s ...). "
                                       "Keys are platform-scoped — a platform.kimi.com key "
                                       "is a 401 on api.moonshot.ai.")
                              *kimi-api-key-env* *kimi-provider-key*)))
    `(("authorization" . ,(concatenate 'string "Bearer " key)))))

(defmethod evo:default-provider-key ((api kimi-chat-completions-api))
  (declare (ignore api))
  *kimi-provider-key*)

(defmethod evo:default-base-url ((api kimi-chat-completions-api))
  (declare (ignore api))
  (kimi--base-url))

(defmethod evo:default-api-key-env ((api kimi-chat-completions-api))
  (declare (ignore api))
  *kimi-api-key-env*)

;;; ---------------------------------------------------------------------------
;;; Thinking level -> reasoning_effort
;;;
;;; K3 always reasons; the dial is the top-level string.  THINKING-PARAM is the
;;; provider config's thinkingLevelMap verbatim — only low/high/max name a rung,
;;; everything else maps to nothing.  KIMI--MODEL-EFFORT is what the request
;;; actually uses: it first clamps the session level onto the model's declared
;;; ladder, so a session-wide :medium sends "low" instead of falling through to
;;; the server default (which is "max" — the opposite of what was asked for).
;;; ---------------------------------------------------------------------------

(defun kimi--effort-string (level)
  (case level
    (:low "low")
    (:high "high")
    (:max "max")
    (t nil)))

(defmethod evo:thinking-param ((api kimi-chat-completions-api) level)
  (declare (ignore api))
  (kimi--effort-string level))

(defun kimi--model-effort (model level)
  (let* ((declared (or (evo.provider:model-effort model) *kimi-effort-levels*))
         (clamped (evo.provider::clamp-effort level declared)))
    (kimi--effort-string (or clamped level))))

;;; ---------------------------------------------------------------------------
;;; Tools
;;; ---------------------------------------------------------------------------

;;; NIL serializes as JSON `false`, not as null or "absent", so an optional
;;; string field is either present with a string or left out entirely.  A
;;; `"description": false` would be a 400 on a field the model never needed.

(defun kimi--tool->json (tool)
  (let ((fn (kimi--jobj "name" (kimi--pget tool :name)
                        "parameters" (kimi--pget tool :input-schema)
                        "strict" (and *kimi-strict-tools* t))))
    (let ((description (kimi--pget tool :description)))
      (when description (setf (gethash "description" fn) description)))
    (kimi--jobj "type" "function" "function" fn)))

(defun kimi--tools->json (tools)
  (when tools
    (map 'vector #'kimi--tool->json tools)))

;;; Deferred tools ("deferredToolsMode": "kimi").
;;;
;;; K3 reads a {"role": "system", "tools": [...]} message as "these tools exist
;;; from here on".  That is the cache-friendly way to grow a tool set: the
;;; declaration is appended, so every byte before it is unchanged and the prefix
;;; cache still hits.  Editing the top-level `tools` array instead would move
;;; the very first tokens of the request and invalidate the cache for the entire
;;; conversation — which is exactly what happens in evo, whose agent can write
;;; and load a new tool mid-session.
;;;
;;; The baseline is per session (the cache key): the first request's tool set is
;;; the core, sent top-level; anything that appears later is deferred, in
;;; first-seen order, and re-sent every request (declarations are per-request —
;;; the server remembers nothing).  Tools that disappear from the active set
;;; drop out of both lists.  With no cache key (a one-shot call) nothing is
;;; deferred.

(defvar *kimi-tool-baselines* (make-hash-table :test #'equal)
  "Session cache key -> (:core (names) :deferred (names)).")

(defun kimi--tool-name (tool) (kimi--pget tool :name))

(defun kimi--tools-named (tools names)
  "TOOLS in NAMES order, skipping names with no tool."
  (loop for name in names
        for tool = (find name tools :key #'kimi--tool-name :test #'equal)
        when tool collect tool))

(defun kimi--split-tools (cache-key tools)
  "Split TOOLS into (values core deferred) for session CACHE-KEY."
  (if (or (not *kimi-deferred-tools*) (null cache-key) (null tools))
      (values tools nil)
      (let* ((names (mapcar #'kimi--tool-name tools))
             (entry (gethash cache-key *kimi-tool-baselines*))
             (core (if entry
                       (remove-if-not (lambda (n) (member n names :test #'equal))
                                      (kimi--pget entry :core))
                       names))
             (deferred (remove-if (lambda (n) (member n core :test #'equal))
                                  ;; already-deferred names keep their order;
                                  ;; genuinely new ones land at the end.
                                  (append (remove-if-not
                                           (lambda (n) (member n names :test #'equal))
                                           (kimi--pget entry :deferred))
                                          (remove-if
                                           (lambda (n)
                                             (member n (kimi--pget entry :deferred)
                                                     :test #'equal))
                                           names)))))
        (setf (gethash cache-key *kimi-tool-baselines*)
              (list :core core :deferred deferred))
        (values (kimi--tools-named tools core)
                (kimi--tools-named tools deferred)))))

(defun kimi--forget-tool-baseline (cache-key)
  "Forget CACHE-KEY's baseline, so the next request re-declares every tool
top-level.  A resumed session re-baselines this way by itself: the table is
process memory, and after a restart the whole current tool set is the core
again.  Either way every active tool is declared somewhere in the request —
only the cache hit rate is at stake."
  (remhash cache-key *kimi-tool-baselines*))

;;; ---------------------------------------------------------------------------
;;; Unified messages -> chat-completions messages
;;; ---------------------------------------------------------------------------

(defun kimi--user-block->json (block)
  (case (kimi--pget block :type)
    (:text (kimi--jobj "type" "text" "text" (or (kimi--pget block :text) "")))
    (:image
     (let ((data (kimi--pget block :data)))
       (if data
           ;; Inline base64 only: K3 rejects public image URLs (the other
           ;; accepted form is ms://<file-id>, which needs a prior upload).
           (kimi--jobj "type" "image_url"
                       "image_url"
                       (kimi--jobj "url"
                                   (format nil "data:~a;base64,~a"
                                           (or (kimi--pget block :media-type) "image/png")
                                           data)))
           ;; A vision-less model had its images degraded by the handoff pass;
           ;; a data-less block here is a bug elsewhere, and a placeholder
           ;; beats a 400.
           (kimi--user-block->json (evo.provider::image-placeholder-block block)))))
    (t (error 'evo:provider-error
              :message (format nil "Unknown user content block type ~s"
                               (kimi--pget block :type))))))

(defun kimi--blocks-text (blocks)
  (evo.util:string-join
   (string #\Newline)
   (loop for b in blocks
         when (eq (kimi--pget b :type) :text)
           collect (or (kimi--pget b :text) ""))))

(defun kimi--tool-call->json (block)
  (kimi--jobj "id" (or (kimi--pget block :id) "")
              "type" "function"
              "function" (kimi--jobj
                          "name" (or (kimi--pget block :name) "")
                          "arguments" (let ((args (kimi--pget block :arguments)))
                                        (if args
                                            (com.inuoe.jzon:stringify
                                             (kimi--sexpr->json args))
                                            "{}")))))

(defun kimi--assistant->json (m)
  "One assistant turn.  reasoning_content goes back verbatim: K3 keeps
Preserved Thinking on and the docs require the whole assistant message to be
replayed, thinking included.  The handoff pass has already dropped thinking
that came from a different model, so nothing cross-model is replayed here."
  (let ((content (kimi--pget m :content))
        (calls nil))
    (dolist (b content)
      (case (kimi--pget b :type)
        ((:text :thinking :image) nil)
        (:tool-call (push (kimi--tool-call->json b) calls))
        (t (error 'evo:provider-error
                  :message (format nil "Unknown content block type ~s"
                                   (kimi--pget b :type))))))
    (let* ((thinking (evo.util:string-join
                      (format nil "~2%")
                      (loop for b in content
                            when (and (eq (kimi--pget b :type) :thinking)
                                      (plusp (length (or (kimi--pget b :thinking) ""))))
                              collect (kimi--pget b :thinking))))
           (obj (kimi--jobj "role" "assistant"
                            ;; "" is what the API itself returns for a
                            ;; tool-call-only turn, and what replaying that
                            ;; message verbatim sends back.
                            "content" (kimi--blocks-text content))))
      (when (plusp (length thinking))
        (setf (gethash "reasoning_content" obj) thinking))
      (when calls
        (setf (gethash "tool_calls" obj) (coerce (nreverse calls) 'vector)))
      obj)))

(defun kimi--tool-result-images (m)
  (remove-if-not (lambda (b) (eq (kimi--pget b :type) :image))
                 (kimi--pget m :content)))

(defun kimi--tool-result->json (m)
  "A tool result.  Chat completions has no is_error flag — the error rides in
the text, which is where the kernel already puts it (\"Tool error: ...\") —
and the API rejects empty content."
  (let ((text (kimi--blocks-text (kimi--pget m :content)))
        (obj (kimi--jobj "role" "tool"
                         "tool_call_id" (or (kimi--pget m :tool-call-id) ""))))
    (let ((name (kimi--pget m :tool-name)))
      (when name (setf (gethash "name" obj) name)))
    (setf (gethash "content" obj)
          (cond ((plusp (length text)) text)
                ((kimi--tool-result-images m) "(image output; the image follows)")
                (t "(no tool output)")))
    obj))

(defun kimi--tool-result-image-message (m)
  "A `tool` message is text: chat completions has no image inside a tool
result.  A tool that hands back a picture (READ on a screenshot) therefore
gets it delivered in the user message that follows the result — dropping it
would leave the model answering blind about an image it was told it had."
  (let ((images (kimi--tool-result-images m)))
    (when images
      (kimi--jobj "role" "user"
                  "content"
                  (coerce (cons (kimi--jobj "type" "text"
                                            "text" (format nil "Image~p returned by the ~a call above:"
                                                           (length images)
                                                           (or (kimi--pget m :tool-name) "tool")))
                                (mapcar #'kimi--user-block->json images))
                          'vector)))))

(defun kimi--user->json (m)
  (let ((blocks (kimi--pget m :content)))
    (kimi--jobj "role" "user"
                "content" (if blocks
                              (map 'vector #'kimi--user-block->json blocks)
                              ;; content must not be empty
                              (vector (kimi--jobj "type" "text" "text" "(empty)"))))))

(defun kimi--messages->json (system messages deferred-tools)
  "System prompt, then the conversation, then — when tools showed up after the
session's first request — the dynamic-tools declaration, appended last so the
cached prefix does not move."
  (let ((out nil))
    (when system
      ;; supportsDeveloperRole is false: the system prompt is a system message.
      (push (kimi--jobj "role" "system" "content" system) out))
    (dolist (m messages)
      (push (ecase (evo.provider:message-role m)
              (:user (kimi--user->json m))
              (:assistant (kimi--assistant->json m))
              (:tool-result (kimi--tool-result->json m)))
            out)
      (when (eq (evo.provider:message-role m) :tool-result)
        (let ((images (kimi--tool-result-image-message m)))
          (when images (push images out)))))
    (when deferred-tools
      ;; A dynamic-tools message carries `tools` and no `content` — sending
      ;; both is a 400 ("cannot be used with content").
      (push (kimi--jobj "role" "system" "tools" (kimi--tools->json deferred-tools))
            out))
    (coerce (nreverse out) 'vector)))

;;; ---------------------------------------------------------------------------
;;; Request building
;;; ---------------------------------------------------------------------------

(defun kimi--build-request-json (&key model system messages tools thinking-level
                                      cache-key)
  (multiple-value-bind (core-tools deferred-tools)
      (kimi--split-tools cache-key tools)
    (let* ((effort (and (kimi--pget model :thinking)
                        (kimi--model-effort model thinking-level)))
           (req (kimi--jobj
                 "model" (kimi--pget model :id)
                 "messages" (kimi--messages->json system
                                                  (kimi--handoff messages model)
                                                  deferred-tools)
                 "stream" t
                 ;; Without this the final chunk carries no usage at all, and
                 ;; every turn would report zero tokens.
                 "stream_options" (kimi--jobj "include_usage" t))))
      (setf (gethash *kimi-max-tokens-field* req)
            (evo.provider:model-max-output model))
      (let ((jt (kimi--tools->json core-tools)))
        (when jt (setf (gethash "tools" req) jt)))
      (when effort
        (setf (gethash "reasoning_effort" req) effort))
      (when cache-key
        ;; Prefix-cache bucket.  The docs ask for a stable session/task id that
        ;; survives quit-and-resume — which is exactly evo's session id.
        (setf (gethash "prompt_cache_key" req) cache-key))
      (com.inuoe.jzon:stringify req))))

(defmethod evo:build-request ((api kimi-chat-completions-api)
                              &key model system messages tools thinking-level
                                   cache-key)
  (declare (ignore api))
  (kimi--build-request-json :model model :system system :messages messages
                            :tools tools :thinking-level thinking-level
                            :cache-key cache-key))

;;; ---------------------------------------------------------------------------
;;; SSE parsing
;;;
;;; One choice (n is fixed at 1).  Thinking arrives as `reasoning_content`
;;; deltas before any `content` delta; tool calls arrive as indexed fragments
;;; whose `arguments` accumulate like text.  The stream ends with
;;; `data: [DONE]`, and with include_usage the usage-only chunk comes after the
;;; finish_reason chunk — so the loop stops on [DONE], not on finish_reason, or
;;; the token counts would be lost.
;;; ---------------------------------------------------------------------------

(defstruct kimi-call id name (args ""))

(defun kimi--usage (obj)
  "Usage plist, or NIL.  prompt_tokens includes the cached tokens; evo's :input
excludes them, so unbundle.  K3's cache is automatic — nothing is ever billed
as a cache write."
  (let ((u (kimi--jget obj "usage")))
    (when (hash-table-p u)
      (let* ((prompt (kimi--jint u "prompt_tokens"))
             ;; Kimi reports cached tokens flat; OpenAI-compatible proxies
             ;; nest them under prompt_tokens_details.  Only one is ever set.
             (cached (max (kimi--jint u "cached_tokens")
                          (kimi--jint u "prompt_tokens_details" "cached_tokens")))
             (completion (kimi--jint u "completion_tokens")))
        (list :input (max 0 (- prompt cached))
              :output completion
              :cache-read cached
              :cache-write 0)))))

(defun kimi--error-text (obj)
  "Error text from a chunk that carries one, or NIL."
  (let ((err (kimi--jget obj "error")))
    (typecase err
      (hash-table (format nil "~a: ~a"
                          (or (kimi--jstring err "type") "error")
                          (or (kimi--jstring err "message") "(no message)")))
      (string (format nil "~a~@[: ~a~]" err (kimi--jstring obj "message")))
      (t nil))))

(defun kimi--stop-reason (finish content)
  "Unknown finish reasons are loud: a guess here is a silently wrong turn."
  (cond ((find :tool-call content :key (lambda (b) (kimi--pget b :type))) :tool-use)
        ((null finish) :stop)           ; truncated stream; the caller retries
        ((equal finish "stop") :stop)
        ((equal finish "length") :length)
        ((equal finish "tool_calls") :tool-use)
        (t (error 'evo:provider-error
                  :message (format nil "Unknown finish reason ~s" finish)))))

(defun kimi--parse-sse-stream (char-stream &key on-event abort-flag)
  "Parse a Kimi chat-completions SSE stream into the adapter result plist."
  (let ((text (make-string-output-stream))
        (thinking (make-string-output-stream))
        (calls (make-hash-table))       ; index -> kimi-call
        (max-index -1)
        (model nil) (finish nil) (stopped-p nil) (error-message nil)
        (usage nil) (started nil))
    (labels ((emit (&rest ev) (when on-event (funcall on-event ev)))
             (start ()
               (unless started
                 (setf started t)
                 (emit :type :message-start)))
             (call-at (index)
               (or (gethash index calls)
                   (setf max-index (max max-index index)
                         (gethash index calls) (make-kimi-call))))
             (handle-tool-calls (deltas)
               (loop for tc across deltas
                     for index = (kimi--jint tc "index")
                     for call = (call-at index)
                     do (let ((id (kimi--jstring tc "id"))
                              (name (kimi--jstring tc "function" "name"))
                              (args (kimi--jstring tc "function" "arguments")))
                          (when id (setf (kimi-call-id call) id))
                          (when name (setf (kimi-call-name call) name))
                          (when args
                            (setf (kimi-call-args call)
                                  (concatenate 'string (kimi-call-args call) args))))))
             (handle-choice (choice)
               (let ((delta (kimi--jget choice "delta")))
                 (when (hash-table-p delta)
                   (when (kimi--jstring delta "role") (start))
                   (let ((s (kimi--jstring delta "reasoning_content")))
                     (when (and s (plusp (length s)))
                       (start)
                       (write-string s thinking)
                       (emit :type :thinking-delta :text s)))
                   (let ((s (kimi--jstring delta "content")))
                     (when (and s (plusp (length s)))
                       (start)
                       (write-string s text)
                       (emit :type :text-delta :text s)))
                   (let ((tc (kimi--jget delta "tool_calls")))
                     (when (vectorp tc) (handle-tool-calls tc))))
                 ;; Some responses put usage on the choice rather than the chunk.
                 (setf usage (or (kimi--usage choice) usage))
                 (let ((reason (kimi--jstring choice "finish_reason")))
                   (when reason (setf finish reason stopped-p t)))))
             (handle (event-type data)
               (declare (ignore event-type))
               (cond
                 ((string= data "[DONE]") :stop)
                 (t
                  (let ((obj (ignore-errors (com.inuoe.jzon:parse data))))
                    (when (hash-table-p obj)
                      (start)
                      (setf model (or (kimi--jstring obj "model") model)
                            usage (or (kimi--usage obj) usage))
                      (let ((err (kimi--error-text obj)))
                        (when err (setf error-message err)))
                      (let ((choices (kimi--jget obj "choices")))
                        (when (vectorp choices)
                          (loop for choice across choices
                                do (handle-choice choice))))))
                  (when error-message :stop)))))
      (when (eq (evo:map-sse-events char-stream #'handle :abort-flag abort-flag)
                :aborted)
        (return-from kimi--parse-sse-stream
          (list :aborted-p t :content nil :stop-reason :aborted
                :usage (or usage (list :input 0 :output 0
                                       :cache-read 0 :cache-write 0)))))
      (let* ((thinking-text (get-output-stream-string thinking))
             (answer (get-output-stream-string text))
             (content
               (append
                (when (plusp (length thinking-text))
                  (list (list :type :thinking :thinking thinking-text)))
                (when (plusp (length answer))
                  (list (list :type :text :text answer)))
                (loop for i from 0 to max-index
                      for call = (gethash i calls)
                      when call
                        collect (let* ((raw (kimi-call-args call))
                                       (args (cond ((zerop (length raw)) nil)
                                                   (t (handler-case
                                                          (kimi--json->sexpr
                                                           (com.inuoe.jzon:parse raw))
                                                        (error () :parse-error))))))
                                  (append
                                   (list :type :tool-call
                                         :id (or (kimi-call-id call)
                                                 (format nil "call_~d" i))
                                         :name (kimi-call-name call))
                                   (if (eq args :parse-error)
                                       (list :arguments nil
                                             :arguments-error
                                             (evo.util:truncate-string raw 2000))
                                       (list :arguments args))))))))
        (list :content content
              :model model
              :stopped-p stopped-p
              :error-message error-message
              :stop-reason (kimi--stop-reason finish content)
              :usage (or usage (list :input 0 :output 0
                                     :cache-read 0 :cache-write 0)))))))

(defmethod evo:parse-stream ((api kimi-chat-completions-api) char-stream
                             &key on-event abort-flag)
  (declare (ignore api))
  (kimi--parse-sse-stream char-stream :on-event on-event :abort-flag abort-flag))

;;; ---------------------------------------------------------------------------
;;; Registration
;;;
;;; Static, and no network: everything here is documented model metadata, so
;;; the model is registered whether or not a key is present.  A missing key is
;;; a clear error at request time (AUTH-HEADERS), not a model that silently
;;; vanishes from the picker.
;;; ---------------------------------------------------------------------------

(evo:register-api *kimi-api-name* (make-instance 'kimi-chat-completions-api))

(defun kimi--register-endpoint ()
  "Register the endpoint, filling in only what config left out.
init.lisp is evaluated before extensions load and REGISTER-PROVIDER merges
field-wise with the later call winning — so registering unconditionally would
silently undo a base URL or key the user wrote in their own config.  Anything
already in the registry stays; anything missing gets the default.

Precedence, strongest first: init.lisp (or post-init.lisp) · the environment ·
the stock endpoint."
  (let ((entry (kimi--provider-entry)))
    (apply #'evo:register-provider *kimi-provider-key*
           (append
            (unless (kimi--pget entry :base-url)
              (list :base-url (kimi--base-url)))
            (unless (kimi--pget entry :api-key-env)
              (list :api-key-env *kimi-api-key-env*))
            ;; KIMI_API_KEY is a convenience alias; provider config reads
            ;; exactly one env var, so the alias is resolved here rather than
            ;; teaching the registry two names — and only when nothing more
            ;; explicit is set.
            (when (and (null (kimi--pget entry :api-key))
                       (null (kimi--env *kimi-api-key-env*))
                       (kimi--env "KIMI_API_KEY"))
              (list :api-key (kimi--env "KIMI_API_KEY")))))))

(kimi--register-endpoint)

(evo:register-model *kimi-model-id*
                    :provider *kimi-provider-key*
                    :api *kimi-api-name*
                    :context-window *kimi-context-window*
                    :max-output *kimi-max-output*
                    :thinking t
                    :vision t
                    :effort *kimi-effort-levels*)

;;; ---------------------------------------------------------------------------
;;; Slash commands
;;; ---------------------------------------------------------------------------

(defun kimi--mask (key)
  (cond ((null key) nil)
        ((<= (length key) 12) "set")
        (t (format nil "~a...~a" (subseq key 0 8) (subseq key (- (length key) 4))))))

(defun kimi--session-usage (agent)
  "Sum this session's kimi-k3 usage: (:input n :output n :cache-read n
:cache-write n :turns n)."
  (let ((totals (list :input 0 :output 0 :cache-read 0 :cache-write 0 :turns 0)))
    (dolist (m (evo.journal:state-messages
                (evo.journal:fold-state (evo.kernel:agent-journal agent)))
               totals)
      (when (and (eq (kimi--pget m :role) :assistant)
                 (eq (kimi--pget m :provider) *kimi-provider-key*)
                 (equal (kimi--pget m :model) *kimi-model-id*))
        (let ((u (kimi--pget m :usage)))
          (when u
            (incf (getf totals :turns))
            (dolist (bucket '(:input :output :cache-read :cache-write))
              (incf (getf totals bucket) (kimi--pget u bucket 0)))))))))

(defun kimi--cost (usage)
  "USD for USAGE at *KIMI-PRICING*."
  (/ (loop for bucket in '(:input :output :cache-read :cache-write)
           sum (* (kimi--pget usage bucket 0)
                  (kimi--pget *kimi-pricing* bucket 0)))
     1000000.0d0))

(evo:register-command "kimi:status"
  (lambda (ctx)
    (declare (ignore ctx))
    (let* ((config (ignore-errors (evo.provider:provider-config *kimi-provider-key*)))
           (key (or (kimi--trim (kimi--pget config :api-key)) (kimi--api-key)))
           (model (ignore-errors (evo.provider:find-model *kimi-model-id*
                                                          *kimi-provider-key*))))
      (with-output-to-string (s)
        (format s "kimi-provider diagnostics~%")
        (format s "  api: ~:[missing~;registered~] (~(~a~))~%"
                (member *kimi-api-name* (evo:api-keys)) *kimi-api-name*)
        (format s "  provider: ~(~s~) -> ~a~a~%" *kimi-provider-key*
                (if config (kimi--pget config :base-url) "unregistered")
                (if config "/v1/chat/completions" ""))
        (format s "  api key: ~:[missing (set ~a or KIMI_API_KEY)~;present (~:*~a)~]~%"
                (kimi--mask key) *kimi-api-key-env*)
        (format s "  model: ~:[unregistered~;~:*~a~] (ctx ~d, max out ~d)~%"
                (and model (kimi--pget model :id))
                *kimi-context-window* *kimi-max-output*)
        (format s "  reasoning_effort ladder: ~{~(~a~)~^ ~} (default max)~%"
                *kimi-effort-levels*)
        (format s "  thinking level map:~{ ~a~} (* = clamped, no rung of its own)~%"
                (loop for level in evo.provider:+effort-levels+
                      collect (format nil "~(~a~)->~:[~a*~;~:*~a~]"
                                      level (kimi--effort-string level)
                                      (and model (kimi--model-effort model level)))))
        (format s "  max-tokens field: ~a · tool strict: ~:[false~;true~]~%"
                *kimi-max-tokens-field* *kimi-strict-tools*)
        (format s "  deferred (dynamically loaded) tools: ~:[off~;on~]~%"
                *kimi-deferred-tools*)
        (format s "  pricing (USD/1M): in ~a · out ~a · cache read ~a · cache write ~a"
                (kimi--pget *kimi-pricing* :input) (kimi--pget *kimi-pricing* :output)
                (kimi--pget *kimi-pricing* :cache-read)
                (kimi--pget *kimi-pricing* :cache-write)))))
  :description "Show Kimi (Moonshot AI) provider diagnostics")

(evo:register-command "kimi:cost"
  (lambda (ctx)
    (let ((agent (kimi--pget ctx :agent)))
      (if (null agent)
          "No agent in this context."
          (let* ((usage (kimi--session-usage agent))
                 (turns (kimi--pget usage :turns)))
            (if (zerop turns)
                "No kimi-k3 turns in this session yet."
                (format nil (cat "kimi-k3 this session — ~d turn~:p~%"
                                 "~2tinput ~:d (+ ~:d cached) · output ~:d~%"
                                 "~2testimated cost $~,4f (USD/1M: in ~a, out ~a, cached in ~a)")
                        turns (kimi--pget usage :input) (kimi--pget usage :cache-read)
                        (kimi--pget usage :output) (kimi--cost usage)
                        (kimi--pget *kimi-pricing* :input)
                        (kimi--pget *kimi-pricing* :output)
                        (kimi--pget *kimi-pricing* :cache-read)))))))
  :description "Estimated cost of this session's kimi-k3 usage")
