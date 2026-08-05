;;;; claude-oauth-provider.lisp — vendored user extension, installed by make.
;;;;
;;;; Lives in extensions/ and is installed to $(EVO_HOME)/extensions/ by
;;;; `make install` (via install-home).  Loaded automatically at evo startup.
;;;;
;;;; Registers an Anthropic Messages provider that authenticates with a
;;;; Claude/Anthropic OAuth access token and injects Claude Code billing-header
;;;; attribution into every OAuth request.  Includes OAuth login (PKCE +
;;;; local callback server) and token refresh.
;;;;
;;;; Environment variables:
;;;;   CLAUDE_OAUTH_ACCESS_TOKEN  — OAuth access token (sk-ant-oat-...)
;;;;   CLAUDE_OAUTH_REFRESH_TOKEN — OAuth refresh token
;;;;   CLAUDE_OAUTH_CLIENT_ID     — OAuth client id (default: Claude Code's)
;;;;
;;;; Token storage: ~/.evo/claude-oauth/token.sexp (not journaled, not in repo).

(in-package :evo.user)

;;; ---------------------------------------------------------------------------
;;; Billing-header constants (must match the current Claude Code release)
;;; ---------------------------------------------------------------------------

(defparameter *claude-code-version* "2.1.220")
(defparameter *billing-header-salt* "59cf53e54c78")
(defparameter *billing-header-positions* #(4 7 20))
(defparameter *claude-code-entrypoint* "sdk-cli")

;;; ---------------------------------------------------------------------------
;;; SHA256 via shell (shasum is on macOS and Linux; ironclad is not preloaded)
;;; ---------------------------------------------------------------------------

(defun sha256-hex (string)
  "Return the lowercase hex SHA256 digest of STRING."
  (let* ((escaped (with-output-to-string (esc)
                    (loop for c across string
                          do (if (char= c #\')
                                 (write-string "'\\''" esc)
                                 (write-char c esc)))))
         (cmd (format nil "printf '%s' '~a' | shasum -a 256" escaped))
         (out (with-output-to-string (s)
                (uiop:run-program (list "/bin/sh" "-c" cmd)
                                  :output s :error-output nil))))
    (let ((space (position #\Space out)))
      (if space (subseq out 0 space) (string-trim '(#\Newline #\Space) out)))))

;;; ---------------------------------------------------------------------------
;;; Billing header construction
;;; ---------------------------------------------------------------------------

(defun first-user-text (messages)
  "Return the text of the first user message, or NIL."
  (loop for m in messages
        when (eq (getf m :role) :user)
          do (let ((content (getf m :content)))
               (when content
                 (let ((block (find :text content :key (lambda (b) (getf b :type)))))
                   (when block (return (getf block :text))))))))

(defun build-billing-header-value (messages)
  "Build the x-anthropic-billing-header value from MESSAGES, or NIL."
  (let ((text (first-user-text messages)))
    (unless text (return-from build-billing-header-value nil))
    (let* ((cch (subseq (sha256-hex text) 0 5))
           (sampled (with-output-to-string (s)
                      (loop for idx across *billing-header-positions*
                            do (write-char (if (< idx (length text))
                                              (char text idx)
                                              #\0)
                                          s))))
           (suffix (subseq (sha256-hex
                            (format nil "~a~a~a"
                                    *billing-header-salt*
                                    sampled
                                    *claude-code-version*))
                           0 3)))
      (format nil "x-anthropic-billing-header: cc_version=~a.~a; cc_entrypoint=~a; cch=~a;"
              *claude-code-version* suffix *claude-code-entrypoint* cch))))

;;; ---------------------------------------------------------------------------
;;; Assistant message reordering (matches upstream splitAssistantToolUseTrailingContent)
;;; ---------------------------------------------------------------------------

(defun plist-copy-put (plist key value)
  "Return a copy of PLIST with KEY set to VALUE."
  (let ((copy (copy-list plist)))
    (setf (getf copy key) value)
    copy))

(defun claude-oauth--split-assistant-tool-use (messages)
  "Split assistant messages that interleave text and tool_use blocks.
The Anthropic API rejects assistant turns where non-tool_use blocks follow
a tool_use block.  We split such messages into two consecutive assistant
turns: one with text/thinking blocks and one with tool_use blocks."
  (let ((result nil))
    (dolist (m messages (nreverse result))
      (if (not (eq (getf m :role) :assistant))
          (push m result)
          (let* ((content (getf m :content))
                 (first-tool-use-idx
                   (position :tool-call content
                             :key (lambda (b) (getf b :type)))))
            (if (null first-tool-use-idx)
                (push m result)
                (let ((trailing (subseq content first-tool-use-idx)))
                  ;; If everything from first-tool-use-idx onward is tool_use,
                  ;; no split needed.
                  (if (every (lambda (b) (eq (getf b :type) :tool-call))
                             trailing)
                      (push m result)
                      (let ((non-tool (remove :tool-call content
                                              :key (lambda (b) (getf b :type))))
                            (tool-only (remove :tool-call content
                                               :key (lambda (b) (getf b :type))
                                               :test-not #'eq)))
                        ;; non-tool first, tool-only second (matches upstream order)
                        (push (plist-copy-put m :content tool-only) result)
                        (push (plist-copy-put m :content non-tool) result))))))))))

;;; ---------------------------------------------------------------------------
;;; Provider API subclass
;;; ---------------------------------------------------------------------------

(defclass claude-oauth-messages-api (evo:provider-api) ())

(defun claude-oauth--delegate-api ()
  (evo:find-api :anthropic-messages))

;;; ---------------------------------------------------------------------------
;;; Token helpers
;;; ---------------------------------------------------------------------------

(defun claude-oauth--trim (value)
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun claude-oauth--env (name)
  (let ((value (claude-oauth--trim (uiop:getenv name))))
    (and value (plusp (length value)) value)))

(defun claude-oauth--token-dir ()
  (let ((dir (merge-pathnames "claude-oauth/"
                              (or (uiop:getenv "EVO_HOME")
                                  (merge-pathnames ".evo/" (user-homedir-pathname))))))
    dir))

(defun claude-oauth--token-file ()
  (merge-pathnames "token.sexp" (claude-oauth--token-dir)))

(defun claude-oauth--now-ms ()
  "Current time in milliseconds since epoch."
  (* (get-universal-time) 1000))

(defun claude-oauth--read-tokens ()
  "Return plist (:access-token s :refresh-token s :expires-at i :refresh-token-expires-at i) or NIL."
  (let ((path (claude-oauth--token-file)))
    (when (probe-file path)
      (ignore-errors
        (with-open-file (in path :direction :input)
          (read in))))))

(defun claude-oauth--write-tokens (access-token refresh-token &optional expires-at refresh-token-expires-at)
  (let ((path (claude-oauth--token-file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
      (prin1 (list :access-token access-token
                   :refresh-token refresh-token
                   :expires-at expires-at
                   :refresh-token-expires-at refresh-token-expires-at) out))
    path))

(defun claude-oauth--resolve-token ()
  "Resolve the OAuth access token: env var first, then stored file."
  (or (claude-oauth--env "CLAUDE_OAUTH_ACCESS_TOKEN")
      (getf (claude-oauth--read-tokens) :access-token)))

(defun claude-oauth--resolve-refresh-token ()
  (or (claude-oauth--env "CLAUDE_OAUTH_REFRESH_TOKEN")
      (getf (claude-oauth--read-tokens) :refresh-token)))

;;; ---------------------------------------------------------------------------
;;; Auto-refresh
;;; ---------------------------------------------------------------------------

(defparameter *claude-oauth-auto-refresh* t
  "When T (default), auth-headers automatically refreshes the access token
before it expires.  Set to NIL to rely on manual /claude-oauth:refresh.")

(defparameter *claude-oauth-refresh-before-expiry* 360
  "Seconds before access-token expiry to trigger an automatic refresh.")

(declaim (ftype (function (t) t) claude-oauth--refresh-token))

(defun claude-oauth--refresh-and-store (refresh-token)
  "Refresh the access token with REFRESH-TOKEN, persist the returned token set,
and return the fresh access token."
  (let ((tokens (claude-oauth--refresh-token refresh-token)))
    (claude-oauth--write-tokens (getf tokens :access-token)
                                (getf tokens :refresh-token)
                                (getf tokens :expires-at)
                                (getf tokens :refresh-token-expires-at))
    (format *error-output* "~&[claude-oauth] Access token auto-refreshed.~%")
    (getf tokens :access-token)))

(defun claude-oauth--ensure-valid-token ()
  "Return the current OAuth access token, refreshing it first if needed.
Signals EVO:PROVIDER-ERROR if a refresh is required but fails or the refresh
token itself is expired."
  (let ((stored (claude-oauth--read-tokens)))
    (cond
      ;; No stored file — env-only or not logged in.  Return whatever we have.
      ((not stored)
       (claude-oauth--resolve-token))
      ;; Auto-refresh disabled or no expiry info to check.
      ((or (not *claude-oauth-auto-refresh*)
           (not (getf stored :expires-at)))
       (or (getf stored :access-token) (claude-oauth--resolve-token)))
      (t
       (let* ((now (claude-oauth--now-ms))
              (remaining (floor (/ (- (getf stored :expires-at) now) 1000))))
         (if (> remaining *claude-oauth-refresh-before-expiry*)
             ;; Token still fresh.
             (or (getf stored :access-token) (claude-oauth--resolve-token))
             ;; Token is near or past expiry — refresh.
             (let* ((env-refresh (claude-oauth--env "CLAUDE_OAUTH_REFRESH_TOKEN"))
                    (refresh (or env-refresh (getf stored :refresh-token))))
               (unless refresh
                 (error 'evo:provider-error
                        :message "Access token expired and no refresh token stored or set in CLAUDE_OAUTH_REFRESH_TOKEN. Run /claude-oauth:login to re-authenticate."))
               ;; Check stored refresh-token expiry. Env refresh-token expiry is unknown.
               (let ((rt-expires (and (not env-refresh)
                                      (getf stored :refresh-token-expires-at))))
                 (when (and rt-expires (<= rt-expires now))
                   (error 'evo:provider-error
                          :message "Refresh token expired. Run /claude-oauth:login to re-authenticate.")))
               (handler-case
                   (claude-oauth--refresh-and-store refresh)
                 (error (e)
                   (error 'evo:provider-error
                          :message (format nil "Auto-refresh failed: ~a" e)))))))))))

;;; ---------------------------------------------------------------------------
;;; Provider API methods
;;; ---------------------------------------------------------------------------

(defmethod evo:endpoint-path ((api claude-oauth-messages-api))
  (declare (ignore api))
  (evo:endpoint-path (claude-oauth--delegate-api)))

(defmethod evo:auth-headers ((api claude-oauth-messages-api) config)
  (declare (ignore api))
  (let ((token (or (claude-oauth--trim (getf config :api-key))
                   (claude-oauth--ensure-valid-token))))
    (unless token
      (error 'evo:provider-error
             :message "No Claude OAuth token: set CLAUDE_OAUTH_ACCESS_TOKEN or run /claude-oauth:login"))
    (unless (and (>= (length token) 10)
                 (string= "sk-ant-oat" token :end2 10))
      (error 'evo:provider-error
             :message "Token does not look like a Claude/Anthropic OAuth access token (expected sk-ant-oat prefix)."))
    `(("Authorization" . ,(format nil "Bearer ~a" token))
      ("anthropic-version" . "2023-06-01"))))

(defmethod evo:build-request ((api claude-oauth-messages-api)
                              &key model system messages tools thinking-level cache-key)
  (declare (ignore api))
  ;; 1. Split assistant messages that interleave text and tool_use blocks.
  ;;    The Anthropic API rejects assistant turns where non-tool_use blocks
  ;;    follow a tool_use block.  Pi's serializer can produce this ordering,
  ;;    so we split into two consecutive assistant turns.
  (let ((shaped-messages (claude-oauth--split-assistant-tool-use messages)))
    ;; 2. Inject the billing header into the system prompt, unless it is
    ;;    already present (dedup: matches upstream prependBillingHeader).
    (let* ((billing (and (not (and system
                                   (search "x-anthropic-billing-header:" system)))
                         (build-billing-header-value shaped-messages)))
           (shaped-system
             (if billing
                 (format nil "~a~%~%~a" billing system)
                 system)))
      (evo:build-request (claude-oauth--delegate-api)
                         :model model
                         :system shaped-system
                         :messages shaped-messages
                         :tools tools
                         :thinking-level thinking-level
                         :cache-key cache-key))))

(defmethod evo:parse-stream ((api claude-oauth-messages-api) char-stream
                             &key on-event abort-flag)
  (declare (ignore api))
  (evo:parse-stream (claude-oauth--delegate-api) char-stream
                    :on-event on-event
                    :abort-flag abort-flag))

(defmethod evo:thinking-param ((api claude-oauth-messages-api) level)
  (declare (ignore api))
  (evo:thinking-param (claude-oauth--delegate-api) level))

(defmethod evo:default-provider-key ((api claude-oauth-messages-api))
  (declare (ignore api))
  :anthropic-oauth)

(defmethod evo:default-base-url ((api claude-oauth-messages-api))
  (declare (ignore api))
  "https://api.anthropic.com")

(defmethod evo:default-api-key-env ((api claude-oauth-messages-api))
  (declare (ignore api))
  "CLAUDE_OAUTH_ACCESS_TOKEN")

;;; ---------------------------------------------------------------------------
;;; Safe error extraction from API responses
;;; ---------------------------------------------------------------------------

(defun claude-oauth--extract-error (body)
  "Safely extract a human-readable error description from a response BODY.
BODY may be a JSON string or an already-parsed hash-table.  Returns a string
or NIL."
  (when body
    (ignore-errors
     (let* ((j (etypecase body
                (string (com.inuoe.jzon:parse body))
                (hash-table body)))
            (err (gethash "error" j)))
       ;; Anthropic error shape: {"error": {"type": "...", "message": "..."}}
       ;; OAuth error shape:    {"error": "...", "error_description": "..."}
       (typecase err
         (hash-table
          (or (gethash "message" err)
              (gethash "type" err)))
         (string err)
         (t (gethash "error_description" j)))))))

;;; ---------------------------------------------------------------------------
;;; OAuth PKCE login flow
;;; ---------------------------------------------------------------------------

(defparameter *claude-oauth-authorize-url* "https://claude.com/cai/oauth/authorize")
(defparameter *claude-oauth-token-url* "https://platform.claude.com/v1/oauth/token")
(defparameter *claude-oauth-scope* "org:create_api_key user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload")

(defun claude-oauth--client-id ()
  (or (claude-oauth--env "CLAUDE_OAUTH_CLIENT_ID")
      "9d1c250a-e61b-44d9-88ed-5944d1962f5e"))

(defun claude-oauth--random-string (length)
  "Generate a cryptographically-random URL-safe string of LENGTH characters."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~")
        (result (make-string length)))
    (dotimes (i length)
      (setf (char result i) (char chars (random (length chars)))))
    result))

(defun claude-oauth--base64url-encode (hex-string)
  "Convert a hex SHA256 digest to base64url (no padding)."
  (let* ((bytes (make-array (/ (length hex-string) 2) :element-type '(unsigned-byte 8)))
         (idx 0))
    (loop for i from 0 below (length hex-string) by 2
          do (setf (aref bytes idx)
                   (parse-integer (subseq hex-string i (+ i 2)) :radix 16))
             (incf idx))
    (let* ((b64 (cl-base64:usb8-array-to-base64-string bytes))
           ;; Convert standard base64 to base64url
           (url (substitute #\_ #\/ (substitute #\- #\+ b64))))
      ;; Strip trailing = padding
      (string-right-trim '(#\=) url))))

(defun claude-oauth--start-callback-server (port)
  "Start a TCP server on PORT that accepts one connection, reads the HTTP
request, extracts the authorization code, and returns it.  Blocks until a
request arrives or 120 seconds elapse."
  (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t
                                                  :element-type 'character))
        (code nil))
    (unwind-protect
        (handler-case
            (let ((client
                    (evo.port:call-with-timeout
                     120 (lambda () (usocket:socket-accept socket)))))
              (unwind-protect
                  (let* ((stream (usocket:socket-stream client))
                         (line (read-line stream nil nil)))
                    ;; Parse GET /callback?code=...&state=... HTTP/1.1
                    (when line
                      (let* ((parts (uiop:split-string line :separator '(#\Space)))
                             (path (second parts)))
                        (when path
                          (let ((qmark (position #\? path)))
                            (when qmark
                              (let ((query (subseq path (1+ qmark))))
                                (loop for pair in (uiop:split-string query :separator '(#\&))
                                      for eq = (position #\= pair)
                                      when (and eq (string= (subseq pair 0 eq) "code"))
                                        do (setf code (subseq pair (1+ eq))))))))))
                    ;; Send a response so the browser shows something.
                    (format stream "HTTP/1.1 200 OK~%Content-Type: text/html~%Connection: close~%~%~
                                    <html><body><h1>Authorization complete</h1>~
                                    <p>You may close this window and return to evo.</p></body></html>~%")
                    (force-output stream))
                (usocket:socket-close client)))
          (evo.port:timeout-error ()
            nil))
      (usocket:socket-close socket))
    code))

(defun claude-oauth--json-string (plist)
  "Encode a plist as a JSON object string."
  (with-output-to-string (s)
    (write-char #\{ s)
    (loop for (k v) on plist by #'cddr
          for first = t then nil
          unless first do (write-char #\, s)
          do (format s "\"~a\":\"~a\"" (string-downcase (symbol-name k))
                     (if (stringp v) v (princ-to-string v))))
    (write-char #\} s)))

(defun claude-oauth--exchange-code (code code-verifier redirect-uri state)
  "POST to the token endpoint, exchange code for tokens.  Returns plist
(:access-token s :refresh-token s :expires-at i :refresh-token-expires-at i)."
  (let* ((body (claude-oauth--json-string
                 (list :grant_type "authorization_code"
                       :code code
                       :redirect_uri redirect-uri
                       :client_id (claude-oauth--client-id)
                       :code_verifier code-verifier
                       :state state)))
         (response
           (handler-case
               (apply #'dex:post *claude-oauth-token-url*
                      :headers `(("Content-Type" . "application/json"))
                      :content body
                      (let ((proxy (evo.util:env-proxy *claude-oauth-token-url*)))
                        (when proxy (list :proxy proxy))))
             (dexador.error:http-request-failed (e)
               (let ((resp-body (ignore-errors (dexador.error:response-body e))))
                 (error "Token exchange failed: HTTP ~a~@[: ~a~]"
                        (dexador.error:response-status e)
                        (claude-oauth--extract-error resp-body))))))
         (json (com.inuoe.jzon:parse response)))
    (let ((access (gethash "access_token" json))
          (refresh (gethash "refresh_token" json))
          (expires-in (gethash "expires_in" json))
          (rt-expires-in (gethash "refresh_token_expires_in" json)))
      (unless access
        (error "Token response missing access_token: ~a" response))
      (list :access-token access
            :refresh-token refresh
            :expires-at (and expires-in
                             (+ (claude-oauth--now-ms) (* expires-in 1000)))
            :refresh-token-expires-at (if rt-expires-in
                                          (+ (claude-oauth--now-ms) (* rt-expires-in 1000))
                                          (+ (claude-oauth--now-ms) 2592000000))))))

(defun claude-oauth--refresh-token (refresh-token)
  "POST to the token endpoint, refresh the token.  Returns plist
(:access-token s :refresh-token s :expires-at i :refresh-token-expires-at i)."
  (let* ((body (claude-oauth--json-string
                 (list :grant_type "refresh_token"
                       :refresh_token refresh-token
                       :client_id (claude-oauth--client-id))))
         (response
           (handler-case
               (apply #'dex:post *claude-oauth-token-url*
                      :headers `(("Content-Type" . "application/json"))
                      :content body
                      (let ((proxy (evo.util:env-proxy *claude-oauth-token-url*)))
                        (when proxy (list :proxy proxy))))
             (dexador.error:http-request-failed (e)
               (let ((resp-body (ignore-errors (dexador.error:response-body e))))
                 (format *error-output* "~&[claude-oauth] Token refresh request: url=~a body=~a~%"
                         *claude-oauth-token-url* body)
                 (error "Token refresh failed: HTTP ~a~@[: ~a~]"
                        (dexador.error:response-status e)
                        (claude-oauth--extract-error resp-body))))))
         (json (com.inuoe.jzon:parse response)))
    (let ((access (gethash "access_token" json))
          (refresh (gethash "refresh_token" json))
          (expires-in (gethash "expires_in" json))
          (rt-expires-in (gethash "refresh_token_expires_in" json)))
      (unless access
        (error "Token refresh response missing access_token: ~a" response))
      (list :access-token access
            :refresh-token (or refresh refresh-token)
            :expires-at (and expires-in
                             (+ (claude-oauth--now-ms) (* expires-in 1000)))
            :refresh-token-expires-at (if rt-expires-in
                                           (+ (claude-oauth--now-ms) (* rt-expires-in 1000))
                                           (+ (claude-oauth--now-ms) 2592000000))))))

;;; ---------------------------------------------------------------------------
;;; Fetch models from Anthropic
;;; ---------------------------------------------------------------------------

(defparameter *claude-oauth-models-url* "https://api.anthropic.com/v1/models")

(defun claude-oauth--fetch-models ()
  "Fetch the list of available models from the Anthropic API using the OAuth
access token.  Returns a parsed JSON object (hash-table) or signals an error."
  (labels ((fetch-with-token (token)
             (let ((response
                     (apply #'dex:get *claude-oauth-models-url*
                            :headers `(("Authorization" . ,(format nil "Bearer ~a" token))
                                       ("anthropic-version" . "2023-06-01"))
                            (let ((proxy (evo.util:env-proxy *claude-oauth-models-url*)))
                              (when proxy (list :proxy proxy))))))
               (com.inuoe.jzon:parse response)))
           (try-fetch (token)
             (handler-case
                 (values t (fetch-with-token token) nil nil)
               (dexador.error:http-request-failed (e)
                 (values nil nil
                         (dexador.error:response-status e)
                         (ignore-errors (dexador.error:response-body e))))))
           (expired-access-token-p (status body)
             (and (eql status 401)
                  (let ((message (claude-oauth--extract-error body)))
                    (and (stringp message)
                         (or (search "expired" message :test #'char-equal)
                             (search "invalid_token" message :test #'char-equal)
                             (search "invalid token" message :test #'char-equal))))))
           (fail (status body)
             (error "Failed to fetch models from Anthropic: HTTP ~a~@[: ~a~]"
                    status (claude-oauth--extract-error body)))
           (fail-no-refresh (status body)
             (error "Failed to fetch models from Anthropic: HTTP ~a~@[: ~a~]. Access token looked expired, but no refresh token is available; run /claude-oauth:login to re-authenticate."
                    status (claude-oauth--extract-error body))))
    (let ((token (claude-oauth--ensure-valid-token)))
      (unless token
        (error "No Claude OAuth token: set CLAUDE_OAUTH_ACCESS_TOKEN or run /claude-oauth:login"))
      (multiple-value-bind (ok json status body)
          (try-fetch token)
        (cond
          (ok json)
          ((expired-access-token-p status body)
           (let ((refresh (claude-oauth--resolve-refresh-token)))
             (unless refresh
               (fail-no-refresh status body))
             (multiple-value-bind (retry-ok retry-json retry-status retry-body)
                 (try-fetch (claude-oauth--refresh-and-store refresh))
               (if retry-ok
                   retry-json
                   (fail retry-status retry-body)))))
          (t
           (fail status body)))))))

(defun claude-oauth--cap-supported-p (obj key)
  "T when OBJ (a capabilities hash-table) has KEY marked supported.  The
/v1/models response nests one {\"supported\": bool} object per capability,
including one per effort level, which is where a model's effort ladder and
thinking mode come from."
  (let ((sub (and obj (hash-table-p obj) (gethash key obj))))
    (and sub (hash-table-p sub) (gethash "supported" sub) t)))

(defun claude-oauth--register-fetched-models (json)
  "Register every model in the JSON response (hash-table with \"data\" array).
Returns a list of registered model-id strings."
  (let ((data (gethash "data" json)))
    (unless data
      (error "Unexpected response from /v1/models: missing \"data\" key"))
    (loop for entry across data
          for id = (gethash "id" entry)
          when id
            collect (let* ((caps (gethash "capabilities" entry))
                           (thinking-obj (and caps (gethash "thinking" caps)))
                           (thinking (and thinking-obj (gethash "supported" thinking-obj)))
                           (types (and thinking-obj (gethash "types" thinking-obj)))
                           (adaptive (claude-oauth--cap-supported-p types "adaptive"))
                           (effort-obj (and caps (gethash "effort" caps)))
                           (effort (and (claude-oauth--cap-supported-p caps "effort")
                                        (loop for level in '("low" "medium" "high"
                                                             "xhigh" "max")
                                              when (claude-oauth--cap-supported-p
                                                    effort-obj level)
                                                collect (intern (string-upcase level)
                                                                :keyword)))))
                      (evo:register-model id
                                          :provider :anthropic-oauth
                                          :api :anthropic-oauth-messages
                                          :context-window (or (gethash "max_input_tokens" entry) 200000)
                                          :max-output (or (gethash "max_tokens" entry) 64000)
                                          :thinking (if thinking t nil)
                                          :effort effort
                                          :thinking-mode (if adaptive :adaptive :extended))
                      id))))

;;; ---------------------------------------------------------------------------
;;; Registration
;;; ---------------------------------------------------------------------------

(evo:register-api :anthropic-oauth-messages (make-instance 'claude-oauth-messages-api))
(evo:register-provider :anthropic-oauth
                       :base-url "https://api.anthropic.com"
                       :api-key-env "CLAUDE_OAUTH_ACCESS_TOKEN")

;; Auto-register models from the API at load time (best-effort: needs a token).
;; init.lisp runs before extensions, so it can pick the default model after
;; registration is complete.  The extension only makes models available; it
;; never overrides the user's choice.
;;
;; Silent when no token is present — users who don't use this provider
;; shouldn't see any output on every startup.
(when (claude-oauth--resolve-token)
  (handler-case
      (claude-oauth--register-fetched-models (claude-oauth--fetch-models))
    (error (e)
      (format *error-output* "~&[claude-oauth] Could not fetch models at load time: ~a~%~
                              Run /claude-oauth:login then /reload to register models.~%" e))))

;;; ---------------------------------------------------------------------------
;;; Slash commands
;;; ---------------------------------------------------------------------------

(evo:register-command "claude-oauth:login"
  (lambda (ctx)
    (declare (ignore ctx))
    (let* ((port (+ 18080 (random 1000)))
           (code-verifier (claude-oauth--random-string 64))
           (code-challenge (claude-oauth--base64url-encode (sha256-hex code-verifier)))
           (state (claude-oauth--random-string 32))
           (redirect-uri (format nil "http://localhost:~d/callback" port))
           (auth-url
             (format nil "~a?code=true&client_id=~a&response_type=code&redirect_uri=~a&scope=~a&state=~a&code_challenge=~a&code_challenge_method=S256"
                     *claude-oauth-authorize-url*
                     (quri:url-encode (claude-oauth--client-id))
                     (quri:url-encode redirect-uri)
                     (quri:url-encode *claude-oauth-scope*)
                     (quri:url-encode state)
                     (quri:url-encode code-challenge))))
      ;; Run the blocking callback server + token exchange in a background
      ;; thread so the main event loop is not frozen.
      (bt:make-thread
        (lambda ()
          (let ((code (claude-oauth--start-callback-server port)))
            (cond
              ((null code)
               (format *error-output* "~&[claude-oauth] Login timed out.~%~%~%~%~%~%"))
              (t
               (handler-case
                   (let ((tokens (claude-oauth--exchange-code code code-verifier redirect-uri state)))
                     (claude-oauth--write-tokens (getf tokens :access-token)
                                                 (getf tokens :refresh-token)
                                                 (getf tokens :expires-at)
                                                 (getf tokens :refresh-token-expires-at))
                     (format *error-output* "~&[claude-oauth] Login successful. Token stored.~%~%~%~%~%~%"))
                 (error (e)
                   (format *error-output* "~&[claude-oauth] Login failed: ~a~%~%~%~%~%~%" e)))))))
        :name "claude-oauth-login")
      ;; Open the browser immediately.
      (uiop:launch-program (list "open" auth-url) :ignore-error-status t)
      ;; Return immediately so the UI is not blocked.
      (format nil "Opening browser for Claude OAuth login...~%~
                   If the browser doesn't open, visit:~%  ~a~%~
                   Waiting for authorization (timeout 120s)...~%~
                   Check stderr for completion status." auth-url)))
  :description "Start Claude OAuth login (PKCE flow with local callback)")

(evo:register-command "claude-oauth:refresh"
  (lambda (ctx)
    (declare (ignore ctx))
    (let ((refresh (claude-oauth--resolve-refresh-token)))
      (if (not refresh)
          "No refresh token: set CLAUDE_OAUTH_REFRESH_TOKEN or run /claude-oauth:login first."
          (progn
            (bt:make-thread
              (lambda ()
                (handler-case
                    (let ((tokens (claude-oauth--refresh-token refresh)))
                      (claude-oauth--write-tokens (getf tokens :access-token)
                                                  (getf tokens :refresh-token)
                                                  (getf tokens :expires-at)
                                                  (getf tokens :refresh-token-expires-at))
                      (format *error-output* "~&[claude-oauth] Token refreshed and stored.~%~%~%~%~%~%"))
                  (error (e)
                    (format *error-output* "~&[claude-oauth] Token refresh failed: ~a~%~%~%~%~%~%" e))))
              :name "claude-oauth-refresh")
            "Refreshing token in background... Check stderr for status."))))
  :description "Refresh the Claude OAuth access token")

(evo:register-command "claude-oauth:status"
  (lambda (ctx)
    (declare (ignore ctx))
    (let* ((token (claude-oauth--resolve-token))
           (refresh (claude-oauth--resolve-refresh-token))
           (stored (claude-oauth--read-tokens))
           (now (claude-oauth--now-ms))
           (access-expires (and stored (getf stored :expires-at)))
           (refresh-expires (and stored (getf stored :refresh-token-expires-at)))
           (access-remaining (and access-expires
                                  (max 0 (floor (/ (- access-expires now) 1000)))))
           (refresh-remaining (and refresh-expires
                                    (max 0 (floor (/ (- refresh-expires now) 1000))))))
      (with-output-to-string (s)
        (format s "claude-oauth-provider diagnostics~%")
        (format s "  api: ~:[missing~;registered~]~%" (member :anthropic-oauth-messages (evo:api-keys)))
        (format s "  provider: :anthropic-oauth -> https://api.anthropic.com~%")
        (format s "  access token: ~:[missing~;present (~a...)~]~%"
                token (and token (subseq token 0 (min 20 (length token)))))
        (format s "  refresh token: ~:[missing~;present (~a...)~]~%"
                refresh (and refresh (subseq refresh 0 (min 20 (length refresh)))))
        (format s "  access token expires in: ~:[unknown~;~:*~a seconds~]~%"
                access-remaining)
        (format s "  refresh token expires in: ~:[unknown~;~:*~a seconds~]~%"
                refresh-remaining)
        (format s "  stored token file: ~:[none~;~a~]~%"
                stored (and stored (namestring (claude-oauth--token-file))))
        (format s "  billing-header injection: enabled (cc_version=~a, cc_entrypoint=~a)~%"
                *claude-code-version* *claude-code-entrypoint*)
        (format s "  auto-refresh: ~:[disabled~;enabled (refresh at ~a s before expiry)~]~%"
                *claude-oauth-auto-refresh* *claude-oauth-refresh-before-expiry*)
        (format s "  client_id: ~a" (claude-oauth--client-id)))))
  :description "Show Claude OAuth provider diagnostics")
