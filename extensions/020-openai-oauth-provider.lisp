;;;; 020-openai-oauth-provider.lisp — vendored user extension, installed by make.
;;;;
;;;; Lives in extensions/ and is installed to $(EVO_HOME)/extensions/ by
;;;; `make install-home`.  Loaded automatically at evo startup, after
;;;; 020-claude-oauth-provider.lisp (load order is the file name).
;;;;
;;;; Registers an OpenAI OAuth provider for the GPT 5.6 / GPT 6 model series
;;;; over the kernel's own :openai-responses wire adapter.  There is no new
;;;; protocol here: the OAuth extension supplies the
;;;; Authorization/ChatGPT-Account-ID/originator/User-Agent headers, applies
;;;; the Codex Responses Lite request shape, runs PKCE login against
;;;; auth.openai.com, and stores and refreshes the token.
;;;;
;;;; What this registers
;;;;   api      :openai-oauth-responses — subclass of provider-api, delegates
;;;;            build-request / parse-stream / endpoint-path to :openai-responses
;;;;   provider :openai-oauth — https://chatgpt.com/backend-api/codex
;;;;   models   gpt-5.6-{sol,terra,luna} · gpt-6-{astra,sol,luna} · gpt-6.1-sol
;;;;            — registered statically, and only once a credential exists:
;;;;            272000 context, 16384 output, full effort ladder, vision.
;;;;
;;;; The wire body starts with the delegate's public Responses request, then
;;;; adopts the Codex Responses Lite layout: instructions and namespaced tools
;;;; are developer input items, parallel calls are disabled, max_output_tokens is
;;;; omitted, reasoning uses all-turn context without summaries, and
;;;; "reasoning.encrypted_content" stays in `include` for stateless replay.
;;;;
;;;; What was measured elsewhere / supplied by the user, not guessed here:
;;;;   authorize  https://auth.openai.com/oauth/authorize
;;;;   token      https://auth.openai.com/oauth/token
;;;;   client id  app_EMoamEEZ73f0CkXaXp7hrann
;;;;   callback   http://127.0.0.1:<port>/auth/callback
;;;;   account id id_token claim https://api.openai.com/auth.chatgpt_account_id
;;;;
;;;; Environment variables:
;;;;   OPENAI_OAUTH_ACCESS_TOKEN  — overrides the stored access token
;;;;   OPENAI_OAUTH_REFRESH_TOKEN — overrides the stored refresh token
;;;;   OPENAI_OAUTH_ACCOUNT_ID    — overrides the account id from the id_token
;;;;   OPENAI_OAUTH_CLIENT_ID     — overrides the client id
;;;;
;;;; Token storage: EVO_HOME/openai-oauth/token.sexp (0600, not journaled,
;;;; not in the repo).

(in-package :evo.user)

;; Defined later in this file (a config value the token-endpoint bodies need
;; before the credential section); declared so the compiler does not warn.
(declaim (ftype (function () t) openai-oauth--client-id))

;;; ---------------------------------------------------------------------------
;;; Constants (supplied; see the file header)
;;; ---------------------------------------------------------------------------

(defparameter *openai-oauth-authorize-url* "https://auth.openai.com/oauth/authorize")
(defparameter *openai-oauth-token-url* "https://auth.openai.com/oauth/token")
(defparameter *openai-oauth-default-client-id* "app_EMoamEEZ73f0CkXaXp7hrann")
(defparameter *openai-oauth-scope*
  "openid profile email offline_access api.connectors.read api.connectors.invoke")
(defparameter *openai-oauth-request-base-url* "https://chatgpt.com/backend-api/codex")
(defparameter *openai-oauth-originator* "codex_cli_rs")
(defparameter *openai-oauth-callback-path* "/auth/callback")
(defparameter *openai-oauth-account-claim* "https://api.openai.com/auth")

(defparameter *openai-oauth-models*
  '("gpt-5.6-sol" "gpt-5.6-terra" "gpt-5.6-luna"
    "gpt-6-astra" "gpt-6.1-sol" "gpt-6-sol" "gpt-6-luna")
  "The OpenAI OAuth models evo supports — the GPT 5.6 and GPT 6 series only.")

(defparameter *openai-oauth-context-window* 272000)
(defparameter *openai-oauth-max-output* 16384)

;;; ---------------------------------------------------------------------------
;;; SHA256 (PKCE S256).  Self-contained: the kernel ships no digest.
;;; ---------------------------------------------------------------------------

(defparameter *openai-oauth-sha256-k*
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
    #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
    #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
    #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
    #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
    #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
    #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
    #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun openai-oauth--sha256-hex (string)
  "Return the lowercase hex SHA256 digest of STRING's UTF-8 bytes."
  (let* ((input (flexi-streams:string-to-octets string :external-format :utf-8))
         (input-length (length input))
         (total-length (* 64 (ceiling (+ input-length 9) 64)))
         (message (make-array total-length :element-type '(unsigned-byte 8)
                                           :initial-element 0))
         (bit-length (* input-length 8))
         (words (make-array 64 :element-type '(unsigned-byte 32)
                               :initial-element 0))
         (h0 #x6a09e667) (h1 #xbb67ae85) (h2 #x3c6ef372) (h3 #xa54ff53a)
         (h4 #x510e527f) (h5 #x9b05688c) (h6 #x1f83d9ab) (h7 #x5be0cd19))
    (replace message input)
    (setf (aref message input-length) #x80)
    (dotimes (i 8)
      (setf (aref message (- total-length 1 i))
            (ldb (byte 8 (* i 8)) bit-length)))
    (labels ((u32 (value) (ldb (byte 32 0) value))
             (rotr (value amount)
               (u32 (logior (ash value (- amount)) (ash value (- 32 amount)))))
             (choose (x y z) (logxor (logand x y) (logand (lognot x) z)))
             (majority (x y z) (logxor (logand x y) (logand x z) (logand y z))))
      (loop for offset from 0 below total-length by 64
            do (dotimes (i 16)
                 (let ((base (+ offset (* i 4))))
                   (setf (aref words i)
                         (logior (ash (aref message base) 24)
                                 (ash (aref message (+ base 1)) 16)
                                 (ash (aref message (+ base 2)) 8)
                                 (aref message (+ base 3))))))
               (loop for i from 16 below 64
                     for x = (aref words (- i 15))
                     for y = (aref words (- i 2))
                     for s0 = (logxor (rotr x 7) (rotr x 18) (ash x -3))
                     for s1 = (logxor (rotr y 17) (rotr y 19) (ash y -10))
                     do (setf (aref words i)
                              (u32 (+ (aref words (- i 16)) s0
                                      (aref words (- i 7)) s1))))
               (let ((a h0) (b h1) (c h2) (d h3) (e h4) (f h5) (g h6) (h h7))
                 (dotimes (i 64)
                   (let* ((sum1 (logxor (rotr e 6) (rotr e 11) (rotr e 25)))
                          (temp1 (u32 (+ h sum1 (choose e f g)
                                          (aref *openai-oauth-sha256-k* i)
                                          (aref words i))))
                          (sum0 (logxor (rotr a 2) (rotr a 13) (rotr a 22)))
                          (temp2 (u32 (+ sum0 (majority a b c)))))
                     (setf h g g f f e e (u32 (+ d temp1))
                           d c c b b a a (u32 (+ temp1 temp2)))))
                 (setf h0 (u32 (+ h0 a)) h1 (u32 (+ h1 b))
                       h2 (u32 (+ h2 c)) h3 (u32 (+ h3 d))
                       h4 (u32 (+ h4 e)) h5 (u32 (+ h5 f))
                       h6 (u32 (+ h6 g)) h7 (u32 (+ h7 h))))))
    (string-downcase
     (format nil "~8,'0x~8,'0x~8,'0x~8,'0x~8,'0x~8,'0x~8,'0x~8,'0x"
             h0 h1 h2 h3 h4 h5 h6 h7))))

;;; ---------------------------------------------------------------------------
;;; base64url (PKCE challenge, JWT segments)
;;; ---------------------------------------------------------------------------

(defun openai-oauth--base64url-encode (bytes)
  "Encode an octet vector as base64url without padding."
  (let ((b64 (cl-base64:usb8-array-to-base64-string bytes)))
    (string-right-trim '(#\=)
                       (substitute #\_ #\/ (substitute #\- #\+ b64)))))

(defun openai-oauth--base64url-of-hex (hex-string)
  "Convert a hex digest to base64url without padding."
  (let* ((bytes (make-array (/ (length hex-string) 2)
                            :element-type '(unsigned-byte 8)))
         (idx 0))
    (loop for i from 0 below (length hex-string) by 2
          do (setf (aref bytes idx)
                   (parse-integer (subseq hex-string i (+ i 2)) :radix 16))
             (incf idx))
    (openai-oauth--base64url-encode bytes)))

(defun openai-oauth--base64url-decode (text)
  "Decode base64url TEXT (with or without padding) to a string."
  (let* ((standard (substitute #\+ #\- (substitute #\/ #\_ text)))
         (padded (concatenate 'string standard
                              (make-string (mod (- 4 (mod (length standard) 4)) 4)
                                           :initial-element #\=)))
         (bytes (cl-base64:base64-string-to-usb8-array padded)))
    (flexi-streams:octets-to-string bytes :external-format :utf-8)))

;;; ---------------------------------------------------------------------------
;;; PKCE / state
;;; ---------------------------------------------------------------------------

(defun openai-oauth--random-string (octets)
  "Base64url text from OCTETS bytes supplied by the OS entropy source."
  (openai-oauth--base64url-encode (evo.port:random-octets octets)))

(defun openai-oauth--code-challenge (code-verifier)
  "PKCE S256: base64url(sha256(verifier)) without padding."
  (openai-oauth--base64url-of-hex (openai-oauth--sha256-hex code-verifier)))

(defun openai-oauth--redirect-uri (port)
  (format nil "http://127.0.0.1:~d~a" port *openai-oauth-callback-path*))

;;; ---------------------------------------------------------------------------
;;; URL / query handling (callback)
;;; ---------------------------------------------------------------------------

(defun openai-oauth--url-decode (text)
  "Percent-decode TEXT, '+' as space.  A literal non-ASCII character is kept;
percent escapes are bytes and the result is decoded as UTF-8."
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8)
                                          :fill-pointer 0 :adjustable t)))
    (loop with i = 0 with n = (length text)
          while (< i n)
          for ch = (char text i)
          do (cond
               ((char= ch #\+)
                (vector-push (char-code #\Space) octets)
                (incf i))
               ((and (char= ch #\%) (< (+ i 2) n)
                     (digit-char-p (char text (1+ i)) 16)
                     (digit-char-p (char text (+ i 2)) 16))
                (vector-push (parse-integer text :start (1+ i) :end (+ i 3) :radix 16)
                             octets)
                (incf i 3))
               (t
                (let ((code (char-code ch)))
                  (if (< code 128)
                      (vector-push-extend code octets)
                      (loop for b across (flexi-streams:string-to-octets
                                          (string ch) :external-format :utf-8)
                            do (vector-push-extend b octets)))
                  (incf i)))))
    (flexi-streams:octets-to-string
     (coerce octets '(simple-array (unsigned-byte 8) (*)))
     :external-format :utf-8)))

(defun openai-oauth--query-pairs (query)
  "QUERY as an alist of (NAME . VALUE), percent-decoded."
  (loop for pair in (uiop:split-string query :separator '(#\&))
        for eq = (position #\= pair)
        when eq
          collect (cons (openai-oauth--url-decode (subseq pair 0 eq))
                        (openai-oauth--url-decode (subseq pair (1+ eq))))))

(defun openai-oauth--callback-result (request-target expected-state)
  "Validate a callback REQUEST-TARGET (path?query) against EXPECTED-STATE.
Returns (values CODE ERROR): CODE the authorization code on success, otherwise
NIL with a short reason.  A missing or mismatched state is refused outright —
an unvalidated state is a CSRF hole."
  (let* ((qmark (position #\? request-target))
         (path (if qmark (subseq request-target 0 qmark) request-target))
         (query (if qmark (subseq request-target (1+ qmark)) "")))
    (if (not (string= path *openai-oauth-callback-path*))
        (values nil "unexpected callback path")
        (let* ((pairs (openai-oauth--query-pairs query))
               (state (cdr (assoc "state" pairs :test #'equal)))
               (code (cdr (assoc "code" pairs :test #'equal)))
               (oauth-error (cdr (assoc "error" pairs :test #'equal))))
          (cond
            ((null state) (values nil "no state in the callback"))
            ((not (and expected-state (string= state expected-state)))
             (values nil "state mismatch — refusing the callback"))
            (oauth-error
             (values nil (format nil "authorization error: ~a" oauth-error)))
            ((or (null code) (zerop (length code)))
             (values nil "no authorization code in the callback"))
            (t (values code nil)))))))

;;; ---------------------------------------------------------------------------
;;; JSON helpers
;;; ---------------------------------------------------------------------------

(defun openai-oauth--json-object (pairs)
  "PAIRS as a hash-table jzon stringifies to a JSON object."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair pairs) (setf (gethash (car pair) table) (cdr pair)))
    table))

(defun openai-oauth--form-encode (pairs)
  "PAIRS as an application/x-www-form-urlencoded body."
  (format nil "~{~a~^&~}"
          (mapcar (lambda (pair)
                    (format nil "~a=~a"
                            (quri:url-encode (car pair))
                            (quri:url-encode (cdr pair))))
                  pairs)))

(defun openai-oauth--exchange-body (code verifier redirect-uri)
  "The authorization-code grant body, form encoded."
  (openai-oauth--form-encode
   (list (cons "grant_type" "authorization_code")
         (cons "code" code)
         (cons "redirect_uri" redirect-uri)
         (cons "client_id" (openai-oauth--client-id))
         (cons "code_verifier" verifier))))

(defun openai-oauth--refresh-body (refresh-token)
  "The refresh-token grant body, JSON encoded."
  (com.inuoe.jzon:stringify
   (openai-oauth--json-object
    (list (cons "grant_type" "refresh_token")
          (cons "refresh_token" refresh-token)
          (cons "client_id" (openai-oauth--client-id))))))

(defun openai-oauth--extract-error (body)
  "A safe, short description of a token-endpoint error BODY (string or parsed),
never a token.  Returns a string or NIL."
  (when body
    (ignore-errors
      (let* ((json (etypecase body
                     (string (evo.util:parse-json body))
                     (hash-table body)))
             (err (gethash "error" json))
             (description (gethash "error_description" json)))
        (evo.util:truncate-string
         (or description
             (typecase err
               (hash-table (or (gethash "message" err) (gethash "type" err)))
               (string err)))
         300)))))

;;; ---------------------------------------------------------------------------
;;; Time
;;; ---------------------------------------------------------------------------

(defun openai-oauth--now-ms ()
  "Current time in milliseconds since the Unix epoch."
  (* 1000 (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0))))

;;; ---------------------------------------------------------------------------
;;; Token store
;;; ---------------------------------------------------------------------------

(defun openai-oauth--trim (value)
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun openai-oauth--env (name)
  (ignore-errors (openai-oauth--trim (uiop:getenv name))))

(defun openai-oauth--token-dir ()
  (merge-pathnames "openai-oauth/" (evo.util:evo-home)))

(defun openai-oauth--token-file ()
  (merge-pathnames "token.sexp" (openai-oauth--token-dir)))

(defun openai-oauth--read-tokens ()
  "The stored token plist, or NIL.  (:access-token :refresh-token :id-token
:account-id :expires-at :refresh-token-expires-at)."
  (let ((path (openai-oauth--token-file)))
    (when (probe-file path)
      (ignore-errors
        (with-open-file (in path :direction :input)
          (let ((*read-eval* nil))
            (read in)))))))

(defvar *openai-oauth-token-lock* (bt:make-lock "openai-oauth-token")
  "Serializes token refresh and persistence, so a manual /openai-oauth:refresh
and an automatic refresh from AUTH-HEADERS cannot spend the single-use refresh
token twice or let an older write win.")

(defun openai-oauth--write-tokens (access-token refresh-token id-token account-id
                                   expires-at refresh-token-expires-at)
  "Persist the token set atomically and owner-only: a 0600 temp file in the
same directory, then a rename over the target, so a concurrent reader sees the
old file or the new one and the secret is never world-readable."
  (let* ((path (openai-oauth--token-file))
         (temporary (merge-pathnames (format nil "token.~a.tmp" (random 100000000))
                                     (uiop:pathname-directory-pathname path))))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (evo.port:write-private-file
            temporary
            (with-standard-io-syntax
              (let ((*print-pretty* nil))
                (prin1-to-string
                 (list :access-token access-token
                       :refresh-token refresh-token
                       :id-token id-token
                       :account-id account-id
                       :expires-at expires-at
                       :refresh-token-expires-at refresh-token-expires-at)))))
           (uiop:rename-file-overwriting-target temporary path)
           (evo.port:chmod-private path))
      (when (probe-file temporary) (ignore-errors (delete-file temporary))))
    path))

;;; ---------------------------------------------------------------------------
;;; JWT (id_token claim, access-token expiry)
;;; ---------------------------------------------------------------------------

(defun openai-oauth--jwt-payload (token)
  "The decoded payload of a JWT TOKEN as a JSON hash-table, or NIL."
  (when (and (stringp token) (plusp (length token)))
    (let ((parts (uiop:split-string token :separator '(#\.))))
      (when (>= (length parts) 2)
        (ignore-errors
          (let ((payload (evo.util:parse-json (openai-oauth--base64url-decode
                                               (second parts)))))
            (and (hash-table-p payload) payload)))))))

(defun openai-oauth--id-token-account-id (id-token)
  "The chatgpt_account_id claim of ID-TOKEN, or NIL."
  (let* ((payload (openai-oauth--jwt-payload id-token))
         (auth (and payload (gethash *openai-oauth-account-claim* payload))))
    (and (hash-table-p auth) (gethash "chatgpt_account_id" auth))))

(defun openai-oauth--jwt-expiry (token)
  "The `exp` claim of TOKEN as epoch milliseconds, or NIL."
  (let* ((payload (openai-oauth--jwt-payload token))
         (exp (and payload (gethash "exp" payload))))
    (and (integerp exp) (* exp 1000))))

(defun openai-oauth--tokens-from-json (json &optional previous)
  "A token plist from a token-endpoint JSON response, carrying PREVIOUS values
forward when the response omits them (a refresh need not rotate every field)."
  (let* ((access (gethash "access_token" json))
         (refresh (gethash "refresh_token" json))
         (id-token (gethash "id_token" json))
         (expires-in (gethash "expires_in" json)))
    (unless (and (stringp access) (plusp (length access)))
      (error "OpenAI OAuth token response carried no access_token"))
    (list :access-token access
          :refresh-token (or refresh (and previous (getf previous :refresh-token)))
          :id-token (or id-token (and previous (getf previous :id-token)))
          :account-id (or (openai-oauth--id-token-account-id id-token)
                          (and previous (getf previous :account-id)))
          :expires-at (or (and (numberp expires-in)
                               (+ (openai-oauth--now-ms) (* expires-in 1000)))
                          (openai-oauth--jwt-expiry access)
                          (and previous (getf previous :expires-at)))
          :refresh-token-expires-at (and previous (getf previous :refresh-token-expires-at)))))

;;; ---------------------------------------------------------------------------
;;; Credential resolution
;;; ---------------------------------------------------------------------------

(defun openai-oauth--client-id ()
  (or (openai-oauth--env "OPENAI_OAUTH_CLIENT_ID")
      *openai-oauth-default-client-id*))

(defun openai-oauth--resolve-access-token ()
  (or (openai-oauth--env "OPENAI_OAUTH_ACCESS_TOKEN")
      (getf (openai-oauth--read-tokens) :access-token)))

(defun openai-oauth--resolve-refresh-token ()
  (or (openai-oauth--env "OPENAI_OAUTH_REFRESH_TOKEN")
      (getf (openai-oauth--read-tokens) :refresh-token)))

(defun openai-oauth--resolve-account-id ()
  (or (openai-oauth--env "OPENAI_OAUTH_ACCOUNT_ID")
      (getf (openai-oauth--read-tokens) :account-id)))

;;; ---------------------------------------------------------------------------
;;; Token endpoint
;;; ---------------------------------------------------------------------------

(defun openai-oauth--redact (text secrets)
  "Remove known request secrets from an OAuth error string."
  (let ((text text))
    (dolist (secret secrets text)
      (when (and text (stringp secret) (plusp (length secret)))
        (setf text (evo.util:string-replace secret "<redacted>" text :all t))))))

(defun openai-oauth--post-token (body content-type &rest secrets)
  "POST BODY to the token endpoint.  Returns the response string; converts an
HTTP failure into a safe message with request SECRETS redacted."
  (handler-case
      (evo:with-proxy (proxy *openai-oauth-token-url*)
        (apply #'dex:post *openai-oauth-token-url*
               :headers (list (cons "Content-Type" content-type))
               :content body
               (when proxy (list :proxy proxy))))
    (dexador.error:http-request-failed (e)
      (error "OpenAI OAuth token request failed: HTTP ~a~@[: ~a~]"
             (dexador.error:response-status e)
             (openai-oauth--redact
              (openai-oauth--extract-error
               (ignore-errors (dexador.error:response-body e)))
              secrets)))))

(defun openai-oauth--exchange-code (code verifier redirect-uri)
  "Exchange an authorization CODE for tokens.  Form encoded, as the endpoint
requires for a code grant."
  (openai-oauth--tokens-from-json
   (evo.util:parse-json
    (openai-oauth--post-token (openai-oauth--exchange-body code verifier redirect-uri)
                              "application/x-www-form-urlencoded"
                              code verifier))))

(defun openai-oauth--refresh-token (refresh-token &optional previous)
  "Refresh the access token.  JSON encoded, as the endpoint requires for a
refresh grant."
  (openai-oauth--tokens-from-json
   (evo.util:parse-json
    (openai-oauth--post-token (openai-oauth--refresh-body refresh-token)
                              "application/json"
                              refresh-token))
   previous))

;;; ---------------------------------------------------------------------------
;;; Auto-refresh
;;; ---------------------------------------------------------------------------

(defparameter *openai-oauth-auto-refresh* t
  "When T (default), AUTH-HEADERS refreshes the access token before it expires.
Set to NIL to rely on manual /openai-oauth:refresh, and on the environment.")

(defparameter *openai-oauth-refresh-before-expiry* 300
  "Seconds before access-token expiry to trigger an automatic refresh.")

(defun openai-oauth--fresh-p (stored)
  (let ((expires-at (and stored (getf stored :expires-at))))
    (and expires-at
         (> (floor (/ (- expires-at (openai-oauth--now-ms)) 1000))
            *openai-oauth-refresh-before-expiry*))))

(defun openai-oauth--refresh-and-store (refresh-token)
  "Refresh, persist the returned token set, and return the fresh access token.
Serialized, and re-reads the stored set under the lock first: a concurrent
refresh may already have rotated it, and spending the single-use token again
would invalidate the newer one."
  (bt:with-lock-held (*openai-oauth-token-lock*)
    (let ((stored (openai-oauth--read-tokens)))
      (if (and stored (openai-oauth--fresh-p stored))
          (getf stored :access-token)
          (let ((tokens (openai-oauth--refresh-token refresh-token stored)))
            (openai-oauth--write-tokens (getf tokens :access-token)
                                        (getf tokens :refresh-token)
                                        (getf tokens :id-token)
                                        (getf tokens :account-id)
                                        (getf tokens :expires-at)
                                        (getf tokens :refresh-token-expires-at))
            (format *error-output* "~&[openai-oauth] Access token refreshed.~%")
            (getf tokens :access-token))))))

(defun openai-oauth--ensure-valid-token ()
  "The current access token, refreshed first when near expiry.  When proactive
refresh cannot replace a stored token, return that token and let the provider
report a normal authorization failure.  An explicit OPENAI_OAUTH_ACCESS_TOKEN
wins and is never refreshed."
  (or (openai-oauth--env "OPENAI_OAUTH_ACCESS_TOKEN")
      (let ((stored (openai-oauth--read-tokens)))
        (cond
          ((null stored) nil)
          ((or (not *openai-oauth-auto-refresh*) (not (getf stored :expires-at)))
           (getf stored :access-token))
          ((openai-oauth--fresh-p stored) (getf stored :access-token))
          (t
           (let ((refresh (or (openai-oauth--env "OPENAI_OAUTH_REFRESH_TOKEN")
                              (getf stored :refresh-token))))
             (if (not refresh)
                 (progn
                   (format *error-output*
                           "~&[openai-oauth] Access token is stale and no refresh token is available.~%")
                   (getf stored :access-token))
                 (handler-case
                     (openai-oauth--refresh-and-store refresh)
                   (error (e)
                     ;; Keep the process alive and let the request report a normal
                     ;; authorization failure, as Codex does when proactive refresh
                     ;; cannot replace a still-present access token.
                     (format *error-output* "~&[openai-oauth] Auto-refresh failed: ~a~%" e)
                     (getf stored :access-token))))))))))

;;; ---------------------------------------------------------------------------
;;; Provider API: a credential wrapper over :openai-responses
;;; ---------------------------------------------------------------------------

(defclass openai-oauth-responses-api (evo:provider-api) ())

(defun openai-oauth--delegate-api ()
  (evo:find-api :openai-responses))

(defun openai-oauth--user-agent ()
  "A plain evo identity.  Deliberately NOT a Codex version string: this client
is evo, and the endpoint is asked to serve it as such."
  (format nil "~a (openai-oauth)" evo.port:*program-name*))

(defmethod evo:endpoint-path ((api openai-oauth-responses-api))
  (declare (ignore api))
  (evo:endpoint-path (openai-oauth--delegate-api)))

(defmethod evo:auth-headers ((api openai-oauth-responses-api) config)
  (declare (ignore api))
  (let ((token (or (openai-oauth--trim (getf config :api-key))
                   (openai-oauth--ensure-valid-token))))
    (unless token
      (error 'evo:provider-error
             :message (cat "No OpenAI OAuth token: set OPENAI_OAUTH_ACCESS_TOKEN "
                           "or run /openai-oauth:login")))
    (let ((account (openai-oauth--resolve-account-id)))
      (append
       `(("Authorization" . ,(format nil "Bearer ~a" token)))
       (when account `(("ChatGPT-Account-ID" . ,account)))
       `(("originator" . ,*openai-oauth-originator*)
         ("x-openai-internal-codex-responses-lite" . "true")
         ("User-Agent" . ,(openai-oauth--user-agent)))))))

(defmethod evo:build-request ((api openai-oauth-responses-api)
                              &key model system messages tools thinking-level)
  (declare (ignore api))
  ;; The ChatGPT/Codex route uses Responses Lite: instructions and tools are
  ;; developer input items, function tools live in the `functions` namespace,
  ;; parallel calls are off, and public-API output-token controls are omitted.
  (let* ((options (evo.util:pget model :responses-options))
         (options (cond ((stringp options) (evo.util:parse-json options))
                        ((hash-table-p options) options)))
         (configured-reasoning (and options (gethash "reasoning" options)))
         (explicit-summary-p (and (hash-table-p configured-reasoning)
                                  (nth-value 1 (gethash "summary" configured-reasoning))))
         (request (evo.util:parse-json
                   (evo:build-request (openai-oauth--delegate-api)
                                      :model model :system nil :messages messages
                                      :tools tools :thinking-level thinking-level)))
         (wire-tools (gethash "tools" request))
         (prefix #()))
    (when system
      (let ((item (make-hash-table :test #'equal))
            (part (make-hash-table :test #'equal)))
        (setf (gethash "type" part) "input_text"
              (gethash "text" part) system
              (gethash "type" item) "message"
              (gethash "role" item) "developer"
              (gethash "content" item) (vector part)
              prefix (vector item))))
    (remhash "max_output_tokens" request)
    (let ((reasoning (gethash "reasoning" request)))
      (when (hash-table-p reasoning)
        (unless explicit-summary-p (remhash "summary" reasoning))
        (setf (gethash "context" reasoning) "all_turns")))
    (let ((function-tools nil) (other-tools nil))
      (when (vectorp wire-tools)
        (loop for tool across wire-tools
              if (member (gethash "type" tool) '("function" "custom") :test #'equal)
                do (push tool function-tools)
              else do (push tool other-tools)))
      (when (or function-tools other-tools)
        (let ((additional-tools (coerce (nreverse other-tools) 'vector))
              (additional (make-hash-table :test #'equal)))
          (when function-tools
            (let ((namespace (make-hash-table :test #'equal)))
              (setf (gethash "type" namespace) "namespace"
                    (gethash "name" namespace) "functions"
                    (gethash "description" namespace) ""
                    (gethash "tools" namespace) (coerce (nreverse function-tools) 'vector)
                    additional-tools (concatenate 'vector (vector namespace) additional-tools))))
          (setf (gethash "type" additional) "additional_tools"
                (gethash "role" additional) "developer"
                (gethash "tools" additional) additional-tools
                prefix (concatenate 'vector (vector additional) prefix))))
      (remhash "tools" request))
    (when (plusp (length prefix))
      (setf (gethash "input" request)
            (concatenate 'vector prefix (gethash "input" request))))
    (setf (gethash "tool_choice" request) "auto"
          (gethash "parallel_tool_calls" request) nil)
    (com.inuoe.jzon:stringify request)))

(defmethod evo:parse-stream ((api openai-oauth-responses-api) char-stream
                             &key on-event abort-flag)
  (declare (ignore api))
  (evo:parse-stream (openai-oauth--delegate-api) char-stream
                    :on-event on-event :abort-flag abort-flag))

(defmethod evo:default-provider-key ((api openai-oauth-responses-api))
  (declare (ignore api))
  :openai-oauth)

(defmethod evo:default-base-url ((api openai-oauth-responses-api))
  (declare (ignore api))
  *openai-oauth-request-base-url*)

(defmethod evo:default-api-key-env ((api openai-oauth-responses-api))
  (declare (ignore api))
  "OPENAI_OAUTH_ACCESS_TOKEN")

(defmethod evo:api-credentials-available-p ((api openai-oauth-responses-api) registration)
  "Readiness reads configuration only — no network, and never a refresh, which
would spend a single-use refresh token to answer a question about a session
that may never run.  The token itself is never returned."
  (declare (ignore api))
  (and (or (openai-oauth--env "OPENAI_OAUTH_ACCESS_TOKEN")
           (openai-oauth--trim (evo.util:pget registration :api-key))
           (getf (openai-oauth--read-tokens) :access-token))
       t))

;;; ---------------------------------------------------------------------------
;;; Registration
;;; ---------------------------------------------------------------------------

(evo:register-api :openai-oauth-responses (make-instance 'openai-oauth-responses-api))

(defun openai-oauth--register-endpoint ()
  "Register the endpoint, filling in only what config left out.  init.lisp is
evaluated before extensions load, and re-registration merges field-wise, so an
unconditional call would silently undo a base URL the user wrote themselves."
  (let ((registered (evo:provider-registration :openai-oauth)))
    (apply #'evo:register-provider :openai-oauth
           (append (unless (evo.util:pget registered :base-url)
                     (list :base-url *openai-oauth-request-base-url*))
                   (unless (evo.util:pget registered :api-key-env)
                     (list :api-key-env "OPENAI_OAUTH_ACCESS_TOKEN"))))))

(openai-oauth--register-endpoint)

(defun openai-oauth--has-key-p ()
  "Whether an OpenAI OAuth credential is available right now."
  (evo:api-credentials-available-p (evo:find-api :openai-oauth-responses)
                                   (evo:provider-registration :openai-oauth)))

(defun openai-oauth--register-models ()
  "Register the bundled models once a credential is available."
  (when (openai-oauth--has-key-p)
    (dolist (id *openai-oauth-models*)
      (evo:register-model id
                          :provider :openai-oauth
                          :api :openai-oauth-responses
                          :context-window *openai-oauth-context-window*
                          :max-output *openai-oauth-max-output*
                          :effort t :vision t))))

(openai-oauth--register-models)

;;; ---------------------------------------------------------------------------
;;; Login flow
;;; ---------------------------------------------------------------------------

(define-condition openai-oauth--login-cancelled (serious-condition) ()
  (:documentation "Interrupts a login thread parked on the callback server, so
disposal can join it without waiting out the browser timeout."))

(defun openai-oauth--open-browser (url)
  "Open URL in the platform's default browser, returning NIL if no launcher."
  (let* ((program-name (cond ((evo.port:windows-p) "rundll32.exe")
                             ((uiop:os-macosx-p) "open")
                             (t "xdg-open")))
         (program (evo.port:program-in-path program-name)))
    (when program
      (uiop:launch-program
       (if (evo.port:windows-p)
           (list (namestring program) "url.dll,FileProtocolHandler" url)
           (list (namestring program) url))
       :input nil :output nil :error-output nil :ignore-error-status t))))

(defun openai-oauth--listen-callback ()
  "Bind one of the two loopback ports registered for Codex's public OAuth client."
  (or (loop for port in '(1455 1457)
            for socket = (handler-case
                             (usocket:socket-listen "127.0.0.1" port
                                                    :reuse-address t
                                                    :element-type 'character)
                           (error () nil))
            when socket return (list socket port))
      (error 'evo:provider-error
             :message "OpenAI OAuth login could not bind callback port 1455 or 1457.")))

(defun openai-oauth--choose-port ()
  "The preferred loopback port registered for Codex's public OAuth client."
  1455)

(defun openai-oauth--authorize-url (redirect-uri state code-challenge)
  (format nil "~a?~a"
          *openai-oauth-authorize-url*
          (openai-oauth--form-encode
           (list (cons "client_id" (openai-oauth--client-id))
                 (cons "response_type" "code")
                 (cons "redirect_uri" redirect-uri)
                 (cons "scope" *openai-oauth-scope*)
                 (cons "state" state)
                 (cons "code_challenge" code-challenge)
                 (cons "code_challenge_method" "S256")
                 (cons "id_token_add_organizations" "true")
                 (cons "codex_cli_simplified_flow" "true")
                 (cons "originator" *openai-oauth-originator*)))))

(defun openai-oauth--write-callback-response (stream status title body)
  ;; The browser may close a preconnect before reading.  That must not abort the
  ;; listener or turn an already-validated callback into a failed login.
  (ignore-errors
    (format stream
            (cat "HTTP/1.1 ~a~C~CContent-Type: text/html; charset=utf-8~C~C"
                 "Connection: close~C~C~C~C"
                 "<html><body><h1>~a</h1><p>~a</p></body></html>~%")
            status #\Return #\Newline #\Return #\Newline
            #\Return #\Newline #\Return #\Newline title body)
    (force-output stream)))

(defun openai-oauth--start-callback-server (socket expected-state)
  "Wait up to 120 seconds for the validated callback on SOCKET.  Browser
preconnects and unrelated paths receive 404 without consuming the one login."
  (let ((result nil)
        (deadline (+ (get-universal-time) 120)))
    (unwind-protect
         (loop until result
               for remaining = (- deadline (get-universal-time))
               do (when (<= remaining 0)
                    (setf result (list :error "timed out"))
                    (loop-finish))
                  (let ((client
                          (handler-case
                              (evo.port:call-with-timeout
                               remaining (lambda () (usocket:socket-accept socket)))
                            (evo.port:timeout-error () :timeout))))
                    (when (eq client :timeout)
                      (setf result (list :error "timed out"))
                      (loop-finish))
                    (unwind-protect
                         (let* ((stream (usocket:socket-stream client))
                                (line (handler-case
                                          (evo.port:call-with-timeout
                                           (min 5 (max 1 remaining))
                                           (lambda () (read-line stream nil nil)))
                                        (evo.port:timeout-error () nil)
                                        (error () nil)))
                                (target (and line
                                             (second (uiop:split-string
                                                      line :separator '(#\Space)))))
                                (qmark (and target (position #\? target)))
                                (path (and target
                                           (if qmark (subseq target 0 qmark) target))))
                           (if (not (and path (string= path *openai-oauth-callback-path*)))
                               (openai-oauth--write-callback-response
                                stream "404 Not Found" "Not found"
                                "Waiting for the OpenAI authorization callback.")
                               (multiple-value-bind (code error)
                                   (openai-oauth--callback-result target expected-state)
                                 (setf result (list :code code :error error))
                                 (if code
                                     (openai-oauth--write-callback-response
                                      stream "200 OK" "Authorization complete"
                                      "You may close this window and return to evo.")
                                     (openai-oauth--write-callback-response
                                      stream "400 Bad Request" "Authorization failed"
                                      "The callback was rejected. Return to evo for details.")))))
                      (usocket:socket-close client))))
      (usocket:socket-close socket))
    (values (getf result :code) (getf result :error))))

;;; ---------------------------------------------------------------------------
;;; Slash commands
;;; ---------------------------------------------------------------------------

(evo:register-command "openai-oauth:login"
  (lambda (ctx)
    (declare (ignore ctx))
    (let* ((binding (openai-oauth--listen-callback))
           (socket (first binding))
           (port (second binding))
           (verifier (openai-oauth--random-string 64))
           (challenge (openai-oauth--code-challenge verifier))
           (state (openai-oauth--random-string 32))
           (redirect-uri (openai-oauth--redirect-uri port))
           (auth-url (openai-oauth--authorize-url redirect-uri state challenge)))
      ;; The blocking callback server + token exchange run as a TRACKED task, so
      ;; a /reload stops and joins a login still waiting on the browser instead
      ;; of leaving it holding the callback port.
      (let ((login-thread nil))
        (setf login-thread
              (evo:spawn-task
               :name :openai-oauth-login
               :run (lambda ()
                      (handler-case
                          (multiple-value-bind (code error)
                              (openai-oauth--start-callback-server socket state)
                            (if error
                                (format *error-output* "~&[openai-oauth] Login failed: ~a~%~%" error)
                                (handler-case
                                    (let ((tokens (openai-oauth--exchange-code
                                                   code verifier redirect-uri)))
                                      (bt:with-lock-held (*openai-oauth-token-lock*)
                                        (openai-oauth--write-tokens
                                         (getf tokens :access-token)
                                         (getf tokens :refresh-token)
                                         (getf tokens :id-token)
                                         (getf tokens :account-id)
                                         (getf tokens :expires-at)
                                         (getf tokens :refresh-token-expires-at)))
                                      (openai-oauth--register-models)
                                      (format *error-output* "~&[openai-oauth] Login successful. Token stored.~%~%"))
                                  (error (e)
                                    (format *error-output* "~&[openai-oauth] Login failed: ~a~%~%" e)))))
                        (openai-oauth--login-cancelled ()
                          (ignore-errors (usocket:socket-close socket))
                          (format *error-output* "~&[openai-oauth] Login cancelled.~%~%"))
                        (error (e)
                          (ignore-errors (usocket:socket-close socket))
                          (format *error-output* "~&[openai-oauth] Login failed: ~a~%~%" e))))
               :stop (lambda ()
                       (let ((thread login-thread))
                         (when (and thread (bt:thread-alive-p thread))
                           (ignore-errors
                             (bt:interrupt-thread
                              thread
                              (lambda () (error 'openai-oauth--login-cancelled))))))))))
      (openai-oauth--open-browser auth-url)
      (format nil
              (cat "Opening browser for OpenAI OAuth login...~%"
                   "If the browser doesn't open, visit:~%  ~a~%"
                   "Waiting for authorization (timeout 120s)...~%"
                   "Check stderr for completion status.")
              auth-url)))
  :description "Start OpenAI OAuth login (PKCE flow with local callback)")

(evo:register-command "openai-oauth:refresh"
  (lambda (ctx)
    (declare (ignore ctx))
    (let ((refresh (openai-oauth--resolve-refresh-token)))
      (if (not refresh)
          "No refresh token: set OPENAI_OAUTH_REFRESH_TOKEN or run /openai-oauth:login first."
          (progn
            ;; Shares the auto-refresh path, so a manual refresh cannot race an
            ;; automatic one into spending the same single-use refresh token.
            (evo:spawn-task
             :name :openai-oauth-refresh
             :run (lambda ()
                    (handler-case
                        (progn
                          (openai-oauth--refresh-and-store refresh)
                          (format *error-output* "~&[openai-oauth] Token refreshed and stored.~%~%"))
                      (error (e)
                        (format *error-output* "~&[openai-oauth] Token refresh failed: ~a~%~%" e)))))
            "Refreshing token in background... Check stderr for status."))))
  :description "Refresh the OpenAI OAuth access token")

(evo:register-command "openai-oauth:status"
  (lambda (ctx)
    (declare (ignore ctx))
    (let* ((stored (openai-oauth--read-tokens))
           (token (openai-oauth--resolve-access-token))
           (refresh (openai-oauth--resolve-refresh-token))
           (account (openai-oauth--resolve-account-id))
           (now (openai-oauth--now-ms))
           (access-expires (and stored (getf stored :expires-at)))
           (access-remaining (and access-expires
                                  (max 0 (floor (/ (- access-expires now) 1000))))))
      (with-output-to-string (s)
        (format s "openai-oauth-provider diagnostics~%")
        (format s "  api: ~:[missing~;registered~]~%"
                (member :openai-oauth-responses (evo:api-keys)))
        (format s "  provider: :openai-oauth -> ~a~%" *openai-oauth-request-base-url*)
        (format s "  access token: ~:[missing~;present~]~%" token)
        (format s "  refresh token: ~:[missing~;present~]~%" refresh)
        (format s "  account id: ~:[missing~;present~]~%" account)
        (format s "  access token expires in: ~:[unknown~;~:*~a seconds~]~%"
                access-remaining)
        (format s "  stored token file: ~:[none~;~a~]~%"
                stored (and stored (namestring (openai-oauth--token-file))))
        (format s "  models registered: ~:[no~;yes~]~%"
                (find-if (lambda (model)
                           (and (equal (evo.util:pget model :id)
                                       (first *openai-oauth-models*))
                                (eq (evo.util:pget model :provider) :openai-oauth)))
                         (evo.provider:all-models)))
        (format s "  auto-refresh: ~:[disabled~;enabled (refresh at ~a s before expiry)~]~%"
                *openai-oauth-auto-refresh* *openai-oauth-refresh-before-expiry*)
        (format s "  client_id: ~a" (openai-oauth--client-id)))))
  :description "Show OpenAI OAuth provider diagnostics")
