;;;; 500-mcp.lisp — MCP client: a remote server's tools, as evo tools.
;;;;
;;;; Vendored user extension, installed to $(EVO_HOME)/extensions/ by
;;;; `make install`.  Loaded at startup like any other extension.
;;;;
;;;; What it does: for every server listed in the `:mcp-servers` setting it
;;;; speaks MCP over Streamable HTTP — initialize, tools/list — and registers
;;;; each remote tool as an evo tool named `<server>__<tool>`.  Calling one
;;;; sends `tools/call` and hands the result blocks (text and images) back to
;;;; the model.  The server's `instructions`, if it sends any, become a prompt
;;;; note, so its own words about how to use it reach the agent.
;;;;
;;;; Configure it in ~/.evo/init.lisp (or <project>/.evo/init.lisp):
;;;;
;;;;   (evo:set-setting :mcp-servers
;;;;     '((:name "notes"
;;;;        :url "https://notes.example.com/mcp"
;;;;        :headers (("Authorization" . "Bearer sk-…")))))
;;;;
;;;;   (evo:set-setting :mcp-timeout 120)   ; per-request read timeout, seconds
;;;;
;;;; `/mcp` shows what connected and what it registered; `/reload` reconnects
;;;; (config is re-read, so that is also how a server is added or removed).
;;;;
;;;; Deliberately small:
;;;;   - one transport, Streamable HTTP (no stdio servers, no SSE-only ones)
;;;;   - no auth flow, no OAuth, no token refresh.  A server that wants a
;;;;     credential gets it from :headers, verbatim, on every request.
;;;;   - tools only: no resources, no prompts, no sampling, no notifications.
;;;;
;;;; The seam that makes remote tools honest: the tool is registered with the
;;;; server's own JSON Schema (hash-table schemas pass through the kernel
;;;; verbatim) and with :arguments :json, so it is handed the model's exact
;;;; JSON.  The keywordized plist would downcase and de-underscore every key —
;;;; harmless for a fixed contract like `path`, fatal for arguments whose keys
;;;; are data, e.g. {"files": {"src/App.jsx": "…"}}.

(in-package :evo.user)

(defparameter *mcp-protocol-version* "2025-06-18")
(defparameter *mcp-client-name* "evo")
(defparameter *mcp-client-version* "0.1")
(defparameter *mcp-connect-timeout* 15
  "Seconds to wait for the TCP/TLS connection to an MCP server.")
(defparameter *mcp-default-timeout* 120
  "Default per-request read timeout; override with the :mcp-timeout setting.")

(defstruct mcp-server
  name url headers session-id tools instructions (status :new) error)

(defvar *mcp-servers* nil
  "Servers from the current generation, in config order.")

(defvar *mcp-prompt-notes* nil
  "Prompt notes registered by any generation — cleared before re-registering,
so a server dropped from the config takes its instructions with it.")

(defvar *mcp-request-counter* 0)

(define-condition mcp-error (error)
  ((text :initarg :text :reader mcp-error-text))
  (:report (lambda (c s) (write-string (mcp-error-text c) s))))

;;; A session the server has forgotten (HTTP 404 on a request that carried a
;;; session id).  Recoverable: re-initialize and call again, once.
(define-condition mcp-session-expired (mcp-error) ())

(defun mcp-timeout ()
  (or (evo:setting :mcp-timeout) *mcp-default-timeout*))

(defun mcp-json (&rest kvs)
  "A JSON object as jzon reads and writes them: string keys, hash-table."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun mcp-jget (object key)
  (and (hash-table-p object) (gethash key object)))

(defun mcp-nonempty (string)
  (and (stringp string) (plusp (length string)) string))

;;; ---------------------------------------------------------------------------
;;; Transport: one POST per JSON-RPC message (Streamable HTTP).
;;; ---------------------------------------------------------------------------

(defun mcp-header-name (name)
  "A header NAME as it goes on the wire.  STRING, not PRINC-TO-STRING: a
symbol's printed form depends on *print-case*/*print-escape*, and a header
named \":authorization\" fails authentication in a way that reads like a
credential problem.  Header names are case-insensitive on the wire, so
lowercasing them here also keeps them from colliding with evo's own."
  (string-downcase (typecase name
                     (string name)
                     (symbol (symbol-name name))
                     (t (princ-to-string name)))))

(defun mcp-header-value (value)
  (if (stringp value) value (princ-to-string value)))

(defun mcp-normalize-headers (headers)
  "Config headers -> a dexador alist.  Accepts ((\"K\" . \"V\") …),
((\"K\" \"V\") …), a flat (\"K\" \"V\" …) or a keyword plist
(:authorization \"…\")."
  (cond ((null headers) nil)
        ((or (stringp (first headers)) (symbolp (first headers)))
         (loop for (k v) on headers by #'cddr
               collect (cons (mcp-header-name k) (mcp-header-value v))))
        (t (loop for h in headers
                 unless (consp h) do (error 'mcp-error :text (format nil "bad header ~s" h))
                 collect (cons (mcp-header-name (car h))
                               (mcp-header-value (if (consp (cdr h)) (second h) (cdr h))))))))

(defun mcp-decode-body (raw)
  "Response bytes -> string.  UTF-8: an MCP server may answer in any language."
  (cond ((null raw) "")
        ((stringp raw) raw)
        (t (handler-case
               (flexi-streams:octets-to-string
                (coerce raw '(vector (unsigned-byte 8))) :external-format :utf-8)
             (error () (map 'string #'code-char raw))))))

(defun mcp-http-post (server body &key initialize)
  "POST BODY (a JSON string) to SERVER.  Returns (values TEXT STATUS CONTENT-TYPE).
An HTTP error is data here, not a condition: the server's own body usually
says what went wrong, and the JSON-RPC layer above reports it."
  (let* ((url (mcp-server-url server))
         (headers (append (list (cons "content-type" "application/json")
                                (cons "accept" "application/json, text/event-stream"))
                          ;; The protocol version is required on every request
                          ;; after initialize, and meaningless on initialize
                          ;; itself (it travels in the params there).
                          (unless initialize
                            (list (cons "mcp-protocol-version" *mcp-protocol-version*)))
                          (let ((sid (mcp-server-session-id server)))
                            (when sid (list (cons "mcp-session-id" sid))))
                          (mcp-server-headers server))))
    (evo:with-proxy (proxy url)
      (multiple-value-bind (raw status response-headers)
          (handler-case
              (apply #'dex:post url
                     :headers headers :content body
                     :force-binary t :keep-alive nil
                     :connect-timeout *mcp-connect-timeout*
                     :read-timeout (mcp-timeout)
                     (when proxy (list :proxy proxy)))
            (dexador.error:http-request-failed (e)
              (values (dexador.error:response-body e)
                      (dexador.error:response-status e)
                      (dexador.error:response-headers e)))
            (error (e)
              (error 'mcp-error :text (format nil "~a: ~a" url e))))
        ;; The server may hand out a session id on any response; carry it.
        (let ((sid (mcp-nonempty (mcp-jget response-headers "mcp-session-id"))))
          (when sid (setf (mcp-server-session-id server) sid)))
        (values (mcp-decode-body raw) status
                (mcp-jget response-headers "content-type"))))))

(defun mcp-sse-payloads (text)
  "The `data:` payloads of an SSE body, in order — a Streamable HTTP server
may answer a single request with a one-event stream instead of plain JSON."
  (let ((payloads nil)
        (data nil))
    (flet ((flush ()
             (when data
               (let ((payload (evo.util:string-join (string #\Newline) (nreverse data))))
                 (setf data nil)
                 (let ((json (handler-case (com.inuoe.jzon:parse payload) (error () nil))))
                   (when json (push json payloads)))))))
      (dolist (raw (uiop:split-string text :separator (string #\Newline)))
        (let ((line (string-right-trim '(#\Return) raw)))
          (cond ((zerop (length line)) (flush))
                ((evo.util:string-prefix-p "data:" line)
                 (push (string-left-trim " " (subseq line 5)) data)))))
      (flush))
    (nreverse payloads)))

(defun mcp-response-message (text content-type)
  "The JSON-RPC message in a response body: the body itself, or the first
result/error message of an SSE stream.  NIL for an empty body (a notification's
202)."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (cond ((zerop (length text)) nil)
          ((search "text/event-stream" (or content-type ""))
           (find-if (lambda (m)
                      (or (nth-value 1 (mcp-jget m "result"))
                          (nth-value 1 (mcp-jget m "error"))))
                    (mcp-sse-payloads text)))
          (t (handler-case (com.inuoe.jzon:parse text)
               (error ()
                 (error 'mcp-error
                        :text (format nil "invalid JSON in response: ~a"
                                      (evo.util:truncate-string text 200 "…")))))))))

(defun mcp-request (server method &key params initialize)
  "One JSON-RPC request/response.  Returns the result object; signals
MCP-ERROR for a transport, HTTP or JSON-RPC error."
  (let* ((id (incf *mcp-request-counter*))
         (body (com.inuoe.jzon:stringify
                (mcp-json "jsonrpc" "2.0" "id" id "method" method
                          "params" (or params (mcp-json))))))
    (multiple-value-bind (text status content-type)
        (mcp-http-post server body :initialize initialize)
      (when (and (eql status 404) (mcp-server-session-id server))
        (setf (mcp-server-session-id server) nil)
        (error 'mcp-session-expired :text "MCP session expired"))
      (unless (and (integerp status) (<= 200 status 299))
        (error 'mcp-error
               :text (format nil "HTTP ~a from ~a: ~a" status (mcp-server-url server)
                             (evo.util:truncate-string text 300 "…"))))
      (let ((message (mcp-response-message text content-type)))
        (unless message
          (error 'mcp-error :text (format nil "empty response to ~a" method)))
        (let ((err (mcp-jget message "error")))
          (when err
            (error 'mcp-error
                   :text (format nil "~a~@[ (code ~a)~]"
                                 (or (mcp-nonempty (mcp-jget err "message")) "MCP error")
                                 (mcp-jget err "code")))))
        (or (mcp-jget message "result") (mcp-json))))))

(defun mcp-notify (server method &key params)
  "Fire-and-forget notification; the server answers 202 with no body."
  (mcp-http-post server (com.inuoe.jzon:stringify
                         (mcp-json "jsonrpc" "2.0" "method" method
                                   "params" (or params (mcp-json)))))
  t)

;;; ---------------------------------------------------------------------------
;;; Handshake and catalog
;;; ---------------------------------------------------------------------------

(defun mcp-connect (server)
  "initialize + notifications/initialized + tools/list."
  (setf (mcp-server-session-id server) nil)
  (let ((init (mcp-request server "initialize" :initialize t
                           :params (mcp-json
                                    "protocolVersion" *mcp-protocol-version*
                                    "capabilities" (mcp-json)
                                    "clientInfo" (mcp-json "name" *mcp-client-name*
                                                           "version" *mcp-client-version*)))))
    (setf (mcp-server-instructions server) (mcp-nonempty (mcp-jget init "instructions")))
    (mcp-notify server "notifications/initialized")
    (setf (mcp-server-tools server) (mcp-fetch-tools server)
          (mcp-server-status server) :connected
          (mcp-server-error server) nil)
    server))

(defun mcp-fetch-tools (server)
  "Every page of tools/list."
  (let ((tools nil)
        (cursor nil))
    (loop
      (let ((result (mcp-request server "tools/list"
                                 :params (if cursor (mcp-json "cursor" cursor) (mcp-json)))))
        (loop for tool across (or (mcp-jget result "tools") #())
              do (push tool tools))
        (setf cursor (mcp-nonempty (mcp-jget result "nextCursor")))
        (unless cursor (return))))
    (nreverse tools)))

(defun mcp-safe-name (string)
  "A tool name the wire accepts: [A-Za-z0-9_-] only."
  (map 'string
       (lambda (ch)
         (if (or (char<= #\a ch #\z) (char<= #\A ch #\Z) (char<= #\0 ch #\9)
                 (find ch "_-"))
             ch
             #\_))
       string))

(defun mcp-tool-name (server remote-name)
  "`<server>__<tool>`, capped at the 64 characters tool names are allowed."
  (evo.util:truncate-string
   (mcp-safe-name (format nil "~a__~a" (mcp-server-name server) remote-name))
   64 ""))

(defun mcp-tool-description (server tool)
  (or (mcp-nonempty (mcp-jget tool "description"))
      (mcp-nonempty (mcp-jget tool "title"))
      (format nil "~a on MCP server ~a." (mcp-jget tool "name") (mcp-server-name server))))

(defun mcp-tool-schema (tool)
  "The server's own inputSchema, verbatim — the kernel passes a JSON Schema
hash-table straight through, so nothing the DSL cannot express is lost."
  (let ((schema (mcp-jget tool "inputSchema")))
    (if (hash-table-p schema)
        schema
        (mcp-json "type" "object" "properties" (mcp-json)))))

(defun mcp-register-tools (server)
  (dolist (tool (mcp-server-tools server))
    (let ((remote-name (mcp-nonempty (mcp-jget tool "name"))))
      (when remote-name
        (evo:register-tool (mcp-tool-name server remote-name)
          :description (mcp-tool-description server tool)
          :schema (mcp-tool-schema tool)
          ;; Exact JSON in, exact JSON out: see the file header.
          :arguments :json
          :execute (let ((server server) (remote-name remote-name))
                     (lambda (args) (mcp-call-tool server remote-name args))))))))

(defun mcp-register-instructions (server)
  "A server's own instructions, as a prompt note — its words about how its
tools are meant to be used, which no tool description carries."
  (let ((text (mcp-server-instructions server)))
    (when text
      (let ((name (format nil "mcp:~a" (mcp-server-name server))))
        (pushnew name *mcp-prompt-notes* :test #'equal)
        (evo:register-prompt-note
         name
         (format nil "## MCP server `~a`~%~%Its tools are registered as `~a__<tool>`.~%~%~a"
                 (mcp-server-name server)
                 (mcp-safe-name (mcp-server-name server))
                 text))))))

;;; ---------------------------------------------------------------------------
;;; Calling
;;; ---------------------------------------------------------------------------

(defun mcp-result-blocks (result)
  "MCP content blocks -> evo content blocks.  Text stays text, an image
becomes an image block the model actually sees, anything else is shown as its
JSON rather than dropped."
  (let ((blocks nil))
    (loop for b across (or (mcp-jget result "content") #())
          for type = (mcp-jget b "type")
          do (cond
               ((equal type "text")
                (push (list :type :text :text (or (mcp-jget b "text") "")) blocks))
               ((equal type "image")
                (let ((data (mcp-jget b "data")))
                  (push (evo.media:make-image-block
                         :data data
                         :media-type (or (mcp-nonempty (mcp-jget b "mimeType")) "image/png")
                         :name "mcp image"
                         :bytes (if (stringp data) (floor (* 3 (length data)) 4) 0)
                         :source "mcp")
                        blocks)))
               ((equal type "resource")
                (let ((resource (mcp-jget b "resource")))
                  (push (list :type :text
                              :text (or (mcp-jget resource "text")
                                        (com.inuoe.jzon:stringify b)))
                        blocks)))
               (t (push (list :type :text :text (com.inuoe.jzon:stringify b)) blocks))))
    ;; A server that answers only in structuredContent still has to say
    ;; something to the model.
    (let ((structured (mcp-jget result "structuredContent")))
      (when (and structured (null blocks))
        (push (list :type :text :text (com.inuoe.jzon:stringify structured)) blocks)))
    (nreverse blocks)))

(defun mcp-blocks-text (blocks)
  (evo.util:string-join
   (string #\Newline)
   (loop for b in blocks when (eq (getf b :type) :text) collect (getf b :text))))

(defun mcp-call-tool (server remote-name args &key (retry t))
  "tools/call.  ARGS is the model's exact JSON object (:arguments :json)."
  (handler-case
      (let* ((result (mcp-request server "tools/call"
                                  :params (mcp-json "name" remote-name
                                                    "arguments" (if (hash-table-p args)
                                                                    args
                                                                    (mcp-json)))))
             (blocks (mcp-result-blocks result)))
        (when (eq t (mcp-jget result "isError"))
          ;; A tool-level error is the tool's own answer: signal it so the loop
          ;; marks the result an error, with the server's text in it.
          (error 'mcp-error :text (or (mcp-nonempty (mcp-blocks-text blocks))
                                      "MCP tool reported an error")))
        (or blocks (list (list :type :text :text "(no content)"))))
    (mcp-session-expired ()
      (if retry
          ;; The server forgot the session (restart, idle timeout).  Shake
          ;; hands again and call once more; a second expiry is a real error.
          (progn (mcp-connect server)
                 (mcp-call-tool server remote-name args :retry nil))
          (error 'mcp-error :text "MCP session expired")))))

;;; ---------------------------------------------------------------------------
;;; Boot + /mcp
;;; ---------------------------------------------------------------------------

(defun mcp-server-from-spec (spec)
  (make-mcp-server :name (or (mcp-nonempty (getf spec :name)) "mcp")
                   :url (getf spec :url)
                   :headers (mcp-normalize-headers (getf spec :headers))))

(defun mcp-boot ()
  "Connect to every configured server and register its tools.  A server that
fails is recorded and skipped: one unreachable MCP server must not cost the
session its other tools, nor its startup."
  (dolist (name *mcp-prompt-notes*) (evo:register-prompt-note name nil))
  (setf *mcp-prompt-notes* nil
        *mcp-servers* nil)
  (dolist (spec (evo:setting :mcp-servers))
    (let ((server (mcp-server-from-spec spec)))
      (push server *mcp-servers*)
      (handler-case
          (progn
            (unless (mcp-nonempty (mcp-server-url server))
              (error 'mcp-error :text "no :url in the server spec"))
            (mcp-connect server)
            (mcp-register-tools server)
            (mcp-register-instructions server))
        (error (e)
          (setf (mcp-server-status server) :error
                (mcp-server-error server) (format nil "~a" e))
          (warn "MCP server ~a: ~a" (mcp-server-name server) e)))))
  (setf *mcp-servers* (nreverse *mcp-servers*)))

(defun mcp-status-report ()
  (if (null *mcp-servers*)
      (cat "No MCP servers configured.  In ~/.evo/init.lisp:" #\Newline
           #\Newline
           "(evo:set-setting :mcp-servers" #\Newline
           "  '((:name \"example\"" #\Newline
           "     :url \"https://example.com/mcp\"" #\Newline
           "     :headers ((\"Authorization\" . \"Bearer …\")))))" #\Newline
           #\Newline
           "Then /reload.")
      (with-output-to-string (out)
        (dolist (server *mcp-servers*)
          (format out "~a  ~a~%" (mcp-server-name server) (mcp-server-url server))
          (if (eq (mcp-server-status server) :connected)
              (progn
                (format out "  connected · ~d tool~:p~@[ · headers: ~a~]~%"
                        (length (mcp-server-tools server))
                        (and (mcp-server-headers server)
                             (evo.util:string-join
                              ", " (mapcar #'car (mcp-server-headers server)))))
                (dolist (tool (mcp-server-tools server))
                  (format out "    ~a~%" (mcp-tool-name server (mcp-jget tool "name")))))
              (format out "  not connected: ~a~%" (mcp-server-error server))))
        (format out "~%/reload re-reads the config and reconnects."))))

(evo:register-command "mcp"
  (lambda (ctx) (declare (ignore ctx)) (mcp-status-report))
  :description "MCP servers: what connected, and the tools it registered")

(mcp-boot)
