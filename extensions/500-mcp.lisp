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
;;;; Connecting is asynchronous: the config is read at boot and each server is
;;;; contacted by its own background task.  A server that answers late is not
;;;; a server whose tools are lost — the tool list is resolved every turn, so
;;;; they are there from the next turn on.  See "Boot" at the bottom of this
;;;; file for the threading contract.
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
  name url headers session-id tools instructions (status :connecting) error)

;;; Two variables, two owners.  *MCP-SERVERS* is read by other threads (/mcp
;;; and the status line render on the TUI thread), so it and the slots those
;;; readers can observe — STATUS, ERROR, TOOLS — are guarded by *MCP-LOCK*.
;;; The lock is never held across a request.
;;;
;;; SESSION-ID and INSTRUCTIONS are not guarded, and are shared all the same.
;;; The boot task writes both during the handshake; a worker running one of
;;; the server's tools then reads SESSION-ID on every call, and rewrites both
;;; when it re-shakes hands on an expired session (MCP-CALL-TOOL).  The first
;;; write reaches the worker through *REGISTRY-LOCK*: the handshake finishes
;;; before the server's tools are registered, and a worker finds a tool only
;;; under that lock.  What stays open is two workers re-shaking hands with one
;;; server at once — each ends up with a working session id, and the last
;;; write wins.

(defvar *mcp-lock* (bt:make-lock "mcp"))

(defvar *mcp-servers* nil
  "Servers from the current generation, in config order.  Guarded by *MCP-LOCK*.")

(defvar *mcp-prompt-notes* nil
  "Prompt notes registered by any generation — cleared before re-registering,
so a server dropped from the config takes its instructions with it.  Touched
only by the boot task and by the load that starts it, and a load always joins
the previous task first, so it needs no lock of its own.")

(defvar *mcp-request-counter* 0
  "JSON-RPC id source.  The boot task and the worker running a tool call both
talk to servers at the same time, and two threads incf-ing a shared counter
would eventually put the same id on one connection twice — a client that
matches responses by id would then take the wrong one.")

(define-condition mcp-error (error)
  ((text :initarg :text :reader mcp-error-text))
  (:report (lambda (c s) (write-string (mcp-error-text c) s))))

;;; A session the server has forgotten (HTTP 404 on a request that carried a
;;; session id).  Recoverable: re-initialize and call again, once.
(define-condition mcp-session-expired (mcp-error) ())

;;; A boot the kernel is tearing down.  Delivered by interrupt into a
;;; connecting task's thread, where it unwinds the request in flight.  Nothing
;;; may convert it into an ordinary error on the way out — see MCP-HTTP-POST.
(define-condition mcp-boot-cancelled (error) ())

(defun mcp-timeout ()
  (or (evo:setting :mcp-timeout) *mcp-default-timeout*))

(defun mcp-next-request-id ()
  "A JSON-RPC id, unique across every thread talking to a server."
  (bt:with-lock-held (*mcp-lock*) (incf *mcp-request-counter*)))

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
            ;; A boot being cancelled arrives here as an interrupt.  It must
            ;; not be laundered into an MCP-ERROR below: the caller
            ;; distinguishes "this server failed" from "this boot is over",
            ;; and getting that wrong records a cancellation as a failure and
            ;; swallows the unwind.
            (mcp-boot-cancelled (e) (error e))
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
  (let* ((id (mcp-next-request-id))
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
  "initialize + notifications/initialized + tools/list.  Returns the server's
tool list; publishing the new state is the caller's, so a server is never
reported connected before its tools are registered."
  (setf (mcp-server-session-id server) nil)
  (let ((init (mcp-request server "initialize" :initialize t
                           :params (mcp-json
                                    "protocolVersion" *mcp-protocol-version*
                                    "capabilities" (mcp-json)
                                    "clientInfo" (mcp-json "name" *mcp-client-name*
                                                           "version" *mcp-client-version*)))))
    (setf (mcp-server-instructions server) (mcp-nonempty (mcp-jget init "instructions")))
    (mcp-notify server "notifications/initialized")
    (mcp-fetch-tools server)))

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

(defun mcp-register-tools (server tools)
  (dolist (tool tools)
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
          (progn (mcp-publish-server server :connected :tools (mcp-connect server))
                 (mcp-call-tool server remote-name args :retry nil))
          (error 'mcp-error :text "MCP session expired")))))

;;; ---------------------------------------------------------------------------
;;; Boot
;;; ---------------------------------------------------------------------------
;;;
;;; Connecting is network I/O with a read timeout measured in minutes, and this
;;; file loads on the session's own boot path.  Done inline, one unreachable
;;; server held the whole session hostage — no prompt could be typed until the
;;; last one timed out, and a reload re-paid the whole cost.  So boot does the
;;; cheap half (read the config, publish the server list as :connecting) and
;;; hands the network half to tracked background tasks — ONE PER SERVER, so a
;;; server that hangs delays nothing but itself — which register each server's
;;; tools and prompt note the moment that server answers.
;;;
;;; What that buys the session, precisely.  The tool list is resolved per turn
;;; (EVO.KERNEL:ACTIVE-TOOLS, called from PREPARE-NEXT-TURN), so a server that
;;; answers late is not a server whose tools are lost: they are in the next
;;; turn's list, and the model discovers them then.  Only a server that FAILS
;;; loses anything — it is recorded as :error and its tools stay absent until
;;; /reload reconnects.
;;;
;;; Threading, in full:
;;;
;;;   * *MCP-LOCK* guards the server list and the slots a reader on another
;;;     thread can observe (STATUS, ERROR, TOOLS).  It is never held across a
;;;     request, and REQUEST-REPAINT is called outside it: a lock held while
;;;     waiting on a socket is a lock that deadlocks a reload.
;;;   * Each task is tracked by EVO:SPAWN-TASK, and its :stop INTERRUPTS the
;;;     thread with MCP-BOOT-CANCELLED.  A flag alone would not do — the thread
;;;     spends its life inside DEX:POST, where no flag can reach it, and the
;;;     kernel abandons a task that ignores its stop after ~5s, which would let
;;;     the outgoing generation register tools after the incoming one loaded.
;;;     The interrupt unwinds Dexador on the task's own thread, so the socket
;;;     is closed by its owner.  Nothing may convert that condition into an
;;;     ordinary error on the way out (MCP-HTTP-POST is careful about this) or
;;;     the cancellation would be recorded as a server failure instead.
;;;   * The interrupt may unwind the HANDSHAKE and nothing else.  It is
;;;     asynchronous: sent unconditionally, it lands wherever the task happens
;;;     to be — halfway through REGISTER-TOOL's write to the registry's hash
;;;     table, which an unwind can leave corrupt, or past the task's last
;;;     handler, where the condition escapes the thread altogether.  So the
;;;     interrupt function checks, on the task's own thread, whether the task
;;;     is still inside MCP-BOOT-HANDSHAKE, and does nothing if it is not.  A
;;;     task stopped after its handshake sees the flag instead and registers
;;;     nothing; one stopped mid-registration finishes it, which costs
;;;     nothing, because the kernel sweeps the registry after joining it.
;;;   * A stopped server is published as :CANCELLED.  After a /reload that
;;;     succeeds nobody sees it, because the list is replaced; after one that
;;;     fails, the rollback restores the registries but cannot restart the
;;;     tasks it stopped, and "connecting…" for ever would be a lie.
;;;   * Nothing here WARNs.  A task runs on its own thread, where a warning is
;;;     printed straight over the TUI; a failure is recorded on the server, and
;;;     /mcp and the status line are where it is read.
;;;   * /reload stops and joins every task before the next generation loads, so
;;;     two boots never overlap and a stale task cannot publish into a live
;;;     registry.

(defun mcp-server-from-spec (spec)
  (make-mcp-server :name (or (mcp-nonempty (getf spec :name)) "mcp")
                   :url (getf spec :url)
                   :headers (mcp-normalize-headers (getf spec :headers))))

(defun mcp-publish-server (server status &key error (tools nil tools-p))
  "Publish what another thread may observe about SERVER: STATUS, ERROR, and
TOOLS when given — an empty list included, so a server that reconnects with no
tools stops listing the ones it had.  Repaint is requested AFTER the lock is
released — it takes the TUI's own lock, and there is no reason to order the
two."
  (bt:with-lock-held (*mcp-lock*)
    (setf (mcp-server-status server) status
          (mcp-server-error server) error)
    (when tools-p (setf (mcp-server-tools server) tools)))
  (evo.tui:request-repaint))

(defstruct (mcp-boot (:conc-name mcp-boot-))
  (lock (bt:make-lock "mcp-boot"))
  thread                                ; published by the task itself
  (cancelled nil)
  ;; True while the task is inside MCP-BOOT-HANDSHAKE, the one span :stop may
  ;; interrupt.  Written by the task and read by the interrupt function, which
  ;; runs on the task's own thread — so it needs no lock.
  (interruptible nil))

(defun mcp-boot-cancelled-p (boot)
  (bt:with-lock-held ((mcp-boot-lock boot)) (mcp-boot-cancelled boot)))

(defun mcp-boot-cancel (boot)
  "Stop the connecting task: raise the flag, then interrupt the thread in case
it is inside the handshake, where no flag can reach it.  Called from the thread
doing the disposal, which then joins the task's thread."
  (let (thread)
    (bt:with-lock-held ((mcp-boot-lock boot))
      (setf (mcp-boot-cancelled boot) t
            thread (mcp-boot-thread boot)))
    (when (and thread (bt:thread-alive-p thread))
      (ignore-errors
        (bt:interrupt-thread thread
                             (lambda ()
                               ;; Runs on the task's thread, wherever it is.
                               ;; Outside the handshake it does nothing: the
                               ;; task reads the flag instead.
                               (when (mcp-boot-interruptible boot)
                                 (error 'mcp-boot-cancelled))))))
    t))

(defun mcp-boot-handshake (boot server)
  "Shake hands with SERVER and return its tools.  The only span of the task
that :stop may interrupt: it is request and response and nothing else, so an
unwind here costs the request and leaves no shared state half-written."
  (unwind-protect
       (progn
         (setf (mcp-boot-interruptible boot) t)
         ;; Checked after raising INTERRUPTIBLE, not before.  A stop that came
         ;; earlier found nothing to interrupt, and this is what catches it;
         ;; a stop from here on is an interrupt that lands.
         (when (mcp-boot-cancelled-p boot)
           (error 'mcp-boot-cancelled))
         (unless (mcp-nonempty (mcp-server-url server))
           (error 'mcp-error :text "no :url in the server spec"))
         (mcp-connect server))
    (setf (mcp-boot-interruptible boot) nil)))

(defun mcp-boot-run (boot server)
  "The connecting task's body: one server, from handshake to registration.
One task per server, so a server that hangs delays only its own tools.  A
failure is recorded on the server, not signalled: one unreachable server must
not cost the session the others, nor its startup."
  ;; Publish our own thread handle as the first act: the kernel may call :stop
  ;; before this thread's first form runs, and a stop with no thread to
  ;; interrupt would leave the task uncancellable for the length of a request.
  (bt:with-lock-held ((mcp-boot-lock boot))
    (setf (mcp-boot-thread boot) (bt:current-thread)))
  (handler-case
      (let ((tools (mcp-boot-handshake boot server)))
        ;; A stop that came after the handshake interrupted nothing; it is
        ;; honoured here.
        (if (mcp-boot-cancelled-p boot)
            (mcp-publish-server server :cancelled)
            (progn
              (mcp-register-tools server tools)
              (mcp-register-instructions server)
              ;; Last, so the status line never promises tools the agent
              ;; cannot call yet.
              (mcp-publish-server server :connected :tools tools))))
    (mcp-boot-cancelled () (mcp-publish-server server :cancelled))
    (error (e) (mcp-publish-server server :error :error (format nil "~a" e)))))

(defun mcp-boot ()
  "Read the config and start connecting — without waiting for it.

Returns as soon as the tasks are started.  The server list is published up
front so /mcp and the status line can say what is being contacted."
  (dolist (name *mcp-prompt-notes*) (evo:register-prompt-note name nil))
  (setf *mcp-prompt-notes* nil)
  (let ((servers (loop for spec in (evo:setting :mcp-servers)
                       collect (mcp-server-from-spec spec))))
    (bt:with-lock-held (*mcp-lock*) (setf *mcp-servers* servers))
    (dolist (server servers)
      (let ((boot (make-mcp-boot)))
        (evo:spawn-task :name (format nil "mcp-connect-~a" (mcp-server-name server))
                        :run (lambda () (mcp-boot-run boot server))
                        :stop (lambda () (mcp-boot-cancel boot)))))
    (evo.tui:request-repaint))
  nil)

;;; ---------------------------------------------------------------------------
;;; /mcp and the status line
;;; ---------------------------------------------------------------------------

(defun mcp-status-report ()
  "What /mcp prints.  The state is snapshotted under the lock and formatted
outside it, so a long report never holds the lock the boot task needs."
  (let ((servers (bt:with-lock-held (*mcp-lock*)
                   (loop for server in *mcp-servers*
                         collect (list :name (mcp-server-name server)
                                       :url (mcp-server-url server)
                                       :status (mcp-server-status server)
                                       :error (mcp-server-error server)
                                       :headers (mcp-server-headers server)
                                       :tools (loop for tool in (mcp-server-tools server)
                                                    for remote = (mcp-nonempty
                                                                  (mcp-jget tool "name"))
                                                    when remote
                                                      collect (mcp-tool-name
                                                               server remote)))))))
    (if (null servers)
        ;; (string #\Newline), not #\Newline: CAT concatenates with
        ;; CONCATENATE, whose arguments are sequences.  The line breaks stay
        ;; outside the literals on purpose — see CAT's own docstring.
        (cat "No MCP servers configured.  In ~/.evo/init.lisp:" (string #\Newline)
             (string #\Newline)
             "(evo:set-setting :mcp-servers" (string #\Newline)
             "  '((:name \"example\"" (string #\Newline)
             "     :url \"https://example.com/mcp\"" (string #\Newline)
             "     :headers ((\"Authorization\" . \"Bearer …\")))))" (string #\Newline)
             (string #\Newline)
             "Then /reload.")
        (with-output-to-string (out)
          (dolist (server servers)
            (let ((headers (evo.util:pget server :headers)))
              (format out "~a  ~a~%" (evo.util:pget server :name)
                      (evo.util:pget server :url))
              (case (evo.util:pget server :status)
                (:connected
                 (let ((tools (evo.util:pget server :tools)))
                   (format out "  connected · ~d tool~:p~@[ · headers: ~a~]~%"
                           (length tools)
                           (and headers
                                (evo.util:string-join ", " (mapcar #'car headers))))
                   (dolist (name tools)
                     (format out "    ~a~%" name))))
                (:connecting
                 (format out "  connecting…~@[ · headers: ~a~]~%"
                         (and headers
                              (evo.util:string-join ", " (mapcar #'car headers)))))
                (:cancelled
                 (format out "  not connected: stopped by /reload before it answered~%"))
                (t (format out "  not connected: ~a~%"
                           (evo.util:pget server :error))))))
          (format out "~%/reload re-reads the config and reconnects.")))))

(defun mcp-status-label (&optional tui)
  "The status line's MCP segment: nothing when no server is configured, else a
one-line summary.  Runs on the TUI thread on every repaint, so it only reads
what the boot task published — no I/O, and the lock is held for the read and
nothing else."
  (declare (ignore tui))
  (let ((summary
          (bt:with-lock-held (*mcp-lock*)
            (when *mcp-servers*
              (let ((connecting 0) (connected 0) (failed 0) (cancelled 0) (tools 0))
                (dolist (server *mcp-servers*)
                  (case (mcp-server-status server)
                    (:connected (incf connected)
                                (incf tools (length (mcp-server-tools server))))
                    (:error (incf failed))
                    (:cancelled (incf cancelled))
                    (t (incf connecting))))
                (evo.util:string-join
                 " · "
                 (remove nil
                         (list (when (plusp connecting)
                                 (format nil "~d connecting" connecting))
                               (when (plusp connected)
                                 (format nil "~d up, ~d tool~:p" connected tools))
                               (when (plusp failed)
                                 (format nil "~d failed" failed))
                               (when (plusp cancelled)
                                 (format nil "~d cancelled" cancelled))))))))))
    (when summary (evo.tui::dim (format nil "mcp ~a" summary)))))

(evo:register-command "mcp"
  (lambda (ctx) (declare (ignore ctx)) (mcp-status-report))
  :description "MCP servers: what connected, and the tools it registered")

;; Order 400 keeps this inboard of the core segments on the right.
(evo.tui:add-status-segment :mcp #'mcp-status-label :side :right :order 400)
(evo:on-unload (lambda () (evo.tui:remove-status-segment :mcp)))

(mcp-boot)
