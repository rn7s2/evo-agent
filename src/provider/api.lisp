;;;; api.lisp — the provider-API protocol and its registry.
;;;;
;;;; A provider API is one wire protocol (Anthropic Messages, OpenAI
;;;; Responses, ...), implemented as a CLOS class with methods for request
;;;; building and stream parsing.  APIs are kernel-curated: they are
;;;; registered at load time by the files in this module and are not
;;;; user-extensible — users register MODELS and PROVIDERS (registry.lisp),
;;;; which name an API by its :api keyword.
;;;;
;;;; The adapter contract every API implementation must honor:
;;;;
;;;;  - PARSE-STREAM returns a result plist:
;;;;      :content        list of unified content blocks
;;;;      :model          server-reported model id string (or nil)
;;;;      :stopped-p      true iff a terminal event was seen
;;;;      :error-message  string or nil
;;;;      :stop-reason    one of :stop :length :tool-use :error :aborted
;;;;      :usage          (:input n :output n :cache-read n :cache-write n)
;;;;                      — :input EXCLUDES cached/cache-written tokens
;;;;      :aborted-p      true when the abort flag fired mid-stream
;;;;  - Events emitted via :on-event (a plist per event):
;;;;      :message-start · :text-delta · :thinking-delta · :tool-call-start
;;;;  - Errors are data at runtime: call-provider converts stream errors and
;;;;    HTTP failures into assistant messages, never signals into the loop.
;;;;    Config-resolution errors (unknown model/API/provider) DO signal —
;;;;    they are boot/config bugs, caught by the CLI preflight.
;;;;  - Unknown stop reasons / statuses are loud provider-errors, not guesses.

(in-package :evo.provider)

(defclass provider-api () ()
  (:documentation "One wire protocol.  Stateless; a single instance per
API lives in the registry."))

(defgeneric endpoint-path (api)
  (:documentation "URL path appended to the provider base-url, e.g. \"/v1/messages\"."))

(defgeneric auth-headers (api config)
  (:documentation "Alist of API-specific request headers.  CONFIG is the
resolved provider config plist (:base-url :api-key).  content-type is added
by the transport, not here."))

(defgeneric build-request (api &key model system messages tools
                                    thinking-level cache-key)
  (:documentation "Serialized JSON request body string.  Responsible for
running the handoff pass over MESSAGES."))

(defgeneric parse-stream (api char-stream &key on-event abort-flag)
  (:documentation "Parse one response stream into the adapter result plist
described in the file header."))

(defgeneric thinking-param (api level)
  (:documentation "Map a thinking LEVEL (:off :low :medium :high :xhigh) to
the API's native parameter (Anthropic budget_tokens integer, OpenAI effort
string).  NIL means thinking off."))

(defgeneric perform-request (api url headers body &key on-event abort-flag)
  (:documentation "Execute one request and return the result plist.  The
default method (core.lisp) streams SSE over dexador and delegates to
PARSE-STREAM; override for a non-SSE framing.  May signal transport
errors — the retry loop in call-provider classifies them."))

;;; Kernel-curated provider defaults: base URL and canonical API-key env var
;;; are properties of the API, seeded into the provider registry so an env
;;; key alone is enough config (registry.lisp).

(defgeneric default-provider-key (api))
(defgeneric default-base-url (api))
(defgeneric default-api-key-env (api))

;;; Registry: ordered alist, load-time population, not user-extensible.

(defvar *apis* nil
  "Ordered alist of (:api-keyword . provider-api-instance).")

(defun register-api (key instance)
  (let ((entry (assoc key *apis*)))
    (if entry
        (setf (cdr entry) instance)
        (setf *apis* (append *apis* (list (cons key instance))))))
  key)

(defun api-keys ()
  (mapcar #'car *apis*))

(defun find-api (key)
  (or (cdr (assoc key *apis*))
      (error "Unknown provider API ~s. Kernel APIs: ~{~s~^, ~}."
             key (api-keys))))
