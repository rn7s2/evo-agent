;;;; api.lisp — the provider-API protocol and its registry.
;;;;
;;;; A provider API is one wire protocol (Anthropic Messages, OpenAI
;;;; Responses, ...), implemented as a CLOS class with methods for request
;;;; building and stream parsing.  The bundled APIs are registered at load
;;;; time by the files in this module; an extension registers its own the
;;;; same way, through the public EVO surface (evo:register-api) — the
;;;; protocol is an extension point, not a kernel privilege.  Either way a
;;;; MODEL names its API by the :api keyword (registry.lisp).
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
;;;;      :message-start · :text-delta · :thinking-delta
;;;;    (:tool-call-start is NOT a stream event: arguments are still
;;;;    streaming when a tool block opens, so the kernel emits it from
;;;;    run-tool-call once the parsed :arguments plist exists)
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
  (:documentation "Map a thinking LEVEL (:low :medium :high :xhigh :max) to
the API's native parameter (Anthropic budget_tokens integer, OpenAI effort
string).  NIL means the level names no rung on the ladder (there is no off
rung), and the adapter should send no thinking parameter at all.

Level alone is not always enough: where the native knob depends on the
model as well — Anthropic's output_config.effort ladder differs per model,
and only some models take adaptive thinking — BUILD-REQUEST consults the
registry (:effort, :thinking-mode) directly.  This generic stays the
level-only mapping, which is what a simple adapter needs."))

(defgeneric perform-request (api url headers body &key on-event abort-flag
                                                 abort-cleanup &allow-other-keys)
  (:documentation "Execute one request and return the result plist.  The
 default method (core.lisp) streams SSE over dexador and delegates to
 PARSE-STREAM; override for a non-SSE framing.  May signal transport
 errors — the retry loop in call-provider classifies them.  ABORT-CLEANUP,
 when supplied, registers a cleanup function that should unblock the
 current request (usually by closing its stream)."))

;;; Self-seeding defaults: base URL and canonical API-key env var are
;;; properties of the API, seeded into the provider registry so an env key
;;; alone is enough config to talk to a stock endpoint (registry.lisp).
;;;
;;; All three default to NIL so an API is useful the moment the wire
;;; protocol is implemented.  A NIL DEFAULT-PROVIDER-KEY means "seeds
;;; nothing": reset-user-registries skips the API and its provider is
;;; registered from init.lisp like any other.  Without these defaults an
;;; extension-defined API would boot fine and then die on the *next*
;;; reset — i.e. on /reload — with a no-applicable-method error.

(defgeneric default-provider-key (api)
  (:method ((api provider-api)) nil)
  (:documentation "Provider key this API seeds into the registry, or NIL to
seed nothing."))

(defgeneric default-base-url (api)
  (:method ((api provider-api)) nil)
  (:documentation "Stock endpoint base URL for the seeded provider."))

(defgeneric default-api-key-env (api)
  (:method ((api provider-api)) nil)
  (:documentation "Canonical API-key environment variable for the seeded
provider."))

;;; Registry: ordered alist, populated at load time by the files in this
;;; module and at boot by extensions.

(defvar *apis* nil
  "Ordered alist of (:api-keyword . provider-api-instance).")

(defun register-api (key instance)
  "Register (or replace, keeping position) the provider API under KEY.
Validates eagerly so a typo in an extension errors at load time rather
than at the first model call.  Re-registering is how a reloaded extension
stays idempotent."
  (unless (keywordp key)
    (error "register-api: key must be a keyword, got ~s" key))
  (unless (typep instance 'provider-api)
    (error "register-api ~s: instance must be a PROVIDER-API subclass, got ~s"
           key (type-of instance)))
  (let ((entry (assoc key *apis*)))
    (if entry
        (setf (cdr entry) instance)
        (setf *apis* (append *apis* (list (cons key instance))))))
  key)

(defun api-keys ()
  (mapcar #'car *apis*))

(defun find-api (key)
  (or (cdr (assoc key *apis*))
      (error (cat "Unknown provider API ~s. Registered APIs: ~{~s~^, ~}.~%"
                  "Bundled APIs are always present; an extension-defined one is~%"
                  "only there once its extension has run (evo:register-api).")
             key (api-keys))))
