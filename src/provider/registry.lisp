;;;; registry.lisp — user-registrable model and provider registries.
;;;;
;;;; evo ships no built-in model table: models and provider overrides are
;;;; registered from init.lisp (config-as-code) through the public EVO API.
;;;; Both registries preserve registration order (the /model picker shows
;;;; models in order) and are reset before each userspace boot so config
;;;; re-evaluation is idempotent.  Providers are re-seeded from the kernel
;;;; APIs' defaults (base-url + canonical api-key env var), so an env key
;;;; alone is enough to talk to a stock endpoint.

(in-package :evo.provider)

;;; Models.

(defvar *models* nil
  "Registered model plists, in registration order.")

(defparameter +effort-levels+ '(:low :medium :high :xhigh :max)
  "Effort levels an API may accept, weakest first.  A model declares the
subset it supports with :effort; the adapter clamps a request down to the
strongest supported level that does not exceed the one asked for.

This is also evo's whole thinking ladder: there is no off rung.  An agent
that cannot think is not worth driving, and the two ladders being the same
list is what lets a session-wide level go straight to output_config.effort.")

(defun normalize-thinking-level (level)
  "Coerce a journaled or configured thinking LEVEL onto the ladder; NIL when
it names no rung, so callers can fall through to their default.  :off is the
retired rung: sessions and init.lisp files written when thinking could be
switched off fold onto the weakest live level instead of failing to resume
\(or, worse, silently sending no dial at all)."
  (cond ((member level +effort-levels+) level)
        ((eq level :off) (first +effort-levels+))))

(defun normalize-effort (id effort)
  "Canonicalize a model's :effort declaration to a subset of +EFFORT-LEVELS+
in ladder order.  NIL means the model has no effort parameter; T means all
levels."
  (cond ((null effort) nil)
        ((eq effort t) +effort-levels+)
        ((and (listp effort)
              (every (lambda (l) (member l +effort-levels+)) effort))
         (remove-if-not (lambda (l) (member l effort)) +effort-levels+))
        (t (error (cat "register-model ~a: :effort must be nil, t, or a list of "
                       "~{~(~s~)~^ ~}, got ~s")
                  id +effort-levels+ effort))))

(defun clamp-effort (level supported)
  "The strongest level in SUPPORTED that does not exceed LEVEL, or NIL when
SUPPORTED is empty or offers nothing that low.  Clamping is what keeps a
session-wide :max usable on a model whose ladder stops at :high: the request
degrades instead of failing."
  (let ((want (position level +effort-levels+)))
    (when want
      (car (last (loop for l in +effort-levels+
                       for i from 0
                       when (and (<= i want) (member l supported))
                         collect l))))))

(defun register-model* (id &key provider (api :anthropic-messages)
                             context-window max-output
                             (vision t) effort (thinking-mode :effort-only)
                             max-input-items responses-options)
  "Register (or replace, keeping position) a model.  A model's identity is
its (id, provider) pair: the same id under different providers (direct vs.
proxy) are distinct, both selectable models; re-registering the same pair
replaces it in place.  Validates eagerly so a typo errors at init-load
time, not mid-run."
  (unless (and (stringp id) (plusp (length id)))
    (error "register-model: id must be a non-empty string, got ~s" id))
  (find-api api)                        ; unknown :api errors here
  (unless (keywordp provider)
    (error "register-model ~a: :provider must be a keyword, got ~s" id provider))
  (unless (and (integerp context-window) (plusp context-window))
    (error "register-model ~a: :context-window must be a positive integer, got ~s"
           id context-window))
  (unless (and (integerp max-output) (plusp max-output))
    (error "register-model ~a: :max-output must be a positive integer, got ~s"
           id max-output))
  (unless (or (null max-input-items)
              (and (integerp max-input-items) (plusp max-input-items)))
    (error "register-model ~a: :max-input-items must be a positive integer or nil, got ~s"
           id max-input-items))
  (unless (member thinking-mode '(:adaptive :effort-only))
    (error (cat "register-model ~a: :thinking-mode must be :adaptive "
                "or :effort-only, got ~s")
           id thinking-mode))
  (let ((model (list :id id :provider provider :api api
                     :context-window context-window :max-output max-output
                     :vision (and vision t)
                     :thinking-mode thinking-mode
                     :effort (normalize-effort id effort)
                     :max-input-items max-input-items
                     :responses-options responses-options))
        (tail (member t *models*
                      :key (lambda (m) (and (string= (pget m :id) id)
                                            (equal (pget m :provider) provider))))))
    (if tail
        (setf (car tail) model)         ; replace in place: picker position stable
        (setf *models* (append *models* (list model))))
    id))

(defun model-providers (id)
  "Providers registered for ID, in registration order.  More than one means
the bare id is ambiguous — callers disambiguate with FIND-MODEL's PROVIDER."
  (mapcar (lambda (m) (pget m :provider))
          (remove-if-not (lambda (m) (string= (pget m :id) id)) *models*)))

(defun all-models () *models*)

(defun find-model (id &optional provider)
  "Resolve a model id to its registered plist.  Plists pass through
\(call-provider convenience).  With PROVIDER, match that exact provider —
how a journaled /model choice names which of several same-id entries it
meant.  A bare id resolves to the FIRST registration of that id: the same
id under several providers is legal, so the default has to be a rule, and
registration order is the one the user wrote.  Session-level selection is
explicit (the picker journals :provider), so first-wins only ever decides
the initial default.  No fallback: an unknown id is a config error."
  (cond ((consp id) id)
        (provider
         (or (find-if (lambda (m) (and (string= (pget m :id) id)
                                       (equal (pget m :provider) provider)))
                      *models*)
             (error (cat "Unknown model ~s for provider ~(~a~).~%"
                         "Registered providers for ~s: ~:[none~;~:*~{~(~a~)~^, ~}~]~%"
                         "Register it in init.lisp or post-init.lisp:~%  "
                         "(evo:register-model ~s~%    "
                         ":provider ~(~s~)~%    "
                         ":context-window 200000 :max-output 64000)")
                    id provider id (model-providers id) id provider)))
        ((find id *models* :key (lambda (m) (pget m :id)) :test #'string=))
        (t (error (cat "Unknown model ~s: no registered model has that id.~%"
                       "Registered models: ~:[none — is your init.lisp or post-init.lisp missing?~;~:*~{~a~^, ~}~]~%"
                       "Register it in init.lisp or post-init.lisp:~%  "
                       "(evo:register-model ~s~%    "
                       ":provider :anthropic~%    "
                       ":context-window 200000 :max-output 64000)")
                  id (mapcar (lambda (m) (pget m :id)) *models*) id))))

(defun split-model-ref (text)
  "TEXT — a model reference as it is written on a command line — as
\(values ID PROVIDER): \"claude-opus-5\" is that id with no provider,
\"claude-opus-5@anthropic\" is that id served by :ANTHROPIC.  The **last** @
separates, so an id that contains one (\"gemini@2024\") still names its
provider when a provider is given.  Signals when the reference names no id."
  (let ((at (and (stringp text) (position #\@ text :from-end t))))
    (cond ((null at) (values text nil))
          ((zerop at) (error "~s names a provider but no model id" text))
          (t (values (subseq text 0 at)
                     (intern (string-upcase (subseq text (1+ at))) :keyword))))))

(defun find-model-ref (text)
  "Resolve a command-line model reference — ID, or ID@PROVIDER — to its
registered plist.  Errors exactly as FIND-MODEL does when nothing matches."
  (multiple-value-bind (id provider) (split-model-ref text)
    (find-model id provider)))

(defun model-context-window (model) (pget model :context-window))
(defun model-max-output (model) (pget model :max-output))
(defun model-max-input-items (model)
  "The endpoint's cap on input items (messages, tool calls, tool results,
reasoning blocks) per request, or NIL for no cap."
  (pget model :max-input-items))
(defun model-effort (model) (pget model :effort))
(defun model-thinking-mode (model) (or (pget model :thinking-mode) :effort-only))

(defun model-vision-p (model)
  "True when MODEL accepts image input.  Defaults to true for a model plist
that predates the flag: today's frontier models all see, and the alternative
default would silently blind a correctly-configured session."
  (if (member :vision model) (and (pget model :vision) t) t))

;;; Providers: endpoint + credential config.  Re-registration merges
;;; field-wise, so a later init file overrides only the keys it gives.

(defvar *providers* nil
  "Ordered alist of (provider-key . (:base-url s :api-key s :api-key-env s)).")

(defun register-provider* (key &rest kvs &key base-url api-key api-key-env)
  (declare (ignore base-url api-key api-key-env))
  (unless (keywordp key)
    (error "register-provider: key must be a keyword, got ~s" key))
  (let ((entry (assoc key *providers*)))
    (if entry
        (setf (cdr entry) (plist-merge (cdr entry) kvs))
        (setf *providers* (append *providers* (list (cons key kvs))))))
  key)

(defun provider-keys ()
  "Every registered provider key, in registration order."
  (mapcar #'car *providers*))

(defun registry-name (key)
  "KEY — a name a person writes in config (a provider key, an API key), a
string already carrying it, or NIL — as the name to publish: the key's own
spelling, dashes and all.

A registry key is a *name*, not an enum.  A provider is `:super-relay` in
init.lisp and `super-relay` in `--model id@super-relay`; an API is
`:anthropic-messages` where it is registered and in a model's `:api`.  Both
must read that way in a document too.  Published as a keyword they would not:
the JSON mapping snake_cases a keyword VALUE on purpose, for enums
\(:lane-report -> \"lane_report\"), so a catalog would say `providers[].name`
\"super-relay\" and `models[].provider` \"super_relay\" about the same thing,
and `models[].api` \"anthropic_messages\" about a name its own init.lisp
writes with dashes.

The rule is fixed here rather than in the mapping: which values are names is
the caller's to know (an API is, a lane state is not), and only the caller can
say so."
  (etypecase key
    (null nil)
    (string key)
    (symbol (string-downcase (symbol-name key)))))

(defun provider-registration (key)
  "What is registered for provider KEY, as given to REGISTER-PROVIDER — a
fresh plist of the fields set so far (:base-url :api-key :api-key-env), merged
across every call — or NIL when KEY is not registered.  Unresolved, so an
extension can tell what the user already configured (and only fill in the
rest); PROVIDER-CONFIG is what a request resolves."
  (copy-list (cdr (assoc key *providers*))))

(defun provider-config (key)
  "Resolved config for provider KEY: (:base-url ... :api-key ...).
Key resolution: explicit :api-key, else the :api-key-env variable, else
empty (some proxies need none)."
  (let ((conf (cdr (or (assoc key *providers*)
                       (error (cat "No provider ~s is registered — add~%  "
                                   "(evo:register-provider ~s :base-url \"https://...\" :api-key-env \"...\")~%"
                                   "to your init.lisp or post-init.lisp.")
                              key key)))))
    (let ((base-url (pget conf :base-url)))
      (unless base-url
        (error "Provider ~s has no :base-url — pass one to register-provider in your init.lisp."
               key))
      (list :base-url base-url
            :api-key (or (pget conf :api-key)
                         (let ((env (pget conf :api-key-env)))
                           (and env (getenv env)))
                         "")))))

(defun reset-user-registries ()
  "Clear models; re-seed providers from the registered APIs' defaults.
Called before each userspace boot so config re-evaluation is idempotent.
An API with no DEFAULT-PROVIDER-KEY seeds nothing — the usual case for an
extension-defined API, whose provider comes from init.lisp like any other."
  (setf *models* nil *providers* nil)
  (loop for (nil . api) in *apis*
        for key = (default-provider-key api)
        when key
          do (apply #'register-provider* key
                    (append (let ((url (default-base-url api)))
                              (when url (list :base-url url)))
                            (let ((env (default-api-key-env api)))
                              (when env (list :api-key-env env))))))
  nil)
