;;;; catalog.lisp — what this session can use, as one document.
;;;;
;;;; GET /catalog answers it live, and `evo-agent catalog --json` /
;;;; `evo-swarm catalog --json` print the same thing with no listener at all
;;;; (docs/serve.md): the models, the providers and whether they have a key,
;;;; the thinking ladder, the languages, the tools, skills and commands, and
;;;; — for a swarm — the models a lane can run.
;;;;
;;;; Two rules hold over the whole document.  **No key ever appears**: a
;;;; provider says whether it has one and which variable it comes from, never
;;;; the key itself.  And **one bad entry never takes the document down**: an
;;;; entry that cannot be built is left out and named in `warnings`, because
;;;; a catalog is exactly what a client reads when something is wrong with the
;;;; configuration.

(in-package :evo.serve)

(defvar *kernel-apis* (api-keys)
  "The provider APIs a lane starts with: the ones the kernel bundled, captured
when serve was loaded — before any init file or extension could register
another.  A lane boots --no-userspace, so a model whose API is not here runs
in a lane only if the swarm's in-lanes code loaded that API's extension into
it, which no catalog can know without running a lane.")

(defvar *op-catalog* nil
  "The POST /ops operations, as the ops layer registers them: a list of
(:name NAME :args SCHEMA :precondition :none|:idle|:quiescent).  Empty until
a program fills it; the catalog is still complete without it.")

;;; Readiness: could this session run this model right now?

(defun provider-has-key-p (registration)
  "Whether PROVIDER (a provider-registration plist) has a usable key: one set
with :api-key, or the variable it names holding something."
  (or (plusp (length (or (getf registration :api-key) "")))
      (let ((env (getf registration :api-key-env)))
        (and env (plusp (length (or (getenv env) "")))))))

(defun model-readiness (model)
  "Whether a session could run MODEL now: (values ready reason code).  REASON
is a short sentence naming what is missing — a provider, its address, or the
variable its key comes from — and is NIL when there is nothing missing.  CODE
is the same fact as a machine code (\"provider_unregistered\", \"no_base_url\",
\"no_api_key\") for a caller that has to branch on it.  Neither ever quotes a
value out of the configuration."
  (let ((reg (provider-registration (pget model :provider))))
    (cond
      ((null reg) (values nil "its provider is not registered" "provider_unregistered"))
      ((null (getf reg :base-url))
       (values nil "its provider has no address configured" "no_base_url"))
      ((and (null (getf reg :api-key))
            (let ((env (getf reg :api-key-env)))
              (and env (zerop (length (or (getenv env) ""))))))
       (values nil (format nil "no API key: set ~a" (getf reg :api-key-env)) "no_api_key"))
      (t (values t nil nil)))))

(defun model-reasoning-p (model)
  "Whether MODEL can be asked to think: an effort ladder, or an adapter that
decides for itself when to think."
  (and (or (evo.provider:model-effort model)
           (eq (evo.provider:model-thinking-mode model) :adaptive))
       t))

(defun model-status (model)
  "MODEL as a launch-time verdict: (:id :provider :ok :reason)."
  (multiple-value-bind (ready reason) (model-readiness model)
    (list :id (pget model :id) :provider (pget model :provider)
          :ok ready :reason reason)))

(defun lane-model-status (model)
  "MODEL as a lane would see it: the same verdict, and not runnable at all
when its API is one a lane does not have."
  (multiple-value-bind (ready reason) (model-readiness model)
    (if (member (pget model :api) *kernel-apis*)
        (list :id (pget model :id) :provider (pget model :provider)
              :ok ready :reason reason)
        (list :id (pget model :id) :provider (pget model :provider)
              :ok nil
              :reason (format nil "its API ~(~a~) comes from an extension: load that extension in the lanes with (evo.swarm:in-lanes ...) in swarm.lisp"
                              (pget model :api))))))

;;; Building the document, one isolated entry at a time.

(defun catalog-entries (items name-of build warnings what)
  "BUILD each of ITEMS into an entry, in order.  An entry that cannot be built
is dropped and named in WARNINGS — never lets its condition reach the caller,
and never quotes anything but the entry's own name."
  (let ((out nil))
    (dolist (item items (coerce (nreverse out) 'vector))
      (let ((name (ignore-errors (funcall name-of item))))
        (handler-case (push (funcall build item) out)
          (error ()
            (push (if name
                      (format nil "~a ~a could not be read and was left out" what name)
                      (format nil "a ~a entry could not be read and was left out" what))
                  warnings)))))))

(defun model-catalog (warnings)
  (catalog-entries
   (all-models)
   (lambda (m) (pget m :id))
   (lambda (model)
     (multiple-value-bind (ready reason) (model-readiness model)
       (list :id (pget model :id)
             :provider (pget model :provider)
             ;; The registry has no separate display name: the id is what a
             ;; person types and what a picker shows.
             :name (pget model :id)
             :api (pget model :api)
             :context-window (pget model :context-window)
             :reasoning (model-reasoning-p model)
             :images (evo.provider:model-vision-p model)
             :ready ready
             :reason reason)))
   warnings "model"))

(defun provider-api (key)
  "The API provider KEY belongs to: the one that seeds it, else the API of a
model registered under it, else NIL."
  (or (loop for api-key in (api-keys)
            for api = (ignore-errors (find-api api-key))
            when (and api (eq (default-provider-key api) key)) return api-key)
      (loop for model in (all-models)
            when (eq (pget model :provider) key) return (pget model :api))))

(defun provider-catalog (warnings)
  (catalog-entries
   (provider-keys)
   (lambda (key) (string-downcase (symbol-name key)))
   (lambda (key)
     (let ((reg (provider-registration key)))
       (list :name (string-downcase (symbol-name key))
             :api (provider-api key)
             :has-key (provider-has-key-p reg)
             :key-env (getf reg :api-key-env))))
   warnings "provider"))

(defun language-catalog ()
  (coerce (loop for pack in (all-prompt-languages)
                collect (list :code (getf pack :code) :name (getf pack :name)))
          'vector))

(defun command-catalog-entries ()
  (coerce (loop for (name . description) in (evo.command:command-catalog)
                collect (list :name name :description description :args-hint nil))
          'vector))

(defun skill-catalog ()
  (coerce (loop for skill in (available-skills)
                collect (list :name (getf skill :name)
                              :description (getf skill :description)))
          'vector))

(defun tool-catalog ()
  (coerce (loop for name in (all-tool-names)
                for tool = (find-tool name)
                collect (list :name name
                              :description (and tool (tool-description tool))))
          'vector))

(defun session-model (agent)
  "The model this session's next turn runs on, or NIL when it does not
resolve — the second half of `default_model` (the first half, the id, is kept
even when it does not resolve, so a client can say what is wrong with it)."
  (handler-case (effective-model (fold-state (agent-journal agent)) agent)
    (error () nil)))

(defun default-model-entry (agent)
  (let* ((state (fold-state (agent-journal agent)))
         (id (handler-case (effective-model-id state agent) (error () nil)))
         (model (session-model agent)))
    (when (or id model)
      (list :id (or (pget model :id) id)
            :provider (pget model :provider)))))

(defun lane-catalog (swarm warnings)
  "The lane half of a swarm's catalog: the models a lane can run, computed
from the kernel API set without starting one.  SWARM is the swarm's lane
configuration as a plist (:lane-model :lane-provider :lane-thinking :workers)."
  (declare (ignore swarm))
  (list :models (catalog-entries
                 (all-models)
                 (lambda (m) (pget m :id))
                 (lambda (model)
                   (let ((status (lane-model-status model)))
                     (list :id (getf status :id) :provider (getf status :provider)
                           :ok (getf status :ok) :reason (getf status :reason))))
                 warnings "lane model")))

(defun catalog-plist (agent &key swarm ops)
  "CONTRACT §5.6: everything this session can use, as one document.  AGENT
supplies the session's default model; SWARM, when given, adds the lane half
(evo-swarm only) — a plist (:lane-model :lane-provider :lane-thinking
:workers) describing how lanes are configured.  OPS is the POST /ops
catalogue; it defaults to *OP-CATALOG*, which is where the ops layer puts it.
An entry that cannot be built is left out and named in WARNINGS."
  (let ((warnings nil))
    (append
     (list :models (model-catalog warnings)
           :providers (provider-catalog warnings)
           :default-model (default-model-entry agent)
           :thinking-levels (coerce (mapcar (lambda (level)
                                              (string-downcase (symbol-name level)))
                                            +effort-levels+)
                                    'vector)
           :languages (language-catalog)
           :ops (coerce (or ops *op-catalog*) 'vector)
           :commands (command-catalog-entries)
           :skills (skill-catalog)
           :tools (tool-catalog))
     (when swarm (list :lanes (lane-catalog swarm warnings)))
     (list :warnings (coerce (nreverse warnings) 'vector)))))
