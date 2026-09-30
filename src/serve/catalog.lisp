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

(defvar *catalog-lanes-hook* nil
  "The `lanes` half of GET /catalog (CONTRACT §5.6), for a program that runs
lanes: a function of one argument — the WARNINGS list the catalog is filling —
returning that half's plist.  A program registers its own:

  (setf evo.serve:*catalog-lanes-hook* #'evo.serve:lane-catalog)

A server that runs no lanes leaves this NIL, and the document then has no
`lanes` key at all — the contract makes that half the swarm's, and a null is
not an object a client can read .models from.  (The offline CLI passes its
half as CATALOG-PLIST's :SWARM argument instead, since it builds the document
without a server.)")

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

(defun catalog-model (model)
  (multiple-value-bind (ready reason) (model-readiness model)
    (list :id (pget model :id)
          :provider (pget model :provider)
          ;; The registry has no separate display name: the id is what a person
          ;; types and what a picker shows.
          :name (or (pget model :name) (pget model :id))
          :api (provider-api (pget model :provider))
          :context-window (pget model :context-window)
          ;; The verdicts are Lisp booleans; the document's are JSON ones.
          :reasoning (wire-boolean (model-reasoning-p model))
          :images (wire-boolean (ignore-errors (evo.provider:model-vision-p model)))
          :ready (wire-boolean ready)
          :reason reason)))

(defun catalog-provider (key)
  (let ((registration (provider-registration key)))
    (list :name (string-downcase (symbol-name key))
          :api (provider-api key)
          :has-key (wire-boolean (provider-has-key-p registration))
          :key-env (getf registration :api-key-env))))

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
                           :ok (wire-boolean (getf status :ok))
                           :reason (getf status :reason))))
                 warnings "lane model")))

(defun catalog-plist (agent &key swarm)
  "Everything a client needs to offer this session's choices (CONTRACT §5.6).
AGENT is the session it describes; SWARM, when given, is the program-specific
part (the models a lane can run) — evo-swarm passes it."
  (let ((warnings nil)
        (state (ignore-errors (fold-state (agent-journal agent)))))
    (let* ((default-id (or (and state (evo.journal:state-model state))
                             (setting :model)))
             ;; The provider is the one the *model* is registered under — the
             ;; same answer /catalog gives for the model itself — so a client
             ;; that looks the pair up in MODELS finds it.  A session-level
             ;; override only decides between registrations of one id.
             (lanes (handler-case
                        (or (and swarm (lane-catalog warnings))
                            (and *catalog-lanes-hook*
                                 (funcall *catalog-lanes-hook* warnings)))
                      (error ()
                        (push "the lanes half could not be read and was left out"
                              warnings)
                        nil)))
             (default-provider (and default-id
                                    (or (ignore-errors
                                          (pget (find-model
                                                 default-id
                                                 (ignore-errors
                                                  (evo.kernel:effective-model-provider
                                                   state default-id)))
                                                :provider))
                                        (ignore-errors (evo.kernel:effective-model-provider
                                                        state default-id))))))
        (let ((doc (list :models (catalog-entries (all-models)
                                                  (lambda (m) (pget m :id))
                                                  #'catalog-model warnings "model")
                         :providers (catalog-entries (provider-keys)
                                                     (lambda (k) (string-downcase (symbol-name k)))
                                                     #'catalog-provider warnings "provider")
                         :default-model (and default-id (list :id default-id :provider default-provider))
                         ;; Exactly the levels the session accepts, in the
                         ;; order /thinking takes them: the ladder has no off
                         ;; rung — the CLI, /thinking and evo-swarm all refuse
                         ;; one, and a retired :off found in an old journal or
                         ;; init.lisp is normalized onto the lowest rung, not
                         ;; listed here as if it were a choice.
                         :thinking-levels (coerce (mapcar (lambda (level)
                                                            (string-downcase (symbol-name level)))
                                                          +effort-levels+)
                                                  'vector)
                         :languages (coerce (loop for pack in (all-prompt-languages)
                                                  collect (list :code (pget pack :code)
                                                                :name (or (pget pack :name)
                                                                          (pget pack :native)
                                                                          (pget pack :code))))
                                            'vector)
                         :ops (coerce (if *server-for-catalog*
                                          (server-ops-catalog *server-for-catalog*)
                                          (loop for op in (all-ops)
                                                collect (list :name (op-name op)
                                                              :args (op-arg-schema op)
                                                              :precondition
                                                              (string-downcase
                                                               (symbol-name (op-precondition op))))))
                                      'vector)
                         :commands (coerce (catalog-commands) 'vector)
                         :skills (coerce (loop for skill in (available-skills)
                                               collect (list :name (pget skill :name)
                                                             :description (pget skill :description)))
                                         'vector)
                         :tools (coerce (loop for name in (all-tool-names)
                                              for tool = (find-tool name)
                                              collect (list :name name
                                                            :description (and tool (tool-description tool))))
                                        'vector)
                         :warnings (coerce (nreverse warnings) 'vector))))
          ;; evo-swarm's half, when a program answered for it (CONTRACT §5.6,
          ;; §9): absent rather than null, so a client sees "not a swarm"
          ;; rather than nothing to read.
          (if lanes (list* :lanes lanes doc) doc)))))

(defmethod catalog-for-server ((server server) agent)
  (let ((*server-for-catalog* server))
    (catalog-plist agent)))

;;; GET /sessions (CONTRACT §2, §5.6): the same body the offline CLI prints.

(defun epoch-ms (universal-time)
  "A universal-time (seconds) as epoch milliseconds, which is what the
protocol's times are."
  (when universal-time
    (* 1000 (- universal-time (encode-universal-time 0 0 0 1 1 1970 0)))))

(defun session-record (path)
  "One session, as the session index and the offline CLI describe it
(CONTRACT §2).  Fields the journal does not carry read as null rather than as
a guess; the index (its own work package) fills them in."
  (let* ((journal (ignore-errors (open-journal path)))
         (header (and journal (journal-header journal)))
         (updated (ignore-errors (file-write-date path))))
    (list :id (and header (pget header :id))
          :path (namestring path)
          :cwd (and header (pget header :cwd))
          :program (and header (pget header :program))
          :swarm-id (and header (pget header :swarm-id))
          :title (and journal
                      (truncate-string (or (evo.command:first-user-prompt journal) "") 80))
          :created-at (epoch-ms updated)
          :updated-at (epoch-ms updated)
          :entries (if journal (length (journal-entries journal)) 0))))

(defun sessions-body (&key all cwd program)
  "{\"sessions\": […]} — the CLI's body and GET /sessions' (CONTRACT §2).
ALL takes every directory; PROGRAM keeps the sessions a given program wrote."
  (let* ((paths (if all
                    (ignore-errors
                      (directory (merge-pathnames "*.sexp"
                                                  (evo.journal:sessions-directory
                                                   (or cwd (uiop:getcwd))))))
                    (ignore-errors (mapcar (lambda (s) (pget s :path))
                                           (list-sessions (or cwd (uiop:getcwd)))))))
         (records (remove nil (mapcar (lambda (path)
                                        (handler-case (session-record path)
                                          (error () nil)))
                                      paths))))
    (list :sessions (coerce (if program
                                (remove-if-not (lambda (r) (equal (getf r :program) program))
                                               records)
                                records)
                            'vector))))
