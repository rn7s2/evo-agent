;;;; catalog.lisp — GET /catalog: everything this session can be told to do.
;;;;
;;;; The GUI's chooser state, in one document: which models exist (and which
;;;; are usable right now, and why not), which providers have a key, the
;;;; language packs, the commands, the skills, the tools, and the ops with
;;;; their argument schemas.  Nothing here is ever a secret — a provider says
;;;; whether it HAS a key, never the key (CONTRACT §5.6).
;;;;
;;;; The builder is total: an entry that raises is dropped and named in
;;;; `warnings`, because one broken extension must not cost a client the whole
;;;; catalog (the old /registry answered 500 for the same case).

(in-package :evo.serve)

(defvar *server-for-catalog* nil
  "The server whose ops (and eval gate) the catalog should describe.  Bound
around a catalog build; a builder without a server describes every op.")

(defvar *kernel-apis* (api-keys)
  "The provider APIs a lane starts with: the ones the kernel bundled, captured
when serve was loaded — before any init file or extension could register
another.  A lane boots --no-userspace, so a model whose API is not here runs
in a lane only if the swarm's in-lanes code loaded that API's extension into
it, which no catalog can know without running a lane.")

(defun kernel-api-registry ()
  "The API registry a fresh `--no-userspace` lane boots with: *KERNEL-APIS*
and their instances.  A program that has to evaluate what a lane would end up
with (EVO.SWARM does, for `check` and `catalog`) rebinds the registry to this
as the lane's starting point — anything its own forms register lands on the
copy."
  (loop for key in *kernel-apis*
        for api = (ignore-errors (find-api key))
        when api collect (cons key api)))

(defun provider-api (key)
  "The API provider KEY belongs to: the one that seeds it, else the API of a
model registered under it, else NIL."
  (or (loop for api-key in (api-keys)
            for api = (ignore-errors (find-api api-key))
            when (and api (eq (default-provider-key api) key)) return api-key)
      (loop for model in (all-models)
            when (eq (pget model :provider) key) return (pget model :api))))

(defun provider-api-instance (key)
  "The API instance provider KEY's requests go through: the one that seeds it,
else the API of a model registered under it, else NIL.  PROVIDER-API answers
the API's registry key; this is the instance carrying its methods."
  (let ((api (provider-api key)))
    (and api (ignore-errors (find-api api)))))

(defun provider-has-key-p (key)
  "Whether provider KEY can authenticate right now — what the catalog's
`providers` half reports and what MODEL-READINESS asks before it calls a model
usable.  Its API decides (API-CREDENTIALS-AVAILABLE-P), so an API that keeps
its own credential — a token in a file, say — is not judged by the plain
environment-variable rule; a provider that names no API at all gets that rule.
The verdict only: no key, variable value or token is ever quoted
\(CONTRACT §5.6)."
  (let ((registration (provider-registration key))
        (api (provider-api-instance key)))
    (if api
        (api-credentials-available-p api registration)
        (registration-credentials-available-p registration))))

(defun model-reasoning-p (model)
  "Whether MODEL can be asked to think: an effort ladder, or an adapter that
decides for itself when to think."
  (and (or (evo.provider:model-effort model)
           (eq (evo.provider:model-thinking-mode model) :adaptive))
       t))

(defun model-readiness (model)
  "Whether a session could run MODEL now: (values ready reason code).  REASON
is a short sentence naming what is missing — a provider, its address, or the
variable its key comes from — and is NIL when nothing is.  CODE is the same
fact as a machine code (\"provider_unregistered\", \"no_base_url\",
\"no_api_key\") for a caller that has to branch on it.  Neither ever quotes a
value out of the configuration.

The credential is the providing API's to judge (PROVIDER-HAS-KEY-P), so an
API that keeps its own is not judged by the environment-variable rule.  Only a
provider that *declares* a credential source is asked for one — a stock local
endpoint registered with a bare :base-url needs no key, and is not refused
one."
  (let* ((provider (pget model :provider))
         (registration (ignore-errors (provider-registration provider))))
    (cond
      ((null registration)
       (values nil "its provider is not registered" "provider_unregistered"))
      ((null (getf registration :base-url))
       (values nil "its provider has no address configured" "no_base_url"))
      ((and (or (getf registration :api-key) (getf registration :api-key-env))
            (not (provider-has-key-p provider)))
       (values nil (if (getf registration :api-key-env)
                       (format nil "no API key: set ~a" (getf registration :api-key-env))
                       "no API key configured for its provider")
               "no_api_key"))
      (t (values t nil nil)))))

(defun model-status (model)
  "MODEL as a launch-time verdict: (:id :provider :ok :reason)."
  (multiple-value-bind (ready reason) (model-readiness model)
    (list :id (pget model :id) :provider (pget model :provider)
          :ok ready :reason reason)))

(defun lane-model-status (model &optional (apis *kernel-apis*))
  "MODEL as a lane would see it: the same verdict, and not runnable at all
when its API is one a lane does not have.  APIS is the API set of the lane in
question — the kernel's by default, which is what a lane boots with before
anyone has worked out what it ends up with; the swarm's offline check passes
the set its evaluation of the lane forms ended with."
  (multiple-value-bind (ready reason) (model-readiness model)
    (if (member (pget model :api) apis)
        (list :id (pget model :id) :provider (pget model :provider)
              :ok ready :reason reason)
        (list :id (pget model :id) :provider (pget model :provider)
              :ok nil
              :reason (format nil "its API ~(~a~) comes from an extension: load that extension in the lanes with (evo.swarm:in-lanes ...) in swarm.lisp"
                              (pget model :api))))))

(defun lane-model-entry (status)
  "A LANE-MODEL-STATUS as the catalog's lane half carries it."
  (list :id (getf status :id) :provider (getf status :provider)
        :ok (wire-boolean (getf status :ok)) :reason (getf status :reason)))

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
is dropped and named in WARNINGS — its condition never reaches the caller, and
nothing but the entry's own name is quoted."
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
          :has-key (wire-boolean (provider-has-key-p key))
          :key-env (getf registration :api-key-env))))

(defun catalog-commands ()
  (loop for (name . description) in (evo.command:command-catalog)
        collect (list :name name :description description :args-hint nil)))

(defun lane-catalog (warnings &optional (apis *kernel-apis*))
  "The lane half computed from the API set alone (APIS, the kernel's by
default): every registered model, judged with this session's registries.
That is what a program that runs lanes but has not worked out what a lane
would end up with can say — evo-swarm's live coordinator, whose lanes are
already running the code that would tell it.  Its offline `check` and
`catalog` evaluate the lane forms instead and pass the result to
CATALOG-PLIST rather than calling this."
  (list :models (catalog-entries
                 (all-models)
                 (lambda (m) (pget m :id))
                 (lambda (model) (lane-model-entry (lane-model-status model apis)))
                 warnings "lane model")))

(defun catalog-plist (agent &key swarm)
  "Everything a client needs to offer this session's choices (CONTRACT §5.6).
AGENT is the session it describes; SWARM, when given, is the program-specific
half — the models a lane can run — which a program that runs lanes passes in.
A program that has worked out what a lane ends up with passes that (evo-swarm
evaluates the lane forms for its offline `catalog`); one that only knows it
runs lanes passes (LANE-CATALOG warnings), the kernel-API-set answer."
  (let ((warnings nil)
        (state (ignore-errors (fold-state (agent-journal agent)))))
    (let* ((default-id (or (and state (evo.journal:state-model state))
                             (setting :model)))
             ;; The provider is the one the *model* is registered under — the
             ;; same answer /catalog gives for the model itself — so a client
             ;; that looks the pair up in MODELS finds it.  A session-level
             ;; override only decides between registrations of one id.
             (lanes (handler-case
                        (or swarm
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
