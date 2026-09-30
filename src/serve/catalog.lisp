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

(defun provider-has-key-p (registration)
  (let ((key (getf registration :api-key))
        (env (getf registration :api-key-env)))
    (and (or (and (stringp key) (plusp (length key)))
             (and env (plusp (length (or (getenv env) "")))))
         t)))

(defun catalog-model (model)
  (let* ((id (pget model :id))
         (provider (pget model :provider))
         (registration (ignore-errors (provider-registration provider))))
    (list :id id
          :provider provider
          :name (or (pget model :name) id)
          :api (ignore-errors (pget registration :api))
          :context-window (pget model :context-window)
          :reasoning (and (or (pget model :effort) (pget model :thinking-mode)) t)
          :images (and (ignore-errors (evo.provider:model-vision-p model)) t)
          :ready (if (provider-has-key-p registration) t 'false)
          :reason (unless (provider-has-key-p registration) "no_api_key"))))

(defun catalog-provider (key)
  (let ((registration (provider-registration key)))
    (list :name key
          :api (ignore-errors (pget registration :api))
          :has-key (if (provider-has-key-p registration) t 'false)
          :key-env (getf registration :api-key-env))))

(defun catalog-commands ()
  (loop for (name . description) in (evo.command:command-catalog)
        collect (list :name name :description description :args-hint nil)))

(defun catalog-plist (agent &key swarm)
  "Everything a client needs to offer this session's choices (CONTRACT §5.6).
AGENT is the session it describes; SWARM, when given, is the program-specific
part (the models a lane can run) — evo-swarm passes it."
  (let ((warnings nil)
        (state (ignore-errors (fold-state (agent-journal agent)))))
    (flet ((collect (what fn)
             (handler-case (funcall fn)
               (error ()
                 (push (format nil "~a could not be encoded" what) warnings)
                 nil))))
      (let* ((models (remove nil (mapcar (lambda (m) (collect "a model" (lambda () (catalog-model m))))
                                         (all-models))))
             (default-id (or (and state (evo.journal:state-model state))
                             (setting :model)))
             (default-provider (and default-id
                                    (ignore-errors (evo.kernel:effective-model-provider
                                                    state default-id)))))
        (list :models (coerce models 'vector)
              :providers (coerce (remove nil
                                         (mapcar (lambda (key)
                                                   (collect "a provider"
                                                            (lambda () (catalog-provider key))))
                                                 (provider-keys)))
                                 'vector)
              :default-model (and default-id (list :id default-id :provider default-provider))
              ;; The levels the session accepts: "off" plus the effort
              ;; ladder, in the order /thinking takes them.
              :thinking-levels (coerce (cons "off"
                                             (mapcar #'string-downcase
                                                     (mapcar #'symbol-name +effort-levels+)))
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
              :lanes (when swarm swarm)
              :warnings (coerce (nreverse warnings) 'vector))))))

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
