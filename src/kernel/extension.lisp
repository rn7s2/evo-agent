;;;; extension.lisp — self-extension and the public API.
;;;;
;;;; The EVO package is the whole public surface: register-tool,
;;;; register-command, event hooks, load-extension.  Core and user extensions
;;;; build on it; nothing bypasses it.  Userspace code lives in EVO.USER and
;;;; is rebuilt from source on every boot: boot = load kernel, then
;;;; replay the path's :load entries against the source files on disk.

(in-package :evo.kernel)

(defvar *current-journal* nil
  "Journal that :load entries are appended to during extension loading.")

(defvar *loaded-extension-paths* nil
  "Truenames loaded this boot; :load replay skips them.")

(defun load-extension* (path &key (reason "loaded") journal (record t))
  "Compile + load PATH into userspace; journal a :load entry.
CL redefinition semantics: new definitions apply from the next call."
  (let* ((path (truename path))
         (journal (or journal *current-journal*)))
    (let ((*package* (find-package :evo.user)))
      (handler-bind ((warning #'muffle-warning))
        (multiple-value-bind (fasl warnings-p failure-p)
            (compile-file path :verbose nil :print nil)
          (declare (ignore warnings-p))
          (when failure-p
            (error "Extension ~a failed to compile" path))
          (load fasl))))
    (pushnew (namestring path) *loaded-extension-paths* :test #'equal)
    (when (and record journal)
      (append-entry journal (list :type :load
                                  :path (namestring path)
                                  :reason reason)))
    path))

(defun extension-files (directory)
  (sort (directory (merge-pathnames "*.lisp" directory))
        #'string< :key #'namestring))

(defun boot-extensions (&key journal (cwd (uiop:getcwd)))
  "Load global then project extension directories, journaling each."
  (dolist (dir (list (merge-pathnames "extensions/" (evo-home))
                     (merge-pathnames "extensions/" (project-evo-dir cwd))))
    (dolist (file (extension-files dir))
      (handler-case
          (load-extension* file :reason "boot" :journal journal)
        (error (e)
          (warn "Skipping extension ~a: ~a" file e))))))

(defun load-init-file (path)
  "Evaluate a config file in userspace.  Plain load, no compile-file: no
fasl litter next to config, and ECL evaluates it without the compiler."
  (when (probe-file path)
    (let ((*package* (find-package :evo.user)))
      (handler-case (load path :verbose nil :print nil)
        (error (e)
          (format *error-output* "~&evo: error in init file ~a: ~a~%" path e)))))
  path)

(defun boot-userspace (&key journal (cwd (uiop:getcwd)))
  "Reset user registries and settings, evaluate init files (global then
project — an override is just a later call), then load extension
directories, then evaluate post-init files (global then project) so
extensions can register models before post-init picks a default.
Init files are environment, not history: re-evaluated every
boot, never journaled (unlike extension :load entries), so the reset makes
this idempotent for /reload and repeated boots."
  (evo.util:reset-settings)
  (evo.provider:reset-user-registries)
  (load-init-file (merge-pathnames "init.lisp" (evo-home)))
  (load-init-file (merge-pathnames "init.lisp" (project-evo-dir cwd)))
  (boot-extensions :journal journal :cwd cwd)
  (load-init-file (merge-pathnames "post-init.lisp" (evo-home)))
  (load-init-file (merge-pathnames "post-init.lisp" (project-evo-dir cwd))))

(defun replay-loads (state &key journal)
  "Replay a resumed session's :load entries against the files on disk.
Files already loaded this boot are skipped; missing files are reported, not
fatal — a corrupted runtime is repaired by fixing/removing a source file."
  (declare (ignore journal))
  (dolist (entry (evo.journal:state-loads state))
    (let ((path (pget entry :path)))
      (unless (member path *loaded-extension-paths* :test #'equal)
        ;; Progress goes to stderr so a supervisor quarantining a failed
        ;; boot can report which :load entry was reached.
        (format *error-output* "~&evo: replaying :load ~a~%" path)
        (handler-case
            (if (probe-file path)
                (load-extension* path :reason "replay" :record nil)
                (warn "Journaled :load file missing, skipping: ~a" path))
          (error (e)
            (warn "Replaying :load of ~a failed: ~a" path e)))))))

(defparameter *kernel-packages*
  '(:evo.port :evo.util :evo.journal :evo.provider :evo.kernel :evo.cli :evo
    :evo.todo :evo.plan :evo.memory :evo.tui))

(defun lock-kernel-packages ()
  "Package locks: permissive but not suicidal — touching the kernel
requires an explicit, auditable evo.port:unlock-package."
  (dolist (name *kernel-packages*)
    (let ((pkg (find-package name)))
      (when pkg (evo.port:lock-package pkg)))))

;;; The public API (EVO package).

(in-package :evo)

(defvar *agent* nil
  "The live agent, bound by the CLI for the duration of a session.")

(defmacro register-tool (name &key description schema execute)
  "Register a tool.  NAME is a string; SCHEMA is a sexpr schema
\(:object (prop :type :string :description \"...\") ...); EXECUTE is a
function of one argument (the args plist) returning the model-visible
content string (optionally (values content details))."
  `(evo.kernel:register-tool* :name ,name :description ,description
                              :schema ,schema :execute ,execute
                              :source :extension))

(defvar *commands* (make-hash-table :test #'equal)
  "Slash commands.  The MVP has no TUI; commands registered here are
resolved by the CLI's --command flag and by future frontends.")

(defun register-command (name fn &key description)
  (setf (gethash name *commands*) (list :fn fn :description description))
  name)

(defun on (event fn)
  "Subscribe FN to a kernel event: :session-start :turn-end :tool-call ...
A :tool-call hook may return (:block t :reason ...) or (:arguments ...)."
  (evo.kernel:add-hook event fn))

(defun load-extension (path &key (reason "requested"))
  (evo.kernel:load-extension* path :reason reason))

;;; Config (init.lisp) API: models, providers, settings.

(defun register-model (id &rest args)
  "Register a model in init.lisp:
 (evo:register-model \"claude-sonnet-5\" :provider :anthropic
   :api :anthropic-messages :context-window 200000 :max-output 64000
   :thinking t)
Re-registering an id replaces it in place; evo ships no built-in models."
  (apply #'evo.provider:register-model* id args))

(defun register-provider (key &rest args)
  "Register or override a provider endpoint in init.lisp:
 (evo:register-provider :anthropic :base-url \"http://127.0.0.1:8787\"
   :api-key \"sk-...\")   ; or :api-key-env \"ANTHROPIC_API_KEY\"
Stock endpoints for the kernel APIs are pre-seeded; overriding merges
field-wise."
  (apply #'evo.provider:register-provider* key args))

(defun set-setting (key value)
  "Set a setting from init.lisp, e.g. (evo:set-setting :model \"...\")."
  (evo.util:set-setting key value))

(defun setting (key &optional default)
  (evo.util:setting key default))

(defun set-active-tools (agent names)
  "Journal a :tools-change entry; takes effect at the next save point.
NAMES nil restores the full registered tool set."
  (evo.journal:append-entry (evo.kernel:agent-journal agent)
                            (list :type :tools-change
                                  :tools (coerce (or names (evo.kernel:all-tool-names))
                                                 'vector))))

(defun all-tools ()
  (evo.kernel:all-tool-names))

(defun current-goal (&optional (agent *agent*))
  (evo.kernel:current-goal agent))

(defun steer (text &optional (agent *agent*))
  "Queue a steering message; picked up at the next turn boundary."
  (evo.kernel:queue-steering agent text))

(defun inject-context (text &key key (agent *agent*))
  "Append a :custom-message entry — content visible to the LLM.  With KEY, a
:transform-context hook can filter it back out later (mode discipline)."
  (evo.journal:append-entry
   (evo.kernel:agent-journal agent)
   (append (list :type :custom-message
                 :message (list :role :user
                                :content (list (list :type :text :text text))))
           (when key (list :key key)))))

(defun custom-state (key &optional (agent *agent*))
  "Current value of extension state KEY (fold over :custom entries)."
  (evo.journal:custom-state
   (evo.journal:fold-state (evo.kernel:agent-journal agent)) key))

(defun set-custom-state (key data &optional (agent *agent*))
  "Persist extension state: appends a :custom entry (invisible to the LLM;
survives restart and compaction untouched)."
  (evo.journal:append-entry (evo.kernel:agent-journal agent)
                            (list :type :custom :key key :data data))
  data)
