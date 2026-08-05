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

(defun extension-fasl-path (source-path)
  "Return the expected fasl path for SOURCE-PATH (same dir, .fasl extension)."
  (make-pathname :type "fasl" :defaults source-path))

(defun load-extension* (path &key (reason "loaded") journal (record t))
  "Compile + load PATH into userspace; journal a :load entry.
CL redefinition semantics: new definitions apply from the next call.
Stale fasl files are deleted before compilation so a changed source is
always recompiled (ECL may skip if the fasl is newer than the source)."
  (let* ((path (truename path))
         (journal (or journal *current-journal*)))
    ;; Delete any stale fasl to force recompilation.
    (let ((fasl (extension-fasl-path path)))
      (when (probe-file fasl)
        (delete-file fasl)))
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

;;; Snapshot/restore for extension registries so /reload is idempotent.
;;; reset-user-registries clears models and providers; we also need to clear
;;; extension-sourced commands, tools, and APIs so that removing a registration
;;; from an extension file takes effect on /reload.

(defvar *pre-extension-registries* nil
  "Saved state of all registries before extension loading.  Restored on
/reload so that re-running extensions is idempotent — registrations removed
from source are cleaned up.")

(defun snapshot-extension-registries ()
  "Save the current registry state; called before boot-extensions."
  (setf *pre-extension-registries*
        (list :commands (loop for k being the hash-keys of evo::*commands* collect k)
              :apis (mapcar #'car evo.provider::*apis*)
              :tools (loop for k being the hash-keys of *tool-registry* collect k))))

(defun restore-extension-registries ()
  "Remove registrations added by extensions (anything not in the pre-extension snapshot)."
  (when *pre-extension-registries*
    ;; Commands: remove entries not in the pre-extension set.
    (let ((keep (pget *pre-extension-registries* :commands)))
      (loop for k being the hash-keys of evo::*commands*
            unless (member k keep :test #'equal)
            do (remhash k evo::*commands*)))
    ;; APIs: keep only entries whose key was in the pre-extension set.
    (let ((keep (pget *pre-extension-registries* :apis)))
      (setf evo.provider::*apis*
            (remove-if-not (lambda (entry) (member (car entry) keep :test #'eq))
                           evo.provider::*apis*)))
    ;; Tools: remove entries not in the pre-extension set.
    (let ((keep (pget *pre-extension-registries* :tools)))
      (loop for k being the hash-keys of *tool-registry*
            unless (member k keep :test #'equal)
            do (remhash k *tool-registry*)))))

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
  (restore-extension-registries)
  (load-init-file (merge-pathnames "init.lisp" (evo-home)))
  (load-init-file (merge-pathnames "init.lisp" (project-evo-dir cwd)))
  (snapshot-extension-registries)
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
  '(:evo.port :evo.util :evo.media :evo.journal :evo.provider :evo.kernel
    :evo.cli :evo :evo.todo :evo.plan :evo.memory :evo.eval :evo.tui))

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
 (evo:register-model \"deepseek-v4-pro\" :provider :deepseek
   :api :anthropic-messages :context-window 1000000 :max-output 192000
   :thinking t :effort t)
A model's identity is its (id, provider) pair: register the same id under a
different provider and both are selectable (e.g. direct vs. proxy).
Re-registering the same pair replaces it in place; evo ships no built-in
models.

:effort declares the levels the model accepts for Anthropic's
output_config.effort — t for all of (:low :medium :high :xhigh :max), a
subset list for models that stop short (Opus 4.5 has no xhigh or max), nil
(the default) for models without the parameter.  A thinking level above
what the model supports is clamped down rather than rejected.  Worth
checking per endpoint rather than assuming: a third-party API that accepts
budget_tokens may quietly ignore it and steer on effort alone, and then a
model registered without :effort has a thinking dial wired to nothing.

:thinking is a capability, not a preference — nil for a model that does not
think.  There is no off level; the ladder starts at :low.

:thinking-mode is :extended (the default — thinking.budget_tokens) or
:adaptive, where the model decides when to think and evo sends a mode
instead of a budget.  Anthropic models from 4.6 on are :adaptive; on 4.7
and later budget_tokens is rejected outright.

:vision declares image input, and defaults to t.  Give :vision nil to a
text-only model: pasted images then degrade to a text placeholder for that
model instead of the endpoint rejecting every request that replays one."
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

(defun steer (text &optional (agent *agent*) (images nil))
  "Queue a steering message; picked up at the next turn boundary.
IMAGES, when given, is a list of :image content blocks — build them with
evo.media:attach-image-file or evo.media:clipboard-image."
  (evo.kernel:queue-steering agent text :images images))

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
