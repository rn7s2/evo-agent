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

;;; Extension ownership.
;;;
;;; Everything an extension file registers while it loads belongs to that file's
;;; OWNER token: hooks, background tasks, function patches, and any cleanup it
;;; registers itself.  A reload bumps the generation, so the previous
;;; generation's registrations can be disposed wholesale instead of accumulating
;;; — the difference between "load ran twice" and "the effect happens twice".

(defvar *extension-generation* 0
  "Bumped once per userspace boot.  Owners carry the generation they were
created in; disposal targets owners from earlier generations.")

(defstruct (extension-owner (:constructor %make-extension-owner))
  path          ; namestring of the extension source, or NIL for the kernel
  generation)

(defvar *extension-disposers* nil
  "Alist (OWNER . THUNK), newest first.  Run when OWNER's generation is
disposed; each thunk must be idempotent and must not signal.")

(defvar *extension-tasks* nil
  "Alist (OWNER . TASK) of background tasks an extension owns.  A task is a
plist (:name :thread :stop) — disposal calls :stop and then joins :thread, so a
poller never outlives the generation that started it.")

(defun register-extension-disposer (thunk &key (owner *extension-owner*))
  "Register THUNK to run when OWNER's generation is disposed."
  (push (cons owner thunk) *extension-disposers*)
  thunk)

(defun register-extension-task (&key name thread stop (owner *extension-owner*))
  "Track a background THREAD owned by an extension.  STOP, when given, is
called first and should make the thread return promptly."
  (push (cons owner (list :name name :thread thread :stop stop))
        *extension-tasks*)
  thread)

(defvar *task-stop-seconds* 5
  "How long disposal waits for a tracked task's thread after calling its stop
function.  A task that ignores its stop is abandoned with a warning naming it
— one leaked thread and a loud message beat a reload that hangs forever.")

(defun stop-extension-task (task)
  "Stop TASK and reap its thread.  The join is bounded: an unbounded join
would let one misbehaving extension freeze every future /reload with no
diagnosis, which is an accident the kernel should not allow (extensions get to
break things deliberately, not by forgetting to honour their own stop flag)."
  (let ((stop (pget task :stop))
        (thread (pget task :thread)))
    (when stop (ignore-errors (funcall stop)))
    (when thread
      (loop repeat (ceiling (* *task-stop-seconds* 20))
            while (bt:thread-alive-p thread)
            do (sleep 0.05))
      (if (bt:thread-alive-p thread)
          (warn "Extension task ~s did not stop within ~ds; abandoning its thread"
                (pget task :name) *task-stop-seconds*)
          (ignore-errors (bt:join-thread thread))))))

(defun dispose-extension-owners (&key (before *extension-generation*))
  "Dispose every owner from a generation older than BEFORE: stop and join its
tasks, then run its disposers, then withdraw its hooks.  Tasks go first — a
disposer may free something a running task is still reading."
  (flet ((stale-p (owner)
           (and owner (< (extension-owner-generation owner) before))))
    (let ((stale-tasks (remove-if-not #'stale-p *extension-tasks* :key #'car))
          (stale-disposers (remove-if-not #'stale-p *extension-disposers* :key #'car)))
      (setf *extension-tasks*
            (remove-if #'stale-p *extension-tasks* :key #'car)
            *extension-disposers*
            (remove-if #'stale-p *extension-disposers* :key #'car))
      (dolist (entry stale-tasks) (stop-extension-task (cdr entry)))
      (dolist (entry stale-disposers) (ignore-errors (funcall (cdr entry))))
      ;; Hooks last, and swept by generation rather than by the owners seen
      ;; above: a hook may be the only thing an extension registered.
      (remove-hooks-if (lambda (entry) (stale-p (hook-entry-owner entry))))))
  t)

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
    (let ((*package* (find-package :evo.user))
          ;; Everything this file registers belongs to this owner, so a later
          ;; generation can withdraw exactly what this load installed.
          (*extension-owner* (%make-extension-owner
                              :path (namestring path)
                              :generation *extension-generation*)))
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
  "The .lisp files of DIRECTORY in load order — sorted by file name, which
is the whole ordering mechanism.  Hence the naming convention: every
extension file starts with a fixed-width three-digit rank, `NNN-name.lisp`,
so the order is visible in `ls` instead of hiding in the alphabet.
000-099 foundations others build on (providers, credentials, settings);
100-899 ordinary tools, commands, hooks, prompt text; 900-999 the last
word — a wrapper loaded last is the outermost one.  Files without a rank
still load, sorting wherever their name falls."
  (sort (directory (merge-pathnames "*.lisp" directory))
        #'string< :key #'file-namestring))

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

;;; The runtime catalog: everything a turn resolves against.  Reload builds a
;;; new generation and installs it in one step; if the build dies partway, the
;;; captured catalog goes back untouched, so a session never runs on a runtime
;;; that is half old and half new.

(defstruct (runtime-catalog (:constructor %make-runtime-catalog))
  generation models providers apis tools commands settings prompt-notes)

(defun capture-runtime-catalog ()
  "Copy every registry a turn reads.  Copies, not aliases: the point is to be
able to put this exact runtime back."
  (%make-runtime-catalog
   :generation *extension-generation*
   :models (copy-list evo.provider::*models*)
   :providers (mapcar (lambda (e) (cons (car e) (copy-list (cdr e))))
                      evo.provider::*providers*)
   :apis (copy-alist evo.provider::*apis*)
   :tools (let ((copy (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k copy) v)) *tool-registry*)
            copy)
   :commands (let ((copy (make-hash-table :test #'equal)))
               (maphash (lambda (k v) (setf (gethash k copy) v)) evo::*commands*)
               copy)
   :settings (evo.util:capture-settings)
   :prompt-notes (copy-alist *prompt-notes*)))

(defun install-runtime-catalog (catalog)
  "Make CATALOG the live runtime in one step."
  (setf evo.provider::*models* (runtime-catalog-models catalog)
        evo.provider::*providers* (runtime-catalog-providers catalog)
        evo.provider::*apis* (runtime-catalog-apis catalog)
        *prompt-notes* (runtime-catalog-prompt-notes catalog))
  (clrhash *tool-registry*)
  (maphash (lambda (k v) (setf (gethash k *tool-registry*) v))
           (runtime-catalog-tools catalog))
  (clrhash evo::*commands*)
  (maphash (lambda (k v) (setf (gethash k evo::*commands*) v))
           (runtime-catalog-commands catalog))
  (evo.util:restore-settings (runtime-catalog-settings catalog))
  (incf *registry-generation*)
  catalog)

(defun boot-userspace (&key journal (cwd (uiop:getcwd)))
  "Build a new userspace generation and install it.

Order: settings and user registries reset, init files (global then project — an
override is just a later call), extension directories, then post-init files, so
extensions can register models before post-init picks a default.  Init files are
environment, not history: re-evaluated every boot, never journaled.

The registry build is all-or-nothing: a failure anywhere restores the previous
catalog rather than leaving a half-built runtime.

DISPOSAL HAPPENS FIRST, before a line of the new generation is loaded.  That
ordering is not a detail: a reloaded file reuses the same package and the same
globals, so an old generation's stop function writes the very variable the new
generation's task reads.  Disposing afterwards therefore lets the outgoing
generation reach into the incoming one — in practice, the old poller's `stop`
silently killing the new poller.  Generations must not overlap.

The cost is that a build which then fails leaves the previous generation's hooks
and tasks withdrawn (its registries are restored), and the failed build's own
registrations swept the same way.  That is the right way round: a
stale-but-consistent runtime with no extensions beats two generations of the
same extension running at once, and fixing the file and reloading again is the
normal repair."
  (let ((previous (capture-runtime-catalog))
        (previous-generation *extension-generation*)
        (installed nil))
    (unwind-protect
         (progn
           (incf *extension-generation*)
           ;; Retire the outgoing generation before the incoming one exists.
           (dispose-extension-owners :before *extension-generation*)
           (evo.util:reset-settings)
           (evo.provider:reset-user-registries)
           (restore-extension-registries)
           (load-init-file (merge-pathnames "init.lisp" (evo-home)))
           (load-init-file (merge-pathnames "init.lisp" (project-evo-dir cwd)))
           (snapshot-extension-registries)
           (boot-extensions :journal journal :cwd cwd)
           (load-init-file (merge-pathnames "post-init.lisp" (evo-home)))
           (load-init-file (merge-pathnames "post-init.lisp" (project-evo-dir cwd)))
           (setf installed t)
           (incf *registry-generation*)
           t)
      (unless installed
        ;; The failed build may itself have registered hooks and started
        ;; tasks.  Sweep them BEFORE rolling the counter back: they carry the
        ;; aborted generation's number, which after the rollback would read as
        ;; newer-than-live and dodge every later disposal — each retried
        ;; reload would then stack another copy of whatever loaded before the
        ;; failure.
        (dispose-extension-owners :before (1+ *extension-generation*))
        (setf *extension-generation* previous-generation)
        (install-runtime-catalog previous)))))

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
    :evo.cli :evo :evo.todo :evo.memory :evo.eval :evo.tui))

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

(defun on (event fn &key name)
  "Subscribe FN to a kernel event: :session-start :session-end :turn-end
:tool-call ...
A :tool-call hook may return (:block t :reason ...) or (:arguments ...).

Pass NAME from any file a reload can re-run: a named hook REPLACES the previous
registration of that name, while an anonymous one appends, so an unnamed hook in
a reloadable extension fires once more after every reload.  Either way the hook
belongs to the loading extension and is withdrawn when its generation is
disposed."
  (evo.kernel:add-hook event fn :name name))

(defun on-unload (thunk)
  "Run THUNK when the calling extension's generation is disposed (a /reload, or
the extension being removed).  Use it to undo anything the kernel cannot see:
a function patch, a cache, an external resource."
  (evo.kernel:register-extension-disposer thunk))

(defun spawn-task (&key name run stop)
  "Start RUN in a background thread owned by the calling extension.

The thread is tracked: on reload STOP is called and the thread is JOINED before
the new generation loads, so a poller cannot outlive the extension that started
it (the failure mode being two generations of the same poller running at once).
RUN must return promptly once STOP has been called — a thread still alive after
EVO.KERNEL:*TASK-STOP-SECONDS* is abandoned with a warning naming the task,
because the alternative is a reload that hangs forever."
  (let ((thread (bt:make-thread run :name (or (and name (string name)) "evo-extension-task"))))
    (evo.kernel:register-extension-task :name name :thread thread :stop stop)
    thread))

(defun register-prompt-note (name text)
  "Append TEXT (a self-contained markdown snippet) to every system prompt,
under the name NAME.  Re-registering NAME replaces its text — idempotent
across extension reloads — and NIL TEXT removes it.  For extensions that
change what the agent should DO: e.g. the LaTeX-math renderer asks the
agent to write formulas as LaTeX because they now render as images.

TEXT may instead be a function of the active language pack (a plist; its
:code is the language) returning the snippet, so a note can follow /lang
the way the prompt's own sections do — see extensions/400-efficiency.lisp."
  (evo.kernel:register-prompt-note name text))

(defun register-prompt-language (code &rest args)
  "Register a prompt language pack — the system prompt's own words, and the
language the model answers in:
 (evo:register-prompt-language \"zh-CN\" :name \"Chinese (Simplified)\"
   :native \"简体中文\" :response-language \"简体中文\"
   :sections (list :base \"...\" :guidelines \"...\"))
Sections left out fall back to English, so a pack may translate as much or
as little as it likes.  See extensions/100-lang-zh-cn.lisp for a full one.
Re-registration replaces the pack, so reloading is idempotent."
  (apply #'evo.kernel:register-prompt-language code args))

(defun set-language (code &optional agent)
  "Choose the prompt language: a registered pack code (\"en\", \"zh-CN\")
switches the system prompt into that language and asks for replies in it;
any other string (\"Korean\") is taken as a response-language hint on the
default pack.  Called from init.lisp it is the session default; called with
an AGENT at runtime the choice is journaled and survives a restart.  It
governs what the MODEL reads and writes — evo's own interface stays as it
is."
  (evo.kernel:set-prompt-language code agent))

(defun load-extension (path &key (reason "requested"))
  "Compile and load PATH into userspace (package EVO.USER), journaling the
load so a resumed session replays it.  This is the durable half of
self-extension — code evaluated into the image dies with the process, a
loaded FILE comes back — and it is what the `eval` tool calls to install a
new tool, command, or hook mid-session."
  (evo.kernel:load-extension*
   path :reason reason
   :journal (and *agent* (evo.kernel:agent-journal *agent*))))

;;; Config (init.lisp) API: models, providers, settings.

(defun register-model (id &rest args)
  "Register a model in init.lisp:
 (evo:register-model \"claude-opus-5\" :provider :anthropic
   :context-window 1000000 :max-output 128000
   :effort t :thinking-mode :adaptive)
A model's identity is its (id, provider) pair: register the same id under a
different provider and both are selectable (e.g. direct vs. proxy).
Re-registering the same pair replaces it in place; evo ships no built-in
models.  :api defaults to :anthropic-messages, the one bundled wire API —
every supported endpoint speaks it.

The knobs are context window, max output, effort ladder, thinking mode and
vision; a model that cannot think at all is not worth driving and has no
switch.

:effort declares the levels the model accepts for Anthropic's
output_config.effort — t for all of (:low :medium :high :xhigh :max), a
subset list for a model that stops short, nil (the default) for one
without the parameter.  A thinking level above what the model supports is
clamped down rather than rejected.  Worth measuring per endpoint rather
than assuming: an endpoint may accept the parameter and quietly ignore it,
and then the /thinking dial is wired to nothing.

:thinking-mode is :effort-only (the default — no `thinking` object on the
wire, the smallest request every Messages-compatible endpoint accepts) or
:adaptive, which also sends thinking {type adaptive, display summarized} —
what Anthropic's own models (Sonnet 5, Opus 5, Fable 5) want, and what
makes their reasoning summaries visible.

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
