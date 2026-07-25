;;;; extension.lisp — self-extension (§10) and the public API (D13).
;;;;
;;;; The EVO package is the whole public surface: register-tool,
;;;; register-command, event hooks, load-extension.  Core and user extensions
;;;; build on it; nothing bypasses it.  Userspace code lives in EVO.USER and
;;;; is rebuilt from source on every boot (D2): boot = load kernel, then
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
  "Load global then project extension directories (§10.3), journaling each."
  (dolist (dir (list (merge-pathnames "extensions/" (evo-home))
                     (merge-pathnames "extensions/" (project-evo-dir cwd))))
    (dolist (file (extension-files dir))
      (handler-case
          (load-extension* file :reason "boot" :journal journal)
        (error (e)
          (warn "Skipping extension ~a: ~a" file e))))))

(defun replay-loads (state &key journal)
  "Replay a resumed session's :load entries (§4.2) against the files on disk.
Files already loaded this boot are skipped; missing files are reported, not
fatal — a corrupted runtime is repaired by fixing/removing a source file."
  (declare (ignore journal))
  (dolist (entry (evo.journal:state-loads state))
    (let ((path (pget entry :path)))
      (unless (member path *loaded-extension-paths* :test #'equal)
        (handler-case
            (if (probe-file path)
                (load-extension* path :reason "replay" :record nil)
                (warn "Journaled :load file missing, skipping: ~a" path))
          (error (e)
            (warn "Replaying :load of ~a failed: ~a" path e)))))))

(defparameter *kernel-packages*
  '(:evo.util :evo.journal :evo.provider :evo.kernel :evo.cli :evo))

(defun lock-kernel-packages ()
  "Package locks (D8): permissive but not suicidal — touching the kernel
requires an explicit, auditable sb-ext:unlock-package."
  (dolist (name *kernel-packages*)
    (let ((pkg (find-package name)))
      (when pkg (sb-ext:lock-package pkg)))))

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
  "Slash commands (§12).  The MVP has no TUI; commands registered here are
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

(defun set-active-tools (agent names)
  "Journal a :tools-change entry; takes effect at the next save point."
  (evo.journal:append-entry (evo.kernel:agent-journal agent)
                            (list :type :tools-change
                                  :tools (coerce names 'vector))))

(defun current-goal (&optional (agent *agent*))
  (evo.kernel:current-goal agent))
