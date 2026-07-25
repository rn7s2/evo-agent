;;;; lore.lisp — the lore system (§9, M3).
;;;;
;;;; Human knowledge, guidance, and constraints, durable across the whole
;;;; session and immune to summarization: lore is injected into the
;;;; system-prompt region EVERY turn — never entrusted to the compactor.
;;;; Scopes: global ~/.evo/lore.sexp, project .evo/lore.sexp (sexpr file,
;;;; one (:text ... :timestamp ...) form per line), plus session-scoped
;;;; entries riding the journal as :custom entries under key "lore".

(in-package :evo.kernel)

(defun lore-file (scope &optional (cwd (uiop:getcwd)))
  (ecase scope
    (:global (merge-pathnames "lore.sexp" (evo-home)))
    (:project (merge-pathnames "lore.sexp" (project-evo-dir cwd)))))

(defun read-lore-file (path)
  (when (probe-file path)
    (with-open-file (in path :direction :input :external-format :utf-8)
      (loop for form = (read-sexpr-stream in)
            until (eq form :eof)
            when (pget form :text)
              collect (pget form :text)))))

(defun add-lore (text &key (scope :project) (cwd (uiop:getcwd)))
  "Append TEXT to the lore store for SCOPE (:global or :project)."
  (let ((path (lore-file scope cwd)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :external-format :utf-8
                              :if-exists :append :if-does-not-exist :create)
      (write-sexpr-line (list :text text :timestamp (iso8601-now)) out))
    text))

(defun add-session-lore (agent text)
  "Session-scoped lore: rides the journal as :custom state."
  (let* ((state (fold-state (agent-journal agent)))
         (existing (custom-state state "lore"))
         (updated (concatenate 'vector (or existing #()) (vector text))))
    (append-entry (agent-journal agent)
                  (list :type :custom :key "lore" :data updated))
    text))

(defun all-lore (&key state (cwd (uiop:getcwd)))
  "Every lore entry in scope order: global, project, then session (STATE)."
  (append (read-lore-file (lore-file :global cwd))
          (read-lore-file (lore-file :project cwd))
          (when state (coerce (or (custom-state state "lore") #()) 'list))))
