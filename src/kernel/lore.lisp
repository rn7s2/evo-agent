;;;; lore.lisp — the lore system.
;;;;
;;;; Human knowledge, guidance, and constraints, durable across the whole
;;;; session and immune to summarization: lore is injected into the
;;;; system-prompt region EVERY turn — never entrusted to the compactor.
;;;; Scopes: global ~/.evo/lore.sexp, project .evo/lore.sexp (sexpr file,
;;;; one (:id ... :text ... :timestamp ...) form per line), plus
;;;; session-scoped entries riding the journal as :custom entries under key
;;;; "lore".
;;;;
;;;; Lore is stricter than memory: the agent must never curate it on its own
;;;; initiative.  The `lore` tool lets the agent edit or remove entries, but
;;;; only when the user has explicitly asked to change their lore.

(in-package :evo.kernel)

(defun lore-file (scope &optional (cwd (uiop:getcwd)))
  (ecase scope
    (:global (merge-pathnames "lore.sexp" (evo-home)))
    (:project (merge-pathnames "lore.sexp" (project-evo-dir cwd)))))

(defun lore-entry-id (entry)
  "The id of ENTRY, synthesizing a stable one for legacy id-less entries so
they can still be referenced (and get a persisted id on the next rewrite)."
  (or (pget entry :id)
      (format nil "lore-~(~8,'0x~)"
              (logand (sxhash (pget entry :text)) #xffffffff))))

(defun normalize-lore-entry (entry)
  "Coerce a stored entry (plist, or bare legacy string) into a full plist."
  (let ((entry (if (stringp entry) (list :text entry) entry)))
    (list :id (lore-entry-id entry)
          :text (pget entry :text)
          :timestamp (pget entry :timestamp))))

;;; File-scoped lore (:global, :project) ---------------------------------

(defun read-lore-file (path)
  "Full lore entries stored in PATH, each normalized to a plist with an :id."
  (when (probe-file path)
    (with-open-file (in path :direction :input :external-format :utf-8)
      (loop for form = (read-sexpr-stream in)
            until (eq form :eof)
            when (pget form :text)
              collect (normalize-lore-entry form)))))

(defun write-lore-file (entries path)
  "Rewrite PATH atomically with ENTRIES (one form per line)."
  (let ((temporary (merge-pathnames
                    (format nil "lore.~a.tmp" (gen-id))
                    (uiop:pathname-directory-pathname path))))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (out temporary :direction :output
                                          :external-format :utf-8
                                          :if-exists :error
                                          :if-does-not-exist :create)
             (dolist (entry entries)
               (write-sexpr-line
                (list :id (lore-entry-id entry)
                      :text (pget entry :text)
                      :timestamp (or (pget entry :timestamp) (iso8601-now)))
                out)))
           (uiop:rename-file-overwriting-target temporary path))
      (when (probe-file temporary) (delete-file temporary))))
  entries)

;;; Session-scoped lore (rides the journal) ------------------------------

(defun read-session-lore (state)
  "Full session lore entries from STATE, normalized to plists."
  (map 'list #'normalize-lore-entry (or (custom-state state "lore") #())))

(defun write-session-lore (agent entries)
  "Persist ENTRIES as this session's lore (a :custom journal entry)."
  (append-entry (agent-journal agent)
                (list :type :custom :key "lore"
                      :data (map 'vector
                                 (lambda (e)
                                   (list :id (lore-entry-id e)
                                         :text (pget e :text)
                                         :timestamp (or (pget e :timestamp)
                                                        (iso8601-now))))
                                 entries)))
  entries)

;;; Add ------------------------------------------------------------------

(defun add-lore (text &key (scope :project) (cwd (uiop:getcwd)))
  "Append TEXT to the file-scoped lore store for SCOPE (:global or :project).
Returns the new entry's id."
  (let* ((path (lore-file scope cwd))
         (entry (list :id (format nil "lore-~a" (gen-id))
                      :text text :timestamp (iso8601-now))))
    (write-lore-file (append (read-lore-file path) (list entry)) path)
    (pget entry :id)))

(defun add-session-lore (agent text)
  "Session-scoped lore: rides the journal as :custom state.  Returns the id."
  (let* ((state (fold-state (agent-journal agent)))
         (entry (list :id (format nil "lore-~a" (gen-id))
                      :text text :timestamp (iso8601-now))))
    (write-session-lore agent (append (read-session-lore state) (list entry)))
    (pget entry :id)))

;;; Query ----------------------------------------------------------------

(defun all-lore-entries (&key state (cwd (uiop:getcwd)))
  "Every lore entry, in scope order (global, project, session), each a plist
with :id :text :timestamp :scope."
  (append
   (mapcar (lambda (e) (append e (list :scope :global)))
           (read-lore-file (lore-file :global cwd)))
   (mapcar (lambda (e) (append e (list :scope :project)))
           (read-lore-file (lore-file :project cwd)))
   (when state
     (mapcar (lambda (e) (append e (list :scope :session)))
             (read-session-lore state)))))

(defun all-lore (&key state (cwd (uiop:getcwd)))
  "Every lore entry's text, in scope order.  Retained for callers that only
want the guidance strings; use ALL-LORE-ENTRIES to keep the ids."
  (mapcar (lambda (e) (pget e :text))
          (all-lore-entries :state state :cwd cwd)))

;;; Edit / remove --------------------------------------------------------

(defun find-lore-scope (id &key state (cwd (uiop:getcwd)))
  "The scope (:global :project :session) holding lore ID, or NIL."
  (flet ((has (entries) (find id entries :key #'lore-entry-id :test #'equal)))
    (cond ((has (read-lore-file (lore-file :global cwd))) :global)
          ((has (read-lore-file (lore-file :project cwd))) :project)
          ((and state (has (read-session-lore state))) :session)
          (t nil))))

(defun edit-lore (id new-text &key (agent evo:*agent*) (cwd (uiop:getcwd)))
  "Replace the text of lore ID wherever it lives.  Returns the updated entry."
  (let* ((state (and agent (fold-state (agent-journal agent))))
         (scope (find-lore-scope id :state state :cwd cwd)))
    (unless scope (error "No lore has id ~a" id))
    (flet ((update (entries)
             (mapcar (lambda (e)
                       (if (equal id (lore-entry-id e))
                           (list :id id :text new-text
                                 :timestamp (iso8601-now))
                           e))
                     entries)))
      (ecase scope
        (:session (write-session-lore agent (update (read-session-lore state))))
        ((:global :project)
         (let ((path (lore-file scope cwd)))
           (write-lore-file (update (read-lore-file path)) path)))))
    (list :id id :text new-text :scope scope)))

(defun remove-lore (id &key (agent evo:*agent*) (cwd (uiop:getcwd)))
  "Delete lore ID wherever it lives.  Returns the removed entry's text."
  (let* ((state (and agent (fold-state (agent-journal agent))))
         (scope (find-lore-scope id :state state :cwd cwd)))
    (unless scope (error "No lore has id ~a" id))
    (flet ((drop (entries)
             (values (remove id entries :key #'lore-entry-id :test #'equal)
                     (find id entries :key #'lore-entry-id :test #'equal))))
      (ecase scope
        (:session
         (multiple-value-bind (kept removed) (drop (read-session-lore state))
           (write-session-lore agent kept)
           (list :id id :text (pget removed :text) :scope scope)))
        ((:global :project)
         (let ((path (lore-file scope cwd)))
           (multiple-value-bind (kept removed) (drop (read-lore-file path))
             (write-lore-file kept path)
             (list :id id :text (pget removed :text) :scope scope))))))))

;;; The `lore` tool ------------------------------------------------------

(defun lore-scope-keyword (value &optional (default :project))
  (cond ((null value) default)
        ((and (stringp value) (string-equal value "project")) :project)
        ((and (stringp value) (string-equal value "global")) :global)
        ((and (stringp value) (string-equal value "session")) :session)
        (t (error "Unknown lore scope ~s (use project, global, or session)"
                  value))))

(defun format-lore-listing (entries)
  (if (null entries)
      "No lore."
      (with-output-to-string (out)
        (dolist (entry entries)
          (format out "- [~a] (~(~a~)) ~a~%"
                  (pget entry :id) (pget entry :scope) (pget entry :text))))))

(defun tool-lore (args)
  (let* ((agent evo:*agent*)
         (state (and agent (fold-state (agent-journal agent))))
         (action (pget args :action)))
    (flet ((arg (key what)
             (let ((v (pget args key)))
               (unless (and (stringp v) (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) v))))
                 (error "lore ~a requires ~a" action what))
               v)))
      (cond
        ((and (stringp action) (string-equal action "query"))
         (format-lore-listing (all-lore-entries :state state)))
        ((and (stringp action) (string-equal action "add"))
         (let* ((scope (lore-scope-keyword (pget args :scope)))
                (text (arg :text "text"))
                (id (if (eq scope :session)
                        (add-session-lore agent text)
                        (add-lore text :scope scope))))
           (format nil "Added ~(~a~) lore [~a]: ~a" scope id text)))
        ((and (stringp action) (string-equal action "edit"))
         (let ((entry (edit-lore (arg :id "id") (arg :text "text"))))
           (format nil "Edited ~(~a~) lore [~a]: ~a"
                   (pget entry :scope) (pget entry :id) (pget entry :text))))
        ((and (stringp action) (string-equal action "remove"))
         (let ((entry (remove-lore (arg :id "id"))))
           (format nil "Removed ~(~a~) lore [~a]: ~a"
                   (pget entry :scope) (pget entry :id) (pget entry :text))))
        (t (error "Unknown lore action ~s (use query, add, edit, or remove)"
                  action))))))

(register-tool*
 :name "lore"
 :description "Query, add, edit, or remove lore — durable user guidance injected into the system prompt every turn, outranking ordinary context. Lore is STRICTER than memory: never add, edit, or remove lore on your own initiative or inference. Only change lore when the user has EXPLICITLY asked you to (e.g. 'edit that lore', 'remove the lore about X', 'add a lore that...'). Reference entries by their [id]. Scope is project (default), global (every project), or session (this session only)."
 :schema '(:object
           (:action :type :string :enum ("query" "add" "edit" "remove")
            :description "Operation to perform")
           (:id :type :string :optional t
            :description "Lore id, required for edit and remove")
           (:text :type :string :optional t
            :description "Lore text, required for add and edit")
           (:scope :type :string :optional t
            :enum ("project" "global" "session")
            :description "Scope for add: project (default), global, or session"))
 :execute #'tool-lore)
