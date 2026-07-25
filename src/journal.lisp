;;;; journal.lisp — the append-only entry tree (§4).
;;;;
;;;; One file per session; line 1 is a header form, every other line one entry
;;;; form with :id/:parent-id/:timestamp.  The file is a tree: branching =
;;;; move the leaf pointer, next append becomes a sibling.  Entries are never
;;;; modified or deleted.  All session state is a fold over the root→leaf
;;;; path.  Write-ahead: entries are appended before being acted on — but
;;;; nothing hits disk until the first assistant message exists (no
;;;; abandoned-session litter).

(in-package :evo.journal)

(defstruct (journal (:constructor %make-journal))
  path            ; pathname of the session file
  header          ; header plist
  (entries (make-array 64 :adjustable t :fill-pointer 0))
  (index (make-hash-table :test #'equal))  ; id -> entry
  leaf-id         ; current leaf entry id (nil for empty journal)
  started-p       ; t once the file exists on disk
  (pending nil)   ; entries buffered before first flush (reverse order)
  ;; The run thread appends while a frontend folds state: one lock guards
  ;; entries/index/leaf.
  (lock (bt:make-lock "journal")))

(defun sessions-directory (&optional (cwd (uiop:getcwd)))
  (merge-pathnames (format nil "sessions/~a/" (encode-cwd cwd)) (evo-home)))

(defun session-file-timestamp ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
            year month day hour min sec)))

(defun make-session-journal (&optional (cwd (uiop:getcwd)))
  "Create a fresh (not yet on-disk) session journal for CWD."
  (let* ((id (gen-id 16))
         (file (format nil "~a_~a.sexp" (session-file-timestamp) id))
         (path (merge-pathnames file (sessions-directory cwd))))
    (%make-journal
     :path path
     :header (list :type :session :version 1 :id id
                   :cwd (namestring (uiop:ensure-directory-pathname cwd))
                   :timestamp (iso8601-now))
     :started-p nil)))

(defun journal-add (journal entry)
  (vector-push-extend entry (journal-entries journal))
  (setf (gethash (pget entry :id) (journal-index journal)) entry)
  entry)

(defun open-journal (path)
  "Reopen a session journal from disk.  Leaf = last appended entry."
  (with-open-file (in path :direction :input :external-format :utf-8)
    (let ((header (read-sexpr-stream in)))
      (when (or (eq header :eof) (not (eq (pget header :type) :session)))
        (error "Not a session file: ~a" path))
      (let ((journal (%make-journal :path (pathname path)
                                    :header header
                                    :started-p t)))
        (loop for entry = (read-sexpr-stream in)
              until (eq entry :eof)
              do (journal-add journal entry)
                 (setf (journal-leaf-id journal) (pget entry :id)))
        journal))))

(defun flush-pending (journal)
  "Write header + buffered entries to disk; subsequent appends go straight through."
  (ensure-directories-exist (journal-path journal))
  (with-open-file (out (journal-path journal)
                       :direction :output :external-format :utf-8
                       :if-exists :error :if-does-not-exist :create)
    (write-sexpr-line (journal-header journal) out)
    (dolist (entry (nreverse (journal-pending journal)))
      (write-sexpr-line entry out)))
  (setf (journal-pending journal) nil
        (journal-started-p journal) t))

(defun write-entry (journal entry)
  (with-open-file (out (journal-path journal)
                       :direction :output :external-format :utf-8
                       :if-exists :append :if-does-not-exist :error)
    (write-sexpr-line entry out)))

(defun assistant-message-entry-p (entry)
  (and (eq (pget entry :type) :message)
       (eq (pget (pget entry :message) :role) :assistant)))

(defun append-entry (journal plist &key parent-id)
  "Append PLIST as a new entry at the leaf (or under PARENT-ID: branching).
Assigns :id/:parent-id/:timestamp.  Returns the completed entry."
  (bt:with-lock-held ((journal-lock journal))
    (let* ((id (loop for candidate = (gen-id)
                     unless (gethash candidate (journal-index journal))
                       return candidate))
           (entry (append (list :type (pget plist :type)
                                :id id
                                :parent-id (or parent-id (journal-leaf-id journal))
                                :timestamp (iso8601-now))
                          (loop for (k v) on plist by #'cddr
                                unless (member k '(:type :id :parent-id :timestamp))
                                  append (list k v)))))
      (validate-journal-value entry) ; fail loudly now, not at deferred flush
      (journal-add journal entry)
      (setf (journal-leaf-id journal) (pget entry :id))
      (cond ((journal-started-p journal)
             (write-entry journal entry))
            (t
             (push entry (journal-pending journal))
             ;; Nothing is written until the first assistant message exists.
             (when (assistant-message-entry-p entry)
               (flush-pending journal))))
      entry)))

(defun find-entry (journal id)
  (bt:with-lock-held ((journal-lock journal))
    (gethash id (journal-index journal))))

(defun %entry-path (journal leaf-id)
  (let ((path nil)
        (seen (make-hash-table :test #'equal)))
    (loop for id = leaf-id then (pget entry :parent-id)
          while id
          for entry = (or (gethash id (journal-index journal))
                          (error "Broken parent chain: no entry ~s" id))
          do (when (gethash id seen)
               (error "Cycle in journal parent chain at ~s — corrupt journal ~a"
                      id (journal-path journal)))
             (setf (gethash id seen) t)
             (push entry path))
    path))

(defun entry-path (journal &optional (leaf-id (journal-leaf-id journal)))
  "Root→leaf list of entries."
  (bt:with-lock-held ((journal-lock journal))
    (%entry-path journal leaf-id)))

;;; State fold (§4.1): context, model, thinking, tools, goal — everything is a
;;; fold over the root→leaf path.  No mutable fields.

(defstruct state
  (messages nil)   ; list of message plists, chronological
  model
  thinking
  tools            ; list of active tool name strings, nil = default set
  goal             ; current goal plist or nil
  (loads nil)      ; list of :load entry plists, chronological
  (custom nil)     ; alist key-string -> data (last :custom entry wins)
  name)

(defun custom-state (state key)
  "Extension state from :custom entries (invisible to the LLM)."
  (cdr (assoc key (state-custom state) :test #'equal)))

(defun compaction-entry->messages (entry)
  "Messages a :compaction checkpoint contributes: the summary as an ordinary
user message in <summary> tags, then the retained tail."
  (cons (list :role :user
              :content (list (list :type :text
                                   :text (format nil "<summary>~%This session was compacted; the conversation so far is summarized below. Continue seamlessly from it.~2%~a~@[~2%Files read so far: ~{~a~^, ~}~]~@[~%Files modified so far: ~{~a~^, ~}~]~%</summary>"
                                                 (pget entry :summary)
                                                 (coerce (or (pget entry :files-read) #()) 'list)
                                                 (coerce (or (pget entry :files-modified) #()) 'list)))))
        (coerce (or (pget entry :retained-tail) #()) 'list)))

(defun fold-state (journal &optional (leaf-id (journal-leaf-id journal)))
  (let ((state (make-state))
        (path (bt:with-lock-held ((journal-lock journal))
                (and leaf-id (%entry-path journal leaf-id)))))
    (dolist (entry path)
      (let ((type (pget entry :type)))
        (case type
          (:message
           (push (pget entry :message) (state-messages state)))
          (:custom-message
           ;; Extension-injected content, visible to the LLM.
           (push (pget entry :message) (state-messages state)))
          (:custom                      ; state for extensions; invisible to LLM
           (let ((key (pget entry :key)))
             (when key
               (setf (state-custom state)
                     (cons (cons key (pget entry :data))
                           (remove key (state-custom state)
                                   :key #'car :test #'equal))))))
          (:model-change
           (setf (state-model state) (pget entry :model)))
          (:thinking-change
           (setf (state-thinking state) (pget entry :thinking)))
          (:tools-change
           (setf (state-tools state) (coerce (pget entry :tools) 'list)))
          (:goal
           (setf (state-goal state)
                 (loop for (k v) on entry by #'cddr
                       unless (member k '(:type :id :parent-id :timestamp))
                         append (list k v))))
          (:load
           (setf (state-loads state)
                 (append (state-loads state) (list entry))))
          (:session-info
           (when (pget entry :name)
             (setf (state-name state) (pget entry :name))))
          (:compaction
           ;; Self-contained checkpoint (§7): context rebuild restarts here as
           ;; [summary, ...retained-tail]; no walk past the compaction.
           (setf (state-messages state)
                 (reverse (compaction-entry->messages entry))))
          (:branch-summary
           (when (pget entry :summary)
             (push (list :role :user
                         :content (list (list :type :text
                                              :text (format nil "<abandoned-branch-summary>~%~a~%</abandoned-branch-summary>"
                                                            (pget entry :summary)))))
                   (state-messages state))))
          (:label)
          (t nil))))
    (setf (state-messages state) (nreverse (state-messages state)))
    state))

;;; Fork (§4.4): copy the root→entry path into a new session file.

(defun fork-session (journal &optional (leaf-id (journal-leaf-id journal)))
  "Write the root→LEAF-ID path as a fresh session file.  Returns its path.
Entry ids are preserved (the parent chain must stay intact)."
  (let* ((path (entry-path journal leaf-id))
         (id (gen-id 16))
         (header (let ((h (copy-list (journal-header journal))))
                   (setf (getf h :id) id
                         (getf h :timestamp) (iso8601-now))
                   h))
         (file (merge-pathnames
                (format nil "~a_~a.sexp" (session-file-timestamp) id)
                (make-pathname :name nil :type nil
                               :defaults (journal-path journal)))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :external-format :utf-8
                              :if-exists :error :if-does-not-exist :create)
      (write-sexpr-line header out)
      (dolist (entry path)
        (write-sexpr-line entry out)))
    file))

;;; Session listing (§4.4) — bounded header scan.

(defun list-sessions (&optional (cwd (uiop:getcwd)))
  "List sessions for CWD, newest first: plists of :path + header fields."
  (let ((files (sort (directory (merge-pathnames "*.sexp" (sessions-directory cwd)))
                     #'string> :key #'namestring)))
    (loop for path in files
          for header = (ignore-errors
                        (with-open-file (in path :direction :input :external-format :utf-8)
                          (read-sexpr-stream in)))
          when (and (consp header) (eq (pget header :type) :session))
            collect (list* :path (namestring path) header))))

(defun latest-session (&optional (cwd (uiop:getcwd)))
  (pget (first (list-sessions cwd)) :path))
