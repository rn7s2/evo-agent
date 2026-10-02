;;;; journal.lisp — the append-only entry tree.
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
  (listeners nil) ; FUNCTIONS notified after each append, in registration order
  ;; The run thread appends while a frontend folds state: one lock guards
  ;; entries/index/leaf.
  (lock (bt:make-lock "journal"))
  ;; Folded state memoised per leaf id (design G1): reads between two appends
  ;; are free, and an append extends the parent's state by one entry instead of
  ;; re-walking the path.  Its own lock, never held with the one above.
  (fold-cache (make-hash-table :test #'equal)) ; leaf id -> state
  (fold-order nil)                             ; leaf ids, oldest first
  (fold-lock (bt:make-lock "journal-fold")))

(defun sessions-directory (&optional (cwd (uiop:getcwd)))
  "Where CWD's sessions live: ~/.evo/sessions/<encoded cwd>/, unless
EVO_SESSIONS_DIR names a directory for this process's sessions outright.  An
evo-swarm lane runs with one, so its journals stay out of the coordinator's
/resume list and a restarted lane's --resume finds its own session."
  (let ((override (getenv "EVO_SESSIONS_DIR")))
    (if (plusp (length override))
        (uiop:ensure-directory-pathname override)
        (merge-pathnames (format nil "sessions/~a/" (encode-cwd cwd)) (evo-home)))))

(defun session-file-timestamp ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
            year month day hour min sec)))

(defun make-session-journal (&optional (cwd (uiop:getcwd)) &key (program "evo-agent") swarm-id)
  "Create a fresh (not yet on-disk) session journal for CWD.  PROGRAM names the
frontend the session belongs to (\"evo-agent\", \"evo-swarm\", or \"lane\" for a
swarm lane); SWARM-ID is recorded for a coordinator, so a session list can say
which swarm a session drove without opening the file."
  (let* ((id (gen-id 16))
         (file (format nil "~a_~a.sexp" (session-file-timestamp) id))
         (path (merge-pathnames file (sessions-directory cwd))))
    (%make-journal
     :path path
     :header (list :type :session :version 1 :id id
                   :cwd (namestring (uiop:ensure-directory-pathname cwd))
                   :program program
                   :swarm-id swarm-id
                   :timestamp (iso8601-now))
     :started-p nil)))

(defun set-session-header (journal &rest fields)
  "Update the session header's FIELDS (a plist).  Returns the new header, and
writes the session index again — a session's program and swarm id are exactly
what the index is for.  Two fields are written after session creation: a
coordinator records its :swarm-id, and a session created before :program
existed is stamped with the frontend that opened it."
  (let ((header (copy-list (journal-header journal))))
    (loop for (k v) on fields by #'cddr
          do (setf (getf header k) v))
    (setf (journal-header journal) header)
    (when (journal-started-p journal)
      (rewrite-journal-header journal))
    (index-session journal)
    header))

(defun rewrite-journal-header (journal)
  "Rewrite line 1 of the on-disk session file with the journal's current
header.  The header is the one line that is not an append, so it is rewritten
whole: a temporary file beside it, then a rename.

Done under the journal lock, and form-by-form rather than line-by-line, because
an entry may contain a string with newlines — copying lines would truncate it —
and because an append landing between the copy and the rename would be lost."
  (let* ((path (journal-path journal))
         (temporary (merge-pathnames
                     (format nil "~a.header-~a" (file-namestring path) (gen-id))
                     (uiop:pathname-directory-pathname path))))
    (bt:with-lock-held ((journal-lock journal))
      (unwind-protect
           (progn
             (with-open-file (out temporary :direction :output
                                            :external-format :utf-8
                                            :if-exists :error
                                            :if-does-not-exist :create)
               (write-sexpr-line (journal-header journal) out)
               (with-open-file (in path :direction :input :external-format :utf-8)
                 (read-sexpr-stream in)         ; the old header
                 (loop for entry = (read-sexpr-stream in)
                       until (eq entry :eof)
                       do (write-sexpr-line entry out))))
             (uiop:rename-file-overwriting-target temporary path))
        (when (probe-file temporary) (ignore-errors (delete-file temporary)))))))

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

(defun session-id-from-path (path)
  "The session id a journal file name carries (…_<id>.sexp), or NIL."
  (let* ((name (file-namestring path))
         (dot (position #\. name :from-end t))
         (underscore (and dot (position #\_ name :from-end t :end dot))))
    (when (and dot underscore (< (1+ underscore) dot))
      (subseq name (1+ underscore) dot))))

(defun reopen-session (path &key (program "evo-agent"))
  "The session at PATH: the journal on disk when there is one, otherwise an
empty journal whose path is exactly PATH — a session nothing has been
journalled to yet.

A supervisor restart names the session its child was on before it died; an
idle server has answered nothing, so its journal was never written.  Coming
back to it must not move the client to another journal, so the path (and the
id its file name carries) is kept, and the file appears the moment something
is journalled (CONTRACT §1, F4)."
  (if (probe-file path)
      (open-journal path)
      (%make-journal :path (pathname path)
                     :header (list :type :session :version 1
                                   :id (or (session-id-from-path path) (gen-id 16))
                                   :cwd (namestring (uiop:ensure-directory-pathname
                                                     (uiop:getcwd)))
                                   :program program
                                   :timestamp (iso8601-now))
                     :started-p nil)))

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

(defun append-entry (journal plist &key parent-id id)
  "Append PLIST as a new entry at the leaf (or under PARENT-ID: branching).
Assigns :parent-id/:timestamp, and an :id unless the caller pre-minted one —
a frontend that needs to name an entry before it exists (a streaming message,
a queued input) mints it with GEN-ID and passes it here.  Returns the completed
entry.

Registered listeners are notified after the entry is part of the journal, with
the journal lock RELEASED — a listener that folds state (the view model does)
must be able to read the journal it was told about."
  (let ((entry nil)
        (flushed nil))
    (bt:with-lock-held ((journal-lock journal))
      (let* ((id (or id (loop for candidate = (gen-id)
                              unless (gethash candidate (journal-index journal))
                                return candidate)))
             (new (append (list :type (pget plist :type)
                                :id id
                                :parent-id (or parent-id (journal-leaf-id journal))
                                :timestamp (iso8601-now))
                          (loop for (k v) on plist by #'cddr
                                unless (member k '(:type :id :parent-id :timestamp))
                                  append (list k v)))))
        (setf entry new)
        (validate-journal-value entry) ; fail loudly now, not at deferred flush
        (journal-add journal entry)
        (setf (journal-leaf-id journal) (pget entry :id))
        (cond ((journal-started-p journal)
               (write-entry journal entry))
              (t
               (push entry (journal-pending journal))
               ;; Nothing is written until the first assistant message exists.
               (when (assistant-message-entry-p entry)
                 (flush-pending journal)
                 (setf flushed t))))
        (extend-fold-cache journal entry)))
    ;; Outside the lock: a listener folds the journal it was told about, and
    ;; the session index reads the path back — both need the lock themselves.
    (note-journal-append journal entry)
    ;; The file exists now, so the session is one the index can name:
    ;; `sessions` lists it from here on, not only once it ends.
    (when flushed (index-session journal))
    entry))

(defun add-journal-listener (journal fn)
  "Call FN (journal entry) after every entry is appended to JOURNAL.  The view
model subscribes this way; nothing else watches a journal for appends."
  (bt:with-lock-held ((journal-lock journal))
    (setf (journal-listeners journal)
          (append (remove fn (journal-listeners journal) :test #'eq) (list fn))))
  fn)

(defun remove-journal-listener (journal fn)
  "Withdraw FN from JOURNAL's append notifications."
  (bt:with-lock-held ((journal-lock journal))
    (setf (journal-listeners journal)
          (remove fn (journal-listeners journal) :test #'eq)))
  fn)

(defun note-journal-append (journal entry)
  "Tell JOURNAL's listeners about ENTRY.  The list is copied under the lock and
the calls happen outside it: a listener folds the journal, which needs the
lock.  A listener that signals is named and dropped from this notification,
never from the journal."
  (dolist (fn (bt:with-lock-held ((journal-lock journal))
               (copy-list (journal-listeners journal))))
    (handler-case (funcall fn journal entry)
      (error (e) (warn "Journal append listener failed: ~a" e)))))

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

;;; State fold: context, model, thinking, tools, goal — everything is a
;;; fold over the root→leaf path.  No mutable fields.
;;;
;;; The messages are accumulated newest-first and materialised chronologically
;;; on the first read, so folding one entry onto a previous state is O(1) and
;;; two states never share a list that either of them mutates.  That is what
;;; makes the per-leaf cache (below) an extension rather than a rebuild.

(defstruct state
  (messages-rev nil)   ; newest-first; STATE-MESSAGES reverses a copy of it
  (messages-cache nil) ; the chronological list, once somebody asked
  model
  model-provider   ; provider keyword disambiguating MODEL, or nil
  thinking
  tools            ; list of active tool names, nil = default set
  goal             ; current goal plist or nil
  (loads nil)      ; list of :load entry plists, chronological
  ;; Prompt-cache accounting: the session's own totals over every assistant
  ;; message on the path.  Numbers, not a plist, because states share structure
  ;; with their parent in the fold cache — an incremented list would be mutated
  ;; under a cached ancestor.
  (cache-input 0)  ; sent input tokens; cached ones are OUT of this
  (cache-read 0)   ; … the provider read this many from its prompt cache
  (cache-write 0)  ; … and wrote this many into it
  (custom nil))    ; alist key-string -> data (last :custom entry wins)

(defun state-messages (state)
  "STATE's messages, chronologically.  Computed from the reverse-order
accumulator on first use, and cached: the fold touches the accumulator only."
  (or (state-messages-cache state)
      (setf (state-messages-cache state)
            (reverse (copy-list (state-messages-rev state))))))

(defun state-push-message (state message)
  (push message (state-messages-rev state))
  (setf (state-messages-cache state) nil))

(defun state-cache-stats (state)
  "STATE's prompt-cache accounting as the normalized usage shape — totals of
\(:input n :cache-read n :cache-write n) over every assistant message on the
path.  :input excludes the cached tokens (src/provider/api.lisp states the
contract), so the three sum to what the session actually sent.  A session that
has made no request reads all zeros, which the status line renders as \"0%
cached\" rather than as unknown."
  (list :input (state-cache-input state)
        :cache-read (state-cache-read state)
        :cache-write (state-cache-write state)))

(defun note-cache-usage (state message)
  "Fold MESSAGE's provider-reported usage into STATE's running totals.  Only
the provider reports usage, and only assistant messages carry it: an error or a
provider that reports none contributes nothing."
  (let ((usage (pget message :usage)))
    (when usage
      (incf (state-cache-input state) (or (pget usage :input) 0))
      (incf (state-cache-read state) (or (pget usage :cache-read) 0))
      (incf (state-cache-write state) (or (pget usage :cache-write) 0)))))

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
                                                 (coerce (or (pget entry :files-modified) #()) 'list))))
              :usage (list :input 0
                           :output (or (pget entry :summary-tokens) 0)
                           :cache-read 0 :cache-write 0))
        (coerce (or (pget entry :retained-tail) #()) 'list)))

(defun fold-entry (state entry)
  "Apply one journal ENTRY to STATE.  The single place entry types are
interpreted: FOLD-STATE walks a path through it, and APPEND-ENTRY extends a
cached state with exactly one entry, so the two can never disagree."
  (case (pget entry :type)
    (:message
     (state-push-message state (pget entry :message))
     ;; Cache totals are the session's, not the fold's: a compaction drops
     ;; messages from the context but does not un-send them, so the totals
     ;; deliberately survive the :compaction branch below.
     (when (eq (pget (pget entry :message) :role) :assistant)
       (note-cache-usage state (pget entry :message))))
    (:custom-message
     ;; Extension-injected content, visible to the LLM.  Tagged with
     ;; the entry's :key so a transform-context hook can filter it
     ;; back out when it stops being relevant.
     (state-push-message state
                         (if (pget entry :key)
                             (pput (pget entry :message) :meta (list :key (pget entry :key)))
                             (pget entry :message))))
    (:custom                      ; state for extensions; invisible to LLM
     (let ((key (pget entry :key)))
       (when key
         (setf (state-custom state)
               (cons (cons key (pget entry :data))
                     (remove key (state-custom state)
                             :key #'car :test #'equal))))))
    (:model-change
     (setf (state-model state) (pget entry :model)
           (state-model-provider state) (pget entry :provider)))
    (:thinking-change
     (setf (state-thinking state) (pget entry :thinking)))
    (:tools-change
     (setf (state-tools state) (coerce (pget entry :tools) 'list)))
    (:goal
     (setf (state-goal state)
           (loop for (k v) on entry by #'cddr
                 unless (member k '(:type :id :parent-id :timestamp))
                   append (list k v))))
    (:recover
     ;; The supervisor's account of how the previous run ended —
     ;; appended only by the child booting after a restart; the
     ;; entries themselves are the history.
     (setf (state-custom state)
           (cons (cons "recovery"
                       (loop for (k v) on entry by #'cddr
                             unless (member k '(:type :id :parent-id :timestamp))
                               append (list k v)))
                 (remove "recovery" (state-custom state)
                         :key #'car :test #'equal))))
    (:load
     (setf (state-loads state)
           (append (state-loads state) (list entry))))
    (:compaction
     ;; Self-contained checkpoint: context rebuild restarts here as
     ;; [summary, ...retained-tail]; no walk past the compaction.
     (setf (state-messages-rev state)
           (reverse (compaction-entry->messages entry))
           (state-messages-cache state) nil))
    (:branch-summary
     (when (pget entry :summary)
       (state-push-message
        state
        (list :role :user
              :content (list (list :type :text
                                   :text (format nil "<abandoned-branch-summary>~%~a~%</abandoned-branch-summary>"
                                                 (pget entry :summary))))))))
    ;; :label and :notice are journal records only — nothing folds out of them.
    (:label) (:notice)
    (t nil))
  state)

;;; The fold cache, keyed by leaf id.  Two callers want the same answer over and
;;; over — a frontend repainting its status line, and the context estimate —
;;; and neither may pay for a walk of the whole path each time (design G1).

(defparameter *fold-cache-size* 16
  "How many folded states to keep.  One leaf's chain of states shares its list
structure with its parent's, so keeping a few costs little and evicting one only
costs a re-fold, never correctness.")

(defun cached-fold (journal leaf-id)
  (bt:with-lock-held ((journal-fold-lock journal))
    (gethash leaf-id (journal-fold-cache journal))))

(defun cache-fold (journal leaf-id state)
  (bt:with-lock-held ((journal-fold-lock journal))
    (setf (gethash leaf-id (journal-fold-cache journal)) state)
    (pushnew leaf-id (journal-fold-order journal) :test #'equal)
    (loop while (> (length (journal-fold-order journal)) *fold-cache-size*)
          for evicted = (car (last (journal-fold-order journal)))
          do (remhash evicted (journal-fold-cache journal))
             (setf (journal-fold-order journal)
                   (remove evicted (journal-fold-order journal) :test #'equal)))
    state))

(defun extend-fold-cache (journal entry)
  "Cache the state at JOURNAL's new leaf: ENTRY applied to whatever the parent
leaf's state was, when that state is still known.  A leaf whose parent is not
cached is simply left to be folded on demand."
  (let* ((parent-id (pget entry :parent-id))
         (parent (or (and parent-id (cached-fold journal parent-id))
                     (and (null parent-id) (make-state)))))
    (when parent
      (cache-fold journal (pget entry :id) (fold-entry (copy-state parent) entry)))))

(defun empty-state ()
  "The state a session with nothing in it has: no messages, no model, no
thinking level, no tools, no goal.  The effective-* chain (EVO.KERNEL) reads
configuration off a state, so a caller that has to resolve what *configuration
alone* says — evo-swarm's offline check resolves a lane's settings, and a lane
has no session of yours — asks with this rather than with a NIL that is not a
state at all."
  (make-state))

(defun fold-state (journal &optional (leaf-id (journal-leaf-id journal)))
  "The session state at LEAF-ID: everything is a fold over the root→leaf path.
Memoised per leaf id (design G1)."
  (or (cached-fold journal leaf-id)
      (let ((state (make-state))
            (path (bt:with-lock-held ((journal-lock journal))
                    (and leaf-id (%entry-path journal leaf-id)))))
        (dolist (entry path) (fold-entry state entry))
        (cache-fold journal leaf-id state))))

;;; Fork: copy the root→entry path into a new session file.

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

;;; Session listing — bounded header scan.

(defun session-updated (path)
  "Last-write time of session file PATH as an ISO-8601 UTC string, or NIL.
The journal is append-only, so the file's mtime is the moment the session
was last worked in — which is not its creation time once you resume an old
session and keep going."
  (let ((written (ignore-errors (file-write-date path))))
    (and written (iso8601-utc written))))

(defun session-newer-p (a b)
  "Order two session plists: most recently updated first.  Both keys are
fixed-width strings, so this is a plain lexicographic compare; the path
breaks ties (mtime has one-second resolution, and two sessions can easily
share a second)."
  (let ((ua (or (pget a :updated) ""))
        (ub (or (pget b :updated) "")))
    (if (string= ua ub)
        (and (string> (or (pget a :path) "") (or (pget b :path) "")) t)
        (and (string> ua ub) t))))

(defun sort-sessions (sessions)
  "SESSIONS, most recently updated first."
  (sort (copy-list sessions) #'session-newer-p))

(defun list-sessions (&optional (cwd (uiop:getcwd)))
  "List sessions for CWD, most recently updated first: plists of :path,
:updated (last write, ISO-8601 UTC) + header fields.  Ordering by last
write rather than by creation is what makes the newest entry the session
you were last in, even if you got there through /resume."
  (sort-sessions
   (loop for path in (directory (merge-pathnames "*.sexp" (sessions-directory cwd)))
         for header = (ignore-errors
                       (with-open-file (in path :direction :input :external-format :utf-8)
                         (read-sexpr-stream in)))
         when (and (consp header) (eq (pget header :type) :session))
           collect (list* :path (namestring path)
                          :updated (session-updated path)
                          header))))

(defun latest-session (&optional (cwd (uiop:getcwd)))
  "Path of the session for CWD that was worked in most recently — which is
what a bare `--resume` and a supervisor restart should reopen, even when the
last thing you did was resume an older session."
  (pget (first (list-sessions cwd)) :path))

;;; The session index (~/.evo/sessions/index.jsonl).
;;;
;;; One JSON object per line, last line per id wins.  A session appends its own
;;; row when it starts, when it switches journals, when its header changes and
;;; when it ends, which is enough for a list to be useful without making every
;;; append a file write somewhere else.  `sessions --json` reads the index, and
;;; a row is therefore allowed to be stale (updated-at and entries especially):
;;; correctness over precision is the trade, and a rescan rebuilds the whole
;;; file from the journals on disk.

(defun session-index-path ()
  "The index every session of this home appends its row to."
  (merge-pathnames "index.jsonl" (merge-pathnames "sessions/" (evo-home))))

(defun one-line (text &key (limit 80))
  "TEXT as one line of at most LIMIT characters: a session title is read in a
list, where a newline or a paragraph is all noise."
  (let ((collapsed (string-join
                    " "
                    (remove "" (uiop:split-string (or text "")
                                                  :separator '(#\Space #\Tab #\Newline #\Return))
                            :test #'string=))))
    (cond ((zerop (length collapsed)) nil)
          ((<= (length collapsed) limit) collapsed)
          (t (concatenate 'string (subseq collapsed 0 (1- limit)) "…")))))

(defun message-text-block (message)
  "The text of MESSAGE's first text block."
  (pget (find :text (pget message :content) :key (lambda (b) (pget b :type))) :text))

(defun journal-title (journal)
  "What makes JOURNAL recognisable in a list: the first user text on the
current leaf path, on one line, at most 80 characters."
  (one-line
   (loop for entry in (if (journal-leaf-id journal) (entry-path journal) nil)
         for message = (and (eq (pget entry :type) :message) (pget entry :message))
         when (and message (eq (pget message :role) :user))
           return (message-text-block message))))

(defun journal-entry-count (journal)
  "How many entries JOURNAL holds — the whole file, compactions included."
  (length (journal-entries journal)))

(defun iso8601->unix-ms (text)
  "TEXT (an ISO-8601 UTC stamp) in epoch milliseconds, or NIL."
  (and (stringp text)
       (ignore-errors
        (* 1000 (local-time:timestamp-to-unix (local-time:parse-timestring text))))))

(defun universal->unix-ms (universal)
  "A CL universal time (seconds since 1900) in epoch milliseconds."
  (and universal (* 1000 (- universal 2208988800))))

(defun session-record (journal &key updated-at)
  "JOURNAL's index row: the fields CONTRACT §2 names, snake_case on the wire.
PROGRAM defaults to \"evo-agent\" for a session written before the header had
one; SWARM-ID is NIL except for a coordinator."
  (let ((header (journal-header journal)))
    (list :id (pget header :id)
          :path (namestring (journal-path journal))
          :cwd (pget header :cwd)
          :program (or (pget header :program) "evo-agent")
          :swarm-id (pget header :swarm-id)
          :title (journal-title journal)
          :created-at (or (iso8601->unix-ms (pget header :timestamp))
                          (universal->unix-ms (get-universal-time)))
          :updated-at (or updated-at
                          (universal->unix-ms (ignore-errors
                                               (file-write-date (journal-path journal))))
                          (universal->unix-ms (get-universal-time)))
          :entries (journal-entry-count journal))))

(defun json-encode (value)
  "VALUE as JSON text: a plist (keyword keys) -> object with snake_case keys,
a list or vector -> array, a string or integer -> itself, NIL -> null.

The core cannot reach serve's encoder (that one belongs to a frontend), and
what the session index and `sessions --json` write is flat objects of strings,
numbers and null, so this carries its own."
  (com.inuoe.jzon:stringify (json-value value)))

(defun json-value (value)
  (typecase value
    (null 'null)
    ((eql t) t)
    (string value)
    (integer value)
    (keyword (substitute #\_ #\- (string-downcase (symbol-name value))))
    (cons (if (keywordp (car value))
              (let ((object (make-hash-table :test #'equal)))
                (loop for (key item) on value by #'cddr
                      do (setf (gethash (substitute #\_ #\- (string-downcase
                                                               (symbol-name key)))
                                        object)
                               (json-value item)))
                object)
              (map 'vector #'json-value value)))
    (vector (map 'vector #'json-value value))
    (t (princ-to-string value))))

(defun json-object (plist)
  "PLIST (keyword keys) as one line of JSON."
  (json-encode plist))

(defun normalize-index-record (record)
  "RECORD read from the index with its optional fields nil rather than absent,
so callers see one shape whichever path produced it."
  (list :id (pget record :id)
        :path (pget record :path)
        :cwd (or (pget record :cwd) "")
        :program (pget record :program)
        :swarm-id (pget record :swarm-id)
        :title (pget record :title)
        :created-at (pget record :created-at)
        :updated-at (pget record :updated-at)
        :entries (or (pget record :entries) 0)))

(defun index-session (journal)
  "Append JOURNAL's row to the session index.  No-op for a session that has
nothing on disk yet (the index lists sessions, not intents) or that has no
journal at all.  Returns the row."
  (when (and journal (journal-started-p journal))
    (let ((record (session-record journal)))
      (handler-case
          (let ((path (session-index-path)))
            (ensure-directories-exist path)
            (with-open-file (out path :direction :output :if-exists :append
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
              (write-string (json-object record) out)
              (terpri out)))
        (error (e) (warn "Could not write the session index: ~a" e)))
      record)))

(defun index-text-field (object name)
  "One string field of a parsed index line, or NIL when it is absent — JSON
null reads back as a symbol, not NIL, and every nullable field here is a
string."
  (let ((value (gethash name object)))
    (and (stringp value) value)))

(defun index-line-record (line)
  "LINE of the index as a record plist, or NIL when it is not one."
  (let ((object (ignore-errors (evo.util:parse-json line))))
    (when (hash-table-p object)
      (let ((id (gethash "id" object))
            (path (gethash "path" object)))
        (when (and (stringp id) (stringp path))
          (list :id id
                :path path
                :cwd (index-text-field object "cwd")
                :program (index-text-field object "program")
                :swarm-id (index-text-field object "swarm_id")
                :title (index-text-field object "title")
                :created-at (let ((v (gethash "created_at" object)))
                              (and (integerp v) v))
                :updated-at (let ((v (gethash "updated_at" object)))
                              (and (integerp v) v))
                :entries (let ((v (gethash "entries" object)))
                           (and (integerp v) v))))))))

(defun read-session-index ()
  "Every row of the index, last line per id winning, newest first."
  (let ((path (session-index-path))
        (rows (make-hash-table :test #'equal))
        (order nil))
    (when (probe-file path)
      (with-open-file (in path :direction :input :external-format :utf-8
                               :if-does-not-exist nil)
        (loop for line = (read-line in nil nil)
              while line
              for record = (index-line-record (string-trim '(#\Return #\Newline) line))
              when record
                do (unless (gethash (pget record :id) rows)
                     (push (pget record :id) order))
                   (setf (gethash (pget record :id) rows) record))))
    (sort (loop for id in order
                for record = (gethash id rows)
                when record collect (normalize-index-record record))
          #'session-record-newer-p)))

(defun session-record-newer-p (a b)
  "Most recently updated first; the path breaks a tie."
  (let ((ua (or (pget a :updated-at) 0))
        (ub (or (pget b :updated-at) 0)))
    (if (= ua ub)
        (string> (or (pget a :path) "") (or (pget b :path) ""))
        (> ua ub))))

(defun scan-session-file (path)
  "One session file read whole for the index: its header, its entry count and
its opening prompt.  NIL for a file that is not a session."
  (handler-case
      (with-open-file (in path :direction :input :external-format :utf-8)
        (let ((header (read-sexpr-stream in)))
          (when (and (consp header) (eq (pget header :type) :session))
            (let ((entries 0) (title nil))
              (loop for entry = (read-sexpr-stream in)
                    until (eq entry :eof)
                    do (incf entries)
                       (when (and (null title)
                                  (eq (pget entry :type) :message)
                                  (eq (pget (pget entry :message) :role) :user))
                         (setf title (one-line (message-text-block (pget entry :message))))))
              (list :id (pget header :id)
                    :path (namestring (pathname path))
                    :cwd (pget header :cwd)
                    :program (or (pget header :program) "evo-agent")
                    :swarm-id (pget header :swarm-id)
                    :title title
                    :created-at (or (iso8601->unix-ms (pget header :timestamp))
                                    (universal->unix-ms (ignore-errors (file-write-date path))))
                    :updated-at (universal->unix-ms (ignore-errors (file-write-date path)))
                    :entries entries)))))
    (error () nil)))

(defun scan-sessions ()
  "Rebuild the session list from the journals on disk, newest first.  This is
what `sessions --rescan` prints and what the index falls back to when it is
missing — every field comes from the files, so it is always right."
  (sort (loop for directory in (ignore-errors
                                (directory (merge-pathnames "sessions/*/" (evo-home))))
              append (loop for file in (ignore-errors
                                        (directory (merge-pathnames "*.sexp" directory)))
                           for record = (scan-session-file file)
                           when record collect (normalize-index-record record)))
        #'session-record-newer-p))

(defun rebuild-session-index ()
  "Rewrite the index from the journals on disk.  Returns what was written."
  (let ((records (scan-sessions))
        (path (session-index-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
      (dolist (record records)
        (write-string (json-object record) out)
        (terpri out)))
    records))

(defun session-list (&key rescan cwd program all)
  "The sessions the index knows, newest first.  Scanned from the journals when
the index is missing or RESCAN is true.  CWD (default: the working directory)
restricts the list to one folder unless ALL; PROGRAM restricts it to one
frontend (\"evo-agent\", \"evo-swarm\", \"lane\")."
  (let ((records (if (or rescan (not (probe-file (session-index-path))))
                     (scan-sessions)
                     (read-session-index))))
    (remove-if-not
     (lambda (record)
       (and (or all
                (equal (string-right-trim "/" (or (pget record :cwd) ""))
                       (string-right-trim
                        "/" (namestring (uiop:ensure-directory-pathname
                                         (or cwd (uiop:getcwd)))))))
            (or (null program) (equal (pget record :program) program))))
     records)))
