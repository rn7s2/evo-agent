;;;; mirror.lisp — a lane, mirrored: the same VIEW protocol a client runs,
;;;; applied to a lane's op stream and republished as topic `lane:N`.
;;;;
;;;; The coordinator is the only publisher of its own op log, so a lane's
;;;; items and state reach every client under one cursor (CONTRACT §6, §4.3):
;;;; the mirror applies each op from the lane's /stream to its own copy,
;;;; keeping the newest *mirror-items* items, and publishes the op again as
;;;; `lane:N` through EVO.SERVE:PUBLISH-OP.  Items older than the window, and
;;;; image bytes, are proxied to the lane on demand.
;;;;
;;;; A lane's own journal moving (a /new in it, a rewind) or its process
;;;; restarting rebuilds the mirror; both are published as topic.reset
;;;; `lane:N`, which is what a client re-snapshots on.

(in-package :evo.swarm)

(defparameter *mirror-items* 500
  "Items kept per lane topic.  Older ones are fetched from the lane (CONTRACT §6).")

(defparameter *lane-topic* "session"
  "The lane's own topic name: one lane, one session.")

(defstruct (mirror (:constructor %make-mirror (lane topic)))
  lane
  topic                 ; "lane:N"
  state                 ; the lane's topic state (§4.2), or NIL before its first snapshot
  (items nil)           ; chronological, oldest first, at most *MIRROR-ITEMS*
  (dropped 0)           ; items evicted from the front — "there is more below"
  (has-more nil)        ; the lane's own snapshot said so
  cursor                ; "<epoch>.<seq>": where this mirror has read to — only
                        ; the mirror's own thread writes it
  (noted nil)           ; item ids already handed to the layer above (newest first)
  (lock (bt:make-lock "lane-mirror")))

(defun make-mirror (lane)
  (%make-mirror lane (lane-topic lane)))

;;; Plists as JSON: a merge patch, and a key-removing copy.

(defun plist-without (plist keys)
  (loop for (k v) on plist by #'cddr
        unless (member k keys)
          append (list k v)))

(defun merge-patch (target patch)
  "Apply a JSON merge patch (RFC 7386) to TARGET (a plist): objects merge,
arrays and scalars replace whole."
  (if (not (listp patch))
      patch
      (let ((out (and (listp target) (copy-list target))))
        (loop for (k v) on patch by #'cddr
              do (let ((old (getf out k)))
                   (setf (getf out k)
                         (if (and (listp v) (consp v) (listp old) (consp old))
                             (merge-patch old v)
                             v))))
        out)))

;;; The items.

(defvar *mirror-change-hook* nil
  "Called with (MIRROR) for every item a mirror adds or changes: the TUI's
scrollback follows one lane through this (tui.lisp), so nothing below the
presentation layer has to know a screen exists.")

(defun mirror-changed (mirror)
  (when *mirror-change-hook* (funcall *mirror-change-hook* mirror)))

(defun mirror-note-item (mirror item &key quiet)
  "Note ITEM once, however many patches follow it: the layer above hears about
a report or a finished run exactly once.  QUIET notes something the snapshot
brought back — history the coordinator has already heard — so the id is
remembered and nothing is said."
  (let ((id (getf item :id)))
    (when id
      (let ((fresh (bt:with-lock-held ((mirror-lock mirror))
                     (unless (member id (mirror-noted mirror) :test #'equal)
                       (push id (mirror-noted mirror))
                       (when (> (length (mirror-noted mirror)) 200)
                         (setf (mirror-noted mirror)
                               (subseq (mirror-noted mirror) 0 200)))
                       t))))
        (when (and fresh (not quiet)) (lane-item-arrived mirror item))))))

(defun mirror-item-position (mirror id)
  (position id (mirror-items mirror) :key (lambda (item) (getf item :id)) :test #'equal))

(defun mirror-item-ref (mirror id)
  (let ((pos (mirror-item-position mirror id)))
    (and pos (nth pos (mirror-items mirror)))))

(defun mirror-trim (mirror)
  "Forget items past the window, counting them: they are what `has_more` means."
  (let ((excess (- (length (mirror-items mirror)) *mirror-items*)))
    (when (plusp excess)
      (setf (mirror-items mirror) (nthcdr excess (mirror-items mirror)))
      (incf (mirror-dropped mirror) excess))))

(defun mirror-add (mirror item after)
  "Put ITEM in the mirror: after the id AFTER, or at the end when AFTER is NIL
or names an item the mirror no longer holds.  An item whose id is already
there is replaced where it stands — a replayed op never duplicates a row."
  (let* ((items (mirror-items mirror))
         (id (getf item :id))
         (existing (and id (mirror-item-position mirror id))))
    (cond
      (existing (setf (nth existing items) item))
      (t
       (let ((pos (and after (position after items
                                       :key (lambda (i) (getf i :id)) :test #'equal))))
         (setf (mirror-items mirror)
               (if pos
                   (append (subseq items 0 (1+ pos)) (list item) (subseq items (1+ pos)))
                   (append items (list item))))))))
  (mirror-trim mirror))

(defun mirror-field (field)
  (intern (string-upcase field) :keyword))

(defun mirror-append (mirror id field text)
  (let ((item (mirror-item-ref mirror id)))
    (when item
      (let ((key (mirror-field field)))
        (setf (getf item key) (concatenate 'string (or (getf item key) "") text))))))

(defun mirror-patch (mirror id patch)
  (let ((item (mirror-item-ref mirror id)))
    (when item
      (let ((patched (merge-patch item patch)))
        (setf (nth (mirror-item-position mirror id) (mirror-items mirror)) patched)))))

(defun mirror-remove (mirror id)
  (let ((pos (mirror-item-position mirror id)))
    (when pos
      (setf (mirror-items mirror)
            (append (subseq (mirror-items mirror) 0 pos)
                    (subseq (mirror-items mirror) (1+ pos)))))))

;;; Publishing into the coordinator's op log.

(defun publish-op (op-plist)
  "Publish OP-PLIST on the coordinator's server, when it has one (the TUI has
no op log).  Returns T when it went out."
  (let ((server (and *swarm* (swarm-server *swarm*))))
    (when server
      (evo.serve:publish-op server (list* :op (getf op-plist :op)
                                          :topic (getf op-plist :topic)
                                          (plist-without op-plist '(:op :topic :seq :ts))))
      t)))

(defun mirror-publish (mirror op)
  "Republish one of the lane's ops under this mirror's topic."
  (publish-op (list* :op (getf op :op) :topic (mirror-topic mirror)
                     (plist-without op '(:op :topic :seq :ts)))))

;;; Applying the lane's stream.

(defun mirror-apply (mirror op-plist)
  "Apply one op from the lane's stream to MIRROR and republish it as this
lane's topic.  Unknown ops are ignored (a newer lane must not break an older
coordinator).  Returns T when the mirror changed.

Nothing is published while the mirror is locked: the swarm's own state is
built from the mirrors, so taking the swarm lock under the mirror lock would
order the two the other way round."
  (let ((op (getf op-plist :op))
        (republish t) (rebuild nil) (reason nil) (state-changed nil) (arrived nil))
    (bt:with-lock-held ((mirror-lock mirror))
      (cond
        ((equal op "item.add")
         (mirror-add mirror (getf op-plist :item) (getf op-plist :after))
         (setf arrived (getf op-plist :item)))
        ((equal op "item.append")
         (mirror-append mirror (getf op-plist :id) (getf op-plist :field)
                        (getf op-plist :text)))
        ((equal op "item.patch")
         (mirror-patch mirror (getf op-plist :id) (getf op-plist :patch)))
        ((equal op "item.remove")
         (mirror-remove mirror (getf op-plist :id)))
        ((equal op "state.patch")
         (setf (mirror-state mirror) (merge-patch (mirror-state mirror)
                                                  (getf op-plist :patch))
               state-changed t))
        ((equal op "topic.reset")
         ;; The lane switched journals or moved its leaf: its items are not
         ;; ours to patch, so the whole mirror is rebuilt.
         (setf rebuild t republish nil reason (getf op-plist :reason)))
        ((equal op "stream.reset")
         (setf rebuild t republish nil reason "lane_restarted"))
        (t (setf republish nil))))
    (cond (rebuild (mirror-rebuild mirror reason))
          (republish (mirror-publish mirror op-plist)))
    (when state-changed (swarm-lane-changed mirror))
    (when arrived (mirror-note-item mirror arrived))
    (when (or republish rebuild) (mirror-changed mirror))
    (if rebuild :rebuilt (and republish t))))

(defun mirror-items-after (mirror id)
  "The items after ID, oldest first — or all of them when ID is NIL or is not
in the window any more."
  (bt:with-lock-held ((mirror-lock mirror))
    (let* ((items (mirror-items mirror))
           (pos (and id (position id items :key (lambda (i) (getf i :id)) :test #'equal))))
      (copy-list (if pos (subseq items (1+ pos)) items)))))

(defun mirror-last-items (mirror n)
  "The newest N items, oldest first."
  (bt:with-lock-held ((mirror-lock mirror))
    (let ((all (mirror-items mirror)))
      (copy-list (last all (min n (length all)))))))

(defun mirror-state-set (mirror state)
  "Replace the mirror's topic state (a snapshot's, or a test's)."
  (bt:with-lock-held ((mirror-lock mirror))
    (setf (mirror-state mirror) state)))

(defun mirror-load (mirror)
  "Take a fresh snapshot of the lane into MIRROR and return the cursor its
stream should start at.  Nothing is published: whether this is news is the
caller's to say (MIRROR-REBUILD is the version that tells the clients)."
  (let* ((lane (mirror-lane mirror))
         (snapshot (lane-snapshot lane :topics *lane-topic* :items *mirror-items*))
         (entry (getf (getf snapshot :topics) (lane-topic-key)))
         (cursor (format nil "~a.~a" (getf snapshot :epoch) (getf snapshot :seq))))
    (bt:with-lock-held ((mirror-lock mirror))
      (setf (mirror-state mirror) (getf entry :state)
            (mirror-items mirror) (coerce (or (getf entry :items) #()) 'list)
            (mirror-has-more mirror) (and (getf entry :has-more) t)
            (mirror-dropped mirror) 0
            (mirror-cursor mirror) cursor))
    (dolist (item (mirror-items mirror)) (mirror-note-item mirror item :quiet t))
    cursor))

(defun mirror-rebuild (mirror &optional (reason "lane_restarted"))
  "Replace the mirror from a fresh snapshot of the lane, and tell every client
to re-snapshot this topic.  REASON is the topic.reset reason (§5.3).  Returns
T when the lane could be reached."
  (let ((cursor (ignore-errors (mirror-load mirror))))
    (when cursor
      (publish-op (list :op "topic.reset" :topic (mirror-topic mirror)
                        :reason (or reason "lane_restarted")))
      (swarm-lane-changed mirror)
      t)))

(defun lane-topic-key ()
  "The lane's own topic as a keyword — how its snapshot names it."
  (intern (string-upcase *lane-topic*) :keyword))

;;; The topic provider (CONTRACT §7): what serve asks a topic for.

(defmethod evo.serve:topic-snapshot ((mirror mirror) &key (items 200))
  (bt:with-lock-held ((mirror-lock mirror))
    (let* ((all (mirror-items mirror))
           (n (length all))
           (window (if (> n items) (nthcdr (- n items) all) all)))
      (list :state (mirror-state mirror)
            :items (coerce window 'vector)
            :has-more (and (or (plusp (mirror-dropped mirror))
                               (and (getf (mirror-has-more mirror) t) (> n items)))
                           t)))))

(defmethod evo.serve:topic-items-before ((mirror mirror) before limit)
  "Items older than BEFORE.  In the window, from the mirror; below it, from the
lane, which still has them."
  (bt:with-lock-held ((mirror-lock mirror))
    (let ((pos (and before (mirror-item-position mirror before))))
      (when pos
        (let* ((items (mirror-items mirror))
               (start (max 0 (- pos limit)))
               (window (subseq items start pos)))
          (return-from evo.serve:topic-items-before
            (values window (and (or (plusp start) (plusp (mirror-dropped mirror))) t)))))))
  (lane-items-before (mirror-lane mirror) *lane-topic* before limit))

(defmethod evo.serve:topic-item ((mirror mirror) id)
  (bt:with-lock-held ((mirror-lock mirror))
    (let ((item (mirror-item-ref mirror id)))
      (when item (return-from evo.serve:topic-item item))))
  (lane-item (mirror-lane mirror) *lane-topic* id))

(defmethod evo.serve:topic-media ((mirror mirror) id n)
  (lane-media (mirror-lane mirror) *lane-topic* id n))

(defun mirror-lane-state (mirror)
  "The lane's own topic state (§4.2), or NIL before its first snapshot."
  (and mirror
       (bt:with-lock-held ((mirror-lock mirror))
         (mirror-state mirror))))

(defun mirror-note-lane-state (mirror)
  "Copy what the lane says about itself into the swarm's record of it: its
status (so `starting → idle` and `working → idle` are the lane's own words),
its task and step clocks, and the goal it cached.  The swarm's own states —
:starting while it comes up, :down, :stopped — are not overwritten by a
snapshot from a lane that is not there any more."
  (let* ((lane (mirror-lane mirror))
         (state (mirror-lane-state mirror))
         (status (getf state :status))
         (task (getf state :task)))
    (when lane
      (with-swarm-lock ()
        (let ((own (lane-state lane)))
          ;; The lane's own status is authoritative while its process is
          ;; there: :down and :stopped are the swarm's word that it is not.
          (setf (lane-state lane)
                (cond ((member own '(:down :stopped)) own)
                      ((equal status "running") :working)
                      ((equal status "compacting") :compacting)
                      ((equal status "idle") :idle)
                      (t :starting))
                (lane-task-started lane) (getf task :started-at)
                (lane-step-started lane) (getf task :step-started-at)))))))

(defun mirror-last-item (mirror)
  "The newest item the mirror holds, as a short (kind summary) row (§4.3)."
  (bt:with-lock-held ((mirror-lock mirror))
    (let ((item (car (last (mirror-items mirror)))))
      (when item
        (list :kind (getf item :kind)
              :summary (or (truncate-string (or (getf item :text) "") 80 "…")
                           (getf item :name)))))))
