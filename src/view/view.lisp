;;;; view.lisp — the live view: a projection that is kept up to date rather
;;;; than recomputed.
;;;;
;;;; One view per session topic.  The session thread (and the run worker, when
;;;; a run streams) feeds it kernel events and journal appends; every change
;;;; becomes an op and goes to the attached publish function, which is the
;;;; serve layer's op log.  Reads come from here and never queue behind the
;;;; session thread: a snapshot is copied out under a short lock, so a client
;;;; that asks while the model is thinking gets its answer immediately.
;;;;
;;;; The invariant this file owes the rest of the system is the one the unit
;;;; suite property-tests: after any sequence of events and appends, the items
;;;; a client built by applying the ops are the items PROJECT-JOURNAL gives for
;;;; the journal now on disk.  Every journaled item is therefore written from
;;;; the ENTRY, never from the event: events carry what is happening (a delta,
;;;; a tool starting), entries carry what happened.

(in-package :evo.view)

(defparameter *op-item-add* "item.add")
(defparameter *op-item-append* "item.append")
(defparameter *op-item-patch* "item.patch")
(defparameter *op-item-remove* "item.remove")
(defparameter *op-state-patch* "state.patch")
(defparameter *op-topic-reset* "topic.reset")

(defstruct (view (:conc-name v-) (:constructor %make-view))
  agent
  (topic "session")
  publish                                ; (lambda (op-plist)), or NIL
  (lock (bt:make-lock "evo-view"))
  items                ; item plists, in order — always a MAKE-ITEM-VECTOR
  (index (make-hash-table :test #'equal))  ; id -> item
  (wire (make-hash-table :test #'equal))   ; id -> bounded item (snapshot cache)
  (media (make-hash-table :test #'equal))  ; id -> vector of :image blocks
  state
  (ctx nil)             ; the walk's memory: tool-call arguments, last goal, …
  (task nil)            ; the running task, as state's plist, or NIL
  (task-kind nil)       ; "run" or "compact"
  (compacting nil)      ; a compaction is in flight (inside a run or on its own)
  (assistant-item nil)  ; id of the assistant item being streamed
  (tool-item nil)       ; id of the tool item currently running
  (run-tokens 0)        ; tokens this run has spent (the goal readout's live part)
  (held nil)            ; why the agent is held, or NIL
  (context nil)         ; live context accounting, while a run streams
  (counter 0))

(defvar *views* nil
  "Every live view in this process, so a hold announcement reaches the topics
that show it.  A view is never removed: a process owns one or two of them for
as long as it lives.")

;;; Publishing.

(defun emit-op (view op-name &rest fields)
  "Hand one op to the attached publisher.  Called with VIEW's lock held: the
publisher is an op log, which only appends, so op order is mutation order and a
client applying the ops in sequence sees every change in the order they
happened."
  (let ((publish (v-publish view)))
    (when publish
      (funcall publish (list* :op op-name :topic (v-topic view)
                              :ts (now-ms) fields)))))

(defun emit-state-patch (view patch)
  (when patch (emit-op view *op-state-patch* :patch patch)))

;;; Item storage.  Items are replaced wholesale, never mutated in place: a
;;; plist handed to a client (or cached here) must stay the value it was.

(defun item-wire (view item)
  "ITEM bounded for a snapshot or an op, memoized until the item changes."
  (let ((id (pget item :id)))
    (or (gethash id (v-wire view))
        (setf (gethash id (v-wire view)) (truncate-item item)))))

(defun item-index (view id)
  "Where ID sits in the transcript.  Searched from the end: the item a change
lands on is almost always the newest one — a streaming message, the tool that
is running — so the common case is one step."
  (loop for i downfrom (1- (length (v-items view))) to 0
        when (equal (pget (aref (v-items view) i) :id) id) return i))

(defun store-item (view item)
  (vector-push-extend item (v-items view))
  (setf (gethash (pget item :id) (v-index view)) item)
  item)

(defun replace-item (view item)
  "Adopt ITEM, which has the id of an item already in the transcript."
  (let* ((id (pget item :id))
         (i (item-index view id)))
    (unless i (error "view: no item ~a to replace" id))
    (remhash id (v-wire view))
    (setf (gethash id (v-index view)) item
          (aref (v-items view) i) item)
    item))

(defun make-item-vector (&optional (size 16))
  "The items slot is always an adjustable vector with a fill pointer: appends
are the hot path and every mutation has to keep it that way."
  (make-array size :adjustable t :fill-pointer 0))

(defun drop-item (view id)
  (let ((i (item-index view id)))
    (when i
      (let ((kept (make-item-vector (length (v-items view)))))
        (loop for j from 0 below (length (v-items view))
              unless (= j i) do (vector-push-extend (aref (v-items view) j) kept))
        (setf (v-items view) kept))
      (remhash id (v-index view))
      (remhash id (v-wire view))
      (emit-op view *op-item-remove* :id id))))

(defun last-item-id (view)
  (when (plusp (length (v-items view)))
    (pget (aref (v-items view) (1- (length (v-items view)))) :id)))

(defun append-item (view item)
  "Add ITEM at the end of the transcript and publish it."
  (let ((after (last-item-id view)))
    (store-item view item)
    (emit-op view *op-item-add* :item (item-wire view item) :after after)
    item))

(defun patch-item (view new)
  "Adopt NEW, replacing the item with its id, and publish what changed.  An id
that is not in the transcript yet is an ADD: the client must hear about the
item itself, not about fields of one it has never seen."
  (let* ((id (pget new :id))
         (old (gethash id (v-index view))))
    (if (null old)
        (append-item view new)
        (let* ((old-wire (item-wire view old))
               (new-wire (truncate-item new))
               (patch (loop for (k v) on new-wire by #'cddr
                            unless (equal (pget old-wire k) v) append (list k v))))
          (replace-item view new)
          (setf (gethash id (v-wire view)) new-wire)
          (when patch (emit-op view *op-item-patch* :id id :patch patch))
          new))))

(defun patch-item-fields (view id &rest fields)
  "Change a few fields of the item ID holds, publishing a merge patch."
  (let ((item (gethash id (v-index view))))
    (when item
      (let ((new (loop for (k v) on fields by #'cddr
                       do (setf item (pput item k v))
                       finally (return item))))
        (replace-item view new)
        (emit-op view *op-item-patch* :id id :patch fields)
        new))))

(defun append-item-text (view id field text)
  "Grow a streaming item's text and publish the delta."
  (let ((item (gethash id (v-index view))))
    (when item
      (let ((new (pput item field (concatenate 'string
                                               (or (pget item field) "")
                                               text))))
        (replace-item view new)
        (emit-op view *op-item-append* :id id :field (enum-string field) :text text)
        new))))

;;; State.

(defun jobs-wire ()
  "The running background jobs as state's list: id, name, status, start time."
  (coerce (loop for job in (running-jobs)
                collect (list :id (pget job :id)
                              :name (or (first (uiop:split-string
                                                (or (pget job :command) "")
                                                :separator '(#\Newline)))
                                        "")
                              :status (enum-string (pget job :status))
                              :started-at (* 1000 (pget job :started-at))))
          'vector))

(defun view-status (view)
  (cond ((v-compacting view) "compacting")
        ((v-task view) "running")
        ((v-held view) "waiting")
        (t "idle")))

(defun refresh-state (view)
  "Recompute the topic state from the journal and the view's own run facts, and
publish the fields that moved.  The view's own paths call this with the lock
held; VIEW-REFRESH-STATE is the same thing for a frontend that knows the fold
moved without an append."
  (setf (v-held view) (agent-hold-reason (v-agent view)))
  (let* ((agent (v-agent view))
         (journal (agent-journal agent))
         (fold (fold-state journal))
         (state (journal-state journal
                               :agent agent
                               :status (view-status view)
                               :task (v-task view)
                               :queue (queued-ids (v-items view))
                               :jobs (jobs-wire)
                               :live-goal-tokens (v-run-tokens view)
                               :context (v-context view)))
         (old (v-state view)))
    ;; Resolution of a provider for a message that names only a model id reads
    ;; the fold, so the walk's copy of it has to move with the journal.
    (setf (pctx-fold (v-ctx view)) fold
          (v-state view) state)
    (when old
      (emit-state-patch view (loop for (k v) on state by #'cddr
                                   unless (equal v (pget old k)) append (list k v))))
    state))

(defun view-refresh-state (view)
  "Re-derive the topic state and publish what moved.  For a change no append
reports — a setting, the provider registry, a hold — which a frontend knows
about and the journal does not."
  (bt:with-lock-held ((v-lock view))
    (refresh-state view)))

;;; The task clock: epoch ms, and it counts the STEP, not the task.  A spinner
;;; says only that something is alive; the clock is what tells a slow step from
;;; a wedged one.

(defun view-begin-task (view kind id)
  (let ((now (now-ms)))
    (setf (v-task-kind view) (enum-string kind)
          (v-task view) (list :id id :kind (enum-string kind)
                              :turn nil
                              :started-at now
                              :step-started-at now))))

(defun view-step (view)
  (let ((task (v-task view)))
    (when task (setf (v-task view) (pput task :step-started-at (now-ms))))))

(defun view-end-task (view)
  (setf (v-task view) nil
        (v-task-kind view) nil
        (v-compacting view) nil
        (v-run-tokens view) 0
        (v-context view) nil
        (v-held view) (agent-hold-reason (v-agent view))))

(defun view-live-context (view &key tokens delta source)
  "The context readout while a run streams: provider usage when there is any,
otherwise the last estimate advanced by what the events added."
  (let* ((current (or (v-context view) (pget (v-state view) :context)))
         (value (or tokens (+ (or (pget current :tokens) 0) (or delta 0)))))
    (setf (v-context view) (list :tokens value
                                 :window (pget current :window)
                                 :source (or source "estimate")))))

;;; Input that is queued but not yet journaled.  The id is the entry id the
;;; kernel minted when it queued the input (CONTRACT §3: pre-minted ids), so
;;; the same id names the item before and after it becomes a journal entry.

(defun view-input-queued (view &key id text images queue)
  "Show input that is queued but not yet sent.  IMAGES is a list of :image
content blocks (EVO.MEDIA:MAKE-IMAGE-BLOCK) — the same blocks the message will
carry when it is journaled.  Returns the item id."
  (bt:with-lock-held ((v-lock view))
    (let* ((id (or id (format nil "q_~a" (gen-id))))
           (blocks (coerce (or images #()) 'list))
           (item (user-item id (now-ms)
                            :text (or text "")
                            :images (image-wires id blocks)
                            :status "queued"
                            :queue (enum-string (or queue :now)))))
      (when blocks (setf (gethash id (v-media view)) (coerce blocks 'vector)))
      ;; Queueing the same id twice (a retried op, a replayed event) is the
      ;; same item, not a second one.
      (patch-item view item)
      (refresh-state view)
      id)))

(defun view-input-cancelled (view id)
  "The queued input ID was withdrawn before it was sent, so it leaves the
transcript — which is also what a re-projection of the journal says, since a
cancelled input is never journaled.  Returns T when there was one."
  (bt:with-lock-held ((v-lock view))
    (let ((item (gethash id (v-index view))))
      (when (and item (equal (pget item :status) "queued"))
        (drop-item view id)
        (refresh-state view)
        t))))

;;; Journal appends.

(defun cache-media (view entry id)
  "Remember a user message's image blocks, so VIEW-MEDIA can serve the bytes."
  (let ((blocks (blocks-images (content-blocks (pget entry :message)))))
    (when blocks (setf (gethash id (v-media view)) (coerce blocks 'vector)))))

(defun handle-user-append (view entry)
  "A user turn is journaled.  Input that was shown while it waited carries the
entry's id already (the kernel mints it when it queues the input, CONTRACT §3),
so this is the same row, now sent; anything else is a new item."
  (let* ((id (pget entry :id))
         (existing (gethash id (v-index view))))
    (cache-media view entry id)
    (if existing
        ;; The entry is authoritative, its timestamp included: a row that
        ;; waited is dated by when it was journaled, not by when it was typed
        ;; into a queue (which is the same second, to the millisecond).
        (patch-item-fields view id :status "sent" :ts (entry-ms entry))
        (patch-item view (entry->item entry (v-ctx view))))))

(defun handle-assistant-append (view entry)
  "The message is journaled.  Its id is the one the stream announced, so this
finishes the item the client has been watching; an item that was never
announced (a rebuild, or a frontend that missed the start) is added here."
  (let ((id (pget entry :id)))
    (patch-item view (entry->item entry (v-ctx view)))
    (setf (v-assistant-item view) id)))

(defun handle-tool-append (view entry)
  (let* ((item (entry->item entry (v-ctx view)))
         (id (and item (pget item :id))))
    (when item
      (if (gethash id (v-index view)) (patch-item view item) (append-item view item)))))

(defun handle-append (view entry)
  (let ((type (pget entry :type)))
    (case type
      (:message
       (case (pget (pget entry :message) :role)
         (:user (handle-user-append view entry))
         (:assistant (handle-assistant-append view entry))
         (:tool-result (handle-tool-append view entry))
         (t nil))
       (refresh-state view))
      ((:custom-message :notice :compaction :branch-summary)
       (let ((item (entry->item entry (v-ctx view))))
         (when item
           (if (gethash (pget item :id) (v-index view))
               (patch-item view item)
               (append-item view item))))
       (refresh-state view))
      (:goal
       (let ((item (entry->item entry (v-ctx view))))
         (when item (append-item view item)))
       (refresh-state view))
      (:recover
       (entry->item entry (v-ctx view))  ; remembered for the note that follows
       (refresh-state view))
      ((:model-change :thinking-change :tools-change :load :custom :label)
       (refresh-state view))
      (t nil))))

(defun view-on-append (view entry)
  "Feed one journal append to VIEW."
  (bt:with-lock-held ((v-lock view))
    (handle-append view entry)))

;;; Kernel events.  Events say what is happening; the journal entry that
;;; follows says what happened, and wins.

(defun handle-message-start (view event)
  "The model has started talking.  The event names the entry the attempt will
be journaled as (CONTRACT §3: the id is minted at message-start), so the item
that streams here IS that entry — the append that follows finishes it rather
than replacing it.  A message-start with no id has nothing to name the line
with; the entry still arrives when it is appended."
  (let ((id (pget event :entry-id)))
    (when id
      (let* ((model (pget (v-state view) :model))
             (item (list :id id :kind "assistant" :ts (now-ms)
                         :text "" :thinking "" :status "streaming"
                         :error nil
                         :model (pget model :id)
                         :provider (pget model :provider)
                         :usage nil)))
        ;; A retried attempt announces the same id again: the item is the
        ;; attempt, not a second one.
        (unless (gethash id (v-index view)) (append-item view item))
        (setf (v-assistant-item view) id)))))

(defun handle-text-delta (view event)
  (let ((id (v-assistant-item view)))
    (when id (append-item-text view id :text (or (pget event :text) "")))))

(defun handle-thinking-delta (view event)
  (let ((id (v-assistant-item view)))
    (when id (append-item-text view id :thinking (or (pget event :text) "")))))

(defun handle-message-end (view event)
  (let* ((id (v-assistant-item view))
         (item (and id (gethash id (v-index view)))))
    ;; The journal entry is authoritative and it arrives first: an item the
    ;; entry already finished is left alone.  The event is what fills in a
    ;; message whose entry this frontend never saw.
    (when (and item (equal (pget item :status) "streaming"))
      (let* ((stop (pget event :stop-reason))
             (status (case stop (:error "error") (:aborted "aborted")
                            (:length "length") (t "final"))))
        (patch-item-fields view id
                           :status status
                           :error (pget event :error)
                           :model (or (pget event :model) (pget item :model))
                           :usage (or (usage-wire (pget event :usage))
                                      (pget item :usage))))))
  (let ((usage (pget event :usage)))
    (when (and usage (plusp (usage-total-tokens usage)))
      (incf (v-run-tokens view) (usage-total-tokens usage))
      (view-live-context view :tokens (usage-total-tokens usage) :source "usage")))
  (refresh-state view))

(defun handle-tool-call-start (view event)
  (let* ((call-id (pget event :id))
         (item (tool-item (tool-id call-id) (now-ms)
                          :call-id call-id
                          :name (pget event :name)
                          :args (pget event :arguments)
                          :status "running"
                          :result nil
                          :parent (v-assistant-item view))))
    (setf (gethash call-id (pctx-calls (v-ctx view)))
          (list :name (pget event :name) :arguments (pget event :arguments)))
    (patch-item view item)
    (setf (v-tool-item view) (pget item :id))))

(defun handle-tool-result (view event)
  ;; The result's text comes from the journal entry the tool appended; the
  ;; event's own copy is cut to 500 characters and is only a liveness signal.
  (let ((item (gethash (tool-id (pget event :id)) (v-index view))))
    (when item
      (patch-item-fields view (pget item :id)
                         :status (if (pget event :is-error) "error" "ok")))
    (setf (v-tool-item view) nil)
    (view-live-context view :delta (ceiling (or (pget event :content-chars) 0) 4))))

(defun handle-run-outcome (view event)
  (let ((outcome (pget event :outcome)))
    (when (member outcome '(:aborted :error :length))
      (append-item view
                   (list :id (format nil "ro_~a" (or (pget event :run-id)
                                                     (incf (v-counter view))))
                         :kind "run_outcome" :ts (now-ms)
                         :outcome (enum-string outcome)
                         :error (when (eq outcome :error) (pget event :error)))))))

(defun handle-notice (view event)
  ;; A durable notice is journaled as a :notice entry and the entry is the
  ;; item, so showing the event too would double it.  An ephemeral one exists
  ;; only here, which is what DURABLE is for.
  (unless (pget event :durable)
    (append-item view
                 (list :id (or (pget event :entry-id)
                               (format nil "n_~a" (gen-id)))
                       :kind "notice" :ts (now-ms)
                       :severity (enum-string
                                  (or (pget event :severity)
                                      (case (pget event :style)
                                        (:error :error) (:notice :warn)
                                        (t :info))))
                       :text (or (pget event :text) "")
                       :source (enum-string (or (pget event :source) :extension))
                       :durable nil))))

(defun handle-provider-retry (view event)
  ;; The dead attempt's partial text is about to be streamed again from the
  ;; top: what was shown of it is dropped, or the retry would read as the text
  ;; said twice.  (The entry the attempt is journaled as keeps the id of that
  ;; first attempt, so the item survives the retry.)
  (let ((streaming (and (v-assistant-item view)
                        (gethash (v-assistant-item view) (v-index view)))))
    (when (and streaming (equal (pget streaming :status) "streaming"))
      (patch-item-fields view (pget streaming :id) :text "" :thinking "")))
  (let* ((id "retry")
         (delay (pget event :delay))
         (item (list :id id :kind "provider_retry" :ts (now-ms)
                     :attempt (pget event :attempt)
                     :max (pget event :max)
                     :delay-ms (cond ((null delay) nil)
                                     ((numberp delay) (round (* 1000 delay)))
                                     (t delay))
                     :reason (pget event :reason))))
    (if (gethash id (v-index view)) (patch-item view item) (append-item view item))))

(defun handle-event (view event)
  (case (pget event :type)
    (:run-start
     (view-begin-task view :run (or (pget event :run-id) (gen-id)))
     (refresh-state view))
    (:turn-start
     (view-step view)
     (let ((task (v-task view)))
       (when task
         (setf (v-task view) (pput (pput task :turn (pget event :turn))
                                   :step-started-at (now-ms)))))
     (refresh-state view))
    (:message-start (handle-message-start view event))
    (:text-delta (handle-text-delta view event))
    (:thinking-delta (handle-thinking-delta view event))
    (:message-end (handle-message-end view event))
    (:tool-call-start (handle-tool-call-start view event))
    (:tool-result (handle-tool-result view event))
    (:compaction-start
     (unless (v-task view)
       (view-begin-task view :compact (format nil "c_~a" (gen-id))))
     (setf (v-compacting view) t)
     (view-step view)
     (refresh-state view))
    (:compaction-end
     (setf (v-compacting view) nil)
     (when (and (v-task view) (equal (v-task-kind view) "compact"))
       (view-end-task view))
     (view-step view)
     (refresh-state view))
    (:run-end
     (handle-run-outcome view event)
     (view-end-task view)
     (refresh-state view))
    (:settled
     ;; A frontend-level settle: nothing is running any more, so the hold is
     ;; what decides between idle and waiting.
     (setf (v-held view) (agent-hold-reason (v-agent view)))
     (refresh-state view))
    (:provider-retry (handle-provider-retry view event))
    (:notice (handle-notice view event))
    (:todo-changed (refresh-state view))
    (:steering
     (let ((id (pget event :entry-id)))
       (when (and id (gethash id (v-index view)))
         (patch-item-fields view id :status "sent")
         (refresh-state view))))
    (:input-queued
     (view-input-queued view :id (pget event :id) :text (pget event :text)
                              :images (pget event :images)
                              :queue (pget event :queue)))
    (:input-cancelled (view-input-cancelled view (pget event :id)))
    (t nil)))

(defun view-on-event (view event)
  "Feed one kernel event to VIEW."
  (bt:with-lock-held ((v-lock view))
    (handle-event view event)))

;;; Holds: a settled agent that is not free.

(defun view-note-hold (view)
  (bt:with-lock-held ((v-lock view))
    (let ((reason (agent-hold-reason (v-agent view))))
      (unless (equal reason (v-held view))
        (setf (v-held view) reason)
        (refresh-state view)))))

(defun view-hold-changed (payload)
  (let ((agent (pget payload :agent)))
    (dolist (view *views*)
      (when (eq (v-agent view) agent) (view-note-hold view)))))

(add-hook :hold-changed #'view-hold-changed :name :view-hold)

;;; Reading.

(defun view-snapshot (view &key (items 200))
  "The topic as a client reads it: (:state … :items #(…) :has-more …).  The
newest ITEMS items are returned; older ones are reachable through
VIEW-ITEMS-BEFORE."
  (bt:with-lock-held ((v-lock view))
    (let* ((all (v-items view))
           (n (length all))
           (count (if (and items (plusp items)) (min items n) n))
           (start (- n count)))
      (list :state (v-state view)
            :items (loop for i from start below n
                         collect (item-wire view (aref all i)) into list
                         finally (return (coerce list 'vector)))
            :has-more (> start 0)))))

(defun view-items-before (view before-id limit)
  "The items older than BEFORE-ID (NIL: the newest), newest first, plus whether
even older items exist."
  (bt:with-lock-held ((v-lock view))
    (let* ((all (v-items view))
           (n (length all))
           (end (if before-id (or (item-index view before-id) n) n))
           (limit (max 0 (or limit 100)))
           (start (max 0 (- end limit))))
      (values (loop for i from (1- end) downto start
                    collect (item-wire view (aref all i)) into list
                    finally (return (coerce list 'vector)))
              (> start 0)))))

(defun view-item (view id)
  "The item ID names, whole: thinking and tool output untruncated."
  (bt:with-lock-held ((v-lock view))
    (gethash id (v-index view))))

(defun view-media (view id n)
  "The bytes of the Nth image of item ID: (values octets media-type), or NIL."
  (bt:with-lock-held ((v-lock view))
    (let* ((blocks (gethash id (v-media view)))
           (block (and blocks (integerp n) (>= n 0) (< n (length blocks))
                       (aref blocks n))))
      (when block
        (let ((octets (ignore-errors (base64->octets (or (pget block :data) "")))))
          (values octets (or (pget block :media-type) "application/octet-stream")))))))

(defun view-attach (view publish)
  "Point VIEW's changes at PUBLISH — (lambda (op-plist)), the serve layer's op
log.  Ops emitted before a publisher is attached are dropped: the first read a
client makes is a snapshot, which is authoritative."
  (bt:with-lock-held ((v-lock view))
    (setf (v-publish view) publish)))

(defparameter *reset-reasons*
  '((:session-switched . "session_switched")
    (:leaf-moved . "leaf_moved")
    (:lane-restarted . "lane_restarted")
    (:swarm-switched . "swarm_switched"))
  "The reasons a client is told to re-read a topic instead of applying ops.")

(defun view-reset (view reason)
  "Rebuild VIEW from the journal as it stands now, and tell clients to re-read
it: this is what a leaf move, a session switch or a lane restart does, and it
is why only the append path has to be incremental."
  (bt:with-lock-held ((v-lock view))
    (let* ((agent (v-agent view))
           (journal (agent-journal agent))
           (fold (fold-state journal)))
      (multiple-value-bind (items state) (project-journal journal agent)
        (setf (v-items view) (make-item-vector)
              (v-index view) (make-hash-table :test #'equal)
              (v-wire view) (make-hash-table :test #'equal)
              (v-media view) (make-hash-table :test #'equal)
              (v-ctx view) (make-pctx :fold fold)
              (v-state view) state
              (v-assistant-item view) nil
              (v-tool-item view) nil)
        (loop for item across items do (store-item view item))
        (cache-media-from-journal view journal)
        (emit-op view *op-topic-reset*
                 :reason (or (cdr (assoc reason *reset-reasons*))
                             (enum-string reason)
                             "session_switched"))))
    (refresh-state view)))

(defun cache-media-from-journal (view journal)
  "Re-index the image blocks of every user message on the path, so a client can
still fetch an image after a rebuild."
  (dolist (entry (entry-path journal))
    (when (and (eq (pget entry :type) :message)
               (eq (pget (pget entry :message) :role) :user))
      (cache-media view entry (pget entry :id)))))

;;; Construction.

(defun make-view (agent &key (topic "session"))
  "A live view of AGENT's session, topic TOPIC.  Reads work immediately; ops go
nowhere until VIEW-ATTACH gives them a publisher."
  (let* ((journal (agent-journal agent))
         (fold (fold-state journal))
         (view (%make-view :agent agent :topic topic
                           :items (make-item-vector)
                           :ctx (make-pctx :fold fold)
                           :held (agent-hold-reason agent))))
    (multiple-value-bind (items state) (project-journal journal agent)
      (loop for item across items do (store-item view item))
      (cache-media-from-journal view journal)
      (setf (v-state view) (pput state :status (view-status view))))
    (push view *views*)
    view))

;;; The client's half of the protocol.
;;;
;;; A frontend that only ever sees ops ends up holding exactly what a snapshot
;;; would have given it — that is the invariant the unit suite property-tests
;;; (project the journal, drive the view, apply the ops, compare).  It is also
;;; what a coordinator runs to mirror a lane's stream into a lane:N topic, so
;;; it lives here rather than in a client: one implementation, exercised by the
;;; core's own tests.

(defun apply-op (items op &optional state)
  "ITEMS (a vector of wire items) and STATE with one op from the stream
applied.  Returns (values items state); an op this build does not know is
ignored, which is how a client survives a server that grew an op first."
  (let* ((kind (pget op :op))
         (id (pget op :id))
         (list (coerce items 'list))
         (pos (and id (position id list :key (lambda (i) (pget i :id))
                                     :test #'equal))))
    (cond
      ((equal kind "item.add")
       (let ((item (pget op :item))
             (after (pget op :after)))
         (if (null after)
             (if (null list) (setf list (list item)) (push item list))
             (let ((i (position after list :key (lambda (x) (pget x :id))
                                         :test #'equal)))
               (setf list (if i
                              (append (subseq list 0 (1+ i)) (list item)
                                      (subseq list (1+ i)))
                              (append list (list item))))))))
      ((and (equal kind "item.append") pos)
       (let* ((item (nth pos list))
              (field (if (equal (pget op :field) "thinking") :thinking :text)))
         (setf (nth pos list)
               (pput item field (concatenate 'string
                                             (or (pget item field) "")
                                             (or (pget op :text) ""))))))
      ((and (equal kind "item.patch") pos)
       (let ((item (nth pos list)))
         (loop for (k v) on (pget op :patch) by #'cddr
               do (setf item (pput item k v)))
         (setf (nth pos list) item)))
      ((and (equal kind "item.remove") pos)
       (setf list (remove id list :key (lambda (i) (pget i :id)) :test #'equal)))
      ((equal kind "state.patch")
       (let ((new (copy-list (or state nil))))
         (loop for (k v) on (pget op :patch) by #'cddr
               do (setf (getf new k) v))
         (setf state new)))
      (t nil))
    (values (coerce list 'vector) state)))

(defun apply-ops (items ops &optional state)
  "ITEMS and STATE with every op in OPS applied, in order.  See APPLY-OP."
  (dolist (op ops (values items state))
    (multiple-value-setq (items state) (apply-op items op state))))
