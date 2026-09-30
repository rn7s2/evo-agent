;;;; view-fallback.lisp — a stand-in session topic, for serve's own tests.
;;;;
;;;; The session topic's real provider is the view (`evo.view`, CONTRACT §7),
;;;; written in src/view/ by its own work package.  This file is *not* that
;;;; projection: it is the smallest thing that turns the kernel's events into
;;;; items and ops so that serve's protocol — snapshot, stream, paging, ops,
;;;; cancellation — can be exercised end to end before the view lands.  When
;;;; the view is loaded, INSTALL-SESSION-TOPIC uses it instead (view-topic.lisp)
;;;; and this file is dead weight to be deleted with the merge.
;;;;
;;;; What it does not do: journal folding (it sees events, not history), the
;;;; kinds the view will add (lane reports, goals, compactions, context), or
;;;; images.  What it does do is keep item ids stable, publish the same ops
;;;; the view will, and never block the session thread.

(in-package :evo.serve)

(defparameter *fallback-max-items* 1000
  "Items the fallback keeps.  The real view keeps the whole root→leaf path;
this is a test double, and a bounded one.")

(defstruct (fallback-topic (:constructor %make-fallback-topic))
  server
  (lock (bt:make-lock "serve-view"))
  ;; Items oldest first.  The same plist object is in ITEMS and in INDEX, and
  ;; it is changed in place, so the two can never disagree about an item.
  (items nil)
  (index (make-hash-table :test #'equal))
  ;; The untruncated text of an item, which /items/<id> serves while the
  ;; snapshot carries the bounded one: id -> (:thinking … :result-text …).
  (full (make-hash-table :test #'equal))
  (streaming nil))     ; the assistant item being built, or NIL

(defun make-fallback-topic (server agent)
  (declare (ignore agent))
  (%make-fallback-topic :server server))

(defun fallback-item-id (prefix)
  (format nil "~a_~a" prefix (gen-id 4)))

(defun fallback-patch (item patch)
  "Set PATCH's keys on ITEM in place, so every holder of the item sees it."
  (loop for (k v) on patch by #'cddr do (setf (getf item k) v))
  item)

(defun fallback-trim (topic)
  "Drop the oldest items beyond the cap."
  (loop while (> (length (fallback-topic-items topic)) *fallback-max-items*)
        for item = (pop (fallback-topic-items topic))
        do (remhash (getf item :id) (fallback-topic-index topic))
           (remhash (getf item :id) (fallback-topic-full topic))))

(defun fallback-add (topic item)
  "Add ITEM (a plist with a new :id) at the end and publish item.add."
  (let ((previous (car (last (fallback-topic-items topic)))))
    (setf (getf item :ts) (op-now-ms))
    (setf (fallback-topic-items topic)
          (append (fallback-topic-items topic) (list item)))
    (setf (gethash (getf item :id) (fallback-topic-index topic)) item)
    (setf (gethash (getf item :id) (fallback-topic-full topic))
          (list :thinking (getf item :thinking)
                :result-text (getf (getf item :result) :text)))
    (fallback-trim topic)
    (publish-op (fallback-topic-server topic)
                (list :op "item.add" :topic "session" :item item
                      :after (and previous (getf previous :id))))
    item))

(defun fallback-update (topic id fn)
  "Change the item ID with FN, a mutator of its plist.  Lock held."
  (let ((item (gethash id (fallback-topic-index topic))))
    (when item
      (funcall fn item)
      ;; The untruncated text is kept beside the item, so /items/<id> can
      ;; serve what the snapshot bounded.
      (let ((full (gethash id (fallback-topic-full topic))))
        (when full
          (when (getf item :thinking) (setf (getf full :thinking) (getf item :thinking)))
          (when (getf (getf item :result) :text)
            (setf (getf full :result-text) (getf (getf item :result) :text)))))))
  nil)

;;; The events.

(defun fallback-state (topic)
  "The session's derived state: the command layer's session summary, which is
what every frontend shows about a session."
  (let* ((server (fallback-topic-server topic))
         (agent (server-agent server)))
    (handler-case (evo.command:session-summary agent)
      (error () nil))))

(defun fallback-publish-state (topic)
  (publish-state-patch (fallback-topic-server topic) "session" (fallback-state topic)))

(defun fallback-on-event (topic event)
  "One kernel event → items and ops.  Returns T: this provider consumes
everything, if only by ignoring it."
  (bt:with-lock-held ((fallback-topic-lock topic))
    (case (getf event :type)
      (:message-start
       (setf (fallback-topic-streaming topic)
             (fallback-add topic (list :id (fallback-item-id "e")
                                       :kind :assistant :text "" :thinking ""
                                       :status :streaming
                                       :model (getf event :model)
                                       :provider (getf event :provider)))))
      ((:text-delta :thinking-delta)
       (fallback-text-delta topic (getf event :type) (or (getf event :text) "")))
      (:message-end
       (fallback-message-end topic event))
      (:tool-call-start
       (fallback-tool-call topic event))
      (:tool-result
       (fallback-tool-result topic event))
      (:steering
       (fallback-user-item topic (getf event :text) :status :sent))
      (:notice
       (fallback-add topic (list :id (fallback-item-id "n") :kind :notice
                                 :severity (or (getf event :severity) :info)
                                 :text (getf event :text)
                                 :source (or (getf event :source) :serve)
                                 :durable (and (getf event :durable) t))))
      ;; The task clock and the run outcome are serve's own state (it owns the
      ;; task), so the fallback only has to republish the derived state.
      ((:task-start :task-end :run-end)
       (fallback-publish-state topic)))
    t))

(defun fallback-text-delta (topic type text)
  "Text that grew: the item takes it, and so does the op log (where a run of
them is coalesced into one op)."
  (let* ((field (if (eq type :text-delta) :text :thinking))
         (item (fallback-topic-streaming topic))
         (id (and item (getf item :id))))
    (when id
      (fallback-update topic id
                       (lambda (i)
                         (setf (getf i field)
                               (concatenate 'string (or (getf i field) "") text))))
      (publish-op (fallback-topic-server topic)
                  (list :op "item.append" :topic "session" :id id
                        :field field :text text)))))

(defun fallback-message-end (topic event)
  "The streaming answer is done: its status, its usage, and the state the
client draws beside it."
  (let* ((item (fallback-topic-streaming topic))
         (id (and item (getf item :id)))
         (reason (getf event :stop-reason))
         (error (getf event :error)))
    (setf (fallback-topic-streaming topic) nil)
    (when id
      (let ((patch (list :status (cond (error :error)
                                       ((eq reason :aborted) :aborted)
                                       ((eq reason :length) :length)
                                       (t :final))
                         :error error
                         :usage (and (getf event :usage)
                                     (fallback-usage (getf event :usage))))))
        (fallback-update topic id (lambda (i) (fallback-patch i patch)))
        (publish-op (fallback-topic-server topic)
                    (list :op "item.patch" :topic "session" :id id :patch patch)))
      (fallback-publish-state topic))))

(defun fallback-tool-call (topic event)
  (fallback-add topic (list :id (format nil "t_~a" (or (getf event :id) (gen-id 4)))
                            :kind :tool
                            :call-id (getf event :id)
                            :name (getf event :name)
                            :args (getf event :arguments)
                            :status :running
                            :parent (let ((answer (fallback-topic-streaming topic)))
                                      (and answer (getf answer :id))))))

(defun fallback-tool-result (topic event)
  (let* ((content (or (getf event :content) ""))
         (id (format nil "t_~a" (or (getf event :id) (gen-id 4))))
         (patch (list :status (if (getf event :is-error) :error :ok)
                      :result (list :text (truncate-string content 4096)
                                    :chars (or (getf event :content-chars)
                                               (length content))
                                    :truncated (> (length content) 4096)))))
    (fallback-update topic id (lambda (i) (fallback-patch i patch)))
    (publish-op (fallback-topic-server topic)
                (list :op "item.patch" :topic "session" :id id :patch patch))))

(defun fallback-usage (usage)
  "The provider's usage plist as the item's: input and output tokens."
  (list :input (getf usage :input-tokens)
        :output (getf usage :output-tokens)
        :cache-read (getf usage :cache-read-input-tokens)
        :cache-write (getf usage :cache-creation-input-tokens)))

(defun fallback-user-item (topic text &key status queue id)
  "The user's turn as an item: QUEUED when it was just accepted, SENT when the
run drained it."
  (bt:with-lock-held ((fallback-topic-lock topic))
    (let ((existing (when id (gethash id (fallback-topic-index topic)))))
      (cond
        (existing
         (fallback-update topic id (lambda (i) (setf (getf i :status) status)))
         existing)
        (t
         (fallback-add topic (list :id (or id (fallback-item-id "e")) :kind :user
                                   :text text :images #() :status status
                                   :queue (or queue :now))))))))

(defun fallback-queued-input (topic id text images queue)
  "A queued user turn: an item exists before anything runs, so the client can
draw it and cancel it (input.cancel)."
  (declare (ignore images))
  (bt:with-lock-held ((fallback-topic-lock topic))
    (fallback-add topic (list :id id :kind :user :text text :images #()
                              :status :queued :queue queue))))

;;; The provider protocol.

(defun fallback-wire-item (topic item)
  "ITEM as a snapshot or op carries it: the text is bounded (CONTRACT §4.1),
while /items/<id> serves it whole."
  (let ((item (copy-list item))
        (full (gethash (getf item :id) (fallback-topic-full topic))))
    (when (and full (getf full :thinking)
               (> (length (getf full :thinking)) 16384))
      (setf (getf item :thinking) (truncate-string (getf full :thinking) 16384)
            (getf item :thinking-truncated) t))
    (when (and full (getf full :result-text)
               (> (length (getf full :result-text)) 4096))
      (setf (getf item :result)
            (append (getf item :result)
                    (list :text (truncate-string (getf full :result-text) 4096)
                          :truncated t))))
    item))

(defmethod topic-snapshot ((topic fallback-topic) &key (items 200))
  (bt:with-lock-held ((fallback-topic-lock topic))
    (let* ((all (fallback-topic-items topic))
           (recent (last all (min items *fallback-max-items*))))
      (list :state (fallback-state topic)
            :items (coerce (mapcar (lambda (item) (fallback-wire-item topic item))
                                   recent)
                           'vector)
            :has-more (> (length all) (length recent))))))

(defmethod topic-items-before ((topic fallback-topic) before-id limit)
  (bt:with-lock-held ((fallback-topic-lock topic))
    (let* ((all (fallback-topic-items topic))
           (cut (if before-id
                    (position before-id all :key (lambda (i) (getf i :id))
                                        :test #'equal)
                    (length all)))
           (end (or cut (length all)))
           (start (max 0 (- end (max 1 limit)))))
      (values (coerce (mapcar (lambda (item) (fallback-wire-item topic item))
                              (subseq all start end))
                      'vector)
              (> start 0)))))

(defmethod topic-item ((topic fallback-topic) id)
  (bt:with-lock-held ((fallback-topic-lock topic))
    (let ((item (gethash id (fallback-topic-index topic)))
          (full (gethash id (fallback-topic-full topic))))
      (when item
        (let ((copy (copy-list item)))
          (when (getf full :thinking) (setf (getf copy :thinking) (getf full :thinking)))
          (when (getf full :result-text)
            (setf (getf copy :result)
                  (append (getf copy :result)
                          (list :text (getf full :result-text)
                                :truncated nil))))
          copy)))))

(defmethod topic-media ((topic fallback-topic) id n)
  (declare (ignore topic id n))
  (values nil nil))

(defmethod topic-feed-event ((topic fallback-topic) event)
  (fallback-on-event topic event))

(defmethod topic-feed-append ((topic fallback-topic) entry)
  (declare (ignore topic entry))
  nil)                                 ; the fallback lives on events alone

(defmethod topic-provider-reset ((topic fallback-topic) reason)
  "The fallback has no history to rebuild from: the items go, and the client's
re-snapshot of the topic is empty until the next events arrive."
  (declare (ignore reason))
  (bt:with-lock-held ((fallback-topic-lock topic))
    (setf (fallback-topic-items topic) nil
          (fallback-topic-index topic) (make-hash-table :test #'equal)
          (fallback-topic-full topic) (make-hash-table :test #'equal)
          (fallback-topic-streaming topic) nil)))

(defmethod topic-provider-queued-input ((topic fallback-topic) id text images queue)
  (fallback-queued-input topic id text images queue))

(defmethod topic-provider-input-cancelled ((topic fallback-topic) id)
  (bt:with-lock-held ((fallback-topic-lock topic))
    (fallback-update topic id (lambda (i) (fallback-patch i (list :status :cancelled))))))
