;;;; view-topic.lisp — the session topic's provider: the view, wired to serve.
;;;;
;;;; CONTRACT §7 is the interface.  `evo.view:make-view` projects the agent's
;;;; journal into items and state; `view-attach` hands it a publisher (every
;;;; change becomes an op, from inside the view's own lock, so op order is
;;;; mutation order); `view-on-event` and `view-on-append` keep it current; and
;;;; `view-snapshot` and friends answer the reads the protocol needs.  This
;;;; file is that wiring and nothing else — the projection lives in src/view/.

(in-package :evo.serve)

(defstruct (view-topic (:constructor %make-view-topic))
  server
  view
  ;; The journal the view is following, and the listener that follows it.  A
  ;; session switch points the agent at another journal; a listener left on the
  ;; old one would feed a view of a session nobody is looking at.
  journal
  listener
  leaf)

(defun make-view-topic (server agent)
  "The session topic: the view of AGENT, publishing into SERVER's op log."
  (let ((topic (%make-view-topic :server server
                                 :view (evo.view:make-view agent :topic "session"))))
    (evo.view:view-attach (view-topic-view topic)
                          (lambda (op-plist) (publish-op server op-plist)))
    (follow-journal topic)
    topic))

(defun follow-journal (topic)
  "Listen to the agent's current journal."
  (let* ((agent (server-agent (view-topic-server topic)))
         (journal (agent-journal agent)))
    (unless (eq journal (view-topic-journal topic))
      (when (view-topic-journal topic)
        (remove-journal-listener (view-topic-journal topic)
                                 (view-topic-listener topic)))
      (let ((view (view-topic-view topic)))
        (setf (view-topic-listener topic)
              (lambda (journal entry)
                (evo.view:view-on-append view entry)
                ;; A normal append advances the leaf and has already updated the
                ;; live view above.  Remember it here so a later host refresh does
                ;; not mistake that append for an out-of-band rewind and rebuild
                ;; away an assistant message that is still streaming.
                (setf (view-topic-leaf topic) (journal-leaf-id journal))))
        (add-journal-listener journal (view-topic-listener topic))))
    (setf (view-topic-journal topic) journal
          (view-topic-leaf topic) (journal-leaf-id journal))
    topic))

(defun sync-view-topic (topic)
  "The fold may have moved under the view in a way no append reports: the
journal switched (a listener on the old one), or the leaf moved (the whole
item list changed, and nothing was appended).  Both are a rebuild, and the
client is told to re-read the topic."
  (let* ((agent (server-agent (view-topic-server topic)))
         (journal (agent-journal agent)))
    (cond
      ((not (eq journal (view-topic-journal topic)))
       (follow-journal topic)
       (evo.view:view-reset (view-topic-view topic) :session-switched))
      ((not (equal (journal-leaf-id journal) (view-topic-leaf topic)))
       (let ((current (journal-leaf-id journal))
             (seen (view-topic-leaf topic)))
         (if (or (null seen)
                 (find seen (entry-path journal current)
                       :key (lambda (entry) (pget entry :id)) :test #'equal))
             ;; The journal publishes its new leaf before notifying listeners.
             ;; A refresh can race that notification; this is still an append,
             ;; not a rewind, and rebuilding would delete the streaming item.
             (progn
               (setf (view-topic-leaf topic) current)
               (evo.view:view-refresh (view-topic-view topic)))
             (progn
               (setf (view-topic-leaf topic) current)
               (evo.view:view-reset (view-topic-view topic) :leaf-moved)))))
      ;; Anything else the fold changed (a setting, a model, the provider
      ;; registry) leaves the items alone but can move the state: re-derive it.
      (t (evo.view:view-refresh (view-topic-view topic)))))
  t)

(defmethod topic-snapshot ((topic view-topic) &key (items 200))
  (evo.view:view-snapshot (view-topic-view topic) :items items))

(defmethod topic-items-before ((topic view-topic) before-id limit)
  (evo.view:view-items-before (view-topic-view topic) before-id limit))

(defmethod topic-item ((topic view-topic) id)
  (evo.view:view-item (view-topic-view topic) id))

(defmethod topic-media ((topic view-topic) id n)
  (evo.view:view-media (view-topic-view topic) id n))

(defmethod topic-feed-event ((topic view-topic) event)
  (evo.view:view-on-event (view-topic-view topic) event)
  t)

(defmethod topic-feed-append ((topic view-topic) entry)
  (evo.view:view-on-append (view-topic-view topic) entry)
  t)

(defmethod topic-provider-reset ((topic view-topic) reason)
  "Rebuild from the journal as it stands now (a leaf move, a lane restart)."
  (follow-journal topic)
  (evo.view:view-reset (view-topic-view topic) reason))

(defmethod topic-provider-queued-input ((topic view-topic) id text images queue)
  "Input a client queued and can still cancel: it is in the transcript before
it is journaled, which is what makes the row drawable at once."
  (evo.view:view-input-queued (view-topic-view topic) :id id :text text
                                                      :images images :queue queue))

(defmethod topic-provider-input-cancelled ((topic view-topic) id)
  "The queued input was withdrawn: it leaves the transcript (and the view
publishes that removal itself)."
  (evo.view:view-input-cancelled (view-topic-view topic) id))

(defmethod topic-provider-sync ((topic view-topic))
  (sync-view-topic topic))

(defun install-session-topic (server agent)
  "Install the provider for the `session` topic (CONTRACT §7)."
  (register-topic server "session" (make-view-topic server agent)))
