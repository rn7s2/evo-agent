;;;; topics.lisp — topic providers: the things a client subscribes to.
;;;;
;;;; A topic is a name ("session", "swarm", "lane:1"), a state, and a list of
;;;; items.  Serve knows the protocol — snapshot, ops, paging, media — and
;;;; nothing about what a topic means: the provider does (CONTRACT §7).  The
;;;; session's own provider is the view (evo.view); evo-swarm registers the
;;;; swarm and lane topics without serve knowing they exist.
;;;;
;;;; Only two names are special anywhere in this file: "session" (the one
;;;; topic serve itself installs and owns the task clock of) and the "lane:*"
;;;; wildcard, which a request may use to mean every registered lane.

(in-package :evo.serve)

;;; The provider protocol.  A provider is any object; these are the four
;;; methods it must answer (the defaults answer "nothing there", so a
;;; half-built provider degrades instead of failing).

(defgeneric topic-snapshot (provider &key items)
  (:documentation "PROVIDER's state and newest ITEMS items, taken together:
(:state PLIST :items #(…) :has-more BOOL).  Thread-safe; the op log's lock is
held across the call, so it must not call back into the server.")
  (:method ((provider t) &key items)
    (declare (ignore items))
    (list :state nil :items #() :has-more nil)))

(defgeneric topic-items-before (provider before-id limit)
  (:documentation "Up to LIMIT items older than BEFORE-ID (a string; NIL for
the newest): (values ITEMS HAS-MORE).")
  (:method ((provider t) before-id limit)
    (declare (ignore before-id limit))
    (values #() nil)))

(defgeneric topic-item (provider id)
  (:documentation "One item, whole — no truncation, whatever the snapshot did
to it — or NIL when the topic does not have it.")
  (:method ((provider t) id)
    (declare (ignore id))
    nil))

(defgeneric topic-media (provider id n)
  (:documentation "The octets of image number N of item ID, with its media
type: (values OCTETS MEDIA-TYPE), or NIL.")
  (:method ((provider t) id n)
    (declare (ignore id n))
    nil))

;;; How serve feeds a provider.  These are not part of the client protocol:
;;; they are how the kernel's events and the server's own clock reach the view.

(defgeneric topic-feed-event (provider event)
  (:documentation "A kernel (or serve-synthesised) event.  Returns T when the
provider turned it into protocol changes; a provider that rebuilds itself from
journal appends alone has no use for events, so serve publishes its own
notices for it.")
  (:method ((provider t) event)
    (declare (ignore event))
    nil))

(defgeneric topic-feed-append (provider entry)
  (:documentation "A journal entry, after it was appended.")
  (:method ((provider t) entry)
    (declare (ignore entry))
    nil))

(defgeneric topic-provider-reset (provider reason)
  (:documentation "The journal switched, the leaf moved, or the process this
topic mirrors restarted: rebuild from the current journal and republish.")
  (:method ((provider t) reason)
    (declare (ignore reason))
    nil))

(defgeneric topic-provider-queued-input (provider id text images queue)
  (:documentation "A user turn was queued with a pre-minted item ID, before
anything ran.  A provider that mints its own IDs has nothing to do.")
  (:method ((provider t) id text images queue)
    (declare (ignore id text images queue))
    nil))

(defgeneric topic-provider-sync (provider)
  (:documentation "The fold may have moved under the provider in a way no
append reports (a journal switch, a leaf move): re-derive what it must, or do
nothing when appends already said everything.")
  (:method ((provider t)) nil))

(defgeneric topic-provider-input-cancelled (provider id)
  (:documentation "A queued user item was cancelled: mark it, so the next
snapshot agrees with the item.patch op serve published.")
  (:method ((provider t) id)
    (declare (ignore id))
    nil))

;;; The registry.

(defun register-topic (server name provider)
  "Publish PROVIDER as the topic NAME on SERVER.  Any thread; re-registering a
name replaces it (a swarm re-registers a lane after its restart)."
  (bt:with-lock-held ((server-topics-lock server))
    (setf (gethash name (server-topics server)) provider))
  provider)

(defun unregister-topic (server name)
  (bt:with-lock-held ((server-topics-lock server))
    (remhash name (server-topics server))))

(defun topic-provider (server name)
  (bt:with-lock-held ((server-topics-lock server))
    (gethash name (server-topics server))))

(defun topic-provider-names (server)
  (bt:with-lock-held ((server-topics-lock server))
    (sort (loop for name being the hash-keys of (server-topics server) collect name)
          #'string<)))

(defun wildcard-topic-p (name)
  (and (stringp name) (search "*" name)))

(defun topic-name-matches-p (pattern name)
  "PATTERN with at most one \"*\" matches NAME (\"lane:*\" matches
\"lane:12\")."
  (let ((star (position #\* pattern)))
    (if (null star)
        (string= pattern name)
        (let ((offset (- (length name) (- (length pattern) (1+ star)))))
          (and (>= (length name) (1- (length pattern)))
               (string= pattern name :start1 0 :end1 star :start2 0 :end2 star)
               (>= offset 0)
               (string= pattern name :start1 (1+ star) :end1 (length pattern)
                                    :start2 offset :end2 (length name)))))))

(defun expand-topic-names (server spec)
  "SPEC — \"session,swarm,lane:*\", or a list of the same — as the concrete
registered topic names in SERVER, in request order and without duplicates.  A
wildcard expands to every match; an unknown exact name is dropped."
  (let ((parts (if (listp spec)
                   spec
                   (uiop:split-string (or spec "") :separator ",")))
        (seen nil) (out nil))
    (dolist (raw parts)
      (let ((name (string-trim '(#\Space #\Tab) raw)))
        (unless (zerop (length name))
          (dolist (candidate (if (wildcard-topic-p name)
                                 (remove-if-not (lambda (n) (topic-name-matches-p name n))
                                                (topic-provider-names server))
                                 (let ((provider (topic-provider server name)))
                                   (and provider (list name)))))
            (unless (member candidate seen :test #'equal)
              (push candidate seen)
              (push candidate out))))))
    (nreverse out)))

;;; Publishing.

(defun topic-reset (server topic reason)
  "The topic's meaning changed wholesale (its journal switched, its leaf
moved, the process it mirrors restarted): every client re-snapshots it.  This
is not an append and carries no item."
  (let ((provider (topic-provider server topic)))
    (when provider (topic-provider-reset provider reason)))
  (publish-op server (list :op "topic.reset" :topic topic :reason (string-downcase reason))))

(defun publish-state-patch (server topic patch)
  "A merge patch on TOPIC's state (arrays replaced whole)."
  (publish-op server (list :op "state.patch" :topic topic :patch patch)))

(defun publish-item-add (server topic item &key after)
  (publish-op server (list :op "item.add" :topic topic :item item :after after)))

(defun publish-notice-op (server topic text &key (severity :info) (source :serve) durable data)
  "A notice as an item.add, for providers that have no view of their own to
put it in."
  (publish-item-add server topic
                    (list :id (format nil "n_~a" (gen-id 4))
                          :kind :notice :ts (op-now-ms)
                          :severity severity :text text :source source
                          :durable (and durable t) :data data)))

(defun topic-notice (server topic text &key (severity :info) (source :serve)
                                            durable data)
  "Tell TOPIC's provider that TEXT happened.  The provider turns it into an
item (the view does); a provider that ignores events gets an item.add built
here, so a notice can never be lost over the wire."
  (let ((provider (topic-provider server topic)))
    (when (or (null provider)
              (not (topic-feed-event provider
                                     (list :type :notice :severity severity :text text
                                           :source source :durable durable :data data))))
      (publish-notice-op server topic text :severity severity :source source
                                              :durable durable :data data))))

(defun topic-name-for-notice (server)
  "Where the server's own notices belong: the session topic when there is
one, else the swarm topic."
  (cond ((topic-provider server "session") "session")
        ((topic-provider server "swarm") "swarm")
        (t (first (topic-provider-names server)))))

(defun topic-on-event (server event)
  "Feed one kernel event to every provider that wants events."
  (when (eq (getf event :type) :steering)
    (forget-completed-input server (or (getf event :text) "")))
  (dolist (name (topic-provider-names server))
    (let ((provider (topic-provider server name)))
      (when provider
        (handler-case (topic-feed-event provider event)
          (serious-condition () nil))))))

(defun feed-append (server entry)
  "Feed one journal append to every provider (the journal's own append hook,
installed while a server serves)."
  (dolist (name (topic-provider-names server))
    (let ((provider (topic-provider server name)))
      (when provider
        (handler-case (topic-feed-append provider entry)
          (serious-condition () nil))))))

(defun log-user-input (server text)
  "A user turn arrived from the frontend's front door (the run-requested
message): the provider decides how to show it."
  (topic-notice server "session" text :severity :info :source :user))

;;; The session's own state — status, task, model, context, goal, queue — is
;;; the view's: it derives all of it from the journal and the events, and it
;;; publishes the fields that move (CONTRACT §4.2).  Serve adds nothing to it,
;;; which is what keeps one writer per key.

(defun sync-session-topic (server)
  "Ask the session topic to re-derive what appends cannot report (a journal
switch, a leaf move)."
  (let ((provider (topic-provider server "session")))
    (when provider (topic-provider-sync provider))))

;;; Snapshots.

(defparameter *snapshot-attempts* 8
  "How many times a snapshot retries to catch a moment when no op is being
published.  A snapshot takes microseconds and appends are coalesced over
50 ms, so the first attempt almost always lands; the cap is there so a busy
session cannot stall a reader for ever.")

(defun topics-snapshot (server names items)
  "Snapshot every topic in NAMES as of one seq: (values EPOCH SEQ
((NAME . SNAPSHOT) …)).

A client must be able to fold the ops after SEQ onto this state and land on
the state itself — no op applied twice, none missing.  Two things give it:
the buffered appends are written out first, so every byte the snapshot shows
has a seq; and the seq is read again after the providers have answered, so a
snapshot taken while an op was being published is taken again.  A snapshot
takes microseconds and appends are coalesced over 50 ms, so the first attempt
almost always lands.

The op log's lock is deliberately *not* held while a provider answers: a
provider publishes its ops from inside the critical section that changes its
state, so holding the log's lock here would deadlock two threads that take
the same two locks in opposite orders.  The seq re-read is what replaces it."
  (let ((log (server-oplog server)))
    (loop repeat *snapshot-attempts*
          do (multiple-value-bind (epoch seq) (op-log-cursor log)
               (let ((snaps (%collect-topic-snapshots server names items)))
                 (multiple-value-bind (epoch-now seq-now) (op-log-cursor log)
                   (when (and (equal epoch epoch-now) (= seq seq-now))
                     (return-from topics-snapshot (values epoch seq snaps)))))))
    (%topic-snapshot-last-attempt server names items)))

(defun %collect-topic-snapshots (server names items)
  (loop for name in names
        for provider = (topic-provider server name)
        collect (cons name (when provider (topic-snapshot provider :items items)))))

(defun %topic-snapshot-last-attempt (server names items)
  "The final attempt of TOPICS-SNAPSHOT, taken whether or not the session was
quiet: a reader that keeps losing the race gets a snapshot one op old rather
than none at all."
  (multiple-value-bind (epoch seq) (op-log-cursor (server-oplog server))
    (values epoch seq (%collect-topic-snapshots server names items))))

