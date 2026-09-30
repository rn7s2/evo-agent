;;;; oplog.lisp — the session's op log: protocol changes, numbered, once.
;;;;
;;;; Where the old event log carried *events* (task-start, text-delta, …) and
;;;; left every client to rebuild the meaning, this log carries *ops*: the
;;;; changes a client applies to the state it already has (CONTRACT §5.3).
;;;;
;;;;   item.add / item.append / item.patch / item.remove   one transcript item
;;;;   state.patch                                          a topic's state
;;;;   topic.reset / stream.reset                           re-snapshot requests
;;;;
;;;; Producers are the topic providers (the view, a lane mirror) through
;;;; PUBLISH-OP; consumers are the SSE writers, each reading from its own
;;;; cursor.  An op is encoded to JSON once, when it is published, on the
;;;; thread that owns the values in it.
;;;;
;;;; A cursor is "<epoch>.<seq>": the epoch is minted once per process, so a
;;;; cursor from another epoch is a restart, whichever pid is or is not alive
;;;; (CONTRACT §5.3).  Seq is one monotonic counter, and it is what makes the
;;;; log a log: a snapshot reports the seq it was taken at, and a client that
;;;; reconnects replays from there.
;;;;
;;;; Two knobs keep the log small without losing anything a client needs:
;;;; item.append ops for one item are coalesced over at most 50 ms (a 2000-op
;;;; answer becomes a handful), and retention is by time — the last ten
;;;; minutes, and never more than 50 000 ops.  A client that falls out of
;;;; retention is told to re-snapshot, which is cheap.
;;;;
;;;; Waiting is by condition variable, on both sides: an SSE writer blocks
;;;; until an op arrives (no 50 ms poll), and the session thread blocks until
;;;; there is a message (no 20 ms poll).

(in-package :evo.serve)

(defparameter *op-retention-seconds* 600
  "Ops older than this are dropped: a cursor into them gets stream.reset
rather than a hole.")

(defparameter *op-retention-count* 50000
  "Hard cap on retained ops, whatever their age.  Each is one encoded JSON
object, so this bounds memory.")

(defparameter *append-coalesce-seconds* 1/20
  "item.append ops for one item within this window are merged into one op.
Short enough that a person cannot see the delay, long enough that a streaming
answer costs tens of ops rather than thousands.")

(defstruct (op-log (:constructor %make-op-log))
  (lock (bt:make-lock "serve-ops"))
  ;; Streams wait on CV for ops to read.  The flusher waits on FLUSHER-CV for a
  ;; coalescing deadline to arrive: a condition variable of its own, because
  ;; bordeaux-threads has no broadcast, and a shutdown notify aimed at "the
  ;; waiter" would otherwise wake a stream and leave the flusher parked — which
  ;; is exactly how a server that has served stops shutting down.
  (cv (bt:make-condition-variable :name "serve-ops"))
  (flusher-cv (bt:make-condition-variable :name "serve-flush"))
  (stopping nil)
  epoch
  (ring (make-array *op-retention-count* :initial-element nil))
  (last-seq 0)
  ;; The seq of the oldest op still retained.  > LAST-SEQ means the log holds
  ;; nothing (either it never had an op, or every one has been purged).
  (oldest-seq 1)
  ;; Buffered item.append ops, the coalescing window still open:
  ;; (:topic :id :field :text :deadline).
  (pending nil))

(defun make-op-log ()
  (%make-op-log :epoch (format nil "~(~{~2,'0x~}~)"
                               (coerce (evo.port:random-octets 4) 'list))))

(defconstant +unix-epoch-offset+ 2208988800
  "Seconds between 1900-01-01 (universal time) and 1970-01-01 (the Unix
epoch), which is the epoch the protocol's timestamps count from.")

(defun op-now-ms ()
  "Wall-clock milliseconds since the Unix epoch — what op timestamps, item
times and task clocks carry (CONTRACT §4)."
  (let ((units internal-time-units-per-second))
    (+ (* 1000 (- (get-universal-time) +unix-epoch-offset+))
       (floor (* 1000 (mod (get-internal-real-time) units)) units))))

(defun format-cursor (log &optional (seq (op-log-last-seq log)))
  "The cursor naming SEQ in LOG: \"7f3a91c2.1042\"."
  (format nil "~a.~d" (op-log-epoch log) seq))

(defun parse-cursor (text)
  "TEXT (\"<epoch>.<seq>\") as (values EPOCH SEQ), or NIL when it is not a
cursor at all — an unknown cursor, which a stream answers with
stream.reset{cursor_unknown}, never with a silent wait."
  (when (and (stringp text) (plusp (length text)))
    (let ((dot (position #\. text)))
      (when (and dot (plusp dot) (< dot (1- (length text))))
        (let ((seq (ignore-errors (parse-integer text :start (1+ dot)))))
          (when (and seq (not (minusp seq)))
            (values (subseq text 0 dot) seq)))))))

;;; Appending.

(defun op-log-encode (op)
  "OP — a plist with :op, :topic, :seq, :ts — as JSON text.  An op holding a
value outside the vocabulary must not wedge the log: it goes out as an
\"unprintable\" op naming the original, which is what a client can do nothing
with but skip."
  (handler-case (encode-json op)
    (error ()
      (encode-json (list :op "unprintable" :seq (getf op :seq)
                         :ts (getf op :ts) :topic (getf op :topic)
                         :original (let ((name (getf op :op)))
                                     (if (stringp name) name "?")))))))

(defun op-log-append (log op)
  "Store OP (a plist without :seq/:ts) under a fresh seq.  Lock held.
Returns the seq."
  (let* ((now (op-now-ms))
         (seq (incf (op-log-last-seq log)))
         (ring (op-log-ring log))
         (topic (getf op :topic))
         (entry (list seq now (op-log-encode (append (list :seq seq :ts now) op)) topic)))
    (setf (aref ring (mod seq (length ring))) entry)
    (op-log-purge log now)
    seq))

(defun op-log-purge (log now-ms)
  "Drop the ops retention no longer covers.  Lock held."
  (let ((ring (op-log-ring log))
        (capacity (length (op-log-ring log)))
        (cutoff (- now-ms (* 1000 *op-retention-seconds*))))
    (loop while (and (<= (op-log-oldest-seq log) (op-log-last-seq log))
                     (let* ((seq (op-log-oldest-seq log))
                            (entry (aref ring (mod seq capacity))))
                       (or (null entry)                    ; overwritten
                           (>= (- (op-log-last-seq log) seq) capacity)
                           (< (second entry) cutoff))))
          do (setf (aref ring (mod (op-log-oldest-seq log) capacity)) nil)
             (incf (op-log-oldest-seq log)))))

(defun op-log-flush-item (log topic id)
  "Append the buffered appends for one item, oldest first, so they keep their
order relative to whatever op is about to be published for it.  Lock held."
  (dolist (entry (op-log-pending log))
    (when (and (equal topic (getf entry :topic)) (equal id (getf entry :id)))
      (op-log-append log (list :op "item.append" :topic topic :id id
                               :field (getf entry :field) :text (getf entry :text)))))
  (setf (op-log-pending log)
        (remove-if (lambda (entry)
                     (and (equal topic (getf entry :topic))
                          (equal id (getf entry :id))))
                   (op-log-pending log))))

(defun op-log-flush-expired (log &optional force)
  "Append every buffered append whose coalescing window has closed (or all of
them, when FORCE).  Lock held.  Returns T when anything was appended."
  (let ((now (get-internal-real-time))
        (appended nil)
        (kept nil))
    (dolist (entry (op-log-pending log))
      (if (or force (> now (getf entry :deadline)))
          (progn
            (setf appended t)
            (op-log-append log (list :op "item.append" :topic (getf entry :topic)
                                     :id (getf entry :id)
                                     :field (getf entry :field)
                                     :text (getf entry :text))))
          (push entry kept)))
    (setf (op-log-pending log) (nreverse kept))
    appended))

(defun op-log-buffer-append (log op)
  "Merge OP into the buffered appends for the same item, or start a fresh
window.  Lock held."
  (let* ((topic (getf op :topic))
         (id (getf op :id))
         (field (getf op :field))
         (text (getf op :text))
         (now (get-internal-real-time))
         (window (* *append-coalesce-seconds* internal-time-units-per-second))
         (entry (find-if (lambda (e) (and (equal topic (getf e :topic))
                                          (equal id (getf e :id))
                                          (equal field (getf e :field))))
                         (op-log-pending log))))
    (cond
      ;; The window is still open: the text rides along with the op already
      ;; scheduled.
      ((and entry (<= now (getf entry :deadline)))
       (setf (getf entry :text) (concatenate 'string (getf entry :text) text)))
      (t
       ;; Stale window: that op goes out now and this text opens the next one.
       (when entry (op-log-flush-item log topic id))
       (setf (op-log-pending log)
             (append (op-log-pending log)
                     (list (list :topic topic :id id :field field :text text
                                 :deadline (+ now window)))))))))

(defun op-log-publish (log op)
  "Append OP — (:op \"item.append\" :topic \"session\" …) — to LOG.  Returns
the seq the op was given (for a coalesced append, the seq of the last op
actually written; the text is visible to a snapshot immediately either way).
Any thread."
  (bt:with-lock-held ((op-log-lock log))
    (let ((name (getf op :op)))
      (if (and (equal name "item.append") (getf op :id) (stringp (getf op :text)))
          (op-log-buffer-append log op)
          ;; Anything else for an item must not overtake its text.
          (when (getf op :id)
            (op-log-flush-item log (getf op :topic) (getf op :id))))
      (let ((buffered (and (equal name "item.append") (getf op :id)
                           (stringp (getf op :text)))))
        (let ((seq (if buffered
                       (op-log-last-seq log)
                       (op-log-append log op))))
          ;; A buffered append is not in the log yet: the flusher is the one to
          ;; wake (its deadline just moved).  A written op is what streams wait
          ;; for.
          (if buffered
              (bt:condition-notify (op-log-flusher-cv log))
              (bt:condition-notify (op-log-cv log)))
          seq)))))

(defun publish-op (server op-plist)
  "Publish OP-PLIST (:op, :topic, and the op's own fields) on SERVER's log.
Every client subscribed to :topic sees it; :seq and :ts are assigned here.
Returns the seq."
  (op-log-publish (server-oplog server) op-plist))

(defun op-log-flush-all (log)
  "Write out every buffered append.  Lock held.  Used by a snapshot, which
must not serve text that no op accounts for."
  (op-log-flush-expired log t))

;;; Reading.

(defun op-log-cursor (log)
  "Flush buffered appends, then report where the log stands: (values EPOCH
SEQ).  Taking a snapshot uses this, so every op up to SEQ is in the log and
none after it — the text still buffered counts as part of the snapshot."
  (bt:with-lock-held ((op-log-lock log))
    (op-log-flush-all log)
    (bt:condition-notify (op-log-cv log))
    (bt:condition-notify (op-log-flusher-cv log))
    (values (op-log-epoch log) (op-log-last-seq log))))

(defun op-log-ops-after (log since)
  "Ops with seq > SINCE, as ((seq json) …), oldest first.  Second value is the
first seq missed when SINCE fell out of retention (NIL when nothing was);
third is T when SINCE is beyond the end of the log."
  (bt:with-lock-held ((op-log-lock log))
    (%op-log-ops-after log since)))

(defun %op-log-ops-after (log since)
  "As OP-LOG-OPS-AFTER, with the lock already held."
  (let* ((ring (op-log-ring log))
         (last (op-log-last-seq log))
         (oldest (op-log-oldest-seq log)))
    (values (loop for seq from (max (1+ since) oldest) to last
                  for entry = (aref ring (mod seq (length ring)))
                  when entry collect (list (first entry) (third entry) (fourth entry)))
            (and (< since (1- oldest)) (<= since last) (1+ since))
            (> since last))))

(defun op-log-wait (log since timeout)
  "Block until LOG holds an op after SINCE, or TIMEOUT seconds pass, or the
log's own condition is signalled.  Returns the ops exactly as
OP-LOG-OPS-AFTER does — possibly none, which is the caller's cue to ping."
  (bt:with-lock-held ((op-log-lock log))
    (let ((oldest (op-log-oldest-seq log))
          (last (op-log-last-seq log)))
      (cond
        ;; Something to send right now.
        ((and (< since last) (>= (1+ since) oldest))
         (%op-log-ops-after log since))
        ;; The cursor is history: waiting cannot help, so answer the caller
        ;; at once and let it reset.
        ((and (< since (1- oldest)) (<= since last))
         (%op-log-ops-after log since))
        (t
         ;; Nothing to send: block until an op is published, the server is
         ;; shutting down, or TIMEOUT passes (the caller's cue to ping, which
         ;; is also how a vanished peer is noticed).
         (when (op-log-stopping log)
           ;; Shutting down: answer now, so a stream can write what it has and
           ;; close instead of sleeping through its own death.
           (return-from op-log-wait (%op-log-ops-after log since)))
         (bt:condition-wait (op-log-cv log) (op-log-lock log) :timeout timeout)
         (%op-log-ops-after log since))))))

(defun op-log-wake (log)
  "Shutdown: nothing more will be published, and every waiter must look at
STOPPING-P rather than at the log.  bordeaux-threads has no broadcast, so a
stream that is asleep may still be waiting for its next ping — the socket
closing under it is what ends it — but the flusher has a condition variable
nobody else waits on, so the notify that ends its shift always reaches it."
  (bt:with-lock-held ((op-log-lock log))
    (setf (op-log-stopping log) t)
    (bt:condition-notify (op-log-cv log))
    (bt:condition-notify (op-log-flusher-cv log))))

(defun op-log-wait-seconds (log)
  "Seconds until the earliest buffered append must go out, for the flusher to
wait on — or NIL when nothing is buffered, which means it waits for a publish
rather than waking to find nothing."
  (let ((now (get-internal-real-time)))
    (when (op-log-pending log)
      (max 0.0
           (/ (loop for entry in (op-log-pending log)
                    minimize (- (getf entry :deadline) now))
              (float internal-time-units-per-second 1.0d0))))))
