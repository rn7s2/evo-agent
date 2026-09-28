;;;; api.lisp — the swarm's read-only HTTP API: what it answers, as data.
;;;;
;;;; Every lane's own `serve` stays authoritative for that lane: this is a
;;;; read-only window on the swarm, and the transcript and the event stream
;;;; are relays of the lane's own endpoints, never a second source of truth.
;;;; Nothing here ever carries a lane's bearer token or URL out.
;;;;
;;;; A `lane-state` event is how a machine watches the swarm: it goes through
;;;; SWARM-PUBLISH (view.lisp), so a served coordinator's /events carries it
;;;; and a TUI coordinator, which has no event stream, simply drops it.

(in-package :evo.swarm)

;;; Lane goal state, cached.

(defun note-lane-goal (lane goal)
  "Cache GOAL — the plist a lane's /state reports — remembering when."
  (with-swarm-lock ()
    (setf (lane-goal lane) goal
          (lane-goal-at lane) (get-universal-time))))

(defun note-lane-goal-status (lane status)
  "Note a goal STATUS string from one of the lane's events, without a /state
round trip: an event that names a goal is the source of truth for it too."
  (when status
    (with-swarm-lock ()
      (let ((goal (lane-goal lane)))
        (if goal
            (setf (getf goal :status) status)
            (setf (lane-goal lane) (list :status status)))))))

(defun cached-lane-goal-status (lane)
  (with-swarm-lock () (getf (lane-goal lane) :status)))

;;; The `lane-state` event: published on the coordinator's own event stream
;;; whenever a lane's state, task or goal status changes.

(defun lane-state-event (lane)
  "LANE's shape as a `lane-state` event.  Read under the swarm lock."
  (list :type :lane-state
        :lane (lane-n lane)
        :state (lane-state lane)
        :task (lane-task lane)
        :goal (getf (lane-goal lane) :status)
        :restarts (lane-restarts lane)
        :pid (lane-pid lane)))

(defun maybe-publish-lane-state (lane)
  "Publish LANE's `lane-state` when its state, task or goal status changed —
and only then, so a stream of text deltas does not become a stream of
identical events.  A no-op under a frontend with no event stream (the TUI)."
  (let ((event nil))
    (with-swarm-lock ()
      (let ((key (list (lane-state lane) (lane-task lane)
                       (getf (lane-goal lane) :status))))
        (unless (equal key (lane-published lane))
          (setf (lane-published lane) key
                event (lane-state-event lane)))))
    (when event (swarm-publish event))))

;;; GET /lanes.

(defun lane-info (lane)
  "LANE's public shape for GET /lanes: no token, no URL, no port."
  (let ((snapshot (lane-snapshot lane)))
    (list :n (getf snapshot :n)
          :state (getf snapshot :state)
          :task (getf snapshot :task)
          :task-age (getf snapshot :task-age)
          :step-age (getf snapshot :step-age)
          :pid (getf snapshot :pid)
          :worktree (getf snapshot :worktree)
          :branch (getf snapshot :branch)
          :restarts (getf snapshot :restarts)
          :reports (getf snapshot :reports)
          :goal (getf snapshot :goal))))

(defun swarm-summary ()
  (with-swarm-lock ()
    (list :id (swarm-id *swarm*)
          :dir (namestring (swarm-dir *swarm*))
          :cwd (namestring (swarm-cwd *swarm*))
          :workers (swarm-workers *swarm*)
          :busy (count-if (lambda (lane)
                            (member (lane-state lane) '(:working :compacting)))
                          (swarm-lanes *swarm*))
          :stopping (and (swarm-stopping *swarm*) t))))

(defun swarm-lanes-response ()
  "The GET /lanes body: the swarm, and one row per lane."
  (list :swarm (swarm-summary)
        :lanes (coerce (mapcar #'lane-info (swarm-lanes *swarm*)) 'vector)))

;;; GET /lanes/N/transcript — the lane's own /transcript, decoded.

(defun lane-transcript (lane &key limit)
  "The messages LANE's next turn would send, as serve replied — read-only."
  (multiple-value-bind (status reply)
      (lane-get lane (if limit
                         (format nil "/transcript?limit=~d" limit)
                         "/transcript"))
    (lane-ok lane status reply "transcript")
    reply))

;;; GET /lanes/N/events — a live, resumable, read-only relay of the lane's
;;; own /events.  The lane's serve owns the ids and the resume semantics; the
;;; relay forwards Last-Event-ID/since and copies each event through
;;; unchanged, so a client's cursor stays the lane's cursor.

(defun copy-lane-events (in out)
  "Relay the SSE stream IN to OUT, ids and types untouched.  Returns the last
id relayed.  Ends when either side does."
  (let ((last nil))
    (read-sse-events in
                     (lambda (id type data)
                       (evo.serve:write-sse-event out type data :id id)
                       (when (and id (or (null last) (> id last)))
                         (setf last id)))
                     :on-comment (lambda (text)
                                   (evo.serve:write-sse-comment out text)))
    last))

(defun relay-lane-events (lane out &key since)
  "Copy LANE's events to OUT.  SINCE is the client's cursor, forwarded as the
lane's `?since=`; :LIVE tails from the lane's next event.  Read-only: the
swarm's own lane cursor is left alone."
  (let ((in (open-event-stream lane :since since)))
    (unwind-protect (copy-lane-events in out)
      (ignore-errors (close in)))))
