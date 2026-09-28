;;;; events.lisp — the session's event log: every event, numbered, once.
;;;;
;;;; Producers are the run worker (kernel events through the agent's events
;;;; callback), the session thread (task starts and ends, command output) and
;;;; nobody else; consumers are SSE streams, each reading from its own cursor.
;;;; An event is encoded to JSON once, when it is published — on the thread
;;;; that owns the values in it — and every stream then sends the same text.
;;;;
;;;; Ids are consecutive integers from 1, so a client that reconnects with
;;;; Last-Event-ID N gets exactly the events after N.  The log keeps the last
;;;; *EVENT-LOG-CAPACITY* events; a cursor older than that gets a `gap` event
;;;; naming what it missed, never a silent hole.  Ids restart with the
;;;; process: a supervisor restart is a new log, announced by a `hello` event
;;;; carrying the process's own id.
;;;;
;;;; One lock guards the ring; readers poll (design.md §6 keeps waiting simple
;;;; and portable — no condition variables, no atomics).

(in-package :evo.serve)

(defparameter *event-log-capacity* 20000
  "Events kept for replay.  Each is one encoded line, so this bounds memory.")

(defstruct (event-log (:constructor %make-event-log))
  (lock (bt:make-lock "serve-events"))
  (ring (make-array *event-log-capacity* :initial-element nil))
  (last-id 0))

(defun make-event-log (&key (capacity *event-log-capacity*))
  (%make-event-log :ring (make-array capacity :initial-element nil)))

(defun publish (log event)
  "Append EVENT (a plist with :type) to LOG.  Returns its id.  Any thread."
  (let* ((json (event->json event))
         (type (string-downcase (princ-to-string (getf event :type :event)))))
    (bt:with-lock-held ((event-log-lock log))
      (let* ((id (incf (event-log-last-id log)))
             (ring (event-log-ring log)))
        (setf (aref ring (mod id (length ring))) (list id type json))
        id))))

(defun last-event-id (log)
  (bt:with-lock-held ((event-log-lock log))
    (event-log-last-id log)))

(defun events-after (log cursor)
  "Events with id > CURSOR still in LOG, oldest first, as (id type json).
Second value: the first id missed when CURSOR fell out of the ring, else NIL."
  (bt:with-lock-held ((event-log-lock log))
    (let* ((ring (event-log-ring log))
           (last (event-log-last-id log))
           (oldest (max 1 (1+ (- last (length ring)))))
           (start (max (1+ cursor) oldest)))
      (values (loop for id from start to last
                    collect (aref ring (mod id (length ring))))
              (and (< (1+ cursor) oldest) (<= (1+ cursor) last) (1+ cursor))))))
