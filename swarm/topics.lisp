;;;; topics.lisp — the swarm as one observable thing: topic `swarm` (CONTRACT
;;;; §4.3), the coordinator's hold, and the one human action on lanes.
;;;;
;;;; The swarm's own topic is state-only: every lane transition publishes a
;;;; state.patch carrying the whole state, so a client that applied the ops in
;;;; order holds exactly what a snapshot would have given it.  Where a lane's
;;;; own state lives is the lane's mirror (mirror.lisp), republished as
;;;; `lane:N`; this file only flattens it into that lane's row.

(in-package :evo.swarm)

;;; The swarm's state.

(defun lanes-busy-locked (swarm)
  "SWARM's working or compacting lanes.  The swarm lock is held."
  (remove-if-not (lambda (lane) (member (lane-state lane) '(:working :compacting)))
                 (swarm-lanes swarm)))

(defun lanes-busy (&optional (swarm *swarm*))
  "Every lane of SWARM that is working or compacting."
  (and swarm
       (bt:with-lock-held ((swarm-lock swarm))
         (lanes-busy-locked swarm))))

(defun lanes-busy-p (&optional (swarm *swarm*))
  (and (lanes-busy swarm) t))

(defun lane-model-config (swarm)
  "What the lanes run, as §4.3's config: an {id, provider} object or NIL.
The provider is published by name (REGISTRY-NAME), like every other provider
in a document."
  (let ((id (swarm-lane-model swarm)))
    (when id
      (list :id id :provider (registry-name (swarm-lane-provider swarm))))))

(defun lane-row (lane)
  "One lane of the swarm topic (§4.3), with what its own mirror knows."
  (let* ((mirror (lane-mirror lane))
         (state (mirror-lane-state mirror)))
    (list :n (lane-n lane)
          :state (lane-state lane)
          :task (lane-task lane)
          :task-started-at (lane-task-started lane)
          :step-started-at (lane-step-started lane)
          :restarts (lane-restarts lane)
          :pid (getf (lane-ready lane) :pid)
          :worktree (lane-worktree lane)
          :branch (lane-branch lane)
          :model (getf state :model)
          :context (getf state :context)
          :goal (getf state :goal)
          :todos (getf state :todos)
          :reports (lane-reports lane)
          :last-item (mirror-last-item mirror))))

(defun swarm-state (&optional (swarm *swarm*))
  "The swarm topic's state (§4.3), read under the swarm lock."
  (when swarm
    (bt:with-lock-held ((swarm-lock swarm))
      (list :id (swarm-id swarm)
            :workers (swarm-workers swarm)
            :status (list :busy (length (lanes-busy-locked swarm))
                          ;; WIRE-BOOLEAN: a field the contract calls a bool is
                          ;; one on the wire, and this one is NIL most of the
                          ;; time — which would read as "unknown", not "no".
                          :waiting-on-lanes (wire-boolean
                                             (and (not (swarm-coordinator-busy swarm))
                                                  (lanes-busy-locked swarm))))
            :config (list :lane-model (lane-model-config swarm)
                          :lane-thinking (swarm-lane-thinking swarm))
            :lanes (coerce (mapcar #'lane-row (swarm-lanes swarm)) 'vector)))))

(defun publish-swarm-state (&optional (swarm *swarm*) force)
  "Publish the swarm topic when its state changed (or FORCE).  Every lane
transition lands here, including starting→idle (CONTRACT §4.3)."
  (when swarm
    (let* ((state (swarm-state swarm))
           ;; EQUALP, not EQUAL: a topic state holds arrays (the lane rows,
           ;; todos, segments) and EQUAL does not descend into an array, so
           ;; EQUAL would call every state new and publish on every check.
           (changed (or force (not (equalp state (swarm-published swarm))))))
      (when changed
        (bt:with-lock-held ((swarm-lock swarm))
          (setf (swarm-published swarm) state))
        (publish-op (list :op "state.patch" :topic "swarm" :patch state))))))

(defun swarm-lane-changed (&optional mirror)
  "A lane changed: take what the lane says about itself (its status, its task
and step clocks) into the swarm's record of it, then refresh the swarm topic
and the TUI.  MIRROR is the lane's when that is how we heard — a lane's own
state.patch is what turns `working` into `idle`."
  (when mirror (mirror-note-lane-state mirror))
  (publish-swarm-state)
  ;; A lane transition may start or end the coordinator's hold, and the hold
  ;; can change while the coordinator is already settled: tell whoever renders
  ;; its status, or it would show `idle` while its lanes still work (§4.2).
  (let ((agent (and *swarm* (swarm-agent *swarm*))))
    (when agent (note-hold-changed agent)))
  (swarm-repaint))

;;; The topic provider for `swarm` itself: state, no items.

(defparameter *swarm-identity* '(:name "evo-swarm" :version "0.1.0")
  "Who the coordinator's program is: GET /health's :program and the ready
file's :program, so a client can tell a swarm from a bare agent.")

(defclass swarm-topic () ()
  (:documentation "The coordinator's view of its lanes as one topic (§4.3)."))

(defmethod evo.serve:topic-snapshot ((topic swarm-topic) &key (items 200))
  (declare (ignore items))
  (list :state (swarm-state) :items #() :has-more nil))

(defmethod evo.serve:topic-items-before ((topic swarm-topic) before limit)
  (declare (ignore topic before limit))
  (values nil nil))

(defmethod evo.serve:topic-item ((topic swarm-topic) id)
  (declare (ignore topic id))
  nil)

(defmethod evo.serve:topic-media ((topic swarm-topic) id n)
  (declare (ignore topic id n))
  (values nil nil))

(defun register-swarm-topics (server swarm)
  "Register the swarm topic and one topic per lane on SERVER (§7), so the
coordinator's own op log is where every client reads the swarm from.  A
coordinator with no op log (the TUI) has no topics to publish: it is a
no-op."
  (when server
    (evo.serve:register-topic server "swarm" (make-instance 'swarm-topic))
    (dolist (lane (swarm-lanes swarm))
      (evo.serve:register-topic server (lane-topic lane) (lane-mirror lane)))))

;;; The hold: why the coordinator is `waiting` while its lanes work.

(defun coordinator-hold-reason (agent)
  "The swarm's hold predicate (a core hook): a reason string while AGENT's
coordinator has nothing to do but its lanes are working, else NIL.  The VIEW
reports status `waiting` for a settled agent any hold predicate claims
(CONTRACT §4.2)."
  (when (and *swarm* (eq agent (swarm-agent *swarm*)))
    (let ((busy (lanes-busy)))
      (when busy (format nil "~d lane~:p working" (length busy))))))

;;; The one human action on lanes: run.interrupt with scope lane or swarm
;;; (CONTRACT §6, design §7.4).  A human may stop work, never redirect it.
;;;
;;; The methods specialise on the SERVER's own class rather than on T: serve
;;; dispatches first on the server, so a method that only specialised on the
;;; scope would lose to serve's default and never run.

(defun human-interrupt-note-text (lanes)
  "What the coordinator reads when a human stops LANE work."
  (format nil "[human] stopped lane~p ~{~d~^, ~} (interrupt).~@[ Lane~p ~{~d~^, ~} will report or settle; the rest keep working.~]"
          (length lanes) lanes
          (length lanes) lanes))

(defun human-interrupt-note (server lanes)
  "Tell the coordinator a human stopped lane work: a queued input with a
:human-action origin (§3), so it hears it at its next turn rather than now —
the human's stop is not a reason to spend a coordinator turn.  Queued as
after_run input: delivered when a run next starts, or at the settle of one
already going.

The queue is the coordinator's own transcript, so the note is shown in it at
once, as a queued row that keeps the id the kernel minted: the same call
serve's QUEUE-SESSION-INPUT makes for a client's own after-run input.  A
client reading /snapshot sees why the lanes stopped, and when a run next
drains the queue the row becomes the journaled message — and the human-action
item (§3) — under that same id."
  (let ((agent (and *swarm* (swarm-agent *swarm*))))
    (when agent
      (ignore-errors
        (let ((text (human-interrupt-note-text lanes))
              (origin (list :kind :human-action :action :interrupt
                            :lanes (coerce lanes 'vector))))
          (let ((id (queue-followup agent text :origin origin)))
            (let ((provider (and server (evo.serve:topic-provider server "session"))))
              (when provider
                (evo.serve:topic-provider-queued-input provider id text nil "after_run")))
            id))))))

(defun interrupt-lane-now (lane)
  "Stop LANE's run through its own run.interrupt.  Returns T when it was
running something.  The lane answers {interrupted: [...]}, always an array
(CONTRACT §5.5): an idle lane's is [], which decodes to #() — a vector, so
non-NIL — and must count as nothing stopped."
  (let ((result (ignore-errors (lane-op lane "run.interrupt" (list :scope "session")))))
    (let ((interrupted (getf result :interrupted)))
      (and (typep interrupted 'sequence)
           (plusp (length interrupted))))))

(defmethod evo.serve:lane-exists-p ((server evo.serve::server) lane)
  "The lanes are this program's, so this server's lanes are the swarm's: a
number the swarm has a lane for is one run.interrupt can name.  serve asks
before every :lane method runs, which is what makes an unknown lane NOT_FOUND
rather than the failed op this file's own method would raise looking it up."
  (declare (ignore server))
  (and (integerp lane) (find-lane lane) t))

(defmethod evo.serve:interrupt-scope ((server evo.serve::server) (scope (eql :lane)) lane)
  "One lane, stopped now; the coordinator is told, queued after its current
run.  No other lane is touched.  A lane that was not running anything is not
reported as interrupted."
  (let ((n (and lane (ignore-errors (parse-integer (princ-to-string lane))))))
    (unless (and n (find-lane n))
      (error "no lane ~a" (or lane "given")))
    (let ((lane (find-lane n)))
      (when (interrupt-lane-now lane)
        (human-interrupt-note server (list n))
        (list (lane-topic lane))))))

(defmethod evo.serve:interrupt-scope ((server evo.serve::server) (scope (eql :swarm)) lane)
  "The coordinator and every lane, stopped now — what `Stop swarm` means."
  (declare (ignore lane))
  (let ((swarm (and (eq server (swarm-server *swarm*)) *swarm*))
        (interrupted nil))
    (when swarm
      (when (swarm-coordinator-busy swarm)
        (ignore-errors (request-abort (swarm-agent swarm)))
        (push "session" interrupted))
      (let ((stopped nil))
        (dolist (lane (swarm-lanes swarm))
          (when (interrupt-lane-now lane) (push (lane-n lane) stopped)))
        (when stopped
          ;; The lanes are stopped; the coordinator is told, after its run.
          (setf stopped (sort stopped #'<))
          (human-interrupt-note server stopped)
          (dolist (n stopped)
            (push (format nil "lane:~d" n) interrupted))))
      (nreverse interrupted))))
