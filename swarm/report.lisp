;;;; report.lisp — what the coordinator hears from a lane, and how it hears it.
;;;;
;;;; A lane's own items are the only channel (CONTRACT §3, §4.1): the mirror
;;;; notes each item once, and the two that mean something to the coordinator —
;;;; a `report` tool call, and a finished run — become its input, carrying an
;;;; :origin plist that says what they are.  The prose is for the model; the
;;;; origin is for every client, which renders a lane_report or lane_event item
;;;; instead of parsing English.

(in-package :evo.swarm)

(defun tell-coordinator (text &key (style :notice) origin)
  "Give the coordinator TEXT as input: queued to its next turn boundary when
it is working, starting a run when it is idle — and shown to the human.  ORIGIN
rides the message into the journal and is never shown to the model."
  (let ((agent (and *swarm* (swarm-agent *swarm*))))
    (when agent
      (queue-steering agent text :origin origin)
      (swarm-say text :style style)
      (evo:request-run))))

(defun tell-lane-event (lane event &key detail outcome goal-status (severity :error))
  "Tell the coordinator something happened to LANE — the origin carries it as
data (§3), the text as what the model reads.  EVENT is one of §4.1's:
:run-ended :error :failed-to-start :init-failed :crashed :down :restarted."
  (tell-coordinator (format nil "[lane ~d] ~(~a~)~@[ — ~a~]"
                            (lane-n lane) event detail)
                    :style (if (eq severity :error) :error :notice)
                    :origin (list :kind :lane-event :lane (lane-n lane)
                                  :event (intern (string-upcase (string event)) :keyword)
                                  :outcome outcome :goal-status goal-status
                                  :detail detail :severity severity)))

(defun report-text (lane report)
  "[lane N report] done: … — the prose the coordinator's model reads.  The
fields also travel as the message's :origin, which is what a client renders."
  (format nil "[lane ~d report] done: ~a~@[~%evidence: ~a~]~@[~%next: ~a~]~@[~%blocked: ~a~]~@[~%requests: ~a~]~@[~%goal: ~a~]"
          (lane-n lane)
          (or (getf report :done) "")
          (getf report :evidence) (getf report :next)
          (getf report :blocked) (getf report :requests)
          (getf report :goal)))

(defun report-origin (lane report)
  "A REPORT tool call's arguments as a :lane-report origin (§3)."
  (list :kind :lane-report :lane (lane-n lane)
        :done (getf report :done) :evidence (getf report :evidence)
        :next (getf report :next) :blocked (getf report :blocked)
        :requests (getf report :requests)
        :goal (let ((goal (getf report :goal)))
                (and goal (intern (string-upcase (string goal)) :keyword)))))

(defun goal-disposition (status)
  "What a settled lane's goal STATUS tells the coordinator, or NIL for no
goal.  A lane's goal re-steers it for as long as it is active, so a lane that
settles with its goal still active is not done: it errored or was stopped."
  (cond ((null status) nil)
        ((equal status "active") "active, but the lane is idle until steered")
        ((equal status "budget-limited") "budget-limited (out of tokens)")
        (t status)))

(defun lane-goal-status (lane)
  "The goal status the lane's own topic state reports, as a string or NIL."
  (let ((goal (getf (mirror-lane-state (lane-mirror lane)) :goal)))
    (and (stringp (getf goal :status)) (getf goal :status))))

(defun run-ended-text (lane outcome)
  (format nil "[lane ~d] run ended (~a)~@[ — goal: ~a~]~@[ — task: ~a~]"
          (lane-n lane) outcome
          (goal-disposition (lane-goal-status lane))
          (with-swarm-lock ()
            (and (lane-task lane)
                 (truncate-string (lane-task lane) 80 "…")))))

;;; The two items that are news.

(defun lane-reported (lane report)
  "LANE's report tool call: the coordinator's input gets it as a
:lane-report message (§3), which is the only place a lane's report exists as
data."
  (with-swarm-lock () (incf (lane-reports lane)))
  (swarm-lane-changed)
  (tell-coordinator (report-text lane report) :origin (report-origin lane report)))

(defun lane-run-ended (lane item)
  "LANE's run finished: the coordinator hears it as a :lane-event message —
which goal status it settled with is what says whether \"done\" is done."
  (let ((outcome (getf item :outcome))
        (goal (lane-goal-status lane)))
    (tell-coordinator (run-ended-text lane outcome)
                      :style (if (member outcome '("error") :test #'equal) :error :notice)
                      :origin (list :kind :lane-event :lane (lane-n lane)
                                    :event :run-ended :outcome outcome
                                    :goal-status (and goal (intern (string-upcase goal) :keyword))
                                    :severity (if (equal outcome "error") :error :info)))
    (swarm-lane-changed)))

(defun lane-item-arrived (mirror item)
  "An item of MIRROR's lane just arrived, once.  Reports and finished runs are
the two that mean something to the coordinator; everything else it can read in
the lane's topic."
  (let ((lane (mirror-lane mirror))
        (kind (getf item :kind)))
    (when lane
      (cond
        ((and (equal kind "tool") (equal (getf item :name) "report")
              (getf item :args))
         (lane-reported lane (getf item :args)))
        ((equal kind "run_outcome")
         (lane-run-ended lane item))))))
