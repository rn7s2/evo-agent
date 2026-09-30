;;;; tui.lisp — the human's read-only window on the lanes.
;;;;
;;;; Input always goes to the coordinator; lanes are only watched.  A status
;;;; line segment shows every lane at a glance, /lanes lists them in full
;;;; (state, step clock, task), and /lane N follows one lane's transcript live
;;;; in the scrollback until /lane off.

(in-package :evo.swarm)

(defun lane-glyph (state)
  (case state
    (:working "●") (:compacting "◐") (:idle "○") (:starting "◌")
    (t "✗")))

(defun lanes-segment (ctx)
  "Status line: one glyph per lane (● working, ◐ compacting, ○ idle,
◌ starting, ✗ down or stopped), and how many are busy.  Plain text — the
segment declares its style and each frontend paints it."
  (declare (ignore ctx))
  (when *swarm*
    (let ((states (with-swarm-lock ()
                    (mapcar #'lane-state (swarm-lanes *swarm*)))))
      (format nil "lanes ~{~a~} ~d/~d busy"
              (mapcar #'lane-glyph states)
              (count-if (lambda (s) (member s '(:working :compacting))) states)
              (length states)))))

(defun lanes-command (ctx)
  (declare (ignore ctx))
  (if *swarm*
      (format nil "swarm ~a — ~a~%~{~a~^~%~}~%/lane N follows a lane's transcript; /lane off stops."
              (swarm-id *swarm*) (namestring (swarm-dir *swarm*))
              (mapcar #'lane-status-line (swarm-lanes *swarm*)))
      "no swarm is running"))

(defun lane-view-command (ctx)
  "/lane N — follow lane N's transcript live (read-only); /lane off stops."
  (let* ((args (string-trim " " (or (getf ctx :args) "")))
         (n (ignore-errors (parse-integer args)))
         (lane (and n (find-lane n))))
    (cond
      ((or (string-equal args "off") (zerop (length args)))
       (dolist (l (swarm-lanes *swarm*))
         (with-swarm-lock () (setf (lane-watched l) nil)))
       "lane view off")
      ((null lane) (format nil "no lane ~a — /lanes lists them" args))
      (t
       (dolist (l (swarm-lanes *swarm*))
         (with-swarm-lock () (setf (lane-watched l) (eq l lane))))
       (let ((recent (ignore-errors (tool-lane-transcript (list :lane n :limit 6)))))
         (format nil "following lane ~d (read-only; your input still goes to the coordinator)~@[~%~a~]"
                 n recent))))))

(defun register-swarm-commands ()
  "The coordinator's swarm commands, for every frontend — over HTTP they are
POST /command {\"text\": \"/lanes\"}.  Separate from INSTALL-TUI-OBSERVATION, so
a coordinator with no screen still has them."
  (evo:register-command "lanes" #'lanes-command
                        :description "list the swarm's lanes: state, step clock, task")
  (evo:register-command "lane" #'lane-view-command
                        :description "follow lane N's transcript live (read-only); /lane off"))

(defun install-tui-observation ()
  "What the TUI has that a headless coordinator does not: a segment on the
status line.  Commands are not the TUI's (REGISTER-SWARM-COMMANDS) — a
segment is a screen."
  (evo:define-status-segment :swarm-lanes #'lanes-segment :side :right :order 300
                             :style :muted))
