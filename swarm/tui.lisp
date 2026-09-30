;;;; tui.lisp — the human's read-only window on the lanes.
;;;;
;;;; Input always goes to the coordinator; lanes are only watched.  A status
;;;; line segment shows every lane at a glance, /lanes lists them in full
;;;; (state, step clock, task), and /lane N follows one lane's items live in
;;;; the scrollback until /lane off.  What is shown comes from the lane's
;;;; mirror (mirror.lisp) — the same items a client of the headless swarm
;;;; reads, so the two frontends cannot drift apart.

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
      (format nil "swarm ~a — ~a~{~%~a~}~%/lane N follows a lane's items; /lane off stops."
              (swarm-id *swarm*) (namestring (swarm-dir *swarm*))
              (mapcar #'lane-status-line (swarm-lanes *swarm*)))
      "no swarm is running"))

;;; Following one lane in the scrollback.

(defun item-line (n item)
  "ITEM as one line for the scrollback, or NIL for an item that has nothing
to say there."
  (let ((kind (getf item :kind)))
    (cond
      ((equal kind "user") (format nil "  [lane ~d] > ~a" n (item-text item)))
      ((equal kind "assistant") (and (item-text item) (format nil "  [lane ~d] ~a" n (item-text item))))
      ((equal kind "tool")
       (format nil "  [lane ~d] → ~a ~@[(~a)~]"
               n (getf item :name) (getf item :status)))
      ((equal kind "lane_report")
       (format nil "  [lane ~d] report: ~a" n (truncate-string (or (getf item :done) "") 200 "…")))
      ((equal kind "notice")
       (format nil "  [lane ~d] ~a" n (getf item :text)))
      ((equal kind "run_outcome")
       (format nil "  [lane ~d] run ended (~a)" n (getf item :outcome)))
      (t nil))))

(defun item-text (item)
  (let ((text (getf item :text)))
    (when (and text (plusp (length text)))
      (truncate-string (substitute #\Space #\Newline text) 400 "…"))))

(defun watch-lane-items (mirror)
  "The TUI is following MIRROR's lane (or is not): print the items that
arrived since the last one printed.  Called from the lane's own thread."
  (let ((lane (mirror-lane mirror)))
    (when lane
      (let ((cursor (with-swarm-lock ()
                      (and (lane-watched lane) (lane-watch-cursor lane)))))
        (when (or cursor (with-swarm-lock () (lane-watched lane)))
          (let ((items (mirror-items-after mirror cursor)))
            (dolist (item items)
              (let ((line (item-line (lane-n lane) item)))
                (when line (evo.tui:post-notice line :style :dim))))
            (when items
              (with-swarm-lock ()
                (setf (lane-watch-cursor lane) (getf (car (last items)) :id))))))))))

(defun show-lane-tail (lane &key (items 8))
  "Print the newest ITEMS of LANE's mirror, as /lane N starts following it."
  (let ((recent (mirror-last-items (lane-mirror lane) items)))
    (dolist (item recent)
      (let ((line (item-line (lane-n lane) item)))
        (when line (evo.tui:post-notice line :style :dim))))
    (with-swarm-lock ()
      (setf (lane-watch-cursor lane) (and recent (getf (car (last recent)) :id))))))

(defun lane-view-command (ctx)
  "/lane N — follow lane N's items live (read-only); /lane off stops."
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
         (with-swarm-lock () (setf (lane-watched l) nil)))
       (with-swarm-lock () (setf (lane-watched lane) t))
       (show-lane-tail lane)
       (format nil "following lane ~d (read-only; your input still goes to the coordinator)"
               n)))))

(defun register-swarm-commands ()
  "The coordinator's swarm commands, for every frontend — over HTTP they are
POST /ops {\"op\":\"command.run\",\"args\":{\"name\":\"lanes\"}}.  Separate from
INSTALL-TUI-OBSERVATION, so a coordinator with no screen still has them."
  (evo:register-command "lanes" #'lanes-command
                        :description "list the swarm's lanes: state, step clock, task")
  (evo:register-command "lane" #'lane-view-command
                        :description "follow lane N's items live (read-only); /lane off"))

(defun install-tui-observation ()
  "What the TUI has that a headless coordinator does not: a segment on the
status line.  Commands are not the TUI's (REGISTER-SWARM-COMMANDS) — a
segment is a screen."
  (evo.tui:add-status-segment :swarm-lanes #'lanes-segment :side :right :order 300)
  ;; The scrollback follows a lane through the mirror's change hook: the same
  ;; items a client of the headless swarm reads.
  (setf *mirror-change-hook* 'watch-lane-items))
