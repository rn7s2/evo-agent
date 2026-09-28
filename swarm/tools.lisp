;;;; tools.lisp — the coordinator's hands on the lanes.
;;;;
;;;; Every tool reaches a lane through serve's public HTTP API (client.lisp)
;;;; and nothing else.  They run on the coordinator's run thread; what a lane
;;;; says back arrives separately, through its event subscription
;;;; (lanes.lisp), as the coordinator's input.

(in-package :evo.swarm)

(defun lane-arg (args &key (required t))
  "The lane named by ARGS' :lane, which must exist."
  (let ((n (getf args :lane)))
    (cond ((and (null n) (not required)) nil)
          ((not (integerp n)) (error "lane must be a lane number (1-~d)" (swarm-workers *swarm*)))
          ((find-lane n))
          (t (error "no lane ~d — lanes are 1-~d" n (swarm-workers *swarm*))))))

(defun text-arg (args key)
  (let ((text (getf args key)))
    (unless (and (stringp text) (plusp (length text)))
      (error "~(~a~) must be a non-empty string" key))
    text))

(defun lane-status-line (lane)
  (destructuring-bind (&key n state task pid worktree branch restarts step-age reports &allow-other-keys)
      (lane-snapshot lane)
    (format nil "lane ~d  ~(~a~)~@[ · step ~a~]~@[ · task: ~a~]~@[ · worktree ~a~]~@[ (~a)~] · ~d report~:p~@[ · ~a~]~@[ · pid ~a~]"
            n state (and step-age (evo.tui::short-duration step-age))
            (and task (truncate-string (substitute #\Space #\Newline task) 60 "…"))
            worktree branch reports
            (and (plusp restarts) (format nil "~d restart~:p" restarts))
            pid)))

(defun wait-until-idle (lane &key (seconds 30))
  "Poll LANE until it reports no task.  T when it did within SECONDS."
  (loop repeat (* seconds 5)
        do (multiple-value-bind (status state) (ignore-errors (lane-get lane "/state"))
             (when (and (eql status 200) (equal (getf state :status) "idle"))
               (with-swarm-lock () (setf (lane-state lane) :idle))
               (return t)))
           (sleep 0.2)))

(defun reply-output (reply)
  "The text lines a command REPLY carried."
  (format nil "~{~a~^~%~}"
          (loop for line across (or (getf reply :output) #())
                collect (getf line :text))))

(defun set-lane-task (lane task)
  (with-swarm-lock ()
    (setf (lane-task lane) task
          (lane-state lane) :working
          (lane-task-started lane) (get-universal-time)
          (lane-step-started lane) (get-universal-time)))
  (record-swarm))

;;; The tools.

(defun tool-lanes (args)
  (declare (ignore args))
  (format nil "~{~a~^~%~}" (mapcar #'lane-status-line (swarm-lanes *swarm*))))

(defun first-idle-lane ()
  (find :idle (swarm-lanes *swarm*) :key (lambda (l) (with-swarm-lock () (lane-state l)))))

(defun tool-delegate (args)
  (let* ((task (text-arg args :task))
         (lane (or (lane-arg args :required nil)
                   (first-idle-lane)
                   (error "every lane is busy — wait for one to report, or steer one")))
         (objective (getf args :objective)))
    (let ((state (with-swarm-lock () (lane-state lane))))
      (unless (eq state :idle)
        (error "lane ~d is ~(~a~); delegate to an idle lane, or use steer_lane / interrupt_and_steer"
               (lane-n lane) state)))
    (when (and objective (plusp (length objective)))
      (lane-eval lane (forms->code
                       `((evo.kernel:create-goal-entry evo:*agent* ,objective)))))
    (multiple-value-bind (status reply) (lane-post lane "/prompt" :body (list :text task))
      (lane-ok lane status reply "prompt")
      (set-lane-task lane (or objective task))
      (format nil "Delegated to lane ~d~@[ with goal: ~a~]. It reports back here when it has something; end your turn to wait.~@[~%~a~]"
              (lane-n lane) objective
              (let ((out (reply-output reply))) (and (plusp (length out)) out))))))

(defun tool-steer-lane (args)
  (let ((lane (lane-arg args)) (text (text-arg args :text)))
    (multiple-value-bind (status reply) (lane-post lane "/steer" :body (list :text text))
      (if (eql status 409)
          (format nil "Lane ~d is idle, so there is nothing to steer — use delegate to give it work."
                  (lane-n lane))
          (progn (lane-ok lane status reply "steer")
                 (format nil "Steered lane ~d; it sees this at its next turn boundary."
                         (lane-n lane)))))))

(defun interrupt (lane)
  (multiple-value-bind (status reply) (lane-post lane "/interrupt")
    (lane-ok lane status reply "interrupt")
    (let ((interrupted (getf (getf reply :data) :interrupted)))
      (values interrupted (wait-until-idle lane)))))

(defun tool-interrupt-lane (args)
  (let ((lane (lane-arg args)))
    (multiple-value-bind (interrupted idle) (interrupt lane)
      (cond ((not interrupted) (format nil "Lane ~d was not running anything." (lane-n lane)))
            (idle (format nil "Lane ~d interrupted; it is idle." (lane-n lane)))
            (t (format nil "Lane ~d was told to stop but is not idle yet." (lane-n lane)))))))

(defun tool-interrupt-and-steer (args)
  (let ((lane (lane-arg args)) (text (text-arg args :text)))
    (multiple-value-bind (interrupted idle) (interrupt lane)
      (declare (ignore interrupted))
      (unless idle
        (error "lane ~d did not stop within 30s; try again or restart_lane" (lane-n lane)))
      (multiple-value-bind (status reply) (lane-post lane "/prompt" :body (list :text text))
        (lane-ok lane status reply "prompt")
        (set-lane-task lane text)
        (format nil "Lane ~d interrupted and redirected." (lane-n lane))))))

(defun tool-lane-command (args)
  (let* ((lane (lane-arg args))
         (command (text-arg args :command))
         (command (if (string-prefix-p "/" command) command (concatenate 'string "/" command))))
    (multiple-value-bind (status reply) (lane-command lane command)
      (format nil "HTTP ~d~@[ — ~a~]~%~a~@[~%data: ~a~]" status (getf reply :error)
              (reply-output reply)
              (and (getf reply :data) (evo.serve:encode-json (getf reply :data)))))))

(defun tool-lane-eval (args)
  (let* ((lane (lane-arg args))
         (code (text-arg args :code))
         (keep (if (member :keep args) (getf args :keep) t)))
    (let ((reply (lane-eval lane code)))
      (when keep
        (with-swarm-lock ()
          (setf (lane-extra-forms lane) (append (lane-extra-forms lane) (list code))))
        (record-swarm))
      (format nil "~a~:[~;~%(kept: replayed if lane ~d restarts)~]"
              (getf (getf reply :data) :result) keep (lane-n lane)))))

(defun render-message (message)
  (let ((role (getf message :role)))
    (format nil "~a: ~{~a~^ ~}" role
            (loop for block across (coerce (or (getf message :content) #()) 'vector)
                  collect (let ((type (getf block :type)))
                            (cond ((equal type "text")
                                   (truncate-string (getf block :text) 400 "…"))
                                  ((equal type "tool-call")
                                   (format nil "[call ~a ~a]" (getf block :name)
                                           (truncate-string
                                            (evo.serve:encode-json (getf block :arguments))
                                            200 "…")))
                                  ((equal type "image") "[image]")
                                  (t (format nil "[~a]" type))))))))

(defun tool-lane-transcript (args)
  (let* ((lane (lane-arg args))
         (limit (or (getf args :limit) 20)))
    (multiple-value-bind (status reply)
        (lane-get lane (format nil "/transcript?limit=~d" limit))
      (lane-ok lane status reply "transcript")
      (let ((messages (getf reply :messages)))
        (if (zerop (length messages))
            (format nil "Lane ~d's transcript is empty." (lane-n lane))
            (format nil "~{~a~^~%~}" (map 'list #'render-message messages)))))))

(defun journal-reports (lane)
  "LANE's reports, oldest first, read from its journal — the source of truth:
they are its `report` tool calls, so they outlive this process and a resume."
  (multiple-value-bind (status reply) (lane-get lane "/journal")
    (lane-ok lane status reply "journal")
    (loop for entry across (or (getf reply :entries) #())
          for message = (getf entry :message)
          when (equal (getf message :role) "assistant")
            append (loop for block across (coerce (or (getf message :content) #()) 'vector)
                         when (and (equal (getf block :type) "tool-call")
                                   (equal (getf block :name) "report"))
                           collect (getf block :arguments)))))

(defun tool-lane-reports (args)
  (let* ((lane (lane-arg args))
         (limit (or (getf args :limit) 10))
         (reports (journal-reports lane)))
    (if reports
        (format nil "~{~a~^~%~%~}"
                (mapcar (lambda (r) (report-text lane r))
                        (last reports limit)))
        (format nil "Lane ~d has not reported yet." (lane-n lane)))))

(defun tool-restart-lane (args)
  (let* ((lane (lane-arg args))
         (mode (or (getf args :mode) "restart")))
    (cond
      ((equal mode "reinit")
       (initialize-lane lane)
       (format nil "Lane ~d re-initialized in place." (lane-n lane)))
      ((member mode '("restart" "fresh") :test #'equal)
       (if (restart-lane lane :fresh (equal mode "fresh"))
           (format nil "Lane ~d restarted~:[ (its session resumed)~; with a fresh session~]."
                   (lane-n lane) (equal mode "fresh"))
           (error "lane ~d did not come back up — see its lane.log" (lane-n lane))))
      (t (error "mode must be reinit, restart or fresh")))))

(defun git (&rest args)
  "Run git in the swarm's cwd.  Returns its output; signals with it on failure."
  (multiple-value-bind (out err code)
      (uiop:run-program (list* "git" "-C" (namestring (swarm-cwd *swarm*)) args)
                        :output :string :error-output :string :ignore-error-status t)
    (unless (zerop code)
      (error "git ~{~a~^ ~} failed: ~a" args (string-trim '(#\Newline) (or err out))))
    out))

(defun tool-lane-worktree (args)
  (let* ((lane (lane-arg args))
         (action (or (getf args :action) "create")))
    (unless (eq (with-swarm-lock () (lane-state lane)) :idle)
      (error "lane ~d must be idle to move it (interrupt it first)" (lane-n lane)))
    (cond
      ((equal action "create")
       (when (lane-worktree lane)
         (error "lane ~d already works in ~a" (lane-n lane) (lane-worktree lane)))
       (let* ((branch (or (getf args :branch)
                          (format nil "swarm/~a/lane-~d" (swarm-id *swarm*) (lane-n lane))))
              (path (namestring (merge-pathnames (format nil "worktrees/lane-~d/" (lane-n lane))
                                                 (swarm-dir *swarm*)))))
         (ensure-directories-exist (merge-pathnames "worktrees/" (swarm-dir *swarm*)))
         (git "worktree" "add" "-b" branch (string-right-trim "/" path) "HEAD")
         (with-swarm-lock ()
           (setf (lane-worktree lane) path
                 (lane-branch lane) branch
                 (lane-cwd lane) (uiop:ensure-directory-pathname path)))
         (restart-lane lane)
         (format nil "Lane ~d now works in its own worktree ~a on branch ~a (restarted there, session kept). Merge that branch when it reports done; remove the worktree with action remove."
                 (lane-n lane) path branch)))
      ((equal action "remove")
       (let ((path (lane-worktree lane)) (branch (lane-branch lane)))
         (unless path (error "lane ~d has no worktree" (lane-n lane)))
         (with-swarm-lock ()
           (setf (lane-worktree lane) nil
                 (lane-branch lane) nil
                 (lane-cwd lane) (swarm-cwd *swarm*)))
         (restart-lane lane)
         (git "worktree" "remove" "--force" (string-right-trim "/" path))
         (format nil "Lane ~d is back in the shared directory; worktree removed, branch ~a kept for you to merge or delete."
                 (lane-n lane) branch)))
      (t (error "action must be create or remove")))))

;;; Registration.

(defmacro deftool (name description schema function)
  `(evo:register-tool ,name :description ,description :schema ',schema :execute #',function))

(defparameter *swarm-tool-names*
  '("lanes" "delegate" "steer_lane" "interrupt_lane" "interrupt_and_steer"
    "lane_command" "lane_eval" "lane_transcript" "lane_reports" "restart_lane"
    "lane_worktree"))

(defun register-swarm-tools ()
  (deftool "lanes"
    "List the swarm's lanes: state (idle, working, down...), step clock, current task, worktree, reports."
    (:object) tool-lanes)
  (deftool "delegate"
    "Give a lane a task. The lane is a separate agent that cannot see this conversation: the task must carry everything it needs, including how to tell it is done. With objective, the lane works on it as a goal until done. Picks the first idle lane unless lane is given. Returns at once; the lane reports back as input."
    (:object (:task :type :string :description "The complete instructions for the lane")
             (:lane :type :integer :optional t :description "Lane number; default: the first idle lane")
             (:objective :type :string :optional t :description "Make it the lane's goal: what done means"))
    tool-delegate)
  (deftool "steer_lane"
    "Add guidance to a working lane; it sees it at its next turn boundary, without stopping."
    (:object (:lane :type :integer) (:text :type :string)) tool-steer-lane)
  (deftool "interrupt_lane" "Stop what a lane is doing now; it goes idle."
    (:object (:lane :type :integer)) tool-interrupt-lane)
  (deftool "interrupt_and_steer"
    "Stop a lane now and give it new instructions in the same step."
    (:object (:lane :type :integer) (:text :type :string)) tool-interrupt-and-steer)
  (deftool "lane_command"
    "Run a slash command in a lane, exactly as typed in its TUI: /goal, /model <id>, /compact, /lore ..., /thinking, /tree, /new ..."
    (:object (:lane :type :integer) (:command :type :string)) tool-lane-command)
  (deftool "lane_eval"
    "Evaluate Lisp in a lane's image (package EVO.USER), as its eval tool would — e.g. to install a tool or an MCP server it asked for and you agreed to. Kept (replayed when the lane restarts) unless keep is false."
    (:object (:lane :type :integer) (:code :type :string)
             (:keep :type :boolean :optional t :description "Replay on restart (default true)"))
    tool-lane-eval)
  (deftool "lane_transcript" "Read the last messages of a lane's context."
    (:object (:lane :type :integer) (:limit :type :integer :optional t)) tool-lane-transcript)
  (deftool "lane_reports" "Read the reports a lane has sent, oldest first."
    (:object (:lane :type :integer) (:limit :type :integer :optional t)) tool-lane-reports)
  (deftool "restart_lane"
    "Re-initialize a lane in place (reinit), restart its process keeping its session (restart), or start it over with a fresh session (fresh)."
    (:object (:lane :type :integer)
             (:mode :type :string :enum ("reinit" "restart" "fresh") :optional t))
    tool-restart-lane)
  (deftool "lane_worktree"
    "Give an idle lane its own git worktree and branch (create) so its edits cannot collide with others', or move it back to the shared directory (remove; the branch is kept for you to merge)."
    (:object (:lane :type :integer)
             (:action :type :string :enum ("create" "remove") :optional t)
             (:branch :type :string :optional t :description "Branch name for create"))
    tool-lane-worktree))
