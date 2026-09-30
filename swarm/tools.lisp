;;;; tools.lisp — the coordinator's hands on the lanes.
;;;;
;;;; Every tool reaches a lane through the new protocol (client.lisp) and
;;;; nothing else: input goes to `input.send`, commands to `command.run`,
;;;; code to `eval`, a stop to `run.interrupt`.  They run on the coordinator's
;;;; run thread; what a lane says back arrives separately, through its mirror
;;;; (mirror.lisp), as the coordinator's input.

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

(defun lane-aged (ms)
  "How long ago MS (epoch milliseconds) was, in the shared short form."
  (and ms (evo.view:short-duration (max 0 (- (evo.view:now-ms) ms)))))

(defun lane-status-line (lane)
  (destructuring-bind (&key n state task pid worktree branch restarts reports model &allow-other-keys)
      (lane-snapshot lane)
    (format nil "lane ~d  ~(~a~)~@[ · step ~a~]~@[ · task: ~a~]~@[ · ~a~]~@[ · worktree ~a~]~@[ (~a)~] · ~d report~:p~@[ · ~a~]~@[ · pid ~a~]"
            n state
            (lane-aged (with-swarm-lock () (lane-step-started lane)))
            (and task (truncate-string (substitute #\Space #\Newline task) 60 "…"))
            (getf model :id)
            worktree branch reports
            (and (plusp restarts) (format nil "~d restart~:p" restarts))
            pid)))

(defun lane-status (lane)
  "LANE's own status word, from its mirror: what its topic says."
  (getf (mirror-lane-state (lane-mirror lane)) :status))

(defun wait-until-idle (lane &key (seconds 30))
  "Wait for LANE to report no task.  T when it did within SECONDS."
  (loop repeat (* seconds 10)
        when (equal (lane-status lane) "idle")
          do (with-swarm-lock () (setf (lane-state lane) :idle))
             (return t)
        do (sleep 0.1)))

(defun notices-text (result)
  "The notices a command's structured result carried (§5.5 command.run)."
  (let ((notices (getf result :notices)))
    (when (and notices (plusp (length notices)))
      (format nil "~{~a~^~%~}" (map 'list (lambda (n) (getf n :text)) notices)))))

(defun set-lane-task (lane task)
  (with-swarm-lock ()
    (setf (lane-task lane) task
          (lane-state lane) :working
          (lane-task-started lane) (evo.view:now-ms)
          (lane-step-started lane) (evo.view:now-ms)))
  (swarm-lane-changed)
  (record-swarm))

;;; The tools.

(defun tool-lanes (args)
  (declare (ignore args))
  (format nil "~{~a~^~%~}" (mapcar #'lane-status-line (swarm-lanes *swarm*))))

(defun first-idle-lane ()
  (find :idle (swarm-lanes *swarm*)
        :key (lambda (l) (with-swarm-lock () (lane-state l)))))

(defun send-input (lane text &key (queue "now"))
  "Give LANE TEXT through input.send (§5.5).  Returns the result plist."
  (lane-op lane "input.send" (list :text text :queue queue)))

(defun tool-delegate (args)
  (let* ((task (text-arg args :task))
         (lane (or (lane-arg args :required nil)
                   (first-idle-lane)
                   (error "every lane is busy — wait for one to report, or steer one")))
         (objective (getf args :objective)))
    (let ((state (with-swarm-lock () (lane-state lane))))
      (unless (eq state :idle)
        (error "lane ~d is ~(~a~); delegate to an idle lane, or use steer_lane / interrupt_lane with a text"
               (lane-n lane) state)))
    (when (and objective (plusp (length objective)))
      (lane-op lane "eval" (list :code (forms->code
                                        `((evo.kernel:create-goal-entry evo:*agent* ,objective))))))
    (let* ((result (send-input lane task))
           (blocked (getf result :blocked)))
      (when blocked
        (error "lane ~d did not accept the task (~a)" (lane-n lane) blocked))
      (set-lane-task lane (or objective task))
      (format nil "Delegated to lane ~d~@[ with goal: ~a~]. It reports back here when it has something; end your turn to wait."
              (lane-n lane) objective))))

(defun tool-steer-lane (args)
  (let ((lane (lane-arg args)) (text (text-arg args :text)))
    (if (eq (with-swarm-lock () (lane-state lane)) :idle)
        (format nil "Lane ~d is idle, so there is nothing to steer — use delegate to give it work."
                (lane-n lane))
        (progn (send-input lane text)
               (format nil "Steered lane ~d; it sees this at its next turn boundary."
                       (lane-n lane))))))

(defun interrupt-lane (lane)
  "Stop LANE now.  Returns (values INTERRUPTED IDLE)."
  (let* ((result (lane-op lane "run.interrupt" (list :scope "session")))
         (interrupted (getf result :interrupted)))
    (values interrupted (wait-until-idle lane))))

(defun tool-interrupt-lane (args)
  "Stop LANE now; with a text, then give it that text as its new instructions."
  (let ((lane (lane-arg args))
        (text (let ((text (getf args :text))) (and text (plusp (length text)) text))))
    (multiple-value-bind (interrupted idle) (interrupt-lane lane)
      (cond
        (text
         (unless idle
           (error "lane ~d did not stop within 30s; try again or restart_lane" (lane-n lane)))
         (send-input lane text)
         (set-lane-task lane text)
         (format nil "~:[Lane ~d was not running; it has the new instructions~;Lane ~d interrupted and redirected~]."
                 interrupted (lane-n lane)))
        ((not interrupted) (format nil "Lane ~d was not running anything." (lane-n lane)))
        (idle (format nil "Lane ~d interrupted; it is idle." (lane-n lane)))
        (t (format nil "Lane ~d was told to stop but is not idle yet." (lane-n lane)))))))

(defun tool-lane-command (args)
  (let* ((lane (lane-arg args))
         (text (text-arg args :command))
         (text (string-left-trim "/" text))
         (space (position #\Space text))
         (name (if space (subseq text 0 space) text))
         (rest (if space (string-left-trim " " (subseq text space)) "")))
    (let* ((result (lane-op lane "command.run" (list :name name :args rest)
                                                   :timeout 120))
           (notices (notices-text result))
           (data (getf result :data))
           (choices (getf result :choices)))
      (format nil "~@[~a~]~@[~%(data: ~a)~]~@[~%(choices: ~a)~]"
              notices
              (and data (evo.serve:encode-json data))
              (and choices (evo.serve:encode-json choices))))))

(defun tool-lane-eval (args)
  (let* ((lane (lane-arg args))
         (code (text-arg args :code))
         (keep (if (member :keep args) (getf args :keep) t)))
    (let ((result (lane-op lane "eval" (list :code code) :timeout 120)))
      (when keep
        (with-swarm-lock ()
          (setf (lane-extra-forms lane) (append (lane-extra-forms lane) (list code))))
        (record-swarm))
      (format nil "~a~:[~;~%(kept: replayed if lane ~d restarts)~]"
              (getf result :value) keep (lane-n lane)))))

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
  "What LANE's next turn would send, from its /debug/context (§5.6)."
  (let* ((lane (lane-arg args))
         (limit (or (getf args :limit) 20))
         (body (lane-get-body lane "/debug/context" :what "context"))
         (messages (or (getf body :messages) (getf body :items))))
    (if (and messages (plusp (length messages)))
        (let ((tail (last (coerce messages 'list) limit)))
          (format nil "~{~a~^~%~}" (mapcar #'render-message tail)))
        (format nil "Lane ~d's context is empty." (lane-n lane)))))

(defun journal-lane-reports (lane)
  "LANE's reports as the coordinator's own journal holds them: each is a
message whose :origin is the lane-report metadata (§3), so nothing is parsed
out of prose."
  (let ((entries (ignore-errors
                   (coerce (journal-entries
                            (agent-journal (swarm-agent *swarm*)))
                           'list))))
    (loop for entry in entries
          for origin = (or (getf entry :origin) (getf (getf entry :message) :origin))
          when (and (equal (getf entry :type) :message)
                    (equal (getf origin :kind) :lane-report)
                    (eql (getf origin :lane) (lane-n lane)))
            collect origin)))

(defun report-line (n origin)
  (format nil "[lane ~d report] done: ~a~@[~%evidence: ~a~]~@[~%next: ~a~]~@[~%blocked: ~a~]~@[~%requests: ~a~]~@[~%goal: ~a~]"
          n (or (getf origin :done) "")
          (getf origin :evidence) (getf origin :next)
          (getf origin :blocked) (getf origin :requests)
          (getf origin :goal)))

(defun tool-lane-reports (args)
  (let* ((lane (lane-arg args))
         (limit (or (getf args :limit) 10))
         (reports (journal-lane-reports lane)))
    (if reports
        (format nil "~{~a~^~%~%~}"
                (mapcar (lambda (r) (report-line (lane-n lane) r))
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
       (let ((fresh (equal mode "fresh")))
         (stop-lane lane)
         (with-swarm-lock () (setf (lane-stopping lane) nil))
         (if (restart-lane lane :fresh fresh)
             (format nil "Lane ~d restarted~:[ (its session resumed)~; with a fresh session~]."
                     (lane-n lane) fresh)
             (error "lane ~d did not come back up — see its lane.log" (lane-n lane)))))
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
  '("lanes" "delegate" "steer_lane" "interrupt_lane" "lane_command" "lane_eval"
    "lane_transcript" "lane_reports" "restart_lane" "lane_worktree"))

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
  (deftool "interrupt_lane"
    "Stop what a lane is doing now; it goes idle. With text, it is then given that text as its new instructions, in the same step."
    (:object (:lane :type :integer)
             (:text :type :string :optional t
              :description "New instructions to give the lane once it has stopped; omit to only stop it"))
    tool-interrupt-lane)
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
