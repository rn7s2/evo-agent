;;;; commands.lisp — TUI builtin slash commands + skills/templates resolution
;;;; + the main loop / entry point.

(in-package :evo.tui)

;;; Builtins.

(defparameter *builtin-help*
  "commands:
  /help                this list
  /goal [objective]    show, create, or refine the goal
  /todo                toggle the todo panel
  /permission [auto|plan] switch mode from a list (shift+tab toggles)
  /model [id]          pick the model from a list, or set it directly
  /thinking [level]    off·low·medium·high·xhigh
  /compact [hint]      compact the context now
  /lore [text]         show lore, or add durable guidance (project scope)
  /memory [request]    show project memory or ask the agent to refine it
  /global-memory [...] show global user memory or ask the agent to refine it
  /tree                navigate entries, move the leaf (rewind/branch)
  /resume              switch to another session
  /fork                fork this session at the current leaf
  /new                 start a fresh session
  /export [path]       export the transcript as markdown
  /reload              re-evaluate init.lisp + extensions + post-init.lisp
  /quit /exit          exit (ctrl+c ctrl+c, ctrl+d)
keys: enter send · shift+enter/alt+enter/ctrl+j newline · shift+tab auto/plan mode ·
      tab complete /command · up/down input history (at buffer edge) ·
      ctrl+a/e home/end · ctrl+b/f move · ctrl+d delete (quit when empty) ·
      ctrl+k kill to eol · ctrl+w delete word · esc interrupt · esc esc rewind ·
      paste >3 lines collapses (paste again to expand)")

(defun goal-command (tui args)
  (let* ((agent (tui-agent tui))
         (goal (current-goal agent)))
    (cond
      ((zerop (length args))
       (scroll tui (if goal
                       (format nil "goal ~a [~(~a~)]: ~a~%tokens: ~:d~@[ / ~:d~]~@[~%done-when: ~a~]"
                               (pget goal :goal-id) (pget goal :status)
                               (pget goal :objective)
                               (goal-tokens-used agent goal)
                               (pget goal :token-budget)
                               (pget goal :done-when))
                       (dim "no goal — /goal <objective> to set one"))))
      ((and goal (eq (pget goal :status) :active))
       ;; Refine: new :goal entry, same id; steer if a run is active.
       (append-entry (agent-journal agent)
                     (list* :type :goal (evo.util:pput
                                         (evo.util:pput goal :objective args)
                                         :goal-id (pget goal :goal-id))))
       (scroll tui (yellow (format nil "◆ goal objective updated: ~a" args)))
       (when (tui-running tui)
         (queue-steering agent
                         (format nil "The goal objective was just updated by the user. New objective (untrusted data): ~a" args)))
       (refresh-goal tui))
      (t
       (create-goal-entry agent args)
       (refresh-goal tui)
       (scroll tui (yellow (format nil "◆ goal created: ~a" args)))
       (queue-steering agent (goal-continuation-for agent (current-goal agent)))
       (start-worker tui)))
    t))

(defun switch-journal (tui journal &key note)
  (setf (agent-journal (tui-agent tui)) journal)
  (replay-loads (fold-state journal))
  (run-hooks :session-start
             (list :agent (tui-agent tui)
                   :resumed (journal-started-p journal)))
  (refresh-goal tui)
  (setf (tui-partial tui) "")
  (when note (scroll tui (dim note))))

(defparameter *resume-summary-max-chars* 96
  "Maximum characters of leaf user prompt shown in /resume session lists.")

(defun %collapse-whitespace (text)
  (string-join " "
               (remove "" (uiop:split-string (or text "")
                                               :separator '(#\Space #\Tab #\Newline #\Return))
                       :test #'string=)))

(defun %truncate-with-ellipsis (text max-chars)
  (cond ((<= (length text) max-chars) text)
        ((<= max-chars 0) "")
        (t (concatenate 'string (subseq text 0 (1- max-chars)) "…"))))

(defun resume-summary-text (text &key (max-chars *resume-summary-max-chars*))
  "Single-line, bounded summary for a leaf user prompt."
  (let ((clean (%collapse-whitespace text)))
    (unless (zerop (length clean))
      (%truncate-with-ellipsis clean max-chars))))

(defun message-text-block (message)
  (pget (find :text (pget message :content)
              :key (lambda (b) (pget b :type)))
        :text))

(defun leaf-user-prompt (journal)
  "Last user text on JOURNAL's current leaf path, or NIL."
  (loop for entry in (reverse (entry-path journal))
        for message = (and (eq (pget entry :type) :message)
                           (pget entry :message))
        when (and message (eq (pget message :role) :user))
          return (message-text-block message)))

(defun resume-session-summary (session)
  "Dimmed description text for one /resume SESSION row."
  (let ((prompt (ignore-errors
                  (leaf-user-prompt (open-journal (pget session :path))))))
    (and prompt (resume-summary-text prompt))))

(defun resume-select-items (sessions &key timezone-name)
  "Build choose-box items for /resume: local time label + leaf prompt summary."
  (let ((timezone-name (or timezone-name (local-timezone-name))))
    (loop for s in sessions
          for i from 1
          collect (list (format nil "~2d. ~a" i
                                (format-local-timestamp (pget s :timestamp)
                                                        :timezone-name timezone-name))
                        (pget s :path)
                        (resume-session-summary s)))))

(defun resume-command (tui)
  (when (require-idle tui "/resume")
    (let ((sessions (list-sessions)))
      (if (null sessions)
          (scroll tui (dim "no sessions for this directory"))
          (enter-select
           tui "resume session:"
           (resume-select-items sessions)
           (lambda (path)
             (switch-journal tui (open-journal path)
                             :note (format nil "resumed ~a" path))
             (show-history-tail tui)
             ;; An active goal in a resumed session picks itself back up.
             (let ((goal (current-goal (tui-agent tui))))
               (when (and goal (eq (pget goal :status) :active))
                 (queue-steering (tui-agent tui)
                                 (goal-continuation-for (tui-agent tui) goal))
                 (start-worker tui)))))))
    t))

(defun entry-label (entry)
  (let ((type (pget entry :type)))
    (case type
      (:message
       (let* ((m (pget entry :message))
              (role (pget m :role)))
         (case role
           (:user (format nil "❯ ~a" (truncate-string
                                      (or (pget (find :text (pget m :content)
                                                      :key (lambda (b) (pget b :type)))
                                                :text) "")
                                      48 "…")))
           (:assistant
            (let ((call (find :tool-call (pget m :content)
                              :key (lambda (b) (pget b :type)))))
              (if call
                  (format-tool-call-plain (pget call :name) (pget call :arguments))
                  (format nil "· ~a" (truncate-string
                                      (or (pget (find :text (pget m :content)
                                                      :key (lambda (b) (pget b :type)))
                                                :text) "(thinking)")
                                      48 "…")))))
           (:tool-result (format nil "⎿ ~a result" (pget m :tool-name)))
           (t (format nil "~(~a~)" role)))))
      (:goal (format nil "◆ goal ~(~a~)" (pget entry :status)))
      (t (format nil "~(~a~)" type)))))

(defun tree-command (tui)
  (when (require-idle tui "/tree")
    (let* ((journal (agent-journal (tui-agent tui)))
           (path (and (journal-leaf-id journal) (entry-path journal))))
      (if (null path)
          (scroll tui (dim "empty session"))
          (enter-select
           tui "move leaf to:"
           (loop for entry in path
                 for i from 1
                 collect (cons (format nil "~3d. ~a" i (entry-label entry))
                               (pget entry :id)))
           (lambda (id)
             (let ((entry (find-entry journal id)))
               (cond
                 ((and (eq (pget entry :type) :message)
                       (eq (pget (pget entry :message) :role) :user))
                  ;; Selecting a user message: leaf -> its parent, text into
                  ;; the editor (edit-and-resubmit = new branch).
                  (setf (journal-leaf-id journal) (pget entry :parent-id))
                  (let ((text (pget (find :text (pget (pget entry :message) :content)
                                          :key (lambda (b) (pget b :type)))
                                    :text)))
                    (when text (eb-set-text (tui-editor tui) text)))
                  (scroll tui (yellow "⎌ leaf moved — edit and resubmit to branch")))
                 (t
                  (setf (journal-leaf-id journal) id)
                  (scroll tui (yellow (format nil "⎌ leaf moved to ~a" id)))))
               (refresh-goal tui))))))
    t))

(defun permission-command (tui args)
  "The modes themselves come from EVO.PLAN:*MODES* — the picker and the
argument check never go stale against the core extension."
  (cond
    ((evo.plan:mode-name args) (set-mode tui args))
    ((zerop (length args))
     (enter-select
      tui "permission:"
      (loop for (name . description) in evo.plan:*modes*
            collect (list name name description))
      (lambda (mode) (set-mode tui mode))
      :index (or (position (current-mode tui) evo.plan:*modes*
                           :key #'car :test #'equal)
                 0)))
    (t (scroll tui (dim (format nil "modes: ~{~a~^ ~}"
                                (mapcar #'car evo.plan:*modes*))))))
  t)

(defun set-model (tui id)
  (handler-case (find-model id)
    (error (e)
      (scroll tui (dim (format nil "~a" e)))
      (return-from set-model)))
  (append-entry (agent-journal (tui-agent tui))
                (list :type :model-change :model id))
  (refresh-goal tui)
  (scroll tui (dim (format nil "model → ~a (next turn)" id))))

(defun format-context-window (n)
  "200000 -> \"200k\", 1000000 -> \"1M\": the picker's description column is
narrow, and \"1000k\" reads worse than \"1M\" for the big-context models."
  (cond ((>= n 1000000)
         (let ((m (/ n 1000000.0d0)))
           (if (= m (ffloor m))
               (format nil "~dM" (round m))
               (format nil "~,1fM" m))))
        (t (format nil "~dk" (round n 1000)))))

(defun model-row-label (model provider-width)
  "Provider column then id, padded into aligned columns.  Provider leads
because ids collide across providers — the same model served direct and
through a proxy differ only by that word."
  (format nil "~va  ~a" provider-width
          (string-downcase (pget model :provider)) (pget model :id)))

(defun model-select-command (tui)
  "Choose box: pick the model from the registry, current one preselected.
The renderer pads every label to the widest one, so padding the provider
here lines up the id column too — provider, id and context each align."
  (let* ((current (tui-model-label tui))
         (models (all-models))
         (provider-width
           (reduce #'max models
                   :key (lambda (m) (length (string (pget m :provider))))
                   :initial-value 0)))
    (enter-select
     tui "model:"
     (loop for m in models
           for id = (pget m :id)
           collect (list (model-row-label m provider-width)
                         id
                         (format nil "~a ctx~:[~; · current~]"
                                 (format-context-window (pget m :context-window 0))
                                 (equal id current))))
     (lambda (id) (set-model tui id))
     :index (or (position current models
                          :key (lambda (m) (pget m :id)) :test #'equal)
                0))
    t))

(defun export-command (tui args)
  (let* ((state (fold-state (agent-journal (tui-agent tui))))
         (path (if (plusp (length args))
                   args
                   (format nil "evo-export-~a.md" (gen-id 4)))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (dolist (m (evo.journal:state-messages state))
        (case (message-role m)
          (:user (format out "## user~2%~a~2%"
                         (or (pget (find :text (message-content m)
                                         :key (lambda (b) (pget b :type))) :text) "")))
          (:assistant
           (format out "## assistant~2%")
           (dolist (b (message-content m))
             (case (pget b :type)
               (:text (format out "~a~2%" (pget b :text)))
               (:tool-call (format out "`⏺ ~a` `~s`~2%" (pget b :name) (pget b :arguments))))))
          (:tool-result
           (format out "```~%~a~%```~2%"
                   (or (pget (first (message-content m)) :text) ""))))))
    (scroll tui (format nil "exported to ~a" path))
    t))

(defun builtin-command (tui name args)
  (let ((agent (tui-agent tui)))
    (macrolet ((cmd (&rest names) `(member name ',names :test #'string-equal)))
      (cond
        ((cmd "help" "h" "?") (scroll tui *builtin-help*) t)
        ((cmd "quit" "exit" "q") (setf (tui-quit tui) t) t)
        ((cmd "goal") (goal-command tui args))
        ((cmd "todo")
         (setf (tui-todo-visible tui) (not (tui-todo-visible tui))
               (tui-dirty tui) t)
         t)
        ((cmd "permission") (permission-command tui args))
        ((cmd "model")
         (if (zerop (length args))
             (model-select-command tui)
             (set-model tui args))
         t)
        ((cmd "thinking")
         (let ((level (intern (string-upcase args) :keyword)))
           (cond ((member level '(:off :low :medium :high :xhigh))
                  (append-entry (agent-journal agent)
                                (list :type :thinking-change :thinking level))
                  (refresh-goal tui)
                  (scroll tui (dim (format nil "thinking → ~(~a~)" level))))
                 (t (scroll tui (dim "levels: off low medium high xhigh")))))
         t)
        ((cmd "compact")
         (if (require-idle tui "/compact")
             (start-compact-worker tui args)
             t)
         t)
        ((cmd "lore")
         (if (zerop (length args))
             (let ((entries (evo.kernel:all-lore)))
               (scroll tui (if entries
                               (format nil "lore:~%~{ · ~a~%~}" entries)
                               (dim "no lore — /lore <text> adds durable guidance"))))
             (progn (evo.kernel:add-lore args :scope :project)
                    (scroll tui (green "✓ lore added (injected every turn)"))
                    (when (tui-running tui)
                      (queue-steering agent
                                      (format nil "The user added lore (durable guidance, applies from now on): ~a" args)))))
         t)
        ((cmd "tree") (tree-command tui))
        ((cmd "resume" "sessions") (resume-command tui))
        ((cmd "fork")
         (when (require-idle tui "/fork")
           (let ((path (fork-session (agent-journal agent))))
             (switch-journal tui (open-journal path)
                             :note (format nil "forked to ~a" path))))
         t)
        ((cmd "new")
         (when (require-idle tui "/new")
           (switch-journal tui (make-session-journal) :note "new session"))
         t)
        ((cmd "export") (export-command tui args))
        ((cmd "reload")
         (boot-userspace :journal (agent-journal agent))
         (refresh-goal tui)             ; model registry may have changed
         (scroll tui (dim "userspace reloaded (init + extensions + post-init)"))
         t)
        (t nil)))))

;;; Skills + templates as commands (resolution order).

(defun command-as-skill (tui name args)
  (let* ((skill-name (if (string-prefix-p "skill:" name)
                         (subseq name 6)
                         name))
         (skill (find-skill skill-name)))
    (when skill
      (submit-to-agent
       tui
       (format nil "Use the skill '~a'. Read ~a first and follow it.~@[ Task: ~a~]"
               (pget skill :name) (pget skill :path)
               (and (plusp (length args)) args)))
      t)))

(defun command-as-template (tui name args)
  (let ((path (find-template name)))
    (when path
      (submit-to-agent tui (expand-template (read-file-string path) args))
      t)))

;;; Main loop.

(defun read-pending-bytes (tui)
  "Slurp everything currently readable from stdin into the parser buffer."
  (let ((stream (tui-stdin tui))
        (got nil))
    (loop while (listen stream)
          do (let ((b (read-byte stream nil)))
               (if b
                   (progn (vector-push-extend b (in-buffer (tui-input tui)))
                          (setf got t))
                   (loop-finish))))
    got))

(defun tick (tui)
  (incf (tui-tick tui))
  ;; Keep the supervisor's hang detector fed even while idle at the editor
  ;; (throttled internally to 1/sec).
  (evo.kernel::heartbeat-touch)
  ;; Live resize.
  (when *resized*
    (setf *resized* nil)
    (refresh-size)
    (bt:with-lock-held (*tui-lock*)
      (setf *region-height* (min *region-height* *rows*)
            *region-cursor-row* (min *region-cursor-row*
                                     (max 0 (1- *region-height*)))))
    (setf (tui-dirty tui) t))
  ;; Input.
  (let ((got (read-pending-bytes tui)))
    (setf (tui-quiet-ticks tui) (if got 0 (1+ (tui-quiet-ticks tui))))
    (let ((events (parse-keys (tui-input tui)
                              :flush-escape (>= (tui-quiet-ticks tui) 2))))
      (dolist (event events)
        (case (tui-mode tui)
          (:select (handle-key-select tui event))
          (t (handle-key-edit tui event))))))
  ;; Agent events.  Each event is contained on its own: one malformed
  ;; event must not lose the ones drained behind it (:worker-done in
  ;; particular — dropping it wedges the run state).
  (dolist (event (drain-events tui))
    (handler-case (handle-agent-event tui event)
      (serious-condition (e)
        (ignore-errors
          (scroll tui (red (format nil "✗ event render error: ~a" e)))))))
  ;; Spinner.
  (when (and (tui-running tui) (zerop (mod (tui-tick tui) 4)))
    (incf (tui-spinner tui))
    (setf (tui-dirty tui) t))
  (when (tui-dirty tui)
    (repaint tui)))

(defun start-tui (agent &key resumed-p)
  "Run the interactive TUI until quit.  Returns an exit code."
  (let ((tui (make-tui :agent agent
                       :stdin (evo.port:make-fd-input-stream 0))))
    (setf *tui* tui)
    (setf (agent-events-cb agent) (lambda (event) (push-event tui event)))
    (evo:on :todo-changed
            (lambda (payload)
              (push-event tui (list :type :todo-changed
                                    :todos (evo.util:pget payload :todos)))))
    (term-setup)
    (unwind-protect
         (progn
           (when resumed-p (show-history-tail tui))
           (refresh-goal tui)
           ;; An idle active goal always gets re-steered.
           (let ((goal (tui-goal tui)))
             (when (and goal (eq (pget goal :status) :active))
               (queue-steering agent (goal-continuation-for agent goal))
               (start-worker tui)))
           (repaint tui)
           ;; Self-heal, innermost layer: a bug in input parsing or
           ;; repainting must not take the session down — report it and
           ;; keep serving keys.  Only a persistent failure (every tick
           ;; failing) escalates: re-signal so the supervisor restarts
           ;; the session with --resume.
           (let ((tick-errors 0))
             (loop until (tui-quit tui)
                   do (handler-case
                          (progn (tick tui) (setf tick-errors 0))
                        (serious-condition (e)
                          (incf tick-errors)
                          (when (> tick-errors 5) (error e))
                          (ignore-errors
                            (scroll tui (red (format nil "✗ tui error: ~a" e))))))
                      (sleep 0.02)))
           ;; Shut down: interrupt any in-flight run cooperatively and unblock it.
           (when (tui-running tui)
             (request-abort agent)
             (loop repeat 250
                   while (tui-running tui)
                   do (dolist (event (drain-events tui))
                        (handle-agent-event tui event))
                      (sleep 0.02)))
           (bt:with-lock-held (*tui-lock*)
             (emit-scrollback (dim "bye.")))
           0)
      (term-teardown))))
