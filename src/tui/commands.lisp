;;;; commands.lisp — TUI builtin slash commands + skills/templates resolution
;;;; (§12) + the main loop / entry point.

(in-package :evo.tui)

;;; Builtins.

(defparameter *builtin-help*
  "commands:
  /help                this list
  /goal [objective]    show, create, or refine the goal
  /todo                toggle the todo panel
  /model [id]          show or set the model (journaled; next turn)
  /thinking [level]    off·low·medium·high·xhigh
  /compact [hint]      compact the context now
  /lore [text]         show lore, or add durable guidance (project scope)
  /tree                navigate entries, move the leaf (rewind/branch)
  /resume              switch to another session
  /fork                fork this session at the current leaf
  /new                 start a fresh session
  /export [path]       export the transcript as markdown
  /reload              reload extension directories
  /quit                exit (ctrl+c ctrl+c, ctrl+d)
keys: enter send · shift+enter/alt+enter/ctrl+j newline · esc interrupt ·
      esc esc rewind · paste >3 lines collapses (paste again to expand)")

(defun goal-command (tui args)
  (let* ((agent (tui-agent tui))
         (goal (current-goal agent)))
    (cond
      ((zerop (length args))
       (scroll tui (if goal
                       (format nil "goal ~a [~(~a~)]: ~a~%tokens: ~:d / ~:d~@[~%done-when: ~a~]"
                               (pget goal :goal-id) (pget goal :status)
                               (pget goal :objective)
                               (goal-tokens-used agent goal)
                               (pget goal :token-budget)
                               (pget goal :done-when))
                       (dim "no goal — /goal <objective> to set one"))))
      ((and goal (eq (pget goal :status) :active))
       ;; Refine: new :goal entry, same id; steer if a run is active (§8.2).
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
  (refresh-goal tui)
  (setf (tui-partial tui) "")
  (when note (scroll tui (dim note))))

(defun resume-command (tui)
  (when (require-idle tui "/resume")
    (let ((sessions (list-sessions)))
      (if (null sessions)
          (scroll tui (dim "no sessions for this directory"))
          (enter-select
           tui "resume session:"
           (loop for s in sessions
                 for i from 1
                 collect (cons (format nil "~2d. ~a" i (pget s :timestamp))
                               (pget s :path)))
           (lambda (path)
             (switch-journal tui (open-journal path)
                             :note (format nil "resumed ~a" path))
             (show-history-tail tui)
             ;; An active goal in a resumed session picks itself back up (§8).
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
                  (format nil "⏺ ~a" (pget call :name))
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
                  ;; the editor (edit-and-resubmit = new branch, §4.4).
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
        ((cmd "model")
         (cond ((zerop (length args))
                (scroll tui (format nil "models: ~{~a~^, ~}"
                                    (mapcar (lambda (m) (pget m :id)) *models*))))
               (t (append-entry (agent-journal agent)
                                (list :type :model-change :provider :anthropic
                                      :model args))
                  (refresh-goal tui)
                  (scroll tui (dim (format nil "model → ~a (next turn)" args)))))
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
             (progn (scroll tui (dim "compacting…"))
                    (handler-case
                        (progn (evo.kernel:compact-now agent :hint args)
                               (scroll tui (green "✓ compacted")))
                      (error (e) (scroll tui (red (format nil "✗ compact: ~a" e))))))
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
         (boot-extensions :journal (agent-journal agent))
         (scroll tui (dim "extension directories reloaded"))
         t)
        (t nil)))))

;;; Skills + templates as commands (§12 resolution order).

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
  ;; Live resize (D4).
  (when *resized*
    (setf *resized* nil)
    (refresh-size)
    (bt:with-lock-held (*tui-lock*)
      (setf *region-height* (min *region-height* *rows*)))
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
  ;; Agent events.
  (dolist (event (drain-events tui))
    (handle-agent-event tui event))
  ;; Spinner.
  (when (and (tui-running tui) (zerop (mod (tui-tick tui) 4)))
    (incf (tui-spinner tui))
    (setf (tui-dirty tui) t))
  (when (tui-dirty tui)
    (repaint tui)))

(defun banner (tui &key resumed-p)
  (let ((agent (tui-agent tui)))
    (scroll tui (format nil "~a ~a"
                        (bold "evo")
                        (dim (format nil "· ~a · enter sends · shift+enter newline · /help · ~a"
                                     (or (agent-model-override agent)
                                         (setting :model "claude-sonnet-5"))
                                     (if resumed-p "resumed" "new session")))))))

(defun start-tui (agent &key resumed-p)
  "Run the interactive TUI until quit.  Returns an exit code."
  (let ((tui (make-tui :agent agent
                       :stdin (sb-sys:make-fd-stream 0 :input t :buffering :none
                                                       :element-type '(unsigned-byte 8)))))
    (setf *tui* tui)
    (setf (agent-events-cb agent) (lambda (event) (push-event tui event)))
    (evo:on :todo-changed
            (lambda (payload)
              (push-event tui (list :type :todo-changed
                                    :todos (evo.util:pget payload :todos)))))
    (term-setup)
    (unwind-protect
         (progn
           (banner tui :resumed-p resumed-p)
           (when resumed-p (show-history-tail tui))
           (refresh-goal tui)
           ;; An idle active goal always gets re-steered (§8.2).
           (let ((goal (tui-goal tui)))
             (when (and goal (eq (pget goal :status) :active))
               (queue-steering agent (goal-continuation-for agent goal))
               (start-worker tui)))
           (repaint tui)
           (loop until (tui-quit tui)
                 do (tick tui)
                    (sleep 0.02))
           ;; Shut down: interrupt any in-flight run cooperatively.
           (when (tui-running tui)
             (setf (agent-abort-flag agent) t)
             (loop repeat 250
                   while (tui-running tui)
                   do (dolist (event (drain-events tui))
                        (handle-agent-event tui event))
                      (sleep 0.02)))
           (bt:with-lock-held (*tui-lock*)
             (emit-scrollback (dim "bye.")))
           0)
      (term-teardown))))
