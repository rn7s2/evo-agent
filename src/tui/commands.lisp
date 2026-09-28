;;;; commands.lisp — the TUI's side of slash commands, and the main loop /
;;;; entry point.
;;;;
;;;; What a command DOES lives in the core command layer (src/command/), shared
;;;; with `evo serve`; this file is the TUI as its host: output becomes
;;;; scrollback, a choice becomes a picker, a refusal a dim line.  Only the
;;;; presentational commands — /help, /todo, /theme, /image, /quit — are the
;;;; TUI's own.

(in-package :evo.tui)

;;; Builtins.

(defparameter *builtin-help*
  "commands:
  /help                this list
  /goal [text]         show, create, or refine the goal; /goal pause|resume
  /todo                toggle the todo panel
  /theme [dark|light]  switch the light/dark theme (math colours follow it)
  /model [id]          pick the model from a list, or set it directly
  /thinking [level]    low·medium·high·xhigh·max (changing it mid-session
                       drops the provider prompt cache)
  /lang [code]         pick the language of the system prompt and replies
                       (\"en\" only by default; install a pack for more;
                       no code: a list)
  /compact [hint]      compact the context now
  /image [path ...]    attach an image to the message being typed
                       (no path: the system clipboard, same as ctrl+v)
  /lore [text]         show project+session lore (with ids), or add project-scope guidance; ask me to edit/remove by id
  /global-lore [text]  show global (every project) lore, or add user-scope guidance
  /memory [request]    show project memory or ask the agent to refine it
  /global-memory [...] show global user memory or ask the agent to refine it
  /eval <sexpr>        evaluate one sexpr in the live image (progn to group;
                       tab completes functions/variables, up/down recalls)
  /tree [id]           navigate entries, move the leaf (rewind/branch)
  /rewind              move the leaf above your last message (esc esc)
  /resume [n]          switch to another session
  /fork                fork this session at the current leaf
  /new                 start a fresh session
  /export [path]       export the transcript as markdown
  /reload              re-evaluate init.lisp + extensions + post-init.lisp
  /quit /exit          exit (ctrl+c ctrl+c, ctrl+d)
keys: enter send · shift+enter/alt+enter/ctrl+j newline ·
      tab complete /command or /eval symbol · up/down input history (at buffer edge) ·
      ctrl+a/e home/end · ctrl+b/f move · ctrl+d delete (quit when empty) ·
      ctrl+k kill to eol · ctrl+w delete word · esc interrupt · esc esc rewind ·
      ctrl+v attach the clipboard image · paste/drop an image path to attach it ·
      a big paste collapses to a token (paste it again to expand it)
images: no terminal can hand an app the image itself, so evo reads the clipboard
      when you ask it to: ctrl+v (works everywhere), ctrl+alt+v (where the
      terminal keeps ctrl+v for its own paste), cmd+v/right-click paste (where
      the terminal still sends the empty paste — VS Code, Cursor), /image with
      no argument (works always), or paste/drop the file's path.")

;;; The TUI as the command layer's host.

(defmethod evo.command:host-agent ((tui tui)) (tui-agent tui))
(defmethod evo.command:host-running-p ((tui tui)) (tui-running tui))
(defmethod evo.command:host-start-run ((tui tui)) (start-worker tui))
(defmethod evo.command:host-start-compact ((tui tui) hint)
  (start-compact-worker tui hint))

(defmethod evo.command:host-say ((tui tui) text &optional (style :plain))
  (scroll tui (ecase style
                (:plain text)
                (:dim (dim text))
                (:notice (yellow text))
                (:success (green text))
                (:error (red text)))))

(defmethod evo.command:host-refresh ((tui tui)) (refresh-goal tui))

(defmethod evo.command:host-choose ((tui tui) title items action &key (index 0))
  ;; The action runs from a key press long after the command returned, so a
  ;; refusal at pick time (the session got busy meanwhile) is shown here.
  (enter-select tui title items
                (lambda (value)
                  (evo.command:with-refusals-shown (tui) (funcall action value)))
                :index index))

(defmethod evo.command:host-set-draft ((tui tui) text)
  (eb-set-text (tui-editor tui) text))

(defmethod evo.command:host-session-switched ((tui tui))
  (setf (tui-partial tui) ""))

(defmethod evo.command:host-show-history ((tui tui)) (show-history-tail tui))
(defmethod evo.command:host-submit ((tui tui) text &optional images)
  (submit-to-agent tui text images))
(defmethod evo.command:host-command-context ((tui tui)) (list :tui tui))
(defmethod evo.command:host-interrupt-hint ((tui tui)) "esc to interrupt")

;;; The TUI's own, presentational builtins.

(defun image-command (tui args)
  "/image [path ...] — attach images to the message being typed.  With no
argument it grabs the system clipboard, which is what ctrl+v does; the
command exists for the cases a keystroke cannot reach: a path you can
tab-complete-free type, several files at once, or a terminal that eats
ctrl+v."
  (if (zerop (length args))
      (paste-clipboard-image tui)
      (dolist (path (or (evo.media:split-shell-tokens args) (list args)))
        (attach-image-path tui path)))
  t)

(defun theme-command (tui args)
  "Set or toggle the TUI light/dark theme.  The theme is the shared :theme
setting (also settable in init.lisp); extensions read it — the LaTeX-math
renderer, for one, picks its glyph colour from it.  Applies to new output;
already-painted scrollback keeps its colours."
  (let* ((cur (setting :theme :dark))
         (new (cond ((string-equal args "light") :light)
                    ((string-equal args "dark") :dark)
                    ((or (zerop (length args)) (string-equal args "toggle"))
                     (if (eq cur :light) :dark :light))
                    (t nil))))
    (cond
      (new (set-setting :theme new)
           (setf (tui-dirty tui) t)
           (scroll tui (dim (format nil "theme → ~(~a~) (applies to new output)" new))))
      (t (scroll tui (dim "usage: /theme [dark | light | toggle]"))))
    t))

(defun tui-builtin-command (tui name args)
  "The presentational builtins; T when NAME is one of them."
  (macrolet ((cmd (&rest names) `(member name ',names :test #'string-equal)))
    (cond
      ((cmd "help" "h" "?") (scroll tui *builtin-help*) t)
      ((cmd "quit" "exit" "q") (setf (tui-quit tui) t) t)
      ((cmd "todo")
       (setf (tui-todo-visible tui) (not (tui-todo-visible tui))
             (tui-dirty tui) t)
       t)
      ((cmd "theme") (theme-command tui args))
      ((cmd "image" "img") (image-command tui args))
      (t nil))))

(defun builtin-command (tui name args)
  "Every builtin the TUI answers: its own, then the core's.  T when handled."
  (or (tui-builtin-command tui name args)
      (evo.command:with-refusals-shown (tui)
        (evo.command:builtin-command tui name args))
      ;; A refused builtin was still a builtin.
      (and (or (assoc name evo.command:*builtin-commands* :test #'string-equal)
               (member name '("rewind" "language" "sessions") :test #'string-equal))
           t)))

;;; The core commands, under the names the TUI (and its tests) call them by.

(defun session-quiescent-p (tui) (evo.command:session-quiescent-p tui))

(defun require-session-quiescent (tui what)
  (evo.command:with-refusals-shown (tui)
    (evo.command:require-session-quiescent tui what)))

(defun require-idle (tui what)
  (evo.command:with-refusals-shown (tui)
    (evo.command:require-idle tui what)))

(defun switch-journal (tui journal &key note)
  (evo.command:switch-journal tui journal :note note))

(defun lore-command (tui args scope &key (cwd (uiop:getcwd)))
  (evo.command:lore-command tui args scope :cwd cwd))

(defun goal-command (tui args)
  (evo.command:with-refusals-shown (tui) (evo.command:goal-command tui args))
  t)

(defun set-model (tui model)
  (evo.command:with-refusals-shown (tui) (evo.command:set-model tui model)))

(defun export-command (tui args) (evo.command:export-command tui args))

;;; Main loop.

(defun read-pending-bytes (tui)
  "Slurp everything currently readable from stdin into the parser buffer."
  (evo.port:read-available-input (tui-stdin tui) (in-buffer (tui-input tui))))

(defun tick (tui)
  (incf (tui-tick tui))
  ;; Keep the supervisor's hang detector fed even while idle at the editor
  ;; (throttled internally to 1/sec).
  (heartbeat-touch)
  ;; Live resize.  Signal-driven where there is a signal for it, polled
  ;; where there is not (Windows) — both land in *RESIZED*.
  (poll-terminal-resize)
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
    (let* ((keys (parse-keys (tui-input tui)
                             :flush-escape (>= (tui-quiet-ticks tui) 2)))
           (events (tick-key-events tui keys)))
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
                       :stdin (evo.port:make-stdin-stream))))
    (setf *tui* tui)
    ;; Every agent event — the todo tool's :todo-changed included — arrives
    ;; through the one queue the TUI thread drains.
    (setf (agent-events-cb agent) (lambda (event) (push-event tui event)))
    (term-setup)
    (unwind-protect
         (progn
           (when resumed-p (show-history-tail tui))
           (refresh-goal tui)
           ;; An idle active goal always gets re-steered; start-worker's
           ;; model gate scrolls a startup warning if the model went
           ;; stale.  With no goal, check explicitly — a missing model
           ;; should surface now, not at the first submit.
           (let ((goal (tui-goal tui)))
             (if (and goal (eq (pget goal :status) :active))
                 (progn
                   (queue-steering agent (goal-continuation-for agent goal))
                   (start-worker tui))
                 (check-model-ready tui)))
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
           ;; The session is going away: tell extensions BEFORE task shutdown
           ;; so anything held open on the user's behalf (a notification
           ;; waiting for a reply) comes down while the session still owns it.
           (end-session agent)
           ;; Shut down: ask the task to stop, then keep draining until its
           ;; :worker-done arrives — that handler is what joins the thread, so
           ;; draining here is how the task is actually reaped rather than
           ;; merely abandoned.
           (unless (shutdown-task tui)
             (ignore-errors
               (scroll tui (dim "a background task did not stop in time; exiting anyway"))))
           (bt:with-lock-held (*tui-lock*)
             (emit-scrollback (dim "bye.")))
           0)
      (term-teardown))))
