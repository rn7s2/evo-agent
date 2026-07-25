;;;; tui.lisp — the interactive frontend, a core extension.
;;;;
;;;; Single cooperative main loop: poll raw stdin bytes -> key events; drain
;;;; the agent event queue (the run executes on a worker thread and steering
;;;; queues carry mid-run input); recompose and repaint the managed
;;;; region.  Slash command resolution: extension commands ->
;;;; builtins -> skills -> prompt templates -> send to the agent.

(in-package :evo.tui)

(defstruct (tui (:conc-name tui-))
  agent
  stdin
  (input (make-input-state))
  (editor (make-edit-buffer))
  (events nil) (events-lock (bt:make-lock "tui-events"))
  worker
  (running nil)
  (partial "")
  (thinking-tail "")
  (spinner 0)
  (tick 0)
  (quiet-ticks 0)
  (todo-visible t)
  (todos nil)
  (goal nil)
  (model-label "") (thinking-label "")
  (mode :edit)
  select-title select-items select-index select-action
  (last-esc 0)
  (last-ctrl-c 0)
  (dirty t)
  (quit nil))

(defvar *tui* nil)

(defun push-event (tui event)
  (bt:with-lock-held ((tui-events-lock tui))
    (push event (tui-events tui))))

(defun drain-events (tui)
  (bt:with-lock-held ((tui-events-lock tui))
    (nreverse (shiftf (tui-events tui) nil))))

(defun now-ms ()
  (round (* 1000 (/ (get-internal-real-time) internal-time-units-per-second))))

;;; Scrollback helpers.

(defun scroll (tui text)
  (bt:with-lock-held (*tui-lock*)
    (emit-scrollback text))
  (setf (tui-dirty tui) t))

(defun flush-partial (tui)
  (when (plusp (length (tui-partial tui)))
    (scroll tui (tui-partial tui))
    (setf (tui-partial tui) "")))

(defun refresh-goal (tui)
  "Re-derive the cached fold state (goal, todos, model/thinking labels).
Compose-region uses only these caches: repaints must not fold the journal
while the run thread is appending to it."
  (let* ((agent (tui-agent tui))
         (state (fold-state (agent-journal agent))))
    (setf (tui-goal tui) (evo.journal:state-goal state)
          (tui-todos tui) (custom-state state "todo")
          (tui-model-label tui) (or (evo.journal:state-model state)
                                    (agent-model-override agent)
                                    (setting :model "claude-sonnet-5"))
          (tui-thinking-label tui) (string-downcase
                                    (or (evo.journal:state-thinking state)
                                        (agent-thinking-override agent)
                                        (setting :thinking :medium))))))

;;; Worker thread.

(defun start-worker (tui)
  (unless (tui-running tui)
    (setf (tui-running tui) t
          (agent-abort-flag (tui-agent tui)) nil)
    (setf (tui-worker tui)
          (bt:make-thread
           (lambda ()
             (let ((outcome (handler-case (run-until-settled (tui-agent tui))
                              (error (e)
                                (push-event tui (list :type :worker-error
                                                      :text (format nil "~a" e)))
                                :error))))
               (push-event tui (list :type :worker-done :outcome outcome))))
           :name "evo-run"))))

(defun submit-to-agent (tui text)
  (scroll tui (format nil "~a~a" (bold (cyan "❯ ")) text))
  (queue-steering (tui-agent tui) text)
  (start-worker tui))

;;; Agent event handling (events arrive from the worker thread via the queue).

(defun handle-agent-event (tui event)
  (case (pget event :type)
    (:text-delta
     (setf (tui-partial tui)
           (concatenate 'string (tui-partial tui) (pget event :text)))
     (loop for pos = (position #\Newline (tui-partial tui))
           while pos
           do (scroll tui (subseq (tui-partial tui) 0 pos))
              (setf (tui-partial tui) (subseq (tui-partial tui) (1+ pos))))
     (setf (tui-dirty tui) t))
    (:thinking-delta
     (let ((tail (concatenate 'string (tui-thinking-tail tui)
                              (substitute #\Space #\Newline (pget event :text)))))
       (setf (tui-thinking-tail tui)
             (if (> (length tail) 60) (subseq tail (- (length tail) 60)) tail))
       (setf (tui-dirty tui) t)))
    (:tool-call-start
     (flush-partial tui)
     (scroll tui (format nil "~a~a" (cyan "⏺ ") (or (pget event :name) "?"))))
    (:tool-result
     (let* ((content (or (pget event :content) ""))
            (first-line (subseq content 0 (or (position #\Newline content)
                                              (length content)))))
       (scroll tui (if (pget event :is-error)
                       (red (format nil "  ⎿ ~a" first-line))
                       (dim (format nil "  ⎿ ~a" first-line))))))
    (:message-end
     (flush-partial tui)
     (setf (tui-thinking-tail tui) "")
     (let ((err (pget event :error)))
       (when err (scroll tui (red (format nil "✗ ~a" err))))))
    (:todo-changed
     (setf (tui-todos tui) (pget event :todos)
           (tui-dirty tui) t))
    (:worker-error
     (scroll tui (red (format nil "✗ internal error in run: ~a" (pget event :text)))))
    (:worker-done
     (flush-partial tui)
     (setf (tui-running tui) nil (tui-worker tui) nil (tui-thinking-tail tui) "")
     (refresh-goal tui)
     (let ((goal (tui-goal tui)))
       (when (and goal (member (pget goal :status) '(:complete :blocked :budget-limited)))
         (scroll tui (yellow (format nil "◆ goal ~a: ~a"
                                     (pget goal :goal-id)
                                     (string-downcase (pget goal :status)))))))
     (setf (tui-dirty tui) t))
    (t nil)))

;;; Region composition.

(defun status-line (tui)
  (let ((goal (tui-goal tui)))
    (dim (format nil "~a · ~a~a~a"
                 (tui-model-label tui) (tui-thinking-label tui)
                 (if goal
                     (format nil " · goal ~a (~(~a~)) ~dk/~dk"
                             (pget goal :goal-id) (pget goal :status)
                             (round (pget goal :tokens-used 0) 1000)
                             (round (pget goal :token-budget 1) 1000))
                     "")
                 (if (tui-running tui) "" " · enter sends · ⇧⏎ newline · /help")))))

(defun spinner-char (tui)
  (char "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" (mod (tui-spinner tui) 10)))

(defun compose-region (tui)
  "Returns (values lines cursor-row cursor-col)."
  (let ((lines nil) (cursor-row 0) (cursor-col 0))
    (when (and (tui-running tui) (plusp (length (tui-partial tui))))
      (push (tui-partial tui) lines))
    (when (tui-running tui)
      (push (dim (format nil "~c ~a  esc interrupt"
                         (spinner-char tui)
                         (if (plusp (length (tui-thinking-tail tui)))
                             (format nil "✻ ~a" (tui-thinking-tail tui))
                             "working")))
            lines))
    (when (and (tui-todo-visible tui) (tui-todos tui)
               (plusp (length (tui-todos tui))))
      (loop for item across (tui-todos tui)
            for i from 0
            when (< i 8)
              do (push (dim (format nil " ~a ~a"
                                    (evo.todo::status-glyph (pget item :status))
                                    (pget item :text)))
                       lines))
      (when (> (length (tui-todos tui)) 8)
        (push (dim (format nil "   +~d more" (- (length (tui-todos tui)) 8))) lines)))
    (setf lines (nreverse lines))
    (case (tui-mode tui)
      (:select
       (let* ((items (tui-select-items tui))
              (index (tui-select-index tui))
              (window 8)
              (start (max 0 (min (- index (floor window 2))
                                 (- (length items) window))))
              (end (min (length items) (+ start window))))
         (setf lines
               (append lines
                       (list (bold (tui-select-title tui)))
                       (loop for i from start below end
                             for (label . nil) = (nth i items)
                             collect (if (= i index)
                                         (reverse-video (format nil " ~a " label))
                                         (format nil "  ~a" label)))
                       (list (dim "↑/↓ move · enter select · esc cancel"))))
         (setf cursor-row (1- (length lines)) cursor-col 0)))
      (t
       (setf lines (append lines (list (status-line tui))))
       (multiple-value-bind (rows crow ccol)
           (eb-display-rows (tui-editor tui) (max 10 (- *cols* 3)))
         (let ((base (length lines)))
           (setf lines
                 (append lines
                         (loop for row in rows
                               for i from 0
                               collect (concatenate 'string
                                                    (if (zerop i) (cyan "❯ ") "  ")
                                                    row))))
           (setf cursor-row (+ base crow)
                 cursor-col (+ 2 ccol))))))
    (values lines cursor-row cursor-col)))

(defun repaint (tui)
  (multiple-value-bind (lines cursor-row cursor-col) (compose-region tui)
    (bt:with-lock-held (*tui-lock*)
      (draw-region lines cursor-row cursor-col)))
  (setf (tui-dirty tui) nil))

;;; History tail on resume.

(defun show-history-tail (tui &key (max-messages 6))
  (let* ((messages (evo.journal:state-messages
                    (fold-state (agent-journal (tui-agent tui)))))
         (tail (last messages max-messages)))
    (dolist (m tail)
      (case (message-role m)
        (:user (let ((text (pget (find :text (message-content m)
                                       :key (lambda (b) (pget b :type)))
                                 :text)))
                 (when text
                   (scroll tui (format nil "~a~a" (bold (cyan "❯ "))
                                       (truncate-string text 500))))))
        (:assistant
         (dolist (block (message-content m))
           (case (pget block :type)
             (:text (scroll tui (truncate-string (pget block :text) 2000)))
             (:tool-call (scroll tui (format nil "~a~a" (cyan "⏺ ")
                                             (pget block :name)))))))
        (t nil)))))

;;; Rewind (double-escape).

(defun rewind-to-last-user (tui)
  (let* ((agent (tui-agent tui))
         (journal (agent-journal agent))
         (path (and (journal-leaf-id journal) (entry-path journal))))
    (let ((entry (find-if (lambda (e)
                            (and (eq (pget e :type) :message)
                                 (eq (pget (pget e :message) :role) :user)))
                          path :from-end t)))
      (cond
        ((null entry) (scroll tui (dim "nothing to rewind")))
        (t
         (setf (journal-leaf-id journal) (pget entry :parent-id))
         (let ((text (pget (find :text (pget (pget entry :message) :content)
                                 :key (lambda (b) (pget b :type)))
                           :text)))
           (when text (eb-set-text (tui-editor tui) text)))
         (refresh-goal tui)
         (scroll tui (yellow "⎌ rewound — edit and resubmit to branch")))))))

;;; Key handling.

(defun handle-key-edit (tui event)
  (let ((eb (tui-editor tui)))
    (cond
      ((consp event)
       (case (first event)
         (:char (eb-insert-char eb (second event)))
         (:paste (eb-paste eb (second event)))
         (:ctrl
          (case (second event)
            (#\c (cond ((not (eb-empty-p eb)) (eb-clear eb))
                       ((< (- (now-ms) (tui-last-ctrl-c tui)) 1200)
                        (setf (tui-quit tui) t))
                       (t (scroll tui (dim "ctrl+c again to quit")))))
            (#\d (when (eb-empty-p eb) (setf (tui-quit tui) t)))
            (#\a (eb-move eb :home))
            (#\e (eb-move eb :end))
            (#\k (eb-kill-line eb))
            (#\u (eb-clear eb))
            (#\w (eb-delete-word eb))
            (#\l (bt:with-lock-held (*tui-lock*)
                   (wr (esc "2J") (esc "H")) (flush)
                   (setf *region-height* 0)))
            (#\j (eb-newline eb))))     ; Ctrl+J: newline everywhere
         (t nil))
       (when (eq (first event) :ctrl)
         (setf (tui-last-ctrl-c tui) (if (eql (second event) #\c) (now-ms) 0))))
      (t
       (case event
         (:enter (submit tui))
         ((:shift-enter :newline) (eb-newline eb))
         (:backspace (eb-backspace eb))
         (:delete (eb-delete eb))
         (:delete-word (eb-delete-word eb))
         ((:up :down :left :right :home :end :word-left :word-right)
          (eb-move eb event))
         (:escape
          (cond ((tui-running tui)
                 (setf (agent-abort-flag (tui-agent tui)) t)
                 (scroll tui (dim "✗ interrupting…")))
                ((< (- (now-ms) (tui-last-esc tui)) 600)
                 (rewind-to-last-user tui))
                (t nil))
          (setf (tui-last-esc tui) (now-ms)))
         (t nil))))
    (setf (tui-dirty tui) t)))

(defun handle-key-select (tui event)
  (let ((n (length (tui-select-items tui))))
    (cond
      ((eq event :up) (setf (tui-select-index tui)
                            (mod (1- (tui-select-index tui)) n)))
      ((eq event :down) (setf (tui-select-index tui)
                              (mod (1+ (tui-select-index tui)) n)))
      ((eq event :enter)
       (let ((item (nth (tui-select-index tui) (tui-select-items tui)))
             (action (tui-select-action tui)))
         (setf (tui-mode tui) :edit)
         (funcall action (cdr item))))
      ((or (eq event :escape) (and (consp event) (eql (second event) #\c)))
       (setf (tui-mode tui) :edit)
       (scroll tui (dim "cancelled"))))
    (setf (tui-dirty tui) t)))

(defun enter-select (tui title items action)
  (setf (tui-select-title tui) title
        (tui-select-items tui) items
        (tui-select-index tui) 0
        (tui-select-action tui) action
        (tui-mode tui) :select
        (tui-dirty tui) t))

;;; Submit + slash command resolution.

(defun submit (tui)
  (let ((text (string-trim '(#\Space #\Newline)
                           (eb-submit-text (tui-editor tui)))))
    (eb-clear (tui-editor tui))
    (cond
      ((zerop (length text)))
      ((and (> (length text) 1) (char= (char text 0) #\/)
            (not (char= (char text 1) #\/)))
       (dispatch-command tui text))
      (t (submit-to-agent tui text)))))

(defun dispatch-command (tui text)
  (let* ((space (position #\Space text))
         (name (subseq text 1 space))
         (args (string-trim " " (if space (subseq text (1+ space)) ""))))
    (cond
      ;; 1. extension commands
      ((gethash name evo::*commands*)
       (let ((fn (pget (gethash name evo::*commands*) :fn)))
         (handler-case
             (let ((result (funcall fn (list :agent (tui-agent tui) :args args :tui tui))))
               (when (stringp result) (scroll tui result))
               (refresh-goal tui))
           (error (e) (scroll tui (red (format nil "✗ /~a: ~a" name e)))))))
      ;; 2. builtins
      ((builtin-command tui name args))
      ;; 3. skills (/skill:name or /name matching a skill)
      ((command-as-skill tui name args))
      ;; 4. prompt templates
      ((command-as-template tui name args))
      (t (scroll tui (dim (format nil "unknown command /~a — /help lists commands" name)))))))

(defun require-idle (tui what)
  (if (tui-running tui)
      (progn (scroll tui (dim (format nil "~a needs an idle agent (esc to interrupt)" what)))
             nil)
      t))
