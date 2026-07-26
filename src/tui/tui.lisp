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
  (agent-mode "auto")   ; cached "mode" custom state: "auto" | "plan"
  (model-label "") (thinking-label "")
  ;; Live context accounting: authoritative at idle (estimated from the
  ;; fold), advanced event-by-event while a run streams so the status line
  ;; moves with every message and tool call.
  (context-tokens 0)
  (context-window nil)
  (goal-run-tokens 0)
  (mode :edit)
  select-title select-items select-index select-action
  ;; Input history (this process, most recent first) + browse state.
  (history nil)
  (history-index nil)   ; nil = not browsing
  (history-draft nil)   ; buffer text stashed when browsing started
  ;; Live /command completion popup state.
  (complete-index 0)
  (complete-prefix nil)
  (complete-dismissed nil)  ; prefix hidden by esc, until it changes
  (complete-candidates nil) ; cached command names while the popup is open
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
         (state (fold-state (agent-journal agent)))
         (model-id (or (evo.journal:state-model state)
                       (agent-model-override agent)
                       (setting :model "claude-sonnet-5"))))
    (setf (tui-goal tui) (evo.journal:state-goal state)
          (tui-todos tui) (custom-state state "todo")
          (tui-agent-mode tui) (or (custom-state state "mode") "auto")
          (tui-model-label tui) model-id
          (tui-context-window tui) (model-context-window (find-model model-id))
          (tui-context-tokens tui) (evo.kernel:estimate-context-tokens
                                    (evo.journal:state-messages state))
          (tui-goal-run-tokens tui) 0
          (tui-thinking-label tui) (string-downcase
                                    (or (evo.journal:state-thinking state)
                                        (agent-thinking-override agent)
                                        (setting :thinking :medium))))))

;;; Plan/auto mode.  The TUI applies the mode policy through the public
;;; extension API (custom state + tool gating + context injection); the
;;; plan-mode extension adds enforcement hooks on top when installed.

(defparameter *plan-mode-tools* '("read" "bash" "get_goal" "todo"))

(defparameter *plan-mode-instructions*
  "PLAN MODE is on. Do not modify anything: no writing or editing files, no
state-changing shell commands. Explore the code, then produce a concrete
step-by-step plan and present it to the user. When the user is satisfied
they will switch you back to auto mode (shift+tab or /mode) to execute.")

(defun current-mode (tui)
  (or (tui-agent-mode tui) "auto"))

(defun set-mode (tui mode)
  (unless (equal mode (current-mode tui))
    (let ((agent (tui-agent tui)))
      (cond
        ((equal mode "plan")
         (evo:set-custom-state "mode" "plan" agent)
         (evo:set-active-tools agent *plan-mode-tools*)
         (evo:inject-context *plan-mode-instructions* :key "plan-mode" :agent agent)
         (scroll tui (yellow "◇ plan mode — read-only; shift+tab or /mode to execute")))
        (t
         (evo:set-custom-state "mode" "auto" agent)
         (evo:set-active-tools agent nil)   ; nil = full tool set
         (scroll tui (yellow "◆ auto mode — full permissions"))))
      (setf (tui-agent-mode tui) mode
            (tui-dirty tui) t))))

(defun toggle-mode (tui)
  (set-mode tui (if (equal (current-mode tui) "plan") "auto" "plan")))

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
     ;; Tool results enter the next request's context: grow the live
     ;; estimate (chars/4, same rule as compaction accounting).
     (incf (tui-context-tokens tui)
           (ceiling (or (pget event :content-chars) 0) 4))
     (let* ((content (or (pget event :content) ""))
            (first-line (subseq content 0 (or (position #\Newline content)
                                              (length content)))))
       (scroll tui (if (pget event :is-error)
                       (red (format nil "  ⎿ ~a" first-line))
                       (dim (format nil "  ⎿ ~a" first-line))))))
    (:message-end
     (flush-partial tui)
     (setf (tui-thinking-tail tui) "")
     ;; Provider-reported usage re-anchors the live context estimate and
     ;; advances the goal's token count for this run.
     (let ((usage (pget event :usage)))
       (when (and usage (plusp (usage-total-tokens usage)))
         (setf (tui-context-tokens tui) (usage-total-tokens usage))
         (incf (tui-goal-run-tokens tui) (usage-total-tokens usage))))
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
     ;; Race guard: input submitted while the worker was settling queues
     ;; steering the run no longer polls — the tick loop handles keys
     ;; before draining events, so restart the worker for it here.
     (when (steering-pending-p (tui-agent tui))
       (start-worker tui))
     (setf (tui-dirty tui) t))
    (t nil)))

;;; Region composition.

(defun fmt-ktokens (n)
  (format nil "~dk" (round n 1000)))

(defun context-label (tui)
  (let ((used (tui-context-tokens tui))
        (window (tui-context-window tui)))
    (if window
        (format nil "ctx ~a/~a (~d%)"
                (fmt-ktokens used) (fmt-ktokens window)
                (min 100 (round (* 100 used) (max 1 window))))
        (format nil "ctx ~a" (fmt-ktokens used)))))

(defun goal-label (goal live-tokens)
  (let ((budget (pget goal :token-budget)))
    (format nil "goal ~a (~(~a~)) ~a~@[/~a~]"
            (pget goal :goal-id) (pget goal :status)
            (fmt-ktokens (+ (pget goal :tokens-used 0) live-tokens))
            (and budget (fmt-ktokens budget)))))

(defun mode-indicator (tui)
  "Leftmost status element; plan mode stands out."
  (if (equal (current-mode tui) "plan")
      (yellow "◇ plan")
      (dim "◆ auto")))

(defun status-line (tui)
  (let ((goal (tui-goal tui)))
    (concatenate 'string
                 (mode-indicator tui)
                 (dim (format nil " · ~a · ~a · ~a~@[ · ~a~]"
                              (tui-model-label tui) (tui-thinking-label tui)
                              (context-label tui)
                              (and goal (goal-label goal (tui-goal-run-tokens tui))))))))

(defparameter *working-frames* "|/-\\"
  "Rotating slash while the agent is executing.")
(defparameter *thinking-frames* "✢✳✶✻✽✻✶✳"
  "Pulsing star while the model is thinking.")
(defparameter *idle-char* #\○)

(defun activity-line (tui)
  "The permanent activity indicator.  Always one line — settling to idle
instead of disappearing, so the region height does not oscillate."
  (cond
    ((and (tui-running tui) (plusp (length (tui-thinking-tail tui))))
     (dim (format nil "~c thinking · ~a  esc interrupt"
                  (char *thinking-frames*
                        (mod (tui-spinner tui) (length *thinking-frames*)))
                  (tui-thinking-tail tui))))
    ((tui-running tui)
     (dim (format nil "~c working  esc interrupt"
                  (char *working-frames*
                        (mod (tui-spinner tui) (length *working-frames*))))))
    (t (dim (format nil "~c idle" *idle-char*)))))

(defun separator-line ()
  (dim (make-string (max 10 (1- *cols*)) :initial-element #\─)))

(defun select-window-height ()
  "Choose boxes are bounded: at most 8 rows, fewer on short terminals."
  (max 3 (min 8 (- *rows* 6))))

(defun compose-region (tui)
  "Returns (values lines cursor-row cursor-col).  Layout: streaming tail,
then each section (activity, todo) opened by a horizontal rule, the editbox
wrapped between two rules, and the model status line under the editbox."
  (let ((lines nil) (cursor-row 0) (cursor-col 0)
        (sep (separator-line)))
    (when (and (tui-running tui) (plusp (length (tui-partial tui))))
      (push (tui-partial tui) lines))
    (push sep lines)
    (push (activity-line tui) lines)
    (when (and (tui-todo-visible tui) (tui-todos tui)
               (plusp (length (tui-todos tui))))
      (push sep lines)
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
              (n (length items))
              (window (select-window-height))
              (start (max 0 (min (- index (floor window 2)) (- n window))))
              (end (min n (+ start window)))
              (width (loop for item in items
                           maximize (length (select-item-label item)))))
         (setf lines
               (append lines
                       (list sep
                             (bold (if (> n window)
                                       (format nil "~a ~d/~d"
                                               (tui-select-title tui) (1+ index) n)
                                       (tui-select-title tui))))
                       (loop for i from start below end
                             for item = (nth i items)
                             for label = (select-item-label item)
                             for desc = (select-item-desc item)
                             ;; both prefixes are 2 columns wide and the dim
                             ;; description sits in its own aligned column
                             collect (concatenate
                                      'string
                                      (if (= i index)
                                          (reverse-video (format nil "● ~a" label))
                                          (format nil "  ~a" label))
                                      (if (plusp (length (or desc "")))
                                          (concatenate
                                           'string
                                           (make-string (+ 2 (- width (length label)))
                                                        :initial-element #\Space)
                                           (dim desc))
                                          "")))
                       (list (dim "↑/↓ move · enter select · esc cancel") sep)))
         (setf cursor-row (1- (length lines)) cursor-col 0)))
      (t
       (multiple-value-bind (rows crow ccol)
           (eb-display-rows (tui-editor tui) (max 10 (- *cols* 3)))
         (multiple-value-bind (cprefix cmatches) (completion-context tui)
           (declare (ignore cprefix))
           (let ((base (1+ (length lines)))) ; editor rows start after the top rule
             (setf lines
                   (append lines
                           (list sep)
                           (loop for row in rows
                                 for i from 0
                                 collect (concatenate 'string
                                                      (if (zerop i) (cyan "❯ ") "  ")
                                                      row))
                           (when cmatches (completion-rows tui cmatches))
                           (list sep (status-line tui))))
             (setf cursor-row (+ base crow)
                   cursor-col (+ 2 ccol)))))))
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

;;; Slash-command completion (Tab).

(defparameter *builtin-commands*
  '(("help" . "commands and keys")
    ("goal" . "show, create, or refine the goal")
    ("todo" . "toggle the todo panel")
    ("mode" . "switch auto/plan mode (shift+tab toggles)")
    ("model" . "pick the model from a list, or set it directly")
    ("thinking" . "off·low·medium·high·xhigh")
    ("compact" . "compact the context now")
    ("lore" . "show lore, or add durable guidance")
    ("tree" . "navigate entries, move the leaf (rewind/branch)")
    ("resume" . "switch to another session")
    ("fork" . "fork this session at the current leaf")
    ("new" . "start a fresh session")
    ("export" . "export the transcript as markdown")
    ("reload" . "reload extension directories")
    ("quit" . "exit")
    ("exit" . "same as /quit")))

(defun template-names ()
  (loop for dir in (evo.kernel::template-directories)
        append (mapcar #'pathname-name
                       (ignore-errors (directory (merge-pathnames "*.md" dir))))))

(defun all-commands ()
  "Completion candidates as (name . description), mirroring dispatch order:
extension commands, builtins, skills, prompt templates (first wins)."
  (sort (remove-duplicates
         (append (loop for name being the hash-keys of evo::*commands*
                         using (hash-value cmd)
                       collect (cons name (or (pget cmd :description)
                                              "extension command")))
                 (copy-alist *builtin-commands*)
                 (mapcar (lambda (s) (cons (pget s :name)
                                           (or (pget s :description) "skill")))
                         (available-skills))
                 (mapcar (lambda (name) (cons name "prompt template"))
                         (template-names)))
         :key #'car :test #'string= :from-end t)
        #'string< :key #'car))

(defparameter *completion-max-rows* 6)

(defun command-word-p (eb)
  "The buffer is a single line holding just a /command word being typed."
  (let ((line (first (eb-lines eb))))
    (and (= 1 (length (eb-lines eb)))
         (plusp (length line))
         (char= (char line 0) #\/)
         (not (find #\Space line)))))

(defun completion-context (tui)
  "Live popup state: (values prefix matches) while a /command word is being
typed and suggestions exist; nil otherwise.  Esc hides the popup until the
prefix changes.  Candidates are cached for the popup's lifetime."
  (let ((eb (tui-editor tui)))
    (cond
      ((not (command-word-p eb))
       (setf (tui-complete-candidates tui) nil
             (tui-complete-prefix tui) nil
             (tui-complete-dismissed tui) nil)
       nil)
      (t
       (let ((prefix (subseq (first (eb-lines eb)) 1)))
         (unless (equal prefix (tui-complete-prefix tui))
           (setf (tui-complete-prefix tui) prefix
                 (tui-complete-index tui) 0)
           (unless (equal prefix (tui-complete-dismissed tui))
             (setf (tui-complete-dismissed tui) nil)))
         (unless (tui-complete-dismissed tui)
           (let ((matches (remove-if-not
                           (lambda (entry) (string-prefix-p prefix (car entry)))
                           (or (tui-complete-candidates tui)
                               (setf (tui-complete-candidates tui)
                                     (all-commands))))))
             (when matches (values prefix matches)))))))))

(defun completion-rows (tui matches)
  "Bounded popup rows under the editor: a window of (name . description)
suggestions around the selection, descriptions dim in an aligned column,
plus an overflow indicator."
  (let* ((n (length matches))
         (window (max 3 (min *completion-max-rows* (- *rows* 8))))
         (index (min (tui-complete-index tui) (1- n)))
         (start (max 0 (min (- index (floor window 2)) (- n window))))
         (end (min n (+ start window)))
         (width (loop for (name . nil) in matches maximize (length name))))
    (setf (tui-complete-index tui) index)
    (append
     (loop for i from start below end
           for (name . desc) = (nth i matches)
           collect (concatenate
                    'string
                    (if (= i index)
                        (reverse-video (format nil "● /~a" name))
                        (format nil "  /~a" name))
                    (if (plusp (length (or desc "")))
                        (concatenate 'string
                                     (make-string (+ 2 (- width (length name)))
                                                  :initial-element #\Space)
                                     (dim desc))
                        "")))
     (when (> n window)
       (list (dim (format nil "  … ~d/~d" (1+ index) n)))))))

(defun accept-completion (tui name)
  "Replace the command word with NAME, ready for arguments."
  (eb-set-text (tui-editor tui) (format nil "/~a " name)))

(defun complete-command (tui)
  "Tab: accept the popup's highlighted command.  Re-shows a popup hidden by
esc.  Outside a /command word, Tab stays a literal tab character."
  (setf (tui-complete-dismissed tui) nil)
  (multiple-value-bind (prefix matches) (completion-context tui)
    (declare (ignore prefix))
    (cond
      (matches
       (accept-completion tui (car (nth (tui-complete-index tui) matches))))
      ((command-word-p (tui-editor tui))
       (scroll tui (dim (format nil "no command matches ~a"
                                (first (eb-lines (tui-editor tui)))))))
      (t (eb-insert-char (tui-editor tui) #\Tab)))))

;;; Input history: up/down recall previous submissions when the cursor
;;; cannot move further within the buffer's own lines.

(defun history-reset-browse (tui)
  (setf (tui-history-index tui) nil (tui-history-draft tui) nil))

(defun history-remember (tui text)
  (unless (equal text (first (tui-history tui)))
    (push text (tui-history tui)))
  (history-reset-browse tui))

(defun history-prev (tui)
  (let ((eb (tui-editor tui))
        (hist (tui-history tui))
        (index (tui-history-index tui)))
    (cond ((null hist))
          ((null index)
           (setf (tui-history-draft tui) (eb-text eb)
                 (tui-history-index tui) 0)
           (eb-set-text eb (first hist)))
          ((< index (1- (length hist)))
           (incf (tui-history-index tui))
           (eb-set-text eb (nth (tui-history-index tui) hist))))))

(defun history-next (tui)
  (let ((eb (tui-editor tui))
        (index (tui-history-index tui)))
    (when index
      (if (zerop index)
          (progn (eb-set-text eb (or (tui-history-draft tui) ""))
                 (history-reset-browse tui))
          (progn (decf (tui-history-index tui))
                 (eb-set-text eb (nth (tui-history-index tui) (tui-history tui))))))))

(defun edit-up (tui)
  "Completion popup first, then line motion, then history."
  (multiple-value-bind (prefix matches) (completion-context tui)
    (declare (ignore prefix))
    (let ((eb (tui-editor tui)))
      (cond
        (matches (setf (tui-complete-index tui)
                       (mod (1- (tui-complete-index tui)) (length matches))))
        ((plusp (eb-line eb)) (eb-move eb :up))
        (t (history-prev tui))))))

(defun edit-down (tui)
  (multiple-value-bind (prefix matches) (completion-context tui)
    (declare (ignore prefix))
    (let ((eb (tui-editor tui)))
      (cond
        (matches (setf (tui-complete-index tui)
                       (mod (1+ (tui-complete-index tui)) (length matches))))
        ((< (eb-line eb) (1- (length (eb-lines eb)))) (eb-move eb :down))
        (t (history-next tui))))))

;;; Key handling.

(defun handle-key-edit (tui event)
  (let ((eb (tui-editor tui)))
    (cond
      ((consp event)
       (case (first event)
         (:char (if (char= (second event) #\Tab)
                    (complete-command tui)
                    (eb-insert-char eb (second event))))
         (:paste (eb-paste eb (second event)))
         (:ctrl
          (case (second event)
            (#\c (cond ((not (eb-empty-p eb))
                        (eb-clear eb)
                        (history-reset-browse tui))
                       ((< (- (now-ms) (tui-last-ctrl-c tui)) 1200)
                        (setf (tui-quit tui) t))
                       (t (scroll tui (dim "ctrl+c again to quit")))))
            (#\d (if (eb-empty-p eb)    ; readline: delete-char, EOF when empty
                     (setf (tui-quit tui) t)
                     (eb-delete eb)))
            (#\a (eb-move eb :home))
            (#\e (eb-move eb :end))
            (#\b (eb-move eb :left))
            (#\f (eb-move eb :right))
            (#\p (edit-up tui))
            (#\n (edit-down tui))
            (#\k (eb-kill-line eb))
            (#\u (eb-clear eb) (history-reset-browse tui))
            (#\w (eb-delete-word eb))
            (#\l (bt:with-lock-held (*tui-lock*)
                   (wr (esc "2J") (esc "H")) (flush)
                   (setf *region-height* 0 *region-cursor-row* 0)))
            (#\i (complete-command tui)) ; Ctrl+I = Tab on some terminals
            (#\j (eb-newline eb))))     ; Ctrl+J: newline everywhere
         (t nil))
       (when (eq (first event) :ctrl)
         (setf (tui-last-ctrl-c tui) (if (eql (second event) #\c) (now-ms) 0))))
      (t
       (case event
         (:enter (submit tui))
         ((:shift-enter :newline) (eb-newline eb))
         (:shift-tab (toggle-mode tui))
         (:backspace (eb-backspace eb))
         (:delete (eb-delete eb))
         (:delete-word (eb-delete-word eb))
         (:up (edit-up tui))
         (:down (edit-down tui))
         ((:left :right :home :end :word-left :word-right)
          (eb-move eb event))
         (:escape
          (let ((popup-prefix (completion-context tui)))
            (cond
              (popup-prefix        ; first esc just hides the popup
               (setf (tui-complete-dismissed tui) popup-prefix
                     (tui-last-esc tui) 0))
              (t
               (cond ((tui-running tui)
                      (setf (agent-abort-flag (tui-agent tui)) t)
                      (scroll tui (dim "✗ interrupting…")))
                     ((< (- (now-ms) (tui-last-esc tui)) 600)
                      (rewind-to-last-user tui))
                     (t nil))
               (setf (tui-last-esc tui) (now-ms))))))
         (t nil))))
    (setf (tui-dirty tui) t)))

;; Select items: (label . value) or (label value description) — the
;; description renders dim in an aligned column.
(defun select-item-label (item) (first item))
(defun select-item-value (item) (if (consp (cdr item)) (second item) (cdr item)))
(defun select-item-desc (item) (and (consp (cdr item)) (third item)))

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
         (funcall action (select-item-value item))))
      ((or (eq event :escape) (and (consp event) (eql (second event) #\c)))
       (setf (tui-mode tui) :edit)
       (scroll tui (dim "cancelled"))))
    (setf (tui-dirty tui) t)))

(defun enter-select (tui title items action &key (index 0))
  (setf (tui-select-title tui) title
        (tui-select-items tui) items
        (tui-select-index tui) (min (max 0 index) (1- (length items)))
        (tui-select-action tui) action
        (tui-mode tui) :select
        (tui-dirty tui) t))

;;; Submit + slash command resolution.

(defun submit (tui)
  ;; Enter with the popup open on a partial command word accepts the
  ;; highlighted suggestion instead of submitting the fragment.
  (multiple-value-bind (prefix matches) (completion-context tui)
    (when (and matches (not (member prefix matches :key #'car :test #'string=)))
      (accept-completion tui (car (nth (tui-complete-index tui) matches)))
      (return-from submit)))
  (let ((text (string-trim '(#\Space #\Newline)
                           (eb-submit-text (tui-editor tui)))))
    (eb-clear (tui-editor tui))
    (cond
      ((zerop (length text)))
      (t
       (history-remember tui text)
       (if (and (> (length text) 1) (char= (char text 0) #\/)
                (not (char= (char text 1) #\/)))
           (dispatch-command tui text)
           (submit-to-agent tui text))))))

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
