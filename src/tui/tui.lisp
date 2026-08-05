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
  (md (make-md))        ; markdown fence state for the streaming text
  (thinking-tail "")
  (compacting nil)
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
  (recalled-text nil)   ; text history put in the buffer; no popup while it stands
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
    (scroll tui (md-render-line (tui-partial tui) (tui-md tui)))
    (setf (tui-partial tui) "")))

(defun refresh-goal (tui &key (reset-goal-run-tokens t))
  "Re-derive the cached fold state (goal, todos, model/thinking labels).
Compose-region uses only these caches: repaints must not fold the journal
while the run thread is appending to it."
  (let* ((agent (tui-agent tui))
         (state (fold-state (agent-journal agent)))
         ;; Defensive: a missing/unregistered model (e.g. /reload removed
         ;; it) must not kill the render thread — /model is the recovery.
         (model-id (handler-case (evo.kernel:effective-model-id state agent)
                     (error () nil)))
         ;; Resolve once: the label, the context window and the run all have
         ;; to agree on WHICH registration is live when an id is served by
         ;; several providers.
         (model (and model-id
                     (handler-case
                         (find-model model-id
                                     (evo.kernel:effective-model-provider state model-id))
                       (error () nil)))))
    (setf (tui-goal tui) (evo.journal:state-goal state)
          (tui-todos tui) (custom-state state "todo")
          (tui-agent-mode tui) (or (custom-state state "mode")
                                   evo.plan:*default-mode*)
          ;; Same id under several providers: name the active one, since the
          ;; bare id no longer identifies the endpoint on its own.
          (tui-model-label tui) (cond ((null model-id) "(no model)")
                                      ((and model (cdr (model-providers model-id)))
                                       (format nil "~a (~(~a~))" model-id
                                               (pget model :provider)))
                                      (t model-id))
          (tui-context-window tui) (and model (model-context-window model))
          (tui-context-tokens tui) (evo.kernel:estimate-context-tokens
                                    (evo.journal:state-messages state))
          (tui-thinking-label tui) (string-downcase
                                    (evo.kernel:effective-thinking
                                     state (agent-thinking-override agent))))
    (when reset-goal-run-tokens
      (setf (tui-goal-run-tokens tui) 0))))

;;; Plan/auto mode.  Policy — tool gating, instruction injection, the
;;; enforcement hooks — belongs to the plan-mode core extension (EVO.PLAN);
;;; the TUI is presentation: switch, announce, indicate.

(defun current-mode (tui)
  (or (tui-agent-mode tui) evo.plan:*default-mode*))

(defun mode-announcement (mode)
  (yellow (if (equal mode "plan")
              "◇ plan mode — read-only; shift+tab or /permission to execute"
              "◆ auto mode — full permissions")))

(defun set-mode (tui mode)
  "Switch modes and announce it.  A switch to the current mode is a no-op —
EVO.PLAN:SET-MODE returns NIL and nothing is journaled or scrolled."
  (let ((mode (evo.plan:set-mode mode (tui-agent tui))))
    (when mode
      (setf (tui-agent-mode tui) mode
            (tui-dirty tui) t)
      (scroll tui (mode-announcement mode)))))

(defun toggle-mode (tui)
  (set-mode tui (if (equal (current-mode tui) "plan") "auto" "plan")))

;;; Worker thread.

(defun check-model-ready (tui)
  "Non-fatal counterpart of the CLI's headless preflight: T when the
session's effective model resolves in the registry.  On failure scroll
the config error and the recovery path — the TUI must stay up, /model
and /reload are how the registry gets fixed."
  (let ((agent (tui-agent tui)))
    (handler-case
        (let* ((state (fold-state (agent-journal agent)))
               (id (evo.kernel:effective-model-id state agent)))
          (find-model id (evo.kernel:effective-model-provider state id))
          t)
      (error (e)
        (scroll tui (red (format nil "✗ ~a" e)))
        (scroll tui (dim "recover with /model (pick a registered model) or /reload (after fixing init.lisp)"))
        nil))))

(defun start-worker (tui)
  (unless (tui-running tui)
    ;; Model gate, before anything reaches the journal: queued steering
    ;; is memory-only until the run drains it, so a blocked submit is
    ;; not lost — /model or /reload releases it.
    (unless (check-model-ready tui)
      (when (steering-pending-p (tui-agent tui))
        (scroll tui (dim "input stays queued — it runs once the model resolves")))
      (return-from start-worker))
    (setf (tui-running tui) t
          (tui-compacting tui) nil
          (agent-abort-flag (tui-agent tui)) nil)
    (setf (tui-worker tui)
          (bt:make-thread
           (lambda ()
             ;; Self-heal invariant: :worker-done ALWAYS arrives.  The
             ;; handler catches SERIOUS-CONDITION (not just ERROR — think
             ;; storage exhaustion), and the unwind-protect covers exits
             ;; handler-case cannot see (thread interrupts, implementation
             ;; aborts): a lost :worker-done leaves the TUI "running"
             ;; forever with no worker behind it — the frozen-TUI bug.
             (let ((outcome :error))
               (unwind-protect
                    (setf outcome
                          (handler-case (run-until-settled (tui-agent tui))
                            (serious-condition (e)
                              (push-event tui (list :type :worker-error
                                                    :text (format nil "~a" e)))
                              :error)))
                 (push-event tui (list :type :worker-done :outcome outcome)))))
           :name "evo-run"))))

(defun start-compact-worker (tui hint)
  "Run manual compaction on the worker thread so the TUI can keep repainting
and ESC can interrupt the summarization request."
  (unless (tui-running tui)
    (unless (check-model-ready tui)     ; summarization needs the model too
      (return-from start-compact-worker))
    (let ((agent (tui-agent tui)))
      (setf (tui-running tui) t
            (tui-compacting tui) t
            (agent-abort-flag agent) nil)
      (setf (tui-worker tui)
            (bt:make-thread
             (lambda ()
               (let ((outcome :error))
                 (unwind-protect
                      (progn
                        (emit-event agent :type :compaction-start)
                        (setf outcome
                              (handler-case
                                  (progn
                                    (evo.kernel:compact-now agent :hint hint)
                                    (if (agent-abort-flag agent)
                                        (progn
                                          (push-event tui (list :type :compact-result
                                                                :outcome :aborted))
                                          :aborted)
                                        (progn
                                          (push-event tui (list :type :compact-result
                                                                :outcome :stop))
                                          :stop)))
                                (serious-condition (e)
                                  (if (agent-abort-flag agent)
                                      (progn
                                        (push-event tui (list :type :compact-result
                                                              :outcome :aborted))
                                        :aborted)
                                      (progn
                                        (push-event tui (list :type :compact-result
                                                              :outcome :error
                                                              :text (format nil "~a" e)))
                                        :error))))))
                   (emit-event agent :type :compaction-end)
                   (push-event tui (list :type :worker-done :outcome outcome)))))
             :name "evo-compact")))))

(defun user-prompt-block (text)
  "User prompts sit between two rules in scrollback — mirroring the
editbox they were typed in — so they stand out when scanning history."
  (let ((sep (separator-line)))
    (format nil "~a~%~a~a~%~a" sep (bold (cyan "❯ ")) text sep)))

(defun submit-to-agent (tui text)
  (scroll tui (user-prompt-block text))
  (queue-steering (tui-agent tui) text)
  (start-worker tui))

;;; Tool call display formatting.

(defparameter *tool-call-max-width* 80
  "Max rendered width for a tool-call line before truncation.")

(defparameter *tool-key-args*
  '(("bash" . ("command"))
    ("read" . ("path"))
    ("write" . ("path"))
    ("edit" . ("path" "old_string"))
    ("create_goal" . ("objective"))
    ("update_goal" . ("status"))
    ("todo" . ("items"))
    ("load_extension" . ("path")))
  "Alist mapping tool name -> list of key argument names to show.")

(defun tool-arg-value (arguments name)
  "Value for argument NAME in an arguments plist.  Provider JSON keys
land as hyphenated keywords (\"old_string\" -> :OLD-STRING), while
*tool-key-args* names keep the schema's underscores — fold case and _/-
so both spellings match."
  (flet ((canon (s) (substitute #\- #\_ (string-upcase s))))
    (loop for (k v) on arguments by #'cddr
          when (and (symbolp k) (equal (canon (string k)) (canon name)))
            return v)))

(defun format-tool-call-plain (name arguments)
  "Format a tool call as one line, no ANSI: ⏺ name(key=\"val\", ...),
truncated at *tool-call-max-width*.  Total by construction: this renders
inside the TUI tick loop and on session resume, so malformed ARGUMENTS
(non-list, dotted, odd-length) degrade to the bare name — never signal."
  (or (ignore-errors
        (let* ((arguments (and (listp arguments) arguments))
               (keys (or (cdr (assoc name *tool-key-args* :test #'equal))
                         (loop for k in arguments by #'cddr
                               when (symbolp k)
                                 collect (substitute #\_ #\-
                                                     (string-downcase (string k))))))
               (arg-strs
                 (loop for key in keys
                       for val = (tool-arg-value arguments key)
                       when val
                         collect (format nil "~a=~a" (string-downcase key)
                                         (substitute #\Space #\Newline
                                                     (format nil "~s" val))))))
          (truncate-string
           (if arg-strs
               (format nil "⏺ ~a(~{~a~^, ~})" name arg-strs)
               (format nil "⏺ ~a" name))
           *tool-call-max-width* "…")))
      (format nil "⏺ ~a" name)))

(defun format-tool-call (name arguments)
  "FORMAT-TOOL-CALL-PLAIN in scrollback colors."
  (cyan (format-tool-call-plain name arguments)))

(defun interrupt-run (tui)
  "Interrupt the active run and unblock the worker's current operation."
  (when (tui-running tui)
    (request-abort (tui-agent tui))
    (scroll tui (dim "✗ interrupting…"))
    t))

;;; Agent event handling (events arrive from the worker thread via the queue).

(defun handle-agent-event (tui event)
  (case (pget event :type)
    (:text-delta
     (setf (tui-partial tui)
           (concatenate 'string (tui-partial tui) (pget event :text)))
     (loop for pos = (position #\Newline (tui-partial tui))
           while pos
           do (scroll tui (md-render-line (subseq (tui-partial tui) 0 pos)
                                          (tui-md tui)))
              (setf (tui-partial tui) (subseq (tui-partial tui) (1+ pos))))
     (setf (tui-dirty tui) t))
    (:thinking-delta
     (let* ((tail (concatenate 'string (tui-thinking-tail tui)
                               (substitute #\Space #\Newline (pget event :text))))
            ;; Reserve room for the fixed parts of the activity line:
            ;; "✳ thinking · " (14 cols — · renders as 2 in CJK terminals)
            ;; + "  esc interrupt " (16 cols, trailing space for breathing room)
            ;; = 30 cols.
            (budget (max 10 (- *cols* 30))))
       (setf (tui-thinking-tail tui)
             (if (> (length tail) budget) (subseq tail (- (length tail) budget)) tail))
       (setf (tui-dirty tui) t)))
    (:tool-call-start
     (flush-partial tui)
     (scroll tui (format-tool-call (or (pget event :name) "?")
                                   (pget event :arguments))))
    (:tool-result
     ;; Tool results enter the next request's context: grow the live
     ;; estimate (chars/4, same rule as compaction accounting).
     (incf (tui-context-tokens tui)
           (ceiling (or (pget event :content-chars) 0) 4))
     (let* ((content (string-right-trim '(#\Newline)
                                        (or (pget event :content) "")))
            (lines (or (uiop:split-string content :separator '(#\Newline))
                       (list "")))
            (shown (subseq lines 0 (min 3 (length lines))))
            (hidden (- (length lines) (length shown)))
            (paint (if (pget event :is-error) #'red #'dim)))
       (scroll tui
               (format nil "~{~a~^~%~}"
                       (append
                        (loop for line in shown
                              for prefix = "  ⎿ " then "    "
                              collect (funcall paint
                                               (concatenate 'string prefix line)))
                        (when (plusp hidden)
                          (list (funcall paint
                                         (format nil "    … +~d line~:p" hidden)))))))))
    (:message-end
     (flush-partial tui)
     (setf (tui-md tui) (make-md))      ; fences don't leak across messages
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
    (:compaction-start
     (setf (tui-compacting tui) t
           (tui-dirty tui) t))
    (:compaction-end
     (setf (tui-compacting tui) nil)
     (refresh-goal tui :reset-goal-run-tokens nil)
     (setf (tui-dirty tui) t))
    (:compact-result
     (case (pget event :outcome)
       (:stop (scroll tui (green "✓ compacted")))
       (:aborted (scroll tui (dim "✗ compact interrupted")))
       (:error (scroll tui (red (format nil "✗ compact: ~a" (pget event :text)))))))
    (:worker-error
     (scroll tui (red (format nil "✗ internal error in run: ~a" (pget event :text)))))
    (:worker-done
     ;; Reset the run state FIRST: if any of the rendering below signals,
     ;; the TUI must already know the worker is gone (a stuck running=t
     ;; with no worker means no run can ever start again).
     (setf (tui-running tui) nil
           (tui-worker tui) nil
           (tui-compacting tui) nil
           (tui-thinking-tail tui) "")
     (flush-partial tui)
     (setf (tui-md tui) (make-md))
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
    ((and (tui-running tui) (tui-compacting tui))
     (dim (format nil "~c compacting...  esc interrupt"
                  (char *working-frames*
                        (mod (tui-spinner tui) (length *working-frames*))))))
    ((and (tui-running tui) (plusp (length (tui-thinking-tail tui))))
     (dim (format nil "~c thinking · ~a  esc interrupt "
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
      ;; Soft-wrap the still-streaming line so a long paragraph shows in full
      ;; instead of a single truncated row.  Re-wrapped every repaint against
      ;; the current *cols*, so it tracks resizes; bounded to the tail so a
      ;; paragraph longer than the screen can't push the editor off it and
      ;; desync the region's relative-cursor math.
      (let* ((rows (wrap-visible (md-render-preview (tui-partial tui) (tui-md tui))
                                 (1- *cols*)))
             (cap (max 1 (- *rows* 8)))
             (rows (if (> (length rows) cap)
                       (nthcdr (- (length rows) cap) rows)
                       rows)))
        (dolist (row rows) (push row lines))))
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
         (multiple-value-bind (cprefix cmatches ckind) (completion-context tui)
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
                           (when cmatches (completion-rows tui cmatches ckind))
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
                   (scroll tui (user-prompt-block (truncate-string text 500))))))
        (:assistant
         (dolist (block (message-content m))
           (case (pget block :type)
             (:text (scroll tui (md-render-text
                                 (truncate-string (pget block :text) 2000))))
             (:tool-call (scroll tui (format-tool-call (pget block :name)
                                                         (pget block :arguments)))))))
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

(defun mode-argument-command-p (name)
  "Mode names are accepted only as /permission arguments, never as standalone
/commands — the registry of them is EVO.PLAN:*MODES*."
  (and (evo.plan:mode-name name) t))

(defparameter *builtin-commands*
  '(("help" . "commands and keys")
    ("goal" . "show, create, or refine the goal")
    ("todo" . "toggle the todo panel")
    ("permission" . "switch auto/plan mode (shift+tab toggles)")
    ("model" . "pick the model from a list, or set it directly")
    ("thinking" . "low·medium·high·xhigh·max")
    ("compact" . "compact the context now")
    ("lore" . "show lore, or add project-scope guidance")
    ("global-lore" . "show lore, or add user-scope guidance")
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
  (sort (remove-if (lambda (entry) (mode-argument-command-p (car entry)))
                   (remove-duplicates
                    (append (loop for name being the hash-keys of evo::*commands*
                                    using (hash-value cmd)
                                  collect (cons name (or (pget cmd :description)
                                                         "extension command")))
                            (copy-alist *builtin-commands*)
                            (mapcar (lambda (s) (cons (format nil "skill:~a" (pget s :name))
                                                      (or (pget s :description) "skill")))
                                    (available-skills))
                            (mapcar (lambda (name) (cons name "prompt template"))
                                    (template-names)))
                    :key #'car :test #'string= :from-end t))
        #'string< :key #'car))

(defparameter *completion-max-rows* 6)

(defun command-word-p (eb)
  "The buffer is a single line holding just a /command word being typed."
  (let ((line (first (eb-lines eb))))
    (and (= 1 (length (eb-lines eb)))
         (plusp (length line))
         (char= (char line 0) #\/)
         (not (find #\Space line)))))

(defparameter *symbol-completion-command* "eval"
  "The command whose content completes against the image rather than
against a name list.  Its content is Lisp being typed into the running
image, so the candidates are that image's own functions and variables —
EVO.EVAL decides what qualifies, the popup only renders it.")

(defun symbol-completion-buffer-p (eb)
  "The buffer is an invocation of the symbol-completing command, past its
command word (the space is what ends the word and starts the content)."
  (let* ((line (first (eb-lines eb)))
         (space (position #\Space line)))
    (and space
         (plusp (length line))
         (char= (char line 0) #\/)
         (string-equal *symbol-completion-command* (subseq line 1 space)))))

(defun symbol-token-at-cursor (eb)
  "The symbol token being typed at the cursor: (values TOKEN START), or NIL
when the cursor is not in completable content.  The command word itself is
never completed over — on the first line the content starts after it."
  (when (symbol-completion-buffer-p eb)
    (let* ((line (eb-current-line eb))
           (end (eb-col eb))
           (start (evo.eval:token-start line end)))
      (unless (and (zerop (eb-line eb))
                   (<= start (position #\Space (first (eb-lines eb)))))
        (values (subseq line start end) start)))))

(defun completion-target (eb)
  "What the cursor sits on: (values PREFIX KIND), KIND being :command for a
/command word or :symbol for content inside the symbol-completing command.
NIL when there is nothing to complete."
  (if (command-word-p eb)
      (values (subseq (first (eb-lines eb)) 1) :command)
      (let ((token (symbol-token-at-cursor eb)))
        (when token (values token :symbol)))))

(defun completion-matches (tui kind prefix)
  "Candidates for PREFIX as (name . description).  The command list is
fixed for the popup's lifetime and cached; symbols are asked of the image
on every keystroke, since evaluating can define more of them."
  (ecase kind
    (:command
     (remove-if-not (lambda (entry) (string-prefix-p prefix (car entry)))
                    (or (tui-complete-candidates tui)
                        (setf (tui-complete-candidates tui) (all-commands)))))
    (:symbol (evo.eval:completions-for prefix))))

(defun completion-context (tui)
  "Live popup state: (values prefix matches kind) while something is being
typed that has suggestions; nil otherwise.  Only new input raises a popup —
content up/down recalled has none until it is edited.  Esc hides the popup
until the prefix changes."
  (let ((eb (tui-editor tui)))
    (multiple-value-bind (prefix kind) (completion-target eb)
      (cond
        ((recalled-buffer-p tui) nil)
        ((null kind)
         (setf (tui-complete-candidates tui) nil
               (tui-complete-prefix tui) nil
               (tui-complete-dismissed tui) nil)
         nil)
        (t
         (unless (eq kind :command)
           (setf (tui-complete-candidates tui) nil))
         (unless (equal prefix (tui-complete-prefix tui))
           (setf (tui-complete-prefix tui) prefix
                 (tui-complete-index tui) 0)
           (unless (equal prefix (tui-complete-dismissed tui))
             (setf (tui-complete-dismissed tui) nil)))
         (unless (tui-complete-dismissed tui)
           (let ((matches (completion-matches tui kind prefix)))
             (unless (completion-settled-p prefix matches)
               (when matches (values prefix matches kind))))))))))

(defun completion-settled-p (prefix matches)
  "MATCHES leave nothing to choose: the only candidate is the word already
typed.  Such a popup shows the user their own input back, and — worse —
captures the up/down presses that would otherwise browse history.  So a
name completes itself out of existence the moment it is whole."
  (and matches
       (null (rest matches))
       (string= prefix (car (first matches)))))

(defun completion-label (kind name)
  "How a candidate reads in the popup: a command wears its slash, a symbol
is shown exactly as it would be typed."
  (if (eq kind :command) (concatenate 'string "/" name) name))

(defun completion-rows (tui matches kind)
  "Bounded popup rows under the editor: a window of (name . description)
suggestions around the selection, descriptions dim in an aligned column,
plus an overflow indicator."
  (let* ((n (length matches))
         (window (max 3 (min *completion-max-rows* (- *rows* 8))))
         (index (min (tui-complete-index tui) (1- n)))
         (start (max 0 (min (- index (floor window 2)) (- n window))))
         (end (min n (+ start window)))
         (width (loop for (name . nil) in matches
                      maximize (length (completion-label kind name)))))
    (setf (tui-complete-index tui) index)
    (append
     (loop for i from start below end
           for (name . desc) = (nth i matches)
           for label = (completion-label kind name)
           collect (concatenate
                    'string
                    (if (= i index)
                        (reverse-video (format nil "● ~a" label))
                        (format nil "  ~a" label))
                    (if (plusp (length (or desc "")))
                        (concatenate 'string
                                     (make-string (+ 2 (- width (length label)))
                                                  :initial-element #\Space)
                                     (dim desc))
                        "")))
     (when (> n window)
       (list (dim (format nil "  … ~d/~d" (1+ index) n)))))))

(defun accept-completion (tui name kind)
  "Put NAME in the buffer: a command replaces the whole word and opens its
argument; a symbol replaces just the token under the cursor, leaving the
rest of the form — and the closing parens — alone."
  (let ((eb (tui-editor tui)))
    (if (eq kind :command)
        (eb-set-text eb (format nil "/~a " name))
        (multiple-value-bind (token start) (symbol-token-at-cursor eb)
          (declare (ignore token))
          (let ((line (eb-current-line eb)))
            (setf (eb-current-line eb)
                  (concatenate 'string (subseq line 0 start) name
                               (subseq line (eb-col eb)))
                  (eb-col eb) (+ start (length name))))))))

(defun complete-at-point (tui)
  "Tab: accept the popup's highlighted candidate — a /command word, or a
function or variable inside the symbol-completing command.  Re-shows a
popup hidden by esc, or withheld from recalled content: asking for
completion outright is new input, whatever put the text there.  With
nothing to complete, Tab stays a literal tab."
  (setf (tui-complete-dismissed tui) nil
        (tui-recalled-text tui) nil)
  (multiple-value-bind (prefix matches kind) (completion-context tui)
    (declare (ignore prefix))
    (if matches
        (accept-completion tui (car (nth (tui-complete-index tui) matches)) kind)
        (multiple-value-bind (target target-kind) (completion-target (tui-editor tui))
          (cond
            ((null target-kind) (eb-insert-char (tui-editor tui) #\Tab))
            ;; Already whole: there is nothing to add and nothing to report.
            ((completion-settled-p target (completion-matches tui target-kind target)))
            ((eq target-kind :command)
             (scroll tui (dim (format nil "no command matches /~a" target))))
            ((plusp (length target))
             (scroll tui (dim (format nil "no function or variable matches ~a" target))))
            (t (eb-insert-char (tui-editor tui) #\Tab)))))))

;;; Input history: up/down recall previous submissions when the cursor
;;; cannot move further within the buffer's own lines.
;;;
;;; Everything submitted is recalled — ordinary text, "//" literals, every
;;; /command, whole or partial.  Nothing is filtered, because the popup is
;;; kept out of the way instead: a suggestion list is something only new
;;; input asks for, never something recalled content brings with it.  A
;;; popup captures up/down, so one appearing on a recalled entry would
;;; strand browsing on it.
;;;
;;; That is one comparison, not a flag to keep in sync: the recalled text is
;;; remembered, and the popup stays shut for exactly as long as the buffer
;;; still holds it.  Editing it — a character, a backspace, a paste — makes
;;; the buffer new input again, and suggestions come back on their own.

(defun history-reset-browse (tui)
  (setf (tui-history-index tui) nil (tui-history-draft tui) nil))

(defun history-remember (tui text)
  (unless (equal text (first (tui-history tui)))   ; consecutive duplicate
    (push text (tui-history tui)))
  (history-reset-browse tui))

(defun history-recall (tui text)
  "Put a recalled TEXT in the buffer, marked as recalled so no popup opens
on it."
  (eb-set-text (tui-editor tui) text)
  (setf (tui-recalled-text tui) text))

(defun recalled-buffer-p (tui)
  "The buffer is still exactly what history put there, untouched."
  (and (tui-recalled-text tui)
       (equal (eb-text (tui-editor tui)) (tui-recalled-text tui))))

(defun history-prev (tui)
  (let ((eb (tui-editor tui))
        (hist (tui-history tui))
        (index (tui-history-index tui)))
    (cond ((null hist))
          ((null index)
           (setf (tui-history-draft tui) (eb-text eb)
                 (tui-history-index tui) 0)
           (history-recall tui (first hist)))
          ((< index (1- (length hist)))
           (incf (tui-history-index tui))
           (history-recall tui (nth (tui-history-index tui) hist))))))

(defun history-next (tui)
  (let ((index (tui-history-index tui)))
    (when index
      (if (zerop index)
          (progn (history-recall tui (or (tui-history-draft tui) ""))
                 (history-reset-browse tui))
          (progn (decf (tui-history-index tui))
                 (history-recall tui (nth (tui-history-index tui)
                                          (tui-history tui))))))))

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
                    (complete-at-point tui)
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
            (#\i (complete-at-point tui)) ; Ctrl+I = Tab on some terminals
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
              ((tui-running tui)
               (interrupt-run tui)
               (setf (tui-last-esc tui) 0))
              (t
               (when (< (- (now-ms) (tui-last-esc tui)) 600)
                 (rewind-to-last-user tui))
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
  (when (null items)
    (return-from enter-select
      (scroll tui (format nil "~a: nothing to select" title))))
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
  (multiple-value-bind (prefix matches kind) (completion-context tui)
    (when (and matches (not (member prefix matches :key #'car :test #'string=)))
      (accept-completion tui (car (nth (tui-complete-index tui) matches)) kind)
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
      ;; Mode names are only /permission arguments.  Keep stale userspace
      ;; extensions or templates named "plan"/"auto" from resurrecting
      ;; standalone slash commands after an upgrade.
      ((mode-argument-command-p name)
       (scroll tui (dim (format nil "/~a is not a command — use /permission ~a"
                               name (string-downcase name))))
       t)
      ;; 1. extension commands
      ((gethash name evo::*commands*)
       (let ((fn (pget (gethash name evo::*commands*) :fn)))
         (handler-case
             (let ((result (funcall fn (list :agent (tui-agent tui) :args args :tui tui))))
               (when (stringp result) (scroll tui result))
               (refresh-goal tui)
               (when (and (steering-pending-p (tui-agent tui))
                          (not (tui-running tui)))
                 (start-worker tui)))
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
