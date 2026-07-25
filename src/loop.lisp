;;;; loop.lisp — the kernel turn loop (§6).
;;;;
;;;; A run = many turns; a turn = one assistant message + its tool batch.
;;;; Loop: poll steering -> LLM call -> execute tools -> save point -> repeat
;;;; while tool calls or queued messages remain.  Steering is polled at turn
;;;; boundaries only.  The context is rebuilt wholesale from the journal at
;;;; every save point — the loop context is always a derived snapshot.
;;;; Every event carries a run id + monotonic turn index.

(in-package :evo.kernel)

;;; Event hooks (extension API §10 builds on these).

(defvar *event-hooks* (make-hash-table))  ; event-keyword -> list of fns

(defun add-hook (event fn)
  (push fn (gethash event *event-hooks*))
  fn)

(defun run-hooks (event payload)
  "Run hooks for EVENT.  Returns the list of non-nil hook results."
  (loop for fn in (gethash event *event-hooks*)
        for result = (handler-case (funcall fn payload)
                       (error (e)
                         (warn "Hook for ~s failed: ~a" event e)
                         nil))
        when result collect result))

(defvar *settled-hooks* nil
  "Functions (agent last-outcome) -> t if they queued more work.
Run-until-settled consults them when a run goes idle; the goal driver (§8)
plugs in here.")

;;; The agent.

(defstruct agent
  journal
  (steering nil)          ; pending steering texts (FIFO)
  (followups nil)         ; pending follow-up texts (FIFO)
  (abort-flag nil)
  events-cb               ; fn (event-plist), or nil
  (run-id (gen-id))
  (turn-index 0)
  model-override          ; CLI/default model id when the fold has none
  thinking-override
  (retry-count 0))

(defun emit-event (agent &rest event)
  (let ((cb (agent-events-cb agent)))
    (when cb
      (funcall cb (append event (list :run-id (agent-run-id agent)
                                      :turn (agent-turn-index agent)))))))

(defun queue-steering (agent text)
  (setf (agent-steering agent) (append (agent-steering agent) (list text))))

(defun queue-followup (agent text)
  (setf (agent-followups agent) (append (agent-followups agent) (list text))))

(defun drain-steering (agent)
  "Append queued steering texts as user message entries.  Returns count."
  (let ((texts (agent-steering agent)))
    (setf (agent-steering agent) nil)
    (dolist (text texts)
      (append-entry (agent-journal agent)
                    (list :type :message
                          :message (list :role :user
                                         :content (list (list :type :text :text text)))))
      (emit-event agent :type :steering :text text))
    (length texts)))

;;; Save point (§6): the whole context snapshot is rebuilt between turns.

(defun prepare-next-turn (agent)
  (let* ((state (fold-state (agent-journal agent)))
         (tools (active-tools state))
         (model-id (or (evo.journal:state-model state)
                       (agent-model-override agent)
                       (setting :model "claude-sonnet-5")))
         (thinking (or (evo.journal:state-thinking state)
                       (agent-thinking-override agent)
                       (setting :thinking :medium))))
    (list :state state
          :tools tools
          :model (find-model model-id)
          :thinking thinking
          :system (build-system-prompt tools))))

;;; Tool batch execution (sequential, D9) with :tool-call interception —
;;; the one point permission gates / plan mode / sandboxing build on.

(defun intercept-tool-call (name args)
  "Run :tool-call hooks.  Returns (values args blocked-p reason)."
  (let ((results (run-hooks :tool-call (list :name name :arguments args))))
    (dolist (r results (values args nil nil))
      (when (consp r)
        (cond ((pget r :block)
               (return (values args t (or (pget r :reason) "Blocked by extension hook"))))
              ((pget r :arguments)
               (setf args (pget r :arguments))))))))

(defparameter *max-tool-result-chars* 50000)

(defun run-tool-call (agent call)
  "Execute one tool call; append its tool-result entry."
  (let* ((name (pget call :name))
         (id (pget call :id))
         (tool (find-tool name))
         content details is-error)
    (cond
      ((pget call :arguments-error)
       (setf content (format nil "Tool arguments were not valid JSON: ~a"
                             (pget call :arguments-error))
             is-error t))
      ((null tool)
       (setf content (format nil "Unknown tool: ~a" name) is-error t))
      (t
       (multiple-value-bind (args blocked-p reason)
           (intercept-tool-call name (pget call :arguments))
         (if blocked-p
             (setf content (format nil "Tool call blocked: ~a" reason) is-error t)
             (multiple-value-setq (content details is-error)
               (execute-tool tool args))))))
    (let ((content (truncate-string (or content "") *max-tool-result-chars*)))
      (append-entry (agent-journal agent)
                    (append
                     (list :type :message
                           :message (list :role :tool-result
                                          :tool-call-id id
                                          :tool-name name
                                          :is-error (and is-error t)
                                          :content (list (list :type :text :text content))))
                     (when details (list :details details))))
      (emit-event agent :type :tool-result :name name :id id
                        :is-error (and is-error t)
                        :content (truncate-string content 500)))))

(defun message-tool-calls (message)
  (remove-if-not (lambda (b) (eq (pget b :type) :tool-call))
                 (message-content message)))

(defun synthesize-truncation-results (agent message)
  "Truncation guard (§6): on :length, tool calls are NOT executed — salvaged
JSON can validate yet be incomplete.  Each gets an error result."
  (dolist (call (message-tool-calls message))
    (append-entry (agent-journal agent)
                  (list :type :message
                        :message (list :role :tool-result
                                       :tool-call-id (pget call :id)
                                       :tool-name (pget call :name)
                                       :is-error t
                                       :content (list (list :type :text :text
                                                            "Your response hit the output-token limit, so this tool call was not executed. Re-issue it.")))))))

(defun run (agent)
  "One run: turns until the model stops with no pending steering.
Returns :stop :length :error :aborted."
  (emit-event agent :type :run-start)
  (let ((outcome
          (loop
            (when (agent-abort-flag agent)
              (return :aborted))
            (drain-steering agent)
            (emit-event agent :type :turn-start)
            (let* ((ctx (prepare-next-turn agent))
                   (state (pget ctx :state))
                   (assistant
                     (call-provider
                      :model (pget ctx :model)
                      :system (pget ctx :system)
                      :messages (evo.journal:state-messages state)
                      :tools (mapcar #'tool->provider-spec (pget ctx :tools))
                      :thinking-level (pget ctx :thinking)
                      :abort-flag (lambda () (agent-abort-flag agent))
                      :on-event (lambda (ev) (apply #'emit-event agent ev)))))
              (append-entry (agent-journal agent) (list :type :message :message assistant))
              (emit-event agent :type :message-end
                                :stop-reason (message-stop-reason assistant)
                                :usage (message-usage assistant)
                                :error (pget assistant :error-message))
              (incf (agent-turn-index agent))
              (run-hooks :turn-end (list :agent agent :message assistant))
              (ecase (message-stop-reason assistant)
                (:tool-use
                 (dolist (call (message-tool-calls assistant))
                   (run-tool-call agent call)))
                (:length
                 (if (message-tool-calls assistant)
                     (synthesize-truncation-results agent assistant)
                     (return :length)))
                (:error (return :error))
                (:aborted (return :aborted))
                (:stop
                 (unless (agent-steering agent)
                   (return :stop))))))))
    (emit-event agent :type :run-end :outcome outcome)
    outcome))

(defun last-assistant-message (agent)
  (let ((messages (evo.journal:state-messages (fold-state (agent-journal agent)))))
    (find :assistant messages :key #'message-role :from-end t)))

(defparameter *max-turn-retries* 2)

(defun run-until-settled (agent)
  "Outer driver (§6): run -> post-run check (retryable error? queued
messages? active goal?) -> continue.  Returns the final outcome."
  (loop
    (let ((outcome (run agent)))
      (cond
        ((eq outcome :aborted) (return outcome))
        ((eq outcome :error)
         (let ((msg (last-assistant-message agent)))
           (if (and (pget msg :retryable)
                    (< (agent-retry-count agent) *max-turn-retries*))
               (incf (agent-retry-count agent))
               (return outcome))))
        (t
         (setf (agent-retry-count agent) 0)
         (cond
           ((agent-followups agent)
            (queue-steering agent (pop (agent-followups agent))))
           ((loop for fn in *settled-hooks*
                    thereis (funcall fn agent outcome)))
           (t (return outcome))))))))
