;;;; loop.lisp — the kernel turn loop.
;;;;
;;;; A run = many turns; a turn = one assistant message + its tool batch.
;;;; Loop: poll steering -> LLM call -> execute tools -> save point -> repeat
;;;; while tool calls or queued messages remain.  Steering is polled at turn
;;;; boundaries only.  The context is rebuilt wholesale from the journal at
;;;; every save point — the loop context is always a derived snapshot.
;;;; Every event carries a run id + monotonic turn index.

(in-package :evo.kernel)

;;; Event hooks (the extension API builds on these).

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
Run-until-settled consults them when a run goes idle; the goal driver
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
  (retry-count 0)
  (compact-retried nil)   ; overflow-recovery guard: compact + retry ONCE
  (abort-cleanups nil)    ; fns that unblock the currently-running operation
  ;; The TUI steers from its input thread while a run thread drains;
  ;; queue access is the one cross-thread seam.
  (lock (bt:make-lock "agent-queues")))

;; Heartbeat: the kernel touches a file on every event so the
;; supervisor can distinguish long tool calls from a hung process.
;; Throttled to 1/sec; enabled by the EVO_HEARTBEAT_FILE env var.

(defvar *heartbeat-file* :uninitialized)
(defvar *heartbeat-last* 0)

(defun heartbeat-touch ()
  (when (eq *heartbeat-file* :uninitialized)
    (setf *heartbeat-file* (getenv "EVO_HEARTBEAT_FILE")))
  (let ((path *heartbeat-file*)
        (now (get-universal-time)))
    (when (and path (> now *heartbeat-last*))
      (setf *heartbeat-last* now)
      (ignore-errors
       (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
         (princ now out))))))

(defun emit-event (agent &rest event)
  (heartbeat-touch)
  (let ((cb (agent-events-cb agent)))
    (when cb
      (funcall cb (append event (list :run-id (agent-run-id agent)
                                      :turn (agent-turn-index agent)))))))

(defun queue-steering (agent text)
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-steering agent) (append (agent-steering agent) (list text)))))

(defun queue-followup (agent text)
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-followups agent) (append (agent-followups agent) (list text)))))

(defun steering-pending-p (agent)
  (bt:with-lock-held ((agent-lock agent))
    (and (agent-steering agent) t)))

(defun pop-followup (agent)
  (bt:with-lock-held ((agent-lock agent))
    (pop (agent-followups agent))))

(defun drain-steering (agent)
  "Append queued steering texts as user message entries.  Returns count."
  (let ((texts (bt:with-lock-held ((agent-lock agent))
                 (prog1 (agent-steering agent)
                   (setf (agent-steering agent) nil)))))
    (dolist (text texts)
      (append-entry (agent-journal agent)
                    (list :type :message
                          :message (list :role :user
                                         :content (list (list :type :text :text text)))))
      (emit-event agent :type :steering :text text))
    (length texts)))

(defvar *executing-agent* nil
  "Agent whose tool call is currently executing on this thread, if any.")

(defun request-abort (agent)
  "Set AGENT's abort flag and run all registered unblock cleanups now.

WARNING: cleanups run on THIS thread — the caller's, typically the TUI thread,
NOT on the worker thread that registered them.  Any cleanup that touches a
resource owned by the worker thread is a cross-thread data race.

Process handles (SBCL's sb-ext:process-* or ECL's ext:external-process-*) are
the most dangerous case: sb-ext:process-wait and ext:external-process-wait
call waitpid(2) under the hood, and concurrent waitpid on the same PID from
two threads is undefined behavior at the C level.  The process struct's
status/exit-code slots are also unsynchronized — a reader on the worker
thread can see a torn state mid-write.  This can crash the runtime.

Tools that own thread-local resources (child processes, FFI handles) should
poll agent-abort-flag themselves and tear down those resources on their own
thread — do NOT register such teardowns here.

Closing a stream from another thread is also a data race on the stream
reference, though in practice the consequences are caught by ignore-errors.
Setting a flag is the only truly safe cleanup."
  (let (cleanups)
    (bt:with-lock-held ((agent-lock agent))
      (setf (agent-abort-flag agent) t)
      (setf cleanups (copy-list (agent-abort-cleanups agent))))
    (dolist (cleanup cleanups)
      (ignore-errors (funcall cleanup)))
    t))

(defun add-abort-cleanup (agent cleanup)
  "Register CLEANUP until the returned unregister function is called.
If AGENT is already aborted, CLEANUP is invoked immediately too."
  (let (run-now)
    (bt:with-lock-held ((agent-lock agent))
      (push cleanup (agent-abort-cleanups agent))
      (setf run-now (agent-abort-flag agent)))
    (when run-now
      (ignore-errors (funcall cleanup)))
    (lambda ()
      (bt:with-lock-held ((agent-lock agent))
        (setf (agent-abort-cleanups agent)
              (remove cleanup (agent-abort-cleanups agent)
                      :test #'eq :count 1))))))

(defmacro with-abort-cleanup ((agent cleanup) &body body)
  "Register CLEANUP for cross-thread abort notification.

PITFALL: CLEANUP runs on the aborting thread (the TUI thread), NOT on the
worker thread that entered this macro.  This is a cross-thread data race for
any resource tied to the spawning thread.

Process handles (SBCL's sb-ext:process-* or ECL's ext:external-process-*) are
the worst case: process-wait calls waitpid(2), which is undefined behavior
when called concurrently from two threads on the same PID; the process
struct's status/exit-code slots are also unsynchronized and a reader on the
worker thread can see a torn state mid-write.  This can crash the runtime.

Closing a stream from another thread is also a data race on the stream
reference, though the consequences are typically caught by ignore-errors
rather than crashing.

If CLEANUP needs to tear down a thread-local resource (a child process, an
FFI handle, a stream), do NOT use this macro.  Instead, poll
agent-abort-flag from the worker thread and perform the teardown there."
  `(let ((unregister (add-abort-cleanup ,agent ,cleanup)))
     (unwind-protect
          (progn ,@body)
       (funcall unregister))))

;;; Save point: the whole context snapshot is rebuilt between turns.

(defun effective-model-id (state agent)
  "The session's model id: journaled choice, else CLI override, else the
:model setting.  No kernel default — config must name a model (normally
guaranteed by the CLI preflight)."
  (or (evo.journal:state-model state)
      (agent-model-override agent)
      (setting :model)
      (error "No model is configured — set one in init.lisp: (evo:set-setting :model \"...\")")))

(defun effective-model-provider (state id)
  "Which provider serves model ID this session, when the id is registered
under more than one.  A journaled /model choice names its provider; failing
that the :model-provider setting names the config default, but only if it
actually serves ID — a stale setting must not break an unrelated model.
NIL means \"first registration wins\"."
  (or (evo.journal:state-model-provider state)
      (let ((configured (setting :model-provider)))
        (and configured
             (member configured (model-providers id))
             configured))))

(defun prepare-next-turn (agent)
  (let* ((state (fold-state (agent-journal agent)))
         (tools (active-tools state))
         (model-id (effective-model-id state agent))
         (thinking (or (evo.journal:state-thinking state)
                       (agent-thinking-override agent)
                       (setting :thinking :medium))))
    ;; Projection pipeline: journal entries -> agent messages ->
    ;; (transform-context) -> provider messages.  Extensions hook the
    ;; middle stage; output is never written back.
    (let ((messages (evo.journal:state-messages state)))
      (dolist (hook (gethash :transform-context *event-hooks*))
        (let ((result (handler-case (funcall hook messages)
                        (error (e)
                          (warn "transform-context hook failed: ~a" e)
                          messages))))
          (when (listp result) (setf messages result))))
      (list :state state
            :tools tools
            :messages messages
            :model (find-model model-id (effective-model-provider state model-id))
            :thinking thinking
            ;; Session id = OpenAI prompt_cache_key (cache affinity).
            :cache-key (pget (evo.journal:journal-header (agent-journal agent)) :id)
            :system (build-system-prompt tools
                                         :lore (all-lore-entries :state state)
                                         :model model-id)))))

;;; Tool batch execution (sequential) with :tool-call interception —
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
    ;; Announce the call up front, with the fully-parsed arguments: the
    ;; display must show what is running while it runs, not after.
    (emit-event agent :type :tool-call-start :name name :id id
                      :arguments (pget call :arguments))
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
                (let ((*executing-agent* agent))
                  (execute-tool tool args)))))))
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
                        :content-chars (length content)
                        :content (truncate-string content 500)))))

(defun message-tool-calls (message)
  (remove-if-not (lambda (b) (eq (pget b :type) :tool-call))
                 (message-content message)))

(defun synthesize-truncation-results (agent message)
  "Truncation guard: on :length, tool calls are NOT executed — salvaged
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
            ;; Threshold compaction check at the save point.
            (let* ((state (fold-state (agent-journal agent)))
                   (id (effective-model-id state agent)))
              (when (compaction-needed-p state (find-model
                                                id
                                                (effective-model-provider state id)))
                (emit-event agent :type :compaction-start)
                (unwind-protect
                     (handler-case (compact-now agent)
                       (error (e) (warn "Compaction failed, continuing uncompacted: ~a" e)))
                  (emit-event agent :type :compaction-end))
                (when (agent-abort-flag agent)
                  (return :aborted))))
            (let* ((ctx (prepare-next-turn agent))
                   (assistant
                     (call-provider
                      :model (pget ctx :model)
                      :system (pget ctx :system)
                      :messages (pget ctx :messages)
                      :tools (mapcar #'tool->provider-spec (pget ctx :tools))
                      :thinking-level (pget ctx :thinking)
                      :cache-key (pget ctx :cache-key)
                      :abort-flag (lambda () (agent-abort-flag agent))
                      :abort-cleanup (lambda (cleanup)
                                       (add-abort-cleanup agent cleanup))
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
                   (when (agent-abort-flag agent)
                     (return))
                   (run-tool-call agent call)
                   (when (agent-abort-flag agent)
                     (return)))
                 (when (agent-abort-flag agent)
                   (return :aborted)))
                (:length
                 (if (message-tool-calls assistant)
                     (synthesize-truncation-results agent assistant)
                     (return :length)))
                (:error (return :error))
                (:aborted (return :aborted))
                (:stop
                 (unless (steering-pending-p agent)
                   (return :stop))))))))
    (emit-event agent :type :run-end :outcome outcome)
    outcome))

(defun last-assistant-message (agent)
  (let ((messages (evo.journal:state-messages (fold-state (agent-journal agent)))))
    (find :assistant messages :key #'message-role :from-end t)))

(defparameter *max-turn-retries* 2)

(defun run-until-settled (agent)
  "Outer driver: run -> post-run check (retryable error? queued
messages? active goal?) -> continue.  Returns the final outcome."
  (loop
    (let ((outcome (run agent)))
      (cond
        ((eq outcome :aborted) (return outcome))
        ((eq outcome :error)
         (let ((msg (last-assistant-message agent)))
           (cond
             ;; Overflow recovery: compact + retry once.
             ((and (overflow-error-p msg) (not (agent-compact-retried agent)))
              (setf (agent-compact-retried agent) t)
              (emit-event agent :type :compaction-start)
              (unwind-protect
                   (handler-case (compact-now agent)
                     (error (e)
                       (warn "Overflow-recovery compaction failed: ~a" e)
                       (return outcome)))
                (emit-event agent :type :compaction-end))
              (when (agent-abort-flag agent)
                (return :aborted)))
             ((and (pget msg :retryable)
                   (< (agent-retry-count agent) *max-turn-retries*))
              (incf (agent-retry-count agent)))
             (t (return outcome)))))
        (t
         (setf (agent-retry-count agent) 0)
         (setf (agent-compact-retried agent) nil)
         (let ((followup (pop-followup agent)))
           (cond
             (followup (queue-steering agent followup))
             ((steering-pending-p agent))   ; steered while settling: go again
             ((loop for fn in *settled-hooks*
                      thereis (handler-case (funcall fn agent outcome)
                                (error (e)
                                  (warn "Settled hook failed: ~a" e)
                                  nil))))
             (t (return outcome)))))))))
