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
;;;
;;; A hook is OWNED: it carries the extension generation that registered it, so
;;; reloading a generation can withdraw exactly its own hooks.  A NAMEd hook is
;;; additionally replace-on-re-register within one owner, which is what makes
;;; loading the same file twice idempotent instead of cumulative.

(defvar *event-hooks* (make-hash-table)) ; event-keyword -> list of HOOK-ENTRY

(defvar *extension-owner* nil
  "Owner token (see extension.lisp) for registrations made while loading an
extension file, or NIL for kernel-level registrations.")

(defstruct (hook-entry (:constructor %make-hook-entry))
  name          ; symbol/keyword, or NIL for an anonymous hook
  owner         ; *EXTENSION-OWNER* captured at registration
  fn)

(defun add-hook (event fn &key name (owner *extension-owner*))
  "Append FN to EVENT's hook list.  Hooks run in registration order, and
registration order is extension load order, so the `NNN-` rank in a file
name decides who sees a payload first here too — the alternative (pushing)
inverts the order the user wrote and makes the ranks lie.

NAME makes the registration idempotent for its owner: re-registering the same
NAME replaces the previous entry in place, keeping its position.  An anonymous
hook always appends, so a file that registers one and is loaded twice installs
it twice — pass NAME from anything an extension reload can re-run."
  (let* ((entries (gethash event *event-hooks*))
         (existing (and name
                        (find-if (lambda (e)
                                   (and (equal (hook-entry-name e) name)
                                        (eql (hook-entry-owner e) owner)))
                                 entries))))
    (cond (existing (setf (hook-entry-fn existing) fn))
          (t (setf (gethash event *event-hooks*)
                   (append entries (list (%make-hook-entry :name name :owner owner
                                                           :fn fn)))))))
  fn)

(defun remove-hooks-if (predicate)
  "Withdraw every hook entry satisfying PREDICATE, returning how many went.
Disposing an extension generation goes through here, so a reloaded extension
does not leave its previous self subscribed."
  (let ((removed 0))
    (maphash (lambda (event entries)
               (let ((kept (remove-if predicate entries)))
                 (incf removed (- (length entries) (length kept)))
                 (setf (gethash event *event-hooks*) kept)))
             *event-hooks*)
    removed))

(defun event-hook-functions (event)
  "The functions registered for EVENT, in run order.  Callers that drive hooks
themselves (the context transform threads a value through them) go through this
rather than touching the entry structs."
  (mapcar #'hook-entry-fn (gethash event *event-hooks*)))

(defun run-hooks (event payload)
  "Run hooks for EVENT.  Returns the list of non-nil hook results."
  (loop for entry in (gethash event *event-hooks*)
        for result = (handler-case (funcall (hook-entry-fn entry) payload)
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
  (steering nil)          ; pending steering texts (FIFO), guarded by LOCK
  (followups nil)         ; pending follow-up texts (FIFO), guarded by LOCK
  (control nil)           ; TUI -> worker control messages, guarded by LOCK
  (abort-latched nil)     ; worker-owned after it consumes :ABORT from CONTROL
  events-cb               ; fn (event-plist), or nil
  (run-id (gen-id))
  (turn-index 0)
  model-override          ; CLI/default model id when the fold has none
  thinking-override
  (retry-count 0)
  (compact-retried nil)   ; overflow-recovery guard: compact + retry ONCE
  (abort-cleanups nil)    ; worker-owned fns, run when it consumes :ABORT
  ;; The TUI sends immutable queue/control messages while the run worker drains
  ;; them.  LOCK protects only those handoff queues; execution state above is
  ;; otherwise owned by the one run worker and is never mutated by the TUI.
  (lock (bt:make-lock "agent-mailbox")))

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

(defun queue-steering (agent text &key images)
  "Queue a user turn for the next turn boundary.  IMAGES is a list of :image
content blocks (evo.media builds them); they ride the same queue as the text
so that a message and its screenshots can never be split across turns."
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-steering agent)
          (append (agent-steering agent) (list (list :text text :images images))))))

(defun queue-followup (agent text)
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-followups agent) (append (agent-followups agent) (list text)))))

(defun steering-pending-p (agent)
  (bt:with-lock-held ((agent-lock agent))
    (and (agent-steering agent) t)))

(defun pop-followup (agent)
  (bt:with-lock-held ((agent-lock agent))
    (pop (agent-followups agent))))

(defun agent-pending-work-p (agent)
  "True when session-owned input or control remains in AGENT's mailbox."
  (bt:with-lock-held ((agent-lock agent))
    (and (or (agent-steering agent)
             (agent-followups agent)
             (agent-control agent))
         t)))

(defun reset-agent-session-state (agent)
  "Reset ephemeral execution state after a journal switch.  The caller must
first prove there is no task and no pending mailbox work; this function never
drops input as an implementation convenience."
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-control agent) nil
          (agent-abort-latched agent) nil
          (agent-abort-cleanups agent) nil))
  (setf (agent-run-id agent) (gen-id)
        (agent-turn-index agent) 0
        (agent-retry-count agent) 0
        (agent-compact-retried agent) nil)
  agent)

(defun steering-content (text images)
  "Content blocks for one queued user turn.  Images come first: both vision
stacks read an image better when the text that asks about it follows it."
  (append images
          (when (plusp (length (or text "")))
            (list (list :type :text :text text)))))

(defun drain-steering (agent)
  "Append queued steering turns as user message entries.  Returns count."
  (let ((queued (bt:with-lock-held ((agent-lock agent))
                  (prog1 (agent-steering agent)
                    (setf (agent-steering agent) nil))))
        (appended 0))
    (dolist (item queued)
      (let* ((text (pget item :text))
             (images (pget item :images))
             (content (steering-content text images)))
        ;; An item with neither text nor images journals nothing, and must not
        ;; be counted either: the count is what tells the run there is new
        ;; input to answer.
        (when content
          (append-entry (agent-journal agent)
                        (list :type :message
                              :message (list :role :user :content content)))
          (incf appended)
          (emit-event agent :type :steering :text text
                            :images (length images)))))
    appended))

(defvar *executing-agent* nil
  "Agent whose tool call is currently executing on this thread, if any.")

(defun request-abort (agent)
  "Queue an abort control message for AGENT's worker.  The caller never mutates
worker-owned execution state or runs resource cleanup.  The worker consumes the
message through AGENT-ABORT-FLAG, latches it, and runs its cleanup callbacks on
the same thread that owns the in-flight operation."
  (bt:with-lock-held ((agent-lock agent))
    (unless (member :abort (agent-control agent))
      (setf (agent-control agent)
            (append (agent-control agent) (list :abort)))))
  t)

(defun agent-abort-flag (agent)
  "Worker-side cancellation safe point.  Consume queued control messages,
latch abort for the rest of this run, and invoke registered cleanup callbacks
on the worker thread.  Non-worker callers may inspect the result but must use
REQUEST-ABORT to request a state change."
  (let (abort cleanups)
    (bt:with-lock-held ((agent-lock agent))
      (when (member :abort (agent-control agent))
        (setf (agent-control agent)
              (remove :abort (agent-control agent)))
        (unless (agent-abort-latched agent)
          (setf (agent-abort-latched agent) t
                cleanups (copy-list (agent-abort-cleanups agent)))))
      (setf abort (agent-abort-latched agent)))
    (dolist (cleanup cleanups)
      (ignore-errors (funcall cleanup)))
    abort))

(defun reset-agent-run-control (agent)
  "Empty AGENT's control mailbox and abort latch.  Called by the TUI thread
only while no worker exists — before it publishes a new task, and after it has
joined a finished one (clearing an :abort the finished run never consumed)."
  (bt:with-lock-held ((agent-lock agent))
    (setf (agent-control agent) nil
          (agent-abort-latched agent) nil
          (agent-abort-cleanups agent) nil)))

(defun add-abort-cleanup (agent cleanup)
  "Register CLEANUP for the active operation.  CLEANUP is always invoked by
the worker through AGENT-ABORT-FLAG, never by the TUI/requesting thread."
  (let (run-now)
    (bt:with-lock-held ((agent-lock agent))
      (push cleanup (agent-abort-cleanups agent))
      (setf run-now (agent-abort-latched agent)))
    (when run-now
      (ignore-errors (funcall cleanup)))
    (lambda ()
      (bt:with-lock-held ((agent-lock agent))
        (setf (agent-abort-cleanups agent)
              (remove cleanup (agent-abort-cleanups agent)
                      :test #'eq :count 1))))))

(defmacro with-abort-cleanup ((agent cleanup) &body body)
  "Run CLEANUP on the worker thread if AGENT receives an abort while BODY is
active, and unregister it on every exit.  Cleanup therefore preserves resource
ownership; it must not be treated as a callback on the requesting TUI thread."
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

(defun effective-thinking (state &optional override)
  "The thinking level for this turn: the journaled /thinking choice, then
the --thinking flag, then the :thinking setting, then :medium.  Every
candidate is normalized onto the ladder, so a retired :off left in an old
journal or init.lisp degrades to the weakest live rung instead of reaching
an adapter that no longer knows the word."
  (or (normalize-thinking-level (evo.journal:state-thinking state))
      (normalize-thinking-level override)
      (normalize-thinking-level (setting :thinking))
      :medium))

(defun prepare-next-turn (agent)
  (let* ((state (fold-state (agent-journal agent)))
         (tools (active-tools state))
         (model-id (effective-model-id state agent))
         (model (find-model model-id (effective-model-provider state model-id)))
         (thinking (effective-thinking state (agent-thinking-override agent))))
    ;; Projection pipeline: journal entries -> agent messages ->
    ;; (transform-context) -> provider messages.  Extensions hook the
    ;; middle stage; output is never written back.
    (let ((messages (evo.journal:state-messages state)))
      (dolist (hook (event-hook-functions :transform-context))
        (let ((result (handler-case (funcall hook messages)
                        (error (e)
                          (warn "transform-context hook failed: ~a" e)
                          messages))))
          (when (listp result) (setf messages result))))
      (list :state state
            :tools tools
            :messages messages
            :model model
            :thinking thinking
            ;; Session id = OpenAI prompt_cache_key (cache affinity).
            :cache-key (pget (evo.journal:journal-header (agent-journal agent)) :id)
            :system (build-system-prompt tools
                                         :lore (all-lore-entries :state state)
                                         :model model-id
                                         :vision (model-vision-p model)
                                         :language (language-request state))))))

;;; Tool batch execution (sequential) with :tool-call interception —
;;; the one point permission gates / read-only policies / sandboxing build on.

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

(defun truncate-result-blocks (blocks)
  "Trim the TEXT in BLOCKS to a shared *MAX-TOOL-RESULT-CHARS* budget.  An
image block passes through whole — it is already capped in bytes by
EVO.MEDIA:*MAX-IMAGE-BYTES*, and half an image is nothing."
  (let ((left *max-tool-result-chars*))
    (loop for block in blocks
          collect (if (eq (pget block :type) :text)
                      (let ((text (truncate-string (or (pget block :text) "") (max left 0))))
                        (decf left (length text))
                        (pput block :text text))
                      block))))

(defun result-display-text (blocks)
  "What the host shows for a tool result: its text, with anything the model
looks at rather than reads named in one line."
  (string-join (string #\Newline)
               (loop for block in blocks
                     collect (case (pget block :type)
                               (:text (or (pget block :text) ""))
                               (:image (format nil "[image ~a]"
                                               (evo.media:image-summary block)))
                               (t (format nil "[~(~a~)]" (pget block :type)))))))

(defun result-context-chars (blocks)
  "Chars to charge the live context estimate for BLOCKS.  Images are priced
in tokens, and every consumer of this number divides chars by 4."
  (loop for block in blocks
        sum (if (eq (pget block :type) :image)
                (* 4 *image-block-tokens*)
                (length (or (pget block :text) "")))))

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
    (let* ((blocks (or (truncate-result-blocks (tool-content-blocks content))
                       (list (list :type :text :text ""))))
           (display (result-display-text blocks)))
      (append-entry (agent-journal agent)
                    (append
                     (list :type :message
                           :message (list :role :tool-result
                                          :tool-call-id id
                                          :tool-name name
                                          :is-error (and is-error t)
                                          :content blocks))
                     (when details (list :details details))))
      (emit-event agent :type :tool-result :name name :id id
                        :is-error (and is-error t)
                        :content-chars (result-context-chars blocks)
                        :content (truncate-string display 500)))))

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
