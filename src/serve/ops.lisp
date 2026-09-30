;;;; ops.lisp — POST /ops: the client's writes (CONTRACT §5.5).
;;;;
;;;; Every op is one entry in a table: a name, the argument schema the catalog
;;;; publishes, the state it needs (none / idle / quiescent) and the function
;;;; that runs it.  The function runs on the session thread, in arrival order,
;;;; exactly where the TUI runs a typed command; it returns the op's `result`
;;;; object or signals OP-ERROR with a code from the contract's list.
;;;;
;;;; An op is answered with HTTP 200 whatever happened (except a bad token, a
;;;; malformed envelope, or a server that is shutting down): the client
;;;; branches on the code, never on the status.  An error message never quotes
;;;; a value the caller sent — a message is a sentence about the *kind* of
;;;; failure, so no secret can come back out of one.

(in-package :evo.serve)

(defstruct (op (:constructor %make-op))
  name precondition args fn schema)

(defvar *ops* (make-hash-table :test #'equal)
  "Every op, by name.")

(defun register-op (name fn &key precondition args schema)
  "Register NAME.  PRECONDITION is :none, :idle or :quiescent; ARGS is the
argument spec the catalog publishes (a plist of keyword -> plist with :type,
:required, :values)."
  (setf (gethash name *ops*)
        (%make-op :name name :fn fn :precondition (or precondition :none)
                  :args args :schema schema))
  name)

(defun find-op (name)
  (and (stringp name) (gethash name *ops*)))

(defun all-ops ()
  (sort (loop for op being the hash-values of *ops* collect op)
        #'string< :key #'op-name))

(defun op-arg-schema (op)
  "OP's arguments as the JSON schema the catalog publishes."
  (or (op-schema op)
      (let* ((spec (op-args op))
             (properties (loop for (key value) on spec by #'cddr
                               append (list key
                                            (append (list :type (getf value :type))
                                                    (when (getf value :values)
                                                      (list :enum
                                                            (coerce (getf value :values)
                                                                    'vector)))
                                                    (when (getf value :description)
                                                      (list :description
                                                            (getf value :description)))))))
             (required (loop for (key value) on spec by #'cddr
                             when (getf value :required)
                               collect (substitute #\_ #\-
                                                   (string-downcase (symbol-name key))))))
        (append (list :type "object")
                (when properties (list :properties properties))
                (when required (list :required (coerce required 'vector)))))))

;;; Arguments.  A bad argument is `invalid_args` with a fixed message.

;; JSON keys arrive as keywords with "_" read as "-" (:line_count ->
;; :LINE-COUNT), so an op's argument names are dashed here and underscore on
;; the wire: :ITEM-ID is "item_id" in a request and in the catalog.

(defun op-arg (args key &key required (type :string))
  (let ((value (getf args key)))
    (cond ((null value)
           (when required (op-fail "invalid_args" "missing required argument"))
           nil)
          ((ecase type
             (:string (stringp value))
             (:boolean (or (eq value t) (eq value 'false)))
             (:number (numberp value))
             (:object (listp value))
             (:array (or (listp value) (vectorp value))))
           value)
          (t (op-fail "invalid_args" "argument has the wrong type")))))

(defun op-arg-enum (args key values &key required default)
  (let ((value (op-arg args key :required required)))
    (cond ((null value) default)
          ((member value values :test #'equal) value)
          (t (op-fail "invalid_args" "argument is not one of the accepted values")))))

;;; Preconditions.  `busy` is a task running; `not_quiescent` is anything that
;;; moving the session under would strand.

(defun check-op-precondition (server op)
  (case (op-precondition op)
    (:idle (when (server-task server)
             (op-fail "busy" "the session is running a task")))
    (:quiescent
     (when (server-task server)
       (op-fail "not_quiescent" "the session is running a task"))
     (when (agent-pending-work-p (server-agent server))
       (op-fail "not_quiescent" "the session has queued input")))
    (t t))
  t)

;;; Replies.

(defun op-reply (server rid result)
  "The reply for a successful op.  RESULT is the op's own object — an op with
nothing to report answers `{}`, never `null`: a client should be able to read
`result` as an object whatever the op was."
  (list :rid rid :ok t :seq (op-log-last-seq (server-oplog server))
        :result (or result (json-object-value nil))))

(defun op-error-reply (server rid code message &optional detail)
  (list :rid rid :ok 'false
        :seq (op-log-last-seq (server-oplog server))
        :error (list :code code :message message :detail detail)))

(defun error-reply-from (server rid condition)
  (cond
    ((typep condition 'op-error)
     (op-error-reply server rid (op-error-code condition) (op-error-message condition)
                     (op-error-detail condition)))
    ((typep condition 'evo.command:command-refused)
     (op-error-reply server rid
                     (ecase (evo.command:command-refused-kind condition)
                       (:conflict "busy") (:invalid "invalid_args") (:not-found "not_found"))
                     (evo.command:command-refused-text condition)))
    (t (op-error-reply server rid "op_failed" (format nil "~a" condition)))))

;;; The session's identity, as an op reports it.

(defun session-info (server)
  (let* ((agent (server-agent server))
         (journal (agent-journal agent)))
    (list :id (pget (journal-header journal) :id)
          :path (namestring (journal-path journal))
          :leaf (journal-leaf-id journal)
          :program (or (pget (journal-header journal) :program)
                       (server-program server)))))

;;; Queued input: the id an item gets when it is accepted, and how the view is
;;; told about it.

(defun queue-session-input (server text images queue later)
  "Queue TEXT as the user's own turn and return the item id it will keep.

The kernel mints the id at queue time and the journaled entry reuses it, so the
row the user is looking at keeps its identity when it is sent (CONTRACT §3).
IMAGES are the :image blocks themselves: the view draws the item (and /media
serves its bytes) before anything runs.  QUEUE is what the client asked for (it
is what the item says); LATER is whether it really goes to the follow-up queue,
which is what a task in flight makes true."
  (let* ((agent (server-agent server))
         (id (if later
                 (queue-followup agent text)
                 (queue-steering agent text :images images :from-user t))))
    (let ((provider (topic-provider server "session")))
      (when provider
        ;; The blocks, not just a count: the view needs them for the item's
        ;; images (and /media serves their bytes).
        (topic-provider-queued-input provider id text images queue)))
    id))

(defun cancel-queue-entry (agent text queue)
  "Remove the queue entry whose text is TEXT.  T when one was there.  This is
the bridge for kernels whose queue entries carry no id yet (CONTRACT §3): it
reaches into the mailbox under the mailbox's own lock — nothing else may touch
those lists — and finds the entry by the text the item holds."
  (let ((lock (find-symbol "AGENT-LOCK" :evo.kernel))
        (steering (find-symbol "AGENT-STEERING" :evo.kernel))
        (followups (find-symbol "AGENT-FOLLOWUPS" :evo.kernel)))
    (when (and lock steering followups)
      (labels ((get-key (name) (funcall (fdefinition name) agent))
               (set-key (name value)
                 (funcall (fdefinition (list 'setf name)) value agent)))
        (bt:with-lock-held ((funcall (fdefinition lock) agent))
          (if (equal queue "after_run")
              (let ((entries (get-key followups)))
                (if (member text entries :test #'equal)
                    (progn (set-key followups (remove text entries :count 1
                                                                  :test #'equal))
                           t)
                    nil))
              (let ((entries (get-key steering)))
                (if (find text entries :key (lambda (e) (pget e :text)) :test #'equal)
                    (progn (set-key steering
                                    (remove text entries :count 1
                                                     :key (lambda (e) (pget e :text))
                                                     :test #'equal))
                           t)
                    nil))))))))

;;; Ops: input.

(defun serve-images (args)
  "The op's images as :image content blocks.  A bad one is refused: running
the turn without the picture it asked about is worse than not running it."
  (let ((images (getf args :images)))
    (unless (or (null images) (listp images) (vectorp images))
      (op-fail "invalid_args" "images must be an array"))
    (loop for image across (coerce (or images #()) 'vector)
          collect (cond
                    ((and (listp image) (stringp (getf image :data)))
                     (let* ((octets (handler-case (base64->octets (getf image :data))
                                      (error () (op-fail "invalid_args"
                                                         "image data is not base64"))))
                            (sniffed (evo.media:sniff-media-type octets)))
                       (unless sniffed
                         (op-fail "invalid_args" "image data is not a supported image"))
                       (evo.media:make-image-block
                        :data (getf image :data) :media-type sniffed
                        :name (or (getf image :name) "image")
                        :bytes (length octets) :source "http")))
                    ((and (listp image) (stringp (getf image :path)))
                     (multiple-value-bind (block reason)
                         (evo.media:attach-image-file (getf image :path))
                       (or block
                           (op-fail "invalid_args" "image could not be read: ~a" reason))))
                    (t (op-fail "invalid_args" "each image needs data or a path"))))))

(defun op-input-send (server args)
  "The user's turn: it starts a run, or lands at the next turn boundary of the
one in flight (CONTRACT §5.5)."
  (let* ((text (or (op-arg args :text) ""))
         (images (serve-images args))
         (queue (op-arg-enum args :queue '("now" "after_run") :default "now"))
         ;; What the client asked for, which is what the item says.  "now"
         ;; while the model does not resolve still waits — that is what
         ;; BLOCKED reports — and "after_run" with nothing running is simply
         ;; now, because a follow-up queue nobody drains is a dropped prompt.
         (later (and (equal queue "after_run") (server-task server) t)))
    (when (getf args :topic)
      (unless (equal (getf args :topic) "session")
        (op-fail "not_found" "no such topic on this server")))
    (unless (or (plusp (length text)) images)
      (op-fail "invalid_args" "nothing to send"))
    (let* ((agent (server-agent server))
           (ready (handler-case (progn (effective-model (fold-state (agent-journal agent))
                                                         agent)
                                       t)
                    (error () nil)))
           ;; Was anything already going?  That is what decides whether this
           ;; input waits: a task in flight, or a model that does not resolve.
           (busy (and (server-task server) t)))
      (let ((id (queue-session-input server text images queue later)))
        (cond ((and ready (not later)) (start-run server))
              ((not ready)
               (server-notice server "the model does not resolve — input stays queued"
                              :severity :warn :source :serve)))
        ;; QUEUED is the truth about the input, not about the reply: input that
        ;; starts a run now has been *sent* (the client's row becomes the user
        ;; message), and input waiting on a turn boundary or on the model has
        ;; not.
        (list :item-id id
              :queued (wire-boolean (or busy (not ready)))
              :blocked (unless ready "model_not_ready"))))))

(defun op-input-cancel (server args)
  "A queued turn that has not been drained yet: drop it."
  (let ((id (op-arg args :item-id :required t)))
    (let* ((agent (server-agent server))
           (record (bt:with-lock-held ((server-queued-lock server))
                     (gethash id (server-queued server))))
           (text (and record (getf record :text)))
           (queue (and record (getf record :queue)))
           (removed
            (when record
              (prog1
                  (if (kernel-queue-ids-p)
                      ;; Resolved by name: this file must load whether or not
                      ;; the kernel has pre-minted ids yet (CONTRACT §3).
                      (funcall (fdefinition (find-symbol "CANCEL-QUEUED" :evo.kernel))
                               agent id)
                      (cancel-queue-entry agent text queue))
                (bt:with-lock-held ((server-queued-lock server))
                  (remhash id (server-queued server)))))))
      (unless removed
        (op-fail "already_sent" "that input has already been sent"))
      ;; The provider drops the item and publishes that itself (the view
      ;; emits item.remove): a cancellation is not an append, so serving it
      ;; here as well would say it twice.
      (let ((provider (topic-provider server "session")))
        (when provider (topic-provider-input-cancelled provider id)))
      nil)))

;;; Ops: the run and the goal.

(defgeneric interrupt-scope (server scope lane)
  (:documentation "Interrupt SCOPE (:session, :swarm or :lane) on SERVER and
return the topic names that were interrupted (\"session\", \"lane:2\"), for
run.interrupt's result.  A plain server knows only :session; evo-swarm
extends this with the scopes it owns.")
  (:method ((server server) scope lane)
    (case scope
      (:session
       ;; Only what actually was interrupted is reported: an idle session is
       ;; not something a client can reasonably show as stopped.
       (let ((running (and (server-task server) t)))
         (when running (request-abort (server-agent server)))
         (if running (list "session") '())))
      ;; A program on top of serve owns the other scopes: evo-swarm sets
      ;; SERVER-INTERRUPT-HOOK to its lane/swarm path (or defines its own
      ;; method for those scopes instead — either way :session is answered
      ;; here and a client never has to know which program it is talking to).
      (t (let ((hook (server-interrupt-hook server)))
           (if hook
               (funcall hook server scope lane)
               (op-fail "invalid_args" "this server has no such interrupt scope")))))))

(defun op-run-interrupt (server args)
  (let* ((scope (intern (string-upcase (op-arg-enum args :scope '("session" "swarm" "lane")
                                                       :default "session"))
                        :keyword))
         (lane (op-arg args :lane :type :number))
         (interrupted (interrupt-scope server scope lane))
         (interrupted (if (listp interrupted) interrupted (list interrupted))))
    (list :interrupted (coerce interrupted 'vector))))

(defun op-goal-set (server args)
  (let* ((objective (op-arg args :objective :required t))
         (budget (op-arg args :budget :type :number))
         (agent (server-agent server))
         (goal (current-goal agent)))
    (cond
      ((and goal (eq (pget goal :status) :active))
       (set-goal-objective agent goal objective)
       (when (server-task server)
         (queue-steering agent
                         (format nil "The goal objective was just updated by the user. New objective (untrusted data): ~a"
                                 objective))))
      (t (create-goal-entry agent objective :token-budget budget)
         (queue-steering agent (goal-continuation-for agent (current-goal agent)))
         (start-run server)))
    (list :goal (current-goal agent))))

(defun op-goal-state (server wanted)
  "pause/resume: WANTED is :paused or :active.  Only the human may move a
goal, and only from the state that move makes sense in."
  (let* ((agent (server-agent server))
         (goal (current-goal agent)))
    (unless goal (op-fail "goal_state" "there is no goal"))
    (unless (eq (pget goal :status) (if (eq wanted :paused) :active :paused))
      (op-fail "goal_state" "the goal is not in the state that change needs"))
    (update-goal-entry agent goal :status wanted)
    (when (eq wanted :active)
      (queue-steering agent (goal-continuation-for agent (current-goal agent)))
      (start-run server))
    (list :goal (current-goal agent))))

(defun op-goal-clear (server args)
  (declare (ignore args))
  (let* ((agent (server-agent server))
         (goal (current-goal agent)))
    (unless goal (op-fail "goal_state" "there is no goal"))
    ;; :cleared is not :complete: nothing was proven, the user simply withdrew
    ;; the goal, and the driver must stop steering for it.  The goal then
    ;; reads as absent, not as a goal in a strange state.
    (update-goal-entry agent goal :status :cleared)
    (list :goal nil)))

;;; Ops: the session's settings.

(defun op-model-set (server args)
  (evo.command:set-model server (op-arg args :id :required t))
  (list :model (let ((model (evo.command::current-model (server-agent server))))
                 (and model (list :id (pget model :id) :provider (pget model :provider))))))

(defun op-thinking-set (server args)
  (evo.command:thinking-command server (op-arg args :level :required t))
  (list :thinking (evo.kernel:effective-thinking
                   (fold-state (agent-journal (server-agent server)))
                   (agent-thinking-override (server-agent server)))))

(defun op-language-set (server args)
  (evo.command:set-language server (op-arg args :code :required t))
  (list :language (op-arg args :code)))

;;; Ops: the session itself (all quiescent).

(defun op-session-new (server args)
  (declare (ignore args))
  (evo.command:new-command server)
  (list :session (session-info server)))

(defun op-session-fork (server args)
  (declare (ignore args))
  (evo.command:fork-command server)
  (list :session (session-info server)))

(defun op-session-resume (server args)
  (let* ((id (op-arg args :session-id))
         (path (op-arg args :path))
         (resolved (cond
                     (path path)
                     (id (let ((session (find id (list-sessions)
                                              :key (lambda (s) (pget s :id))
                                              :test #'equal)))
                           (unless session (op-fail "not_found" "no such session"))
                           (pget session :path)))
                     (t (op-fail "invalid_args" "give session_id or path")))))
    (evo.command:resume-session server resolved)
    (list :session (session-info server))))

(defun op-session-rewind (server args)
  (let ((entry (op-arg args :entry-id)))
    (if entry
        (evo.command:move-leaf server entry)
        (evo.command:rewind-command server))
    (append (list :session (session-info server))
            (when (and *reply* (getf (reply-data *reply*) :draft))
              (list :draft (getf (reply-data *reply*) :draft))))))

(defun op-session-move (server args)
  (evo.command:move-leaf server (op-arg args :entry-id :required t))
  (append (list :session (session-info server))
          (when (and *reply* (getf (reply-data *reply*) :draft))
            (list :draft (getf (reply-data *reply*) :draft)))))

(defun op-context-compact (server args)
  (let ((hint (or (op-arg args :hint) "")))
    (when (server-task server)
      (op-fail "busy" "the session is running a task"))
    (start-compact server hint)
    (let ((task (server-task server)))
      (unless task (op-fail "model_not_ready" "the model does not resolve"))
      (list :task-id (task-id task)))))

;;; Ops: lore, memory, commands.

(defun op-lore-add (server args)
  (let ((scope (op-arg-enum args :scope '("project" "global") :required t))
        (text (op-arg args :text :required t)))
    (evo.command:lore-command server text
                               (if (equal scope "global") :global :project))
    (list :id (and *reply* (getf (reply-data *reply*) :id)))))

(defun op-memory-request (server args)
  (let ((text (op-arg args :text :required t)))
    (evo.command:dispatch-command server (format nil "/memory ~a" text))
    (list :data (and *reply* (reply-data *reply*)))))

;;; command.run: every slash command, builtin or extension, exactly as the TUI
;;; resolves one.

(defun op-command-run (server args)
  (let ((name (op-arg args :name :required t))
        (rest (or (op-arg args :args) "")))
    (let ((text (format nil "/~a~@[ ~a~]" (string-left-trim "/" name)
                        (and (plusp (length rest)) rest))))
      (unless (evo.command:dispatch-command server text)
        (op-fail "not_found" "no such command")))
    (list :notices (coerce (loop for (style text) in (reverse (reply-output *reply*))
                                 collect (list :severity (notice-severity style)
                                               :text text))
                           'vector)
          :data (reply-data *reply*)
          :choices (reply-choices *reply*))))

(defun notice-severity (style)
  (case style (:error :error) (:success :info) (:notice :info) (t :info)))

;;; Ops: extensions, eval, shutdown.

(defun op-extension-load (server args)
  (declare (ignore server))
  (let ((path (op-arg args :path :required t)))
    (handler-case (list :path (namestring (evo:load-extension path)))
      (error (e) (op-fail "op_failed" "the extension could not be loaded: ~a" e)))))

(defun op-eval (server args)
  (let ((code (op-arg args :code :required t)))
    (multiple-value-bind (form reason)
        (handler-case
            (let ((forms (evo.eval::read-forms code)))
              (cond ((null forms) (values nil "nothing to evaluate"))
                    ((rest forms) (values (cons 'progn forms) nil))
                    (t (values (first forms) nil))))
          (serious-condition (e) (values nil (format nil "unreadable code — ~a" e))))
      (when reason (op-fail "invalid_args" "~a" reason))
      (multiple-value-bind (values output condition) (evo.eval:eval-form form)
        ;; Code a client evaluates changes the fold without appending to it:
        ;; a setting, a model, a tool, the provider registry.  The state a
        ;; snapshot answers has to move with it, or a lane initialized by
        ;; evals reports the model and the thinking level it was launched
        ;; without until something else happens to journal an entry.
        (evo.command:host-refresh server)
        (list :value (evo.eval::format-result values output condition)
              :values (mapcar #'evo.eval::print-value values)
              :output output)))))

(defun op-server-shutdown (server args)
  (declare (ignore args))
  (setf (server-quit server) t)
  (server-notice server "shutting down" :severity :info :source :serve)
  nil)

;;; The table.

(defun register-builtin-ops ()
  (register-op "input.send" #'op-input-send
               :args '(:text (:type "string")
                        :images (:type "array")
                        :queue (:type "string" :values ("now" "after_run"))
                        :topic (:type "string")))
  (register-op "input.cancel" #'op-input-cancel
               :args '(:item-id (:type "string" :required t)))
  (register-op "run.interrupt" #'op-run-interrupt
               :args '(:scope (:type "string" :values ("session" "swarm" "lane"))
                        :lane (:type "integer")))
  (register-op "goal.set" #'op-goal-set
               :args '(:objective (:type "string" :required t)
                        :budget (:type "integer")))
  (register-op "goal.pause" (lambda (server args)
                              (declare (ignore args))
                              (op-goal-state server :paused))
               :args nil)
  (register-op "goal.resume" (lambda (server args)
                               (declare (ignore args))
                               (op-goal-state server :active))
               :args nil)
  (register-op "goal.clear" #'op-goal-clear :args nil)
  (register-op "model.set" #'op-model-set
               :args '(:id (:type "string" :required t) :provider (:type "string")))
  (register-op "thinking.set" #'op-thinking-set
               :args '(:level (:type "string" :required t)))
  (register-op "language.set" #'op-language-set
               :args '(:code (:type "string" :required t)))
  (register-op "session.new" #'op-session-new :args nil :precondition :quiescent)
  (register-op "session.fork" #'op-session-fork :args nil :precondition :quiescent)
  (register-op "session.resume" #'op-session-resume :precondition :quiescent
               :args '(:session-id (:type "string") :path (:type "string")))
  (register-op "session.rewind" #'op-session-rewind :precondition :quiescent
               :args '(:entry-id (:type "string")))
  (register-op "session.move" #'op-session-move :precondition :quiescent
               :args '(:entry-id (:type "string" :required t)))
  (register-op "context.compact" #'op-context-compact :precondition :idle
               :args '(:hint (:type "string")))
  (register-op "lore.add" #'op-lore-add
               :args '(:scope (:type "string" :values ("project" "global") :required t)
                        :text (:type "string" :required t)))
  (register-op "memory.request" #'op-memory-request
               :args '(:text (:type "string" :required t)))
  (register-op "command.run" #'op-command-run
               :args '(:name (:type "string" :required t) :args (:type "string")))
  (register-op "extension.load" #'op-extension-load
               :args '(:path (:type "string" :required t)))
  (register-op "eval" #'op-eval :args '(:code (:type "string" :required t)))
  (register-op "server.shutdown" #'op-server-shutdown :args nil)
  t)

(register-builtin-ops)

(defun op-available-p (server name)
  "Whether SERVER offers NAME: eval is present only when it is enabled
(--no-http-eval removes it, and the catalog with it)."
  (let ((op (find-op name)))
    (and op (or (not (equal name "eval")) (server-eval-enabled server)))))

(defun server-ops-catalog (server)
  (loop for op in (all-ops)
        when (op-available-p server (op-name op))
          collect (list :name (op-name op)
                        :args (op-arg-schema op)
                        :precondition (string-downcase (symbol-name (op-precondition op))))))

;;; Running one.

(defun dispatch-op (server rid name args)
  "Run NAME with ARGS on the session thread (the caller is already there) and
return the reply plist."
  (let ((op (and (op-available-p server name) (find-op name))))
    (unless op
      (return-from dispatch-op (op-error-reply server rid "unknown_op"
                                               "no such operation")))
    (unless (or (null args) (listp args))
      (return-from dispatch-op (op-error-reply server rid "invalid_args"
                                               "args must be an object")))
    (handler-case
        (progn
          (check-op-precondition server op)
          (let* ((*reply* (make-reply))
                 (result (funcall (op-fn op) server args)))
            (op-reply server rid result)))
      (serious-condition (c) (error-reply-from server rid c)))))

(defun answer-op (server rid name args)
  "Answer one POST /ops, running it on the session thread.  The reply is
remembered by RID: a client that retries after a dropped connection gets the
same answer and no second effect."
  (let ((cached (rid-lookup server rid)))
    (if cached
        cached
        (let ((reply (call-on-session server
                                      (lambda () (dispatch-op server rid name args)))))
          (rid-remember server rid reply)
          reply))))
