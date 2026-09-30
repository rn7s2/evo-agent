;;;; project.lisp — the journal, projected into the view model.
;;;;
;;;; PROJECT-JOURNAL is a pure function: a journal in, the items a frontend
;;;; draws and the topic state out.  It is the whole definition of what the
;;;; view model means, and the live view (view.lisp) is an optimization of it —
;;;; it applies kernel events and journal appends to a projection it holds, so
;;;; a long session is never re-projected to render one delta.  The two must
;;;; agree, which is what the unit suite's property test pins down: applying
;;;; the ops a run emitted to the projection of the journal it started from
;;;; gives the projection of the journal it ended with.
;;;;
;;;; Items cover the WHOLE root→leaf path, compactions included: a `compaction'
;;;; item marks the point where the model's context was replaced by a summary,
;;;; and the scrollback keeps everything before it.  The fold — what the model
;;;; actually sees — is a different thing and lives in the kernel.

(in-package :evo.view)

;;; Time.  Everything on the wire is epoch milliseconds.

(defun now-ms ()
  "The current time, epoch milliseconds."
  (let ((ts (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix ts))
       (floor (local-time:nsec-of ts) 1000000))))

(defun iso->ms (stamp)
  "An ISO-8601 UTC timestamp (a journal entry's :timestamp) as epoch ms, or NIL."
  (when (and (stringp stamp) (plusp (length stamp)))
    (ignore-errors
     (* 1000 (local-time:timestamp-to-unix (local-time:parse-timestring stamp))))))

(defun entry-ms (entry)
  (or (iso->ms (pget entry :timestamp)) (now-ms)))

;;; Message content.

(defun concat-strings (strings)
  (with-output-to-string (out) (dolist (s strings) (write-string (or s "") out))))

(defun content-blocks (message)
  "MESSAGE's content as a list of blocks.  A bare string counts as one text
block: the provider layer always writes blocks, but a hand-built or old entry
may not."
  (let ((content (pget message :content)))
    (cond ((null content) nil)
          ((stringp content) (list (list :type :text :text content)))
          ((vectorp content) (coerce content 'list))
          ((listp content) content)
          (t nil))))

(defun blocks-text (blocks)
  "The text of BLOCKS, concatenated — not joined: it is what the streaming
deltas accumulated to."
  (concat-strings (loop for block in blocks
                        when (eq (pget block :type) :text)
                          collect (pget block :text))))

(defun blocks-thinking (blocks)
  (concat-strings (loop for block in blocks
                        when (eq (pget block :type) :thinking)
                          collect (pget block :thinking))))

(defun blocks-images (blocks)
  (remove-if-not (lambda (b) (eq (pget b :type) :image)) blocks))

(defun image-wires (id blocks)
  "The image descriptors for an item: name, media type, size and where to fetch
the bytes.  A vector, never a list — an empty JSON array is not null."
  (coerce (loop for block in (blocks-images blocks)
                for n from 0
                collect (list :name (or (pget block :name) "image")
                              :media-type (pget block :media-type)
                              :bytes (or (pget block :bytes) 0)
                              :href (format nil "/media/~a/~d" id n)))
          'vector))

(defun usage-wire (usage)
  (when usage
    (list :input (or (pget usage :input) 0)
          :output (or (pget usage :output) 0)
          :cache-read (or (pget usage :cache-read) 0)
          :cache-write (or (pget usage :cache-write) 0))))

;;; Truncation.  A snapshot and an op carry a bounded item; VIEW-ITEM and
;;; GET /items/<id> carry the whole one.

(defparameter *thinking-max-chars* (* 16 1024))
(defparameter *result-max-chars* 4096)
(defparameter *truncation-marker* " … [truncated]")

(defun truncate-text (text limit)
  "TEXT cut to at most LIMIT characters, the marker included: a bound a client
can rely on is worth more than the last few words."
  (if (<= (length text) limit)
      text
      (concatenate 'string
                   (subseq text 0 (max 0 (- limit (length *truncation-marker*))))
                   *truncation-marker*)))

(defun truncate-thinking (item)
  (let ((thinking (pget item :thinking)))
    (if (and (stringp thinking) (> (length thinking) *thinking-max-chars*))
        (pput item :thinking (truncate-text thinking *thinking-max-chars*))
        item)))

(defun truncate-result (item)
  (let* ((result (pget item :result))
         (text (and result (pget result :text))))
    (if (and (stringp text) (> (length text) *result-max-chars*))
        (pput item :result (list :text (truncate-text text *result-max-chars*)
                                 :chars (length text)
                                 :truncated t))
        item)))

(defun truncate-item (item)
  "ITEM as a snapshot or an op carries it."
  (case (pget item :kind)
    ("assistant" (truncate-thinking item))
    ("tool" (truncate-result item))
    (t item)))

(defun result-wire (text)
  (list :text (or text "")
        :chars (length (or text ""))
        :truncated :false))

;;; Item construction, one function per kind.
;;;
;;; Enum VALUES are strings, not keywords: the wire wants snake_case
;;; (`lane_report', `after_run', `objective_updated') and the serve encoder
;;; renders a keyword value as a lowercased symbol name, dashes intact.

(defun enum-string (value)
  "VALUE as the wire's snake_case string: :after-run -> \"after_run\".  A string
passes through, NIL stays NIL."
  (cond ((null value) nil)
        ((stringp value) value)
        ((symbolp value) (substitute #\_ #\- (string-downcase (symbol-name value))))
        (t (princ-to-string value))))

(defun user-item (id ts &key text images (status "sent") (queue "now"))
  (list :id id :kind "user" :ts ts
        :text (or text "") :images (or images #())
        :status status :queue queue))

(defun assistant-item (id ts message provider)
  (let* ((stop (pget message :stop-reason))
         (status (case stop (:error "error") (:aborted "aborted")
                        (:length "length") (t "final")))
         (item (list :id id :kind "assistant" :ts ts
                     :text (blocks-text (content-blocks message))
                     :thinking (blocks-thinking (content-blocks message))
                     :status status
                     :error (pget message :error-message)
                     :model (pget message :model)
                     :provider provider))
         ;; USAGE is left out when the model reported none: absent says
         ;; "unknown", where a null reads as zero tokens.
         (usage (usage-wire (pget message :usage))))
    (if usage (pput item :usage usage) item)))

(defun tool-item (id ts &key call-id name args status result parent)
  (list :id id :kind "tool" :ts ts
        :call-id call-id
        :name name
        :args args
        :status status
        :result result
        :parent parent))

(defun tool-id (call-id) (format nil "t_~a" call-id))

;;; Origins: a message the session did not type.  The entry carries the facts
;;; as a plist beside the text the model reads, so nobody parses the prose.

(defun origin-item (entry origin)
  "The item ORIGIN asks for, or NIL: an origin whose :kind names no item kind
is left to the ordinary rules (the message is shown as what it says it is)."
  (let ((id (pget entry :id)) (ts (entry-ms entry)) (kind (pget origin :kind)))
    (case kind
      (:lane-report
       (list :id id :kind "lane_report" :ts ts
             :lane (pget origin :lane)
             :done (pget origin :done)
             :evidence (pget origin :evidence)
             :next (pget origin :next)
             :blocked (pget origin :blocked)
             :requests (pget origin :requests)
             :goal (enum-string (pget origin :goal))))
      (:lane-event
       (list :id id :kind "lane_event" :ts ts
             :lane (pget origin :lane)
             :event (enum-string (pget origin :event))
             :outcome (pget origin :outcome)
             :goal-status (enum-string (pget origin :goal-status))
             :detail (pget origin :detail)
             :severity (enum-string (pget origin :severity))))
      (:goal
       (list :id id :kind "goal" :ts ts
             :event (enum-string (pget origin :event))
             :goal-id (pget origin :goal-id)
             :objective (pget origin :objective)
             :budget (pget origin :budget)
             :tokens (pget origin :tokens)))
      (:command-note
       (list :id id :kind "command_note" :ts ts
             :command (pget origin :command)
             :text (pget origin :text)))
      (:human-action
       (list :id id :kind "human_action" :ts ts
             :action (enum-string (pget origin :action))
             :lanes (coerce (or (pget origin :lanes) #()) 'vector)))
      (:context
       (list :id id :kind "context" :ts ts
             :key (pget origin :key)
             :text (or (pget origin :text) (message-text (pget entry :message)))))
      (:notice
       (list :id id :kind "notice" :ts ts
             :severity (enum-string (or (pget origin :severity) :info))
             :text (or (pget origin :text) (message-text (pget entry :message)))
             :source (enum-string (or (pget origin :source) :extension))
             :durable t))
      (t nil))))

(defun message-text (message)
  (blocks-text (content-blocks message)))

;;; The goal item a :goal ENTRY implies.  The entry is state (it is the fold);
;;; the item is the transition it made, so a settle that only moved the token
;;; count adds nothing to the transcript.

(defun goal-entry-event (entry previous)
  (let ((status (pget entry :status))
        (prev-status (pget previous :status))
        (id (pget entry :goal-id))
        (prev-id (pget previous :goal-id)))
    (cond ((null previous) "created")
          ((not (equal id prev-id)) "created")
          ((and (eq status :complete) (not (eq prev-status :complete))) "complete")
          ((and (eq status :cleared) (not (eq prev-status :cleared))) "cleared")
          ((and (eq status :paused) (not (eq prev-status :paused))) "paused")
          ((and (eq status :budget-limited)
                (not (eq prev-status :budget-limited))) "budget_limited")
          ((and (eq prev-status :paused) (eq status :active)) "resumed")
          ((and (pget entry :objective)
                (not (equal (pget entry :objective) (pget previous :objective))))
           "objective_updated")
          (t nil))))

(defun goal-entry-item (entry previous)
  (let ((event (goal-entry-event entry previous)))
    (when event
      (list :id (pget entry :id) :kind "goal" :ts (entry-ms entry)
            :event event
            :goal-id (pget entry :goal-id)
            :objective (pget entry :objective)
            :budget (pget entry :token-budget)
            :tokens (pget entry :tokens-used)))))

;;; The walk.

(defstruct pctx
  (calls (make-hash-table :test #'equal))  ; tool call id -> (:name :arguments)
  (assistant nil)                          ; id of the assistant item in scope
  (goal nil)                               ; last :goal entry on the path
  (recover nil)                            ; last :recover entry on the path
  (fold nil))                              ; the session's fold, for resolution

(defun model-provider-for (fold model-id)
  "Which provider serves MODEL-ID for this session: the only one registered,
or the session's journaled choice when the id is registered under several."
  (let ((providers (ignore-errors (model-providers model-id))))
    (cond ((null providers) nil)
          ((null (cdr providers)) (car providers))
          (t (or (state-model-provider fold) (car providers))))))

(defun note-tool-calls (ctx message)
  "Remember the arguments of every tool call in MESSAGE, so the tool-result
entry that follows can be given them: the call is inside the assistant
message, the result is its own entry."
  (dolist (block (content-blocks message))
    (when (eq (pget block :type) :tool-call)
      (setf (gethash (pget block :id) (pctx-calls ctx))
            (list :name (pget block :name) :arguments (pget block :arguments))))))

(defun entry->item (entry ctx)
  "The item ENTRY contributes to the view, or NIL when it contributes none.
CTX carries what the walk has to remember between entries."
  (let ((type (pget entry :type)))
    (case type
      (:message
       (let* ((message (pget entry :message))
              (role (pget message :role))
              (origin (pget entry :origin))
              (id (pget entry :id)))
         (case role
           (:user
            (or (and origin (origin-item entry origin))
                (user-item id (entry-ms entry)
                           :text (message-text message)
                           :images (image-wires id (content-blocks message)))))
           (:assistant
            (note-tool-calls ctx message)
            (setf (pctx-assistant ctx) id)
            (assistant-item id (entry-ms entry) message
                            (model-provider-for (pctx-fold ctx)
                                                (pget message :model))))
           (:tool-result
            (let* ((call-id (pget message :tool-call-id))
                   (call (gethash call-id (pctx-calls ctx))))
              (tool-item (tool-id call-id) (entry-ms entry)
                         :call-id call-id
                         :name (or (pget message :tool-name) (pget call :name))
                         :args (pget call :arguments)
                         :status (if (pget message :is-error) "error" "ok")
                         :result (result-wire
                                  (result-display-text (content-blocks message)))
                         :parent (pctx-assistant ctx))))
           (t nil))))
      (:custom-message
       (let ((key (pget entry :key))
             (origin (pget entry :origin)))
         (cond
           ((equal key "recovery")
            (let ((recovery (pctx-recover ctx)))
              (list :id (pget entry :id) :kind "recovery" :ts (entry-ms entry)
                    :status (enum-string (and recovery (pget recovery :status)))
                    :code (pget recovery :code)
                    :attempt (pget recovery :attempt)
                    :reason (pget recovery :reason))))
           ;; An injection may carry its own kind (EVO:INJECT-CONTEXT takes an
           ;; :origin); anything else is a context item keyed by its entry.
           ((and origin (origin-item entry origin)))
           (t (list :id (pget entry :id) :kind "context" :ts (entry-ms entry)
                    :key key
                    :text (message-text (pget entry :message)))))))
      (:recover (setf (pctx-recover ctx) entry) nil)
      (:notice
       (list :id (pget entry :id) :kind "notice" :ts (entry-ms entry)
             :severity (enum-string (or (pget entry :severity) :info))
             :text (or (pget entry :text) "")
             :source (enum-string (pget entry :source))
             :durable t))
      (:branch-summary
       (when (pget entry :summary)
         (list :id (pget entry :id) :kind "context" :ts (entry-ms entry)
               :key "branch-summary" :text (pget entry :summary))))
      (:compaction
       (list :id (pget entry :id) :kind "compaction" :ts (entry-ms entry)
             :summary (or (pget entry :summary) "")
             :tokens-before (pget entry :tokens-before)
             :tokens-after (pget entry :tokens-after)
             :manual (and (pget entry :manual) t)))
      (:goal
       (let ((item (goal-entry-item entry (pctx-goal ctx))))
         (setf (pctx-goal ctx) entry)
         item))
      (t nil))))

;;; State.

(defun fold-model-id (fold)
  "The model the journal names, without an agent to ask for an override."
  (or (state-model fold) (setting :model)))

(defun todo-wire (todos)
  "The session's checklist as state wants it: text + a snake_case status."
  (coerce (loop for entry across (or todos #())
                collect (list :text (or (pget entry :text) "")
                              :status (enum-string (or (pget entry :status) :pending))))
          'vector))

(defun model-state (model-id model)
  (list :id model-id
        :provider (and model (pget model :provider))
        :ready (and model t)
        :reason (unless model
                  (if model-id
                      (format nil "model ~a is not registered" model-id)
                      "no model is configured"))))

(defun goal-state (goal live-tokens)
  "The goal as state wants it, or NIL: a cleared goal is gone (goal.clear), so
the state says there is no goal rather than one with a status nobody acts on."
  (when (and goal (not (eq (pget goal :status) :cleared)))
    (list :goal-id (pget goal :goal-id)
          :objective (pget goal :objective)
          :status (enum-string (pget goal :status))
          :budget (pget goal :token-budget)
          :tokens (+ (or (pget goal :tokens-used) 0) (or live-tokens 0)))))

(defun session-state (journal)
  (let ((header (journal-header journal)))
    (list :id (pget header :id)
          :path (namestring (journal-path journal))
          :leaf (journal-leaf-id journal)
          :program (or (pget header :program) "evo-agent")
          :started-at (iso->ms (pget header :timestamp)))))

(defun state-model-label (model)
  "The status line's name for the session's model, from the state's model
object: the id, with the provider in parentheses when the id alone does not
identify the endpoint (the same rule MODEL-LABEL-TEXT applies in the TUI)."
  (let ((id (pget model :id))
        (provider (pget model :provider)))
    (cond ((null id) "(no model)")
          ((and provider (< 1 (length (ignore-errors (model-providers id)))))
           (format nil "~a (~(~a~))" id provider))
          (t id))))

(defun state-segment-context (state)
  "The context plist the status segments read, built from the topic state — the
same keys the TUI fills from its own slots, so both render the same line."
  (let ((goal (pget state :goal))
        (context (pget state :context)))
    (list :model-label (state-model-label (pget state :model))
          :thinking (pget state :thinking)
          :context-tokens (pget context :tokens)
          :context-window (pget context :window)
          ;; Journal shape, which is what GOAL-LABEL-TEXT renders; :tokens
          ;; already carries what a live run has spent.
          :goal (and goal (list :goal-id (pget goal :goal-id)
                                :status (pget goal :status)
                                :token-budget (pget goal :budget)
                                :tokens-used (pget goal :tokens)))
          :goal-run-tokens 0
          :jobs (running-jobs-summary))))

(defun state-segments (state)
  "The topic's status segments, as CONTRACT §4.2 asks for them: name, order,
side, the text, and the segment's own structured data.  Names and sides are
already wire strings — a client must not have to know a keyword."
  (coerce (loop for cell in (status-segments-live (state-segment-context state))
                collect (list :name (enum-string (pget cell :name))
                              :order (pget cell :order)
                              :side (enum-string (pget cell :side))
                              :text (pget cell :text)
                              :data (pget cell :data)))
          'vector))

(defun journal-state (journal &key agent status task queue jobs live-goal-tokens context)
  "The topic state a journal implies.  The runtime facts a journal cannot know
— what is running, what is queued, what holds it, what a live run has spent —
come from the caller."
  (let* ((fold (fold-state journal))
         (model-id (handler-case
                       (if agent
                           (or (state-model fold)
                               (ignore-errors (agent-model-override agent))
                               (setting :model))
                           (fold-model-id fold))
                     (error () nil)))
         (model (and model-id
                     (handler-case (find-model model-id (model-provider-for fold model-id))
                       (error () nil))))
         (messages (state-messages fold))
         (state (list :status (or status "idle")
                      :task task
                      :model (model-state model-id model)
                      :thinking (string-downcase
                                 (string (effective-thinking
                                          fold
                                          (and agent
                                               (ignore-errors
                                                (agent-thinking-override agent))))))
                      :language (pget (resolve-language (language-request fold)) :code)
                      :context (or context
                                   (list :tokens (estimate-context-tokens messages)
                                         :window (and model (model-context-window model))
                                         :source "estimate"))
                      :goal (goal-state (state-goal fold) live-goal-tokens)
                      :todos (todo-wire (custom-state fold "todo"))
                      :queue (coerce (or queue #()) 'vector)
                      :jobs (coerce (or jobs #()) 'vector)
                      :segments #()
                      :session (session-state journal))))
    ;; The status segments read the state, so they are evaluated last.
    (setf (getf state :segments) (state-segments state))
    state))

(defun queued-ids (items)
  "The ids of the user items still waiting to be sent, oldest first."
  (loop for item across (or items #())
        when (and (equal (pget item :kind) "user")
                  (equal (pget item :status) "queued"))
          collect (pget item :id)))

(defun project-items (journal &key fold)
  "The items of JOURNAL's current root→leaf path, in order."
  (let ((ctx (make-pctx :fold fold))
        (items nil))
    (dolist (entry (entry-path journal) (values (coerce (nreverse items) 'vector) ctx))
      (let ((item (entry->item entry ctx)))
        (when item (push item items))))))

(defun project-journal (journal &optional agent)
  "The view model of JOURNAL: (values items state).

AGENT, when given, supplies the facts a journal cannot record on its own — the
model override the process was launched with, the tokens an in-flight run of
the session's goal has spent.  Without one this is a pure function of the
journal."
  (let* ((fold (fold-state journal))
         (items (project-items journal :fold fold)))
    (values items
            (journal-state journal :agent agent :queue (queued-ids items)))))
