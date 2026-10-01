;;;; command.lisp — the command layer: what a slash command DOES, once, for
;;;; every frontend.
;;;;
;;;; A frontend decides how a command is typed and how its answer looks; this
;;;; file decides what `/goal`, `/model`, `/compact`, `/tree`, `/resume`, ...
;;;; do to the session.  The TUI and `evo serve` both dispatch through
;;;; DISPATCH-COMMAND, so the same words have the same effect wherever they are
;;;; typed — which is the whole point: anything a human can do in the TUI, a
;;;; coordinator can do over HTTP, and neither can drift from the other.
;;;;
;;;; The frontend is reached through the HOST protocol below.  A host owns the
;;;; session's one task (design.md §6) and its output; the command layer only
;;;; ever asks it things — is a task running, start a run for the steering I
;;;; just queued, show this line, offer these choices — and never touches its
;;;; state.  Methods specialize on the frontend's own object (the TUI's struct,
;;;; serve's server), so the core names no frontend.
;;;;
;;;; A command that cannot run now does not print and carry on: it signals
;;;; COMMAND-REFUSED, which unwinds the command and tells the frontend why.
;;;; The TUI shows the reason dimmed, exactly where it always did; serve turns
;;;; it into an HTTP status (409 for a busy session, 400 for bad arguments).
;;;;
;;;; Resolution order is the TUI's, unchanged: extension commands, then
;;;; builtins (the frontend's own presentational ones first), then skills,
;;;; then prompt templates.

(in-package :evo.command)

;;; The host protocol.

(defgeneric host-agent (host)
  (:documentation "The agent HOST's session runs."))

(defgeneric host-running-p (host)
  (:documentation "True while HOST owns a live task (a run or a compaction)."))

(defgeneric host-start-run (host)
  (:documentation "Start a run worker for the steering queued on HOST's agent,
unless a task is already running (it will drain the queue itself).  The host
applies its model gate: input it cannot run yet stays queued."))

(defgeneric host-start-compact (host hint)
  (:documentation "Start a manual compaction task with HINT (may be empty)."))

(defgeneric host-notice (host text &key severity durable data)
  (:documentation "Show TEXT to the user, as a notice.

SEVERITY is :INFO (the default), :WARN or :ERROR — the three levels every
frontend renders, and the whole of what a notice says about itself; the colours
and dimming a terminal used to apply per call site are the frontend's business.

DURABLE also JOURNALS the notice (a :notice entry: invisible to the model,
ignored by the context fold), so it survives a rebuild and a restart.  That is
for the facts a session should still show later — a goal transition, a run that
died, what a compaction did — while an ephemeral notice lives only in the
frontend that showed it.

DATA is an arbitrary plist of notice payload.  Its :source names the producer
(:command, :extension, :swarm, :serve or :goal) and defaults to :command.")
  (:method ((host t) text &key severity durable data)
    (when durable
      (journal-notice (host-agent host) text
                      :severity (or severity :info)
                      :source (or (pget data :source) :command)
                      :data data))))

(defun journal-notice (agent text &key severity source data)
  "Append a :notice entry — what a durable notice leaves behind."
  (append-entry (agent-journal agent)
                (append (list :type :notice
                              :severity (or severity :info)
                              :text text
                              :source (or source :command))
                        (when data (list :data data)))))

(defgeneric host-refresh (host)
  (:documentation "The session's fold state changed (goal, model, leaf...):
re-derive whatever the host caches from it.")
  (:method ((host t)) nil))

(defgeneric host-choose (host title items action &key index)
  (:documentation "Offer ITEMS to choose from; call ACTION with the chosen
value.  An item is (LABEL VALUE DESCRIPTION) or (LABEL . VALUE); INDEX is the
preselected one.  An interactive host opens a picker.  The default method
cannot ask, so it lists the choices instead — the same command then takes the
choice as its argument (`/model <id>`, `/resume <n>`, `/tree <id>`)."))

(defgeneric host-set-draft (host text)
  (:documentation "Hand TEXT back to the user for editing — the text of a user
message the leaf was just moved above, so editing and resubmitting it branches.")
  (:method ((host t) text)
    (host-notice host text)))

(defgeneric host-session-switched (host)
  (:documentation "The agent now runs another journal: drop anything shown
from the old one.")
  (:method ((host t)) nil))

(defgeneric host-show-history (host)
  (:documentation "A session was just resumed: show the tail of its history,
where the host can show one.")
  (:method ((host t)) nil))

(defgeneric host-submit (host text &optional images)
  (:documentation "Send TEXT (and IMAGES) to the agent as the user's own turn,
exactly as if it had been typed.")
  (:method ((host t) text &optional images)
    (queue-steering (host-agent host) text :images images :from-user t)
    (host-start-run host)))

(defgeneric host-command-context (host)
  (:documentation "Extra keys this host adds to an extension command's context
plist (the TUI passes :tui).")
  (:method ((host t)) nil))

(defgeneric host-interrupt-hint (host)
  (:documentation "How the user interrupts a running task on this host, for
the text of a refusal.")
  (:method ((host t)) "interrupt it first"))

(defgeneric host-data (host key value)
  (:documentation "Report a structured result of the command in progress —
the goal after /goal, the path /export wrote.  A host that answers with data
(serve) collects it; one that only shows text ignores it.")
  (:method ((host t) key value)
    (declare (ignore key value))
    nil))

(defgeneric host-command-failed (host name condition)
  (:documentation "An extension command /NAME signalled CONDITION.")
  (:method ((host t) name condition)
    (host-notice host (format nil "✗ /~a: ~a" name condition) :severity :error)))

;;; Refusals.

(define-condition command-refused (error)
  ((text :initarg :text :reader command-refused-text)
   (kind :initarg :kind :initform :conflict :reader command-refused-kind
         :documentation ":CONFLICT — not now, the session is busy; :INVALID —
not with these arguments; :NOT-FOUND — no such thing."))
  (:report (lambda (c s) (write-string (command-refused-text c) s))))

(defun refuse (kind control &rest args)
  "Unwind the command in progress with a reason the frontend shows."
  (error 'command-refused :kind kind :text (apply #'format nil control args)))

(defmacro with-refusals-shown ((host) &body body)
  "Run BODY; a refusal becomes a dim line on HOST instead of an unwind past
the caller.  For callers with no dispatcher above them — a picker's action runs
long after the command that opened it returned.  Returns BODY's value, or NIL
when it was refused."
  (let ((c (gensym "C")))
    `(handler-case (progn ,@body)
       (command-refused (,c)
         (host-notice ,host (command-refused-text ,c) :severity :warn)
         nil))))

;;; Quiescence (design.md §6).

(defun session-quiescent-p (host)
  "True when switching journals or rebuilding userspace cannot strand work.
A queued model-gated submission is work even though no task exists."
  (and (not (host-running-p host))
       (not (agent-pending-work-p (host-agent host)))))

(defun require-session-quiescent (host what)
  "Refuse unless HOST's session has no task and nothing queued: WHAT moves the
leaf or switches journals, and input queued against the old one would be
answered in the wrong place."
  (cond ((host-running-p host)
         (refuse :conflict "~a needs an idle agent (~a)" what (host-interrupt-hint host)))
        ((agent-pending-work-p (host-agent host))
         (refuse :conflict "~a cannot switch sessions while input is queued; resolve the model and run it first" what))
        (t t)))

(defun require-idle (host what)
  "Refuse while HOST runs a task.  Queued input is welcome (see §6: /reload
is how a model-gated submit gets released)."
  (when (host-running-p host)
    (refuse :conflict "~a needs an idle agent (~a)" what (host-interrupt-hint host)))
  t)

(defun release-queued-input (host)
  "Steering queued while nothing ran (a model-gated submit, a command's
hand-off) gets its run now."
  (when (and (steering-pending-p (host-agent host))
             (not (host-running-p host)))
    (host-start-run host)))

;;; Journal switches.

(defun switch-journal (host journal &key note)
  "Point HOST's agent at JOURNAL.  A guard, not a refusal: every caller has
already required quiescence, so getting here busy is a bug."
  (unless (session-quiescent-p host)
    (error "Cannot switch journals while the current session owns pending work"))
  (switch-session (host-agent host) journal)
  (host-refresh host)
  (host-session-switched host)
  (host-data host :session (namestring (journal-path journal)))
  (when note (host-notice host note)))

;;; /goal

(defun goal-command (host args)
  "/goal [text|pause|resume] — with no args, show the goal; with prose,
create or refine it.  pause/resume are the human's goal controls: only the
user can pause an active goal (the agent's update_goal cannot), and a paused
goal is resumed from here too."
  (let* ((agent (host-agent host))
         (goal (current-goal agent))
         (verb (string-downcase (string-trim '(#\Space #\Tab) args))))
    (cond
      ((zerop (length args))
       (host-notice host
                    (if goal
                        (format nil "goal ~a [~(~a~)]: ~a~%tokens: ~:d~@[ / ~:d~]"
                                (pget goal :goal-id) (pget goal :status)
                                (pget goal :objective)
                                (goal-tokens-used agent goal)
                                (pget goal :token-budget))
                        "no goal — /goal <objective> to set one")))
      ((equal verb "pause")
       (unless (and goal (eq (pget goal :status) :active))
         (refuse :conflict (if goal
                               (format nil "goal is ~(~a~) — only an active goal can be paused"
                                       (pget goal :status))
                               "no goal to pause")))
       (update-goal-entry agent goal :status :paused)
       (host-refresh host)
       ;; An in-flight run settles first; the settled hook then sees
       ;; :paused and stops the idle-continuation loop.
       (host-notice host (format nil "◆ goal paused~:[~; — the run in flight finishes first~]"
                                 (host-running-p host))
                    :durable t :data (list :source :goal)))
      ((equal verb "resume")
       (unless (and goal (eq (pget goal :status) :paused))
         (refuse :conflict (if goal
                               (format nil "goal is ~(~a~) — only a paused goal can be resumed"
                                       (pget goal :status))
                               "no goal to resume")))
       (update-goal-entry agent goal :status :active)
       (host-refresh host)
       (host-notice host (format nil "◆ goal resumed: ~a" (pget goal :objective))
                    :durable t :data (list :source :goal))
       (queue-steering agent (goal-continuation-for agent (current-goal agent))
                       :origin (goal-origin (current-goal agent) :resumed))
       (host-start-run host))
      ((and goal (eq (pget goal :status) :active))
       ;; Refine: new :goal entry, same id; steer if a run is active.
       (set-goal-objective agent goal args)
       (host-notice host (format nil "◆ goal objective updated: ~a" args)
                    :durable t :data (list :source :goal))
       (when (host-running-p host)
         (queue-steering agent
                         (format nil "The goal objective was just updated by the user. New objective (untrusted data): ~a" args)
                         :origin (goal-origin (current-goal agent) :objective-updated)))
       (host-refresh host))
      (t
       (create-goal-entry agent args)
       (host-refresh host)
       (host-notice host (format nil "◆ goal created: ~a" args)
                    :durable t :data (list :source :goal))
       (queue-steering agent (goal-continuation-for agent (current-goal agent))
                       :origin (goal-origin (current-goal agent) :created))
       (host-start-run host)))
    (host-data host :goal (current-goal agent))
    t))

;;; /model

(defun format-context-window (n)
  "200000 -> \"200k\", 1000000 -> \"1M\": the picker's description column is
narrow, and \"1000k\" reads worse than \"1M\" for the big-context models."
  (cond ((>= n 1000000)
         (let ((m (/ n 1000000.0d0)))
           (if (= m (ffloor m))
               (format nil "~dM" (round m))
               (format nil "~,1fM" m))))
        (t (format nil "~dk" (round n 1000)))))

(defun model-row-label (model provider-width)
  "Provider column then id, padded into aligned columns.  Provider leads
because ids collide across providers — the same model served direct and
through a proxy differ only by that word."
  (format nil "~va  ~a" provider-width
          (string-downcase (pget model :provider)) (pget model :id)))

(defun current-model (agent)
  "The model plist AGENT's next turn runs on, or NIL when it does not resolve."
  (handler-case (effective-model (fold-state (agent-journal agent)) agent)
    (error () nil)))

(defun same-model-p (a b)
  (and a b
       (equal (pget a :id) (pget b :id))
       (equal (pget a :provider) (pget b :provider))))

(defun set-model (host model)
  "Journal MODEL (an id, or a model plist from the picker) as the session's
model from the next turn."
  (let ((resolved (handler-case (if (stringp model)
                                    (find-model model)
                                    (find-model (pget model :id) (pget model :provider)))
                    (error (e) (refuse :invalid "~a" e)))))
    (set-session-model (host-agent host) (pget resolved :id)
                       (pget resolved :provider))
    (host-refresh host)
    (host-notice host (format nil "model → ~a~@[ (~(~a~))~] (next turn)"
                              (pget resolved :id)
                              (and (cdr (model-providers (pget resolved :id)))
                                   (pget resolved :provider))))
    (host-data host :model (list :id (pget resolved :id)
                                 :provider (registry-name (pget resolved :provider))))
    ;; A submit blocked by the model gate left its steering queued in
    ;; memory; a valid model releases it.
    (release-queued-input host)
    t))

(defun model-select (host)
  "Choose the model from the registry, current one preselected.  Padding the
provider lines up the id column too — provider, id and context each align.
The value is the whole model plist: the same id under different providers are
distinct entries, and the journaled choice must say which."
  (let* ((current (current-model (host-agent host)))
         (models (all-models))
         (provider-width
           (reduce #'max models
                   :key (lambda (m) (length (string (pget m :provider))))
                   :initial-value 0)))
    (host-choose
     host "model:"
     (loop for m in models
           collect (list (model-row-label m provider-width)
                         m
                         (format nil "~a ctx~:[~; · current~]"
                                 (format-context-window (pget m :context-window 0))
                                 (same-model-p m current))))
     (lambda (model) (set-model host model))
     :index (or (and current (position-if (lambda (m) (same-model-p m current)) models))
                0))
    t))

;;; /thinking

(defun thinking-command (host args)
  (let ((level (intern (string-upcase args) :keyword)))
    (unless (member level +effort-levels+)
      (refuse :invalid "levels: low medium high xhigh max"))
    (set-session-thinking (host-agent host) level)
    (host-refresh host)
    (host-notice host (format nil "thinking → ~(~a~)" level))
    (host-data host :thinking level)
    t))

;;; /lang

(defun set-language (host code)
  "Journal the language choice so it outlives a restart and a compaction,
the way a model pick does.  A code naming no registered pack is kept as a
response-language hint — the prompt stays in the default language and the
model is asked to answer in what the user named."
  (let ((pack (find-prompt-language code)))
    (set-prompt-language code (host-agent host))
    (host-refresh host)
    (host-notice host (if pack
                         (format nil "language → ~a (~a) — next turn"
                                 (pget pack :native) (pget pack :code))
                         (format nil "language → ~a — no prompt pack for it, so replies only (next turn)"
                                 code)))
    (host-data host :language code)
    t))

(defun language-select (host)
  "Choose among the registered prompt language packs, current one preselected.
The label is the endonym — someone looking for their own language scans for
the word they write it with, not its English name."
  (let* ((state (fold-state (agent-journal (host-agent host))))
         (current (resolve-language (language-request state)))
         (packs (all-prompt-languages)))
    (host-choose
     host "language:"
     (loop for p in packs
           collect (list (pget p :native)
                         (pget p :code)
                         (format nil "~a~:[~; · current~]" (pget p :name)
                                 (equal (pget p :code) (pget current :code)))))
     (lambda (code) (set-language host code))
     :index (or (position (pget current :code) packs
                          :key (lambda (p) (pget p :code)) :test #'equal)
                0))
    t))

;;; /lore, /global-lore

(defun lore-command (host args scope &key (cwd (uiop:getcwd)))
  "Show lore relevant to SCOPE, or add durable guidance at SCOPE (:project or
:global). /lore lists project and session lore (what applies here); /global-lore
lists only global (every project) lore — mirroring /memory vs /global-memory."
  (let* ((agent (host-agent host))
         (label (if (eq scope :global) "global lore" "lore"))
         (state (fold-state (agent-journal agent))))
    (if (zerop (length args))
        (let ((entries (remove-if-not
                        (lambda (e)
                          (if (eq scope :global)
                              (eq (getf e :scope) :global)
                              (member (getf e :scope) '(:project :session))))
                        (all-lore-entries :state state :cwd cwd))))
          (host-data host :lore entries)
          (if entries
              (host-notice host (format nil "~a (ask me to edit/remove by id):~%~{ · [~a] (~(~a~)) ~a~%~}"
                                        label
                                        (loop for e in entries
                                              collect (getf e :id)
                                              collect (getf e :scope)
                                              collect (getf e :text))))
              (host-notice host (format nil "no ~a — /~a <text> adds durable guidance"
                                        label (if (eq scope :global) "global-lore" "lore")))))
        (let ((id (add-lore args :scope scope :cwd cwd)))
          (host-notice host (format nil "✓ ~a added [~a] (injected every turn)" label id))
          (host-data host :id id)
          (when (host-running-p host)
            (queue-steering agent
                            (format nil "The user added ~a (durable guidance, applies from now on): ~a"
                                    label args)
                            :origin (list :kind :command-note
                                          :command (if (eq scope :global) "global-lore" "lore")
                                          :text args)))))
    t))

;;; /compact

(defun compact-command (host args)
  (require-idle host "/compact")
  (host-start-compact host args)
  t)

;;; /tree and rewind

(defun message-text-block (message)
  (pget (find :text (pget message :content)
              :key (lambda (b) (pget b :type)))
        :text))

(defparameter *tool-call-max-width* 80
  "Max rendered width for a tool-call line before truncation.")

(defparameter *tool-key-args*
  '(("bash" . ("command"))
    ("read" . ("path"))
    ("write" . ("path"))
    ("edit" . ("path" "old_string"))
    ("create_goal" . ("objective"))
    ("update_goal" . ("status" "objective"))
    ("todo" . ("items"))
    ("eval" . ("code")))
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

(defun format-tool-call-plain (name arguments &optional arguments-json)
  "Format a tool call as one line, no ANSI: ⏺ name(key=\"val\", ...),
truncated at *tool-call-max-width*.  Total by construction: this renders
inside the TUI tick loop and on session resume, so malformed ARGUMENTS
(non-list, dotted, odd-length) degrade to the bare name — never signal.

A :json tool is shown its raw ARGUMENTS-JSON instead: the plist spelling of
its keys is not what the tool received, and a display that renames the file
the model just wrote is worse than no display."
  (when (and arguments-json
             (eq arguments-json
                 (tool-call-display-arguments name arguments arguments-json)))
    (return-from format-tool-call-plain
      (truncate-string (format nil "⏺ ~a(~a)" name
                               (substitute #\Space #\Newline arguments-json))
                       *tool-call-max-width* "…")))
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

(defun entry-label (entry)
  "One line naming a journal ENTRY, for the /tree picker."
  (let ((type (pget entry :type)))
    (case type
      (:message
       (let* ((m (pget entry :message))
              (role (pget m :role)))
         (case role
           (:user (format nil "❯ ~a" (truncate-string (or (message-text-block m) "")
                                                      48 "…")))
           (:assistant
            (let ((call (find :tool-call (pget m :content)
                              :key (lambda (b) (pget b :type)))))
              (if call
                  (format-tool-call-plain (pget call :name) (pget call :arguments))
                  (format nil "· ~a" (truncate-string
                                      (or (message-text-block m) "(thinking)")
                                      48 "…")))))
           (:tool-result (format nil "⎿ ~a result" (pget m :tool-name)))
           (t (format nil "~(~a~)" role)))))
      (:goal (format nil "◆ goal ~(~a~)" (pget entry :status)))
      (t (format nil "~(~a~)" type)))))

(defun user-message-entry-p (entry)
  (and (eq (pget entry :type) :message)
       (eq (pget (pget entry :message) :role) :user)))

(defun move-leaf (host id)
  "Move HOST's leaf to entry ID.  Selecting a user message moves the leaf to
its parent and hands the text back for editing (edit-and-resubmit = new
branch)."
  (require-session-quiescent host "/tree")
  (let* ((journal (agent-journal (host-agent host)))
         (entry (find-entry journal id)))
    (unless entry
      (refuse :not-found "no entry ~a in this session" id))
    (cond
      ((user-message-entry-p entry)
       (setf (journal-leaf-id journal) (pget entry :parent-id))
       (let ((text (message-text-block (pget entry :message))))
         (when text
           (host-set-draft host text)
           (host-data host :draft text)))
       (host-notice host "⎌ leaf moved — edit and resubmit to branch"))
      (t
       (setf (journal-leaf-id journal) id)
       (host-notice host (format nil "⎌ leaf moved to ~a" id))))
    (host-data host :leaf (journal-leaf-id journal))
    (host-refresh host)
    t))

(defun tree-command (host args)
  "/tree — choose an entry on the current path and move the leaf there;
/tree <id> moves it directly.  Moving the leaf re-parents what the next turn
answers, so queued input must not survive the move: it was typed against the
old leaf."
  (require-session-quiescent host "/tree")
  (let* ((journal (agent-journal (host-agent host)))
         (path (and (journal-leaf-id journal) (entry-path journal))))
    (cond
      ((plusp (length args)) (move-leaf host args))
      ((null path) (host-notice host "empty session"))
      (t
       (host-choose
        host "move leaf to:"
        (loop for entry in path
              for i from 1
              collect (cons (format nil "~3d. ~a" i (entry-label entry))
                            (pget entry :id)))
        (lambda (id) (move-leaf host id))))))
  t)

(defun rewind-command (host)
  "Rewind: move the leaf above the last user message and hand its text back
for editing — the TUI's double escape, and `/rewind` everywhere."
  (require-session-quiescent host "rewind")
  (let* ((journal (agent-journal (host-agent host)))
         (path (and (journal-leaf-id journal) (entry-path journal)))
         (entry (find-if #'user-message-entry-p path :from-end t)))
    (cond
      ((null entry) (host-notice host "nothing to rewind"))
      (t
       (setf (journal-leaf-id journal) (pget entry :parent-id))
       (let ((text (message-text-block (pget entry :message))))
         (when text
           (host-set-draft host text)
           (host-data host :draft text)))
       (host-data host :leaf (journal-leaf-id journal))
       (host-refresh host)
       (host-notice host "⎌ rewound — edit and resubmit to branch"))))
  t)

;;; /resume, /fork, /new

(defparameter *resume-summary-max-chars* 96
  "Maximum characters of leaf user prompt shown in /resume session lists.")

(defun %collapse-whitespace (text)
  (string-join " "
               (remove "" (uiop:split-string (or text "")
                                               :separator '(#\Space #\Tab #\Newline #\Return))
                       :test #'string=)))

(defun %truncate-with-ellipsis (text max-chars)
  (cond ((<= (length text) max-chars) text)
        ((<= max-chars 0) "")
        (t (concatenate 'string (subseq text 0 (1- max-chars)) "…"))))

(defun resume-summary-text (text &key (max-chars *resume-summary-max-chars*))
  "Single-line, bounded summary for a leaf user prompt."
  (let ((clean (%collapse-whitespace text)))
    (unless (zerop (length clean))
      (%truncate-with-ellipsis clean max-chars))))

(defun first-user-prompt (journal)
  "First user text on JOURNAL's current leaf path, or NIL.  The opening
prompt is what makes a session recognisable months later; the last one is
usually \"continue\" or an injected goal-continuation nudge, which looks the
same in every row."
  (loop for entry in (entry-path journal)
        for message = (and (eq (pget entry :type) :message)
                           (pget entry :message))
        when (and message (eq (pget message :role) :user))
          return (message-text-block message)))

(defun resume-session-summary (session)
  "Description text for one /resume SESSION row."
  (let ((prompt (ignore-errors
                  (first-user-prompt (open-journal (pget session :path))))))
    (and prompt (resume-summary-text prompt))))

(defun resume-select-items (sessions &key timezone-name)
  "Items for /resume: creation-time label + opening prompt summary.  SESSIONS
arrive from LIST-SESSIONS already ordered by last write, so the label time and
the row order deliberately disagree: you scan from the top for the session you
touched most recently, and read the date to place it."
  (let ((timezone-name (or timezone-name (local-timezone-name))))
    (loop for s in sessions
          for i from 1
          collect (list (format nil "~2d. ~a" i
                                (format-local-timestamp (pget s :timestamp)
                                                        :timezone-name timezone-name))
                        (pget s :path)
                        (resume-session-summary s)))))

(defun resume-session (host path)
  "Switch HOST to the session at PATH; an active goal there picks itself back
up."
  (require-session-quiescent host "/resume")
  (let ((journal (handler-case (open-journal path)
                   (error (e) (refuse :not-found "cannot open ~a: ~a" path e)))))
    (switch-journal host journal :note (format nil "resumed ~a" path))
    (host-show-history host)
    (let ((goal (current-goal (host-agent host))))
      (when (and goal (eq (pget goal :status) :active))
        (queue-steering (host-agent host)
                        (goal-continuation-for (host-agent host) goal)
                        :origin (goal-origin goal :continue))
        (host-start-run host)))
    t))

(defun resume-command (host args)
  "/resume — choose a session of this directory; /resume <n|path> switches
directly (N counts the list /resume shows, 1 = the one last worked in)."
  (require-session-quiescent host "/resume")
  (let ((sessions (list-sessions)))
    (cond
      ((plusp (length args))
       (let* ((n (ignore-errors (parse-integer args)))
              (path (if n
                        (pget (nth (1- n) sessions) :path)
                        args)))
         (unless (and path (or (null n) (<= 1 n (length sessions))))
           (refuse :not-found "no session ~a — /resume lists them" args))
         (resume-session host path)))
      ((null sessions) (host-notice host "no sessions for this directory"))
      (t
       (host-choose host "resume session:"
                    (resume-select-items sessions)
                    (lambda (path)
                      ;; Re-checked at pick time: a settling task or a
                      ;; model-gated submit can land between opening the
                      ;; picker and choosing.
                      (resume-session host path))))))
  t)

(defun fork-command (host)
  (require-session-quiescent host "/fork")
  (let ((path (fork-session (agent-journal (host-agent host)))))
    (switch-journal host (open-journal path)
                    :note (format nil "forked to ~a" path)))
  t)

(defun new-command (host)
  (require-session-quiescent host "/new")
  (switch-journal host (make-session-journal) :note "new session")
  t)

;;; /export

(defun export-image (block path index)
  "Write an image block beside the export as a sidecar file and return its
file name.  A markdown transcript that dropped the screenshots would not be
the transcript; base64 inline would not be readable."
  (let* ((name (format nil "~a-img~d.~a"
                       (pathname-name path) index
                       (evo.media:media-type-extension (pget block :media-type))))
         (file (merge-pathnames name (uiop:pathname-directory-pathname
                                      (uiop:ensure-absolute-pathname
                                       path (uiop:getcwd))))))
    (write-file-octets file (base64->octets (pget block :data)))
    name))

(defun export-command (host args)
  (let* ((state (fold-state (agent-journal (host-agent host))))
         (path (if (plusp (length args))
                   args
                   (format nil "evo-export-~a.md" (gen-id 4))))
         (image-index 0))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (dolist (m (state-messages state))
        (case (message-role m)
          (:user (format out "## user~2%~a~2%"
                         (or (pget (find :text (message-content m)
                                         :key (lambda (b) (pget b :type))) :text) ""))
                 (dolist (b (message-content m))
                   (when (evo.media:image-block-p b)
                     (let ((file (ignore-errors
                                  (export-image b path (incf image-index)))))
                       (format out "~@[![~a](~a)~2%~]" (and file (pget b :name)) file)))))
          (:assistant
           (format out "## assistant~2%")
           (dolist (b (message-content m))
             (case (pget b :type)
               (:text (format out "~a~2%" (pget b :text)))
               (:tool-call (format out "`⏺ ~a` `~s`~2%" (pget b :name) (pget b :arguments))))))
          (:tool-result
           (format out "```~%~a~%```~2%"
                   (result-display-text (message-content m)))
           ;; A tool may hand back a picture (read on a screenshot); the
           ;; transcript keeps it beside the text, same as a user's.
           (dolist (b (message-content m))
             (when (evo.media:image-block-p b)
               (let ((file (ignore-errors
                            (export-image b path (incf image-index)))))
                 (format out "~@[![~a](~a)~2%~]" (and file (pget b :name)) file))))))))
    (host-notice host (format nil "exported to ~a" path))
    (host-data host :path (namestring (uiop:ensure-absolute-pathname path (uiop:getcwd))))
    t))

;;; /reload

(defun reload-command (host)
  "Reload rebuilds models, providers, APIs, tools, commands and hooks.  A run
holding the old generation must not observe the new one halfway through, so
reload waits for an idle session."
  (require-idle host "/reload")
  (boot-userspace :journal (agent-journal (host-agent host)))
  (host-refresh host)                   ; model registry may have changed
  (host-notice host "userspace reloaded (init + extensions + post-init)")
  ;; Steering blocked on the model gate re-runs it; still-broken config
  ;; re-reports the error instead of silently sitting.
  (when (steering-pending-p (host-agent host))
    (host-start-run host))
  t)

;;; Builtins.

(defparameter *builtin-commands*
  '(("goal" . "show, create, refine, pause, or resume the goal")
    ("model" . "pick the model from a list, or set it directly")
    ("thinking" . "low·medium·high·xhigh·max")
    ("lang" . "language of the system prompt and replies")
    ("compact" . "compact the context now")
    ("lore" . "show lore, or add project-scope guidance")
    ("global-lore" . "show lore, or add user-scope guidance")
    ("tree" . "navigate entries, move the leaf (rewind/branch)")
    ("rewind" . "move the leaf above your last message (esc esc)")
    ("resume" . "switch to another session")
    ("fork" . "fork this session at the current leaf")
    ("new" . "start a fresh session")
    ("export" . "export the transcript as markdown")
    ("reload" . "re-evaluate init files, extensions and post-init files"))
  "The frontend-independent builtins as (name . description), for completion
and listings.  A frontend adds its own presentational ones.")

(defun builtin-command (host name args)
  "Run builtin /NAME with ARGS on HOST.  Returns T when NAME is a builtin."
  (macrolet ((cmd (&rest names) `(member name ',names :test #'string-equal)))
    (cond
      ((cmd "goal") (goal-command host args))
      ((cmd "model")
       (if (zerop (length args)) (model-select host) (set-model host args)))
      ((cmd "thinking") (thinking-command host args))
      ((cmd "lang" "language")
       (if (zerop (length args)) (language-select host) (set-language host args)))
      ((cmd "compact") (compact-command host args))
      ((cmd "lore") (lore-command host args :project))
      ((cmd "global-lore") (lore-command host args :global))
      ((cmd "tree") (tree-command host args))
      ((cmd "rewind") (rewind-command host))
      ((cmd "resume" "sessions") (resume-command host args))
      ((cmd "fork") (fork-command host))
      ((cmd "new") (new-command host))
      ((cmd "export") (export-command host args))
      ((cmd "reload") (reload-command host))
      (t nil))))

;;; Skills + templates as commands.

(defun command-as-skill (host name args)
  (let* ((skill-name (if (string-prefix-p "skill:" name)
                         (subseq name 6)
                         name))
         (skill (find-skill skill-name)))
    (when skill
      (host-submit
       host
       (format nil "Use the skill '~a'. Read ~a first and follow it.~@[ Task: ~a~]"
               (pget skill :name) (pget skill :path)
               (and (plusp (length args)) args)))
      t)))

(defun command-as-template (host name args)
  (let ((path (find-template name)))
    (when path
      (host-submit host (expand-template (read-file-string path) args))
      t)))

;;; Extension commands.

(defun run-extension-command (host name args)
  "Run the registered command /NAME.  Its string result is shown; steering it
queued gets a run."
  (let ((fn (pget (find-command name) :fn)))
    (handler-case
        (let ((result (funcall fn (list* :agent (host-agent host) :args args
                                         :host host (host-command-context host)))))
          (when (stringp result) (host-notice host result))
          (host-refresh host)
          (release-queued-input host))
      (command-refused (c) (error c))
      (error (e) (host-command-failed host name e)))
    t))

;;; Dispatch.

(defun parse-command (text)
  "Split \"/name args...\" into (values NAME ARGS), ARGS trimmed."
  (let* ((text (string-left-trim " " text))
         (start (if (and (plusp (length text)) (char= (char text 0) #\/)) 1 0))
         (space (position #\Space text :start start)))
    (values (subseq text start space)
            (string-trim " " (if space (subseq text (1+ space)) "")))))

(defun dispatch-command (host text &key frontend-builtin)
  "Resolve and run the slash command TEXT (\"/name args\") on HOST, in the
order every frontend shares: extension commands, builtins — FRONTEND-BUILTIN,
a function (name args) -> handled-p for the frontend's own presentational ones,
then the core's — skills, prompt templates.  Returns T when something handled
it, NIL for an unknown command.  A refusal propagates as COMMAND-REFUSED."
  (multiple-value-bind (name args) (parse-command text)
    (cond
      ((zerop (length name)) nil)
      ((find-command name) (run-extension-command host name args))
      ((and frontend-builtin (funcall frontend-builtin name args)))
      ((builtin-command host name args))
      ((command-as-skill host name args))
      ((command-as-template host name args))
      (t nil))))

(defun template-names ()
  (loop for dir in (template-directories)
        append (mapcar #'pathname-name
                       (ignore-errors (directory (merge-pathnames "*.md" dir))))))

(defun command-catalog (&optional frontend-builtins)
  "Every command name a user can type, as (name . description), in dispatch
order with the first registration winning, sorted by name: extension
commands, FRONTEND-BUILTINS (an alist like *BUILTIN-COMMANDS*) and the core's
builtins, skills, prompt templates."
  (sort (remove-duplicates
         (append (loop for (name . cmd) in (registered-commands)
                       collect (cons name (or (pget cmd :description)
                                              "extension command")))
                 (copy-alist frontend-builtins)
                 (copy-alist *builtin-commands*)
                 (mapcar (lambda (s) (cons (format nil "skill:~a" (pget s :name))
                                           (or (pget s :description) "skill")))
                         (available-skills))
                 (mapcar (lambda (name) (cons name "prompt template"))
                         (template-names)))
         :key #'car :test #'string= :from-end t)
        #'string< :key #'car))

;;; Completion: what a cursor sits on, and what could go there.
;;;
;;; A frontend hands in the whole input and the caret as a character offset
;;; into it; what could be completed there, and which range a candidate's
;;; name replaces, is decided here — once — so the TUI's popup and a GUI
;;; through serve's `complete` op answer the same thing instead of each
;;; re-implementing evo's own token rules.

(defparameter *symbol-completion-command* "eval"
  "The command whose argument content completes against the running image:
its text is Lisp being typed there, so the candidates are that image's own
functions and variables (EVO.EVAL:COMPLETIONS-FOR) rather than a name list.")

(defun completion-space-p (char)
  "Whitespace, as a command word's boundary."
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun completion-word-char-p (char)
  "A character a /command word may hold after its slash.  A second \"/\" is
none of them, which is what keeps a path from being completed as a command."
  (or (alphanumericp char)
      (member char '(#\: #\_ #\-) :test #'char=)))

(defun completion-run-start (text cursor test)
  "Where the run of characters satisfying TEST around the caret begins: the
first index at or before CURSOR whose character does not satisfy it."
  (let ((i cursor))
    (loop while (and (plusp i) (funcall test (char text (1- i)))) do (decf i))
    i))

(defun completion-run-end (text cursor test)
  "Where the run of characters satisfying TEST around the caret ends: the
first index at or after CURSOR whose character does not satisfy it."
  (let ((i cursor)
        (n (length text)))
    (loop while (and (< i n) (funcall test (char text i))) do (incf i))
    i))

(defun command-completion (text cursor)
  "The /command word at CURSOR in TEXT, as (values START END); NIL when the
caret is not in one.  A command word begins with \"/\" at the start of the
text or after whitespace — anywhere in prose, not only on a line of its own
— and runs on in letters, digits, \":\", \"_\" and \"-\".  START is just
past the slash: a candidate's NAME is typed without it."
  (let* ((start (completion-run-start text cursor #'completion-word-char-p))
         (slash (1- start)))
    (when (and (>= slash 0)
               (char= (char text slash) #\/)
               (or (zerop slash) (completion-space-p (char text (1- slash)))))
      (values start (completion-run-end text cursor #'completion-word-char-p)))))

(defun symbol-content-start (text)
  "Where the symbol-completing command's content begins in TEXT — just past
the space that ends its command word — or NIL when TEXT does not begin with
that invocation.  The command word itself is not content to complete over."
  (let* ((line (subseq text 0 (position #\Newline text)))
         (space (position #\Space line)))
    (when (and space
               (plusp (length line))
               (char= (char line 0) #\/)
               (string-equal *symbol-completion-command* (subseq line 1 space)))
      (1+ space))))

(defun symbol-completion (text cursor content)
  "The symbol token at CURSOR in TEXT when CONTENT — where the
symbol-completing command's content begins — is non-NIL and the caret is at
or past it, as (values START END); NIL otherwise.  The token's own
boundaries are EVO.EVAL's: what a symbol may hold and what ends one is the
reading of Lisp, which /eval already owns."
  (when (and content (>= cursor content))
    (let ((start (evo.eval:token-start text cursor)))
      (when (>= start content)
        (values start (evo.eval:token-end text cursor))))))

(defun completion-target (text cursor)
  "What a caret is on, frontend-independently: (values KIND START END) for
TEXT with the caret at CURSOR, a character offset counted in Lisp characters
(a client whose offsets are UTF-16 converts first).  KIND is :command for a
/command word the caret is inside, :symbol for a token inside the
symbol-completing command's content, and NIL — with START and END NIL — when
the caret sits on nothing completable.  START..END is the range a chosen
candidate's NAME replaces: for a command it is the word after the slash, so
a name is never typed with one.  The token or word holding the caret is the
whole one, so a caret in the middle replaces the unit it is in rather than
leaving half of it behind.

The content of the symbol-completing command is Lisp, so inside it — at or
past where its content begins — only a symbol is completed: a \"/word\" in a
list or a string is a token there, never a slash command.  Everywhere else,
the command word itself included, a command word completes as one."
  (let* ((cursor (max 0 (min cursor (length text))))
         (content (symbol-content-start text)))
    (if (and content (>= cursor content))
        (multiple-value-bind (start end) (symbol-completion text cursor content)
          (if start
              (values :symbol start end)
              (values nil nil nil)))
        (multiple-value-bind (start end) (command-completion text cursor)
          (if start
              (values :command start end)
              (values nil nil nil))))))

(defun completion-items (kind prefix &optional commands)
  "Candidates for PREFIX as (name . description): the commands a client can
type, or the image's own answer for a symbol.  COMMANDS, when given, is the
command list to filter instead of asking for it — a frontend that caches it
for the life of a popup, as the TUI does."
  (ecase kind
    (:command (remove-if-not (lambda (entry) (string-prefix-p prefix (car entry)))
                             (or commands (command-catalog))))
    (:symbol (evo.eval:completions-for prefix))))

;;; State: what a frontend shows about the session, derived from the fold.

(defun session-summary (agent)
  "The session's derived state as a plist — model, thinking, context, goal,
todos, jobs, journal — the facts a status line (or a remote controller) shows.
Total: a model that does not resolve reads as NIL, never signals."
  (let* ((journal (agent-journal agent))
         (state (fold-state journal))
         (model-id (handler-case (effective-model-id state agent)
                     (error () nil)))
         (model (and model-id
                     (handler-case (find-model model-id
                                               (effective-model-provider state model-id))
                       (error () nil))))
         (goal (state-goal state)))
    (list :model model-id
          :provider (and model (registry-name (pget model :provider)))
          :model-ready (and model t)
          :thinking (effective-thinking state (agent-thinking-override agent))
          :language (pget (resolve-language (language-request state)) :code)
          :context-tokens (estimate-context-tokens (state-messages state))
          :context-window (and model (model-context-window model))
          :goal (and goal (append goal
                                  (list :tokens-used-live
                                        (goal-tokens-used agent goal))))
          :todos (or (custom-state state "todo") #())
          :jobs (running-jobs-summary)
          :session (namestring (journal-path journal))
          :session-id (pget (journal-header journal) :id)
          :session-started (and (journal-started-p journal) t)
          :leaf (journal-leaf-id journal)
          :pending-input (and (agent-pending-work-p agent) t))))
