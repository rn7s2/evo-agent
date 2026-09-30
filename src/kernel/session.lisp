;;;; session.lisp — session operations: the journal writes a frontend asks for.
;;;;
;;;; A frontend decides WHEN something happens to the session — a flag, a key,
;;;; a picker — and the core decides WHAT it does.  Everything here writes the
;;;; journal or re-points the agent at one, so no frontend builds an entry by
;;;; hand or has to know the order a session comes up in.

(in-package :evo.kernel)

(defun boot-session (agent &key resumed-p no-userspace)
  "Bring AGENT's session up, then announce :session-start.

Userspace is built first — init files, extension directories, post-init —
with AGENT's journal as the one extension loads are recorded in, and a
RESUMED-P session then replays its own :load entries.  NO-USERSPACE is
quarantine mode: nothing of the user's is loaded, and only the kernel
registries are seeded."
  (let ((journal (agent-journal agent)))
    (if no-userspace
        (progn                          ; kernel registries still need seeding
          (reset-settings)
          (reset-user-registries))
        (let ((*current-journal* journal))
          (boot-userspace :journal (and (not resumed-p) journal))
          (when resumed-p
            (replay-loads (fold-state journal)))))
    (run-hooks :session-start (list :agent agent :resumed resumed-p))
    ;; The session index is written when its identity is known: here on start,
    ;; when the file first lands, on a switch and on the way out.
    (index-session journal)
    agent))

(defun switch-session (agent journal)
  "Re-point AGENT at JOURNAL — another session, a fork, a fresh one — and
bring it up the way a restart would: the agent's ephemeral run state reset,
JOURNAL's :load entries replayed, :session-start announced.

The caller must first prove the session quiescent (no task running, nothing in
the mailbox): input queued against the old journal would otherwise be answered
in the new one."
  (setf (agent-journal agent) journal)
  (reset-agent-session-state agent)
  (replay-loads (fold-state journal))
  (run-hooks :session-start
             (list :agent agent :resumed (journal-started-p journal)))
  (index-session journal)
  agent)

(defun set-session-model (agent model-id &optional provider)
  "Journal a model choice: the session's next turn runs on MODEL-ID, served by
PROVIDER when the id is registered under more than one (NIL leaves the choice
to the registry).  Journaled, so it survives a restart and a compaction."
  (append-entry (agent-journal agent)
                (list* :type :model-change :model model-id
                       (and provider (list :provider provider)))))

(defun set-session-thinking (agent level)
  "Journal a thinking LEVEL (one of +EFFORT-LEVELS+) for the session's next
turns."
  (append-entry (agent-journal agent)
                (list :type :thinking-change :thinking level)))

(defun recovery-note-text (recovery)
  "One factual sentence about how the previous run ended, for the
transcript — what the supervisor saw, nothing else.  No advice: whether to
redo the interrupted work is the agent's call, made with the facts."
  (let* ((status (pget recovery :status))
         (code (pget recovery :code))
         (attempt (pget recovery :attempt))
         (duration (pget recovery :duration))
         (reason (pget recovery :reason))
         (seconds (format nil "~d ~:[seconds~;second~]" duration (eql duration 1))))
    (format nil "The previous run (recovery ~a) ~a."
            (or attempt "?")
            (cond ((and (eq status :signaled) reason)
                   (format nil "was killed by the supervisor: ~a — signal ~a after ~a"
                           reason code seconds))
                  ((eq status :signaled)
                   (format nil "was killed by signal ~a after ~a" code seconds))
                  (t (format nil "exited with code ~a after ~a" code seconds))))))

(defun record-recovery (agent recovery)
  "Journal the supervisor's account of how the run AGENT replaces ended: a
`:recover' entry (state for extensions, folded under `custom-state
\"recovery\"') and a user-role note so the model sees how it got here.
RECOVERY is the plist parsed from the supervisor's EVO_RECOVERY line; the
note carries the facts only."
  (let ((journal (agent-journal agent)))
    (append-entry journal (append (list :type :recover) recovery))
    (append-entry journal
                  (list :type :custom-message
                        :key "recovery"
                        ;; Not the person typing: the supervisor's account of
                        ;; how the last run ended, in its own words.
                        :origin (list :kind :recovery
                                      :status (pget recovery :status)
                                      :code (pget recovery :code)
                                      :attempt (pget recovery :attempt)
                                      :reason (pget recovery :reason))
                        :message (list :role :user
                                       :content (list (list :type :text
                                                            :text (recovery-note-text recovery))))))
    agent))

(defun end-session (agent)
  "Announce :session-end for AGENT — the session is going away.  Every
frontend calls this once on its way out (the TUI on quit, print and event mode
when the run settles, serve on shutdown), BEFORE it stops its task, so
anything an extension holds open on the user's behalf comes down while the
session still owns it."
  (run-hooks :session-end (list :agent agent))
  ;; The last word in the index: the session as it ended (its title, how many
  ;; entries it holds, when it was last worked in).
  (index-session (agent-journal agent))
  agent)

;;; The frontend protocol.
;;;
;;; Some extensions produce input off-thread (a notification's reply field) or
;;; exist only for a person looking at a terminal (a LaTeX renderer's prompt
;;; note, a status-line poller).  They need two answers from whatever frontend
;;; this session runs under, and the core cannot name one: is a human attached,
;;; and will somebody start a run for input queued outside a run?  A frontend
;;; answers by specializing these generics on its own object and binding
;;; *FRONTEND* to it before the session boots, so an extension's load-time
;;; decision already sees it.  No frontend (print mode, event mode, a bare
;;; core in the unit suite) answers NIL to both.

(defvar *frontend* nil
  "The frontend object this session runs under, or NIL for none.  Set by the
CLI before BOOT-SESSION; read through the two generics below.")

(defgeneric frontend-interactive-p (frontend)
  (:documentation "True when FRONTEND puts a human at a terminal in front of
the session (the TUI).  A remote-controlled or scripted frontend answers NIL.")
  (:method ((frontend t)) nil))

(defgeneric frontend-request-run (frontend &key text)
  (:documentation "Ask FRONTEND to start a run for steering ALREADY QUEUED on
the agent (EVO:STEER).  TEXT, when given, is what the frontend may show as the
user's submission.  Returns true when the request was accepted, NIL when this
frontend cannot start runs for off-thread input.  Safe from any thread.")
  (:method ((frontend t) &key text)
    (declare (ignore text))
    nil))

;;; Holds — why a settled agent is not idle.
;;;
;;; A hold is not a task and not a pause: nothing is running, but the agent is
;;; not free to take new work either.  A swarm coordinator whose lanes are
;;; working is exactly this — it has settled, and it must not be shown as idle,
;;; because a person looking at it would think it had stopped.  The holder owns
;;; the answer; everyone rendering the session's status asks for it.

(in-package :evo.kernel)

(defvar *hold-predicates* nil
  "Functions (agent) -> a short reason string while the agent is held, or NIL.")

(defun register-hold-predicate (fn)
  "Register FN as a hold predicate: (lambda (agent) reason-string-or-nil).
Registering the same function twice is a no-op; it returns FN."
  (setf *hold-predicates* (append (remove fn *hold-predicates*) (list fn)))
  fn)

(defun unregister-hold-predicate (fn)
  "Withdraw FN from the hold predicates."
  (setf *hold-predicates* (remove fn *hold-predicates*))
  fn)

(defun agent-hold-reason (agent)
  "The first reason AGENT is held, or NIL when it is free.  A predicate that
signals is skipped (and named), never allowed to take a status read down."
  (loop for fn in *hold-predicates*
        for reason = (handler-case (funcall fn agent)
                       (error (e)
                         (warn "Hold predicate failed: ~a" e)
                         nil))
        when reason return reason))

(defun note-hold-changed (agent)
  "Announce that AGENT's hold may have started or ended, so every reader of its
status re-reads it.  The holder calls this on each transition — it costs
nothing when nobody is watching."
  (run-hooks :hold-changed (list :agent agent))
  t)

(in-package :evo)

(defun frontend-interactive-p ()
  "True when a human sits at an interactive frontend (the TUI) — the question
to ask before offering something only a person at a terminal can use: a reply
field, a rendered formula, a poller that only feeds the status line."
  (and (evo.kernel:frontend-interactive-p evo.kernel:*frontend*) t))

(defun request-run (&key text)
  "Ask the session's frontend to start a run for steering already queued with
EVO:STEER from outside a run (a background thread, a notification reply).
TEXT is echoed as the user's submission where the frontend shows one.  Returns
true when a frontend accepted the request, NIL when none can (print mode)."
  (and (evo.kernel:frontend-request-run evo.kernel:*frontend* :text text) t))
