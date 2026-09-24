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
