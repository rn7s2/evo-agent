;;;; supervision.lisp — the little the supervised child says back to its
;;;; supervisor.
;;;;
;;;; The supervisor (src/cli/supervisor.lisp) restarts its child, so it has to
;;;; know which session the child is on: a bare `--resume` means "the newest
;;;; session in this directory", which is a different session as soon as two
;;;; evos (or two swarm tabs) share a folder.  The child therefore keeps a
;;;; small state directory up to date — which journal it is on now (rewritten
;;;; on start, /new, /fork and /resume), and the port it actually bound, since
;;;; `--port 0` is only chosen once.  Both files belong to the supervisor: it
;;;; names the directory in EVO_SUPERVISOR_STATE_DIR, and nothing here does
;;;; anything without one.
;;;;
;;;; The same directory carries the other two facts only the parent knows and
;;;; the child has to publish: its pid and how many times it has restarted.

(in-package :evo.kernel)

(defparameter +supervisor-state-dir-env+ "EVO_SUPERVISOR_STATE_DIR")
(defparameter +supervisor-pid-env+ "EVO_SUPERVISOR_PID")
(defparameter +supervisor-restarts-env+ "EVO_SUPERVISOR_RESTARTS")

(defun supervisor-state-directory ()
  "The directory this process's supervisor keeps its state in, or NIL when
this process has no supervisor (plain print mode, the unit suite)."
  (let ((dir (getenv +supervisor-state-dir-env+)))
    (and (plusp (length dir)) (uiop:ensure-directory-pathname dir))))

(defun supervisor-state-file (name)
  "The path NAME names inside the supervisor's state directory, or NIL with
no supervisor."
  (let ((dir (supervisor-state-directory)))
    (and dir (merge-pathnames name dir))))

(defun write-supervisor-state (name text)
  "Record TEXT (short) under NAME for the supervisor.  Quiet, and NIL without
one: a session that is not supervised has nobody to tell."
  (let ((file (supervisor-state-file name)))
    (when file
      (ignore-errors (write-file-string file text))
      file)))

(defun note-current-session (&optional path)
  "Tell the supervisor which journal this process is on, so a restart can
resume exactly that session instead of the newest one in the directory.  PATH
defaults to the current agent's journal.  Called by the session layer on boot,
on /new, on /fork and on /resume."
  (let* ((path (or path (and evo:*agent*
                             (ignore-errors (agent-journal evo:*agent*)))))
         (path (and path (namestring (pathname path)))))
    (when path (write-supervisor-state "current-session" path))))

(defun note-bound-port (port)
  "Tell the supervisor the port this process bound, so a restart binds the
same one (`--port 0` is chosen once, not once per life)."
  (when port
    (write-supervisor-state "bound-port" (princ-to-string port))))

(defun supervisor-current-session ()
  "The journal path the supervisor's child last reported, or NIL when it
reported none — restart fresh.  Read by the supervisor, from its own state
directory.

The path is taken as reported, on disk or not: a session nothing has been
journalled to yet has no file, and it is exactly the session an idle server
must come back to (CONTRACT §1, F4).  Asking for the file here started a
brand-new journal instead, so a client's session id changed under it on a
restart."
  (let ((file (supervisor-state-file "current-session")))
    (when file
      (let ((text (ignore-errors (read-file-string file))))
        (when (and text (plusp (length (string-trim '(#\Space #\Newline #\Return #\Tab) text))))
          (string-trim '(#\Space #\Newline #\Return #\Tab) text))))))

(defun supervisor-bound-port ()
  "The port the supervisor's child reported binding, or NIL."
  (let ((file (supervisor-state-file "bound-port")))
    (when file
      (let ((text (and (probe-file file) (ignore-errors (read-file-string file)))))
        (when text
          (let ((n (parse-integer text :junk-allowed t)))
            (and n (<= 0 n 65535) n)))))))

(defun supervisor-pid ()
  "The pid of this process's supervisor, or NIL when unsupervised.  The
supervisor sets it for its child; the child reports it in its ready file so a
client can tell which parent will bring it back."
  (let ((text (getenv +supervisor-pid-env+)))
    (and (plusp (length text)) (ignore-errors (parse-integer text)))))

(defun supervisor-restarts ()
  "How many times this process's supervisor has restarted it before this
life: 0 on a first launch."
  (let ((text (getenv +supervisor-restarts-env+)))
    (if (plusp (length text))
        (or (ignore-errors (parse-integer text)) 0)
        0)))

(defun delete-supervisor-state ()
  "Forget what was reported to the supervisor — the supervisor is going away,
and a stale `current-session` would resume the wrong session for whatever
starts next in this directory.  Best effort."
  (dolist (name '("current-session" "bound-port"))
    (let ((file (supervisor-state-file name)))
      (when file (ignore-errors (delete-file file))))))

(in-package :evo)

(defun note-current-session (&optional path)
  "Tell this session's supervisor which journal it is on (see
EVO.KERNEL:NOTE-CURRENT-SESSION).  The session layer calls it; an extension
rarely needs to."
  (evo.kernel:note-current-session path))
