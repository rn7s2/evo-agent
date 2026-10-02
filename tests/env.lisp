;;;; env.lisp — the environment the suites start from, loaded by every runner.
;;;;
;;;; `make test` is run from wherever a person is, and that is often *inside* a
;;;; running evo session: an evo-swarm lane is where an agent runs it.  A suite
;;;; would then inherit that session's own environment — what its supervisor,
;;;; its serve child, its lane or the shell that started it set — and read it as
;;;; answers to questions about itself.  Three examples, all real:
;;;;
;;;;   EVO_SUPERVISOR_STATE_DIR  `restart-argv` reads the current-session file
;;;;                             the *caller's* session wrote there, so a test
;;;;                             of "a restart resumes nothing it was not told
;;;;                             to" resumes the caller instead;
;;;;   EVO_SESSIONS_DIR          `sessions-directory` puts a test's journal in
;;;;                             the caller's sessions directory, so a test
;;;;                             that lists its own temp folder lists the
;;;;                             caller's sessions;
;;;;   EVO_BABY_EVO=0            the caller switched an extension off for
;;;;                             itself, and a test that has it on fails.
;;;;
;;;; So a run gets a *regular* environment: `CLEAR-SESSION-VARIABLES` unsets
;;;; every EVO_* variable this process inherited, before a suite loads, except
;;;; the two kinds a run is handed on purpose —
;;;;
;;;;   EVO_HOME     the runner points it at its own throwaway home
;;;;                (tests/run-unit.lisp, swarm/tests/run-unit.lisp);
;;;;   EVO_TEST_*   the documented knobs (Makefile: EVO_TEST_BASE_URL,
;;;;                EVO_TEST_API_KEY, EVO_TEST_MODEL, EVO_TEST_VISION_MODEL),
;;;;                which a test that needs one sets or honours itself.
;;;;
;;;; The rule is a *prefix* rule rather than a list of names, so a variable the
;;;; product grows later is cleared without a second edit here.  The cost is
;;;; that a knob a suite must be *told* through the environment has to be named
;;;; EVO_TEST_* to survive — which is the convention this file states.
;;;;
;;;; Variables the code reads today (`getenv "EVO_` in src, extensions, swarm):
;;;; EVO_SESSIONS_DIR, EVO_HOME, EVO_KEY_ENHANCEMENT, EVO_PASTE_BURST,
;;;; EVO_SUPERVISED_CHILD, EVO_NO_SUPERVISOR, EVO_VERBOSE, EVO_RECOVERY,
;;;; EVO_SERVE_TOKEN, EVO_SERVE_WATCH_PID, EVO_SUPERVISOR_STATE_DIR,
;;;; EVO_SUPERVISOR_PID, EVO_SUPERVISOR_RESTARTS, EVO_HEARTBEAT_FILE,
;;;; EVO_BABY_EVO, EVO_BINARY, EVO_PID, EVO_IDE_CONTEXT, EVO_WEBVIEW — and
;;;; whatever a lane, a supervisor or a serve child adds beside them.

(defpackage :evo.test-env
  (:use :cl)
  (:export #:clear-session-variables #:kept-variable-p #:session-variable-p))

(in-package :evo.test-env)

(defparameter +kept+
  '("EVO_HOME")
  "Variables a runner sets for the run itself, and must survive the clearing.")

(defparameter +kept-prefixes+
  '("EVO_TEST_")
  "Prefixes of the documented test knobs (Makefile), which are inputs.")

(defun prefix-p (prefix text)
  "Whether TEXT begins with PREFIX."
  (let ((n (length prefix)))
    (and (<= n (length text)) (string= prefix text :end2 n))))

(defun kept-variable-p (name)
  "Whether NAME is one the run is handed on purpose rather than inherited."
  (member name +kept+ :test #'string=))

(defun session-variable-p (name)
  "Whether NAME describes the session that started this run: an EVO_* variable
that is not one of the two kinds a run is handed.  These are the ones cleared."
  (and (prefix-p "EVO_" name)
       (not (kept-variable-p name))
       (notany (lambda (prefix) (prefix-p prefix name)) +kept-prefixes+)))

(defun variable-names ()
  "The environment's variable names, in the order the system lists them."
  (loop for entry in (evo.port:environ)
        for eq = (position #\= entry)
        collect (subseq entry 0 eq)))

(defun clear-session-variables (&optional (stream *standard-output*))
  "Unset every inherited EVO_* variable but EVO_HOME and EVO_TEST_*.

Naming them on STREAM is what makes a run that inherited a session say so —
names only, because a provider key may be among them.  Returns the names it
cleared, sorted, so a caller can assert on them."
  (let ((cleared (remove-if-not #'session-variable-p (variable-names))))
    (dolist (name cleared)
      (evo.port:unsetenv name))
    (when (and cleared stream)
      (format stream "~&;; env: unset ~{~a~^, ~} — a test run gets a regular environment~%"
              (sort (copy-list cleared) #'string<)))
    (sort cleared #'string<)))
