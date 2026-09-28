;;;; init.lisp — what a fresh lane is given, and the prompt notes.
;;;;
;;;; A lane boots `evo serve --no-userspace`: kernel and core extensions,
;;;; nothing of the user's.  Before it is given any work, and again whenever
;;;; it restarts, the swarm evaluates the BASELINE in it (POST /eval): the
;;;; coordinator's providers, its model and thinking level as defaults, then
;;;; the forms swarm.lisp gave IN-LANES, then whatever models those left out,
;;;; and last the swarm's own: the report tool, the lane's prompt note and tool
;;;; limit.
;;;;
;;;; Secrets never travel as data.  A provider the coordinator reads its key
;;;; from an environment variable keeps that name in the lane; one registered
;;;; with a literal :api-key gets a swarm-private variable instead
;;;; (EVO_SWARM_<PROVIDER>_API_KEY), set in the lane process's environment at
;;;; launch.  So no form, prompt or journal the swarm writes — the
;;;; coordinator's or a lane's — ever holds a key.

(in-package :evo.swarm)

;;; Prompt notes.  swarm.lisp may replace either.

(defparameter *coordinator-note*
  (evo:cat
   "## Swarm coordinator~%"
   "You lead a swarm: ~d worker lanes, each a separate evo agent with its "
   "own context, working in parallel.  You are the only one who talks to the "
   "user; lanes talk only to you.~%~%"
   "Splitting and delegating work is your own decision — do not wait to be "
   "told.  The user gives you goals, not lane assignments, and will rarely "
   "mention lanes at all.  For anything bigger than a quick answer or a small "
   "edit, decide yourself how to divide it and put lanes on it, without asking "
   "permission.  The lanes exist for parallel throughput: run independent "
   "pieces at the same time on different lanes rather than one after another, "
   "and treat an idle lane as wasted capacity.  Keep for yourself only what is "
   "quick, tightly coupled, or needs the user.~%~%"
   "How to work:~%"
   "- Explore just enough to split the work into independent, lane-sized "
   "pieces, each with a clear, checkable done criterion.  Investigation "
   "splits too — surveying a codebase, reproducing a bug, researching options, "
   "reviewing — not only edits.~%"
   "- Delegate with the `delegate` tool (a task, and for anything substantial "
   "an objective, which becomes the lane's goal).  Give each lane "
   "everything it needs: it cannot see this conversation.~%"
   "- Keep lanes busy.  While they work, prepare the next pieces and review "
   "what has come back; when a lane finishes and its work checks out, give it "
   "the next piece.  Handle a lane that is stuck, off track or asking a "
   "question yourself — answer, steer, reassign — and bring the user only "
   "decisions that are really theirs.~%"
   "- Lanes share this working directory by default.  When pieces would edit "
   "the same files, give a lane its own git worktree (`lane_worktree`) and "
   "merge its branch yourself when it reports done.~%"
   "- Lanes report with messages that arrive here as `[lane N ...]`: reports, "
   "finished runs, errors, restarts.  You do not need to poll; end your turn "
   "and you are woken when something arrives.  Use `lanes` for status, "
   "`lane_transcript` and `lane_reports` for detail.~%"
   "- Redirect a lane with `steer_lane` (next turn boundary) or "
   "`interrupt_and_steer` (now).  A lane that asks for a capability can be "
   "given one with `lane_eval` if you agree.~%"
   "- Integrate and verify the lanes' work yourself — run the tests, read the "
   "diffs — before telling the user it is done.  Relay what one lane needs "
   "from another; lanes never talk to each other.~%")
  "The coordinator's prompt note; ~d is the lane count.")

(defparameter *worker-note*
  (evo:cat
   "## Swarm lane ~d~%"
   "You are lane ~d of ~d in a swarm.  A coordinator agent gives you tasks and "
   "is the only one you report to; you never talk to the user or to other "
   "lanes, and your messages arrive from the coordinator.~%~%"
   "- Do the task you were given, within its scope, until its done criterion "
   "holds.  ~a~%"
   "- After each meaningful piece of work, call the `report` tool: what is "
   "done, the evidence (commands run, test output, files changed), what is "
   "next or what blocks you, and any request (a tool, a decision, something "
   "from another lane).  Report when you finish, and when you are stuck — "
   "never go quiet.~%"
   "- If you need a capability you lack, ask for it in a report's requests; "
   "the coordinator may install it into you.~%")
  "A lane's prompt note: its number, its number again, the lane count, and a
sentence about where it works.")

(defun set-coordinator-note (text)
  "Replace the coordinator's prompt note (a FORMAT control taking the lane
count), from swarm.lisp.  Takes effect from the next prompt built: the
registered note is a function that formats it with the live lane count."
  (setf *coordinator-note* text))

(defun set-worker-note (text)
  "Replace the lanes' prompt note (a FORMAT control taking lane, lane, lane
count and a where-you-work sentence).  Lanes get it when next initialized."
  (setf *worker-note* text))

(defun coordinator-note (&optional (workers (if *swarm* (swarm-workers *swarm*) 6)))
  (format nil *coordinator-note* workers))

(defun worker-note (lane workers)
  (format nil *worker-note* (lane-n lane) (lane-n lane) workers
          (if (lane-worktree lane)
              (format nil "You work in your own git worktree ~a, on branch ~a; commit your work there — the coordinator merges it."
                      (lane-worktree lane) (lane-branch lane))
              "You share the working directory with the coordinator and the other lanes, so keep to the files your task names.")))

;;; Tool limits.

(defvar *coordinator-tools* nil
  "Tool names the coordinator is limited to, or NIL for every tool.")

(defvar *lane-tools* nil
  "Alist (LANE-N-or-T . names): the tools a lane is limited to; T applies to
every lane without its own entry.  No entry means every tool.")

(defun set-coordinator-tools (names)
  "Limit the coordinator to NAMES (strings), or NIL for every tool.  Applied
at startup, on /reload and in every new session (APPLY-COORDINATOR-TOOLS)."
  (setf *coordinator-tools* names))

(defvar *applied-coordinator-tools* nil
  "The coordinator tool limit last journaled, so an unchanged one is not
journaled again on every /reload.")

(defun apply-coordinator-tools (agent &key new-session)
  "Journal the coordinator's tool limit in AGENT's session when it changed —
or, in a NEW-SESSION (whose journal carries no limit yet), whenever there is
one.  Lifting a limit restores every tool."
  (let ((names (and *coordinator-tools*
                    (remove-duplicates *coordinator-tools* :test #'equal))))
    (when (if new-session
              names
              (not (equal names *applied-coordinator-tools*)))
      (evo:set-active-tools agent names))
    (setf *applied-coordinator-tools* names)))

(defun set-lane-tools (names &key lanes)
  "Limit LANES (a list of lane numbers; NIL = every lane) to NAMES, or with
NAMES NIL give them every tool again.  The report tool is always kept."
  (if lanes
      (dolist (n lanes) (setf *lane-tools* (acons n names (remove n *lane-tools* :key #'car))))
      (setf *lane-tools* (acons t names (remove t *lane-tools* :key #'car)))))

(defun lane-tool-limit (lane)
  (let ((entry (or (assoc (lane-n lane) *lane-tools*) (assoc t *lane-tools*))))
    (and entry (cdr entry)
         (remove-duplicates (cons "report" (cdr entry)) :test #'equal))))

;;; Credentials.

(defun key-env-name (provider)
  "The swarm-private variable a literal key for PROVIDER travels in."
  (format nil "EVO_SWARM_~a_API_KEY"
          (substitute #\_ #\- (string-upcase (symbol-name provider)))))

(defun provider-env-var (provider)
  "The environment variable PROVIDER's key is read from in a lane: its own
:api-key-env, or the swarm-private one for a literal key; NIL for none."
  (let ((reg (provider-registration provider)))
    (cond ((plusp (length (getf reg :api-key))) (key-env-name provider))
          ((getf reg :api-key-env)))))

(defun lane-secret-environment ()
  "\"VAR=value\" strings for every provider registered with a literal key:
the only place a key crosses into a lane is its process environment."
  (loop for key in (provider-keys)
        for literal = (getf (provider-registration key) :api-key)
        when (plusp (length literal))
          collect (format nil "~a=~a" (key-env-name key) literal)))

;;; The baseline.

(defun model-registration-form (model)
  "(evo:register-model ...) reproducing MODEL (a registry plist)."
  `(evo:register-model ,(getf model :id)
                       ,@(loop for (k v) on model by #'cddr
                               unless (eq k :id)
                                 append (list k (if (consp v) `(quote ,v) v)))))

(defun model-fill-in-form (model)
  "MODEL (a coordinator registry plist) registered in a lane — unless the lane
has it already (IN-LANES registered it its own way), or lacks its API: an API
an extension defines exists in a lane only if IN-LANES loaded that extension,
and a model the lane cannot register is one it cannot use, not an error."
  `(when (and (member ,(getf model :api) (evo:api-keys))
              (not (ignore-errors (evo.provider:find-model ,(getf model :id)
                                                           ,(getf model :provider)))))
     ,(model-registration-form model)))

(defun lane-model-check-form (lane)
  "A form checking, in the lane, that its default model is registered: the
one failure a skipped model can cause.  Its error names the missing API and
what to do about it, so it reaches the coordinator as that, not as an unknown
API."
  (let ((apis (mapcar (lambda (model) (cons (getf model :id) (getf model :api)))
                      (all-models))))
    `(when (and (evo:setting :model)
                (not (ignore-errors (evo.provider:find-model (evo:setting :model)
                                                             (evo:setting :model-provider)))))
       (error "lane ~d cannot use its model ~a~@[: its API ~s is not in the lane — an extension defines it, so load that extension in the lanes with (evo.swarm:in-lanes ...) in swarm.lisp~]"
              ,(lane-n lane) (evo:setting :model)
              ;; The model's API, when that is what the lane lacks.  No
              ;; variables: a symbol built here would be this package's,
              ;; which a lane has never heard of.
              (and (not (member (cdr (assoc (evo:setting :model) ',apis :test #'equal))
                                (evo:api-keys)))
                   (cdr (assoc (evo:setting :model) ',apis :test #'equal)))))))

(defun provider-registration-form (key)
  (let ((reg (provider-registration key)))
    `(evo:register-provider ,key
                            ,@(when (getf reg :base-url) (list :base-url (getf reg :base-url)))
                            ,@(let ((env (provider-env-var key)))
                                (when env (list :api-key-env env))))))

(defparameter *report-tool-source*
  "(evo:register-tool \"report\"
  :description \"Report to the swarm coordinator — your only channel to it. Call it after each meaningful piece of work, when you finish, and whenever you are blocked: what is done, the evidence for it, what is next or what blocks you, and anything you need.\"
  :schema '(:object
            (:done :type :string :description \"What you finished since your last report\")
            (:evidence :type :string :optional t
             :description \"Proof: commands run and their output, tests, files changed\")
            (:next :type :string :optional t :description \"What you will do next\")
            (:blocked :type :string :optional t :description \"What stops you, if anything\")
            (:requests :type :string :optional t
             :description \"What you need from the coordinator: a tool, a decision, another lane's result\"))
  :execute (lambda (args)
             (evo.kernel:emit-event (or evo.kernel:*executing-agent* evo:*agent*)
                                    :type :report
                                    :done (getf args :done)
                                    :evidence (getf args :evidence)
                                    :next (getf args :next)
                                    :blocked (getf args :blocked)
                                    :requests (getf args :requests))
             \"Delivered to the coordinator.\"))"
  "The report tool, as registered in every lane: it emits a :report event,
which the coordinator's subscription to the lane turns into input.  Kept as
source and read in EVO.USER, the package a lane evaluates in: a form built
here would carry this package's symbols, which a lane has never heard of.")

(defun report-tool-form ()
  (let ((*package* (find-package :evo.user))
        (*read-eval* nil))
    (read-from-string *report-tool-source*)))

;;; in-lanes — code swarm.lisp has every lane evaluate.

(defvar *lane-forms* nil
  "What IN-LANES recorded, in order: plists (:LANE var :LANES var :SOURCE
path :FORMS forms).")

(defmacro in-lanes ((&optional lane lanes) &body forms)
  "Evaluate FORMS in every lane, not here — swarm.lisp's init.lisp for lanes:

  (evo.swarm:in-lanes (lane lanes)
    (load \"~/.evo/extensions/020-claude-oauth-provider.lisp\")
    (when (<= lane 2)
      (evo:set-setting :model \"claude-opus-5\")))

LANE and LANES, if named, are bound around each form to the lane's number
(1..LANES) and the lane count; (in-lanes () ...) binds neither.  The forms run
in the lane's EVO.USER one at a time, in order, each read after the one before
it ran, with *LOAD-TRUENAME* the swarm.lisp they came from — before any work,
and again whenever the lane restarts.  Every call adds to what lanes run."
  (dolist (var (list lane lanes))
    (when var
      (unless (and (symbolp var) (not (keywordp var)) (not (constantp var)))
        (error "evo.swarm:in-lanes: ~s is not a variable name" var))))
  `(add-lane-forms ',lane ',lanes ',forms *load-truename*))

(defun add-lane-forms (lane lanes forms source)
  (setf *lane-forms*
        (append *lane-forms*
                (list (list :lane lane :lanes lanes :forms forms
                            :source (and source (namestring source))))))
  (length *lane-forms*))

(defun lane-code-forms (lane swarm)
  "Every IN-LANES form, each wrapped in its bindings for LANE."
  (loop for entry in *lane-forms*
        for source = (getf entry :source)
        for bindings = (append
                        (when source
                          `((*load-truename* (pathname ,source))
                            (*load-pathname* (pathname ,source))))
                        (when (getf entry :lane) `((,(getf entry :lane) ,(lane-n lane))))
                        (when (getf entry :lanes) `((,(getf entry :lanes) ,(swarm-workers swarm)))))
        append (mapcar (lambda (form) (if bindings `(let ,bindings ,form) form))
                       (getf entry :forms))))

;;; What swarm.lisp sets, fresh on every boot and /reload.

(defparameter *default-coordinator-note* *coordinator-note*)
(defparameter *default-worker-note* *worker-note*)

(defun load-swarm-config (cwd)
  "Forget what swarm.lisp said last time, then load ~/.evo/swarm.lisp and
<cwd>/.evo/swarm.lisp — as the last step of evo's userspace build (after
post-init.lisp; see EVO.KERNEL:*POST-INIT-HOOKS*), so swarm.lisp can set the
coordinator's models as well as the swarm's own settings."
  (setf *lane-forms* nil
        *lane-tools* nil
        *coordinator-tools* nil
        *coordinator-note* *default-coordinator-note*
        *worker-note* *default-worker-note*)
  (load-init-file (merge-pathnames "swarm.lisp" (evo-home)))
  (load-init-file (merge-pathnames "swarm.lisp" (project-evo-dir cwd)))
  ;; On /reload the swarm is running: its coordinator gets the new limit.  At
  ;; startup there is no swarm yet, and RUN-SWARM applies it after boot.
  (when *swarm*
    (apply-coordinator-tools (swarm-agent *swarm*))))

;;; The baseline, in order.

(defun baseline-forms (lane swarm)
  "Everything a lane is given, in order:
 1. the coordinator's providers (keys by variable name only), and its model
    and thinking level as the lane's defaults — what IN-LANES may override;
 2. every IN-LANES form from swarm.lisp;
 3. the coordinator's models those forms did not register and whose API the
    lane has (only now: a model whose API an extension defines needs IN-LANES
    to load it first; without that, the model is skipped), then a check that
    the lane's default model is registered, with an error saying why not;
 4. the report tool, the lane's prompt note, its tool limit — the swarm's
    own, last, so IN-LANES cannot lose them;
 5. a run for any goal continuation its resumed session left queued."
  (let* ((agent evo:*agent*)
         (state (and agent (fold-state (agent-journal agent))))
         (model-id (and state (ignore-errors (effective-model-id state agent))))
         (provider (and model-id (effective-model-provider state model-id)))
         (limit (lane-tool-limit lane)))
    (append
     (mapcar #'provider-registration-form (provider-keys))
     (when model-id `((evo:set-setting :model ,model-id)))
     (when provider `((evo:set-setting :model-provider ,provider)))
     (when state
       `((evo:set-setting :thinking ,(effective-thinking state (agent-thinking-override agent)))))
     (lane-code-forms lane swarm)
     (mapcar #'model-fill-in-form (all-models))
     (list (lane-model-check-form lane))
     (list (report-tool-form)
           `(evo:register-prompt-note "swarm-worker"
                                      ,(worker-note lane (swarm-workers swarm))))
     (when limit `((evo:set-active-tools evo:*agent* ',limit)))
     '((when (evo.kernel:steering-pending-p evo:*agent*) (evo:request-run))))))

(defun forms->code (forms)
  "FORMS as source text a lane reads back in EVO.USER."
  (with-standard-io-syntax
    (let ((*package* (find-package :evo.user))
          (*print-readably* nil)
          (*print-case* :downcase)
          (*print-pretty* nil))
      (format nil "~{~s~%~}" forms))))


