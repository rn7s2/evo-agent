;;;; init.lisp — what a fresh lane is given: worker init is a program.
;;;;
;;;; A lane boots `evo serve --no-userspace`: kernel and core extensions,
;;;; nothing of the user's.  What it then becomes is decided here, by
;;;; GENERATORS — functions of the lane and the swarm that return forms, which
;;;; the swarm evaluates in the lane (POST /eval) before it is given any work,
;;;; and again whenever it restarts.  ~/.evo/swarm.lisp and
;;;; <project>/.evo/swarm.lisp add, replace or remove generators; the default
;;;; one, :BASELINE, gives every lane what the coordinator itself runs on.
;;;;
;;;; Secrets never travel as data.  A provider the coordinator reads its key
;;;; from an environment variable keeps that name in the lane; one registered
;;;; with a literal :api-key gets a swarm-private variable instead
;;;; (EVO_SWARM_<PROVIDER>_API_KEY), set in the lane process's environment at
;;;; launch.  So no form, prompt or journal — the coordinator's or a lane's —
;;;; ever holds a key.

(in-package :evo.swarm)

;;; Prompt notes.  swarm.lisp may replace either.

(defparameter *coordinator-note*
  (evo:cat
   "## Swarm coordinator~%"
   "You coordinate a swarm: ~d worker lanes, each a separate evo agent with its "
   "own context, working in parallel.  You are the only one who talks to the "
   "user; lanes talk only to you.~%~%"
   "How to work:~%"
   "- Explore just enough to split the work into lane-sized pieces, each with "
   "a clear, checkable done criterion.  Do small or tightly coupled work "
   "yourself.~%"
   "- Delegate with the `delegate` tool (a task, and for anything substantial "
   "an objective with done_when, which becomes the lane's goal).  Give each "
   "lane everything it needs: it cannot see this conversation.~%"
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
count), from swarm.lisp.  Takes effect from the next prompt built."
  (setf *coordinator-note* text)
  (install-coordinator-note))

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

(defun install-coordinator-note ()
  (evo:register-prompt-note "swarm-coordinator" (coordinator-note)))

;;; Tool limits.

(defvar *coordinator-tools* nil
  "Tool names the coordinator is limited to, or NIL for every tool.")

(defvar *lane-tools* nil
  "Alist (LANE-N-or-T . names): the tools a lane is limited to; T applies to
every lane without its own entry.  No entry means every tool.")

(defun set-coordinator-tools (names)
  "Limit the coordinator to NAMES (strings), or NIL for every tool."
  (setf *coordinator-tools* names))

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

(defun baseline-forms (lane swarm)
  "The default generator: the coordinator's models and providers (keys by
variable name only), its model and thinking as the lane's defaults, the
report tool, the lane's prompt note, its tool limit — and a run for any goal
continuation its resumed session left queued behind the missing model."
  (declare (ignore swarm))
  (let* ((agent evo:*agent*)
         (state (and agent (fold-state (agent-journal agent))))
         (model-id (and state (ignore-errors (effective-model-id state agent))))
         (provider (and model-id (effective-model-provider state model-id)))
         (limit (lane-tool-limit lane)))
    (append
     (mapcar #'provider-registration-form (provider-keys))
     (mapcar #'model-registration-form (all-models))
     (when model-id `((evo:set-setting :model ,model-id)))
     (when provider `((evo:set-setting :model-provider ,provider)))
     (when state
       `((evo:set-setting :thinking ,(effective-thinking state (agent-thinking-override agent)))))
     (list (report-tool-form)
           `(evo:register-prompt-note "swarm-worker"
                                      ,(worker-note lane (swarm-workers *swarm*))))
     (when limit `((evo:set-active-tools evo:*agent* ',limit)))
     '((when (evo.kernel:steering-pending-p evo:*agent*) (evo:request-run))))))

(defvar *worker-inits* (list (cons :baseline 'baseline-forms))
  "Ordered alist (NAME . FUNCTION): every generator, run in order for each
lane initialization.  FUNCTION takes (LANE SWARM) and returns a list of forms.")

(defun default-worker-init (lane swarm)
  "The baseline generator, callable from a replacement that extends it."
  (baseline-forms lane swarm))

(defun add-worker-init (name function)
  "Add (or replace, keeping its place) the generator NAME.  FUNCTION takes the
lane and the swarm and returns forms to evaluate in the lane, e.g.
  (evo.swarm:add-worker-init :my-tools
    (lambda (lane swarm)
      (declare (ignore swarm))
      (when (evenp (evo.swarm:lane-n lane))
        '((evo:load-extension \"/home/me/tools/lint.lisp\")))))"
  (let ((entry (assoc name *worker-inits*)))
    (if entry
        (setf (cdr entry) function)
        (setf *worker-inits* (append *worker-inits* (list (cons name function))))))
  name)

(defun remove-worker-init (name)
  "Remove the generator NAME — :BASELINE included, for a swarm.lisp that
builds lanes from scratch."
  (setf *worker-inits* (remove name *worker-inits* :key #'car))
  name)

(defun worker-inits () (mapcar #'car *worker-inits*))

(defun init-forms (lane swarm)
  "Every generator's forms for LANE, in order."
  (loop for (nil . fn) in *worker-inits*
        append (funcall fn lane swarm)))

(defun forms->code (forms)
  "FORMS as source text a lane reads back in EVO.USER."
  (with-standard-io-syntax
    (let ((*package* (find-package :evo.user))
          (*print-readably* nil)
          (*print-case* :downcase)
          (*print-pretty* nil))
      (format nil "~{~s~%~}" forms))))
