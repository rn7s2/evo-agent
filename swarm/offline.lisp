;;;; offline.lisp — `evo-swarm catalog --json` and `evo-swarm check --json`.
;;;;
;;;; Both answer a question the GUI has to answer before it can start a swarm,
;;;; and neither starts one (nor a listener, nor a lane):
;;;;
;;;;   catalog   what a swarm launched from here could use — the coordinator's
;;;;             catalog, plus the models a lane can run, worked out by
;;;;             evaluating the swarm's own lane configuration in a sandbox
;;;;             (LANE-PLAN, below) instead of by running a lane and asking it.
;;;;   check     whether a launch would work: are the models resolvable, is
;;;;             each model's API present where it runs, are the keys there —
;;;;             and what the launch would resolve: the coordinator's effort
;;;;             and the lanes' (their own forms, see LANE-PLAN), and the lane
;;;;             count, so a client's controls open on the truth instead of on
;;;;             medium and six (CONTRACT §2).
;;;;
;;;; The userspace is booted in-process first (init files, extensions,
;;;; swarm.lisp), because that is what registers the models and providers these
;;;; answers are about.  Nothing is left behind: no session is written, no
;;;; process is spawned.

(in-package :evo.swarm)

;;; Where the models come from.

(defun session-model-entry (agent)
  "The model this session's next turn would run on, as (values model reason)."
  (let* ((state (fold-state (agent-journal agent)))
         (id (handler-case (effective-model-id state agent) (error () nil)))
         (model (handler-case (effective-model state agent) (error () nil))))
    (cond (model (values model nil))
          (id (values nil "the model it names is not a registered model"))
          (t (values nil "no model is configured")))))

(defun check-problem (code message)
  (list :code code :message message))

(defun check-model-entry (ref agent)
  "One half of `check`: REF (ID[@PROVIDER]) when the launch names a model, else
the session's own.  Returns the entry {:id :provider :ok :reason} as its value
and the problem it found — a plist, or NIL — as a second one.  Two values, not
a list the caller pushes onto: PUSH only ever grew this function's own
binding, so every problem a check found was thrown away by the caller that
asked for it."
  (multiple-value-bind (model reason code)
      (if ref
          (handler-case (values (find-model-ref ref) nil nil)
            (error () (values nil "no registered model matches it" "model_unresolved")))
          (session-model-entry agent))
    (if (null model)
        (values (list :id ref :provider nil :ok nil :reason reason)
                (check-problem (or code "model_unresolved")
                               (format nil "the model ~@[~a ~]the swarm would run is not usable: ~a"
                                       ref reason)))
        (multiple-value-bind (ready why why-code) (evo.serve:model-readiness model)
          (values (list :id (getf model :id) :provider (getf model :provider)
                        :ok ready :reason why)
                  (unless ready
                    (check-problem (or why-code "model_unready")
                                   (format nil "the model ~a cannot run: ~a"
                                           (getf model :id) why))))))))

(defun check-lane-entry (plan)
  "The model a lane would start with, and the problem with it — a plist, or
NIL.  The verdict comes from PLAN, the lane itself evaluated (LANE-PLAN): a
lane's model is whatever the swarm's in-lanes code left it on, and its API
counts as present only if that same code registered it in the lane.  The entry
and the problem, as CHECK-MODEL-ENTRY returns them."
  (let ((status (lane-plan-status plan)))
    (values (list :id (getf status :id) :provider (getf status :provider)
                  :ok (getf status :ok) :reason (getf status :reason))
            (unless (getf status :ok)
              (check-problem
               (lane-plan-problem-code plan)
               (format nil "a lane cannot run ~@[~a~]~@[: ~a~]"
                       (getf status :id) (getf status :reason)))))))

(defun check-thinking (opts agent resumed-p &optional plan)
  "The effort a launch from these OPTS would resolve, as two names: the
coordinator's and the lanes'.

The coordinator's is evo's own chain — the journaled /thinking choice, the
--thinking flag, the :thinking setting, then medium (EFFECTIVE-THINKING, which
normalizes anything retired onto a live rung).  A lane's is what its own forms
leave it on (PLAN): --lane-thinking over the swarm's in-lanes settings, over
the coordinator's.  With no PLAN — a caller that has not evaluated the lane
forms — the chain the launch applies stands in: --lane-thinking, then what a
resumed record restored, then the coordinator's (CONTRACT §1, §4.3)."
  (let* ((state (ignore-errors (fold-state (agent-journal agent))))
         ;; The launch's --thinking is where setup-agent would have put it (the
         ;; journal), so it belongs in the chain as the override too.
         (coordinator (effective-thinking state (or (getf opts :thinking)
                                                    (agent-thinking-override agent))))
         (record (and resumed-p (ignore-errors (evo:custom-state "swarm" agent))))
         ;; A level retired from the ladder — an :off left in a record or a
         ;; setting — is normalized onto a live rung, as the launch does.
         (lane (or (and plan (lane-plan-thinking plan))
                   (normalize-thinking-level (getf opts :lane-thinking))
                   (normalize-thinking-level (and record (getf record :lane-thinking)))
                   coordinator)))
    (values (string-downcase (symbol-name coordinator))
            (string-downcase (symbol-name lane)))))

(defun check-workers (opts)
  "How many lanes OPTS asks for, and the problem with that number, if any."
  (let ((n (getf opts :workers)))
    (cond
      ((null n) (values (resolved-workers opts) nil))
      ((and (integerp n) (<= 1 n 64)) (values n nil))
      (t (values n
                 (check-problem "invalid_workers"
                                (format nil "--workers must be a number from 1 to 64, got ~a" n)))))))

(defun resolved-workers (opts)
  "The lane count a launch from OPTS would run: --workers, else the
:swarm-workers setting, else 6 — the launch's own rule (RUN-SWARM), for a
caller that has to compute what a lane would end up with before it knows
whether the number is even legal."
  (or (let ((n (getf opts :workers))) (and (integerp n) (<= 1 n 64) n))
      (let ((setting (setting :swarm-workers))) (and (integerp setting) setting))
      6))

;;; What a lane would end up with.
;;;
;;; A lane boots `evo-agent serve --no-userspace`: the kernel's registries and
;;; nothing of the user's, then the coordinator's baseline — its providers, its
;;; model and thinking level, the swarm's IN-LANES code, the models those left
;;; out (swarm/init.lisp).  Which model and thinking level that ends on is
;;; `.evo/swarm.lisp`'s business, so the only honest way to answer "what would
;;; a lane run" is to evaluate that code the way a lane does.  `check` and
;;; `catalog` do it here, in a sandbox of the coordinator's own process:
;;; nothing is spawned, no model is called, and nothing of the evaluation is
;;; left behind.
;;;
;;; What the sandbox cannot undo is what a form does to the image itself:
;;; loading an extension defines classes and methods, which are global.  That
;;; is why the forms are only ever evaluated for what they *register* — and
;;; why an extension loaded this way must be idempotent, which every evo
;;; extension is by contract (register-api replaces, REGISTER-PROVIDER merges).

(defstruct (lane-plan (:constructor %make-lane-plan))
  model-id    ; the model id a lane's settings end on, or NIL
  provider    ; the provider serving it
  api         ; its API's registry key — what the lane must have registered
  ready       ; whether the lane could authenticate and run it
  reason      ; why not, when it could not — never a credential
  thinking    ; the rung a lane's settings end on, or NIL
  apis        ; the provider APIs the lane's registry ended with, in order
  models      ; every registered model as LANE-MODEL-STATUS judges it
  problems)   ; what the lane's forms could not do

(defun call-with-lane-sandbox (thunk)
  "Call THUNK with the runtime a fresh `--no-userspace` lane boots with — the
kernel's APIs and the providers they seed, no models, no settings — and with
GETENV answering the variables a launch puts in a lane's environment, so a
lane's own configuration computes the verdict that lane would compute.  The
coordinator's runtime goes back afterwards whatever THUNK does.

Only the registries a lane's configuration shows up in are swapped: models,
providers, APIs and settings are the lane's copies, and everything THUNK
registers elsewhere (tools, commands, prompt notes) is put back with the
runtime catalog (CAPTURE-/INSTALL-RUNTIME-CATALOG, the pair /reload uses)."
  (let ((saved (evo.kernel:capture-runtime-catalog))
        ;; Read off the coordinator's providers BEFORE the sandbox replaces
        ;; them: the keys a launch would set in the lane process.
        (environment (loop for entry in (lane-secret-environment)
                           for eq = (position #\= entry)
                           collect (cons (subseq entry 0 eq)
                                         (subseq entry (1+ eq))))))
    (unwind-protect
         (let ((*settings* nil)
               (evo.provider::*models* nil)
               (evo.provider::*providers* nil)
               (evo.provider::*apis* (evo.serve:kernel-api-registry))
               (*environment-overlay* environment))
           (evo.provider:reset-user-registries)
           (funcall thunk))
      (evo.kernel:install-runtime-catalog saved))))

(defun lane-setup-forms-for (opts agent resumed-p)
  "The setup half of a lane's baseline (LANE-SETUP-FORMS) for a swarm launched
with OPTS from AGENT's session: lane 1 of the resolved worker count, with the
lane configuration the launch would give it — its flags, else what a resumed
record restored."
  (let ((record (and resumed-p (ignore-errors (evo:custom-state "swarm" agent)))))
    (lane-setup-forms
     (%make-lane :n 1)
     (%make-swarm :workers (resolved-workers opts)
                  :lane-model (or (getf opts :lane-model) (getf record :lane-model))
                  :lane-provider (or (getf opts :lane-provider) (getf record :lane-provider))
                  :lane-thinking (or (getf opts :lane-thinking) (getf record :lane-thinking))
                  :agent agent))))

(defun lane-plan (opts agent &optional resumed-p)
  "What lane 1 of a swarm launched with OPTS from AGENT's session would end up
with: its model, provider and thinking level, the APIs its own code registers,
and every model as it would see it.  Each form that fails becomes a problem
rather than an error — a swarm.lisp with a broken form is a launch that would
fail, which is exactly what `check` exists to report."
  (let ((forms (lane-setup-forms-for opts agent resumed-p))
        ;; The coordinator's models, to name the API of one the lane cannot
        ;; register itself: "its API comes from an extension" and "there is no
        ;; such model" are different answers.
        (configured (all-models)))
    (call-with-lane-sandbox
     (lambda ()
       (let ((problems nil))
         (dolist (form forms)
           (handler-case (eval form)
             (error (e)
               (push (check-problem
                      "lane_config_failed"
                      (format nil "the lanes' configuration fails to run: ~a" e))
                     problems))))
         (let* ((id (setting :model))
                ;; A lane has no session of yours: its state folds nothing.
                (lane-state (evo.journal:empty-state))
                (provider (and id (ignore-errors (effective-model-provider lane-state id))))
                (model (and id (ignore-errors (find-model id provider))))
                (declared (and id (or model
                                      (find id configured
                                            :key (lambda (m) (pget m :id))
                                            :test #'string=))))
                (apis (api-keys)))
           (multiple-value-bind (ready reason)
               (if model (evo.serve:model-readiness model) (values nil nil))
             (%make-lane-plan
              :model-id id
              :provider (or (and declared (getf declared :provider)) provider)
              :api (and declared (getf declared :api))
              :ready ready
              :reason reason
              :thinking (effective-thinking lane-state nil)
              :apis apis
              ;; The models the *lane* has, not the coordinator's: what the
              ;; fill-ins registered, plus whatever its in-lanes code
              ;; registered on its own.
              :models (loop for m in (all-models)
                            collect (evo.serve:lane-model-status m apis))
              :problems (nreverse problems)))))))))

(defun lane-plan-status (plan)
  "PLAN's model as a lane would see it, in LANE-MODEL-STATUS's shape
\(:id :provider :ok :reason).  A model whose API the lane's own code did not
load is not runnable there, however ready it is here."
  (let ((id (lane-plan-model-id plan))
        (api (lane-plan-api plan)))
    (cond
      ((null id)
       (list :id nil :provider nil :ok nil
             :reason (or (lane-plan-reason plan) "the lanes' forms name no model")))
      ((null api)
       (list :id id :provider (lane-plan-provider plan) :ok nil
             :reason (or (lane-plan-reason plan) "no registered model matches it")))
      ((member api (lane-plan-apis plan))
       (list :id id :provider (lane-plan-provider plan)
             :ok (and (lane-plan-ready plan) t)
             :reason (lane-plan-reason plan)))
      (t
       (list :id id :provider (lane-plan-provider plan) :ok nil
             :reason (format nil "its API ~(~a~) comes from an extension: load that extension in the lanes with (evo.swarm:in-lanes ...) in swarm.lisp"
                             api))))))

(defun lane-plan-problem-code (plan)
  "Why PLAN's lane model is not runnable, as the machine code a chooser
branches on: the model is not registered at all, it is registered but cannot
authenticate, or its API is one the lane never loaded."
  (if (null (lane-plan-api plan))
      "lane_model_unresolved"
      (if (member (lane-plan-api plan) (lane-plan-apis plan))
          "lane_model_unready"
          "lane_api_missing")))

(defun lane-plan-catalog (plan)
  "PLAN as the catalog's lane half: every model, as this lane would see it."
  (list :models (coerce (mapcar #'evo.serve:lane-model-entry (lane-plan-models plan))
                        'vector)))

;;; The two commands.

(defun write-json-line (value)
  (write-line (evo.serve:encode-json value))
  (finish-output))

(defun cmd-catalog (opts)
  "Print what a swarm launched from here could use, and exit.  Lanes are
computed without running one: the lane's setup forms are evaluated in a
sandbox, so an in-lanes model and an API an extension loads into the lane are
both reflected (LANE-PLAN, EVO.SERVE:CATALOG-PLIST)."
  (install-coordinator nil)
  (multiple-value-bind (agent resumed-p) (evo.cli:setup-agent opts)
    (write-json-line (evo.serve:catalog-plist
                      agent :swarm (lane-plan-catalog (lane-plan opts agent resumed-p))))
    0))

(defun check-entry-wire (entry)
  "ENTRY as the check document carries it: `ok` is a bool on the wire, where
NIL would encode as null — and a client parsing a flag cannot read null."
  (pput entry :ok (wire-boolean (getf entry :ok))))

(defun cmd-check (opts)
  "Print whether a launch from here would work, and exit 1 when it would not
(CONTRACT §2)."
  (install-coordinator nil)
  (multiple-value-bind (agent resumed-p) (evo.cli:setup-agent opts)
    ;; Worked out once, before anything is reported: a lane's model, thinking
    ;; level and API set are what its own forms leave it on, not what a chain
    ;; of defaults would guess.
    (let ((problems nil)
          (plan (lane-plan opts agent resumed-p)))
      (multiple-value-bind (model problem) (check-model-entry (getf opts :model) agent)
        (when problem (push problem problems))
        (multiple-value-bind (lane lane-problem) (check-lane-entry plan)
          (when lane-problem (push lane-problem problems))
          ;; Whatever the lanes' own code could not do, in the order it failed.
          (dolist (p (reverse (lane-plan-problems plan))) (push p problems))
          (multiple-value-bind (workers workers-problem) (check-workers opts)
            (when workers-problem (push workers-problem problems))
            (multiple-value-bind (thinking lane-thinking)
                (check-thinking opts agent resumed-p plan)
              (let ((ok (null problems)))
                (write-json-line (list :ok (wire-boolean ok)
                                       :model (check-entry-wire model)
                                       :lane-model (check-entry-wire lane)
                                       ;; What evo itself resolves for a launch
                                       ;; with no flags, so a client's controls
                                       ;; open on these instead of hard-coding
                                       ;; medium and six.
                                       :thinking thinking
                                       :lane-thinking lane-thinking
                                       :workers workers
                                       :problems (coerce (reverse problems) 'vector)))
                (if ok 0 1)))))))))
