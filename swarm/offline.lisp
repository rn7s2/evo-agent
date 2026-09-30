;;;; offline.lisp — `evo-swarm catalog --json` and `evo-swarm check --json`.
;;;;
;;;; Both answer a question the GUI has to answer before it can start a swarm,
;;;; and neither starts one (nor a listener, nor a lane):
;;;;
;;;;   catalog   what a swarm launched from here could use — the coordinator's
;;;;             catalog, plus the models a lane can run, computed from the
;;;;             kernel API set (evo.serve:*kernel-apis*) instead of by running
;;;;             a lane and asking it.
;;;;   check     whether a launch would work: are the models resolvable, is
;;;;             each model's API present where it runs, are the keys there.
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

(defun check-lane-entry (ref agent)
  "The models a lane would start with: REF (--lane-model), else the
coordinator's own — and, either way, whether that model's API is one a lane
has (evo.serve:*kernel-apis*) before it is asked whether the model is usable.
The entry and the problem, as CHECK-MODEL-ENTRY returns them."
  (multiple-value-bind (model reason code)
      (if ref
          (handler-case (values (find-model-ref ref) nil nil)
            (error () (values nil "no registered model matches it" "lane_model_unresolved")))
          (session-model-entry agent))
    (if (null model)
        (values (list :id ref :provider nil :ok nil :reason reason)
                (check-problem (or code "lane_model_unresolved")
                               (format nil "the model the lanes would run ~@[~a ~]is not usable: ~a"
                                       ref reason)))
        (let* ((status (evo.serve:lane-model-status model))
               (ok (getf status :ok))
               (why (getf status :reason)))
          (values (list :id (getf model :id) :provider (getf model :provider)
                        :ok ok :reason why)
                  (unless ok
                    (check-problem
                     (if (member (getf model :api) evo.serve:*kernel-apis*)
                         "lane_model_unready"
                         "lane_api_missing")
                     (format nil "a lane cannot run ~a: ~a" (getf model :id) why))))))))

(defun check-workers (opts)
  "How many lanes OPTS asks for, and the problem with that number, if any."
  (let ((n (getf opts :workers)))
    (cond
      ((null n) (values (or (let ((setting (setting :swarm-workers)))
                              (and (integerp setting) setting))
                          6)
                        nil))
      ((and (integerp n) (<= 1 n 64)) (values n nil))
      (t (values n
                 (check-problem "invalid_workers"
                                (format nil "--workers must be a number from 1 to 64, got ~a" n)))))))

;;; The two commands.

(defun swarm-config-plist (opts)
  "How this invocation configures lanes, as a plist — what the catalog's lane
half and the swarm record both describe."
  (list :lane-model (getf opts :lane-model)
        :lane-provider (getf opts :lane-provider)
        :lane-thinking (getf opts :lane-thinking)
        :workers (getf opts :workers)))

(defun write-json-line (value)
  (write-line (evo.serve:encode-json value))
  (finish-output))

(defun cmd-catalog (opts)
  "Print what a swarm launched from here could use, and exit.  Lanes are
computed without running one (EVO.SERVE:CATALOG-PLIST)."
  (install-coordinator nil)
  (multiple-value-bind (agent resumed-p) (evo.cli:setup-agent opts)
    (declare (ignore resumed-p))
    (write-json-line (evo.serve:catalog-plist agent :swarm (swarm-config-plist opts)))
    0))

(defun cmd-check (opts)
  "Print whether a launch from here would work, and exit 1 when it would not
(CONTRACT §2)."
  (install-coordinator nil)
  (multiple-value-bind (agent resumed-p) (evo.cli:setup-agent opts)
    (declare (ignore resumed-p))
    (let ((problems nil))
      (multiple-value-bind (model problem) (check-model-entry (getf opts :model) agent)
        (when problem (push problem problems))
        (multiple-value-bind (lane lane-problem)
            (check-lane-entry (getf opts :lane-model) agent)
          (when lane-problem (push lane-problem problems))
          (multiple-value-bind (workers workers-problem) (check-workers opts)
            (declare (ignore workers))
            (when workers-problem (push workers-problem problems))
            (let ((ok (null problems)))
              (write-json-line (list :ok ok
                                     :model model
                                     :lane-model lane
                                     :problems (coerce (reverse problems) 'vector)))
              (if ok 0 1))))))))
