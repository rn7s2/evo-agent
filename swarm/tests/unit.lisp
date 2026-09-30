;;;; swarm/tests/unit.lisp — unit tests for evo-swarm: worker init (and keeping
;;;; secrets out of it), in-lanes, tool limits, the journal record, lane
;;;; events turned into coordinator input, restart arguments.  Nothing here
;;;; starts a process; tests/swarm-e2e.py does that.

(defpackage :evo.swarm.tests
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel :evo.swarm)
  (:export #:run-all))

(in-package :evo.swarm.tests)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (name form)
  `(handler-case
       (if ,form
           (incf *pass*)
           (progn (incf *fail*) (format t "FAIL ~a: ~s was NIL~%" ,name ',form)))
     (error (e)
       (incf *fail*)
       (format t "FAIL ~a: signaled ~a~%" ,name e))))

(defun tmp-dir ()
  (string-right-trim "/\\" (namestring (uiop:temporary-directory))))

(defmacro with-registries (() &body body)
  "BODY with a clean model/provider registry and settings, restored after."
  `(let ((saved (evo.kernel:capture-runtime-catalog)))
     (unwind-protect
          (progn (reset-settings) (reset-user-registries) ,@body)
       (evo.kernel:install-runtime-catalog saved))))

(defun fresh-agent ()
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "~a/evo-swarm-unit-~a/" (tmp-dir) (gen-id)))))
    (ensure-directories-exist dir)
    (make-agent :journal (make-session-journal dir))))

(defun test-swarm (&key (workers 3) agent view server)
  "A swarm of WORKERS lanes, with no processes and no ports: what the swarm
computes and journals, without anything running."
  (let ((swarm (evo.swarm::%make-swarm
                :id "unit" :workers workers :agent agent :view view :server server
                :dir (uiop:ensure-directory-pathname
                      (format nil "~a/evo-swarm-unit-~a/" (tmp-dir) (gen-id)))
                :cwd (uiop:getcwd))))
    (setf (evo.swarm::swarm-lanes swarm)
          (loop for n from 1 to workers
                collect (evo.swarm::make-lane-for swarm n)))
    swarm))

(defun read-all (code)
  "Every form in CODE, read as a lane reads it: EVO.USER, no #. evaluation."
  (let ((*package* (find-package :evo.user)) (*read-eval* nil))
    (with-input-from-string (in code)
      (loop for form = (read in nil :eof) until (eq form :eof) collect form))))

(defun plist-keys (value)
  "Every keyword key anywhere in VALUE — plists, lists, vectors."
  (cond ((and (consp value) (keywordp (car value)))
         (loop for (k v) on value by #'cddr append (cons k (plist-keys v))))
        ((consp value) (append (plist-keys (car value)) (plist-keys (cdr value))))
        ((and (vectorp value) (not (stringp value)))
         (loop for x across value append (plist-keys x)))
        (t nil)))

(defmacro with-published ((ops) &body body)
  "BODY with PUBLISH-OP bound to a collector: OPS is what a client would have
received, oldest first.  Nothing here needs a server."
  `(let ((,ops nil)
         (saved (symbol-function 'evo.swarm::publish-op)))
     (unwind-protect
          (progn
            ;; Appended, not pushed: the body reads OPS in the order a client
            ;; would have received them, not backwards.
            (setf (symbol-function 'evo.swarm::publish-op)
                  (lambda (op) (setf ,ops (append ,ops (list op))) t))
            ,@body
            ,ops)
       (setf (symbol-function 'evo.swarm::publish-op) saved))))

(defmacro with-lane-snapshot ((state &key (items '()) (has-more nil)) &body body)
  "BODY with LANE-SNAPSHOT answering STATE/ITEMS — the lane's own snapshot,
without a lane process."
  `(let ((saved (symbol-function 'evo.swarm::lane-snapshot)))
     (unwind-protect
          (progn
            (setf (symbol-function 'evo.swarm::lane-snapshot)
                  (lambda (lane &key topics items timeout)
                    (declare (ignore lane topics items timeout))
                    (list :epoch "e1" :seq 10
                          :topics (list :session
                                        (list :state ,state
                                              :items (coerce ,items 'vector)
                                              :has-more ,has-more)))))
            ,@body)
       (setf (symbol-function 'evo.swarm::lane-snapshot) saved))))

(defun symbols-in (form)
  (cond ((and (symbolp form) form) (list form))
        ((consp form) (append (symbols-in (car form)) (symbols-in (cdr form))))
        ((and (vectorp form) (not (stringp form)))
         (loop for x across form append (symbols-in x)))))

;;; A view that remembers what it was told, for the tests: the swarm's
;;; frontend seam (view.lisp) reaches it exactly as it reaches the TUI's or
;;; serve's.

(defclass recording-view (view)
  ((said :initform nil) (repaints :initform 0)
   (events :initform nil) (ran :initform nil)))

(defmethod view-say ((view recording-view) text &key style)
  (push (list text style) (slot-value view 'said)))

(defmethod view-repaint ((view recording-view))
  (incf (slot-value view 'repaints)))

(defmethod view-record-event ((view recording-view) event)
  (push event (slot-value view 'events)))

(defmethod view-run ((view recording-view) agent resumed-p)
  (setf (slot-value view 'ran) (list agent resumed-p))
  7)

(defun said (view)
  "What VIEW was told, oldest first."
  (reverse (slot-value view 'said)))

(defun test-baseline ()
  (with-registries ()
    (register-provider* :stub :base-url "http://127.0.0.1:1" :api-key "LITERAL-SECRET")
    (register-provider* :envy :base-url "http://127.0.0.1:2" :api-key-env "ENVY_KEY")
    (register-model* "m-a" :provider :stub :context-window 1000 :max-output 100
                           :effort '(:low :medium))
    (set-setting :model "m-a")
    (let* ((agent (fresh-agent))
           (evo:*agent* agent)
           (*swarm* (test-swarm :agent agent))
           (lane (first (swarm-lanes *swarm*)))
           (forms (baseline-forms lane *swarm*))
           (code (evo.swarm::forms->code forms)))
      (check "baseline: no literal secret in the forms"
             (not (search "LITERAL-SECRET" code)))
      (check "baseline: a literal key becomes a swarm-private variable name"
             (search ":api-key-env \"EVO_SWARM_STUB_API_KEY\"" code))
      (check "baseline: an env-var key keeps its variable name"
             (search ":api-key-env \"ENVY_KEY\"" code))
      (check "baseline: the secret travels in the lane's environment only"
             (member "EVO_SWARM_STUB_API_KEY=LITERAL-SECRET"
                     (evo.swarm::lane-secret-environment) :test #'equal))
      (check "baseline: a provider without a literal key adds no variable"
             (notany (lambda (e) (search "ENVY" e)) (evo.swarm::lane-secret-environment)))
      (check "baseline: the coordinator's model is registered and the default"
             (and (search "(register-model \"m-a\"" code)
                  (search "(set-setting :model \"m-a\")" code)))
      (check "baseline: the effort ladder is quoted, not evaluated"
             (let ((model (third (first (fill-in-forms (read-all code))))))
               (equal '(quote (:low :medium)) (getf (cddr model) :effort))))
      (check "baseline: the report tool and the lane's prompt note"
             (and (search "(register-tool \"report\"" code)
                  (search "## Swarm lane 1" code)))
      (let ((read (read-all code)))
        (check "baseline: the code reads back in a lane (EVO.USER), form for form"
               (= (length read) (length forms)))
        (check "baseline: no swarm symbol reaches a lane that has never heard of the swarm"
               (notany (lambda (s) (eq (symbol-package s) (find-package :evo.swarm)))
                       (symbols-in read))))
      ;; The report tool really is a tool: evaluate its registration here.
      (let ((events nil))
        (setf (agent-events-cb agent) (lambda (e) (push e events)))
        (eval (evo.swarm::report-tool-form))
        (execute-tool (find-tool "report") '(:done "x" :evidence "y"))
        (let ((event (find :report events :key (lambda (e) (getf e :type)))))
          (check "report tool: emits a :report event with its fields"
                 (and event (equal "x" (getf event :done)) (equal "y" (getf event :evidence))))
          (check "report tool: no goal, none in the event" (null (getf event :goal))))
        ;; goal "complete" closes the lane's goal in the same run.
        (evo.kernel:create-goal-entry agent "deliver x")
        (setf events nil)
        (execute-tool (find-tool "report") '(:done "x" :goal "active"))
        (check "report tool: goal active leaves the goal active, and says so"
               (and (eq :active (getf (evo:current-goal agent) :status))
                    (equal "active" (getf (find :report events :key (lambda (e) (getf e :type))) :goal))))
        (setf events nil)
        (let ((result (execute-tool (find-tool "report") '(:done "all of x" :goal "complete"))))
          (check "report tool: goal complete closes the goal"
                 (eq :complete (getf (evo:current-goal agent) :status)))
          (check "report tool: ...the event carries it"
                 (equal "complete" (getf (find :report events :key (lambda (e) (getf e :type)))
                                         :goal)))
          (check "report tool: ...and the lane is told"
                 (search "goal is complete" result)))
        (check "report tool: a completed goal no longer re-steers the lane"
               (null (evo.kernel::goal-settled-hook agent :stop)))
        (let ((result (execute-tool (find-tool "report") '(:done "again" :goal "complete"))))
          (check "report tool: completing twice is a no-op, not an error"
                 (search "already complete" result)))
        (evo.kernel:create-goal-entry agent "paused one")
        (evo.kernel:update-goal-entry agent (evo:current-goal agent) :status :paused)
        (setf events nil)
        (let ((result (execute-tool (find-tool "report") '(:done "y" :goal "complete"))))
          (check "report tool: a goal that cannot close still delivers the report"
                 (and (find :report events :key (lambda (e) (getf e :type)))
                      (eq :paused (getf (evo:current-goal agent) :status))
                      (search "not closed" result))))))))

(defun test-in-lanes ()
  (with-registries ()
    (register-provider* :stub :base-url "http://127.0.0.1:1" :api-key-env "STUB_KEY")
    (register-model* "m-a" :provider :stub :context-window 1000 :max-output 100)
    (set-setting :model "m-a")
    (let* ((project (uiop:ensure-directory-pathname
                     (format nil "~a/evo-swarm-project-~a/" (tmp-dir) (gen-id))))
           (global (merge-pathnames "swarm.lisp" (evo-home)))
           (local (merge-pathnames ".evo/swarm.lisp" project))
           (evo.swarm::*lane-forms* nil)
           (evo.swarm::*lane-tools* nil)
           (evo.swarm::*coordinator-tools* nil)
           (evo.swarm::*worker-note* evo.swarm::*worker-note*)
           (evo.swarm::*coordinator-note* evo.swarm::*coordinator-note*)
           (agent (fresh-agent))
           (evo:*agent* agent)
           (*swarm* (test-swarm :agent agent :workers 4))
           (lane (third (swarm-lanes *swarm*))))
      (ensure-directories-exist global)
      (ensure-directories-exist local)
      (write-file-string global "(evo.swarm:in-lanes (lane) (list :global lane))")
      (write-file-string local "(evo:set-setting :swarm-workers 4)
(evo.swarm:in-lanes (n total)
  (defvar *in-lanes-probe* t)
  (list n total (namestring *load-truename*)))
(evo.swarm:in-lanes () (evo:set-setting :thinking :low))")
      (unwind-protect
           (progn
             (evo.swarm::load-swarm-config project)
             (let* ((code (lane-code-forms* lane))
                    (forms (read-all (evo.swarm::forms->code (baseline-forms lane *swarm*))))
                    (heads (mapcar (lambda (f) (and (consp f) (car f))) forms))
                    (first-code (position (first code) forms :test #'equal)))
               (check "in-lanes: nothing runs in the coordinator"
                      (not (boundp (find-symbol "*IN-LANES-PROBE*" :evo.user))))
               (check "in-lanes: ~/.evo/swarm.lisp's forms first, then the project's, in order"
                      (and (= 4 (length code))
                           (search "(list :global lane)" (evo.swarm::forms->code (list (first code))))
                           (search "(set-setting :thinking :low)"
                                   (evo.swarm::forms->code (last code)))))
               (check "in-lanes: the named variables are the lane's number and the lane count"
                      (equal (list :global 3) (eval (first code))))
               (check "in-lanes: *load-truename* is the swarm.lisp the form came from"
                      (equal (list 3 4 (namestring (truename local))) (eval (third code))))
               (check "in-lanes: () binds no lane variable"
                      (let ((bindings (second (fourth code))))
                        (equal '(*load-truename* *load-pathname*) (mapcar #'first bindings))))
               (check "in-lanes: after the coordinator's providers and defaults"
                      (and first-code
                           (< (position 'evo:register-provider heads) first-code)
                           (< (position '(evo:set-setting :model "m-a") forms :test #'equal)
                              first-code)))
               (check "in-lanes: before the coordinator's models (filled in if missing) and the report tool"
                      (let ((last-code (position (car (last code)) forms :test #'equal)))
                        (and last-code
                             (< last-code (position (first (fill-in-forms forms)) forms))
                             (< last-code (position 'evo:register-tool heads)))))
               (check "in-lanes: the forms read back in a lane"
                      (= (length forms) (length (baseline-forms lane *swarm*)))))
             (evo.swarm::load-swarm-config project)
             (check "in-lanes: a /reload starts from scratch, not on top"
                    (= 3 (length evo.swarm::*lane-forms*)))
             (check "in-lanes: swarm.lisp's settings apply"
                    (eql 4 (setting :swarm-workers)))
             (check "in-lanes: a keyword is not a variable name"
                    (handler-case (progn (macroexpand '(in-lanes (:lane) (foo))) nil)
                      (error () t))))
        (delete-file global)))))

(defun lane-code-forms* (lane)
  (evo.swarm::lane-code-forms lane *swarm*))

(defun fill-in-forms (forms)
  "The baseline's coordinator-model forms: (when (and ...) (register-model ...))."
  (remove-if-not (lambda (f)
                   (and (consp f) (eq (car f) 'when)
                        (consp (third f)) (eq (car (third f)) 'evo:register-model)))
                 forms))

(defun test-model-fill-in ()
  "A coordinator model reaches a lane only if the lane has its API and in-lanes
did not register it; a lane whose default model is missing says why."
  (with-registries ()
    (register-provider* :stub :base-url "http://127.0.0.1:1" :api-key-env "STUB_KEY")
    (register-api :unit-ext-api (make-instance 'provider-api))
    (register-model* "m-a" :provider :stub :context-window 1000 :max-output 100)
    (register-model* "m-ext" :provider :stub :api :unit-ext-api
                             :context-window 1000 :max-output 100)
    (let* ((agent (fresh-agent))
           (evo:*agent* agent)
           (*swarm* (test-swarm :agent agent))
           (lane (first (swarm-lanes *swarm*)))
           (fills (fill-in-forms (baseline-forms lane *swarm*)))
           (check-form (evo.swarm::lane-model-check-form lane)))
      (check "fill-in: one form per coordinator model" (= 2 (length fills)))
      ;; A lane: no user models, and no extension API.
      (let ((evo.provider::*apis* (remove :unit-ext-api evo.provider::*apis* :key #'car)))
        (reset-user-registries)
        (register-provider* :stub :base-url "http://127.0.0.1:1" :api-key-env "STUB_KEY")
        (check "fill-in: a model whose API the lane lacks is skipped, not an error"
               (handler-case (progn (mapc #'eval fills) t) (error () nil)))
        (check "fill-in: ...and the lane has the others"
               (and (ignore-errors (find-model "m-a" :stub))
                    (not (ignore-errors (find-model "m-ext" :stub)))))
        (set-setting :model "m-a")
        (check "model check: a default model the lane has passes"
               (handler-case (progn (eval check-form) t) (error () nil)))
        (set-setting :model "m-ext")
        (let ((message (handler-case (progn (eval check-form) nil)
                         (error (e) (princ-to-string e)))))
          (check "model check: a default model on a missing API is an error that says why"
                 (and message
                      (search "lane 1 cannot use its model m-ext" message)
                      (search ":UNIT-EXT-API" message)
                      (search "in-lanes" message))))
        ;; in-lanes registered m-a its own way: the fill-in leaves it alone.
        (register-model* "m-a" :provider :stub :context-window 5 :max-output 5)
        (mapc #'eval fills)
        (check "fill-in: a model in-lanes registered is kept as it is"
               (eql 5 (getf (find-model "m-a" :stub) :context-window)))))))

(defun test-tool-limits ()
  (let ((evo.swarm::*lane-tools* nil)
        (*swarm* (test-swarm)))
    (destructuring-bind (one two three) (swarm-lanes *swarm*)
      (check "limits: none by default" (null (evo.swarm::lane-tool-limit one)))
      (set-lane-tools '("read" "bash"))
      (check "limits: every lane, and report is always kept"
             (and (equal '("report" "read" "bash") (evo.swarm::lane-tool-limit two))))
      (set-lane-tools '("read") :lanes '(3))
      (check "limits: a lane's own entry wins"
             (equal '("report" "read") (evo.swarm::lane-tool-limit three)))
      (check "limits: ...and the others keep the swarm-wide one"
             (equal '("report" "read" "bash") (evo.swarm::lane-tool-limit one)))
      (let* ((agent (fresh-agent)) (evo:*agent* agent))
        (declare (ignorable agent))
        (check "limits: the baseline applies the limit"
               (equal '(evo:set-active-tools evo:*agent* (quote ("report" "read")))
                      (find 'evo:set-active-tools
                            (read-all (evo.swarm::forms->code (baseline-forms three *swarm*)))
                            :key (lambda (f) (and (consp f) (car f))))))))))

(defun test-notes ()
  (let* ((*swarm* (test-swarm :workers 5))
         (lane (fourth (swarm-lanes *swarm*))))
    (check "notes: a lane knows its number and the lane count"
           (search "You are lane 4 of 5" (worker-note lane 5)))
    (check "notes: a shared-directory lane is told to keep to its files"
           (search "share the working directory" (worker-note lane 5)))
    (setf (evo.swarm::lane-worktree lane) "/w/lane-4/" (evo.swarm::lane-branch lane) "b4")
    (check "notes: a worktree lane is told where it works and on what branch"
           (search "own git worktree /w/lane-4/, on branch b4" (worker-note lane 5)))
    (check "notes: the coordinator knows the lane count"
           (search "5 worker lanes" (coordinator-note 5)))
    (let ((saved evo.swarm::*worker-note*))
      (unwind-protect
           (progn (set-worker-note "lane ~d/~d/~d ~a")
                  (check "notes: swarm.lisp can replace the worker note"
                         (string-prefix-p "lane 4/4/5 " (worker-note lane 5))))
        (setf evo.swarm::*worker-note* saved)))
    (let ((saved evo.swarm::*coordinator-note*))
      (unwind-protect
           (progn (set-coordinator-note "Lead ~d lanes.")
                  (check "notes: a replaced coordinator note gets the live lane count"
                         (equal "Lead 5 lanes." (coordinator-note))))
        (setf evo.swarm::*coordinator-note* saved)))))

(defun test-coordinator-tools ()
  (let* ((agent (fresh-agent))
         (evo.swarm::*coordinator-tools* nil)
         (evo.swarm::*applied-coordinator-tools* nil))
    (flet ((changes ()
             (count :tools-change (evo.journal:journal-entries (agent-journal agent))
                    :key (lambda (e) (getf e :type)))))
      (evo.swarm::apply-coordinator-tools agent)
      (check "coordinator tools: no limit, nothing journaled" (= 0 (changes)))
      (set-coordinator-tools '("read" "lanes"))
      (evo.swarm::apply-coordinator-tools agent)
      (check "coordinator tools: a limit is journaled" (= 1 (changes)))
      (evo.swarm::apply-coordinator-tools agent)
      (check "coordinator tools: an unchanged limit is not journaled again (/reload)"
             (= 1 (changes)))
      (evo.swarm::apply-coordinator-tools agent :new-session t)
      (check "coordinator tools: a new session gets the limit" (= 2 (changes)))
      (set-coordinator-tools nil)
      (evo.swarm::apply-coordinator-tools agent)
      (check "coordinator tools: lifting the limit restores every tool" (= 3 (changes))))))

(defun test-goal-hold ()
  (let* ((agent (fresh-agent))
         (other (fresh-agent))
         (*swarm* (test-swarm :agent agent))
         (lane (first (swarm-lanes *swarm*)))
         (evo.kernel:*goal-hold-predicates* '(evo.swarm::hold-goal-while-lanes-work)))
    (dolist (l (swarm-lanes *swarm*)) (setf (lane-state l) :idle))
    (create-goal-entry agent "ship it")
    (check "goal hold: every lane idle, the goal nudges as usual"
           (evo.kernel::goal-settled-hook agent :stop))
    (evo.kernel::drain-steering agent)
    (dolist (state '(:working :compacting))
      (setf (lane-state lane) state)
      (check (format nil "goal hold: a ~(~a~) lane holds the coordinator's goal" state)
             (and (null (evo.kernel::goal-settled-hook agent :stop))
                  (not (evo.kernel:steering-pending-p agent))
                  (eq :active (getf (current-goal agent) :status)))))
    (setf (lane-state lane) :working)
    (create-goal-entry other "not the coordinator")
    (check "goal hold: only the coordinator's goal is held"
           (evo.kernel::goal-settled-hook other :stop))
    (dolist (state '(:starting :down :stopped))
      (setf (lane-state lane) state)
      (check (format nil "goal hold: a ~(~a~) lane holds nothing" state)
             (evo.kernel::goal-settled-hook agent :stop))
      (evo.kernel::drain-steering agent))
    (let ((*swarm* nil))
      (check "goal hold: no swarm, no hold"
             (null (evo.swarm::hold-goal-while-lanes-work agent nil))))))

(defun test-lane-input ()
  "What a lane's own items tell the coordinator: a report tool call and a
finished run, each with the :origin plist CONTRACT §3 defines — no prose is
parsed anywhere."
  (let* ((agent (fresh-agent))
         (evo.kernel:*frontend* nil)
         (recording (make-instance 'recording-view))
         (*swarm* (test-swarm :agent agent :view recording))
         (lane (first (swarm-lanes *swarm*))))
    (evo.swarm::mirror-note-item (evo.swarm::lane-mirror lane)
                                 '(:id "t1" :kind "tool" :name "report"
                                   :args (:done "built it" :evidence "make ok"
                                                :goal "active")))
    (check "input: a report becomes coordinator input"
           (let ((queued (evo.kernel::agent-steering agent)))
             (and queued (search "[lane 1 report] done: built it"
                                 (getf (first queued) :text))
                  (search "evidence: make ok" (getf (first queued) :text)))))
    (check "input: ...with a :lane-report origin, never parsed prose"
           (equal '(:kind :lane-report :lane 1
                    :done "built it" :evidence "make ok" :next nil
                    :blocked nil :requests nil :goal :active)
                  (getf (first (evo.kernel::agent-steering agent)) :origin)))
    (check "input: ...and the lane's report count goes up"
           (= 1 (evo.swarm::lane-reports lane)))
    (check "input: the report is shown through the swarm's view, not the TUI"
           (let ((said (slot-value recording 'said)))
             (and said (search "[lane 1 report] done: built it" (caar said))
                  (eq :notice (second (first said))))))
    (evo.kernel::drain-steering agent)
    ;; The same item arriving again (a patch, a snapshot) is not news twice.
    (evo.swarm::mirror-note-item (evo.swarm::lane-mirror lane)
                       '(:id "t1" :kind "tool" :name "report" :args (:done "built it")))
    (check "input: a re-patched report is not reported twice"
           (and (not (steering-pending-p agent)) (= 1 (evo.swarm::lane-reports lane))))
    ;; A finished run.
    (evo.swarm::mirror-state-set (evo.swarm::lane-mirror lane) '(:status "idle" :goal (:status "complete")))
    (evo.swarm::mirror-note-item (evo.swarm::lane-mirror lane)
                       '(:id "r1" :kind "run_outcome" :outcome "stop"))
    (check "input: a finished run says which goal status the lane settled with"
           (search "[lane 1] run ended (stop) — goal: complete"
                   (getf (first (evo.kernel::agent-steering agent)) :text)))
    (check "input: ...and carries it as a lane-event origin"
           (equal '(:kind :lane-event :lane 1 :event :run-ended :outcome "stop"
                    :goal-status :complete :severity :info)
                  (getf (first (evo.kernel::agent-steering agent)) :origin)))
    (evo.kernel::drain-steering agent)
    (evo.swarm::mirror-state-set (evo.swarm::lane-mirror lane) '(:status "idle" :goal (:status "active")))
    (evo.swarm::mirror-note-item (evo.swarm::lane-mirror lane) '(:id "r2" :kind "run_outcome" :outcome "aborted"))
    (check "input: a stopped lane with an active goal waits to be steered"
           (search "goal: active, but the lane is idle until steered"
                   (getf (first (evo.kernel::agent-steering agent)) :text)))
    (evo.kernel::drain-steering agent)
    ;; Nothing else is news.
    (evo.swarm::mirror-note-item (evo.swarm::lane-mirror lane) '(:id "a1" :kind "assistant" :text "hi"))
    (check "input: streamed text is not coordinator input"
           (not (steering-pending-p agent)))
    (check "input: a run that simply stopped says no goal it never had"
           (progn (evo.swarm::mirror-state-set (evo.swarm::lane-mirror lane) '(:status "idle"))
                  (evo.swarm::mirror-note-item (evo.swarm::lane-mirror lane)
                                     '(:id "r3" :kind "run_outcome" :outcome "stop"))
                  (let ((text (getf (first (evo.kernel::agent-steering agent)) :text)))
                    (and (search "[lane 1] run ended (stop)" text)
                         (not (search "goal:" text))))))))

;;; The frontend seam (view.lisp).  What the swarm calls to be seen or run: a
;;; notice, a repaint, a machine event, the run itself — each routed to the
;;; swarm's view, and a no-op when no swarm is up.

(defun test-view ()
  (let ((*swarm* nil))
    (check "view: no swarm, no notice" (null (swarm-say "x")))
    (check "view: no swarm, no repaint" (null (swarm-repaint))))
  (let* ((agent (fresh-agent))
         (recording (make-instance 'recording-view))
         (*swarm* (test-swarm :agent agent :view recording)))
    (check "view: the swarm carries its view" (eq recording (swarm-view *swarm*)))
    (swarm-say "hello" :style :error)
    (swarm-say "quiet")
    (check "view: a notice reaches the view, with its style (and :dim by default)"
           (equal '(("hello" :error) ("quiet" :dim)) (said recording)))
    (swarm-repaint)
    (swarm-repaint)
    (check "view: a repaint reaches the view" (= 2 (slot-value recording 'repaints)))
    (check "view: the run goes to the view, and its exit code comes back"
           (and (eql 7 (swarm-run agent t))
                (equal (list agent t) (slot-value recording 'ran)))))
  ;; The TUI view with no terminal up: nothing painted, nothing published, and
  ;; — the point of the seam — nothing signalled.
  (let ((view (make-instance 'tui-view)))
    (check "view: a TUI notice is dropped when no TUI is running"
           (null (view-say view "x")))
    (check "view: a TUI repaint is dropped when no TUI is running"
           (null (view-repaint view)))))

(defun test-serve-view ()
  "A serve view says through the command layer's host protocol, and running
the coordinator is serving it: no client of the swarm ever names the
server's internals."
  (let* ((server (evo.serve:make-server :port 0 :token "t"))
         (view (make-instance 'serve-view :server server)))
    (check "serve view: it names its server" (eq server (serve-view-server view)))
    (check "serve view: a notice goes to the command layer's host protocol"
           (handler-case (progn (view-say view "lane 2 is working" :style :notice) t)
             (error () nil)))
    (check "serve view: a repaint is nothing to do" (null (view-repaint view)))))

(defun test-record ()
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 2)))
    (destructuring-bind (one two) (swarm-lanes *swarm*)
      (declare (ignore one))
      (setf (evo.swarm::lane-worktree two) "/wt/lane-2/"
            (evo.swarm::lane-branch two) "swarm/unit/lane-2"
            (evo.swarm::lane-cwd two) #p"/wt/lane-2/"
            (evo.swarm::lane-task two) "the task"
            (evo.swarm::lane-extra-forms two) (list "(defun x ())")))
    (setf (evo.swarm::swarm-lane-model *swarm*) "lane-model"
          (evo.swarm::swarm-lane-provider *swarm*) :stub
          (evo.swarm::swarm-lane-thinking *swarm*) :high)
    (evo.swarm::record-swarm)
    (let ((record (evo:custom-state "swarm" agent)))
      (check "record: the swarm is journaled on the coordinator's session"
             (and record (equal "unit" (getf record :id)) (= 2 (getf record :workers))))
      (check "record: the lanes' configuration is swarm data, not a user file"
             (and (equal "lane-model" (getf record :lane-model))
                  (eq :stub (getf record :lane-provider))
                  (eq :high (getf record :lane-thinking))))
      (let ((restored (evo.swarm::make-swarm :agent agent :workers 9 :record record
                                             :evo-binary "/x/evo")))
        (check "record: a resumed swarm keeps its id, directory and lane count"
               (and (equal "unit" (swarm-id restored))
                    (equal (namestring (swarm-dir *swarm*)) (namestring (swarm-dir restored)))
                    (= 2 (length (swarm-lanes restored)))))
        (check "record: ...and the lanes' model configuration"
               (and (equal "lane-model" (evo.swarm::swarm-lane-model restored))
                    (eq :stub (evo.swarm::swarm-lane-provider restored))
                    (eq :high (evo.swarm::swarm-lane-thinking restored))))
        (let ((two (second (swarm-lanes restored))))
          (check "record: a lane's worktree, branch, cwd, task and evals come back"
                 (and (equal "/wt/lane-2/" (lane-worktree two))
                      (equal "/wt/lane-2/" (namestring (lane-cwd two)))
                      (equal "the task" (lane-task two))
                      (equal '("(defun x ())") (evo.swarm::lane-extra-forms two))))
          (check "record: a resumed lane has no ready file yet, and a mirror of its own"
                 (and (null (evo.swarm::lane-ready two))
                      (typep (evo.swarm::lane-mirror two) 'evo.swarm::mirror)
                      (equal "lane:2"
                             (evo.swarm::mirror-topic (evo.swarm::lane-mirror two))))))))))

(defun test-session-switch ()
  "/resume, /new and /fork on the coordinator: a resumed session recording
another swarm gets that swarm's lanes back; any other keeps the running one."
  (let* ((agent (fresh-agent))
         (view (make-instance 'recording-view))
         (*swarm* (test-swarm :agent agent :workers 2 :view view))
         (current *swarm*)
         (saved-start (symbol-function 'evo.swarm::start-lanes))
         (saved-stop (symbol-function 'evo.swarm::stop-swarm))
         (started nil) (stopped nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'evo.swarm::start-lanes)
                 (lambda (swarm &key resume) (push (list swarm resume) started))
                 (symbol-function 'evo.swarm::stop-swarm)
                 (lambda (&optional (swarm *swarm*)) (push swarm stopped)))
           ;; A session with no swarm (/new): the running lanes come along.
           (evo.swarm::adopt-session-swarm agent)
           (check "switch: a session with no swarm keeps the running lanes"
                  (and (eq current *swarm*) (null started) (null stopped)))
           (check "switch: ... and records them"
                  (equal "unit" (getf (evo:custom-state "swarm" agent) :id)))
           ;; The same swarm recorded (/fork, or this very session): kept.
           (evo.swarm::adopt-session-swarm agent)
           (check "switch: a session recording this swarm keeps it"
                  (and (eq current *swarm*) (null started) (null stopped)))
           ;; Another swarm recorded (/resume of an older session).
           (let ((dir (format nil "~a/evo-swarm-unit-~a/" (tmp-dir) (gen-id))))
             (evo:set-custom-state
              "swarm" (list :id "older" :dir dir :workers 3
                            :lanes (vector (list :n 1 :cwd (namestring (uiop:getcwd))
                                                 :task "old task" :extra-forms #())
                                           (list :n 2 :cwd (namestring (uiop:getcwd))
                                                 :extra-forms #())
                                           (list :n 3 :cwd (namestring (uiop:getcwd))
                                                 :extra-forms #())))
              agent)
             (with-published (ops)
               (evo.swarm::adopt-session-swarm agent)
               (check "switch: every topic is reset for the clients (§7.3)"
                      (let ((resets (remove-if-not (lambda (op)
                                                     (equal "topic.reset" (getf op :op)))
                                                   ops)))
                        (and (equal '("swarm" "lane:1" "lane:2")
                                    (mapcar (lambda (op) (getf op :topic)) resets))
                             (every (lambda (op)
                                      (equal "swarm_switched" (getf op :reason)))
                                    resets)))))
             (check "switch: the running lanes stop"
                    (equal (list current) stopped))
             (check "switch: the recorded swarm replaces them"
                    (and (not (eq current *swarm*))
                         (equal "older" (swarm-id *swarm*))
                         (equal dir (namestring (swarm-dir *swarm*)))
                         (= 3 (length (swarm-lanes *swarm*)))
                         (equal "old task" (lane-task (first (swarm-lanes *swarm*))))))
             (check "switch: its lanes come up resuming their sessions"
                    (equal (list (list *swarm* t)) started))
             (check "switch: the new swarm keeps the frontend and agent"
                    (and (eq view (evo.swarm::swarm-view *swarm*))
                         (eq agent (evo.swarm::swarm-agent *swarm*))))
             (check "switch: the session still records the swarm it has"
                    (equal "older" (getf (evo:custom-state "swarm" agent) :id)))))
      (setf (symbol-function 'evo.swarm::start-lanes) saved-start
            (symbol-function 'evo.swarm::stop-swarm) saved-stop)))
  ;; A lane stopped before it launched is not launched after.
  (let* ((*swarm* (test-swarm :workers 1))
         (lane (first (swarm-lanes *swarm*))))
    (setf (evo.swarm::lane-dir lane)
          (uiop:ensure-directory-pathname
           (format nil "~a/evo-swarm-unit-~a/" (tmp-dir) (gen-id)))
          (evo.swarm::lane-stopping lane) t)
    (check "switch: a stopped lane does not launch"
           (and (null (evo.swarm::launch-lane lane))
                (null (evo.swarm::lane-process lane))))
    (check "switch: ... nor come up"
           (null (evo.swarm::bring-up-lane lane)))))

(defun test-launch-environment ()
  "A lane is launched into the coordinator's session, not into the
coordinator's supervision: none of these variables may reach a lane — planted
here with a value no lane may inherit — while the lane's own token, session
directory and watched pid must.  The list is written out rather than read from
the strip list, so a name dropped from that list fails here."
  (let* ((forbidden '("EVO_SUPERVISED_CHILD" "EVO_HEARTBEAT_FILE" "EVO_NO_SUPERVISOR"
                      "EVO_SERVE_TOKEN" "EVO_SESSIONS_DIR" "EVO_SERVE_WATCH_PID"
                      "EVO_SUPERVISOR_STATE_DIR" "EVO_SUPERVISOR_PID"
                      "EVO_SUPERVISOR_RESTARTS" "EVO_RECOVERY"))
         (saved (mapcar (lambda (name) (cons name (getenv name))) forbidden))
         (stale "swarm-unit-stale"))
    (unwind-protect
         (let* ((*swarm* (test-swarm))
                (lane (first (swarm-lanes *swarm*))))
           (check "launch: the strip list names every variable a lane must not inherit"
                  (every (lambda (name)
                           (member (concatenate 'string name "=")
                                   evo.swarm::*stripped-environment* :test #'equal))
                         forbidden))
           (dolist (name forbidden)
             (evo.port:setenv name stale))
           (setf (evo.swarm::lane-dir lane) #p"/tmp/lane-1/")
           (let ((env (evo.swarm::lane-environment lane)))
             (check "launch: not one of the coordinator's variables is inherited"
                    (null (intersection
                           env
                           (mapcar (lambda (name) (concatenate 'string name "=" stale))
                                   forbidden)
                           :test #'equal)))
             (check "launch: the lane's own sessions directory is set"
                    (member "EVO_SESSIONS_DIR=/tmp/lane-1/sessions/" env :test #'equal))))
      (dolist (pair saved)
        (evo.port:setenv (car pair) (or (cdr pair) ""))))))

(defun test-sessions-dir ()
  (let ((saved (getenv "EVO_SESSIONS_DIR")))
    (unwind-protect
         (progn
           (evo.port:setenv "EVO_SESSIONS_DIR" "/tmp/elsewhere/lane-3/sessions")
           (check "sessions: EVO_SESSIONS_DIR puts a process's sessions elsewhere"
                  (equal "/tmp/elsewhere/lane-3/sessions/"
                         (namestring (sessions-directory))))
           (evo.port:setenv "EVO_SESSIONS_DIR" "")
           (check "sessions: unset, they live under the cwd's directory as ever"
                  (search "/sessions/" (namestring (sessions-directory)))))
      (evo.port:setenv "EVO_SESSIONS_DIR" (or saved "")))))

(defun test-cli ()
  (check "cli: --workers"
         (eql 3 (getf (evo.swarm::parse-args '("--workers" "3")) :workers)))
  (check "cli: a bad worker count is a usage error"
         (handler-case (progn (evo.swarm::parse-args '("--workers" "0")) nil)
           (evo.cli:usage-error () t)))
  (check "cli: an unknown flag is a usage error"
         (handler-case (progn (evo.swarm::parse-args '("--wat")) nil)
           (evo.cli:usage-error () t)))
  (check "cli: --lane-model splits id@provider"
         (let ((opts (evo.swarm::parse-args '("--lane-model" "stub-a@stub"))))
           (and (equal "stub-a" (getf opts :lane-model))
                (eq :stub (getf opts :lane-provider)))))
  (check "cli: a bare --lane-model has no provider"
         (let ((opts (evo.swarm::parse-args '("--lane-model" "stub-a"))))
           (and (equal "stub-a" (getf opts :lane-model))
                (null (getf opts :lane-provider)))))
  (check "cli: --lane-thinking is an effort level"
         (eq :high (getf (evo.swarm::parse-args '("--lane-thinking" "high")) :lane-thinking)))
  (check "cli: a bad --lane-thinking is a usage error"
         (handler-case (progn (evo.swarm::parse-args '("--lane-thinking" "wat")) nil)
           (evo.cli:usage-error () t)))
  (check "cli: a restart keeps the swarm's flags and the lane configuration"
         (equal '("--workers" "4" "--evo" "/x/evo" "--no-userspace"
                  "--lane-model" "stub-a" "--lane-thinking" "medium")
                (evo.swarm::restart-argv '("--workers" "4" "--model" "m" "--evo" "/x/evo"
                                          "--thinking" "high" "--no-userspace"
                                          "--lane-model" "stub-a" "--lane-thinking" "medium"
                                          "--resume" "/old/path")))))

(defun test-serve-cli ()
  "`evo-swarm serve`: serve's flags alongside the swarm's, the loopback guard,
and what a restarted coordinator keeps."
  (check "serve: the subcommand is recognised"
         (getf (evo.swarm::parse-args '("serve")) :serve))
  (check "serve: the port defaults to serve's own"
         (eql evo.cli:*serve-default-port*
              (getf (evo.swarm::parse-args '("serve")) :port)))
  (check "serve: serve's flags and the swarm's parse together"
         (let ((opts (evo.swarm::parse-args
                      '("serve" "--host" "0.0.0.0" "--port" "9000"
                        "--ready-file" "/tmp/ready.json" "--watch-stdin" "--allow-remote"
                        "--workers" "2" "--evo" "/x/evo" "--no-userspace"))))
           (and (getf opts :serve)
                (equal "0.0.0.0" (getf opts :host))
                (eql 9000 (getf opts :port))
                (equal "/tmp/ready.json" (getf opts :ready-file))
                (getf opts :watch-stdin)
                (getf opts :allow-remote)
                (eql 2 (getf opts :workers))
                (equal "/x/evo" (getf opts :evo))
                (getf opts :no-userspace))))
  (check "serve: a serve flag without the subcommand is unknown"
         (handler-case (progn (evo.swarm::parse-args '("--host" "0.0.0.0")) nil)
           (evo.cli:usage-error () t)))
  (check "serve: a port outside 0..65535 is an error"
         (handler-case (progn (evo.swarm::parse-args '("serve" "--port" "70000")) nil)
           (error () t)))
  (check "serve: a bad flag is a usage error, not a crash"
         (handler-case (progn (evo.swarm::parse-args '("serve" "--wat")) nil)
           (evo.cli:usage-error () t)))
  (check "serve: loopback needs no --allow-remote"
         (equal "127.0.0.1" (evo.swarm::check-serve-host "127.0.0.1" '(:serve t))))
  (check "serve: another address is refused without --allow-remote"
         (handler-case (progn (evo.swarm::check-serve-host "0.0.0.0" '(:serve t)) nil)
           (evo.cli:usage-error () t)))
  (check "serve: ...and taken with it"
         (equal "0.0.0.0" (evo.swarm::check-serve-host "0.0.0.0" '(:serve t :allow-remote t))))
  (let ((argv (evo.swarm::restart-argv
               '("serve" "--workers" "4" "--model" "m" "--evo" "/x/evo"
                 "--thinking" "high" "--no-userspace" "--resume" "/old"
                 "--host" "127.0.0.1" "--port" "9000" "--ready-file" "/tmp/ready.json"
                 "--watch-stdin" "--allow-remote"))))
    (check "serve: a restarted coordinator keeps the swarm's own flags"
           (and (equal "serve" (first argv))
                (member "--workers" argv :test #'equal)
                (member "--evo" argv :test #'equal)
                (member "--no-userspace" argv :test #'equal)))
    (check "serve: ...and the door it serves on, from the shared layer"
           (and (member "--ready-file" argv :test #'equal)
                (member "/tmp/ready.json" argv :test #'equal)
                (member "--watch-stdin" argv :test #'equal)
                (member "--port" argv :test #'equal)))
    (check "serve: ...and does not re-pass the session's model or thinking"
           (notany (lambda (a) (member a '("--model" "--thinking") :test #'equal)) argv))
    (check "serve: ...nor a bare --resume"
           (not (member "--resume" argv :test #'equal)))))

(defun test-serve-exit-codes ()
  "`evo-swarm`'s exit code for a command line that cannot start anything: 64,
whatever raised it — a mistyped flag cannot be fixed by trying it again, so
the supervisor must never restart it.  (These argv never get past the
preconditions, so nothing is spawned; EVO_NO_SUPERVISOR keeps even the last
one out of the supervisor.)"
  (let ((saved (getenv "EVO_NO_SUPERVISOR"))
        (saved-token (getenv "EVO_SERVE_TOKEN")))
    (unwind-protect
         (progn
           (evo.port:setenv "EVO_NO_SUPERVISOR" "1")
           (evo.port:setenv "EVO_SERVE_TOKEN" "")
           (flet ((code (&rest argv)
                    ;; The usage text and the complaint are not the point;
                    ;; only what it exits with.
                    (let ((*standard-output* (make-broadcast-stream))
                          (*error-output* (make-broadcast-stream)))
                      (evo.swarm::main argv))))
             (check "exit: --help is 0" (eql 0 (code "--help")))
             (check "exit: --version is 0" (eql 0 (code "--version")))
             (check "exit: an unknown flag is 64" (eql 64 (code "--wat")))
             (check "exit: a serve flag without serve is 64"
                    (eql 64 (code "--host" "0.0.0.0")))
             (check "exit: a bad --port is 64" (eql 64 (code "serve" "--port" "nope")))
             (check "exit: a --ready-file with no path is 64"
                    (eql 64 (code "serve" "--ready-file")))
             (check "exit: a non-loopback --host without --allow-remote is 64"
                    (eql 64 (code "serve" "--ready-file" "/tmp/ready.json"
                                  "--host" "0.0.0.0")))))
      (evo.port:setenv "EVO_NO_SUPERVISOR" (or saved ""))
      (evo.port:setenv "EVO_SERVE_TOKEN" (or saved-token "")))))

;;; The mirror (mirror.lisp): a lane's ops applied to the coordinator's own
;;; copy and republished as topic lane:N, which is how every client reads a
;;; lane (CONTRACT §4.3, §6).

(defun test-mirror ()
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 1))
         (lane (first (swarm-lanes *swarm*)))
         (mirror (evo.swarm::lane-mirror lane)))
    (check "mirror: one per lane, named for its topic"
           (and mirror (equal "lane:1" (evo.swarm::mirror-topic mirror))))
    (let ((ops (with-published (ops)
                 (evo.swarm::mirror-apply mirror '(:op "item.add" :topic "session" :after nil
                                        :item (:id "e1" :kind "user" :ts 1 :text "hi")))
                 (evo.swarm::mirror-apply mirror '(:op "item.add" :topic "session" :after "e1"
                                        :item (:id "e2" :kind "assistant" :ts 2
                                               :text "yo" :status "streaming")))
                 (evo.swarm::mirror-apply mirror '(:op "item.append" :topic "session" :id "e2"
                                        :field "text" :text " there"))
                 (evo.swarm::mirror-apply mirror '(:op "item.patch" :topic "session" :id "e2"
                                        :patch (:status "final" :usage (:input 7))))
                 (evo.swarm::mirror-apply mirror '(:op "item.add" :topic "session" :after nil
                                        :item (:id "e3" :kind "tool" :name "read"))
                               ))))
      (check "mirror: every op is republished under the lane's topic"
             (and (= 5 (length ops))
                  (every (lambda (op) (equal "lane:1" (getf op :topic))) ops)))
      (check "mirror: ...keeping the op's own fields"
             (and (equal "item.append" (getf (third ops) :op))
                  (equal " there" (getf (third ops) :text))
                  (equal "final" (getf (getf (fourth ops) :patch) :status)))))
    (check "mirror: the lane's items read back in order"
           (equal '("e1" "e2" "e3")
                  (mapcar (lambda (i) (getf i :id)) (evo.swarm::mirror-items mirror))))
    (check "mirror: an append concatenates and a patch keeps the rest"
           (let ((item (evo.swarm::mirror-item-ref mirror "e2")))
             (and (equal "yo there" (getf item :text))
                  (equal "final" (getf item :status))
                  (eql 2 (getf item :ts))
                  (eql 7 (getf (getf item :usage) :input)))))
    (check "mirror: an item id that arrives twice is replaced, not duplicated"
           (progn (evo.swarm::mirror-apply mirror '(:op "item.add" :topic "session" :after nil
                                         :item (:id "e3" :kind "tool" :name "read"
                                                :status "ok")))
                  (and (= 3 (length (evo.swarm::mirror-items mirror)))
                       (equal "ok" (getf (evo.swarm::mirror-item-ref mirror "e3") :status)))))
    (check "mirror: an unknown op is ignored, not an error"
           (null (evo.swarm::mirror-apply mirror '(:op "something.new" :topic "session"))))
    ;; The topic provider (§7): what serve serves from.
    (let ((snapshot (evo.serve:topic-snapshot mirror :items 200)))
      (check "mirror: its snapshot is the lane's own state and items"
             (and (= 3 (length (getf snapshot :items)))
                  (null (getf snapshot :has-more)))))
    (multiple-value-bind (items more) (evo.serve:topic-items-before mirror "e3" 10)
      (check "mirror: paging backwards stays in the window"
             (and (equal '("e1" "e2") (mapcar (lambda (i) (getf i :id)) items))
                  (null more))))
    (check "mirror: one item whole" (equal "e1" (getf (evo.serve:topic-item mirror "e1") :id)))
    ;; The lane's own status is the swarm's record of it.
    (evo.swarm::mirror-apply mirror '(:op "state.patch" :topic "session"
                           :patch (:status "running" :model (:id "stub-a"))))
    (check "mirror: a state patch lands on its topic state"
           (equal "running" (getf (evo.swarm::mirror-lane-state mirror) :status)))
    (check "mirror: ...and the lane shows working"
           (eq :working (lane-state lane)))
    (evo.swarm::mirror-apply mirror '(:op "state.patch" :topic "session" :patch (:status "idle")))
    (check "mirror: ...and idle again when the lane says so"
           (eq :idle (lane-state lane)))
    ;; The window, and what has_more means.
    (let ((evo.swarm::*mirror-items* 3))
      (dotimes (i 5)
        (evo.swarm::mirror-apply mirror (list :op "item.add" :topic "session" :after nil
                                   :item (list :id (format nil "x~d" i) :kind "user"))))
      (check "mirror: only the newest *evo.swarm::mirror-items* are kept"
             (equal '("x2" "x3" "x4")
                    (mapcar (lambda (i) (getf i :id)) (evo.swarm::mirror-items mirror))))
      (check "mirror: has_more says the rest are below"
             (getf (evo.serve:topic-snapshot mirror :items 200) :has-more)))))

(defun test-mirror-rebuild ()
  "A lane's process restarting, or its journal moving, is a topic.reset: its
items are not ours to patch (CONTRACT §5.3, §6)."
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 1))
         (lane (first (swarm-lanes *swarm*)))
         (mirror (evo.swarm::lane-mirror lane)))
    (with-lane-snapshot ((list :status "idle")
                         :items (list (list :id "e1" :kind "user" :ts 1 :text "hello")))
      (let ((ops (with-published (ops) (evo.swarm::mirror-rebuild mirror "lane_restarted"))))
        (check "rebuild: the lane's items are taken whole from its snapshot"
               (equal '("e1") (mapcar (lambda (i) (getf i :id)) (evo.swarm::mirror-items mirror))))
        (check "rebuild: ...and every client is told to re-snapshot"
               (equal '("topic.reset" "lane:1" "lane_restarted")
                      (list (getf (first ops) :op) (getf (first ops) :topic)
                            (getf (first ops) :reason)))))
      (check "rebuild: the cursor is the snapshot's, not the stream's"
             (equal "e1.10" (evo.swarm::mirror-cursor mirror))))
    ;; A stream.reset from the lane is the same thing.
    (with-lane-snapshot ((list :status "running")
                         :items (list (list :id "n1" :kind "assistant" :ts 2 :text "fresh")))
      (let ((ops (with-published (ops)
                   (evo.swarm::mirror-apply mirror '(:op "stream.reset" :topic "session"
                                          :reason "cursor_too_old")))))
        (check "rebuild: a stream.reset is a rebuild with a client-visible reset"
               (and (equal "topic.reset" (getf (first ops) :op))
                    (equal "lane_restarted" (getf (first ops) :reason))
                    (equal '("n1") (mapcar (lambda (i) (getf i :id)) (evo.swarm::mirror-items mirror)))))))))

(defun test-topics ()
  "The swarm is one observable thing (CONTRACT §4.3): its topic carries the
whole state, and every lane transition publishes it."
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 2))
         (lane (first (swarm-lanes *swarm*))))
    (setf (evo.swarm::swarm-lane-model *swarm*) "stub-a"
          (evo.swarm::swarm-lane-provider *swarm*) :stub
          (evo.swarm::swarm-lane-thinking *swarm*) :high)
    (let ((ops (with-published (ops) (evo.swarm::publish-swarm-state *swarm* t))))
      (check "topics: the swarm topic is one state.patch"
             (and (= 1 (length ops))
                  (equal "state.patch" (getf (first ops) :op))
                  (equal "swarm" (getf (first ops) :topic))))
      (let ((state (getf (first ops) :patch)))
        (check "topics: it names the swarm, its workers and its lanes"
               (and (equal "unit" (getf state :id))
                    (eql 2 (getf state :workers))
                    (= 2 (length (getf state :lanes)))))
        (check "topics: the lane configuration is its config"
               (and (equal "stub-a" (getf (getf (getf state :config) :lane-model) :id))
                    (eq :stub (getf (getf (getf state :config) :lane-model) :provider))
                    (eq :high (getf (getf state :config) :lane-thinking))))
        (check "topics: a lane row has its number, state and clocks"
               (let ((row (elt (getf state :lanes) 0)))
                 (and (eql 1 (getf row :n)) (eq :starting (getf row :state))
                      (member :task-started-at (plist-keys row))
                      (member :step-started-at (plist-keys row)))))))
    (check "topics: an unchanged swarm publishes nothing"
           (null (with-published (ops) (evo.swarm::publish-swarm-state *swarm*))))
    ;; Every transition, including starting -> idle (D2).
    (let ((ops (with-published (ops)
                 (evo.swarm::with-swarm-lock () (setf (lane-state lane) :idle))
                 (evo.swarm::swarm-lane-changed))))
      (check "topics: a lane transition publishes the swarm state"
             (and (= 1 (length ops))
                  (eq :idle (getf (elt (getf (getf (first ops) :patch) :lanes) 0)
                                  :state)))))
    (evo.swarm::with-swarm-lock () (setf (lane-state lane) :working))
    (let ((ops (with-published (ops) (evo.swarm::swarm-lane-changed))))
      (check "topics: busy counts the working lanes"
             (eql 1 (getf (getf (getf (first ops) :patch) :status) :busy))))
    (check "topics: waiting_on_lanes is the coordinator settled with lanes working"
           (progn (setf (evo.swarm::swarm-coordinator-busy *swarm*) nil)
                  (getf (getf (evo.swarm::swarm-state) :status) :waiting-on-lanes)))
    (check "topics: ...and off while the coordinator is running"
           (progn (setf (evo.swarm::swarm-coordinator-busy *swarm*) t)
                  (not (getf (getf (evo.swarm::swarm-state) :status) :waiting-on-lanes))))
    ;; The hold the VIEW reads to report status `waiting` (§4.2).
    (check "topics: the swarm holds the coordinator while its lanes work"
           (stringp (evo.swarm::coordinator-hold-reason agent)))
    (check "topics: ...and holds nobody else"
           (null (evo.swarm::coordinator-hold-reason (fresh-agent))))))

(defun followup-text (entry)
  "A queued follow-up's text, whether the kernel stores the entry as a string
or as a plist carrying its origin too."
  (if (stringp entry) entry (getf entry :text)))

(defun test-interrupt ()
  "The one human action on lanes (CONTRACT §5.5, design §7.4): run.interrupt
with scope lane or swarm, mediated through serve's hook."
  (let* ((agent (fresh-agent))
         (evo.kernel:*frontend* nil)
         (*swarm* (test-swarm :agent agent :workers 2))
         (server (evo.serve:make-server :port 0 :token "t"))
         (saved (symbol-function 'evo.swarm::lane-op))
         (ops nil) (interrupted t))
    (setf (evo.swarm::swarm-server *swarm*) server)
    (unwind-protect
         (progn
           (setf (symbol-function 'evo.swarm::lane-op)
                 (lambda (lane op args &key timeout)
                   (declare (ignore lane timeout))
                   (push (list op args) ops)
                   (and interrupted (list :interrupted t))))
           (check "interrupt: scope lane stops exactly that lane"
                  (equal '("lane:1") (evo.serve:interrupt-scope server :lane 1)))
           (check "interrupt: ...through that lane's own run.interrupt"
                  (equal '(("run.interrupt" (:scope "session"))) ops))
           (check "interrupt: ...and the coordinator is told a human did it"
                  (let ((queued (evo.kernel::agent-followups agent)))
                    (and queued (search "[human] stopped lane 1"
                                        (followup-text (first queued))))))
           (setf ops nil)
           (check "interrupt: scope swarm stops the coordinator and every lane"
                  (progn (setf (evo.swarm::swarm-coordinator-busy *swarm*) t)
                         (equal '("session" "lane:1" "lane:2")
                                (evo.serve:interrupt-scope server :swarm nil))))
           (check "interrupt: ...one run.interrupt per lane"
                  (= 2 (length ops)))
           (check "interrupt: ...and one human-action message naming every lane"
                  (let ((queued (evo.kernel::agent-followups agent)))
                    (and queued
                         (some (lambda (entry)
                                 (search "[human] stopped lanes 1, 2"
                                         (followup-text entry)))
                               queued))))
           ;; Nothing running: nothing is reported as interrupted.
           (setf ops nil interrupted nil
                 (evo.kernel::agent-followups agent) nil)
           (setf (evo.swarm::swarm-coordinator-busy *swarm*) nil)
           (check "interrupt: an idle lane is not reported as interrupted"
                  (null (evo.serve:interrupt-scope server :lane 1)))
           (check "interrupt: an idle swarm is not reported either"
                  (null (evo.serve:interrupt-scope server :swarm nil)))
           (check "interrupt: ...and the coordinator hears nothing about it"
                  (null (evo.kernel::agent-followups agent)))
           (check "interrupt: an unknown lane is a refusal"
                  (handler-case (progn (evo.serve:interrupt-scope server :lane 9) nil)
                    (error () t)))
           (check "interrupt: another server's swarm is not ours to stop"
                  (null (evo.serve:interrupt-scope (evo.serve:make-server :port 0 :token "u")
                                                   :swarm nil))))
      (setf (symbol-function 'evo.swarm::lane-op) saved))))

(defun test-lane-launch-args ()
  "How a lane is launched (CONTRACT §1, §6): its own port, its own ready file,
a pipe we hold, and an exact --resume."
  (let* ((*swarm* (test-swarm :workers 1))
         (lane (first (swarm-lanes *swarm*)))
         (args (evo.swarm::lane-launch-args lane :resume nil)))
    (check "launch: no supervisor of its own (the coordinator restarts it)"
           (member "--no-supervisor" args :test #'equal))
    (check "launch: it picks its own port and publishes its own ready file"
           (and (equal "0" (second (member "--port" args :test #'equal)))
                (equal (namestring (evo.swarm::ready-file-path lane))
                       (second (member "--ready-file" args :test #'equal)))))
    (check "launch: stdin is a pipe, so EOF is the coordinator going away"
           (member "--watch-stdin" args :test #'equal))
    (check "launch: a fresh lane resumes nothing"
           (not (member "--resume" args :test #'equal)))
    (let ((session (merge-pathnames "sessions/s.sexp" (evo.swarm::lane-dir lane))))
      (ensure-directories-exist session)
      (write-file-string session "")
      (setf (evo.swarm::lane-session-path lane) (namestring session))
      (check "launch: a resumed lane names its exact session, never a bare --resume"
             (let ((args (evo.swarm::lane-launch-args lane :resume t)))
               (and (member "--resume" args :test #'equal)
                    (member (namestring session) args :test #'equal))))
      (delete-file session))))

(defun test-sse-reader ()
  "serve's stream cursor is the frame's own id text; the reader hands it over
whole, and it splits into the epoch and the seq at the dot."
  (let ((seen nil))
    (with-input-from-string
        (in (format nil ": ping~%~%id: 7f3a.7~%event: op~%data: {\"op\":\"item.add\"}~%~%id: 7f3a.8~%event: op~%data: {}~%~%"))
      (evo.swarm::read-sse-events in (lambda (id type data) (push (list id type data) seen))))
    (check "sse: ids, types and data, comments skipped"
           (equal '(("7f3a.7" "op" "{\"op\":\"item.add\"}") ("7f3a.8" "op" "{}"))
                  (nreverse seen)))
    (check "sse: an id splits into the epoch and the seq"
           (multiple-value-bind (epoch seq) (evo.swarm::split-cursor "7f3a.12")
             (and (equal "7f3a" epoch) (eql 12 seq))))))

(defun test-pid-alive ()
  (check "port: this process is alive" (evo.port:pid-alive-p (evo.port:getpid)))
  (check "port: an unused pid is not" (not (evo.port:pid-alive-p 999999999))))

(defun test-sample-config ()
  "docs/examples/swarm.lisp is what people copy: it must load as shipped."
  (let ((evo.util:*settings* (copy-list evo.util:*settings*))
        (evo.swarm::*lane-forms* nil))
    (let ((*package* (find-package :evo.user)))
      (load (merge-pathnames "docs/examples/swarm.lisp" (uiop:getcwd))))
    (check "sample swarm.lisp: sets the lane count"
           (eql 4 (setting :swarm-workers)))))

(defun test-swarm-check ()
  "`evo-swarm check --json` (CONTRACT §2): whether a launch would work, and
why not — the answer a GUI's chooser needs before it spawns anything."
  (let ((saved-models evo.provider::*models*)
        (saved-providers (copy-alist evo.provider::*providers*))
        (journal (make-session-journal))
        (agent nil))
    (unwind-protect
         (progn
           (register-provider* :fixture
                               :base-url "http://127.0.0.1:1/v1"
                               :api-key-env "EVO_TEST_FIXTURE_KEY")
           (register-model* "fixture-model" :provider :fixture :api :anthropic-messages
                            :context-window 200000 :max-output 8000)
           (setf agent (make-agent :journal journal))
           (evo.port:setenv "EVO_TEST_FIXTURE_KEY" "")
           (multiple-value-bind (entry problem)
               (evo.swarm::check-model-entry "fixture-model" agent)
             (check "check: a model with no key is not ok, and says which variable"
                    (and (not (getf entry :ok))
                         (search "EVO_TEST_FIXTURE_KEY" (getf entry :reason))))
             (check "check: ...and the problem is the caller's to report"
                    ;; The code names what is missing, not just "not ready":
                    ;; a chooser can act on no_api_key and cannot on unready.
                    (equal "no_api_key" (getf problem :code))))
           (multiple-value-bind (entry problem)
               (evo.swarm::check-model-entry "no-such-model" agent)
             (check "check: an unknown model name is reported, not signalled"
                    (and (equal "no-such-model" (getf entry :id))
                         (not (getf entry :ok))))
             (check "check: ...as model_unresolved"
                    (equal "model_unresolved" (getf problem :code))))
           (evo.port:setenv "EVO_TEST_FIXTURE_KEY" "sk-1")
           (multiple-value-bind (entry problem)
               (evo.swarm::check-model-entry "fixture-model" agent)
             (check "check: with a key present the model is ok"
                    (and (getf entry :ok) (null (getf entry :reason)) (null problem))))
           (check "check: --model ID@PROVIDER resolves that registration"
                  (getf (evo.swarm::check-model-entry "fixture-model@fixture" agent) :ok))
           (check "check: an unresolvable ID@PROVIDER is a problem, not a crash"
                  (not (getf (evo.swarm::check-model-entry
                              "fixture-model@nope" agent) :ok)))
           (check "check: --workers out of range is a problem"
                  (equal "invalid_workers"
                         (getf (nth-value 1 (evo.swarm::check-workers '(:workers 999)))
                               :code)))
           (check "check: --workers in range is not"
                  (null (nth-value 1 (evo.swarm::check-workers '(:workers 3))))))
      (setf evo.provider::*models* saved-models
            evo.provider::*providers* saved-providers)
      (evo.port:setenv "EVO_TEST_FIXTURE_KEY" ""))))

(defun run-all ()
  (let ((*pass* 0) (*fail* 0))
    (test-baseline)
    (test-in-lanes)
    (test-model-fill-in)
    (test-tool-limits)
    (test-notes)
    (test-coordinator-tools)
    (test-goal-hold)
    (test-lane-input)
    (test-mirror)
    (test-mirror-rebuild)
    (test-topics)
    (test-interrupt)
    (test-lane-launch-args)
    (test-view)
    (test-serve-view)
    (test-record)
    (test-session-switch)
    (test-launch-environment)
    (test-sessions-dir)
    (test-cli)
    (test-serve-cli)
    (test-serve-exit-codes)
    (test-sse-reader)
    (test-pid-alive)
    (test-catalog)
    (test-swarm-check)
    (test-sample-config)
    ;; The two questions the GUI asks before it starts a swarm, answered
    ;; offline (swarm/offline.lisp, CONTRACT §2).
    (test-swarm-check)
    (format t "~%swarm: ~d passed, ~d failed~%" *pass* *fail*)
    (if (zerop *fail*) 0 1)))
