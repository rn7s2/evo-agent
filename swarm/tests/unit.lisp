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

(defun plist-without (plist keys)
  "PLIST without the KEYS in it, in order."
  (loop for (k v) on plist by #'cddr unless (member k keys) append (list k v)))

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

(defmethod view-say ((view recording-view) text &key style (source :swarm))
  (push (list text style source) (slot-value view 'said)))

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
    ;; A finished run: the lane moves back to idle, which is the one signal
    ;; every ending gives — serve appends a run_outcome item only for an
    ;; ending that went wrong (aborted, error, length).
    (run-ends lane '(:status "idle" :goal (:status "complete")) nil 1000)
    (check "input: a finished run says which goal status the lane settled with"
           (search "[lane 1] run ended (stop) — goal: complete"
                   (getf (first (evo.kernel::agent-steering agent)) :text)))
    (check "input: ...and carries it, task included, as a lane-event origin"
           (let ((origin (getf (first (evo.kernel::agent-steering agent)) :origin)))
             (and (equal '(:kind :lane-event :lane 1 :event :run-ended :outcome "stop"
                           :goal-status :complete)
                         (plist-without origin '(:task :severity)))
                  (member :task (plist-keys origin)))))
    (evo.kernel::drain-steering agent)
    ;; An ending that went wrong left an item naming it.
    (run-ends lane '(:status "idle" :goal (:status "active"))
                        '(:id "r2" :kind "run_outcome" :outcome "aborted") 2000)
    (check "input: a stopped lane with an active goal waits to be steered"
           (and (search "[lane 1] run ended (aborted)"
                        (getf (first (evo.kernel::agent-steering agent)) :text))
                (search "goal: active, but the lane is idle until steered"
                        (getf (first (evo.kernel::agent-steering agent)) :text))))
    (evo.kernel::drain-steering agent)
    ;; Nothing else is news.
    (evo.swarm::mirror-note-item (evo.swarm::lane-mirror lane) '(:id "a1" :kind "assistant" :text "hi"))
    (check "input: streamed text is not coordinator input"
           (not (steering-pending-p agent)))
    (check "input: a run that simply stopped says no goal it never had"
           (progn (run-ends lane '(:status "idle") nil 3000)
                  (let ((text (getf (first (evo.kernel::agent-steering agent)) :text)))
                    (and (search "[lane 1] run ended (stop)" text)
                         (not (search "goal:" text))))))
    ;; A status tick of a run that is still going (its step clock moved) is
    ;; not an ending: the status has to leave "running".
    (evo.kernel::drain-steering agent)
    (check "input: a running lane's status tick is not a run ending"
           (let ((mirror (evo.swarm::lane-mirror lane)))
             (evo.swarm::with-swarm-lock () (setf (evo.swarm::mirror-ended-at mirror) -1000000))
             (evo.swarm::mirror-state-set mirror '(:status "running" :task (:started-at 4000)))
             (evo.swarm::mirror-note-lane-state mirror)
             (evo.swarm::mirror-state-set mirror '(:status "running"
                                                   :task (:started-at 4000 :step-started-at 4100)))
             (evo.swarm::mirror-note-lane-state mirror)
             (not (steering-pending-p agent))))
    ;; ...and an idle lane that is still idle says nothing at all.
    (evo.kernel::drain-steering agent)
    (check "input: a lane still idle after a snapshot is not a run ending"
           (progn (evo.swarm::mirror-note-lane-state (evo.swarm::lane-mirror lane))
                  (not (steering-pending-p agent))))))

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
    (check "view: a notice reaches the view, with its style and its source"
           (equal '(("hello" :error :swarm) ("quiet" :dim :swarm))
                  (said recording)))
    (swarm-repaint)
    (swarm-repaint)
    (check "view: a repaint reaches the view" (= 2 (slot-value recording 'repaints)))
    (check "view: the run goes to the view, and its exit code comes back"
           (and (eql 7 (swarm-run agent t))
                (equal (list agent t) (slot-value recording 'ran))))
    (check "view: a frontend that does not show the queue says it as a notice"
           (null (swarm-shows-queued-input-p))))
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
  ;; `--prompt-note`: the agent's own flag, read by the agent's own reader (the
  ;; coordinator registers it, and LANE-LAUNCH-ARGS passes it on).
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "~a/evo-swarm-note-~a/" (tmp-dir) (gen-id))))
         (note (merge-pathnames "gui-renderer.md" dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist note)
           (write-file-string note "Render it yourself.\n")
           (check "serve: --prompt-note is read like the agent's serve reads it"
                  (equal (list :path (namestring note) :name "gui-renderer.md"
                               :text "Render it yourself.\n")
                         (first (getf (evo.swarm::parse-args
                                       (list "serve" "--prompt-note" (namestring note)))
                                      :prompt-notes))))
           (check "serve: --prompt-note repeats"
                  (= 2 (length (getf (evo.swarm::parse-args
                                      (list "serve" "--prompt-note" (namestring note)
                                            "--prompt-note" (namestring note)))
                                     :prompt-notes))))
           (check "serve: --prompt-note outside serve is unknown"
                  (handler-case
                      (progn (evo.swarm::parse-args
                              (list "--prompt-note" (namestring note)))
                             nil)
                    (evo.cli:usage-error () t)))
           (check "serve: --prompt-note refuses a path that is not there"
                  (handler-case
                      (progn (evo.swarm::parse-args
                              '("serve" "--prompt-note" "/nowhere/at/all.md"))
                             nil)
                    (evo.cli:usage-error () t)))
           (check "serve: --prompt-note needs a path"
                  (handler-case (progn (evo.swarm::parse-args '("serve" "--prompt-note")) nil)
                    (evo.cli:usage-error () t))))
      (ignore-errors (uiop:delete-directory-tree dir :validate t
                                                 :if-does-not-exist :ignore))))
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
                 "--watch-stdin" "--allow-remote"
                 ;; notes are not journalled, so the command line is the only
                 ;; place a restarted coordinator can learn them from.
                 "--prompt-note" "/w/a.md" "--prompt-note" "/w/b.md"))))
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
           (not (member "--resume" argv :test #'equal)))
    (check "serve: ...and every --prompt-note"
           (equal '("--prompt-note" "/w/a.md" "--prompt-note" "/w/b.md")
                  (loop for rest on argv
                        when (equal (first rest) "--prompt-note")
                          collect (first rest) and collect (second rest))))))

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
             (getf (evo.serve:topic-snapshot mirror :items 200) :has-more)))
    ;; A lane whose own snapshot said has_more (a long session): the mirror
    ;; holds that as a boolean, and its snapshot must still answer — it once
    ;; signalled "malformed property list: T" and served the topic as a 500.
    (setf (evo.swarm::mirror-dropped mirror) 0
          (evo.swarm::mirror-has-more mirror) t)
    (let ((snapshot (handler-case (evo.serve:topic-snapshot mirror :items 200)
                      (error () :failed))))
      (check "mirror: a lane that has more below snapshots, and says so"
             (and (listp snapshot) (eq t (getf snapshot :has-more)))))
    (setf (evo.swarm::mirror-has-more mirror) nil)
    (check "mirror: a window that cuts the mirror short has more below"
           (getf (evo.serve:topic-snapshot mirror :items 1) :has-more))))

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

(defun test-bring-up-announces ()
  "A lane's topic is registered — empty — when the swarm starts, before the
lane's process exists, and MIRROR-LOAD publishes nothing by design (mirror.lisp).
The first bring-up is therefore what tells a client that snapshotted the empty
topic to read it again: without it that client keeps a lane with no model,
context or cache chips, and `hasn't been given work yet` over a transcript the
lane's own session is holding.  A swarm resumed from a journal is the same
client's case — it has no reset of its own to rely on (CONTRACT §5.3)."
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 1))
         (lane (first (swarm-lanes *swarm*)))
         (saved-launch (symbol-function 'evo.swarm::launch-lane))
         (saved-ready (symbol-function 'evo.swarm::wait-for-ready))
         (saved-init (symbol-function 'evo.swarm::initialize-lane)))
    (unwind-protect
         (progn
           (setf (symbol-function 'evo.swarm::launch-lane)
                 (lambda (lane &key resume)
                   (declare (ignore lane resume))
                   t)
                 (symbol-function 'evo.swarm::wait-for-ready)
                 (lambda (lane &key epoch)
                   (declare (ignore lane epoch))
                   (list :epoch "e1" :session (list :path "/tmp/lane-1.sexp")))
                 (symbol-function 'evo.swarm::initialize-lane)
                 (lambda (lane) (declare (ignore lane)) nil))
           (labels ((resets (ops)
                      (remove-if-not (lambda (op) (equal "topic.reset" (getf op :op)))
                                     ops))
                    ;; WITH-PUBLISHED answers the ops it collected, so the
                    ;; call's own value is taken by side effect.
                    (bring-up (&rest args)
                      (let ((came-up nil))
                        (let ((ops (with-published (ops)
                                     (setf came-up (apply #'evo.swarm::bring-up-lane
                                                          lane args)))))
                          (values came-up ops)))))
             (with-lane-snapshot ((list :status "idle" :model (list :id "stub-a" :ready t))
                                  :items (list (list :id "e1" :kind "user" :ts 1
                                                     :text "old work")))
               (multiple-value-bind (came-up ops) (bring-up)
                 (check "bring-up: the lane came up"
                        (and came-up ops))
                 (check "bring-up: a lane's first state is announced to clients"
                        (equal '(("topic.reset" "lane:1" "lane_restarted"))
                               (mapcar (lambda (op) (list (getf op :op) (getf op :topic)
                                                          (getf op :reason)))
                                       (resets ops))))
                 (check "bring-up: ...after the snapshot it just took"
                        (and (equal "idle" (getf (evo.swarm::mirror-lane-state
                                                  (evo.swarm::lane-mirror lane))
                                                 :status))
                             (equal '("e1") (mapcar (lambda (i) (getf i :id))
                                                    (evo.swarm::mirror-items
                                                     (evo.swarm::lane-mirror lane)))))))
               ;; The lane's topic has a state now: a plain bring-up is not news.
               (with-lane-snapshot ((list :status "working"))
                 (check "bring-up: a lane that was only followed again is not announced twice"
                        (null (resets (multiple-value-bind (came-up ops) (bring-up :resume t)
                                        (declare (ignore came-up))
                                        ops)))))
               ;; A restart is news whatever the mirror holds: the caller says so.
               (with-lane-snapshot ((list :status "idle"))
                 (check "bring-up: a restarted lane is announced"
                        (equal "topic.reset"
                               (getf (first (resets
                                             (multiple-value-bind (came-up ops)
                                                 (bring-up :resume t :reset t)
                                               (declare (ignore came-up))
                                               ops)))
                                     :op)))))))
      (setf (symbol-function 'evo.swarm::launch-lane) saved-launch
            (symbol-function 'evo.swarm::wait-for-ready) saved-ready
            (symbol-function 'evo.swarm::initialize-lane) saved-init))))

(defun test-bring-up-failure-cleanup ()
  "A lane whose process never becomes ready must not leave a live process
behind: the failed BRING-UP closes the pipe that holds it, kills and reaps the
process it owns, clears its process, pipe, ready and ready file, and marks it
down with a :failed-to-start event.  The lane stays retryable — nothing holds
it stopping, and a later bring-up reclaims it."
  (let* ((events nil)
         (*swarm* (test-swarm :workers 1))
         (lane (first (swarm-lanes *swarm*)))
         (sentinel (list :pid 4242))
         (stdin (make-string-input-stream ""))
         (killed nil) (waited nil)
         (saved-launch (symbol-function 'evo.swarm::launch-lane))
         (saved-ready (symbol-function 'evo.swarm::wait-for-ready))
         (saved-init (symbol-function 'evo.swarm::initialize-lane))
         (saved-kill (symbol-function 'evo.port:process-kill-tree))
         (saved-wait (symbol-function 'evo.port:process-wait))
         (saved-event (symbol-function 'evo.swarm::tell-lane-event)))
    (unwind-protect
         (progn
           (ensure-directories-exist (evo.swarm::lane-dir lane))
           (write-file-string (evo.swarm::ready-file-path lane) "{}\n")
           ;; What the real LAUNCH-LANE installs, and a wait that times out.
           (setf (symbol-function 'evo.swarm::launch-lane)
                 (lambda (lane &key resume)
                   (declare (ignore resume))
                   (setf (evo.swarm::lane-process lane) sentinel
                         (evo.swarm::lane-stdin lane) stdin)
                   sentinel)
                 (symbol-function 'evo.swarm::wait-for-ready)
                 (lambda (lane &key epoch) (declare (ignore lane epoch)) nil)
                 (symbol-function 'evo.port:process-kill-tree)
                 (lambda (process) (push process killed) t)
                 (symbol-function 'evo.port:process-wait)
                 (lambda (process) (push process waited) t)
                 ;; The event itself is asserted, not delivered: delivering it
                 ;; queues coordinator input and asks for a run.
                 (symbol-function 'evo.swarm::tell-lane-event)
                 (lambda (lane event &key detail &allow-other-keys)
                   (push (list (lane-n lane) event detail) events)
                   nil))
           (check "bring-up: a lane that never becomes ready does not come up"
                  (null (bring-up-lane lane)))
           (check "bring-up: ...it is marked down"
                  (eq :down (lane-state lane)))
           (check "bring-up: ...its process, pipe and ready are cleared"
                  (and (null (evo.swarm::lane-process lane))
                       (null (evo.swarm::lane-stdin lane))
                       (null (evo.swarm::lane-ready lane))))
           (check "bring-up: ...the process it owned is killed and reaped"
                  (and (equal (list sentinel) killed)
                       (equal (list sentinel) waited)))
           (check "bring-up: ...the pipe that holds it is closed"
                  (not (open-stream-p stdin)))
           (check "bring-up: ...the stale ready file is gone"
                  (not (probe-file (evo.swarm::ready-file-path lane))))
           (check "bring-up: ...the coordinator hears :failed-to-start"
                  (let ((event (first events)))
                    (and (eql 1 (length events))
                         (eql 1 (first event))
                         (eq :failed-to-start (second event))
                         (search (namestring (evo.swarm::lane-log-path lane))
                                 (third event)))))
           ;; Retryable by design: no rescue, but nothing wedges either.  A
           ;; later bring-up reclaims the same lane with a process of its own.
           (check "bring-up: the failed lane is not left stopping"
                  (not (evo.swarm::lane-stopping lane)))
           (let ((second (list :pid 5252)))
             (setf (symbol-function 'evo.swarm::launch-lane)
                   (lambda (lane &key resume)
                     (declare (ignore resume))
                     (setf (evo.swarm::lane-process lane) second)
                     second)
                   (symbol-function 'evo.swarm::wait-for-ready)
                   (lambda (lane &key epoch)
                     (declare (ignore lane epoch))
                     (list :epoch "e2" :session (list :path "/tmp/lane-1.sexp")))
                   (symbol-function 'evo.swarm::initialize-lane)
                   (lambda (lane) (declare (ignore lane)) nil))
             (with-lane-snapshot ((list :status "idle" :model (list :id "stub-a" :ready t)))
               (check "bring-up: a retry after the failure comes up"
                      (and (bring-up-lane lane)
                           (eq second (evo.swarm::lane-process lane))
                           (equal "idle"
                                  (getf (evo.swarm::mirror-lane-state
                                         (evo.swarm::lane-mirror lane))
                                        :status)))))))
      (setf (symbol-function 'evo.swarm::launch-lane) saved-launch
            (symbol-function 'evo.swarm::wait-for-ready) saved-ready
            (symbol-function 'evo.swarm::initialize-lane) saved-init
            (symbol-function 'evo.port:process-kill-tree) saved-kill
            (symbol-function 'evo.port:process-wait) saved-wait
            (symbol-function 'evo.swarm::tell-lane-event) saved-event))))

(defun test-topics ()
  "The swarm is one observable thing (CONTRACT §4.3): its topic carries the
whole state, and every lane transition publishes it."
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 2))
         (lane (first (swarm-lanes *swarm*))))
    (setf (evo.swarm::swarm-lane-model *swarm*) "stub-a"
          ;; A dashed key, so the published config proves REGISTRY-NAME is
          ;; what crosses the wire rather than the snake_case an enum gets.
          (evo.swarm::swarm-lane-provider *swarm*) :stub-relay
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
                    (equal "stub-relay"
                           (getf (getf (getf state :config) :lane-model) :provider))
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
                  (eq t (getf (getf (evo.swarm::swarm-state) :status) :waiting-on-lanes))))
    (check "topics: ...and false, not null, while the coordinator is running"
           (progn (setf (evo.swarm::swarm-coordinator-busy *swarm*) t)
                  (eq :false (getf (getf (evo.swarm::swarm-state) :status)
                                   :waiting-on-lanes))))
    ;; The hold the VIEW reads to report status `waiting` (§4.2).
    (check "topics: the swarm holds the coordinator while its lanes work"
           (stringp (evo.swarm::coordinator-hold-reason agent)))
    (check "topics: ...and holds nobody else"
           (null (evo.swarm::coordinator-hold-reason (fresh-agent))))))

(defun run-ends (lane state &optional item started)
  "LANE was working since STARTED (epoch ms) and STATE (its own topic's state)
says it is not any more, with ITEM as the newest item of its topic — how a
run's ending reaches the swarm.  STARTED is explicit so a test can order runs
that the clock would put in the same millisecond."
  (let ((mirror (evo.swarm::lane-mirror lane)))
    ;; The lane says it is running (its own word, not the coordinator's
    ;; optimistic one), then that it is not.
    (evo.swarm::with-swarm-lock () (setf (evo.swarm::mirror-ended-at mirror) -1000000))
    (evo.swarm::mirror-state-set mirror (list :status "running"
                                              :task (list :started-at started)))
    (evo.swarm::mirror-note-lane-state mirror)
    (evo.swarm::with-swarm-lock ()
      (setf (evo.swarm::lane-task-started lane) started))
    (evo.swarm::mirror-state-set mirror state)
    ;; The item goes in as the stream puts it there, so the state moving back
    ;; to idle finds it (that is what names an ending that went wrong).
    (when item
      (evo.swarm::mirror-apply mirror (list :op "item.add"
                                            :item (append item (list :ts started)))))
    (evo.swarm::mirror-note-lane-state mirror)))

(defun followup-text (entry)
  "A queued follow-up's text, whether the kernel stores the entry as a string
or as a plist carrying its origin too."
  (if (stringp entry) entry (getf entry :text)))

(defclass recording-session-topic ()
  ((queued-inputs :initform nil :accessor recorded-queued))
  (:documentation "A session topic that records what a program queues into it."))

(defmethod evo.serve:topic-provider-queued-input ((topic recording-session-topic)
                                                  id text images queue)
  "What serve does with a queued input: the client's transcript gets a row."
  (declare (ignore images))
  (push (list id text queue) (recorded-queued topic)))

(defun test-interrupt ()
  "The one human action on lanes (CONTRACT §5.5, design §7.4): run.interrupt
with scope lane or swarm, mediated through serve's hook."
  (let* ((agent (fresh-agent))
         (evo.kernel:*frontend* nil)
         (*swarm* (test-swarm :agent agent :workers 2))
         (server (evo.serve:make-server :port 0 :token "t"))
         (session-topic nil)
         (saved (symbol-function 'evo.swarm::lane-op))
         (ops nil) (interrupted t))
    (setf (evo.swarm::swarm-server *swarm*) server)
    ;; The session topic a client reads: the note has to show up in the
    ;; coordinator's queue there, not only in its own mailbox.
    (setf session-topic (make-instance 'recording-session-topic))
    (evo.serve:register-topic server "session" session-topic)
    (unwind-protect
         (progn
           (setf (symbol-function 'evo.swarm::lane-op)
                 (lambda (lane op args &key timeout)
                   (declare (ignore lane timeout))
                   (push (list op args) ops)
                   ;; What a lane's serve really answers: an array either
                   ;; way, [] when it was idle (CONTRACT §5.5).
                   (list :interrupted (if interrupted (vector "session") (vector)))))
           (check "interrupt: scope lane stops exactly that lane"
                  (equal '("lane:1") (evo.serve:interrupt-scope server :lane 1)))
           (check "interrupt: ...through that lane's own run.interrupt"
                  (equal '(("run.interrupt" (:scope "session"))) ops))
           (check "interrupt: ...and the coordinator is told a human did it"
                  (let ((queued (evo.kernel::agent-followups agent)))
                    (and queued (search "[human] stopped lane 1"
                                        (followup-text (first queued))))))
           ;; It reaches the coordinator, but it is not a reason to spend a
           ;; turn now: the note is after-run input, with the origin that
           ;; says a person's stop caused it (CONTRACT §3).
           (check "interrupt: ...as after-run input, not as steering now"
                  (and (first (evo.kernel::agent-followups agent))
                       (null (evo.kernel::agent-steering agent))))
           (check "interrupt: ...carrying the human action that caused it"
                  (equalp '(:kind :human-action :action :interrupt :lanes #(1))
                          (getf (first (evo.kernel::agent-followups agent)) :origin)))
           ;; A client reading the session sees the note waiting, as a queued
           ;; row under the same id the journal will carry (§3).
           (check "interrupt: ...and shown in the session's queue for a client"
                  (let ((queued (first (recorded-queued session-topic))))
                    (and queued
                         (equal (third queued) "after_run")
                         (search "[human] stopped lane 1" (second queued))
                         (equal (first queued)
                                (getf (first (evo.kernel::agent-followups agent)) :id)))))
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
           ;; Only the lane that was running is named: the others answered
           ;; [] and stopped nothing.
           (setf ops nil)
           (setf (symbol-function 'evo.swarm::lane-op)
                 (lambda (lane op args &key timeout)
                   (declare (ignore timeout))
                   (push (list op args) ops)
                   (list :interrupted (if (= (evo.swarm::lane-n lane) 2)
                                          (vector "session")
                                          (vector)))))
           (check "interrupt: scope swarm names only the lanes it stopped"
                  (equal '("lane:2") (evo.serve:interrupt-scope server :swarm nil)))
           (check "interrupt: ...and so does the coordinator's note"
                  (let ((queued (evo.kernel::agent-followups agent)))
                    (and (= 1 (length queued))
                         (search "[human] stopped lane 2 (interrupt)"
                                 (followup-text (first queued))))))
           (setf (evo.kernel::agent-followups agent) nil)
           (check "interrupt: ...and the coordinator hears nothing about it"
                  (null (evo.kernel::agent-followups agent)))
           (check "interrupt: an unknown lane is a refusal"
                  (handler-case (progn (evo.serve:interrupt-scope server :lane 9) nil)
                    (error () t)))
           (check "interrupt: another server's swarm is not ours to stop"
                  (null (evo.serve:interrupt-scope (evo.serve:make-server :port 0 :token "u")
                                                   :swarm nil))))
      (setf (symbol-function 'evo.swarm::lane-op) saved))))

(defun test-tools ()
  "What the coordinator's model is told about its lanes (tools.lisp): one line
per lane, and a refusal it can act on rather than a crash."
  (let* ((*swarm* (test-swarm :workers 2))
         (idle (first (swarm-lanes *swarm*)))
         (busy (second (swarm-lanes *swarm*))))
    (evo.swarm::with-swarm-lock ()
      (setf (lane-state idle) :idle
            (evo.swarm::lane-ready idle) '(:pid 4242)
            (lane-state busy) :working
            (lane-task busy) "write the thing"
            (lane-worktree busy) "/tmp/wt"
            (evo.swarm::lane-restarts busy) 2))
    (let ((text (evo.swarm::tool-lanes nil)))
      (check "tools: /lanes lists every lane, idle and working"
             (and (search "lane 1  idle" text) (search "lane 2  working" text)))
      (check "tools: ...with its pid, its task and its worktree"
             (and (search "pid 4242" text) (search "write the thing" text)
                  (search "/tmp/wt" text)))
      (check "tools: ...and how often it has been restarted"
             (search "2 restarts" text))
      ;; A lane with nothing filled in — down, no process, no step clock — is
      ;; rendered, not signalled: this is the shape that reads NIL as a number.
      (evo.swarm::with-swarm-lock ()
        (setf (lane-state idle) :down (evo.swarm::lane-ready idle) nil
              (lane-task busy) nil (lane-worktree busy) nil
              (evo.swarm::lane-restarts busy) 0))
      (check "tools: a down lane with no process still renders"
             (and (search "lane 1  down" (evo.swarm::tool-lanes nil))
                  (search "0 reports" (evo.swarm::tool-lanes nil)))))
    ;; Delegation refuses with a sentence when every lane is busy — the model
    ;; is told what to do, and nothing is sent to a lane.
    (evo.swarm::with-swarm-lock ()
      (setf (lane-state idle) :working (lane-state busy) :working))
    (check "tools: delegating to nothing busy says what to do instead"
           (handler-case (progn (evo.swarm::tool-delegate '(:task "x")) nil)
             (error (e) (search "every lane is busy" (format nil "~a" e)))))))

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
    ;; A client's own system-prompt notes reach the lanes: a lane's transcript
    ;; is a client's to show too, and the flag is the only carrier (notes are
    ;; not journalled).  Every launch goes through here — a lane that crashed
    ;; and came back is launched by the same call.
    (setf (evo.swarm::swarm-prompt-notes *swarm*)
          (list "/w/gui-renderer.md" "/w/theme.md"))
    (check "launch: the launch's --prompt-note files are passed to the lane, in order"
           (equal '("--prompt-note" "/w/gui-renderer.md" "--prompt-note" "/w/theme.md")
                  (loop for rest on (evo.swarm::lane-launch-args lane :resume nil)
                        when (equal (first rest) "--prompt-note")
                          collect (first rest) and collect (second rest))))
    (check "launch: a lane with no notes is launched without the flag"
           (let ((saved (evo.swarm::swarm-prompt-notes *swarm*)))
             (unwind-protect
                  (progn (setf (evo.swarm::swarm-prompt-notes *swarm*) nil)
                         (not (member "--prompt-note"
                                      (evo.swarm::lane-launch-args lane :resume nil)
                                      :test #'equal)))
               (setf (evo.swarm::swarm-prompt-notes *swarm*) saved))))
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
        (saved-settings (evo.util:capture-settings))
        (journal (make-session-journal))
        (agent nil))
    (unwind-protect
         (progn
           (register-provider* :fixture
                               :base-url "http://127.0.0.1:1/v1"
                               :api-key-env "EVO_TEST_FIXTURE_KEY")
           (register-model* "fixture-model" :provider :fixture :api :anthropic-messages
                            :context-window 200000 :max-output 8000
                            ;; A model that offers a subset of the ladder, in
                            ;; another order: what the answer says is the
                            ;; subset the model takes, in the ladder's order.
                            :effort '(:max :low))
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
             (check "check: a model that did not resolve lists no levels — [] , not null"
                    (and (equalp #() (getf entry :effort-levels))
                         (search "\"effort_levels\":["
                                 (evo.serve:encode-json
                                  (evo.swarm::check-entry-wire entry)))
                         (not (search "\"effort_levels\":null"
                                      (evo.serve:encode-json
                                       (evo.swarm::check-entry-wire entry))))))
             (check "check: ...as model_unresolved"
                    (equal "model_unresolved" (getf problem :code))))
           (evo.port:setenv "EVO_TEST_FIXTURE_KEY" "sk-1")
           (multiple-value-bind (entry problem)
               (evo.swarm::check-model-entry "fixture-model" agent)
             (check "check: with a key present the model is ok"
                    (and (getf entry :ok) (null (getf entry :reason)) (null problem))))
           (multiple-value-bind (entry problem)
               (evo.swarm::check-model-entry "fixture-model" agent)
             (declare (ignore problem))
             (check "check: a model names the effort levels it takes, in ladder order"
                    (equalp #("low" "max") (getf entry :effort-levels)))
             (check "check: ...and they are the same list the catalog carries"
                    (equalp (getf entry :effort-levels)
                            (evo.serve:model-effort-levels
                             (find-model "fixture-model" :fixture)))))
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
                  (null (nth-value 1 (evo.swarm::check-workers '(:workers 3)))))
           ;; What a launch with no flags resolves, so a GUI's controls open
           ;; on the truth instead of on medium and six (CONTRACT §2).
           (check "check: the effort is evo's own chain — medium by default"
                  (equal '("medium" "medium")
                         (multiple-value-list
                          (evo.swarm::check-thinking nil agent nil))))
           (check "check: ...--lane-thinking names the lanes' own"
                  (equal '("medium" "max")
                         (multiple-value-list
                          (evo.swarm::check-thinking '(:lane-thinking :max) agent nil))))
           ;; The :thinking setting and the flags, in the order the launch
           ;; applies them.
           (evo.util:set-setting :thinking :high)
           (check "check: the :thinking setting moves both"
                  (equal '("high" "high")
                         (multiple-value-list
                          (evo.swarm::check-thinking nil agent nil))))
           (check "check: --thinking and --lane-thinking win over the setting"
                  (equal '("low" "max")
                         (multiple-value-list
                          (evo.swarm::check-thinking '(:thinking :low :lane-thinking :max)
                                                     agent nil))))
           (check "check: a retired :off normalizes onto the weakest live rung"
                  (equal '("low" "low")
                         (multiple-value-list
                          (evo.swarm::check-thinking '(:thinking :off :lane-thinking :off)
                                                     agent nil))))
           ;; The other document a launch's client reads before it spawns
           ;; anything: the catalog states the same effort as a word, so a
           ;; page's control opens on it rather than hard-coding medium.
           (check "catalog: the document states the default thinking check reports"
                  (equal "high" (getf (evo.serve:catalog-plist agent) :default-thinking)))
           (check "catalog: ...the same level check names for the coordinator"
                  (equal (getf (evo.serve:catalog-plist agent) :default-thinking)
                         (nth-value 0 (evo.swarm::check-thinking nil agent nil))))
           (evo.util:set-setting :thinking :off)
           (check "catalog: a retired :off setting is the weakest live rung, not the word"
                  (equal "low" (getf (evo.serve:catalog-plist agent) :default-thinking)))
           (evo.util:set-setting :thinking :high)
           ;; A resumed swarm restored its own lane thinking; the flags, when
           ;; given, still win over it (as run-swarm applies them).
           (evo:set-custom-state "swarm" (list :lane-thinking :xhigh) agent)
           (check "check: a resumed swarm's lane thinking beats the coordinator's"
                  (equal '("high" "xhigh")
                         (multiple-value-list
                          (evo.swarm::check-thinking nil agent t))))
           (check "check: ...unless the launch names one"
                  (equal '("high" "low")
                         (multiple-value-list
                          (evo.swarm::check-thinking '(:lane-thinking :low) agent t))))
           (check "check: the document's bools are bools, not nulls"
                  (let ((wire (evo.swarm::check-entry-wire
                               (list :id "m" :provider "p" :ok nil :reason "why"))))
                    (and (eq :false (getf wire :ok))
                         (equal "why" (getf wire :reason))
                         (eq t (getf (evo.swarm::check-entry-wire
                                      (list :id "m" :ok t))
                                     :ok)))))
           ;; A provider is a name, not an enum (REGISTRY-NAME): the document
           ;; says "foo-bar", the way init.lisp and --model id@foo-bar spell
           ;; it, not the "foo_bar" the JSON mapping gives a keyword value.
           (register-provider* :foo-bar :base-url "http://127.0.0.1:7/v1"
                                        :api-key-env "EVO_TEST_FOO_KEY")
           (register-model* "dash-model" :provider :foo-bar :api :anthropic-messages
                            :context-window 200000 :max-output 8000)
           (evo.port:setenv "EVO_TEST_FOO_KEY" "sk-1")
           (let* ((entry (evo.swarm::check-model-entry "dash-model@foo-bar" agent))
                  (wire (evo.swarm::check-entry-wire entry)))
             (check "check: a model names its provider with its dashes"
                    (equal "foo-bar" (getf entry :provider)))
             (check "check: ...and the document encodes it that way"
                    (and (search "\"provider\":\"foo-bar\"" (evo.serve:encode-json wire))
                         (not (search "foo_bar" (evo.serve:encode-json wire))))))
           (let* ((plan (evo.swarm::lane-plan '(:lane-model "dash-model"
                                                :lane-provider :foo-bar)
                                              agent))
                  (entry (evo.swarm::check-lane-entry plan)))
             (check "check: the lane's model names its provider the same way"
                    (equal "foo-bar" (getf entry :provider)))
             (check "check: ...and the encoder agrees with the document it sits in"
                    (search "\"provider\":\"foo-bar\""
                            (evo.serve:encode-json (evo.swarm::check-entry-wire entry))))))
      (setf evo.provider::*models* saved-models
            evo.provider::*providers* saved-providers)
      (evo.util:restore-settings saved-settings)
      (evo.port:setenv "EVO_TEST_FIXTURE_KEY" ""))))

(defclass lane-plan-fake-api (provider-api) ()
  (:documentation "An API an extension defines: the fixture for \"a model whose
API the in-lanes code loaded into the lane\" — and, registered only in the
coordinator, for one that stays out of it."))

(defun test-lane-plan ()
  "What a lane would end up with, and both halves of the answers the GUI reads
(swarm/offline.lisp, CONTRACT §2): the lane's own settings decide its model and
thinking level, its own code decides which APIs it has, and evaluating it here
changes nothing about the coordinator."
  (with-registries ()
    (register-provider* :stub :base-url "http://127.0.0.1:1" :api-key "LITERAL-SECRET")
    (register-model* "m-a" :provider :stub :context-window 1000 :max-output 100
                     :effort t)
    (register-model* "m-b" :provider :stub :context-window 1000 :max-output 100
                     :effort '(:medium :high))
    ;; An API only this process has: the coordinator's own extension, which
    ;; the lanes never load.
    (register-api :lane-plan-outer (make-instance 'lane-plan-fake-api))
    (register-provider* :outer :base-url "http://127.0.0.1:8/v1")
    (register-model* "outer-model" :provider :outer :api :lane-plan-outer
                     :context-window 1000 :max-output 100 :effort '(:high))
    (set-setting :model "m-a")
    (set-setting :thinking :high)
    (let* ((session-agent (fresh-agent))
           (evo:*agent* session-agent)
           (evo.swarm::*lane-forms* nil)
           (*swarm* (test-swarm :agent session-agent)))
      (evo.swarm:in-lanes ()
        (evo:set-setting :model "m-b")
        (evo:set-setting :model-provider :stub)
        (evo:set-setting :thinking :low))
      (let ((plan (evo.swarm::lane-plan nil session-agent)))
        (check "lane plan: the lane's own settings name the model"
               (equal "m-b" (evo.swarm::lane-plan-model-id plan)))
        (check "lane plan: ...and its provider"
               (eq :stub (evo.swarm::lane-plan-provider plan)))
        (check "lane plan: ...and its thinking level"
               (eq :low (evo.swarm::lane-plan-thinking plan)))
        (check "lane plan: the in-lanes model is one a lane can run"
               (getf (evo.swarm::lane-plan-status plan) :ok))
        (check "lane plan: the lane model's effort levels travel with the verdict"
               (and (equalp #("medium" "high")
                            (getf (evo.swarm::lane-plan-status plan) :effort-levels))
                    (equalp #("medium" "high")
                            (getf (evo.swarm::check-lane-entry plan) :effort-levels))))
        (check "lane plan: ...and the lane half of the catalog carries the same ones"
               (equalp #("medium" "high")
                       (getf (find "m-b"
                                   (getf (evo.swarm::lane-plan-catalog plan) :models)
                                   :key (lambda (m) (getf m :id)) :test #'equal)
                             :effort-levels)))
        ;; The document `evo-swarm catalog --json` prints is CATALOG-PLIST with
        ;; this half, and it states the effort a launch from here would start
        ;; on — the coordinator's own chain, not a lane's, which is the level
        ;; this same document's `lanes` half is silent about.
        (let ((document (evo.serve:catalog-plist
                         session-agent :swarm (evo.swarm::lane-plan-catalog plan))))
          (check "lane plan: the swarm's document states the coordinator's default thinking"
                 (equal "high" (getf document :default-thinking)))
          (check "lane plan: ...which the lanes' own level does not move"
                 (and (eq :low (evo.swarm::lane-plan-thinking plan))
                      (find "m-b" (getf (getf document :lanes) :models)
                            :key (lambda (m) (getf m :id)) :test #'equal))))
        (check "lane plan: a lane's literal key is one it has"
               ;; The key never travels as data: the lane's own environment
               ;; carries it, and the sandbox answers as that process would.
               (and (getf (evo.swarm::lane-plan-status plan) :ok)
                    (not (search "LITERAL-SECRET"
                                 (evo.swarm::forms->code
                                  (evo.swarm::lane-setup-forms-for nil session-agent nil)))))))
      ;; The flags beat the lane's own config, as they do everywhere else in
      ;; evo — over the in-lanes settings, and over what a resumed record
      ;; restored (which a flag wrote there in the first place).
      (let ((plan (evo.swarm::lane-plan '(:lane-model "m-a" :lane-provider :stub
                                          :lane-thinking :xhigh)
                                        session-agent)))
        (check "lane plan: --lane-model beats the in-lanes model"
               (equal "m-a" (evo.swarm::lane-plan-model-id plan)))
        (check "lane plan: --lane-thinking beats the in-lanes level"
               (eq :xhigh (evo.swarm::lane-plan-thinking plan)))
        (check "lane plan: ...and the levels reported are that model's own"
               (equalp #("low" "medium" "high" "xhigh" "max")
                       (getf (evo.swarm::lane-plan-status plan) :effort-levels))))
      (evo:set-custom-state "swarm"
                            (list :lane-model "m-b" :lane-provider :stub)
                            session-agent)
      (check "lane plan: a resumed record's lane model is re-applied too"
             (equal "m-b" (evo.swarm::lane-plan-model-id
                           (evo.swarm::lane-plan nil session-agent t))))
      ;; What a lane's code registers on its own: an API an extension
      ;; defines, the provider using it, and the model that runs on it.
      (let ((evo.swarm::*lane-forms* nil))
        (evo.swarm:in-lanes ()
          (evo:register-api :lane-plan-fake (make-instance 'lane-plan-fake-api))
          (evo:register-provider :lane-plan-fake-provider
                                 :base-url "http://127.0.0.1:9/v1")
          (evo:register-model "fake-model" :provider :lane-plan-fake-provider
                              :api :lane-plan-fake
                              :context-window 1000 :max-output 100)
          (evo:set-setting :model "fake-model")
          (evo:set-setting :model-provider :lane-plan-fake-provider))
        (let* ((plan (evo.swarm::lane-plan nil session-agent))
               (status (evo.swarm::lane-plan-status plan))
               (models (getf (evo.swarm::lane-plan-catalog plan) :models))
               (fake (find "fake-model" models
                           :key (lambda (m) (getf m :id)) :test #'equal)))
          (check "lane plan: an API the lane's own code registers is in the lane"
                 (member :lane-plan-fake (evo.swarm::lane-plan-apis plan)))
          (check "lane plan: ...so a model using it is one a lane can run"
                 (and (getf status :ok)
                      (eq :lane-plan-fake (evo.swarm::lane-plan-api plan))))
          (check "lane plan: ...and the catalog lists it for lanes"
                 (and fake (eq t (getf fake :ok)) (null (getf fake :reason))))
          (check "lane plan: a provider that declares no key is not refused one"
                 (eq t (getf fake :ok)))
          (check "lane plan: a model with no effort parameter lists no levels"
                 (equalp #() (getf fake :effort-levels)))
          (check "lane plan: the models the lane has are listed"
                 (and (find "m-b" models :key (lambda (m) (getf m :id)) :test #'equal)
                      (vectorp models)))))
      ;; A model whose API only the coordinator has is not one a lane can
      ;; run, however ready it is here.
      (let ((evo.swarm::*lane-forms* nil))
        (set-setting :model "outer-model")
        (set-setting :model-provider :outer)
        (let* ((plan (evo.swarm::lane-plan nil session-agent))
               (entry (evo.swarm::check-lane-entry plan))
               (problem (nth-value 1 (evo.swarm::check-lane-entry plan))))
          (check "lane plan: a model whose API no lane code loaded is not runnable there"
                 (and (not (getf entry :ok))
                      (equal "lane_api_missing" (getf problem :code))
                      (search "in-lanes" (getf entry :reason))))
          (check "lane plan: ...and its own reason names the API"
                 (search "lane-plan-outer" (getf entry :reason)))
          ;; The levels are the registration's, not the verdict's: a client's
          ;; menu states them whether or not the lane can run the model.
          (check "lane plan: ...and a model the lane cannot run still states its levels"
                 (equalp #("high") (getf entry :effort-levels)))))
      ;; A form that fails is a problem, not a crash — and it does not cost
      ;; the rest of the answer.
      (set-setting :model "m-a")
      (set-setting :model-provider nil)
      (let ((evo.swarm::*lane-forms* nil))
        (evo.swarm:in-lanes () (error "this swarm.lisp form is broken"))
        (let* ((plan (evo.swarm::lane-plan nil session-agent))
               (problem (first (evo.swarm::lane-plan-problems plan))))
          (check "lane plan: a form that fails becomes a problem"
                 (equal "lane_config_failed" (getf problem :code)))
          (check "lane plan: ...and the rest of the answer is still there"
                 (equal "m-a" (evo.swarm::lane-plan-model-id plan)))))
      ;; Nothing of the evaluation reached the coordinator: the whole
      ;; runtime — the registries and the settings — is the one it had.
      (let ((models (all-models))
            (providers (copy-alist evo.provider::*providers*))
            (apis (api-keys))
            (settings (capture-settings)))
        (evo.swarm::lane-plan nil session-agent)
        (check "lane plan: the coordinator's models are untouched"
               (equal models (all-models)))
        (check "lane plan: ...its providers"
               (equal providers evo.provider::*providers*))
        (check "lane plan: ...its API registry, extension APIs included"
               (equal apis (api-keys)))
        (check "lane plan: ...its settings"
               (equal settings (capture-settings)))))))

(defun test-offline-resume-workers ()
  "The lane count the offline answers resolve (swarm/offline.lisp): an explicit
--workers, else the roster a resumed swarm records, else the :swarm-workers
setting, else 6.  A resumed swarm keeps its recorded roster, so a smaller
--workers never shrinks it and an inflated config default never enlarges it —
and that count is what the lanes' in-lanes code is bound to."
  (with-registries ()
    (register-provider* :stub :base-url "http://127.0.0.1:1" :api-key-env "STUB_KEY")
    (register-model* "m-a" :provider :stub :context-window 1000 :max-output 100)
    (let ((record2 (list :lanes (vector (list :n 1) (list :n 2))))
          (record5 (list :lanes (vector (list :n 1) (list :n 2) (list :n 3)
                                        (list :n 4) (list :n 5)))))
      ;; No record: evo's own chain, what a fresh swarm resolves.
      (check "offline workers: with nothing named, six"
             (eql 6 (evo.swarm::resolved-workers nil)))
      (check "offline workers: a fresh swarm takes the config default"
             (progn (set-setting :swarm-workers 4)
                    (eql 4 (evo.swarm::resolved-workers nil))))
      (check "offline workers: --workers beats the config default"
             (eql 7 (evo.swarm::resolved-workers '(:workers 7))))
      ;; A resumed record is the floor; only an explicit --workers raises it.
      (check "offline workers: a resumed record is the count"
             (eql 2 (evo.swarm::resolved-workers nil record2)))
      (check "offline workers: --workers above the record raises the count"
             (eql 5 (evo.swarm::resolved-workers '(:workers 5) record2)))
      (check "offline workers: --workers below the record does not shrink it"
             (eql 5 (evo.swarm::resolved-workers '(:workers 2) record5)))
      (check "offline workers: --workers at the record is the record"
             (eql 5 (evo.swarm::resolved-workers '(:workers 5) record5)))
      (check "offline workers: an inflated config default does not enlarge a resume"
             (progn (set-setting :swarm-workers 9)
                    (and (eql 2 (evo.swarm::resolved-workers nil record2))
                         (eql 3 (evo.swarm::resolved-workers '(:workers 3) record2)))))
      (check "offline workers: a record with no roster is no count"
             (null (evo.swarm::recorded-lane-count '(:lane-model "m-a"))))
      (check "offline workers: ...and an empty roster is zero"
             (eql 0 (evo.swarm::recorded-lane-count (list :lanes #()))))
      ;; CHECK-WORKERS, which is what `check` reports as :workers.
      (check "check: no flag and a resumed record -> the record"
             (eql 2 (evo.swarm::check-workers nil record2)))
      (check "check: ...--workers above it"
             (eql 5 (evo.swarm::check-workers '(:workers 5) record2)))
      (check "check: ...--workers below it"
             (eql 5 (evo.swarm::check-workers '(:workers 2) record5)))
      (check "check: ...--workers at it, and no problem"
             (and (eql 5 (evo.swarm::check-workers '(:workers 5) record5))
                  (null (nth-value 1 (evo.swarm::check-workers '(:workers 5) record5)))))
      (check "check: a resumed record raises no problem of its own"
             (null (nth-value 1 (evo.swarm::check-workers nil record2))))
      (check "check: an invalid --workers is a problem, record or not"
             (equal "invalid_workers"
                    (getf (nth-value 1 (evo.swarm::check-workers '(:workers 999) record2))
                          :code))))
    ;; LANE-SETUP-FORMS-FOR is the half LANE-PLAN (and `check`) evaluates; its
    ;; in-lanes bindings are the resumed count, with :swarm-workers still at the
    ;; 9 from above.
    (let* ((agent (fresh-agent))
           (evo:*agent* agent)
           (evo.swarm::*lane-forms* nil))
      (evo:set-custom-state "swarm"
                            (list :lane-model "m-a" :lane-provider :stub
                                  :lanes (vector (list :n 1) (list :n 2) (list :n 3)))
                            agent)
      (evo.swarm:in-lanes (n total)
        (progn
          (evo:register-model (format nil "lane-count-~d" total)
                              :provider :stub :context-window 1000 :max-output 100)
          (list :lane n :lanes total)))
      (let ((plan (evo.swarm::lane-plan nil agent t)))
        (check "lane plan: its sandbox binds in-lanes to the resumed count"
               (and (find "lane-count-3" (evo.swarm::lane-plan-models plan)
                          :key (lambda (m) (getf m :id)) :test #'equal)
                    (not (find "lane-count-9" (evo.swarm::lane-plan-models plan)
                               :key (lambda (m) (getf m :id)) :test #'equal)))))
      (let* ((forms (evo.swarm::lane-setup-forms-for nil agent t))
             (probe (find-if (lambda (form)
                               (search "list :lane"
                                       (evo.swarm::forms->code (list form))))
                             forms)))
        (check "lane plan: ...and the lane's form itself names the lane and that count"
               (equal (list :lane 1 :lanes 3) (eval probe)))))))

;;; Runtime lane growth (lanes.lisp, tui.lisp, main.lisp).  The roster can
;;; grow while the swarm runs — `/lanes N`, and an explicit `--workers` on
;;; resume — so the order of the journal write, the lane-topic registration and
;;; the roster publication is what a crash in the middle leaves behind.  A
;;; count that is not larger is a no-op (growth only), and the default lane
;;; number on resume is the recorded one, never a growth target.
;;;
;;; Nothing here starts a process: LAUNCH-LANE and START-LANES are replaced.

(defun eventually (predicate &key (seconds 5))
  "True once PREDICATE does, within SECONDS: START-LANES boots its lanes on
threads of its own, and the replacements here finish at once."
  (loop repeat (* seconds 100)
        when (funcall predicate) return t
        do (sleep 0.01)))

(defun lane-numbers (swarm)
  "The lane numbers of SWARM's roster, in order."
  (mapcar #'lane-n (swarm-lanes swarm)))

;;; A thread-safe recorder for LANE-LAUNCH: START-LANES starts one thread per
;;; lane, so the calls must be collected under a lock (an unsynchronised APPEND
;;; loses updates) and the threads waited for before LAUNCH-LANE is put back —
;;; the recorder must not be uninstalled while one is still running.
;;; Cross-thread order is not meaningful, so callers compare sorted numbers.

(defstruct (launch-recorder (:constructor make-launch-recorder ()))
  (lock (bt:make-lock "launch-recorder"))
  entries      ; (LANE-N RESUME) per call, newest first (push order is arbitrary)
  lanes)       ; the lane objects, for waiting on their threads

(defun recorder-add (recorder entry lane)
  (bt:with-lock-held ((launch-recorder-lock recorder))
    (push entry (launch-recorder-entries recorder))
    (pushnew lane (launch-recorder-lanes recorder))))

(defun recorder-calls (recorder)
  "RECORDER's calls, one (LANE-N RESUME) each."
  (bt:with-lock-held ((launch-recorder-lock recorder))
    (reverse (launch-recorder-entries recorder))))

(defun recorder-lane-numbers (recorder)
  "The lanes RECORDER saw called for, sorted: the calls come from the swarm's
own threads, so their collection order proves nothing."
  (sort (mapcar #'first (recorder-calls recorder)) #'<))

(defun settle-lane-thread (lane)
  "Wait for the thread START-LANES started for LANE to finish, so a recorder
can be uninstalled without a launch still running."
  (let ((thread (evo.swarm::lane-subscriber lane)))
    (when thread
      (loop repeat 500 while (bt:thread-alive-p thread) do (sleep 0.01)))))

(defun settle-launch-threads (recorder)
  (dolist (lane (bt:with-lock-held ((launch-recorder-lock recorder))
                  (copy-list (launch-recorder-lanes recorder))))
    (settle-lane-thread lane)))

(defmacro with-lane-recorder ((recorder) &body body)
  "BODY with LAUNCH-LANE replaced: RECORDER collects the lanes called for, and
no process is started.  Its threads are waited for and the function put back
when BODY is done."
  `(let ((,recorder (make-launch-recorder))
         (saved (symbol-function 'launch-lane)))
     (unwind-protect
          (progn
            (setf (symbol-function 'launch-lane)
                  (lambda (lane &key resume)
                    (recorder-add ,recorder (list (lane-n lane) resume) lane)
                    nil))
            ,@body)
       (settle-launch-threads ,recorder)
       (setf (symbol-function 'launch-lane) saved))))

(defmacro with-start-lanes-recorder ((calls &key agent) &body body)
  "BODY with START-LANES replaced: CALLS collects one plist per call — :SWARM,
:RESUME, :LANES, and, with AGENT, :RECORD, the swarm journal as it read at
boot time.  Nothing is launched; the caller is START-LANES' own thread, so the
collection is not shared."
  `(let ((,calls nil)
         (saved (symbol-function 'start-lanes)))
     (unwind-protect
          (progn
            (setf (symbol-function 'start-lanes)
                  (lambda (swarm &key resume (lanes (swarm-lanes swarm)))
                    (setf ,calls
                          (append ,calls
                                  (list (list :swarm swarm :resume resume :lanes lanes
                                              :record (and ,agent
                                                           (evo:custom-state "swarm" ,agent))))))
                    t))
            ,@body)
       (setf (symbol-function 'start-lanes) saved))))

(defun test-start-lanes-subset ()
  "START-LANES takes the lanes to start, not the whole roster: growth starts
only the lanes it added.  It starts nothing while the swarm is stopping, and
its lanes come from the swarm it was given rather than whatever *SWARM* is
when its threads run.  A lane whose process cannot start is marked down and
the coordinator is told, without taking another lane with it."
  (let* ((*swarm* (test-swarm :workers 4))
         (lanes (swarm-lanes *swarm*)))
    (with-lane-recorder (rec)
      (check "start-lanes: it launches exactly the lanes it is given"
             (progn (start-lanes *swarm* :lanes (list (third lanes) (fourth lanes)))
                    (and (eventually (lambda () (= 2 (length (recorder-calls rec)))))
                         (equal '(3 4) (recorder-lane-numbers rec))))))
    (with-lane-recorder (rec)
      (check "start-lanes: the default is the whole roster, with RESUME"
             (progn (start-lanes *swarm* :resume t)
                    (and (eventually (lambda () (= 4 (length (recorder-calls rec)))))
                         (equal '(1 2 3 4) (recorder-lane-numbers rec))
                         (every #'second (recorder-calls rec))))))
    ;; A stopping swarm starts nothing: the check is under the swarm lock,
    ;; before any thread exists.
    (let ((*swarm* (test-swarm :workers 2)))
      (evo.swarm::with-swarm-lock () (setf (evo.swarm::swarm-stopping *swarm*) t))
      (with-lane-recorder (rec)
        (check "start-lanes: a stopping swarm starts no lane"
               (progn (start-lanes *swarm*)
                      (sleep 0.3)
                      (null (recorder-calls rec)))))))
  ;; The threads close over their SWARM argument: *SWARM* is NIL here, so a
  ;; session switch rebinding it cannot send the lanes to the wrong swarm.
  (let ((*swarm* nil)
        (other (test-swarm :workers 2)))
    (with-lane-recorder (rec)
      (check "start-lanes: it starts the swarm it was given, not *SWARM*"
             (progn (start-lanes other :lanes (list (second (swarm-lanes other))))
                    (and (eventually (lambda () (= 1 (length (recorder-calls rec)))))
                         (equal '(2) (recorder-lane-numbers rec)))))))
  ;; A launch that fails: the lane is marked down and said so, and the failure
  ;; does not escape its own thread.
  (let* ((view (make-instance 'recording-view))
         (*swarm* (test-swarm :workers 2 :view view))
         (lane (first (swarm-lanes *swarm*)))
         (saved (symbol-function 'launch-lane)))
    (unwind-protect
         (progn
           (setf (symbol-function 'launch-lane)
                 (lambda (lane &key resume)
                   (declare (ignore lane resume))
                   (error "no evo-agent binary")))
           (start-lanes *swarm* :lanes (list lane))
           (check "start-lanes: a lane whose process cannot start is marked down"
                  (eventually (lambda () (eq :down (lane-state lane)))))
           (settle-lane-thread lane)
           (check "start-lanes: ...and the coordinator is told"
                  (some (lambda (said) (search "no evo-agent binary" (first said)))
                        (said view))))
      (settle-lane-thread lane)
      (setf (symbol-function 'launch-lane) saved))))

(defun test-lane-growth ()
  "ENSURE-LANE-COUNT stages the expanded roster: it writes the journal with
the new lanes while the LIVE swarm still holds the old one, registers the new
lane topics, and only then installs the roster — so nothing a client can read
names a lane it has no topic for, and a crash in the middle leaves the swarm
on the roster it had.  The new lanes are appended after the old ones, which
are kept; a count that is not larger is a no-op."
  (let* ((agent (fresh-agent))
         (server (evo.serve:make-server :port 0 :token "t"))
         (*swarm* (test-swarm :agent agent :server server :workers 2))
         (old (copy-list (swarm-lanes *swarm*)))
         (published nil)
         ;; Any lane row a swarm state.patch named while that lane's topic was
         ;; not registered: growth must publish nothing a client cannot read.
         (uncovered nil)
         ;; What the LIVE swarm looked like on each journal write.
         (live-at-persist nil)
         (saved-publish (symbol-function 'evo.swarm::publish-op))
         (saved-set (symbol-function 'evo:set-custom-state)))
    (unwind-protect
         (progn
           (register-swarm-topics server *swarm*)
           ;; What the topic registry held when the roster went out: no swarm
           ;; state.patch may name a lane the server has no topic for.
           (setf (symbol-function 'evo.swarm::publish-op)
                 (lambda (op)
                   (setf published (append published (list op)))
                   (when (and (equal "swarm" (getf op :topic))
                              (equal "state.patch" (getf op :op)))
                     (loop for row across (getf (getf op :patch) :lanes)
                           unless (evo.serve:topic-provider
                                   server (format nil "lane:~d" (getf row :n)))
                             do (pushnew (getf row :n) uncovered)))
                   t))
           ;; The journal write is instrumented and still goes through: at the
           ;; moment the new roster is persisted the LIVE swarm must be the old
           ;; one — the expanded roster is staged, not installed.
           (setf (symbol-function 'evo:set-custom-state)
                 (lambda (&rest args)
                   (push (list :workers (evo.swarm::swarm-workers *swarm*)
                               :lanes (length (swarm-lanes *swarm*)))
                         live-at-persist)
                   (apply saved-set args)))
           (with-start-lanes-recorder (calls :agent agent)
             (check "growth: it answers how many lanes it added"
                    (eql 3 (evo.swarm::ensure-lane-count *swarm* 5)))
             (check "growth: the roster grew, and the count with it"
                    (and (eql 5 (evo.swarm::swarm-workers *swarm*))
                         (equal '(1 2 3 4 5) (lane-numbers *swarm*))))
             (check "growth: the old lanes are the same objects, not rebuilt"
                    (and (eq (first (swarm-lanes *swarm*)) (first old))
                         (eq (second (swarm-lanes *swarm*)) (second old))))
             (check "growth: a new lane gets a directory of its own under the swarm"
                    (equal (namestring (merge-pathnames "lane-5/" (swarm-dir *swarm*)))
                           (namestring (evo.swarm::lane-dir (fifth (swarm-lanes *swarm*))))))
             (check "growth: a new lane starts in the swarm's directory, with no task, worktree or evals"
                    (let ((lane (fifth (swarm-lanes *swarm*))))
                      (and (equal (namestring (evo.swarm::swarm-cwd *swarm*))
                                  (namestring (lane-cwd lane)))
                           (null (lane-task lane))
                           (null (lane-worktree lane))
                           (null (evo.swarm::lane-extra-forms lane)))))
             (check "growth: it boots only the lanes it added"
                    (and (= 1 (length calls))
                         (equal '(3 4 5) (mapcar #'lane-n (getf (first calls) :lanes)))))
             (check "growth: ...on the swarm it grew"
                    (eq *swarm* (getf (first calls) :swarm)))
             (check "growth: the journal names every lane before anything boots"
                    (let ((record (getf (first calls) :record)))
                      (and (eql 5 (getf record :workers))
                           (= 5 (length (getf record :lanes)))))))
           ;; The staging, in one place: durable first (written against the
           ;; live old roster), then the topics, then the install that is news
           ;; to a client.
           (check "growth: the journal was written against the live old roster"
                  (equal '((:workers 2 :lanes 2)) live-at-persist))
           (check "growth: the live roster is installed only after the journal"
                  (and (eql 5 (evo.swarm::swarm-workers *swarm*))
                       (equal '(1 2 3 4 5) (lane-numbers *swarm*))))
           (check "growth: every state.patch named only lanes with a topic"
                  (and published (null uncovered)))
           (check "growth: the new lane topics are registered"
                  (every (lambda (n) (evo.serve:topic-provider
                                      server (format nil "lane:~d" n)))
                         '(3 4 5)))
           (check "growth: the roster is republished on the swarm topic"
                  (let ((patch (car (last (remove-if-not
                                           (lambda (op) (equal "swarm" (getf op :topic)))
                                           published)))))
                    (and patch
                         (eql 5 (getf (getf patch :patch) :workers))
                         (= 5 (length (getf (getf patch :patch) :lanes))))))
           ;; Not larger: a no-op, and nothing is booted.
           (with-start-lanes-recorder (calls)
             (check "growth: the same count is a no-op"
                    (and (eql 0 (evo.swarm::ensure-lane-count *swarm* 5))
                         (null calls)
                         (equal '(1 2 3 4 5) (lane-numbers *swarm*))))
             (check "growth: a smaller count is a no-op, never a shrink"
                    (and (eql 0 (evo.swarm::ensure-lane-count *swarm* 2))
                         (null calls)
                         (equal '(1 2 3 4 5) (lane-numbers *swarm*))))))
      (setf (symbol-function 'evo.swarm::publish-op) saved-publish
            (symbol-function 'evo:set-custom-state) saved-set))))

;;; A frontend host for the command layer's idle guard: `/lanes N` is a change
;;; to what the coordinator waits on, so it asks the HOST whether a task is
;;; running (EVO.COMMAND:REQUIRE-IDLE) — a busy *lane* is exactly when more
;;; lanes help.

(defclass test-command-host ()
  ((busy :initform nil :accessor test-host-busy)))

(defmethod evo.command:host-running-p ((host test-command-host))
  (test-host-busy host))

(defun test-lanes-command ()
  "`/lanes N` grows the swarm to N lanes: 1..64 total, N at or below the
current count is a no-op string (never a shrink), an argument that is not a
count in range is refused :invalid, and growth needs the coordinator idle —
refused :conflict while the host runs a task.  A busy *lane* is not a reason
to refuse: that is exactly when more lanes help."
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 2))
         (fn (progn (register-swarm-commands)
                    (getf (find-command "lanes") :fn)))
         (host (make-instance 'test-command-host))
         (answer (lambda (args) (funcall fn (list :args args :host host))))
         (refusal (lambda (args)
                    (handler-case (progn (funcall fn (list :args args :host host)) :none)
                      (evo.command:command-refused (c) c)))))
    (check "lanes: the command is registered" (functionp fn))
    ;; Listing still works with no argument.
    (check "lanes: bare /lanes still lists the lanes"
           (let ((text (funcall answer "")))
             (and (stringp text) (search "lane 1" text) (search "lane 2" text))))
    (with-start-lanes-recorder (calls :agent agent)
      (check "lanes: /lanes 5 grows the roster and answers"
             (and (stringp (funcall answer "5"))
                  (eql 5 (evo.swarm::swarm-workers *swarm*))
                  (equal '(1 2 3 4 5) (lane-numbers *swarm*))
                  (equal '(3 4 5) (mapcar #'lane-n (getf (first calls) :lanes))))))
    (with-start-lanes-recorder (calls)
      (check "lanes: the current count is a no-op, not a refusal"
             (and (stringp (funcall answer "5"))
                  (null calls)
                  (equal '(1 2 3 4 5) (lane-numbers *swarm*)))))
    (with-start-lanes-recorder (calls)
      (check "lanes: a smaller count is a no-op, never a shrink"
             (and (stringp (funcall answer "2"))
                  (null calls)
                  (equal '(1 2 3 4 5) (lane-numbers *swarm*)))))
    ;; Out of range or not a number: refused :invalid, and nothing changes.
    (with-start-lanes-recorder (calls)
      (dolist (bad '("0" "65" "1000" "-3" "abc" "2.5"))
        (let ((refusal (funcall refusal bad)))
          (check (format nil "lanes: /lanes ~s is refused :invalid" bad)
                 (and (typep refusal 'evo.command:command-refused)
                      (eq :invalid (evo.command:command-refused-kind refusal))
                      (null calls)
                      (eql 5 (evo.swarm::swarm-workers *swarm*))
                      (equal '(1 2 3 4 5) (lane-numbers *swarm*))))))
    (let* ((bounded-agent (fresh-agent))
           (*swarm* (test-swarm :agent bounded-agent :workers 2)))
      (with-start-lanes-recorder (calls :agent bounded-agent)
        (check "lanes: 64 is within the bound"
               (and (stringp (funcall answer "64"))
                    (eql 64 (evo.swarm::swarm-workers *swarm*))))))
    ;; The coordinator is the one that may not be mid-run; a busy lane is fine.
    (let ((*swarm* (test-swarm :agent agent :workers 2)))
      (setf (test-host-busy host) t)
      (with-start-lanes-recorder (calls)
        (let ((refusal (funcall refusal "4")))
          (check "lanes: a busy coordinator refuses growth :conflict"
                 (and (typep refusal 'evo.command:command-refused)
                      (eq :conflict (evo.command:command-refused-kind refusal))
                      (null calls)
                      (eql 2 (evo.swarm::swarm-workers *swarm*))))))
      (setf (test-host-busy host) nil)
      (evo.swarm::with-swarm-lock ()
        (setf (lane-state (first (swarm-lanes *swarm*))) :working))
      (with-start-lanes-recorder (calls :agent agent)
        (check "lanes: a busy lane does not block growth"
               (and (stringp (funcall answer "4"))
                    (eql 4 (evo.swarm::swarm-workers *swarm*))
                    (equal '(3 4) (mapcar #'lane-n (getf (first calls) :lanes))))))))))

(defun test-resume-workers-minimum ()
  "A resumed swarm comes back at the count its record names, whatever number
the launch passed.  An explicit minimum grows it through the same routine; the
default (no --workers) is not a minimum at all, or every resume of a two-lane
swarm would grow to six."
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent :workers 2)))
    (record-swarm)
    (let* ((record (evo:custom-state "swarm" agent))
           (restored (evo.swarm::make-swarm :agent agent :workers 9 :record record
                                            :evo-binary "/x/evo")))
      (check "resume: the record decides the count, not the launch number"
             (and (eql 2 (evo.swarm::swarm-workers restored))
                  (equal '(1 2) (lane-numbers restored))))
      (check "resume: ...and the record names it"
             (and (eql 2 (getf record :workers))
                  (= 2 (length (getf record :lanes)))))
      (let ((old (copy-list (swarm-lanes restored))))
        (let ((*swarm* restored))
          (with-start-lanes-recorder (calls :agent agent)
            (check "resume: an explicit higher minimum grows the swarm"
                   (eql 2 (evo.swarm::ensure-lane-count restored 4)))
            (check "resume: ...keeping the recorded lanes"
                   (and (eq (first (swarm-lanes restored)) (first old))
                        (equal '(1 2 3 4) (lane-numbers restored))
                        (equal '(3 4) (mapcar #'lane-n (getf (first calls) :lanes)))))))
        (let ((*swarm* restored))
          (with-start-lanes-recorder (calls)
            (check "resume: a minimum at the count is a no-op"
                   (and (eql 0 (evo.swarm::ensure-lane-count restored 4))
                        (null calls)))
            (check "resume: a minimum below the count never shrinks"
                   (and (eql 0 (evo.swarm::ensure-lane-count restored 1))
                        (equal '(1 2 3 4) (lane-numbers restored))))))))))

(defun test-lane-growth-journal-failure ()
  "Growth journals before it boots anything, so a journal write that fails
leaves no trace: the live roster is untouched (the expanded one was only
staged), nothing boots, no lane topic appears and the swarm topic is not
republished.  The error reaches the caller — growth records with :required,
where an ordinary shape update tolerates a failed write."
  (let* ((agent (fresh-agent))
         (server (evo.serve:make-server :port 0 :token "t"))
         (*swarm* (test-swarm :agent agent :server server :workers 2))
         (published nil)
         (saved-set (symbol-function 'evo:set-custom-state))
         (saved-publish (symbol-function 'evo.swarm::publish-op)))
    (unwind-protect
         (progn
           (register-swarm-topics server *swarm*)
           (setf (symbol-function 'evo.swarm::publish-op)
                 (lambda (op) (setf published (append published (list op))) t)
                 (symbol-function 'evo:set-custom-state)
                 (lambda (&rest args) (declare (ignore args)) (error "the journal is down")))
           (check "record: an ordinary shape update tolerates a failed journal write"
                  (handler-case (progn (record-swarm) t) (error () nil)))
           (check "record: growth's record does not"
                  (handler-case (progn (record-swarm :required t) nil) (error () t)))
           (with-start-lanes-recorder (calls)
             (check "growth: a failed journal write fails the growth"
                    (handler-case (progn (evo.swarm::ensure-lane-count *swarm* 5) nil)
                      (error () t)))
             (check "growth: ...the live roster is untouched"
                    (and (eql 2 (evo.swarm::swarm-workers *swarm*))
                         (equal '(1 2) (lane-numbers *swarm*))))
             (check "growth: ...nothing boots"
                    (null calls)))
           (check "growth: ...no lane topic is registered"
                  (null (evo.serve:topic-provider server "lane:3")))
           (check "growth: ...the swarm topic is not republished"
                  (null (remove-if-not (lambda (op) (equal "swarm" (getf op :topic)))
                                       published))))
      (setf (symbol-function 'evo:set-custom-state) saved-set
            (symbol-function 'evo.swarm::publish-op) saved-publish))))

(defun test-worker-note-growth ()
  "A grown swarm's count reaches the lanes' prompt note, and never goes
backwards: the form a restarted lane evaluates names the new count, and a
smaller count arriving later — an initialization racing the growth — does not
put the old note back."
  (with-registries ()
    (let ((lane (evo.swarm::%make-lane :n 7)))
      (labels ((apply-note (workers)
                 (eval (evo.swarm::worker-note-form lane workers))
                 (cdr (assoc "swarm-worker" (evo.kernel::prompt-notes-snapshot)
                             :test #'equal))))
        (eval '(defvar evo.user::*swarm-worker-count* 0))
        (setf (symbol-value 'evo.user::*swarm-worker-count*) 0)
        (let ((six (apply-note 6)))
          (check "note: growth names the lane's count in its note"
                 (and (stringp six) (search "7 of 6" six)))
          (check "note: a later smaller count does not put the old note back"
                 (equal six (apply-note 4)))
          (check "note: a later larger count does"
                 (let ((eight (apply-note 8)))
                   (and (stringp eight) (search "7 of 8" eight)
                        (not (equal six eight))))))))))

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
    (test-bring-up-announces)
    (test-bring-up-failure-cleanup)
    (test-topics)
    (test-interrupt)
    (test-tools)
    (test-lane-launch-args)
    (test-view)
    (test-serve-view)
    (test-record)
    (test-session-switch)
    ;; Growing the roster while the swarm runs (lanes.lisp, tui.lisp,
    ;; main.lisp): /lanes N, the resume minimum, and the growth routine.
    (test-start-lanes-subset)
    (test-lane-growth)
    (test-lane-growth-journal-failure)
    (test-worker-note-growth)
    (test-lanes-command)
    (test-resume-workers-minimum)
    (test-launch-environment)
    (test-sessions-dir)
    (test-cli)
    (test-serve-cli)
    (test-serve-exit-codes)
    (test-sse-reader)
    (test-pid-alive)
    (test-sample-config)
    ;; The two questions the GUI asks before it starts a swarm, answered
    ;; offline (swarm/offline.lisp, CONTRACT §2).
    (test-swarm-check)
    (test-lane-plan)
    (test-offline-resume-workers)
    (format t "~%swarm: ~d passed, ~d failed~%" *pass* *fail*)
    (if (zerop *fail*) 0 1)))
