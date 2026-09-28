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

(defun test-swarm (&key (workers 3) agent view)
  (let ((swarm (evo.swarm::%make-swarm
                :id "unit" :workers workers :agent agent :view view
                :dir (uiop:ensure-directory-pathname
                      (format nil "~a/evo-swarm-unit-~a/" (tmp-dir) (gen-id)))
                :cwd (uiop:getcwd))))
    (setf (evo.swarm::swarm-lanes swarm)
          (loop for n from 1 to workers
                collect (evo.swarm::%make-lane :n n :port (+ 20000 n) :token "t"
                                               :cwd (uiop:getcwd))))
    swarm))

(defun read-all (code)
  "Every form in CODE, read as a lane reads it: EVO.USER, no #. evaluation."
  (let ((*package* (find-package :evo.user)) (*read-eval* nil))
    (with-input-from-string (in code)
      (loop for form = (read in nil :eof) until (eq form :eof) collect form))))

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

(defmethod view-publish ((view recording-view) event)
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

(defun test-events ()
  (let* ((agent (fresh-agent))
         (evo.kernel:*frontend* nil)
         (recording (make-instance 'recording-view))
         (*swarm* (test-swarm :agent agent :view recording))
         (lane (first (swarm-lanes *swarm*))))
    (evo.swarm::handle-lane-event lane "task-start" '(:type "task-start" :kind "run"))
    (check "events: a task start makes the lane working"
           (eq :working (lane-state lane)))
    (check "events: ...and starts its step clock"
           (evo.swarm::lane-step-started lane))
    (check "events: ...and repaints the frontend"
           (= 1 (slot-value recording 'repaints)))
    (check "events: running is not news for the coordinator"
           (not (steering-pending-p agent)))
    (evo.swarm::handle-lane-event lane "report"
                                  '(:type "report" :done "built it" :evidence "make ok"))
    (check "events: a report becomes coordinator input"
           (let ((queued (evo.kernel::agent-steering agent)))
             (and queued (search "[lane 1 report] done: built it"
                                 (getf (first queued) :text))
                  (search "evidence: make ok" (getf (first queued) :text)))))
    (check "events: the report is shown through the swarm's view, not the TUI"
           (let ((said (slot-value recording 'said)))
             (and said (search "[lane 1 report] done: built it" (caar said))
                  (eq :notice (second (first said))))))
    (evo.kernel::drain-steering agent)
    (evo.swarm::handle-lane-event lane "report"
                                  '(:type "report" :done "all of it" :goal "complete"))
    (check "events: a report says the lane's goal status"
           (search "goal: complete" (getf (first (evo.kernel::agent-steering agent)) :text)))
    (evo.kernel::drain-steering agent)
    (evo.swarm::handle-lane-event lane "settled" '(:type "settled" :outcome "stop"))
    (check "events: settling makes the lane idle" (eq :idle (lane-state lane)))
    (check "events: ...and tells the coordinator the run ended"
           (let ((text (getf (first (evo.kernel::agent-steering agent)) :text)))
             (and (search "[lane 1] run ended (stop)" text)
                  (not (search "goal:" text)))))
    (evo.kernel::drain-steering agent)
    (evo.swarm::handle-lane-event lane "settled"
                                  '(:type "settled" :outcome "stop" :goal "complete"))
    (check "events: a run end says a lane's goal is complete"
           (search "[lane 1] run ended (stop) — goal: complete"
                   (getf (first (evo.kernel::agent-steering agent)) :text)))
    (evo.kernel::drain-steering agent)
    (evo.swarm::handle-lane-event lane "settled"
                                  '(:type "settled" :outcome "aborted" :goal "active"))
    (check "events: ...and that a stopped lane with an active goal waits to be steered"
           (search "goal: active, but the lane is idle until steered"
                   (getf (first (evo.kernel::agent-steering agent)) :text)))
    (evo.kernel::drain-steering agent)
    (evo.swarm::handle-lane-event lane "task-end" '(:type "task-end" :error "boom"))
    (check "events: an error in a lane reaches the coordinator"
           (search "[lane 1] error: boom"
                   (getf (first (evo.kernel::agent-steering agent)) :text)))
    (evo.kernel::drain-steering agent)
    (evo.swarm::handle-lane-event lane "text-delta" '(:type "text-delta" :text "hi"))
    (check "events: streamed text is not coordinator input"
           (not (steering-pending-p agent)))))

;;; The frontend seam (view.lisp).  What the swarm calls to be seen or run: a
;;; notice, a repaint, a machine event, the run itself — each routed to the
;;; swarm's view, and a no-op when no swarm is up.

(defun test-view ()
  (let ((*swarm* nil))
    (check "view: no swarm, no notice" (null (swarm-say "x")))
    (check "view: no swarm, no repaint" (null (swarm-repaint)))
    (check "view: no swarm, no event" (null (swarm-publish (list :type :lane-state)))))
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
    (swarm-publish (list :type :lane-state :lane 2 :state :working))
    (check "view: a machine event reaches the view as the plist it was given"
           (equal '((:type :lane-state :lane 2 :state :working))
                  (reverse (slot-value recording 'events))))
    (check "view: the run goes to the view, and its exit code comes back"
           (and (eql 7 (swarm-run agent t))
                (equal (list agent t) (slot-value recording 'ran)))))
  ;; The TUI view with no terminal up: nothing painted, nothing published, and
  ;; — the point of the seam — nothing signalled.
  (let ((view (make-instance 'tui-view)))
    (check "view: a TUI notice is dropped when no TUI is running"
           (null (view-say view "x")))
    (check "view: a TUI repaint is dropped when no TUI is running"
           (null (view-repaint view)))
    (check "view: the TUI has no event stream"
           (null (view-publish view (list :type :lane-state))))))

(defun test-serve-view ()
  "A serve view says through the command layer's host protocol and publishes
through serve's own log: the call site never names the server's internals."
  (let* ((server (evo.serve:make-server :port 0 :token "t"))
         (view (make-instance 'serve-view :server server)))
    (check "serve view: it names its server" (eq server (serve-view-server view)))
    (view-say view "lane 2 is working" :style :notice)
    (view-publish view (list :type :lane-state :lane 2 :state :working))
    (let ((events (evo.serve::events-after (evo.serve::server-log server) 0)))
      (check "serve view: a notice is one :output event on the server's log"
             (and (= 2 (length events))
                  (equal "output" (second (first events)))
                  (search "\"lane 2 is working\"" (third (first events)))
                  (search "\"style\":\"notice\"" (third (first events)))))
      (check "serve view: a published event keeps its own type and fields"
             (and (equal "lane-state" (second (second events)))
                  (search "\"lane\":2" (third (second events)))
                  (search "\"state\":\"working\"" (third (second events))))))
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
    (evo.swarm::record-swarm)
    (let ((record (evo:custom-state "swarm" agent)))
      (check "record: the swarm is journaled on the coordinator's session"
             (and record (equal "unit" (getf record :id)) (= 2 (getf record :workers))))
      (let ((restored (evo.swarm::make-swarm :agent agent :workers 9 :record record
                                             :evo-binary "/x/evo")))
        (check "record: a resumed swarm keeps its id, directory and lane count"
               (and (equal "unit" (swarm-id restored))
                    (equal (namestring (swarm-dir *swarm*)) (namestring (swarm-dir restored)))
                    (= 2 (length (swarm-lanes restored)))))
        (let ((two (second (swarm-lanes restored))))
          (check "record: a lane's worktree, branch, cwd, task and evals come back"
                 (and (equal "/wt/lane-2/" (lane-worktree two))
                      (equal "/wt/lane-2/" (namestring (lane-cwd two)))
                      (equal "the task" (lane-task two))
                      (equal '("(defun x ())") (evo.swarm::lane-extra-forms two))))
          (check "record: a resumed lane gets a fresh token and port"
                 (and (= 64 (length (evo.swarm::lane-token two)))
                      (integerp (evo.swarm::lane-port two)))))))))

(defun test-launch-environment ()
  (let ((saved (getenv "EVO_SUPERVISED_CHILD")))
    (unwind-protect
         (let* ((*swarm* (test-swarm))
                (lane (first (swarm-lanes *swarm*))))
           (evo.port:setenv "EVO_SUPERVISED_CHILD" "1")
           (setf (evo.swarm::lane-dir lane) #p"/tmp/lane-1/"
                 (evo.swarm::lane-token lane) "tok")
           (let ((env (evo.swarm::lane-environment lane)))
             (check "launch: the coordinator's supervision is not inherited"
                    (notany (lambda (e) (string-prefix-p "EVO_SUPERVISED_CHILD=" e)) env))
             (check "launch: the lane's token, sessions and watched pid are set"
                    (and (member "EVO_SERVE_TOKEN=tok" env :test #'equal)
                         (member "EVO_SESSIONS_DIR=/tmp/lane-1/sessions/" env :test #'equal)
                         (member (format nil "EVO_SERVE_WATCH_PID=~d" (evo.port:getpid))
                                 env :test #'equal)))))
      (evo.port:setenv "EVO_SUPERVISED_CHILD" (or saved "")))))

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
  (check "cli: a restart keeps the swarm's flags and drops the session's"
         (equal '("--workers" "4" "--evo" "/x/evo" "--no-userspace")
                (remove "--resume"
                        (evo.swarm::restart-argv '("--workers" "4" "--model" "m" "--evo" "/x/evo"
                                                   "--thinking" "high" "--no-userspace"
                                                   "--resume" "/old/path"))
                        :test #'equal))))

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
                        "--token-file" "/tmp/serve.token" "--allow-remote"
                        "--workers" "2" "--evo" "/x/evo" "--no-userspace"))))
           (and (getf opts :serve)
                (equal "0.0.0.0" (getf opts :host))
                (eql 9000 (getf opts :port))
                (equal "/tmp/serve.token" (getf opts :token-file))
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
  (check "serve: a restarted coordinator keeps where it listens and its token"
         (equal '("serve" "--workers" "4" "--evo" "/x/evo" "--no-userspace"
                  "--host" "127.0.0.1" "--port" "9000" "--token-file" "/tmp/t"
                  "--allow-remote")
                (remove "--resume"
                        (evo.swarm::restart-argv
                         '("serve" "--workers" "4" "--model" "m" "--evo" "/x/evo"
                           "--thinking" "high" "--no-userspace" "--resume" "/old"
                           "--host" "127.0.0.1" "--port" "9000" "--token-file" "/tmp/t"
                           "--allow-remote"))
                        :test #'equal)))
  (check "serve: ...and does not re-pass the session's model or thinking"
         (notany (lambda (a) (member a '("--model" "--thinking") :test #'equal))
                 (evo.swarm::restart-argv '("serve" "--model" "m" "--thinking" "high")))))

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
             (check "exit: a --token-file with no path is 64"
                    (eql 64 (code "serve" "--token-file")))
             (check "exit: serve with no way to hand its token over is 64"
                    (eql 64 (code "serve")))
             (check "exit: a non-loopback --host without --allow-remote is 64"
                    (eql 64 (code "serve" "--token-file" "/tmp/serve.token"
                                  "--host" "0.0.0.0")))))
      (evo.port:setenv "EVO_NO_SUPERVISOR" (or saved ""))
      (evo.port:setenv "EVO_SERVE_TOKEN" (or saved-token "")))))

;;; The read-only HTTP API (api.lisp, routes.lisp): lane state with its cached
;;; goal, GET /lanes, a lane's transcript, and its own event stream relayed.

(defun plist-keys (value)
  "Every keyword key anywhere in VALUE — plists, lists, vectors."
  (cond ((and (consp value) (keywordp (car value)))
         (loop for (k v) on value by #'cddr append (cons k (plist-keys v))))
        ((consp value) (append (plist-keys (car value)) (plist-keys (cdr value))))
        ((and (vectorp value) (not (stringp value)))
         (loop for x across value append (plist-keys x)))
        (t nil)))

(defun get-request (path &key query headers)
  (evo.serve::%make-request :method "GET" :path path :query query
                            :headers headers :body ""))

(defun rendered (fn)
  "What FN writes to a fresh octet stream, as a string."
  (flexi-streams:octets-to-string
   (flexi-streams:with-output-to-sequence (out) (funcall fn out))))

(defun test-lane-api ()
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent))
         (lane (first (swarm-lanes *swarm*))))
    (note-lane-goal lane (list :goal-id "g1" :objective "ship it" :status "active"))
    (check "api: a cached goal reaches the lane's snapshot"
           (equal "active" (getf (getf (evo.swarm::lane-snapshot lane) :goal) :status)))
    (note-lane-goal-status lane "complete")
    (check "api: an event's goal status updates the cached goal"
           (equal "complete" (cached-lane-goal-status lane)))
    (note-lane-goal-status (second (swarm-lanes *swarm*)) "active")
    (check "api: an event naming a goal with none cached creates one"
           (equal "active" (cached-lane-goal-status (second (swarm-lanes *swarm*)))))
    (let* ((body (swarm-lanes-response))
           (lanes (getf body :lanes))
           (info (aref lanes 0))
           (keys (plist-keys info)))
      (check "api: /lanes carries the swarm and one row per lane"
             (and (= 3 (length lanes))
                  (equal "unit" (getf (getf body :swarm) :id))
                  (eql 0 (getf (getf body :swarm) :busy))))
      (check "api: a lane row has its number, state, goal and clocks"
             (and (= 1 (getf info :n))
                  (eq :starting (getf info :state))
                  (equal "complete" (getf (getf info :goal) :status))
                  (member :task-age keys)
                  (member :step-age keys)))
      (check "api: a lane row never carries a token, url, port or dir"
             (null (intersection '(:token :url :port :dir) keys))))))

(defun test-lane-state-events ()
  (let* ((agent (fresh-agent))
         (evo.kernel:*frontend* nil)
         (recording (make-instance 'recording-view))
         (*swarm* (test-swarm :agent agent :view recording))
         (lane (first (swarm-lanes *swarm*))))
    (maybe-publish-lane-state lane)
    (check "lane-state: the first shape is published through the view"
           (let ((event (first (slot-value recording 'events))))
             (and (eq :lane-state (getf event :type))
                  (= 1 (getf event :lane)))))
    (maybe-publish-lane-state lane)
    (check "lane-state: an unchanged lane publishes nothing"
           (= 1 (length (slot-value recording 'events))))
    (evo.swarm::handle-lane-event lane "task-start" '(:type "task-start" :kind "run"))
    (check "lane-state: a state change publishes again"
           (let ((event (first (slot-value recording 'events))))
             (and (= 2 (length (slot-value recording 'events)))
                  (eq :working (getf event :state)))))
    (evo.swarm::handle-lane-event lane "report"
                                  '(:type "report" :done "did it" :goal "active"))
    (check "lane-state: a goal status change publishes"
           (let ((event (first (slot-value recording 'events))))
             (and (= 3 (length (slot-value recording 'events)))
                  (equal "active" (getf event :goal)))))
    (evo.kernel::drain-steering agent)
    (evo.swarm::handle-lane-event lane "settled" '(:type "settled" :outcome "stop"))
    (check "lane-state: settling publishes the lane idle"
           (and (= 4 (length (slot-value recording 'events)))
                (eq :idle (getf (first (slot-value recording 'events)) :state))))
    (evo.kernel::drain-steering agent)
    ;; A TUI view has no event stream: publishing is a no-op, not an error.
    (let ((*swarm* (test-swarm :agent agent)))
      (check "lane-state: a view without a stream drops it"
             (null (maybe-publish-lane-state lane))))))

(defun test-swarm-json-handlers ()
  (let* ((agent (fresh-agent))
         (*swarm* (test-swarm :agent agent)))
    (note-lane-goal (first (swarm-lanes *swarm*))
                    (list :goal-id "g1" :objective "ship it" :status "active"))
    (let ((text (rendered (lambda (out)
                            (evo.swarm::handle-swarm-lanes nil nil nil out)))))
      (check "handlers: GET /lanes writes the swarm, its lanes and their goals"
             (and (search "\"lanes\"" text) (search "\"swarm\"" text)
                  (search "\"goal\"" text) (search "ship it" text)
                  (search "\"status\":\"active\"" text)))
      (check "handlers: GET /lanes writes no token and no port key"
             (and (not (search "\"token\"" text)) (not (search "\"port\"" text)))))
    (check "handlers: the bare /lanes path through the prefix route is the listing"
           (search "\"lanes\""
                   (rendered (lambda (out)
                               (evo.swarm::handle-swarm-lane-route
                                nil (get-request "/lanes") nil out)))))
    (check "handlers: an unknown action under /lanes is a JSON 404"
           (search "no such swarm endpoint"
                   (rendered (lambda (out)
                               (evo.swarm::handle-swarm-lane-route
                                nil (get-request "/lanes/3/reports") nil out)))))
    (let ((text (rendered (lambda (out)
                            (evo.swarm::handle-swarm-lane-transcript
                             nil (get-request "/lanes/9/transcript") nil out)))))
      (check "handlers: a transcript for an unknown lane is a 404 JSON error"
             (and (search "404 Not Found" text) (search "no lane 9" text))))
    (let ((text (rendered (lambda (out)
                            (evo.swarm::handle-swarm-lane-transcript
                             nil (get-request "/lanes/x/transcript") nil out)))))
      (check "handlers: a transcript without a lane number is a 400 JSON error"
             (and (search "400 Bad Request" text) (search "lane number" text))))
    ;; A reply that cannot be a stream is an HTTP error, never a 200 SSE head.
    (let ((text (rendered (lambda (out)
                            (evo.swarm::handle-swarm-lane-events
                             nil (get-request "/lanes/9/events") nil out)))))
      (check "handlers: an unknown lane's events is a 404 JSON error"
             (and (search "404 Not Found" text) (search "no lane 9" text))))
    (let ((text (rendered (lambda (out)
                            (evo.swarm::handle-swarm-lane-events
                             nil (get-request "/lanes/x/events") nil out)))))
      (check "handlers: events without a lane number is a 400, before any SSE head"
             (and (search "400 Bad Request" text)
                  (search "lane number" text)
                  (not (search "text/event-stream" text)))))
    (check "handlers: /lanes with no swarm is a JSON error"
           (let ((*swarm* nil))
             (search "no swarm"
                     (rendered (lambda (out)
                                 (evo.swarm::handle-swarm-lanes nil nil nil out))))))))

(defun test-lane-event-relay ()
  (let* ((sequence
           (flexi-streams:with-output-to-sequence (out)
             (with-input-from-string
                 (in (format nil ": hello~%~%id: 7~%event: report~%data: {\"done\":\"x\"}~%~%id: 8~%event: settled~%data: {}~%~%"))
               (check "relay: the last id relayed is returned"
                      (eql 8 (copy-lane-events in out))))))
         (text (flexi-streams:octets-to-string sequence)))
    (check "relay: ids, types, data and keepalive comments are copied through"
           (and (search ": hello" text)
                (search "id: 7" text)
                (search "event: report" text)
                (search "data: {\"done\":\"x\"}" text)
                (search "id: 8" text)
                (search "event: settled" text)))))

(defun test-swarm-route-parsing ()
  (check "routes: the lane number comes out of the path"
         (and (eql 3 (evo.swarm::route-lane "3/transcript"))
              (eql 12 (evo.swarm::route-lane "/12/events"))
              (null (evo.swarm::route-lane ""))))
  (check "routes: the action comes out of the path"
         (and (equal "/transcript" (evo.swarm::route-action "3/transcript"))
              (equal "/events" (evo.swarm::route-action "12/events"))
              (null (evo.swarm::route-action "3"))))
  (check "routes: the path below /lanes is taken whole or as a suffix"
         (and (equal "3/events" (evo.swarm::route-path (get-request "/lanes/3/events")))
              (equal "3/events" (evo.swarm::route-path (get-request "/3/events")))
              (equal "" (evo.swarm::route-path (get-request "/lanes")))
              (equal "" (evo.swarm::route-path nil))))
  ;; What a prefix route's handler reads when the router bound it.
  (let ((evo.serve:*route-tail* "3/events"))
    (check "routes: a prefix route's tail is the path below /lanes"
           (equal "3/events" (evo.swarm::route-path (get-request "/lanes/3/events")))))
  (let ((resumable (get-request "/lanes/1/events" :query '(("since" . "42"))))
        (fresh (get-request "/lanes/1/events")))
    (check "routes: ?since is the resume cursor"
           (eql 42 (evo.swarm::request-cursor resumable)))
    (check "routes: no cursor at all tails the lane live"
           (eq :live (evo.swarm::request-cursor fresh)))
    (setf (evo.serve::request-headers resumable) '(("last-event-id" . "99")))
    (check "routes: Last-Event-ID wins over ?since"
           (eql 99 (evo.swarm::request-cursor resumable)))
    (check "routes: a bad or negative event cursor is invalid"
           (and (eq :invalid (evo.swarm::request-cursor
                              (get-request "/x" :query '(("since" . "abc")))))
                (eq :invalid (evo.swarm::request-cursor
                              (get-request "/x" :query '(("since" . "-1")))))))
    (check "routes: ?limit follows serve's non-negative integer contract"
           (and (eql 5 (evo.swarm::request-limit
                        (get-request "/x" :query '(("limit" . "5")))))
                (eql 0 (evo.swarm::request-limit
                        (get-request "/x" :query '(("limit" . "0")))))
                (eq :invalid (evo.swarm::request-limit
                              (get-request "/x" :query '(("limit" . "-1")))))
                (eq :invalid (evo.swarm::request-limit
                              (get-request "/x" :query '(("limit" . "abc")))))
                (null (evo.swarm::request-limit fresh))))))

(defun test-swarm-route-registration ()
  (let ((server (evo.serve:make-server :token "t"))
        (before (length (evo.serve:server-routes (evo.serve:make-server :token "t")))))
    (declare (ignorable server))
    (register-swarm-routes)
    (check "routes: registering adds one route"
           (= (1+ before) (length (evo.serve:server-routes server))))
    (register-swarm-routes)
    (check "routes: re-registering replaces, never stacks"
           (= (1+ before) (length (evo.serve:server-routes server))))
    (multiple-value-bind (handler status tail)
        (evo.serve:route-request "GET" "/lanes")
      (check "routes: GET /lanes takes the swarm's handler"
             (and (eq handler #'evo.swarm::handle-swarm-lane-route) (null tail))))
    (multiple-value-bind (handler status tail)
        (evo.serve:route-request "GET" "/lanes/3/transcript")
      (check "routes: a lane's path takes it too, tail and all"
             (and (eq handler #'evo.swarm::handle-swarm-lane-route)
                  (equal "3/transcript" tail))))
    (multiple-value-bind (handler status tail)
        (evo.serve:route-request "POST" "/lanes")
      (declare (ignore tail))
      (check "routes: the swarm's API is read-only (POST is a 405)"
             (and (null handler) (eql 405 status))))
    (check "routes: the swarm says who it is, for a client that asks"
           (equal '(:name "evo-swarm" :version "0.1.0" :features ("swarm"))
                  *swarm-identity*))))

(defun test-sse-reader ()
  (let ((seen nil))
    (with-input-from-string
        (in (format nil ": hello~%~%id: 7~%event: report~%data: {\"a\":1}~%~%id: 8~%event: settled~%data: {}~%~%"))
      (evo.swarm::read-sse-events in (lambda (id type data) (push (list id type data) seen))))
    (check "sse: ids, types and data, comments skipped"
           (equal '((7 "report" "{\"a\":1}") (8 "settled" "{}")) (nreverse seen)))))

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

(defun run-all ()
  (let ((*pass* 0) (*fail* 0))
    (test-baseline)
    (test-in-lanes)
    (test-model-fill-in)
    (test-tool-limits)
    (test-notes)
    (test-coordinator-tools)
    (test-events)
    (test-lane-api)
    (test-lane-state-events)
    (test-swarm-json-handlers)
    (test-lane-event-relay)
    (test-swarm-route-parsing)
    (test-swarm-route-registration)
    (test-view)
    (test-serve-view)
    (test-record)
    (test-launch-environment)
    (test-sessions-dir)
    (test-cli)
    (test-serve-cli)
    (test-serve-exit-codes)
    (test-sse-reader)
    (test-pid-alive)
    (test-sample-config)
    (format t "~%swarm: ~d passed, ~d failed~%" *pass* *fail*)
    (if (zerop *fail*) 0 1)))
