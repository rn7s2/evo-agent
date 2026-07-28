;;;; eval.lisp — /eval, a core extension.
;;;;
;;;; A REPL into the running image.  `/eval <sexpr>` evaluates the command's
;;;; content in EVO.USER — the very package agent-written code and every
;;;; extension is loaded into — so the whole image is reachable without
;;;; ceremony: registered tools (evo:all-tools, evo.kernel:find-tool),
;;;; extension functions and state, the live agent (evo:*agent*), the kernel
;;;; packages one prefix away.  No sandbox, no separate environment: this is
;;;; the user's own hand on the image the session is running in.
;;;;
;;;; Exactly one form, deliberately.  Two forms on a line hide a typo — a
;;;; stray paren silently makes a second one — and leave "the result"
;;;; ambiguous, so the rejection names the count and points at (progn ...):
;;;; explicit sequencing, one value.
;;;;
;;;; Reading runs with *read-eval* off.  The arity check must happen before
;;;; anything executes, or rejecting a second form could still have run the
;;;; first one through #. at read time.
;;;;
;;;; Output is captured rather than written: in the TUI a stray write to
;;;; *standard-output* lands in the middle of the frame.  Evaluation is
;;;; synchronous on the caller's thread — a non-terminating form wedges the
;;;; frontend until the supervisor's hang timeout (EVO_HANG_TIMEOUT) kills
;;;; the session and resumes it.

(in-package :evo.eval)

(defparameter *eval-package-name* :evo.user
  "Package /eval reads and evaluates in.  EVO.USER uses CL and EVO, so the
public API needs no prefix, and kernel internals are one prefix away.")

(defparameter *print-length-limit* 200
  "Elements printed per sequence in a result.  A live image holds objects
whose printed form is unbounded; the frontend gets a bounded string.")

(defparameter *print-level-limit* 6
  "Nesting depth printed in a result.")

(defvar *eof* (list :eof)
  "Fresh object used as the reader's end-of-input marker: EQ to nothing a
form could read as.")

(defun eval-package ()
  (or (find-package *eval-package-name*) (find-package :cl-user)))

;;; Reading: exactly one form, before anything runs.

(defun read-form-at (text start)
  "Read one form from TEXT starting at START.  Returns (values FORM END);
FORM is *EOF* when only whitespace or comments remain."
  (let ((*package* (eval-package))
        (*read-eval* nil))
    (read-from-string text nil *eof* :start start)))

(defun read-forms (text)
  "Every form in TEXT, in order.  Signals a reader error on malformed input."
  (loop with start = 0
        for (form end) = (multiple-value-list (read-form-at text start))
        until (eq form *eof*)
        collect form
        do (setf start end)))

(defun single-form (text)
  "Read TEXT as exactly one form.  Returns (values FORM NIL) on success, or
\(values NIL REASON) when TEXT is empty, unreadable, or more than one form."
  (multiple-value-bind (forms reason)
      (handler-case (values (read-forms text) nil)
        (serious-condition (e)
          (values nil (format nil "unreadable sexpr — ~a" e))))
    (cond
      (reason (values nil reason))
      ((null forms)
       (values nil "nothing to evaluate — usage: /eval <sexpr>"))
      ((null (rest forms)) (values (first forms) nil))
      (t (values nil (format nil "~d forms, expected exactly one — wrap them in one form: (progn ...)"
                             (length forms)))))))

;;; Evaluation.

(defun eval-form (form)
  "Evaluate FORM in the image.  Returns (values VALUES OUTPUT CONDITION):
the values it returned, everything it printed, and the condition that
stopped it (NIL when it returned normally)."
  (let ((output (make-string-output-stream))
        (values nil)
        (condition nil))
    (let ((*standard-output* output)
          (*error-output* output)
          (*trace-output* output)
          (*package* (eval-package)))
      (handler-case (setf values (multiple-value-list (eval form)))
        (serious-condition (e) (setf condition e))))
    (values values (get-output-stream-string output) condition)))

;;; Rendering.

(defun print-value (value)
  "VALUE as a bounded, non-looping string.  A live image can hold a
circular structure or a broken print-object method; neither may take the
frontend down."
  (handler-case
      (let ((*package* (eval-package))
            (*print-case* :downcase)
            (*print-circle* t)
            (*print-length* *print-length-limit*)
            (*print-level* *print-level-limit*)
            (*print-readably* nil)
            (*print-pretty* nil))
        (prin1-to-string value))
    (serious-condition (e)
      (format nil "#<unprintable ~(~a~): ~a>" (type-of value) e))))

(defun format-result (values output condition)
  (with-output-to-string (out)
    (when (plusp (length output))
      (write-string (string-right-trim '(#\Newline #\Return) output) out)
      (terpri out))
    (cond
      (condition (format out "✗ ~(~a~): ~a" (type-of condition) condition))
      ((null values) (write-string "⇒ ; no values" out))
      (t (format out "~{⇒ ~a~^~%~}" (mapcar #'print-value values))))))

;;; The command.

(defun eval-command (context)
  "The /eval command: evaluate exactly one sexpr in the live image."
  (multiple-value-bind (form reason) (single-form (or (pget context :args) ""))
    (if reason
        (format nil "✗ /eval: ~a" reason)
        (multiple-value-bind (values output condition) (eval-form form)
          (format-result values output condition)))))

(evo:register-command "eval" #'eval-command
  :description "evaluate one sexpr in the live image")
