;;;; eval.lisp — /eval and the `eval` tool, a core extension.
;;;;
;;;; A REPL into the running image, offered twice: `/eval <sexpr>` to the
;;;; user, the `eval` tool to the model.  Both evaluate in EVO.USER — the
;;;; very package agent-written code and every extension is loaded into — so
;;;; the whole image is reachable without ceremony: registered tools
;;;; (evo:all-tools, evo.kernel:find-tool), extension functions and state,
;;;; the live agent (evo:*agent*), the kernel packages one prefix away.  No
;;;; sandbox, no separate environment: this is a hand on the image the
;;;; session is running in.
;;;;
;;;; The command takes exactly one form, deliberately.  Two forms on a line
;;;; hide a typo — a stray paren silently makes a second one — and leave
;;;; "the result" ambiguous, so the rejection names the count and points at
;;;; (progn ...): explicit sequencing, one value.  The tool takes a body,
;;;; equally deliberately: its caller is writing a program, not typing a
;;;; line, and defining a helper and then calling it is one thought.
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

(defun format-result (values output condition &key (error-prefix "✗ "))
  "Everything the evaluation produced, as one string: what it printed, then
its values or the condition that stopped it.  ERROR-PREFIX marks the failure
line for a reader with nothing else to go on — the command's own output.  The
tool passes none: its failures arrive already marked as failed tool calls,
and a second marker would just be noise inside one."
  (with-output-to-string (out)
    (when (plusp (length output))
      (write-string (string-right-trim '(#\Newline #\Return) output) out)
      (terpri out))
    (cond
      (condition (format out "~a~(~a~): ~a" error-prefix (type-of condition) condition))
      ((null values) (write-string "⇒ ; no values" out))
      (t (format out "~{⇒ ~a~^~%~}" (mapcar #'print-value values))))))

;;; Completion: what the image has to offer.
;;;
;;; The frontend asks what a half-typed token could become; deciding that is
;;; the same knowledge /eval already owns — which package the content reads
;;; in, and what a symbol has to be to be worth offering.  Only functions
;;; and variables are offered: the image interns far more symbols than it
;;; can call or read, and a suggestion you cannot evaluate is noise.

(defparameter *token-delimiters*
  '(#\Space #\Tab #\Newline #\Return #\( #\) #\' #\" #\` #\, #\; #\#)
  "Characters that end a symbol token.  Colon is absent deliberately: it is
part of the token, so a package qualifier completes as one piece.")

(defun token-start (text end)
  "Index where the symbol token ending at END in TEXT begins."
  (let ((i (min end (length text))))
    (loop while (and (plusp i)
                     (not (member (char text (1- i)) *token-delimiters*)))
          do (decf i))
    i))

(defun split-qualifier (token)
  "TOKEN split at its package marker: (values PACKAGE NAME MARKER), MARKER
being \":\", \"::\", or NIL when TOKEN names no package.  An empty PACKAGE
is the keyword package, the way the reader treats a leading colon."
  (let ((colon (position #\: token)))
    (if (null colon)
        (values nil token nil)
        (let ((marker (if (and (< (1+ colon) (length token))
                               (char= (char token (1+ colon)) #\:))
                          "::"
                          ":")))
          (values (subseq token 0 colon)
                  (subseq token (+ colon (length marker)))
                  marker)))))

(defun completion-package (name)
  (if (zerop (length name))
      (find-package :keyword)
      (or (find-package (string-upcase name)) (find-package name))))

(defun symbol-kind (symbol)
  "What SYMBOL is, as a description, or NIL when it is neither a function
nor a variable — nothing /eval could call or read."
  (let ((kinds nil))
    (cond ((keywordp symbol) (push "keyword" kinds))
          ((not (boundp symbol)))
          ((constantp symbol) (push "constant" kinds))
          (t (push "variable" kinds)))
    (cond ((special-operator-p symbol) (push "special operator" kinds))
          ((macro-function symbol) (push "macro" kinds))
          ((fboundp symbol)
           (push (if (typep (ignore-errors (fdefinition symbol)) 'generic-function)
                     "generic function"
                     "function")
                 kinds)))
    (when kinds (string-join ", " kinds))))

(defun prefix-match-p (prefix name)
  "Case-insensitive: symbol names are stored upcased and typed lowercase."
  (and (<= (length prefix) (length name))
       (string-equal prefix name :end2 (length prefix))))

(defun offer (symbol prefix qualifier)
  "SYMBOL as a (name . description) candidate when it matches PREFIX and is
worth offering, else NIL.  NAME is the whole replacement text, qualifier
included, so accepting it replaces the token as typed."
  (let ((name (symbol-name symbol)))
    (when (prefix-match-p prefix name)
      (let ((kind (symbol-kind symbol)))
        (when kind
          (cons (concatenate 'string qualifier (string-downcase name)) kind))))))

(defun qualified-completions (package prefix qualifier external-only)
  "Symbols of PACKAGE matching PREFIX.  A single colon offers what the
package exports; a double colon offers what it owns — an inherited symbol
belongs to the package it came from, and is reachable under that name."
  (let ((out nil))
    (flet ((collect (symbol)
             (let ((candidate (offer symbol prefix qualifier)))
               (when candidate (push candidate out)))))
      (if external-only
          (do-external-symbols (symbol package) (collect symbol))
          (do-symbols (symbol package)
            (when (eq (symbol-package symbol) package) (collect symbol)))))
    out))

(defun accessible-completions (prefix)
  "Symbols reachable unqualified while evaluating: the eval package's own,
plus everything CL and EVO bring into it."
  (let ((out nil))
    (do-symbols (symbol (eval-package))
      (let ((candidate (offer symbol prefix "")))
        (when candidate (push candidate out))))
    out))

(defun package-completions (prefix)
  "Package names and nicknames matching PREFIX, offered with their colon
already attached so the next keystroke lands in a qualified completion."
  (let ((out nil))
    (dolist (package (list-all-packages) out)
      (dolist (name (cons (package-name package) (package-nicknames package)))
        (when (prefix-match-p prefix name)
          (push (cons (concatenate 'string (string-downcase name) ":") "package")
                out))))))

(defun completions-for (token)
  "Candidates for TOKEN, a partly typed symbol: (name . description) pairs
sorted by name, where NAME replaces TOKEN whole.  NIL when TOKEN gives
nothing to filter on — an empty token, or a bare colon, would offer the
whole image rather than complete anything."
  (multiple-value-bind (package-part name-part marker) (split-qualifier token)
    (let ((qualifier (or package-part "")))
      (when (or (plusp (length name-part)) (plusp (length qualifier)))
        (sort (remove-duplicates
               (if marker
                   (let ((package (completion-package qualifier)))
                     (when package
                       (qualified-completions
                        package name-part
                        (concatenate 'string (string-downcase qualifier) marker)
                        (string= marker ":"))))
                   (append (accessible-completions name-part)
                           (package-completions name-part)))
               :key #'car :test #'string= :from-end t)
              #'string< :key #'car)))))

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

;;; The tool.
;;;
;;; The same evaluator, reached by the model instead of the user, and the
;;; shortest path it has to a number that must be right or a fact about the
;;; runtime it is inside: no file to write, no shell to spawn, no other
;;; language's arithmetic — CL's rationals and bignums are exact.
;;;
;;; A condition is signalled rather than rendered, so a failed evaluation
;;; comes back as a failed tool call instead of a success whose text happens
;;; to begin with a cross; captured output survives inside the message.
;;;
;;; What eval does not do is persist.  Definitions made here live in the
;;; image until the process exits and nothing journals them, so anything
;;; worth keeping goes in a file loaded with (evo:load-extension "..."):
;;; that load IS journaled, and replays when the session resumes.

(defparameter *tool-result-limit* (* 16 1024 1024)
  "Chars of rendered result the tool hands back.  Not the context budget —
the loop trims every tool result to the shared EVO.KERNEL::*MAX-TOOL-RESULT-CHARS*
after this — but a backstop against a runaway print, which is unbounded in a
way the journal and the heap are not.  Deliberately far above anything a
deliberate evaluation produces: truncating a real result is the loop's job,
and doing it twice would hide half an answer for no reason.")

(defun tool-eval (args)
  (let ((forms (handler-case (read-forms (or (pget args :code) ""))
                 (serious-condition (e) (error "unreadable code — ~a" e)))))
    (when (null forms)
      (error "nothing to evaluate — `code` must hold at least one sexpr"))
    (multiple-value-bind (values output condition)
        (eval-form (if (rest forms) (cons 'progn forms) (first forms)))
      (let ((text (truncate-string
                   (format-result values output condition :error-prefix "")
                   *tool-result-limit*)))
        (if condition (error "~a" text) text)))))

(evo:register-tool "eval"
  :description "Evaluate Common Lisp in your own runtime (package EVO.USER) and get the values back. Reach for this FIRST for any deterministic computation or check: arithmetic is exact (rationals and bignums, no float error, no overflow, no silent truncation), and it beats doing sums in your head or spawning a shell for them. It is also how you interrogate the image you are running inside — (evo:all-tools), evo:*agent*, (describe 'foo), (apropos \"pattern\"), any function you have loaded — and how you verify a claim before making it. `code` is a body: several forms run in order, the last one's values come back, and anything printed is shown above them. Definitions made here are NOT journaled and vanish on restart; for something durable, write a .lisp file and evaluate (evo:load-extension \"/path/x.lisp\"), which is journaled and replayed on resume. No sandbox and no timeout: it runs on the session's thread, so an unbounded loop wedges the session."
  :schema '(:object
            (:code :type :string
             :description "One or more Lisp forms, evaluated in order in EVO.USER; the last form's values are the result"))
  :execute #'tool-eval)
