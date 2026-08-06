;;;; windows-probe.lisp -- ask a Windows machine the questions this codebase
;;;; cannot answer from a Unix one.  Self-contained: plain SBCL, no evo, no
;;;; quicklisp, no sb-posix (which Windows SBCL does not have).  Every probe is
;;;; wrapped, so one failure cannot hide the rest, and every line is flushed as
;;;; it is written, so even a hard crash leaves on disk everything learned up
;;;; to that point.
;;;;
;;;; Two ways to run it, and BOTH are wanted -- the interesting difference is
;;;; between a process that owns its console and a process spawned by another:
;;;;
;;;;   1) In a console, with plain SBCL:
;;;;        sbcl --script windows-probe.lisp
;;;;
;;;;   2) Inside the real binary, as a boot extension -- this runs it in the
;;;;      supervised child, which is where the TUI dies:
;;;;        copy windows-probe.lisp %USERPROFILE%\.evo\extensions\000-probe.lisp
;;;;        .\evo.exe          (let it fail, then delete that file again)
;;;;
;;;; Both runs append to  %USERPROFILE%\evo-windows-probe.txt  -- send that file.
;;;;
;;;; The file is deliberately kept in CR-LF (see .gitattributes): it doubles as
;;;; a standing check that CR-LF Lisp source compiles and reads.

(defparameter *probe-report*
  (merge-pathnames "evo-windows-probe.txt" (user-homedir-pathname)))

(defvar *probe-stream* nil)

(defun probe-say (control &rest args)
  "Write one report line and flush it: the next probe may be the fatal one."
  (let ((line (apply #'format nil control args)))
    (when *probe-stream*
      (write-line line *probe-stream*)
      (finish-output *probe-stream*))
    (ignore-errors
     (write-line line *error-output*)
     (finish-output *error-output*))))

(defmacro probe (label &body body)
  "Run BODY, reporting either its value or the condition it signalled."
  `(probe-say "~a: ~a" ,label
              (handler-case (progn ,@body)
                (error (e) (format nil "FAILED: ~a" e))
                (storage-condition (e) (format nil "FAILED: ~a" e)))))

(defun hex (n)
  (if (integerp n) (format nil "~d (#x~x)" n n) (format nil "~a" n)))

(defun env (name)
  (or (sb-ext:posix-getenv name) "(unset)"))

(defun stream-fd (stream)
  (if (typep stream 'sb-sys:fd-stream)
      (sb-sys:fd-stream-fd stream)
      :not-an-fd-stream))

;;; Win32 calls, for comparison only.  GetStdHandle is what SBCL itself uses to
;;; build its standard streams on Windows; if the fds above match these numbers
;;; then an "fd" here is an OS handle, and a literal 1 for stdout is wrong.

#+win32
(progn
  (sb-alien:define-alien-routine ("GetStdHandle" %std-handle)
      sb-alien:system-area-pointer
    (which sb-alien:int))
  (sb-alien:define-alien-routine ("GetConsoleMode" %get-console-mode)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (mode (* sb-alien:unsigned-int)))
  (sb-alien:define-alien-routine ("GetConsoleOutputCP" %console-output-cp)
      sb-alien:unsigned-int)
  (sb-alien:define-alien-routine ("GetLastError" %last-error)
      sb-alien:unsigned-int)

  (defun std-handle (which)
    (sb-sys:sap-int (%std-handle which)))

  (defun console-mode-of (handle)
    (sb-alien:with-alien ((mode sb-alien:unsigned-int))
      (if (zerop (%get-console-mode (sb-sys:int-sap handle) (sb-alien:addr mode)))
          (format nil "GetConsoleMode failed, GetLastError=~d" (%last-error))
          (format nil "#x~x" mode)))))

;;; Writing.  This is the whole question: which number does MAKE-FD-STREAM want
;;; for stdout here?  Each candidate gets its own attempt and its own marker,
;;; so the console shows which one actually reached the screen.

(defun try-write (label where)
  (probe label
    (let ((stream (sb-sys:make-fd-stream where :output t :buffering :full
                                               :external-format :utf-8)))
      (write-string (format nil "[probe marker via ~a]~%" label) stream)
      (finish-output stream)
      "OK -- wrote and flushed")))

(defun try-input-stream (label where)
  (probe label
    (let ((stream (sb-sys:make-fd-stream where :input t :buffering :none
                                               :element-type '(unsigned-byte 8))))
      (format nil "constructed, open-stream-p=~a" (open-stream-p stream)))))

(defun run-probe ()
  (with-open-file (out *probe-report* :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
    (let ((*probe-stream* out))
      (probe-say "================ evo windows probe ================")
      (probe "when"
        (multiple-value-bind (sec min hour day month year) (get-decoded-time)
          (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                  year month day hour min sec)))
      (probe "implementation" (format nil "~a ~a on ~a"
                                      (lisp-implementation-type)
                                      (lisp-implementation-version)
                                      (machine-type)))
      (probe "windows feature" (if (member :win32 *features*) "yes" "no (not Windows)"))
      (probe "role" (format nil "supervised child=~a, EVO_HOME=~a, TERM=~a"
                            (env "EVO_SUPERVISED_CHILD") (env "EVO_HOME") (env "TERM")))

      ;; Control question first: does SBCL's OWN stdout work here?  If this
      ;; fails too, the problem is the console/inheritance, not evo's fd.
      (probe-say "---- control: SBCL's own standard output ----")
      (probe "write via *standard-output*"
        (progn (write-string (format nil "[probe marker via *standard-output*]~%"))
               (finish-output *standard-output*)
               "OK -- wrote and flushed"))

      (probe-say "---- the standard streams SBCL built for itself ----")
      (probe "*stdout* type" (type-of sb-sys:*stdout*))
      (probe "*stdout* fd" (hex (stream-fd sb-sys:*stdout*)))
      (probe "*stdin* fd" (hex (stream-fd sb-sys:*stdin*)))
      (probe "*stderr* fd" (hex (stream-fd sb-sys:*stderr*)))

      #+win32
      (progn
        (probe-say "---- GetStdHandle, for comparison with those fds ----")
        (probe "STD_OUTPUT_HANDLE" (hex (std-handle -11)))
        (probe "STD_INPUT_HANDLE" (hex (std-handle -10)))
        (probe "STD_ERROR_HANDLE" (hex (std-handle -12)))
        (probe "console mode (stdout)" (console-mode-of (std-handle -11)))
        (probe "console mode (stdin)" (console-mode-of (std-handle -10)))
        (probe "console output codepage" (%console-output-cp)))

      (probe-say "---- writing to stdout: which number works? ----")
      (try-write "literal 1 -- what evo used to do" 1)
      (try-write "the fd of SBCL's own *stdout*" (stream-fd sb-sys:*stdout*))
      #+win32
      (try-write "GetStdHandle(STD_OUTPUT_HANDLE)" (std-handle -11))

      (probe-say "---- reading stdin: which number constructs? ----")
      (try-input-stream "literal 0 -- what evo used to do" 0)
      (try-input-stream "the fd of SBCL's own *stdin*" (stream-fd sb-sys:*stdin*))

      (probe-say "---- does a child inherit our stdout? ----")
      (probe "run-program echo with :output t"
        (let ((process
                #+win32 (sb-ext:run-program "cmd.exe"
                                            (list "/c" "echo" "[probe child stdout ok]")
                                            :output t :error t :wait t :search t)
                #-win32 (sb-ext:run-program "/bin/echo"
                                            (list "[probe child stdout ok]")
                                            :output t :error t :wait t)))
          (format nil "exit ~a -- its marker should be on the console above"
                  (sb-ext:process-exit-code process))))

      (probe-say "================ end of probe ================")))
  (ignore-errors
   (format *error-output* "~&probe report appended to: ~a~%" (namestring *probe-report*))
   (finish-output *error-output*))
  t)

(run-probe)
