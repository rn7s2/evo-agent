;;;; windows-probe.lisp -- ask a Windows machine the questions this codebase
;;;; cannot answer from a Unix one.  Self-contained: plain SBCL, no evo, no
;;;; quicklisp, no sb-posix (which Windows SBCL does not have).  Every probe is
;;;; wrapped, so one failure cannot hide the rest, and every line is flushed as
;;;; it is written, so even a hard crash leaves on disk everything learned up
;;;; to that point.
;;;;
;;;; ROUND 2.  Round 1 settled two things, by measurement, on Windows 11 with
;;;; SBCL 2.6.7 and console codepage 936:
;;;;
;;;;   * an "fd" is an OS handle here.  *stdout* fd = 116 = GetStdHandle
;;;;     (STD_OUTPUT_HANDLE), and a stream on the literal 1 dies with "The
;;;;     handle is invalid" -- which is what the TUI did.
;;;;   * a console handle is written through the wide console API, so a stream
;;;;     forced to :external-format :utf-8 hands it UTF-8 bytes that come out
;;;;     as UTF-16: "[probe marker...]" rendered as "灛潲敢洠牡敫...".
;;;;
;;;; What is still unknown, and what this round asks: which external format
;;;; SBCL picked for the console, whether a stream built that way prints both
;;;; ASCII and the box-drawing the TUI needs, and -- the one that decides
;;;; whether the TUI can read keys at all -- whether a byte stream on the
;;;; console handle returns the bytes you type.
;;;;
;;;; Two ways to run it, and BOTH are wanted:
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
;;;; Garbled text on the console is a result, not a problem: the report file is
;;;; written UTF-8 and stays readable.  The last probe waits for you to type;
;;;; skipping it (ctrl-c, or just closing the window) loses nothing else.
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

;;; The two strings the TUI actually needs to put on screen: plain ASCII, and
;;; the box drawing plus prompt glyph it paints every frame.

(defparameter *ascii-sample* "ABC-123")
(defparameter *tui-sample*
  (coerce (list (code-char #x2500) (code-char #x2500)   ; horizontal rule
                #\Space
                (code-char #x276F)                      ; the prompt chevron
                #\Space
                (code-char #x25CB))                     ; the idle spinner
          'string))

#+win32
(progn
  (sb-alien:define-alien-routine ("GetStdHandle" %std-handle)
      sb-alien:system-area-pointer
    (which sb-alien:int))
  (sb-alien:define-alien-routine ("GetConsoleOutputCP" %console-output-cp)
      sb-alien:unsigned-int)
  (sb-alien:define-alien-routine ("SetConsoleOutputCP" %set-console-output-cp)
      sb-alien:int (codepage sb-alien:unsigned-int))
  (sb-alien:define-alien-routine ("SetConsoleMode" %set-console-mode)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (mode sb-alien:unsigned-int))
  (sb-alien:define-alien-routine ("GetConsoleMode" %get-console-mode)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (mode (* sb-alien:unsigned-int)))

  (defun std-handle (which)
    (sb-sys:sap-int (%std-handle which)))

  (defun console-mode-of (handle)
    (sb-alien:with-alien ((mode sb-alien:unsigned-int))
      (if (zerop (%get-console-mode (sb-sys:int-sap handle) (sb-alien:addr mode)))
          nil
          mode))))

(defun try-write (label where external-format)
  "Build a stream the way the TUI does and print both samples through it.
Whether they are legible on the console is the answer; the report only says
that the write itself did not signal."
  (probe label
    (let ((stream (sb-sys:make-fd-stream where :output t :buffering :full
                                               :external-format external-format)))
      (write-string (format nil "[~a] ascii=~a tui=~a~%"
                            label *ascii-sample* *tui-sample*)
                    stream)
      (finish-output stream)
      (format nil "wrote without error, external-format=~a"
              (stream-external-format stream)))))

(defun run-probe ()
  (with-open-file (out *probe-report* :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
    (let ((*probe-stream* out))
      (probe-say "============ evo windows probe, round 2 ============")
      (probe "when"
        (multiple-value-bind (sec min hour day month year) (get-decoded-time)
          (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                  year month day hour min sec)))
      (probe "implementation" (format nil "~a ~a on ~a"
                                      (lisp-implementation-type)
                                      (lisp-implementation-version)
                                      (machine-type)))
      (probe "role" (format nil "supervised child=~a, EVO_HOME=~a, TERM=~a"
                            (env "EVO_SUPERVISED_CHILD") (env "EVO_HOME") (env "TERM")))

      (probe-say "---- what SBCL chose for its own streams ----")
      (probe "*stdout* fd" (hex (stream-fd sb-sys:*stdout*)))
      (probe "*stdout* external-format" (stream-external-format sb-sys:*stdout*))
      (probe "*stdin* fd" (hex (stream-fd sb-sys:*stdin*)))
      (probe "*stdin* external-format" (stream-external-format sb-sys:*stdin*))
      (probe "*stdin* element-type" (stream-element-type sb-sys:*stdin*))
      #+win32
      (probe "console output codepage" (%console-output-cp))

      (probe-say "---- printing: which stream shows readable text? ----")
      (probe "via *standard-output*"
        (progn (format t "[via *standard-output*] ascii=~a tui=~a~%"
                       *ascii-sample* *tui-sample*)
               (finish-output)
               "wrote without error"))
      (try-write "own stream, SBCL's own external-format"
                 (stream-fd sb-sys:*stdout*)
                 (stream-external-format sb-sys:*stdout*))
      (try-write "own stream, forced :utf-8 (round 1 showed this garbles)"
                 (stream-fd sb-sys:*stdout*)
                 :utf-8)
      #+win32
      (probe "SetConsoleOutputCP(65001), then :utf-8 again"
        (progn (%set-console-output-cp 65001)
               (try-write "own stream, :utf-8 at codepage 65001"
                          (stream-fd sb-sys:*stdout*) :utf-8)
               (format nil "codepage now ~a" (%console-output-cp))))

      ;; The TUI's key reading: raw bytes off the console handle, with the
      ;; console in the mode evo puts it in (VT input, no line editing, no
      ;; echo).  If this returns nothing or returns UTF-16, the input half
      ;; needs the same treatment the output half just got.
      (probe-say "---- reading keys: does a byte stream see them? ----")
      #+win32
      (probe "console input mode before"
        (hex (console-mode-of (std-handle -10))))
      (probe "byte stream on stdin's descriptor"
        (let ((stream (sb-sys:make-fd-stream (stream-fd sb-sys:*stdin*)
                                             :input t :buffering :none
                                             :element-type '(unsigned-byte 8))))
          (format nil "constructed, open-stream-p=~a" (open-stream-p stream))))
      (probe-say "NOW: type  abc  and press Enter (or ctrl-c to stop here).")
      #+win32
      (probe "set raw VT input mode"
        (let ((ok (%set-console-mode (sb-sys:int-sap (std-handle -10)) #x200)))
          (format nil "SetConsoleMode(ENABLE_VIRTUAL_TERMINAL_INPUT)=~a" ok)))
      (probe "bytes read"
        (let ((stream (sb-sys:make-fd-stream (stream-fd sb-sys:*stdin*)
                                             :input t :buffering :none
                                             :element-type '(unsigned-byte 8)))
              (bytes nil)
              (deadline (+ (get-universal-time) 20)))
          (loop while (and (< (get-universal-time) deadline) (< (length bytes) 8))
                do (let ((b (read-byte stream nil :eof)))
                     (when (eq b :eof) (return))
                     (push b bytes)))
          (format nil "~a  (as characters: ~s)"
                  (reverse bytes)
                  (map 'string (lambda (b) (if (< 31 b 127) (code-char b) #\.))
                       (reverse bytes)))))
      #+win32
      (probe "restore console input mode"
        (hex (%set-console-mode (sb-sys:int-sap (std-handle -10)) #x7)))
      (probe-say "============ end of round 2 ============")))
  (ignore-errors
   (format *error-output* "~&probe report appended to: ~a~%" (namestring *probe-report*))
   (finish-output *error-output*))
  t)

(run-probe)
