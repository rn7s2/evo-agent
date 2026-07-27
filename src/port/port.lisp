;;;; port.lisp — the implementation portability layer (SBCL + ECL).
;;;;
;;;; Every use of a non-standard host facility goes through EVO.PORT:
;;;; process spawning and control, environment, argv, exit, package locks,
;;;; package-local nicknames, fd streams, signal handling, tty detection.
;;;; Supporting another implementation means editing this file (plus the
;;;; build entry in build.lisp) and nothing else.

(in-package :evo.port)

#-(or sbcl ecl)
(error "evo runs on SBCL or ECL; this is ~a" (lisp-implementation-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-posix))

;;; Process lifecycle (exit, argv, self-path, environment).

(defun exit-lisp (code)
  "Terminate the process with exit CODE."
  #+sbcl (sb-ext:exit :code code)
  #+ecl (ext:quit code))

(defun argv ()
  "Command-line arguments, excluding the program name."
  #+sbcl (rest sb-ext:*posix-argv*)
  #+ecl (loop for i from 1 below (si:argc) collect (si:argv i)))

(defun runtime-pathname ()
  "Pathname of the running executable (for re-spawning ourselves)."
  #+sbcl sb-ext:*runtime-pathname*
  #+ecl
  (let ((argv0 (si:argv 0)))
    (if (find #\/ argv0)
        (truename (merge-pathnames argv0 (uiop:getcwd)))
        (or (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                :separator '(#\:))
                  for found = (and (plusp (length dir))
                                   (probe-file
                                    (merge-pathnames argv0
                                                     (uiop:ensure-directory-pathname dir))))
                  when found return found)
            (error "evo: cannot locate own executable (argv[0] = ~a)" argv0)))))

(defun environ ()
  "The current environment as a list of \"VAR=VALUE\" strings."
  #+sbcl (sb-ext:posix-environ)
  #+ecl (ext:environ))

(defun setenv (name value)
  "Set environment variable NAME to VALUE in this process."
  #+sbcl (sb-posix:setenv name value 1)
  #+ecl (ext:setenv name value))

;;; Child processes.
;;;
;;; The handle returned by LAUNCH-CHILD is opaque; pass it only to
;;; PROCESS-ALIVE-P / PROCESS-KILL / PROCESS-WAIT.

(defun launch-child (program args &key (input t) (output t) (error-output t)
                                       environment)
  "Spawn PROGRAM (an absolute path) with ARGS, without waiting.
INPUT/OUTPUT/ERROR-OUTPUT: t inherits the parent's fd, a pathname redirects
to that file (superseding), nil is the null device; ERROR-OUTPUT may also be
:output to merge stderr into OUTPUT.  ENVIRONMENT nil inherits the parent's
environment; otherwise a list of \"VAR=VALUE\" strings."
  #+sbcl
  (apply #'sb-ext:run-program program args
         :wait nil
         :input input :output output
         :error (if (eq error-output :output) :output error-output)
         :if-output-exists :supersede
         (when environment (list :environment environment)))
  #+ecl
  (multiple-value-bind (stream code process)
      (apply #'ext:run-program program args
             :wait nil
             :input input :output output
             :error (if (eq error-output :output) :output error-output)
             :if-output-exists :supersede
             (when environment (list :environ environment)))
    (declare (ignore stream code))
    process))

(defun process-alive-p (process)
  #+sbcl (sb-ext:process-alive-p process)
  #+ecl (member (ext:external-process-status process)
                '(:running :stopped :resumed)))

(defun process-kill (process)
  "SIGKILL the child; unblock it from any state."
  #+sbcl (sb-ext:process-kill process sb-unix:sigkill)
  #+ecl (ext:terminate-process process t))

(defun process-wait (process)
  "Block until PROCESS exits.  Returns (values STATUS CODE): STATUS is
:exited or :signaled; CODE is the exit code or the signal number."
  #+sbcl
  (progn
    (sb-ext:process-wait process)
    (values (if (eq (sb-ext:process-status process) :signaled)
                :signaled
                :exited)
            (sb-ext:process-exit-code process)))
  #+ecl
  (multiple-value-bind (status code) (ext:external-process-wait process t)
    (values (if (eq status :signaled) :signaled :exited) code)))

;;; Packages.

(defun lock-package (package)
  #+sbcl (sb-ext:lock-package package)
  #+ecl (si:package-lock (find-package package) t))

(defun unlock-package (package)
  #+sbcl (sb-ext:unlock-package package)
  #+ecl (si:package-lock (find-package package) nil))

(defun add-package-local-nickname (nickname actual-package
                                   &optional (package *package*))
  #+sbcl (sb-ext:add-package-local-nickname nickname actual-package package)
  #+ecl (ext:add-package-local-nickname nickname actual-package package))

;;; Fd streams (the TUI talks to the tty directly, bypassing *standard-io*).

(defun make-fd-output-stream (fd &key (external-format :utf-8))
  "A fully-buffered character stream writing to file descriptor FD."
  #+sbcl (sb-sys:make-fd-stream fd :output t :buffering :full
                                   :external-format external-format)
  #+ecl (ext:make-stream-from-fd fd :output :buffering :full
                                    :element-type 'character
                                    :external-format external-format))

(defun make-fd-input-stream (fd)
  "An unbuffered (unsigned-byte 8) stream reading file descriptor FD."
  #+sbcl (sb-sys:make-fd-stream fd :input t :buffering :none
                                   :element-type '(unsigned-byte 8))
  #+ecl (ext:make-stream-from-fd fd :input :buffering :none
                                    :element-type '(unsigned-byte 8)))

;;; Signals & tty.

;; 28 on every platform evo targets (Darwin and Linux).
(defconstant +sigwinch+ 28)

(defun install-signal-handler (signo thunk)
  "Run THUNK (of no arguments) whenever this process receives SIGNO."
  #+sbcl (sb-sys:enable-interrupt signo
                                  (lambda (signal info context)
                                    (declare (ignore signal info context))
                                    (funcall thunk)))
  #+ecl (ext:set-signal-handler signo thunk))

(defun tty-p ()
  "True when both stdin and stdout are a terminal."
  #+sbcl (and (plusp (sb-unix:unix-isatty 0))
              (plusp (sb-unix:unix-isatty 1)))
  ;; ECL exposes no isatty, and interactive-stream-p is asymmetric (T for a
  ;; pty stdin, NIL for the same pty's stdout) — ask the shell, which sees
  ;; our own fds because launch-child inherits them.  Same no-FFI stance as
  ;; the tui's /bin/stty usage.
  #+ecl (zerop (nth-value 1 (process-wait
                             (launch-child
                              "/bin/sh" '("-c" "test -t 0 && test -t 1"))))))

;;; Binary entry plumbing.

(defun ensure-in-image-compiler ()
  "Make COMPILE-FILE (self-extension) work inside the shipped binary.
SBCL always carries its native compiler; the ECL binary switches to the
bytecodes compiler — the C one would need the cmp module plus a host C
toolchain at runtime."
  #+sbcl nil
  #+ecl (ext:install-bytecodes-compiler))

#+ecl
(defvar *main-process* nil
  "The main thread, captured by DISABLE-DEBUGGER: fatal exits must be
routed through it (see below).")

(defun disable-debugger ()
  "No interactive debugger in the shipped binary: print and die instead.

ECL needs two things beyond *DEBUGGER-HOOK*.  First, unhandled conditions
in secondary threads consult only EXT:*INVOKE-DEBUGGER-HOOK* — with just
*DEBUGGER-HOOK* set, a crashed worker drops into the interactive debugger
and reads stdin, which under the TUI's raw mode means a silently hung
session the supervisor cannot detect (the main loop keeps feeding the
heartbeat).  Second, EXT:QUIT from a secondary thread loses the exit code
(the process reports 0), which the supervisor protocol reads as a clean
exit and stops — so a non-main thread must exit by interrupting the main
thread instead."
  #+sbcl (sb-ext:disable-debugger)
  #+ecl
  (progn
    (setf *main-process* mp:*current-process*)
    (flet ((fatal-hook (condition hook)
             (declare (ignore hook))
             (format *error-output* "~&evo: fatal [~a]: ~a~%"
                     (mp:process-name mp:*current-process*) condition)
             (finish-output *error-output*)
             (if (or (null *main-process*)
                     (eq mp:*current-process* *main-process*))
                 (ext:quit 1)
                 (progn
                   (mp:interrupt-process *main-process*
                                         (lambda () (ext:quit 1)))
                   (sleep 5)            ; last resort if main never unwinds:
                   (ext:quit 1)))))    ; exit code degrades to 0, but we exit
      (setf *debugger-hook* #'fatal-hook
            ext:*invoke-debugger-hook* #'fatal-hook))))
