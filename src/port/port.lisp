;;;; port.lisp — the implementation *and platform* portability layer.
;;;;
;;;; Every use of a non-standard host facility goes through EVO.PORT:
;;;; process spawning and control, environment, argv, exit, package locks,
;;;; package-local nicknames, fd streams, terminal mode and size, signal
;;;; handling, tty detection, the system shell.  Supporting another
;;;; implementation means editing this file (plus the build entry in
;;;; build.lisp) and nothing else.
;;;;
;;;; Two axes, not one: implementation (SBCL, ECL) and platform (Unix,
;;;; Windows).  Windows is SBCL-only — ECL would need its own set of
;;;; branches and nobody has walked that path; make.ps1 refuses anything
;;;; else.  Windows branches read on the :EVO-WINDOWS feature pushed below
;;;; rather than on #+win32 directly, so teaching a new implementation
;;;; about Windows is one form, not fifty.

(in-package :evo.port)

#-(or sbcl ecl)
(error "evo runs on SBCL or ECL; this is ~a" (lisp-implementation-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or win32 windows mswindows mingw32 mingw64)
  (pushnew :evo-windows *features*))

#+(and ecl evo-windows)
(error "evo on Windows requires SBCL; ECL is supported on Unix only.")

;; sb-posix is a Unix contrib: on Windows it is either absent or a stub, and
;; every call site below has a kernel32 branch instead.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(and sbcl (not evo-windows)) (require :sb-posix))

(defun windows-p ()
  "True on Windows.  Exported so the rest of evo can branch at runtime
instead of sprinkling read-time conditionals through portable code."
  #+evo-windows t
  #-evo-windows nil)

;;; Windows: kernel32, straight through sb-alien.
;;;
;;; The Unix side of this file deliberately shells out to stty rather than
;;; calling ioctl, because variadic ioctl is not safely callable through an
;;; implementation FFI on arm64 Darwin.  Neither reason applies here:
;;; nothing below is variadic, kernel32 is already mapped into every
;;; process, and Windows has no stty to shell out to.  (Calls are declared
;;; without an explicit convention, which is right for x64 — the only
;;; Windows target the build supports.)

#+evo-windows
(progn
  (defconstant +std-input-handle+ -10)
  (defconstant +std-output-handle+ -11)
  ;; Console input modes.
  (defconstant +enable-processed-input+ #x0001)
  (defconstant +enable-line-input+ #x0002)
  (defconstant +enable-echo-input+ #x0004)
  (defconstant +enable-virtual-terminal-input+ #x0200)
  ;; Console output modes.
  (defconstant +enable-processed-output+ #x0001)
  (defconstant +enable-wrap-at-eol-output+ #x0002)
  (defconstant +enable-virtual-terminal-processing+ #x0004)
  (defconstant +utf8-codepage+ 65001)

  (sb-alien:define-alien-routine ("GetStdHandle" %std-handle)
      sb-alien:system-area-pointer
    (which sb-alien:int))
  (sb-alien:define-alien-routine ("GetConsoleMode" %get-console-mode)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (mode (* sb-alien:unsigned-int)))
  (sb-alien:define-alien-routine ("SetConsoleMode" %set-console-mode)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (mode sb-alien:unsigned-int))
  (sb-alien:define-alien-routine ("GetConsoleScreenBufferInfo"
                                  %get-console-screen-buffer-info)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (info (* sb-alien:short)))
  (sb-alien:define-alien-routine ("SetConsoleCP" %set-console-cp)
      sb-alien:int (codepage sb-alien:unsigned-int))
  (sb-alien:define-alien-routine ("SetConsoleOutputCP" %set-console-output-cp)
      sb-alien:int (codepage sb-alien:unsigned-int))
  (sb-alien:define-alien-routine ("GetCurrentProcessId" %current-process-id)
      sb-alien:unsigned-int)
  (sb-alien:define-alien-routine ("GetEnvironmentStringsA" %environment-strings)
      sb-alien:system-area-pointer)
  (sb-alien:define-alien-routine ("FreeEnvironmentStringsA"
                                  %free-environment-strings)
      sb-alien:int (block sb-alien:system-area-pointer))
  (sb-alien:define-alien-routine ("SetEnvironmentVariableA"
                                  %set-environment-variable)
      sb-alien:int (name sb-alien:c-string) (value sb-alien:c-string))

  (defun console-mode (handle)
    "The console mode bits of HANDLE, or NIL when it is not a console
(a pipe, a file, a service with no window station)."
    (ignore-errors
     (sb-alien:with-alien ((mode sb-alien:unsigned-int))
       (when (/= 0 (%get-console-mode handle (sb-alien:addr mode)))
         mode))))

  (defun set-console-mode (handle mode)
    (ignore-errors (/= 0 (%set-console-mode handle mode))))

  (defun console-size (handle)
    "(values ROWS COLS) of HANDLE's console *window* (not its scrollback
buffer, which is usually taller), or NIL.

CONSOLE_SCREEN_BUFFER_INFO is eleven SHORTs with no padding — dwSize,
dwCursorPosition, wAttributes, srWindow (left top right bottom),
dwMaximumWindowSize — so an array of shorts is a faithful stand-in for the
struct and needs no groveling."
    (ignore-errors
     (sb-alien:with-alien ((info (array sb-alien:short 11)))
       (when (/= 0 (%get-console-screen-buffer-info
                    handle (sb-alien:addr (sb-alien:deref info 0))))
         (let ((left (sb-alien:deref info 5))
               (top (sb-alien:deref info 6))
               (right (sb-alien:deref info 7))
               (bottom (sb-alien:deref info 8)))
           (values (1+ (- bottom top)) (1+ (- right left))))))))

  (defun sap-c-string (sap offset)
    "The NUL-terminated string at SAP+OFFSET, and its length in bytes.
Read byte by byte rather than through an alien c-string cast: this is a
fallback path that must not be the thing that breaks, and a byte-for-char
mapping is only wrong for non-ASCII in a non-UTF-8 code page."
    (let ((end offset))
      (loop until (zerop (sb-sys:sap-ref-8 sap end)) do (incf end))
      (let* ((length (- end offset))
             (string (make-string length)))
        (dotimes (i length)
          (setf (char string i) (code-char (sb-sys:sap-ref-8 sap (+ offset i)))))
        (values string length))))

  (defun win32-environ ()
    "The Win32 environment block as \"VAR=VALUE\" strings.  Entries whose
name starts with = are the per-drive current directories Windows hides in
there (\"=C:=C:\\\\work\"); they are not inheritable settings and are dropped."
    (ignore-errors
     (let ((block (%environment-strings)))
       (unwind-protect
            (loop with offset = 0
                  for (entry length) = (multiple-value-list
                                        (sap-c-string block offset))
                  while (plusp length)
                  do (incf offset (1+ length))
                  unless (char= (char entry 0) #\=)
                    collect entry)
         (%free-environment-strings block))))))

(defun find-fbound-symbol (package name)
  "The function named NAME in PACKAGE, or NIL if either is missing.  Used
where SBCL *may* have wrapped a facility on this platform: asking at
runtime keeps the file readable on the platforms where it did not."
  (let* ((package (find-package package))
         (symbol (and package (find-symbol name package))))
    (and symbol (fboundp symbol) (symbol-function symbol))))

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
  #+(and sbcl (not evo-windows)) (sb-ext:posix-environ)
  ;; SBCL does define POSIX-ENVIRON on Windows, but which build has it has
  ;; moved over the years; the kernel32 block is the answer either way.
  #+(and sbcl evo-windows)
  (let ((native (find-fbound-symbol "SB-EXT" "POSIX-ENVIRON")))
    (or (and native (ignore-errors (funcall native)))
        (win32-environ)))
  #+ecl (ext:environ))

(defun getpid ()
  "This process's own PID."
  #+(and sbcl (not evo-windows)) (sb-posix:getpid)
  #+(and sbcl evo-windows) (%current-process-id)
  #+ecl (si:getpid))

(defun setenv (name value)
  "Set environment variable NAME to VALUE in this process."
  #+(and sbcl (not evo-windows)) (sb-posix:setenv name value 1)
  ;; Two environments live in a Windows process — the Win32 block that
  ;; CreateProcess hands to children, and the C runtime's copy that getenv
  ;; reads — and they are only synchronised through the setter you use.
  ;; Set both, so a variable set here is seen both by this image and by
  ;; anything it spawns.
  #+(and sbcl evo-windows)
  (let ((crt (find-fbound-symbol "SB-POSIX" "SETENV")))
    (%set-environment-variable name value)
    (when crt (ignore-errors (funcall crt name value 1)))
    value)
  #+ecl (ext:setenv name value))

(define-condition timeout-error (error)
  ((seconds :initarg :seconds :reader timeout-error-seconds))
  (:report (lambda (condition stream)
             (format stream "Operation timed out after ~a seconds."
                     (timeout-error-seconds condition)))))

(defun call-with-timeout (seconds function)
  "Call FUNCTION, signaling TIMEOUT-ERROR if it does not finish in SECONDS."
  (handler-case
      (bt:with-timeout (seconds)
        (funcall function))
    (bt:timeout ()
      (error 'timeout-error :seconds seconds))))

;;; Child processes.
;;;
;;; The handle returned by LAUNCH-CHILD is opaque; pass it only to
;;; PROCESS-ALIVE-P / PROCESS-KILL / PROCESS-WAIT.

(defun path-separator ()
  "The character that separates entries in PATH."
  #+evo-windows #\;
  #-evo-windows #\:)

(defun executable-suffixes (name)
  "Suffixes to try after NAME when searching PATH.  Unix: none, the file
either is there or is not.  Windows: PATHEXT, because `git` on disk is
`git.exe` — and a name already spelled with an extension is taken as final."
  (declare (ignorable name))
  #-evo-windows '("")
  #+evo-windows
  (if (find #\. name)
      '("")
      (cons "" (remove-if (lambda (s) (zerop (length s)))
                          (uiop:split-string
                           (or (uiop:getenv "PATHEXT") ".COM;.EXE;.BAT;.CMD")
                           :separator '(#\;))))))

(defun program-in-path (name)
  "Absolute pathname of NAME on PATH, or NIL.  RUN-PROGRAM does not search
PATH (SBCL :search defaults NIL), so callers pass absolute paths."
  (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                       :separator (list (path-separator)))
        thereis (and (plusp (length dir))
                     (loop for suffix in (executable-suffixes name)
                           thereis (ignore-errors
                                    (probe-file
                                     (merge-pathnames
                                      (concatenate 'string name suffix)
                                      (uiop:ensure-directory-pathname dir))))))))

(defvar *new-session-prefix* :unresolved
  "Cached argv prefix that runs a child in its own session with no
controlling terminal, or NIL if no mechanism exists on this host.")

;; setsid(2) fails (EPERM) when the caller is already a process-group leader
;; — and RUN-PROGRAM launches our child as exactly that.  So the shim must
;; fork first (like setsid(1) does): the grandchild is never a group leader,
;; setsid() there succeeds, and it execs the real command in a fresh session
;; with no controlling terminal.  The forking parent waits and propagates the
;; child's status (128+signal on a signal death, else the exit code), so our
;; RUN-PROGRAM handle still reports the right code; PROCESS-KILL-TREE reaches
;; the grandchild through pgrep -P.
(defparameter *perl-setsid-shim*
  (concatenate
   'string
   "my $p=fork; defined $p or die 'fork';"
   " if($p==0){POSIX::setsid(); exec(@ARGV) or die 'exec';}"
   " waitpid($p,0); exit(($? & 127) ? 128+($? & 127) : ($? >> 8));"))

(defun new-session-prefix ()
  "An argv prefix that runs the child in a new session with no controlling
terminal.  Prefer a perl fork+setsid shim (perl ships on macOS, which has no
setsid binary); fall back to setsid(1) -w (-w so our handle waits on the real
child).  Cached; NIL when neither exists (then WRAP-NEW-SESSION no-ops).

Windows has no controlling terminal to detach from — a console process that
wants to prompt allocates its own window rather than stealing ours — so
there is nothing to wrap and this is always NIL there."
  #+evo-windows nil
  #-evo-windows
  (if (eq *new-session-prefix* :unresolved)
      (setf *new-session-prefix*
            (let ((perl (program-in-path "perl"))
                  (setsid (program-in-path "setsid")))
              (cond
                (perl (list (namestring perl) "-MPOSIX" "-e"
                            *perl-setsid-shim* "--"))
                (setsid (list (namestring setsid) "-w"))
                (t nil))))
      *new-session-prefix*))

(defun wrap-new-session (program args)
  "Rewrite (PROGRAM . ARGS) so the child starts in a new session with no
controlling terminal: a prompt that opens /dev/tty (sudo, ssh, gpg) then
fails at once instead of hanging a non-interactive agent forever.  Best
effort — returns PROGRAM/ARGS unchanged if no mechanism is available."
  (let ((prefix (new-session-prefix)))
    (if prefix
        (values (first prefix) (append (rest prefix) (cons program args)))
        (values program args))))

(defun launch-child (program args &key (input t) (output t) (error-output t)
                                       environment new-session)
  "Spawn PROGRAM (an absolute path) with ARGS, without waiting.
INPUT/OUTPUT/ERROR-OUTPUT: t inherits the parent's fd, a pathname redirects
to that file (superseding), nil is the null device; ERROR-OUTPUT may also be
:output to merge stderr into OUTPUT.  ENVIRONMENT nil inherits the parent's
environment; otherwise a list of \"VAR=VALUE\" strings.  NEW-SESSION detaches
the child from the controlling terminal (see WRAP-NEW-SESSION)."
  (when new-session
    (multiple-value-setq (program args) (wrap-new-session program args)))
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

#+evo-windows
(defun taskkill (pid &key tree)
  "TerminateProcess PID (and with TREE, everything it spawned) via
taskkill.exe — the Windows answer to `kill -9`, and to `pgrep -P` recursion
for the tree case.  Best effort: NIL when taskkill is missing or refuses."
  (let ((exe (and pid (program-in-path "taskkill"))))
    (when exe
      (ignore-errors
       (let ((process (launch-child (namestring exe)
                                    (append '("/F") (when tree '("/T"))
                                            (list "/PID" (princ-to-string pid)))
                                    :input nil :output nil :error-output nil)))
         (zerop (nth-value 1 (process-wait process))))))))

(defun process-kill (process)
  "SIGKILL the child; unblock it from any state."
  #+(and sbcl (not evo-windows)) (sb-ext:process-kill process sb-unix:sigkill)
  ;; SBCL maps a kill on Windows onto TerminateProcess, but only for the
  ;; signals it knows; taskkill is the fallback that always exists.
  #+(and sbcl evo-windows)
  (or (ignore-errors (sb-ext:process-kill process 9) t)
      (taskkill (ignore-errors (process-pid process))))
  #+ecl (ext:terminate-process process t))

(defun process-pid (process)
  "Return the OS pid for PROCESS, or NIL if the implementation cannot expose it."
  #+sbcl (sb-ext:process-pid process)
  #+ecl (ext:external-process-pid process))

(defun child-pids (pid)
  "Immediate child pids of PID, via pgrep.  Best-effort helper for cleanup.
Always NIL on Windows, where PROCESS-KILL-TREE gets the whole tree from
taskkill /T in one call and never needs to walk it."
  #+evo-windows (declare (ignore pid))
  #+evo-windows nil
  #-evo-windows
  (when pid
    (let* ((out-file (uiop:with-temporary-file (:pathname p :keep t) p))
           (pgrep (ignore-errors
                    (launch-child "/bin/sh"
                                  (list "-c" (format nil "pgrep -P ~d" pid))
                                  :input nil :output out-file :error-output nil))))
      (unwind-protect
           (when pgrep
             (process-wait pgrep)
             (let ((out (ignore-errors
                          (with-open-file (in out-file :direction :input
                                                       :external-format :utf-8)
                            (loop for line = (read-line in nil)
                                  while line
                                  for n = (parse-integer line :junk-allowed t)
                                  when n collect n)))))
               out))
        (ignore-errors (delete-file out-file))))))

(defun process-kill-tree (process)
  "Best-effort SIGKILL of PROCESS and its current descendants."
  (labels ((kill-pid-tree (pid)
             #+evo-windows (taskkill pid :tree t)
             #-evo-windows
             (progn
               (dolist (child (child-pids pid))
                 (kill-pid-tree child))
               (ignore-errors
                #+sbcl (sb-unix:unix-kill pid sb-unix:sigkill)
                #+ecl (si:system (format nil "kill -9 ~d >/dev/null 2>&1" pid))))))
    (let ((pid (ignore-errors (process-pid process))))
      (if pid
          (kill-pid-tree pid)
          (process-kill process)))))

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

;; 28 on every platform evo targets (Darwin and Linux).  Windows has no
;; SIGWINCH at all; see INSTALL-SIGNAL-HANDLER.
(defconstant +sigwinch+ 28)

(defun install-signal-handler (signo thunk)
  "Run THUNK (of no arguments) whenever this process receives SIGNO.
Returns T when a handler is in place and NIL when this platform has no such
signal — Windows delivers no SIGWINCH, so callers that need the condition
have to poll for it instead (see EVO.TUI::POLL-TERMINAL-RESIZE)."
  #+evo-windows (declare (ignore signo thunk))
  #+evo-windows nil
  #-evo-windows
  (progn
    #+sbcl (sb-sys:enable-interrupt signo
                                    (lambda (signal info context)
                                      (declare (ignore signal info context))
                                      (funcall thunk)))
    #+ecl (ext:set-signal-handler signo thunk)
    t))

(defun tty-p ()
  "True when both stdin and stdout are a terminal."
  #+(and sbcl (not evo-windows))
  (and (plusp (sb-unix:unix-isatty 0))
       (plusp (sb-unix:unix-isatty 1)))
  ;; On Windows "is it a terminal" is "does it have a console mode": a pipe
  ;; or a redirected file makes GetConsoleMode fail, which is exactly the
  ;; question isatty answers on Unix.
  #+(and sbcl evo-windows)
  (and (console-mode (%std-handle +std-input-handle+))
       (console-mode (%std-handle +std-output-handle+))
       t)
  ;; ECL exposes no isatty, and interactive-stream-p is asymmetric (T for a
  ;; pty stdin, NIL for the same pty's stdout) — ask the shell, which sees
  ;; our own fds because launch-child inherits them.  Same no-FFI stance as
  ;; the tui's /bin/stty usage.
  #+ecl (zerop (nth-value 1 (process-wait
                             (launch-child
                              "/bin/sh" '("-c" "test -t 0 && test -t 1"))))))

;;; The terminal: raw mode, cooked mode, size.
;;;
;;; Unix drives this through /bin/stty (no curses, no FFI: variadic ioctl is
;;; not safely callable through implementation FFIs on arm64 Darwin, and
;;; stty runs only at startup/exit/resize).  Windows drives it through
;;; SetConsoleMode, where the same three calls also buy the two things that
;;; make the rest of the TUI portable for free: ENABLE_VIRTUAL_TERMINAL_INPUT
;;; makes the console send the very CSI sequences input.lisp already parses,
;;; and ENABLE_VIRTUAL_TERMINAL_PROCESSING makes it honour the ANSI evo
;;; prints.  The mode token is opaque — a stty -g string on Unix, a plist of
;;; console mode words on Windows — and only ever handed back to RESTORE.

#-evo-windows
(defun stty (&rest args)
  "Run /bin/stty on the controlling terminal, returning its trimmed output."
  (string-trim '(#\Newline #\Space)
               (with-output-to-string (out)
                 (uiop:run-program (cons "/bin/stty" args)
                                   :input :interactive :output out
                                   :ignore-error-status t))))

(defun terminal-raw-mode ()
  "Put the terminal in raw mode (no echo, no line discipline, keys as
bytes) and return an opaque token for RESTORE-TERMINAL-MODE, or NIL when
there is no terminal to configure."
  #-evo-windows
  (let ((saved (ignore-errors (stty "-g"))))
    (ignore-errors (stty "raw" "-echo"))
    ;; An empty answer means stty had no terminal to ask about: there is
    ;; nothing to restore later, and NIL says so.
    (and saved (plusp (length saved)) saved))
  #+evo-windows
  (let* ((in (%std-handle +std-input-handle+))
         (out (%std-handle +std-output-handle+))
         (in-mode (console-mode in))
         (out-mode (console-mode out)))
    (when in-mode
      (set-console-mode in (logior (logandc2 in-mode
                                             (logior +enable-processed-input+
                                                     +enable-line-input+
                                                     +enable-echo-input+))
                                   +enable-virtual-terminal-input+)))
    (when out-mode
      (set-console-mode out (logior out-mode
                                    +enable-processed-output+
                                    +enable-wrap-at-eol-output+
                                    +enable-virtual-terminal-processing+)))
    ;; UTF-8 both ways, or every box-drawing rule and CJK glyph evo prints
    ;; lands in the console's legacy code page as mojibake.
    (ignore-errors (%set-console-output-cp +utf8-codepage+))
    (ignore-errors (%set-console-cp +utf8-codepage+))
    (when (or in-mode out-mode)
      (list :input in-mode :output out-mode))))

(defun restore-terminal-mode (token)
  "Undo TERMINAL-RAW-MODE, restoring exactly what TOKEN captured."
  (when token
    #-evo-windows (ignore-errors (stty token))
    #+evo-windows
    (progn
      (when (getf token :input)
        (set-console-mode (%std-handle +std-input-handle+) (getf token :input)))
      (when (getf token :output)
        (set-console-mode (%std-handle +std-output-handle+) (getf token :output))))
    t))

(defun terminal-sane ()
  "Best-effort cooked mode, with no saved state to go back to: the
supervisor's recovery path after a child died inside raw mode."
  #-evo-windows (ignore-errors (stty "sane"))
  #+evo-windows
  (progn
    (set-console-mode (%std-handle +std-input-handle+)
                      (logior +enable-processed-input+ +enable-line-input+
                              +enable-echo-input+))
    (set-console-mode (%std-handle +std-output-handle+)
                      (logior +enable-processed-output+
                              +enable-wrap-at-eol-output+
                              +enable-virtual-terminal-processing+)))
  t)

(defun terminal-size ()
  "(values ROWS COLS) of the controlling terminal, or NIL when unknown."
  #-evo-windows
  (let* ((size (ignore-errors (stty "size")))
         (parts (and size (uiop:split-string size :separator '(#\Space))))
         (rows (and (= 2 (length parts)) (parse-integer (first parts) :junk-allowed t)))
         (cols (and (= 2 (length parts)) (parse-integer (second parts) :junk-allowed t))))
    (when (and rows cols (plusp rows) (plusp cols))
      (values rows cols)))
  #+evo-windows
  (multiple-value-bind (rows cols) (console-size (%std-handle +std-output-handle+))
    (when (and rows cols (plusp rows) (plusp cols))
      (values rows cols))))

;;; The system shell.
;;;
;;; The bash tool and any extension that wants a shell go through here, so
;;; "which shell, spelled how" is decided in one place.  Windows takes the
;;; command through a script file rather than an argv: the command line
;;; there is a single string that each interpreter re-splits by its own
;;; rules, so a command containing quotes (git commit -m \"...\" — that is,
;;; most of them) is mangled somewhere between us and the shell.  A file has
;;; no quoting rules at all.

(defun shell-name ()
  "How to describe this platform's shell to the model, e.g. \"/bin/sh -c\".
Deliberately not a PATH probe: this string is baked into the tool schema
when the image is dumped, so it must not depend on what happened to be
installed on the build machine.  Every supported Windows ships PowerShell;
the cmd.exe fallback in WRITE-SHELL-SCRIPT is for images that stripped it."
  #-evo-windows "/bin/sh -c"
  #+evo-windows "PowerShell")

#+evo-windows
(defun write-shell-script (command)
  "COMMAND in a scratch script, plus the argv that runs it.  Returns
(values PROGRAM ARGS PATH); the caller deletes PATH."
  (let* ((powershell (or (program-in-path "pwsh") (program-in-path "powershell")))
         (path (merge-pathnames (format nil "evo-cmd-~d-~36r.~a"
                                        (getpid) (random (expt 2 48))
                                        (if powershell "ps1" "cmd"))
                                (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (cond
        (powershell
         ;; The BOM is not decoration: Windows PowerShell 5.1 reads a
         ;; BOM-less file in the ANSI code page and would garble any
         ;; non-ASCII in the command.  The epilogue propagates a native
         ;; program's exit code, which -File otherwise swallows.
         (write-char (code-char #xFEFF) out)
         (format out "$ErrorActionPreference = 'Continue'~%")
         (format out "$OutputEncoding = [Console]::OutputEncoding = ~
                      [System.Text.Encoding]::UTF8~%")
         (format out "~a~%" command)
         (format out "if ($LASTEXITCODE -ne $null) { exit $LASTEXITCODE }~%exit 0~%"))
        (t
         (format out "@echo off~%chcp 65001 > nul~%~a~%exit /b %ERRORLEVEL%~%"
                 command))))
    (if powershell
        (values (namestring powershell)
                (list "-NoProfile" "-NonInteractive" "-ExecutionPolicy" "Bypass"
                      "-File" (namestring path))
                path)
        (values (or (uiop:getenv "COMSPEC") "cmd.exe")
                (list "/c" (namestring path))
                path))))

(defun shell-invocation (command)
  "(values PROGRAM ARGS SCRATCH) that runs COMMAND through the system
shell.  SCRATCH is a file the caller must delete when the child is done, or
NIL when the platform needed none."
  #-evo-windows (values "/bin/sh" (list "-c" command) nil)
  #+evo-windows (write-shell-script command))

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
