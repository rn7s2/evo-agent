;;;; 360-baby-evo.lisp — "Baby Evo": a desktop notification when evo goes idle.
;;;;
;;;; You start a long run, switch to another window, and then keep glancing
;;;; back to see whether evo is still working.  Baby Evo watches for the moment
;;;; the agent stops needing you and posts a macOS notification titled
;;;; "Baby Evo: I'm done!" whose body is the tail of what evo just said,
;;;; prefixed with the git repo and branch when the working directory is one —
;;;; "[repo:branch] the last thing evo said".
;;;;
;;;; It can also ANSWER BACK.  Posted through terminal-notifier with -reply,
;;;; the notification carries a reply field: whatever you type there is queued
;;;; as evo's next turn.  terminal-notifier waits for that reply, so while the
;;;; banner is up Baby Evo holds a lightweight process open in the background
;;;; and evo stays idle until you reply, dismiss it, or it times out.  With no
;;;; terminal-notifier it falls back to a plain osascript banner — fire and
;;;; forget, no reply — so the feature still works everywhere on macOS.
;;;;
;;;; WHERE "IDLE" COMES FROM.  The one honest definition of idle is "the outer
;;;; driver returned": EVO.KERNEL:RUN-UNTIL-SETTLED is what the TUI's run
;;;; worker calls, and it does not return while a turn is in flight, while
;;;; steering is queued, or while an active goal keeps re-steering itself.  So
;;;; this extension wraps that one exported function — the same unlock / patch
;;;; / ON-UNLOAD dance 900-ide-context.lisp uses on SUBMIT-TO-AGENT.  The
;;;; obvious alternative, EVO.KERNEL::*SETTLED-HOOKS*, is internal and is NOT
;;;; consulted when a run ends in :ERROR — and an errored run is exactly the
;;;; idle you most want told about.  Wrapping catches every exit.
;;;;
;;;; THE ALERT STYLE IS THE ONE THING THE USER MUST SET.  macOS decides from
;;;; the app's alert style whether a notification fades and whether it shows a
;;;; reply field at all; a CLI cannot write that (verified: the plist write is
;;;; ignored, the usernoted database is SIP-protected).  Style "Temporary"
;;;; (banner) = fades in ~5s and NO reply button; "Persistent" (alerts) =
;;;; stays until dismissed AND shows the reply field.  That is why /notify
;;;; doctor exists: it walks the user through flipping terminal-notifier's
;;;; style to Persistent, then proves it with a test.
;;;;
;;;; :ABORTED is the one outcome deliberately left silent: it means the user
;;;; pressed escape, so the user is already at the keyboard.
;;;;
;;;; MACOS ONLY.  On any other OS nothing is patched and nothing is registered
;;;; except /notify itself, which explains why it is inert.
;;;;
;;;; Settings (override in init.lisp):
;;;;   :baby-evo            t       master on/off
;;;;   :baby-evo-body-chars 140     how much of the last response to show
;;;;   :baby-evo-sound      "Ping"  macOS sound name; NIL for a silent banner
;;;;   :baby-evo-reply      t       offer the reply field (needs terminal-notifier)
;;;;   :baby-evo-timeout    120     seconds to wait for a reply; NIL = forever
;;;;
;;;; Command:  /notify [status] | on | off | doctor

(in-package :evo.user)

;;; ---------------------------------------------------------------------------
;;; Platform gate
;;; ---------------------------------------------------------------------------

;; A DEFPARAMETER rather than a call at each use: the answer cannot change
;; while the image runs, and one variable is what lets the unit tests exercise
;; the unsupported path on a Mac — the same trick EVO.MEDIA's CLIPBOARD-GAP
;; uses to stay testable off-platform.
(defparameter *baby-evo-macos-p* (and (uiop:os-macosx-p) t)
  "True when this image is running on macOS — the only supported platform.")

(defparameter *baby-evo-osascript* "osascript"
  "The always-available posting program (plain banner, no reply).")

(defparameter *baby-evo-terminal-notifier* "terminal-notifier"
  "The program that posts a banner WITH a reply field.  Named rather than
hardcoded at the call site so a test can point it somewhere harmless.")

(defparameter +baby-evo-group+ "baby-evo"
  "terminal-notifier -group id.  One notification per group is ever shown, so
a new idle banner replaces the previous one instead of stacking.")

(defun baby-evo-osascript-p ()
  (and (evo.port:program-in-path *baby-evo-osascript*) t))

(defun baby-evo-terminal-notifier-p ()
  (and (evo.port:program-in-path *baby-evo-terminal-notifier*) t))

(defun baby-evo-supported-p ()
  "True when Baby Evo is on a platform it supports.  macOS, and only macOS:
`display notification` has no equivalent evo can rely on elsewhere, and a
half-working notifier is worse than an honest refusal."
  *baby-evo-macos-p*)

(defun baby-evo-enabled-p ()
  "True when notifications are both possible and switched on."
  (and (baby-evo-supported-p) (and (evo:setting :baby-evo t) t)))

(defun baby-evo-reply-wanted-p ()
  "Whether to offer the reply field; T by default."
  (let ((v (evo:setting :baby-evo-reply t)))
    (and (not (null v)) t)))

(defun baby-evo-reply-possible-p ()
  "True when a reply-capable banner can be posted right now."
  (and (baby-evo-supported-p)
       (baby-evo-reply-wanted-p)
       (baby-evo-terminal-notifier-p)))

(defun baby-evo-timeout ()
  "Seconds terminal-notifier waits for a reply; NIL means wait forever.  120
by default so an abandoned banner cannot hold an idle evo open indefinitely.
A bad value falls back to the default rather than signalling."
  (let ((v (evo:setting :baby-evo-timeout 120)))
    (cond ((null v) nil)
          ((and (integerp v) (plusp v)) v)
          (t 120))))

;;; ---------------------------------------------------------------------------
;;; The message
;;; ---------------------------------------------------------------------------

(defparameter +baby-evo-title+ "Baby Evo: I'm done!"
  "The notification title.  Fixed on purpose: it is what the user learns to
recognize out of the corner of an eye, so it does not vary with the outcome.")

(defun baby-evo-body-chars ()
  "How many characters of the last response to show; 140 by default.  A bad
value falls back to the default rather than signalling."
  (let ((v (evo:setting :baby-evo-body-chars 140)))
    (if (and (integerp v) (plusp v)) v 140)))

(defun baby-evo-whitespace-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun baby-evo-flatten (text)
  "TEXT as one line: every run of whitespace collapses to a single space and
the result is trimmed.  A notification body is one short line — markdown
structure only wastes the few characters there are."
  (let ((out (make-string-output-stream))
        (pending nil)
        (started nil))
    (loop for c across text
          do (cond ((baby-evo-whitespace-p c) (when started (setf pending t)))
                   (t (when pending (write-char #\Space out) (setf pending nil))
                      (write-char c out)
                      (setf started t))))
    (get-output-stream-string out)))

;;; --- the git prefix ---------------------------------------------------------

(defun baby-evo-git-prefix-live (dir)
  "The \"name:branch\" of the git repository containing DIR, or NIL.
Two cheap probes, both best-effort: `git rev-parse --show-toplevel` for the
repo root (its basename is the name) and `--abbrev-ref HEAD` for the branch.
Any failure — not a repo, no git, detached nothing — yields NIL and the body
goes out unprefixed."
  (handler-case
      (flet ((git (&rest args)
               (multiple-value-bind (out err code)
                   (uiop:run-program (append (list "git" "-C" (namestring dir)) args)
                                     :output :string :error-output :string
                                     :ignore-error-status t)
                 (declare (ignore err))
                 (and (eql code 0)
                      (string-trim '(#\Space #\Newline #\Return) (or out ""))))))
        (let ((top (git "rev-parse" "--show-toplevel")))
          (when (and top (plusp (length top)))
            (let* ((name (car (last (uiop:split-string top :separator "/"))))
                   (branch (git "rev-parse" "--abbrev-ref" "HEAD")))
              (when (plusp (length name))
                (if (and branch (plusp (length branch)))
                    (format nil "~a:~a" name branch)
                    name))))))
    (error () nil)))

(defvar *baby-evo-git-lookup* #'baby-evo-git-prefix-live
  "The function (DIRECTORY) -> \"name:branch\" or NIL.  A variable so the
tests can stub the git probes without a real repository.")

(defun baby-evo-git-prefix (&optional (dir (uiop:getcwd)))
  "The \"[name:branch] \" prefix for the body, or \"\" when not in a repo."
  (let ((ctx (and (fboundp 'baby-evo-git-prefix-live)
                  (ignore-errors (funcall *baby-evo-git-lookup* dir)))))
    (if (and ctx (plusp (length ctx)))
        (format nil "[~a] " ctx)
        "")))

(defun baby-evo-body (text &optional (prefix (baby-evo-git-prefix)))
  "The notification body: PREFIX (\"[repo:branch] \" or \"\") then TEXT
flattened to one line and cut to the configured length with a single ellipsis.
Never signals and never returns the empty string — an empty body renders as a
blank banner, which reads as a bug."
  (let* ((flat (baby-evo-flatten (or text "")))
         (limit (baby-evo-body-chars)))
    (cond ((zerop (length flat)) "(evo finished without saying anything)")
          ((<= (length flat) limit) (concatenate 'string prefix flat))
          (t (concatenate 'string prefix (subseq flat 0 limit) "…")))))

(defun baby-evo-last-response-text (agent)
  "The text of the last assistant message in AGENT's journal, or NIL.
Read from the journal rather than cached from a :turn-end hook so it is still
right after a /reload, a session switch or a resume — the journal is the
truth, extension memory is not."
  (let ((journal (and agent (evo.kernel:agent-journal agent))))
    (when journal
      (let ((message (find :assistant
                           (evo.journal:state-messages
                            (evo.journal:fold-state journal))
                           :key #'evo.provider:message-role :from-end t)))
        (when message
          (evo.util:string-join
           " "
           (loop for block in (evo.provider:message-content message)
                 when (eq (evo.util:pget block :type) :text)
                   collect (or (evo.util:pget block :text) ""))))))))

;;; ---------------------------------------------------------------------------
;;; Posting it
;;; ---------------------------------------------------------------------------

(defun baby-evo-applescript-string (text)
  "TEXT as an AppleScript string literal, quotes included.
This is the whole of the injection story: the body is model output, and a
response containing a double quote would otherwise close the literal early and
leave the rest of the sentence to run as AppleScript."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across text
          do (cond ((or (char= c #\") (char= c #\\))
                    (write-char #\\ out) (write-char c out))
                   ((char< c #\Space) (write-char #\Space out))
                   (t (write-char c out))))
    (write-char #\" out)))

(defun baby-evo-sound-name ()
  "Configured sound name, or NIL for a silent banner."
  (let ((v (evo:setting :baby-evo-sound "Ping")))
    (and (stringp v) (plusp (length v)) v)))

(defun baby-evo-script (title body &optional (sound (baby-evo-sound-name)))
  "The AppleScript that posts one plain (no-reply) notification."
  (format nil "display notification ~a with title ~a~@[ sound name ~a~]"
          (baby-evo-applescript-string body)
          (baby-evo-applescript-string title)
          (and sound (baby-evo-applescript-string sound))))

(defun baby-evo-post-via-osascript (title body)
  "Post one plain banner via osascript — fire and forget, no reply.
Returns (values OK-P DETAIL).  OK-P means osascript accepted the script, NOT
that a banner appeared — osascript exits 0 either way, and only the human can
confirm delivery."
  (if (not (baby-evo-osascript-p))
      (values nil (format nil "no ~a on PATH" *baby-evo-osascript*))
      (multiple-value-bind (out err code)
          (uiop:run-program (list *baby-evo-osascript*
                                  "-e" (baby-evo-script title body))
                            :output :string :error-output :string
                            :ignore-error-status t)
        (declare (ignore out))
        (if (eql code 0)
            (values t nil)
            (values nil (string-trim '(#\Space #\Newline #\Return)
                                     (or err "")))))))

;;; --- the reply path (terminal-notifier, waits for the human) ----------------

(defparameter +baby-evo-reply-prompt+ "Reply to steer…"
  "The placeholder in the notification's reply field.")

(defun baby-evo-reply-argv (title body)
  "The terminal-notifier argv for a reply banner.  No -timeout when the
setting is NIL, which is what makes it wait forever.  -sound carries the
configured notification sound (a macOS sound name); omitting it is how a NIL
:baby-evo-sound asks for a silent banner."
  (append (list "-title" title
                "-message" body
                "-group" +baby-evo-group+
                "-reply" +baby-evo-reply-prompt+)
          (let ((sound (baby-evo-sound-name)))
            (and sound (list "-sound" sound)))
          (let ((timeout (baby-evo-timeout)))
            (and timeout (list "-timeout" (princ-to-string timeout))))))

;; DEFPARAMETER, not DEFVAR: a reload must pick up the current definition, and
;; DEFVAR would leave a previously-loaded (possibly stale) launcher in place.
(defparameter *baby-evo-launcher*
  (lambda (argv outfile)
    "Spawn terminal-notifier with ARGV, stdout redirected to OUTFILE, and
return the process handle.  A variable so the tests can substitute a fake
that answers immediately.  stderr is merged INTO stdout (:error-output
:output): passing the same pathname for both would open the file twice and
fail with 'File exists'.  The program is resolved to an absolute path because
LAUNCH-CHILD does not search PATH."
    (evo.port:launch-child (namestring (evo.port:program-in-path
                                        *baby-evo-terminal-notifier*))
                           argv
                           :input nil :output outfile :error-output :output))
  "Spawns the reply-notification process; see the lambda.")

(defvar *baby-evo-process-waiter* #'evo.port:process-wait
  "How the watch thread blocks on the reply process.  A variable so tests can
return immediately.")

;;; The pending alert.  Exactly ONE reply banner is ever outstanding: the slot
;;; holds its process and the thread waiting on it, guarded by a lock.  The
;;; watch is a BACKGROUND thread, never the run worker — a blocking wait on the
;;; worker would freeze evo the moment it went idle.

(defstruct (baby-evo-alert (:constructor %make-baby-evo-alert))
  process      ; the terminal-notifier child
  thread       ; the background thread waiting on it
  cancelled-p) ; set when evo became non-idle; the watch must not steer

(defvar *baby-evo-alert* nil
  "The outstanding reply alert, or NIL.  Owned by *BABY-EVO-ALERT-LOCK*.")

(defvar *baby-evo-alert-lock* (bt:make-lock "baby-evo-alert"))

(defun baby-evo-cancel-alert ()
  "Dismiss the outstanding reply banner, if any: kill its process and ask the
watch thread to stop.  Safe to call repeatedly and from any thread; the watch
thread reaps its own process.  Returns T if there was an alert to cancel.

Called whenever evo stops being idle — the user submitted a new message, a
goal re-steered, the feature was switched off — because a 'I am done' banner
is a lie the instant evo is working again, and a stale reply field must not
steer a turn the user has moved past."
  (let ((alert (bt:with-lock-held (*baby-evo-alert-lock*)
                 (prog1 *baby-evo-alert*
                   (when *baby-evo-alert*
                     (setf (baby-evo-alert-cancelled-p *baby-evo-alert*) t)
                     (setf *baby-evo-alert* nil))))))
    (when alert
      (ignore-errors
        (when (and (baby-evo-alert-process alert)
                   (evo.port:process-alive-p (baby-evo-alert-process alert)))
          (evo.port:process-kill (baby-evo-alert-process alert))))
      ;; Interrupt the watch thread so it returns promptly even if the process
      ;; is mid-wait; it reaps its own child in its unwind-protect.
      (let ((thread (baby-evo-alert-thread alert)))
        (when (and thread (bt:thread-alive-p thread))
          (ignore-errors
            (bt:interrupt-thread thread (lambda () (throw :baby-evo-stop nil))))))
      t)))

(defun baby-evo-read-reply (outfile code)
  "Interpret a finished reply process: the typed text, or NIL for timeout /
dismissed / cancelled.  terminal-notifier prints the reply on stdout; a
timeout exits 6 with @TIMEOUT, a click prints @ACTIONCLICKED, a close @CLOSED."
  (let* ((raw (ignore-errors (evo.util:read-file-string outfile)))
         (trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) (or raw ""))))
    (cond ((or (eql code 6)
               (string= trimmed "")
               (string= trimmed "@TIMEOUT")
               (string= trimmed "@CLOSED")
               (string= trimmed "@ACTIONCLICKED"))
           nil)
          (t trimmed))))

(defun baby-evo-watch-reply (agent process outfile title)
  "The body of the background watch thread: block on PROCESS, then either
steer AGENT with the reply or do nothing.  This thread OWNS the process — it
launched nothing but it is the only thread that waits on or reaps it, which is
what the process-ownership rule requires.  Never signals."
  (catch :baby-evo-stop
    (unwind-protect
         (handler-case
             (multiple-value-bind (status code)
                 (funcall *baby-evo-process-waiter* process)
               (declare (ignore status))
               (let ((reply (baby-evo-read-reply outfile code)))
                 ;; Only steer if evo is STILL idle — a cancelled alert means
                 ;; the user moved on without it, and its reply is stale.
                 (let ((cancelled (bt:with-lock-held (*baby-evo-alert-lock*)
                                    (or (null *baby-evo-alert*)
                                        (baby-evo-alert-cancelled-p
                                         *baby-evo-alert*)))))
                   (when (and reply (not cancelled))
                     (evo.kernel:queue-followup agent reply)
                     ;; The agent may be sitting idle; nudge a repaint so the
                     ;; queued follow-up is noticed at the next opportunity.
                     (ignore-errors (evo.tui:request-repaint))))))
           (error (e)
             (warn "baby-evo: reply watch failed for ~s: ~a" title e)))
      (ignore-errors
        (when (and process (evo.port:process-alive-p process))
          (evo.port:process-kill process)))
      (ignore-errors (delete-file outfile))
      ;; Clear the slot if it is still OUR alert (a cancel already nilled it).
      (bt:with-lock-held (*baby-evo-alert-lock*)
        (when (and *baby-evo-alert*
                   (eq (baby-evo-alert-process *baby-evo-alert*) process))
          (setf *baby-evo-alert* nil)))))
  nil)

(defun baby-evo-post-reply (agent title body)
  "Post a reply-capable banner and return at once.  The wait for the human
happens on a background watch thread; a typed reply is queued as AGENT's next
turn.  Returns (values OK-P DETAIL)."
  (if (not (baby-evo-reply-possible-p))
      (values nil "reply not possible (need terminal-notifier and :baby-evo-reply)")
      (handler-case
          (progn
            ;; One alert at a time: a fresh idle replaces the last banner.
            (baby-evo-cancel-alert)
            (let* ((outfile (merge-pathnames
                             (format nil "baby-evo-reply-~a.out" (evo.util:gen-id 8))
                             (uiop:temporary-directory)))
                   (process (funcall *baby-evo-launcher*
                                     (baby-evo-reply-argv title body)
                                     outfile))
                   (alert (%make-baby-evo-alert :process process)))
              (bt:with-lock-held (*baby-evo-alert-lock*)
                (setf *baby-evo-alert* alert))
              (setf (baby-evo-alert-thread alert)
                    (bt:make-thread
                     (lambda () (baby-evo-watch-reply agent process outfile title))
                     :name "baby-evo-reply-watch"))
              (values t nil)))
        (error (e) (values nil (format nil "~a" e))))))

;;; --- the transport dispatch ---------------------------------------------------

(defun baby-evo-post-default (agent title body)
  "The default transport: a reply-capable banner when terminal-notifier and
the setting allow, else a plain osascript banner."
  (if (baby-evo-reply-possible-p)
      (baby-evo-post-reply agent title body)
      (baby-evo-post-via-osascript title body)))

(defparameter *baby-evo-poster* #'baby-evo-post-default
  "The function (AGENT TITLE BODY) -> (values OK-P DETAIL) that actually posts.
A reply-capable poster queues any reply as AGENT's next turn itself.  A
variable rather than a direct call so the whole idle -> title -> body -> reply
chain can be exercised without a real banner — the same reason EVO.MEDIA keeps
its clipboard readers and downscalers in variables.")

(defun baby-evo-post (agent title body)
  "Post one notification through the platform gate.  Returns (values OK-P
DETAIL).

Errors are values, not conditions: the only caller sits in the return path of
every run, where a signalled condition would turn a finished run into a broken
one over a cosmetic banner."
  (if (not (baby-evo-supported-p))
      (values nil (format nil "unsupported platform (~a) — macOS only"
                          (software-type)))
      (handler-case (funcall *baby-evo-poster* agent title body)
        (error (e) (values nil (format nil "~a" e))))))

;;; ---------------------------------------------------------------------------
;;; Announce + record
;;; ---------------------------------------------------------------------------

(defvar *baby-evo-last-result* nil
  "Plist (:at :ok :detail :body :reply) for the most recent attempt — what
/notify status reports, and what makes a silent failure visible instead of
silent.")

(defun baby-evo-record (ok detail body &key reply)
  (setf *baby-evo-last-result*
        (list :at (evo.util:iso8601-now) :ok (and ok t)
              :detail detail :body body :reply reply))
  (values ok detail))

(defun baby-evo-announce (agent)
  "Post the idle notification for AGENT: title fixed, body the truncated last
response (git-prefixed).  Returns (values OK-P DETAIL)."
  (let ((body (baby-evo-body (baby-evo-last-response-text agent))))
    (multiple-value-bind (ok detail)
        (baby-evo-post agent +baby-evo-title+ body)
      (baby-evo-record ok detail body))))

(defun baby-evo-send-test-notification
    (&optional (body "This is a test notification from Baby Evo."))
  "Post a test banner right now, bypassing the on/off setting so /notify
doctor can prove the delivery path independently of the toggle.  With a reply
banner the reply is NOT steered into a turn (this is a test); it is just
reported in the last-attempt record."
  (multiple-value-bind (ok detail)
      (baby-evo-post evo:*agent* +baby-evo-title+ body)
    (baby-evo-record ok detail body)))

;;; ---------------------------------------------------------------------------
;;; Settings / setup helpers (for the agent driving /notify doctor)
;;; ---------------------------------------------------------------------------

(defparameter +baby-evo-settings-url+
  "x-apple.systempreferences:com.apple.Notifications-Settings.extension"
  "Deep link to System Settings > Notifications.")

(defun baby-evo-run-open (url)
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program (list "open" url)
                            :output :string :error-output :string
                            :ignore-error-status t)
        (declare (ignore out))
        (if (eql code 0) (values t nil)
            (values nil (string-trim '(#\Space #\Newline) (or err "")))))
    (error (e) (values nil (format nil "~a" e)))))

(defun baby-evo-open-notification-settings ()
  "Open System Settings at the Notifications pane.  Returns (values OK-P DETAIL)."
  (if (not *baby-evo-macos-p*)
      (values nil "not macOS")
      (baby-evo-run-open +baby-evo-settings-url+)))

(defun baby-evo-open-terminal-notifier-style ()
  "Open System Settings at terminal-notifier's own Notifications pane — where
the alert style that decides the fade and the reply field lives.  Returns
(values OK-P DETAIL)."
  (if (not *baby-evo-macos-p*)
      (values nil "not macOS")
      (baby-evo-run-open
       (format nil "~a?id=fr.julienxx.oss.terminal-notifier"
               +baby-evo-settings-url+))))

(defun baby-evo-open-script-editor-style ()
  "Open System Settings at Script Editor's Notifications pane — relevant to
the osascript fallback.  Returns (values OK-P DETAIL)."
  (if (not *baby-evo-macos-p*)
      (values nil "not macOS")
      (baby-evo-run-open
       (format nil "~a?id=com.apple.ScriptEditor2" +baby-evo-settings-url+))))

(defun baby-evo-brew-p ()
  (and (evo.port:program-in-path "brew") t))

(defun baby-evo-install-terminal-notifier ()
  "Install terminal-notifier via Homebrew — the program that adds the reply
field.  Returns (values OK-P DETAIL).  Never run silently from a hook; only
ever at the user's explicit request inside /notify doctor."
  (cond ((not *baby-evo-macos-p*) (values nil "not macOS"))
        ((baby-evo-terminal-notifier-p) (values t "already installed"))
        ((not (baby-evo-brew-p)) (values nil "no brew on PATH"))
        (t
         (handler-case
             (multiple-value-bind (out err code)
                 (uiop:run-program (list "brew" "install" "terminal-notifier")
                                   :output :string :error-output :string
                                   :ignore-error-status t)
               (declare (ignore out))
               (if (eql code 0)
                   (values (baby-evo-terminal-notifier-p)
                           (and (not (baby-evo-terminal-notifier-p))
                                "installed but not on PATH yet — open a new shell"))
                   (values nil (string-trim '(#\Space #\Newline) (or err "")))))
           (error (e) (values nil (format nil "~a" e)))))))

(defun baby-evo-diagnose ()
  "Run `terminal-notifier -diagnose` and return its report (or the osascript
situation when terminal-notifier is absent).  How the agent reads the live
alert style, which it cannot change itself."
  (if (baby-evo-terminal-notifier-p)
      (handler-case
          (string-trim '(#\Space #\Newline)
                       (or (uiop:run-program (list *baby-evo-terminal-notifier* "-diagnose")
                                             :output :string :error-output :string
                                             :ignore-error-status t)
                           ""))
        (error (e) (format nil "diagnose failed: ~a" e)))
      "terminal-notifier not installed — reply field unavailable; osascript banner only"))

(defun baby-evo-sound-names ()
  "The macOS sound names usable for :baby-evo-sound, from
/System/Library/Sounds.  Best-effort; empty if the directory is unreadable."
  (ignore-errors
    (sort (loop for f in (directory "/System/Library/Sounds/*.aiff")
                collect (pathname-name f))
          #'string<)))

;;; ---------------------------------------------------------------------------
;;; The idle seam: wrap RUN-UNTIL-SETTLED
;;; ---------------------------------------------------------------------------

(defvar *baby-evo-original-run-until-settled* nil
  "The unpatched EVO.KERNEL:RUN-UNTIL-SETTLED.  DEFVAR, not DEFPARAMETER: a
reload must not overwrite the saved original with NIL, or the restore would
install the wrapper as if it were the original and stack a second one.")

(defun baby-evo-pending-work-p (agent)
  "True when AGENT still has queued work that has not run yet — steering or
follow-ups sitting in its mailbox.  A notification fired while work is queued
would be a lie: evo is not idle, it is about to run again.  (In practice
run-until-settled has already drained these before returning, but the check
is the difference between 'usually right' and 'cannot fire early'.)"
  (and agent (evo.kernel:agent-pending-work-p agent)))

(defun baby-evo-idle-outcome-p (agent outcome)
  "True when OUTCOME means evo is now genuinely idle — done, and waiting for
the human.  This is the whole correctness of the feature, so it is explicit:

- :stop    — the run finished.  A COMPLETED goal arrives here (its work is
             done); so does a plain reply.  NOTIFY.
- :error   — the run gave up (final, non-retryable).  The agent is stuck
             until the human looks; that is exactly when to tell them.  NOTIFY.
- :length  — output truncated, waiting on the human to continue.  NOTIFY.
- :aborted — the user pressed escape, so the user is already at the keyboard.
             A banner then is pure noise.  DO NOT notify.

What about queued work and ACTIVE goals?  They never reach here with work
left: run-until-settled only returns after draining steering and follow-ups,
and an ACTIVE goal's settled-hook re-steers the loop so it keeps running.
So by the time OUTCOME is in hand there is no pending turn — and the
defensive BABY-EVO-PENDING-WORK-P check above closes even that door.  An
active goal therefore does NOT notify (it is still working); a completed one
DOES (it just finished)."
  (and (not (eq outcome :aborted))
       (not (baby-evo-pending-work-p agent))))

(defun baby-evo-on-idle (agent outcome)
  "Evo just went idle with OUTCOME.  Notify only when it is genuinely idle —
see BABY-EVO-IDLE-OUTCOME-P — and the feature is on."
  (when (and (baby-evo-enabled-p)
             (baby-evo-idle-outcome-p agent outcome))
    (baby-evo-announce agent)))

(defun baby-evo-run-until-settled (agent)
  "Wrapper: dismiss any stale 'done' banner the moment a run starts (evo is no
longer idle), run the real driver, then announce that evo is idle.

Two rules this must never break, because it sits in the call path of every
run: the original outcome is returned unchanged, and nothing here signals."
  ;; Entry = a run is starting = evo is working again.  A 'I am done' banner
  ;; left over from the last idle is now a lie, and its reply field must not
  ;; steer a turn the user has already moved past by typing directly.
  (handler-case (baby-evo-cancel-alert)
    (error (e) (warn "baby-evo: dismissing stale alert failed: ~a" e)))
  (let ((outcome (funcall *baby-evo-original-run-until-settled* agent)))
    (handler-case (baby-evo-on-idle agent outcome)
      (error (e) (warn "baby-evo: notification failed: ~a" e)))
    outcome))

(defun baby-evo-install ()
  "Patch RUN-UNTIL-SETTLED.  Idempotent: the original is captured once, so
loading this file again replaces the wrapper instead of wrapping the wrapper."
  (let ((pkg (symbol-package 'evo.kernel:run-until-settled)))
    (unless *baby-evo-original-run-until-settled*
      (setf *baby-evo-original-run-until-settled*
            (symbol-function 'evo.kernel:run-until-settled)))
    (evo.port:unlock-package pkg)
    (unwind-protect
         (setf (symbol-function 'evo.kernel:run-until-settled)
               #'baby-evo-run-until-settled)
      (evo.port:lock-package pkg))
    t))

(defun baby-evo-uninstall ()
  "Put RUN-UNTIL-SETTLED back the way this extension found it and dismiss any
outstanding banner.  The kernel cannot see a function patch or a background
watch, so the undo is handed back via ON-UNLOAD."
  (baby-evo-cancel-alert)
  (when *baby-evo-original-run-until-settled*
    (let ((pkg (symbol-package 'evo.kernel:run-until-settled)))
      (evo.port:unlock-package pkg)
      (unwind-protect
           (setf (symbol-function 'evo.kernel:run-until-settled)
                 *baby-evo-original-run-until-settled*)
        (evo.port:lock-package pkg)))
    (setf *baby-evo-original-run-until-settled* nil))
  t)

(defun baby-evo-installed-p ()
  "True when the idle seam is currently patched in."
  (and *baby-evo-original-run-until-settled* t))

;;; ---------------------------------------------------------------------------
;;; /notify
;;; ---------------------------------------------------------------------------

(defun baby-evo-last-attempt-text ()
  "One line about the most recent attempt, or NIL if there has not been one."
  (let ((last *baby-evo-last-result*))
    (when last
      (format nil "~a — ~a~@[ (~a)~]~@[ reply: ~s~]"
              (evo.util:pget last :at)
              (if (evo.util:pget last :ok) "posted" "FAILED")
              (evo.util:pget last :detail)
              (evo.util:pget last :reply)))))

(defun baby-evo-status-text ()
  "What is on, what is possible, and what happened last."
  (cond
    ((not *baby-evo-macos-p*)
     (format nil (evo:cat "baby-evo: unsupported — idle notifications need "
                          "macOS, and this is ~a.~%"
                          "Nothing is installed; /notify has nothing to "
                          "switch on here.")
             (software-type)))
    (t
     (let ((reply (baby-evo-reply-possible-p)))
       (format nil (evo:cat "baby-evo ~a · idle seam ~a · body ~d chars · "
                            "sound ~a~%"
                            "mode: ~a~%"
                            "title: ~a~%"
                            "last: ~a")
               (if (evo:setting :baby-evo t) "on" "off")
               (if (baby-evo-installed-p) "installed" "NOT installed")
               (baby-evo-body-chars)
               (or (baby-evo-sound-name) "silent")
               (cond (reply (format nil "reply field (waits ~:[forever~;~as~])"
                                    (baby-evo-timeout) (baby-evo-timeout)))
                     ((baby-evo-terminal-notifier-p)
                      "banner only — :baby-evo-reply is off")
                     (t "banner only — no terminal-notifier (replies need it; /notify doctor)"))
               +baby-evo-title+
               (or (baby-evo-last-attempt-text)
                   "nothing posted yet — /notify doctor sets up and tests it"))))))

(defun baby-evo-doctor-prompt ()
  "The turn Baby Evo asks the agent to take when the user types /notify doctor.

Deliberately a PROMPT and not a script: setting notifications up is a
conversation with checkpoints only a human can pass — did a banner actually
appear? does it have a reply field? — so the work belongs in the normal chat
turn loop, where the agent can look, ask and adapt.  What this hands over is
the diagnosis the user cannot be expected to know, above all that the ALERT
STYLE is human-set and a CLI cannot write it."
  (format nil
          (evo:cat
           "The user just ran `/notify doctor`. Walk them through getting "
           "Baby Evo's idle notifications working — ideally WITH the reply "
           "field — interactively, one step at a time: ask, act, check, adapt "
           "to what they say. Do not dump all of this on them at once.~%~%"
           "Diagnosis already gathered:~%"
           "- platform: ~a (macOS is required: ~:[NO — say so and stop~;yes~])~%"
           "- terminal-notifier on PATH: ~:[NO — the reply field needs it~;yes~]~%"
           "- reply field available: ~:[no~;yes~]~%"
           "- current alert style / report:~%~a~%"
           "- setting :baby-evo: ~:[off~;on~]~%"
           "- idle seam patched into run-until-settled: ~:[NO~;yes~]~%"
           "- last attempt: ~a~%~%"
           "Things you know that the user does not:~%"
           "- The reply field and the non-fading banner BOTH come from one "
           "human-only setting: System Settings > Notifications > "
           "terminal-notifier > Alert Style = \"Persistent\" (not "
           "\"Temporary\"). A CLI CANNOT set this — the plist write is "
           "ignored and the database is SIP-protected, verified. Do not try "
           "to script it; guide the user to flip it.~%"
           "- \"Temporary\" = fades in about 5s and shows NO reply button. "
           "\"Persistent\" = stays until dismissed and shows the reply field.~%"
           "- terminal-notifier's OWN permission is the one that matters for "
           "the reply path. The first banner macOS shows its grant prompt; "
           "if the user dismissed it, the toggle is System Settings > "
           "Notifications > terminal-notifier > Allow notifications. Confirm "
           "it is on — this is the permission to check, not evo's.~%"
           "- If terminal-notifier is not installed, the reply field is "
           "impossible; offer to run `(evo.user::baby-evo-install-terminal-notifier)` "
           "(a `brew install` — ASK first), and note that osascript still "
           "gives a plain banner without it.~%"
           "- For the osascript FALLBACK, macOS attributes the banner to "
           "Script Editor; its row (enable + Alert Style = Persistent) is the "
           "fallback equivalent, and there is no \"evo\" row.~%"
           "- The notification sound is the :baby-evo setting `:baby-evo-sound` "
           "(default \"Ping\"). Any name from `(evo.user::baby-evo-sound-names)` "
           "works; NIL makes a silent banner. This is a real, changeable "
           "setting — offer to set it if the user asks.~%"
           "- Both tools exit 0 whether or not a banner appeared, so the only "
           "proof is the user's own eyes. Always ask.~%"
           "- A Focus mode silently suppresses banners; if they see nothing "
           "and permissions look right, ask about Focus / Do Not Disturb.~%~%"
           "Tools for this, through the eval tool:~%"
           "- `(evo.user::baby-evo-send-test-notification)` posts a test "
           "banner now, ignoring the on/off setting. With terminal-notifier "
           "this BLOCKS until they reply, dismiss, or time out — tell them a "
           "banner is coming and to try the reply field.~%"
           "- `(evo.user::baby-evo-open-terminal-notifier-style)` opens "
           "terminal-notifier's Notifications pane (where Alert Style lives).~%"
           "- `(evo.user::baby-evo-open-script-editor-style)` and "
           "`(evo.user::baby-evo-open-notification-settings)` for the fallback.~%"
           "- `(evo.user::baby-evo-install-terminal-notifier)` brew-installs it.~%"
           "- `(evo.user::baby-evo-diagnose)` re-reads the live alert style.~%"
           "- `(evo.user::baby-evo-status-text)` re-reads the current state.~%"
           "- `/notify on` is what finally switches the feature on — theirs "
           "to type, so tell them to.~%~%"
           "Suggested shape: confirm the platform. If terminal-notifier is "
           "missing, ask whether to install it. If the alert style is not "
           "Persistent, open the pane and have them set Alert Style = "
           "Persistent, then re-check with `baby-evo-diagnose`. Then send one "
           "test banner and ASK: did it appear, did it stay, and did the "
           "reply field work? If yes, tell them to run `/notify on` if it is "
           "off, and stop. If no, fix delivery (enabled, Focus off, style "
           "Persistent) and test again.")
          (software-type)
          *baby-evo-macos-p*
          (baby-evo-terminal-notifier-p)
          (baby-evo-reply-possible-p)
          (baby-evo-diagnose)
          (and (evo:setting :baby-evo t) t)
          (baby-evo-installed-p)
          (or (baby-evo-last-attempt-text) "none this session")))

(defun baby-evo-command (ctx)
  "/notify [status] | on | off | doctor"
  (let* ((raw (string-trim " " (or (evo.util:pget ctx :args) "")))
         (arg (string-downcase raw)))
    (cond
      ((or (string= arg "") (string= arg "status"))
       (baby-evo-status-text))
      ((string= arg "on")
       (if (not (baby-evo-supported-p))
           (baby-evo-status-text)
           (progn (evo:set-setting :baby-evo t)
                  (baby-evo-install)
                  "baby-evo on — a notification when evo goes idle")))
      ((string= arg "off")
       (if (not (baby-evo-supported-p))
           (baby-evo-status-text)
           (progn (evo:set-setting :baby-evo nil)
                  ;; Dismiss any banner already up — the feature is off now.
                  (baby-evo-cancel-alert)
                  ;; The seam stays patched: it is a cheap no-op while the
                  ;; setting is off, and leaving it in place means /notify on
                  ;; takes effect immediately without re-patching a function
                  ;; some thread may already be inside.
                  "baby-evo off — no more idle notifications")))
      ((string= arg "doctor")
       (if (not *baby-evo-macos-p*)
           (baby-evo-status-text)
           ;; Hand the work to the agent as an ordinary turn: the TUI's
           ;; command dispatcher starts the run worker as soon as it sees
           ;; queued steering, so this returns and the conversation continues.
           (progn (evo:steer (baby-evo-doctor-prompt)
                             (or (evo.util:pget ctx :agent) evo:*agent*))
                  "◆ baby-evo doctor — handing over to the agent…")))
      (t "usage: /notify [status | on | off | doctor]"))))

(evo:register-command
 "notify" #'baby-evo-command
 :description "Baby Evo idle notifications: status | on | off | doctor")

;;; ---------------------------------------------------------------------------
;;; Install
;;; ---------------------------------------------------------------------------

(defun baby-evo-session-start (payload)
  "Re-patch when a session (re)starts.  The journal replays this file's :load,
but the patch lives in memory, not in the journal."
  (declare (ignore payload))
  (when (baby-evo-supported-p) (baby-evo-install)))

;; NAMEd: this file is re-loaded on every /reload and on :load replay, and an
;; anonymous hook would install another copy of itself each time.
(when (baby-evo-supported-p)
  (evo:on :session-start #'baby-evo-session-start :name :baby-evo-install)
  (baby-evo-install)
  (evo:on-unload #'baby-evo-uninstall))
