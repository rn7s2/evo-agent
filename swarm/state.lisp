;;;; state.lisp — the swarm and its lanes, as data.
;;;;
;;;; One lock guards all of it (design.md §6): a lane's subscriber thread,
;;;; the coordinator's tools on its run thread, and the TUI's status line
;;;; each take it for the few slots they touch, and nobody holds it across
;;;; I/O.

(in-package :evo.swarm)

(defstruct (lane (:constructor %make-lane))
  "One worker lane — a supervised `evo serve` process.  Every slot is guarded
by the swarm's lock: the lane's subscriber thread, the coordinator's tools
(on its run thread) and the TUI's status segment all read or write it."
  n port token
  process            ; the launch handle: the lane's own supervisor parent
  pid                ; the serve child's pid, from /health and `hello`
  dir                ; <swarm dir>/lane-N/: sessions, token, log, url
  cwd                ; where the lane works: the swarm's cwd or a worktree
  worktree branch    ; set while the lane is isolated in a git worktree
  (state :starting)  ; :starting :idle :working :compacting :down :stopped
  task               ; the task it was last given
  task-started step-started
  (reports nil)      ; report plists, newest first
  (extra-forms nil)  ; code the coordinator evaluated into it, oldest first
  (cursor 0)         ; last event id seen from this process
  subscriber         ; the thread reading its events
  (restarts 0)
  (stopping nil)     ; set while the swarm itself stops or restarts it
  (watched nil)      ; the TUI is showing its live transcript
  (partial ""))      ; streamed text not yet shown as a whole line

(define-condition lane-error (error)
  ((lane :initarg :lane :reader lane-error-lane)
   (status :initarg :status :reader lane-error-status)
   (text :initarg :text :reader lane-error-text))
  (:report (lambda (c s)
             (format s "lane ~a: ~@[HTTP ~a: ~]~a"
                     (lane-n (lane-error-lane c)) (lane-error-status c)
                     (lane-error-text c)))))

(defstruct (swarm (:constructor %make-swarm))
  id dir cwd workers lanes evo-binary agent
  (lock (bt:make-lock "evo-swarm"))
  (stopping nil))

(defvar *swarm* nil "The running swarm.")

(defmacro with-swarm-lock (() &body body)
  `(bt:with-lock-held ((swarm-lock *swarm*)) ,@body))

(defun find-lane (n &optional (swarm *swarm*))
  (and swarm (find n (swarm-lanes swarm) :key #'lane-n)))

(defun lane-snapshot (lane)
  "LANE's status as a plist, read under the lock."
  (with-swarm-lock ()
    (list :n (lane-n lane) :state (lane-state lane) :task (lane-task lane)
          :pid (lane-pid lane) :port (lane-port lane)
          :worktree (lane-worktree lane) :branch (lane-branch lane)
          :restarts (lane-restarts lane)
          :step-age (and (lane-step-started lane)
                         (member (lane-state lane) '(:working :compacting))
                         (- (get-universal-time) (lane-step-started lane)))
          :reports (length (lane-reports lane)))))

(defun fresh-token ()
  "A lane's bearer token: 32 bytes from the OS, as hex."
  (format nil "~(~{~2,'0x~}~)" (coerce (evo.port:random-octets 32) 'list)))
