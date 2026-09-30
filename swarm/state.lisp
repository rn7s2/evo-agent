;;;; state.lisp — the swarm and its lanes, as data.
;;;;
;;;; One lock guards all of it (design.md §6): a lane's mirror thread, the
;;;; coordinator's tools on its run thread, and the TUI's status line each take
;;;; it for the few slots they touch, and nobody holds it across I/O.
;;;;
;;;; Times are absolute epoch milliseconds (CONTRACT §4.3): a client renders
;;;; clocks itself, so the swarm never publishes an age.

(in-package :evo.swarm)

(defstruct (lane (:constructor %make-lane))
  "One worker lane — an `evo-agent serve` process the coordinator owns.  Every
slot is guarded by the swarm's lock: the lane's mirror thread, the coordinator's
tools (on its run thread) and the TUI's status segment all read or write it."
  n dir                 ; <swarm dir>/lane-N/: ready file, sessions, log
  cwd                   ; where the lane works: the swarm's cwd or a worktree
  worktree branch       ; set while the lane is isolated in a git worktree
  process               ; the launch handle for the lane's process
  stdin                 ; the pipe that holds it open (--watch-stdin: EOF stops it)
  ready                 ; its ready-file plist: port, token, epoch, pid, session
  (state :starting)     ; :starting :idle :working :compacting :down :stopped
  task task-started step-started
  (reports 0)           ; how many reports it has sent
  (extra-forms nil)     ; code the coordinator evaluated into it, oldest first
  subscriber            ; the thread mirroring its snapshot and stream
  mirror                ; the lane:N mirror (mirror.lisp)
  epoch                 ; the process epoch its stream is on
  session-path          ; its exact current journal path (--resume for a restart)
  (restarts 0)
  (stopping nil)        ; set while the swarm itself stops or restarts it
  (watched nil)         ; the TUI is showing its live items
  (watch-printed (make-hash-table :test #'equal))) ; item id -> how much was shown

(define-condition lane-error (error)
  ((lane :initarg :lane :reader lane-error-lane)
   (code :initarg :code :initform nil :reader lane-error-code)
   (text :initarg :text :reader lane-error-text))
  (:report (lambda (c s)
             (format s "lane ~a: ~@[~a: ~]~a"
                     (lane-n (lane-error-lane c)) (lane-error-code c)
                     (lane-error-text c)))))

(defstruct (swarm (:constructor %make-swarm))
  id dir cwd workers lanes evo-binary agent
  view                  ; where its notices go and how it runs: a VIEW (view.lisp)
  server                ; the coordinator's serve server, or NIL (the TUI)
  lane-model lane-provider   ; what the lanes run (CONTRACT §1, §4.3), or NIL
  lane-thinking
  (coordinator-busy nil)  ; is the coordinator's session running a task right now
  (lock (bt:make-lock "evo-swarm"))
  (stopping nil)
  published)            ; the swarm topic state last published, for diffing

(defvar *swarm* nil "The running swarm.")

(defmacro with-swarm-lock (() &body body)
  `(bt:with-lock-held ((swarm-lock *swarm*)) ,@body))

(defun find-lane (n &optional (swarm *swarm*))
  (and swarm (find n (swarm-lanes swarm) :key #'lane-n)))

(defun lane-topic (lane)
  "The topic name a lane is published under (CONTRACT §4.3)."
  (format nil "lane:~d" (lane-n lane)))

(defun lane-status-plist (lane)
  "LANE's status as a plist, read under the lock: what the swarm's own tools
and status line show (the LANE:SNAPSHOT name belongs to the protocol call in
client.lisp, which answers a lane's own /snapshot)."
  (with-swarm-lock ()
    (let ((state (and (lane-mirror lane) (mirror-lane-state (lane-mirror lane)))))
      (list :n (lane-n lane) :state (lane-state lane) :task (lane-task lane)
            :pid (getf (lane-ready lane) :pid)
            :worktree (lane-worktree lane) :branch (lane-branch lane)
            :restarts (lane-restarts lane)
            :model (getf state :model)
            :goal (getf state :goal)
            :reports (lane-reports lane)))))
