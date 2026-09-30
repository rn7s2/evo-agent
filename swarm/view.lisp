;;;; view.lisp — how the coordinator is shown and run: the swarm's frontend
;;;; seam.
;;;;
;;;; The swarm needs four things from whatever frontend the coordinator runs
;;;; under, and nothing else: somewhere to put a notice, a nudge to redraw,
;;;; somewhere to publish an event only a machine reads, and the call that runs
;;;; the session until it quits.  Both frontends answer them — the TUI, and
;;;; `evo serve` for a headless coordinator — so nothing below this file names
;;;; one: the rest of the swarm reaches the frontend only through SWARM-SAY,
;;;; SWARM-REPAINT, SWARM-PUBLISH and SWARM-RUN.  That is what keeps the swarm
;;;; the same program over HTTP as in a terminal.
;;;;
;;;; A view is not the kernel's frontend object (EVO.KERNEL:*FRONTEND*): that
;;;; one answers "is a human here, and who starts a run for off-thread input",
;;;; which the TUI's stateless TUI-FRONTEND and serve's server answer
;;;; themselves.  A view is where the coordinator's output goes.

(in-package :evo.swarm)

(defclass view () ()
  (:documentation "Where a coordinator's notices go, and how it runs.  One
instance per swarm, in the SWARM's VIEW slot."))

(defgeneric view-say (view text &key style)
  (:documentation "Show TEXT in VIEW.  STYLE is the command layer's: :plain,
:dim, :notice, :success, :error.  Safe from any thread."))

(defgeneric view-repaint (view)
  (:documentation "The lanes changed: let VIEW redraw, if it has a screen."))

(defgeneric view-publish (view event)
  (:documentation "Publish EVENT (a plist with :type) on VIEW's event stream,
when it has one.  What only a machine reads — lane state — travels this way
rather than as a notice; a view with no stream ignores it."))

(defgeneric view-run (view agent resumed-p)
  (:documentation "Run AGENT's session under VIEW until it quits, RESUMEed-P
telling it whether the session was already started.  Returns the process's
exit code."))

;;; The TUI.  Stateless: EVO.TUI's own dynamic default finds the live TUI, so
;;; a view made before the terminal comes up still reaches it.

(defclass tui-view (view) ()
  (:documentation "The coordinator in the human's terminal."))

(defmethod view-say ((view tui-view) text &key (style :dim))
  (declare (ignore view))
  (evo.tui:post-notice text :style style))

(defmethod view-repaint ((view tui-view))
  (declare (ignore view))
  (evo.tui:request-repaint))

(defmethod view-publish ((view tui-view) event)
  ;; The TUI has no event stream: its screen is the whole of it.
  (declare (ignore view event))
  nil)

(defmethod view-run ((view tui-view) agent resumed-p)
  (declare (ignore view))
  (evo.tui:start-tui agent :resumed-p resumed-p))

;;; serve.  The coordinator is a session on a socket: what it says is an event
;;; on its log, and running it is serving it.

(defclass serve-view (view)
  ((server :initarg :server :reader serve-view-server))
  (:documentation "A headless coordinator: an `evo serve` session an HTTP
client drives."))

(defmethod view-say ((view serve-view) text &key (style :dim))
  ;; Through the command layer's host protocol, which publishes it as an
  ;; :output event — what a client's /events stream carries.  A lane's thread
  ;; has no EVO.SERVE:*REPLY* bound (bindings are per-thread), so a notice
  ;; never lands in a command's reply.
  (evo.command:host-notice (serve-view-server view) text
                           :severity (case style (:error :error) (:notice :warn) (t :info))))

(defmethod view-repaint ((view serve-view))
  ;; Nothing to redraw: a client reads the lanes with
  ;; POST /command {"text": "/lanes"}.
  (declare (ignore view))
  nil)

(defmethod view-publish ((view serve-view) event)
  (evo.serve:server-publish (serve-view-server view) event))

(defmethod view-run ((view serve-view) agent resumed-p)
  (evo.serve:serve (serve-view-server view) agent :resumed-p resumed-p))

;;; What the swarm calls.  One place that finds the view, so the rest of the
;;; swarm never has to know which frontend is up — or that there is one.

(defun swarm-say (text &key (style :dim))
  "Show TEXT in the coordinator's frontend, when one is up.  Any thread."
  (let ((view (and *swarm* (swarm-view *swarm*))))
    (when view (view-say view text :style style))))

(defun swarm-repaint ()
  "The lanes changed: let the coordinator's frontend redraw, if it can."
  (let ((view (and *swarm* (swarm-view *swarm*))))
    (when view (view-repaint view))))

(defun swarm-publish (event)
  "Publish EVENT (a plist with :type) on the coordinator's frontend, when it
has an event stream.  A no-op in the TUI, so lane-state code can announce
itself without asking which frontend is up."
  (let ((view (and *swarm* (swarm-view *swarm*))))
    (when view (view-publish view event))))

(defun swarm-run (agent resumed-p)
  "Run the coordinator's session under the swarm's view.  Returns the
frontend's exit code."
  (view-run (swarm-view *swarm*) agent resumed-p))
