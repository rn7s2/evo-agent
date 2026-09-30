;;;; status.lisp — the status-line segment registry, in the core.
;;;;
;;;; The status line is composed from named segments rather than formatted in
;;;; one place, because more than one party wants a piece of it: the core shows
;;;; the model, the thinking level, the context size, the goal and the running
;;;; jobs, and extensions want indicators of their own.  The obvious
;;;; alternative — everyone wraps the function that formats the line and
;;;; appends to the string the previous wrapper returned — looks fine until one
;;;; of those wrappers pads to the full terminal width to right-align itself.
;;;; Everything the outer wrappers append then lands past the right edge and is
;;;; truncated away by DRAW-REGION, silently.  A registry keeps the layout
;;;; decision in one renderer that can see every claim on the line at once.
;;;;
;;;; The registry lives in the core (it used to be evo.tui's) for the reason the
;;;; whole view model exists: the TUI's status line and a GUI's readout must be
;;;; the same list, rendered twice, not two lists that drift apart.  A segment
;;;; returns PLAIN text and declares a STYLE, so a frontend that is not a
;;;; terminal has something to render; styling is the frontend's business.
;;;;
;;;; ORDER counts inward from the segment's own edge: on the left, ascending
;;;; order runs left-to-right; on the right, ascending order runs right-to-left.
;;;; So a segment's order is "how close to my edge do I sit", the same sentence
;;;; on both sides, and the line degrades by dropping from the middle outward.

(in-package :evo.view)

(defstruct (status-segment (:constructor %make-status-segment))
  (name nil :read-only t)        ; keyword or string identifying the segment
  (function nil :read-only t)    ; (ctx) -> plain text, or NIL to show nothing
  (side :left :read-only t)      ; :left or :right
  (order 500 :read-only t)       ; distance from that side's edge
  (style nil :read-only t)       ; :muted :accent :error :success :warning, or NIL
  (data nil :read-only t))       ; plist, or (ctx) -> plist, for structured readers

(defvar *status-segments* nil
  "Registered status segments, unordered.  Rebound as a whole list on every
change so a rendering thread never observes a partially updated list.")

(defparameter *status-separator* " · "
  "Between adjacent segments on the same side.")

(defun define-status-segment (name function &key (side :left) (order 500) style data)
  "Register FUNCTION as a status-line segment under NAME.

FUNCTION is called with a context plist (see STATUS-SEGMENTS-LIVE) every time
the line is composed and returns plain display text — the frontend applies the
segment's STYLE — or NIL to show nothing right now.  It must be cheap and must
not block; cache in a poller task (EVO:SPAWN-TASK) if the value is expensive.
Errors are swallowed: a segment that signals is skipped, it does not take the
status line down with it.

SIDE is :LEFT or :RIGHT.  ORDER counts inward from that side's edge, so on the
right a lower ORDER sits closer to the right edge.  STYLE is a semantic role
the frontend paints with (:muted is what the core's own segments wear); DATA is
a plist — or a function of the context returning one — that structured readers
(the GUI's state.segments) get alongside the text, for values that are not
meant to be parsed back out of the rendered string.

Registering an existing NAME replaces it, which makes extension reloads
idempotent.  A segment an extension file registers while it loads belongs to
that file's generation, like its hooks: a reload withdraws it before the file
runs again, so a file that stops registering it — or is deleted — takes it off
the line."
  (check-type name (or symbol string))
  (unless (member side '(:left :right))
    (error "status segment ~s: SIDE must be :LEFT or :RIGHT, got ~s" name side))
  (setf *status-segments*
        (append (remove name *status-segments*
                        :key #'status-segment-name :test #'equal)
                (list (%make-status-segment :name name :function function
                                            :side side :order order
                                            :style style :data data))))
  (when *extension-owner*
    (register-extension-disposer (lambda () (remove-status-segment name))))
  name)

(defun remove-status-segment (name)
  "Unregister the status segment called NAME."
  (setf *status-segments*
        (remove name *status-segments* :key #'status-segment-name :test #'equal))
  name)

(defun status-segments (&optional side)
  "Registered segments, optionally only those on SIDE, in visual left-to-right
order.  Ties on ORDER keep registration order, so the core segments stay put
when an extension picks the same number."
  ;; LOOP COLLECT, not REMOVE-IF-NOT: REMOVE and friends are permitted to share
  ;; structure with their input, and STABLE-SORT and NREVERSE below are
  ;; destructive — a shared tail would let a repaint scramble the registry.
  (let* ((all (loop for segment in *status-segments*
                    when (or (null side) (eq (status-segment-side segment) side))
                      collect segment))
         (sorted (stable-sort all #'< :key #'status-segment-order)))
    ;; Ascending order runs outward from the edge, so the right side's visual
    ;; sequence is the reverse of its order.
    (if (eq side :right) (nreverse sorted) sorted)))

(defun status-segment-text (segment context)
  "SEGMENT's display text under CONTEXT, or NIL when it renders nothing this
time — including when it signals, which must never take the line down."
  (let ((text (ignore-errors (funcall (status-segment-function segment) context))))
    (and (stringp text) (plusp (length text)) text)))

(defun status-segment-value (segment context)
  "SEGMENT's structured DATA under CONTEXT (NIL when it declares none)."
  (let ((data (status-segment-data segment)))
    (if (functionp data)
        (ignore-errors (funcall data context))
        data)))

(defun status-segments-live (context &optional side)
  "Evaluate the registry against CONTEXT: a plist per segment that rendered
something, in the visual left-to-right order a frontend draws them.

  (:name … :order n :side :left|:right :style … :text \"…\" :data …)

This is the one evaluation path; the TUI paints the text, and the view model
publishes the same list as state.segments."
  (loop for segment in (status-segments side)
        for text = (status-segment-text segment context)
        when text
          collect (list :name (status-segment-name segment)
                        :order (status-segment-order segment)
                        :side (status-segment-side segment)
                        :style (status-segment-style segment)
                        :text text
                        :data (status-segment-value segment context))))

;;; The core's own claims on the line.  Registered like anybody else's, so the
;;; layout has no privileged path through it.  The context they read is built
;;; by whichever frontend composes the line (the TUI from its own slots, the
;;; view model from its state).

(defun fmt-ktokens (n)
  "N tokens, compact for a status cell: thousands while that fits — \"34k\",
\"999k\" — then whole millions, so a 1M context window reads \"1M\" and not
\"1000k\", the way the model menus and the design spell it.

Rounding is CL's, half to even, applied at each step: 999_499 is 999k,
999_500 is 1M, and 1_500_000 — 1.5M — rounds up to 2M."
  (let ((thousands (round (or n 0) 1000)))
    (if (>= thousands 1000)
        (format nil "~dM" (round thousands 1000))
        (format nil "~dk" thousands))))

(defun short-duration (seconds)
  "Compact elapsed clock for the status line: 45s, 3m, 1h2m."
  (let ((seconds (max 0 (round (or seconds 0)))))
    (cond ((< seconds 60) (format nil "~ds" seconds))
          ((< seconds 3600) (format nil "~dm" (floor seconds 60)))
          (t (multiple-value-bind (h rest) (floor seconds 3600)
               (format nil "~dh~dm" h (floor rest 60)))))))

(defun model-label-text (model-id model)
  "The status line's name for the session's model: its id, with the provider
in parentheses when the id alone does not identify the endpoint."
  (cond ((null model-id) "(no model)")
        ((and model (cdr (model-providers model-id)))
         (format nil "~a (~(~a~))" model-id (pget model :provider)))
        (t model-id)))

(defun cache-stats-percent (stats)
  "The share of input tokens the provider read from its prompt cache: cache
reads over all input tokens.  The normalized usage plist keeps cached tokens
OUT of :input, so :input + :cache-read + :cache-write is what the session
actually sent.  No tokens sent yet is 0%, not unknown — a fresh session's
cache rate is zero."
  (let ((in (or (getf stats :input) 0))
        (cr (or (getf stats :cache-read) 0))
        (cw (or (getf stats :cache-write) 0)))
    (round (* 100 cr) (max 1 (+ in cr cw)))))

(defun cache-stats-label-text (context)
  "\"N% cached\" once the session has sent anything, else NIL.  A session that
has made no request has no rate to speak of, and an always-on \"0% cached\"
costs a narrow status line the room its innermost segments (an IDE
selection, say) need: the line drops innermost cells first to fit."
  (let ((stats (getf context :cache-stats)))
    (when (plusp (+ (or (getf stats :input) 0)
                    (or (getf stats :cache-read) 0)
                    (or (getf stats :cache-write) 0)))
      (format nil "~d% cached" (cache-stats-percent stats)))))

(defun context-label-text (used window)
  (if window
      (format nil "ctx ~a/~a (~d%)"
              (fmt-ktokens used) (fmt-ktokens window)
              (min 100 (round (* 100 (or used 0)) (max 1 window))))
      (format nil "ctx ~a" (fmt-ktokens used))))

(defun goal-label-text (goal live-tokens)
  (let ((budget (pget goal :token-budget)))
    (format nil "goal ~a (~(~a~)) ~a~@[/~a~]"
            (pget goal :goal-id) (pget goal :status)
            (fmt-ktokens (+ (or (pget goal :tokens-used) 0) (or live-tokens 0)))
            (and budget (fmt-ktokens budget)))))

(defun jobs-label-text (jobs)
  "The ▷ background-jobs cell, or NIL when no job is running.  JOBS is
RUNNING-JOBS-SUMMARY's plist."
  (when jobs
    (let* ((n (getf jobs :count))
           (clock (short-duration (- (get-universal-time) (getf jobs :since))))
           (cmd (or (getf jobs :command) ""))
           (line (or (first (uiop:split-string cmd :separator '(#\Newline))) ""))
           (short (if (> (length line) 24)
                      (concatenate 'string (subseq line 0 23) "…")
                      line)))
      (if (> n 1)
          (format nil "▷ ~d jobs · ~a" n clock)
          (format nil "▷ ~a · ~a" short clock)))))

(defun segment-thinking-level (context)
  (let ((level (getf context :thinking)))
    (and level (string-downcase (string level)))))

(defun model-status-text (context) (getf context :model-label))
(defun thinking-status-text (context) (segment-thinking-level context))
(defun context-status-text (context)
  (context-label-text (getf context :context-tokens) (getf context :context-window)))
(defun cache-stats-status-text (context)
  (cache-stats-label-text context))
(defun goal-status-text (context)
  (let ((goal (getf context :goal)))
    (and goal (goal-label-text goal (getf context :goal-run-tokens)))))

(define-status-segment :model #'model-status-text :side :left :order 100 :style :muted
                       :data (lambda (c) (list :label (getf c :model-label))))
(define-status-segment :thinking #'thinking-status-text :side :left :order 200 :style :muted
                       :data (lambda (c) (and (getf c :thinking)
                                              (list :level (segment-thinking-level c)))))
(define-status-segment :context #'context-status-text :side :left :order 300 :style :muted
                       :data (lambda (c) (list :tokens (getf c :context-tokens)
                                               :window (getf c :context-window))))
;; The same name the shipped userspace extension registers
;; (~/.evo/extensions/340-cache-stats.lisp), on purpose: whichever loads last
;; owns the chip, and a session with that extension loaded replaces this
;; segment instead of showing two "N% cached" claims side by side.  Sessions
;; that load no userspace at all — a swarm lane, `serve --no-userspace` — get
;; this one.
(define-status-segment :cache-stats #'cache-stats-status-text
                       :side :left :order 350 :style :muted
                       :data (lambda (c) (getf c :cache-stats)))
(define-status-segment :goal #'goal-status-text :side :left :order 400 :style :muted
                       :data (lambda (c) (getf c :goal)))
(define-status-segment :jobs (lambda (c) (jobs-label-text (getf c :jobs)))
                       :side :right :order 200 :style :muted
                       :data (lambda (c) (getf c :jobs)))
