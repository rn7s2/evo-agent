;;;; 340-cache-stats.lisp — session prompt-cache hit rate in the status line.
;;;;
;;;; Adds a "N% cached" segment just right of the core :context segment
;;;; (core order: :model 100 :thinking 200 :context 300 :goal 400, so 350
;;;; sits between context and goal — directly beside ctx when no goal runs).
;;;;
;;;; Data source is the NORMALIZED usage plist every provider adapter already
;;;; produces — (:input n :output n :cache-read n :cache-write n), with :input
;;;; excluding cached tokens (src/provider/api.lisp documents the contract).
;;;; Nothing here touches an Anthropic payload: a provider with no cache
;;;; concept reports zeros, and the segment stays hidden until some provider
;;;; actually reports cache activity (read or write).
;;;;
;;;; Timing: hooking :turn-end gives the same freshness as the ctx readout.
;;;; Usage only exists once a request completes — no provider reports it
;;;; mid-stream — and :turn-end fires per request (loop.lisp emits
;;;; :message-end, then runs the hook), so the same repaint that re-anchors
;;;; ctx already sees the new totals.  No poller, no TUI internals.
;;;;
;;;; Totals persist as a :custom journal entry (invisible to the LLM,
;;;; survives restart and compaction); :session-start folds them back in.
;;;; In-memory totals survive /reload on purpose: the journal does not
;;;; rewind, so neither should the rate.

(in-package :evo.user)

(defparameter +cache-stats-key+ "cache-stats"
  "Journal :custom key the session totals persist under.")

(defvar *cache-stats-lock* (bt:make-lock "cache-stats"))

(defvar *cache-stats* (list :input 0 :cache-read 0 :cache-write 0)
  "Session totals.  Hook writes on the agent thread, the status segment
reads on the TUI thread — hence the lock.")

(defun cache-stats-record (usage)
  "Fold one request's normalized usage plist into the session totals.
Returns true when the totals moved.  USAGE may be NIL (error messages) or
carry missing keys (adapters that omit zero fields); both read as nothing."
  (when usage
    (let ((in (or (evo.util:pget usage :input) 0))
          (cr (or (evo.util:pget usage :cache-read) 0))
          (cw (or (evo.util:pget usage :cache-write) 0)))
      (when (plusp (+ in cr cw))
        (bt:with-lock-held (*cache-stats-lock*)
          (incf (getf *cache-stats* :input) in)
          (incf (getf *cache-stats* :cache-read) cr)
          (incf (getf *cache-stats* :cache-write) cw))
        t))))

(defun cache-stats-turn-end (payload)
  (when (cache-stats-record
         (evo.provider:message-usage (evo.util:pget payload :message)))
    (evo:set-custom-state +cache-stats-key+
                          (bt:with-lock-held (*cache-stats-lock*)
                            (copy-list *cache-stats*))
                          (evo.util:pget payload :agent))))

(defun cache-stats-session-start (payload)
  "Restore totals persisted by earlier boots of this session.  NIL (a fresh
session) resets to zero."
  (let ((saved (evo:custom-state +cache-stats-key+ (evo.util:pget payload :agent))))
    (bt:with-lock-held (*cache-stats-lock*)
      (setf *cache-stats*
            (list :input (or (evo.util:pget saved :input) 0)
                  :cache-read (or (evo.util:pget saved :cache-read) 0)
                  :cache-write (or (evo.util:pget saved :cache-write) 0))))))

(defun cache-stats-label (tui)
  "Status segment: \"N% cached\" — cache-read over total input tokens
(:input + :cache-read + :cache-write; the normalized plist keeps cached
tokens OUT of :input, so the sum is the real input).  NIL while no provider
has reported any cache activity, rather than a noise \"0%\"."
  (declare (ignore tui))
  (bt:with-lock-held (*cache-stats-lock*)
    (let ((in (getf *cache-stats* :input))
          (cr (getf *cache-stats* :cache-read))
          (cw (getf *cache-stats* :cache-write)))
      (when (plusp (+ cr cw))
        (evo.tui::dim (format nil "~d% cached" (round (* 100 cr) (+ in cr cw))))))))

(evo:on :turn-end #'cache-stats-turn-end :name :cache-stats)
(evo:on :session-start #'cache-stats-session-start :name :cache-stats)
(evo.tui:add-status-segment :cache-stats #'cache-stats-label :side :left :order 350)
