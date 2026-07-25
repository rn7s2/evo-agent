;;;; model-table.lisp — hand-written model table.
;;;;
;;;; Costs are per million tokens, as rationals so journal accounting stays
;;;; exact.  :provider names a provider config in settings
;;;; (:providers (:anthropic (:base-url ... :api-key ...))).

(in-package :evo.provider)

(defparameter *models*
  '((:id "claude-fable-5" :provider :anthropic :api :anthropic-messages
     :context-window 200000 :max-output 64000 :thinking t
     :cost (:input 5 :output 25 :cache-read 1/2 :cache-write 25/4))
    (:id "claude-opus-5" :provider :anthropic :api :anthropic-messages
     :context-window 200000 :max-output 64000 :thinking t
     :cost (:input 5 :output 25 :cache-read 1/2 :cache-write 25/4))
    (:id "claude-sonnet-5" :provider :anthropic :api :anthropic-messages
     :context-window 200000 :max-output 64000 :thinking t
     :cost (:input 3 :output 15 :cache-read 3/10 :cache-write 15/4))
    (:id "claude-haiku-4-5-20251001" :provider :anthropic :api :anthropic-messages
     :context-window 200000 :max-output 64000 :thinking t
     :cost (:input 1 :output 5 :cache-read 1/10 :cache-write 5/4))
    ;; Local test proxy models (Anthropic Messages wire format).
    (:id "ark-deepseek-v4-pro" :provider :anthropic :api :anthropic-messages
     :context-window 1000000 :max-output 64000 :thinking t
     :cost (:input 0 :output 0 :cache-read 0 :cache-write 0))
    (:id "ark-glm-5.2" :provider :anthropic :api :anthropic-messages
     :context-window 1000000 :max-output 64000 :thinking t
     :cost (:input 0 :output 0 :cache-read 0 :cache-write 0))
    (:id "deepseek-v4-pro" :provider :anthropic :api :anthropic-messages
     :context-window 1000000 :max-output 64000 :thinking t
     :cost (:input 0 :output 0 :cache-read 0 :cache-write 0))
    (:id "deepseek-v4-flash" :provider :anthropic :api :anthropic-messages
     :context-window 1000000 :max-output 64000 :thinking t
     :cost (:input 0 :output 0 :cache-read 0 :cache-write 0))))

(defun find-model (id)
  (or (find id *models* :key (lambda (m) (pget m :id)) :test #'string=)
      ;; Unknown models still work against the default provider with
      ;; conservative limits — the table is data, not a gate.
      (list :id id :provider :anthropic :api :anthropic-messages
            :context-window 200000 :max-output 32000 :thinking t
            :cost '(:input 0 :output 0 :cache-read 0 :cache-write 0))))

(defun model-context-window (model) (pget model :context-window))
(defun model-max-output (model) (pget model :max-output))

;;; Thinking levels → Anthropic budget_tokens.
(defun thinking-budget (level)
  (case level
    ((nil :off) nil)
    (:low 2048)
    (:medium 8192)
    (:high 16384)
    (:xhigh 32768)
    (t nil)))
