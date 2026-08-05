;;;; init.lisp — sample evo configuration.
;;;;
;;;; Copy to ~/.evo/init.lisp (global) or <project>/.evo/init.lisp and edit.
;;;; Config is code: evo evaluates the global file, then the project file,
;;;; on every boot — an override is just a later call.  evo ships no
;;;; built-in model table, so at minimum register one model and set :model.

;;; Models.  :api names a kernel provider API (:anthropic-messages or
;;; :openai-responses); :provider names an endpoint config (see below).
;;; The /model picker lists models in registration order.

;;; Thinking.  :thinking is a capability (t = this model reasons), not a
;;; preference; the level comes from /thinking — low, medium, high, xhigh,
;;; max, with no off rung.  How that level reaches the wire is per-model:
;;;
;;;   :effort        the levels the endpoint accepts for Anthropic's
;;;                  output_config.effort — t for all five, or a subset list
;;;                  for a model that stops short (a level above the subset
;;;                  is clamped down, not rejected).  nil (the default) means
;;;                  the model has no effort parameter at all.
;;;   :thinking-mode :extended (the default) sends thinking.budget_tokens;
;;;                  :adaptive lets the model decide when to think and sends
;;;                  a mode instead of a budget — what Anthropic models from
;;;                  4.6 on want, since budget_tokens is deprecated there.
;;;
;;; Which knob an endpoint honours is worth measuring rather than assuming:
;;; DeepSeek's, for one, accepts budget_tokens and quietly ignores it, so a
;;; model registered there without :effort has a /thinking dial connected to
;;; nothing.

;; DeepSeek v4 over the Anthropic API: 1M context, thinking steered by
;; effort.  Its ladder is low/medium/high/xhigh/ultra/max, a superset of
;; evo's, so every rung passes through unclamped and :effort t is accurate.
;; The :deepseek endpoint is registered under Providers below.
(evo:register-model "deepseek-v4-flash"
  :provider :deepseek :api :anthropic-messages
  :context-window 1000000 :max-output 192000
  :thinking t :effort t)

(evo:register-model "deepseek-v4-pro"
  :provider :deepseek :api :anthropic-messages
  :context-window 1000000 :max-output 192000
  :thinking t :effort t)

;; OpenAI Responses API models (272k input window; long-context surcharge
;; tiers above that are not modeled).  No :effort declaration, so the level
;; maps straight to reasoning.effort — declare a subset if your endpoint
;; rejects the top rungs.
(evo:register-model "gpt-5.6-sol"
  :provider :openai :api :openai-responses
  :context-window 272000 :max-output 128000 :thinking t)

(evo:register-model "gpt-5.6-terra"
  :provider :openai :api :openai-responses
  :context-window 272000 :max-output 128000 :thinking t)

(evo:register-model "gpt-5.6-luna"
  :provider :openai :api :openai-responses
  :context-window 272000 :max-output 128000 :thinking t)

;;; Providers.  The stock :anthropic and :openai endpoints are pre-seeded
;;; (base URL + ANTHROPIC_API_KEY / OPENAI_API_KEY env vars), so you only
;;; need register-provider for a proxy, a literal key, or a custom endpoint.
;;; Re-registering merges field-wise.

;; The endpoint the DeepSeek models above route to.  :api-key-env reads the
;; key at request time; :api-key takes a literal string instead.
(evo:register-provider :deepseek
  :base-url "https://api.deepseek.com/anthropic"
  :api-key-env "DEEPSEEK_API_KEY")

;;; Settings.  :model is required (there is no default); the rest have
;;; kernel defaults.

(evo:set-setting :model "deepseek-v4-pro")
;; When that id is registered under several providers, the first
;; registration wins by default; :model-provider names a different one.
;; /model overrides both for the session.
;; (evo:set-setting :model-provider :ark)
;; (evo:set-setting :thinking :medium)        ; low|medium|high|xhigh|max
;; (evo:set-setting :goal-token-budget 500000)
;; (evo:set-setting :compact-reserve 16000)
;; (evo:set-setting :compact-keep-recent 20000)

;;; Anything an extension can do, config can do too: register-tool,
;;; register-command, event hooks (evo:on), evo:load-extension.
