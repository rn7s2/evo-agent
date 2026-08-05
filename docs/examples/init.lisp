;;;; init.lisp — sample evo configuration.
;;;;
;;;; Copy to ~/.evo/init.lisp (global) or <project>/.evo/init.lisp and edit.
;;;; Config is code: evo evaluates the global file, then the project file,
;;;; on every boot — an override is just a later call.  evo ships no
;;;; built-in model table, so at minimum register one model and set :model.

;;; Models.  :api names a kernel provider API (:anthropic-messages or
;;; :openai-responses); :provider names an endpoint config (see below).
;;; The /model picker lists models in registration order.

(evo:register-model "claude-fable-5"
  :provider :anthropic :api :anthropic-messages
  :context-window 200000 :max-output 64000 :thinking t)

(evo:register-model "claude-opus-5"
  :provider :anthropic :api :anthropic-messages
  :context-window 200000 :max-output 64000 :thinking t)

(evo:register-model "claude-sonnet-5"
  :provider :anthropic :api :anthropic-messages
  :context-window 200000 :max-output 64000 :thinking t)

(evo:register-model "claude-haiku-4-5-20251001"
  :provider :anthropic :api :anthropic-messages
  :context-window 200000 :max-output 64000 :thinking t)

;; OpenAI Responses API models (272k input window; long-context surcharge
;; tiers above that are not modeled).
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

;; (evo:register-provider :anthropic
;;   :base-url "http://127.0.0.1:8787"
;;   :api-key "sk-...")

;; A separate provider key for a local proxy speaking a stock API:
;; (evo:register-provider :ark
;;   :base-url "http://127.0.0.1:8787" :api-key "sk-...")
;; (evo:register-model "ark-deepseek-v4-pro"
;;   :provider :ark :api :anthropic-messages
;;   :context-window 1000000 :max-output 64000 :thinking t)

;;; The same model served by several providers.  A model's identity is its
;;; (id, provider) pair, so registering one id twice under different
;;; providers gives two entries — both listed in /model, each routing to its
;;; own endpoint.  (Re-registering the SAME pair replaces it in place.)

;; (evo:register-model "claude-sonnet-5"
;;   :provider :ark :api :anthropic-messages
;;   :context-window 200000 :max-output 64000 :thinking t)

;;; Settings.  :model is required (there is no default); the rest have
;;; kernel defaults.

(evo:set-setting :model "claude-sonnet-5")
;; When that id is registered under several providers, the first
;; registration wins by default; :model-provider names a different one.
;; /model overrides both for the session.
;; (evo:set-setting :model-provider :ark)
;; (evo:set-setting :thinking :medium)        ; off|low|medium|high|xhigh
;; (evo:set-setting :goal-token-budget 500000)
;; (evo:set-setting :compact-reserve 16000)
;; (evo:set-setting :compact-keep-recent 20000)

;;; Anything an extension can do, config can do too: register-tool,
;;; register-command, event hooks (evo:on), evo:load-extension.
