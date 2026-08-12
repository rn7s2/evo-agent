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

;;; Vision.  :vision declares image input and defaults to t, because every
;;; frontier model now sees.  Declare :vision nil for one that does not: evo
;;; then degrades a pasted image to a named text placeholder for that model,
;;; instead of the endpoint rejecting every request that replays it — one
;;; screenshot would otherwise poison the rest of the session.  This is worth
;;; measuring too, and for the same reason as the thinking knobs: an endpoint
;;; that advertises multimodal support may still 400 on an image, and one that
;;; advertises none may accept the request and quietly drop the image, which
;;; is worse — the model then answers about a picture it never saw.

;; DeepSeek v4 over the Anthropic API: 1M context, thinking steered by
;; effort.  Its ladder is low/medium/high/xhigh/ultra/max, a superset of
;; evo's, so every rung passes through unclamped and :effort t is accurate.
;; The :deepseek endpoint is registered under Providers below.
(evo:register-model "deepseek-v4-flash"
  :provider :deepseek :api :anthropic-messages
  :context-window 1000000 :max-output 192000
  :thinking t :effort t :vision nil)

(evo:register-model "deepseek-v4-pro"
  :provider :deepseek :api :anthropic-messages
  :context-window 1000000 :max-output 192000
  :thinking t :effort t :vision nil)

;; OpenAI Responses API models (272k input window; long-context surcharge
;; tiers above that are not modeled).  No :effort declaration, so the level
;; maps straight to reasoning.effort — declare a subset if your endpoint
;; rejects the top rungs.  These take image input, so :vision stays default.
(evo:register-model "gpt-5.6-sol"
  :provider :openai :api :openai-responses
  :context-window 272000 :max-output 128000 :thinking t)

(evo:register-model "gpt-5.6-terra"
  :provider :openai :api :openai-responses
  :context-window 272000 :max-output 128000 :thinking t)

(evo:register-model "gpt-5.6-luna"
  :provider :openai :api :openai-responses
  :context-window 272000 :max-output 128000 :thinking t)

;; Kimi K3 needs no register-model here: the vendored extension
;; extensions/020-kimi-provider.lisp registers the :kimi-chat-completions
;; API, the :moonshotai endpoint and the kimi-k3 model itself, and reads
;; MOONSHOT_API_KEY (or KIMI_API_KEY) for the key.  Extensions load after
;; this file, so naming it below as :model still resolves — settings are
;; read after the whole boot, not while this file runs.  See "Providers"
;; below for keeping the key in config instead of the environment.

;;; Providers.  The stock :anthropic and :openai endpoints are pre-seeded
;;; (base URL + ANTHROPIC_API_KEY / OPENAI_API_KEY env vars), so you only
;;; need register-provider for a proxy, a literal key, or a custom endpoint.
;;; Re-registering merges field-wise.

;; The endpoint the DeepSeek models above route to.  :api-key-env reads the
;; key at request time; :api-key takes a literal string instead.
(evo:register-provider :deepseek
  :base-url "https://api.deepseek.com/anthropic"
  :api-key-env "DEEPSEEK_API_KEY")

;; Keys in config rather than the environment: :api-key takes the literal
;; string.  This is the whole setup for Kimi — the extension registers the
;; :moonshotai endpoint itself and fills in only what this file left out, so
;; a key (or base URL) written here wins over MOONSHOT_API_KEY, over the
;; KIMI_API_KEY alias, and over the stock endpoint.
;;
;; Naming a provider that already exists is fine and is the point: this is
;; register-OR-MERGE, so the call below sets one field and leaves every other
;; one — the base URL included — exactly as it was.  (Whether :moonshotai
;; already exists when this file runs depends on the boot: on a fresh start
;; the extension has not loaded yet and this call creates the entry; on
;; /reload the endpoint is already seeded and this merges into it.  Same
;; result either way.)  Uncomment and edit:
;;
;; (evo:register-provider :moonshotai :api-key "sk-...")
;;
;; A literal key means the file holds a credential: keep it in ~/.evo (not a
;; project directory that gets committed), and chmod 600 it.  :api-key-env
;; names a different variable instead, if what you want is only to rename it:
;;
;; (evo:register-provider :moonshotai :api-key-env "WORK_MOONSHOT_KEY")
;;
;; The same call moves Kimi to the China platform.  Accounts and keys are
;; platform-scoped — a platform.kimi.com key is a 401 on api.moonshot.ai and
;; vice versa — so change both together:
;;
;; (evo:register-provider :moonshotai
;;   :base-url "https://api.moonshot.cn" :api-key "sk-...")
;;
;; /kimi:status prints the resulting wiring (endpoint, whether a key was
;; found, thinking ladder) without printing the key itself.

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
;; (evo:set-setting :language "English")      ; response-language hint

;;; Theme.  :dark (the default) or :light — the /theme command toggles it
;;; live.  It must MATCH your terminal's background: evo paints colours legible
;;; for that background (and the LaTeX-math renderer, which reads :theme, sets
;;; its glyph colour from it), so :light on a dark terminal is unreadable.
;; (evo:set-setting :theme :light)
;; A conditional default — e.g. light only when run via the evo-vscode webview
;; (its terminal palette already follows VS Code; :theme only drives evo's own
;; colours and the math glyph colour, so match it to your VS Code theme):
;; (when (uiop:getenv "EVO_WEBVIEW")
;;   (evo:set-setting :theme :light))

;;; Math rendering (extensions/300-latex-math.lisp).  LaTeX in agent output —
;;; $…$, $$…$$, \(…\), \[…\] — renders as a real typeset image inline, with
;;; inline formulas baseline-aligned with the prose around them.  Needs a
;;; LaTeX toolchain (latex + dvipng) and a kitty-graphics terminal.  In VS Code,
;;; run evo through the evo-vscode extension (crisp, device resolution);
;;; kitty/Ghostty/WezTerm work out of the box.  Absent the toolchain it stays
;;; off and shows source.  Prerequisites and calibration: docs/math.md.  The
;;; three geometry settings below are AUTO-DETECTED in the evo-vscode webview;
;;; set them (CSS px, via tests/math-calibrate.py) only for other terminals:
;;
;; (evo:set-setting :math-cell-px 18)            ; terminal row height, CSS px
;; (evo:set-setting :math-cell-w-px 9)           ; terminal col width, CSS px
;; (evo:set-setting :math-dpi 110)               ; 110 ≈ prose size; 220 = 2x
;;
;;; The rest has working defaults:
;;
;; (evo:set-setting :math t)                     ; master on/off (default t)
;; (evo:set-setting :math-baseline-frac 0.8)     ; baseline position in a row
;; (evo:set-setting :math-snap-px 2)             ; nudge <= Npx to save a row
;; (evo:set-setting :math-x-advance :terminal)   ; :terminal (exact) | :manual
;; (evo:set-setting :math-pixel-align t)         ; sub-cell baseline offset
;; (evo:set-setting :math-inline-mode :aligned)  ; :aligned | :break | :raw
;; (evo:set-setting :math-foreground nil)        ; xcolor name; nil = :theme
;; (evo:set-setting :math-border "1pt")          ; whitespace around a formula
;; (evo:set-setting :math-max-bytes 786432)      ; largest PNG to emit

;;; Anything an extension can do, config can do too: register-tool,
;;; register-command, event hooks (evo:on), evo:load-extension.
