;;;; init.lisp — sample evo configuration.
;;;;
;;;; Copy to ~/.evo/init.lisp (global) or <project>/.evo/init.lisp and edit.
;;;; Config is code: evo evaluates the global file, then the project file,
;;;; on every boot — an override is just a later call.  evo ships no
;;;; built-in model table, so at minimum register one model and set :model.

;;; Models.  One wire API ships with evo — the Anthropic Messages API — and
;;; :api defaults to it, so a registration is the model id, an endpoint, and
;;; four knobs: context window, max output, effort ladder, vision.  Any
;;; endpoint that speaks the Messages API works.  The /model picker lists
;;; models in registration order.

;;; Thinking.  Every model on the picker reasons — a model that cannot think
;;; is not worth driving, so there is no capability switch.  The level comes
;;; from /thinking — low, medium, high, xhigh, max, with no off rung.  How
;;; that level reaches the wire is per-model:
;;;
;;;   :effort        the levels the endpoint accepts for Anthropic's
;;;                  output_config.effort — t for all five, or a subset list
;;;                  for a model that stops short (a level above the subset
;;;                  is clamped down, not rejected).  nil (the default) means
;;;                  the model has no effort parameter, and the /thinking
;;;                  dial is wired to nothing.
;;;   :thinking-mode :effort-only (the default) sends no `thinking` object
;;;                  at all — the smallest request, and the shape every
;;;                  Messages-compatible endpoint accepts (on Kimi Code's
;;;                  K3, anything else routes the request to an older
;;;                  model).  :adaptive also sends thinking {type adaptive,
;;;                  display summarized} — what Anthropic's own models want,
;;;                  and what makes their reasoning summaries visible.
;;;
;;; Which knob an endpoint honours is worth measuring rather than assuming:
;;; an endpoint may accept a parameter and quietly ignore it, and then the
;;; /thinking dial moves nothing.

;;; Vision.  :vision declares image input and defaults to t, because every
;;; frontier model now sees.  Declare :vision nil for one that does not: evo
;;; then degrades a pasted image to a named text placeholder for that model,
;;; instead of the endpoint rejecting every request that replays it — one
;;; screenshot would otherwise poison the rest of the session.  This is worth
;;; measuring too, and for the same reason as the thinking knobs: an endpoint
;;; that advertises multimodal support may still 400 on an image, and one that
;;; advertises none may accept the request and quietly drop the image, which
;;; is worse — the model then answers about a picture it never saw.

;; The supported Anthropic models, on the stock :anthropic endpoint
;; (pre-seeded: api.anthropic.com + ANTHROPIC_API_KEY, see Providers below).
;; All: 1M context, 128K output, the full effort ladder, adaptive
;; thinking, vision.
(evo:register-model "claude-sonnet-5"
  :provider :anthropic
  :context-window 1000000 :max-output 128000
  :effort t :thinking-mode :adaptive)

(evo:register-model "claude-opus-5"
  :provider :anthropic
  :context-window 1000000 :max-output 128000
  :effort t :thinking-mode :adaptive)

(evo:register-model "claude-fable-5"
  :provider :anthropic
  :context-window 1000000 :max-output 128000
  :effort t :thinking-mode :adaptive)

(evo:register-model "claude-fable-5-1"
  :provider :anthropic
  :context-window 1000000 :max-output 128000
  :effort t :thinking-mode :adaptive)

;; A third-party endpoint speaking the same Messages API: DeepSeek v4.
;; Thinking is steered by effort alone, so the default :effort-only mode is
;; right.  The official rungs are low/high/max (default high; the API
;; reference says medium and xhigh are accepted only as compatibility
;; aliases that fold UP to high) — declaring the real three lets evo clamp
;; an off-ladder level down instead, same as Kimi's K3.  Text-only — the
;; endpoint's own compatibility table lists image content blocks as Not
;; Supported — hence :vision nil.  The :deepseek endpoint is registered
;; under Providers below.
(evo:register-model "deepseek-v4-flash"
  :provider :deepseek
  :context-window 1000000 :max-output 192000
  :effort '(:low :high :max) :vision nil)

(evo:register-model "deepseek-v4-pro"
  :provider :deepseek
  :context-window 1000000 :max-output 192000
  :effort '(:low :high :max) :vision nil)

;; Kimi K3 needs no register-model here: the vendored extension
;; extensions/020-kimi-provider.lisp registers the :kimi endpoint
;; (https://api.kimi.com/coding, driven by the kernel's own
;; :anthropic-messages adapter — Kimi Code speaks the Messages API) and both
;; K3 ids, k3 and k3-256k, reading KIMI_API_KEY for the key.  Extensions load
;; after this file, so naming one below as :model still resolves — settings
;; are read after the whole boot, not while this file runs.  See "Providers"
;; below for keeping the key in config instead of the environment.

;;; Providers.  The stock :anthropic endpoint is pre-seeded (base URL +
;;; ANTHROPIC_API_KEY env var), so you only need register-provider for a
;;; proxy, a literal key, or a custom endpoint.  Re-registering merges
;;; field-wise — e.g. route the Anthropic models through a local proxy with
;;; (evo:register-provider :anthropic :base-url "http://127.0.0.1:8787").

;; The endpoint the DeepSeek models above route to.  :api-key-env reads the
;; key at request time; :api-key takes a literal string instead.
(evo:register-provider :deepseek
  :base-url "https://api.deepseek.com/anthropic"
  :api-key-env "DEEPSEEK_API_KEY")

;; Keys in config rather than the environment: :api-key takes the literal
;; string.  This is the whole setup for Kimi — the extension registers the
;; :kimi endpoint itself and fills in only what this file left out, so a key
;; (or base URL) written here wins over KIMI_API_KEY and over the stock
;; endpoint.
;;
;; Naming a provider that already exists is fine and is the point: this is
;; register-OR-MERGE, so the call below sets one field and leaves every other
;; one — the base URL included — exactly as it was.  (Whether :kimi already
;; exists when this file runs depends on the boot: on a fresh start the
;; extension has not loaded yet and this call creates the entry; on /reload
;; the endpoint is already there and this merges into it.  Same result either
;; way.)  Uncomment and edit:
;;
;; (evo:register-provider :kimi :api-key "sk-...")
;;
;; A literal key means the file holds a credential: keep it in ~/.evo (not a
;; project directory that gets committed), and chmod 600 it.  :api-key-env
;; names a different variable instead, if what you want is only to rename it:
;;
;; (evo:register-provider :kimi :api-key-env "WORK_KIMI_KEY")
;;
;; Keys are platform-scoped: a Kimi Code key (Kimi Code Console) works on
;; api.kimi.com/coding, and a platform.moonshot.ai / .cn key does not — those
;; belong to the open platform, a different endpoint and a different account.

;;; Settings.  :model is required (there is no default); the rest have
;;; kernel defaults.

(evo:set-setting :model "claude-opus-5")
;; When that id is registered under several providers, the first
;; registration wins by default; :model-provider names a different one.
;; /model overrides both for the session.
;; (evo:set-setting :model-provider :ark)
;; (evo:set-setting :thinking :medium)        ; low|medium|high|xhigh|max
;; (evo:set-setting :goal-token-budget 500000)
;; (evo:set-setting :compact-reserve 16000)
;; (evo:set-setting :compact-keep-recent 20000)
;; Language of the system prompt and of replies.  A registered pack code
;; ("en", or "zh-CN" from extensions/100-lang-zh-cn.lisp) switches the prompt
;; itself into that language; any other string ("Korean") is just a
;; response-language hint.  /lang switches it live.
;; (evo:set-setting :language "zh-CN")

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
