;;;; 020-kimi-provider.lisp — Kimi Code (Moonshot AI) K3, over the kernel's
;;;; Anthropic-messages adapter.  Vendored user extension, installed by
;;;; `make install` (via install-home) into $(EVO_HOME)/extensions/ and loaded
;;;; automatically at startup.
;;;;
;;;; What this registers
;;;;   provider :kimi     — https://api.kimi.com/coding  (+ /v1/messages)
;;;;   model    k3        — 1,048,576 ctx, image + video in
;;;;   model    k3-256k   — 262,144 ctx, image in, ~half the quota of k3
;;;;
;;;; There is NO wire protocol here on purpose.  Kimi Code speaks both
;;;; protocols — OpenAI-compatible at https://api.kimi.com/coding/v1 and
;;;; Anthropic-compatible at https://api.kimi.com/coding/ — and the Anthropic
;;;; one is a faithful Messages API (x-api-key auth, SSE deltas, signed
;;;; thinking blocks, cache_read_input_tokens, tool_use), so evo's own
;;;; :anthropic-messages adapter drives it.  This file is therefore config,
;;;; not a second protocol to maintain: an endpoint, two models, done.

;;;; Only K3 is supported, by design.

;;;; The model metadata, from the Kimi Code docs and confirmed against
;;;; GET /coding/v1/models:
;;;;   context   k3 up to 1M (Allegretto plan and above; a Moderato plan caps
;;;;             k3 at 256k and answers 401 above it), k3-256k fixed 256k.
;;;;   thinking  K3 always reasons.  The dial is Anthropic's
;;;;             output_config.effort, and ONLY that — hence
;;;;             :thinking-mode :effort-only, which sends no `thinking`
;;;;             object at all: a budget would be ignored, and a `thinking`
;;;;             of type disabled routes the request to an older model.
;;;;   effort    K3's official rungs are low/high/max (default high), and
;;;;             exactly those are declared: evo clamps an off-ladder level
;;;;             down itself — medium -> low, xhigh -> high — so a request
;;;;             never spends more than asked.  Left undeclared, the
;;;;             endpoint would round up instead (it maps medium -> high
;;;;             and xhigh -> max; unknown strings are a 400).
;;;;   vision    Both take images.  Video (k3 only) is not a thing evo sends.
;;;;
;;;; Keys are platform-scoped: a Kimi Code key (Kimi Code Console) is what
;;;; api.kimi.com/coding wants — a platform.moonshot.ai / .cn key is a 401
;;;; there, and vice versa.
;;;;
;;;; Environment variables:
;;;;   KIMI_API_KEY   — API key (sent as x-api-key by the Anthropic adapter)
;;;;   KIMI_BASE_URL  — endpoint override; a trailing "/v1" is stripped, so
;;;;                    both spellings of the base URL work.
;;;;
;;;; Or write it in config instead, which wins over both — see
;;;; docs/examples/init.lisp:
;;;;
;;;;   (evo:register-provider :kimi :api-key "sk-...")
;;;;
;;;; init.lisp is evaluated before extensions load, so this extension fills in
;;;; only the fields config left out (KIMI--REGISTER-ENDPOINT).
;;;;
;;;; Reference: https://www.kimi.com/code/docs/en/kimi-code/models.html

(in-package :evo.user)

(defparameter *kimi-provider-key* :kimi)
(defparameter *kimi-default-base-url* "https://api.kimi.com/coding")
(defparameter *kimi-api-key-env* "KIMI_API_KEY")

(defparameter *kimi-max-output* 131072
  "max_tokens for both ids.  The endpoint enforces no ceiling of its own
\(1048577 is accepted), so this is the documented K3 output cap.")

(defun kimi--trim (value)
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun kimi--normalize-base-url (url)
  "Accept either the Anthropic base URL (https://api.kimi.com/coding) or the
OpenAI-style one (…/coding/v1): ENDPOINT-PATH supplies the /v1 prefix, so a
configured one would double it."
  (when url
    (let ((url (string-right-trim "/" url)))
      (if (and (>= (length url) 3) (string= "/v1" url :start2 (- (length url) 3)))
          (string-right-trim "/" (subseq url 0 (- (length url) 3)))
          url))))

(defun kimi--base-url ()
  (or (kimi--normalize-base-url (kimi--trim (uiop:getenv "KIMI_BASE_URL")))
      *kimi-default-base-url*))

(defun kimi--register-endpoint ()
  "Register the endpoint, filling in only what config left out.
init.lisp is evaluated before extensions load and REGISTER-PROVIDER merges
field-wise with the later call winning — so registering unconditionally would
silently undo a base URL or key the user wrote in their own config.

Precedence, strongest first: init.lisp (or post-init.lisp) · the environment ·
the stock endpoint."
  (let ((entry (cdr (assoc *kimi-provider-key* evo.provider::*providers*))))
    (apply #'evo:register-provider *kimi-provider-key*
           (append
            (unless (evo.util:pget entry :base-url)
              (list :base-url (kimi--base-url)))
            (unless (evo.util:pget entry :api-key-env)
              (list :api-key-env *kimi-api-key-env*))))))

(kimi--register-endpoint)

;;; Static, and no network: this is documented model metadata, so both models
;;; register whether or not a key is present.  A missing key is a clear error
;;; at request time, not a model that silently vanishes from the picker.

(evo:register-model "k3"
  :provider *kimi-provider-key* :api :anthropic-messages
  :context-window 1048576 :max-output *kimi-max-output*
  :thinking-mode :effort-only :effort '(:low :high :max) :vision t)

(evo:register-model "k3-256k"
  :provider *kimi-provider-key* :api :anthropic-messages
  :context-window 262144 :max-output *kimi-max-output*
  :thinking-mode :effort-only :effort '(:low :high :max) :vision t)
