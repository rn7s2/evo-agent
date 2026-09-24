;;;; package.lisp — EVO.TUI, the interactive frontend.
;;;;
;;;; Defined here rather than in src/packages.lisp: the TUI is built on the
;;;; core, so its package exists only once the core is loaded, and nothing in
;;;; the core can name it.

(defpackage :evo.tui
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:start-tui
           ;; Status line composition — the supported way for an extension to
           ;; claim a piece of the bottom line (see docs/extension-api.md).
           #:add-status-segment #:remove-status-segment #:status-segments
           #:request-repaint #:request-run #:tui-live-p
           ;; Math rendering seam — an extension installs a rasterizer here
           ;; (see extensions/300-latex-math.lisp and docs/extension-api.md).
           #:register-math-renderer #:*math-renderer* #:*math-enabled*
           #:*math-live-preview* #:md-split-math #:render-math-span
           ;; Prose-styler seam — an extension restyles plain prose words here
           ;; (see extensions/350-bionic-reader.lisp and docs/extension-api.md).
           #:register-prose-styler #:*prose-styler* #:*prose-styling-suppressed*
           #:style-prose))
