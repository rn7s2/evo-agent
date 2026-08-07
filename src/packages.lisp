;;;; packages.lisp — package definitions for evo.
;;;;
;;;; Kernel packages (EVO.UTIL, EVO.JOURNAL, EVO.PROVIDER, EVO.KERNEL, EVO.CLI)
;;;; are locked at boot.  Userspace (EVO.USER) is unlocked; all
;;;; agent-written code lives there.  EVO is the public extension API surface —
;;;; both core and user extensions build on it, nothing bypasses it.

;; Implementation portability layer: the only package that may touch
;; sb-* / ext: / si: symbols.  See src/port/port.lisp.
(defpackage :evo.port
  (:use :cl)
  (:export #:exit-lisp #:argv #:runtime-pathname #:environ #:setenv #:getpid
           #:call-with-timeout #:timeout-error
           #:launch-child #:process-alive-p #:process-kill #:process-kill-tree
           #:process-wait #:process-pid
           #:program-in-path #:windows-p #:path-separator
           #:shell-invocation #:shell-name
           #:lock-package #:unlock-package #:add-package-local-nickname
           #:make-stdout-stream #:make-stdin-stream
           #:read-available-input
           #:std-descriptor #:std-external-format
           #:install-signal-handler #:+sigwinch+
           #:terminal-raw-mode #:restore-terminal-mode #:terminal-sane
           #:terminal-size
           #:tty-p #:disable-debugger #:ensure-in-image-compiler))

(defpackage :evo.util
  (:use :cl)
  (:export #:getenv #:env-proxy #:with-proxy #:*request-proxy*
           #:ensure-winhttp-proxy #:iso8601-now #:format-local-timestamp #:local-timezone-name
           #:gen-id #:reseed-ids #:pget #:pput #:plist-merge
           #:evo-home #:project-evo-dir #:encode-cwd #:ensure-directory
           #:write-sexpr-line #:read-sexpr #:read-sexpr-stream #:validate-journal-value
           #:setting #:set-setting #:reset-settings #:*settings*
           #:cat #:normalize-newlines #:crlf-newlines #:crlf-p
           #:string-join #:string-prefix-p #:truncate-string
           #:count-substring #:string-replace
           #:read-file-string #:write-file-string
           #:read-file-octets #:write-file-octets
           #:octets->base64 #:base64->octets))

;; Images in: clipboard grabs, file attachments, media-type sniffing.  Used
;; by the frontends (TUI, CLI) to build the :image content blocks the
;; provider adapters already know how to encode.
(defpackage :evo.media
  (:use :cl :evo.util)
  (:export #:*max-image-bytes* #:*max-image-dimension* #:*clipboard-readers*
           #:*downscalers*
           #:sniff-media-type #:file-media-type #:image-file-p #:media-type-extension
           #:make-image-block #:image-block-p #:image-summary #:format-bytes
           #:attach-image-file #:clipboard-image
           #:pasted-image-paths #:split-shell-tokens #:split-windows-tokens))

(defpackage :evo.journal
  (:use :cl :evo.util)
  (:export #:journal #:make-session-journal #:open-journal #:journal-path
           #:journal-entries #:journal-leaf-id #:journal-header #:journal-started-p
           #:append-entry #:find-entry #:entry-path #:fold-state #:fork-session
           #:state-messages #:state-model #:state-model-provider #:state-thinking
           #:state-tools
           #:state-goal #:state-loads #:state-name #:state-custom #:custom-state
           #:list-sessions #:latest-session #:sessions-directory))

(defpackage :evo.provider
  (:use :cl :evo.util)
  (:export #:find-model #:all-models #:model-providers
           #:model-context-window #:model-max-output
           #:model-effort #:model-thinking-mode #:model-vision-p #:+effort-levels+
           #:normalize-thinking-level
           #:register-model* #:register-provider* #:provider-config
           #:reset-user-registries
           #:call-provider #:provider-error
           #:parse-sse-stream #:parse-responses-sse-stream
           ;; provider-API protocol — an extension point: subclass
           ;; PROVIDER-API, implement the generics, REGISTER-API it.
           #:provider-api #:find-api #:register-api #:api-keys
           #:endpoint-path #:auth-headers
           #:build-request #:parse-stream #:thinking-param #:perform-request
           #:map-sse-events
           #:default-provider-key #:default-base-url #:default-api-key-env
           #:message-role #:message-content #:message-stop-reason
           #:usage-total-tokens #:message-usage))

(defpackage :evo.kernel
  (:use :cl :evo.util :evo.journal :evo.provider)
  (:export ;; tools
           #:tool #:tool-name #:tool-description #:tool-schema #:tool-execute-fn
           #:register-tool* #:find-tool #:all-tool-names #:active-tools
           #:schema->json-schema #:execute-tool
           ;; loop
           #:run #:run-until-settled #:make-agent #:agent
           #:agent-journal #:agent-events-cb #:agent-abort-flag
           #:agent-model-override #:agent-thinking-override
           #:request-abort #:with-abort-cleanup
           #:queue-steering #:queue-followup #:emit-event #:steering-pending-p
           ;; prompt, skills, templates
           #:build-system-prompt #:available-skills #:find-skill
           #:find-template #:expand-template
           ;; extension api internals
           #:run-hooks #:add-hook #:load-extension* #:boot-extensions
           #:boot-userspace #:load-init-file
           #:replay-loads #:lock-kernel-packages
           #:effective-model-id #:effective-model-provider #:effective-thinking
           ;; goal
           #:current-goal #:goal-continuation-message #:goal-continuation-for
           #:register-goal-tools #:create-goal-entry #:goal-tokens-used
           ;; lore + compaction
           #:add-lore #:add-session-lore #:all-lore #:all-lore-entries
           #:edit-lore #:remove-lore #:find-lore-scope
           #:compact-now #:compaction-needed-p #:estimate-context-tokens
           #:overflow-error-p #:select-cut))

;; Public API for extensions, config (init.lisp), and userspace code.
;;
;; The provider-API protocol is imported rather than re-defined: EVO:PARSE-STREAM
;; and EVO.PROVIDER:PARSE-STREAM are the same symbol, so an extension can
;; subclass and specialize the wire protocol without naming a kernel package.
(defpackage :evo
  (:use :cl)
  (:import-from :evo.util #:cat #:normalize-newlines #:crlf-newlines #:with-proxy)
  (:import-from :evo.provider
                #:provider-api #:register-api #:find-api #:api-keys
                #:endpoint-path #:auth-headers #:build-request #:parse-stream
                #:thinking-param #:perform-request #:map-sse-events
                #:default-provider-key #:default-base-url #:default-api-key-env
                #:provider-error)
  (:export #:cat #:normalize-newlines #:crlf-newlines #:with-proxy
           #:register-tool #:register-command #:on #:load-extension
           #:register-model #:register-provider #:set-setting #:setting
           #:set-active-tools #:all-tools #:*agent* #:current-goal
           #:steer #:inject-context #:custom-state #:set-custom-state
           ;; provider-API protocol (imported from EVO.PROVIDER above)
           #:provider-api #:register-api #:find-api #:api-keys
           #:endpoint-path #:auth-headers #:build-request #:parse-stream
           #:thinking-param #:perform-request #:map-sse-events
           #:default-provider-key #:default-base-url #:default-api-key-env
           #:provider-error))

;; Userspace: all agent-written tools and code live here.  Unlocked.
(defpackage :evo.user
  (:use :cl :evo))

;; Core extensions: bundled, built on the same extension API.
(defpackage :evo.todo
  (:use :cl :evo.util :evo.journal :evo.kernel)
  (:export #:current-todos #:format-todos))

(defpackage :evo.memory
  (:use :cl :evo.util)
  (:export #:*memory-kinds* #:memory-file #:read-memories #:render-memories))

;; /eval — one sexpr, evaluated in the live image (EVO.USER).
(defpackage :evo.eval
  (:use :cl :evo.util)
  (:export #:*eval-package-name* #:eval-package
           #:single-form #:eval-form
           ;; completion source: the image's own answer to "what could this
           ;; half-typed token be?", which frontends render.
           #:token-start #:completions-for #:symbol-kind))

(defpackage :evo.tui
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:start-tui
           ;; Status line composition — the supported way for an extension to
           ;; claim a piece of the bottom line (see docs/extension-api.md).
           #:add-status-segment #:remove-status-segment #:status-segments))

(defpackage :evo.cli
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:main #:setup-agent #:toplevel))
