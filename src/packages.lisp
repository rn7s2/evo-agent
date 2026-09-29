;;;; packages.lisp — package definitions for evo's core.
;;;;
;;;; The core's packages (EVO.UTIL, EVO.JOURNAL, EVO.PROVIDER, EVO.KERNEL, ...)
;;;; are locked at boot.  Userspace (EVO.USER) is unlocked; all
;;;; agent-written code lives there.  EVO is the public extension API surface —
;;;; both core and user extensions build on it, nothing bypasses it.
;;;;
;;;; The frontends are not here: EVO.TUI and EVO.CLI each define their own
;;;; package (src/tui/package.lisp, src/cli/package.lisp) on top of these,
;;;; so nothing in the core can name them.

;; Implementation portability layer: the only package that may touch
;; sb-* / ext: / si: symbols.  See src/port/port.lisp.
(defpackage :evo.port
  (:use :cl)
  (:export #:exit-lisp #:add-exit-hook #:run-exit-hooks
           #:argv #:runtime-pathname #:environ #:setenv #:getpid
           #:*program-name*
           #:call-with-timeout #:timeout-error
           #:launch-child #:process-alive-p #:pid-alive-p #:process-kill #:process-kill-tree
           #:reap-pid-tree #:process-wait #:process-pid
           #:program-in-path #:windows-p #:path-separator
           #:shell-invocation #:shell-name
           #:lock-package #:unlock-package #:add-package-local-nickname
           #:make-stdout-stream #:make-stdin-stream
           #:read-available-input
           #:std-descriptor #:std-external-format
           #:install-signal-handler #:+sigwinch+
           #:terminal-raw-mode #:restore-terminal-mode #:terminal-sane
           #:terminal-size
           #:tty-p #:disable-debugger #:ensure-in-image-compiler
           #:chmod-private #:write-private-file #:random-octets))

(defpackage :evo.util
  (:use :cl)
  (:export #:getenv #:env-proxy #:with-proxy #:*request-proxy*
           #:ensure-winhttp-proxy #:iso8601-now #:iso8601-utc
           #:format-local-timestamp #:local-timezone-name
           #:gen-id #:reseed-ids #:pget #:pput #:plist-merge
           #:evo-home #:project-evo-dir #:encode-cwd
           #:write-sexpr-line #:read-sexpr-stream #:validate-journal-value
           #:setting #:set-setting #:reset-settings #:*settings*
           #:capture-settings #:restore-settings
           #:cat #:normalize-newlines #:crlf-newlines #:crlf-p
           #:string-join #:string-prefix-p #:truncate-string
           #:count-substring #:string-replace
           #:read-file-string #:write-file-string
           #:read-file-octets #:write-file-octets
           #:octets->base64 #:base64->octets))

;; Images in: clipboard grabs, file attachments, media-type sniffing.  Builds
;; the :image content blocks the provider adapters already know how to encode
;; — for the `read` tool, and for the frontends' attachments.
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
           #:list-sessions #:latest-session #:sessions-directory
           #:session-updated #:sort-sessions))

(defpackage :evo.provider
  (:use :cl :evo.util)
  (:export #:find-model #:all-models #:model-providers
           #:model-context-window #:model-max-output #:model-max-input-items
           #:model-effort #:model-thinking-mode #:model-vision-p #:+effort-levels+
           #:normalize-thinking-level
           #:register-model* #:register-provider* #:provider-config
           #:provider-registration #:provider-keys #:json->sexpr
           #:reset-user-registries
           #:call-provider #:provider-error
           #:parse-sse-stream
           ;; provider-API protocol — an extension point: subclass
           ;; PROVIDER-API, implement the generics, REGISTER-API it.
           #:provider-api #:find-api #:register-api #:api-keys
           #:endpoint-path #:auth-headers
           #:build-request #:parse-stream #:perform-request
           #:map-sse-events
           #:default-provider-key #:default-base-url #:default-api-key-env
           #:message-role #:message-content #:message-stop-reason
           #:usage-total-tokens #:message-usage))

(defpackage :evo.kernel
  (:use :cl :evo.util :evo.journal :evo.provider)
  (:export ;; tools
           #:tool #:tool-name #:tool-description #:tool-schema #:tool-execute-fn
           #:tool-arguments
           #:register-tool* #:find-tool #:all-tool-names #:active-tools
           #:schema->json-schema #:execute-tool #:tool-call-arguments
           #:tool-call-display-arguments
           #:tool-content-blocks #:result-display-text
           ;; loop
           #:run #:run-until-settled #:make-agent #:agent
           #:agent-journal #:agent-events-cb #:agent-abort-flag
           #:agent-model-override #:agent-thinking-override
           #:request-abort #:reset-agent-run-control #:with-abort-cleanup
           #:*executing-agent*
           #:queue-steering #:queue-followup #:emit-event #:steering-pending-p
           #:agent-pending-work-p #:reset-agent-session-state
           #:heartbeat-touch
           ;; prompt, skills, templates
           #:build-system-prompt #:register-prompt-note
           ;; prompt language packs
           #:register-prompt-language #:find-prompt-language
           #:all-prompt-languages #:prompt-section #:*prompt-sections*
           #:*default-language* #:language-code #:language-request
           #:resolve-language #:set-prompt-language
           #:available-skills #:find-skill
           #:template-directories #:find-template #:expand-template
           ;; extension api internals
           #:run-hooks #:add-hook #:event-hook-functions
           #:remove-hooks-if #:load-extension* #:*current-journal*
           #:boot-extensions #:boot-userspace #:load-init-file #:*post-init-hooks*
           #:replay-loads #:lock-kernel-packages
           ;; slash-command registry (resolved by a frontend)
           #:register-command* #:find-command #:registered-commands
           ;; extension ownership + runtime generations
           #:*extension-owner* #:*extension-generation*
           #:register-extension-disposer #:register-extension-task
           #:dispose-extension-owners #:*task-stop-seconds*
           #:capture-runtime-catalog #:install-runtime-catalog
           #:effective-model-id #:effective-model-provider #:effective-model
           #:effective-thinking
           ;; session operations — the journal writes a frontend asks for
           #:boot-session #:switch-session
           #:set-session-model #:set-session-thinking #:end-session
           ;; frontend protocol — answered by whichever frontend runs
           #:*frontend* #:frontend-interactive-p #:frontend-request-run
           ;; goal
           #:current-goal #:goal-continuation-message #:goal-continuation-for
           #:register-goal-tools #:create-goal-entry #:goal-tokens-used
           #:update-goal-entry #:set-goal-objective #:complete-goal
           #:*goal-hold-predicates* #:goal-held-p
           ;; lore + compaction
           #:add-lore #:add-session-lore #:all-lore-entries
           #:edit-lore #:remove-lore #:find-lore-scope
           #:compact-now #:compaction-needed-p #:estimate-context-tokens
           #:count-input-items
           #:overflow-error-p #:select-cut
           ;; background jobs
           #:running-jobs-summary))

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
                #:perform-request #:map-sse-events
                #:default-provider-key #:default-base-url #:default-api-key-env
                #:provider-error #:provider-registration #:json->sexpr)
  (:export #:cat #:normalize-newlines #:crlf-newlines #:with-proxy
           #:register-tool #:register-command #:on #:on-unload #:spawn-task
           #:load-extension
           #:register-prompt-note #:register-prompt-language #:set-language
           #:register-model #:register-provider #:set-setting #:setting
           #:set-active-tools #:all-tools #:*agent* #:current-goal
           #:steer #:inject-context #:custom-state #:set-custom-state
           ;; the frontend this session runs under
           #:frontend-interactive-p #:request-run
           ;; provider-API protocol (imported from EVO.PROVIDER above)
           #:provider-api #:register-api #:find-api #:api-keys
           #:endpoint-path #:auth-headers #:build-request #:parse-stream
           #:perform-request #:map-sse-events
           #:default-provider-key #:default-base-url #:default-api-key-env
           #:provider-error
           ;; what a provider was registered with, unresolved
           #:provider-registration
           ;; parsed JSON (jzon values) -> keyword plists, the bridge the
           ;; provider layer uses for tool arguments
           #:json->sexpr))

;; Userspace: all agent-written tools and code live here.  Unlocked.
(defpackage :evo.user
  (:use :cl :evo))

;; Core extensions: bundled, built on the same extension API.
(defpackage :evo.todo
  (:use :cl :evo.util :evo.journal :evo.kernel)
  (:export #:current-todos #:format-todos #:status-glyph))

(defpackage :evo.lang.en
  (:use :cl))

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

;; The command layer: what a slash command does to the session, once, for
;; every frontend (src/command/command.lisp).  Frontends implement its HOST
;; protocol; the core names none of them.
(defpackage :evo.command
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export ;; the host protocol a frontend implements
           #:host-agent #:host-running-p #:host-start-run #:host-start-compact
           #:host-say #:host-refresh #:host-choose #:host-set-draft
           #:host-session-switched #:host-show-history #:host-submit
           #:host-command-context #:host-interrupt-hint #:host-data
           #:host-command-failed
           ;; refusals and quiescence
           #:command-refused #:command-refused-text #:command-refused-kind
           #:refuse #:with-refusals-shown
           #:session-quiescent-p #:require-session-quiescent #:require-idle
           #:release-queued-input #:switch-journal
           ;; dispatch
           #:dispatch-command #:parse-command #:builtin-command
           #:command-catalog #:*builtin-commands* #:template-names
           ;; the commands, callable directly
           #:goal-command #:set-model #:model-select #:thinking-command
           #:set-language #:language-select #:lore-command #:compact-command
           #:tree-command #:move-leaf #:rewind-command
           #:resume-command #:resume-session #:fork-command #:new-command
           #:export-command #:reload-command
           #:command-as-skill #:command-as-template
           ;; display helpers the frontends share
           #:format-context-window #:model-row-label
           #:format-tool-call-plain #:tool-arg-value
           #:*tool-key-args* #:*tool-call-max-width*
           #:entry-label #:message-text-block #:first-user-prompt
           #:resume-summary-text #:resume-select-items
           #:*resume-summary-max-chars* #:export-image
           ;; derived state
           #:session-summary))
