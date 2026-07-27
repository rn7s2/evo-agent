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
  (:export #:exit-lisp #:argv #:runtime-pathname #:environ #:setenv
           #:launch-child #:process-alive-p #:process-kill #:process-kill-tree
           #:process-wait #:process-pid
           #:lock-package #:unlock-package #:add-package-local-nickname
           #:make-fd-output-stream #:make-fd-input-stream
           #:install-signal-handler #:+sigwinch+
           #:tty-p #:disable-debugger #:ensure-in-image-compiler))

(defpackage :evo.util
  (:use :cl)
  (:export #:getenv #:iso8601-now #:format-local-timestamp #:local-timezone-name
           #:gen-id #:reseed-ids #:pget #:pput #:plist-merge
           #:evo-home #:project-evo-dir #:encode-cwd #:ensure-directory
           #:write-sexpr-line #:read-sexpr #:read-sexpr-stream #:validate-journal-value
           #:setting #:set-setting #:reset-settings #:*settings*
           #:string-join #:string-prefix-p #:truncate-string
           #:count-substring #:string-replace
           #:read-file-string #:write-file-string))

(defpackage :evo.journal
  (:use :cl :evo.util)
  (:export #:journal #:make-session-journal #:open-journal #:journal-path
           #:journal-entries #:journal-leaf-id #:journal-header #:journal-started-p
           #:append-entry #:find-entry #:entry-path #:fold-state #:fork-session
           #:state-messages #:state-model #:state-thinking #:state-tools
           #:state-goal #:state-loads #:state-name #:state-custom #:custom-state
           #:list-sessions #:latest-session #:sessions-directory))

(defpackage :evo.provider
  (:use :cl :evo.util)
  (:export #:find-model #:all-models #:model-context-window #:model-max-output
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
           #:replay-loads #:lock-kernel-packages #:effective-model-id
           ;; goal
           #:current-goal #:goal-continuation-message #:goal-continuation-for
           #:register-goal-tools #:create-goal-entry #:goal-tokens-used
           ;; lore + compaction
           #:add-lore #:add-session-lore #:all-lore
           #:compact-now #:compaction-needed-p #:estimate-context-tokens
           #:overflow-error-p #:select-cut))

;; Public API for extensions, config (init.lisp), and userspace code.
;;
;; The provider-API protocol is imported rather than re-defined: EVO:PARSE-STREAM
;; and EVO.PROVIDER:PARSE-STREAM are the same symbol, so an extension can
;; subclass and specialize the wire protocol without naming a kernel package.
(defpackage :evo
  (:use :cl)
  (:import-from :evo.provider
                #:provider-api #:register-api #:find-api #:api-keys
                #:endpoint-path #:auth-headers #:build-request #:parse-stream
                #:thinking-param #:perform-request #:map-sse-events
                #:default-provider-key #:default-base-url #:default-api-key-env
                #:provider-error)
  (:export #:register-tool #:register-command #:on #:load-extension
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

;; Plan/auto modes.  Uses nothing but EVO.UTIL and the public API, the way a
;; userspace extension would.
(defpackage :evo.plan
  (:use :cl :evo.util)
  (:export #:*modes* #:*default-mode* #:*plan-tools* #:*plan-bash-allowlist*
           #:*plan-instructions* #:*instruction-key*
           #:mode-name #:current-mode #:plan-mode-p #:set-mode
           #:bash-block-reason))

(defpackage :evo.memory
  (:use :cl :evo.util)
  (:export #:*memory-kinds* #:memory-file #:read-memories #:render-memories))

(defpackage :evo.tui
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:start-tui))

(defpackage :evo.cli
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:main #:setup-agent #:toplevel))
