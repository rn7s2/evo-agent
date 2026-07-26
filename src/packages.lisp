;;;; packages.lisp — package definitions for evo.
;;;;
;;;; Kernel packages (EVO.UTIL, EVO.JOURNAL, EVO.PROVIDER, EVO.KERNEL, EVO.CLI)
;;;; are locked at boot.  Userspace (EVO.USER) is unlocked; all
;;;; agent-written code lives there.  EVO is the public extension API surface —
;;;; both core and user extensions build on it, nothing bypasses it.

;; Implementation portability layer: the only package that may touch
;; sb-* / ext: / si: symbols.  See src/port.lisp.
(defpackage :evo.port
  (:use :cl)
  (:export #:exit-lisp #:argv #:runtime-pathname #:environ #:setenv
           #:launch-child #:process-alive-p #:process-kill #:process-wait
           #:lock-package #:unlock-package #:add-package-local-nickname
           #:make-fd-output-stream #:make-fd-input-stream
           #:install-signal-handler #:+sigwinch+
           #:tty-p #:disable-debugger #:ensure-in-image-compiler))

(defpackage :evo.util
  (:use :cl)
  (:export #:getenv #:iso8601-now #:gen-id #:reseed-ids #:pget #:pput #:plist-merge
           #:evo-home #:project-evo-dir #:encode-cwd #:ensure-directory
           #:write-sexpr-line #:read-sexpr #:read-sexpr-stream #:validate-journal-value
           #:load-settings #:setting #:*settings*
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
  (:export #:find-model #:*models* #:model-context-window #:model-max-output
           #:call-provider #:provider-error
           #:parse-sse-stream #:thinking-budget
           #:message-role #:message-content #:message-stop-reason
           #:usage-total-tokens #:message-usage #:message-cost))

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
           #:queue-steering #:queue-followup #:emit-event #:steering-pending-p
           ;; prompt, skills, templates
           #:build-system-prompt #:available-skills #:find-skill
           #:find-template #:expand-template
           ;; extension api internals
           #:run-hooks #:add-hook #:load-extension* #:boot-extensions
           #:replay-loads #:lock-kernel-packages
           ;; goal
           #:current-goal #:goal-continuation-message #:goal-continuation-for
           #:register-goal-tools #:create-goal-entry #:goal-tokens-used
           ;; lore + compaction
           #:add-lore #:add-session-lore #:all-lore
           #:compact-now #:compaction-needed-p #:estimate-context-tokens
           #:overflow-error-p #:select-cut))

;; Public API for extensions and userspace code.
(defpackage :evo
  (:use :cl)
  (:export #:register-tool #:register-command #:on #:load-extension
           #:set-active-tools #:all-tools #:*agent* #:current-goal
           #:steer #:inject-context #:custom-state #:set-custom-state))

;; Userspace: all agent-written tools and code live here.  Unlocked.
(defpackage :evo.user
  (:use :cl :evo))

;; Core extensions: bundled, built on the same extension API.
(defpackage :evo.todo
  (:use :cl :evo.util :evo.journal :evo.kernel)
  (:export #:current-todos #:format-todos))

(defpackage :evo.tui
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:start-tui))

(defpackage :evo.cli
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:main #:setup-agent #:toplevel))
