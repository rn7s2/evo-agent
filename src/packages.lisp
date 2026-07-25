;;;; packages.lisp — package definitions for evo.
;;;;
;;;; Kernel packages (EVO.UTIL, EVO.JOURNAL, EVO.PROVIDER, EVO.KERNEL, EVO.CLI)
;;;; are locked at boot (D8).  Userspace (EVO.USER) is unlocked; all
;;;; agent-written code lives there.  EVO is the public extension API surface —
;;;; both core and user extensions build on it, nothing bypasses it (D13).

(defpackage :evo.util
  (:use :cl)
  (:export #:getenv #:iso8601-now #:gen-id #:pget #:pput #:plist-merge
           #:evo-home #:project-evo-dir #:encode-cwd #:ensure-directory
           #:write-sexpr-line #:read-sexpr #:read-sexpr-stream #:validate-journal-value
           #:load-settings #:setting #:*settings*
           #:string-join #:string-prefix-p #:truncate-string
           #:read-file-string #:write-file-string))

(defpackage :evo.journal
  (:use :cl :evo.util)
  (:export #:journal #:make-session-journal #:open-journal #:journal-path
           #:journal-entries #:journal-leaf-id #:journal-header #:journal-started-p
           #:append-entry #:find-entry #:entry-path #:fold-state
           #:state-messages #:state-model #:state-thinking #:state-tools
           #:state-goal #:state-loads #:state-name
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
           #:queue-steering #:queue-followup #:emit-event
           ;; prompt
           #:build-system-prompt
           ;; extension api internals
           #:run-hooks #:add-hook #:load-extension* #:boot-extensions
           #:replay-loads #:lock-kernel-packages
           ;; goal
           #:current-goal #:goal-continuation-message #:register-goal-tools
           #:create-goal-entry))

;; Public API for extensions and userspace code.
(defpackage :evo
  (:use :cl)
  (:export #:register-tool #:register-command #:on #:load-extension
           #:set-active-tools #:*agent* #:current-goal))

;; Userspace: all agent-written tools and code live here.  Unlocked.
(defpackage :evo.user
  (:use :cl :evo))

(defpackage :evo.cli
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:main))
