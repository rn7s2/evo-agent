;;;; package.lisp — EVO.SERVE, the HTTP frontend (`evo serve`).
;;;;
;;;; Defined here rather than in src/packages.lisp for the same reason as the
;;;; TUI's: the frontend is built on the core, so nothing in the core can name
;;;; it.  See docs/serve.md for the protocol and CONTRACT.md §5 for what it
;;;; must be.

(defpackage :evo.serve
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export ;; the server a program runs
           #:make-server #:serve #:resolve-token #:loopback-host-p
           #:server #:server-agent #:server-token #:server-host #:server-port
           #:server-quit #:server-ready-file #:server-eval-enabled
           #:server-epoch #:server-seq #:server-cursor #:server-oplog
           #:server-interrupt-hook #:server-shutdown-hook
           #:server-program #:server-version #:server-identity
           #:server-status #:server-task #:task-id #:task-kind
           #:task-started #:task-step-started
           ;; the serving life: the ready file it publishes, and the parent
           ;; death it watches for (CONTRACT §1, §8)
           #:write-ready-file #:ready-file-value #:delete-ready-file
           #:write-json-atomically #:mint-epoch #:watch-stdin-eof
           ;; topics: how a program adds what it observes (CONTRACT §7)
           #:register-topic #:unregister-topic #:topic-provider
           #:topic-provider-names #:expand-topic-names
           #:topic-snapshot #:topic-items-before #:topic-item #:topic-media
           #:topic-feed-event #:topic-feed-append #:topic-provider-reset
           #:topic-provider-sync #:sync-session-topic
           #:topic-provider-queued-input #:topic-provider-input-cancelled
           #:topic-reset #:topic-notice #:topic-on-event
           #:publish-op #:publish-state-patch #:publish-item-add
           #:op-now-ms
           ;; the interrupt scopes the program owns (CONTRACT §5.5)
           #:interrupt-scope #:lane-exists-p
           ;; the catalog builder (CONTRACT §5.6) and what readiness means
           #:catalog-plist #:catalog-for-server #:sessions-body
           #:model-readiness #:model-status #:lane-model-status #:lane-model-entry
           ;; the `lanes` half: a program that runs lanes registers it
           #:*catalog-lanes-hook* #:lane-catalog
           #:*op-catalog* #:*kernel-apis* #:kernel-api-registry
           ;; ops, for a program that adds its own
           #:register-op #:find-op #:all-ops #:op-error #:op-error-code
           #:op-error-message #:op-fail #:dispatch-op #:answer-op
           ;; the seams a program extends: who it is, and the routes it adds
           #:*identity* #:server-routes
           #:add-route #:make-route #:route-path #:route-method #:route-handler
           #:route-prefix-p #:*route-tail*
           ;; what a route handler answers with
           #:write-json #:write-error #:write-response #:write-response-octets
           #:write-sse-head #:write-sse-event #:write-sse-comment
           #:stream-ops #:write-sse-reset #:*sse-ping-seconds*
           ;; the mapping between evo's sexprs and JSON
           #:sexpr->json-value #:json-value->sexpr #:encode-json #:decode-json
           #:plist-p #:json-object-value
           ;; HTTP, exposed for the tests
           #:read-request #:request-method #:request-path #:request-query
           #:request-headers #:request-body #:request-header #:request-query-param
           #:route-request #:respond #:http-fail))
