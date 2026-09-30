;;;; package.lisp — EVO.SERVE, the HTTP frontend (`evo serve`).
;;;;
;;;; Defined here rather than in src/packages.lisp for the same reason as the
;;;; TUI's: the frontend is built on the core, so nothing in the core can name
;;;; it.  See docs/serve.md for the protocol and CONTRACT.md §5 for what it
;;;; must be.

(defpackage :evo.serve
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:make-server #:serve #:resolve-token #:loopback-host-p
           ;; the serving life: its epoch, the ready file it publishes, and
           ;; the parent death it watches for
           #:server-epoch #:write-ready-file #:ready-file-value
           #:delete-ready-file #:write-json-atomically #:mint-epoch
           #:watch-stdin-eof
           ;; what this session can use (GET /catalog, and the offline
           ;; `catalog --json` of evo-agent and evo-swarm)
           #:catalog-plist #:model-readiness #:model-status #:lane-model-status
           #:*op-catalog* #:*kernel-apis*
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
