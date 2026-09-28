;;;; package.lisp — EVO.SERVE, the HTTP frontend (`evo serve`).
;;;;
;;;; Defined here rather than in src/packages.lisp for the same reason as the
;;;; TUI's: the frontend is built on the core, so nothing in the core can name
;;;; it.  See docs/serve.md for the protocol and design.md §16.2 for why it is
;;;; shaped this way.

(defpackage :evo.serve
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:make-server #:serve #:resolve-token #:loopback-host-p
           ;; the seams a program extends: who it is, and the routes it adds
           #:*identity* #:server-identity #:server-routes
           #:add-route #:make-route #:route-path #:route-method #:route-handler
           #:route-prefix-p #:*route-tail*
           ;; what a route handler answers with: JSON, SSE, and the same
           ;; numbered, resumable event log the session writes to
           #:server-publish #:server-cursor #:last-event-id #:write-json #:write-error
           #:write-sse-head #:write-sse-event #:write-sse-comment #:stream-events
           ;; the event mapping (docs/serve.md "Events")
           #:sexpr->json-value #:json-value->sexpr #:encode-json #:decode-json
           #:event->json
           ;; HTTP, exposed for the tests
           #:read-request #:write-response #:route-request
           #:request-method #:request-path #:request-query #:request-headers
           #:request-body #:request-header))
