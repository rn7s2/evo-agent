;;;; view-topic.lisp — the session topic's provider, when the view is loaded.
;;;;
;;;; CONTRACT §7 is the interface: `evo.view:make-view` builds a projection of
;;;; the agent's journal, `view-attach` hands it a publish callback (every
;;;; change becomes an op), `view-on-event` / `view-on-append` feed it, and
;;;; `view-snapshot` / `view-items-before` / `view-item` / `view-media` answer
;;;; the reads serve's protocol needs.  This file is that wiring and nothing
;;;; else — the projection lives in src/view/.
;;;;
;;;; The symbols are resolved once, at install time, rather than at read time:
;;;; "evo" is built without the view system in the work packages that do not
;;;; need it (tests/evo-only.lisp loads the frontends alone), so a file that
;;;; named EVO.VIEW:MAKE-VIEW directly would fail to load there.  A missing
;;;; view is not an error — INSTALL-SESSION-TOPIC falls back to the local test
;;;; double (view-fallback.lisp).

(in-package :evo.serve)

(defstruct (view-topic (:constructor %make-view-topic))
  server
  agent
  view
  (fn nil))                            ; name -> function, resolved once

(defun view-fn (topic name)
  (or (getf (view-topic-fn topic) name)
      (setf (getf (view-topic-fn topic) name)
            (let ((symbol (find-symbol name :evo.view)))
              (unless (and symbol (fboundp symbol))
                (error "evo.view:~a is not defined" name))
              (fdefinition symbol)))))

(defun view-topic-available-p ()
  (and (find-package :evo.view)
       (let ((maker (find-symbol "MAKE-VIEW" :evo.view)))
         (and maker (fboundp maker)))))

(defun make-view-topic (server agent)
  "The real session topic: the view, attached to this server's op log."
  (let ((topic (%make-view-topic :server server :agent agent)))
    (setf (view-topic-view topic)
          (funcall (view-fn topic "MAKE-VIEW") agent :topic "session"))
    (funcall (view-fn topic "VIEW-ATTACH")
             (view-topic-view topic)
             (lambda (op-plist) (publish-op server op-plist)))
    topic))

(defmethod topic-snapshot ((topic view-topic) &key (items 200))
  (funcall (view-fn topic "VIEW-SNAPSHOT") (view-topic-view topic) :items items))

(defmethod topic-items-before ((topic view-topic) before-id limit)
  (funcall (view-fn topic "VIEW-ITEMS-BEFORE") (view-topic-view topic) before-id limit))

(defmethod topic-item ((topic view-topic) id)
  (funcall (view-fn topic "VIEW-ITEM") (view-topic-view topic) id))

(defmethod topic-media ((topic view-topic) id n)
  (funcall (view-fn topic "VIEW-MEDIA") (view-topic-view topic) id n))

(defmethod topic-feed-event ((topic view-topic) event)
  (funcall (view-fn topic "VIEW-ON-EVENT") (view-topic-view topic) event)
  t)

(defmethod topic-feed-append ((topic view-topic) entry)
  (funcall (view-fn topic "VIEW-ON-APPEND") (view-topic-view topic) entry)
  t)

(defmethod topic-provider-reset ((topic view-topic) reason)
  (funcall (view-fn topic "VIEW-RESET") (view-topic-view topic) reason))

(defun install-session-topic (server agent)
  "Install the provider for the `session` topic: the view when the view system
is loaded, the local test double otherwise (CONTRACT §7)."
  (register-topic server "session"
                  (if (view-topic-available-p)
                      (make-view-topic server agent)
                      (make-fallback-topic server agent))))
