;;;; evo-swarm.asd — evo-swarm: one coordinator agent and a pool of worker
;;;; lanes (docs/swarm.md).
;;;;
;;;; Its own system, on top of evo's: the coordinator is an evo session with
;;;; the TUI (from "evo"), and each lane is the evo binary running `evo
;;;; serve`, driven over HTTP.  The dependency points one way only — nothing in
;;;; "evo" names the swarm, and `make test` loads "evo" alone to prove it
;;;; (tests/evo-only.lisp), as it loads "evo/core" alone for the frontends.

(asdf:defsystem "evo-swarm"
  :description "evo-swarm — a coordinator agent driving a pool of evo worker lanes."
  :author "evo-agent"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("evo" "dexador" "usocket" "bordeaux-threads" "flexi-streams")
  :serial t
  :components ((:module "swarm"
                :serial t
                :components ((:file "package")
                             (:file "state")
                             (:file "client")
                             (:file "init")
                             (:file "lanes")
                             (:file "tools")
                             (:file "tui")
                             (:file "main")))))
