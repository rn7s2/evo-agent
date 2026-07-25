;;;; evo.asd — system definition for evo, the self-evolving agent.

(asdf:defsystem "evo"
  :description "evo — a goal-oriented, self-evolving agent (MVP: M0 provider core + M1 kernel loop/journal + print/event modes + basic goal driver)."
  :author "evo-agent"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("dexador" "com.inuoe.jzon" "flexi-streams" "bordeaux-threads")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "journal")
                             (:file "model-table")
                             (:file "provider")
                             (:file "tools")
                             (:file "prompt")
                             (:file "loop")
                             (:file "extension")
                             (:file "builtin-tools")
                             (:file "goal")
                             (:file "cli")))))

(asdf:defsystem "evo/tests"
  :description "Unit tests for evo."
  :depends-on ("evo")
  :serial t
  :components ((:module "tests"
                :components ((:file "unit")))))
