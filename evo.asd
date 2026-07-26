;;;; evo.asd — system definition for evo, the self-evolving agent.

(asdf:defsystem "evo"
  :description "evo — a goal-oriented, self-evolving agent."
  :author "evo-agent"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("dexador" "com.inuoe.jzon" "flexi-streams" "bordeaux-threads")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "port")
                             (:file "util")
                             (:file "journal")
                             (:file "model-table")
                             (:file "provider")
                             (:file "provider-openai")
                             (:file "tools")
                             (:file "prompt")
                             (:file "loop")
                             (:file "lore")
                             (:file "compact")
                             (:file "extension")
                             (:file "builtin-tools")
                             (:file "todo")
                             (:file "goal")
                             (:module "tui"
                              :serial t
                              :components ((:file "term")
                                           (:file "input")
                                           (:file "editor")
                                           (:file "render")
                                           (:file "tui")
                                           (:file "commands")))
                             (:file "cli")
                             (:file "supervisor")))))

(asdf:defsystem "evo/tests"
  :description "Unit tests for evo."
  :depends-on ("evo")
  :serial t
  :components ((:module "tests"
                :components ((:file "unit")))))
