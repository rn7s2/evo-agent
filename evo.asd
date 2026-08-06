;;;; evo.asd — system definition for evo, the self-evolving agent.

(asdf:defsystem "evo"
  :description "evo — a goal-oriented, self-evolving agent."
  :author "evo-agent"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("dexador" "com.inuoe.jzon" "flexi-streams" "bordeaux-threads"
               "local-time")
  :serial t
  :components ((:module "src"
                :serial t
                ;; One directory per component; each is a single package.
                ;; Order is load order — foundations, kernel, then the
                ;; extensions and frontends built on top of it.
                :components ((:file "packages")
                             (:module "port"
                              :serial t
                              :components ((:file "port")))
                             (:module "util"
                              :serial t
                              :components ((:file "util")))
                             (:module "media"
                              :serial t
                              :components ((:file "media")))
                             (:module "journal"
                              :serial t
                              :components ((:file "journal")))
                             (:module "provider"
                              :serial t
                              :components ((:file "api")
                                           (:file "registry")
                                           (:file "core")
                                           (:file "anthropic")
                                           (:file "openai")))
                             (:module "kernel"
                              :serial t
                              :components ((:file "tools")
                                           (:file "prompt")
                                           (:file "loop")
                                           (:file "lore")
                                           (:file "compact")
                                           (:file "extension")
                                           (:file "builtin-tools")
                                           (:file "goal")))
                             ;; Core extensions: bundled, but built on the
                             ;; same public API as userspace ones.
                             (:module "core-ext"
                              :serial t
                              :components ((:file "todo")
                                           (:file "memory")
                                           (:file "eval")))
                             (:module "tui"
                              :serial t
                              :components ((:file "term")
                                           (:file "input")
                                           (:file "editor")
                                           (:file "render")
                                           (:file "markdown")
                                           (:file "tui")
                                           (:file "commands")))
                             (:module "cli"
                              :serial t
                              :components ((:file "cli")
                                           (:file "supervisor")))))))

(asdf:defsystem "evo/tests"
  :description "Unit tests for evo."
  :depends-on ("evo")
  :serial t
  :components ((:module "tests"
                :components ((:file "unit")))))
