;;;; evo.asd — system definitions for evo, the self-evolving agent.
;;;;
;;;; Two layers, two systems.  "evo/core" is the agent: foundations, kernel
;;;; and the core extensions, loadable with no frontend at all.  "evo" is the
;;;; shipped program: the core plus the interactive TUI and the CLI that
;;;; composes them.  The split is what keeps the dependency pointing one way —
;;;; a core file that reached for a frontend would fail to load "evo/core" on
;;;; its own, and `make test` loads it on its own (tests/core-only.lisp).

(asdf:defsystem "evo/core"
  :description "evo's core — kernel, foundations and core extensions, no frontend."
  :author "evo-agent"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("dexador" "com.inuoe.jzon" "flexi-streams" "bordeaux-threads"
               "local-time")
  :serial t
  :components ((:module "src"
                :serial t
                ;; One directory per component; each is a single package.
                ;; Order is load order — foundations, kernel, then the core
                ;; extensions built on top of it.
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
                                           (:file "anthropic")))
                             (:module "kernel"
                              :serial t
                              :components ((:file "tools")
                                           (:file "prompt")
                                           (:file "loop")
                                           (:file "lore")
                                           (:file "compact")
                                           (:file "extension")
                                           (:file "session")
                                           (:file "jobs")
                                           (:file "builtin-tools")
                                           (:file "goal")))
                             ;; Core extensions: bundled, but built on the
                             ;; same public API as userspace ones.
                             (:module "core-ext"
                              :serial t
                              :components ((:file "lang-en")
                                           (:file "todo")
                                           (:file "memory")
                                           (:file "eval")))))))

(asdf:defsystem "evo"
  :description "evo — a goal-oriented, self-evolving agent."
  :author "evo-agent"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("evo/core" "bordeaux-threads" "flexi-streams")
  :serial t
  :components ((:module "src"
                :serial t
                ;; The frontends, each defining its own package on top of the
                ;; core's: the TUI first, then the CLI that composes the two.
                :components ((:module "tui"
                              :serial t
                              :components ((:file "package")
                                           (:file "term")
                                           (:file "input")
                                           (:file "editor")
                                           (:file "render")
                                           (:file "math")
                                           (:file "markdown")
                                           (:file "tui")
                                           (:file "commands")))
                             (:module "cli"
                              :serial t
                              :components ((:file "package")
                                           (:file "cli")
                                           (:file "supervisor")))))))

(asdf:defsystem "evo/tests"
  :description "Unit tests for evo."
  :depends-on ("evo")
  :serial t
  :components ((:module "tests"
                :components ((:file "unit")))))
