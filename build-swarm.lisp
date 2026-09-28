;;;; build-swarm.lisp — build the evo-swarm executable into build/evo-swarm.
;;;; Run via: make build [LISP=sbcl|ecl], or `./make.ps1 build` on Windows.
;;;;
;;;; build.lisp does the work; this only names what it builds.  A separate
;;;; process from evo's build, because saving an SBCL image ends the process.

(defvar cl-user::*build-system* "evo-swarm")
(defvar cl-user::*build-output* "build/evo-swarm")
(defvar cl-user::*build-toplevel* '("EVO.SWARM" "TOPLEVEL"))

(load (merge-pathnames "build.lisp" (directory-namestring *load-truename*)))
