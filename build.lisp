;;;; build.lisp — build the single evo executable (images are build
;;;; artifacts only).  Run via: make build [LISP=sbcl|ecl]
;;;; on Unix, or `./make.ps1 build` on Windows (SBCL only there).
;;;;
;;;; SBCL: the Makefile invokes sbcl with --dynamic-space-size 4096 and we
;;;; save with :save-runtime-options t, so the heap size is baked into the
;;;; binary and ALL argv goes to evo's own main — no launcher, no
;;;; --end-runtime-options.  On Windows the artifact must be named .exe:
;;;; the loader dispatches on the extension, and an extensionless image is
;;;; a file Windows will not run.
;;;;
;;;; ECL: monolithic-lib-op archives evo plus every dependency, and
;;;; c:build-program links that against libasdf.a (the compiled asdf+uiop
;;;; module — program-op alone cannot bundle uiop, which it treats as
;;;; preloaded) into a native executable.  The ECL heap grows on demand,
;;;; so nothing is baked in.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)

(ensure-directories-exist "build/")

#+sbcl
(sb-ext:save-lisp-and-die #+(or win32 windows mswindows) "build/evo.exe"
                          #-(or win32 windows mswindows) "build/evo"
                          :executable t
                          :save-runtime-options t
                          :toplevel #'evo.cli:toplevel)

#+ecl
(progn
  ;; Every contrib module `require`d so far (asdf/uiop, sockets, plus
  ;; whatever the build environment itself pulled in, e.g. cmp) is linked
  ;; in as its prebuilt SYS:lib<module>.a — ASDF treats provided modules
  ;; as preloaded and would bundle nothing, leaving their packages
  ;; undefined at binary startup.  Extra riders are harmless: the runtime
  ;; compiler is switched to bytecodes at startup regardless.
  (defparameter *contrib-libs*
    (let ((seen nil))
      (loop for module in (reverse *modules*)   ; chronological provide order
            for name = (string-downcase module)
            for lib = (probe-file (pathname (format nil "SYS:lib~a.a" name)))
            when (and lib (not (member (namestring lib) seen :test #'equal)))
              do (push (namestring lib) seen)
              and collect lib)))
  (require :cmp)
  (asdf:operate 'asdf:monolithic-lib-op "evo")
  (let ((lib (first (asdf:output-files 'asdf:monolithic-lib-op "evo")))
        (shim-src (merge-pathnames "build/preload-systems.lisp" (uiop:getcwd)))
        (shim-obj (merge-pathnames "build/preload-systems.o" (uiop:getcwd))))
    ;; Dependencies may call (asdf:find-system ...) while initializing —
    ;; dexador derives its User-Agent from its system version — and the
    ;; binary's ASDF has no source registry.  Register every build-time
    ;; system as preloaded so those lookups succeed offline.
    (with-open-file (s shim-src :direction :output :if-exists :supersede)
      (format s "(in-package :cl-user)~%")
      (dolist (name (asdf:already-loaded-systems))
        (let ((version (ignore-errors
                         (asdf:component-version (asdf:find-system name)))))
          (format s "(uiop:symbol-call :asdf '#:register-preloaded-system ~s~@[ :version ~s~])~%"
                  name version))))
    (compile-file shim-src :output-file shim-obj :system-p t)
    (c:build-program "build/evo"
                     :lisp-files (append *contrib-libs* (list shim-obj lib))
                     :epilogue-code '(evo.cli:toplevel)))
  (format t "~&; wrote build/evo (linked contribs: ~{~a~^ ~})~%"
          (mapcar #'pathname-name *contrib-libs*))
  (uiop:quit 0))
