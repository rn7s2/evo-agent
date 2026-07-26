LISP ?= sbcl
EVO_HOME ?= $(HOME)/.evo
PREFIX ?= /usr/local
# Heap for the SBCL-built binary — baked in via :save-runtime-options (D10).
# ECL's heap grows on demand; no build-time size there.
HEAP_MB ?= 4096

# Per-implementation "load a script non-interactively" invocations.  The
# scripts themselves exit explicitly, so ECL only needs stdin closed to
# guarantee no REPL is left behind on error.
ifeq ($(LISP),ecl)
RUN_SCRIPT = $(LISP) -q --load
BUILD_SCRIPT = $(LISP) -q --load
STDIN_GUARD = < /dev/null
else
RUN_SCRIPT = $(LISP) --non-interactive --load
BUILD_SCRIPT = $(LISP) --dynamic-space-size $(HEAP_MB) --non-interactive --load
STDIN_GUARD =
endif

.PHONY: build test integration tui-test clean install install-home

build:
	$(BUILD_SCRIPT) build.lisp $(STDIN_GUARD)

install: build
	install -m 755 build/evo $(PREFIX)/bin/evo

test:
	$(RUN_SCRIPT) tests/run-unit.lisp $(STDIN_GUARD)

integration: build
	tests/integration.sh

tui-test: build
	tests/tui.exp

# Seed corpus (§10.4): docs + example extensions into the global evo home.
# plan-mode is installed active; the other examples stay reference-only.
# The sample init.lisp is reference-only too: evo requires a real
# $(EVO_HOME)/init.lisp (no built-in model table) — copy and edit it.
install-home:
	mkdir -p $(EVO_HOME)/extensions $(EVO_HOME)/docs/examples $(EVO_HOME)/skills $(EVO_HOME)/prompts
	cp docs/*.md $(EVO_HOME)/docs/
	cp docs/examples/init.lisp $(EVO_HOME)/docs/examples/
	cp extensions/examples/*.lisp $(EVO_HOME)/docs/examples/
	cp extensions/plan-mode.lisp $(EVO_HOME)/extensions/

clean:
	rm -rf build
