# evo — build & install
#
# Configuration variables (override on the command line, e.g. `make LISP=ecl`):
#   LISP     — Common Lisp implementation used to build and run scripts.
#   EVO_HOME — Global evo home seeded by `install-home` (docs, examples, ...).
#   PREFIX   — Install prefix; the binary lands in $(PREFIX)/bin/evo.
#   HEAP_MB  — Dynamic heap (MiB) for the SBCL-built binary, baked in via
#              :save-runtime-options (D10). ECL grows its heap on demand, so
#              this has no effect there.
LISP ?= sbcl
EVO_HOME ?= $(HOME)/.evo
PREFIX ?= /usr/local
HEAP_MB ?= 4096

# Per-implementation "load a script non-interactively" invocations. The
# scripts exit explicitly, so ECL only needs stdin closed (STDIN_GUARD) to
# guarantee no REPL is left behind on error. BUILD_SCRIPT additionally sizes
# the heap for SBCL (see HEAP_MB above).
ifeq ($(LISP),ecl)
RUN_SCRIPT = $(LISP) -q --load
BUILD_SCRIPT = $(LISP) -q --load
STDIN_GUARD = < /dev/null
else
RUN_SCRIPT = $(LISP) --non-interactive --load
BUILD_SCRIPT = $(LISP) --dynamic-space-size $(HEAP_MB) --non-interactive --load
STDIN_GUARD =
endif

# All targets are actions, not files — declare them phony so they always run.
.PHONY: build test integration tui-test clean install install-home

# Compile the standalone evo binary into build/evo.
build:
	$(BUILD_SCRIPT) build.lisp $(STDIN_GUARD)

# Out-of-box install: build the binary, seed $(EVO_HOME) (install-home), then
# drop the binary into $(PREFIX)/bin. Falls back to sudo when the target dir
# isn't writable.
install: build install-home
	@if [ -w $(PREFIX)/bin ] || mkdir -p $(PREFIX)/bin 2>/dev/null && [ -w $(PREFIX)/bin ]; then \
	  install -m 755 build/evo $(PREFIX)/bin/evo; \
	else \
	  echo "Need sudo to write to $(PREFIX)/bin"; \
	  sudo install -m 755 build/evo $(PREFIX)/bin/evo; \
	fi

# Run the unit-test suite.
test:
	$(RUN_SCRIPT) tests/run-unit.lisp $(STDIN_GUARD)

# End-to-end integration tests against a freshly built binary.  The backend is
# configurable via EVO_TEST_BASE_URL / EVO_TEST_API_KEY / EVO_TEST_MODEL; the
# live tests skip cleanly when no backend is reachable.
integration: build
	tests/integration.sh

# Expect-driven TUI tests against a freshly built binary: the general smoke
# test, the same-id/multi-provider model routing test, image paste through a
# real vision model (EVO_TEST_VISION_MODEL), then the IDE bridge (no backend
# needed — it drives the state file the editor plugin writes).
tui-test: build
	tests/tui.exp
	tests/model-provider.exp
	tests/image-paste.exp
	tests/ide-context.exp

# Seed corpus: docs + example extensions into the global evo home.
# Everything installed under docs/ is reference-only — nothing ships active in
# $(EVO_HOME)/extensions (plan mode is a core extension, in the binary).
# Vendored extensions in extensions/ are installed directly into
# $(EVO_HOME)/extensions/ and loaded at startup.
# The sample init.lisp is reference-only too: evo requires a real
# $(EVO_HOME)/init.lisp (no built-in model table) — copy and edit it.
install-home:
	mkdir -p $(EVO_HOME)/extensions $(EVO_HOME)/docs/examples $(EVO_HOME)/skills $(EVO_HOME)/prompts
	cp docs/*.md $(EVO_HOME)/docs/
	cp docs/examples/init.lisp $(EVO_HOME)/docs/examples/
	cp extensions/examples/*.lisp $(EVO_HOME)/docs/examples/
	cp extensions/*.lisp $(EVO_HOME)/extensions/
	rm -f $(EVO_HOME)/extensions/*.fasl

# Remove build artifacts.
clean:
	rm -rf build
