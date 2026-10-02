# evo — build & install (Unix: macOS, Linux).
#
# Windows has no make and no /bin/sh: use make.ps1 beside this file, which
# takes the same targets and the same knobs as parameters (SBCL only there).
#
# Configuration variables (override on the command line, e.g. `make LISP=ecl`):
#   LISP     — Common Lisp implementation used to build and run scripts.
#   EVO_HOME — Global evo home seeded by `install-home` (docs, examples, ...).
#   PREFIX   — Install prefix; the binaries land in $(PREFIX)/bin/evo-agent
#              and $(PREFIX)/bin/evo-swarm, with $(PREFIX)/bin/evo a soft
#              link to evo-swarm — the name for the whole product.
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
.PHONY: build test integration tui-test tui-test-offline serve-test swarm-test swarm-serve-test supervisor-test clean install install-home

# Compile both binaries — build/evo-agent (the agent alone) and
# build/evo-swarm (its own system on top of evo — evo-swarm.asd,
# docs/swarm.md) — then point build/evo at the swarm binary: `evo` is the
# product's name, evo-swarm the program.  A soft link here, a copy on Windows
# (make.ps1), which has no dependable symlink to build with.
build:
	$(BUILD_SCRIPT) build.lisp $(STDIN_GUARD)
	$(BUILD_SCRIPT) build-swarm.lisp $(STDIN_GUARD)
	ln -sfn evo-swarm build/evo

# Out-of-box install: build both binaries, seed $(EVO_HOME) (install-home),
# then drop evo-agent and evo-swarm into $(PREFIX)/bin and link evo to
# evo-swarm (replacing any older evo there — the link is the upgrade).  Falls
# back to sudo when the target dir isn't writable.
install: build install-home
	@if [ -w $(PREFIX)/bin ] || mkdir -p $(PREFIX)/bin 2>/dev/null && [ -w $(PREFIX)/bin ]; then \
	  install -m 755 build/evo-agent $(PREFIX)/bin/evo-agent; \
	  install -m 755 build/evo-swarm $(PREFIX)/bin/evo-swarm; \
	  ln -sfn evo-swarm $(PREFIX)/bin/evo; \
	else \
	  echo "Need sudo to write to $(PREFIX)/bin"; \
	  sudo install -m 755 build/evo-agent $(PREFIX)/bin/evo-agent; \
	  sudo install -m 755 build/evo-swarm $(PREFIX)/bin/evo-swarm; \
	  sudo ln -sfn evo-swarm $(PREFIX)/bin/evo; \
	fi

# Run the unit-test suites — after proving the core loads on its own, without
# the TUI or the CLI (tests/core-only.lisp), and evo without the swarm
# (tests/evo-only.lisp): each layer builds on the one below, never the other
# way round.  Then evo's suite, then the swarm's.
#
# Every runner clears the running session's own EVO_* variables first, so the
# result does not depend on what started it (tests/env.lisp): the same counts
# come out of a shell, out of CI, and out of an evo-swarm lane.
test:
	$(RUN_SCRIPT) tests/core-only.lisp $(STDIN_GUARD)
	$(RUN_SCRIPT) tests/evo-only.lisp $(STDIN_GUARD)
	$(RUN_SCRIPT) tests/run-unit.lisp $(STDIN_GUARD)
	$(RUN_SCRIPT) swarm/tests/run-unit.lisp $(STDIN_GUARD)

# End-to-end integration tests against a freshly built binary.  The backend is
# configurable via EVO_TEST_BASE_URL / EVO_TEST_API_KEY / EVO_TEST_MODEL; the
# live tests skip cleanly when no backend is reachable.
integration: build
	tests/integration.sh

# Expect-driven TUI tests against a freshly built binary: the general smoke
# test, the same-id/multi-provider model routing test, image paste through a
# real vision model (EVO_TEST_VISION_MODEL), then six that need no backend at
# all — pasting in every shape a terminal sends it, the IDE bridge (which
# drives the state file the editor plugin writes), background jobs on the
# status line, the activity clock restarting on every step, switching the
# system prompt's language, and the MCP client against a stub server
# (tests/mcp-server.py, needs python3).
tui-test: build
	tests/tui.exp
	tests/interrupt.exp
	tests/model-provider.exp
	tests/image-paste.exp
	tests/paste.exp
	tests/ide-context.exp
	tests/jobs.exp
	tests/step-clock.exp
	tests/lang.exp
	tests/mcp.exp

# The subset of the pty tests that need no backend at all: each registers a
# model so the TUI starts but never sends anything to it.  Unlike the rest of
# tui-test these run in CI (.github/workflows/ci.yml) — no live model, just
# expect driving a real pty.  Requires expect and a built binary.
tui-test-offline: build
	tests/paste.exp
	tests/ide-context.exp
	tests/jobs.exp
	tests/step-clock.exp
	tests/lang.exp
	tests/mcp.exp

# `evo-agent serve` end to end, over HTTP only and with no backend: a stub
# Messages endpoint (tests/stub-messages.py) stands in for the model, which
# the test registers through POST /eval like any coordinator would.  Needs
# python3 and a built binary; runs in CI (.github/workflows/ci.yml).
serve-test: build
	tests/serve-e2e.py build/evo-agent

# evo-swarm end to end, with no backend: the coordinator driven through a
# pseudo-terminal, the stub Messages endpoint scripting both it and the lanes
# (tests/swarm-e2e.py; python3 and git).  Runs in CI too.
swarm-test: build
	tests/swarm-e2e.py build

# evo-swarm serve end to end, over HTTP only and with no backend: the headless
# swarm driven through serve's protocol plus the swarm feature, the stub
# scripting both the coordinator and its lanes (tests/swarm-serve-e2e.py;
# python3).  Needs the evo-agent and evo-swarm binaries.
swarm-serve-test: build
	tests/swarm-serve-e2e.py build

# The supervisor, live: a supervised server comes back on its exact session
# and port (S3/S4), an idle one comes back on the session it never wrote to
# disk (F4), and the token a client holds survives the restart (T1).  Both
# binaries (tests/supervisor-e2e.py; python3).
supervisor-test: build
	tests/supervisor-e2e.py build/evo-agent

# Seed corpus: docs + example extensions into the global evo home.
# Everything installed under docs/ is reference-only — nothing ships active in
# $(EVO_HOME)/extensions (core extensions ship inside the binary).
# Vendored extensions in extensions/ are installed directly into
# $(EVO_HOME)/extensions/ and loaded at startup.
# The sample init.lisp is reference-only too: evo requires a real
# $(EVO_HOME)/init.lisp (no built-in model table) — copy and edit it.
install-home:
	mkdir -p $(EVO_HOME)/extensions $(EVO_HOME)/docs/examples $(EVO_HOME)/skills $(EVO_HOME)/prompts
	cp docs/*.md $(EVO_HOME)/docs/
	cp docs/examples/init.lisp docs/examples/swarm.lisp $(EVO_HOME)/docs/examples/
	cp extensions/examples/*.lisp $(EVO_HOME)/docs/examples/
	cp extensions/*.lisp $(EVO_HOME)/extensions/
	rm -f $(EVO_HOME)/extensions/*.fasl

# Remove build artifacts.
clean:
	rm -rf build
