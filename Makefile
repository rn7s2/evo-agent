SBCL ?= sbcl
EVO_HOME ?= $(HOME)/.evo
PREFIX ?= /usr/local
# Heap for the built binary — baked in via :save-runtime-options (D10).
HEAP_MB ?= 4096

.PHONY: build test integration tui-test clean install install-home

build:
	$(SBCL) --dynamic-space-size $(HEAP_MB) --non-interactive --load build.lisp

install: build
	install -m 755 build/evo $(PREFIX)/bin/evo

test:
	$(SBCL) --non-interactive --load tests/run-unit.lisp

integration: build
	tests/integration.sh

tui-test: build
	tests/tui.exp

# Seed corpus (§10.4): docs + example extensions into the global evo home.
# plan-mode is installed active; the other examples stay reference-only.
install-home:
	mkdir -p $(EVO_HOME)/extensions $(EVO_HOME)/docs/examples $(EVO_HOME)/skills $(EVO_HOME)/prompts
	cp docs/*.md $(EVO_HOME)/docs/
	cp extensions/examples/*.lisp $(EVO_HOME)/docs/examples/
	cp extensions/plan-mode.lisp $(EVO_HOME)/extensions/

clean:
	rm -rf build
