SBCL ?= sbcl

.PHONY: build test integration clean

build:
	$(SBCL) --non-interactive --load build.lisp
	chmod +x bin/evo

test:
	$(SBCL) --non-interactive --load tests/run-unit.lisp

integration: build
	tests/integration.sh

clean:
	rm -rf build
