RUNTESTS = run-tests -strip-roots -dispatch-root "$$PWD/tests"

BYTECOMPILE = sh ./tools/compile
INSTALL = sh ./tools/install

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "       install: Install scheme modules to system paths"
	@echo "       compile: Byte-compile the client library"
	@echo "         clean: Remove generated files (compilation, documentation etc)"
	@echo "           doc: Generate documentation in various formats"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

compile:
	$(BYTECOMPILE)

clean:
	@(cd doc && $(MAKE) clean;)
	find . -name "*.go" -exec rm -f '{}' +
	find . -name "*~" -exec rm -f '{}' +

doc:
	@(cd doc && $(MAKE) all;)

install:
	$(INSTALL)

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all compile clean clean doc install test test-debug test-verbose
