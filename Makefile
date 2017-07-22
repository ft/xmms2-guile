TOPDIR = .
include $(TOPDIR)/common.mk

SH = /bin/sh
GENERATE_IPC = $(GUILE_CALL) $(TOPDIR)/tools/generate-ipc-from-xml
XMMS2_IPC_XML = /usr/src/xmms2/src/ipc.xml
RUNTESTS = SCHEME_INTERPRETER="$(GUILE_BINARY)" run-tests
RUNTESTS += -strip-roots -dispatch-root "$(TEST_PATH)"
INSTALL = $(SH) ./tools/install

CFLAGS = -Wunsupported-warning -Wunused-variable -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH)

COMPILE = $(GUILD_BINARY) compile $(CFLAGS)

OBJECTS = ${MODULES:.scm=.go}

.SUFFIXES: .scm .go

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "       install: Install scheme modules to system paths"
	@echo "       compile: Byte-compile the client library"
	@echo "           ipc: Generate IPC code from XMMS2's ipc.xml file (XMMS2_IPC_XML)"
	@echo "         clean: Remove generated files (compilation, documentation etc)"
	@echo "           doc: Generate documentation in various formats"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo
	@echo "  Makefile Parameters:"
	@echo
	@echo "    XMMS2_IPC_XML = $(XMMS2_IPC_XML)"
	@echo "     GUILE_BINARY = $(GUILE_BINARY)"
	@echo "     GUILD_BINARY = $(GUILD_BINARY)"
	@echo

.scm.go:
	$(COMPILE) -o $@ $<

compile: $(OBJECTS)

clean:
	@(cd doc && $(MAKE) clean;)
	find . -name "*.go" -exec rm -f '{}' +
	find . -name "*~" -exec rm -f '{}' +
	find . -name "*.failure" -exec rm -f '{}' +

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

ipc:
	$(GENERATE_IPC) "$(XMMS2_IPC_XML)"

.PHONY: all compile clean clean doc ipc install test test-debug test-verbose
