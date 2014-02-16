RUNTESTS = run-tests -strip-roots -dispatch-root "$$PWD/tests"

BYTECOMPILE = sh ./compile

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "  byte-compile: Byte-compile the client library"
	@echo "  fw-simulator: Build simulator version of the remote firmware"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

fw-simulator:
	(if [ -e remote-fw/Makefile ]; then cd remote-fw && $(MAKE) distclean; else true; fi)
	(cd remote-fw && ./configure sim stdout -debug;)
	(cd remote-fw && $(MAKE);)

byte-compile:
	$(BYTECOMPILE)

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all byte-compile fw-simulator test test-debug test-verbose
