RUNTESTS = run-tests -strip-roots -dispatch-root "$$PWD/tests"

BYTECOMPILE = sh ./compile

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "  byte-compile: Byte-compile the client library"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

byte-compile:
	$(BYTECOMPILE)

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all byte-compile test test-debug test-verbose
