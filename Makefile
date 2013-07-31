RUNTESTS = run-tests -strip-roots -dispatch-root "$$PWD/tests"

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all test test-debug test-verbose
