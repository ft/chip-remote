RUNTESTS = run-tests -strip-roots -dispatch-root "$$PWD/tests"

BYTECOMPILE = sh ./compile
INSTALL = sh ./install
ORG_EXPORT = sh ./org-export

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "       install: Install scheme modules to system paths"
	@echo "  byte-compile: Byte-compile the client library"
	@echo "    byte-clean: Remove byte-compiled scheme code"
	@echo "  fw-simulator: Build simulator version of the remote firmware"
	@echo "          spec: Generate various formats of the protocol spec"
	@echo "    spec-clean: Remove spec versions generated by the spec target"
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

byte-clean:
	find scheme -name '*.go' -exec rm '{}' +

install:
	$(INSTALL)

spec: rccep-spec.pdf rccep-spec.html rccep-spec.txt

spec-clean:
	rm -f rccep-spec.tex rccep-spec.pdf rccep-spec.html rccep-spec.txt

rccep-spec.pdf: rccep-spec.org
	$(ORG_EXPORT) pdf $< $@

rccep-spec.html: rccep-spec.org
	$(ORG_EXPORT) html $< $@

rccep-spec.txt: rccep-spec.org
	$(ORG_EXPORT) txt $< $@

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all byte-compile byte-clean fw-simulator install spec spec-clean test test-debug test-verbose
