TOPDIR = .
include $(TOPDIR)/common.mk

RUNTESTS = ./tools/preinst run-tests
RUNTESTS += -strip-roots -dispatch-root "$(TEST_PATH)"

INSTALL = sh ./tools/install
ORG_EXPORT = sh ./tools/org-export

CFLAGS = -O3 -Wunsupported-warning
# These result in lots of warnings from record-type definitions, so I'm
# turning them off for now:
#CFLAGS += -Wunused-variable -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH)

NATIVE_DIR = native-fw

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
	@echo "         clean: Remove byte-compiled scheme code"
	@echo "     native-fw: Build native variant of the remote firmware"
	@echo "          spec: Generate various formats of the protocol spec"
	@echo "    spec-clean: Remove spec versions generated by the spec target"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

.scm.go:
	$(COMPILE) -o $@ $<

compile: $(OBJECTS)

native-fw:
	(cd firmware && mmh -l -P -d ../native-fw system -s build zephyr/native-sim/chip-remote/host/debug)

happiness: native-fw compile doc strict-test

clean-go:
	find scheme -name "*.go"      -exec rm -f '{}' +

clean: clean-go
	find scheme -name "*.mdwn"    -exec rm -f '{}' +
	find .      -name "*~"        -exec rm -f '{}' +
	find .      -name "*.failure" -exec rm -f '{}' +
	$(MAKE) -C doc clean
	rm -Rf $(NATIVE_DIR)

recompile: clean-go compile

doc:
	(cd doc/ && $(MAKE) all;)

install:
	$(INSTALL)

strict-test: $(OBJECTS)
	$(MAKE) test

test:
	$(RUNTESTS)

strict-test-verbose: $(OBJECTS)
	$(MAKE) test-verbose

test-verbose:
	$(RUNTESTS) -verbose

strict-test-debug: $(OBJECTS)
	$(MAKE) test-debug

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

standalone:
	@$(TOPDIR)/tools/make-standalone "$(TOPDIR)"

.PHONY: all
.PHONY: compile clean clean-go recompile
.PHONY: doc
.PHONY: happiness
.PHONY: install
.PHONY: native-fw
.PHONY: standalone
.PHONY: test test-debug test-verbose
.PHONY: strict-test strict-test-debug strict-test-verbose
