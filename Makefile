TOPDIR = .
include $(TOPDIR)/common.mk

GENERATE_IPC = $(GUILE_CALL) $(TOPDIR)/tools/generate-ipc-from-xml
XMMS2_IPC_XML = /usr/src/xmms2/src/ipc.xml
RUNTESTS = SCHEME_INTERPRETER="$(GUILE_BINARY)" run-tests
RUNTESTS += -strip-roots -dispatch-root "$(TEST_PATH)"
#INSTALL = $(GUILE_CALL) $(TOPDIR)/tools/install

INSTALL = sh ./tools/install
ORG_EXPORT = sh ./tools/org-export

CFLAGS = -Wunsupported-warning
# These results in lots of warnings from record-type definitions, so I'm
# turning them off for now:
#CFLAGS += -Wunused-variable -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH)

NATIVE_DIR = native-fw
NATIVE_COMPILE = cmake -GNinja -DUFW_PICK_ZEPHYR=chip-remote-native ../firmware

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
	mkdir -p $(NATIVE_DIR)
	(cd $(NATIVE_DIR) && $(NATIVE_COMPILE))
	(cd $(NATIVE_DIR) && ninja && ninja test)

clean:
	find . -name "*.go" -exec rm -f '{}' +
	find . -name "*~" -exec rm -f '{}' +
	find . -name "*.failure" -exec rm -f '{}' +
	rm -Rf $(NATIVE_DIR)

doc:
	(cd doc/ && $(MAKE) all;)

install:
	$(INSTALL)

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all compile clean doc install native-fw test test-debug test-verbose
