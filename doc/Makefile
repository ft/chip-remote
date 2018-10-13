TOPDIR = ..
include $(TOPDIR)/common.mk

PROTOCOL = rccep-spec
CLIENTLIB = client-library
REFERENCE = chip-remote

MAN = $(REFERENCE).7
PDF = $(PROTOCOL).pdf $(REFERENCE).pdf $(CLIENTLIB).pdf

REFMAN_SECTIONS = ${MODULES:.scm=.mdwn}
TOOL_SECTIONS = ${TOOLS:=.mdwn}

DOCC = $(GUILE_CALL) $(TOPDIR)/tools/docc -t "$(TOPDIR)"
GENERATE_REFMAN_SECTION = $(DOCC) module -g
GENERATE_TOOL_SECTION = $(DOCC) tool
GENERATE_FINAL_MARKDOWN = $(DOCC) combine

PANDOC = pandoc
PANDOC_FLAGS = --self-contained --number-sections --toc
MARKDOWN2PDF = $(PANDOC) $(PANDOC_FLAGS) --template template.latex
MARKDOWN2MAN = $(PANDOC) $(PANDOC_FLAGS) -t man --template template.man

ORGEXPORT = sh $(TOPDIR)/tools/org-export
ORG2PDF = $(ORGEXPORT) pdf

all: pdf man
man: $(MAN)
pdf: $(PDF)

$(REFERENCE).fmdwn: $(REFMAN_SECTIONS) $(TOOL_SECTIONS)

.org.pdf:
	$(ORG2PDF) $< $@

.fmdwn.pdf:
	$(MARKDOWN2PDF) -o $@ $<

.fmdwn.7:
	$(MARKDOWN2MAN) -o $@ $<

.scm.mdwn:
	$(GENERATE_REFMAN_SECTION) -o $@ $<

.mdwn.fmdwn:
	$(GENERATE_FINAL_MARKDOWN) -o $@ $<

$(TOPDIR)/tools/docc.mdwn: $(TOPDIR)/tools/docc
	$(GENERATE_TOOL_SECTION) -o $@ $<
$(TOPDIR)/tools/guile-in-here.mdwn: $(TOPDIR)/tools/guile-in-here
	$(GENERATE_TOOL_SECTION) -o $@ $<
$(TOPDIR)/tools/install.mdwn: $(TOPDIR)/tools/install
	$(GENERATE_TOOL_SECTION) -o $@ $<
$(TOPDIR)/tools/single-test.mdwn: $(TOPDIR)/tools/single-test
	$(GENERATE_TOOL_SECTION) -o $@ $<

.PHONY: all man pdf clean
.SUFFIXES: .scm .mdwn .fmdwn .7 .org .pdf
