TOPDIR = ..
include $(TOPDIR)/common.mk

PROTOCOL = xmms2-protocol
REFERENCE = xmms2-guile

MAN = $(PROTOCOL).7 $(REFERENCE).7
PDF = $(PROTOCOL).pdf $(REFERENCE).pdf
HTML = $(PROTOCOL).html $(REFERENCE).html

REFMAN_SECTIONS = ${MODULES:.scm=.mdwn}
TOOL_SECTIONS = ${TOOLS:=.mdwn}

DOCC = $(GUILE_CALL) $(TOPDIR)/tools/docc -t "$(TOPDIR)"
GENERATE_REFMAN_SECTION = $(DOCC) module -g
GENERATE_TOOL_SECTION = $(DOCC) tool
GENERATE_FINAL_MARKDOWN = $(DOCC) combine

PANDOC = pandoc
PANDOC_FLAGS = --self-contained --number-sections --normalize --smart --toc
MARKDOWN2PDF = $(PANDOC) $(PANDOC_FLAGS) --template template.latex
MARKDOWN2MAN = $(PANDOC) $(PANDOC_FLAGS) -t man --template template.man
MARKDOWN2HTML = $(PANDOC) $(PANDOC_FLAGS) -t html5 --template template.html
MARKDOWN2HTML += -c manual.css --highlight-style=monochrome

all: html pdf man
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

$(PROTOCOL).fmdwn:
$(REFERENCE).fmdwn: $(REFMAN_SECTIONS) $(TOOL_SECTIONS)

clean:
	rm -f *~ xmms2*.fmdwn xmms2*.pdf xmms2*.html xmms2*.7
	find $(TOPDIR)/scheme -name "*.mdwn" -exec rm -f '{}' +

.fmdwn.pdf:
	$(MARKDOWN2PDF) -o $@ $<

.fmdwn.html:
	$(MARKDOWN2HTML) -o $@ $<

.fmdwn.7:
	$(MARKDOWN2MAN) -o $@ $<

.scm.mdwn:
	$(GENERATE_REFMAN_SECTION) -o $@ $<

.mdwn.fmdwn:
	$(GENERATE_FINAL_MARKDOWN) -o $@ $<

$(TOPDIR)/tools/docc.mdwn: $(TOPDIR)/tools/docc
	$(GENERATE_TOOL_SECTION) -o $@ $<
$(TOPDIR)/tools/generate-ipc-from-xml.mdwn: $(TOPDIR)/tools/generate-ipc-from-xml
	$(GENERATE_TOOL_SECTION) -o $@ $<
$(TOPDIR)/tools/guile-in-here.mdwn: $(TOPDIR)/tools/guile-in-here
	$(GENERATE_TOOL_SECTION) -o $@ $<
$(TOPDIR)/tools/install.mdwn: $(TOPDIR)/tools/install
	$(GENERATE_TOOL_SECTION) -o $@ $<
$(TOPDIR)/tools/single-test.mdwn: $(TOPDIR)/tools/single-test
	$(GENERATE_TOOL_SECTION) -o $@ $<

.PHONY: all man html pdf clean
.SUFFIXES: .scm .mdwn .fmdwn .7 .html .pdf
