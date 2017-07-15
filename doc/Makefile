TOPDIR = ..
include ../modules.mk

REFMAN_SECTIONS = ${MODULES:.scm=.mdwn}

.SUFFIXES: .scm .mdwn .fmdwn .7 .html .pdf

GENERATE_REFMAN_SECTION = TOPDIR="$(TOPDIR)" $(TOPDIR)/tools/docc module -g
GENERATE_FINAL_MARKDOWN = TOPDIR="$(TOPDIR)" $(TOPDIR)/tools/docc combine

PANDOC = pandoc
PANDOC_FLAGS = --self-contained --number-sections --normalize --smart --toc
MARKDOWN2HTML = $(PANDOC) $(PANDOC_FLAGS) --highlight-style=monochrome
MARKDOWN2HTML += -c manual.css -t html5 --template template.html
MARKDOWN2MAN = $(PANDOC) $(PANDOC_FLAGS) --highlight-style=monochrome
MARKDOWN2MAN += -c manual.css -t man
MARKDOWN2PDF = $(PANDOC) $(PANDOC_FLAGS) --highlight-style=monochrome
MARKDOWN2PDF += -c manual.css --template template.latex

MAN = xmms2-protocol.7 xmms2-guile.7
PDF = xmms2-protocol.pdf xmms2-guile.pdf
HTML = xmms2-protocol.html xmms2-guile.html

all: html pdf man
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

xmms2-guile.fmdwn: $(REFMAN_SECTIONS)

clean:
	rm -f *~ *.fmdwn *.pdf *.html *.7
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

.PHONY: all man html pdf clean
