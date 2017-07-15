TOPDIR = ..
include ../modules.mk

GENERATE_REFMAN_SECTION = TOPDIR="$(TOPDIR)" $(TOPDIR)/tools/docc module -g
GENERATE_FINAL_MARKDOWN = TOPDIR="$(TOPDIR)" $(TOPDIR)/tools/docc combine

REFMAN_MODULES = $(MODULES_CORE) $(MODULES_GENERATED_CONSTANTS)
REFMAN_MODULES += $(MODULES_GENERATED_IPC)

REFMAN_SECTIONS = ${REFMAN_MODULES:.scm=.mdwn}

.SUFFIXES: .scm .mdwn .fmdwn

MAN = xmms2-protocol.7 xmms2-guile.7
PDF = xmms2-protocol.pdf xmms2-guile.pdf
HTML = xmms2-protocol.html xmms2-guile.html

COMMONOPTIONS = --self-contained --number-sections --normalize --smart --toc

TEX_META = -V --highlight-style=monochrome
HTML_META = -c manual.css

XMMS2PROTO_HTML_META = $(XMMS2PROTO_TEX_META) --highlight-style=monochrome
XMMS2PROTO_MAN_META = -V title="xmms2-protocol" -V section="7" -V date="Jan, 2017"

XMMS2GUILE_HTML_META = $(XMMS2GUILE_TEX_META) --highlight-style=monochrome
XMMS2GUILE_MAN_META = -V title="xmms2-guile" -V section="7" -V date="Jan, 2017"

all: refmansecs html pdf man
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

xmms2-protocol.7: xmms2-protocol.mdwn
	pandoc -t man $(COMMONOPTIONS) $(XMMS2PROTO_MAN_META) -o $@ xmms2-protocol.mdwn
xmms2-protocol.pdf: xmms2-protocol.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(XMMS2PROTO_TEX_META) -o $@ xmms2-protocol.mdwn
xmms2-protocol.html: xmms2-protocol.mdwn
	pandoc --template template.html -t html5 $(HTML_META) $(COMMONOPTIONS) $(XMMS2PROTO_HTML_META) -o $@ xmms2-protocol.mdwn

xmms2-guile.7: xmms2-guile.fmdwn
	pandoc -t man $(COMMONOPTIONS) $(XMMS2GUILE_MAN_META) -o $@ xmms2-guile.fmdwn
xmms2-guile.pdf: xmms2-guile.fmdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(XMMS2GUILE_TEX_META) -o $@ xmms2-guile.fmdwn
xmms2-guile.html: xmms2-guile.fmdwn
	pandoc --template template.html -t html5 $(HTML_META) $(COMMONOPTIONS) $(XMMS2GUILE_HTML_META) -o $@ xmms2-guile.fmdwn

clean:
	rm -f $(HTML) $(PDF) $(MAN) *~
	find $(TOPDIR)/scheme -name "*.mdwn" -exec rm -f '{}' +

refmansecs: $(REFMAN_SECTIONS)

.scm.mdwn:
	$(GENERATE_REFMAN_SECTION) -o $@ $<

.mdwn.fmdwn:
	$(GENERATE_FINAL_MARKDOWN) -o $@ $<

.PHONY: all man html pdf clean refmansecs
