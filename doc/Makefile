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

all: html pdf man
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

xmms2-protocol.7: xmms2-protocol.mdwn
	pandoc -t man $(COMMONOPTIONS) $(XMMS2PROTO_MAN_META) -o $@ xmms2-protocol.mdwn
xmms2-protocol.pdf: xmms2-protocol.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(XMMS2PROTO_TEX_META) -o $@ xmms2-protocol.mdwn
xmms2-protocol.html: xmms2-protocol.mdwn
	pandoc --template template.html -t html5 $(HTML_META) $(COMMONOPTIONS) $(XMMS2PROTO_HTML_META) -o $@ xmms2-protocol.mdwn

xmms2-guile.7: xmms2-guile.mdwn
	pandoc -t man $(COMMONOPTIONS) $(XMMS2GUILE_MAN_META) -o $@ xmms2-guile.mdwn
xmms2-guile.pdf: xmms2-guile.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(XMMS2GUILE_TEX_META) -o $@ xmms2-guile.mdwn
xmms2-guile.html: xmms2-guile.mdwn
	pandoc --template template.html -t html5 $(HTML_META) $(COMMONOPTIONS) $(XMMS2GUILE_HTML_META) -o $@ xmms2-guile.mdwn

clean:
	rm -f $(HTML) $(PDF) $(MAN) *~

.PHONY: all man html pdf clean
