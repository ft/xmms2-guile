MAN = xmms2-protocol.7
PDF = xmms2-protocol.pdf
HTML = xmms2-protocol.html

COMMONOPTIONS = --self-contained --number-sections --normalize --smart --toc

TEX_META = -V --highlight-style=monochrome
HTML_META = -c manual.css

SCMTAP_HTML_META = $(SCMTAP_TEX_META) --highlight-style=monochrome
SCMTAP_MAN_META = -V title="xmms2-protocol" -V section="7" -V date="Feb, 2015"

# Manual page output currently disabled.
all: html pdf
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

xmms2-protocol.7: xmms2-protocol.mdwn
	pandoc -t man $(COMMONOPTIONS) $(SCMTAP_MAN_META) -o $@ xmms2-protocol.mdwn
xmms2-protocol.pdf: xmms2-protocol.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(SCMTAP_TEX_META) -o $@ xmms2-protocol.mdwn
xmms2-protocol.html: xmms2-protocol.mdwn
	pandoc --template template.html -t html5 $(HTML_META) $(COMMONOPTIONS) $(SCMTAP_HTML_META) -o $@ xmms2-protocol.mdwn

clean:
	rm -f $(HTML) $(PDF) $(MAN) *~

.PHONY: all man html pdf clean
