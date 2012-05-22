KINDLEGEN=/usr/local/kindlegen/kindlegen

.SUFFIXES: .opf .ncx .mobi .rst .xhtml ;

SHORTTITLE=RandomStuff

.opf.mobi:
	$(KINDLEGEN) $(@:.mobi=.opf)

all: $(SHORTTITLE).mobi SSE.mobi

CHAPTERS=chapter1 chapter2 chapter3

cover-pic.png:
	python create-cover-art.py > cover-pic.png

$(SHORTTITLE).mobi: makeindex.py $(CHAPTERS:=.rst) cover-pic.png
	python makeindex.py $(CHAPTERS)

SSE.mobi: hack.py cover-pic.png
	python hack.py

clean:
	rm -f *~ *.mobi *.ncx *.opf *.pyc cover-pic.png
	rm -f chapter*.xhtml index.xhtml
