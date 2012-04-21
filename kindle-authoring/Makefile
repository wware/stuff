KINDLEGEN=/usr/local/kindlegen/kindlegen

.SUFFIXES: .opf .ncx .mobi .rst .xhtml ;

SHORTTITLE=RandomStuff

.opf.mobi:
	$(KINDLEGEN) $(@:.mobi=.opf)

all: $(SHORTTITLE).mobi

CHAPTERS=chapter1 chapter2 chapter3

cover-pic.png:
	python create-cover-art.py > cover-pic.png

$(SHORTTITLE).mobi: make-index.py $(CHAPTERS:=.rst) cover-pic.png
	python make-index.py $(CHAPTERS)

clean:
	rm -f *~ *.mobi *.ncx *.opf *.xhtml cover-pic.png
