KINDLEGEN=/usr/local/kindlegen/kindlegen

.SUFFIXES: .opf .ncx .mobi .rst .xhtml ;

.opf.mobi:
	$(KINDLEGEN) $(@:.mobi=.opf)

all: foo.mobi

CHAPTERS=chapter1 chapter2 chapter3

foo.mobi: make-index.py $(CHAPTERS:=.rst) cover-pic.png
	python make-index.py $(CHAPTERS)

clean:
	rm -f *~ *.mobi *.ncx *.opf *.xhtml
