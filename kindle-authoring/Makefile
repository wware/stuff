KINDLEGEN=/usr/local/kindlegen/kindlegen

.SUFFIXES: .opf .ncx .mobi ;

.opf.mobi:
	$(KINDLEGEN) $(@:.mobi=.opf)

all: wills-ubuntu-notes.mobi wills-blog.mobi

wills-blog.mobi: \
	wills-blog.ncx \
	wills-blog.xhtml \
	wills-blog-cover.png

clean:
	rm -f *~ *.mobi
