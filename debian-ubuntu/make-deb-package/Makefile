DEBNAME=$(shell grep '^Package: ' debian/control | cut -c 10-)
VERSION=$(shell grep '^Version: ' debian/control | cut -c 10-)
ARCH=$(shell grep '^Architecture: ' debian/control | cut -c 15-)
SRCNAME=$(DEBNAME)_$(VERSION)_src
SRCDEB=$(SRCNAME).deb
SRCDIR=/tmp/$(SRCNAME)
BINNAME=$(DEBNAME)_$(VERSION)_$(ARCH)
BINDEB=$(BINNAME).deb
BINDIR=/tmp/$(BINNAME)

PYDVER := $(shell python -c "import sys; print sys.version[:3]")
PYLIBDIR = $(PYBASE)/lib/python$(PYDVER)
UNAME:=$(shell uname)
PYBASE := $(shell which python | sed "s%/bin/python%%")
CFLAGS += -I$(PYBASE)/include/python$(PYDVER)
LDFLAGS = -L$(PYLIBDIR)/config -lm -lpython$(PYDVER)
LDSHARED = $(CC) -shared

all: $(SRCDEB) $(BINDEB)

srcdeb: $(SRCDEB)

bindeb: $(BINDEB)

$(BINDIR)/usr/local/bin/say-hello: src/say-hello.c
	mkdir -p $(BINDIR)/usr/local/bin
	gcc $(CFLAGS) $(LDFLAGS) \
		-o $(BINDIR)/usr/local/bin/say-hello src/say-hello.c
	strip --remove-section=.comment --remove-section=.note \
		$(BINDIR)/usr/local/bin/say-hello

lintian: $(SRCDEB) $(BINDEB)
	lintian -iIv --pedantic $(SRCDEB)
	lintian -iIv --pedantic $(BINDEB)

$(BINDEB): $(BINDIR)/usr/local/bin/say-hello
	rm -rf $(BINDIR)/
	mkdir -p $(BINDIR)/DEBIAN/ \
		$(BINDIR)/$(PYLIBDIR)/dist-packages \
		$(BINDIR)/usr/share/doc/$(DEBNAME)/
	cp -r usr $(BINDIR)
	cp python/* $(BINDIR)/$(PYLIBDIR)/dist-packages
	cp debian/copyright $(BINDIR)/usr/share/doc/$(DEBNAME)/
	cp debian/changelog $(BINDIR)/usr/share/doc/$(DEBNAME)/
	gzip --best $(BINDIR)/usr/share/doc/$(DEBNAME)/changelog
	(cd $(BINDIR) ; md5sum $$(find . -type f)) > debian/md5sums
	for x in control md5sums postinst prerm; do \
		cp debian/$$x $(BINDIR)/DEBIAN; done
	fakeroot dpkg-deb --build $(BINDIR)
	mv /tmp/$(BINDEB) .

$(SRCDEB):
	rm -rf $(SRCDIR)/
	mkdir -p $(SRCDIR)/DEBIAN/ \
		$(SRCDIR)/usr/src/$(DEBNAME)/ \
		$(SRCDIR)/usr/share/doc/$(DEBNAME)/
	cp -r . $(SRCDIR)/usr/src/$(DEBNAME)/
	cp debian/copyright $(SRCDIR)/usr/share/doc/$(DEBNAME)/
	cp debian/changelog $(SRCDIR)/usr/share/doc/$(DEBNAME)/
	gzip --best $(SRCDIR)/usr/share/doc/$(DEBNAME)/changelog
	(cd $(SRCDIR) ; md5sum $$(find . -type f)) > debian/md5sums
	for x in control md5sums; do \
		cp debian/$$x $(SRCDIR)/DEBIAN; done
	fakeroot dpkg-deb --build $(SRCDIR)
	mv /tmp/$(SRCDEB) .

clean:
	rm -rf $(SRCDIR)/ $(BINDIR)/
	rm -f debian/md5sums
	rm -f $(SRCDEB) $(BINDEB)
