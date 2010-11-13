DEBNAME=$(shell grep '^Package: ' debian/control | cut -c 10-)
VERSION=$(shell grep '^Version: ' debian/control | cut -c 10-)
ARCH=$(shell grep '^Architecture: ' debian/control | cut -c 15-)

PACKAGE=$(DEBNAME)_$(VERSION)_$(ARCH)
PKGNAME=$(PACKAGE).deb
PKGDIR=/tmp/$(PACKAGE)

SRCPACKAGE=$(DEBNAME)_$(VERSION)_src
SRCPKGNAME=$(SRCPACKAGE).deb
SRCPKGDIR=/tmp/$(SRCPACKAGE)

PYDVER := $(shell python -c "import sys; print sys.version[:3]")
PYLIBDIR = $(PYBASE)/lib/python$(PYDVER)
UNAME:=$(shell uname)
PYBASE := $(shell which python | sed "s%/bin/python%%")
CFLAGS += -I$(PYBASE)/include/python$(PYDVER)
LDFLAGS = -L$(PYLIBDIR)/config -lm -lpython$(PYDVER)
LDSHARED = $(CC) -shared

all: $(PKGNAME)

lintian: $(SRCPKGNAME) $(PKGNAME)
	lintian -iIv --pedantic $(PKGNAME)
	lintian -iIv --pedantic $(SRCPKGNAME)

pkgdir:
	mkdir -p $(PKGDIR)/DEBIAN/ \
		$(PKGDIR)/$(PYLIBDIR)/dist-packages \
		$(PKGDIR)/usr/share/doc/$(DEBNAME)/
	cp -r usr $(PKGDIR)
	cp python/* $(PKGDIR)/$(PYLIBDIR)/dist-packages
	for x in copyright changelog; do \
		cp debian/$$x $(PKGDIR)/usr/share/doc/$(DEBNAME)/; \
	done
	touch pkgdir

inst-size: pkgdir
	dc -e "$$(wc $$(find $(PKGDIR) -type f) | tail -1 | \
		cut -c 12-17 | sed 's/ //g') 1023 + 1024 / p"

$(PKGNAME): pkgdir
	gzip --best $(PKGDIR)/usr/share/doc/$(DEBNAME)/changelog
	(cd $(PKGDIR) ; md5sum $$(find . -type f)) > debian/md5sums
	for x in control md5sums; do \
		cp debian/$$x $(PKGDIR)/DEBIAN; done
	fakeroot dpkg-deb --build $(PKGDIR)
	mv /tmp/$(PKGNAME) .

$(SRCPKGNAME):
	rm -rf $(SRCPKGDIR)/
	mkdir -p $(SRCPKGDIR)/DEBIAN/ \
		$(SRCPKGDIR)/usr/src/$(DEBNAME)/ \
		$(SRCPKGDIR)/usr/share/doc/$(DEBNAME)/
	cp -r . $(SRCPKGDIR)/usr/src/$(DEBNAME)/
	cp debian/copyright $(SRCPKGDIR)/usr/share/doc/$(DEBNAME)/
	cp debian/changelog $(SRCPKGDIR)/usr/share/doc/$(DEBNAME)/
	gzip --best $(SRCPKGDIR)/usr/share/doc/$(DEBNAME)/changelog
	(cd $(SRCPKGDIR) ; md5sum $$(find . -type f)) > debian/md5sums
	for x in control md5sums; do \
		cp debian/$$x $(SRCPKGDIR)/DEBIAN/; \
	done
	fakeroot dpkg-deb --build $(SRCPKGDIR)
	mv /tmp/$(SRCPKGNAME) .

clean:
	rm -rf $(PKGDIR)/ $(PKGNAME)
	rm -rf $(SRCPKGDIR)/ $(SRCPKGNAME)
	rm -f debian/md5sums pkgdir
