# Makefile for NanoCAD Java Prototype
# Copyright (c) 1998 Will Ware, all rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and other materials provided with the distribution.
#
# This software is provided "as is" and any express or implied warranties,
# including, but not limited to, the implied warranties of merchantability
# or fitness for any particular purpose are disclaimed. In no event shall
# Will Ware be liable for any direct, indirect, incidental, special,
# exemplary, or consequential damages (including, but not limited to,
# procurement of substitute goods or services; loss of use, data, or
# profits; or business interruption) however caused and on any theory of
# liability, whether in contract, strict liability, or tort (including
# negligence or otherwise) arising in any way out of the use of this
# software, even if advised of the possibility of such damage.

.SUFFIXES: .java .class

SOURCES = \
	aspirin.java \
	aterm.java \
	atom.java \
	bond.java \
	buckyball.java \
	carbon.java \
	diamond.java \
	diamrod.java \
	dl_atom.java \
	dl_bond.java \
	dlentry.java \
	dlforce.java \
	group.java \
	hydrogen.java \
	lrterm.java \
	lterm.java \
	nanocad.java \
	nitrogen.java \
	oxygen.java \
	propane.java \
	term.java \
	textwin.java \
	tterm.java \
	tworings.java \
	view.java \
	water.java \
	wrapper.java

CLASSES = $(SOURCES:.java=.class)

TGZFILE=ncadjava.tgz
ZIPFILE=ncadjava.zip
TO_TGZ=$(SOURCES) $(CLASSES) Makefile

all: $(CLASSES) nc.tar

classes: $(CLASSES)

.java.class:
	javac $(@:.class=.java)

ncj_cl.zip: $(CLASSES)
	zip -0 ncj_cl.zip $(CLASSES)

tgz: $(TGZFILE)

$(TGZFILE): $(TO_TGZ)
	rm -f $(TGZFILE)
	tar -cf- $(TO_TGZ) | gzip > $(TGZFILE)

zip: $(ZIPFILE)

$(ZIPFILE): $(TO_TGZ)
	rm -f $(ZIPFILE)
	zip $(ZIPFILE) $(TO_TGZ)

nc.tar: $(TGZFILE) $(ZIPFILE) ncj_cl.zip
	rm -f nc.tar
	tar cf nc.tar $(TGZFILE) $(ZIPFILE) ncj_cl.zip
