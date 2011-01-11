## Makefile.am
##
## Copyright (C) 2007, 2008, 2009, 2010 Thien-Thi Nguyen
## Copyright (C) 2001, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
##
## This file is part of Guile-WWW.
##
## Guile-WWW is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3, or
## (at your option) any later version.
##
## Guile-WWW is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with Guile-WWW; see the file COPYING.  If not,
## write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA  02110-1301  USA

info_TEXINFOS = guile-www.texi
guile_www_TEXINFOS = version.texi

BUILT_SOURCES = doc-index-prep
doc-index-prep:
if MAINTAINER_MODE
	for d in . server-utils data ; do		\
	  ( cd ../source/$$d ; $(MAKE) .doc-index ) ; done

guile-www.texi: guile-www.twerp

twerp2texi = guile-tools twerp2texi

.twerp.texi:
	$(twerp2texi) --scheme-implied		\
	  -I ../source/.doc-index		\
	  -I ../source/server-utils/.doc-index	\
	  -I ../source/data/.doc-index		\
	  -d .deps/$*.Ptexi			\
	  -o $@ $<
endif ## MAINTAINER_MODE

CLEANFILES = guile-www.info guile-www.dvi guile-www_toc.html guile-www.html

EXTRA_DIST = guile-www.twerp guile-www.html guile-www.pdf

## Makefile.am ends here
