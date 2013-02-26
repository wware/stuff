## Makefile.am
##
## Copyright (C) 2008, 2009, 2010 Thien-Thi Nguyen
## Copyright (C) 2001, 2003, 2004, 2005, 2006,
##    2007 Free Software Foundation, Inc.
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

SUBDIRS = source doc

ACLOCAL_AMFLAGS = -I build-aux

install-data-hook:
	rm -f "$(DESTDIR)$(GUILE_SITE)/www/METAINFO"
	if $(GUILE_TOOLS) | grep -q make-module-catalog ; then	\
	   $(GUILE_TOOLS) make-module-catalog \
	      -t "$(DESTDIR)" "$(GUILE_SITE)" ; fi

EXTRA_DIST = \
 build-aux/simple-punify \
 examples/README \
 examples/cookie.cgi \
 examples/wwwcat \
 examples/you-are-here \
 contrib

## Makefile.am ends here
