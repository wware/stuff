## Makefile.am
##
## Copyright (C) 2007, 2009, 2010 Thien-Thi Nguyen
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

wwwdir = $(GUILE_SITE)/www/data
www_DATA =					\
 mime-types					\
 http-status

include $(top_srcdir)/build-aux/common.mk

# Module (www data content-type) retired as of Guile-WWW 2.27.
install-data-hook:
	@rm -f '$(DESTDIR)$(wwwdir)/content-type'

## Makefile.am ends here
