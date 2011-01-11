guilewebdir = @GUILE_SITE@/web
internaldir = $(guilewebdir)/internal
ghtmldir = $(guilewebdir)/html

guileweb_DATA = hash-dash.scm session.scm serialize.scm db.scm ssax.scm  \
		xhtml.scm template.scm memcache.scm
ghtml_DATA = html/html-defines.scm html/html-doc.scm html/gen-html.scm html/html-defined.scm

EXTRA_DIST = $(guileweb_DATA) $(ghtml_DATA)

CLEANFILES=xhtmldoc.texinfo guile-web.info html/html-defined.scm
# xhtml tag documentation
xhtmldoc.texinfo:
	guile -s $(srcdir)/html/html-doc.scm > $@

guile-web.info: xhtmldoc.texinfo

# Generated Tags
BUILT_SOURCES=html/html-defined.scm
html/html-defined.scm:
	guile -s $(srcdir)/html/gen-html.scm > $@

info_TEXINFOS = guile-web.texinfo

AM_CFLAGS = @GUILE_CFLAGS@

lib_LTLIBRARIES = libgwebsession.la
libgwebsession_la_SOURCES = session.c
libgwebsession_la_LIBADD = @GUILE_LDFLAGS@