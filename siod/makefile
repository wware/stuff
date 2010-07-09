# Makefile for SIOD under Unix.
# George J. Carrette, gjc@world.std.com
#
#
# Note: When you go to run these from the current directory before installing,
#       don't forget to setenv LD_LIBRARY_PATH "."
#       Static executable 'sample' is build for comparison purposes
#       on some platforms.
# 
MANDIR=$(IROOT)/usr/local/man/man1
BINDIR=$(IROOT)/usr/local/bin
INCDIR=$(IROOT)/usr/local/include
LIBDIR=$(IROOT)/usr/local/lib
LIBSIODDIR=$(LIBDIR)/siod
#
SAMPLE_OBJS = sample.o slib.o sliba.o trace.o
SIOD_OBJS_COMMON = slib.o sliba.o trace.o slibu.o md5.o
#
# -g or -g3 would be appropriate here.
#CDEBUG=-g2 -O0
#
default:
	@echo "*****************************************************"
	@echo "* Please specify target from operating system list: *"
	@echo "*  osf1 hpux solaris linux sgi                      *"
	@echo "*****************************************************"
	@echo
	@echo  " dist    ... siod_tar.gz"
	@echo  " install ... copies to $(BINDIR) etc, see makefile for doc"
	@echo  " clean   ... delete objects and binaries."

.SUFFIXES: .o .so .1 .txt .sl

# the build_driver is the target of the recursive call to make.

build_driver: $(PROGS) $(EXTRA_PROGS)
	@echo "Build done."


# Digital Equipment Corporation, OSF/1, DIGITAL UNIX.
# uname = OSF1

osf1:
	$(MAKE) \
	PROGS="siod sample\
              ndbm.so tar.so parser_pratt.so ss.so regex.so\
              acct.so gd.so statfs.so" \
	CFLAGS="$(CDEBUG) -readonly_strings" \
	LD_EXE_FLAGS="-call_shared" \
	LD_EXE_LIBS="-lm -lc" \
	LD_LIB_FLAGS="-shared" \
	LD_LIB_LIBS="-lm -lc" \
	SO="so" \
	SYBINCS="-I$(SYBASE)/include" \
	SYBLIBS="$(SYBASE)/lib/libct.a $(SYBASE)/lib/libcs.a \
                 $(SYBASE)/lib/libcomn.a $(SYBASE)/lib/libtcl.a \
                 $(SYBASE)/lib/libinsck.a $(SYBASE)/lib/libsdna.a \
                 -ldnet_stub $(SYBASE)/lib/libintl.a -lm -lc" \
        build_driver

# Hewlett-Packard Company, HP-UX
# From: Craig Denson <denson@sdd.hp.com>, hacked up by gjc
# uname = HP-UX

hpux:
	$(MAKE) \
	PROGS="siod ndbm.sl tar.sl parser_pratt.sl ss.sl \
	       regex.sl" \
	CFLAGS="-Ae -O +z -Dhpux -Dunix $(CDEBUG)" \
	LD_EXE_FLAGS="" \
	LD_EXE_LIBS="-lm -ldld" \
	LD_LIB_FLAGS="-b" \
	LD_LIB_LIBS="-lm -ldld -lndbm" \
	SO="sl" \
	SYBINCS="-I$(SYBASE)/include" \
	SYBLIBS="$(SYBASE)/lib/libct.a $(SYBASE)/lib/libtcl.a \
	         $(SYBASE)/lib/libinsck.a \
	         $(SYBASE)/lib/libcs.a $(SYBASE)/lib/libcomn.a \
	         $(SYBASE)/lib/libintl.a \
	         -Wl,-a,archive -lcl $(SYSLIBS) -lBSD -lm" \
        build_driver


### Sun Microsystems ###
# Solaris 2.4, Sybase R10.0.2
# From: Philip G Wilson <pgw9@columbia.edu>
# hacked by gjc for dlopen.
# uname = SunOS, uname -r = 5.3

solaris:
	$(MAKE) \
	PROGS="siod ndbm.so tar.so parser_pratt.so ss.so" \
	CC=gcc \
	LD=gcc \
	CFLAGS="-Wall $(CDEBUG)" \
	LD_EXE_FLAGS="" \
	LD_EXE_LIBS="-lm -lsocket -lnsl -lposix4 -dl" \
	LD_LIB_FLAGS="-G" \
	LD_LIB_LIBS="$(LD_EXE_LIBS)" \
	SO="so" \
	SYBINCS="-I$(SYBASE)/include" \
	SYBLIBS="-L$(SYBASE)/lib -lct -lcs -lcomn -ltcl -ltli \
                 -lintl -lsocket -lnsl -lm" \
        build_driver

## Sun Microsystems ###
# SUN-OS 4.1.1
# From: Leo Harten lph@paradigm.com
# uname = ???, uname -r = ???

sunos:
	$(MAKE) \
	PROGS="siod tar.so parser_pratt.so ss.so" \
	CC=gcc \
	LD=gcc \
	CFLAGS="-Wall $(CDEBUG)" \
	LD_EXE_FLAGS="" \
	LD_EXE_LIBS="-lm -lsocket -lnsl -lposix4 -dl" \
	LD_LIB_FLAGS="-G" \
	LD_LIB_LIBS="$(LD_EXE_LIBS)" \
	SO="so" \
	SYBINCS="-I$(SYBASE)/include" \
	SYBLIBS="-L$(SYBASE)/lib -lct -lcs -lcomn -ltcl -ltli \
                 -lintl -lsocket -lnsl -lm" \
        build_driver

# Tested by GJC on an intel pentium running Linux 1.3.52
# uname = Linux

linux:
	$(MAKE) \
	PROGS="siod tar.so parser_pratt.so ss.so \
	       regex.so acct.so" \
	CC="gcc" \
	LD="gcc" \
	CFLAGS="-fPIC $(CDEBUG)" \
	LD_EXE_FLAGS="-rdynamic" \
	LD_EXE_LIBS="-ldl" \
	LD_LIB_FLAGS="-shared" \
	LD_LIB_LIBS="-lm -lc -ldl" \
	SO="so" \
        build_driver

# Silicon Graphics, MIPS.
# uname = IRIX

sgi:
	$(MAKE) \
	PROGS="siod \
              ndbm.so tar.so parser_pratt.so ss.so"\
	CFLAGS="$(CDEBUG)" \
	LD_EXE_FLAGS="-call_shared" \
	LD_EXE_LIBS="-lm -lc" \
	LD_LIB_FLAGS="-shared" \
	LD_LIB_LIBS="-lm -lc" \
	SO="so" \
        build_driver


### Finally, the actual compilation and linking commands.

libsiod.$(SO): $(SIOD_OBJS_COMMON)
	$(LD) -o libsiod.$(SO) $(LD_LIB_FLAGS) $(SIOD_OBJS_COMMON) \
                               $(LD_LIB_LIBS) 

siod: siod.o libsiod.$(SO) 
	$(CC) -o siod $(LD_EXE_FLAGS) siod.o libsiod.$(SO) $(LD_EXE_LIBS)

sample: $(SAMPLE_OBJS)
	$(CC) -o sample $(LD_EXE_FLAGS) $(SAMPLE_OBJS) $(LD_EXE_LIBS)

.o.$(SO):
	$(LD) -o $@ $(LD_LIB_FLAGS) $< libsiod.$(SO) $(LD_LIB_LIBS)


tar.$(SO): tar.o libsiod.$(SO)

ss.$(SO): ss.o libsiod.$(SO)

acct.$(SO): acct.o libsiod.$(SO)


gd.$(SO): gd.o libsiod.$(SO)
	$(LD) -o gd.$(SO) $(LD_LIB_FLAGS) gd.o libsiod.$(SO) -lgd \
                          $(LD_LIB_LIBS)

ndbm.$(SO): ndbm.o libsiod.$(SO)
	$(LD) -o ndbm.$(SO) $(LD_LIB_FLAGS) ndbm.o libsiod.$(SO) \
                            $(LD_LIB_LIBS)

regex.$(SO): regex.o libsiod.$(SO)
	$(LD) -o regex.$(SO) $(LD_LIB_FLAGS) regex.o libsiod.$(SO) \
                             $(LD_LIB_LIBS)


siod.o: siod.c siod.h

sample.o: sample.c siod.h

slib.o:	slib.c siod.h siodp.h

sliba.o:	sliba.c siod.h siodp.h

trace.o:	trace.c siod.h siodp.h

slibu.o:	slibu.c siod.h siodp.h md5.h

ss.o:	ss.c siod.h ss.h

md5.o:	md5.c md5.h

# Note: http://siva.cshl.org/gd/gd.html
gd.o: gd.c
	$(CC) $(CFLAGS) -c gd.c -I$(INCDIR)

ndbm.o: ndbm.c siod.h

statfs.o: statfs.c siod.h

tar.o: tar.c siod.h

regex.o: regex.c siod.h

acct.o: acct.c siod.h

parser_pratt.o: parser_pratt.c siod.h

MANPAGES = siod.1 snapshot-dir.1 snapshot-compare.1 http-get.1 \
           cp-build.1 ftp-cp.1 csiod.1 ftp-put.1 ftp-test.1

CMDFILES = snapshot-dir snapshot-compare http-get cp-build \
           ftp-cp csiod ftp-put ftp-test ftp-get

LIBFILES = fork-test.scm http-server.scm http-stress.scm http.scm \
           maze-support.scm pratt.scm siod.scm smtp.scm \
           cgi-echo.scm find-files.scm \
           hello.scm parser_pratt.scm pop3.scm selfdoc.scm \
	   sample.c siod.html piechart.scm cgi.scm ftp.scm

SOLIBFILES=gd ndbm tar ss regex acct parser_pratt \
           statfs

PUBINCS = siod.h

SRCFILES = makefile \
           README descrip.mms \
           makefile.com siod.c siod.h siod.opt \
           siodm.c siodp.h slib.c sliba.c slibu.c \
           ss.c \
           ss.h trace.c md5.c md5.h \
           gd.c ndbm.c tar.c regex.c acct.c statfs.c \
           siodshr.opt ss.opt tar.opt parser_pratt.c parser_pratt.opt \
	   libsiod.mak parser_pratt.mak siod.mak \
	   libsiod.def parser_pratt.def make.bat

DISTFILES= $(CMDFILES) $(LIBFILES) $(SRCFILES) $(MANPAGES) \
           $(MANPAGES:.1=.txt)

INTO_BINDIR=$(CMDFILES) siod
INTO_LIBDIR=libsiod.so libsiod.sl

install:
	@echo "Note: This does not do a build. Only installs what already"
	@echo "      sits in the directory."
	-mkdir -p $(MANDIR)
	-mkdir -p $(BINDIR)
	-mkdir -p $(LIBDIR)
	-mkdir -p $(INCDIR)
	-mkdir -p $(LIBSIODDIR)
	-for X in $(INTO_BINDIR)  ; do \
	 cp -f $$X $(BINDIR) ;\
	done
	-for X in $(LIBFILES) ; do \
	 cp -f $$X $(LIBSIODDIR) ;\
	done
	-for X in $(SOLIBFILES) ; do \
	  for E in so sl ; do \
	   cp -f $$X.$$E $(LIBSIODDIR) ;\
	  done ;\
	done
	-for X in $(INTO_LIBDIR) ; do \
	 cp -f $$X $(LIBDIR) ;\
	done
	-for X in $(MANPAGES) ; do \
	 cp -f $$X $(MANDIR) ;\
	done
	-for X in $(PUBINCS) ; do \
	 cp -f $$X $(INCDIR) ;\
	done
	@echo "Install done."

clean:
	-rm *.o *.so *.sl *~ $(MANPAGES:.1=.txt) so_locations \
            siod sample siod.tar siod_tar.gz selfdoc.txt

# make manpage txt files for distribution to people who do not have nroff.

.1.txt:
	nroff -man $< | col -bx > $@

siod.tar: $(DISTFILES)
	tar cvf siod.tar $(DISTFILES)

siod_tar.gz: siod.tar
	gzip -v -c siod.tar > siod_tar.gz

dist: siod_tar.gz
	@echo "distribution kit created."

# another case of makefile trouble. I need the binding of $(SO)
# here.

selfdoc:
	./selfdoc.scm *.so > selfdoc.txt

# 

WIN95BIN=./release/libsiod.dll ./release/libsiod.lib ./release/siod.exe \
         ./release/parser_pratt.dll 

win95bin.zip: $(WIN95BIN)
	zip -rj win95bin.zip $(WIN95BIN)

ftp.std.com: siod_tar.gz
	ftp-put siod.html world.std.com /ftp/pub/$$LOGNAME/siod.html
	ftp-put siod_tar.gz world.std.com /ftp/pub/$$LOGNAME/siod_tar.gz
