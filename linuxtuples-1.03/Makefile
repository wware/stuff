CFLAGS=-fno-strict-aliasing -g -Wall -Wmissing-prototypes -Wstrict-prototypes -fPIC

PREFIX		= /usr/local
ifndef CC
CC		= gcc
endif

# dotted python version (2.3, 2.4)
PYDVER := $(shell python -c "import sys; print sys.version[:3]")
UNAME:=$(shell uname)

ifeq ($(strip $(UNAME)),Darwin)
#---------------------------------------- Mac
CFLAGS		+= -I/System/Library/Frameworks/Python.framework/Versions/$(PYDVER)/lib/python$(PYDVER)/config \
	-I/System/Library/Frameworks/Python.framework/Versions/$(PYDVER)/include/python$(PYDVER)/ -DMACOSX
LDFLAGS		= -Wl,-F. -framework Python
LDSHARED	= gcc -bundle
LDFLAGS_DEBUG	=
else
#---------------------------------------- Unix
PYBASE		:= $(shell which python | sed "s%/bin/python%%")
CFLAGS		+= -I$(PYBASE)/include/python$(PYDVER)
LDFLAGS		= -L$(PYBASE)/lib/python$(PYDVER)/config -lm -lpython$(PYDVER)
LDSHARED	= $(CC) -shared
LDFLAGS_DEBUG	= -lefence
#---------------------------------------- End of Unix
endif

LDFLAGS		+= -L/usr/lib -lm
SITE_PACKAGES	= /usr/lib/python$(PYDVER)/site-packages

ifeq ($(DEBUG),1)
CFLAGS		+= -Wall -DDEBUG -g
LDFLAGS		+= $(LFDFLAGS_DEBUG)
else
CFLAGS		+= -Wall -O3
endif

SRCS		= tuple.c tuple_server.c tuple_client.c fft.c
INSTBINS	= tuple_server tuple_client 
INSTLIBS	= linuxtuples.so
PROGS		= $(INSTBINS) fft $(INSTLIBS)

all: $(PROGS)

debug:
	make DEBUG=1 all

indent:
	indent -i8 -br -npcs *.c *.h

htmldocs:
	doxygen
	(head -10 html/main.html; cat package.html; tail --lines=+10 html/main.html) > main.html
	mv -f main.html html

tuple_server: tuple_server.o tuple.o
	$(CC) -o tuple_server tuple_server.o tuple.o $(LDFLAGS) -lpthread

tuple_client: tuple_client.o tuple.o
fft: fft.o tuple.o

tuple.o: tuple.c tuple.h
tuple_server.o: tuple_server.c tuple.h
tuple_client.o: tuple_client.c tuple.h
py_linuxtuples.o: py_linuxtuples.c tuple.h
fft.o: fft.c tuple.h

linuxtuples.so: py_linuxtuples.o tuple.o
	$(LDSHARED) -o linuxtuples.so py_linuxtuples.o tuple.o $(LDFLAGS)


install: all
	mkdir -p $(PREFIX)/bin $(PREFIX)/lib
	install -v -m 0755 -s $(INSTBINS) $(PREFIX)/bin
	install -v -m 0644 $(INSTLIBS) $(SITE_PACKAGES)

diffs:
	diff -Nur ../linuxtuples-1.02 . >../linuxtuples-diffs

clean:
	rm -f *.o *.pyc *~ $(PROGS)
	rm -rf latex/ html/

