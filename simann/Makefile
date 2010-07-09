INCLUDES=-I/usr/include/python2.5
CC=gcc
CFLAGS=-Wall -O2 $(INCLUDES)
# CFLAGS=-Wall -g -pg $(INCLUDES)
LDFLAGS=$(CFLAGS)
LDSHARED=gcc -shared $(CFLAGS)

all: sahelp.so

clean:
	rm -f *~ *.o *.so *.shar
	rm -f gmon.out sahelp.h

sahelp.o: sahelp.h

sa: sahelp.so
	python sa.py

sahelp.so: sahelp.o
	$(LDSHARED) -o sahelp.so sahelp.o

sahelp.h: sahelp.c
	cproto $(INCLUDES) -s sahelp.c >> sahelp.h 2>/dev/null
