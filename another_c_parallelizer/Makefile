# Use "make DEBUG=1" to get full debug, which
# is quite verbose.

CC = gcc
CFLAGS = -Wall -O3

ifeq ($(DEBUG),1)
CFLAGS += -g -DDEBUG
PFLAGS = -debug
endif

all: quux foo

clean:
	rm -rf *~ *.o quux* foo* core zz

###################################

thr.o: thr.c thr.h

###################################

quux.c: simple.c process.py
	./process.py $(PFLAGS) simple.c quux.c

quux: quux.c thr.o thr.h
	$(CC) $(CFLAGS) -o quux quux.c thr.o

###################################

foo.c: conway.c process.py
	./process.py $(PFLAGS) conway.c foo.c

foo: foo.c thr.o thr.h
	$(CC) $(CFLAGS) -o foo foo.c thr.o

########################################

shar:
	/usr/bin/shar README Makefile simple.c conway.c \
		process.py thr.c thr.h > zz.shar
