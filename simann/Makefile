PYDVER := $(shell python -c "import sys; print sys.version[:3]")
UNAME:=$(shell uname)
# PYBASE := $(shell which python | sed "s%/bin/python%%")
PYBASE := /usr
INCLUDES = -I$(PYBASE)/include/python$(PYDVER)
PYLIBDIR = $(PYBASE)/lib/python$(PYDVER)
CFLAGS += $(INCLUDES)
LDFLAGS = -L$(PYLIBDIR)/config -lm -lpython$(PYDVER)
LDSHARED = $(CC) -shared
CC=gcc
CFLAGS+=-Wall -O2 -fPIC
# CFLAGS+=-Wall -g -pg    # for collecting performance data

all: sahelp.so

clean:
	rm -f *~ *.o *.so *.shar
	rm -f gmon.out

sahelp.o: sahelp.h

sa: sahelp.so
	python sa.py

sahelp.so: sahelp.o
	$(LDSHARED) -o sahelp.so sahelp.o
