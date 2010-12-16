PYDVER := $(shell python -c "import sys; print sys.version[:3]")
PYLIBDIR = $(PYBASE)/lib/python$(PYDVER)
UNAME:=$(shell uname)
PYBASE := $(shell which python | sed "s%/bin/python%%")
INCLUDES = -I$(PYBASE)/include/python$(PYDVER)
CFLAGS += $(INCLUDES)
LDFLAGS = -L$(PYLIBDIR)/config -lm -lpython$(PYDVER)
LDSHARED = $(CC) -shared
CC=gcc
CFLAGS+=-Wall -O2
# CFLAGS+=-Wall -g -pg    # for collecting performance data

all: graph.so

clean:
	rm -f *~ *.bak *.class *.o *.so *.shar .depend
	rm -f gmon.out rdfc graphobject.h

common.o: common.h graphobject.h

graphobject.o: common.h graphobject.h

foo: graph.so
	./foo.py

graph.so: graphobject.o common.o
	$(LDSHARED) -o graph.so graphobject.o common.o

graphobject.h: common.c graphobject.c
	cproto common.c > graphobject.h
	echo '#ifdef Py_PYTHON_H' >> graphobject.h
	cproto $(INCLUDES) -s graphobject.c >> graphobject.h 2>/dev/null
	echo '#endif' >> graphobject.h
