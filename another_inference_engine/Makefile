INCLUDES=-I/usr/include/python2.5
CC=gcc
CFLAGS=-Wall -O2 $(INCLUDES)
# CFLAGS=-Wall -g -pg $(INCLUDES)
LDFLAGS=$(CFLAGS)
LDSHARED=gcc -shared $(CFLAGS)

all: graph.so

clean:
	rm -f *~ *.bak *.class *.o *.so *.shar
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
