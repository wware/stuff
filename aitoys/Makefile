all: igobjsc.so opnames.py gac.so neur.so

test:
	make all
	python gatest.py
	python igtest.py
	python nn.py

debug:
	make clean; make OPT="-g -DPy_DEBUG" all

# make OPT="-Wall" all   <=== build using gcc's lint function

CC = gcc
CFLAGS = $(OPT) -fpic -O2 -I/usr/local/include -I/usr/include/python2.5

.SUFFIXES: .i .c .o .e .so .h .wrap.c

.c.e:
	$(CC) -E $(CFLAGS) $(@:.e=.c) > $@

.o.so:
	gcc -shared -o $@ $^

.i.wrap.c:
	swig -shadow -python -o $@ $(@:.wrap.c=.i)

OBJECTS = gac.o ga.wrap.o igobjsc.o igobjs.wrap.o

objs: $(OBJECTS)

igobjsc.so: igobjsc.o igobjs.wrap.o
gac.so: gac.o ga.wrap.o

gac.o: ga.h
ga.wrap.o: ga.wrap.c
igobjsc.o: igobjs.h ops.h ops.c
igobjs.wrap.o: ops.h

igobjs.wrap.c: igobjs.h ops.h
ga.wrap.c: ga.h

ops.c: procops.py ops.b
	./procops.py -c < ops.b > ops.c
ops.h: procops.py ops.b
	./procops.py -d < ops.b > ops.h
opnames.py: procops.py ops.b
	./procops.py -p < ops.b > opnames.py

clean:
	rm -f core *.e *~ *.o *.so *.wrap.* ga.py *.pyc \
		igobjs.py ops.h ops.c opnames.py

FILES = README Makefile igobjsc.c igobjs.h igobjs.i igtest.py ops.b \
	procops.py iguana.py gac.c ga.h ga.i gatest.py \
	neur.c nn.py

tgz:
	tar cf - $(FILES) | gzip > ../aipp.tgz

shar:
	shar $(FILES) > ../aipp.shar
