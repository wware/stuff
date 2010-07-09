INCL=-I/usr/include/python2.6

LDFLAGS=-I/usr/lib/python2.6
CFLAGS = -Wall -g $(INCL)

tryit: callpy
	./callpy foo main a b c d

callpy: callpy.o
	gcc -o callpy callpy.o $(LDFLAGS) -lpython2.6

callpy.o: callpy.c
	gcc $(CFLAGS) -c -o callpy.o callpy.c

clean:
	rm -f *~ *.o *.pyc callpy core
