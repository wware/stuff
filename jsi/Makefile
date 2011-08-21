CFLAGS=-g -Wall -Werror

all: run

run: tryit
	./tryit

hashtable.o: hashtable.c hashtable.h

jsi-linux.o: jsi-linux.c jsi.h

tryit: jsi-linux.o hashtable.o
	gcc -o tryit jsi-linux.o hashtable.o -lpthread

clean:
	rm -f *~ *.o tryit
