#CFLAGS = -Wall -g
CFLAGS = -Wall -O2

ifeq ($(TESTFFT),1)
CFLAGS+=-DTESTFFT=1
endif

all: test

mt19937ar.o: mt19937ar.c
	gcc $(CFLAGS) -c mt19937ar.c 

fft.o: fft.c
	gcc $(CFLAGS) -c fft.c

test: tryit2.py check-c-code.py
	python check-c-code.py

fft: fft.o mt19937ar.o
	gcc -o fft fft.o mt19937ar.o -lm -lrt

clean:
	rm -f *~ *.o *.pyc
	rm -f mt fft data.* runtime.dat
