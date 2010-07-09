CC = gcc
CFLAGS = -O3 -Wall
# CFLAGS = -g -Wall

all: sndblit

sndblit: sndblit.c
	$(CC) $(CFLAGS) -o sndblit sndblit.c -lm

clean:
	rm -f core *.o *~ sndblit *.tmp *.wav *.blip *.shar

install: sndblit
	cp sndblit /usr/local/bin
	cp sndblit.1 /usr/local/man/man1

shar:
	shar -T sndblit.1 sndblit.c Makefile foo.py > sndblit.shar
