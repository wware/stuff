all: zbuf.so

clean:
	rm -f *.o *.so *.rgb *.gif *.jpg *.mpg *.pyc

zbuf.so: zbuf.c
	gcc -g -Wall -O2 -c -I/usr/include/python2.5 zbuf.c
	gcc -shared -o zbuf.so zbuf.o

shar:
	shar zbuf.c anima.py Makefile PPMtoMPEG.par > zbuf.shar
