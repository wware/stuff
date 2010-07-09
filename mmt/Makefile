all: yo

clean:
	rm -f yo* *~ core

yo.c: mmt.py tryit.c
	./mmt.py < tryit.c | indent - > yo.c

yo.h: yo.c
	cproto yo.c > yo.h

yo: yo.c yo.h
	gcc -O3 -Wall -o yo yo.c

tgz:
	(cd ..; tar cfz mmt.tgz mmt/)

shar:
	(shar tryit.c mmt.py Makefile) > ../mmt.shar
