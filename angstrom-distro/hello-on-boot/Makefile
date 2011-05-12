all: helloworld

helloworld.o: helloworld.c
	${CC} ${CFLAGS} -c -o helloworld.o helloworld.c

helloworld: helloworld.o
	${CC} ${LDFLAGS} -o helloworld helloworld.o -static
