all: nada

nada: nada.c
	gcc -Wall -g -o nada nada.c `pkg-config --cflags --libs gtk+-2.0` -export-dynamic

clean:
	rm -f nada *~
