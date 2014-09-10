all: myhdl.vpi bin2gray.v tb_bin2gray.v ut

clean:
	rm -f *~ *.v *.vhd *.vpi *.o

myhdl.vpi: myhdl_vpi.c
	gcc -I/usr/include/iverilog -I/usr/include/cver -c -o myhdl.o myhdl_vpi.c
	gcc -shared -o myhdl.vpi myhdl.o -lvpi

bin2gray.v tb_bin2gray.v: tryit.py
	python tryit.py

# run unit tests
ut:
	python tryit.py ut
