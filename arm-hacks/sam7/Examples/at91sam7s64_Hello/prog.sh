#!/bin/sh

make clean
make || exit 1

Sam_I_Am set ramwriteallow 0xffffff64 4 , shell sleep 0.1 , \
	writew ffffff64 5a000004 , shell sleep 0.1 , \
	writew ffffff64 5a002004 , shell sleep 0.1 , \
	flash main.hex , shell sleep 0.1 , \
	writew ffffff64 5a00020b
