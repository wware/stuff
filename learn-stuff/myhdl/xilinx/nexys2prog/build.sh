#!/bin/sh

# Minimal Xilinx build environment to show off the Nexys 2 programming
# script.

PROJ=led

# Choose the proper FPGA part ID.  Digilent ships two versions.
#PART=xc3s500e-fg320-5
PART=xc3s1200e-fg320-5

echo run -ifn $PROJ.v -ifmt verilog -ofn $PROJ.ngc -p $PART -top $PROJ | xst
ngdbuild -i -p $PART $PROJ.ngc
map -p $PART $PROJ.ngd
par $PROJ.ncd ${PROJ}_par.ncd $PROJ.pcf
bitgen ${PROJ}_par.ncd $PROJ.bit $PROJ.pcf

./nexys2prog $PROJ.bit
