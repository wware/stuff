#!/bin/sh

source /opt/Xilinx/13.2/ISE_DS/settings32.sh
export PATH=$PATH:/opt/Xilinx/13.2/ISE_DS/ISE/bin/lin
/usr/local/bin/nexys2prog -v $*
