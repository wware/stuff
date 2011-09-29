Hacking MyHDL
=============

Installation on Ubuntu 10.04 (Lucid)::

 sudo add-apt-repository ppa:balau82/ppa
 sudo apt-get update
 sudo apt-get install myhdl gtkwave verilog

Making sure it works::

 (cd /usr/share/doc/myhdl/examples/rs232; python test_rs232.py)

Online manual:

* http://www.myhdl.org/doc/0.6/manual/index.html

Projects:

* http://www.myhdl.org/doku.php/projects:intro
* http://www.antfarm.org/blog/aaronf/2008/02/myhdl_a_brief_discussion_2.html
* /usr/share/doc/myhdl/examples

Cosimulation??
--------------

* http://www.myhdl.org/doc/0.6/manual/cosimulation.html
* http://myhdl.hg.sourceforge.net/hgweb/myhdl/myhdl/file/3d2358a410a9/cosimulation/cver/myhdl_vpi.c

I need to be able to compile myhdl.vpi from the file
myhdl_examples/cosimulation/cver/myhdl_vpi.c.gz.

::

 sudo apt-get install gplcver
 gcc -I/usr/include/iverilog -I/usr/include/cver -c -o myhdl.o myhdl_vpi.c
 gcc -shared -o myhdl.vpi myhdl.o -lvpi

The MicroBlaze core
-------------------

MyBlaze is a MyHDL implementation (LGPL) of the GCC-targetable MicroBlaze soft
processor core which runs on Xilinx FPGAs. Some people have run MMU-less Linux
on it, but it can also run FreeRTOS. I wonder if FPGAs are cheap enough to
make this really worthwhile compared to conventional microcontrollers.

* http://en.wikipedia.org/wiki/MicroBlaze
* http://xilinx.wikidot.com/mb-gnu-tools
* http://xilinx.wikidot.com/microblaze-linux
* http://www.xilinx.com/ipcenter/processor_central/microblaze/doc/mb_tutorial_c2bits.pdf
* http://xlnx.lithium.com/t5/EDK-and-Platform-Studio/Microblaze-gcc-cross-compiler-tool-chain-sources/td-p/87492
* http://www.ruby-forum.com/topic/206716 - works with GCC 4.1.2???
* http://www.freertos.org/index.html?http://www.freertos.org/portmicroblaze.html
* http://gcc.gnu.org/gcc-4.6/changes.html

Here is a guy in Israel who has done a lot of Microblaze work.

* http://billauer.co.il/blog/category/fpga/
* http://billauer.co.il/blog/2011/08/linux-microblaze-howto-tutorial-primer-1/
* http://billauer.co.il/blog/2011/08/linux-microblaze-howto-tutorial-primer-2/
* http://billauer.co.il/blog/2011/08/linux-microblaze-howto-tutorial-primer-3/
* http://billauer.co.il/blog/2011/08/linux-microblaze-howto-tutorial-primer-4/
* http://billauer.co.il/blog/2011/08/dts-of-open-firmware-microblaze/
