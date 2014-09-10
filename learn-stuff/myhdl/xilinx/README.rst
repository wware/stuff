Running the blinker example on a Xilinx board
=============================================

Files in this directory

* **go.sh** is a very short shell script to kick off the Xilinx IDE
* **blinker.v** is Verilog produced by the blinker.py script in the directory
  above this one; I needed to make a couple small manual edits
* **blinker.xise** is a project file for the Xilinx IDE (called ISE)
* **nexys2.ucf** is a constraint file for the `NEXYS 2 board`_ that I'm
  working with
* **blinker.bit** is the programming bitstream that the Xilinx IDE produced to
  program the NEXYS 2 board

.. _`NEXYS 2 board`: http://www.digilentinc.com/Data/Products/NEXYS2/Nexys2_rm.pdf

A **constraint file** sets up constraints on how an FPGA design is to be
placed and routed. In this design, it assigns pin numbers for the chip's
inputs and outputs.

The Xilinx IDE and tools are `freely available`_ for download as long as you
don't need advanced features. Look for the "Download free ISE WebPACK" link
in the lower left. You'll need to get a small license file from Xilinx, which
is also free.

.. _`freely available`: http://www.xilinx.com/products/design-tools/ise-design-suite/index.htm

FPGA notes for the NEXYS 2 board
--------------------------------

Very lucky to find `this discussion`_ where I learned about nexys2prog, a great
little Perl script that helps you to program the NEXYS 2 board without paying for
a potentially expensive USB-JTAG interface. From the nexys2prog docs::

.. _`this discussion`: http://www.edaboard.com/thread145823.html

 Executive summary for Debian/Ubuntu users: after installing ISE and setting up
 your environment to use it, install fxload and libftdi1 using apt-get, and
 build and install UrJTAG from http://urjtag.org. Then call this script with the
 name of the Xilinx bitstream file you would like to program.  That's it.  See
 the attached led.v verilog file and build.sh script for a minimal sample.

Also useful: http://ixo-jtag.sourceforge.net/nexys2-linux-howto.html

I've posted the nexys2prog_ and urjtag_ tarballs for download on Google Docs.

.. _nexys2prog: https://docs.google.com/leaf?id=0B656IJ3nlMSMYjU5ZWJjOGQtYmY1NS00ODcxLWFjYWYtZjk4YjRhOGYzZjNk
.. _urjtag: https://docs.google.com/leaf?id=0B656IJ3nlMSMNzZjNDRmZDctMGM5Yy00OWFmLWIwNTUtZjc2ZDk1YjFhNjU2

The nexys2prog script depends upon your having installed the Xilinx IDE.

Here's how to install nexys2prog on an Ubuntu 10.04 system::

 sudo apt-get install fxload libftdi1 libftdi-dev
 # unpack the tarballs for urjtag and nexys2prog
 (cd urjtag-0.10; ./configure; make; sudo make install)
 sudo cp nexys2prog/nexys2prog /usr/local/bin

As root, do the following to program a bitstream into the NEXYS 2 board::

 sudo ./prog.sh foobar.bit

So I ran the first example in the Digilent text involving gates2.v and gates2_top.v and everything worked great.

Notes on the Papilio One board
------------------------------

These days I use a Macbook more often than a Linux machine, and the Xilinx tools
don't work on Mac so I need to go back to Linux or Windows to build and program the 
".bit" file. Given the neglected dual-boot Linux/Win7 laptop, the simplest approach
is to install Windows ISE WebPack on Win7 along with `ButterflyLoader`_, and then I can
port the Verilog file to Windows and do everything there. The `Getting Started`_ page
has more information, including a Youtube video whose audio channel is broken. I've
added a `UCF constraints file for the Papilio board`_ which I got from `Gadget Factory`_,
who created the Papilio platform.

.. _`ButterflyLoader`: http://gadgetforge.gadgetfactory.net/gf/project/butterflyloader/
.. _`Getting Started`: http://papilio.cc/index.php?n=Papilio.GettingStarted
.. _`UCF constraints file for the Papilio board`: http://gadgetforge.gadgetfactory.net/gf/download/frsrelease/134/412/BPC3003_2.03%2B.ucf
.. _`Gadget Factory`: http://www.gadgetfactory.net/
