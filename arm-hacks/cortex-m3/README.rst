The Cortex-M3
=============

http://www.arm.com/products/processors/cortex-m/cortex-m3.php

The most convenient and cheap Cortex M3 board I've found yet is the STM32VL
Discovery board from ST Microelectronics. This board costs about twelve bucks,
and you should always order two because if one goes bad, you may be able to
use the other to recover the bootloader. Several of these steps would change
if you had a STM32L, which uses STLink-V2 instead of STLink-V1.

As usual, my development environment is Ubuntu 10.04. My tinkering with the
thing has been informed by this tutorial:

https://github.com/texane/stlink/blob/master/doc/tutorial/tutorial.pdf

The first thing to do is clone a copy of the texane/stlink repo on Github. I
will diverge slightly from their procedure, because I already have a working
arm-elf toolchain so I won't use the arm-none-eabi toolchain that they
recommend.

You'll need to pick up a few additional pieces for Ubuntu::

 sudo apt-get libusb-1.0-0 libusb-1.0-0-dev pkg-config

Go into the stlink repo and edit all the Makefiles to change "arm-none-eabi"
to "arm-elf"::

 sed -i 's/none-eabi/elf/g' $(git grep -l none-eabi | grep Makefile)

You'll need to set up a udev rule for the Discovery board::

 sudo cp 49-stlinkv1.rules /etc/udev

You're ready to start hacking. Plug the board in with a mini-USB cable.
In one terminal window, type::

 gdbserver/st-util --stlinkv1

This will connect to the gdbserver stub that's built into the firmware on the
Discovery board. This will run on localhost on port 4242. Now you need a GDB
client. In another terminal window, type::

 cd example/blink
 make     # creates the blink_32VL.elf executable
 arm-elf-gdb
 (gdb) target extended-remote localhost:4242
 (gdb) load blink_32VL.elf
 (gdb) continue

And voila, the lights blink. It is a joy to behold. Your code is running out
of RAM.

Running out of flash
--------------------

*BEFORE* you attempt to write to the area of flash starting at 0x8000000
you should first grab the bytes that are already there, so you can restore
them if things go badly. Had I done this at the outset, I wouldn't have
needed to pull them off the second board::

 flash/flash read v1 goodbits.bin 0x8000000 4096

Next a little bit of preparation::

 (cd example/libs_stm/build; make)
 (cd example/blink_flash; make CONFIG_STM32VL_DISCOVERY=1)

Get out of GDB and kill the gdbserver process. You might need to unplug
the board and plug it back in. Then type::

 flash/flash write v1 example/blink_flash/blink.bin 0x8000000

If all goes well, you'll see a last line that says::

 Flash written and verified! jolly good!

According to the PDF, the board is now ready to run the blink example out
of flash memory instead of RAM. I'm not sure if that involves hitting the
reset button or power cycling but neither has worked for me.

When you throw up your hands in dispair, restore the original good bits.
Unplug the board, plug it in again, look at dmesg output to see if it's
happy, and repeat if it isn't. Once it's happy::

 flash/flash write v1 goodbits.bin 0x8000000

Open questions
--------------

Why can't I get the code to run out of flash?

Why do many bin files seem to be 3312 bytes exactly? Is this some kind of
magic number?
