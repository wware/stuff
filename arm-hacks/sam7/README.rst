SAM7 board and development
==========================

There are some nice deals on SAM7 boards at Sparkfun. It's a 32-bit ARM
architecture with a USB port. I'd like to tinker with it. Some while back I
bought the P256 board ($72 at the time) and haven't yet gotten to it. There is
a getting-started `article by Adam Pierce`_.

.. _article by Adam Pierce: http://www.doctort.org/adam/nerd-notes/getting-started-with-the-olimex-sam7-p256.html

A hugely important piece of doing SAM7 development is getting the toolchain to
work.  I'm working on Ubuntu 10.04 (Lucid) and I've modified a script for this
purpose; see `make-toolchain.py`_.  I modded Adam Pierce's LED blinker
demo a bit to work with the resulting toolchain.  It can run out of RAM or flash.
See ARMBlinkExample/Makefile for details.

.. _make-toolchain.py: https://github.com/wware/stuff/blob/master/sam7/make-toolchain.py

I also got a USB/HID mouse demo working so that the two buttons on Sparkfun's P256
board cause the cursor to move left and right.

So time to make some plans about what to do next. I kicked around the idea of
a SAM7-based `KIM-1`_-esque board with a hex keypad, LED or LCD display, and a
little monitor program. But when it can so easily connect via USB to a laptop,
that seems silly. It might make sense to put a KIM-1-style GUI on the laptop
screen, maybe.

.. _`KIM-1`: http://en.wikipedia.org/wiki/KIM-1

I've also thought about writing a little language interpreter (probably Forth)
to run on the board and make it easy to tinker. I dunno, probably a crappy
idea.

I want to get the thing doing a USB serial port, so that it could do a little
interpreter if it made sense. That would open up a lot of other possibilities.

In the longer term I'd like to make it easy to tinker with all the various
hardware pieces on the chip. I don't know what that would look like, but there
are a lot of them and they're complicated. The idea would be something like
you could make some standardized use of each thing, and if you needed to dig
in and learn more about it, there would be some kind of helpful support for
doing that.

Resources
---------

* http://www.doctort.org/adam/nerd-notes/getting-started-with-the-olimex-sam7-p256.html
  - Adam Pierce's article mentioned above
* http://frank.circleofcurrent.com/mcu_proj_db/ - a nice list of various
  people's projects using ARM processors and microcontrollers
* http://www.atmel.com/dyn/resources/prod_documents/doc6175.pdf -
  AT91SAM7S datasheet
* http://claymore.engineer.gvsu.edu/~steriana/Software/Sam_I_Am/ -
  Sam-I-Am website
