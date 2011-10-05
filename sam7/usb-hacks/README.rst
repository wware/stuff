Fun with USB and other things on the Olimex SAM7S64 header board
================================================================

Lots of handy stuff in FreeRTOS/Demo/Common/drivers/Atmel/at91lib/peripherals.
Other resources:

* http://gandalf.arubi.uni-kl.de/avr_projects/arm_projects/index_at91.html
* http://svn.openpcd.org/trunk/firmware/src/picc/adc.c

To program the SAM7S64 header board, put the jumper on the TST pins (the two
closest to the edge) and plug it into the USB cable for ten seconds, then
unplug it from the cable. Remove the jumper (it's handy to put it on the other
two pins, which enables the green LED). Now you can program the board using
Sam_I_Am::

  make clean
  make prog

By changing the DEVICE_TYPE #define in main.c, you can choose between a USB
HID device that behaves like a mouse, and a CDC/ACM device that either echos
stuff or prints log messages. The mouse hack will move the cursor left and
right. The CDC/ACM hack is currently set up to simply echo lines of ASCII::

  % python hack.py supercalifragilistic
  supercalifragilistic
  supercalifragilistic
  supercalifragilistic
  supercalifragilistic
  supercalifragilistic

It looks like I need to alternate Read and Write operations for the CDC-ACM
device, which is non-ideal because I want to use Write for debug logging,
and it means I need to send at least one character in order to get a message
back. It's not really full-duplex as I would expect.

Me think gooder some day
------------------------

Maybe when I'm smarter about USB, I'll know how to fix this behavior. I should
probably be using interrupts instead of polling the USB.
