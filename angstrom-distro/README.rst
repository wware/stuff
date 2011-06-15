Fun with Android on Beagleboard
===============================

Here is some background about this stuff:
http://en.wikipedia.org/wiki/User:WillWare/Angstrom_and_Beagleboard

Go into your angstrom-setup-scripts directory and make symbolic links to
make-clean.sh and write-sd-card.sh. Then run make-tarballs.sh in this
directory, and it will make the two tarballs.

Now go into the angstrom-setup-scripts directory and run make-clean.sh
(because Bitbake LOVES to hang on to obsolete files) and do your bitbake
command. Then plug in your SD card and run write-sd-card.sh.

There are currently two example modules here. The helloworld module is a
minimal thing that prints "Hello world" as your board boots up by
installing itself as /etc/rc5.d/S52helloworld. This should be visible
in your terminal emulator.

The second module is quux-module, a kernel module that creates a simple
character device. Installing the module in your Angstrom build will put
the file quux.ko in your /lib/modules tree, and tryit.sh in your /home/root
directory::

 #!/bin/sh

 modprobe quux || echo Ouch

 MAJOR=$(dmesg | grep 'quux major' | tail -1 | sed 's/.*=//')
 echo MAJOR=${MAJOR}
 mknod /dev/quux c ${MAJOR} 0

 cat /dev/quux
 cat /dev/quux
 cat /dev/quux
 cat /dev/quux
 cat /dev/quux

When you run tryit.sh, you see this::

 [   37.023468] quux major=249
 [   37.026611] Try: mknod /dev/quux c 249 0
 MAJOR=249
 Kernel module says: Hello world!
 I already told you 1 times Hello world!
 I already told you 2 times Hello world!
 I already told you 3 times Hello world!
 I already told you 4 times Hello world!

So to try all these things, the steps are as follows.

1. Set up the symbolic links for make-clean.sh and
   write-sd-card.sh as described above.
2. Copy or symbolic-link the file wware-image.bb into your
   angstrom-setup-scripts/sources/openembedded/recipes/images
   directory.
3. Make sure your development machine has lighttpd installed.
4. In this directory, run make-tarballs.sh.
5. In the angstrom-setup-scripts directory, run make-clean.sh
   and run "bitbake wware-image".
6. If your SD card isn't yet set up, give it two partitions. The first
   is about 50 megabytes with label "boot" and filesystem VFAT32.
   The second is the remainder of the card with label "Angstrom"
   and filesystem EXT3.
7. When the bitbake command finishes, plug the SD card into a
   USB-SD-card adapter and plug that into your development machine,
   and run write-sd-card.sh.
8. When it finishes, remove the card, plug it into your Beagleboard,
   make sure the serial port is connected to a terminal emulator, and
   apply five volts (NOT MORE as you'll blow the board). You should
   get a boot prompt after seeing the "Hello world" message. Log in as
   "root" with no password. Type "./tryit.sh" and it should install
   the /dev/quux device and interact with it.
