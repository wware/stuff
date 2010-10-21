Remastering Damn Small Linux
============================

I need to do some DSL remastering for a work project. No proprietary
stuff here, just how-to stuff.

Stuff in the Makefile
---------------------

The general idea here is that whatever is in the additions directory
tree will be copied into your filesystem when you boot DSL. It's
useful to take a look at the DSL boot process:
http://www.damnsmalllinux.org/wiki/index.php/DSL_Boot_Process
Whatever is in etc/skel will be copied to your home directory.

The makefile takes a standard DSL 4.4.9 distribution and modifies it
by adding the stuff from the additions tree. Both the original version
and the modified version are embedded zip files. I haven't yet
figured out how to produce a bootable ISO, which I would love to do
as I could then use it with QEMU.

Writing apps with GUIs?
-----------------------
DSL is not a friend to the developer. Afaict the only way to write an
app with a GUI, short of dedicating an entire machine and several days
to becoming a DSL dev guru, is to write in Lua using FLTK as the GUI
toolkit. Whether even this is possible is currently an open question.

Creating a bootable USB stick
-----------------------------

This procedure is mostly taken from
http://www.damnsmalllinux.org/wiki/index.php/Install_to_USB_From_within_Linux
Download a copy of dsl-4.4.9-embedded.zip, or use the stuff in this
project to create a modified zip file. Use fdisk to partition the USB
stick (give it one VFAT 32 W95 partition) and mkfs.vfat -F 32 to set
up the filesystem. Mount the USB stick (sudo mount /dev/sdb1 /flash),
unzip the zip file onto it, and unmount it. Then do "syslinux -s
/dev/sdb1". Then use the "a" command in fdisk to make the partition
bootable. The resulting USB stick will boot on a Windows laptop.

When booting with a USB mouse, you may need to use the boot option
"dsl xsetup" and choose the Xfbdev server.

Random Lore
-----------

I know my usage of mkisofs is not quite correct. I can tell this
because some permissions are messed up in the /etc directory. I've
already spent hours on different permutations of the command line
options for mkisofs, and I've realized that what I've got is good
enough for what I need right now. Becoming a DSL guru would take a lot
more time than I have available.

I got a command line more like this from http://www.damnsmalllinux.org/wiki/index.php/Hacking_DSL::

  mkisofs -hide-rr-moved -allow-leading-dots -R -l -V "TWEAKED VERSION" -v -allow-multidot my-knoppix-tree | \
    ...other stuff in my makefile...

http://www.engineering.uwaterloo.ca/twiki/bin/view/Linux/RemasteringGuide
recommends something like this::

  mkisofs -R -U -V "KNOPPIX.net filesystem" -publisher "KNOPPIX www.knoppix.net" \
     -hide-rr-moved -cache-inodes -no-bak -pad source/KNOPPIX | \
     nice -5 create_compressed_fs - 65536 > master/KNOPPIX/KNOPPIX
  cd master
  rm -f KNOPPIX/md5sums; find -type f -not -name md5sums -not -name boot.cat \
    -not -name isolinux.bin -exec md5sum '{}' \; >> KNOPPIX/md5sums
  mkisofs -pad -l -r -J -v -V "KNOPPIX" -no-emul-boot -boot-load-size 4 -boot-info-table \
    -b boot/isolinux/isolinux.bin -c boot/isolinux/boot.cat -hide-rr-moved -o dsl.iso .

This guy just seems to be, uh, incorrect.
http://www.rootninja.com/how-to-add-files-to-an-iso-image-on-any-linux-distribution/
