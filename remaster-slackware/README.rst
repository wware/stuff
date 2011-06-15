Remastering Slackware 13.1 from the DVD
=======================================

I'm doing this on an Ubuntu machine, with a full Slackware instance
running in VirtualBox. The easiest way to move files back and forth is
to run SSHD on the Ubuntu box, but I had to muddle around a bit to
make it work.

Anyway, copy this script over to the Slackware instance and run it to
produce a bootable ISO. The easiest way to transfer the ISO to a
bootable USB stick is using Ubuntu's StartupDiskCreator. Use it once
from the menus to simply erase the USB stick, and then to actually
write the funky Slackware ISO you need to coerce the choice of image,
otherwise it will look for mysterious undocumented Ununtu specifics.
So do this::

 usb-creator-gtk -i Desktop/liveslack.iso

Hmm, that didn't work. What I think will work is to use the
livecd-tools package available ONLY on Fedora, so I'm going to set up
a VirtualBox Fedora instance. Fun fun fun. Crap -- a VBox Fedora won't
do it, I need to use a live CD of Fedora. It took 4 or 5 hours to
learn that, while doing other things at the same time.

My current problems are:
- I expected to see lots of stuff on the ISO file that doesn't appear
  to be there. Stuff like Python, Guile, etc, that I distinctly
  remember adding to the image.
- It does appear to boot really fast (no X Windows, no networking) but
  it requires the user to make some choices, and then log in, and I
  want it to go straight to a login session without any of that.

Debugging the problem means capturing the stdout and stderr while the
script runs. I don't need the whole script for that, I can have it
bail as soon as the installation of "d" packages is finished.

A Slackware-Ubuntu chimera
--------------------------

I can make a Slackware ISO that boots up just fine in a VirtualBox
environment. But I've tried several different tools to turn that into
a bootable USB stick, and it has proven unreasonably difficult. So the
new plan is as follows.

Both Slackware and Ubuntu build an ISO by creating a target filesystem
(the chroot jail), compressing it, and packaging up the compressed
filesystem with a kernel, an initrd image, and other paraphenalia to
boot into that filesystem. The only problem is that the Slackware
paraphenalia appear to be broken, because apparently bootable USB
sticks aren't a priority for the Slackware community.

So my thought is to tar up the Slackware filesystem, move it to the
Ubuntu world, and use Ubuntu's tools to stick it in what would
otherwise be an Ubuntu bootable USB stick. There are potential issues
like maybe the kernel will be the wrong version. But it currently
looks like the quickest way to get to what I want, which is a Linux
distro with very little extra stuff that boots really fast.

Oops, that doesn't work at all. The Ubuntu boot stuff requires that
apt-get package stuff must exist in the image.

Ubuntu without X?
-----------------

If Ubuntu can be built without X Windows, just running in text mode,
that would do it for me too.

 http://ubuntuforums.org/showthread.php?t=249282
  Download the alternative CD and do a server install.
  Then choose the packages you want.

 http://ubuntuforums.org/showthread.php?t=249282
  You can keep gnome\kde from coming up by just hiding the lancher
  files. I had to do it once to get a vid card to work right::

   sudo mv /etc/init.d/gdm /etc/init.d/gdm.backup

  it will fail to find gdm and default to the console and that way if
  you ever do decide to use gnome it will still be there

On Ubuntu 9.10, you have a directory /etc/init.d/gdm, while on 10.04,
you have a file /etc/init.d/gdm.conf. I think moving it aside is
supposed to work in either case.

I will feel very very stupid if moving gdm was all that was required
to make this work. OK, I tried it, and it didn't work for me. X still
came up. Time to try the server install.

But I do want to find out about this server install. That sounds
really useful.

Handy Ubuntu command for seeing which package sizes are biggest::

 dpkg-query -W --showformat='${Installed-Size;10}\t${Package}\n' | sort -k1,1n

So I tried making a Ubuntu 9.10 server VBox instance and it went
really smoothly, it boots fast, and it's not unreasonably large. The
uncompressed filesystem is around 780 MB. Very good. The next step is
to extract the filesystem from VirtualBox as something mountable::

 VBoxManage clonehd Ubuntu910Server.vdi usrvr.vmdk --format VMDK
 qemu-img convert -f vmdk ~/.VirtualBox/HardDisks/usrvr.vmdk \
     -O raw ~/usrvr.raw
 mount -o loop ~/usrvr.raw ~/tmp

