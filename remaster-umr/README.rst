Remastering Ubuntu Mini Remix
=============================

`Ubuntu Mini Remix <http://www.ubuntu-mini-remix.org/>`_, the creation of
Fabrizio Balliani, fits the bare essentials of the Ubuntu distribution into a
165 megabyte package that boots up quickly and can use minimal computing
resources. You can run it on old computers with small amounts of memory. You
can also use it to make kiosk-like boot disks that boot a user directly into a
specific application.

UMR is minimal. It does not have X Windows or GNOME or any windows manager. It
offers a command-line interface only. I found it was exactly what I needed for
a particular use, but you should understand its limitations before using it.

The customization script in this directory expects debian packages as command
line arguments, and it adds these to the UMR base. You can use an example
debian package that I've provided elsewhere in this repository::

 (cd ../make-debian-pkg/; make)
 ./customize.sh ../make-debian-pkg/foo-bar_1.0_i386.deb 

The script will ask for your password once or twice because it needs "sudo"
privileges in some places. It will create the file
"customized-umr-10.10-i386.iso" in your home directory. You can run this in
VirtualBox, or you can use Ubuntu's StartupDiskCreator utility to put it on
a bootable USB stick.
