#!/bin/sh

# I seem to be havin an issue with not getting my SD card squeaky clean.

rm -rf /media/Angstrom/* || exit 1
sync
sleep 1

rm -rf /media/Angstrom/* || exit 1
sync
sleep 1

bunzip2 < build/tmp-angstrom_2008_1/deploy/glibc/images/beagleboard/Angstrom-wware-image-glibc-ipk-2011.03-beagleboard.rootfs.tar.bz2 | (cd /media/Angstrom/; sudo tar xf - || exit 1)
eject /media/Angstrom || exit 1
