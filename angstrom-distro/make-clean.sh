#!/bin/sh

find build/tmp-angstrom_2008_1/usr/lib \
    build/tmp-angstrom_2008_1/work/beagleboard* \
    build/tmp-angstrom_2008_1/pstage \
    build/tmp-angstrom_2008_1/deploy/glibc \
    build/tmp-angstrom_2008_1/pkgdata/beagleboard* \
    build/tmp-angstrom_2008_1/stamps/beagleboard* \
    build/tmp-angstrom_2008_1/sysroots/arm* \
    sources/downloads \
        \( -name '*quux*' -or -name '*helloworld*' -or -name '*wware*' \) -print -exec rm -rf {} \;
