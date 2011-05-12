#!/bin/bash

export SYSROOTS=${HOME}/angstrom-setup-scripts/build/tmp-angstrom_2008_1/sysroots
export PATH=${PATH}:${SYSROOTS}/i686-linux/usr/armv7a/bin
export ARCH=arm
export CROSS_COMPILE=arm-angstrom-linux-gnueabi-
export TARGETDIR=${SYSROOTS}/beagleboard-angstrom-linux-gnueabi/kernel

make -C $TARGETDIR M=$(pwd) modules
