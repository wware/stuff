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
