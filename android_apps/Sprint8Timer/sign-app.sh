#!/bin/sh

git clean -xdf .
ant release || exit 1

cp bin/Sprint8Timer-unsigned.apk Sprint8Timer-unaligned.apk
jarsigner -verbose -keystore ~/android-release-key.keystore \
    Sprint8Timer-unaligned.apk android || exit 1
zipalign -f -v 4 Sprint8Timer-unaligned.apk Sprint8Timer.apk || exit 1

# are we on the macbook?
if [ -d /mnt/hgfs/staffhome\ On\ My\ Mac/Desktop ]; then
    cp Sprint8Timer.apk /mnt/hgfs/staffhome\ On\ My\ Mac/Desktop
fi
