#!/bin/sh

git clean -xdf .
ant release || exit 1

cp bin/MusicKeyboard-unsigned.apk MusicKeyboard-unaligned.apk
jarsigner -verbose -keystore ~/android-release-key.keystore \
    MusicKeyboard-unaligned.apk android || exit 1
zipalign -f -v 4 MusicKeyboard-unaligned.apk MusicKeyboard.apk || exit 1

# are we on the macbook?
if [ -d /mnt/hgfs/staffhome\ On\ My\ Mac/Desktop ]; then
    cp MusicKeyboard.apk /mnt/hgfs/staffhome\ On\ My\ Mac/Desktop
else
    cp MusicKeyboard.apk ~
fi
