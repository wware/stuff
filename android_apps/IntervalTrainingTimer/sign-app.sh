#!/bin/sh

APPNAME=IntervalTrainer

git clean -xdf .
ant release || exit 1

cp bin/${APPNAME}-unsigned.apk ${APPNAME}-unaligned.apk
jarsigner -verbose -keystore ~/android-release-key.keystore \
    ${APPNAME}-unaligned.apk android || exit 1
zipalign -f -v 4 ${APPNAME}-unaligned.apk ${APPNAME}.apk || exit 1

# are we on the macbook?
if [ -d /mnt/hgfs/staffhome\ On\ My\ Mac/Desktop ]; then
    cp ${APPNAME}.apk /mnt/hgfs/staffhome\ On\ My\ Mac/Desktop
fi
