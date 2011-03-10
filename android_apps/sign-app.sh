#!/bin/bash

APPNAME=$(pwd | sed 's#.*/##')

git clean -xdf
ant release || exit 1

cp bin/${APPNAME}-unsigned.apk ${APPNAME}-unaligned.apk
jarsigner -verbose -keystore ~/android-release-key.keystore \
    ${APPNAME}-unaligned.apk android || exit 1
zipalign -f -v 4 ${APPNAME}-unaligned.apk ${APPNAME}.apk || exit 1
