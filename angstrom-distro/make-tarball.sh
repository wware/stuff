#!/bin/bash

(cd hello-on-boot; tar cfj ../helloworld-1.0.0.tar.bz2 *)

(cat helloworld_1.0.0.bb.1
printf "SRC_URI[md5sum] = \"%s\"\n" \
    $(md5sum helloworld-1.0.0.tar.bz2 | sed 's/ .*//')
printf "SRC_URI[sha256sum] = \"%s\"\n" \
    $(sha256sum helloworld-1.0.0.tar.bz2 | sed 's/ .*//')) > helloworld_1.0.0.bb

################

if [ ! -f kernel-module/quux.ko ]; then
    (cd kernel-module; ./build.sh)
fi

(cd kernel-module; tar cfj ../quux-module-1.0.0.tar.bz2 quux.ko tryit.sh)

(cat quux-module_1.0.0.bb.1
printf "SRC_URI[md5sum] = \"%s\"\n" \
    $(md5sum quux-module-1.0.0.tar.bz2 | sed 's/ .*//')
printf "SRC_URI[sha256sum] = \"%s\"\n" \
    $(sha256sum quux-module-1.0.0.tar.bz2 | sed 's/ .*//')) > quux-module_1.0.0.bb

################

lighttpd -D -f lighttpd.conf
