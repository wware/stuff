#!/bin/sh

/bin/sh autogen.sh --prefix=/usr || exit 1
echo "#define ENABLE_DEBUG_LOGGING 1" >> config.h
make || exit 1
make install || exit 1
