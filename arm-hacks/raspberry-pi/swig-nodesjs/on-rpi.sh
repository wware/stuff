#!/bin/sh

if [ ! -f foo_wrap.cxx ]
then
	echo Run the on-mac.sh script on your host (Mac or Linux), and
	echo transfer foo_wrap.cxx to the Raspberry Pi.
	exit 1
fi

if [ ! -f ./build/Release/foo.node ]
then
    node-gyp clean || exit 1
    node-gyp configure || exit 1
    node-gyp build || exit 1
fi

node tryit.js
