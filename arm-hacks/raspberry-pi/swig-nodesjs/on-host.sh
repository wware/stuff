#!/bin/sh

if [ ! -f foo_wrap.cxx ]
then
    swig -c++ -javascript -node foo.i
fi
