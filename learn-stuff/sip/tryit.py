#!/usr/bin/env python
# http://pyqt.sourceforge.net/Docs/sip4/using.html

import os

HAVE_CPP_FILE = False

########## configure.py ##############

if not os.path.exists("sharedptr.so"):
    if not os.path.exists("sharedptr.o"):
        if HAVE_CPP_FILE:
            incl = '-I/usr/local/Cellar/boost/1.55.0_1/include'
            incl += ' -I/usr/local/Cellar/qt/4.8.6/include'
            assert os.system("g++ -c {0} -o sharedptr.o sharedptr.cpp".format(incl)) == 0
    import sipconfig
    build_file = "sharedptr.sbf"
    config = sipconfig.Configuration()
    assert os.system(" ".join([config.sip_bin, "-c", ".", "-b", build_file, "sharedptr.sip"])) == 0
    makefile = sipconfig.SIPModuleMakefile(config, build_file)
    if HAVE_CPP_FILE:
        makefile.extra_lflags.append("sharedptr.o")
    makefile.extra_lflags.append("-F/usr/local/Cellar/qt/4.8.6/lib -framework QtCore")
    makefile.generate()
    assert os.system("make sharedptr.so") == 0

################ test ########################

import sharedptr

def newB(x):
    b = sharedptr.B()
    b.setX(x)
    return b

a = sharedptr.A()
b = newB(5)

a.b = b
assert b.getX() == 5
assert a.b.getX() == 5

b.setX(11)

assert b.getX() == 11
assert a.b.getX() == 11

a.bList = [newB(2), newB(3), newB(5)]
assert [b.getX() for b in a.bList] == [2, 3, 5]

a.bList = [newB(3), newB(4)]
assert [b.getX() for b in a.bList] == [3, 4]

a.bList.append(newB(5))
# [b.getX() for b in a.bList] => [3, 4, 5]??? NO, because GetCode returns a different list.

print "Success"
