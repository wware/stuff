#!/usr/bin/python
# Example of how to use the CmdLine library.

import os
import sys
from cmdline import CmdLine

CmdLine.intArg("x#int-x#set integer X")
CmdLine.floatArg("y#float-y#set float Y")
CmdLine.stringArg("b#blab#set a string that blabs", "blab")
CmdLine.flag("d#debug#enable debugging")
CmdLine.flag("L#very-very-very-long-command-line-option#does nothing")

@CmdLine.command
def blab():
    """
    blab: Blabbity blab blab blab
    """
    for i in range(5):
        print CmdLine.getString("blab")

@CmdLine.command
def snore():
    """
    snore: print "Zzzzzzzzzzzzz"
    """
    print "Zzzzzzzzzzzzz"

@CmdLine.command
def showOpts():
    """
    show: print values of command line options
    """
    print "debug", CmdLine.getFlag("debug")
    print "x", CmdLine.getInt("int-x")
    print "y", CmdLine.getInt("float-y")

@CmdLine.command
def test():
    """
    test: run a test of this example
    and when it's tested, test it some more
    """
    def trythis(cmd, expected):
        assert os.popen(cmd).read().strip() == expected
    trythis(sys.argv[0] + " snore", "Zzzzzzzzzzzzz")
    trythis(sys.argv[0] + " show", "debug False\nx 0\ny 0")
    trythis(sys.argv[0] + " -x 5 show", "debug False\nx 5\ny 0")
    trythis(sys.argv[0] + " -y 1.7 show", "debug False\nx 0\ny 1.7")

if __name__ == "__main__":
    CmdLine.handle_all(sys.argv[1:])
