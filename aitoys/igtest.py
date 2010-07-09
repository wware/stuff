#!/usr/bin/env python

import os, sys, string, random
import iguana

M = iguana.Machine()

factorial = """
DEFINE FACTORIAL
    DUP IF
        DUP 1 SUB FACTORIAL MUL RETURN
    ENDIF
    DROP 1 RETURN
"""

def stackdumpRun(thread):
    thread.instrucptr = thread.myProgram.startaddr
    while not thread.stopped:
        for i in range(thread.dsptr):
            print '%.8f' % thread[i],
        print
        thread.step()

def oneTest():
    M.compilestring(factorial)
    M.compilestring("""START 8 FACTORIAL PRINT CR
                       3.1415926 4. DDIV DSIN DDUP DMUL DPRINT CR RETURN""")
    M.decompile()
    x = M.Thread()
    x.run()
    print x.stopped

def speedTest():
    global x
    import profile
    M.compilestring(factorial)
    M.compilestring("START " +
                    20 * "50 FACTORIAL DROP " +
                    "STOP")
    x = M.Thread()
    profile.run('for i in range(1000): x.run()')

if len(sys.argv) > 1:
    speedTest()
else:
    oneTest()
