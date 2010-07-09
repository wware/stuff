#!/usr/bin/env python

"""Job control script for LinuxTuples.
jobcontrol.py log   -- show a continuous running log of tuple server activity
jobcontrol.py dump  -- display the current contents of tuple
                       space; tuple elements truncated for
                       brevity when longer than 40 characters
jobcontrol.py size  -- tell how many tuples are in tuple space
jobcontrol.py jobs  -- tell what jobs are currently running on
                       the various machines in the cluster
jobcontrol.py empty -- empty all tuples from tuple space
jobcontrol.py start <program> <N> -- start N instances of a
                                     program distributed around the cluster
jobcontrol.py stop  -- stop all programs distributed around the cluster
jobcontrol.py test  -- run some tests to verify that the tuple server is alive"""

##################################################################################
##
## Be sure you have set your LINUXTUPLES_HOST and LINUXTUPLES_PORT env vars
## or add the host and port parms to the linuxtuples.connect function call 
## on line 52
##
##################################################################################

import sys, os
import time
import types

import linuxtuples

##################################################################################

VALID_OPERATIONS = "log dump size jobs empty start stop test".split()

# Read the slaves from the SLAVES file.
SLAVES = [ ]
for line in open("SLAVES").readlines():
    for machine in line.split():
        SLAVES.append(machine)

STRING_LIMIT = 40

##################################################################################

def main(args):
    if len(args) < 1 or args[0] in ["?", "-?", "help", "-help", "--help"]:
        print __doc__
        sys.exit(0)

    operation = args[0]
    if operation not in VALID_OPERATIONS:
        print __doc__
        sys.exit(0)
        
    # conn = linuxtuples.connect("desktop", 27000)
    conn = linuxtuples.connect()

    if operation == "log":
        doLogging(conn)

    elif operation == "dump":
        doDump(conn)

    elif operation == "size":
        print conn.count()

    elif operation == "jobs":
        for tup in conn.dump([("job running", None, None, None)]):
            print tup

    elif operation == "empty":
        empty_tuple_space(conn)

    elif operation == "start":
        name  =     args[1]
        count = int(args[2])
        doStart(conn, name, count)

    elif operation == "stop":
        doStop(conn)

    elif operation == "test":
        doTest(conn)

##################################################################################

def doLogging(conn):
    while 1:
        conn.log()
        time.sleep(.1)

##################################################################################

def doDump(conn):

    global STRING_LIMIT
    def brief_dump(tup):
        def printable(str):
            str2 = ""
            for ch in list(str):
                if ord(ch) > 32 and ord(ch) < 126:
                    str2 = str2 + ch
                else:
                    str2 = str2 + '?'
            return str2

        items = []
        for item in tup:
            if type(item) == types.StringType and len(item) > STRING_LIMIT:
                item = printable(item[:STRING_LIMIT]) + "..."
            items.append(item)

        if len(items) == 0:
            print "( )"
        elif len(items) == 1:
            print "(%s)" % repr(items.pop(0)) 
        else:
            print "(%s," % repr(items.pop(0)) 
            while 1:
                if len(items) == 1:
                    print "\t%s)" % repr(items.pop(0))
                    break
                print "\t%s," % repr(items.pop(0))

    for tup in conn.dump():
        brief_dump(tup)

##################################################################################

def empty_tuple_space(conn):
    for size in range(1, 30):         # why is the range size 30 items?
        template = tuple(size * [None,])
        while 1:
            if not conn.get_nonblocking(template):
                break

##################################################################################

def doStart(conn, name, count):
    try:
        # file is in the current directory
        inf = open(name)
        inf.close()
        fullname = os.popen('pwd').readline()[:-1] + '/' + name
    except IOError:
        # try to find it on the path
        fullname = os.popen("which " + name).readline()[:-1]
        if not fullname:
            raise IOError, "can't find file " + name
    name = fullname
    for i in range(count):
        whichslave = SLAVES[i % len(SLAVES)]
        conn.put(("process is running", whichslave, name))
        if os.fork() == 0:
            os.execv("/usr/bin/ssh", ["ssh", whichslave, name])

##################################################################################

def doStop(conn):
    while 1:
        tupleItem = conn.get_nonblocking(("process is running", None, None))
        if not tupleItem:
            break
        whichslave, name = tupleItem[1:3]
        cmd = "ssh %s killall %s" % (whichslave, name)
        os.system(cmd)
    empty_tuple_space(conn)

##################################################################################

def doTest(conn):
    n = 4

    print "PUT"
    for i in range(n):
        x = ("abc", i)
        print x
        conn.put(x)

    print "READ"
    for i in range(n):
        print conn.read( ("abc", None) )

    print "DUMP"
    for tup in conn.dump():
        print tup

    print "GET"
    for i in range(n):
        print conn.get( ("abc", None) )

    print "READ_NB"
    print conn.read_nonblocking( ("def", None) )

    print "GET_NB"
    print conn.get_nonblocking( ("def", None) )

    print "PUT"
    conn.put(("def", "foobar"))

    print "DUMP"
    for tup in conn.dump():
        print tup

    print "READ_NB"
    print conn.read_nonblocking( ("def", None) )

    print "GET_NB"
    print conn.get_nonblocking( ("def", None) )

    print "DUMP"
    for tup in conn.dump():
        print tup

##################################################################################
##################################################################################

if __name__ == "__main__":
    main(sys.argv[1:])
