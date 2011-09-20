#!/usr/bin/python
import re, sys
DEBUG = False
argv = sys.argv[1:]

if argv[:1] == ['-d']:
    DEBUG = True
    argv.pop(0)

r = re.compile("[ \t]\n$")
for filename in argv:
    if DEBUG:
        print filename
    lines = open(filename).readlines()
    i = 1
    for L in lines:
        s = r.search(L)
        if s is not None:
            print filename, i
            if DEBUG:
                print s.start(), s.end()
                excess = L[s.start():s.end()]
                print repr(excess)
        i += 1
