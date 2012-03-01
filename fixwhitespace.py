#!/usr/bin/python

import re, sys, string

DEBUG = False
tabWidth = 8

argv = sys.argv[1:]

while len(argv) > 1:
    if argv[0][:1] != "-":
        break
    elif argv[0] == "-d":
        DEBUG = True
        argv.pop(0)
    elif argv[0] == "-t":
        argv.pop(0)
        tabWidth = string.atoi(argv.pop(0))

trailingSpaces = re.compile(" +\n$")

for filename in argv:

    if DEBUG:
        print filename

    fileChanged = False
    newlines = [ ]
    lines = open(filename).readlines()

    for L in lines:

        # untabify
        while True:
            n = -1
            try:
                n = L.index("\t")
            except ValueError:
                break
            spaces = (tabWidth - (n % tabWidth)) * " "
            L = L[:n] + spaces + L[n+1:]
            fileChanged = True

        # remove trailing whitespace
        s = trailingSpaces.search(L)
        if s is not None:
            fileChanged = True
            L = L[:s.start()] + "\n"

        newlines.append(L)

    if fileChanged:
        print filename
        outf = open(filename, "w")
        for L in newlines:
            outf.write(L)
        outf.close()
