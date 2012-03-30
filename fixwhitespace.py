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
        L2 = L.rstrip() + "\n"
        if L2 != L:
            fileChanged = True
            L = L2

        newlines.append(L)

    if fileChanged:
        print filename
        outf = open(filename, "w")
        for L in newlines:
            outf.write(L)
        outf.close()
