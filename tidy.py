#!/usr/bin/python

import glob
import os
import re
import sys
import tempfile

lineEnding = re.compile('[ \t]*[\r\n]+$').search

filelist = [ ]
if len(sys.argv) == 1:
    files = map(lambda x: x.rstrip(),
                os.popen('git grep -l .').readlines())
    for file in files:
        if not (file.startswith('Binary file ')
                and file.endswith(' matches')):
            filelist.append(file)
else:
    for arg in sys.argv[1:]:
        filelist += glob.glob(arg)

for file in filelist:
    lines = open(file).readlines()
    previousLineLength = -1
    for i in range(len(lines)):
        line = lines[i]
        m = lineEnding(line)
        if m is not None:
            line = line[:m.start()]
        if i == len(lines) - 1 and previousLineLength == 0 \
                and len(line) == 0:
            continue
        previousLineLength = len(line)
        lines[i] = line + '\n'
    outf = open(file, 'w')
    for line in lines:
        outf.write(line)
    outf.close()


