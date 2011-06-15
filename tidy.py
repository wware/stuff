#!/usr/bin/python
# This script attempts to clean up text (non-binary) files. Any trailing spaces
# are removed. CRLF line endings are converted to LF. Extra blank lines at the
# end of the file are removed.

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
        previousLineLength = len(line)
        lines[i] = line + '\n'
    i = len(lines) - 1
    while i > 1 and lines[i] == '\n' and lines[i-1] =='\n':
        i -= 1
    lines = lines[:i+1]
    outf = open(file, 'w')
    for line in lines:
        outf.write(line)
    outf.close()

