#!/usr/bin/python

import cgi
import os
import re
import urllib

print "Content-Type: plain/text"
print

r1 = re.compile("<textarea readonly[^>]*>")
r2 = re.compile("</textarea>")
r3 = re.compile("===? (rdf|RDF) ===?")
r4 = re.compile("title=[A-Z][_a-zA-Z0-9]*")

pagename = "Main_Page"
if os.environ.has_key('QUERY_STRING'):
    qs = os.environ['QUERY_STRING']
    m = r4.search(qs)
    if m is not None:
        pagename = qs[m.start()+6:m.end()]

url = "http://localhost/wiki/index.php?title=" + pagename + "&action=edit"
R = urllib.urlopen(url).read()
R = R[r1.search(R).end():r2.search(R).start()].split('\n')

rdfState = 0
for L in R:
    m = r3.search(L)
    if rdfState == 0 and m is not None:
        rdfState = 1
    elif rdfState == 1 and L[:1] == ' ':
        rdfState = 2
        print L[1:]
    elif L[:1] == ' ':
        print L[1:]
    else:
        rdfState = 0
