#!/usr/bin/python
import cgi
import os
import re
import urllib
import HTMLParser

DEBUG=False

print "Content-Type: plain/text"
print
r1 = re.compile("<textarea [^>]*>")
r2 = re.compile("</textarea>")
r3 = re.compile("=+ (rdf|RDF) =+")
r4 = re.compile("title=[A-Z][_:a-zA-Z0-9]*")

pagename = "Main_Page"
if os.environ.has_key('QUERY_STRING'):
    qs = os.environ['QUERY_STRING']
    m = r4.search(qs)
    if m is not None:
        pagename = qs[m.start()+6:m.end()]

if DEBUG:
    print 'pagename=' + pagename

url = "http://localhost/wiki/index.php?title=" + pagename + "&action=edit"
R = urllib.urlopen(url).read()
R = R[r1.search(R).end():r2.search(R).start()]
R = HTMLParser.HTMLParser().unescape(R).split('\n')
if DEBUG:
    print R
    print '=========================='

rdfState = 0
for L in R:
    m = r3.search(L)
    if rdfState == 0 and m is not None:
        rdfState = 1
    elif rdfState == 1 and L == '<pre>':
        rdfState = 2
    elif L == '</pre>':
        rdfState = 0
    elif rdfState == 2:
        print L
