#!/usr/bin/python

import os
import sys


OPF_PREAMBLE = """<?xml version="1.0"?>
<package version="2.0" xmlns="http://www.idpf.org/2007/opf" unique-identifier="BookId">

  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:opf="http://www.idpf.org/2007/opf">
    <dc:title>%(title)s</dc:title>
    <dc:language>en</dc:language>
    <dc:identifier id="BookId" opf:scheme="ISBN">%(isbn)s</dc:identifier>
    <dc:creator opf:file-as="%(author)s" opf:role="aut">%(author)s</dc:creator>
    <meta name="cover" content="cover-pic.png"/>
  </metadata>

  <manifest>
    <item id="tableofcontents" href="index.xhtml" media-type="application/xhtml+xml"/>
"""

OPF_ITEM = """\
    <item id="contents%(index)d" href="%(prefix)s.xhtml"
          media-type="application/xhtml+xml"/>
"""

OPF_POSTAMBLE = """\
    <item id="cover-pic.png" href="cover-pic.png" media-type="image/png"/>
    <item id="ncx" href="%(short)s.ncx" media-type="application/x-dtbncx+xml"/>
  </manifest>

  <spine toc="ncx">
    <itemref idref="tableofcontents" />
  </spine>

  <guide>
    <reference type="toc" title="Table of Contents" href="index.xhtml#toc_xhtml"/>
  </guide>

</package>
"""

NCX_PREAMBLE = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN"
 "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">
<ncx version="2005-1"
     xml:lang="en"
     xmlns="http://www.daisy.org/z3986/2005/ncx/">

  <head>
    <meta name="dtb:uid" content="%(isbn)s"/>
    <meta name="dtb:depth" content="1"/>
    <meta name="dtb:totalPageCount" content="0"/>
    <meta name="dtb:maxPageNumber" content="0"/>
  </head>

  <docTitle>
    <text>%(title)s</text>
  </docTitle>

  <docAuthor>
    <text>%(author)s</text>
  </docAuthor>
"""

INDEX_PREAMBLE = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
 "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head><title>Table of Contents</title></head>
<body>
<div id="toc_xhtml">
"""

INDEX_POSTAMBLE = """</div>
</body>
</html>"""


class NavPoint:

    def __init__(self, text, cls, htmldoc, htmlid):
        global playOrder
        self.text = text
        self.cls = cls
        self.doc = htmldoc
        self.id = htmlid

    def ncxXml(self, playorder):
        d = {"playorder": playorder}
        d.update(self.__dict__)
        return """\
          <navPoint class=\"%(cls)s\" id=\"%(id)s\"
               playOrder=\"%(playorder)d\">
            <navLabel><text>%(text)s</text></navLabel>
            <content src="%(doc)s#%(id)s">
          </navPoint>\n""" % d

    def indexHtml(self, first):
        d = {}
        d.update(self.__dict__)
        if self.cls == "chapter":
            r = """<a href="%(doc)s#%(id)s"><b>%(text)s</b></a><br/>
              <div><ul>
              """
            if not first:
                r = """</ul></div><br/>\n""" + r
            return r % d
        return """\
              <li><a href="%(doc)s#%(id)s">%(text)s</a></li>""" % d


class NavMap:

    def __init__(self):
        self.prefixes = []
        self.navpoints = []

    def loadFromPrefixes(self, prefixes):
        for p in prefixes:
            self.prefixes.append(p)
            self.addChapterFromPrefix(p)

    def addChapterFromPrefix(self, prefix):
        rstfile = prefix + ".rst"
        htmlfile = prefix + ".xhtml"
        if not os.path.exists(htmlfile):
            cmd = "rst2html " + rstfile + " " + htmlfile
            os.system(cmd)

        lines = open(htmlfile).readlines()
        for i in range(len(lines)):
            if lines[i].startswith("<div class=\"document\" id="):
                text = lines[i + 1][18:-6]
                id = lines[i][26:-3]
                self.addChapter(htmlfile, text, id)
            elif lines[i].startswith("<div class=\"section\" id="):
                text = lines[i + 1][4:-6]
                id = lines[i][25:-3]
                self.addSection(htmlfile, text, id)

    def addChapter(self, htmlfile, text, id):
        pt = NavPoint(text, "chapter", htmlfile, id)
        self.navpoints.append(pt)

    def addSection(self, htmlfile, text, id):
        pt = NavPoint(text, "section", htmlfile, id)
        self.navpoints.append(pt)

    def ncx(self, outf):
        outf.write(NCX_PREAMBLE + "\n  <navMap>\n")
        playOrder = 1
        for p in self.navpoints:
            outf.write(p.ncxXml(playOrder))
            playOrder += 1
        outf.write("  </navMap>\n</ncx>\n")

    def index(self, outf):
        outf.write(INDEX_PREAMBLE)
        first = True
        for p in self.navpoints:
            outf.write(p.indexHtml(first))
            first = False
        outf.write("</ul></div>")
        outf.write(INDEX_POSTAMBLE)

    def opf(self, outf):
        # TODO need a different kind of item to handle pictures
        outf.write(OPF_PREAMBLE)
        index = 1
        for p in self.prefixes:
            outf.write(OPF_ITEM % {"prefix": p, "index": index})
            index += 1
        outf.write(OPF_POSTAMBLE)


def doStuff(navmap, d):
    global OPF_PREAMBLE, OPF_POSTAMBLE, NCX_PREAMBLE

    OPF_PREAMBLE = OPF_PREAMBLE % d
    OPF_POSTAMBLE = OPF_POSTAMBLE % d
    NCX_PREAMBLE = NCX_PREAMBLE % d

    outf = open("%(short)s.ncx" % d, "w")
    navmap.ncx(outf)
    outf.close()

    outf = open("%(short)s.opf" % d, "w")
    navmap.opf(outf)
    outf.close()

    outf = open("index.xhtml", "w")
    navmap.index(outf)
    outf.close()

    os.system("%(kindlegen)s %(short)s.opf -c1 -verbose" % d)


if __name__ == '__main__':
    navmap = NavMap()
    navmap.loadFromPrefixes(sys.argv[1:])

    TITLE = "Random Stuff"
    AUTHOR = "Will Ware"
    ISBN = "1234567890X"
    SHORTTITLE = TITLE.replace(" ", "")
    doStuff(navmap, {
        "author": AUTHOR,
        "title": TITLE,
        "isbn": ISBN,
        "short": SHORTTITLE,
        "kindlegen": "/opt/kindlegen/kindlegen"
        })
