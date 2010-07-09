#!/usr/bin/python
# Add commentary to some code

import os, sys, re

headerBoilerplate = """<html>
<title>%(filename)s</title>
<body bgcolor="#FFFFFF">
"""

jscriptBoilerplate= """
function open_window_%(linenum)d()
{
new_window_%(linenum)d = open("","hoverwindow","width=300,height=200,left=10,top=10");
new_window_%(linenum)d.document.open();
new_window_%(linenum)d.document.write("<html><title>Line %(linenum)d</title>");
new_window_%(linenum)d.document.write("<body bgcolor=\\"#FFFF00\\">");
new_window_%(linenum)d.document.write("%(text)s");
new_window_%(linenum)d.document.write("</body></html>");
new_window_%(linenum)d.document.close();
}
function close_window_%(linenum)d()
{
new_window_%(linenum)d.close();
}
"""

class Comments:
    def __init__(self, filename, d=None):
        self.filename = filename
        if d is not None:
            self.commentary = d
        else:
            self.commentary = { }
        self.lines = open(filename).readlines()

    def html(self, outfname):
        ss = sys.stdout
        outf = open(outfname, "w")
        class MyFile:
            def write(self, x):
                outf.write(x)
        sys.stdout = MyFile()
        print headerBoilerplate % {"filename": self.filename}
        if len(self.commentary) > 0:
            print "<script language=\"JavaScript\"><!--"
            for linenum in self.commentary.keys():
                text = self.commentary[linenum]
                print jscriptBoilerplate % {"linenum": linenum,
                                            "text": text}
            print "// -->"
            print "</script>"
        print "<pre>"
        i = 1
        for L in c.lines:
            L = "%8d" % i + " " + L.rstrip()
            if self.commentary.has_key(i):
                L = (("<a href=\"#\" " +
                      "onMouseOver=\"open_window_%(linenum)d()\" " +
                      "onMouseOut=\"close_window_%(linenum)d()\">") %
                     {"linenum": i} +
                     L + "</a>")
            print L
            i += 1
        print "</pre></body></html>"
        sys.stdout = ss

f = "scan.cu"   # borrow a file from NVIDIA, since I want to learn about CUDA

c = Comments(f, {
        176: "Invoke a kernel, use the GPUs"
        # There needs to be a way to refer to another file, and give
        # the option of opening a new tab for it (if not open)
        # scan_best_kernel.cu line 78
        })

c.html("tryit.html")

import webbrowser

webbrowser.open("tryit.html")
