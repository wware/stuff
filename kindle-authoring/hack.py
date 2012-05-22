import makeindex
import os

makeindex.d['short'] = 'SSE'
makeindex.d['title'] = 'Streaming SIMD Extensions'

navmap = makeindex.NavMap()
htmlfile = 'Streaming_SIMD_Extensions.xhtml'

def chapter(title):
    navmap.addChapter(htmlfile, title, title.replace(' ', '_'))

def section(title):
    navmap.addSection(htmlfile, title, title.replace(' ', '_'))

chapter('Registers')
chapter('SSE Instructions')
section('Floating point instructions')
section('Integer instructions')
section('Other instructions')
chapter('Example')
section('Later versions')
section('Software and hardware issues')
section('Related links')
section('References')

outf = open("%(short)s.ncx" % makeindex.d, "w")
navmap.ncx(outf)
outf.close()

outf = open("%(short)s.opf" % makeindex.d, "w")
navmap.opf(outf)
outf.close()

outf = open("index.xhtml", "w")
navmap.index(outf)
outf.close()

os.system("%(kindlegen)s %(short)s.opf -c1 -verbose" % makeindex.d)
