import makeindex
import os

TITLE = "Streaming SSE Extensions"
AUTHOR = "Wikipedia"
ISBN = "1234567890X"
SHORTTITLE = TITLE.replace(" ", "")
d = {
    "author": AUTHOR,
    "title": TITLE,
    "isbn": ISBN,
    "short": "SSE",
    "kindlegen": "/opt/kindlegen/kindlegen"
    }

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

makeindex.doStuff(navmap, d)
