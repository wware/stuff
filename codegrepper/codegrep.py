# code grepper

import os, sys, string, re, cPickle

fileList = [ ]

def microsoftChooser(f):
    return (f.endswith(".cs") or
            f.endswith(".xaml") or
            f.endswith(".reg") or
            f.endswith("proj") or
            f.endswith(".txt") or
            f.endswith(".h") or
            f.endswith(".c") or
            f.endswith(".cpp"))

def linuxChooser(f):
    return (f.endswith(".java") or
            f.endswith(".xml") or
            f.endswith(".html") or
            f.endswith(".txt") or
            f.endswith(".h") or
            f.endswith(".c") or
            f.endswith(".cpp"))

def fetch(path, chooser=None):
    if chooser is None:
        if sys.platform == "win32":
            chooser = microsoftChooser
        elif sys.platform == "linux2":
            chooser = linuxChooser
        else:
            assert False
    holder=[]
    for root, dirs, files in os.walk(path):
        for f in files:
            if chooser(f):
                holder.append(os.path.join(root, f))
    return sorted(holder)

def makeIndex(fileList):
    forward = { }
    reverse = { }
    for i in range(len(fileList)):
        f = fileList[i]
        r = open(f).read()
        these = { }
        for j in range(len(r) - 2):
            triplet = r[j:j+3]
            these[triplet] = 1
            if not forward.has_key(triplet):
                forward[triplet] = { }
            forward[triplet][i] = 1
        reverse[i] = these.keys()
    for triplet in forward.keys():
        forward[triplet] = forward[triplet].keys()
    return (forward, reverse)

"""
When one of the source files changes, updating the dictionary is a pain. If
I can solve that problem, this is a good approach. I can use reverse for that
when I get around to it.
"""

sourceDir = "s:\\src\\theseus_v1_main_green\\src\\Pink\\Client\\source"
indexDir = "s:\\src\\theseus_v1_main_green\\codegrep.index"

def loadFileList(idir=None):
    global fileList
    if idir is None:
        idir = indexDir
    fileList = cPickle.load(open(os.path.join(idir, "fileList")))

def load(idir=None):
    if idir is None:
        idir = indexDir
    fileList = loadFileList(idir)
    #inf = open(os.path.join(idir, "forward"))
    #forward = cPickle.load(inf)
    #inf.close()
    inf = open(os.path.join(idir, "reverse"))
    reverse = cPickle.load(inf)
    inf.close()
    #return (fileList, forward, reverse)
    return (fileList, reverse)

def save(fileList, forward, reverse, idir=None):
    if idir is None:
        idir = indexDir
    d = { }
    triplets = forward.keys()
    for triplet in triplets:
        firstChar = triplet[0]
        try:
            d[firstChar].append(triplet)
        except KeyError:
            d[firstChar] = [ triplet ]
    for firstChar in d.keys():
        fname = os.path.join(idir,
                             "D%06d" % ord(firstChar))
        outf = open(fname, "w")
        d2 = { }
        for triplet in d[firstChar]:
            d2[triplet] = forward[triplet]
        cPickle.dump(d2, outf)
        outf.close()
    outf = open(os.path.join(idir, "fileList"), "w")
    cPickle.dump(fileList, outf)
    outf.close()
    outf = open(os.path.join(idir, "reverse"), "w")
    cPickle.dump(reverse, outf)
    outf.close()

def changedFiles(idir=None, sdir=None):
    lst = [ ]
    if idir is None:
        idir = indexDir
    reftime = os.path.getmtime(os.path.join(idir, "fileList"))
    for fname in L:
        ftime = os.path.getmtime(fname)
        if ftime >= reftime:
            lst.append(fname)
    return lst

def _prehunt(triplet, idir):
    assert len(triplet) == 3
    try:
        fname = os.path.join(idir, "D%06d" % ord(triplet[0]))
        inf = open(fname)
        lst = cPickle.load(inf)[triplet]
        inf.close()
        return lst
    except KeyError:
        return [ ]

def hunt(query, idir=None):
    if idir is None:
        idir = indexDir
    for i in _prehunt(query[:3], idir):
        fname = fileList[i]
        inf = open(fname)
        r = inf.read()
        inf.close()
        try:
            r.index(query)
            lst.append(i)
            continue
        except ValueError:
            pass
    lst.sort()
    return lst

def h(query):
    for x in hunt(query):
        print fileList[x]

def hunti(query, idir=None):
    if idir is None:
        idir = indexDir
    up, lw = string.upper, string.lower
    lst = [ ]
    for (fx, fy, fz) in ((up, up, up), (up, up, lw),
                         (up, lw, up), (up, lw, lw),
                         (lw, up, up), (lw, up, lw),
                         (lw, lw, up), (lw, lw, lw)):
        q = fx(query[0]) + fy(query[1]) + fz(query[2])
        for i in _prehunt(query[:3], idir):
            if i not in lst:
                lst.append(i)
    lst2 = [ ]
    for i in lst:
        fname = fileList[i]
        inf = open(fname)
        r = string.lower(inf.read())
        inf.close()
        try:
            r.index(string.lower(query))
            lst2.append(i)
            continue
        except ValueError:
            pass
    lst2.sort()
    return lst2

def hi(query):
    for x in hunti(query):
        print fileList[x]

def prep(sdir=None):
    if sdir is None:
        sdir = sourceDir
    L = fetch(sdir)
    f, r = makeIndex(L)
    save(L, f, r)

######################################

if False:
    try:
        L, f, r = load()
    except IOError:
        prep()
        L, f, r = load()

if __name__ == "__main__":
    loadFileList()
    h(sys.argv[1])
