import array
import cPickle
import os
import os.path
import stat
import re
import sys
import types

import crc16

# String ID mappings

numStrings = 0
R = ''
try:
    inf = open('/tmp/triplestore/numStrings')
    R = inf.read()
    inf.close()
except IOError:
    pass

if R:
    a = array.array('L')
    a.fromstring(R)
    numStrings = a[0]

def lookupStringId(str):
    global numStrings
    id = numStrings
    numStrings += 1

    a = array.array('B')
    a.fromstring(str)
    crc = crc16.crc16(a)
    filename = '/tmp/triplestore/stringid%04X' % crc
    if not os.path.exists(filename):
        stringToId = { str: id }
    else:
        inf = open(filename)
        stringToId = cPickle.load(inf)
        inf.close()
        if stringToId.has_key(str):
            # already got this string
            return stringToId[str]
        stringToId[str] = id
    outf = open(filename, 'w')
    cPickle.dump(stringToId, outf)
    outf.close()

    crc = int(((id >> 16) ^ id) & 0xffff)
    filename = '/tmp/triplestore/idstring%04X' % crc
    if not os.path.exists(filename):
        idToString = { id: str }
    else:
        inf = open(filename)
        idToString = cPickle.load(inf)
        inf.close()
        idToString[id] = str
    outf = open(filename, 'w')
    cPickle.dump(idToString, outf)
    outf.close()

    return id

def lookupIdString(id):
    crc = int(((id >> 16) ^ id) & 0xffff)
    filename = '/tmp/triplestore/idstring%04X' % crc
    assert os.path.exists(filename)
    inf = open(filename)
    idToString = cPickle.load(inf)
    inf.close()
    return idToString[id]

# S, P, and O Search Engines

s_lengths = array.array('L')
p_lengths = array.array('L')
o_lengths = array.array('L')

for i in range(65536):
    for prefix, ary in (('S', s_lengths),
                        ('P', p_lengths),
                        ('O', o_lengths)):
        filename = '/tmp/triplestore/%s%04X' % (prefix, i)
        try:
            st = os.stat(filename)
            n = st[stat.ST_SIZE] / 12
        except OSError:
            n = 0
        ary.append(n)

def readAndFilterTriples(filename, fltr):
    result = [ ]
    R = ''
    try:
        inf = open(filename)
        R = inf.read()
        inf.close()
    except IOError:
        pass
    while R:
        r, R = R[:12], R[12:]
        triplet = array.array('L')
        triplet.fromstring(r)
        s1, p1, o1 = map(int, triplet)
        if fltr(s1, p1, o1):
            result.append((s1, p1, o1))
    return result

def tripleExistsAlready(s, p, o):
    if type(s) is types.StringType:
        s = lookupStringId(s)
    if type(p) is types.StringType:
        p = lookupStringId(p)
    if type(o) is types.StringType:
        o = lookupStringId(o)
    sindex = int(((s >> 16) ^ s) & 0xffff)
    pindex = int(((p >> 16) ^ p) & 0xffff)
    oindex = int(((o >> 16) ^ o) & 0xffff)
    sCost = s_lengths[sindex]
    pCost = p_lengths[pindex]
    oCost = o_lengths[oindex]
    if sCost < pCost and sCost < oCost:
        fname = '/tmp/triplestore/S%04X' % sindex
    elif pCost < oCost:
        fname = '/tmp/triplestore/P%04X' % pindex
    else:
        fname = '/tmp/triplestore/O%04X' % oindex
    def fltr(s1, p1, o1, s=s, p=p, o=o):
        return s1 is s and p1 is p and o1 is o
    return len(readAndFilterTriples(fname, fltr)) > 0

def sSearch(s):
    if type(s) is types.StringType:
        s = lookupStringId(s)
    sindex = int(((s >> 16) ^ s) & 0xffff)
    fname = '/tmp/triplestore/S%04X' % sindex
    def fltr(s1, p1, o1, s=s):
        return s1 is s
    return readAndFilterTriples(fname, fltr)

def pSearch(p):
    if type(p) is types.StringType:
        p = lookupStringId(p)
    pindex = int(((p >> 16) ^ p) & 0xffff)
    fname = '/tmp/triplestore/P%04X' % pindex
    def fltr(s1, p1, o1, p=p):
        return p1 is p
    return readAndFilterTriples(fname, fltr)

def oSearch(o):
    if type(o) is types.StringType:
        o = lookupStringId(o)
    oindex = int(((o >> 16) ^ o) & 0xffff)
    fname = '/tmp/triplestore/O%04X' % oindex
    def fltr(s1, p1, o1, o=o):
        return o1 is o
    return readAndFilterTriples(fname, fltr)

def spSearch(s, p):
    if type(s) is types.StringType:
        s = lookupStringId(s)
    if type(p) is types.StringType:
        p = lookupStringId(p)
    sindex = int(((s >> 16) ^ s) & 0xffff)
    pindex = int(((p >> 16) ^ p) & 0xffff)
    fname = ((s_lengths[sindex] < p_lengths[pindex]) and
             ('/tmp/triplestore/S%04X' % sindex) or
             ('/tmp/triplestore/P%04X' % pindex))
    def fltr(s1, p1, o1, s=s, p=p):
        return s1 is s and p1 is p
    return readAndFilterTriples(fname, fltr)

def soSearch(s, o):
    if type(s) is types.StringType:
        s = lookupStringId(s)
    if type(o) is types.StringType:
        o = lookupStringId(o)
    sindex = int(((s >> 16) ^ s) & 0xffff)
    oindex = int(((o >> 16) ^ o) & 0xffff)
    fname = ((s_lengths[sindex] < o_lengths[oindex]) and
             ('/tmp/triplestore/S%04X' % sindex) or
             ('/tmp/triplestore/O%04X' % oindex))
    def fltr(s1, p1, o1, s=s, o=o):
        return s1 is s and o1 is o
    return readAndFilterTriples(fname, fltr)

def poSearch(p, o):
    if type(p) is types.StringType:
        p = lookupStringId(p)
    if type(o) is types.StringType:
        o = lookupStringId(o)
    pindex = int(((p >> 16) ^ p) & 0xffff)
    oindex = int(((o >> 16) ^ o) & 0xffff)
    fname = ((p_lengths[pindex] < o_lengths[oindex]) and
             ('/tmp/triplestore/P%04X' % pindex) or
             ('/tmp/triplestore/O%04X' % oindex))
    def fltr(s1, p1, o1, p=p, o=o):
        return p1 is p and o1 is o
    return readAndFilterTriples(fname, fltr)

def storeTriplet(s, p, o):
    if type(s) is types.StringType:
        s = lookupStringId(s)
    if type(p) is types.StringType:
        p = lookupStringId(p)
    if type(o) is types.StringType:
        o = lookupStringId(o)
    if tripleExistsAlready(s, p, o):
        return
    triplet = array.array('L')
    triplet.fromlist([s, p, o])
    def storeTripletHelper(x, prefix, ary):
        crc = int(((x >> 16) ^ x) & 0xffff)
        fname = '/tmp/triplestore/%s%04X' % (prefix, crc)
        if os.path.exists(fname):
            outf = open(fname, 'a')
        else:
            outf = open(fname, 'w')
        outf.write(triplet.tostring())
        outf.close()
        ary[crc] += 1
    storeTripletHelper(s, 'S', s_lengths)
    storeTripletHelper(p, 'P', p_lengths)
    storeTripletHelper(o, 'O', o_lengths)

bestBuyRdfs = [
    "http://products.semweb.bestbuy.com/products/8794691/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/17917499/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/17411383/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/7855172/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/7610962/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/5998254/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/6566138/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/17620479/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/6622425/semanticweb.rdf",
    "http://products.semweb.bestbuy.com/products/17411374/semanticweb.rdf",
    ]

CWM = '/home/wware/gc-merc/cwm-1.2.1/cwm'

url = re.compile("<http://[^>]*>")

if 'show' in sys.argv[1:]:
    for rdf in bestBuyRdfs:
        cmd = CWM + ' ' + rdf + ' --ntriples 2>/dev/null'
        inf = os.popen(cmd)
        while True:
            L = inf.readline()
            if not L:
                break
            L = L.strip()
            if L:
                S = url.search(L)
                start, end = S.start(), S.end()
                sUrl = L[start+1:end-1]  # lose "<" and ">"
                L = L[end:].strip()
                P = url.search(L)
                start, end = P.start(), P.end()
                pUrl = L[start+1:end-1]  # lose "<" and ">"
                o = L[end:].strip()
                assert o.endswith(' .')
                o = o[:-2].strip()
                if o.startswith("<http://"):
                    o = o[1:-1]   # lose "<" and ">"
                print 'S', repr(sUrl)
                print 'P', repr(pUrl)
                print 'O', repr(o)
                print
    sys.exit(0)

if 'load' in sys.argv[1:]:
    for rdf in bestBuyRdfs:
        cmd = CWM + ' ' + rdf + ' --ntriples 2>/dev/null'
        inf = os.popen(cmd)
        while True:
            L = inf.readline()
            if not L:
                break
            L = L.strip()
            if L:
                S = url.search(L)
                start, end = S.start(), S.end()
                sUrl = L[start+1:end-1]  # lose "<" and ">"
                L = L[end:].strip()
                P = url.search(L)
                start, end = P.start(), P.end()
                pUrl = L[start+1:end-1]  # lose "<" and ">"
                o = L[end:].strip()
                assert o.endswith(' .')
                o = o[:-2].strip()
                if o.startswith("<http://"):
                    o = o[1:-1]   # lose "<" and ">"
                storeTriplet(sUrl, pUrl, o)

def gr(x):
    return 'http://purl.org/goodrelations/v1#' + x

def rdfs(x):
    return 'http://www.w3.org/2000/01/rdf-schema#' + x

hasCurrencyValue = gr('hasCurrencyValue')
hasPriceSpecification = gr('hasPriceSpecification')
includesObject = gr('includesObject')
typeOfGood = gr('typeOfGood')
hasMakeAndModel = gr('hasMakeAndModel')
rdfsComment = rdfs('comment')
rdfsLabel = rdfs('label')

if 'tinker' in sys.argv[1:]:
    p = 'http://purl.org/goodrelations/v1#hasCurrencyValue'
    p2 = 'http://purl.org/goodrelations/v1#hasPriceSpecification'
    p3 = 'http://purl.org/goodrelations/v1#includesObject'
    p4 = 'http://purl.org/goodrelations/v1#typeOfGood'
    p5 = 'http://purl.org/goodrelations/v1#hasMakeAndModel'
    p6 = 'http://www.w3.org/2000/01/rdf-schema#comment'
    for s, p, o in pSearch(hasCurrencyValue):
        # o is a price, s is a price spec
        price = lookupIdString(o)[1:]
        n = price.index('"'); price = price[:n]
        print 'price', price
        for s, p, o in poSearch(hasPriceSpecification, s):
            # s is now an offering
            for s, p, o in spSearch(s, includesObject):
                # o is now a TypeAndQuantityNode
                for s, p, o in spSearch(o, typeOfGood):
                    # o is now a
                    # ProductOrServicesSomeInstancesPlaceholder
                    for s, p, o in spSearch(o, hasMakeAndModel):
                        # o is a PoSM
                        for s, p, o in spSearch(o, rdfsLabel):
                            print lookupIdString(o)

a = array.array('L')
a.append(numStrings)
outf = open('/tmp/triplestore/numStrings', 'w')
outf.write(a.tostring())
outf.close()

