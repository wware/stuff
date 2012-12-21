from rdflib import Graph, Namespace, Literal, BNode, RDF, URIRef
import plugin
import sys
import string
import getopt
import pprint

DEBUG = False

badSubstrings = (
    "http://dbpedia.org/ontology/wikiPage",
    "http://dbpedia.org/property/wikiPage",
    #"dbpprop:wikiPage",
    ".dbpedia.org" # international DBpedias are unreliable

    # I don't usually care about all this ontological stuff.
    "http://purl.org/dc/",
    "http://xmlns.com/foaf",
    "http://www.w3.org/1999/02/22-rdf",
    "http://www.w3.org/1999/xhtml/vocab",
    "http://www.w3.org/2000/01/rdf-schema",
    "http://www.w3.org/2002/07/owl",
    "http://www.w3.org/2002/07/owl",
    "http://www.w3.org/2003/06/sw-vocab-status/"
    "http://www.w3.org/2004/02/skos",
    "http://www.w3.org/ns",
    )

def hasSubstring(haystack, needle):
    # case-insenstive
    try:
        unicode(haystack).lower().index(unicode(needle).lower())
        return True
    except ValueError:
        return False

def goodTriple(spo):
    for y in spo:
        for badness in badSubstrings:
            if hasSubstring(y, badness):
                return False
    return True

class GraphLoader:
    def __init__(self, starter, maxcount=1000):
        self.graph = Graph()
        self.already = set()
        self.queue = [ starter ]
        self.already.add(starter)
        while len(self.graph) < maxcount:
            if not self.next():
                print 'OUCH'
                break
            self.explore()
    def enqueue(self, lst):
        for x in lst:
            if x not in self.already:
                self.already.add(x)
                self.queue.append(x)
    def subjects(self):
        return list(set(map(lambda spo: spo[0], self.graph)))
    def predicates(self):
        return list(set(map(lambda spo: spo[1], self.graph)))
    def objects(self):
        return list(set(map(lambda spo: spo[2], self.graph)))
    def next(self):
        #if DEBUG: pprint.pprint(self.queue[:5])
        while len(self.queue) > 0:
            x = unicode(self.queue.pop(0))
            #if DEBUG:
            #    print 'x =', x
            #    pprint.pprint(self.queue[:5])
            try:
                self.graph.parse(x)
                # you'd think this filtering operation would be horribly
                # non-performant but it's actually not too bad
                newgraph = Graph()
                for spo in list(self.graph):
                    if goodTriple(spo):
                        newgraph.add(spo)
                self.graph = newgraph
                if DEBUG:
                    print x, len(self.graph)
                return True
            except KeyboardInterrupt:
                sys.exit(0)
            except Exception, e:
                #print e
                pass
        return False
    def explore(self):
        for spo in self.graph:
            self.enqueue(map(unicode, spo))
    def rdfxml(self):
        return self.graph.serialize()
    def turtle(self):
        return self.graph.serialize(format='turtle')

def usage ():
    print('Usage')

def main():
    global DEBUG
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'rdxc:')
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit()
    oformat = "turtle"
    searchArg = None
    maxcount = 100
    for o, a in opts:
        if o in ("-r", "--read"):
            searchArg = raw_input("Search term: ").decode(sys.stdin.encoding)
        elif o in ("-d", "--debug"):
            DEBUG = True
        elif o in ("-c", "--maxcount"):
            maxcount = string.atoi(a)
        elif o in ("-x", "--xml"):
            oformat = "xml"
        else:
            usage()
            sys.exit()
    if searchArg is not None or len(args) > 0:
        if len(args) > 0:
            searchArg = args[0]
        if DEBUG:
            print "http://dbpedia.org/resource/" + unicode(searchArg.replace(" ","_"))

        g = GraphLoader("http://dbpedia.org/resource/" +
                        searchArg.replace(" ","_"), maxcount=maxcount)
        if oformat == 'turtle':
            print g.turtle()
        else:
            print g.rdfxml()
    else:
        # Create a graph and pull some factoids into it.
        g = Graph()
        g.parse("http://bigasterisk.com/foaf.rdf")  # FOAF info for Drew Perttula
        g.parse("http://www.w3.org/People/Berners-Lee/card.rdf")  # TBL's v-card

        FOAF = Namespace("http://xmlns.com/foaf/0.1/")
        g.parse("http://danbri.livejournal.com/data/foaf") # Several friends of Dan Brickley
        [g.add((s, FOAF['name'], n)) for s,_,n in g.triples((None, FOAF['member_name'], None))]

        if True:
            for row in g.query('SELECT ?aname ?bname WHERE { ' +
                               '?a foaf:knows ?b . ?a foaf:name ?aname . ?b foaf:name ?bname . }',
                               initNs=dict(foaf=Namespace("http://xmlns.com/foaf/0.1/"))):
                print "%s knows %s" % row
        else:
            print g.serialize(format='turtle')

if __name__ == '__main__':
    main()
