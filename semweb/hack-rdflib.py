from rdflib.Graph import Graph
from rdflib import Namespace

# Create a graph and pull some factoids into it.
g = Graph()
g.parse("http://bigasterisk.com/foaf.rdf")  # FOAF info for Drew Perttula
g.parse("http://www.w3.org/People/Berners-Lee/card.rdf")  # TBL's v-card

FOAF = Namespace(
    "http://xmlns.com/foaf/0.1/"   # this works
    #"http://xmlns.com/foaf/spec/20071002.rdf"  # this DOES NOT work
    )
g.parse("http://danbri.livejournal.com/data/foaf") # Several friends of Dan Brickley
# treat all member_names as regular names
[g.add((s, FOAF['name'], n)) for s,_,n in g.triples((None, FOAF['member_name'], None))]

# I can't get anything when I try to fetch http://xmlns.com/foaf/0.1/
# I just get redirected to http://xmlns.com/foaf/spec/
# So I'm not sure what people are doing there

for row in g.query('SELECT ?aname ?bname WHERE { ?a foaf:knows ?b . ?a foaf:name ?aname . ?b foaf:name ?bname . }', 
                   initNs=dict(foaf=Namespace("http://xmlns.com/foaf/0.1/"))):
    print "%s knows %s" % row
