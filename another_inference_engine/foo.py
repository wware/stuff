#!/usr/bin/python

import graph

counter = 1
names = { }

def label(name):
    global counter
    names[counter] = name
    counter += 1
    # make sure the sign bit is zero
    return counter - 1
def variable(name):
    global counter
    names[counter] = name
    counter += 1
    # make sure the sign bit is one
    return -(counter - 1)
def printgraph(g, gtype=type(graph.graph(1))):
    import types
    if isinstance(g, gtype):
        g = g.toList()
    if not isinstance(g, types.ListType):
        raise Exception
    for s, p, o in g:
        print (names[s] + " " +
               names[p] + " " +
               names[o])
def cogitate(graph, rules):
    # we need a size method, this is dumb
    old = graph.size()
    print old
    while 1:
        for r in rules:
            graph.apply(r)
        new = graph.size()
        if new == old:
            break
        old = new
        print old

# nouns and predicates
whitehouse = label("whitehouse")
bill = label("bill")
hillary = label("hillary")
chelsea = label("chelsea")
socks = label("socks")
dubya = label("dubya")
laura = label("laura")
jenna = label("jenna")
barbara = label("barbara")
barney = label("barney")
sibling = label("sibling")
child = label("child")
spouse = label("spouse")
livesin = label("livesin")
liveswith = label("liveswith")
doesnotlivewith = label("doesnotlivewith")
pet = label("pet")
species = label("species")
cat = label("cat")
human = label("human")
hasprop = label("hasprop")
transitive = label("transitive")
symmetric = label("symmetric")
conventional = label("conventional")

# variables
W = variable("W")
X = variable("X")
Y = variable("Y")
Z = variable("Z")

G = graph.graph(200)

########################################
# facts about people and pets
G.add(chelsea, pet, socks)
G.add(hillary, child, chelsea)
G.add(socks, species, cat)
G.add(hillary, spouse, bill)
G.add(hillary, doesnotlivewith, laura)

G.add(dubya, hasprop, conventional)
G.add(dubya, livesin, whitehouse)
G.add(dubya, spouse, laura)
G.add(dubya, pet, barney)
G.add(dubya, child, jenna)
G.add(barbara, sibling, jenna)

# information about relationships
G.add(spouse, hasprop, symmetric)
G.add(sibling, hasprop, symmetric)
G.add(sibling, hasprop, transitive)
G.add(liveswith, hasprop, symmetric)
G.add(liveswith, hasprop, transitive)
G.add(doesnotlivewith, hasprop, symmetric)

#############################################
rules = [
    # definition of symmetric
    graph.rule((Y, X, Z),
               (X, hasprop, symmetric),
               (Z, X, Y)),
    # definition of transitive
    graph.rule((W, X, Y),
               (Y, X, Z),
               (X, hasprop, transitive),
               (W, X, Z)),
    # If X and Y live together and Y lives apart from Z,
    # then X lives apart from Z.
    graph.rule((X, liveswith, Y),
               (Y, doesnotlivewith, Z),
               (X, doesnotlivewith, Z)),
    # Only humans get married.
    graph.rule((X, spouse, Y),
               (X, species, human)),
    # Pet owners live with their pets.
    graph.rule((X, pet, Y),
               (X, liveswith, Y)),
    # Children are the same species as their parents.
    graph.rule((X, child, Y),
               (X, species, Z),
               (Y, species, Z)),
    # People living together share pets.
    graph.rule((Z, pet, Y),
               (Z, liveswith, X),
               (X, species, human),
               (X, pet, Y)),
    # Siblings are of the same species.
    graph.rule((X, sibling, Y),
               (Y, species, Z),
               (X, species, Z)),
    # All pet owners are human.
    graph.rule((X, pet, Y),
               (X, species, human)),
    # People living in the same place live together.
    graph.rule((X, livesin, Z),
               (Y, livesin, Z),
               (X, liveswith, Y)),
    # People living together live in the same place.
    graph.rule((X, livesin, Z),
               (X, liveswith, Y),
               (Y, livesin, Z)),
    # The spouses of conventional people are conventional.
    graph.rule((X, hasprop, conventional),
               (X, spouse, Y),
               (Y, hasprop, conventional)),
    # Conventional spouses share children
    graph.rule((X, hasprop, conventional),
               (X, child, Y),
               (X, spouse, Z),
               (Z, child, Y)),
    # Conventional spouses live together.
    graph.rule((X, hasprop, conventional),
               (X, spouse, Y),
               (X, liveswith, Y)),
    # Siblings share parents, if the parents are conventional.
    graph.rule((X, hasprop, conventional),
               (X, child, Y),
               (Y, sibling, Z),
               (X, child, Z)),
]

cogitate(G, rules)
printgraph(G)
print "=================="


def query(question, template):
    print question
    printgraph(G.query(template))
    print

query("Who lives in the White House?", (0, livesin, whitehouse))

query("Who lives together?", (0, liveswith, 0))

query("Who does not live together?", (0, doesnotlivewith, 0))

query("Who is a human?", (0, species, human))

query("Who has a pet?", (0, pet, 0))

query("Who has kids?", (0, child, 0))
