USE_OWL = False
DUMP_AND_QUIT = False
NOT_REALLY = False

Name = "<http://xmlns.com/foaf/0.1/name>"
Knows = "<http://xmlns.com/foaf/0.1/knows>"
Interest = "<http://xmlns.com/foaf/0.1/interest>"
Event = "<http://purl.org/vocab/bio/0.1/event>"
Birth = "<http://purl.org/vocab/bio/0.1/Birth>"
Marriage = "<http://purl.org/vocab/bio/0.1/Marriage>"
Death = "<http://purl.org/vocab/bio/0.1/Death>"
Rdftype = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
Date = "<http://purl.org/vocab/bio/0.1/date>"
airportName = "<http://www.daml.org/2001/10/html/airport-ont#name>"
iataCode = "<http://www.daml.org/2001/10/html/airport-ont#iataCode>"
nearestAirport = "<http://www.w3.org/2000/10/swap/pim/contact#nearestAirport>"

familyRdf = "family.rdf"
Father = "<" + familyRdf + "/father>"
Mother = "<" + familyRdf + "/mother>"
Son = "<" + familyRdf + "/son>"
Daughter = "<" + familyRdf + "/daughter>"
Grandchild = "<" + familyRdf + "/grandchild>"

danbriFoaf = "http://danbri.livejournal.com/data/foaf"

query = ("SELECT ?eldername ?kidname\n"
         + "    WHERE { ?elder " + Name + " ?eldername.\n"
         + "            ?kid " + Name + " ?kidname.\n"
         + "            ?kid " + Father + " ?elder. }")

# don't know what to do next, gotta read some docs
