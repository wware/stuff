from surf import *

store = Store(  reader='rdflib',
            writer='rdflib',
            rdflib_store = 'IOMemory')

session = Session(store)

print 'Load RDF data'
store.load_triples(source='http://www.w3.org/People/Berners-Lee/card.rdf')

Person = session.get_class(ns.FOAF['Person'])

all_persons = Person.all()

print 'Found %d persons that Tim Berners-Lee knows'%(len(all_persons))
for person in all_persons:
    print person.foaf_name.first

#create a person object
somebody = Person()
somebody_else = Person()

somebody.foaf_knows = somebody_else
