#!/bin/sh

# First download Fuseki and follow instructions at
# http://jena.apache.org/documentation/serving_data/index.html
# so start the server by typing
#   fuseki-server --update --mem /ds
# Then type
#   s-put http://localhost:3030/ds/data default wares.rdf

BIO="http://purl.org/vocab/bio/0.1/"
FOAF="http://xmlns.com/foaf/0.1/"
RLSP="http://purl.org/vocab/relationship/"
#QUERY="SELECT ?aname ?bname WHERE { ?b <${RLSP}childOf> ?a . ?a <${FOAF}name> ?aname . ?b <${FOAF}name> ?bname . }"
QUERY="SELECT ?aname ?bname WHERE { ?b <${BIO}mother> ?a . ?a <${FOAF}name> ?aname . ?b <${FOAF}name> ?bname . }"

s-query $@ --service=http://localhost:3030/ds/query "$QUERY"