Semantic web hacking
====================

Dec 2012
--------

I started fooling with rdflib, a Python library, and in many ways it's more
convenient than using Jena and Java. But it doesn't come with its own OWL
reasoner, and working on Ubuntu Lucid (10.04) I found there must have over
time been different versions of rdflib such that OWL reasoners like FuXi are
hard to get working. Maybe it will all work better on 12.04, but I've become
frustrated for now and am going back to Jena.

http://www.apache.org/dist/jena/binaries/

::

 sudo apt-get install openjdk-6-jdk ant eclipse

Protege is pretty cool. I've reworked my wares.rdf file and scrapped my
family.rdf file because everything I'd defined now exists in the foaf and bio
ontologies. If you do define your own ontology, you can just set checkboxes to
say that properties are functional, symmetric, asymmetric, reflexive, etc.

What I'd like to do is set up willware.net (68.169.50.100) as a RDF server in
some way, using either fuseki, or using Tomcat with a Jena jar. Maybe you want
Tomcat to handle requests coming in, and then run SPARQL queries against an
internal fuseki server. I considered trying to write a templating system that
used RDF in some special way, but vanilla JSP with some calls to Jena should
do it.

From long ago
-------------

This is some Java hacking I did, mostly in October and November of 2003, to
get better acquainted with Hewlett Packard's open-source Jena package, a Java
library for doing semantic web stuff. The basic idea is to build a graph by
reading a few RDF pages off the web, and possibly extend the graph with some
reasoning, and then query the graph. In Jena the querying is done with RDQL,
an SQL-ish-looking query language.

The reasoning and querying is all done locally by the machine that pulled down
the RDFs. None of it is done by the server (unless it was done while
generating the RDFs, for example if they're generated dynamically from some
time-changing data).

There is also a Python package for doing this sort of stuff, called CWM, which
is actually W3C's reference implementation for semantic web software. Maybe
later I'll post some CWM stuff. It's an appealing notion to code this stuff in
a scripting language for the sake of flexibility.
