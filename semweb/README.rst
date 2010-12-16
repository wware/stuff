Semantic web hacking
====================

This is some Java hacking I did, mostly in October and November of
2003, to get better acquainted with Hewlett Packard's open-source
Jena package, a Java library for doing semantic web stuff. The basic
idea is to build a graph by reading a few RDF pages off the web, and
possibly extend the graph with some reasoning, and then query the
graph. In Jena the querying is done with RDQL, an SQL-ish-looking
query language.

The reasoning and querying is all done locally by the machine that
pulled down the RDFs. None of it is done by the server (unless it
was done while generating the RDFs, for example if they're generated
dynamically from some time-changing data).

There is also a Python package for doing this sort of stuff, called
CWM, which is actually W3C's reference implementation for semantic
web software. Maybe later I'll post some CWM stuff. It's an appealing
notion to code this stuff in a scripting language for the sake of
flexibility.