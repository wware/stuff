================
Fooling around with the Neo4J graph database
================

I unpacked the binary 1.0 tarball in /usr/local. That put the Neo4J
library here::

   /usr/local/neo4j-kernel-1.0/neo4j-kernel-1.0.jar

To hack in Java, just use the Makefile in the obvious way. To use Jython,
do this first::

    export CLASSPATH=/usr/local/neo4j-kernel-1.0/neo4j-kernel-1.0.jar:/usr/local/neo4j-kernel-1.0/geronimo-jta_1.1_spec-1.1.1.jar:.

Then you can go ahead and use Jython::

    $ jython
    Jython 2.2.1 on java1.6.0_0
    Type "copyright", "credits" or "license" for more information.
    >>> import Foo
    >>> f = Foo()
    >>> f.go()
    Hello, brave Neo4j world!
    >>> ^D
    $

------------------
What I'd like to do with this
------------------

I assume that Neo4J, if the hype is justified, can do wicked fast SPARQL queries.
That being the case, it can help to iterate production rules, and that means it
can perform deduction on formal systems.

So I'd like to fool around with large-scale deduction, or pattern recognition, or
something along those lines.

I've written about this stuff on Wikipedia here_, in preparation for a PediaPress
book.

.. _here: http://en.wikipedia.org/wiki/User:WillWare/Books/WW-Digital-ML-book

