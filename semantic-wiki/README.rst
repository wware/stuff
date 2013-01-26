A semantic wiki
===============

I tried Semantic Mediawiki and was quite disappointed. Connecting up the
wiki-native ontology to external ontologies is a ridiculously painful exercise.

I've been tinkering to try to find a good way to embed RDF/Turtle or something
equivalent (and reasonably user-painless) into a Mediawiki instance. Here's what
I've come up with. Assuming ubuntu-12.04-server, first copy rdf-cgi.py to
/usr/lib/cgi-bin and "chmod 755" it.

I put in some URL rewriting magic (type "a2enmod rewrite" and see
http://www.debian-administration.org/articles/136). In the Apache config file,
``/etc/apache2/sites-enabled/000-default``, I added these two lines::

 RewriteEngine on
 RewriteRule ^/rdf/([A-Z][_a-zA-Z0-9]*)$  /cgi-bin/rdf-cgi.py?title=$1  [PT]

The rdf-cgi.py script goes into Apache's CGI. Then you
can do something like this::

 wget -O foobar.ttl http://192.168.2.7/rdf/Some_interesting_topic

to fetch a Turtle file.

In the source of the Mediawiki page, you need to notate it like this::

 === rdf ===

  @prefix bio: <http://purl.org/vocab/bio/0.1/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  [] a foaf:Person;
      bio:event _:Nffd7f07194d14e1ab6d7161dba66bbb5;
      foaf:knows _:Nbb207f59f3d246ee9118f2bbb9c9598e;
      foaf:name "Some Person" .

You could make that RDF/XML rather than Turtle, the CGI script wouldn't care,
as long as you correctly put the single space in front to mark it as a code
block, and don't use a "<textarea>" tag anywhere. I personally prefer Turtle
because XML makes hair grow on your palms.

If you put multiple RDF sections on a wiki page, they will be concatenated in
order. So it's a sort of poor man's literate programming.

The future
----------

The next thing I want to do is run a Fuseki server on the same machine, and
let it automatically pull out these Turtle documents.
