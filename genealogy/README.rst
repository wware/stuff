Ware/Hildebrandt/Crossfield/Thompson Genealogy
==============================================

The idea here is to create a human- and machine-readable database of
genealogical information for my extended family. To make the info
machine-readable, there need to be unambiguous syntactical pieces to tell the
computer about the relationships of one piece of data to another. I'm going
with JSON_, which looks like this::

 {
   "name": "William Bates Ware",
   "birthdate": "1959/02/25",
   "birthplace": "New Haven, CT, USA",
   "education": [
     "highschool": {
       "name": "Andrew Warde High School",
       "start": 1973,
       "graduate": 1977
     },
     "undergraduate": {
       "name": "Massachusetts Institute of Technology",
       "start": 1977,
       "graduate": 1981
     }
   ],
   # more details ...
 }

Square brackets indicate lists of similar items. Curly brackets indicate a
key-value dictionary. I will also include `JSON comments`_ in human-written
source documents, but not in machine-generated JSON, in compliance with Doug
Crockford's thoughts on the matter.

Another possibility, still human readable but a bit tedious, is to represent
information in RDF_ can make sense because of the huge effort that's been
spent on software to process RDF. The written form of RDF (or in computer
science language, the "serialization") is usually XML (which is ugly as noted
above) but there are alternatives like Turtle_ (a successor to N3_).

.. _JSON: http://en.wikipedia.org/wiki/JSON
.. _RDF: http://en.wikipedia.org/wiki/Resource_Description_Framework
.. _N3: http://en.wikipedia.org/wiki/Notation3
.. _Turtle: http://en.wikipedia.org/wiki/Turtle_(syntax)
.. _`JSON comments`: http://blog.getify.com/2010/06/json-comments/

I'm not too worried about choosing between JSON and RDF because I think once
the data has been put down in one form or the other, it won't be too hard to
whip up a program to translate to the other form. So I'll go with JSON for
the immediate future because I think it's more human-friendly, and not much
less machine-friendly.

There is a lot of information already posted in RDF and by following a few
conventions it would be possible to connect our genealogy with this larger web
of info, but that can probably be done reasonably easily later, if deemed
desirable.

Privacy
-------

Obviously some people in my family are alive and would not like me to
indiscriminantly publish information about them, even in a Github repository
that nobody ever looks at. So this public stuff will be confined to deceased
ancestors and to people (like myself) who I'm certain won't mind. There will
be another private repo somewhere to address more current info using the same
data conventions so it all plays together nice.

Don't Repeat Yourself
---------------------

There is a general principle in computer science called "Don't Repeat
Yourself" (or DRY for short). This means that the same information should not
be specified in two different places, because it's all too likely that over
time, as people go in and edit things, those two specifications will
eventually differ and then you have this big question of which one is supposed
to be authoritative. Suppose we see something like this::

 {
   "name": "Bob Foo",
   "marriage": {
     "towhom": "Sally Bar",
     "when": 1932
     # ... many more marriage details ...
   }
 },
 {
   "name": "Sally Bar",
   "marriage": {
     "towhom": "Bob Foo",
     "when": 1935
     # ... many more marriage details THAT MIGHT BE DIFFERENT ...
   }
 }

Which description of the marriage is the right one? Could there have been two
marriages, or maybe a renewal of vows? How to deal with this situation?

When we embed one data structure in another (like a marriage description
inside a person description), we will add a "reference" field to it, so that
other descriptions can refer to it just using the reference. This will save
space in the database and it will also prevent violations of the DRY
principle. Applying that to the example above we have::

 {
   "name": "Bob Foo",
   "marriage": {
     "reference": "@BobFooMarriesSallyBar1932"
     "when": 1932
     # ... many more marriage details ...
   }
 },
 {
   "name": "Sally Bar",
   "marriage": "@BobFooMarriesSallyBar1932"
 }

In order that we can identify references when they occur in random places,
we'll require that the first character be an at-sign (@) and that no other
strings except for references can begin with an at-sign.

Photos and other attachments
----------------------------

Not sure exactly what I'll do with this. We'll see. It shouldn't be a very
difficult problem. A photo represented in JSON will include a full pathname in
a repository, a date, and a location. DRY question: do we list the people in
the photo in the JSON for the photo, or do we give each person a link to the
photo, or do we do both?

As long as we assume we can search the entire database any time we want, and
the links (references) aren't broken, it shouldn't matter which we do.

Rendering this stuff into other forms
-------------------------------------

Ultimately we'll want to be able to take these representations and form
web pages or printed documents with them. I am confident that this won't
be terribly difficult, and I'm more worried about getting the data into
digital form than producing that stuff immediately. But before too long
I hope to post a website that presents this stuff in a form for people
who don't want to know anything about JSON.

