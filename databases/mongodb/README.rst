Puttering with MongoDB
======================

(From the website) MongoDB (from "humongous") is a scalable, high-performance,
open source, document-oriented database. Written in C++, MongoDB features:

- Document-oriented storage: JSON-style documents with dynamic schemas offer
  simplicity and power.
- Full Index Support: Index on any attribute, just like you're used to.
- Replication & High Availability: Mirror across LANs and WANs for scale and
  peace of mind.
- Auto-Sharding: Scale horizontally without compromising functionality.
- Querying: Rich, document-based queries.
- Fast In-Place Updates: Atomic modifiers for contention-free performance.
- Map/Reduce: Flexible aggregation and data processing.
- GridFS: Store files of any size without complicating your stack.
- Commercial Support: Commercial support, training, and consulting available.

There are also warnings that, like any new technology, MongoDB can attract
the attention of shallow technology fashionistas.

http://www.youtube.com/watch?v=b2F-DItXtZs&feature=autofb

Isomorphism with RDF triples
----------------------------

I am interested in the possibility of an isomorphism between RDF triples and a
subset of Mongo documents, e.g.::

 http://example.com/alpha http://example.com/bravo http://example.com/charlie.

 {
     "_id": ....,
     "subject": "http://example.com/alpha",
     "http://example.com/bravo": "http://example.com/charlie"
 }

In the list of a document's key-value pairs, each key may appear only once,
but RDF triples have no corresponding restriction. To handle cases where an
RDF database holds multiple triples with the same subject and predicate, two
possibilities occur to me.

One is to make the value a list. The other is to use multiple documents.

If each document represents one (or just a few) triple, there doesn't seem to
be any advantage that this approach could possibly offer over a purposely
designed triple store. So I'm trying to convince myself that this is an
interesting exercise but not having much luck.

To follow this line, one would need to implement production systems on Mongo
documents, which would require an implementation of the Rete algorithm for
Mongo, probably done as a patch in C++ code. A lot of work for no clear gain.

What else is Mongo good for?
----------------------------

I don't know yet. I guess I should wait until I'm getting ready to build a
website, and think about whether it would be easier to implement using Mongo
rather than something like MySQL.
