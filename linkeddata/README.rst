Stuff to do with Linked Data
============================

The linked data initiative (http://linkeddata.org) is getting really interesting. There is a wide
variety of data of very different sorts, in pretty large volumes, and the people involved seem to
have a deep understanding of the technology and how to make the data optimally useful.

So I'd like to spend some time doing stuff with that data, feeding it into Jena or some other
inference engine to see what interesting conclusions can be reached.

What is particularly interesting is it looks like these guys are doing or have done a lot of the
stuff I thought I'd need to do myself. If you look at what would be needed to have inference
engines play a significant role in advancing medical science, you need ontologies relevant to
reasoning about biochemical causalities, and data formatted in compliance with those ontologies,
and those guys have done that with a lot of medical and scientific data.

The piece that's still missing is to frame scientific literature as semantic nets so that an
inference engine or some other piece of software can reason about the logical argument expressed
by a journal article.

I wonder if it makes sense to try to encourage the Quantified Self folks to use linked data. It
kinda seems like it would be a win for them, but initially it would be a bunch of work, and it
probably wouldn't pay off until and unless they started dealing in larger quantities of data.
So that's probably a waste of time.

Does it make sense to build lab equipment that provides data in semantically marked up form? It
could, provided it didn't cause any inconvenience. So

- the data would still need to be human readable
- publishing it to the web should be drop dead easy

The lab equipment will automatically fill in some part of the semantic net: physical units, time
of data acquisition, things that are constrained by how it's wired up or how it works. But then
there will be contextual stuff, like experimental conditions or contextual information that the
equipment can't provide itself.
