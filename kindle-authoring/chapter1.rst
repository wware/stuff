Creating content for the Kindle
===============================

The make-index.py script generates an OPF file and NCX file, which are
used to create a table of contents. I don't know how to create an index at
the end, but I know people have done it.

Unfortunately the version of Docutils for Ubuntu Lucid is only 0.6. At
version 0.8, it picks up "math" tag allowing you to write equations in
LaTeX format and have them rendered in beautiful typography. Maybe I should
do a custom install of Docutils just to get that.
 
Historical
----------

I've wanted to create Kindle content for a while, and I've finally found out
how to do it, thanks to this blog post:
http://longair.net/blog/2010/10/04/the-guardian-on-your-kindle/

Also see
https://github.com/mhl/guardian-for-kindle/blob/master/make-guardian-ebook.py

You need to pick up Kindlegen from Amazon, and use Kompozer to create some
HTML files. See the Makefile in this directory for more info about the
process.

I'm still figuring out pieces of it. I don't have a table of contents yet. I
don't know how to put an image in the middle of a chapter, like a numbered
figure as you'd see in a LaTeX document. I also don't know any way to do math
notation, other than use LaTeX and then stick an image in my HTML. Yick.

I guess that answers the question of how to put an image in the middle of a
chapter -- each chapter is an HTML doc, and you just use a <IMG SRC="...">
tag.

I've attached a PDF style guide from Amazon, which explains how to do a table
of contents on page 13. Amazon is very concerned with making sure readers have
a good experience, but alas, many e-books are being created that ignore the
guidelines.

Anyway, once you've created the mobi file, you want to use a USB cable to
copy it into the "documents" directory of the Kindle.

I also want to make a more interesting cover. Maybe I can do that quickly
in Gimp. Yup, you can do that in Gimp. Cool. Gotta go to sleep now.

Working from a blog feed
------------------------

I browsed to the feed_ for my blog, and did "Save As..." to "wills-blog.xhtml".
I did a little editing in Emacs to remove some extraneous formatting stuff,
and I've gotten an XHTML file that is pretty close to what I'd want.

.. _feed: http://willware.blogspot.com/feeds/posts/default

I want to make each entry a chapter. There are div markers so that should
hopefully be feasible. I need to spend time scratching my head over the XHTML
and Amazon's Guidelines PDF.

It looks like each blog entry is marked by this::

 <div class="entry">

and if I add an "id=entry_X" attribute, I think I should be able to navigate
to individual entries. Then I can set up a proper table of contents.
