Fooling with Dojo
=================

I want to learn more about Dojo, and I've got a copy of "Dojo: The Definitive
Guide" on my Kindle. They recommend CherryPy for a quick painless web server,
so I've got that and Dojo 1.6.1 (the version we use at work) in the repository
now.

Not much in chapter 2 worth doing. One useful thing in chapter 2 is this
diagram a suggested organization of one's Dojo module relative to the HTML that
uses it.

::

 www/
   foo.html
 www/mymodule
   Foo.js

Chapter 2 also discusses ``mixin`` and Base's ``extend`` function. It looks
like these would be helpful for the thing I was working on late today.
