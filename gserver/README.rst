Writing web apps in Guile Scheme
================================

I've read Paul Graham's writings on Lisp, and particularly where he described
enormous productivity gains by writing web apps in Lisp or Scheme (this was a
while back so the common thing at the time might have been Perl) because the
language afforded him and his team a lot of abstractions and expressive power
not available elsewhere.

More recently, Lisp-like languages (primarily Clojure, recently) are lauded
for their lack of side-effects, a very helpful thing in writing a web app
where you want all the history to reside in the database.

So I thought I'd poke around and see what exists for Guile Scheme, and I came
across guile-www and guile-web. So I'm downloading them and tinkering with
them. One of these packages (maybe both) has a database interface. But I think
it would be fun to try MongoDB instead of the vanilla list of relational
databases I see there.
