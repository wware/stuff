Math library for Guile Scheme
=============================

I want to do some stuff:

- z-buffering with transparency
- generating POV-Ray output
- mechanical simulation

  - rigid bodies
  - linear and non-linear springs
  - "jigs" as Mark Sims used to say
  - ultimately, molecular modeling

    - http://chemistry.gsu.edu/Glactone/modeling/MMintro.html

- posting mechanical simulation videos on Youtube

A lot of this stuff involves math, so I'm putting together a little math
library in Scheme. Hopefully if I do a reasonable job of it, all that other
stuff will be relatively straightforward.

How Guile modules work
----------------------

Modules appear in /usr/share/guile/1.8, for example::

 ;; /usr/share/guile/1.8/ice-9/slib.scm
 (use-modules (ice-9 slib))
 (require 'primes)
 (prime? 13)
 => #t

So when my math library is ready, I can store it there. But there is some
interesting and mildly complex stuff involved, so when that time comes, look
more carefully at the ice-9 directory. There is tricky stuff about defining
which functions are exported from the module.  For more about this, see section
2.3.5.2 ("Writing new modules") of the Guile Reference Manual at
http://www.gnu.org/software/guile/manual/guile.html.  Or just grep the manual
for all occurrences of "bessel".
