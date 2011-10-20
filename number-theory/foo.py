import math
import sys
import string

# This is an attempt to find a better version of the 355/113 fraction
# that approximates pi. Needs work. I think I need a rational number
# class for Python; what I'm doing here is assuming the denominator
# should be prime, but I really just want the numerator and denominator
# to be relatively prime.

# python makeprimes.py | python foo.py | sort | head -100

choices = [ ]

sum = 0.

for i in primes:

    n1 = int(math.pi * i)
    n2 = n1 + 1

    p1 = 1. * n1 / i
    p2 = 1. * n2 / i
    err1 = (p1 - math.pi) ** 2
    err2 = (p2 - math.pi) ** 2

    choices.append(("%040d" % (1.0e30 * err1), n1, i, p1))
    choices.append(("%040d" % (1.0e30 * err2), n2, i, p2))

for t in choices:
    print t
