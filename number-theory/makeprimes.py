import math
import sys
import string

N = 10000000

primes = [2]
for i in range(3, N, 2):
    for x in primes:
        if x * x > i:
            primes.append(i)
            break
        if (i % x) == 0:
            # not prime, try next i
            break

for p in primes:
    print p
