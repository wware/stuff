## http://en.wikipedia.org/wiki/Simulated_annealing

import os
import sys
import math
import random
import Tkinter

try:
    import sahelp
except ImportError:
    sahelp = None

# This is what Wikipedia recommends

def simAnn(s0, E, P, neighbor):
    s = s0
    e = E(s)                   # Initial state, energy.
    sbest = s
    ebest = e                  # Initial "best" solution
    k = 0                      # Energy evaluation count.
    while k < kmax and e > emax:  # While time left & not good enough:
        snew = neighbor(s)     # Pick some neighbour.
        enew = E(snew)         # Compute its energy.
        if enew < ebest:       # Is this a new best?
            sbest = snew
            ebest = enew       # Save 'new neighbour' to 'best found'.
        if P(e, enew, temp(k/kmax)) > random():   # Should we move to it?
            s = snew
            e = enew           # Yes, change state.
        k += 1                 # One more evaluation done
    return sbest               # Return the best solution found.

# Being a willful sort of individual, this is what I plan to do.

def simAnn(s0, kmax, emax, E, neighbor):
    s = s0
    e = E(s)                   # Initial state, energy.
    k = 0                      # Energy evaluation count.
    while k < kmax and e > emax:  # While time left & not good enough:
        snew = neighbor(s, k, e)  # Distance to neighbour depends on k, E
        enew = E(snew)         # Compute its energy.
        if enew < e:           # Is this a new best?
            s = snew
            e = enew
        k += 1                 # One more evaluation done
    #print k
    return (s, e)

#########################
# Convenience functions #
#########################

def rnd(x):
    return x * (2 * random.random() - 1)

def descend(k, kmax=100):
    return 10. * math.exp(-(k / kmax))

pseudoRandomBuffer = [ ]
for i in range(10000):
    pseudoRandomBuffer.append(random.random())
prbPointer = 0

def randint(lo, hi):
    # gross hack, including assumption that lo=0
    global prbPointer
    x = pseudoRandomBuffer[prbPointer]
    prbPointer = (prbPointer + 1) % 10000
    return int((hi + 0.9) * x)

def permuteList(lst, swaps):
    #randint = random.randint   # local variable is a little faster
    n = len(lst) - 1
    while swaps > 0:
        j = randint(0, n)
        while True:
            k = randint(0, n)
            if k != j:
                break
        lst[j], lst[k] = lst[k], lst[j]
        swaps -= 1

######################
# Now let's test it. #
######################

# This is the easiest possible problem, a parabola in 3 dimensions
# with one clearly defined minimum, graceful slopes all around. If
# it can't do this, it can't do anything.

def E(s):
    x, y, z = s
    a = ((x - 3) ** 2 +
         (y - 4) ** 2 +
         (z - 5) ** 2)
    return a

def neighbor(s, k, e):
    import math
    x, y, z = s
    delta = descend(k)
    return (x + rnd(delta),
            y + rnd(delta),
            z + rnd(delta))

print simAnn((0., 0., 0.), 50000, 1.0e-20, E, neighbor)

# Next let's try finding roots of a polynomial.
# (x - 2) (x + 2) x = 0 --> roots at -2, 0, 2

def E(s):
    P = s**3 - 4*s
    return P ** 2

def neighbor(s, k, e):
    delta = descend(k)
    return s + rnd(delta)

for i in range(3):
    print "%.4f %.4f" % simAnn(-0.5, 100000, 1.0e-10, E, neighbor)
    print "%.4f %.4f" % simAnn(0.5, 100000, 1.0e-10, E, neighbor)

# throw in a lot of local minima to try to confuse it

def E(s):
    x, y = s
    # make wx and wy large to make the ripples tight
    wx, wy, a, b = 200, 200, 4, 9
    ripple = ((math.sin(wx * (x - a)) ** 2) *
              (math.sin(wy * (y - b)) ** 2))
    return 0.5 * (x - a) ** 2 + 0.03 * (y - b) ** 2 + ripple

def neighbor(s, k, e):
    x, y = s
    delta = descend(k)
    return (x + rnd(delta), y + rnd(delta))

print simAnn((0, 0), 10000, 1.0e-20, E, neighbor)

# Next I need to try some discrete examples. The obvious thing is the
# Traveling Salesman Problem. For N cities, an exhaustive search of the
# search space involves evaluating factorial(N) possible solutions, the
# number of possible permutations of the list of cities. You could throw
# away a factor of two because a solution can be reversed to get an
# equally good solution.

# Number of cities
N = 12

def factorial(n):
    product = 1
    for i in range(2, n+1):
        product *= i
    return product

# Empirically pretty good formula for N up to about 30
KMAX = 10 ** (4 + N / 10)

# What fraction of the search space will we be exploring?
print (1.0 * KMAX) / (factorial(N) / 2)

class City:
    def __init__(self, x, y):
        self.x, self.y = x, y
        self.distCache = { }
    def distance(self, other):
        try:
            return self.distCache[id(other)]
        except KeyError:
            d = ((self.x - other.x) ** 2 +
                 (self.y - other.y) ** 2) ** 0.5
            self.distCache[id(other)] = d
            other.distCache[id(self)] = d
            return d

WIDTH, HEIGHT = 500, 400
cities = [ ]
for i in range(N):
    x = WIDTH * random.random()
    y = HEIGHT * random.random()
    cities.append(City(x, y))

# s is a list of N cities
def E(s):
    totalDistance = 0.
    city1 = s[0]
    for city2 in s[1:]:
        totalDistance += city1.distance(city2)
        city1 = city2
    return totalDistance

def neighbor(s, k, e):
    global permuteList
    s = s[:]
    if sahelp is None:
        permuteList(s, 1 + int((1. * N * k) / KMAX))
    else:
        sahelp.shuffle(s, 1 + int((1. * N * k) / KMAX))
    return s

if False:
    import hotshot
    prof = hotshot.Profile("hotshot_stats")
    def foo():
        simAnn(cities, KMAX, 0, E, neighbor)
    prof.runcall(foo)
    prof.close()
else:
    answer, ignored = simAnn(cities, KMAX, 0, E, neighbor)
    root = Tkinter.Tk()
    canvas = Tkinter.Canvas(root, height=HEIGHT, width=WIDTH)
    canvas.pack()
    city1 = answer[0]
    for city2 in answer[1:]:
        canvas.create_line(city1.x, city1.y,
                           city2.x, city2.y)
        canvas.create_oval(city1.x + 2, city1.y + 2,
                           city1.x - 2, city1.y - 2,
                           fill="#00ff00")
        city1 = city2
    canvas.create_oval(city2.x + 2, city2.y + 2,
                       city2.x - 2, city2.y - 2,
                       fill="#00ff00")
    root.mainloop()
