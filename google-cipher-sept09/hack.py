import string, random

cipher = "8MLDQ6 T UI" + \
    "6TFML RH AA" + \
    "NRA6Q 8EFL " + \
    "DMQ86II2 O3" + \
    "2S5J 13JXOJ"

cipherWords = cipher.split(" ")
print cipherWords

cipherLetters = { }
for w in cipherWords:
    for L in list(w):
        if cipherLetters.has_key(L):
            cipherLetters[L] += 1
        else:
            cipherLetters[L] = 1
print cipherLetters
CLK = cipherLetters.keys()

# Let's assume it's a substitution cipher. They've got letters and digits
# but no punctuation. Let's assume for now that the real message has digits
# and letters in it. We'll also assume that a space is a space.

ABC123 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
realWords = { }
for w in open("/usr/share/dict/words").readlines():
    w = string.upper(w.strip())
    if len(w) > 0:
        # eliminate anything other than letters
        if reduce(lambda prevFlag, newChar:
                      prevFlag and (newChar in ABC123),
                  w, True):
            n = len(w)
            if realWords.has_key(n):
                realWords[n].append(w)
            else:
                realWords[n] = [ w ]

# Let's attach a figure of merit to each possible mapping, and then use either
# a genetic algorithm or simulated annealing to find mappings with high scores.

class Mapping:
    def __init__(self, map=None):
        if map is None:
            self.map = { }
            #for cw in cipherWords[:2]:
            for cw in ("DMQ86II2", "8EFL"):
                #print cw
                i = 0
                while True:
                    try:
                        self.createMappingForWord(cw)
                        break
                    except:
                        pass
                    i += 1
                    if i >= 300:
                        raise Exception
            for c in CLK:
                if not self.map.has_key(c):
                    self.setUniqueMappingFor(c)
        else:
            # make a clone
            self.map = map.copy()
    def createMappingForWord(self, word):
        mightBe = random.choice(realWords[len(word)])
        for i in range(len(word)):
            c1 = word[i]
            c2 = mightBe[i]
            if self.map.has_key(c1):
                if self.map[c1] != c2:
                    raise Exception
            else:
                self.map[c1] = c2
    def setUniqueMappingFor(self, c, c2=None):
        if c2 is None:
            c2 = random.choice(ABC123)
        while True:
            if c2 not in self.map.values():
                self.map[c] = c2
                return
            c2 = random.choice(ABC123)
    def decryptChar(self, c):
        if c == " ": return " "
        return self.map[c]
    def decrypt(self, message):
        return "".join(map(self.decryptChar, list(message)))
    def mutate(self, n):
        for i in range(n):
            c = random.choice(CLK)
            self.setUniqueMappingFor(c)
    def __repr__(self):
        return "<Mapping %f>" % self.merit()
    def merit(self):
        score = 0
        if not hasattr(self, 'figureOfMerit'):
            for w in cipherWords:
                if len(w) > 2:
                    w2 = self.decrypt(w)
                    goodGuys = realWords[len(w)]
                    if w2 in goodGuys:
                        #print "  ", w, w2
                        score += 1
                    else:
                        i = len(w) - 1
                        while i:
                            if w[:i] in map(lambda x: x[:i], goodGuys):
                                score += (1.0 * i) / len(w)
                                break
                            i -= 1
            self.figureOfMerit = score
        return self.figureOfMerit
    def annealStep(self, n):
        for i in range(1000):
            clone = Mapping(self.map)
            clone.mutate(n)
            if clone.merit() > self.merit():
                self.map = clone.map.copy()
    def breed(self, other):
        newguy = Mapping({ })
        for c in CLK:
            if random.random() < 0.5:
                c2 = self.decryptChar(c)
            else:
                c2 = other.decryptChar(c)
            newguy.setUniqueMappingFor(c, c2)
        return newguy

before = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
after  = "456789ABCDE2FGHIJKL0MNOP1QRS3TUVWXYZ"

d = { }
for x, y in map(None, list(before), list(after)):
    d[x] = y

m = Mapping(d)

print m.decrypt(cipher)


raise SystemExit

n = len(CLK)

numgens = 1000
PopulationSize = 1000

population = [ ]

while len(population) < PopulationSize:
    m = None
    try:
        m = Mapping()
        m.merit()
    except:
        pass
    if m is not None:
        population.append(m)
        print len(population)
population.sort(lambda x, y: -cmp(x.merit(), y.merit()))

for generation in range(numgens):

    print generation
    print population[0].decrypt(cipher)
    minmerit, avgmerit, maxmerit = 10000000., 0, -10000000.
    for guy in population:
        #print guy, guy.decrypt(cipher)
        m = guy.merit()
        minmerit = min(m, minmerit)
        maxmerit = max(m, maxmerit)
        avgmerit += m * (1. / PopulationSize)
    print minmerit, avgmerit, maxmerit

    #print generation, population[:6]
    population = population[:PopulationSize/2]
    for i in range(PopulationSize/2):
        j = random.randint(0, PopulationSize/2-1)
        k = random.randint(0, PopulationSize/2-1)
        if random.random() < 0.95:
            parent1 = population[j]
            parent2 = population[k]
            child = parent1.breed(parent2)
        else:
            while True:
                try:
                    child = Mapping()
                    break
                except:
                    pass
        child.merit()
        population.append(child)
    population.sort(lambda x, y: -cmp(x.merit(), y.merit()))
