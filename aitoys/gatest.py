import math, random
import ga

class Population:
    def __init__(self, popsize, chromlength, maxval):
        self.popsize, self.maxval = popsize, maxval
        chromosomeList = [ ]
        for i in range(popsize):
            chromosomeList.append(ga.Chromosome(chromlength, maxval, 1))
        self.chromosomes = chromosomeList
        self.bestYet = None
    def __call__(self, fitness, steps=1):
        popsize, maxval = self.popsize, self.maxval
        while steps:
            newlst = [ ]
            for i in range(5 * popsize):
                u = random.choice(self.chromosomes)
                v = random.choice(self.chromosomes)
                w = u.cross(v)
                w.mutate(.05)
                newlst.append(w)
            ga.sort_by_fitness(newlst, fitness)
            self.chromosomes = newlst[-popsize:]
            steps = steps - 1
        self.bestYet = (fitness(self.chromosomes[-1]),
                        self.chromosomes[-1].tuple())

def fitness(chromosome, pi=math.pi, e=math.e):
    # fitness == closeness to pi
    z = 0.
    m = 1.
    for x in chromosome.tuple():
        z = z + x * m
        m = 0.1 * m
    return -abs(pi - z)

N = 100
L = 10

P = Population(N, L, 10)
for i in range(4):
    P(fitness, 10)
    print P.bestYet
del P.chromosomes
