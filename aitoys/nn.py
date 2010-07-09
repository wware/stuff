import sys, math, random, operator

def coeff(x=.2, r=random.random):
    return x * (2 * r() - 1)

atanFactor = 10.

# Reference implementation
# For very small networks, a C neuron doesn't help much, but for
# large networks it becomes a significant win.
class Neuron:
    def __init__(self, inputs):
        self.inputs = inputs
        self.weights = map(lambda dummy,coeff=coeff: coeff(),
                           inputs)
        self.offset = 0
        self.sum = 0
        self.out = 0
    def iterate(self):
        sum = self.offset
        inputs = self.inputs
        weights = self.weights
        for i in range(len(inputs)):
            sum = sum + weights[i] * inputs[i].out
        self.sum = sum
        self.out = math.atan(atanFactor * sum)
    def adjust(self, delta, K):
        x = self.sum * atanFactor
        delta = delta * atanFactor / (1.0 + x * x)
        self.offset = self.offset + K * delta
        inputs = self.inputs
        weights = self.weights
        i = 0
        for x in inputs:
            w = weights[i]
            weights[i] = w + K * delta * x.out
            x.adjust(delta * w, K)
            i = i + 1
    def force(self, output):
        self.out = output

if len(sys.argv) == 1:
    import neur
    Neuron = neur.new

class Layer:
    def __init__(self, num, inputs=[]):
        self.lst = [ ]
        if isinstance(inputs, Layer):
            inputs = inputs.lst
        for i in range(num):
            self.lst.append(Neuron(inputs))
    def __add__(self, other):
        newguy = Layer(0)
        newguy.lst = self.lst[:]
        for x in other.lst:
            if x not in newguy.lst:
                newguy.lst.append(x)
        return newguy
    def force(self, outvec):
        i = 0
        for x in self.lst:
            x.force(outvec[i])
            i = i + 1
    def iterate(self):
        for x in self.lst:
            x.iterate()
    def output(self):
        return map(lambda n: n.out, self.lst)
    def adjust(self, deltavec, learningconst):
        lst = self.lst
        assert len(deltavec) == len(lst)
        i = 0
        for x in lst:
            x.adjust(deltavec[i], learningconst)
            i = i + 1

class TrainingSet:
    def __init__(self, pairs, iterfunc, adjustfunc, learningconst):
        self.pairs = pairs
        self.iterfunc = iterfunc
        self.adjustfunc = adjustfunc
        self.learningconst = learningconst
    def train(self, n):
        iter = self.iterfunc
        adjust = self.adjustfunc
        learningconst = self.learningconst
        while n:
            for x, y in self.pairs:
                z = iter(x)
                delta = map(operator.sub, y, z)
                adjust(delta, learningconst)
            n = n - 1
    def showoff(self, printflag=0):
        iter = self.iterfunc
        sumdelta = 0.
        for x, y in self.pairs:
            z = iter(x)
            if printflag:
                print z,
            delta = map(operator.sub, y, z)
            for x in delta:
                sumdelta = sumdelta + x * x
        if printflag:
            print
        return sumdelta


L1 = Layer(2)
L2 = Layer(1, L1)
L3 = Layer(1, L1 + L2)

pairs = [([-1, -1], [-1]),
         ([-1,  1], [ 1]),
         ([ 1, -1], [ 1]),
         ([ 1,  1], [-1])]
def iter(x):
    L1.force(x)
    L2.iterate()
    L3.iterate()
    return L3.output()

t = TrainingSet(pairs, iter, L3.adjust, 0.01)

def main():
    for i in range(5):
        t.train(100)
        print t.showoff(1)

if __name__ == "__main__":
    main()
