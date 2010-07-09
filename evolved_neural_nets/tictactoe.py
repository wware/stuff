import math, numpy, types, pickle, random

"""
Serializing notes: Each layer in a network should have a unique id so that
it's easy to wire up the network correctly when de-serializing it.

"""

class Layer:
    def __init__(self, owner, width):
        self.owner = owner
        self.uid = owner.uniqueId()
        self.width = width
        self.inputlayers = [ ]
        offsets = numpy.random.normal(0, 1.0, width)
        offsets = numpy.matrix(offsets)
        offsets.resize((width, 1))
        self.offsets = offsets
        self.outputs = numpy.matrix(numpy.zeros((width, 1)))
    def __repr__(self):
        r = "<Layer@%d" % id(self)
        r += " " + repr(self.width)
        for cx, lx in self.inputlayers:
            pass
        return r + ">"
    def sameDimensions(self, other):
        if self.width != other.width:
            return False
        if len(self.inputlayers) != len(other.inputlayers):
            return False
        return True
    def __eq__(self, other):
        if self.width != other.width:
            return False
        if len(self.inputlayers) != len(other.inputlayers):
            return False
        for (cx,lx),(cy,ly) in map(None, self.inputlayers, other.inputlayers):
            if lx != ly:
                return False
            h = cx - cy
            for z in h.flatten().tolist()[0]:
                if abs(z) > 1.0e-10:
                    return False
        return True
    def addInputLayer(self, layer):
        inwidth = layer.width
        coeffs = numpy.random.normal(0, 1.0, inwidth * self.width)
        coeffs.resize((self.width, inwidth))
        coeffs = numpy.matrix(coeffs)
        self.inputlayers.append((coeffs, layer.uid))
    def compute(self, mult=2.0/math.pi):
        assert len(self.inputlayers) > 0
        getLayer, sums = self.owner.getLayerByUid, self.offsets
        for coeffs, layer in self.inputlayers:
            layer = getLayer(layer)
            sums += coeffs * layer.outputs
        self.outputs = outputs = mult * numpy.arctan(sums)
        return tuple(outputs.flatten().tolist()[0])
    def setoutputs(self, values):
        assert len(self.inputlayers) == 0
        assert type(values) in (types.ListType, types.TupleType)
        values = numpy.matrix(numpy.array(values, dtype=float))
        values.resize((self.width, 1))
        self.outputs = values
    def dumps(self):
        guts = (self.uid, self.width,
                self.inputlayers, self.offsets, self.outputs)
        return pickle.dumps(guts)
    def loads(self, owner, s):
        self.uid, self.width, Z, offsets, outputs = pickle.loads(s)
        self.offsets = numpy.matrix(offsets)
        self.outputs = numpy.matrix(outputs)
        self.inputlayers = [ ]
        for coeff, uid in Z:
            self.inputlayers.append((numpy.matrix(coeff), uid))
    def breed(self, other, newowner):
        def breedMatrices(mat1, mat2):
            assert mat1.shape == mat2.shape
            r = numpy.random.normal(0, 1, mat1.shape)
            return numpy.matrix((r > 0) * numpy.array(mat1) +
                                (r < 0) * numpy.array(mat2))
        assert self.sameDimensions(other)
        newguy = Layer(newowner, self.width)
        # uid, width, offsets, outputs, inputlayers
        newguy.outputs = breedMatrices(self.outputs, other.outputs)
        newguy.offsets = breedMatrices(self.offsets, other.offsets)
        newguy.inputlayers = [ ]
        for (cx, lx), (cy, ly) in map(None, self.inputlayers, other.inputlayers):
            assert lx == ly
            newguy.inputlayers.append((breedMatrices(cx, cy), lx))
        return newguy

class Network:
    def __init__(self, layersizes=(0,)):
        self._uid = 100
        self.layersizes = layersizes
        thislayer = Layer(self, layersizes[0])
        self.layers = [ thislayer ]
        self.layersByUid = { thislayer.uid: thislayer }
        for width in layersizes[1:]:
            w = Layer(self, width)
            self.layersByUid[w.uid] = w
            w.addInputLayer(thislayer)
            self.layers.append(w)
            thislayer = w
    def uniqueId(self):
        u = self._uid
        self._uid += 1
        return u
    def __eq__(self, other):
        if self.layersizes != other.layersizes:
            return False
        for x, y in map(None, self.layers, other.layers):
            if not x == y:
                return False
        return True
    def compute(self, inputs):
        assert type(inputs) in (types.ListType,
                                types.TupleType)
        self.layers[0].setoutputs(inputs)
        for L in self.layers[1:]:
            x = L.compute()
        return x
    def dumps(self):
        return pickle.dumps((self.layersizes,
                             map(lambda x: x.dumps(), self.layers)))
    def loads(self, s):
        self.layersizes, L = pickle.loads(s)
        self.layers = [ ]
        for x in L:
            layer = Layer(self, 0)
            layer.loads(self, x)
            self.layersByUid[layer.uid] = layer
            self.layers.append(layer)
    def getLayerByUid(self, uid):
        return self.layersByUid[uid]
    def breed(self, other):
        assert self.layersizes == other.layersizes
        newguy = Network(tuple(list(self.layersizes)))
        newguy.layers = [ ]
        for L1, L2 in map(None, self.layers, other.layers):
            L3 = L1.breed(L2, newguy)
            newguy.layers.append(L3)
        return newguy

if True:
    def checkersNetwork():
        n = Network((32, 40, 12, 1))
        # connect input layer directly to output
        n.layers[3].addInputLayer(n.layers[0])
        # TODO - also need a representation for the value of a king
        return n

    n = checkersNetwork()
    n2 = Network()
    n2.loads(n.dumps())

    assert n == n2

    assert (n.compute(map(math.sin, range(32))) ==
            n2.compute(map(math.sin, range(32))))

class Board:
    def __init__(self):
        self.squares = (0,0,0,0,0,0,0,0,0)
    def __repr__(self):
        def sq(i, q=self.squares):
            if q[i] == 1:
                return 'X'
            elif q[i] == -1:
                return 'O'
            else:
                return ' '
        return (("%c|%c|%c\n" % (sq(0), sq(1), sq(2))) +
                "-+-+-\n" +
                ("%c|%c|%c\n" % (sq(3), sq(4), sq(5))) +
                "-+-+-\n" +
                ("%c|%c|%c" % (sq(6), sq(7), sq(8))))
    def copy(self):
        b = Board()
        b.squares = tuple(list(self.squares))
        return b
    def __eq__(self, other):
        return self.squares == other.squares
    def __getitem__(self, i):
        return self.squares[i]
    def flip(self):
        self.squares = tuple(map(lambda x: -x, self.squares))
    def setsquare(self, i):
        L = list(self.squares)
        L[i] = 1
        self.squares = tuple(L)
    def draw(self):
        if self.win(1) or self.win(-1):
            return False
        for x in self.squares:
            if abs(x) == 0:
                return False
        return True
    def win(self, x=1):
        for (i,j,k) in ((0,1,2),(3,4,5),(6,7,8),
                        (0,3,6),(1,4,7),(2,5,8),
                        (0,4,8),(2,4,6)):
            if (self.squares[i] == x and
                self.squares[j] == x and
                self.squares[k] == x):
                return True
        return False

class TicTacToePlayer:
    def __init__(self):
        #self.network = Network((9, 20, 6, 1))
        self.network = Network((9, 6, 4, 1))
        self.rating = 0
    def __cmp__(self, other):
        return -cmp(self.rating, other.rating)
    def move(self, board):
        possibilities = [ ]
        for i in range(9):
            if board[i] == 0:
                # this is a potential move
                newboard = board.copy()
                newboard.setsquare(i)
                if not newboard == board:
                    merit = self.network.compute(newboard.squares)
                    possibilities.append((merit, newboard))
        assert len(possibilities) > 0
        possibilities.sort()
        return possibilities[-1][1]
    def breed(self, other):
        newguy = TicTacToePlayer()
        newguy.network = self.network.breed(other.network)
        return newguy

def play(player1, player2, show=False):
    b = Board()
    while True:
        b = player1.move(b)
        if show:
            print b
            print
        if b.win():
            return 1
        if b.draw():
            return 0
        b.flip()
        b = player2.move(b)
        b.flip()
        if show:
            print b
            print
        if b.win(-1):
            return -1
        if b.draw():
            return 0

N = 100
roster = [ ]
for i in range(N):
    player = TicTacToePlayer()
    roster.append(player)

def tournament():
    for i in range(N):
        player.rating = 0.
    for i in range(N):
        print i,
        for j in range(N):
            result = play(roster[i], roster[j])
            if result > 0:
                roster[i].rating += 1
                roster[j].rating -= 1
            elif result < 0:
                roster[j].rating += 1
                roster[i].rating -= 1
    print
    roster.sort()

for generation in range(100):
    for i in range(N/2):
        a = roster[random.randint(0, N/2)]
        b = roster[random.randint(0, N/2)]
        newguy = a.breed(b)
        roster[i+N/2] = newguy
    tournament()

best, good = roster[0], roster[1]

print play(best, good, True)
