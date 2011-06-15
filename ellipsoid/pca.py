import sys, random, Numeric, LinearAlgebra

# We have a bunch of vectors x, and a rotation matrix A which maps
# them to y E[yyT] is diagonal, so the ellipsoid's axes lie along the
# axes of the y frame Then we have E[yyT] = A Lx AT where Lx=E[xxT] is
# x's covariance matrix.

# http://en.wikipedia.org/wiki/Principal_components_analysis

# My interest in this is that I want to be able to form 3D ellipsoids
# that represent the thermal motion of atoms, so that I can represent
# them on a time scale where thermal motion is a blur.

# This slower time scale is the one where Drexler envisions molecular
# machines performing designed motions. If the time scale of designed
# motions is too close to the time scale of thermal motion, a few
# really bad things happen. One is that the machine's operation will
# heat it up, and without a cooling mechanism, it might get hot enough
# for bonds to break. Another is that the coupling between designed
# motion and thermal motion will act like friction.

DIMENSIONS = 2

class RandomPointGenerator:

    class Point:
        def __init__(self, dimensions):
            v = [ ]
            for i in range(dimensions):
                v.append(2 * random.random() - 1)
            if Numeric.vdot(v, v) >= 1.0:
                raise Exception
            self.v = Numeric.reshape(Numeric.array(v), (DIMENSIONS, 1))

    def __init__(self, dimensions):
        self.dimensions = dimensions
        tweak = [ ]
        for i in range(dimensions * dimensions):
            tweak.append(5 * random.random() - 2)
        self.tweakmatrix = Numeric.reshape(Numeric.array(tweak),
                                           (dimensions, dimensions))

    def makePoint(self):
        p = self.Point(self.dimensions)
        p.v = Numeric.matrixmultiply(self.tweakmatrix, p.v)
        return p


class Ellipsoid:

    def __init__(self):
        self.points = [ ]
        self.mu = Numeric.zeros((DIMENSIONS, 1))

    def addPoint(self, pt):
        assert len(pt) == DIMENSIONS
        pt = Numeric.reshape(Numeric.array(pt), (DIMENSIONS, 1))
        self.points.append(pt)
        self.mu = self.mu + pt

    def compute(self):
        N = len(self.points)
        Ninv = 1. / N
        # get the mean, and deviations from the mean
        mu = Ninv * self.mu
        devs = map(lambda pt,mu=mu: pt - mu, self.points)
        # get a covariance matrix
        C = Numeric.zeros((DIMENSIONS, DIMENSIONS))
        for x in devs:
            C = C + Numeric.matrixmultiply(x,
                                           Numeric.transpose(x))
        C = Ninv * C
        # sort eigenvalue/vector pairs in order of decreasing eigenvalue
        evals, V = LinearAlgebra.eigenvectors(C)
        pairs = map(None, evals, V)
        pairs.sort(lambda x, y: cmp(y, x))
        self.eigenvalues = map(lambda p: p[0], pairs)
        A = Numeric.array(map(lambda x: x[1], pairs))
        self.A = Numeric.transpose(A)

    def draw(self, outf=sys.stdout):
        # handle only the 2D case for now
        # draw stuff by emitting bits of Postscript
        outf.write("%!PS\n")
        INCH = 72.0
        RED = (1.0, 0.0, 0.0)
        GREEN = (0.0, 1.0, 0.0)
        BLUE = (0.0, 0.0, 1.0)
        def postscriptColor(color):
            outf.write("%f %f %f setrgbcolor\n" % (color))
        def postscriptPoint(x,y):
            return ((4.25 + x) * INCH,
                    (5.5 - y) * INCH)
        def postscriptLine(x1,y1,x2,y2):
            x1, y1 = postscriptPoint(x1, y1)
            x2, y2 = postscriptPoint(x2, y2)
            outf.write("%f %f moveto\n%f %f lineto\nstroke\n" %
                       (x1, y1, x2, y2))
        def postscriptX(x,y):
            h = 0.05
            postscriptLine(x-h,y-h,x+h,y+h)
            postscriptLine(x-h,y+h,x+h,y-h)
        def rotate(x, y):
            v = Numeric.array([[x], [y]])
            v = Numeric.matrixmultiply(self.A, v)
            return [v[0][0], v[1][0]]
        # Draw all the points
        for p in self.points:
            x, y = p[0][0], p[1][0]
            postscriptX(x, y)
        # Draw the axes of the ellipse
        outf.write("5 setlinewidth\n")
        eig = apply(rotate, [2*self.eigenvalues[0]**.5, 0.0])
        postscriptColor(RED)
        postscriptLine(0, 0, eig[0], eig[1])
        eig = apply(rotate, [0.0, 2*self.eigenvalues[1]**.5])
        postscriptColor(GREEN)
        postscriptLine(0, 0, eig[0], eig[1])
        outf.write("showpage\n")

g = RandomPointGenerator(DIMENSIONS)
e = Ellipsoid()
for i in range(300):
    try:
        x = g.makePoint()
        e.addPoint(x.v)
    except:
        pass
e.compute()
print e.A
print e.eigenvalues

outf = open("foo.ps", "w")
e.draw(outf)
outf.close()

# Run ps2pdf or use ImageMagick's convert to get an image.
