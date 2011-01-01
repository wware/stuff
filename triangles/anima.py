#!/usr/bin/env python

import math, sys, os, random, types, numpy

os.system('make zbuf.so')
import zbuf

# width = 1152  # make a pretty background for hrothgar
width = 200  # quick experiment

height = int(.75 * width)
# numframes = 50
numframes = 15

# Vertices are functions that accept a time argument, and
# return a 6-tuples: (x, y, z, r, g, b).

def moving(xlo, xhi, x0=None):
    if x0 == None:
        x0 = xlo + (xhi - xlo) * random.random()
    # oops, time is already scaled to account for numframes
    a = .6
    b = .4
    dx  = a * (xhi - xlo) * (2 * random.random() - 1)
    dx2 = a * b * (xhi - xlo) * (2 * random.random() - 1)
    dx3 = a * b * b * (xhi - xlo) * (2 * random.random() - 1)
    def func1(t,xlo=xlo,xhi=xhi,x0=x0,
              dx=dx,dx2=dx2,dx3=dx3,sin=math.sin):
        t = 2 * math.pi * t
        x = x0 + dx * sin(t) + dx2 * sin(2 * t) + dx3 * sin(3 * t)
        return min(max(x, xlo), xhi)
    return func1

class Vertex:
    def __init__(self, center=None, color=None):
        if center == None:
            center = (moving(0, width),
                      moving(0, height),
                      moving(0, width / 3))
        if color == None:
            color = (moving(0, width),
                     moving(0, height),
                     moving(0, width / 3))
        self.tup = center + color
    def __call__(self, t=0.):
        lst = [ ]
        for x in self.tup:
            lst.append(x(t))
        return tuple(lst)

class Triangle:
    def __init__(self, vertex0, vertex1, vertex2):
        self.vertex0 = vertex0
        self.vertex1 = vertex1
        self.vertex2 = vertex2
    def xy_area(self, time=0.):
        v0, v1, v2 = self.vertex0, self.vertex1, self.vertex2
        v0 = v0(time)
        v1 = v1(time)
        v2 = v2(time)
        return .5 * abs((v1[0] - v0[0]) * (v2[1] - v0[1]) -
                        (v2[0] - v0[0]) * (v1[1] - v0[1]))
    def render(self, ytop, time=0.):
        v0, v1, v2 = self.vertex0, self.vertex1, self.vertex2
        v0 = v0(time)
        v1 = v1(time)
        v2 = v2(time)
        apply(zbuf.tri, (ytop,) + v0 + v1 + v2)

class Sphere:
    def __init__(self, center, color, radius):
        # have the whole thing move around a little bit
        center = (moving(0, width, center[0]),
                  moving(0, height, center[1]),
                  moving(0, width / 3, center[2]),
                  lambda t,r=color[0]: r,
                  lambda t,g=color[1]: g,
                  lambda t,b=color[2]: b)
        d = (2 * random.random() - 1,
             2 * random.random() - 1,
             2 * random.random() - 1)
        d = numpy.multiply((1. / radius) /
                           math.sqrt(numpy.dot(d, d)),
                           d)
        while 1:
            dcolor=(256 * random.random() - 128,
                    256 * random.random() - 128,
                    256 * random.random() - 128)
            if math.sqrt(numpy.dot(dcolor, dcolor)) > 200:
                break
        def coloredVertex(center, color, direction=d, dcolor=dcolor):
            dotproduct = numpy.dot(center, direction)
            return (center +
                    (min(max(color[0] + dotproduct * dcolor[0], 0), 255),
                     min(max(color[1] + dotproduct * dcolor[1], 0), 255),
                     min(max(color[2] + dotproduct * dcolor[2], 0), 255)))
        triangles = [ ]
        slices = hcuts = 20
        northPole = coloredVertex((0,0,radius), color)
        southPole = coloredVertex((0,0,-radius), color)
        vertices = [northPole, southPole]
        # build latitude circles, j = latitude, i = longitude
        # if phase is zero, it's a big problem! don't know why!
        # it perhaps points to an error in the triangle rendering code
        phase = math.pi / (slices * 3)
        for j in range(1, hcuts):
            z = radius * math.cos(j * math.pi / hcuts)
            r = radius * math.sin(j * math.pi / hcuts)
            oneCircle = [ ]
            for i in range(slices):
                x = r * math.cos(2 * math.pi * i / slices + phase)
                y = r * math.sin(2 * math.pi * i / slices + phase)
                v = coloredVertex((x,y,z), color)
                assert(len(v) == 6)
                oneCircle.append(v)
            vertices = vertices + oneCircle

        # translate the sphere into position, set base color
        m = [[1,0,0],[0,1,0],[0,0,1]]
        for i in range(2):
            z = 2 * math.pi * random.random()
            c, s = math.cos(z), math.sin(z)
            m = numpy.dot([[c,s,0],[-s,c,0],[0,0,1]], m)
            z = 2 * math.pi * random.random()
            c, s = math.cos(z), math.sin(z)
            m = numpy.dot([[c,0,s],[0,1,0],[-s,0,c]], m)
            z = 2 * math.pi * random.random()
            c, s = math.cos(z), math.sin(z)
            m = numpy.dot([[1,0,0],[0,c,s],[0,-s,c]], m)
        for i in range(len(vertices)):
            xyz = numpy.dot(m, vertices[i][:3])
            color = list(vertices[i][3:])
            for j in range(3):
                color[j] = lambda t,x=max(10, color[j]): x
            vertices[i] = tuple(xyz) + tuple(color)

        # ok, let's have the whole thing move around a little bit
        newverts = [ ]
        for v in vertices:
            def func3(t,v=v,center=center):
                return (v[0] + center[0](t),
                        v[1] + center[1](t),
                        v[2] + center[2](t),
                        v[3](t), v[4](t), v[5](t))
            newverts.append(func3)
        vertices = newverts

        # build the triangles connecting the poles with the first
        # and last latitude circles
        fillratio = .4
        def latlong(lat,long,hcuts=hcuts,slices=slices):
            assert 1 <= lat < hcuts
            assert 0 <= long < slices
            return 2 + ((lat - 1) * slices) + long
        for i in range(slices):
            i1 = (i + 1) % slices
            if random.random() < fillratio:
                triangles.append(Triangle(vertices[0], # north pole
                                          vertices[latlong(1,i)],
                                          vertices[latlong(1,i1)]))
            if random.random() < fillratio:
                triangles.append(Triangle(vertices[1], # south pole
                                          vertices[latlong(hcuts-1,i)],
                                          vertices[latlong(hcuts-1,i1)]))
        # build triangles connecting each latitude circle with the next
        for i in range(slices):
            for j in range(1, hcuts-1):
                i1 = (i + 1) % slices
                northwest = latlong(j, i)
                northeast = latlong(j, i1)
                southwest = latlong(j+1, i)
                southeast = latlong(j+1, i1)
                if random.random() < fillratio:
                    triangles.append(Triangle(vertices[northwest],
                                              vertices[southeast],
                                              vertices[northeast]))
                    triangles.append(Triangle(vertices[northwest],
                                              vertices[southeast],
                                              vertices[southwest]))
        self.triangles = triangles
    def render(self, ytop, time):
        for t in self.triangles:
            t.render(ytop, time)

def main(numtriangles, numspheres):
    os.system('rm -f foobar*.ppm')
    bandheight = min(int(200000 / width), height)
    frameimagetype = 'ppm'
    animationtype = 'mpg'
    zbuf.init(width, bandheight)
    renderlist = [ ]
    for i in range(numtriangles):
        area = width * height
        while area > .05 * width * height:
            t = Triangle(Vertex(),
                         Vertex(),
                         Vertex())
            area = t.xy_area()
        renderlist.append(t)
    for i in range(numspheres):
        renderlist.append(Sphere((width * random.random(),
                                  height * random.random(),
                                  (width / 3) * random.random()),
                                 (32 + 192 * random.random(),
                                  32 + 192 * random.random(),
                                  32 + 192 * random.random()),
                                 width * (.1 * random.random() + .05)))
    for frame in range(numframes):
        print 'Frame %d ' % frame,
        sys.stdout.flush()
        outf = open('tmp.rgb', 'w')
        lines = 0
        while lines < height:
            dlines = min(lines + bandheight, height) - lines
            # zbuf.clear(40, 60, 80)
            zbuf.clear(0, 0, 0)
            for r in renderlist:
                r.render(lines, 1. * frame / numframes)
            outf.write(zbuf.tostring()[:3*width*dlines])
            sys.stdout.write(".")
            sys.stdout.flush()
            lines = lines + dlines
        print
        outf.close()
        s = ('convert -size %dx%d tmp.rgb foobar%03d.%s; rm -f tmp.rgb'
             % (width, height, frame, frameimagetype))
        os.system(s)
    if numframes > 1:
        if animationtype == 'avi':
            s = '../mkavi/mkavi -file foobar.avi foobar*.ppm'
        elif animationtype == 'mpg':
            if frameimagetype != 'ppm':
                raise 'to make an MPEG animation you must use PPM frames'
            parfile = open('PPMtoMPEG.par').read()
            pfile = open('tmp.par', 'w')
            print parfile
            pfile.write(parfile % {'inputfilename': 'foobar%03d',
                                   'width': width,
                                   'height': height,
                                   'numframes': numframes
                                  })
            pfile.close()
            s = 'mpeg2encode tmp.par foobar.mpg'
        else:
            s = 'convert '
            if animationtype == 'gif':
                s = s + '-loop 10000 '
            s = s + ('-size %dx%d foobar*.%s foobar.%s'
                     % (width, height, frameimagetype, animationtype))
        print s
        os.system(s)

if __name__ == '__main__':
    numtriangles = eval(sys.argv[1])
    numspheres = eval(sys.argv[2])
    main(numtriangles, numspheres)
