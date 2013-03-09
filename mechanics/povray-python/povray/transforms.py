from math import cos, sin, pi


class Transformation:

    def __init__(self, obj, m00, m01, m02, m10, m11, m12, m20, m21, m22, m30, m31, m32):
        self.obj = obj
        self.m00, self.m01, self.m02 = m00, m01, m02
        self.m10, self.m11, self.m12 = m10, m11, m12
        self.m20, self.m21, self.m22 = m20, m21, m22
        self.m30, self.m31, self.m32 = m30, m31, m32

    def render(self, output):
        output.write("union {\n")
        self.obj.render(output)
        output.write("matrix <%f, %f, %f,\n" % (self.m00, self.m01, self.m02))
        output.write("  %f, %f, %f,\n" % (self.m10, self.m11, self.m12))
        output.write("  %f, %f, %f,\n" % (self.m20, self.m21, self.m22))
        output.write("  %f, %f, %f>\n" % (self.m30, self.m31, self.m32))
        output.write("}\n")


class Transformable:

    def translate(self, x, y, z):
        return Transformation(self, 1, 0, 0, 0, 1, 0, 0, 0, 1, x, y, z)

    def stretch(self, x, y, z):
        return Transformation(self, x, 0, 0, 0, y, 0, 0, 0, z, 0, 0, 0)

    def rotateX(self, theta):
        ct, st = cos(pi * theta / 180.0), sin(pi * theta / 180.0)
        return Transformation(self, 1, 0, 0, 0, ct, st, 0, -st, ct, 0, 0, 0)

    def rotateY(self, theta):
        ct, st = cos(pi * theta / 180.0), sin(pi * theta / 180.0)
        return Transformation(self, ct, 0, -st, 0, 1, 0, st, 0, ct, 0, 0, 0)

    def rotateZ(self, theta):
        ct, st = cos(pi * theta / 180.0), sin(pi * theta / 180.0)
        return Transformation(self, ct, st, 0, -st, ct, 0, 0, 0, 1, 0, 0, 0)
