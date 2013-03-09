from transforms import Transformation


class Transformable:

    def __init__(self):
        self.transforms = []

    def transform(self, t):
        self.transforms.append(t)
        return self   # chainable

    def render_transforms(self, output):
        assert not (None in self.transforms), self
        [t.render(output) for t in self.transforms]

    def translate(self, x, y, z):
        self.transform(Transformation.translate(x, y, z))
        return self

    def stretch(self, x, y, z):
        self.transform(Transformation.stretch(x, y, z))
        return self

    def rotateX(self, theta):
        self.transform(Transformation.rotateX(theta))
        return self

    def rotateY(self, theta):
        self.transform(Transformation.rotateX(theta))
        return self

    def rotateZ(self, theta):
        self.transform(Transformation.rotateX(theta))
        return self


class Scene(list):
    def __init__(self):
        self.append(Includes("colors, woods, stones2"))

    def render(self, output):
        for x in self:
            x.render(output)


class Includes:
    def __init__(self, files):
        self.files = files.split(",")

    def render(self, output):
        for f in self.files:
            output.write("#include \"" + f.strip() + ".inc\"\n")


class Background:
    def __init__(self, color):
        self.color = color

    def render(self, output):
        output.write("background { color " + self.color + " }\n")


class Camera:
    def __init__(self, where=None):
        self.where = (0, 2, -3) if where is None else where

    def render(self, output):
        output.write("camera {\n")
        output.write("  location <%f, %f, %f>\n" % self.where)
        #output.write("  look_at  <0, 1,  2>\n")
        output.write("  look_at  <0, 0, 0>\n")
        output.write("}\n")


class LightSource:
    def render(self, output):
        output.write("light_source {\n")
        output.write("  <2, 4, -3>\n")
        output.write("  color White\n")
        output.write("}\n")
