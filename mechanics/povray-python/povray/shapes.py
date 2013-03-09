from povray.textures import Wood
from povray.transforms import Transformable

PLYWOOD_THICKNESS = 0.25

def render_prism(output, y1, y2, vertices, *other):
	output.write("prism {\n")
	output.write("  linear_sweep\n")
	output.write("  linear_spline\n")
	output.write("  %f, %f, %d\n" % (y1, y2, len(vertices)))
	vertices = map(lambda vertex: "<%f, %f>" % tuple(vertex), vertices)
	output.write("  " + ", ".join(vertices))
	[x.render(output) for x in other]
	output.write("}\n")


class LaserCutPlywood(Transformable):

    y1, y2 = 0, PLYWOOD_THICKNESS

    vertices = []

    def render(self, output):
        render_prism(output, self.y1, self.y2, self.vertices, Wood)
