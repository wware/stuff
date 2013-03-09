import sys

from povray.shapes import LaserCutPlywood
from povray.basics import Scene, Includes, Camera, LightSource, Background


class AndBar(LaserCutPlywood):

    vertices = [(1, 0), (2, 0), (2, 2), (3, 2), (3, 3), (2, 3),
                (2, 11), (3, 11), (3, 12), (2, 12), (2, 14), (1, 14),
                (1, 7), (0, 7), (0, 6), (1, 5), (1, 0)]


s = Scene()
s.append(Background("Cyan"))
s.append(Camera((0, 30, -6)))
s.append(LightSource())
s.append(AndBar().rotateY(90))

s.render(sys.stdout)
