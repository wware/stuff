#!/usr/bin/python

import Image
import ImageDraw
import random
import sys

width, height = 400, 600

buf = (6 * width * height) * "\xff"

im = Image.frombuffer("RGBA", (width, height), buf)

draw = ImageDraw.Draw(im)

def randomXY():
    return (int(random.random() * im.size[0]),
            int(random.random() * im.size[1]))

for n in range(25):
    x1, y1 = randomXY()
    x2, y2 = randomXY()
    xn, yn = y1 - y2, x2 - x1
    dy = (xn**2 + yn**2) ** 0.5
    scalar = (10 + 20 * random.random()) / dy
    x3, y3 = x2 + xn * scalar, y2 + yn * scalar
    x4, y4 = x3 + x1 - x2, y3 + y1 - y2
    r = int(256 * random.random())
    g = int(256 * random.random())
    b = int(256 * random.random())
    color = "#%02X%02X%02X" % (r, g, b)
    draw.polygon((x1, y1, x2, y2, x3, y3, x4, y4),
                 fill=color)

del draw 

# write to stdout
im.save(sys.stdout, "PNG")
