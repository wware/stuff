#!/usr/bin/python

import Image
import ImageDraw
import random
import sys

width, height = 600, 800

buf = (6 * width * height) * "\xff"

im = Image.frombuffer("RGBA", (width, height), buf,
                      "raw", "RGBA", 0, 1)

draw = ImageDraw.Draw(im)

def randomXY():
    a = 1.2
    b = (a - 1.0) / 2
    return (int((a * random.random() - b) * im.size[0]),
            int((a * random.random() - b) * im.size[1]))

def randomColor():
    r = int(256 * random.random())
    g = int(256 * random.random())
    b = int(256 * random.random())
    #a = int(256 * random.random())
    #return "#%02X%02X%02X%02X" % (r, g, b, a)
    return "#%02X%02X%02X" % (r, g, b)

def randomRectangle():
    x1, y1 = randomXY()
    x2, y2 = randomXY()
    xn, yn = y1 - y2, x2 - x1
    dy = (xn**2 + yn**2) ** 0.5
    scalar = (10 + 20 * random.random()) / dy
    x3, y3 = x2 + xn * scalar, y2 + yn * scalar
    x4, y4 = x3 + x1 - x2, y3 + y1 - y2
    draw.polygon((x1, y1, x2, y2, x3, y3, x4, y4),
                 fill=randomColor())

def randomCircle():
    x1, y1 = randomXY()
    r = 30 + 100 * random.random()
    bbox = (x1 - r/2, y1 - r/2, x1 + r/2, y1 + r/2)
    draw.ellipse(bbox, fill=randomColor())

for n in range(50):
    if random.random() > 0.5:
        randomRectangle()
    else:
        randomCircle()

del draw 

# write to stdout
im.save(sys.stdout, "PNG")
