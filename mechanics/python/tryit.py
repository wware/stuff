"""<?xml version="1.0" encoding="iso-8859-1"?>
<xode>
  <world name="world">
    <space>

      <body name="body1">
        <transform>
          <position x="1" y="2" z="0"/>
        </transform>
        <mass>
          <mass_shape density="2500">
            <sphere radius="0.05"/>
          </mass_shape>
        </mass>
      </body>

      <body name="body2">
        <transform>
          <position x="2" y="2" z="0"/>
        </transform>
        <mass>
          <mass_shape density="5000">
            <sphere radius="0.05"/>
          </mass_shape>
        </mass>

        <joint name="joint2">
          <link1 body="body1"/>
          <ball>
            <anchor x="1" y="2" z="0"/>
          </ball>
        </joint>
      </body>

      <joint name="joint1">
        <link1 body="body1"/>
        <ball>
          <anchor x="0" y="2" z="0"/>
        </ball>
      </joint>

    </space>
  </world>
</xode>
"""

import sys
import pygame
from pygame.locals import *
import ode
import xode.parser
import time

def coord(x,y):
    "Convert world coordinates to pixel coordinates."
    return 320+170*x, 400-170*y

def buildObjectsXODE():
    p = xode.parser.Parser()
    root = p.parseString(__doc__)

    world = root.namedChild('world').getODEObject()
    body1 = root.namedChild('body1').getODEObject()
    body2 = root.namedChild('body2').getODEObject()
    j1 = root.namedChild('joint1').getODEObject()
    j2 = root.namedChild('joint2').getODEObject()

    world.setGravity((0,-9.81,0))

    return world, body1, body2, j1, j2

def simulate(world, body1, body2):
    # Initialize pygame
    pygame.init()
    
    # Open a display
    srf = pygame.display.set_mode((640,480))

    clk = pygame.time.Clock()

    # Keep the window open and wait for a key
    fps = 50
    dt = 1.0/fps
    loopFlag = True
    while loopFlag:
        events = pygame.event.get()
        for e in events:
            if e.type==QUIT:
                loopFlag=False
                
        # Clear the screen
        srf.fill((255,255,255))

        x1,y1,z1 = body1.getPosition()
        x2,y2,z2 = body2.getPosition()
        pygame.draw.circle(srf, (55,0,200), coord(x1,y1), 20, 0)
        pygame.draw.line(srf, (55,0,200), coord(0,2), coord(x1,y1), 2)
        pygame.draw.circle(srf, (55,0,200), coord(x2,y2), 20, 0)
        pygame.draw.line(srf, (55,0,200), coord(x1,y1), coord(x2,y2), 2)
        
        pygame.display.flip()

        world.step(dt)
        
        clk.tick(fps)


world, body1, body2, j1, j2 = buildObjectsXODE()
simulate(world, body1, body2)
