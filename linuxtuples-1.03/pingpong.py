#!/usr/bin/python
#
# pingpong.py
# Test program for testing the latency of linuxtuples.
# It pingpongs tuples between two clients.
# First start a server, then execute two clients on two machines
# $ ./pingpong.py foo bar
# $ ./pingpong.py bar foo
# My calculations give me roughly 1100 pinpongs per second.
# Gigabit or 100Mb makes little difference. Locally, it is a little faster.
# Also, n-way pongs are possible
# $ ./pingpong.py tic toe
# $ ./pingpong.py tac tic
# $ ./pingpong.py toe tac
#


import sys
import os
import time
import linuxtuples

if len(sys.argv) != 3 :
  print "Usage:", sys.argv[0], "have want"
  sys.exit(1)

conn = linuxtuples.connect()

counter = 0

h = sys.argv[1]
w = sys.argv[2]

have=h
want=w

for i in range(1000) :
  conn.put((have, counter))
  rv=conn.get((want, None))
  if not i :
    time0 = os.times()[4]
  counter += 1
time1 = os.times()[4]

print "Elapsed:", time1 - time0


