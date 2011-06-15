#!/usr/bin/env python

"""

This is some sample Python code that generates some input for sndblit.
It gives some idea of what the input is supposed to look like. You can
pipe the output directly into sndblit by typing:

   python foo.py | ./sndblit -o quux.wav
   play quux.wav

or you can pipe the output thru 'more' or 'less' to examine how the
information is organized. Also type './sndblit --help' to get more
information about what stuff means.

sndblit gives you the ability to fool with the phases of harmonics. I
haven't tinkered with that ability yet, so I don't have any good
examples of what's interesting. But it should make it possible to do
phase shift and phlanger effects.

"""

import os, sys, math, random

maxtime = 4.0
dt = 1./30.

class Note:
    def __init__(self, freq, start_time):
        self.time = start_time
        self.freq = freq

maxNoteDuration = 5.0

class Instrument:

    def basic_note(self, freq, dt):
        a = math.exp(-dt / 0.3)
        return [ [freq, a, 0, a, 0] ]

    def play_note(self, note, time):
        dt = time - note.time
        if dt < 0 or dt > maxNoteDuration:
            return None
        return self.basic_note(note.freq, dt)

halftone = 2.0 ** (1. / 12)
wholetone = halftone ** 2
C = 256
D = C * wholetone
E = D * wholetone
F = E * halftone
G = F * wholetone
A = G * wholetone
B = A * wholetone
C1 = B * halftone
D1 = C1 * wholetone
E1 = D1 * wholetone
F1 = E1 * halftone


score = [Note(C, 0.0),
         Note(D, 0.2),
         Note(E, 0.4),
         Note(F, 0.6),
         Note(G, 0.8),
         Note(A, 1.0),
         Note(B, 1.2),
         Note(C1, 1.4)]


class FourierSeriesInstrument(Instrument):

    "Based on a Fourier series of coefficient generator functions"

    def __init__(self, numHarmonics):
        self.numHarmonics = numHarmonics

    def coeff(self, freq, h, t):
        return ((1.2 ** h) *
                math.exp(-t / 0.8) *
                math.exp(-t / (0.5 + 0.1 * h)))

    def basic_note(self, freq, dt):
        lst = [ ]
        for i in range(1, self.numHarmonics + 1):
            b = self.coeff(freq, i, dt)
            lst.append( [ i * freq, b, 0, b, 0 ] )
        return lst


I = FourierSeriesInstrument(12)


bliplist = [ ]

for i in range(int(maxtime / dt)):
    thisblip = [ ]
    for note in score:
        z = I.play_note(note, i * dt)
        if z != None:
            thisblip = thisblip + z
    bliplist.append(thisblip)

print dt, len(bliplist)
for blip in bliplist:
    print len(blip)
    for wave in blip:
        for num in wave:
            print num,
        print
