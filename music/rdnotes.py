"""
rdnotes.py -- Python wrapper/glue for other music programs
Copyright (C) 2001 Will Ware

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

This program uses the MIDI parser to generate a note list and
translates it into an input file for the sound blitter. It defines
instruments/voices and assigns them to the MIDI channels. The sound
blitter uses Fourier synthesis of complex exponentials to generate
music and put it into a WAV file. These files can be played on Windows
and Linux boxes, and they can be compiled onto a normal music CD.
"""

import midi, time, os, random, types, getopt
from math import pi, e, cos, log, sqrt, atan2

quickFlag = 0     # render the tune fast, with low sampling rate

def timestamp(msg):
    print time.asctime(time.localtime(time.time())) + "  " + msg

#########################################################################
#########################################################################
#########################################################################

A = 2.0 ** (1.0 / 12.0)   # the ratio of frequencies for a halftone
B = 256.0 / (A ** 60)     # middle C is 256 Hz, I think

# To convert MIDI pitches to frequencies, compute B * A ** pitch

notelist = [ ]

class Note:
    def __init__(self, pitch, velocity, starttime, duration, voice):
        self.freq = B * A ** pitch
        self.velocity = velocity
        self.starttime = starttime
        # duration represents the time the piano key is held
        # down, endtime represents the end of the sound some
        # short time later
        self.duration = duration
        self.endtime = starttime + self.duration + 1.
        self.voice = voice
        self.randomdelay = 0.05 * random.random()
    def series(self):
        raise "Must be overloaded!"
    def at(self, t):
        retval = [ ]
        if self.freq == 0 or t < self.starttime or t > self.endtime:
            return retval
        v = self.velocity / 127.0
        delay = e ** (2j * pi * self.randomdelay)
        for (f, la, ra) in self.voice.at(self.freq,
                                         t - self.starttime,
                                         self.duration):
            mult = v * (delay ** f)    # velocity, phase delay
            retval.append((f * self.freq, la * mult, ra * mult))
        return retval

availableVoices = [
    "Sine",
    "Triangle",
    "Square",
    "Ramp",
    "RampWah",
    "RampVibrato",
    "ReedOrgan",
    "CustomVoice",
    "CustomVoiceTwo"
    ]


def register_note_single(track_index, channel_index, pitch, velocity,
                         keyDownTime, keyUpTime):
    try:
        voice = voiceDict[(track_index, channel_index)]
    except KeyError:
        voice = random.choice(availableVoices)
        print ("Using %s for track %d, channel %d" %
               (voice, track_index, channel_index))
        leftvol = random.random()
        voice = eval(voice)(leftvol)   # create an instance of this voice
        voiceDict[(track_index, channel_index)] = voice
    begin = miditicks2seconds * keyDownTime
    end = miditicks2seconds * keyUpTime
    n = Note(pitch, velocity, begin, end - begin, voice)
    notelist.append(n)

def register_note(track_index, channel_index, pitch, velocity,
                  keyDownTime, keyUpTime):
    # let's set up choruses
    if quickFlag:
        chorus_size = 1
    else:
        chorus_size = 5
    for i in range(chorus_size):
        register_note_single(track_index, channel_index,
                             pitch + (0.05 * (random.random() - .5)),
                             velocity, keyDownTime, keyUpTime)

midi.register_note = register_note

#########################################################################
#########################################################################
#########################################################################

profiling = 0

def play(notelist, wavefile, begintime=0, timelimit=1.0e10):

    def comparestarttime(note1, note2):
        return cmp(note1.starttime, note2.starttime)
    notelist.sort(comparestarttime)
    beginfirstnote = notelist[0].starttime
    endlastnote = -1.e10
    for n in notelist:
        if n.endtime > endlastnote:
            endlastnote = n.endtime
    endlastnote = endlastnote + 1  # just a little extra
    if begintime > beginfirstnote:
        beginfirstnote = begintime
    if timelimit < endlastnote:
        endlastnote = timelimit
    currentDict = { }
    currentKeys = [ ]
    pcount = 0
    bigindex = 0
    t = beginfirstnote
    steps = int((endlastnote - t) / (BLIPSIZE * DT))
    F = open("blips.tmp", "w")
    F.write("%f %d\n" % (BLIPSIZE * DT, steps))
    # make local versions of non-local things; this results
    # in a really surprising performance improvement
    cplx1, l_atan2, l_abs, l_len, F_write, blipsize_dt = \
             1+0j, atan2, abs, len, F.write, BLIPSIZE * DT
    for step in range(steps):

        if (pcount % 500) == 0:
            timestamp("%.1f / %.1f" % (t, endlastnote))
            pcount = 0
        pcount = pcount + 1

        # Compute all the phasors for the current blip.
        currentDict = { }
        currentHasKey = currentDict.has_key

        # play the notes that have already started
        for note in notelist:
            if note.starttime > t:
                break
            for (f, la, ra) in note.at(t):
                if currentHasKey(f):
                    lp, rp = currentDict[f]
                    la, ra = la + lp, ra + rp
                # make sure they're complex-valued
                currentDict[f] = (cplx1 * la, cplx1 * ra)

        currentKeys = currentDict.keys()
        F_write("%d\n" % l_len(currentKeys))
        for f in currentKeys:
            la, ra = currentDict[f]
            lph = l_atan2(la.imag, la.real)
            rph = l_atan2(ra.imag, ra.real)
            la = l_abs(la)
            ra = l_abs(ra)
            F_write("%f %f %f %f %f\n" % (f, la, lph, ra, rph))
        t = t + blipsize_dt

    F.close()
    timestamp("starting blitter")
    if os.name == "nt":
        sndblit = "sndblit.exe"
    else:
        sndblit = "./sndblit"
    sndblit = sndblit + " -o " + wavefile
    if quickFlag:
        sndblit = sndblit + " -s 12000"
    if not profiling:
        if os.system(sndblit + " < blips.tmp") != 0:
            raise "Blitter failed"
        os.remove("left.tmp")
        os.remove("right.tmp")
    timestamp("blitter done")
    os.remove("blips.tmp")

#########################################################################
#########################################################################
#########################################################################

def lowpass(fin, fc, zeta):
    s = 2j * pi * fin
    w0 = 2 * pi * fc
    return s**2 / (s**2 + 2 * zeta * w0 * s + w0**2)

def bandpass(fin, fc, zeta):
    s = 2j * pi * fin
    w0 = 2 * pi * fc
    return (w0 * s) / (s**2 + 2 * zeta * w0 * s + w0**2)

def highpass(fin, fc, zeta):
    s = 2j * pi * fin
    w0 = 2 * pi * fc
    return w0**2 / (s**2 + 2 * zeta * w0 * s + w0**2)

class FourierVoice:
    def __init__(self, leftvol=.5, rightvol=None):
        if rightvol == None:
            rightvol = 1. - leftvol
        series = self.series()
        if 1:
            # normalize the energy in this voice
            if len(series) > 0:
                e2sum = 0
                for (f,a) in series:
                    e2sum = e2sum + abs(a)**2
                m = 1. / sqrt(e2sum)
                for i in range(len(series)):
                    f, a = series[i]
                    series[i] = f, a * m * leftvol, a * m * rightvol
        else:
            for i in range(len(series)):
                f, a = series[i]
                series[i] = f, a * leftvol, a * rightvol
        if quickFlag:
            series = series[:4]
        self.computedSeries = series
        self.releasetime = 0.1
    def __add__(self, other):
        class VoiceSum(FourierVoice):
            def __init__(self, vox1, vox2):
                self.voxen = (vox1, vox2)
            def at(self, freq, t, duration):
                freqs = [ ]; fappend = freqs.append
                series1 = { }
                for (f, L, R) in self.voxen[0].at(freq, t, duration):
                    fappend(f)
                    series1[f] = (L, R)
                series2 = { }
                for (f, L, R) in self.voxen[1].at(freq, t, duration):
                    if f not in freqs: fappend(f)
                    series2[f] = (L, R)
                newlst = [ ]
                for f in freqs:
                    L, R = 0+0j, 0+0j
                    try:
                        L1, R1 = series1[f]
                        L, R = L + L1, R + R1
                    except KeyError:
                        pass
                    try:
                        L1, R1 = series2[f]
                        L, R = L + L1, R + R1
                    except KeyError:
                        pass
                    newlst.append((f, L, R))
                return newlst
        return VoiceSum(self, other)
    def __mul__(self, other):
        class VoiceProduct(FourierVoice):
            def __init__(self, basevoice, mult):
                self.basevoice, self.multiplier = basevoice, mult
            def at(self, freq, t, duration):
                series = self.basevoice.at(freq, t, duration)
                m = self.multiplier
                for i in range(len(series)):
                    f, la, ra = series[i]
                    series[i] = f, m * la, m * ra
                return series
        if (type(other) == types.FloatType or type(other) == types.IntType):
            return VoiceProduct(self, other)
        raise 'voices can only be multiplied by numbers'
    def __rmul__(self, other):
        return self.__mul__(other)
    def series(self):
        raise "Must be overloaded!"
    def ads(self, t, attack, decay, sustain):
        if attack > 0.001 and t < attack:
            return (1. - e**(-t/attack)) / (1. - 1./e)
        m = e ** -((t - attack) / decay)
        return m + (1 - m) * sustain
    def at(self, freq, t, duration):
        if t > duration:
            # here is the key-up case
            m = e ** -((t - duration) / self.releasetime)
            def modify(tup, m=m):
                f, la, ra = tup
                return (f, m * la, m * ra)
            return map(modify, self.keyDown(freq, duration))
        return self.keyDown(freq, t)
    def keyDown(self, freq, t):
        series = self.computedSeries[:]
        m = self.ads(t, 0, 0.5, 0.2)
        for i in range(len(series)):
            f, la, ra = series[i]
            series[i] = (f, m * la, m * ra)
        return series

class Silent(FourierVoice):
    def series(self):
        return [ ]

class Sine(FourierVoice):
    def series(self):
        return [(1, 1)]

class Triangle(FourierVoice):
    # Not sure about these coefficients
    def series(self):
        lst = [ ]
        for i in range(1, 30, 2):
            # almost surely the phase is wrong here, but the ear
            # doesn't perceive constant phase errors, luckily
            lst.append((i, 1.0 / i**2))
        return lst

class Square(FourierVoice):
    def series(self):
        lst = [ ]
        for i in range(1, 30, 2):
            lst.append((i, 1.j / i))
        return lst

class Ramp(FourierVoice):
    def series(self):
        lst = [ ]
        for i in range(1, 16):
            lst.append((i, 1.j / i))
        return lst

class RampWah(Ramp):
    def keyDown(self, freq, t):
        series = self.computedSeries[:]
        m = self.ads(t, 0.15, 0.5, 0.3)
        f0 = 2000 * self.ads(t, 0.3, 0.3, 0.2)
        for i in range(len(series)):
            f, la, ra = series[i]
            g = 2 * bandpass(f * freq, f0, 0.1)
            series[i] = (f, m * g * la, m * g * ra)
        return series

class RampVibrato(Ramp):
    # this sounds delightfully tacky, yet entrancingly gauche
    def keyDown(self, freq, t):
        series = self.computedSeries[:]
        m = self.ads(t, 0, 0.5, 0.3)
        vibrato = e ** (-t / .5) * cos(12 * pi * t)
        f0 = (.7 * vibrato + 1) * 3 * freq
        for i in range(len(series)):
            f, la, ra = series[i]
            g = 2 * bandpass(f * freq, f0, 0.1)
            series[i] = (f, m * g * la, m * g * ra)
        return series

class ReedOrgan(FourierVoice):
    def series(self):
        R = 16.5
        lst = [ ]
        for i in range(1, int(R)):
            lst.append((i, sqrt(R * R - i * i) / R))
        return lst

class CustomVoice(FourierVoice):
    # Hand-tweak the envelopes for each harmonic. This should give
    # a lot of flexibility in designing voices.
    envelopes = [(.8, 1., 1., .6, .55, .5, .45, .4),
                 (.2, .4, .5, .5, .8,  1.,  .7, .6),
                 (0., .1, .8, 1.,  1., .7,  .2, 0.),
                 (.4, .3, .2, .2, .3,  .6,  1., .8),
                 (.6, .7, .7, .8, .9,  .3,  .3, .3)]
    h = .1

    def __init__(self, leftvol=.5, rightvol=None):
        if rightvol == None:
            rightvol = 1. - leftvol
        series = [ ]
        h = self.h
        for j in range(3):
            m = (1. / (j + 1))
            for env in self.envelopes:
                L, R = m * leftvol, m * rightvol
                def thisharmonic(t, h=h, env=env, L=L, R=R):
                    i = int(t / h)
                    if i + 1 >= len(env):
                        a = env[-1] * e ** -(t / .5)
                    else:
                        a = env[i]
                        a = a + (env[i+1] - a) * (t - i * h) / h
                    return (a * L, a * R)
                series.append(thisharmonic)
        if quickFlag:
            series = series[:4]
        self.computedSeries = series
        self.releasetime = 0.1

    def keyDown(self, freq, t):
        retval = [ ]
        for i in range(len(self.computedSeries)):
            harmonicfunc = self.computedSeries[i]
            la, ra = harmonicfunc(t)
            retval.append((i + 1, la, ra))
        return retval


class CustomVoiceTwo(CustomVoice):
    envelopes = [(0., 1., .4, .8, .2, .4, .7, .6, .4, .3, .2),
                 (.8, .5, .2, .5, .8, .9, .9, .4, .2, .1, .6, .8, .8, .7),
                 (.2, .6, 1., 1., 1., 1., .6, .5, .5, .6, .4, .4, .6, .4),
                 (1., .8, .8, .7, .6, .5, .4, .2, 1., .9, .8, .7, .6),
                 (0., .1, .1, .2, .4, .6, 1., 1., .7, .4, 0.)]


class OrganLike(FourierVoice):
    # Hand-tweak the envelopes for each harmonic. This should give
    # a lot of flexibility in designing voices.
    def __init__(self, leftvol=None):
        if leftvol == None:
            leftvol = random.random()
        rightvol = 1. - leftvol
        series = [ ]
        for func in self.makeEnvelopeFunctions():
            def wrapper(t, func=func, L=leftvol, R=rightvol):
                x = func(t)
                return (L * x, R * x)
            series.append(wrapper)
        if quickFlag:
            series = series[:4]
        self.computedSeries = series
        self.releasetime = 0.1


    def makeEnvelopeFunctions(self):
        series = [ ]
        for harmonic in range(1, 20):
            def h(t, harmonic=harmonic):
                return e ** (-t * harmonic / 3.)
            series.append(h)
        return series

    def keyDown(self, freq, t):
        retval = [ ]
        for i in range(len(self.computedSeries)):
            harmonicfunc = self.computedSeries[i]
            la, ra = harmonicfunc(t)
            retval.append((i + 1, la, ra))
        return retval

class PipeOrgan1(OrganLike):
    abclist = [
        (0.0392203437135, 0.426589008641, 1.0),
        (0.293766309395, 0.365836245778, 1.0),
        (0.302345937994, 0.397615106181, 1.0),
        (0.083803871737, 0.382850307681, 1.0)
        ]
    def makeEnvelopeFunctions(self):
        series = [ ]
        for (a, b, c) in self.abclist:
            def expdiff(t, a=a, b=b):
                return e ** (-t/a) - e ** (-t/b)
            # find its maximum value, scale it to c
            m = c / expdiff(log(b/a) / (1./a - 1./b))
            # normalize it
            def h(t, expdiff=expdiff, m=m):
                return m * expdiff(t)
            series.append(h)
        return series

class PipeOrgan2(PipeOrgan1):
    abclist = [
        (0.0335000843592, 0.46309531216, 1.0),
        (0.030077699206, 0.144685724162, 1.0),
        (0.428397994031, 0.432456831859, 1.0),
        (0.00866443313813, 0.39820648669, 1.0)
        ]


#########################################################################

"""miditicks2seconds should really be derived from information in the
MIDI file rather than empirically tweaked by hand like this."""

# miditicks2seconds = .0007   # for sinfonia

# miditicks2seconds = .015   # air for the g string
miditicks2seconds = .002   # .003 is too slow, 001 too fast

# voices are indexed by tuples: (track_index, channel_index)
voiceDict = {
    (0, 1): RampWah(0.),
    (0, 3): Ramp(.3),
    (0, 5): Triangle(.7),
    (0, 6): Sine(1.),
    }

Passacaglia = 0

if Passacaglia:
    # Passacaglia and Fugue in C minor, .0015 is too fast, .0018 a little slow
    miditicks2seconds = .0012
    voiceDict = {
        # (3, 3): Triangle(.1),
        (3, 3): RampVibrato(.1),
        (1, 1): ReedOrgan(.5),
        (2, 2): RampWah(.9),
        }

    def register_note(track_index, channel_index, pitch, velocity,
                      keyDownTime, keyUpTime):
        try:
            voice = voiceDict[(track_index, channel_index)]
        except KeyError:
            voice = random.choice(availableVoices)
            print ("Using %s for track %d, channel %d" %
                   (vox, track_index, channel_index))
            voice = eval(voice)()   # create an instance of this voice
            voiceDict[(track_index, channel_index)] = voice
        begin = miditicks2seconds * keyDownTime
        end = miditicks2seconds * keyUpTime
        if track_index == 3:
            # the bass line is too low!
            pitch = pitch + 12
        n = Note(pitch, velocity, begin, end - begin, voice)
        notelist.append(n)

    midi.register_note = register_note



Brandenburg = 0

if Brandenburg:
    miditicks2seconds = .0016
    voiceDict = {
        (3, 3): Ramp(1.),
        (5, 5): Square(.8),
        (1, 2): ReedOrgan(.5),
        (4, 4): RampVibrato(.2),
        (2, 1): RampWah(0.),
        }
    voiceDict = {
        (3, 3): PipeOrgan1(1.),
        (4, 4): 2 * Triangle(.8),   # a bass line
        (1, 2): Ramp(.5),           # the lead voice
        (5, 5): Square(.2),
        (2, 1): 2 * Sine(0.)
        }


def main(argv):
    global quickFlag, SAMPLERATE, DT, BLIPSIZE, profiling
    quickFlag = 0
    midifile = None
    wavefile = "newguy.wav"
    starttime, finishtime = -1.e5, 1.e5
    optlist, args = getopt.getopt(argv[1:], "s:f:qi:o:p")
    for (option, value) in optlist:
        if option == "-s":
            starttime = eval(value)
        elif option == "-f":
            finishtime = eval(value)
        elif option == "-q":
            quickFlag = 1
        elif option == "-i":
            midifile = value
        elif option == "-o":
            wavefile = value
        elif option == "-p":
            profiling = 1
    if quickFlag:
        SAMPLERATE = 4000
    else:
        SAMPLERATE = 44100
    DT = 1./SAMPLERATE
    BLIPSIZE = int(0.02 / DT)
    timestamp("reading MIDI file")
    m = midi.MidiFile(midifile)
    m.read()
    m.close()
    def goAhead(play=play,notelist=notelist, wavefile=wavefile,
                starttime=starttime, finishtime=finishtime):
        play(notelist, wavefile, starttime, finishtime)
    if profiling:
        import profile
        globals()['goAhead'] = goAhead
        profile.run("goAhead()")
    else:
        goAhead()

if __name__ == "__main__":
    import sys
    main(sys.argv)
