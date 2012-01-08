import wave
from math import pi, sin, exp, log

T = 3
ATTACK = 0.05
DECAY = 0.5
FREQUENCY = 1046.5  # C two octaves above middle C

if 1:
    SAMPFREQ = 16000
    NCHANNELS = 1
    NBYTES = 1
else:
    SAMPFREQ = 44100
    NCHANNELS = 2
    NBYTES = 2

DT = 1.0 / SAMPFREQ
N = int(T / DT)

AMPL = (NBYTES == 1) and 100 or 25000

outf = wave.open('res/raw/tone.wav', 'w')
outf.setnchannels(NCHANNELS)
outf.setsampwidth(NBYTES)
outf.setframerate(SAMPFREQ)
outf.setnframes(N)

class EnvelopeGenerator:

    def __init__(self, attack, decay):
        self.a, self.d = attack, decay
        # choose k such that the max envelope is 1.0
        self.k = 1.0
        b = 1./(1./attack + 1./decay)
        c = log(b / decay)
        emax = self(b * c * decay / (b - decay))
        self.k = 1./emax

    def __call__(self, t):
        return self.k * (1-exp(-t/self.a)) * exp(-t/self.d)

#def f(t, w=2*pi*FREQUENCY, eg=EnvelopeGenerator(ATTACK, DECAY)):
#    return int(AMPL * eg(t) * sin(w * t))

pi_2_0 = 2.0 * pi
pi_0_5 = 0.5 * pi
pi_1_5 = 1.5 * pi

def f(t, w=2*pi*FREQUENCY, eg=EnvelopeGenerator(ATTACK, DECAY)):
    if 1:
        # sine wave
        s = sin(w * t)
    elif 0:
        # square wave
        s = sin(w * t)
        s = (s > 0.0) and 1.0 or -1.0
    elif 0:
        # ramp wave
        phase = (w * t) % pi_2_0
        s = phase / pi - 1.0
    else:
        # triangle wave
        phase = (w * t) % pi_2_0
        if phase < pi_0_5:
            s = phase / pi_0_5
        elif phase < pi_1_5:
            s = 2.0 - phase/pi_0_5
        else:
            s = phase / pi_0_5 - 4.0
    return int(AMPL * eg(t) * s)

for i in range(N):
    t = i * DT
    x = f(t)
    if NBYTES == 1:
        str = chr((x + 128) & 255)
    else:
        xh = (x >> 8) & 255
        xl = x & 255
        str = chr(xl) + chr(xh)
    outf.writeframesraw(NCHANNELS * str)

outf.close()
