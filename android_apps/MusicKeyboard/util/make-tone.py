import wave
from math import pi, sin, exp, log

A_PITCH = 440.0
C_PITCH = A_PITCH * 2**.25   # up a minor third
C_PITCH /= 2.0   # down an octave

T = 2.5
ATTACK = 0.05
DECAY = 0.5
FREQUENCY = C_PITCH

if 1:
    SAMPFREQ = 16000
    NCHANNELS = 1
    NBYTES = 2
else:
    SAMPFREQ = 44100
    NCHANNELS = 1
    NBYTES = 2

DT = 1.0 / SAMPFREQ
N = int(T / DT)

AMPL = (NBYTES == 1) and 100 or 25000

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

for i in range(3 * 12 + 1):

    freqmult = 2.0 ** (i / 12.0)

    outf = wave.open('res/raw/tone%02d.wav' % i, 'w')
    outf.setnchannels(NCHANNELS)
    outf.setsampwidth(NBYTES)
    outf.setframerate(SAMPFREQ)
    outf.setnframes(N)

    def f(t, w=2*pi*FREQUENCY*freqmult,
          eg=EnvelopeGenerator(ATTACK, DECAY)):
        return int(AMPL * eg(t) * sin(w * t))

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
