from myhdl import Signal, delay, always_comb, instance, intbv, bin, always, now


######### General stuff ################

# Papilio board clocked at 32 MHz, audio rate is 40 kHz
MHZ = 32 * 1000 * 1000
SECOND = AUDIO_RATE = 40 * 1000

DIVIDER = MHZ / AUDIO_RATE

N = 14
assert N < 18   # keep multiplier happy

WHOLE = 1 << N
MASK = WHOLE - 1
HALF = 1 << (N - 1)
QUARTER = 1 << (N - 2)

PHASEWIDTH = 24

LOADWIDTH = 4

(RAMP, TRIANGLE, SQWAVE, NOISE) = range(4)

def signed_bus(numbits):
    min = -(1 << (numbits - 1))
    max = 1 << (numbits - 1)
    return Signal(intbv(0, min=min, max=max))

def unsigned_bus(numbits):
    return Signal(intbv(0)[numbits:])

def signed_to_unsigned(nbits, _in, _out):
    __out = unsigned_bus(nbits)
    @always(_in)
    def drive__out():
        __out.next = _in + (1 << (nbits - 1))
    @always(__out)
    def drive_out():
        _out.next = __out
    return (drive__out, drive_out)

def unsigned_to_signed(nbits, _in, _out):
    __out = signed_bus(nbits)
    @always(_in)
    def drive__out():
        __out.next = _in - (1 << (nbits - 1))
    @always(__out)
    def drive_out():
        _out.next = __out
    return (drive__out, drive_out)


############## Simulation stuff ################

def compute_delta_phase(freq):
    ONE_HERTZ = 1. * (1 << PHASEWIDTH) / AUDIO_RATE
    return int(round(ONE_HERTZ * freq))
