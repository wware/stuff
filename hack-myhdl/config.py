from myhdl import Signal, delay, always_comb, instance, intbv, bin, always, now


######### General stuff ################

# Papilio board
MHZ = 32 * 1000 * 1000
AUDIO_RATE = 40 * 1000
PARAM_RATE = 50

DIVIDER = MHZ / AUDIO_RATE
PDIVIDER = MHZ / PARAM_RATE

N = 14
assert N < 18   # keep multiplier happy

WHOLE = 1 << N
MASK = WHOLE - 1
HALF = 1 << (N - 1)
QUARTER = 1 << (N - 2)

PHASEWIDTH = 24

LOADWIDTH = 4


############## Simulation stuff ################

# FREQ = 440  # A above middle C
FREQ = 2000
ONE_HERTZ = 1. * (1 << 24) / AUDIO_RATE
DELTA_PHASE = int(ONE_HERTZ * FREQ)

# SIMTIME = 0.200
# SIMTIME = 0.01
SIMTIME = 0.001
SIMLEN = int(SIMTIME * MHZ)


def simulated_clock(clk, threshold=None):

    if threshold is None:
        @instance
        def clock_driver():
            a = int((1.e9 / MHZ) + 0.5)   # round to nearest integer
            b = int(0.5 * a)
            for i in range(SIMLEN):
                clk.next = 0
                yield delay(b)
                clk.next = 1
                yield delay(a - b)
    else:
        @instance
        def clock_driver():
            a = int((1.e9 / MHZ) + 0.5)   # round to nearest integer
            b = int(0.5 * a)
            for i in range(SIMLEN):
                threshold.next = int(1. * i * (WHOLE - 1) / SIMLEN)
                clk.next = 0
                yield delay(b)
                clk.next = 1
                yield delay(a - b)

    return (clock_driver,)
