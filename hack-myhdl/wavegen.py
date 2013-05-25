import unittest
import sys
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    PHASEWIDTH,
    N,
    HALF,
    MASK,
    RAMP,
    TRIANGLE,
    SQWAVE,
    NOISE,
    unsigned_bus,
    signed_bus
)
from math import log

def log2(x):
    if x < 0:
        return (False, log(-1.0 * x) / log(2.0))
    else:
        return (True, log(1.0 * x) / log(2.0))

# Ramp wave, triangle wave, PW-modulated square wave, or noise
# Clock this at the 40 kHz audio rate
def waveform_generator(clk, select, threshold, delta_phase, _output):

    NOISEWIDTH16 = 16    # depends on polynomial
    NOISEWIDTH13 = 13
    HALFPHASE = 1 << (PHASEWIDTH - 1)
    QUARTERPHASE = 1 << (PHASEWIDTH - 2)
    THREEQUARTERPHASE = HALFPHASE + QUARTERPHASE
    TRIANGLESHIFT = (PHASEWIDTH - N) - 1
    RAMPSHIFT = PHASEWIDTH - N

    phase_counter = unsigned_bus(PHASEWIDTH)
    noise_register_16 = unsigned_bus(NOISEWIDTH16)
    noise_register_13 = unsigned_bus(NOISEWIDTH13)

    @always(clk.posedge)
    def waveforms():
        if noise_register_16 == 0:
            noise_register_16.next = 123
        elif (noise_register_16 ^ (noise_register_16 >> 2) ^
                (noise_register_16 >> 3) ^ (noise_register_16 >> 5)) & 1:
            noise_register_16.next = (1 << 15) + (noise_register_16 >> 1)
        else:
            noise_register_16.next = (noise_register_16 >> 1)

        if noise_register_13 == 0:
            noise_register_13.next = 1787
        elif (noise_register_13 ^ (noise_register_13 >> 1) ^
                (noise_register_13 >> 2) ^ (noise_register_13 >> 5)) & 1:
            noise_register_13.next = (1 << 12) + (noise_register_13 >> 1)
        else:
            noise_register_13.next = (noise_register_13 >> 1)

        if phase_counter + delta_phase >= (1 << PHASEWIDTH):
            phase_counter.next = phase_counter + delta_phase - (1 << PHASEWIDTH)
        else:
            phase_counter.next = phase_counter + delta_phase

        if select == RAMP:
            _output.next = (phase_counter - HALFPHASE) >> RAMPSHIFT
        elif select == TRIANGLE:
            if phase_counter < HALFPHASE:
                _output.next = (phase_counter - QUARTERPHASE) >> TRIANGLESHIFT
            else:
                _output.next = \
                    (THREEQUARTERPHASE - phase_counter) >> TRIANGLESHIFT
        elif select == SQWAVE:
            if phase_counter > (threshold << (PHASEWIDTH - N)):
                _output.next = MASK - HALF
            else:
                _output.next = -HALF
        else:   # NOISE
            _output.next = \
                ((noise_register_16 ^ noise_register_13) & MASK) - HALF

    return waveforms

def make_wavgen_ios():
    clk = Signal(False)
    select = unsigned_bus(2)
    threshold = unsigned_bus(N)
    delta_phase = unsigned_bus(PHASEWIDTH)
    _output = signed_bus(N)
    return (clk, select, threshold, delta_phase, _output)


class TestWaveformGenerator(unittest.TestCase):

    def test_ramp(self):
        pass

    def test_triangle(self):
        pass

    def test_square_wave(self):
        pass

    def test_noise(self):
        pass


def simulate():
    from config import DELTA_PHASE
    clk, select, threshold, delta_phase, _output = make_wavgen_ios()
    wavgen = waveform_generator(clk, select, threshold, delta_phase, _output)

    @instance
    def bench():
        clk.next = 0
        select.next = RAMP
        delta_phase.next = DELTA_PHASE
        threshold.next = HALF
        for i in range(1000):
            yield delay(1)
            clk.next = 1
            yield delay(1)
            clk.next = 0
        select.next = TRIANGLE
        for i in range(1000):
            yield delay(1)
            clk.next = 1
            yield delay(1)
            clk.next = 0
        select.next = SQWAVE
        for i in range(1000):
            yield delay(1)
            clk.next = 1
            yield delay(1)
            clk.next = 0
        select.next = NOISE
        for i in range(1000):
            yield delay(1)
            clk.next = 1
            yield delay(1)
            clk.next = 0

    return (bench, wavgen)


if __name__ == '__main__':
    if 'hdl' in sys.argv[1:]:
        clk, keydown, attack, sustain, decay, release, _out = make_adsr_ios()
        toVerilog(adsr, clk, keydown, attack, sustain, decay, release, _out)
    elif 'sim' in sys.argv[1:]:
        Simulation(traceSignals(simulate)).run()
    else:
        suite = unittest.TestLoader().loadTestsFromTestCase(TestEnvelopeGenerator)
        unittest.TextTestRunner(verbosity=2).run(suite)
