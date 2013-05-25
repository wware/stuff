import sys
import unittest
import math
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    PHASEWIDTH,
    N,
    MASK,
    WHOLE,
    HALF,
    AUDIO_RATE,
    unsigned_bus
)

FRACTION_BITS = 20
LCOUNT_BITS = 6


def lcounter(clk, lzero):
    lbits = unsigned_bus(LCOUNT_BITS)

    @always_comb
    def _output():
        lzero.next = (lbits == 0)

    @always(clk.posedge)
    def count():
        lbits.next = (lbits + 1) & ((1 << LCOUNT_BITS) - 1)

    return (_output, count)


# states
ATTACK, DECAY, RELEASE = range(3)

# Clock this at the 40 kHz audio rate
def state_machine(clk, keydown, threshold, state):
    _state = unsigned_bus(2)

    @always_comb
    def drive_outputs():
        state.next = _state

    @always(clk.posedge)
    def transitions():
        if not keydown:
            _state.next = RELEASE
        elif _state > RELEASE:
            _state.next = RELEASE
        elif _state == RELEASE and keydown:
            _state.next = ATTACK
        elif _state == ATTACK and threshold:
            _state.next = DECAY
        else:
            _state.next = _state

    return (drive_outputs, transitions)


def make_sm_ios():
    clk = Signal(False)
    keydown = Signal(False)
    threshold = Signal(False)
    state = unsigned_bus(2)
    return (clk, keydown, threshold, state)


def exponential_target(state, sustain, target):

    @always_comb
    def choose_target():
        if state == ATTACK:
            target.next = 2 << (N + FRACTION_BITS)
        elif state == DECAY:
            # Line up the MSbit of sustain with the MSbit for target
            target.next = sustain << (N + FRACTION_BITS - 4)
        else:   # RELEASE
            target.next = 0

    return choose_target


def exponential_generator(clk, reset, target, latch_dq, slope, qi):
    """
    ATTACK:  target = 2 << (N + FRACTION_BITS)
    DECAY:   target = S << (N + FRACTION_BITS - 4)
                      ^ the idea here is to left-shift S so its MSB lines up
    RELEASE: target = 0
    """
    q = unsigned_bus(N + FRACTION_BITS + 2)
    dq = unsigned_bus(N + FRACTION_BITS + 2)
    sign_bit = Signal(False)

    @always(clk.posedge)
    def compute_dq():
        if latch_dq and not reset:
            x = target - q
            if target >= q:
                sign_bit.next = False
            else:
                sign_bit.next = True
                x = -x

            if slope & 1:
                # divide by sqrt(2), approximately
                x = (x >> 1) + (x >> 3) + (x >> 4)

            dq.next = x >> (LCOUNT_BITS + 3 + (slope >> 1))

    @always(clk.posedge)
    def update_q():
        if reset:
            q.next = 0
        elif sign_bit:
            q.next = q - dq
        else:
            q.next = q + dq

    @always_comb
    def drive_qi_qf():
        if (q >> FRACTION_BITS) > MASK:
            qi.next = MASK
        else:
            qi.next = q >> FRACTION_BITS

    return (compute_dq, update_q, drive_qi_qf)

def make_expgen_ios():
    clk = Signal(False)
    reset = Signal(False)
    target = unsigned_bus(N + FRACTION_BITS + 2)
    latch_dq = Signal(False)
    slope = unsigned_bus(4)
    qi = unsigned_bus(N)
    return (clk, reset, target, latch_dq, slope, qi)


def adsr(clk, keydown, attack, sustain, decay, release, _out):
    reset = Signal(False)
    keydown1 = Signal(False)
    keydown2 = Signal(False)
    lzero = Signal(False)
    threshold = Signal(False)
    state = unsigned_bus(2)
    target = unsigned_bus(N + FRACTION_BITS + 2)
    latch_dq = Signal(False)
    slope = unsigned_bus(4)
    qi = unsigned_bus(N)

    @always_comb
    def combinatorial():
        latch_dq.next = (keydown1 and not keydown2) or lzero
        threshold.next = (qi == MASK)
        _out.next = qi
        if state == ATTACK:
            slope.next = attack
        elif state == DECAY:
            slope.next = decay
        else:
            slope.next = release

    @always(clk.posedge)
    def synchronous():
        keydown1.next = keydown
        keydown2.next = keydown1
        reset.next = 0

    sm = state_machine(clk, keydown, threshold, state)
    eg = exponential_generator(clk, reset, target, latch_dq, slope, qi)
    et = exponential_target(state, sustain, target)
    lc = lcounter(clk, lzero)

    return (sm, eg, et, lc, synchronous, combinatorial)

def make_adsr_ios():
    clk = Signal(False)
    keydown = Signal(False)
    attack = unsigned_bus(4)
    decay = unsigned_bus(4)
    sustain = unsigned_bus(4)
    release = unsigned_bus(4)
    _out = unsigned_bus(N)
    return (clk, keydown, attack, sustain, decay, release, _out)


class TestEnvelopeGenerator(unittest.TestCase):

    def test_state_machine(self):

        def bench():
            clk, keydown, threshold, state = make_sm_ios()
            sm = state_machine(clk, keydown, threshold, state)

            @instance
            def drive_stuff():
                keydown.next = False
                threshold.next = False
                clk.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                self.assertEqual(RELEASE, state)

                yield delay(1)
                keydown.next = True
                yield delay(1)
                clk.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                self.assertEqual(ATTACK, state)

                threshold.next = True
                yield delay(1)

                clk.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)

                self.assertEqual(DECAY, state)
                keydown.next = False
                yield delay(1)

                clk.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                self.assertEqual(RELEASE, state)

            return (sm, drive_stuff)

        tb = bench()
        sim = Simulation(tb)
        sim.run()

    def test_exponential_generator(self):

        def bench():
            clk, reset, target, latch_dq, slope, qi = make_expgen_ios()
            eg = exponential_generator(clk, reset, target, latch_dq, slope, qi)

            @instance
            def drive_stuff():
                clk.next = 0
                reset.next = 1
                target.next = 0
                latch_dq.next = 0
                slope.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                clk.next = 0
                yield delay(1)
                self.assertEqual(0, qi)
                reset.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                clk.next = 0
                yield delay(1)
                target.next = 1 << N
                latch_dq.next = 1
                yield delay(1)
                clk.next = 1     # this rising edge sets dq
                yield delay(1)
                clk.next = 0
                yield delay(1)
                # target should now be a don't-care
                latch_dq.next = 0
                yield delay(1)
                clk.next = 1     # this rising edge increments q
                yield delay(1)
                clk.next = 0
                yield delay(1)
                self.assertEqual(1 << (N - LCOUNT_BITS - 3), qi)
                clk.next = 1     # this rising edge increments q
                yield delay(1)
                clk.next = 0
                yield delay(1)
                self.assertEqual(2 << (N - LCOUNT_BITS - 3), qi)
                target.next = 0  # let's try moving down now
                latch_dq.next = 1
                yield delay(1)
                clk.next = 1     # this rising edge sets dq, q keeps going
                yield delay(1)
                clk.next = 0
                yield delay(1)
                clk.next = 1     # this rising edge moves q down a little
                yield delay(1)
                clk.next = 0
                yield delay(1)
                # q will continue moving up for one more tick, while
                q_est = (3 << (N - LCOUNT_BITS - 3)) << FRACTION_BITS
                dq_est = q_est >> (LCOUNT_BITS + 3)
                q_est -= dq_est
                self.assertEqual(q_est >> FRACTION_BITS, qi)

            return (eg, drive_stuff)

        tb = bench()
        sim = Simulation(tb)
        sim.run()

    def test_lcounter(self):

        def bench():
            clk = Signal(False)
            lzero = Signal(True)
            lc = lcounter(clk, lzero)

            @instance
            def drive_stuff():
                clk.next = 0
                for j in range(2):
                    for i in range((1 << LCOUNT_BITS) - 1):
                        yield delay(1)
                        clk.next = 1
                        yield delay(1)
                        clk.next = 0
                        self.assertEqual(lzero, 0)
                    yield delay(1)
                    clk.next = 1
                    yield delay(1)
                    clk.next = 0
                    self.assertEqual(lzero, 1)

            return (lc, drive_stuff)

        tb = bench()
        sim = Simulation(tb)
        sim.run()


def simulate():
    clk, keydown, attack, sustain, decay, release, _out = make_adsr_ios()
    _adsr = adsr(clk, keydown, attack, sustain, decay, release, _out)

    @instance
    def bench():
        clk.next = 0
        keydown.next = 0
        attack.next = 8
        sustain.next = 8
        decay.next = 2
        release.next = 3
        for i in range(10000):
            yield delay(1)
            clk.next = 1
            yield delay(1)
            clk.next = 0
        keydown.next = 1
        for i in range(10000):
            yield delay(1)
            clk.next = 1
            yield delay(1)
            clk.next = 0
        keydown.next = 0
        for i in range(10000):
            yield delay(1)
            clk.next = 1
            yield delay(1)
            clk.next = 0

    return (bench, _adsr)


if __name__ == '__main__':
    if 'hdl' in sys.argv[1:]:
        clk, keydown, attack, sustain, decay, release, _out = make_adsr_ios()
        toVerilog(adsr, clk, keydown, attack, sustain, decay, release, _out)
    elif 'sim' in sys.argv[1:]:
        Simulation(traceSignals(simulate)).run()
    else:
        suite = unittest.TestLoader().loadTestsFromTestCase(TestEnvelopeGenerator)
        unittest.TextTestRunner(verbosity=2).run(suite)
