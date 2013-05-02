import math
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    PHASEWIDTH,
    N,
    MASK,
    WHOLE,
    HALF,
    AUDIO_RATE
)

FRACTION_BITS = 20
LCOUNT_BITS = 6

def adsr(audio_tick, A, D, S, R, keydown, _output):

    state = Signal(intbv(0)[2:])
    sign_bit = Signal(False)

    q = Signal(intbv(0)[(N+FRACTION_BITS+2):])
    dq = Signal(intbv(0)[(N+FRACTION_BITS+2):])
    lcount = Signal(intbv(1)[LCOUNT_BITS:])

    @always(audio_tick.posedge)
    def synchronous_stuff():
        if sign_bit:
            if q - dq < 0:
                q.next = 0
            else:
                q.next = q - dq
        else:
            q.next = q + dq

        if state == 0:
            if keydown:
                state.next = 1
                sign_bit.next = False   # diff always positive
                x = (2 << (N + FRACTION_BITS)) - q
                if A & 1:
                    # approximate division by sqrt(2)
                    dq.next = (x >> 1) + (x >> 3) + (x >> 4)
                else:
                    dq.next = x
                dq.next = dq.next >> (LCOUNT_BITS + 3 + (A >> 1))
                lcount.next = (1 << LCOUNT_BITS) - 1
            elif lcount == 0:
                if R & 1:
                    dq.next = (q >> 1) + (q >> 3) + (q >> 4)
                else:
                    dq.next = q
                dq.next = dq.next >> (LCOUNT_BITS + 3 + (R >> 1))
                lcount.next = (1 << LCOUNT_BITS) - 1
            else:
                lcount.next = lcount - 1
        elif state == 1:
            if not keydown:
                state.next = 0
                sign_bit.next = True    # diff always negative
                if R & 1:
                    dq.next = (q >> 1) + (q >> 3) + (q >> 4)
                else:
                    dq.next = q
                dq.next = dq.next >> (LCOUNT_BITS + 3 + (R >> 1))
                lcount.next = (1 << LCOUNT_BITS) - 1
            elif q >= (WHOLE << FRACTION_BITS):
                state.next = 2
                sign_bit.next = True    # diff always negative
                # shift S so that its high bit aligns right
                x = q - (S << (N + FRACTION_BITS - 4))
                if D & 1:
                    dq.next = (x >> 1) + (x >> 3) + (x >> 4)
                else:
                    dq.next = x
                dq.next = dq.next >> (LCOUNT_BITS + 3 + (D >> 1))
                lcount.next = (1 << LCOUNT_BITS) - 1
            elif lcount == 0:
                x = (2 << (N + FRACTION_BITS)) - q
                if A & 1:
                    dq.next = (x >> 1) + (x >> 3) + (x >> 4)
                else:
                    dq.next = x
                dq.next = dq.next >> (LCOUNT_BITS + 3 + (A >> 1))
                lcount.next = (1 << LCOUNT_BITS) - 1
            else:
                lcount.next = lcount - 1
        elif state == 2:
            if not keydown:
                state.next = 0
                sign_bit.next = True    # diff always negative
                if R & 1:
                    dq.next = (q >> 1) + (q >> 3) + (q >> 4)
                else:
                    dq.next = q
                dq.next = dq.next >> (LCOUNT_BITS + 3 + (R >> 1))
                lcount.next = (1 << LCOUNT_BITS) - 1
            elif lcount == 0:
                sign_bit.next = True
                x = q - (S << (N + FRACTION_BITS - 4))
                if D & 1:
                    dq.next = (x >> 1) + (x >> 3) + (x >> 4)
                else:
                    dq.next = x
                dq.next = dq.next >> (LCOUNT_BITS + 3 + (D >> 1))
                lcount.next = (1 << LCOUNT_BITS) - 1
            else:
                lcount.next = lcount - 1
        else:
            # recover the state machine from a bad state
            state.next = 0
            sign_bit.next = False
            dq.next = 0
            lcount.next = (1 << LCOUNT_BITS) - 1

    @always_comb
    def combinatorial():
        if (q >> FRACTION_BITS) >= WHOLE:
            _output.next = WHOLE - 1
        else:
            _output.next = q >> FRACTION_BITS

    return (synchronous_stuff, combinatorial)


################

def test_bench():

    audio_tick = Signal(False)
    keydown = Signal(False)
    _output = Signal(intbv(0)[N:])

    A = Signal(intbv(0)[4:])
    D = Signal(intbv(0)[4:])
    S = Signal(intbv(0)[4:])
    R = Signal(intbv(0)[4:])

    out = Signal(intbv(0)[N:])

    _adsr = adsr(audio_tick, A, D, S, R, keydown, _output)

    @instance
    def clock_driver():
        for i in range(250000):
            A.next = 7
            D.next = 4
            S.next = 8
            R.next = 14
            audio_tick.next = 0
            yield delay(25000 - 10)
            audio_tick.next = 1
            yield delay(10)
        audio_tick.next = 0

    @instance
    def keydown_driver():
        yield delay(100000000)
        keydown.next = 1
        yield delay(2000000000)
        keydown.next = 0

    return (_adsr, clock_driver, keydown_driver)

if __name__ == '__main__':
    Simulation(traceSignals(test_bench)).run()
