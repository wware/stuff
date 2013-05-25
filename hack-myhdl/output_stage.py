import math
import unittest
import sys

from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    MHZ,
    N,
    HALF,
    unsigned_bus,
    signed_bus
)

DT = 1./MHZ
R = 1000
C = 0.01e-6
ALPHA = math.exp(-DT / (R * C))
A = int(round((1. - ALPHA) * (1 << 18)))
B = int(round(ALPHA * (1 << 18)))


# fastclk is the 32 MHz clock to the FPGA
# clk is the audio-rate clock at 40 kHz, which is high for one fastclk period
def interpolator(fastclk, clk, input_data, interp_out):

    # There are 800 fastclk periods during each clk period, so on each fastclk we want to
    # advance 1/800th from the previous sound sample to the current sample. 800 is pretty
    # close to (1<<22) divided by 5243, so we can compute a delta by multiplying the
    # difference between samples by 5243, which is easy because it's constant and not too
    # many non-zero bits. By accumulating this difference and right-shifting 22 bits, we
    # arrive almost exactly where we want to end up after 800 accumulations.

    FRACTION_BITS = 22
    delay_1 = signed_bus(N)
    interp_step = signed_bus(N + FRACTION_BITS)
    interp_data = signed_bus(N + FRACTION_BITS)

    @always(fastclk.posedge)
    def do_stuff():
        if clk:
            interp_data.next = delay_1 << FRACTION_BITS
            delay_1.next = input_data
            x = input_data - delay_1
            # multiply by 5243 in binary
            interp_step.next = (x << 12) + (x << 10) + (x << 6) + (x << 5) + \
                (x << 4) + (x << 3) + (x << 1) + x
        else:
            interp_data.next = interp_data + interp_step

    @always_comb
    def rightshift_for_output():
        interp_out.next = interp_data >> FRACTION_BITS

    return (do_stuff, rightshift_for_output)


def delta_sigma_dac(fastclk, clk, input_data, dac_bit):

    interp_result = signed_bus(N)
    vc_estimate = unsigned_bus(18)
    sum_of_products = unsigned_bus(36)

    # Interpolation is a huge help with anti-aliasing.
    _interp = interpolator(fastclk, clk, input_data, interp_result)

    @always(fastclk.posedge)
    def do_stuff():
        dac_bit.next = (interp_result + HALF) > (sum_of_products >> (36 - N))
        vc_estimate.next = sum_of_products >> 18

    @always_comb
    def multiply():
        if dac_bit:
            sum_of_products.next = (A << 18) + (B * vc_estimate)
        else:
            sum_of_products.next = B * vc_estimate

    return (_interp, do_stuff, multiply)

def make_dsig_ios():
    fastclk = Signal(False)
    clk = Signal(False)
    input_data = signed_bus(N)
    dac_bit = Signal(False)
    return (fastclk, clk, input_data, dac_bit)


class TestOutputStage(unittest.TestCase):
    pass   # TODO write some tests


def simulate():
    fastclk, clk, input_data, dac_bit = make_dsig_ios()
    dsig = delta_sigma_dac(fastclk, clk, input_data, dac_bit)

    @instance
    def bench():
        fastclk.next = 0
        clk.next = 0
        input_data.next = 0
        yield delay(1)
        for j in range(100):
            for i in range(800):
                fastclk.next = 1
                yield delay(1)
                fastclk.next = 0
                yield delay(1)
            input_data.next = input_data + 50
            fastclk.next = 1
            clk.next = 1
            yield delay(1)
            fastclk.next = 0
            yield delay(1)
            clk.next = 0

    return (bench, dsig)


if __name__ == '__main__':
    if 'hdl' in sys.argv[1:]:
        fastclk, clk, input_data, dac_bit = make_dsig_ios()
        toVerilog(dsig, fastclk, clk, input_data, dac_bit)
    elif 'sim' in sys.argv[1:]:
        Simulation(traceSignals(simulate)).run()
    else:
        suite = unittest.TestLoader().loadTestsFromTestCase(TestOutputStage)
        unittest.TextTestRunner(verbosity=2).run(suite)
