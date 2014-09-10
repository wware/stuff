import math
import unittest
import sys

from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    MHZ,
    N,
    WHOLE,
    MASK,
    HALF,
    unsigned_bus,
    signed_bus,
    signed_to_unsigned,
    unsigned_to_signed
)

"""
Make A a 32-bit number. Make B 16 bits, and vc_estimate 16 bits. Then the sum of
A+B*vc_estimate will be 32 bits, clipped to (1<<32)-1 and take
the upper 16 bits for the next value of vc_estimate. Get dac_bit by comparing
vc_estimate with (target << 2).
"""

DT = 1./MHZ
R = 1000
C = 0.01e-6
ALPHA = math.exp(-DT / (R * C))
A = int(round((1. - ALPHA) * (1 << 32)))
B = int(round(ALPHA * (1 << 16)))


# fastclk is the 32 MHz clock to the FPGA
# clk is the audio-rate clock at 40 kHz, which is high for one fastclk period
def interpolator(fastclk, clk, reset, input_data, interp_out):

    # There are 800 fastclk periods during each clk period, so on each fastclk we want to
    # advance 1/800th from the previous sound sample to the current sample. 800 is pretty
    # close to (1<<22) divided by 5243, so we can compute a delta by multiplying the
    # difference between samples by 5243, which is easy because it's constant and not too
    # many non-zero bits. By accumulating this difference and right-shifting 22 bits, we
    # arrive almost exactly where we want to end up after 800 accumulations.

    FRACTION_BITS = 16
    delay_1 = signed_bus(N)
    x = signed_bus(N)
    interp_step = signed_bus(N + FRACTION_BITS)
    interp_data = signed_bus(N + FRACTION_BITS)

    @always(fastclk.posedge, reset.posedge)
    def do_stuff():
        if reset:
            delay_1.next = 0
            interp_data.next = 0
            interp_step.next = 0
        else:
            if clk:
                interp_data.next = delay_1 << FRACTION_BITS
                delay_1.next = input_data
                # multiply by 5243 in binary
                interp_step.next = (x << 12) + (x << 10) + (x << 6) + (x << 5) + \
                    (x << 4) + (x << 3) + (x << 1) + x
            elif (interp_data + interp_step) < interp_data.min:
                interp_data.next = interp_data.min
            elif (interp_data + interp_step) >= interp_data.max:
                interp_data.next = interp_data.max - 1
            else:
                interp_data.next = interp_data + interp_step

    @always_comb
    def rightshift_for_output():
        x.next = input_data - delay_1
        interp_out.next = interp_data >> FRACTION_BITS

    return (do_stuff, rightshift_for_output)


def delta_sigma_dac(fastclk, clk, reset, input_data, dac_bit):

    interp_result = signed_bus(N)
    interp_result_unsigned = unsigned_bus(N)
    # the input of the Xilinx multiplier is an 18-bit factor
    vc_estimate = unsigned_bus(16)
    # the output of the Xilinx multiplier is a 36-bit product
    sum_of_products = unsigned_bus(32)
    dac_bit_internal = Signal(False)

    @always_comb
    def drive_dac_bit():
        dac_bit.next = dac_bit_internal

    @always(fastclk.posedge, reset.posedge)
    def do_stuff():
        # sum_of_products is the next value for vc_estimate, with lots of fraction
        # bits. vc_estimate already has fraction bits beyond N. All these fraction
        # bits are helpful in keeping out audible artifacts.
        if reset:
            dac_bit_internal.next = 0
            vc_estimate.next = 1 << 15
        else:
            dac_bit_internal.next = interp_result_unsigned > (sum_of_products >> (32 - N))
            vc_estimate.next = sum_of_products >> 16

    @always_comb
    def multiply():
        if dac_bit_internal:
            if A + (B * vc_estimate) >= (1 << 32):
                sum_of_products.next = (1 << 32) - 1
            else:
                sum_of_products.next = A + (B * vc_estimate)
        else:
                sum_of_products.next = B * vc_estimate

    things = [
        # Interpolation is a huge help with anti-aliasing.
        interpolator(fastclk, clk, reset, input_data, interp_result),
        drive_dac_bit,
        do_stuff,
        multiply,
        signed_to_unsigned(N, interp_result, interp_result_unsigned)
    ]
    return things

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
