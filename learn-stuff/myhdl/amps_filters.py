import sys
import unittest

from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    WHOLE,
    HALF,
    MASK,
    N,
    signed_bus,
    unsigned_bus,
)

########

def vca(clk, input_signed, input_unsigned, output_signed):
    a = signed_bus(18)
    b = unsigned_bus(18)
    ab = signed_bus(36)

    @always(clk.posedge)
    def get_inputs():
        a.next = input_signed
        b.next = input_unsigned

    @always_comb
    def multiply():
        ab.next = a * b

    @always_comb
    def drive_output():
        output_signed.next = (ab >> N)

    return (get_inputs, multiply, drive_output)

########

def make_ios():
    clk = Signal(False)
    input_signed = signed_bus(N)
    input_unsigned = unsigned_bus(N)
    output_signed = signed_bus(N)
    return (clk, input_signed, input_unsigned, output_signed)


class TestVoltageControlledAmplifier(unittest.TestCase):

    def test_vca(self):

        def bench():
            clk, input_signed, input_unsigned, output_signed = make_ios()

            v = vca(clk, input_signed, input_unsigned, output_signed)

            @instance
            def drive_stuff():
                input_signed.next = 0
                input_unsigned.next = 0
                clk.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                self.assertEqual(0, output_signed)

                input_signed.next = HALF - 1
                input_unsigned.next = MASK
                clk.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                self.assertEqual(HALF - 2, output_signed)

                input_signed.next = -HALF
                input_unsigned.next = MASK
                clk.next = 0
                yield delay(1)
                clk.next = 1
                yield delay(1)
                self.assertEqual(-HALF, output_signed)

            return (v, drive_stuff)

        tb = bench()
        sim = Simulation(tb)
        sim.run()


if __name__ == '__main__':
    if 'hdl' in sys.argv[1:]:
        clk, input_signed, input_unsigned, output_signed = make_ios()
        toVerilog(vca, clk, input_signed, input_unsigned, output_signed)
    else:
        suite = unittest.TestLoader().loadTestsFromTestCase(TestVoltageControlledAmplifier)
        unittest.TextTestRunner(verbosity=2).run(suite)
