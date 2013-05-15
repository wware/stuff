import sys

from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    WHOLE,
    HALF,
    MASK,
    N,
)


def vca(clk, input_signed, input_unsigned, output_signed):
    a = Signal(intbv(0)[18:])
    b = Signal(intbv(0)[18:])
    ab = Signal(intbv(0)[36:])

    @always(clk.posedge)
    def get_inputs():
        a.next = input_signed
        b.next = input_unsigned

    @always_comb
    def drive_output():
        output_signed.next = ((a * b) >> N) + HALF - (b >> 1)

    return (get_inputs, drive_output)


def to_verilog():
    clk = Signal(False)
    input_signed = Signal(intbv(0)[N:])
    input_unsigned = Signal(intbv(0)[N:])
    output_signed = Signal(intbv(0)[N:])
    toVerilog(vca, clk, input_signed, input_unsigned, output_signed)

if __name__ == '__main__':
    if 'hdl' in sys.argv[1:]:
        to_verilog()
    else:
        raise Exception()
