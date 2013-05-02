from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    HALF,
    MASK,
    N,
)


def vca(input_signed, input_unsigned, output_signed):
    ab = Signal(intbv(0)[36:])

    @always_comb
    def multiply():
        ab.next = (HALF << N) + (input_signed - HALF) * input_unsigned

    @always_comb
    def drive_output():
        output_signed.next = (ab >> N) & MASK

    return (multiply, drive_output)
