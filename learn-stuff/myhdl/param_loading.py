import unittest
import math
import sys
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals

def get_nibbles(n):
    return [Signal(intbv(0)[4:]) for i in range(n)]


def bitfields(source, *args):
    retval = []

    def make_f(source, offset, len, signal):
        @always_comb
        def f():
            signal.next = (source >> offset) & ((1 << len) - 1)
        return f

    for offset, len, signal in args:
        retval.append(make_f(source, offset, len, signal))
    return retval

class DaisyChain:

    '''
    This works a bit like JTAG except that it's a four-bit-wide bus. There are two
    clocks, pclk and outclk. Pclk advances the nibbles along the daisy chain, and
    outclk clocks them all simultaneously to the output latches.
    '''

    @classmethod
    def set_up_clocks(cls, pclk, outclk):
        cls.pclk = pclk
        cls.outclk = outclk

    def __init__(self, pdata_in):
        self.pdata_in = pdata_in

    def _param_nibble(self, out, pdata_out):
        pdata_in = self.pdata_in
        self.pdata_in = pdata_out
        q = Signal(intbv(0)[4:])
        r = Signal(intbv(0)[4:])

        @always(self.pclk.posedge)
        def f():
            q.next = pdata_in

        @always(self.outclk.posedge)
        def g():
            r.next = q

        @always_comb
        def h():
            pdata_out.next = q
            out.next = r

        return (f, g, h)

    def param_8(self, out, pdata_out):
        a = Signal(intbv(0)[4:])
        b = Signal(intbv(0)[4:])
        q = Signal(intbv(0)[4:])

        nibble0 = self._param_nibble(a, q)
        nibble1 = self._param_nibble(b, pdata_out)

        @always_comb
        def f():
            out.next = (a << 4) | b

        return (f, nibble0, nibble1)

    def param_16(self, out, pdata_out, limit_bits=16):
        a = Signal(intbv(0)[8:])
        b = Signal(intbv(0)[8:])
        q = Signal(intbv(0)[4:])
        pa = self.param_8(a, q)
        pb = self.param_8(b, pdata_out)

        @always_comb
        def f():
            out.next = ((a << 8) | b) & ((1 << limit_bits) - 1)

        return (pa, pb, f)

    def param_24(self, out, pdata_out):
        a = Signal(intbv(0)[16:])
        b = Signal(intbv(0)[8:])
        q = Signal(intbv(0)[4:])
        pa = self.param_16(a, q)
        pb = self.param_8(b, pdata_out)

        @always_comb
        def f():
            out.next = (a << 8) | b

        return (pa, pb, f)

    def param_32(self, out, pdata_out):
        a = Signal(intbv(0)[16:])
        b = Signal(intbv(0)[16:])
        q = Signal(intbv(0)[4:])
        pa = self.param_16(a, q)
        pb = self.param_16(b, pdata_out)

        @always_comb
        def f():
            out.next = (a << 16) | b

        return (pa, pb, f)


def param_clock_driver(byte_sequence, pclk, period):

    '''
    Helpful for writing tests.
    '''

    NUM_NIBBLES = len(byte_sequence) * 2

    @instance
    def clock_driver():
        for i in range(NUM_NIBBLES):
            pclk.next = 0
            yield delay(period / 2)
            pclk.next = 1
            yield delay(period / 2)
        pclk.next = 0    # 80

    return clock_driver

def daisy_chain_driver(byte_sequence, pdata, outclk, period):

    '''
    Helpful for writing tests.
    '''

    @instance
    def pdata_driver():
        for x in byte_sequence:
            pdata.next = x & 0xF
            yield delay(period)
            pdata.next = (x >> 4) & 0xF
            yield delay(period)
        yield delay(period / 2)
        outclk.next = 1
        yield delay(period / 2)
        outclk.next = 0
        yield delay(2 * period)
        pdata.next = 0

    return pdata_driver


################

def test_bench():

    # these are clock and input signals for the whole daisychain
    pclk = Signal(False)
    pdata = Signal(intbv(0)[4:])
    outclk = Signal(False)
    DaisyChain.set_up_clocks(pclk, outclk)

    # these are the parameters driven by the daisychain
    result1 = Signal(intbv(0)[8:])
    result2 = Signal(intbv(0)[24:])
    result3 = Signal(intbv(0)[32:])

    param_bytes = (
        0x26,    # result3, little-endian
        0x59,
        0x41,
        0x31,
        0x21,    # result2, little-endian
        0x43,
        0x65,
        0x87)    # result 1

    # daisychain links
    a, b, c = get_nibbles(3)
    chain = DaisyChain(pdata)

    r = (
        chain.param_8(result1, a),
        chain.param_24(result2, b),
        chain.param_32(result3, c),
        param_clock_driver(param_bytes, pclk, 10),
        daisy_chain_driver(param_bytes, pdata, outclk, 10),
    )
    return r

if __name__ == '__main__':
    Simulation(traceSignals(test_bench)).run()
