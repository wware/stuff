import unittest
import sys
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL


def bin2gray(B, G, width):
    """ Gray encoder.

    B -- input intbv signal, binary encoded
    G -- output intbv signal, gray encoded
    width -- bit width
    """

    @always_comb
    def logic():
        for i in range(width):
            G.next[i] = B[i + 1] ^ B[i]

    return logic


WIDTH = 4


class TestGrayCodeProperties(unittest.TestCase):

    def testSingleBitChange(self):
        """Check that only one bit changes in successive codewords"""
        def test(B, G, width):
            B.next = intbv(0)
            yield delay(10)
            for i in range(1, 2 ** width):
                G_Z.next = G
                B.next = intbv(i)
                yield delay(10)
                diffcode = bin(int(G ^ G_Z))
                self.assertEqual(diffcode.count('1'), 1)

        for width in range(1, WIDTH):
            B = Signal(intbv(-1))
            G = Signal(intbv(0))
            G_Z = Signal(intbv(0))
            dut = bin2gray(B, G, width)
            check = test(B, G, width)
            sim = Simulation(dut, check)
            sim.run(quiet=1)

    def testUniqueCodeWords(self):
        """Check that all codewords occur exactly once"""
        def test(B, G, width):
            actual = []
            for i in range(2 ** width):
                B.next = intbv(i)
                yield delay(10)
                actual.append(int(G))
            actual.sort()
            expected = range(2 ** width)
            self.assertEqual(actual, expected)

        for width in range(1, WIDTH):
            B = Signal(intbv(-1))
            G = Signal(intbv(0))
            dut = bin2gray(B, G, width)
            check = test(B, G, width)
            sim = Simulation(dut, check)
            sim.run(quiet=1)


def testBench(width):

    B = Signal(intbv(0))
    G = Signal(intbv(0))

    dut = bin2gray(B, G, width)

    @instance
    def stimulus():
        for i in range(2 ** width):
            B.next = intbv(i)
            yield delay(10)
            print "B: " + bin(B, width) + "| G: " + bin(G, width)

    return dut, stimulus


if __name__ == "__main__":
    if 'tb' in sys.argv[1:]:
        sim = Simulation(testBench(WIDTH))
        sim.run(quiet=1)
    elif 'hdl' in sys.argv[1:]:
        B = Signal(intbv(0)[WIDTH:])
        G = Signal(intbv(0)[WIDTH:])
        toVerilog(bin2gray, B, G, WIDTH)
        toVHDL(bin2gray, B, G, WIDTH)
    else:
        suite = unittest.makeSuite(TestGrayCodeProperties)
        unittest.TextTestRunner(verbosity=2).run(suite)
