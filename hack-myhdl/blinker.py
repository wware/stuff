import unittest
import sys
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now


def bitsWide(n, orig=0):
    # it won't synthesize if you don't spell all this out
    return Signal(intbv(orig)[n:])


# On the NEXYS 2 board, the available clock is 50 MHz. If I
# divide it by 3125000, that's 16 Hertz, and when I use that
# to sweep back and forth on the eight LEDs, that will take
# one second for each round trip. It takes 22 bits to count
# up to 3125000. So each tick represents a nanosecond, and a
# half-cycle of the clock is represented by "delay(10)".

DIVIDER = 3125000


def blinker(clk, button, leds, numLeds):

    dividerCounter = bitsWide(22)
    counter = bitsWide(numLeds, 0)
    advance = bitsWide(1)
    direction = bitsWide(1, 1)

    @always(clk.posedge)
    def div():
        if dividerCounter >= DIVIDER:
            dividerCounter.next = 0
            advance.next = 1
        else:
            dividerCounter.next = dividerCounter + 1
            advance.next = 0

    @always(clk.posedge)
    def logic():
        if not button:
            counter.next = 0
            direction.next = 0
        elif advance:
            if direction:
                if counter == numLeds - 1:
                    direction.next = 0
                else:
                    counter.next = counter + 1
            else:
                if counter == 0:
                    direction.next = 1
                else:
                    counter.next = counter - 1
            leds.next = 1 << counter

    return (logic, div)


class TestGrayCodeProperties(unittest.TestCase):

    def testSingleBitChange(self):
        """Check sequence for width of 4"""
        NUMLEDS = 4
        clk = Signal(0)
        button = Signal(1)
        leds = Signal(intbv(0))

        def clkdriver(clk):
            for i in range(30 * DIVIDER):
                yield delay(10)
                clk.next = not clk

        def checkInputs(leds):
            N = 20 * DIVIDER
            yield delay(N)
            self.assertEquals(leds, 0)
            yield delay(N)
            self.assertEquals(leds, 1)
            yield delay(N)
            self.assertEquals(leds, 2)
            yield delay(N)
            self.assertEquals(leds, 4)
            yield delay(N)
            self.assertEquals(leds, 8)
            yield delay(N)
            self.assertEquals(leds, 8)
            yield delay(N)
            self.assertEquals(leds, 4)
            yield delay(N)
            self.assertEquals(leds, 2)
            yield delay(N)
            self.assertEquals(leds, 1)
            yield delay(N)
            self.assertEquals(leds, 1)
            yield delay(N)
            self.assertEquals(leds, 2)
            yield delay(N)
            self.assertEquals(leds, 4)
            yield delay(N)
            self.assertEquals(leds, 8)

        dut = blinker(clk, button, leds, NUMLEDS)
        driver = clkdriver(clk)
        check = checkInputs(leds)
        sim = Simulation(driver, dut, check)
        sim.run(quiet=0)


if __name__ == "__main__":
    if 'hdl' in sys.argv[1:]:
        NUMLEDS = 8
        clk = Signal(intbv(0)[1:])
        button = Signal(intbv(1)[1:])
        leds = Signal(intbv(0)[NUMLEDS:])
        toVerilog(blinker, clk, button, leds, NUMLEDS)
        toVHDL(blinker, clk, button, leds, NUMLEDS)
    else:
        DIVIDER = 100   # let's not take forever to simulate
        suite = unittest.makeSuite(TestGrayCodeProperties)
        unittest.TextTestRunner(verbosity=2).run(suite)
