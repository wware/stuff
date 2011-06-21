#!/usr/bin/python

import serial

"""This is some UBW hacking code to tinker with a 240x160 Kent display
from Sparkfun. For these purposes I only need output pins, so I'm just
opening the serial port for output only."""

PA0 = 0x01
PA1 = 0x02
PA2 = 0x04
PA3 = 0x08

# SPI pin assignments
CS_, SCLK, SO = PA0, PA1, PA2

class UBW:
    def __init__(self):
        self.out = serial.Serial("/dev/ttyACM0", 9600)
        self.out.write("C,0,0,0\r\n")
        self.mostRecent = 0
        self.set(CS_)   # initially high
    def set(self, x):
        self.mostRecent |= x
        print ("O,%d,0,0\r\n" % self.mostRecent)
        self.out.write("O,%d,0,0\r\n" % self.mostRecent)
    def clear(self, x):
        self.mostRecent &= ~x
        self.out.write("O,%d,0,0\r\n" % self.mostRecent)
    def sendByte(self, byt):
        for i in range(8):
            self.set(SCLK)
            if (byt & 0x80):
                print 1
                self.set(SO)
            else:
                print 0
                self.clear(SO)
            self.clear(SCLK)
            byt <<= 1
    def command(self, byteList):
        self.clear(CS_)
        for b in byteList:
            self.sendByte(b)
        self.set(CS_)
    def write(self, addr, byteList):
        self.command([0, addr >> 8, addr] + byteList)
    def fill(self, startAddr, endAddr, value):
        self.command([1, startAddr >> 8, startAddr, endAddr >> 8, endAddr,
                      value])
    def clearBright(self):
        self.command([0x10])
    def clearDark(self):
        self.command([0x12])
    def display(self, addr):
        self.command([0x18, addr >> 8, addr])
    #
    # This is not all the commands available, but it's what I expect
    # to need immediately.
    #

# ubw = UBW()
# ubw.clearDark()

out = serial.Serial("/dev/ttyACM0", 9600)
out.write("C,0,0,0\r\n")
out.write("O,255,0,0\r\n")
out.close()
