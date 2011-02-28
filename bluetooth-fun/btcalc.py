"""
This is a quick little test of the BTM-182 Bluetooth module, available
from Sparkfun (www.sparkfun.com) as either a raw module or a convenient
breakout board. I've set the baud rate to 115.2 kbaud and connected it
to a USB serial port (appearing as /dev/ttyUSB0 on my Linux netbook) and
getting power from a USBMOD4 board from Hobby Engineering, whose only
purpose here is to provide 3.3 volts. The serial port uses a RS-232 level
shifter from Sparkfun.

The Python code below runs on the Linux netbook. It opens the serial port
and provides a teeny command interpreter to anybody connecting over the
Bluetooth serial connection offered by the BTM-182. Right now I'm using
a Macbook with CoolTerm for that, pairing with "Serial Adaptor" using
PIN "1234".

Photo of the boards involved at http://bit.ly/dGkqYi.
"""

import serial
import string

ser = serial.Serial("/dev/ttyUSB0", 115200, timeout=1)

ser.write("Bluetooth calculator\r\n")

ECHO = True
line = ""

try:
    while 1:
        ch = ser.read()
        if ECHO and ch:
            ser.write(ch)
        if ch != '\n':
            line += ch
        else:
            fields = line.upper().strip().split()
            line = ""
            if len(fields) < 1:
                pass
            elif fields[0] == "ADD":
                try:
                    result = reduce(lambda a, b: a + b,
                                    map(string.atof, fields[1:]))
                    ser.write("%f\r\n" % result)
                except:
                    ser.write("ERROR\r\n")
            elif fields[0] == "MULTIPLY":
                try:
                    result = reduce(lambda a, b: a * b,
                                    map(string.atof, fields[1:]))
                    ser.write("%f\r\n" % result)
                except:
                    ser.write("ERROR\r\n")
            elif fields[0] == "QUIT":
                ser.close()
                raise SystemExit
except KeyboardInterrupt:
    ser.close()
