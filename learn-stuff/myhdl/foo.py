import serial
import time

ser = serial.Serial('/dev/tty.usbmodem1d1111', 9600)

MHZ = 32 * 1000 * 1000
AUDIO_RATE = 40 * 1000
FREQ = 440  # A above middle C
ONE_HERTZ = 1. * (1 << 24) / AUDIO_RATE
DELTA_PHASE = int(ONE_HERTZ * FREQ)

chorusing = 0
select = 1    # two bits
keydown = 0
controls = (chorusing << 3) | (select << 1) | keydown

envelope = 0x378f
threshold = 1 << 13

bytes = [
    envelope & 0xFF,
    (envelope >> 8) & 0xFF,
    controls,
    threshold & 0xFF,
    (threshold >> 8) & 0xFF,
    DELTA_PHASE & 0xFF,
    (DELTA_PHASE >> 8) & 0xFF,
    (DELTA_PHASE >> 16) & 0xFF,
]

def keydown():
    bytes[2] |= 1

def keyup():
    bytes[2] &= ~1

while True:
    [ser.write(chr(b)) for b in bytes]
    ser.write(24 * '\0')
    time.sleep(1)
    keydown()
    [ser.write(chr(b)) for b in bytes]
    ser.write(24 * '\0')
    time.sleep(1)
    keyup()

ser.close()
