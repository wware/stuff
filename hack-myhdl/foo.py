import serial
import time

ser = serial.Serial('/dev/tty.usbmodem1d1111', 9600)

while True:
    for i in '0123456789abcdef':
        x = 32 * i
        ser.write(x)
        time.sleep(0.1)

ser.close()