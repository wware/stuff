import serial
import string
import time

ser = serial.Serial('/dev/rfcomm0', 115200, timeout=1)

ser.write('TEST\n')
#print ser.read()

ser.write('CLEAR 2\n')
#print ser.read()

time.sleep(2)

ser.write('SET 2\n')
#print ser.read()

ser.close()
