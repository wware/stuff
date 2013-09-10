# This flashes an LED on a BeagleBone Black.
# I think I got it from https://github.com/alexanderhiam/PyBBIO/blob/master/tests/io_test.py
# so probably I should just clone myself a PyBBIO repo.

from mmap import mmap
import time, struct

GPIO1_offset = 0x4804c000
GPIO1_size = 0x4804cfff-GPIO1_offset
GPIO_OE = 0x134
GPIO_SETDATAOUT = 0x194
GPIO_CLEARDATAOUT = 0x190
USR1 = 1<<22

with open("/dev/mem", "r+b" ) as f:
  mem = mmap(f.fileno(), GPIO1_size, offset=GPIO1_offset)

packed_reg = mem[GPIO_OE:GPIO_OE+4]
reg_status = struct.unpack("<L", packed_reg)[0]
reg_status &= ~(USR1)
mem[GPIO_OE:GPIO_OE+4] = struct.pack("<L", reg_status)

for i in range(5):
    # Set it high:
    mem[GPIO_SETDATAOUT:GPIO_SETDATAOUT+4] = struct.pack("<L", USR1)
    time.sleep(0.5)
    # Set it low:
    mem[GPIO_CLEARDATAOUT:GPIO_CLEARDATAOUT+4] = struct.pack("<L", USR1)
    time.sleep(0.5)

mem.close()