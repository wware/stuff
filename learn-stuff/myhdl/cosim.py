import os
import unittest
from myhdl import Cosimulation
import tryit
from tryit import TestGrayCodeProperties

# I don't quite understand Cosimulation yet. I think the idea is to
# run a "cooperative" simulation where Iverilog's vvp simulates your
# hardware (by way of Verilog) while the inputs are driven and the
# outputs are watched by your Python code.

# Before you run this, you need to have run "python tryit.py" in order
# to create bin2gray.v and tb_bin2gray.v.

# The problem appears to be that in the the two unit test methods,
# where each defines an inner "test" function, the Signal variables
# B and G can be set to None when you hit the "yield delay(10)"
# statement, and I'm pretty sure that should not happen. Possibly a
# bug in the C code?

cmd = "iverilog -o bin2gray -Dwidth=%d bin2gray.v tb_bin2gray.v"


def bin2gray(B, G, width):
    os.system(cmd % width)
    return Cosimulation("vvp -m ./myhdl.vpi bin2gray", B=B, G=G)


tryit.bin2gray = bin2gray
unittest.main()
