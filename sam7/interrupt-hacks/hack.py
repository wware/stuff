import sys

outf = open("/dev/ttyUSB0", "w")
inf = open("/dev/ttyUSB0")

for i in range(8):
    if True:
        outf.write(sys.argv[1])
    else:
        outf.write("a")
    outf.flush()
    print inf.read()

outf.close()
inf.close()
