import os, sys, numpy, math
import tryit2    # also runs unit test on FFT, 2D FFT, inv 2D FFT

os.system('make clean')
os.system('make TESTFFT=1 fft')

outf = open('runtime.dat', 'w')

for i in range(4, 15):
    os.system('rm -f data.py data.pyc')
    os.system('./fft %d > data.py' % i)
    try:
        reload(data)
    except:
        import data
    tryit2.assertVectorsNearlyEqual(data.y, numpy.fft.fft(data.x))
    outf.write('%f %f\n' % (data.size, 0.001 * data.runtime))

outf.close()

if "plot" in sys.argv[1:]:
    outf = os.popen('gnuplot', 'w')
    outf.write('set xlabel "FFT size"\n')
    outf.write('set ylabel "Runtime, milliseconds"\n')
    outf.write('set logscale xy\n')
    outf.write('set grid\n')
    outf.write('plot "runtime.dat" with lines\n')
    outf.write('pause 30\n')
    outf.close()
