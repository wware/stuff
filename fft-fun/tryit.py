import numpy
import Image

showImages = True
inf = Image.open('pig_intestines.jpg')

m = 20
N = 256

# Guess these offsets by examining image data
dx = -120
dy = 38

# LEFT VIEW
box = (m, m, m+N, m+N)
if showImages:
    inf.crop(box).show()
image1 = numpy.asarray(inf.crop(box))

# RIGHT VIEW
box = (m+dx, m +dy, m+N+dx, m+N+dy)
if showImages:
    inf.crop(box).show()
image2 = numpy.asarray(inf.crop(box))

#################################################
# Start fooling with FFTs

f1 = numpy.fft.fft2(image1)
f2 = numpy.fft.fft2(image2)
X = numpy.fft.ifft2(f1 / f2)  # inverse-fft of ratio
X = (X * X.conj()).real

maxrow = maxcol = 0
maxval = 0.
for row in range(-N/2, N/2):
    thisRow = X[row]
    for col in range(-N/2, N/2):
        thisValue = thisRow[col]
        if thisValue > maxval:
            maxrow = row
            maxcol = col
            maxval = thisValue

print "Actual:  ", dx, dy
print "Estimate:", maxcol, maxrow
