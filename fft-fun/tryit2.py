#######################################################################
# Quick review of computational ins and outs of FFTs and 2D FFTS

import numpy
import random

#######################################################################
# These are needed because of teeny variations in floating point
# arithmetic.

def assertVectorsNearlyEqual(x, y):
    z = reduce(lambda u, v: u + v, x - y)
    if z * z.conjugate() > 1.0e-20:
        assert False

def assertMatricesNearlyEqual(X, Y):
    for x, y in map(None, X, Y):
        assertVectorsNearlyEqual(x, y)

#######################################################################
# Preliminaries

N = 128

def randreal():
    return 2 * random.random() - 1

def randcomplex():
    return randreal() + (0+1j) * randreal()

# Create a NxN matric of random complex numbers.
X = [ ]
for i in range(N):
    X.append(numpy.array([randcomplex() for j in range(N)]))
X = numpy.array(X)

#######################################################################
# Confirm this is the correct way to compute a 2D FFT. Yup, it is.

def row_wise_fft(X):
    return numpy.array([numpy.fft.fft(x) for x in list(X)])

Y = row_wise_fft(X).transpose()
Y = row_wise_fft(Y).transpose()

assertMatricesNearlyEqual(numpy.fft.fft2(X), Y)

#######################################################################
# To do an inverse FFT, conjugate the twiddle factors and divide by N
# at the end. The __ifft function defined here does not work in the
# general case, but it still helps to confirm my understanding. See
# the defintion of inverse DFT here:
# http://en.wikipedia.org/wiki/Discrete_Fourier_transform#Definition

# NB: real-valued!
x = numpy.array([randreal() for j in range(N)])

def __ifft(x):
    # conj() here only conjugates the twiddle factors correctly if
    # x is real-valued.
    return (1.0/N) * numpy.fft.fft(x).conj()

assertVectorsNearlyEqual(numpy.fft.ifft(x), __ifft(x))

#######################################################################
# Next build an inverse 2D FFT. This is simply a matter of doing the
# inverse operations in reverse order.

def row_wise_ifft(X):
    # do not use __ifft! it doesn't work in the general case
    return numpy.array([numpy.fft.ifft(x) for x in list(X)])

assertMatricesNearlyEqual(row_wise_ifft(row_wise_fft(X)), X)

Z = row_wise_ifft(Y.transpose())
Z = row_wise_ifft(Z.transpose())

assertMatricesNearlyEqual(X, Z)

# Happy happy joy joy
