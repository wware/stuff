Fun with Fast Fourier Transforms
================================

The immediate need for this is that a friend needs to solve a problem
involving alignment of a pair of similar images. Remembering that convolution
in the time (or space) domain becomes multiplication in the frequency domain,
we can 2D-fft the two images, multiply the results, 2D-un-fft the product, and
get something that has a big spike at the pixel representing the horizontal
and vertical offset that will make the images line up.

Things you'll want to install to prototype stuff in Python on Ubuntu Lucid::

 sudo apt-get install libamd2.2.0 libblas3gf libc6 libgcc1 libgfortran3 \
     liblapack3gf libumfpack5.4.0 libstdc++6 build-essential gfortran \
     libatlas-sse2-dev python-all-dev
 sudo apt-get install python-setuptools python-pip
 sudo apt-get install python-pyfits
 sudo pip install numpy
 sudo pip install scipy
 sudo pip install pylab

Hmm, that's overkill. I don't need scipy, I only need numpy, and numpy drags a
lot less baggage around.