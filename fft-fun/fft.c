#include <time.h>
#include <stdlib.h>
#include <malloc.h>
#include <math.h>

/*
 * Stolen from http://wiki.allegro.cc/index.php?title=FFT
 * Original code by Tobias Dammers (c) 2005, who writes:
 *   "Feel free to use these routines as you see fit."
 * Modified by Will Ware, 2011.
 *
 * The forward FFT. rex holds the real part of the signal, imx the imaginary
 * part. Both arrays must be at least 2^m samples long. The result is returned
 * in the same two arrays, ie the transform is in-place. After transforming,
 * rex holds the real part of the frequency domain, and imx the imaginary
 * part. To calculate the frequency spectrum of an arbitrary block of wave
 * data, put the input in rex[], and set imx[] to all-zero.
 */
void fft(int m, double* rex, double* imx)
{
    int n = 1 << m;
    int nm1 = n - 1;
    int nd2 = n / 2;
    int j = nd2;
    double tr, ti;
    int i;
    int l;

    for (i = 1; i < n; ++i) {
        int k;
        if (i < j) {
            tr = rex[j];
            ti = imx[j];
            rex[j] = rex[i];
            imx[j] = imx[i];
            rex[i] = tr;
            imx[i] = ti;
        }
        k = nd2;
        while ((k <= j) && (k > 0)) {
            j -= k;
            k = k >> 1;
        }
        j += k;
    }

    for (l = 1; l <= m; ++l) {
        int i;
        int le = 1 << l;
        int le2 = le/2;
        double ur = 1.0f;
        double ui = 0.0f;
        double sr = cos(M_PI/(double)le2);
        double si = -sin(M_PI/(double)le2);
        for (j = 1; j <= le2; ++j) {
            for (i = j-1; i <= nm1; i += le) {
                int ip = i + le2;
                tr = rex[ip] * ur - imx[ip] * ui;
                ti = rex[ip] * ui + imx[ip] * ur;
                rex[ip] = rex[i] - tr;
                imx[ip] = imx[i] - ti;
                rex[i] += tr;
                imx[i] += ti;
            }
            tr = ur;
            ur = tr * sr - ui * si;
            ui = tr * si + ui * sr;
        }
    }
}

#ifdef TESTFFT
#define MAX_N 14
static double rex[1 << MAX_N], imx[1 << MAX_N];
struct timespec before, after;
extern double genrand_real1(void);

int main(int argc, char *argv[])
{
    int i;
    long n = MAX_N;
    double seconds;
    // n is a small integer which is converted to a power of 2
    // e.g. n=9 => 512-point FFT
    if (argc > 1) {
        n = strtol(argv[1], NULL, 10);
        if (n > MAX_N)
            n = MAX_N;
    }
    printf("import numpy\n");
    printf("x = numpy.array([\n");
    for (i = 0; i < 1 << n; i++) {
        rex[i] = genrand_real1();
        imx[i] = genrand_real1();
        printf("%.8lf+%.8lfj,\n", rex[i], imx[i]);
    }
    printf("])\n\n");

    clock_gettime(0, &before);
    fft(n, rex, imx);
    clock_gettime(0, &after);

    printf("y = numpy.array([");
    for (i = 0; i < 1 << n; i++) {
        printf("%.8f+%.8fj,\n", rex[i], imx[i]);
    }
    printf("])\n\n");

    seconds = (after.tv_sec + 1.0e-9 * after.tv_nsec)
        - (before.tv_sec + 1.0e-9 * before.tv_nsec);
    printf("size = %d\n", 1 << n);
    printf("runtime = %.6f    # usecs\n",
           1000000.0 * seconds);

    return 0;
}
#endif
