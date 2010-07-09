/**
 * \file fft.c
 * This is adapted from some reference FFT C code found at:
 * http://www.cag.lcs.mit.edu/streamit/results/fft/code/c/unifft.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>

#ifdef MACOSX
#include <malloc/malloc.h>
#else
#include <malloc.h>
#endif

#include "tuple.h"

/************************************************************************************/

static void compute_W(int n, double *W_re, double *W_im);
static void permute_bitrev(int n, double *A_re, double *A_im);
static int bitrev(int inp, int numbits);
static int log_2(int n);
static void fft(int n, double *A_re, double *A_im, double *W_re,
		double *W_im);

/************************************************************************************/

/**
 * Find out where the tuple server is. Go into a loop: get a request
 * to perform an FFT operation, perform it, and put the result back in
 * the tuple space.
 *
 * Three fields in the request tuple are used as unique identifiers to
 * make sure the customer reclaims the correct result, in case several
 * FFTs are being done simultaneously. These identifier fields can be
 * initialized using random_int() in tuple.c.
 */
int
main(int argc, char *argv[])
{
	struct context ctx;
	struct tuple *s, *t, *u;
	double *A_re, *A_im, *W_re, *W_im;

	if (get_server_portnumber(&ctx)) {
		if (argc < 3) {
			/* help message */
			fprintf(stderr,
				"Usage: %s <server> <portnumber>\n", argv[0]);
			return 1;
		}
		strcpy(ctx.peername, argv[1]);
		ctx.portnumber = atoi(argv[2]);
	}

	s = make_tuple("s?????", "fft");
	t = make_tuple("s???ss", "fft done", "", "");

	while (1) {
		int n;
		u = get_tuple(s, &ctx);
		if (u == NULL) {
			perror("get_tuple failed");
			exit(1);
		}
		A_re = (double *) tuple_string_field(u, &n, 4);
		n /= sizeof(double);
		A_im = (double *) tuple_string_field(u, NULL, 5);

		W_re = (double *) malloc(sizeof(double) * n / 2);
		if (W_re == NULL) {
			fprintf(stderr, "out of memory\n");
			return 1;
		}
		W_im = (double *) malloc(sizeof(double) * n / 2);
		if (W_im == NULL) {
			fprintf(stderr, "out of memory\n");
			return 1;
		}

		compute_W(n, W_re, W_im);
		fft(n, A_re, A_im, W_re, W_im);
		permute_bitrev(n, A_re, A_im);

		t->elements[1] = u->elements[1];
		t->elements[2] = u->elements[2];
		t->elements[3] = u->elements[3];

		t->elements[4].data.s.len = n * sizeof(double);
		t->elements[5].data.s.len = n * sizeof(double);
		t->elements[4].data.s.ptr = (char *) A_re;
		t->elements[5].data.s.ptr = (char *) A_im;

		if (put_tuple(t, &ctx)) {
			perror("put_tuple failed");
			exit(1);
		}
		free(W_re);
		free(W_im);
	}

	return 0;
}



/**
 * W will contain roots of unity so that W[bitrev(i,log2n-1)] = e^(2*pi*i/n)
 *
 * n should be a power of 2
 *
 * Note: W is bit-reversal permuted because fft(..) goes faster if this is done.
 *       see that function for more details on why we treat 'i' as
 *       a (log2n-1) bit number.
 */
static void
compute_W(int n, double *W_re, double *W_im)
{
	int i, br;
	int log2n = log_2(n);

	for (i = 0; i < (n / 2); i++) {
		br = bitrev(i, log2n - 1);
		W_re[br] = cos(((double) i * 2.0 * M_PI) / ((double) n));
		W_im[br] = sin(((double) i * 2.0 * M_PI) / ((double) n));
	}
}


/**
 * permutes the array using a bit-reversal permutation
 */
static void
permute_bitrev(int n, double *A_re, double *A_im)
{
	int i, bri, log2n;
	double t_re, t_im;

	log2n = log_2(n);

	for (i = 0; i < n; i++) {
		bri = bitrev(i, log2n);

		/* skip already swapped elements */
		if (bri <= i)
			continue;

		t_re = A_re[i];
		t_im = A_im[i];
		A_re[i] = A_re[bri];
		A_im[i] = A_im[bri];
		A_re[bri] = t_re;
		A_im[bri] = t_im;
	}
}


/**
 * treats inp as a numbits number and bitreverses it.
 * inp < 2^(numbits) for meaningful bit-reversal
 */
static int
bitrev(int inp, int numbits)
{
	int i, rev = 0;
	for (i = 0; i < numbits; i++) {
		rev = (rev << 1) | (inp & 1);
		inp >>= 1;
	}
	return rev;
}


/**
 * \return log n (to the base 2), if n is positive and power of 2
 */
static int
log_2(int n)
{
	int res;
	for (res = 0; n >= 2; res++)
		n = n >> 1;
	return res;
}

/**
 * fft on a set of n points given by A_re and A_im. Bit-reversal
 * permuted roots-of-unity lookup table is given by W_re and W_im.
 * More specifically, W is the array of first n/2 nth roots of unity
 * stored in a permuted bitreversal order. n must be a power of 2 to
 * work.
 *
 * Execution time on a 450MHz PIII is about n * log2(n) * 1.2 usecs.
 */

static void
fft(int n, double *A_re, double *A_im, double *W_re, double *W_im)
{
	double w_re, w_im, u_re, u_im, t_re, t_im;
	int m, g, b;
	int mt, k;

	/* for each stage */
	for (m = n; m >= 2; m >>= 1) {
		mt = m >> 1;

		/* for each group of butterfly */
		for (g = 0, k = 0; g < n; g += m, k++) {
			/* each butterfly group uses only one root of
			 * unity. actually, it is the bitrev of this
			 * group's number k. BUT 'bitrev' it as a
			 * log2n-1 bit number because we are using a
			 * lookup array of nth root of unity and using
			 * cancellation lemma to scale nth root to
			 * n/2, n/4,... th root.
			 *
			 * It turns out like the foll.
			 *   w.re = W[bitrev(k, log2n-1)].re;
			 *   w.im = W[bitrev(k, log2n-1)].im;
			 *
			 * Still, we just use k, because the lookup
			 * array itself is bit-reversal permuted.
			 */
			w_re = W_re[k];
			w_im = W_im[k];

			/* for each butterfly */
			for (b = g; b < (g + mt); b++) {
				t_re = w_re * A_re[b + mt] - w_im * A_im[b +
									 mt];
				t_im = w_re * A_im[b + mt] + w_im * A_re[b +
									 mt];

				u_re = A_re[b];
				u_im = A_im[b];
				A_re[b] = u_re + t_re;
				A_im[b] = u_im + t_im;
				A_re[b + mt] = u_re - t_re;
				A_im[b + mt] = u_im - t_im;

			}
		}
	}
}
