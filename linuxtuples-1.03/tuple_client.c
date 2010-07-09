/*****************************************************************
 *
 * LinuxTuples - an open-source tuple space for Linux clusters
 * Copyright (c) 2003, Will Ware <wware@alum.mit.edu>
 * All rights reserved.
 * 
 *    Redistribution and use in source and binary forms, with or
 *    without modification, are permitted provided that the following
 *    conditions are met:
 * 
 *    + Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 *    + Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials provided
 *    with the distribution.
 *
 *    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 *    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 *    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 *    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *    USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *    AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 *    IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 *    THE POSSIBILITY OF SUCH DAMAGE.
 *
 *****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "tuple.h"

#define PARALLEL 4

#define N  8 * 1024

/**
 * Put out requests to have FFT operations done, and measure how
 * long it takes to get the results back.
 */
int
main(int argc, char *argv[])
{
	struct tuple *s, *t, *u;
	int i, j, iters = 0;
	int r1[PARALLEL], r2[PARALLEL], r3[PARALLEL];
	double x[N], y[N], mult;
	struct timeval T0, T1;
	int rndsock;
	struct context ctx;

	if (get_server_portnumber(&ctx)) {
		if (argc < 3) {
			/* help message */
			fprintf(stderr,
				"Usage: %s <server> <portnumber>\n", argv[0]);
			exit(1);
		}
		strcpy(ctx.peername, argv[1]);
		ctx.portnumber = atoi(argv[2]);
	}

	rndsock = open("/dev/urandom", O_RDONLY);
	mult = 1.0 / pow(2.0, 31);
	for (i = 0; i < N; i++) {
		x[i] = mult * random_int();
		y[i] = mult * random_int();
	}

	s = make_tuple("siiis#s#", "fft",
		       0, 0, 0, x, N * sizeof(double), y, N * sizeof(double));

	t = make_tuple("siii??", "fft done", 0, 0, 0);

	gettimeofday(&T0, NULL);

	while (1) {

		for (j = 0; j < PARALLEL; j++) {
			r1[j] = random_int();
			r2[j] = random_int();
			r3[j] = random_int();
			s->elements[1].data.i = r1[j];
			s->elements[2].data.i = r2[j];
			s->elements[3].data.i = r3[j];
			if (put_tuple(s, &ctx)) {
				perror("put_tuple failed");
				exit(1);
			}
		}

		for (j = 0; j < PARALLEL; j++) {
			t->elements[1].data.i = r1[j];
			t->elements[2].data.i = r2[j];
			t->elements[3].data.i = r3[j];
			u = get_tuple(t, &ctx);
			if (u == NULL) {
				perror("get_tuple failed");
				exit(1);
			}
		}

		gettimeofday(&T1, NULL);
		iters += PARALLEL;
		printf("%f\n", TIMEVAL_DIFF(T0, T1) / iters);
	}

	close(rndsock);
	destroy_tuple(s);
	destroy_tuple(t);
	destroy_tuple(u);

	return 0;
}
