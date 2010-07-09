/*
 * ./mmt.py < tryit.c | indent - > yo.c
 * cproto yo.c > yo.h
 */

#include <string.h>
#include <stdlib.h>
#include "yo.h"

/* Dumb restrictions on parallelized C functions:
 * (1) All variables must start with capital letters. Any variable that
 *     isn't capitalized will not remember its state across context
 *     switches.
 * (2) Function calls are extremely primitive. There are only two legal
 *     syntaxes:
 *     (i)   y = f(arg1, arg2, ...);    // store return value
 *     (ii)  f(arg1, arg2, ...);        // discard return value
 */

#pragma par_on

int add_these(int X, int Y)
{
	int Z;

	Z = X + Y;
	cx;
	return Z;
}

void caller(struct context *My_context)
{
	int X, Y, Z;

	X = 4;
	Y = 3;
	cx;
	Z = add_these(X, Y);
	if (Z != 7) {
		fprintf(stderr, "Thread %08x messed up!\n",
			(int) My_context);
		exit(1);
	}
}

#pragma par_off

#define NCTXTS 100000
struct context *batch[NCTXTS];

int main(void)
{
	int i, any_left = 1;

	for (i = 0; i < NCTXTS; i++) {
		struct context *ctxt;
		batch[i] = ctxt = new_context();
		caller(ctxt, ctxt);
	}

	while (any_left) {
		any_left = 0;
		for (i = 0; i < NCTXTS; i++)
			if (thisfunc(batch[i]))
				any_left = 1;
	}

	printf("All done\n");
	return 0;
}
