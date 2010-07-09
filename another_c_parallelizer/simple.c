/*
 * Random collection of test cases
 */

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#include "thr.h"

#pragma par_on

void spurious(void)
{
	int i;
	CSW;
	for (i = 0; i < 5; i++) {
		printf(" %d", i);
		CSW;
	}
}

int add(int x, int y)
{
	int z, v;
	z = x + y;
#if 0
	/* Test pfork() */
	v = pfork();
	if (v == 0) {
		printf("child\n");
	} else {
		/* Watch out for %08x here, because the "x" is interpreted as a
		 * variable name. Funny, but dangerous. The correct fix is a
		 * real C grammar parser, that can distinguish "x" occurring in
		 * a string from x as a real variable reference.
		 */
		printf("parent, child thread = %08X\n", v);
	}
	NEW_THREAD(spurious);
	NEW_THREAD(spurious);
	NEW_THREAD(spurious);
#endif
	CSW;
	return z;
}

void caller(void)
{
	int result;
	int i, plugh[8];

	/* Make sure arrays work, and context switches don't break
	 * for-loops.
	 */
	for (i = 0; i < 8; i++) {
		plugh[i] = 0xdeadbeef;
		CSW;
	}
	/* Test calls to other parallelized functions. */
	result = add(4, 3) * add(5, 6);
	if (result != 77)
		printf("result is %d, should be 77\n", result);
	CSW;
	/* i should be left at 8 */
	result += i;
	CSW;
	/* Complain if anything goes unexpectedly. */
	if (result != (4 + 3) * (5 + 6) + 8 || plugh[5] != 0xdeadbeef) {
		fprintf(stderr, "result=%d, should be %d\n",
			result, (4 + 3) * (5 + 6) + 8);
		fprintf(stderr, "plugh[5]=0x%08x, should be 0xdeadbeef\n",
			plugh[5]);
		exit(1);
	}
}

#pragma par_off

#define NUMTHREADS 100000

int main(void)
{
	int i;
	ENTER();

#ifdef DEBUG
	/*
	 * When debugging, this allows us to print function names
	 * in our frame stack printouts.
	 */
	register_symbol("spurious", spurious);
	register_symbol("add", add);
	register_symbol("caller", caller);
#endif

	for (i = 0; i < NUMTHREADS; i++)
		new_thread(caller, sizeof(struct caller_frame));
	while (step_thread(1000) == 0);
	printf("\nAll is well\n");
	LEAVE();
	return 0;
}
