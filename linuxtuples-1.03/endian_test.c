/*
 * gcc -o endian_test endian_test.c ; ./endian_test
 *
 * There is an endianness issue on the PowerPC Mac. It definitely
 * applies to 4-byte integers, where it should be fixable with htonl
 * and ntohl. It might also apply to floats and doubles. If we're
 * super-unlucky, it also applies to strings (or twos or fours of
 * bytes within strings), and maybe to byte-array images of structs.
 *
 * The idea here is to see just how extensive the endianness damage
 * is, and see whether the Mac is a salvageable platform.
 */

#include <stdio.h>

#define hack_int(expr)   printf("%s = %d (0x%08X)\n", #expr, expr, expr)

#define hack_float(expr)   printf("%s = %f\n", #expr, expr)

#define hack_double(expr)   printf("%s = %lf\n", #expr, expr)

int main(void)
{
	int i;
	float f;
	double d;
	char s[40];

	i = 0x12345678;
	hack_int(sizeof(int));
	hack_int(i);
	hack_int(htonl(i));
	hack_int(ntohl(i));
	printf("\n");

	/*
	 * I think I'll need to test with real tuples between Linux and
	 * Mac machines before I can be sure to what ills the floats and
	 * doubles may fall victim.
	 */

	f = 3.14159e12;
	hack_int(sizeof(float));
	hack_float(f);
	for (i = 0; i < sizeof(float); i++)
		printf("%02X", ((char*) &f)[i]);
	// what is the float equivalent of htonl/ntohl?
	printf("\n\n");

	d = 3.14159e12;
	hack_int(sizeof(double));
	hack_double(d);
	for (i = 0; i < sizeof(double); i++)
		printf("%02X", ((char*) &d)[i]);
	// what is the double equivalent of htonl/ntohl?
	printf("\n\n");
}


/*
 * Local Variables:
 * c-basic-offset: 8
 * tab-width: 8
 * End:
 */
