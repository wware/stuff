/*
 * gcc -o verify-number verify-number.c
 *
 * We want to take six bytes as a string on the command line, and
 * verify that the last byte is the XOR of the first five. If it
 * is, we dump the first five bytes to standard output with an exit
 * status of zero. Otherwise we dump nothing to stdout and have a
 * non-zero exit status.
 */

#include <stdio.h>
#include <string.h>

//#define DEBUG 1

unsigned long x;
unsigned int bytes[6];

int main(int argc, char *argv[])
{
    int i;
    if (argc != 2)
	return 1;
#ifdef DEBUG
    printf("Got to %d\n", __LINE__);
#endif
    if (strlen(argv[1]) != 12)
	return 1;
#ifdef DEBUG
    printf("Got to %d\n", __LINE__);
#endif
    if (sscanf(argv[1], "%02x%02x%02x%02x%02x%02x",
	       &bytes[0], &bytes[1], &bytes[2],
	       &bytes[3], &bytes[4], &bytes[5]) != 6)
	return 1;
#ifdef DEBUG
    printf("Got to %d\n", __LINE__);
    printf("Bytes: %d %d %d %d %d %d\n",
	   bytes[0], bytes[1], bytes[2],
	   bytes[3], bytes[4], bytes[5]);
#endif
    if ((bytes[0] ^ bytes[1] ^ bytes[2] ^
	 bytes[3] ^ bytes[4]) != bytes[5])
	return 1;
#ifdef DEBUG
    printf("Got to %d\n", __LINE__);
#endif
    printf("%02X%02X%02X%02X%02X",
	   bytes[0], bytes[1], bytes[2],
	   bytes[3], bytes[4]);
    return 0;
}
