#include <stdio.h>

/* heap_ww.c */
void enableDefrag(int yesPlease);
void *pvPortMalloc(size_t xWantedSize);
void vPortFree(void *pv);
size_t xPortGetFreeHeapSize(void);

static void *pointers[5000];

#if 0
static unsigned int lfsr = 0xACE1;

static void restart_prng(void)
{
    lfsr = 0xACE1;
}

// Get a pseudorandom number generator from Wikipedia
static int prng(void)
{
    static unsigned int bit;
    /* taps: 16 14 13 11; characteristic polynomial: x^16 + x^14 + x^13 + x^11 + 1 */
    bit  = ((lfsr >> 0) ^ (lfsr >> 2) ^ (lfsr >> 3) ^ (lfsr >> 5) ) & 1;
    lfsr =  (lfsr >> 1) | (bit << 15);
    return lfsr & 0xffff;
}
#endif

#define SMALL_ALLOCS  35
#define BIG_ALLOCS  12

int main(void)
{
    int i;
    void *p;

    enableDefrag(1);

#if 1
    for (i = 0; i < SMALL_ALLOCS; i++) {
        p = pvPortMalloc(100);
        if (p == NULL) {
            printf("%d small ouch\n", i);
            return 0;
        } else {
            pointers[i] = p;
            printf("%d small OK\n", i);
        }
    }
    for (i = 0; i < SMALL_ALLOCS; i++) {
        vPortFree(pointers[i]);
    }
#endif

    for (i = 0; i < BIG_ALLOCS; i++) {
        p = pvPortMalloc(300);
        if (p == NULL) {
            printf("%d big ouch\n", i);
            return 0;
        } else {
            pointers[i] = p;
            printf("%d big OK\n", i);
        }
    }
    for (i = 0; i < BIG_ALLOCS; i++) {
        vPortFree(pointers[i]);
    }
    return 0;
}
