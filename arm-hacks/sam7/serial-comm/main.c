#include "h64_board.h"
#include "dbgu.h"

#define BUFSIZE 256   // let's keep this a power of two
static char inbuf[BUFSIZE];   // dumb line buffer

static void delay(unsigned char y)
{
    volatile long x;
    while (y--)
        for (x = 0; x < 0x80000; x++);
}

static int strcpy(char *dst, char *src)
{
    while (*src != '\0')
        *dst++ = *src++;
}

static int cmd_starts_with(char *whatever)
{
    char *p = inbuf;
    while (1) {
        char q = *whatever++;
        char r = *p++;
        if (q == '\0') return 1;   // success
        // convert everything to uppercase
        if (q >= 'a' && q <= 'z')
            q += 'A' - 'a';
        if (r >= 'a' && r <= 'z')
            r += 'A' - 'a';
        if (q == r)
            continue;
        return 0;    // strings differ, fail
    }
}

static void process_command(void)
{
    if (cmd_starts_with("HELLO")) {
        uart0_puts("OK\n");
    }
    else {
        uart0_puts("ERROR\n");
    }
}

int main (void)
{
    int inbptr = 0;

    LowLevelInit();
    uart0_init(115200);
    led_init();

    while (1) {
        int c = uart0_getc();
        uart0_putc(c);
        if (c == '\n') {
            uart0_puts("ABC\n");
            process_command();
            inbptr = 0;
        } else {
            inbuf[inbptr++] = c;
            inbuf[inbptr] = '\0';
        }
    }
}
