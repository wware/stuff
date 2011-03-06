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

static char upcase(char c)
{
    if (c >= 'a' && c <= 'z')
        return c + 'A' - 'a';
    else
        return c;
}


static char *recognize_command(char *what_we_want)
{
    char *what_we_got = inbuf;
    while (1) {
        // if we reach the end of what we want with all good
        // matches, we succeed
        if (*what_we_want == '\0')
            return what_we_got;
        // if we see a bad match, we fail immediately
        if (upcase(*what_we_want) != upcase(*what_we_got))
            return NULL;
        what_we_want++;
        what_we_got++;
    }
}

static int interpret_hex_digit(char c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'f')
        return c + (10 - 'a');
    if (c >= 'A' && c <= 'F')
        return c + (10 - 'A');
    return -1;
}

// numbers are hexadecimal
static int get_number(char **p)
{
    char *q, c;
    int x = 0, y;
    if (p == NULL) return 0;
    q = *p;
    while (1) {
        // advance to the start of the number, or '\0'
        c = *q++;
        if (c == '\0') return 0;
        y = interpret_hex_digit(c);
        if (y != -1) break;
    }
    while (1) {
        x = (x << 4) + y;
        c = *q++;
        y = interpret_hex_digit(c);
        if (y == -1) {
            *p = q;  // in case we want to fetch another number
            return x;
        }
    }
}

void sleep(int seconds)
{
    long period = (long) seconds * 2000000L;
    volatile long snooze;
    for (snooze = 0; snooze < period; snooze++);
}

int main (void)
{
    int inbptr = 0;
    char *argp;

    LowLevelInit();
    uart0_init(115200);
    led_init();

    // enable peripheral clock for PIOA and USART0
    AT91C_BASE_PMC->PMC_PCER =
        (1 << AT91C_ID_PIOA) | (1 << AT91C_ID_US0);
    // set up inputs (PA0 thru PA5) and outputs (PA8 thru PA13)
    // PA6 and PA7 are needed for the UART!!!
    AT91C_BASE_PIOA->PIO_PER = 0x3F3F;
    AT91C_BASE_PIOA->PIO_ODR = 0x003F;
    AT91C_BASE_PIOA->PIO_OER = 0x3F00;

    while (1) {
        int c = uart0_getc();
        int x;
        uart0_putc(c);
        if (c == '\n') {

            argp = recognize_command("TEST");
            if (argp != NULL) {
                uart0_puts("Happy happy joy joy\n");
                uart0_puts("OK\n");
                goto cmd_done;
            }

            argp = recognize_command("SET");
            if (argp != NULL) {
                x = get_number(&argp);
                if (x >= 0 && x <= 6) {
                    AT91C_BASE_PIOA->PIO_SODR = 1 << (x + 8);
                    uart0_puts("OK\n");
                } else {
                    uart0_puts("ERROR\n");
                }
                goto cmd_done;
            }

            argp = recognize_command("CLEAR");
            if (argp != NULL) {
                x = get_number(&argp);
                if (x >= 0 && x <= 6) {
                    AT91C_BASE_PIOA->PIO_CODR = 1 << (x + 8);
                    uart0_puts("OK\n");
                } else {
                    uart0_puts("ERROR\n");
                }
                goto cmd_done;
            }

            // For the moment, let's have the phone poll the
            // SAM7 to read the inputs. When we get smarter, the
            // SAM7 will issue an unsolicited notification that
            // an input bit has changed.
            argp = recognize_command("READ");
            if (argp != NULL) {
                int i;
                x = 0;
                for (i = 0; i < 6; i++)
                    if (AT91C_BASE_PIOA->PIO_PDSR & (1 << i))
                        x += (1 << i);
                uart0_put_hex_digit(x >> 4);
                uart0_put_hex_digit(x);
                uart0_puts("\n");
                goto cmd_done;
            }

            // no valid command was recognized
            uart0_puts("ERROR\n");

        cmd_done:
            inbptr = 0;
        } else {
            inbuf[inbptr++] = c;
            inbuf[inbptr] = '\0';
        }
    }
}
