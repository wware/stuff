#include <AT91SAM7S64.h>                    /* AT91SAMT7S64 definitions */
#include "Board.h"

/*** LEDs ***/

static unsigned char led_state;
volatile long delay_count;

static void delay(unsigned char y)
{
    while (y--)
        for (delay_count = 0; delay_count < 0x80000; delay_count++);
}

// Olimex proto board: LED1 is green, LED2 is yellow
// Olimex header board: LED1 is green, no LED2

void AT91F_PIO_CfgOutput(AT91PS_PIO pPio,        // \arg pointer to a PIO controller
                         unsigned int pioEnable) // \arg PIO to be enabled
{
    pPio->PIO_PER = pioEnable; // Set in PIO mode
    pPio->PIO_OER = pioEnable; // Configure in Output
}

void AT91F_PIO_SetOutput(AT91PS_PIO pPio,   // \arg  pointer to a PIO controller
                         unsigned int flag) // \arg  output to be set
{
    pPio->PIO_SODR = flag;
}

void AT91F_PIO_ClearOutput(AT91PS_PIO pPio,   // \arg  pointer to a PIO controller
                           unsigned int flag) // \arg  output to be cleared
{
    pPio->PIO_CODR = flag;
}

void led_init(void)
{
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, LED_MASK);
    AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED_MASK);
    led_state = 0;
}

void morse_code_hex(unsigned char digits, int number)
{
    unsigned char i, j;
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, LED_MASK);
    for (i = 0; i < digits; i++) {
        for (j = 0; j < 4; j++) {
            if ((number >> 4 * (digits - 1 - i)) & (1 << (3 - j))) {
                AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
                delay(4);
                AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
            } else {
                AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
                delay(1);
                AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
            }
            delay(1);
        }
        delay(5);
    }
    // recommend delay(15) between calls
}

void blink_number(unsigned char x, int y)
{
    unsigned char i;
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, LED_MASK);
    while (1) {
        for (i = 0; i < x; i++) {
            AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
            delay(1);
            AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
            delay(1);
        }
        delay(5);
        morse_code_hex(8, y);
        delay(15);
    }
}

void led_blip(void)
{
    AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
    for (delay_count = 0; delay_count < 0x10000; delay_count++);
    AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
}

void advance_leds(void)
{
    switch (led_state) {
    case 0:
        AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1);
        AT91F_PIO_SetOutput  (AT91C_BASE_PIOA, LED2);
        led_state = 1;
        break;
    case 1:
        AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
        led_state = 2;
        break;
    case 2:
        AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED2);
        AT91F_PIO_SetOutput  (AT91C_BASE_PIOA, LED1);
        led_state = 3;
        break;
    case 3:
        AT91F_PIO_SetOutput  (AT91C_BASE_PIOA, LED1|LED2);
        led_state = 0;
        break;
    }
}
