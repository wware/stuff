//*--------------------------------------------------------------------------------------
//*      ATMEL Microcontroller Software Support  -  ROUSSET  -
//*--------------------------------------------------------------------------------------
//* The software is delivered "AS IS" without warranty or condition of any
//* kind, either express, implied or statutory. This includes without
//* limitation any warranty or condition with respect to merchantability or
//* fitness for any particular purpose, or against the infringements of
//* intellectual property rights of others.
//*--------------------------------------------------------------------------------------
//* File Name           : USB HID example
//* Object              :
//* Translator          :
//* 1.0 05/Oct/04 ODi	: CReation
//* 1.1 04/Nov/04 JPP	: Add led1 On at power supply
//*--------------------------------------------------------------------------------------

#include "board.h"

#define BUTTON_SAMPLING 1700 // Sampling frequency of buttons

#define BL_OFF(pio) ((pio) & SW1)
#define BR_OFF(pio) ((pio) & SW2)

#define BL_ON(pio) (!BL_OFF(pio))
#define BR_ON(pio) (!BR_OFF(pio))
#define CLICKL_ON 1
#define CLICKR_ON 2

void delay()
{
    int i;
    for (i = 0; i < 0x100000; i++)
        asm("nop");
}

int main()
{
    LowLevelInit();   /* thanks to Jim Lynch */

    /* Get a pointer to the PIO data structure. The PIO is the "Peripheral
     * input/output controller" and is the part of the ARM chip which can
     * access input/output (GPIO) pins.
     */
    volatile AT91PS_PIO pPIO = AT91C_BASE_PIOA;
    /* Enable PIO to control the LEDs. */
    pPIO->PIO_PER = LED_MASK;
    /* Set LED pins to outputs. */
    pPIO->PIO_OER = LED_MASK;
    while(1) {
	/* Turn on LED1 and turn off LED2. Note that separate registers are
	 * used to set or clear PIO bits.
         */
        pPIO->PIO_SODR = LED1;
        pPIO->PIO_CODR = LED2;
        delay();
	/* Turn off LED1 and turn on LED2. */
        pPIO->PIO_SODR = LED2;
        pPIO->PIO_CODR = LED1;
        delay();
    }
    return 0;
}
