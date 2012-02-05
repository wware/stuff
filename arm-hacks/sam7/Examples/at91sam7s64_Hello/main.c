/******************************************************************************/
/*                                                                            */
/*  HELLO.C:  Hello World Example                                             */
/*                                                                            */
/******************************************************************************/
/*  ported and extended for arm-elf-gcc / WinARM by                           */
/*  Martin Thomas, KL, .de <eversmith@heizung-thomas.de>                      */
/*  modifications Copyright Martin Thomas 2005                                */
/*                                                                            */
/*  Based on file that has been a part of the uVision/ARM development         */
/*  tools, Copyright KEIL ELEKTRONIK GmbH 2002-2004                           */
/******************************************************************************/

/* see file Abstract.txt for more info on the gcc-port */

#include <stdio.h>                          /* I/O Functions */
#include <AT91SAM7S64.h>                    /* AT91SAMT7S64 definitions */
#include "Board.h"
#include "serial.h"

extern void init_timer(void);
extern void wait(unsigned long time);

AT91S_PIO * pPIO = AT91C_BASE_PIOA;         /* Global Pointer to PIO */

/*** Main Program ***/

int main (void)
{
    *AT91C_PMC_PCER = (1 << AT91C_ID_PIOA) |  /* Enable Clock for PIO    */
        (1 << AT91C_ID_IRQ0) |  /* Enable Clock for IRQ0   */
        (1 << AT91C_ID_US0);    /* Enable Clock for USART0 */

    pPIO->PIO_PER  = LED_MASK;                /* Enable PIO for LED1  */
    pPIO->PIO_OER  = LED_MASK;                /* LED1 is an Output     */
    pPIO->PIO_SODR = LED_MASK;                /* Turn off LED ("1")    */
    pPIO->PIO_OWER = LED1;                    /* LED1 ODSR Write Enable  */

    init_timer();                            /* Initialize Timer */

    while (1) {
        wait(400);
        pPIO->PIO_SODR = LED1;
        wait(400);
        pPIO->PIO_CODR = LED1;
    }
}
