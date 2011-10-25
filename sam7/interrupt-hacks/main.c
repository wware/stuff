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
#include "leds.h"
#include "usb.h"

extern void init_timer(void);
extern void wait(unsigned long time);
extern void usb_open(struct _AT91S_USBDEV *usbdev);
extern void main_loop_iteration(void);


/*** Main Program ***/

static isOnline;

void usb_status(unsigned online)
{
    isOnline = online;
}

int main (void)
{
    AT91S_PIO * pPIO = AT91C_BASE_PIOA;         /* Global Pointer to PIO */

    led_init();

    // System peripherals are those appearing inside the "System Controller" block in
    // diagram 2-2 in the AT91SAM7S datasheet. The other peripherals need to be enabled
    // by having their clock turned on in the PMC. Grep for "AT91F_PMC_EnablePeriphClock"
    // in lib_AT91SAM7S64.h. Note that many of these AT91F_FOOBAR_CfgPMC functions are
    // setting the same bits as others, so it's simplest just to set the register once
    // as is done here.
    *AT91C_PMC_PCER = (1 << AT91C_ID_PIOA) |  /* Enable Clock for PIO    */
        (1 << AT91C_ID_IRQ0) |  /* Enable Clock for IRQ0   */
        (1 << AT91C_ID_US0);    /* Enable Clock for USART0 */

    pPIO->PIO_PER  = LED_MASK;                /* Enable PIO for LED1  */
    pPIO->PIO_OER  = LED_MASK;                /* LED1 is an Output     */
    pPIO->PIO_SODR = LED_MASK;                /* Turn off LED ("1")    */
    pPIO->PIO_OWER = LED1;                    /* LED1 ODSR Write Enable  */

    init_timer();                            /* Initialize Timer */

    /* Wait for the end of enumeration
     * When running Linux in a VM on the Macbook, this finishes after you make
     * the choice of whether the device will connect to OSX or Linux.
     */
    while (!isOnline);

    while (1) {
        // Check enumeration
        if (isOnline)
            main_loop_iteration();
    }
}
