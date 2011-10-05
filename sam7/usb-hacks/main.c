//*--------------------------------------------------------------------------------------
//*      ATMEL Microcontroller Software Support  -  ROUSSET  -
//*--------------------------------------------------------------------------------------
//* The software is delivered "AS IS" without warranty or condition of any
//* kind, either express, implied or statutory. This includes without
//* limitation any warranty or condition with respect to merchantability or
//* fitness for any particular purpose, or against the infringements of
//* intellectual property rights of others.
//*--------------------------------------------------------------------------------------

#include "board.h"
#include "dbgu.h"
#include "descriptors.h"
#include "usb.h"

extern void usb_open(struct _AT91S_USBDEV *usbdev);
extern void main_loop_iteration(struct _AT91S_USBDEV *usbdev);

struct _AT91S_USBDEV   usbDevice;

//*--------------------------------------------------------------------------------------
//* Function Name       : main
//* Object              :
//*--------------------------------------------------------------------------------------

int main ( void )
{
    // Init USB device
    LowLevelInit();
    usb_init();
    led_init();
    usb_open(&usbDevice);

    /* Wait for the end of enumeration
     * When running Linux in a VM on the Macbook, this finishes after you make
     * the choice of whether the device will connect to OSX or Linux.
     */
    while (!usbDevice.IsConfigured(&usbDevice));

    while (1) {
        // Check enumeration
        if (usbDevice.IsConfigured(&usbDevice)) {
            main_loop_iteration(&usbDevice);
        }
    }
}
