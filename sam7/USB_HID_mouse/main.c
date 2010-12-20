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
#include "dbgu.h"
#include "usb_descriptors.h"
#include "hid_enumerate.h"

#define BUTTON_SAMPLING 1700 // Sampling frequency of buttons

#define BL_OFF(pio) ((pio) & SW1)
#define BR_OFF(pio) ((pio) & SW2)

#define BL_ON(pio) (!BL_OFF(pio))
#define BR_ON(pio) (!BR_OFF(pio))
#define CLICKL_ON 1
#define CLICKR_ON 2

struct _AT91S_HID   HID;

/*
 * The HID report for this USB mouse is three bytes.
 * Byte 0 = Buttons (3 bits used, 5 are filler)
 * Byte 1 = X       (signed twos-complement)
 * Byte 2 = Y       (signed twos-complement)
 *
 * This must agree with what's in the HID report definition in usb_descriptors.c.
 */
#define HID_REPORT_SIZE 3

//*--------------------------------------------------------------------------------------
//* Function Name       : main
//* Object              :
//*--------------------------------------------------------------------------------------

int main ( void )
{
    char buttons, prevButtons = 0;
    int x = 0;
    unsigned int pioStatus;

    //Init trace DBGU
    //AT91F_DBGU_Init();
    //AT91F_DBGU_Printk("\n\r-I- BasicUSB 1.1 (USB_DP_PUP) \n\r0) Set Pull-UP 1) Clear Pull UP\n\r");

    // Init USB device
    LowLevelInit();
    usb_init();
    // CDC Open by structure initialization
    AT91F_HID_Open(&HID, AT91C_BASE_UDP);
    led_init();

    // Configure the RTT:
    *AT91C_RTTC_RTMR = BUTTON_SAMPLING;

    // Set in PIO mode and Configure in Input
    AT91F_PIOA_CfgPMC();
    AT91F_PIO_CfgInput(AT91C_BASE_PIOA, (SW1|SW2));

    // Wait for the end of enumeration
    while (!HID.IsConfigured(&HID));

    // Start waiting some cmd
    while (1) {
        // Check enumeration
        if (HID.IsConfigured(&HID)) {
            // Wait for several ms
            while ( !((*AT91C_RTTC_RTSR) & AT91C_RTTC_RTTINC) );
            // Check PIO changes
            pioStatus = *AT91C_PIOA_PDSR;
            // Click on Button Left
            if (BL_ON(pioStatus) && BR_OFF(pioStatus)) {
                x = (x > 0) ? -1 : ((-127 < x) ? --x : x);
                AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED2);
                AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1);
            }
            // Click on Button Right
            else if (BL_OFF(pioStatus) && BR_ON(pioStatus)) {
                x = (x < 0) ? 0 : ((x < 127) ? ++x : x);
                AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1);
                AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED2);
            }
            // Click on Both --> left mouse click
            else if (BL_ON(pioStatus) && BR_ON(pioStatus)) {
                buttons |= CLICKL_ON;
                x = 0;
                AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
            }
            // Release both --> release left mouse click
            else {
                buttons &= ~(CLICKL_ON);
                x = 0;
                AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
            }
            // Send Coordinates
            if (prevButtons != buttons || x != 0) {
                char report[HID_REPORT_SIZE] = { buttons, x, 0 };
                prevButtons = buttons;
                HID.SendReport(&HID, report, HID_REPORT_SIZE);
            }
        }
    }
}
