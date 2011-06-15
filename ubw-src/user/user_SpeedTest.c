/*********************************************************************
 *
 *                Microchip USB C18 Firmware Version 1.0
 *
 *********************************************************************
 * FileName:        user.c
 * Dependencies:    See INCLUDES section below
 * Processor:       PIC18
 * Compiler:        C18 2.30.01+
 * Company:         Microchip Technology, Inc.
 *
 * Software License Agreement
 *
 * The software supplied herewith by Microchip Technology Incorporated
 * (the “Company”) for its PICmicro® Microcontroller is intended and
 * supplied to you, the Company’s customer, for use solely and
 * exclusively on Microchip PICmicro Microcontroller products. The
 * software is owned by the Company and/or its supplier, and is
 * protected under applicable copyright laws. All rights are reserved.
 * Any use in violation of the foregoing restrictions may subject the
 * user to criminal sanctions under applicable laws, as well as to
 * civil liability for the breach of the terms and conditions of this
 * license.
 *
 * THIS SOFTWARE IS PROVIDED IN AN “AS IS” CONDITION. NO WARRANTIES,
 * WHETHER EXPRESS, IMPLIED OR STATUTORY, INCLUDING, BUT NOT LIMITED
 * TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE APPLY TO THIS SOFTWARE. THE COMPANY SHALL NOT,
 * IN ANY CIRCUMSTANCES, BE LIABLE FOR SPECIAL, INCIDENTAL OR
 * CONSEQUENTIAL DAMAGES, FOR ANY REASON WHATSOEVER.
 *
 * Author               Date        Comment
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Rawin Rojvanit       11/19/04    Original.
 ********************************************************************/

/******************************************************************************
 * CDC RS-232 Emulation Tutorial Instructions:
 ******************************************************************************
 * Refer to Application Note AN956 for explanation of the CDC class.
 *
 * First take a look at Exercise_Example() and study how functions are called.
 *
 * There are five exercises, each one has a solution in the CDC\user\solutions.
 * Scroll down and look for Exercise_01,_02,_03,_04, and _05.
 * Instructions on what to do is inside each function.
 *
 *****************************************************************************/

/** I N C L U D E S **********************************************************/
#include <p18cxxx.h>
#include <usart.h>
#include "system/typedefs.h"

#include "system/usb/usb.h"

#include "io_cfg.h"             // I/O pin mapping
#include "user/user.h"

/** V A R I A B L E S ********************************************************/
#pragma udata
byte old_sw;

char input_buffer[64];
char output_buffer[64];
char out_ptr;

rom char welcome[]={"PIC18F4550 Full-Speed USB - CDC RS-232 Emulation Demo\r\n\r\n"};
rom char ansi_clrscr[]={"\x1b[2J"};         // ANSI Clear Screen Command

/** P R I V A T E  P R O T O T Y P E S ***************************************/
void BlinkUSBStatus(void);
BOOL SwitchIsPressed(void);

/** D E C L A R A T I O N S **************************************************/
#pragma code
void UserInit(void)
{
    mInitAllLEDs();
    mInitSwitch();
    old_sw = sw;
	// Make all of PORTB an output
	LATB = 0xFF;
	TRISB = 0x00;

}//end UserInit

/******************************************************************************
 * Function:        void ProcessIO(void)
 *
 * PreCondition:    None
 *
 * Input:           None
 *
 * Output:          None
 *
 * Side Effects:    None
 *
 * Overview:        This function is a place holder for other user routines.
 *                  It is a mixture of both USB and non-USB tasks.
 *
 * Note:            None
 *****************************************************************************/
void ProcessIO(void)
{
	char err = 1;
	char bytesIn = 0;
	static char LastPacketByte = 'A';
	char bad = 0;
	char i = 0;

	BlinkUSBStatus();
    // User Application USB tasks
    if((usb_device_state < CONFIGURED_STATE)||(UCONbits.SUSPND==1)) return;

	// Pull in some new data if there is new data to pull in
	bytesIn = getsUSBUSART(input_buffer, 32);

	if (bytesIn > 0)
	{
		// Check to see that all bytes are present
		for (i = 0; i < 30; i++)
		{
			if (input_buffer[i] != (input_buffer[i+1] - 1))
			{
				err = 2;
			}
		}
		if (input_buffer[0] != LastPacketByte + 1)
		{
			err = 3;
		}
		LastPacketByte = input_buffer[0];
		if (bytesIn < 32)
		{
			err = 4;
		}
		if (input_buffer[31] != 13)
		{
			err = 5;
		}

		// And write it out to port B
		LATB = input_buffer[0];
		mLED_2_Toggle();

		// Send our error code back
        if(mUSBUSARTIsTxTrfReady())
        {
			output_buffer[0] = err;
			output_buffer[1] = 0x00;
			putsUSBUSART(output_buffer);
		}
	}

}//end ProcessIO

//void Exercise_Example(void)
//{
//    static byte start_up_state = 0;
//
//    if(start_up_state == 0)
//    {
//        if(Switch2IsPressed())
//            start_up_state++;
//    }
//    else if(start_up_state == 1)
//    {
//        if(mUSBUSARTIsTxTrfReady())
//        {
//            putrsUSBUSART(ansi_clrscr);
//            start_up_state++;
//        }
//    }
//    else if(start_up_state == 2)
//    {
//        if(mUSBUSARTIsTxTrfReady())
//        {
//            putrsUSBUSART("\rMicrochip Technology Inc., 2004\r\n");
//            start_up_state++;
//        }
//    }
//    else if(start_up_state == 3)
//    {
//        if(mUSBUSARTIsTxTrfReady())
//        {
//            putrsUSBUSART(welcome);
//            start_up_state++;
//        }
//    }
//
//}//end Exercise_Example

void Exercise_01(void)
{
    /*
     * Write code in this function that sends a literal null-terminated
     * string of text ("Hello World!\r\n") to the PC when switch 2 is
     * pressed.
     *
     * Useful functions:
     *  Switch2IsPressed() returns '1' when switch 2 is pressed.
     *  putrsUSBUSART(...);
     *
     * See examples in Exercise_Example();
     *
     * Remember, you must check if cdc_trf_state is ready for another
     * transfer or not. When it is ready, the value will equal CDC_TX_READY,
     * or use macro: mUSBUSARTIsTxTrfReady()
     */

    /* Insert code here - 3 lines */

    /* End */

}//end Exercise_01

rom char ex02_string[]={"Type in a string here.\r\n"};
void Exercise_02(void)
{
    /*
     * Write code in this function that sends a null-terminated string
     * of text stored in program memory pointed to by "ex02_string" to
     * the PC when switch 3 is pressed.
     *
     * ex02_string is declared right above this function.
     *
     * Useful functions:
     *  Switch3IsPressed() returns '1' when switch 3 is pressed.
     *  putrsUSBUSART(...);
     *
     * See examples in Exercise_Example();
     *
     * Remember, you must check if cdc_trf_state is ready for another
     * transfer or not. When it is ready, the value will equal CDC_TX_READY,
     * or use macro: mUSBUSARTIsTxTrfReady()
     */

    /* Insert code here - 3 lines*/

    /* End */

}//end Exercise_02

void Exercise_03(void)
{
    /*
     * Write code in this function that reads data from USB and
     * toggles LED D4 when the data read equals ASCII character '1' (0x31)
     *
     * Useful functions:
     *  byte getsUSBUSART(char *buffer, byte len)   See cdc.c for details
     *  mLED_4_Toggle();
     *
     * Use input_buffer[] to store data read from USB.
     */

    /* Insert code here - 3 lines */

    /* End */

}//end Exercise_03

void Exercise_04(void)
{
    /*
     * Before starting Exercise_04(), comment out the call to Exercise_01()
     * in ProcessIO(); This function will need to check Switch2IsPressed().
     *
     * Write code in this function that sends the following 4 bytes of
     * data: 0x30,0x31,0x32,0x33 when switch 2 is pressed.
     * Note that these data is not null-terminated and is located in
     * the data memory.
     *
     * Useful functions:
     *  Switch2IsPressed() returns '1' when switch 2 is pressed.
     *  mUSBUSARTTxRam(byte *pData, byte len) See cdc.h for details.
     *
     * Use output_buffer[] to store the four bytes data.
     *
     * Remember, you must check if cdc_trf_state is ready for another
     * transfer or not. When it is ready, the value will equal CDC_TX_READY,
     * or use macro: mUSBUSARTIsTxTrfReady()
     */

    /* Insert code here - 7 lines */

    /* End */

}//end Exercise_04

/******************************************************************************
 * Function:        void BlinkUSBStatus(void)
 *
 * PreCondition:    None
 *
 * Input:           None
 *
 * Output:          None
 *
 * Side Effects:    None
 *
 * Overview:        BlinkUSBStatus turns on and off LEDs corresponding to
 *                  the USB device state.
 *
 * Note:            mLED macros can be found in io_cfg.h
 *                  usb_device_state is declared in usbmmap.c and is modified
 *                  in usbdrv.c, usbctrltrf.c, and usb9.c
 *****************************************************************************/
void BlinkUSBStatus(void)
{
    static word led_count=0;

    if(led_count == 0)led_count = 10000U;
    led_count--;

    if(UCONbits.SUSPND == 1)
    {
        if(led_count==0)
        {
// It would be nice if this could be a slow blink
            mLED_1_Off();
        }//end if
    }
    else
    {
        if(usb_device_state == DETACHED_STATE)
        {
			mLED_1_Off();
        }
        else if(usb_device_state == ATTACHED_STATE)
        {
            mLED_1_On();
        }
        else if(usb_device_state == POWERED_STATE)
        {
            mLED_1_On();
        }
        else if(usb_device_state == DEFAULT_STATE)
        {
            mLED_1_On();
        }
        else if(usb_device_state == ADDRESS_STATE)
        {
            mLED_1_On();
        }
        else if(usb_device_state == CONFIGURED_STATE)
        {
            if(led_count==0)
            {
				mLED_1_Toggle();
//                mLED_2 = !mLED_1;       // Alternate blink
            }//end if
        }//end if(...)
    }//end if(UCONbits.SUSPND...)

}//end BlinkUSBStatus

BOOL SwitchIsPressed(void)
{
    if(sw != old_sw)
    {
        old_sw = sw;                  	// Save new value
        if(sw == 0)                    	// If pressed
            return TRUE;                // Was pressed
    }//end if
    return FALSE;                       // Was not pressed
}//end SwitchIsPressed

/** EOF user.c ***************************************************************/
