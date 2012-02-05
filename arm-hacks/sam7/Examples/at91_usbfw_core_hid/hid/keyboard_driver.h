/* ----------------------------------------------------------------------------
 *         ATMEL Microcontroller Software Support  -  ROUSSET  -
 * ----------------------------------------------------------------------------
 * Copyright (c) 2006, Atmel Corporation

 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the disclaiimer below.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the disclaimer below in the documentation and/or
 * other materials provided with the distribution.
 *
 * Atmel's name may not be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * DISCLAIMER: THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE
 * DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ----------------------------------------------------------------------------
 */

/*
$Id: keyboard_driver.h 119 2006-10-17 12:51:47Z jjoannic $
*/

#ifndef _KEYBOARD_DRIVER_H
#define _KEYBOARD_DRIVER_H

#ifdef __GNUC__
#define KEYDRV_H_ATTPACKPRE
#define KEYDRV_H_ATTPACKSUF __attribute__((__packed__))
#else
#define KEYDRV_H_ATTPACKPRE __packed
#define KEYDRV_H_ATTPACKSUF
#endif

//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

//! \brief  Number of endpoint used by the HID keyboard driver
#define KBD_NUM_ENDPOINTS               3

//! \brief  Address of the Interrupt-IN endpoint used by the driver
#define KBD_INTERRUPT_IN                0x01

//! \brief  Address of the Interrupt-OUT endpoint used by the driver
#define KBD_INTERRUPT_OUT               0x02

//! \brief  Product ID
#define KBD_PRODUCT_ID                  0x6201

//! \brief  Size of the report emitted by the keyboard driver
#define KBD_INPUT_REPORT_SIZE           4

//! \brief  Size of the report expected by the keyboard driver
#define KBD_OUTPUT_REPORT_SIZE          1

//------------------------------------------------------------------------------
//      Structures
//------------------------------------------------------------------------------
//! Keyboard driver configuration descriptor
KEYDRV_H_ATTPACKPRE typedef struct {

    S_usb_configuration_descriptor sConfiguration;
    S_usb_interface_descriptor     sInterface;
    S_hid_descriptor               sHID;
    S_usb_endpoint_descriptor      sInterruptIn;
    S_usb_endpoint_descriptor      sInterruptOut;

} KEYDRV_H_ATTPACKSUF S_kbd_configuration_descriptor;

//! \brief  HID input report structure used by the keyboard driver to notify the
//!         host of pressed keys.
//!
//!         The first byte is used to report the state of modifier keys. The
//!         other three contains the keycodes of the currently pressed keys.
typedef struct {

    unsigned char bmModifierKeys:8; //!< State of modifier keys
    unsigned char pPressedKeys[3];  //!< Codes of pressed keys

} S_kbd_input_report;

//! \brief  HID output report structure used by the host to control the state of
//!         the keyboard LEDs.
//!
//!         Only the first three bits are relevant, the other 5 are used as
//!         padding bits.
typedef struct {

    unsigned char bmLeds:3,   //!< State of the three keyboard leds
                  bPadding:5; //!< Padding bits

} S_kbd_output_report;

//! \brief  HID keyboard class driver structure
typedef struct {

    S_std_class         sClass;        //!< Standard driver
    S_std_descriptors   sDescriptors;  //!< Descriptor list
    unsigned short      bIdleRate;     //!< Currently set Idle rate
    S_kbd_input_report  sInputReport;  //!< Current input report
    S_kbd_output_report sOutputReport; //!< Current output report
    //! Callback invoked when a new report is received via a SET_REPORT request
    Callback_f          fReportReceivedCallback;

} S_kbd;

//------------------------------------------------------------------------------
//      Exported functions
//------------------------------------------------------------------------------

extern void KBD_Init(S_kbd *pKbd, const S_usb *pUsb, Callback_f fCallback);
extern void KBD_RequestHandler(S_kbd *pKbd);
extern unsigned char KBD_SendReport(const S_kbd *pKbd,
                                    Callback_f  fCallback,
                                    void        *pArgument);
extern unsigned char KBD_ReceiveReport(S_kbd *pKbd,
                                       Callback_f  fCallback,
                                       void        *pArgument);

#endif // defined(_KEYBOARD_DRIVER_H)

