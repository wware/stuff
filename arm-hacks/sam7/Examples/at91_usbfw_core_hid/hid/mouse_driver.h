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
$Id: mouse_driver.h 119 2006-10-17 12:51:47Z jjoannic $
*/

#ifndef _MOUSE_DRIVER_H
#define _MOUSE_DRIVER_H

#ifdef __GNUC__
#define MOUSE_H_ATTPACKPRE
#define MOUSE_H_ATTPACKSUF __attribute__((__packed__))
#else
#define MOUSE_H_ATTPACKPRE __packed
#define MOUSE_H_ATTPACKSUF
#endif


//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

//! \brief  Number of endpoint used by the HID mouse driver
#define MSE_NUM_ENDPOINTS               2

//! \brief  Address of the Interrupt-IN endpoint used by the driver
#define MSE_INTERRUPT_IN                0x01

//! \brief  Product ID expected by the host driver
#define MSE_PRODUCT_ID                  0x6200

//! \brief  Size of the report emitted by the mouse driver
#define MSE_REPORT_SIZE                 3

//! \brief  Left mouse button flag
#define MSE_LEFT_BUTTON                 (1 << 0)

//! \brief  Right mouse button flag
#define MSE_RIGHT_BUTTON                (1 << 1)

//! \brief  Middle mouse button flag
#define MSE_MIDDLE_BUTTON               (1 << 2)

//------------------------------------------------------------------------------
//      Structures
//------------------------------------------------------------------------------
//! Mouse driver configuration descriptor
MOUSE_H_ATTPACKPRE typedef struct {

    S_usb_configuration_descriptor sConfiguration;
    S_usb_interface_descriptor     sInterface;
    S_hid_descriptor               sHID;
    S_usb_endpoint_descriptor      sInterruptIn;

} MOUSE_H_ATTPACKSUF S_mse_configuration_descriptor;

//! \brief  HID report structure used by the mouse driver
//!
//!         The first byte is used to store the state of the 3 mouse buttons,
//!         while the other two indicate the pointer displacement along the X
//!         and Y axis.
typedef struct {

    unsigned char bButtons:8; //!< State (on/off) of the three buttons
    signed char   bX:8;       //!< Displacement on the X-axis
    signed char   bY:8;       //!< Displacement on the Y-axis

} S_mse_report;

//! \brief  HID Mouse class driver structure
typedef struct {

    S_std_class       sClass;       //< Standard driver
    S_std_descriptors sDescriptors; //< Descriptor list
    unsigned char     bIdleRate;    //< Currently set Idle rate
    unsigned char     bProtocol;    //< Currently used protocol
    S_mse_report      sReport;      //< Current report

} S_mse;

//------------------------------------------------------------------------------
//      Exported functions
//------------------------------------------------------------------------------

extern void MSE_Init(S_mse *pMse, const S_usb *pUsb);
extern void MSE_RequestHandler(S_mse *pMse);
extern unsigned char MSE_SendReport(const S_mse *pMse,
                                    Callback_f  fCallback,
                                    void        *pArgument);

#endif // defined(_MOUSE_DRIVER_H)
