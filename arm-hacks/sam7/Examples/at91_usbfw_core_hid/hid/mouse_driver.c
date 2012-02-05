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
$Id: mouse_driver.c 119 2006-10-17 12:51:47Z jjoannic $
*/

//------------------------------------------------------------------------------
//      Includes
//------------------------------------------------------------------------------

#include "core/common.h"
#include "core/device.h"
#include "core/board.h"
#include "core/trace.h"
#include "core/usb.h"
#include "core/standard.h"
#include "hid.h"
#include "mouse_driver.h"

//------------------------------------------------------------------------------
//      Global variables
//------------------------------------------------------------------------------

// Descriptors
//------------------------------------------------------------------------------
//! \brief  Report descriptor for a 3-buttons mouse
static const unsigned char pReport[] = {

    HID_GLOBAL_USAGE_PAGE + 1, HID_USAGE_PAGE_GENERIC_DESKTOP,
    HID_LOCAL_USAGE + 1, HID_USAGE_MOUSE,
    HID_MAIN_COLLECTION + 1, HID_COLLECTION_APPLICATION,
        HID_LOCAL_USAGE + 1, HID_USAGE_POINTER,
        HID_MAIN_COLLECTION + 1, HID_COLLECTION_PHYSICAL,
              HID_GLOBAL_USAGE_PAGE + 1, HID_USAGE_PAGE_BUTTON,
              HID_LOCAL_USAGE_MINIMUM + 1, 1,
              HID_LOCAL_USAGE_MAXIMUM + 1, 3,
              HID_GLOBAL_LOGICAL_MINIMUM + 1, 0,
              HID_GLOBAL_LOGICAL_MAXIMUM + 1, 1,
              HID_GLOBAL_REPORT_COUNT + 1, 3,
              HID_GLOBAL_REPORT_SIZE + 1, 1,
              HID_MAIN_INPUT + 1, HID_VARIABLE,  // 3 button bits
              HID_GLOBAL_REPORT_COUNT + 1, 1,
              HID_GLOBAL_REPORT_SIZE + 1, 5,
              HID_MAIN_INPUT + 1, HID_CONSTANT,  // 5 bit padding
              HID_GLOBAL_USAGE_PAGE + 1, HID_USAGE_PAGE_GENERIC_DESKTOP,
              HID_LOCAL_USAGE + 1, HID_USAGE_X,
              HID_LOCAL_USAGE + 1, HID_USAGE_Y,
              HID_GLOBAL_LOGICAL_MINIMUM + 1, (unsigned char) -127,
              HID_GLOBAL_LOGICAL_MAXIMUM + 1, 127,
              HID_GLOBAL_REPORT_SIZE + 1, 8,
              HID_GLOBAL_REPORT_COUNT + 1, 2,
              HID_MAIN_INPUT + 1, HID_VARIABLE | HID_RELATIVE,
        HID_MAIN_ENDCOLLECTION,
    HID_MAIN_ENDCOLLECTION
};

//! \brief  Standard USB device descriptor
//! \see    S_usb_device_descriptor
static const S_usb_device_descriptor sDevice = {

    sizeof(S_usb_device_descriptor), // Size of this descriptor in bytes (18)
    USB_DEVICE_DESCRIPTOR,           // DEVICE Descriptor Type
    USB2_00,                         // USB 2.0 specification
    USB_CLASS_DEVICE,                // Class is specified in the interface descriptor.
    0x00,                            // No device subclass code
    0x00,                            // No device protocol code
    USB_ENDPOINT0_MAXPACKETSIZE,     // Maximum packet size for endpoint zero
    USB_VENDOR_ATMEL,                // ATMEL Vendor ID
    MSE_PRODUCT_ID,                  // Product ID (6200h)
    0x0001,                          // Device release number 0.01
    0x01,                            // Index of manufacturer description
    0x02,                            // Index of product description
    0x03,                            // Index of serial number description
    0x01                             // One possible configuration
};

//! \brief  Device configuration descriptor
//! \see    S_mse_configuration_descriptor
static const S_mse_configuration_descriptor sConfiguration = {

    // Standard configuration descriptor
    {
        sizeof(S_usb_configuration_descriptor), // Size of this descriptor in bytes (9)
        USB_CONFIGURATION_DESCRIPTOR,           // CONFIGURATION descriptor type
        sizeof(S_mse_configuration_descriptor), // Total length of data returned for this configuration
        0x01,                                   // One interface is used by this configuration
        0x01,                                   // Value 0x01 is used to select this configuration
        0x00,                                   // No string is used to describe this configuration
        USB_CONFIG_SELF_NOWAKEUP,               // Device is self-powered and does not support remote wakeup
        USB_POWER_MA(100)                       // maximum power consumption in mA
    },
    // First Interface Descriptor
    {
        sizeof(S_usb_interface_descriptor), // Size of this descriptor in bytes (9)
        USB_INTERFACE_DESCRIPTOR,           // INTERFACE Descriptor Type
        0x00,                               // Interface 0
        0x00,                               // No alternate settings
        1,                                  // One endpoint used
        USB_CLASS_HID,                      // Class HID (Human Interface Device)
        HID_BOOT_INTERFACE_SUBCLASS,        // Boot Interface subclass
        HID_PROTOCOL_CODE_MOUSE,            // Interface Protocol Mouse
        0x00                                // No associated string descriptor
    },
    // HID-Specific Descriptor
    {
        sizeof(S_hid_descriptor), // Size of this descriptor in bytes (9)
        HID_DESCRIPTOR,           // HID descriptor type
        HID_1_11,                 // HID Class Specification 0x101
        0x00,                     // CountryCode
        0x01,                     // 1 HID class descriptor
        HID_REPORT_DESCRIPTOR,    // Report descriptor type
        sizeof(pReport)           // Total length of Report descriptor
    },
    // Endpoint Descriptor
    {
        sizeof(S_usb_endpoint_descriptor),  // Size of this descriptor in bytes
        USB_ENDPOINT_DESCRIPTOR,            // ENDPOINT descriptor type
        MSE_INTERRUPT_IN | USB_ENDPOINT_IN, // Endpoint number 01h, direction IN
        ENDPOINT_TYPE_INTERRUPT,            // Interrupt endpoint
        MSE_REPORT_SIZE,                    // Maximum packet size is 3 bytes
        10                                  // Interval for polling: 10ms
    }
};

//! \brief  Language ID string descriptor
static const S_usb_language_id sLanguageID = {

    USB_STRING_DESCRIPTOR_SIZE(1),
    USB_STRING_DESCRIPTOR,
    USB_LANGUAGE_ENGLISH_US
};

//! \brief  Manufacturer string descriptor
static const char pManufacturer[] = {

    USB_STRING_DESCRIPTOR_SIZE(5),
    USB_STRING_DESCRIPTOR,
    USB_UNICODE('A'),
    USB_UNICODE('T'),
    USB_UNICODE('M'),
    USB_UNICODE('E'),
    USB_UNICODE('L')
};

//! \brief  Product string descriptor
static const char pProduct[] = {

    USB_STRING_DESCRIPTOR_SIZE(20),
    USB_STRING_DESCRIPTOR,
    USB_UNICODE('A'),
    USB_UNICODE('T'),
    USB_UNICODE('M'),
    USB_UNICODE('E'),
    USB_UNICODE('L'),
    USB_UNICODE(' '),
    USB_UNICODE('A'),
    USB_UNICODE('T'),
    USB_UNICODE('9'),
    USB_UNICODE('1'),
    USB_UNICODE(' '),
    USB_UNICODE('H'),
    USB_UNICODE('I'),
    USB_UNICODE('D'),
    USB_UNICODE(' '),
    USB_UNICODE('M'),
    USB_UNICODE('O'),
    USB_UNICODE('U'),
    USB_UNICODE('S'),
    USB_UNICODE('E'),
};

//! \brief  Serial number string descriptor
static const char pSerialNumber[] = {

    USB_STRING_DESCRIPTOR_SIZE(12),
    USB_STRING_DESCRIPTOR,
    USB_UNICODE('0'),
    USB_UNICODE('1'),
    USB_UNICODE('2'),
    USB_UNICODE('3'),
    USB_UNICODE('4'),
    USB_UNICODE('5'),
    USB_UNICODE('6'),
    USB_UNICODE('7'),
    USB_UNICODE('8'),
    USB_UNICODE('9'),
    USB_UNICODE('A'),
    USB_UNICODE('F')
};

//! \brief  List of string descriptors
static const char *pStrings[] = {

    (char *) &sLanguageID,
    pManufacturer,
    pProduct,
    pSerialNumber
};

//! \brief  List of endpoint descriptors
static const S_usb_endpoint_descriptor *pEndpoints[] = {

    &(sConfiguration.sInterruptIn)
};

//! \brief  Standard descriptors list
static const S_std_descriptors sDescriptors = {

    &sDevice,
    (S_usb_configuration_descriptor *) &sConfiguration,
    pStrings,
    pEndpoints
};

//------------------------------------------------------------------------------
//      Exported functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \brief  Handles standard and class-specific SETUP requests.
//!
//! \param  pMse Pointer to a S_mse instance
//! \see    S_mse
//------------------------------------------------------------------------------
void MSE_RequestHandler(S_mse *pMse)
{
    const S_usb   *pUsb = pMse->sClass.pUsb;
    S_usb_request *pSetup = USB_GetSetup(pUsb);

    TRACE_DEBUG_M("NewReq ");

    // Handle request
    switch (pSetup->bRequest) {
    //----------------------
    case USB_GET_DESCRIPTOR:
    //----------------------
        // Identify requested descriptor
        switch (HBYTE(pSetup->wValue)) {
        //-------------------------
        case HID_REPORT_DESCRIPTOR:
        //-------------------------
            // Send report descriptor
            TRACE_DEBUG_M("Report ");
            USB_Write(pUsb,
                      0,
                      pReport,
                      min(sizeof(pReport), pSetup->wLength),
                      0,
                      0);
            break;

        //------------------
        case HID_DESCRIPTOR:
        //------------------
            // Send HID descriptor
            TRACE_DEBUG_M("HID ");

            USB_Write(pUsb,
                      0,
                      (void *) &sConfiguration.sHID,
                      min(sizeof(S_hid_descriptor), pSetup->wLength),
                      0,
                      0);
            break;

        //------
        default:
        //------
            // Forward request to standard handler
            STD_RequestHandler(&(pMse->sClass));
            break;
        }
        break;

    //--------------------
    case HID_GET_PROTOCOL:
    //--------------------
        // Return the protocol currently in use
        TRACE_DEBUG_M("gProtocol ");
        USB_Write(pUsb,
                  0,
                  &(pMse->bProtocol),
                  1,
                  0,
                  0);
        break;

    //--------------------
    case HID_SET_PROTOCOL:
    //--------------------
        // Set new protocol value
        TRACE_DEBUG_M("SetProtocol ");

        pMse->bProtocol = (unsigned char) pSetup->wValue;
        USB_SendZLP0(pUsb, 0, 0);
    break;

    //----------------
    case HID_GET_IDLE:
    //----------------
        // Return current Idle rate
        TRACE_DEBUG_M("gIdle ");

        USB_Write(pUsb, 0, &(pMse->bIdleRate), 1, 0, 0);
        break;

    //----------------
    case HID_SET_IDLE:
    //----------------
        // Get new Idle rate
        pMse->bIdleRate = HBYTE(pSetup->wValue);
        TRACE_DEBUG_M("sIdle(%d) ", pMse->bIdleRate);

        USB_SendZLP0(pUsb, 0, 0);
        break;

    //------------------
    case HID_GET_REPORT:
    //------------------
        // Send the current report to the host
        TRACE_DEBUG_M("gReport ");

        // Check report type
        if (HBYTE(pSetup->wValue) == HID_INPUT_REPORT) {

            // Send report
            TRACE_DEBUG_M("Input ");
            USB_Write(pUsb, 0, &(pMse->sReport), MSE_REPORT_SIZE, 0, 0);
        }
        else {

            // STALL endpoint
            USB_Stall(pUsb, 0);
        }
        break;

    //------
    default:
    //------
        // Forward request to standard handler
        STD_RequestHandler(&(pMse->sClass));
    }
}

//------------------------------------------------------------------------------
//! \brief  Initializes a HID mouse driver
//!
//!         This method sets the standard descriptors of the device and the
//!         default HID mouse configuration.
//! \param  pMse Pointer to a S_mse instance
//! \param  pUsb Pointer to the S_usb driver instance to use
//! \see    S_mse
//! \see    S_usb
//------------------------------------------------------------------------------
void MSE_Init(S_mse *pMse, const S_usb *pUsb)
{
    // Initialize mouse driver
    pMse->bIdleRate = 0;
    pMse->bProtocol = HID_REPORT_PROTOCOL;
    pMse->sReport.bButtons = 0;
    pMse->sReport.bX = 0;
    pMse->sReport.bY = 0;

    // Initialize standard class attributes
    pMse->sClass.pUsb = pUsb;
    pMse->sClass.pDescriptors = &sDescriptors;

    // Initialize the USB driver
    USB_Init(pUsb);
}

//------------------------------------------------------------------------------
//! \brief  Sends the current mouse report to the host, through the interrupt IN
//!         endpoint
//! \param  pMSe      Pointer to a S_mse instance
//! \param  fCallback Optional callback function to invoke when the report is
//!                   sent
//! \param  pArgument Optional argument to pass to the callback function
//------------------------------------------------------------------------------
unsigned char MSE_SendReport(const S_mse *pMse,
                             Callback_f  fCallback,
                             void        *pArgument)
{
    return USB_Write(pMse->sClass.pUsb, MSE_INTERRUPT_IN, &(pMse->sReport),
                     MSE_REPORT_SIZE, fCallback, pArgument);
}
