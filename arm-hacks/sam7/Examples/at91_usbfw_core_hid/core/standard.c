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
$Id: standard.c 122 2006-10-17 12:56:03Z jjoannic $
*/

//------------------------------------------------------------------------------
//      Includes
//------------------------------------------------------------------------------

#include "common.h"
#include "device.h"
#include "board.h"
#include "trace.h"
#include "usb.h"
#include "standard.h"

//------------------------------------------------------------------------------
//      Internal functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief  Callback for the STD_SetConfiguration function.
//
//         Configures the device and the endpoints
// \param  pClass Pointer to a class driver instance
//------------------------------------------------------------------------------
static void STD_ConfigureEndpoints(const S_std_class *pClass)
{
    unsigned char i;

    // Enter the Configured state
    USB_SetConfiguration(pClass->pUsb);

    // Configure endpoints
    for (i = 0; i < (pClass->pUsb->dNumEndpoints-1); i++) {

        USB_ConfigureEndpoint(pClass->pUsb,
                              pClass->pDescriptors->pEndpoints[i]);
    }
}

//------------------------------------------------------------------------------
// \brief  Sends a zero-length packet and starts the configuration procedure.
// \param  pClass          Pointer to a class driver instance
// \param  bConfiguration  Newly selected configuration
//------------------------------------------------------------------------------
static void STD_SetConfiguration(S_std_class   *pClass,
                                 unsigned char bConfiguration)
{
    USB_SendZLP0(pClass->pUsb,
                 (Callback_f) STD_ConfigureEndpoints,
                 pClass);
}

//------------------------------------------------------------------------------
// \brief  Sends the currently selected configuration to the host.
// \param  pClass Pointer to a class driver instance
//------------------------------------------------------------------------------
static void STD_GetConfiguration(S_std_class *pClass)
{
    if (ISSET(USB_GetState(pClass->pUsb), USB_STATE_CONFIGURED)) {

        pClass->wData = 1;
    }
    else {

        pClass->wData = 0;
    }

    USB_Write(pClass->pUsb, 0, &(pClass->wData), 1, 0, 0);
}

//------------------------------------------------------------------------------
// \brief  Sends the current device status to the host.
// \param  pClass Pointer to a class driver interface
//------------------------------------------------------------------------------
static void STD_GetDeviceStatus(S_std_class *pClass)
{
    // Bus or self-powered ?
    if (ISSET(pClass->pDescriptors->pConfiguration->bmAttibutes,
              USB_CONFIG_SELF_NOWAKEUP)) {

        // Self powered device
        pClass->wDeviceStatus |= SELF_POWERED;
    }
    else {

        // Bus powered device
        pClass->wDeviceStatus &= ~SELF_POWERED;
    }

    // Return the device status
    USB_Write(pClass->pUsb, 0, &(pClass->wDeviceStatus), 2, 0, 0);
}

//------------------------------------------------------------------------------
// \brief  Sends the current status of specified endpoint to the host.
// \param  pClass    Pointer to a class driver instance
// \param  bEndpoint Endpoint number
//------------------------------------------------------------------------------
static void STD_GetEndpointStatus(S_std_class   *pClass,
                                  unsigned char bEndpoint)
{
    //! Retrieve the endpoint current status
    pClass->wData = (unsigned short) USB_Halt(pClass->pUsb,
                                              bEndpoint,
                                              USB_GET_STATUS);

    //! Return the endpoint status
    USB_Write(pClass->pUsb, 0, &(pClass->wData), 2, 0, 0);
}

//------------------------------------------------------------------------------
// \brief  Sends the device descriptor to the host.
//
//         The number of bytes actually sent depends on both the length
//         requested by the host and the actual length of the descriptor.
// \param  pClass  Pointer to a class driver instance
// \param  wLength Number of bytes requested by the host
//------------------------------------------------------------------------------
static void STD_GetDeviceDescriptor(const S_std_class *pClass,
                                    unsigned short    wLength)
{
    USB_Write(pClass->pUsb,
              0,
              (void *) pClass->pDescriptors->pDevice,
              min(sizeof(S_usb_device_descriptor), wLength),
              0,
              0);
}

//------------------------------------------------------------------------------
// \brief  Sends the configuration descriptor to the host.
//
//         The number of bytes actually sent depends on both the length
//         requested by the host and the actual length of the descriptor.
// \param  pClass  Pointer to a class driver instance
// \param  wLength Number of bytes requested by the host
//------------------------------------------------------------------------------
static void STD_GetConfigurationDescriptor(const S_std_class *pClass,
                                           unsigned short    wLength)
{
    USB_Write(pClass->pUsb,
              0,
              (void *) pClass->pDescriptors->pConfiguration,
              min(pClass->pDescriptors->pConfiguration->wTotalLength,
                  wLength),
              0,
              0);
}

#if defined(HIGHSPEED)
//------------------------------------------------------------------------------
// \brief  Sends the qualifier descriptor to the host.
//
//         The number of bytes actually sent depends on both the length
//         requested by the host and the actual length of the descriptor.
// \param  pClass  Pointer to a class driver instance
// \param  wLength Number of bytes requested by the host
//------------------------------------------------------------------------------
static void STD_GetQualifierDescriptor(const S_std_class *pClass,
                                       unsigned short    wLength)
{
    USB_Write(pClass->pUsb,
              0,
              (void *) pClass->pDescriptors->pQualifier,
              min(pClass->pDescriptors->pQualifier->bLength, wLength),
              0,
              0);
}

//------------------------------------------------------------------------------
// \brief  Sends the other speed configuration descriptor to the host.
//
//         The number of bytes actually sent depends on both the length
//         requested by the host and the actual length of the descriptor.
// \param  pClass  Pointer to a class driver instance
// \param  wLength Number of bytes requested by the host
//------------------------------------------------------------------------------
static void STD_GetOSCDescriptor(const S_std_class *pClass,
                                 unsigned short    wLength)
{
    USB_Write(pClass->pUsb,
              0,
              (void *) pClass->pDescriptors->pOtherSpeedConfiguration,
              min(pClass->pDescriptors->pOtherSpeedConfiguration->wTotalLength,
                  wLength),
              0,
              0);
}
#endif

//------------------------------------------------------------------------------
// \brief  Sends the specified string descriptor to the host
//
//         The number of bytes actually sent depends on both the length
//         requested by the host and the actual length of the descriptor.
// \param  pClass  Pointer to a class driver instance
// \param  wLength Number of bytes requested by the host
// \param  wIndex  Index of requested string descriptor
//------------------------------------------------------------------------------
static void STD_GetStringDescriptor(const S_std_class *pClass,
                                    unsigned short    wLength,
                                    unsigned char     bIndex)
{
    USB_Write(pClass->pUsb,
              0,
              (void *) pClass->pDescriptors->pStrings[bIndex],
              min( *(pClass->pDescriptors->pStrings[bIndex]), wLength),
              0,
              0);
}

//------------------------------------------------------------------------------
//      Exported functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \ingroup usb_std_req_hlr
//! \brief   Handles standard SETUP requests
//! \param   pClass Pointer to a class driver instance
//------------------------------------------------------------------------------
void STD_RequestHandler(S_std_class *pClass)
{
    S_usb_request *pSetup = USB_GetSetup(pClass->pUsb);

    TRACE_DEBUG_M("Std ");

    // Handle incoming request
    switch (pSetup->bRequest) {
    //----------------------
    case USB_GET_DESCRIPTOR:
    //----------------------
        TRACE_DEBUG_M("gDesc ");

        // The HBYTE macro returns the upper byte of a word
        switch (HBYTE(pSetup->wValue)) {
        //-------------------------
        case USB_DEVICE_DESCRIPTOR:
        //-------------------------
            TRACE_DEBUG_M("Dev ");
            STD_GetDeviceDescriptor(pClass, pSetup->wLength);
            break;

        //--------------------------------
        case USB_CONFIGURATION_DESCRIPTOR:
        //--------------------------------
            TRACE_DEBUG_M("Cfg ");
            STD_GetConfigurationDescriptor(pClass, pSetup->wLength);
            break;

#if defined(HIGHSPEED)
        //-----------------------------------
        case USB_DEVICE_QUALIFIER_DESCRIPTOR:
        //-----------------------------------
            TRACE_DEBUG_M("Qua ");
            STD_GetQualifierDescriptor(pClass, pSetup->wLength);
            break;

        //--------------------------------------------
        case USB_OTHER_SPEED_CONFIGURATION_DESCRIPTOR:
        //--------------------------------------------
            TRACE_DEBUG_M("OSC ");
            STD_GetOSCDescriptor(pClass, pSetup->wLength);
            break;
#endif
        //-------------------------
        case USB_STRING_DESCRIPTOR:
        //-------------------------
            TRACE_DEBUG_M("Str%d ", LBYTE(pSetup->wValue));
            STD_GetStringDescriptor(pClass,
                                    pSetup->wLength,
                                    LBYTE(pSetup->wValue));
            break;

        //------
        default:
        //------
            TRACE_WARNING(
                "W: STD_RequestHandler: Unknown GetDescriptor = 0x%02X\n\r",
                pSetup->bRequest
            );
            USB_Stall(pClass->pUsb, 0);

        }
        break;

    //-------------------
    case USB_SET_ADDRESS:
    //-------------------
        TRACE_DEBUG_M("sAddr ");
        USB_SendZLP0(pClass->pUsb,
                     (Callback_f) USB_SetAddress,
                     (void *) pClass->pUsb);
        break;

    //-------------------------
    case USB_SET_CONFIGURATION:
    //-------------------------
        TRACE_DEBUG_M("sCfg ");
        STD_SetConfiguration(pClass, (char) pSetup->wValue);
        break;

    //-------------------------
    case USB_GET_CONFIGURATION:
    //-------------------------
        TRACE_DEBUG_M("gCfg ");
        STD_GetConfiguration(pClass);
        break;

    //---------------------
    case USB_CLEAR_FEATURE:
    //---------------------
        TRACE_DEBUG_M("cFeat ");

        switch (pSetup->wValue) {
            //---------------------
            case USB_ENDPOINT_HALT:
            //---------------------
                TRACE_DEBUG_M("Hlt ");
                USB_Halt(pClass->pUsb, LBYTE(pSetup->wIndex), USB_CLEAR_FEATURE);
                USB_SendZLP0(pClass->pUsb, 0, 0);
                break;

            //----------------------------
            case USB_DEVICE_REMOTE_WAKEUP:
            //----------------------------
                TRACE_DEBUG_M("RmWak ");
                pClass->wDeviceStatus &= ~REMOTE_WAKEUP; // Remote wakeup disabled
                USB_SendZLP0(pClass->pUsb, 0, 0);
                break;

            //------
            default:
            //------
                TRACE_DEBUG_H("Sta ");
                USB_Stall(pClass->pUsb, 0);

        }
        break;

    //------------------
    case USB_GET_STATUS:
    //------------------
        TRACE_DEBUG_H("gSta ");

        switch (USB_REQUEST_RECIPIENT(pSetup)) {
        //-------------------------
        case USB_RECIPIENT_DEVICE:
        //-------------------------
            TRACE_DEBUG_M("Dev ");
            STD_GetDeviceStatus(pClass);
            break;

        //---------------------------
        case USB_RECIPIENT_ENDPOINT:
        //---------------------------
            TRACE_DEBUG_M("Ept ");
            STD_GetEndpointStatus(pClass,
                                  LBYTE(pSetup->wIndex));
            break;

        //------
        default:
        //------
            TRACE_WARNING(
                "W: STD_RequestHandler: Unsupported GetStatus = 0x%02X\n\r",
                pSetup->bmRequestType
            );
            USB_Stall(pClass->pUsb, 0);

        }
        break;

    //-------------------
    case USB_SET_FEATURE:
    //-------------------
        TRACE_DEBUG_H("sFeat ");

        switch (pSetup->wValue) {
        //---------------------
        case USB_ENDPOINT_HALT:
        //---------------------
            USB_Halt(pClass->pUsb, LBYTE(pSetup->wIndex), USB_SET_FEATURE);
            USB_SendZLP0(pClass->pUsb, 0, 0);
            break;

        //----------------------------
        case USB_DEVICE_REMOTE_WAKEUP:
        //----------------------------
            pClass->wDeviceStatus |= REMOTE_WAKEUP; // Remote wakeup enabled
            USB_SendZLP0(pClass->pUsb, 0, 0);
            break;

        //------
        default:
        //------
            TRACE_WARNING(
                "W: STD_RequestHandler: Unsupported SetFeature=0x%04X\n\r",
                pSetup->wValue
            );
            USB_Stall(pClass->pUsb, 0);

        }
        break;

    //------
    default:
    //------
        TRACE_WARNING(
            "W: STD_RequestHandler: Unsupported request: 0x%02X\n\r",
            pSetup->bRequest
        );
        USB_Stall(pClass->pUsb, 0);
    }
}


