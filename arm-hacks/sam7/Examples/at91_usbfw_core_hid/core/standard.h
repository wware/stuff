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
$Id: standard.h 108 2006-10-16 08:33:33Z jjoannic $
*/

#ifndef _STANDARD_H
#define _STANDARD_H

//! \brief Useful constants wDeviceStatus
//! \see S_std_class
//! \see usb_20.pdf - Section 9.4.5 - Figure 9-4
//! @{

//! \name wDeviceStatus
//! Information Returned by a GetStatus() Request to a Device.
//! @{

//! \brief 
#define SELF_POWERED              (1<<0)

//! \brief 
#define REMOTE_WAKEUP             (1<<1)

//! @}
//! @}

//------------------------------------------------------------------------------
//      Structures
//------------------------------------------------------------------------------

//! \ingroup usb_std_req_hlr
//! \brief   List of standard descriptors used by the device
typedef struct  {

    //! Device descriptor
    const S_usb_device_descriptor           *pDevice;
    //! Configuration descriptor
    const S_usb_configuration_descriptor    *pConfiguration;
    //! List of string descriptors
    const char                              **pStrings;
    //! List of endpoint descriptors
    const S_usb_endpoint_descriptor         **pEndpoints;
#if defined(HIGHSPEED)
    //! Qualifier descriptor (high-speed only)
    const S_usb_device_qualifier_descriptor *pQualifier;
    //! Other speed configuration descriptor (high-speed only)
    const S_usb_configuration_descriptor    *pOtherSpeedConfiguration;
#endif

} S_std_descriptors;

//! \ingroup usb_std_req_hlr
//! \brief   Standard USB class driver structure.
//!
//!          Used to provide standard driver information so external modules can
//!          still access an internal driver.
typedef struct {

    //! Pointer to a S_usb instance
    const S_usb             *pUsb;
    //! Pointer to the list of descriptors used by the device
    const S_std_descriptors *pDescriptors;
    //! Data buffer used for information returned by a GetStatus() request to
    //! a Device (Figure 9-4. in usb_20.pdf)
    unsigned short           wDeviceStatus;
    //! Data buffer
    unsigned short           wData;

} S_std_class;

//------------------------------------------------------------------------------
//      Exported symbols
//------------------------------------------------------------------------------

extern void STD_RequestHandler(S_std_class *pClass);

#endif // _STANDARD_H
