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
$Id: mouse_example.c 119 2006-10-17 12:51:47Z jjoannic $
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
//      Definitions
//------------------------------------------------------------------------------

#define SW_UP               SWITCH1
#define SW_LEFT             SWITCH2
#define SW_RIGHT            SWITCH3
#define SW_DOWN             SWITCH4

#define SW_RIGHTCLICK       (SW_UP | SW_DOWN)
#define SW_LEFTCLICK        (SW_LEFT | SW_RIGHT)

#define SPEED_X             4
#define SPEED_Y             4

//------------------------------------------------------------------------------
//      Prototypes
//------------------------------------------------------------------------------

//! \brief  Initialization callback
static void CBK_Init(const S_usb *pUsb);

//! \brief  Suspend callback
static void CBK_Suspend(const S_usb *pUsb);

//! \brief  Resume callback
static void CBK_Resume(const S_usb *pUsb);

//! \brief  New request callback
static void CBK_NewRequest(const S_usb *pUsb);

//------------------------------------------------------------------------------
//      Internal variables
//------------------------------------------------------------------------------

//! \brief  List of endpoints (including endpoint 0) used by the device.
//! \see    S_usb_endpoint
static S_usb_endpoint pEndpoints[] = {

    USB_ENDPOINT_SINGLEBANK, // Control endpoint 0
    USB_ENDPOINT_SINGLEBANK, // Interrupt IN endpoint
};

//! \brief  Variable used to store the last received SETUP packet.
//! \see    S_usb_request
//! \see    S_usb
static S_usb_request sSetup;

//! \brief  Variable used to store the current device state
//! \see    S_usb
static unsigned int dState;

//! \brief  List of implemented callbacks
//! \see    S_usb_callbacks
//! \see    S_usb
static const S_usb_callbacks sCallbacks = {

    CBK_Init,
    0, // CBK_Reset
    CBK_Suspend,
    CBK_Resume,
    CBK_NewRequest,
    0  // CBK_SOF
};

//! \brief  USB driver instance
//! \see    S_usb
static const S_usb sUsb = {

    &sDefaultDriver,
    pEndpoints,
    MSE_NUM_ENDPOINTS,
    &sCallbacks,
    &sSetup,
    &dState
};

//! \brief  HID mouse class driver instance
//! \see    S_mse
static S_mse sMse;

//------------------------------------------------------------------------------
//      Internal Functions
//------------------------------------------------------------------------------

// Interrupt Service Routines
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \brief  Handler for the USB controller interrupt
//!
//!         Defers the call to the USB_Handler function.
//------------------------------------------------------------------------------
void ISR_Driver(void)
{
    USB_Handler(&sUsb);
}

//------------------------------------------------------------------------------
//! \brief  Handler for the VBus state change interrupt
//!
//!         This method calls the USB_Attach function to perform the necessary
//!         operations.
//------------------------------------------------------------------------------
#if !defined(USB_BUS_POWERED)
void ISR_VBus(void)
{
    USB_Attach(&sUsb);

    // Acknowledge the interrupt
    AT91F_PIO_GetInterruptStatus(AT91C_PIO_VBUS);
}
#endif // !defined(USB_BUS_POWERED)

// Callbacks
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \brief  Callback invoked during the initialization of the USB driver
//!
//!         Configures and enables USB controller and VBus monitoring interrupts
//! \param  pUsb    Pointer to a S_usb instance
//------------------------------------------------------------------------------
static void CBK_Init(const S_usb *pUsb)
{
    // Configure and enable the USB controller interrupt
    AT91F_AIC_ConfigureIt(AT91C_BASE_AIC,
                          USB_GetDriverID(pUsb),
                          AT91C_AIC_PRIOR_LOWEST,
                          0, //AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL,
                          ISR_Driver);

    AT91F_AIC_EnableIt(AT91C_BASE_AIC, USB_GetDriverID(pUsb));

#ifndef USB_BUS_POWERED
    // Configure VBus monitoring
    BRD_ConfigureVBus(USB_GetDriverInterface(pUsb));

    // Configure and enable the Vbus detection interrupt
    AT91F_AIC_ConfigureIt(AT91C_BASE_AIC,
                          AT91C_ID_VBUS,
                          AT91C_AIC_PRIOR_LOWEST,
                          0, //AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL,
                          ISR_VBus);

    AT91F_PIO_InterruptEnable(AT91C_PIO_VBUS, AT91C_VBUS);
    AT91F_AIC_EnableIt(AT91C_BASE_AIC, AT91C_ID_VBUS);
#else
    // Power up the USB controller
    USB_Attach(pUsb);
#endif
}
//------------------------------------------------------------------------------
//! \brief  Callback invoked when the device becomes suspended
//!
//!         Disables LEDs (if they are used) and then puts the device into
//!         low-power mode. When traces are used, the device does not enter
//!         low-power mode to avoid losing some outputs.
//! \param  pUsb    Pointer to a S_usb instance
//------------------------------------------------------------------------------
static void CBK_Suspend(const S_usb *pUsb)
{
    LED_OFF(LED_POWER);
    LED_OFF(LED_USB);
    LED_OFF(LED_MEM);

#if defined(NOTRACES)
    DEV_Suspend();
#endif
}

//------------------------------------------------------------------------------
//! \brief  Callback invoked when the device leaves the suspended state
//!
//!         The device is first returned to a normal operating mode and LEDs are
//!         re-enabled. When traces are used, the device does not enter
//!         low-power mode to avoid losing some outputs.
//! \param  pUsb    Pointer to a S_usb instance
//------------------------------------------------------------------------------
static void CBK_Resume(const S_usb *pUsb)
{
#if defined(NOTRACES)
    DEV_Resume();
#endif

    LED_INIT();
    LED_ON(LED_POWER);
    LED_OFF(LED_USB);
    LED_OFF(LED_MEM);
}

//------------------------------------------------------------------------------
//! \brief  Callback invoked when a new SETUP request is received
//!
//!         The new request if forwarded to the standard request handler,
//!         which performs the enumeration of the device.
//! \param  pUsb   Pointer to a S_usb instance
//------------------------------------------------------------------------------
static void CBK_NewRequest(const S_usb *pUsb)
{
    MSE_RequestHandler(&sMse);
}

//------------------------------------------------------------------------------
//          Main
//------------------------------------------------------------------------------
int main()
{
    unsigned int dPioStatus;
    S_mse_report *pReport = &(sMse.sReport);
    bool         isChanged;

    TRACE_INIT();
    TRACE_INFO("\n\rMain HID mouse\n\r");

    // Initialize the HID mouse driver
    MSE_Init(&sMse, &sUsb);

    TRACE_INFO("Connecting ... ");

    // Wait for the device to be powered before connecting it
    while (!ISSET(USB_GetState(&sUsb), USB_STATE_POWERED));
    USB_Connect(&sUsb);

    TRACE_INFO("OK\n\r");

    // Main loop
    while (1) {

        // Retrieve PIO status change
        isChanged = false;
        dPioStatus = SWITCH_PIO->PIO_PDSR;

        // Check for clicks on any button
        // Left mouse button
        if (ISCLEARED(dPioStatus, SW_LEFTCLICK)) {

            SET(pReport->bButtons, MSE_LEFT_BUTTON);
            CLEAR(dPioStatus, SW_LEFTCLICK);
            isChanged = true;
        }
        else {

            // Check if button was previously pressed
            if (ISSET(pReport->bButtons, MSE_LEFT_BUTTON)) {

                CLEAR(pReport->bButtons, MSE_LEFT_BUTTON);
                isChanged = true;
            }
        }

        // Right mouse button
        if (ISCLEARED(dPioStatus, SW_RIGHTCLICK)) {

            SET(pReport->bButtons, MSE_RIGHT_BUTTON);
            CLEAR(dPioStatus, SW_RIGHTCLICK);
            isChanged = true;
        }
        else {

            // Check if button was previously pressed
            if (ISSET(pReport->bButtons, MSE_RIGHT_BUTTON)) {

                CLEAR(pReport->bButtons, MSE_RIGHT_BUTTON);
                isChanged = true;
            }
        }

        // Check pointer for movement
        // Left
        if (ISCLEARED(dPioStatus, SW_LEFT)) {

            pReport->bX = -SPEED_X;
            isChanged = true;
        }
        // Right
        else if (ISCLEARED(dPioStatus, SW_RIGHT)) {

            pReport->bX = SPEED_X;
            isChanged = true;
        }
        else {

            pReport->bX = 0;
        }

        // Up
        if (ISCLEARED(dPioStatus, SW_UP)) {

            pReport->bY = -SPEED_Y;
            isChanged = true;
        }
        // Down
        else if (ISCLEARED(dPioStatus, SW_DOWN)) {

            pReport->bY = SPEED_Y;
            isChanged = true;
        }
        else {

            pReport->bY = 0;
        }

        // Send report if a change has occured
        if (isChanged) {

            LED_TOGGLE(LED_MEM);
            MSE_SendReport(&sMse, 0, 0);
        }
    }
}


