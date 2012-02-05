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
$Id: keyboard_example.c 119 2006-10-17 12:51:47Z jjoannic $
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
#include "keyboard_driver.h"

//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

//! \brief  Total number of different keys produced by this example
#define NUM_KEYS                    4

//! \brief  Number of standard keys used
#define NUM_NORMAL_KEYS             3

//! \brief  Number of modifier keys (alt, shift, ...) used
#define NUM_MODIFIER_KEYS           1

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
    USB_ENDPOINT_SINGLEBANK  // Interrupt OUT endpoint
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
    KBD_NUM_ENDPOINTS,
    &sCallbacks,
    &sSetup,
    &dState
};

//! \brief  HID keyboard class driver instance
//! \see    S_kbd
static S_kbd sKbd;

//! \brief  List of PIO lines associated with each key
static unsigned int  pKeyPios[NUM_KEYS] = {SWITCH1, SWITCH2, SWITCH3, SWITCH4};

//! \brief  Key code for each key used
static unsigned char pKeyCodes[NUM_KEYS] = {4, 38, 83, 1 << 1}; // 'a', '9', num. lock, left shift

//! \brief  Status of each key (true = pressed, false = released)
static bool pKeyPressed[NUM_KEYS] = {false, false, false, false};

//! \brief  Number of timer ticks since last report was sent
static unsigned int dTimerTicks;

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
    KBD_RequestHandler(&sKbd);
}

//------------------------------------------------------------------------------
//! \brief  Resets a timer when a new report has been sent.
//!
//!         This is necessary to comply with the configured Idle rate set by
//!         the host. When the timer expires, this means the report has been
//!         unchanged and that the device should resent it once.
//------------------------------------------------------------------------------
static void ResetTimer()
{
    // Check that Idle rate is not null (infinite)
    if (sKbd.bIdleRate > 0) {

        dTimerTicks = 0;
        AT91C_BASE_TC0->TC_RC = (AT91C_MASTER_CLOCK / 2) / 1000;
        AT91C_BASE_TC0->TC_CCR = AT91C_TC_CLKEN;
        AT91C_BASE_TC0->TC_CCR = AT91C_TC_SWTRG;
    }
}

//------------------------------------------------------------------------------
//! \brief  Called when the timer expires after a new input report has been sent
//!
//!         The same report is sent one more time to indicate that it is
//!         unchanged.
//------------------------------------------------------------------------------
static void TimerExpired()
{
    unsigned char bStatus;

    TRACE_DEBUG_H("Expired\n\r");
    AT91C_BASE_TC0->TC_CCR = AT91C_TC_CLKDIS;

    // Send current report once
    do {

        bStatus = KBD_SendReport(&sKbd, 0, 0);
    }
    while (bStatus != USB_STATUS_SUCCESS);
}

//------------------------------------------------------------------------------
//! \brief  Called when a timer tick occurs, every 1ms.
//!
//!         When the current Idle rate has been reached, the TimerExpired method
//!         is triggered.
//------------------------------------------------------------------------------
static void TimerTick()
{
    volatile unsigned int dDummy;

    // Increase tick count
    dTimerTicks++;

    // Check if Idle rate has been reached
    if (dTimerTicks >= (sKbd.bIdleRate*4)) {

        TimerExpired();
    }

    // Clear interrupt
    dDummy = AT91C_BASE_TC0->TC_SR;
}

//------------------------------------------------------------------------------
//! \brief  Called when the host has sent an output report to the device.
//------------------------------------------------------------------------------
static void ReportReceived()
{
    // Check the bmLEDs field
    if (ISSET(sKbd.sOutputReport.bmLeds, 1)) {

        LED_ON(LED_MEM);
    }
    else {

        LED_OFF(LED_MEM);
    }
}

//------------------------------------------------------------------------------
//! \brief  Receive loop for output report sent by the host.
//!
//!         When a new report is received, the KBD_ReceiveReport function is
//!         called again to start a new transfer immediately.
//------------------------------------------------------------------------------
static void ReceiveReports()
{
    unsigned char bStatus;

    do {

        bStatus = KBD_ReceiveReport(&sKbd, (Callback_f) ReportReceived, 0);
    }
    while (bStatus != USB_STATUS_SUCCESS);
}

//------------------------------------------------------------------------------
//          Main
//------------------------------------------------------------------------------
int main()
{
    unsigned int       dPioStatus;
    bool               isChanged;
    unsigned int       dNumPressedKeys;
    unsigned int       dKey;
    S_kbd_input_report *pInputReport = &(sKbd.sInputReport);
    unsigned char      bStatus;

    TRACE_INIT();
    TRACE_INFO("\n\rMain HID keyboard\n\r");

    // Configure timer 0
    // 1 ms precision
    AT91F_TC0_CfgPMC();
    AT91C_BASE_TC0->TC_CMR = AT91C_TC_WAVE | AT91C_TC_WAVESEL_UP_AUTO;
    AT91C_BASE_TC0->TC_IER = AT91C_TC_CPCS;
    AT91F_AIC_ConfigureIt(AT91C_BASE_AIC,
                          AT91C_ID_TC0,
                          AT91C_AIC_PRIOR_LOWEST,
                          0,
                          TimerTick);
    AT91F_AIC_EnableIt(AT91C_BASE_AIC, AT91C_ID_TC0);

    // Initialize the HID driver
    KBD_Init(&sKbd, &sUsb, (Callback_f) ReportReceived);

    TRACE_INFO("Connecting ... ");

    // Wait for the device to be powered before connecting it
    while (!ISSET(USB_GetState(&sUsb), USB_STATE_POWERED));

    USB_Connect(&sUsb);

    TRACE_INFO("OK\n\r");

    // Wait for the device to be configured
    while (!ISSET(USB_GetState(&sUsb), USB_STATE_CONFIGURED));

    // Start receiving reports on the Interrupt OUT pipe
    ReceiveReports();

    // Main loop
    while (1) {

        isChanged = false;
        dNumPressedKeys = 0;

        // Retrieve PIO status change
        dPioStatus = SWITCH_PIO->PIO_PDSR;

        // Check standard keys
        for (dKey = 0; dKey < NUM_NORMAL_KEYS; dKey++) {

            // Check for status mismatch
            if (ISCLEARED(dPioStatus, pKeyPios[dKey]) != pKeyPressed[dKey]) {

                // Update status
                pKeyPressed[dKey] = ISCLEARED(dPioStatus, pKeyPios[dKey]);

                // Check if key has been pressed or released
                if (pKeyPressed[dKey]) {

                    // Update input report
                    pInputReport->pPressedKeys[dNumPressedKeys] = pKeyCodes[dKey];
                    dNumPressedKeys++;
                }

                isChanged = true;
            }
        }

        // Check modifier keys
        for (dKey = NUM_NORMAL_KEYS; dKey < NUM_KEYS; dKey++) {

            // Check for status mismatch
            if (ISCLEARED(dPioStatus, pKeyPios[dKey]) != pKeyPressed[dKey]) {

                // Update status
                pKeyPressed[dKey] = ISCLEARED(dPioStatus, pKeyPios[dKey]);

                // Check if key has been pressed or released
                if (pKeyPressed[dKey]) {

                    // Update input report
                    SET(pInputReport->bmModifierKeys, pKeyCodes[dKey]);
                }
                else {

                    // Update input report
                    CLEAR(pInputReport->bmModifierKeys, pKeyCodes[dKey]);
                }

                isChanged = true;
            }
        }

        // Report status change
        if (isChanged) {

            // Clear remaining keys
            for (dKey = dNumPressedKeys; dKey < NUM_KEYS; dKey++) {

                pInputReport->pPressedKeys[dKey] = 0;
            }

            // Send new report
            do {

                bStatus = KBD_SendReport(&sKbd, (Callback_f) ResetTimer, 0);
            }
            while (bStatus != USB_STATUS_SUCCESS);
        }
    }
}


