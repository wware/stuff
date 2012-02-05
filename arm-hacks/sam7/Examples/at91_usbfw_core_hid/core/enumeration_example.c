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
$Id: enumeration_example.c 108 2006-10-16 08:33:33Z jjoannic $
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
//      Structures
//------------------------------------------------------------------------------

//! \brief  Represents all the data which must be sent when the host requests
//!         a configuration descriptor.
//! \param  sConfiguration Configuration descriptor
//! \param  sInterface     First interface descriptor
//! \see    S_usb_configuration_descriptor
//! \see    S_usb_interface_descriptor
//! \see    usb_20.pdf - Section 9.4.3
typedef struct {

    S_usb_configuration_descriptor sConfiguration;

    S_usb_interface_descriptor     sInterface1;
    S_usb_functional_descriptor    sFunctional1;
    S_usb_functional4_descriptor   sFunctional2;
    S_usb_functional_descriptor    sFunctional3;
    S_usb_functional_descriptor    sFunctional4;
    S_usb_functional_descriptor    sEndpoint3;

    S_usb_interface_descriptor     sInterface2;
    S_usb_endpoint_descriptor      sEndpoint1;
    S_usb_endpoint_descriptor      sEndpoint2;

} S_core_configuration_descriptor;

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

//! \brief  New reset callback
//static void CBK_Reset(const S_usb *pUsb);

//! \brief  New SOF callback
//static void CBK_SOF(const S_usb *pUsb);

//------------------------------------------------------------------------------
//      Internal variables
//------------------------------------------------------------------------------

//! \brief  List of endpoints (including endpoint 0) used by the device.
//! \see    S_usb_endpoint
static S_usb_endpoint pEndpoints[] = {

    USB_ENDPOINT_SINGLEBANK // Control endpoint 0
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
    0,//CBK_Reset
    CBK_Suspend,
    CBK_Resume,
    CBK_NewRequest,
    0 //CBK_SOF
};

//! \brief  USB driver instance
//! \see    S_usb
static const S_usb sUsb = {

    &sDefaultDriver,
    pEndpoints,
    1,
    &sCallbacks,
    &sSetup,
    &dState
};

// Descriptors
//! Device descriptor
static const S_usb_device_descriptor sDeviceDescriptor = {

    sizeof(S_usb_device_descriptor), // Size of this descriptor in bytes
    USB_DEVICE_DESCRIPTOR,           // DEVICE Descriptor Type
    USB2_00,                         // USB Specification 2.0
    USB_CLASS_CDC_DATA,              // Class
    0x00,                            // Subclass
    0x00,                            // Protocol
    USB_ENDPOINT0_MAXPACKETSIZE,     // Maximum packet size for endpoint zero
    USB_VENDOR_ATMEL,                // Vendor ID "ATMEL"
    0x0000,                          // Product ID
    0x0001,                          // Device release number
    0x01,                            // Index 1: manufacturer string
    0x02,                            // Index 2: product string
    0x03,                            // Index 3: serial number string
    0x01                             // One possible configurations
};

//! \brief  Device configuration, which includes the configuration descriptor
//!         and the first interface descriptor in this case.
//! \see    S_core_configuration_descriptor
static const S_core_configuration_descriptor sConfigurationDescriptor = {

    // Configuration descriptor
    {
        sizeof(S_usb_configuration_descriptor),  // Size of this descriptor
        USB_CONFIGURATION_DESCRIPTOR,            // CONFIGURATION descriptor
        sizeof(S_core_configuration_descriptor), // Total length
        0x01,                                    // Number of interfaces
        0x01,                                    // Value to select this configuration
        0x00,                                    // No index for describing this configuration
        USB_CONFIG_SELF_NOWAKEUP,                // Device attributes
        USB_POWER_MA(100)                        // maximum power consumption in mA
    },

    // Interface Descriptor
    {
        sizeof(S_usb_interface_descriptor), // Size of this descriptor in bytes
        USB_INTERFACE_DESCRIPTOR,           // INTERFACE Descriptor Type
        0x00,                               // Interface number 0
        0x00,                               // Value used to select this setting
        0x01,                               // Number of endpoints
        0x02,                               // Interface class
        0x02,                               // Interface subclass
        0x01,                               // Interface protocol
        0x00                                // Index of string descriptor
    },

    // Header Functional Descriptor
    {
        sizeof(S_usb_functional_descriptor), // Size of this descriptor in bytes
        0x24,                                // CS interface
        0x00,                                // I'm a header!
        0x10,                                // CDC 1.10, low byte
        0x01,                                // CDC 1.10, high byte
    },

    // ACM Functional Descriptor
    {
        sizeof(S_usb_functional4_descriptor), // Size of this descriptor in bytes
        0x24,                                // CS interface
        0x02,                                // subtype: ACM
        0x00,                                // no capabilities at all
    },

    // Union Functional Descriptor
    {
        sizeof(S_usb_functional_descriptor), // Size of this descriptor in bytes
        0x24,                                // CS interface
        0x06,                                // subtype: union
        0x00,                                // Master interface: Communication Class
        0x01,                                // Slave Interface: Data Class
    },

    // Call Management Functional Descriptor
    {
        sizeof(S_usb_functional_descriptor), // Size of this descriptor in bytes
        0x24,                                // CS interface
        0x01,                                // Call management
        0x00,                                // capabilities: D1 + D0
        0x01,                                // Data interface: Data class interface 1
    },

    // Endpoint Descriptor
    {
        sizeof(S_usb_endpoint_descriptor),  // Size of this descriptor in bytes
        USB_ENDPOINT_DESCRIPTOR,            // ENDPOINT DescriptorType
        0x83,                               // Endpoint 3, IN
        ENDPOINT_TYPE_INTERRUPT,
        8,                                  // Max Packet Size
        0xFF                                // Interval
    },

    // Interface Descriptor
    {
        sizeof(S_usb_interface_descriptor), // Size of this descriptor in bytes
        USB_INTERFACE_DESCRIPTOR,           // INTERFACE Descriptor Type
        0x00,                               // Interface number 0
        0x00,                               // Value used to select this setting
        0x02,                               // Number of endpoints used by this
                                            // interface (excluding endpoint 0).
        USB_CLASS_DEVICE,                   // Interface class
        0x00,                               // Interface subclass
        0x00,                               // Interface protocol
        0x00                                // Index of string descriptor
    },

    // Endpoint Descriptor
    {
        sizeof(S_usb_endpoint_descriptor),  // Size of this descriptor in bytes
        USB_ENDPOINT_DESCRIPTOR,            // ENDPOINT DescriptorType
        0x01,                               // Endpoint 1, OUT
        ENDPOINT_TYPE_BULK,
        64,                                 // Max Packet Size
        0x00                                // Interval
    },

    // Endpoint Descriptor
    {
        sizeof(S_usb_endpoint_descriptor),  // Size of this descriptor in bytes
        USB_ENDPOINT_DESCRIPTOR,            // ENDPOINT DescriptorType
        0x82,                               // Endpoint 2, IN
        ENDPOINT_TYPE_BULK,
        64,                                 // Max Packet Size
        0x00                                // Interval
    },
};

// String descriptors
//! \brief  Language ID
static const S_usb_language_id sLanguageID = {

    USB_STRING_DESCRIPTOR_SIZE(1),
    USB_STRING_DESCRIPTOR,
    USB_LANGUAGE_ENGLISH_US
};

//! \brief  Manufacturer description
static const char pManufacturer[] = {

    USB_STRING_DESCRIPTOR_SIZE(5),
    USB_STRING_DESCRIPTOR,
    USB_UNICODE('A'),
    USB_UNICODE('T'),
    USB_UNICODE('M'),
    USB_UNICODE('E'),
    USB_UNICODE('L')
};

//! \brief  Product descriptor
static const char pProduct[] = {

    USB_STRING_DESCRIPTOR_SIZE(15),
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
    USB_UNICODE('E'),
    USB_UNICODE('N'),
    USB_UNICODE('U'),
    USB_UNICODE('M')
};

//! \brief  Serial number
static const char pSerial[] = {

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

//! \brief  List of string descriptors used by the device
static const char *pStringDescriptors[] = {

    (char *) &sLanguageID,
    pManufacturer,
    pProduct,
    pSerial
};

//! \brief  List of descriptors used by the device
//! \see    S_std_descriptors
static S_std_descriptors sDescriptors = {

    &sDeviceDescriptor,
    (S_usb_configuration_descriptor *) &sConfigurationDescriptor,
    pStringDescriptors,
    0
};

//! \brief  Standard class driver
//! \see    S_std_class
static S_std_class sClass = {

    &sUsb,
    &sDescriptors
};

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
    //LED_OFF(LED_POWER);
    LED_OFF(LED_USB);
    //LED_OFF(LED_MEM);

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
    //LED_ON(LED_POWER);
    LED_OFF(LED_USB);
    //LED_OFF(LED_MEM);
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
    STD_RequestHandler(&sClass);
}

//------------------------------------------------------------------------------
//! \brief  Callback invoked when a Reset request is received
//!
//! \param  pUsb   Pointer to a S_usb instance
//------------------------------------------------------------------------------
//static void CBK_Reset(const S_usb *pUsb)
//{
    // Put your reset handler here
//}

//------------------------------------------------------------------------------
//! \brief  Callback invoked when a SOF is received
//!
//! \param  pUsb   Pointer to a S_usb instance
//------------------------------------------------------------------------------
//static void CBK_SOF(const S_usb *pUsb)
//{
    // Put your SOF handler here
//}

//------------------------------------------------------------------------------
//          Main
//------------------------------------------------------------------------------
int main()
{
    TRACE_INIT();
    TRACE_INFO("\n\rMain Enumeration\n\r");

    // Initialize the USB driver
    USB_Init(&sUsb);

    TRACE_INFO("Connecting ... ");

    // Wait for the device to be powered before connecting it
    while (!ISSET(USB_GetState(&sUsb), USB_STATE_POWERED));

    USB_Connect(&sUsb);

    TRACE_INFO("OK\n\r");

    // Main loop
    while (1) {

        // Put USB class driver implementation here
    }
}

