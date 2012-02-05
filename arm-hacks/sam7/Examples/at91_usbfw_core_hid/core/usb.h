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
$Id: usb.h 121 2006-10-17 12:54:54Z jjoannic $
*/

#ifndef _USB_H
#define _USB_H

#ifdef __GNUC__

#define __inline static inline
#define INLINEFUNC static inline
#define USB_H_ATTPACKPRE
#define USB_H_ATTPACKSUF __attribute__((__packed__))

#else

#define INLINEFUNC extern __inline
#ifndef inline
    #define inline __inline
#endif
#define USB_H_ATTPACKPRE __packed
#define USB_H_ATTPACKSUF

#endif

#ifndef __inline
#define __inline static inline
#endif

//------------------------------------------------------------------------------
//! \defgroup usb_std_struc USB standard structures
//! \brief Chapter 9 of the USB specification 2.0 (usb_20.pdf) describes a
//!        standard USB device framework. Several structures and associated
//!        constants have been defined on that model and are described here.
//! \see usb_20.pdf - Section 9
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \defgroup usb_api_struct USB API Structures
//! \brief The USB API uses various custom structures to track the state of
//!        the USB controller, endpoints, and the like. These structures are
//!        described here.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \defgroup usb_api_methods USB API Methods
//! \brief  Methods provided by the USB API to manipulate a USB driver.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \defgroup usb_api_callbacks Callback API
//! \brief These callback functions are used by the USB API to notify the
//!        user application of incoming events or actions to perform.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \defgroup usb_std_req_hlr Standard Request Handler
//! \brief This module provides a way to easily handle standard standard
//!        requests.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

// USB standard definitions
//---------------------------------------------------------
//! \ingroup usb_std_req_hlr
//! \defgroup std_dev_req Standard Device Requests
//! These are the standard request defined for a SETUP transaction. Please refer
//! to Section 9.4 of the USB 2.0 specification (usb_20.pdf) for more
//! information. Table 9.4 defines the bRequest values for each request.
//! \see S_usb_request
//! \see usb_20.pdf - Section 9.4
//! @{

//! \brief  Returns the status for the specified recipient.
//! \see    get_status_const
//! \see    usb_20.pdf - Section 9.4.5
#define USB_GET_STATUS                 0x00

//! \brief  Disables a specific feature of the device
//! \see    usb_20.pdf - Section 9.4.1
//! \see    clr_set_feat_const
#define USB_CLEAR_FEATURE              0x01

// Reserved for futur use              0x02

//! \brief  Enables a specific feature of the device
//! \see    clr_set_feat_const
//! \see    set_feat_const
//! \see    usb_20.pdf - 9.4.9
#define USB_SET_FEATURE                0x03

// Reserved for futur use              0x04

//! \brief  Sets the device address for subsequent accesses
//! \see    usb_20.pdf - Section 9.4.6
#define USB_SET_ADDRESS                0x05

//! \brief  Returns the specified descriptor if it exists
//! \see    usb_20.pdf - Section 9.4.3
#define USB_GET_DESCRIPTOR             0x06

//! \brief  Updates existing descriptors or creates new descriptors
//! \brief  This request is optional
//! \see    usb_20.pdf - Section 9.4.8
#define USB_SET_DESCRIPTOR             0x07

//! \brief  Returns the current configuration value of the device
//! \see    usb_20.pdf - Section 9.4.2
#define USB_GET_CONFIGURATION          0x08

//! \brief  Sets the configuration of the device
//! \see    usb_20.pdf - Section 9.4.7
#define USB_SET_CONFIGURATION          0x09

//! \brief  Returns the specified alternate setting for an interface
//! \see    usb_20.pdf - Section 9.4.4
#define USB_GET_INTERFACE              0x0A

//! \brief  Selects an alternate setting for the selected interface
//! \see    usb_20.pdf - Section 9.4.10
#define USB_SET_INTERFACE              0x0B

//! \brief  Sets and reports an endpoint synchronization frame
//! \see    usb_20.pdf - Section 9.4.11
#define USB_SYNCH_FRAME                0x0C
//! @}

//! \ingroup std_dev_req
//! \defgroup clr_set_feat_const Clear/Set Feature - Constants
//! \brief Useful constants when declaring a Clear Feature or Set Feature
//! standard request.
//! \see std_dev_req
//! \see S_usb_request
//! \see usb_20.pdf - Section 9.4 - Table 9.6
//! @{

//! \name Standard Feature Selectors
//! Possible values for the wValue field of the Clear Feature and Set Feature
//! standard requests.
//! @{

//! \brief Halt feature of an endpoint
#define USB_ENDPOINT_HALT              0x00

//! \brief Remote wake-up feature of the device
#define USB_DEVICE_REMOTE_WAKEUP       0x01

//! \brief USB test mode
#define USB_TEST_MODE                  0x02
//! @}
//! @}

//! \ingroup std_dev_req
//! \defgroup set_feat_const Set Feature - Constants
//! \brief Useful constants when declaring a Set Feature standard request
//! \see usb_20.pdf - Section 7.1.20
//! \see usb_20.pdf - Section 9.2.9 - Table 9.7
//! @{

//! \name Test Mode Selectors
//! \brief Test modes available to probe an USB device.
//! @{

//! \brief Tests the high-output drive level on the D+ line
#define TEST_J                         0x01

//! \brief Tests the high-output drive level on the D- line
#define TEST_K                         0x02

//! \brief Tests the output impedance, low-level output voltage and loading
//!        characteristics
#define TEST_SEO_NAK                   0x03

//! \brief Tests rise and fall times, eye patterns and jitter
#define TEST_PACKET                    0x04

//! \brief Tests the hub disconnect detection
#define TEST_FORCE_ENABLE              0x05
//! @}
//! @}

//! \ingroup std_dev_req
//! \defgroup get_set_desc_const Get/Set Descriptor - Constants
//! \brief Useful constants when declaring a Get Descriptor or Set Descriptor
//!        standard request
//! \see StdReq
//! \see S_usb_device_descriptor
//! \see S_usb_configuration_descriptor
//! \see S_usb_endpoint_descriptor
//! \see S_usb_device_qualifier_descriptor
//! \see S_USB_LANGUAGE_ENGLISH_US
//! \see usb_20.pdf - Section 9.5 - Table 9.5
//! @{

//! \name Descriptor Types
//! \brief Possible bDescriptorType values for the descriptor structures.
//!
//! They can be used with Get Descriptor and Set Descriptor standard requests
//! to retrieve/modify them
//! @{

//! \brief Device descriptor
#define USB_DEVICE_DESCRIPTOR                       0x01

//! \brief Configuration descriptor
#define USB_CONFIGURATION_DESCRIPTOR                0x02

//! \brief String descriptor
#define USB_STRING_DESCRIPTOR                       0x03

//! \brief Interface descriptor
#define USB_INTERFACE_DESCRIPTOR                    0x04

//! \brief Endpoint descriptor
#define USB_ENDPOINT_DESCRIPTOR                     0x05

//! \brief Device qualifier descriptor
#define USB_DEVICE_QUALIFIER_DESCRIPTOR             0x06

//! \brief Other speed configuration descriptor
#define USB_OTHER_SPEED_CONFIGURATION_DESCRIPTOR    0x07

//! \brief Interface power descriptor
#define USB_INTERFACE_POWER_DESCRIPTOR              0x08
//! @}
//! @}

//! \ingroup usb_std_struc
//! \defgroup ept_desc_const Endpoint Descriptor - Constants
//! \brief Useful constants when declaring an endpoint descriptor
//! \see S_usb_endpoint_descriptor
//! \see usb_20.pdf - Section 9.6.6 - Table 9.13
//! @{

//! \name  bEndpointAddress field
//! \brief Values for the bEndpointAddress field of an endpoint descriptor.
//! @{

//! \brief Defines an OUT endpoint
#define USB_ENDPOINT_OUT                            (0 << 7)

//! \brief Defines an IN endpoint
#define USB_ENDPOINT_IN                             (1 << 7)
//! @}

//! \name  bmAttributes field
//! \brief These are the four possible tranfer type values for the bmAttributes
//!        field of an endpoint descriptor.
//! @{

//! \brief Defines a CONTROL endpoint
#define ENDPOINT_TYPE_CONTROL          0x00

//! \brief Defines a ISOCHRONOUS endpoint
#define ENDPOINT_TYPE_ISOCHRONOUS      0x01

//! \brief Defines a BULK endpoint
#define ENDPOINT_TYPE_BULK             0x02

//! \brief Defines an INTERRUPT endpoint
#define ENDPOINT_TYPE_INTERRUPT        0x03
//! @}
//! @}

//! \name  bmRequestType field
//! \brief This bitmapped field identifies the characteristics of the specific
//!        request.
//! \see   usb_209.pdf - Table 9-2. Format of Setup Data
//! @{

//! \brief D6...5: Type
//! \brief Defines a standard request
#define USB_STANDARD_REQUEST                0x00

//! \brief Defines a class request
#define USB_CLASS_REQUEST                   0x01

//! \brief Defines a vendor request
#define USB_VENDOR_REQUEST                  0x02

//! \brief Get the type of bmRequestType
#define USB_REQUEST_TYPE(pSetup)            ((pSetup->bmRequestType & 0x60) >> 5)

//! \brief Get the receipient of bmRequestType
#define USB_REQUEST_RECIPIENT(pSetup)       (pSetup->bmRequestType & 0x1F)

//! \brief Recipient is the whole device
#define USB_RECIPIENT_DEVICE                0x00

//! \brief Recipient is an interface
#define USB_RECIPIENT_INTERFACE             0x01

//! \brief Recipient is an endpoint
#define USB_RECIPIENT_ENDPOINT              0x02

//! @}

//! \ingroup usb_std_struc
//! \defgroup ept_desc_macros Endpoint Descriptor - Macros
//! \brief Useful macros when declaring an endpoint descriptor
//! \see S_usb_endpoint_descriptor
//! \see usb_20.pdf - Section 9.6.6 - Table 9.13
//! @{

//! \name bEndpointAddress field
//! @{

//! \brief Returns an endpoint number
#define USB_ENDPOINT_NUMBER(bEndpointAddress)       (bEndpointAddress & 0x0F)

//! \brief Returns an endpoint direction (IN or OUT)
#define USB_ENDPOINT_DIRECTION(bEndpointAddress)    (bEndpointAddress & 0x80)
//! @}
//! @}

//! \ingroup usb_std_req_hlr
//! \defgroup usb_class_codes USB Class Codes
//! These are the class codes approved by the USB-IF organization. They can be
//! used for the bDeviceClass value of a device descriptor, or the
//! bInterfaceClass value of an interface descriptor.
//! \see S_usb_device_descriptor
//! \see S_usb_interface_descriptor
//! \see http://www.usb.org/developers/defined_class
//! @{

//! \brief Indicates that the class information is determined by the interface
//!        descriptor.
#define USB_CLASS_DEVICE                0x00

//! \brief Audio capable devices
#define USB_CLASS_AUDIO                 0x01

//! \brief Communication devices
#define USB_CLASS_COMMUNICATION         0x02

//! \brief Human-interface devices
#define USB_CLASS_HID                   0x03

//! \brief Human-interface devices requiring real-time physical feedback
#define USB_CLASS_PHYSICAL              0x05

//! \brief Still image capture devices
#define USB_CLASS_STILL_IMAGING         0x06

//! \brief Printer devices
#define USB_CLASS_PRINTER               0x07

//! \brief Mass-storage devices
#define USB_CLASS_MASS_STORAGE          0x08

//! \brief Hub devices
#define USB_CLASS_HUB                   0x09

//! \brief Raw-data communication device
#define USB_CLASS_CDC_DATA              0x0A

//! \brief Smartcards devices
#define USB_CLASS_SMARTCARDS            0x0B

//! \brief Protected content devices
#define USB_CLASS_CONTENT_SECURITY      0x0D

//! \brief Video recording devices
#define USB_CLASS_VIDEO                 0x0E

//! \brief Devices that diagnostic devices
#define USB_CLASS_DIAGNOSTIC_DEVICE     0xDC

//! \brief Wireless controller devices
#define USB_CLASS_WIRELESS_CONTROLLER   0xE0

//! \brief Miscellaneous devices
#define USB_CLASS_MISCELLANEOUS         0xEF

//! \brief Application-specific class code
#define USB_CLASS_APPLICATION_SPECIFIC  0xFE

//! \brief Vendor-specific class code
#define USB_CLASS_VENDOR_SPECIFIC       0xFF
//! @}

//! \ingroup usb_std_struc
//! \defgroup dev_desc_const Device Descriptor - Constants
//! Several useful constants when declaring a device descriptor
//! \see S_usb_device_descriptor
//! \see S_usb_device_qualifier_descriptor
//! \see usb_20.pdf - Section 9.6.1 - Table 9.8
//! @{

//! \name USB specification release codes
//! @{

//! bcdUSB field - USB 2.0 specification code
#define USB2_00                     0x0200
#define USB1_10                     0x0110
//! @}

//! \name Vendor IDs
//! @{

//! idVendor - ATMEL Vendor ID
#define USB_VENDOR_ATMEL            0x03EB
//! @}
//! @}

//! \ingroup usb_std_struc
//! \defgroup cfg_desc_const Configuration Descriptor - Constants
//! Several useful constants when declaring a configuration descriptor
//! \see S_usb_configuration_descriptor
//! \see usb_20.pdf - Section 9.6.3 - Table 9.10
//! @{

//! \name bmAttributes field
//! \brief These are the possible values for the bmAttributes field of a
//!        S_usb_configuration_descriptor.
//! @{

//! \brief Device is bus-powered and does not support remote wakeup
#define USB_CONFIG_BUS_NOWAKEUP    0x80

//! \brief Device is self-powered and does not support remote wakeup
#define USB_CONFIG_SELF_NOWAKEUP   0xC0

//! \brief Device is bus-powered and supports remote wakeup
#define USB_CONFIG_BUS_WAKEUP      0xA0

//! \brief Device is self-powered and supports remote wakeup
#define USB_CONFIG_SELF_WAKEUP     0xE0
//! @}

//! \name Power consumption
//! @{

//! Power consumption macro for the Configuration descriptor
#define USB_POWER_MA(power)        (power/2)
//! @}
//! @}

//! \ingroup usb_std_struc
//! \defgroup str_desc_const String Descriptor - Constants
//! \brief Useful constants when declaring a string descriptor.
//! \see S_usb_string_descriptor
//! \see USB_LANGIDs.pdf
//! @{

//! \name Language IDs
//! \brief These are the supported language IDs as defined by the USB-IF group.
//!        They can be used to specified the languages supported by the string
//!        descriptors of a USB device.
//! @{

//! \brief English (United States)
#define USB_LANGUAGE_ENGLISH_US     0x0409
//! @}
//! @}

//! \ingroup usb_std_struc
//! \defgroup str_desc_macros String Descriptor - Macros
//! \brief Several useful macros when declaring a string descriptor.
//! \see S_usb_string_descriptor
//! @{

//! Converts an ASCII character to its Unicode equivalent
#define USB_UNICODE(a)                      (a), 0x00

//! Calculates the size of a string descriptor given the number of ASCII
//! characters in it
#define USB_STRING_DESCRIPTOR_SIZE(size)    ((size * 2) + 2)
//! @}

//! \ingroup usb_api_methods
//! \defgroup usb_api_ret_val Standard return values
//! \brief Values returned by the API methods.
//! @{

//! \brief Last method has completed successfully
#define USB_STATUS_SUCCESS      0

//! \brief Method was aborted because the recipient (device, endpoint, ...) was
//!        busy
#define USB_STATUS_LOCKED       1

//! \brief Method was aborted because of abnormal status
#define USB_STATUS_ABORTED      2

//! \brief Method was aborted because the endpoint or the device has been reset
#define USB_STATUS_RESET        3
//! @}

// Device State
//! \ingroup S_usb_struc
//! \defgroup S_usb_dev_state USB Device States
//! \brief Constant values used to track which USB state the device is currently
//!        in.
//! @{

//! Attached state
#define USB_STATE_ATTACHED                          (1 << 0)

//! Powered state
#define USB_STATE_POWERED                           (1 << 1)

//! Default state
#define USB_STATE_DEFAULT                           (1 << 2)

//! Address state
#define USB_STATE_ADDRESS                           (1 << 3)

//! Configured state
#define USB_STATE_CONFIGURED                        (1 << 4)

//! Suspended state
#define USB_STATE_SUSPENDED                         (1 << 5)

//! @}

//------------------------------------------------------------------------------
//      Structures
//------------------------------------------------------------------------------

// USB standard structures
//-------------------------
//! \ingroup usb_std_struc
//! \brief This structure represents a standard SETUP request
//! \see usb_20.pdf - Section 9.3 - Table 9.2
USB_H_ATTPACKPRE typedef struct {


    unsigned char   bmRequestType:8;    //!< Characteristics of the request
    unsigned char   bRequest:8;         //!< Particular request
    unsigned short  wValue:16;          //!< Request-specific parameter
    unsigned short  wIndex:16;          //!< Request-specific parameter
    unsigned short  wLength:16;         //!< Length of data for the data phase

} USB_H_ATTPACKSUF S_usb_request;

//! \ingroup usb_std_struc
//! \brief This descriptor structure is used to provide information on
//!        various parameters of the device
//!
//! Usage example:
//! \include S_usb_device_descriptor_example.c
//! \see usb_20.pdf - Section 9.6.1
USB_H_ATTPACKPRE typedef struct {

   unsigned char  bLength;              //!< Size of this descriptor in bytes
   unsigned char  bDescriptorType;      //!< DEVICE descriptor type
   unsigned short bscUSB;               //!< USB specification release number
   unsigned char  bDeviceClass;         //!< Class code
   unsigned char  bDeviceSubClass;      //!< Subclass code
   unsigned char  bDeviceProtocol;      //!< Protocol code
   unsigned char  bMaxPacketSize0;      //!< Control endpoint 0 max. packet size
   unsigned short idVendor;             //!< Vendor ID
   unsigned short idProduct;            //!< Product ID
   unsigned short bcdDevice;            //!< Device release number
   unsigned char  iManufacturer;        //!< Index of manu. string descriptor
   unsigned char  iProduct;             //!< Index of prod. string descriptor
   unsigned char  iSerialNumber;        //!< Index of S.N.  string descriptor
   unsigned char  bNumConfigurations;   //!< Number of possible configurations

}  USB_H_ATTPACKSUF S_usb_device_descriptor;

//! \ingroup usb_std_struc
//! \brief This is the standard configuration descriptor structure. It is used
//!        to report the current configuration of the device.
//!
//! Usage example:
//! \include S_usb_configuration_descriptor_example.c
//! \see usb_20.pdf - Section 9.6.3
USB_H_ATTPACKPRE typedef struct {

   unsigned char  bLength;              //!< Size of this descriptor in bytes
   unsigned char  bDescriptorType;      //!< CONFIGURATION descriptor type
   unsigned short wTotalLength;         //!< Total length of data returned
                                        //!< for this configuration
   unsigned char  bNumInterfaces;       //!< Number of interfaces for this
                                        //!< configuration
   unsigned char  bConfigurationValue;  //!< Value to use as an argument for
                                        //!< the Set Configuration request to
                                        //!< select this configuration
   unsigned char  iConfiguration;       //!< Index of string descriptor
                                        //!< describing this configuration
   unsigned char  bmAttibutes;          //!< Configuration characteristics
   unsigned char  bMaxPower;            //!< Maximum power consumption of the
                                        //!< device
}  USB_H_ATTPACKSUF S_usb_configuration_descriptor;

//! \ingroup usb_std_struc
//! \brief Standard interface descriptor. Used to describe a specific interface
//!        of a configuration.
//!
//! Usage example:
//! \include S_usb_interface_descriptor_example.c
//! \see usb_20.pdf - Section 9.6.5
USB_H_ATTPACKPRE typedef struct {

   unsigned char bLength;               //!< Size of this descriptor in bytes
   unsigned char bDescriptorType;       //!< INTERFACE descriptor type
   unsigned char bInterfaceNumber;      //!< Number of this interface
   unsigned char bAlternateSetting;     //!< Value used to select this alternate
                                        //!< setting
   unsigned char bNumEndpoints;         //!< Number of endpoints used by this
                                        //!< interface (excluding endpoint zero)
   unsigned char bInterfaceClass;       //!< Class code
   unsigned char bInterfaceSubClass;    //!< Sub-class
   unsigned char bInterfaceProtocol;    //!< Protocol code
   unsigned char iInterface;            //!< Index of string descriptor
                                        //!< describing this interface
}  USB_H_ATTPACKSUF S_usb_interface_descriptor;

//! \ingroup usb_std_struc
//! \brief This structure is the standard endpoint descriptor. It contains
//!        the necessary information for the host to determine the bandwidth
//!        required by the endpoint.
//!
//! Usage example:
//! \include S_usb_endpoint_descriptor_example.c
//! \see usb_20.pdf - Section 9.6.6
USB_H_ATTPACKPRE typedef struct {

   unsigned char  bLength;              //!< Size of this descriptor in bytes
   unsigned char  bDescriptorType;      //!< ENDPOINT descriptor type
   unsigned char  bEndpointAddress;     //!< Address of the endpoint on the USB
                                        //!< device described by this descriptor
   unsigned char  bmAttributes;         //!< Endpoint attributes when configured
   unsigned short wMaxPacketSize;       //!< Maximum packet size this endpoint
                                        //!< is capable of sending or receiving
   unsigned char  bInterval;            //!< Interval for polling endpoint for
                                        //!< data transfers
}  USB_H_ATTPACKSUF S_usb_endpoint_descriptor;

//! \ingroup usb_std_struc
//! \brief The device qualifier structure provide information on a high-speed
//!        capable device if the device was operating at the other speed.
//!
//! Usage example:
//! \include S_usb_device_qualifier_descriptor_example.c
//! \see usb_20.pdf - Section 9.6.2
USB_H_ATTPACKPRE typedef struct {

   unsigned char  bLength;              //!< Size of this descriptor in bytes
   unsigned char  bDescriptorType;      //!< DEVICE_QUALIFIER descriptor type
   unsigned short bscUSB;               //!< USB specification release number
   unsigned char  bDeviceClass;         //!< Class code
   unsigned char  bDeviceSubClass;      //!< Sub-class code
   unsigned char  bDeviceProtocol;      //!< Protocol code
   unsigned char  bMaxPacketSize0;      //!< Control endpoint 0 max. packet size
   unsigned char  bNumConfigurations;   //!< Number of possible configurations
   unsigned char  bReserved;            //!< Reserved for future use, must be 0

}  USB_H_ATTPACKSUF S_usb_device_qualifier_descriptor;

//! \ingroup usb_std_struc
//! \brief Functional descriptors are needed for CDC/ACM devices, see
//!        http://www.usblyzer.com/usb-communication-device-class-cdc-decoder.htm.
//!
//! Usage example???
//! \include S_usb_device_qualifier_descriptor_example.c
//! \see usb_20.pdf - Section 9.6.2
USB_H_ATTPACKPRE typedef struct {

   unsigned char  bLength;              //!< Size of this descriptor in bytes
   unsigned char  bDescriptorType;      //!< Descriptor type
   unsigned char  bDescriptorSubType;   //!< Descriptor type
   unsigned char  bPayload1;
   unsigned char  bPayload2;

}  USB_H_ATTPACKSUF S_usb_functional_descriptor;

// like functional descriptor, but only one payload byte
USB_H_ATTPACKPRE typedef struct {

   unsigned char  bLength;              //!< Size of this descriptor in bytes
   unsigned char  bDescriptorType;      //!< Descriptor type
   unsigned char  bDescriptorSubType;   //!< Descriptor type
   unsigned char  bPayload;

}  USB_H_ATTPACKSUF S_usb_functional4_descriptor;

//! \ingroup usb_std_struc
//! \brief The S_usb_language_id structure represents the string descriptor
//!        zero, used to specify the languages supported by the device. This
//!        structure only define one language ID.
//!
//! Usage example:
//! \include S_usb_language_id_example.c
//! \see usb_20.pdf - Section 9.6.7 - Table 9.15
USB_H_ATTPACKPRE typedef struct {

   unsigned char  bLength;               //!< Size of this descriptor in bytes
   unsigned char  bDescriptorType;       //!< STRING descriptor type
   unsigned short wLANGID;               //!< LANGID code zero

}  USB_H_ATTPACKSUF S_usb_language_id;

// USB Framework
//---------------

typedef struct _S_usb S_usb;

//! \ingroup usb_api_callbacks
//! \brief Initialization callback function
//!
//! This callback is invoked whenever the USB API is initialized using the
//! USB_Init function. It should perform the following operations:
//! \li If an OS is being used, install the USB driver
//! \li Configure the USB controller interrupt
//! \li Configure the VBus monitoring interrupt
//! \attention Implementation of this callback is \b mandatory
//! \see USB_Init
//! \see S_usb
typedef void (*S_usb_init)(const S_usb *);

//! \ingroup usb_api_callbacks
//! \brief Reset callback function
//!
//! Invoked whenever the device is reset by the host. This function should
//! perform initialization or re-initialization of the user application.
//! \attention Implementation of this callback is \b optional
//! \see USB_Handler
//! \see S_usb
typedef void (*S_usb_reset)(const S_usb *);

//! \ingroup usb_api_callbacks
//! \brief Suspend callback function
//!
//! Invoked when the device is suspended by the host or detached from the bus.
//! If the device must enter low-power mode when suspended, then the necessary
//! code must be implemented here.
//! \see USB_Attach
//! \see USB_Handler
//! \see S_usb
typedef void (*S_usb_suspend)(const S_usb *);

//! \ingroup usb_api_callbacks
//! \brief Resume callback function
//!
//! Invoked when the device is resumed by the host or attached to the bus. If
//! the suspend callback has put the device into low-power mode, then this
//! function must perform the necessary actions to return it to a normal mode of
//! operation.
//! \see S_usb_suspend
//! \see USB_Attach
//! \see USB_Handler
//! \see S_usb
typedef void (*S_usb_resume)(const S_usb *);

//! \ingroup usb_api_callbacks
//! \brief New Request callback function
//!
//! Invoked when a new SETUP request is received. The request can then be
//! retrieved by using the USB_GetSetup function on the S_usb instance.
//! \see USB_Handler
//! \see S_usb
typedef void (*S_usb_new_request)(const S_usb *);

//! \ingroup usb_api_callbacks
//! \brief  Interrupt SOF callback function
//!
//!         Invoked when a SOF interrupt is received.
//! \see    USB_Handler
//! \see    S_usb
typedef void (*S_usb_int_sof)(const S_usb *);

// Methods
typedef char (*S_usb_write)(const S_usb   *pUsb,
                            unsigned char bEndpoint,
                            const void    *pData,
                            unsigned int  dLength,
                            Callback_f    fCallback,
                            void          *pArgument);
typedef char (*S_usb_read)(const S_usb   *pUsb,
                           unsigned char bEndpoint,
                           void          *pData,
                           unsigned int  dLength,
                           Callback_f    fCallback,
                           void *pArgument);
typedef char (*S_usb_stall)(const S_usb   *pUsb,
                            unsigned char bEndpoint);
typedef bool (*S_usb_halt)(const S_usb   *pUsb,
                           unsigned char bEndpoint,
                           unsigned char bRequest);
typedef void (*S_usb_remote_wakeup)(const S_usb *pUsb);
typedef bool (*S_usb_cfg_endpoint)(const S_usb                     * pUsb,
                                   const S_usb_endpoint_descriptor *pEpDesc);
typedef bool (*S_usb_attach)(const S_usb *);
typedef void (*S_usb_set_address)(const S_usb *);
typedef void (*S_usb_set_cfg)(const S_usb *);
typedef void (*S_usb_handler)(const S_usb *);
typedef void (*S_usb_connect)(const S_usb *);
typedef void (*S_usb_disconnect)(const S_usb *);

//------------------------------------------------------------------------------
//! \ingroup usb_api_struct
//! \brief This structure is used to track the current status of an endpoint,
//!        i.e. the current transfer descriptors, the number of FIFO banks used,
//!        and so forth.
//!
//! Each endpoint used by the firmware must have a corresponding S_usb_endpoint
//! structure associated with it.\n
//! Usage example:
//! \include S_usb_endpoint_example.c
//! \see S_usb_ept_macros
//! \see S_usb
//------------------------------------------------------------------------------
typedef struct {

    // Transfer descriptor
    char                    *pData;             //!< \brief Transfer descriptor
                                                //!< pointer to a buffer where
                                                //!< the data is read/stored
    unsigned int            dBytesRemaining;    //!< \brief Number of remaining
                                                //!< bytes to transfer
    unsigned int            dBytesBuffered;     //!< \brief Number of bytes
                                                //!< which have been buffered
                                                //!< but not yet transferred
    unsigned int            dBytesTransferred;  //!< \brief Number of bytes
                                                //!< transferred for the current
                                                //!< operation
    Callback_f              fCallback;          //!< \brief Callback to invoke
                                                //!< after the current transfer
                                                //!< is complete
    void                    *pArgument;         //!< \brief Argument to pass to
                                                //!< the callback function
    // Hardware information
    unsigned int            wMaxPacketSize;     //!< \brief Maximum packet size
                                                //!< for this endpoint
    unsigned int            dFlag;              //!< \brief Hardware flag to
                                                //!< clear upon data reception
    unsigned char           dNumFIFO;           //!< \brief Number of FIFO
                                                //!< buffers defined for this
                                                //!< endpoint
    volatile unsigned int   dState;             //!< Endpoint internal state

} S_usb_endpoint;

//------------------------------------------------------------------------------
//! \ingroup usb_api_struct
//! \brief Pointers to the low-level methods used by the USB controller.
//!
//! This structure is used to provide an abstraction over which USB controller
//! is used by the chip. This means the USB framework is fully portable between
//! AT91 chips and supports chips with more than one controller.
//! \see UDPMethods
//! \see S_usb_driver
//------------------------------------------------------------------------------
typedef struct {

  S_usb_init            init;
  S_usb_write           write;
  S_usb_read            read;
  S_usb_stall           stall;
  S_usb_halt            halt;
  S_usb_remote_wakeup   remoteWakeUp;
  S_usb_cfg_endpoint    configureEndpoint;
  S_usb_attach          attach;
  S_usb_set_address     setAddress;
  S_usb_set_cfg         setConfiguration;
  S_usb_handler         handler;
  S_usb_connect         connect;
  S_usb_disconnect      disconnect;

} S_driver_methods;

//------------------------------------------------------------------------------
//! \ingroup usb_api_struct
//! \brief Low-level driver structure
//!
//! This structure holds information about the USB controller used, such as
//! a pointer to the physical address of the peripheral, to the endpoints FIFO,
//! etc.\n
//! In most case, it is not necessary to declare a S_usb_driver instance: the
//! defaultDriver global variable can be used. This is not possible for chips
//! which have more than one USB controller in them.\n
//! Usage example:
//! \include S_usb_driver_example.c
//! \see defaultDriver
//! \see S_usb
//------------------------------------------------------------------------------
typedef struct {

    void *                  pInterface;     //!< \brief Pointer to the USB
                                            //!< controller peripheral
    void *                  pEndpointFIFO;  //!< \brief Pointer to the endpoints
                                            //!< FIFO buffers (optional, depends
                                            //!< on the USB controller)
    void *                  pDMAInterface;  //!< \brief Pointer to the USB
                                            //!< controller DMA interface
                                            //!< (optional, depends on the USB
                                            //!< controller)
    unsigned int            dID;            //!< \brief ID of the USB controller
                                            //!< peripheral
    unsigned int            dPMC;           //!< \brief ID to enable the USB
                                            //!< controller peripheral clock
    const S_driver_methods* pMethods;       //!< \brief Pointer to
                                            //!< controller-specific methods
} S_usb_driver;

//------------------------------------------------------------------------------
//! \ingroup usb_api_struct
//! \brief Callbacks used by the USB API to notify the user application of
//!        occuring events
//!
//! Usage example:
//! \include S_usb_callbacks_example.c
//! \see usb_api_callbacks
//! \see S_usb
//------------------------------------------------------------------------------
typedef struct {

    // Callbacks
    S_usb_init          init;           //!< Init callback
    S_usb_reset         reset;          //!< Reset callback
    S_usb_suspend       suspend;        //!< Suspend callback
    S_usb_resume        resume;         //!< Resume callback
    S_usb_new_request   newRequest;     //!< NewRequest callback
    S_usb_int_sof       startOfFrame;   //!< SOF interrupt callback

} S_usb_callbacks;

//------------------------------------------------------------------------------
//! \ingroup usb_api_struct
//! \brief Main USB structure used to store the states of the various
//!        components, such as endpoints, callbacks, and so forth.
//!
//! Usage example:
//! \include S_usb_example.c
//! \see s_usb_dev_state
//! \see s_usb_methods
//------------------------------------------------------------------------------
#ifdef __GNUC__
struct _S_usb {
#else
typedef struct _S_usb {
#endif

    const S_usb_driver     *pDriver;        //!< Pointer to the low-level driver
    S_usb_endpoint* const   pEndpoints;     //!< Endpoints list
    unsigned int            dNumEndpoints;  //!< Number of endpoints in list
    const S_usb_callbacks  *pCallbacks;     //!< Pointer to the callbacks
    S_usb_request* const    pSetup;         //!< \brief Pointer to the last
                                            //!< received SETUP packet
    volatile unsigned int* const pState;    //!< Current state of the device
#ifdef __GNUC__
};
#else
} S_usb;
#endif


//------------------------------------------------------------------------------
//      Macros
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! \ingroup usb_api_struct
//! \defgroup s_usb_endpoint_macros S_usb_endpoint - Macros
//! \brief Useful macros when declaring a S_usb_endpoint instance
//! \see S_usb_endpoint
//! @{
//------------------------------------------------------------------------------

//! Declares an endpoint with a single-bank FIFO buffer
#define USB_ENDPOINT_SINGLEBANK {0,0,0,0,0,0,0,0,1,0}

//! Declares an endpoint with a dual-bank FIFO buffer
#define USB_ENDPOINT_DUALBANK {0,0,0,0,0,0,0,0,2,0}
//! @}

//------------------------------------------------------------------------------
//      Inline functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! \ingroup  usb_api_struct
//! \defgroup s_usb_methods S_usb - Methods
//! \brief  Methods used to access/modify a S_usb structure
//! \see    S_usb_driver
//! \see    S_usb
//! @{
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//! \brief  Returns the specified endpoint if it exists or zero if it does not
//! \param  pUsb      Pointer to a S_usb instance
//! \param  bEndpoint Index of endpoint
//! \return Pointer to the requested endpoint, or zero if it does not exist.
//------------------------------------------------------------------------------
__inline S_usb_endpoint * USB_GetEndpoint(const S_usb   *pUsb,
                                               unsigned char bEndpoint)
{
    if (bEndpoint >= pUsb->dNumEndpoints) {

        return 0;
    }
    else {

        return &pUsb->pEndpoints[bEndpoint];
    }
}

//------------------------------------------------------------------------------
//! \brief  Returns a pointer to the last received SETUP request
//! \param  pUsb Pointer to a S_usb instance
//! \return Pointer to the last received SETUP request
//------------------------------------------------------------------------------
__inline S_usb_request * USB_GetSetup(const S_usb *pUsb)
{
    return pUsb->pSetup;
}

//------------------------------------------------------------------------------
//! \brief  Returns a pointer to the USB controller peripheral used by a S_usb
//!         instance.
//! \param  pUsb Pointer to a S_usb instance
//! \return Pointer to the USB controller peripheral
//------------------------------------------------------------------------------
__inline void * USB_GetDriverInterface(const S_usb *pUsb) {

    return pUsb->pDriver->pInterface;
}

//------------------------------------------------------------------------------
//! \brief  Returns the USB controller peripheral ID of a S_usb instance.
//! \param  pUsb Pointer to a S_usb instance
//! \return USB controller peripheral ID
//------------------------------------------------------------------------------
__inline unsigned int USB_GetDriverID(const S_usb *pUsb) {

    return pUsb->pDriver->dID;
}

//------------------------------------------------------------------------------
//! \brief  Returns the USB controller peripheral ID used to enable the
//!         peripheral clock
//! \param  pUsb Pointer to a S_usb instance
//! \return USB controller peripheral ID to enable the peripheral clock
//------------------------------------------------------------------------------
__inline unsigned int USB_GetDriverPMC(const S_usb *pUsb) {

    return pUsb->pDriver->dPMC;
}

//! @}

//------------------------------------------------------------------------------
//      Callbacks
//------------------------------------------------------------------------------
// Invokes the init callback if it exists
__inline void USB_InitCallback(const S_usb *pUsb)
{
    if (pUsb->pCallbacks->init != 0) {

        pUsb->pCallbacks->init(pUsb);
    }
}

// Invokes the reset callback if it exists
__inline void USB_ResetCallback(const S_usb *pUsb)
{
    if (pUsb->pCallbacks->reset != 0) {

        pUsb->pCallbacks->reset(pUsb);
    }
}

// Invokes the suspend callback if it exists
__inline void USB_SuspendCallback(const S_usb *pUsb)
{
    if (pUsb->pCallbacks->suspend != 0) {

        pUsb->pCallbacks->suspend(pUsb);
    }
}

// Invokes the resume callback if it exists
__inline void USB_ResumeCallback(const S_usb *pUsb)
{
    if (pUsb->pCallbacks->resume != 0) {

        pUsb->pCallbacks->resume(pUsb);
    }
}

// Invokes the new request callback if it exists
__inline void USB_NewRequestCallback(const S_usb *pUsb)
{
    if (pUsb->pCallbacks->newRequest != 0) {

        pUsb->pCallbacks->newRequest(pUsb);
    }
}

// Invokes the SOF callback if it exists
__inline void USB_StartOfFrameCallback(const S_usb *pUsb)
{
    pUsb->pCallbacks->startOfFrame(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Sends data through an USB endpoint.
//!
//! This function sends the specified amount of data through a particular
//! endpoint. The transfer finishes either when the data has been completely
//! sent, or an abnormal condition causes the API to abort this operation.
//! An optional user-provided callback can be invoked once the transfer is
//! complete.\n
//! On control endpoints, this function automatically send a Zero-Length Packet
//! (ZLP) if the data payload size is a multiple of the endpoint maximum packet
//! size. This is not the case for bulk, interrupt or isochronous endpoints.
//! \param  pUsb      Pointer to a S_usb instance
//! \param  bEndpoint Number of the endpoint through which to send the data
//! \param  pData     Pointer to a buffer containing the data to send
//! \param  dLength   Size of the data buffer
//! \param  fCallback Callback function to invoke when the transfer finishes
//! \param  pArgument Optional parameter to pass to the callback function
//! \return Result of operation
//! \see    usb_api_ret_val
//! \see    S_usb
//------------------------------------------------------------------------------
__inline char USB_Write(const S_usb *pUsb,
                      unsigned char bEndpoint,
                      const void *pData,
                      unsigned int dLength,
                      Callback_f fCallback,
                      void *pArgument)
{
    return pUsb->pDriver->pMethods->write(pUsb,
                                          bEndpoint,
                                          pData,
                                          dLength,
                                          fCallback,
                                          pArgument);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Sends a Zero-Length Packet (ZLP) through the Control endpoint 0.
//!
//! Since sending a ZLP on endpoint 0 is quite common, this function is provided
//! as an overload to the USB_Write function.
//! \param  pUsb      Pointer to a S_usb instance
//! \param  fCallback Optional callback function to invoke when the transfer
//!                   finishes
//! \param  pArgument Optional parameter to pass to the callback function
//! \return Result of operation
//! \see    USB_Write
//! \see    usb_api_ret_val
//! \see    S_usb
//------------------------------------------------------------------------------
__inline char USB_SendZLP0(const S_usb *pUsb,
                         Callback_f  fCallback,
                         void        *pArgument) {

    return USB_Write(pUsb, 0, 0, 0, fCallback, pArgument);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Receives data on the specified USB endpoint.
//!
//! This functions receives data on a particular endpoint. It finishes either
//! when the provided buffer is full, when a short packet (payload size inferior
//! to the endpoint maximum packet size) is received, or if an abnormal
//! condition causes a transfer abort. An optional user-provided callback can be
//! invoked upon the transfer completion
//! \param  pUsb      Pointer to a S_usb instance
//! \param  bEndpoint Number of the endpoint on which to receive the data
//! \param  pData     Pointer to the buffer in which to store the received data
//! \param  dLength   Size of the receive buffer
//! \param  fCallback Optional user-provided callback function invoked upon the
//!                   transfer completion
//! \param  pArgument Optional parameter to pass to the callback function
//! \return Result of operation
//! \see    usb_api_ret_val
//! \see    S_usb
//------------------------------------------------------------------------------
__inline char USB_Read(const S_usb *pUsb,
                     unsigned char bEndpoint,
                     void *pData,
                     unsigned int dLength,
                     Callback_f fCallback,
                     void *pArgument)
{
    return pUsb->pDriver->pMethods->read(pUsb,
                                         bEndpoint,
                                         pData,
                                         dLength,
                                         fCallback,
                                         pArgument);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Sends a STALL handshake for the next received packet.
//!
//! This function only send one STALL handshake, and only if the next packet
//! is not a SETUP packet (when using this function on a control endpoint).
//! \param  pUsb      Pointer to a S_usb instance
//! \param  bEndpoint Number of endpoint on which to send the STALL
//! \return Result of operation
//! \see    usb_api_ret_val
//! \see    USB_Halt
//! \see    S_usb
//------------------------------------------------------------------------------
__inline char USB_Stall(const S_usb *pUsb, unsigned char bEndpoint)
{
    return pUsb->pDriver->pMethods->stall(pUsb, bEndpoint);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Starts a remote wakeup procedure
//! \param  pUsb Pointer to a S_usb instance
//! \see    S_usb
//------------------------------------------------------------------------------
__inline void USB_RemoteWakeUp(const S_usb *pUsb)
{
    pUsb->pDriver->pMethods->remoteWakeUp(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Clears, sets or retrieves the halt state of the specified endpoint.
//!
//! While an endpoint is in Halt state, it acknowledges every received packet
//! with a STALL handshake.
//! \param  pUsb      Pointer to a S_usb instance
//! \param  bEndpoint Number of the endpoint to alter
//! \param  bRequest  The operation to perform (set, clear or get)
//! \return true if the endpoint is halted, false otherwise
//! \see    S_usb
//------------------------------------------------------------------------------
__inline bool USB_Halt(const S_usb   *pUsb,
                     unsigned char bEndpoint,
                     unsigned char bRequest)
{
    return pUsb->pDriver->pMethods->halt(pUsb,
                                         (unsigned char)(bEndpoint&0x7F),
                                         bRequest);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Sets the device address using the last received SETUP packet.
//!
//! This method must only be called after a SET_ADDRESS standard request has
//! been received. This is because it uses the last received SETUP packet stored
//! in the S_usb structure to determine which address the device should use.
//! \param  pUsb Pointer to a S_usb instance
//! \see    std_dev_req
//! \see    S_usb
//------------------------------------------------------------------------------
__inline void USB_SetAddress(const S_usb *pUsb)
{
    pUsb->pDriver->pMethods->setAddress(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Sets the device configuration using the last received SETUP packet.
//!
//! This method must only be called after a SET_CONFIGURATION standard request
//! has been received. This is necessary because it uses the last received
//! SETUP packet (stored in the S_usb structure) to determine which
//! configuration it should adopt.
//! \param  pUsb Pointer to a S_usb instance
//! \see    std_dev_req
//! \see    S_usb
//------------------------------------------------------------------------------
__inline void USB_SetConfiguration(const S_usb *pUsb)
{
    pUsb->pDriver->pMethods->setConfiguration(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Configures the specified endpoint using the provided endpoint
//!         descriptor.
//!
//! An endpoint must be configured prior to being used. This is not necessary
//! for control endpoint 0, as this operation is automatically performed during
//! initialization.
//! \param  pUsb    Pointer to a S_usb instance
//! \param  pEpDesc Pointer to an endpoint descriptor
//! \return true if the endpoint has been configured, false otherwise
//! \see    S_usb_endpoint_descriptor
//! \see    S_usb
//------------------------------------------------------------------------------
__inline bool USB_ConfigureEndpoint(const S_usb                     *pUsb,
                                  const S_usb_endpoint_descriptor *pEpDesc)
{
    return pUsb->pDriver->pMethods->configureEndpoint(pUsb, pEpDesc);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Event handler for the USB controller peripheral.
//!
//! This function handles low-level events comming from the USB controller
//! peripheral. It then dispatches those events through the user-provided
//! callbacks. The following callbacks can be triggered:
//! \li S_usb_reset
//! \li S_usb_suspend
//! \li S_usb_resume
//! \li S_usb_new_request
//! \param  pUsb Pointer to a S_usb instance
//! \see    usb_api_callbacks
//! \see    S_usb_callbacks
//! \see    S_usb
//------------------------------------------------------------------------------
__inline void USB_Handler(const S_usb * pUsb)
{
    pUsb->pDriver->pMethods->handler(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Handles the attachment or detachment of the device to or from the
//!         USB
//!
//! This method should be called whenever the VBus power line changes state,
//! i.e. the device becomes powered/unpowered. Alternatively, it can also be
//! called to poll the status of the device.\n
//! When the device is detached from the bus, the S_usb_suspend callback is
//! invoked. Conversely, when the device is attached, the S_usb_resume callback
//! is triggered.
//! \param  pUsb Pointer to a S_usb instance
//! \return true if the device is currently attached, false otherwise
//! \see    S_usb_suspend
//! \see    S_usb_resume
//! \see    usb_api_callbacks
//! \see    S_usb
//------------------------------------------------------------------------------
__inline bool USB_Attach(const S_usb *pUsb)
{
    return pUsb->pDriver->pMethods->attach(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Connects the device to the USB.
//!
//! This method enables the pull-up resistor on the D+ line, notifying the host
//! that the device wishes to connect to the bus.
//! \param  pUsb Pointer to a S_usb instance
//! \see    USB_Disconnect
//------------------------------------------------------------------------------
__inline void USB_Connect(const S_usb *pUsb)
{
    pUsb->pDriver->pMethods->connect(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Disconnects the device from the USB.
//!
//! This method disables the pull-up resistor on the D+ line, notifying the host
//! that the device wishes to disconnect from the bus.
//! \param  pUsb Pointer to a S_usb instance
//! \see    USB_Connect
//------------------------------------------------------------------------------
__inline void USB_Disconnect(const S_usb *pUsb)
{
    pUsb->pDriver->pMethods->disconnect(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Initializes the USB API and the USB controller.
//!
//! This method must be called prior to using an other USB method. Before
//! finishing, it invokes the S_usb_init callback.
//! \param  pUsb Pointer to a S_usb instance
//! \see    S_usb_init
//! \see    S_usb
//------------------------------------------------------------------------------
__inline void USB_Init(const S_usb *pUsb)
{
    pUsb->pDriver->pMethods->init(pUsb);
}

//------------------------------------------------------------------------------
//! \ingroup usb_api_methods
//! \brief  Returns the current state of the device.
//! \param  pUsb Pointer to a S_usb instance
//! \return Current state of the device
//! \see    S_usb
//------------------------------------------------------------------------------
__inline unsigned int USB_GetState(const S_usb *pUsb)
{
    return (*(pUsb->pState) & 0x0000FFFF);
}

//------------------------------------------------------------------------------
//      Exports
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//! \ingroup  usb_api_struct
//! \defgroup usb_api_global Global variables
//! \brief  Several global variables are exported by the USB API to be used in
//!         user programs.
//! @{
//------------------------------------------------------------------------------

//! \brief  Global variable holding the methods used by an UDP controller
//! \see    S_driver_methods
//! \see    S_usb_driver
extern const S_driver_methods sUDPMethods;

//! \brief  Default USB driver for the current chip
//! \see    S_usb_driver
//! \see    S_usb
extern const S_usb_driver sDefaultDriver;
//! @}

#undef __inline

#endif // _USB_H
