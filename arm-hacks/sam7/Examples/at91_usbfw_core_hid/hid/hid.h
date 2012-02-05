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
$Id: hid.h 118 2006-10-17 12:50:13Z jjoannic $
*/

#ifndef _HID_H
#define _HID_H

//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

//! \brief  HID standard definitions
//! \see    hid1_11.pdf
//------------------------------------------------------------------------------
#define HID_BM_REQUEST_TYPE         0x21

// Current class version
#define HID_1_11                  0x0111

//! \brief  Class-Specific Request Codes
//! \see    Section 7.2
#define HID_GET_REPORT              0x01    // Mandatory for all devices
#define HID_GET_IDLE                0x02
#define HID_GET_PROTOCOL            0x03    // Mandatory for boot devices
//      Reserved                  0x04-0x08
#define HID_SET_REPORT              0x09
#define HID_SET_IDLE                0x0A
#define HID_SET_PROTOCOL            0x0B    // Mandatory for boot devices

//! \brief  Class Descriptor Types
//! \see    Section 7.1
#define HID_DESCRIPTOR              0x21
#define HID_REPORT_DESCRIPTOR       0x22
#define HID_PHYSICAL_DESCRIPTOR     0x23
//      Reserved                  0x24-0x2F

//! \brief  Protocol Codes
//! \see    Section 4.3
#define HID_PROTOCOL_CODE_NONE         0
#define HID_PROTOCOL_CODE_KEYBOARD     1
#define HID_PROTOCOL_CODE_MOUSE        2
//      Reserved                    3 - 255

//! \brief  Subclass Codes
//! \see    Section 4.2
#define HID_NO_SUBCLASS                0
#define HID_BOOT_INTERFACE_SUBCLASS    1

//! \brief  Boot/Report Protocols
//! \see    Section 7.2.5
#define HID_BOOT_PROTOCOL              0
#define HID_REPORT_PROTOCOL            1

//! \brief  HID report types
#define HID_INPUT_REPORT                1
#define HID_OUTPUT_REPORT               2
#define HID_FEATURE_REPORT              3

// Report descriptor
// Section 6.2.2.4 - Main Items
#define HID_MAIN_INPUT                   0x80
#define HID_MAIN_OUTPUT                  0x90
#define HID_MAIN_FEATURE                 0xB0
#define HID_MAIN_COLLECTION              0xA0
#define HID_MAIN_ENDCOLLECTION           0xC0

// Section 6.2.2.5 - Input, Output, and Features Items
#define HID_DATA                     (0 << 0)
#define HID_CONSTANT                 (1 << 0)
#define HID_ARRAY                    (0 << 1)
#define HID_VARIABLE                 (1 << 1)
#define HID_ABSOLUTE                 (0 << 2)
#define HID_RELATIVE                 (1 << 2)
#define HID_NOWRAP                   (0 << 3)
#define HID_WRAP                     (1 << 3)
#define HID_LINEAR                   (0 << 4)
#define HID_NONLINEAR                (1 << 4)
#define HID_PREFERREDSTATE           (0 << 5)
#define HID_NOPREFERRED              (1 << 5)
#define HID_NONULLPOSITION           (0 << 6)
#define HID_NULLSTATE                (1 << 6)
#define HID_NONVOLATILE              (0 << 7)
#define HID_VOLATILE                 (1 << 7)
#define HID_BITFIELD                 (0 << 8)
#define HID_BUFFEREDBYTES            (1 << 8)

// Section 6.2.2.6 - Collection, End Collection Items
#define HID_COLLECTION_PHYSICAL          0x00
#define HID_COLLECTION_APPLICATION       0x01
#define HID_COLLECTION_LOGICAL           0x02
#define HID_COLLECTION_REPORT            0x03
#define HID_COLLECTION_NAMEDARRAY        0x04
#define HID_COLLECTION_HID_USAGESWITCH   0x05
#define HID_COLLECTION_HID_USAGEMODIFIER 0x06

//! \name   Global items
//! \see    hid1_11.pdf - Section 6.2.2.7
//! @{

//! \brief  Current usage page
#define HID_GLOBAL_USAGE_PAGE            0x04

//! \brief  Minimum value that a variable or array item will report
#define HID_GLOBAL_LOGICAL_MINIMUM       0x14

//! \brief  Maximum value that a variable or array item will report
#define HID_GLOBAL_LOGICAL_MAXIMUM       0x24

//! \brief  Minimum value for the physical extent of a variable item
#define HID_GLOBAL_PHYSICAL_MINIMUM      0x34

//! \brief  Maximum value for the physical extent of a variable item
#define HID_GLOBAL_PHYSICAL_MAXIMUM      0x44

//! \brief  Value of the unit exponent in base 10
#define HID_GLOBAL_UNIT_EXPONENT         0x54

//! \brief  Unit values
#define HID_GLOBAL_UNIT                  0x64

//! \brief  Size of the report fiels in bits
#define HID_GLOBAL_REPORT_SIZE           0x74

//! \brief  Specifies the report ID
#define HID_GLOBAL_REPORT_ID             0x84

//! \brief  Number of data fields for an item
#define HID_GLOBAL_REPORT_COUNT          0x94

//! \brief  Places a copy of the global item state table on the stack
#define HID_GLOBAL_PUSH                  0xA4

//! \brief  Replaces the item state table with the top structure from the stack
#define HID_GLOBAL_POP                   0xB4
//! @}

//! \name   Local items
//! \see    hid1_11.pdf - Section 6.2.2.8
//! @{

//! \brief  Suggested usage for an item or collection
#define HID_LOCAL_USAGE                  0x08

//! \brief  Defines the starting usage associated with an array or bitmap
#define HID_LOCAL_USAGE_MINIMUM          0x18

//! \brief  Defines the ending usage associated with an array or bitmap
#define HID_LOCAL_USAGE_MAXIMUM          0x28

//! \brief  Determines the body part used for a control
#define HID_LOCAL_DESIGNATOR_INDEX       0x38

//! \brief  Defines the index of the starting designator associated with an array
//!         or bitmap
#define HID_LOCAL_DESIGNATOR_MINIMUM     0x48

//! \brief  Defines the index of the ending designator associated with an array
//!         or bitmap
#define HID_LOCAL_DESIGNATOR_MAXIMUM     0x58

//! \brief  String index for a string descriptor
#define HID_LOCAL_STRING_INDEX           0x78

//! \brief  Specifies the first string index when assigning a group of sequential
//!         strings to controls in an array or bitmap
#define HID_LOCAL_STRING_MINIMUM         0x88

//! \brief  Specifies the last string index when assigning a group of sequential
//!         strings to controls in an array or bitmap
#define HID_LOCAL_STRING_MAXIMUM         0x98

//! \brief  Defines the beginning or end of a set of local items
#define HID_LOCAL_DELIMITER              0xA8
//! @}

//! \name   Usage pages
//! \see    HuT1_12.pdf - Section 3 - Table 1
//! @{

//! \brief  Undefined
#define HID_USAGE_PAGE_UNDEFINED            0x00

//! \brief  Generic desktop controls
#define HID_USAGE_PAGE_GENERIC_DESKTOP      0x01

//! \brief  Simulation controls
#define HID_USAGE_PAGE_SIMULATION           0x02

//! \brief  Virtual reality controls
#define HID_USAGE_PAGE_VR                   0x03

//! \brief  Sport controls
#define HID_USAGE_PAGE_SPORT                0x04

//! \brief  Game controls
#define HID_USAGE_PAGE_GAME                 0x05

//! \brief  Generic device controls
#define HID_USAGE_PAGE_GENERIC_DEVICE       0x06

//! \brief  Keyboard/Keypad
#define HID_USAGE_PAGE_KEYBOARD_KEYPAD      0x07

//! \brief  LEDs
#define HID_USAGE_PAGE_LEDS                 0x08

//! \brief  Button
#define HID_USAGE_PAGE_BUTTON               0x09

//! \brief  Ordinal
#define HID_USAGE_PAGE_ORDINAL              0x0A

//! \brief  Telephony
#define HID_USAGE_PAGE_TELEPHONY            0x0B

//! \brief  Consumer
#define HID_USAGE_PAGE_CONSUMER             0x0C

//! \brief  Digitizer
#define HID_USAGE_PAGE_DIGITIZER            0x0D

//! \brief  USB Physical Interface Device definitions for force feedback and
//!         related devices
#define HID_USAGE_PAGE_PID                  0x0F

//! \brief  Unicode
#define HID_USAGE_PAGE_UNICODE              0x10

//! \brief  Alpha-numeric display
#define HID_USAGE_PAGE_ALPHANUM_DISPLAY     0x14

//! \brief  Medical instruments
#define HID_USAGE_PAGE_MEDICAL_INSTRUMENTS  0x40

//! \brief  USB Device Class Definition for Monitor Devices
#define HID_USAGE_PAGE_MONITOR_PAGES        0x80

//! \brief  USB Device Class Definition for Power Devices
#define HID_USAGE_PAGE_POWER_PAGES          0x84

//! \brief  Bar-code scanner
#define HID_USAGE_PAGE_BARCODE_SCANNER      0x8C

//! \brief  Scale
#define HID_USAGE_PAGE_SCALE_PAGE           0x8D

//! \brief  Magnetic stripe reading devices
#define HID_USAGE_PAGE_MSR                  0x8E

//! \brief  USB Device Class Definition for Image Class Devices
#define HID_USAGE_PAGE_CAMERA_CONTROL       0x90

//! \brief  OAAF Definitions for arcade and coinop related devices
#define HID_USAGE_PAGE_ARCADE               0x91
//! @}

//! \name   Generic Desktop Usages
//! \see    HuT1_12.pdf - Section 4 - Table 6
//! @{

//! \brief  Pointer
#define HID_USAGE_POINTER                   0x01

//! \brief  Mouse
#define HID_USAGE_MOUSE                     0x02

//! \brief  Joystick
#define HID_USAGE_JOYSTICK                  0x04

//! \brief  Game pad
#define HID_USAGE_GAMEPAD                   0x05

//! \brief  Keyboard
#define HID_USAGE_KEYBOARD                  0x06

//! \brief  Keypad
#define HID_USAGE_KEYPAD                    0x07

//! \brief  Multi-axis controller
#define HID_USAGE_MULTIAXIS                 0x08

//! \brief  X axis
#define HID_USAGE_X                         0x30

//! \brief  Y axis
#define HID_USAGE_Y                         0x31
//! @}

//! \name  Units
//! \brief Possible values for a HID_GLOBAL_UNIT item
//! \see   hid1_11.pdf - Section 6.2.2.7
//! @{

//! \brief  Standard international linear system
#define UNIT_SYSTEM_SI_LINEAR               0x01

//! \brief  Standard international rotation system
#define UNIT_SYSTEM_SI_ROTATION             0x02

//! \brief  English linear system
#define UNIT_SYSTEM_ENGLISH_LINEAR          0x03

//! \brief  English rotation system
#define UNIT_SYSTEM_ENGLISH_ROTATION        0x04

//! \brief  Length unit
#define UNIT_LENGTH                         0x10

//! \brief  Mass unit
#define UNIT_MASS                           0x01

//! \brief  Time unit
#define UNIT_TIME                           0x10

//! \brief  Temperature unit
#define UNIT_TEMPERATURE                    0x01

//! \brief  Current unit
#define UNIT_CURRENT                        0x10

//! \brief  Luminous intensity unit
#define UNIT_LUMINOSITY                     0x01
//! @}


//------------------------------------------------------------------------------
//      Structures
//------------------------------------------------------------------------------

//! \brief  HID Descriptor (Mouse)
//! \see    hid1_11.pdf - Section E.8
USB_H_ATTPACKPRE typedef struct {

    unsigned char  bLength;            //!< Size of this descriptor in bytes
    unsigned char  bDescriptorType;    //!< HID descriptor type (assigned by USB)
    unsigned short bcdHID;             //!< 0x101 HID Class Specification release number
    unsigned char  bCountryCode;       //!< Hardware target country
    unsigned char  bNumDescriptors;    //!< Number of HID class descriptors to follow
    unsigned char  bRepDescriptorType; //!< Report descriptor type
    unsigned short wItemLength;        //!< Total length of Report descriptor

} USB_H_ATTPACKSUF S_hid_descriptor;

#endif // _HID_H

