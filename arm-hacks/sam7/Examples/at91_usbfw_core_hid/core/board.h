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
$Id: board.h 108 2006-10-16 08:33:33Z jjoannic $
*/

#ifndef _BOARD_H
#define _BOARD_H

// mthomas
#include "common.h"

//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

// Board revisions
#define REV_A           0
#define REV_B           1

// Default revision is REV_B
#ifndef REVISION
    #define REVISION    REV_B
#endif

// Slow clock used at startup (32kHz)
#define SLOWCLOCK       32768

// Main clock
#if defined(OLIMEX_H64)

    #define AT91C_MASTER_CLOCK      48000000

#elif defined(AT91SAM7SEK)  || defined(AT91SAM7XEK) || \
    defined(AT91SAM7A3EK) || defined(AT91SAM7SEEK)

    #define AT91C_MASTER_CLOCK      48000000

#elif defined(AT91RM9200)
    #define AT91C_BAUDRATE            115200
    #define AT91C_MASTER_CLOCK      60000000
    #define PLLAR   0x20263E04 // 179,712000 MHz for PCK 
    #define PLLBR   0x10483E0E // 48,054857 MHz (divider by 2 for USB) // NAT first tests can be done with this config
                               //    MUL= 72 0x48 DIV=14 0xE
    #define PLLBR1  0x102E3E09 // 48,054857 MHz (divider by 2 for USB) // BENOIT suggestion
                               //     MUL= 46 0x2E DIV=0x9
    #define PLLBR2  0x10483E0E // 48,054857 MHz (divider by 2 for USB)
    #define MCKR    0x00000202 // PCK/3 = MCK Master Clock = 59,904000MHz with PLLA selected 

#elif defined(AT91SAM9260EK) || defined(AT91SAM9261EK) || defined(AT91SAM9263EK)
    #define AT91C_MASTER_CLOCK      99300000
#else
    #error core_board.h Main clock
#endif

// Leds
#if defined(OLIMEX_H64)
    #define LED_PIO         AT91C_BASE_PIOA
    #define LED_PIO_ID      AT91C_ID_PIOA
    #define LED_USB         AT91C_PIO_PA8

#elif defined(AT91SAM7SEK)
    #define LED_POWER       AT91C_PIO_PA0  // DS1
    #define LED_USB         AT91C_PIO_PA1  // DS2
    #define LED_MEM         AT91C_PIO_PA2  // DS3
                         // AT91C_PIO_PA3  // DS4
    #define LED_PIO         AT91C_BASE_PIOA
    #define LED_PIO_ID      AT91C_ID_PIOA

#elif defined(AT91SAM7A3EK)
    #define LED_POWER       AT91C_PIO_PA20  // DS1
    #define LED_USB         AT91C_PIO_PA21  // DS2
    #define LED_MEM         AT91C_PIO_PA24  // DS3
                         // AT91C_PIO_PA25  // DS4
    #define LED_PIO         AT91C_BASE_PIOA
    #define LED_PIO_ID      AT91C_ID_PIOA

#elif defined(AT91SAM7XEK)
    #define LED_POWER       AT91C_PIO_PB19  // DS1
    #define LED_USB         AT91C_PIO_PB20  // DS2
    #define LED_MEM         AT91C_PIO_PB21  // DS3
                         // AT91C_PIO_PB22  // DS4
    #define LED_PIO         AT91C_BASE_PIOB
    #define LED_PIO_ID      AT91C_ID_PIOB

#elif defined(AT91SAM7SEEK)
    #define LED_POWER       AT91C_PIO_PA0   // Yellow PowerLED
    #define LED_USB         AT91C_PIO_PA1   // Green  DS1
    #define LED_MEM         AT91C_PIO_PA2   // Green  DS2
    #define LED_PIO         AT91C_BASE_PIOA
    #define LED_PIO_ID      AT91C_ID_PIOA

#elif defined(AT91RM9200EK)
    #define LED_POWER       AT91C_PIO_PB0   // Green  DS2
    #define LED_USB         AT91C_PIO_PB1   // Yellow DS4
    #define LED_MEM         AT91C_PIO_PB2   // Red    DS6
    #define LED_PIO         AT91C_BASE_PIOB
    #define LED_PIO_ID      AT91C_ID_PIOB

#elif defined(AT91SAM9260EK)
    #define LED_POWER       AT91C_PIO_PA1    // Only 1 led
    #define LED_USB         AT91C_PIO_PA6    // DS5
    #define LED_MEM         AT91C_PIO_PA1    // Only 1 led
    #define LED_PIO         AT91C_BASE_PIOA
    #define LED_PIO_ID      AT91C_ID_PIOA

#elif defined(AT91SAM9261EK)
    #define LED_POWER       AT91C_PIO_PA23  // Power LED
    #define LED_USB         AT91C_PIO_PA13  // DS8
    #define LED_MEM         AT91C_PIO_PA14  // DS7
    #define LED_PIO         AT91C_BASE_PIOA
    #define LED_PIO_ID      AT91C_ID_PIOA

#elif defined(AT91SAM9263EK)
// PWM1
// PWM2

    // theses values only for compil:
    #define LED_POWER       AT91C_PIO_PA23
    #define LED_USB         AT91C_PIO_PA13
    #define LED_MEM         AT91C_PIO_PA14
    #define LED_PIO         AT91C_BASE_PIOA
    #define LED_PIO_ID      AT91C_ID_PIOA
#else
#error message core_board.h led
#endif

// Switches
#if defined(OLIMEX_H64)
    #define SWITCH_PIO     AT91C_BASE_PIOA
    #define SWITCH_PIO_ID  AT91C_ID_PIOA
    // no switches

#elif defined(AT91SAM7SEK)
    #define SWITCH1        AT91C_PIO_PA19  // BP1
    #define SWITCH2        AT91C_PIO_PA20  // BP2
    #define SWITCH3        AT91C_PIO_PA15  // BP3
    #define SWITCH4        AT91C_PIO_PA14  // BP4
    #define SWITCH_PIO     AT91C_BASE_PIOA
    #define SWITCH_PIO_ID  AT91C_ID_PIOA

#elif defined(AT91SAM7A3EK)
    #define SWITCH1        AT91C_PIO_PB8    // Joystick UP
    #define SWITCH2        AT91C_PIO_PB12   // Joystick LEFT
    #define SWITCH3        AT91C_PIO_PB13   // Joystick RIGHT
    #define SWITCH4        AT91C_PIO_PB9    // Joystick DOWN
                        // AT91C_PIO_PB14   // Joystick PUSH
    #define SWITCH_PIO     AT91C_BASE_PIOB
    #define SWITCH_PIO_ID  AT91C_ID_PIOB

#elif defined(AT91SAM7XEK)
    #define SWITCH1        AT91C_PIO_PA21  // Joystick UP
    #define SWITCH2        AT91C_PIO_PA23  // Joystick LEFT
    #define SWITCH3        AT91C_PIO_PA24  // Joystick RIGHT
    #define SWITCH4        AT91C_PIO_PA22  // Joystick DOWN
                        // AT91C_PIO_PA25  // Joystick PUSH
    #define SWITCH_PIO     AT91C_BASE_PIOA
    #define SWITCH_PIO_ID  AT91C_ID_PIOA

#elif defined(AT91SAM7SEEK)
    #define SWITCH1        AT91C_PIO_PB23 // Joystick UP
    #define SWITCH2        AT91C_PIO_PB24 // Joystick DOWN
    #define SWITCH3        AT91C_PIO_PB26 // Joystick RIGHT
    #define SWITCH4        AT91C_PIO_PB27 // Joystick LEFT
    #define SWITCH_PIO     AT91C_BASE_PIOB
    #define SWITCH_PIO_ID  AT91C_ID_PIOB
    // Right Clic     : PB22 : BP2
    // Push Left Clic : PB25 : BP1

#elif defined(AT91RM9200EK)
    // no button on the board
    #define SWITCH1        AT91C_PIO_PB23 // NO BUTTON
    #define SWITCH2        AT91C_PIO_PB24 // ONLY for COMPIL
    #define SWITCH3        AT91C_PIO_PB26 // ONLY for COMPIL
    #define SWITCH4        AT91C_PIO_PB27 // ONLY for COMPIL
    #define SWITCH_PIO     AT91C_BASE_PIOB// ONLY for COMPIL
    #define SWITCH_PIO_ID  AT91C_ID_PIOB

#elif defined(AT91SAM9260EK)
    #define SWITCH1        AT91C_PIO_PA30 // BP3
    #define SWITCH2        AT91C_PIO_PA31 // BP4
    #define SWITCH3        AT91C_PIO_PA30 // Only 2 BP
    #define SWITCH4        AT91C_PIO_PA31 // Only 2 BP
    #define SWITCH_PIO     AT91C_BASE_PIOA
    #define SWITCH_PIO_ID  AT91C_ID_PIOA

#elif defined(AT91SAM9261EK)
    #define SWITCH1        AT91C_PIO_PA27 // BP3
    #define SWITCH2        AT91C_PIO_PA26 // BP4
    #define SWITCH3        AT91C_PIO_PA25 // BP5
    #define SWITCH4        AT91C_PIO_PA24 // BP6
    #define SWITCH_PIO     AT91C_BASE_PIOA
    #define SWITCH_PIO_ID  AT91C_ID_PIOA

#elif defined(AT91SAM9263EK)
// PC4
// PC4

#elif defined(MISTRALREVB)
    #define SWITCH1        AT91C_PIO_PA21
    #define SWITCH2        AT91C_PIO_PA23
    #define SWITCH3        AT91C_PIO_PA24
    #define SWITCH4        AT91C_PIO_PA22
    #define SWITCH_PIO     AT91C_BASE_PIOA
    #define SWITCH_PIO_ID  AT91C_ID_PIOA

#else
    #error core_board.h switch
#endif


// Bus power
// If USB_BUS_POWERED if defined, device is powered through VBUS
// VBUS PIO
#if !defined(USB_BUS_POWERED)
    #if defined(OLIMEX_H64)
        // skip all these
        #define USB_BUS_POWERED

    #elif defined(AT91SAM7SEK)
        #define AT91C_VBUS              AT91C_PIO_PA13
        #define AT91C_PIO_VBUS          AT91C_BASE_PIOA
        #define AT91C_ID_VBUS           AT91C_ID_PIOA

    #elif defined(AT91SAM7SEEK)
        #define AT91C_VBUS              AT91C_PIO_PC19
        #define AT91C_PIO_VBUS          AT91C_BASE_PIOC
        #define AT91C_ID_VBUS           AT91C_ID_PIOC

    #elif defined(AT91SAM7XEK) || defined(AT91SAM7A3EK)
        #define USB_BUS_POWERED

    #elif defined(AT91RM9200EK)
        #define AT91C_VBUS              AT91C_PIO_PD4
        #define AT91C_PIO_VBUS          AT91C_BASE_PIOD
        #define AT91C_ID_VBUS           AT91C_ID_PIOD

    #elif defined(AT91SAM9260EK)
        #define AT91C_VBUS              AT91C_PIO_PC5
        #define AT91C_PIO_VBUS          AT91C_BASE_PIOC
        #define AT91C_ID_VBUS           AT91C_ID_PIOC

    #elif defined(AT91SAM9261EK)
        #define AT91C_VBUS              AT91C_PIO_PB29
        #define AT91C_PIO_VBUS          AT91C_BASE_PIOB
        #define AT91C_ID_VBUS           AT91C_ID_PIOB

    #elif defined(AT91SAM9263EK)
        #define AT91C_VBUS              AT91C_PIO_PA25
        #define AT91C_PIO_VBUS          AT91C_BASE_PIOA
        #define AT91C_ID_VBUS           AT91C_ID_PIOA

    #else
        #error core_board.h BUS POWERED
    #endif
#endif // !defined(USB_BUS_POWERED)

// Pull-ups
#if defined(AT91SAM7SEK)
    #define AT91C_PULLUP            AT91C_PIO_PA16
    #define AT91C_PIO_PULLUP        AT91C_BASE_PIOA
    #define AT91C_ID_PULLUP         AT91C_ID_PIOA

#elif defined(AT91SAM7A3EK)
    #define AT91C_PULLUP            AT91C_PIO_PB1
    #define AT91C_PIO_PULLUP        AT91C_BASE_PIOB
    #define AT91C_ID_PULLUP         AT91C_ID_PIOB

#elif defined(AT91RM9200)
    #define AT91C_PULLUP            AT91C_PIO_PD5
    #define AT91C_PIO_PULLUP        AT91C_BASE_PIOD
    #define AT91C_ID_PULLUP         AT91C_ID_PIOD

#endif

//------------------------------------------------------------------------------
//      Internal functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief   Lights on specified LED.
// \param   dLed Pio ID of LED to light on
//------------------------------------------------------------------------------
INLINEFUNC void AT91F_LED_On(unsigned int dLed)
{
    AT91F_PIO_ClearOutput(LED_PIO, dLed);
}

//------------------------------------------------------------------------------
// \brief   Lights off specified LED.
// \param   dLed Pio ID of LED to light off
//------------------------------------------------------------------------------
INLINEFUNC void AT91F_LED_Off(unsigned int dLed)
{
    AT91F_PIO_SetOutput(LED_PIO, dLed);
}

//------------------------------------------------------------------------------
// \brief   Initializes the PIO for LED control
//------------------------------------------------------------------------------
INLINEFUNC void AT91F_LED_Init()
{
    AT91F_PMC_EnablePeriphClock(AT91C_BASE_PMC, 1 << LED_PIO_ID);
    AT91F_PIO_CfgOutput(LED_PIO, LED_USB);
#ifndef OLIMEX_H64
    AT91F_PIO_CfgOutput(LED_PIO, LED_POWER);
    AT91F_PIO_CfgOutput(LED_PIO, LED_MEM);
#endif
}

//------------------------------------------------------------------------------
// \brief   Toggle the status of specified LED
// \param   dLed Pio ID of LED
//------------------------------------------------------------------------------
INLINEFUNC void AT91F_LED_Toggle(unsigned int dLed)
{
    if (ISSET(AT91F_PIO_GetInput(LED_PIO), dLed)) {

        AT91F_PIO_ClearOutput(LED_PIO, dLed);
    }
    else {

        AT91F_PIO_SetOutput(LED_PIO, dLed);
    }
}

//------------------------------------------------------------------------------
// \brief   Initializes the PIO for BUTTON control
//------------------------------------------------------------------------------
INLINEFUNC void AT91F_BUTTON_Init()
{
    AT91F_PMC_EnablePeriphClock(AT91C_BASE_PMC, 1 << SWITCH_PIO_ID);
    //AT91F_PIO_CfgInput(SWITCH_PIO, SWITCH1);
    //AT91F_PIO_CfgInput(SWITCH_PIO, SWITCH2);
    //AT91F_PIO_CfgInput(SWITCH_PIO, SWITCH3);
    //AT91F_PIO_CfgInput(SWITCH_PIO, SWITCH4);
}

//------------------------------------------------------------------------------
//      Macros
//------------------------------------------------------------------------------
#if defined(NOLEDS)
    #define LED_INIT()
    #define LED_ON(led)
    #define LED_OFF(led)
    #define LED_TOGGLE(led)
#else
    #define LED_INIT()      AT91F_LED_Init()
    #define LED_ON(led)     AT91F_LED_On(led)
    #define LED_OFF(led)    AT91F_LED_Off(led)
    #define LED_TOGGLE(led) AT91F_LED_Toggle(led)
#endif

//------------------------------------------------------------------------------
//      Prototypes
//------------------------------------------------------------------------------

extern void BRD_ConfigureVBus(void *pInterface);
extern bool BRD_IsVBusConnected(void *pInterface);
extern void BRD_ConfigurePullUp(void *pInterface);
extern void BRD_ConnectPullUp(void *pInterface);
extern void BRD_DisconnectPullUp(void *pInterface);
extern bool BRD_IsPullUpConnected(void *pInterface);

#endif // _BOARD_H

