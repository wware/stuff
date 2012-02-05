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
$Id: trace.h 108 2006-10-16 08:33:33Z jjoannic $
*/

#ifndef _TRACE_H
#define _TRACE_H

#if !defined(NOTRACES)

//------------------------------------------------------------------------------
//      Includes
//------------------------------------------------------------------------------
#include <stdio.h>

// mthomas
#include "common.h"
#ifdef __GNUC__
#define TRACE_PRINTF iprintf
#else
#define TRACE_PRINTF printf
#endif


//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

#define TR_INFO
#define TR_WARNING
#define TR_ERROR
#define TR_FATAL
#define TR_DEBUG_H
#define TR_DEBUG_M          // Class-level debug
#define TR_DEBUG_L          // USB-level debug

#define DBGU_BAUDRATE 115200

//------------------------------------------------------------------------------
//      Inline functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief  Initializes the DBGU
// \param  dMCK      Main oscillator frequency
// \param  dBaudrate Desired baudrate
//------------------------------------------------------------------------------
INLINEFUNC void AT91F_DBGU_Init(unsigned int dMCK,
                                   unsigned int dBaudrate)
{
    // Clock DBGU and configure its pins
    AT91F_DBGU_CfgPMC();
    AT91F_DBGU_CfgPIO();

    AT91F_US_Configure((AT91PS_USART) AT91C_BASE_DBGU,
                       dMCK,
                       AT91C_US_ASYNC_MODE,
                       dBaudrate,
                       0);

    // Enable Transmitter & Receiver
    AT91F_US_EnableTx((AT91PS_USART) AT91C_BASE_DBGU);
    AT91F_US_EnableRx((AT91PS_USART) AT91C_BASE_DBGU);
}

#endif // !defined(NOTRACES)

//------------------------------------------------------------------------------
//      Macro
//------------------------------------------------------------------------------

#if !defined(NOTRACES)
#ifdef __GNUC__
	#define TRACE_INIT()  \
	do { \
		setvbuf(stdout, NULL, _IONBF, 0); \
		AT91F_DBGU_Init(AT91C_MASTER_CLOCK, DBGU_BAUDRATE); \
	} while (0)
#else
    #define TRACE_INIT()    AT91F_DBGU_Init(AT91C_MASTER_CLOCK, DBGU_BAUDRATE)
#endif
#else
    #define TRACE_INIT(...)
#endif

#if defined(TR_DEBUG_H)
    #define TRACE_DEBUG_H(...)      TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_DEBUG_H(...)
#endif // TR_DEBUG_H

#if defined(TR_DEBUG_M)
    #define TRACE_DEBUG_M(...)      TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_DEBUG_M(...)
#endif // TR_DEBUG_M

#ifdef TR_DEBUG_L
    #define TRACE_DEBUG_L(...)      TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_DEBUG_L(...)
#endif // TR_DEBUG_L

#if defined(TR_DEBUG_L) || defined(TR_DEBUG_M) || defined(TR_DEBUG_H)
    #define TRACE_DEBUG_ALL(...)    TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_DEBUG_ALL(...)
#endif

#ifdef TR_INFO
    #define TRACE_INFO(...)         TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_INFO(...)
#endif // TR_INFO

#ifdef TR_WARNING
    #define TRACE_WARNING(...)      TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_WARNING(...)
#endif // TR_WARNING

#ifdef TR_ERROR
    #define TRACE_ERROR(...)        TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_ERROR(...)
#endif // TR_ERROR

#ifdef TR_FATAL
    #define TRACE_FATAL(...)        TRACE_PRINTF(__VA_ARGS__)
#else
    #define TRACE_FATAL(...)
#endif // TR_FATAL

#endif // _TRACE_H
