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
$Id: common.h 108 2006-10-16 08:33:33Z jjoannic $
*/

#ifndef _COMMON_H
#define _COMMON_H

//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

#ifdef __GNUC__
#define __inline static inline
#define INLINEFUNC static inline
#else
#define INLINEFUNC extern __inline
#ifndef inline
    #define inline __inline
#endif
#endif

//------------------------------------------------------------------------------
//      Types
//------------------------------------------------------------------------------

// \brief  Boolean type
typedef enum {false = 0, true = 1} bool;

// \brief  Generic callback function type
//
//         Since ARM Procedure Call standard allow for 4 parameters to be
//         stored in r0-r3 instead of being pushed on the stack, functions with
//         less than 4 parameters can be cast into a callback in a transparent
//         way.
typedef void (*Callback_f)(unsigned int, unsigned int,
                           unsigned int, unsigned int);

//------------------------------------------------------------------------------
//      Macros
//------------------------------------------------------------------------------

// Set or clear flag(s) in a register
#define SET(register, flags)        ((register) = (register) | (flags))
#define CLEAR(register, flags)      ((register) &= ~(flags))

// Poll the status of flags in a register
#define ISSET(register, flags)      (((register) & (flags)) == (flags))
#define ISCLEARED(register, flags)  (((register) & (flags)) == 0)

// Returns the higher/lower byte of a word
#define HBYTE(word)                 ((unsigned char) ((word) >> 8))
#define LBYTE(word)                 ((unsigned char) ((word) & 0x00FF))

//------------------------------------------------------------------------------
//      Inline functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief  Returns the minimum value between two integers
// \param  dValue1 First value to compare
// \param  dValue2 Second value to compare
// \return Minimum value between two integers
//------------------------------------------------------------------------------
INLINEFUNC unsigned int min(unsigned int dValue1, unsigned int dValue2)
{
    if (dValue1 < dValue2) {

        return dValue1;
    }
    else {

        return dValue2;
    }
}

//------------------------------------------------------------------------------
// \brief  Returns the index of the last set (1) bit in an integer
// \param  dValue Integer value to parse
// \return Position of the leftmost set bit in the integer
//------------------------------------------------------------------------------
INLINEFUNC signed char lastSetBit(unsigned int dValue)
{
    signed char bIndex = -1;

    if (dValue & 0xFFFF0000) {

        bIndex += 16;
        dValue >>= 16;
    }

    if (dValue & 0xFF00) {

        bIndex += 8;
        dValue >>= 8;
    }

    if (dValue & 0xF0) {

        bIndex += 4;
        dValue >>= 4;
    }

    if (dValue & 0xC) {

        bIndex += 2;
        dValue >>= 2;
    }

    if (dValue & 0x2) {

        bIndex += 1;
        dValue >>= 1;
    }

    if (dValue & 0x1) {

        bIndex++;
    }

    return bIndex;
}

#endif // _COMMON_H

/*@}*/

