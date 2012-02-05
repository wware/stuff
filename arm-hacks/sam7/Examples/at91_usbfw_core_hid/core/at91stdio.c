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
$Id: at91stdio.c 108 2006-10-16 08:33:33Z jjoannic $
*/

//------------------------------------------------------------------------------
//      Includes
//------------------------------------------------------------------------------

#include <stdio.h>
#include "common.h"
#include "device.h"

//------------------------------------------------------------------------------
//      Definitions
//------------------------------------------------------------------------------

#define DBGU_BUFFER_SIZE            64

//------------------------------------------------------------------------------
//      Global variables
//------------------------------------------------------------------------------

// \brief  First buffer used with the PDC
static unsigned char pBuffer1[DBGU_BUFFER_SIZE];

// \brief  Second buffer used with the PDC
static unsigned char pBuffer2[DBGU_BUFFER_SIZE];

// \brief  Denotes the current buffer used for storing received characters
static unsigned char *pCurrentBuffer = pBuffer1;

// \brief  Current index in the buffer
static unsigned int dCurrentIndex = 0;

//------------------------------------------------------------------------------
//      Exported functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief  Outputs a character on the DBGU.
//
//         The output pipe must be stdout, otherwise this function does nothing.
//         The data is buffered until the buffer is full or a \n character is
//         received, at which point the PDC starts the actual transfer.
// \param out The character to send
// \param f   Output pipe
// \return The character sent if successful, EOF otherwise.
//------------------------------------------------------------------------------
// mthomas 
#ifdef __GNUC__
int my_fputc(int out, FILE *f)
#else
int fputc(int out, FILE *f)
#endif
{
    if (f == stdout) {

#if defined(AT91SAM9261) || defined (AT91RM9200)
        while (!AT91F_US_TxReady((AT91PS_USART)AT91C_BASE_DBGU));
        AT91F_US_PutChar((AT91PS_USART)AT91C_BASE_DBGU, (char)out);
        return out;
    }
#else
        // Mask interrupts
        // WARNING: This must be done differently with an OS !
        unsigned int mask = AT91C_BASE_AIC->AIC_IMR;
        AT91C_BASE_AIC->AIC_IDCR = 0xFFFFFFFF;

        // Store character in buffer
        pCurrentBuffer[dCurrentIndex] = (unsigned char) out;
        dCurrentIndex++;

        // Check if the buffer is full, or if a '\n' character has been received
        if ((dCurrentIndex == DBGU_BUFFER_SIZE) || (out == '\r')) {

            // Start the transfer with the PDC
            while (!AT91F_PDC_IsTxEmpty((AT91PS_PDC) &(AT91C_BASE_DBGU->DBGU_RPR)));
            AT91F_PDC_SetTx((AT91PS_PDC) &(AT91C_BASE_DBGU->DBGU_RPR),
                            (char *) pCurrentBuffer,
                            dCurrentIndex);

            // Switch buffers
            if (pCurrentBuffer == pBuffer1) {

                pCurrentBuffer = pBuffer2;
            }
            else {

                pCurrentBuffer = pBuffer1;
            }
            dCurrentIndex = 0;
        }

        // Unmask interrupts
        AT91C_BASE_AIC->AIC_IECR = mask;
    }
#endif

    return EOF;
}



