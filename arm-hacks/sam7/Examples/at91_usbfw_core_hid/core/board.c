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
$Id: board.c 108 2006-10-16 08:33:33Z jjoannic $
*/

//------------------------------------------------------------------------------
//      Includes
//------------------------------------------------------------------------------

#include "common.h"
#include "device.h"
#include "board.h"

//------------------------------------------------------------------------------
// \brief   Indicates the state of the VBus power line associated with the
//          specified interface.
// \param   pInterface Pointer to an USB controller interface
// \return  true if VBus is detected, false otherwise
//------------------------------------------------------------------------------
bool BRD_IsVBusConnected(void *pInterface)
{
#ifdef USB_BUS_POWERED
    return true;
#else
    if (ISSET(AT91F_PIO_GetInput(AT91C_PIO_VBUS), AT91C_VBUS)) {

        return true;
    }
    else {

        return false;
    }
#endif
}

//------------------------------------------------------------------------------
// \brief   Enables the external pull-up on D+ associated with the specified
//          USB controller
// \param   pInterface Pointer to an USB controller interface
//------------------------------------------------------------------------------
void BRD_ConnectPullUp(void *pInterface)
{
#if defined(AT91SAM7SEK) || (defined(AT91SAM7A3EK) && (REVISION == REV_B))
    AT91F_PIO_ClearOutput(AT91C_PIO_PULLUP, AT91C_PULLUP);

#elif (defined(AT91SAM7A3EK) && (REVISION == REV_A)) || defined(AT91RM9200)
    AT91F_PIO_SetOutput(AT91C_PIO_PULLUP, AT91C_PULLUP);
#endif
}

//------------------------------------------------------------------------------
// \brief   Disables the external pull-up on D+ associated with the specified
//          USB controller
// \param   pInterface Pointer to an USB controller interface
//------------------------------------------------------------------------------
void BRD_DisconnectPullUp(void *pInterface)
{
#if defined(AT91SAM7SEK) || (defined(AT91SAM7A3EK) && (REVISION == REV_B))
    AT91F_PIO_SetOutput(AT91C_PIO_PULLUP, AT91C_PULLUP);

#elif (defined(AT91SAM7A3EK) && (REVISION == REV_A)) || defined(AT91RM9200)
    AT91F_PIO_ClearOutput(AT91C_PIO_PULLUP, AT91C_PULLUP);
#endif
}

//------------------------------------------------------------------------------
// \brief   Indicates the state of the external pull-up associated with the
//          specified interface.
// \param   pInterface Pointer to an USB controller interface
// \return  true if the pull-up is currently connected, false otherwise.
//------------------------------------------------------------------------------
bool BRD_IsPullUpConnected(void *pInterface)
{
#if defined(AT91SAM7SEK) || (defined(AT91SAM7A3EK) && (REVISION == REV_B))
    if (ISSET(AT91F_PIO_GetInput(AT91C_PIO_PULLUP), AT91C_PULLUP)) {
#elif (defined(AT91SAM7A3EK) && (REVISION == REV_A)) || defined(AT91RM9200)

    if (!ISSET(AT91F_PIO_GetInput(AT91C_PIO_PULLUP), AT91C_PULLUP)) {
#else
    if (false) {
#endif
        return false;
    }
    else {

        return true;
    }
}

//------------------------------------------------------------------------------
// \brief   Configures the external pull-up on the D+ line associated with
//          the specified USB controller.
// \param   pInterface Pointer to the USB controller interface
//------------------------------------------------------------------------------
void BRD_ConfigurePullUp(void *pInterface)
{
#if defined(AT91SAM7SEK) || defined(AT91SAM7A3EK) || defined(AT91RM9200)
    AT91F_PMC_EnablePeriphClock(AT91C_BASE_PMC, 1 << AT91C_ID_PULLUP);
    AT91F_PIO_CfgOutput(AT91C_PIO_PULLUP, AT91C_PULLUP);
#endif
}

//------------------------------------------------------------------------------
// \brief   Configures the VBus monitoring PIO associated with the specified
//          USB controller.
// \param   pInterface Pointer to the USB controller interface
//------------------------------------------------------------------------------
void BRD_ConfigureVBus(void *pInterface)
{
#if !defined(USB_BUS_POWERED)
    AT91F_PMC_EnablePeriphClock(AT91C_BASE_PMC, 1 << AT91C_ID_VBUS);
    AT91F_PIO_CfgPullup(AT91C_PIO_VBUS, ~AT91C_VBUS);
    AT91F_PIO_CfgInput(AT91C_PIO_VBUS, AT91C_VBUS);
#endif
}

