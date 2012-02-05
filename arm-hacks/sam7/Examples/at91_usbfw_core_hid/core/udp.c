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
$Id: udp.c 108 2006-10-16 08:33:33Z jjoannic $
*/

//------------------------------------------------------------------------------
//      Includes
//------------------------------------------------------------------------------

#include "common.h"
#include "device.h"
#include "board.h"
#include "trace.h"
#include "usb.h"

#ifdef UDP

#define UDP_STATE_SHOULD_RECONNECT      0x10000000
#define UDP_EPTYPE_INDEX                8
#define UDP_EPDIR_INDEX                 10

#define ISR_MASK                      0x00003FFF

//------------------------------------------------------------------------------
//      Structures
//------------------------------------------------------------------------------

// \brief  Endpoint states
typedef enum {

    endpointStateDisabled,
    endpointStateIdle,
    endpointStateWrite,
    endpointStateRead,
    endpointStateHalted

} EndpointState_t;

//------------------------------------------------------------------------------
//      Macros
//------------------------------------------------------------------------------

// \brief  Clear flags in the UDP_CSR register and waits for synchronization
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \param  dFlags    Flags to clear
#define UDP_CLEAREPFLAGS(pUsb, bEndpoint, dFlags) { \
    while (!ISCLEARED(UDP_GetDriverInterface(pUsb)->UDP_CSR[bEndpoint], dFlags)) \
        CLEAR(UDP_GetDriverInterface(pUsb)->UDP_CSR[bEndpoint], dFlags); \
}

// \brief  Set flags in the UDP_CSR register and waits for synchronization
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \param  dFlags    Flags to clear
#define UDP_SETEPFLAGS(pUsb, bEndpoint, dFlags) { \
    while (ISCLEARED(UDP_GetDriverInterface(pUsb)->UDP_CSR[bEndpoint], dFlags)) \
        SET(UDP_GetDriverInterface(pUsb)->UDP_CSR[bEndpoint], dFlags); \
}

//------------------------------------------------------------------------------
//      Internal Functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief  Returns a pointer to the UDP controller interface used by an USB
//         driver
//
//         The pointer is cast to the correct type (AT91PS_UDP).
// \param  pUsb Pointer to a S_usb instance
// \return Pointer to the USB controller interface
// \see    S_usb
//------------------------------------------------------------------------------
INLINEFUNC AT91PS_UDP UDP_GetDriverInterface(const S_usb *pUsb)
{
    return (AT91PS_UDP) pUsb->pDriver->pInterface;
}

//------------------------------------------------------------------------------
// \brief  Enables the peripheral clock of the USB controller associated with
//         the specified USB driver
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
INLINEFUNC void UDP_EnableMCK(const S_usb *pUsb)
{
    AT91C_BASE_PMC->PMC_PCER = 1 << USB_GetDriverID(pUsb);
}

//------------------------------------------------------------------------------
// \brief  Disables the peripheral clock of the USB controller associated with
//         the specified USB driver
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
INLINEFUNC void UDP_DisableMCK(const S_usb *pUsb)
{
    AT91C_BASE_PMC->PMC_PCDR = 1 << USB_GetDriverID(pUsb);
}

//------------------------------------------------------------------------------
// \brief  Enables the 48MHz clock of the USB controller associated with
//         the specified USB driver
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
INLINEFUNC void UDP_EnableUDPCK(const S_usb *pUsb)
{
    SET(AT91C_BASE_PMC->PMC_SCER, USB_GetDriverPMC(pUsb));
}

//------------------------------------------------------------------------------
// \brief  Disables the 48MHz clock of the USB controller associated with
//         the specified USB driver
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
INLINEFUNC void UDP_DisableUDPCK(const S_usb *pUsb)
{
    SET(AT91C_BASE_PMC->PMC_SCDR, USB_GetDriverPMC(pUsb));
}

//------------------------------------------------------------------------------
// \brief  Enables the transceiver of the USB controller associated with
//         the specified USB driver
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
INLINEFUNC void UDP_EnableTransceiver(const S_usb *pUsb)
{
    CLEAR(UDP_GetDriverInterface(pUsb)->UDP_TXVC, AT91C_UDP_TXVDIS);
}

//------------------------------------------------------------------------------
// \brief  Disables the transceiver of the USB controller associated with
//         the specified USB driver
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
INLINEFUNC void UDP_DisableTransceiver(const S_usb *pUsb)
{
    SET(UDP_GetDriverInterface(pUsb)->UDP_TXVC, AT91C_UDP_TXVDIS);
}

//------------------------------------------------------------------------------
// \brief  Invokes the callback associated with a finished transfer on an
//         endpoint
// \param  pEndpoint Pointer to a S_usb_endpoint instance
// \param  bStatus   Status code returned by the transfer operation
// \see    Status codes
// \see    S_usb_endpoint
//------------------------------------------------------------------------------
INLINEFUNC void UDP_EndOfTransfer(S_usb_endpoint *pEndpoint,
                                       char bStatus)
{
    if ((pEndpoint->dState == endpointStateWrite)
        || (pEndpoint->dState == endpointStateRead)) {

        TRACE_DEBUG_L("EoT ");

        // Endpoint returns in Idle state
        pEndpoint->dState = endpointStateIdle;

        // Invoke callback is present
        if (pEndpoint->fCallback != 0) {

            pEndpoint->fCallback((unsigned int) pEndpoint->pArgument,
                                 (unsigned int) bStatus,
                                 pEndpoint->dBytesTransferred,
                                 pEndpoint->dBytesRemaining
                                 + pEndpoint->dBytesBuffered);
        }
    }
}

//------------------------------------------------------------------------------
// \brief  Clears the correct RX flag in an endpoint status register
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \see    S_usb_endpoint
// \see    S_usb
//------------------------------------------------------------------------------
static void UDP_ClearRXFlag(const S_usb * pUsb,
                            unsigned char bEndpoint)
{
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);

    // Clear flag
    UDP_CLEAREPFLAGS(pUsb, bEndpoint, pEndpoint->dFlag);

    // Swap banks
    if (pEndpoint->dFlag == AT91C_UDP_RX_DATA_BK0) {

        if (pEndpoint->dNumFIFO > 1) {

            // Swap bank if in dual-fifo mode
            pEndpoint->dFlag = AT91C_UDP_RX_DATA_BK1;
        }
    }
    else {

        pEndpoint->dFlag = AT91C_UDP_RX_DATA_BK0;
    }
}

//------------------------------------------------------------------------------
// \brief  Transfers a data payload from the current tranfer buffer to the
//         endpoint FIFO.
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \return Number of bytes transferred
// \see    S_usb
//------------------------------------------------------------------------------
static unsigned int UDP_WritePayload(const S_usb * pUsb,
                                     unsigned char bEndpoint)
{
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);
    unsigned int   dBytes;
    unsigned int   dCtr;

    // Get the number of bytes to send
    dBytes = min(pEndpoint->wMaxPacketSize, pEndpoint->dBytesRemaining);

    // Transfer one packet in the FIFO buffer
    for (dCtr = 0; dCtr < dBytes; dCtr++) {

        pInterface->UDP_FDR[bEndpoint] = *(pEndpoint->pData);
        pEndpoint->pData++;
    }

    pEndpoint->dBytesBuffered += dBytes;
    pEndpoint->dBytesRemaining -= dBytes;

    return dBytes;
}

//------------------------------------------------------------------------------
// \brief  Transfers a data payload from an endpoint FIFO to the current
//         transfer buffer.
// \param  pUsb        Pointer to a S_usb instance
// \param  bEndpoint   Index of endpoint
// \param  wPacketSize Size of received data packet
// \return Number of bytes transferred
// \see    S_usb
//------------------------------------------------------------------------------
static unsigned int UDP_GetPayload(const S_usb * pUsb,
                                   unsigned char bEndpoint,
                                   unsigned short wPacketSize)
{
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);
    unsigned int   dBytes;
    unsigned int   dCtr;

    // Get number of bytes to retrieve
    dBytes = min(pEndpoint->dBytesRemaining, wPacketSize);

    // Retrieve packet
    for (dCtr = 0; dCtr < dBytes; dCtr++) {

        *pEndpoint->pData = (char) pInterface->UDP_FDR[bEndpoint];
        pEndpoint->pData++;
    }

    pEndpoint->dBytesRemaining -= dBytes;
    pEndpoint->dBytesTransferred += dBytes;
    pEndpoint->dBytesBuffered += wPacketSize - dBytes;

    return dBytes;
}

//------------------------------------------------------------------------------
// \brief  Transfers a received SETUP packet from endpoint 0 FIFO to the
//         S_usb_request structure of an USB driver
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
static void UDP_GetSetup(S_usb const *pUsb)
{
    char *pData = (char *) USB_GetSetup(pUsb);
    AT91PS_UDP pInterface = UDP_GetDriverInterface(pUsb);
    unsigned int dCtr;

    // Copy packet
    for (dCtr = 0; dCtr < 8; dCtr++) {

        *pData = (char) pInterface->UDP_FDR[0];
        pData++;
    }
}

//------------------------------------------------------------------------------
// \brief  This function reset all endpoint transfer descriptors
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
static void UDP_ResetEndpoints(const S_usb *pUsb)
{
    S_usb_endpoint *pEndpoint;
    unsigned char bEndpoint;

    // Reset the transfer descriptor of every endpoint
    for (bEndpoint = 0; bEndpoint < pUsb->dNumEndpoints; bEndpoint++) {

        pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);

        // Reset endpoint transfer descriptor
        pEndpoint->pData = 0;
        pEndpoint->dBytesRemaining = 0;
        pEndpoint->dBytesTransferred = 0;
        pEndpoint->dBytesBuffered = 0;
        pEndpoint->fCallback = 0;
        pEndpoint->pArgument = 0;

        // Configure endpoint characteristics
        pEndpoint->dFlag = AT91C_UDP_RX_DATA_BK0;
        pEndpoint->dState = endpointStateDisabled;
    }
}

//------------------------------------------------------------------------------
// \brief  Disable all endpoints (except control endpoint 0), aborting current
//         transfers if necessary.
// \param  pUsb Pointer to a S_usb instance
//------------------------------------------------------------------------------
static void UDP_DisableEndpoints(const S_usb *pUsb)
{
    S_usb_endpoint *pEndpoint;
    unsigned char bEndpoint;

    // For each endpoint, if it is enabled, disable it and invoke the callback
    // Control endpoint 0 is not disabled
    for (bEndpoint = 1; bEndpoint < pUsb->dNumEndpoints; bEndpoint++) {

        pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);
        UDP_EndOfTransfer(pEndpoint, USB_STATUS_RESET);
        pEndpoint->dState = endpointStateDisabled;
    }
}

//------------------------------------------------------------------------------
// \brief  Endpoint interrupt handler.
//
//         Handle IN/OUT transfers, received SETUP packets and STALLing
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \see    S_usb
//------------------------------------------------------------------------------
static void UDP_EndpointHandler(const S_usb *pUsb, unsigned char bEndpoint)
{
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);
    AT91PS_UDP pInterface = UDP_GetDriverInterface(pUsb);
    unsigned int dStatus = pInterface->UDP_CSR[bEndpoint];

    TRACE_DEBUG_L("Ept%d ", bEndpoint);

    // Handle interrupts
    // IN packet sent
    if (ISSET(dStatus, AT91C_UDP_TXCOMP)) {

        TRACE_DEBUG_L("Wr ");

        // Check that endpoint was in Write state
        if (pEndpoint->dState == endpointStateWrite) {

            // End of transfer ?
            if ((pEndpoint->dBytesBuffered < pEndpoint->wMaxPacketSize)
                ||
                (!ISCLEARED(dStatus, AT91C_UDP_EPTYPE)
                 && (pEndpoint->dBytesRemaining == 0)
                 && (pEndpoint->dBytesBuffered == pEndpoint->wMaxPacketSize))) {

                TRACE_DEBUG_L("%d ", pEndpoint->dBytesBuffered);

                pEndpoint->dBytesTransferred += pEndpoint->dBytesBuffered;
                pEndpoint->dBytesBuffered = 0;

                // Disable interrupt if this is not a control endpoint
                if (!ISCLEARED(dStatus, AT91C_UDP_EPTYPE)) {

                    SET(pInterface->UDP_IDR, 1 << bEndpoint);
                }

                UDP_EndOfTransfer(pEndpoint, USB_STATUS_SUCCESS);
            }
            else {

                // Transfer remaining data
                TRACE_DEBUG_L("%d ", pEndpoint->wMaxPacketSize);

                pEndpoint->dBytesTransferred += pEndpoint->wMaxPacketSize;
                pEndpoint->dBytesBuffered -= pEndpoint->wMaxPacketSize;

                // Send next packet
                if (pEndpoint->dNumFIFO == 1) {

                    // No double buffering
                    UDP_WritePayload(pUsb, bEndpoint);
                    UDP_SETEPFLAGS(pUsb, bEndpoint, AT91C_UDP_TXPKTRDY);
                }
                else {

                    // Double buffering
                    UDP_SETEPFLAGS(pUsb, bEndpoint, AT91C_UDP_TXPKTRDY);
                    UDP_WritePayload(pUsb, bEndpoint);
                }
            }
        }

        // Acknowledge interrupt
        UDP_CLEAREPFLAGS(pUsb, bEndpoint, AT91C_UDP_TXCOMP);
    }
    // OUT packet received
    if (ISSET(dStatus, AT91C_UDP_RX_DATA_BK0)
        || ISSET(dStatus, AT91C_UDP_RX_DATA_BK1)) {

        TRACE_DEBUG_L("Rd ");

        // Check that the endpoint is in Read state
        if (pEndpoint->dState != endpointStateRead) {

            // Endpoint is NOT in Read state
            if (ISCLEARED(dStatus, AT91C_UDP_EPTYPE)
                && ISCLEARED(dStatus, 0xFFFF0000)) {

                // Control endpoint, 0 bytes received
                // Acknowledge the data and finish the current transfer
                TRACE_DEBUG_L("Ack ");
                UDP_ClearRXFlag(pUsb, bEndpoint);

                UDP_EndOfTransfer(pEndpoint, USB_STATUS_SUCCESS);
            }
            else if (ISSET(dStatus, AT91C_UDP_FORCESTALL)) {

                // Non-control endpoint
                // Discard stalled data
                TRACE_DEBUG_L("Disc ");
                UDP_ClearRXFlag(pUsb, bEndpoint);
            }
            else {

                // Non-control endpoint
                // Nak data
                TRACE_DEBUG_L("Nak ");
                SET(pInterface->UDP_IDR, 1 << bEndpoint);
            }
        }
        else {

            // Endpoint is in Read state
            // Retrieve data and store it into the current transfer buffer
            unsigned short wPacketSize = (unsigned short) (dStatus >> 16);

            TRACE_DEBUG_L("%d ", wPacketSize);

            UDP_GetPayload(pUsb, bEndpoint, wPacketSize);
            UDP_ClearRXFlag(pUsb, bEndpoint);

            if ((pEndpoint->dBytesRemaining == 0)
                || (wPacketSize < pEndpoint->wMaxPacketSize)) {

                // Disable interrupt if this is not a control endpoint
                if (!ISCLEARED(dStatus, AT91C_UDP_EPTYPE)) {

                    SET(pInterface->UDP_IDR, 1 << bEndpoint);
                }

                UDP_EndOfTransfer(pEndpoint, USB_STATUS_SUCCESS);
            }
        }
    }
    // SETUP packet received
    if (ISSET(dStatus, AT91C_UDP_RXSETUP)) {

        TRACE_DEBUG_L("Stp ");

        // If a transfer was pending, complete it
        // Handle the case where during the status phase of a control write
        // transfer, the host receives the device ZLP and ack it, but the ack
        // is not received by the device
        if ((pEndpoint->dState == endpointStateWrite)
            || (pEndpoint->dState == endpointStateRead)) {

            UDP_EndOfTransfer(pEndpoint, USB_STATUS_SUCCESS);
        }

        // Copy the setup packet in S_usb
        UDP_GetSetup(pUsb);

        // Set the DIR bit before clearing RXSETUP in Control IN sequence
        if (USB_GetSetup(pUsb)->bmRequestType & 0x80) {

            UDP_SETEPFLAGS(pUsb, bEndpoint, AT91C_UDP_DIR);
        }

        UDP_CLEAREPFLAGS(pUsb, bEndpoint, AT91C_UDP_RXSETUP);

        // Forward the request to the upper layer
        USB_NewRequestCallback(pUsb);
    }
    // STALL sent
    if (ISSET(dStatus, AT91C_UDP_STALLSENT)) {

        TRACE_WARNING("Sta ");

        // Acknowledge the stall flag
        UDP_CLEAREPFLAGS(pUsb, bEndpoint, AT91C_UDP_STALLSENT);

        // If the endpoint is not halted, clear the stall condition
        if (pEndpoint->dState != endpointStateHalted) {

            UDP_CLEAREPFLAGS(pUsb, bEndpoint, AT91C_UDP_FORCESTALL);
        }
    }
}

//------------------------------------------------------------------------------
//      Exported functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief  Configure an endpoint with the provided endpoint descriptor
// \param  pUsb    Pointer to a S_usb instance
// \param  pEpDesc Pointer to the endpoint descriptor
// \return true if the endpoint is now configured, false otherwise
// \see    S_usb_endpoint_descriptor
// \see    S_usb
//------------------------------------------------------------------------------
bool UDP_ConfigureEndpoint(const S_usb                     *pUsb,
                           const S_usb_endpoint_descriptor *pEpDesc)
{
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);
    S_usb_endpoint *pEndpoint;
    unsigned char  bEndpoint;
    unsigned char  bType;
    bool           isINEndpoint;
    unsigned int   dFlags;

    // NULL descriptor -> Control endpoint 0
    if (pEpDesc == 0) {

        bEndpoint = 0;
        bType = ENDPOINT_TYPE_CONTROL;
        isINEndpoint = false;
    }
    else {

        bEndpoint = (unsigned char) (pEpDesc->bEndpointAddress & 0x7);
        bType = (unsigned char) (pEpDesc->bmAttributes & 0x3);

        if (ISSET(pEpDesc->bEndpointAddress, 1 << 7)) {

            isINEndpoint = true;
        }
        else {

            isINEndpoint = false;
        }
    }

    // Get pointer on endpoint
    pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);
    if (pEndpoint == 0) {

        return false;
    }

    // Configure wMaxPacketSize
    if (pEpDesc != 0) {

        pEndpoint->wMaxPacketSize = pEpDesc->wMaxPacketSize;
    }
    else {

        pEndpoint->wMaxPacketSize = USB_ENDPOINT0_MAXPACKETSIZE;
    }

    // Abort the current transfer is the endpoint was configured and in
    // Write or Read state
    if ((pEndpoint->dState == endpointStateRead)
        || (pEndpoint->dState == endpointStateWrite)) {

        UDP_EndOfTransfer(pEndpoint, USB_STATUS_RESET);
    }

    // Enter IDLE state
    pEndpoint->dState = endpointStateIdle;

    // Reset Endpoint Fifos
    SET(pInterface->UDP_RSTEP, 1 << bEndpoint);
    CLEAR(pInterface->UDP_RSTEP, 1 << bEndpoint);

    // Configure endpoint
    dFlags = AT91C_UDP_EPEDS;
    SET(dFlags, bType << UDP_EPTYPE_INDEX);

    if (isINEndpoint) {

        SET(dFlags, 1 << UDP_EPDIR_INDEX);
    }

    if (bType == ENDPOINT_TYPE_CONTROL) {

        SET(pInterface->UDP_IER, 1 << bEndpoint);
    }

    TRACE_DEBUG_L("CfgEpt%d ", bEndpoint);

    UDP_SETEPFLAGS(pUsb, bEndpoint, dFlags);

    return true;
}

//------------------------------------------------------------------------------
// \brief  UDP interrupt handler
//
//         Manages device resume, suspend, end of bus reset. Forwards endpoint
//         interrupts to the appropriate handler.
// \param  pUsb Pointer to a S_usb instance
//------------------------------------------------------------------------------
void UDP_Handler(const S_usb *pUsb)
{
    AT91PS_UDP          pInterface = UDP_GetDriverInterface(pUsb);
    unsigned int        dStatus;
    unsigned char       bEndpoint;

    if ( (!ISSET(USB_GetState(pUsb), USB_STATE_SUSPENDED))
       && (ISSET(USB_GetState(pUsb), USB_STATE_POWERED))){

        LED_TOGGLE(LED_USB);
    }

    TRACE_DEBUG_L("Hlr ");

    // Get interrupts status
    dStatus = pInterface->UDP_ISR & pInterface->UDP_IMR & ISR_MASK;

    // Handle all UDP interrupts
    while (dStatus != 0) {

        // Start Of Frame (SOF)
        if (ISSET(dStatus, AT91C_UDP_SOFINT)) {

            TRACE_DEBUG_L("SOF ");

            // Invoke the SOF callback
            USB_StartOfFrameCallback(pUsb);

            // Acknowledge interrupt
            SET(pInterface->UDP_ICR, AT91C_UDP_SOFINT);
            CLEAR(dStatus, AT91C_UDP_SOFINT);
        }

        // Suspend
        if (dStatus == AT91C_UDP_RXSUSP) {

            TRACE_DEBUG_L("Susp ");

            if (!ISSET(USB_GetState(pUsb), USB_STATE_SUSPENDED)) {

                // The device enters the Suspended state
                //      MCK + UDPCK must be off
                //      Pull-Up must be connected
                //      Transceiver must be disabled

                // Enable wakeup
                SET(pInterface->UDP_IER, AT91C_UDP_WAKEUP | AT91C_UDP_RXRSM);

                // Acknowledge interrupt
                SET(pInterface->UDP_ICR, AT91C_UDP_RXSUSP);

                SET(*(pUsb->pState), USB_STATE_SUSPENDED);
                UDP_DisableTransceiver(pUsb);
                UDP_DisableMCK(pUsb);
                UDP_DisableUDPCK(pUsb);

                // Invoke the Suspend callback
                USB_SuspendCallback(pUsb);

            }
        }
        // Resume
        else if (ISSET(dStatus, AT91C_UDP_WAKEUP)
              || ISSET(dStatus, AT91C_UDP_RXRSM)) {

            // Invoke the Resume callback
            USB_ResumeCallback(pUsb);

            TRACE_DEBUG_L("Res ");

            // The device enters Configured state
            //      MCK + UDPCK must be on
            //      Pull-Up must be connected
            //      Transceiver must be enabled

            if (ISSET(USB_GetState(pUsb), USB_STATE_SUSPENDED)) {

                // Powered state
                UDP_EnableMCK(pUsb);
                UDP_EnableUDPCK(pUsb);

                // Default state
                if (ISSET(USB_GetState(pUsb), USB_STATE_DEFAULT)) {

                    UDP_EnableTransceiver(pUsb);
                }

                CLEAR(*(pUsb->pState), USB_STATE_SUSPENDED);
            }
            SET(pInterface->UDP_ICR,
                AT91C_UDP_WAKEUP | AT91C_UDP_RXRSM | AT91C_UDP_RXSUSP);
            SET(pInterface->UDP_IDR, AT91C_UDP_WAKEUP | AT91C_UDP_RXRSM);
        }
        // End of bus reset
        else if (ISSET(dStatus, AT91C_UDP_ENDBUSRES)) {

            TRACE_DEBUG_L("EoBRes ");

            // The device enters the Default state
            //      MCK + UDPCK are already enabled
            //      Pull-Up is already connected
            //      Transceiver must be enabled
            //      Endpoint 0 must be enabled
            SET(*(pUsb->pState), USB_STATE_DEFAULT);
            UDP_EnableTransceiver(pUsb);

            // The device leaves the Address & Configured states
            CLEAR(*(pUsb->pState), USB_STATE_ADDRESS | USB_STATE_CONFIGURED);
            UDP_ResetEndpoints(pUsb);
            UDP_DisableEndpoints(pUsb);
            UDP_ConfigureEndpoint(pUsb, 0);

            // Flush and enable the Suspend interrupt
            SET(pInterface->UDP_ICR,
                AT91C_UDP_WAKEUP | AT91C_UDP_RXRSM | AT91C_UDP_RXSUSP);

            // Enable the Start Of Frame (SOF) interrupt if needed
            if (pUsb->pCallbacks->startOfFrame != 0) {

                SET(pInterface->UDP_IER, AT91C_UDP_SOFINT);
            }

            // Invoke the Reset callback
            USB_ResetCallback(pUsb);

            // Acknowledge end of bus reset interrupt
            SET(pInterface->UDP_ICR, AT91C_UDP_ENDBUSRES);
        }
        // Endpoint interrupts
        else {

            while (dStatus != 0) {

                // Get endpoint index
                bEndpoint = lastSetBit(dStatus);
                UDP_EndpointHandler(pUsb, bEndpoint);

                CLEAR(pInterface->UDP_CSR[bEndpoint],
                      AT91C_UDP_TXCOMP | AT91C_UDP_RX_DATA_BK0
                    | AT91C_UDP_RX_DATA_BK1 | AT91C_UDP_RXSETUP
                    | AT91C_UDP_STALLSENT);

                CLEAR(dStatus, 1 << bEndpoint);
            }
        }

        // Retrieve new interrupt status
        dStatus = pInterface->UDP_ISR & pInterface->UDP_IMR & ISR_MASK;

        // Mask unneeded interrupts
        if (!ISSET(USB_GetState(pUsb), USB_STATE_DEFAULT)) {

            dStatus &= AT91C_UDP_ENDBUSRES | AT91C_UDP_SOFINT;
        }

        TRACE_DEBUG_ALL("\n\r");
        if (dStatus != 0) {

            TRACE_DEBUG_L("  - ");
        }
    }

    if ( (!ISSET(USB_GetState(pUsb), USB_STATE_SUSPENDED))
       && (ISSET(USB_GetState(pUsb), USB_STATE_POWERED))){

        LED_TOGGLE(LED_USB);
    }
}

//------------------------------------------------------------------------------
// \brief  Sends data through an USB endpoint
//
//         Sets up the transfer descriptor, write one or two data payloads
//         (depending on the number of FIFO banks for the endpoint) and then
//         starts the actual transfer. The operation is complete when all
//         the data has been sent.
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \param  pData     Pointer to a buffer containing the data to send
// \param  dLength   Length of the data buffer
// \param  fCallback Optional function to invoke when the transfer finishes
// \param  pArgument Optional argument for the callback function
// \return Operation result code
// \see    Operation result codes
// \see    Callback_f
// \see    S_usb
//------------------------------------------------------------------------------
char UDP_Write(const S_usb   *pUsb,
               unsigned char bEndpoint,
               const void    *pData,
               unsigned int  dLength,
               Callback_f    fCallback,
               void          *pArgument)
{
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);

    // Check that the endpoint is in Idle state
    if (pEndpoint->dState != endpointStateIdle) {

        return USB_STATUS_LOCKED;
    }

    TRACE_DEBUG_L("Write%d(%d) ", bEndpoint, dLength);

    // Setup the transfer descriptor
    pEndpoint->pData = (char *) pData;
    pEndpoint->dBytesRemaining = dLength;
    pEndpoint->dBytesBuffered = 0;
    pEndpoint->dBytesTransferred = 0;
    pEndpoint->fCallback = fCallback;
    pEndpoint->pArgument = pArgument;

    // Send one packet
    pEndpoint->dState = endpointStateWrite;
    UDP_WritePayload(pUsb, bEndpoint);
    UDP_SETEPFLAGS(pUsb, bEndpoint, AT91C_UDP_TXPKTRDY);

    // If double buffering is enabled and there is data remaining,
    // prepare another packet
    if ((pEndpoint->dNumFIFO > 1) && (pEndpoint->dBytesRemaining > 0)) {

        UDP_WritePayload(pUsb, bEndpoint);
    }

    // Enable interrupt on endpoint
    SET(pInterface->UDP_IER, 1 << bEndpoint);

    return USB_STATUS_SUCCESS;
}

//------------------------------------------------------------------------------
// \brief  Reads incoming data on an USB endpoint
//
//         This methods sets the transfer descriptor and activate the endpoint
//         interrupt. The actual transfer is then carried out by the endpoint
//         interrupt handler. The Read operation finishes either when the
//         buffer is full, or a short packet (inferior to endpoint maximum
//         packet size) is received.
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \param  pData     Pointer to a buffer to store the received data
// \param  dLength   Length of the receive buffer
// \param  fCallback Optional callback function
// \param  pArgument Optional callback argument
// \return Operation result code
// \see    Callback_f
// \see    S_usb
//------------------------------------------------------------------------------
char UDP_Read(const S_usb   *pUsb,
              unsigned char bEndpoint,
              void          *pData,
              unsigned int  dLength,
              Callback_f    fCallback,
              void          *pArgument)
{
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);

    //! Return if the endpoint is not in IDLE state
    if (pEndpoint->dState != endpointStateIdle) {

        return USB_STATUS_LOCKED;
    }

    TRACE_DEBUG_L("Read%d(%d) ", bEndpoint, dLength);

    // Endpoint enters Read state
    pEndpoint->dState = endpointStateRead;

    // Set the transfer descriptor
    pEndpoint->pData = (char *) pData;
    pEndpoint->dBytesRemaining = dLength;
    pEndpoint->dBytesBuffered = 0;
    pEndpoint->dBytesTransferred = 0;
    pEndpoint->fCallback = fCallback;
    pEndpoint->pArgument = pArgument;

    // Enable interrupt on endpoint
    SET(pInterface->UDP_IER, 1 << bEndpoint);

    return USB_STATUS_SUCCESS;
}

//------------------------------------------------------------------------------
// \brief  Clears, sets or returns the Halt state on specified endpoint
//
//         When in Halt state, an endpoint acknowledges every received packet
//         with a STALL handshake. This continues until the endpoint is
//         manually put out of the Halt state by calling this function.
// \param  pUsb Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \param  bRequest  Request to perform
//                   -> USB_SET_FEATURE, USB_CLEAR_FEATURE, USB_GET_STATUS
// \return true if the endpoint is currently Halted, false otherwise
// \see    S_usb
//------------------------------------------------------------------------------
bool UDP_Halt(const S_usb   *pUsb,
              unsigned char bEndpoint,
              unsigned char bRequest)
{
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);

    // Clear the Halt feature of the endpoint if it is enabled
    if ((bRequest == USB_CLEAR_FEATURE)
        && (pEndpoint->dState == endpointStateHalted)) {

        TRACE_DEBUG_L("Unhalt%d ", bEndpoint);

        // Return endpoint to Idle state
        pEndpoint->dState = endpointStateIdle;

        // Clear FORCESTALL flag
        UDP_CLEAREPFLAGS(pUsb, bEndpoint, AT91C_UDP_FORCESTALL);

        // Reset Endpoint Fifos, beware this is a 2 steps operation
        SET(pInterface->UDP_RSTEP, 1 << bEndpoint);
        CLEAR(pInterface->UDP_RSTEP, 1 << bEndpoint);
    }
    // Set the Halt feature on the endpoint if it is not already enabled
    // and the endpoint is not disabled
    else if ((bRequest == USB_SET_FEATURE)
             && (pEndpoint->dState != endpointStateHalted)
             && (pEndpoint->dState != endpointStateDisabled)) {

        TRACE_DEBUG_L("Halt%d ", bEndpoint);

        // Abort the current transfer if necessary
        UDP_EndOfTransfer(pEndpoint, USB_STATUS_ABORTED);

        // Put endpoint into Halt state
        UDP_SETEPFLAGS(pUsb, bEndpoint, AT91C_UDP_FORCESTALL);
        pEndpoint->dState = endpointStateHalted;

        // Enable the endpoint interrupt
        SET(pInterface->UDP_IER, 1 << bEndpoint);
    }

    // Return the endpoint halt status
    if (pEndpoint->dState == endpointStateHalted) {

        return true;
    }
    else {

        return false;
    }
}

//------------------------------------------------------------------------------
// \brief  Causes the endpoint to acknowledge the next received packet with
//         a STALL handshake.
//
//         Further packets are then handled normally.
// \param  pUsb      Pointer to a S_usb instance
// \param  bEndpoint Index of endpoint
// \return Operation result code
// \see    S_usb
//------------------------------------------------------------------------------
char UDP_Stall(const S_usb *pUsb,
               unsigned char bEndpoint)
{
    S_usb_endpoint *pEndpoint = USB_GetEndpoint(pUsb, bEndpoint);

    // Check that endpoint is in Idle state
    if (pEndpoint->dState != endpointStateIdle) {

        TRACE_WARNING("W: UDP_Stall: Endpoint%d locked\n\r", bEndpoint);
        return USB_STATUS_LOCKED;
    }

    TRACE_DEBUG_L("Stall%d ", bEndpoint);

    UDP_SETEPFLAGS(pUsb, bEndpoint, AT91C_UDP_FORCESTALL);

    return USB_STATUS_SUCCESS;
}

//------------------------------------------------------------------------------
// \brief  Activates a remote wakeup procedure
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
void UDP_RemoteWakeUp(const S_usb *pUsb)
{
    AT91PS_UDP pInterface = UDP_GetDriverInterface(pUsb);

    UDP_EnableMCK(pUsb);
    UDP_EnableUDPCK(pUsb);
    UDP_EnableTransceiver(pUsb);

    TRACE_DEBUG_L("Remote WakeUp ");

    // Activates a remote wakeup (edge on ESR)
    SET(pInterface->UDP_GLBSTATE, AT91C_UDP_ESR);
    // Then clear ESR
    CLEAR(pInterface->UDP_GLBSTATE, AT91C_UDP_ESR);
}

//------------------------------------------------------------------------------
// \brief  Handles attachment or detachment from the USB when the VBus power
//         line status changes.
// \param  pUsb Pointer to a S_usb instance
// \return true if VBus is present, false otherwise
// \see    S_usb
//------------------------------------------------------------------------------
bool UDP_Attach(const S_usb *pUsb)
{
    AT91PS_UDP pInterface = UDP_GetDriverInterface(pUsb);

    TRACE_DEBUG_L("Attach(");

    // Check if VBus is present
    if (!ISSET(USB_GetState(pUsb), USB_STATE_POWERED)
        && BRD_IsVBusConnected(pInterface)) {

        // Powered state:
        //      MCK + UDPCK must be on
        //      Pull-Up must be connected
        //      Transceiver must be disabled

        // Invoke the Resume callback
        USB_ResumeCallback(pUsb);

        UDP_EnableMCK(pUsb);
        UDP_EnableUDPCK(pUsb);

        // Reconnect the pull-up if needed
        if (ISSET(*(pUsb->pState), UDP_STATE_SHOULD_RECONNECT)) {

            USB_Connect(pUsb);
            CLEAR(*(pUsb->pState), UDP_STATE_SHOULD_RECONNECT);
        }

        // Clear the Suspend and Resume interrupts
        SET(pInterface->UDP_ICR,
            AT91C_UDP_WAKEUP | AT91C_UDP_RXRSM | AT91C_UDP_RXSUSP);

        SET(pInterface->UDP_IER, AT91C_UDP_RXSUSP);

        // The device is in Powered state
        SET(*(pUsb->pState), USB_STATE_POWERED);

    }
    else if (ISSET(USB_GetState(pUsb), USB_STATE_POWERED)
             && !BRD_IsVBusConnected(pInterface)) {

        // Attached state:
        //      MCK + UDPCK off
        //      Pull-Up must be disconnected
        //      Transceiver must be disabled

        // Warning: MCK must be enabled to be able to write in UDP registers
        // It may have been disabled by the Suspend interrupt, so re-enable it
        UDP_EnableMCK(pUsb);

        // Disable interrupts
        SET(pInterface->UDP_IDR, AT91C_UDP_WAKEUP | AT91C_UDP_RXRSM
                               | AT91C_UDP_RXSUSP | AT91C_UDP_SOFINT);

        UDP_DisableEndpoints(pUsb);
        UDP_DisableTransceiver(pUsb);

        // Disconnect the pull-up if needed
        if (ISSET(USB_GetState(pUsb), USB_STATE_DEFAULT)) {

            USB_Disconnect(pUsb);
            SET(*(pUsb->pState), UDP_STATE_SHOULD_RECONNECT);
        }

        UDP_DisableMCK(pUsb);
        UDP_DisableUDPCK(pUsb);

        // The device leaves the all states except Attached
        CLEAR(*(pUsb->pState), USB_STATE_POWERED | USB_STATE_DEFAULT
              | USB_STATE_ADDRESS | USB_STATE_CONFIGURED | USB_STATE_SUSPENDED);

        // Invoke the Suspend callback
        USB_SuspendCallback(pUsb);
    }

    TRACE_DEBUG_L("%d) ", ISSET(USB_GetState(pUsb), USB_STATE_POWERED));

    return ISSET(USB_GetState(pUsb), USB_STATE_POWERED);
}

//------------------------------------------------------------------------------
// \brief  Sets or unsets the device address
//
//         This function directly accesses the S_usb_request instance located
//         in the S_usb structure to extract its new address.
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
void UDP_SetAddress(S_usb const *pUsb)
{
    unsigned short wAddress = USB_GetSetup(pUsb)->wValue;
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);

    TRACE_DEBUG_L("SetAddr(%d) ", wAddress);

    // Set address
    SET(pInterface->UDP_FADDR, AT91C_UDP_FEN | wAddress);

    if (wAddress == 0) {

        SET(pInterface->UDP_GLBSTATE, 0);

        // Device enters the Default state
        CLEAR(*(pUsb->pState), USB_STATE_ADDRESS);
    }
    else {

        SET(pInterface->UDP_GLBSTATE, AT91C_UDP_FADDEN);

        // The device enters the Address state
        SET(*(pUsb->pState), USB_STATE_ADDRESS);
    }
}

//------------------------------------------------------------------------------
// \brief  Changes the device state from Address to Configured, or from
//         Configured to Address.
//
//         This method directly access the last received SETUP packet to
//         decide on what to do.
// \see    S_usb
//------------------------------------------------------------------------------
void UDP_SetConfiguration(S_usb const *pUsb)
{
    unsigned short wValue = USB_GetSetup(pUsb)->wValue;
    AT91PS_UDP     pInterface = UDP_GetDriverInterface(pUsb);

    TRACE_DEBUG_L("SetCfg() ");

    // Check the request
    if (wValue != 0) {

        // Enter Configured state
        SET(*(pUsb->pState), USB_STATE_CONFIGURED);
        SET(pInterface->UDP_GLBSTATE, AT91C_UDP_CONFG);
    }
    else {

        // Go back to Address state
        CLEAR(*(pUsb->pState), USB_STATE_CONFIGURED);
        SET(pInterface->UDP_GLBSTATE, AT91C_UDP_FADDEN);

        // Abort all transfers
        UDP_DisableEndpoints(pUsb);
    }
}

//------------------------------------------------------------------------------
// \brief  Enables the pull-up on the D+ line to connect the device to the USB.
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
void UDP_Connect(const S_usb *pUsb)
{
#if defined(UDP_INTERNAL_PULLUP)
    SET(UDP_GetDriverInterface(pUsb)->UDP_TXVC, AT91C_UDP_PUON);

#elif defined(UDP_INTERNAL_PULLUP_BY_MATRIX)
    TRACE_DEBUG_L("PUON 1\n\r");
    AT91C_BASE_MATRIX->MATRIX_USBPCR |= AT91C_MATRIX_USBPCR_PUON;

#else
    BRD_ConnectPullUp(UDP_GetDriverInterface(pUsb));

#endif
}

//------------------------------------------------------------------------------
// \brief  Disables the pull-up on the D+ line to disconnect the device from
//         the bus.
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
void UDP_Disconnect(const S_usb *pUsb)
{
#if defined(UDP_INTERNAL_PULLUP)
    CLEAR(UDP_GetDriverInterface(pUsb)->UDP_TXVC, AT91C_UDP_PUON);

#elif defined(UDP_INTERNAL_PULLUP_BY_MATRIX)
    TRACE_DEBUG_L("PUON 0\n\r");
    AT91C_BASE_MATRIX->MATRIX_USBPCR &= ~AT91C_MATRIX_USBPCR_PUON;

#else
    BRD_DisconnectPullUp(UDP_GetDriverInterface(pUsb));

#endif
    // Device leaves the Default state
    CLEAR(*(pUsb->pState), USB_STATE_DEFAULT);
}

//------------------------------------------------------------------------------
// \brief  Initializes the specified USB driver
//
//         This function initializes the current FIFO bank of endpoints,
//         configures the pull-up and VBus lines, disconnects the pull-up and
//         then trigger the Init callback.
// \param  pUsb Pointer to a S_usb instance
// \see    S_usb
//------------------------------------------------------------------------------
void UDP_Init(const S_usb *pUsb)
{
    unsigned int dIndex;
    AT91PS_UDP   pInterface = UDP_GetDriverInterface(pUsb);

    TRACE_DEBUG_L("Init()\n\r");

    // Init data banks
    for (dIndex = 0; dIndex < pUsb->dNumEndpoints; dIndex++) {

        pUsb->pEndpoints[dIndex].dFlag = AT91C_UDP_RX_DATA_BK0;
    }

    // External pull-up on D+
    // Configure
    BRD_ConfigurePullUp(pInterface);

    // Disable
    UDP_Disconnect(pUsb);

    // Device is in the Attached state
    *(pUsb->pState) = USB_STATE_ATTACHED;

    // Disable the UDP transceiver and interrupts
    UDP_EnableMCK(pUsb);
    SET(pInterface->UDP_IDR, AT91C_UDP_RXRSM);
    UDP_Connect(pUsb);
    UDP_DisableTransceiver(pUsb);
    UDP_DisableMCK(pUsb);
    UDP_Disconnect(pUsb);

    // Configure interrupts
    USB_InitCallback(pUsb);
}

//------------------------------------------------------------------------------
//      Global variables
//------------------------------------------------------------------------------

// \brief Low-level driver methods to use with the UDP USB controller
// \see S_driver_methods
const S_driver_methods sUDPMethods = {

    UDP_Init,
    UDP_Write,
    UDP_Read,
    UDP_Stall,
    UDP_Halt,
    UDP_RemoteWakeUp,
    UDP_ConfigureEndpoint,
    UDP_Attach,
    UDP_SetAddress,
    UDP_SetConfiguration,
    UDP_Handler,
    UDP_Connect,
    UDP_Disconnect
};

// \brief  Default driver when an UDP controller is present on a chip
const S_usb_driver sDefaultDriver = {

    AT91C_BASE_UDP,
    0,
    0,
    AT91C_ID_UDP,
    AT91C_PMC_UDP,
    &sUDPMethods
};

#endif // UDP


