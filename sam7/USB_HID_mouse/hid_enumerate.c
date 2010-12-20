//*----------------------------------------------------------------------------
//*      ATMEL Microcontroller Software Support  -  ROUSSET  -
//*----------------------------------------------------------------------------
//* The software is delivered "AS IS" without warranty or condition of any
//* kind, either express, implied or statutory. This includes without
//* limitation any warranty or condition with respect to merchantability or
//* fitness for any particular purpose, or against the infringements of
//* intellectual property rights of others.
//*----------------------------------------------------------------------------
//* File Name           : cdc_enumerate.c
//* Object              : Handle HID enumeration
//*
//* 1.0 Oct 05 2004 	: ODi Creation
//*----------------------------------------------------------------------------
#include "board.h"
#include "usb_descriptors.h"
#include "hid_enumerate.h"

typedef unsigned char  uchar;
typedef unsigned short ushort;
typedef unsigned int   uint;

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define EP_NUMBER 1

/* USB standard request code */
#define STD_GET_STATUS_ZERO           0x0080
#define STD_GET_STATUS_INTERFACE      0x0081
#define STD_GET_STATUS_ENDPOINT       0x0082

#define STD_CLEAR_FEATURE_ZERO        0x0100
#define STD_CLEAR_FEATURE_INTERFACE   0x0101
#define STD_CLEAR_FEATURE_ENDPOINT    0x0102

#define STD_SET_FEATURE_ZERO          0x0300
#define STD_SET_FEATURE_INTERFACE     0x0301
#define STD_SET_FEATURE_ENDPOINT      0x0302

#define STD_SET_ADDRESS               0x0500
#define STD_GET_DESCRIPTOR            0x0680
#define STD_SET_DESCRIPTOR            0x0700
#define STD_GET_CONFIGURATION         0x0880
#define STD_SET_CONFIGURATION         0x0900
#define STD_GET_INTERFACE             0x0A81
#define STD_SET_INTERFACE             0x0B01
#define STD_SYNCH_FRAME               0x0C82

/* HID Class Specific Request Code */
#define STD_GET_HID_DESCRIPTOR        0x0681
#define STD_SET_IDLE                  0x0A21

static uchar AT91F_UDP_IsConfigured(AT91PS_HID);
static void AT91F_HID_SendReport(AT91PS_HID, char *report, int length);
static void setup_packet_handler(AT91PS_HID);


//*----------------------------------------------------------------------------
//* \fn    AT91F_HID_Open
//* \brief
//*----------------------------------------------------------------------------
AT91PS_HID AT91F_HID_Open(AT91PS_HID pHid, AT91PS_UDP pUdp)
{
    pHid->pUdp = pUdp;
    pHid->currentConfiguration = 0;
    pHid->IsConfigured = AT91F_UDP_IsConfigured;
    pHid->SendReport   = AT91F_HID_SendReport;
    return pHid;
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_UDP_IsConfigured
//* \brief Test if the device is configured and handle enumeration
//*----------------------------------------------------------------------------
static uchar AT91F_UDP_IsConfigured(AT91PS_HID pHid)
{
    AT91PS_UDP pUDP;
    AT91_REG isr;
    pUDP = pHid->pUdp;
    isr = pUDP->UDP_ISR;
    if (isr & AT91C_UDP_ENDBUSRES) {
        // handle an end-of-bus-reset interrupt
        pUDP->UDP_ICR = AT91C_UDP_ENDBUSRES;
        // reset all endpoints
        pUDP->UDP_RSTEP  = 0xf;
        pUDP->UDP_RSTEP  = 0;
        // Enable the function
        pUDP->UDP_FADDR = AT91C_UDP_FEN;
        // Configure endpoint 0
        pUDP->UDP_CSR[0] = (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_CTRL);
    }
    else if (isr & AT91C_UDP_EPINT0) {
        // handle an endpoint zero interrupt
        pUDP->UDP_ICR = AT91C_UDP_EPINT0;
        setup_packet_handler(pHid);
    }
    return pHid->currentConfiguration;
}


//*----------------------------------------------------------------------------
//* \fn    AT91F_HID_SendCoordinates
//* \brief Send Data through endpoint 1
//*----------------------------------------------------------------------------
static void AT91F_HID_SendReport(AT91PS_HID pHid, char *report, int reportSize)
{
    int i;
    AT91PS_UDP pUdp = pHid->pUdp;

    // Send report to the host
    while (reportSize--)
        *((volatile char *) (pUdp->UDP_FDR + EP_NUMBER)) = *report++;
    pUdp->UDP_CSR[EP_NUMBER] |= AT91C_UDP_TXPKTRDY;

    // Wait for the end of transmission
    while ( !(pUdp->UDP_CSR[EP_NUMBER] & AT91C_UDP_TXCOMP) )
        pHid->IsConfigured(pHid);

    // Clear AT91C_UDP_TXCOMP flag
    if (pUdp->UDP_CSR[EP_NUMBER] & AT91C_UDP_TXCOMP) {
        pUdp->UDP_CSR[EP_NUMBER] &= ~AT91C_UDP_TXCOMP;
        // then wait for it to be recognized as cleared
        while (pUdp->UDP_CSR[EP_NUMBER] & AT91C_UDP_TXCOMP);
    }
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_USB_SendData
//* \brief Send Data through the control endpoint
//*----------------------------------------------------------------------------
static void AT91F_USB_SendData(AT91PS_UDP pUdp, const char *pData, uint length)
{
    uint cpt = 0;
    AT91_REG csr;

    do {
        cpt = MIN(length, 8);
        length -= cpt;

        while (cpt--)
            pUdp->UDP_FDR[0] = *pData++;

        if (pUdp->UDP_CSR[0] & AT91C_UDP_TXCOMP) {
            pUdp->UDP_CSR[0] &= ~(AT91C_UDP_TXCOMP);
            while (pUdp->UDP_CSR[0] & AT91C_UDP_TXCOMP);
        }

        pUdp->UDP_CSR[0] |= AT91C_UDP_TXPKTRDY;
        do {
            csr = pUdp->UDP_CSR[0];
            // Data IN stage has been stopped by a status OUT
            if (csr & AT91C_UDP_RX_DATA_BK0) {
                pUdp->UDP_CSR[0] &= ~(AT91C_UDP_RX_DATA_BK0);
                return;
            }
        } while ( !(csr & AT91C_UDP_TXCOMP) );

    } while (length);

    if (pUdp->UDP_CSR[0] & AT91C_UDP_TXCOMP) {
        pUdp->UDP_CSR[0] &= ~(AT91C_UDP_TXCOMP);
        while (pUdp->UDP_CSR[0] & AT91C_UDP_TXCOMP);
    }
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_USB_SendZlp
//* \brief Send zero length packet through the control endpoint
//*----------------------------------------------------------------------------
void AT91F_USB_SendZlp(AT91PS_UDP pUdp)
{
    pUdp->UDP_CSR[0] |= AT91C_UDP_TXPKTRDY;
    while ( !(pUdp->UDP_CSR[0] & AT91C_UDP_TXCOMP) );
    pUdp->UDP_CSR[0] &= ~(AT91C_UDP_TXCOMP);
    while (pUdp->UDP_CSR[0] & AT91C_UDP_TXCOMP);
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_USB_SendStall
//* \brief Stall the control endpoint
//*----------------------------------------------------------------------------
void AT91F_USB_SendStall(AT91PS_UDP pUdp)
{
    pUdp->UDP_CSR[0] |= AT91C_UDP_FORCESTALL;
    while ( !(pUdp->UDP_CSR[0] & AT91C_UDP_ISOERROR) );
    pUdp->UDP_CSR[0] &= ~(AT91C_UDP_FORCESTALL | AT91C_UDP_ISOERROR);
    while (pUdp->UDP_CSR[0] & (AT91C_UDP_FORCESTALL | AT91C_UDP_ISOERROR));
}

//*----------------------------------------------------------------------------
//* \fn    setup_packet_handler
//* \brief This function is a callback invoked when a SETUP packet is received
//*----------------------------------------------------------------------------
static void setup_packet_handler(AT91PS_HID pHid)
{
    AT91PS_UDP pUDP = pHid->pUdp;
    uchar bmRequestType, bRequest;
    ushort wValue, wIndex, wLength, wStatus;

    if ( !(pUDP->UDP_CSR[0] & AT91C_UDP_RXSETUP) )
        return;

    bmRequestType = pUDP->UDP_FDR[0];
    bRequest      = pUDP->UDP_FDR[0];
    wValue        = (pUDP->UDP_FDR[0] & 0xFF);
    wValue       |= (pUDP->UDP_FDR[0] << 8);
    wIndex        = (pUDP->UDP_FDR[0] & 0xFF);
    wIndex       |= (pUDP->UDP_FDR[0] << 8);
    wLength       = (pUDP->UDP_FDR[0] & 0xFF);
    wLength      |= (pUDP->UDP_FDR[0] << 8);

    if (bmRequestType & 0x80) {
        pUDP->UDP_CSR[0] |= AT91C_UDP_DIR;
        while ( !(pUDP->UDP_CSR[0] & AT91C_UDP_DIR) );
    }
    pUDP->UDP_CSR[0] &= ~AT91C_UDP_RXSETUP;
    while ( (pUDP->UDP_CSR[0]  & AT91C_UDP_RXSETUP)  );

    // Handle supported standard device request Cf Table 9-3 in USB specification Rev 1.1
    switch ((bRequest << 8) | bmRequestType) {
    case STD_GET_DESCRIPTOR:
        if (wValue == 0x100)       // Return Device Descriptor
            AT91F_USB_SendData(pUDP, devDescriptor,
                               MIN(devDescriptorSize, wLength));
        else if (wValue == 0x200)  // Return Configuration Descriptor
            AT91F_USB_SendData(pUDP, cfgDescriptor,
                               MIN(cfgDescriptorSize, wLength));

        // String descriptors are special
        else if (wValue == 0x300)
            AT91F_USB_SendData(pUDP, languageStringDescriptor,
                               MIN(languageStringDescriptorSize, wLength));
        else if (wValue == 0x301)
            AT91F_USB_SendData(pUDP, vendorStringDescriptor,
                               MIN(vendorStringDescriptorSize, wLength));
        else if (wValue == 0x302)
            AT91F_USB_SendData(pUDP, productStringDescriptor,
                               MIN(productStringDescriptorSize, wLength));
        else if (wValue == 0x303)
            AT91F_USB_SendData(pUDP, serialStringDescriptor,
                               MIN(serialStringDescriptorSize, wLength));

        else
            AT91F_USB_SendStall(pUDP);
        break;
    case STD_SET_ADDRESS:
        AT91F_USB_SendZlp(pUDP);
        pUDP->UDP_FADDR = (AT91C_UDP_FEN | wValue);
        pUDP->UDP_GLBSTATE  = (wValue) ? AT91C_UDP_FADDEN : 0;
        break;
    case STD_SET_CONFIGURATION:
        pHid->currentConfiguration = wValue;
        AT91F_USB_SendZlp(pUDP);
        pUDP->UDP_GLBSTATE  = (wValue) ? AT91C_UDP_CONFG : AT91C_UDP_FADDEN;
        pUDP->UDP_CSR[EP_NUMBER] =
            (wValue) ? (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_IN) : 0;
        break;
    case STD_GET_CONFIGURATION:
        AT91F_USB_SendData(pUDP, (char *) &(pHid->currentConfiguration),
                           sizeof(pHid->currentConfiguration));
        break;
    case STD_GET_STATUS_ZERO:
        wStatus = 0;
        AT91F_USB_SendData(pUDP, (char *) &wStatus, sizeof(wStatus));
        break;
    case STD_GET_STATUS_INTERFACE:
        wStatus = 0;
        AT91F_USB_SendData(pUDP, (char *) &wStatus, sizeof(wStatus));
        break;
    case STD_GET_STATUS_ENDPOINT:
        wStatus = 0;
        wIndex &= 0x0F;
        if ((pUDP->UDP_GLBSTATE & AT91C_UDP_CONFG) && (wIndex <= 3)) {
            wStatus = (pUDP->UDP_CSR[wIndex] & AT91C_UDP_EPEDS) ? 0 : 1;
            AT91F_USB_SendData(pUDP, (char *) &wStatus, sizeof(wStatus));
        }
        else if ((pUDP->UDP_GLBSTATE & AT91C_UDP_FADDEN) && (wIndex == 0)) {
            wStatus = (pUDP->UDP_CSR[wIndex] & AT91C_UDP_EPEDS) ? 0 : 1;
            AT91F_USB_SendData(pUDP, (char *) &wStatus, sizeof(wStatus));
        }
        else
            AT91F_USB_SendStall(pUDP);
        break;
    case STD_SET_FEATURE_ZERO:
        AT91F_USB_SendStall(pUDP);
        break;
    case STD_SET_FEATURE_INTERFACE:
        AT91F_USB_SendZlp(pUDP);
        break;
    case STD_SET_FEATURE_ENDPOINT:
        wIndex &= 0x0F;
        if ((wValue == 0) && wIndex && (wIndex <= 3)) {
            pUDP->UDP_CSR[wIndex] = 0;
            AT91F_USB_SendZlp(pUDP);
        }
        else
            AT91F_USB_SendStall(pUDP);
        break;
    case STD_CLEAR_FEATURE_ZERO:
        AT91F_USB_SendStall(pUDP);
        break;
    case STD_CLEAR_FEATURE_INTERFACE:
        AT91F_USB_SendZlp(pUDP);
        break;
    case STD_CLEAR_FEATURE_ENDPOINT:
        wIndex &= 0x0F;
        if ((wValue == 0) && wIndex && (wIndex <= 3)) {
            if (wIndex == 1)
                pUDP->UDP_CSR[1] =
                    (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_OUT);
            else if (wIndex == 2)
                pUDP->UDP_CSR[2] =
                    (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_IN);
            else if (wIndex == 3)
                pUDP->UDP_CSR[3] =
                    (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_ISO_IN);
            AT91F_USB_SendZlp(pUDP);
        }
        else
            AT91F_USB_SendStall(pUDP);
        break;

	// handle HID class requests
    case STD_GET_HID_DESCRIPTOR:
        if (wValue == 0x2200)
            AT91F_USB_SendData(pUDP, hidReportDescriptor,
                               MIN(hidReportDescriptorSize, wLength));
        else
            AT91F_USB_SendStall(pUDP);
        break;

    case STD_SET_IDLE:
        AT91F_USB_SendZlp(pUDP);
        break;

    default:
        AT91F_USB_SendStall(pUDP);
        break;
    }
}
