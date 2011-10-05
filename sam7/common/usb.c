//*----------------------------------------------------------------------------
//*      ATMEL Microcontroller Software Support  -  ROUSSET  -
//*----------------------------------------------------------------------------
//* The software is delivered "AS IS" without warranty or condition of any
//* kind, either express, implied or statutory. This includes without
//* limitation any warranty or condition with respect to merchantability or
//* fitness for any particular purpose, or against the infringements of
//* intellectual property rights of others.
//*----------------------------------------------------------------------------
//* File Name           : usb.c
//* Object              : Handle CDC or HID enumeration, other USB stuff
//*----------------------------------------------------------------------------

#include "board.h"
#include "descriptors.h"
#include "usb.h"

typedef unsigned char  uchar;
typedef unsigned short ushort;
typedef unsigned int   uint;

#define MIN(a, b) (((a) < (b)) ? (a) : (b))

static uchar AT91F_UDP_IsConfigured(AT91PS_USBDEV pUsbdev);
static uint AT91F_CDC_Read(AT91PS_USBDEV pUsbdev, char *pData, uint length);
static uint AT91F_CDC_Write(AT91PS_USBDEV pUsbdev, const char *pData, uint length);
static void AT91F_HID_SendReport(AT91PS_USBDEV, char *report, int length);
static void setup_packet_handler(AT91PS_USBDEV pUsbdev);

typedef struct {
    unsigned int dwDTERRate;
    char bCharFormat;
    char bParityType;
    char bDataBits;
} AT91S_CDC_LINE_CODING, *AT91PS_CDC_LINE_CODING;

AT91S_CDC_LINE_CODING line = {
    115200, // baudrate
    0,      // 1 Stop Bit
    0,      // None Parity
    8       // 8 Data bits
};

//*----------------------------------------------------------------------------
//* \fn    AT91F_USBDEV_Open
//* \brief
//*----------------------------------------------------------------------------
AT91PS_USBDEV AT91F_USBDEV_Open(AT91PS_USBDEV pUsbdev, AT91PS_UDP pUdp, unsigned char cdcHid)
{
    pUsbdev->pUdp = pUdp;
    pUsbdev->cdcOrHid = cdcHid;   // 1: HID, 0: CDC/ACM
    pUsbdev->currentConfiguration = 0;
    pUsbdev->currentConnection    = 0;
    pUsbdev->currentRcvBank       = AT91C_UDP_RX_DATA_BK0;
    pUsbdev->IsConfigured = AT91F_UDP_IsConfigured;
    pUsbdev->Write        = AT91F_CDC_Write;
    pUsbdev->Read         = AT91F_CDC_Read;
    pUsbdev->SendReport   = AT91F_HID_SendReport;
    return pUsbdev;
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_UDP_IsConfigured
//* \brief Test if the device is configured and handle enumeration
//*----------------------------------------------------------------------------
static uchar AT91F_UDP_IsConfigured(AT91PS_USBDEV pUsbdev)
{
    AT91PS_UDP pUDP = pUsbdev->pUdp;
    AT91_REG isr = pUDP->UDP_ISR;

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
        setup_packet_handler(pUsbdev);
    }
    return pUsbdev->currentConfiguration;
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_CDC_Read
//* \brief Read available data from Endpoint OUT
//*----------------------------------------------------------------------------
static uint AT91F_CDC_Read(AT91PS_USBDEV pUsbdev, char *pData, uint length)
{
    AT91PS_UDP pUdp = pUsbdev->pUdp;
    uint packetSize;
    uint nbBytesRcv = 0;
    uint currentReceiveBank = pUsbdev->currentRcvBank;
    while (length) {
        if ( !AT91F_UDP_IsConfigured(pUsbdev) )
            break;
        if (pUdp->UDP_CSR[CDC_EP_OUT] & currentReceiveBank) {
            packetSize = MIN(pUdp->UDP_CSR[CDC_EP_OUT] >> 16, length);
            length -= packetSize;
            if (packetSize < CDC_EP_OUT_SIZE)
                length = 0;
            while (packetSize--)
                pData[nbBytesRcv++] = pUdp->UDP_FDR[CDC_EP_OUT];
            // See doc6175.pdf sections 35.5.2.7 and 35.6.10
            // clear the current bank bit (BANK 0 or BANK 1)
            pUdp->UDP_CSR[CDC_EP_OUT] &= ~currentReceiveBank;
            // wait for synchronization
            while ((pUdp->UDP_CSR[CDC_EP_OUT] & currentReceiveBank)
                   == currentReceiveBank);
            if (currentReceiveBank == AT91C_UDP_RX_DATA_BK0)
                currentReceiveBank = AT91C_UDP_RX_DATA_BK1;
            else
                currentReceiveBank = AT91C_UDP_RX_DATA_BK0;
        }
    }
    pUsbdev->currentRcvBank = currentReceiveBank;
    return nbBytesRcv;
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_USBDEV_Write
//* \brief Send through endpoint 2
//*----------------------------------------------------------------------------
static uint AT91F_CDC_Write(AT91PS_USBDEV pUsbdev, const char *pData, uint length)
{
    AT91PS_UDP pUdp = pUsbdev->pUdp;
    uint cpt = 0;

    // Send the first packet
    cpt = MIN(length, CDC_EP_IN_SIZE);
    length -= cpt;
    while (cpt--) pUdp->UDP_FDR[CDC_EP_IN] = *pData++;
    pUdp->UDP_CSR[CDC_EP_IN] |= AT91C_UDP_TXPKTRDY;

    while (length) {
        // Fill the second bank
        cpt = MIN(length, CDC_EP_IN_SIZE);
        length -= cpt;
        while (cpt--) pUdp->UDP_FDR[CDC_EP_IN] = *pData++;
        // Wait for the the first bank to be sent
        while ( !(pUdp->UDP_CSR[CDC_EP_IN] & AT91C_UDP_TXCOMP) )
            if ( !AT91F_UDP_IsConfigured(pUsbdev) ) return length;
        pUdp->UDP_CSR[CDC_EP_IN] &= ~(AT91C_UDP_TXCOMP);
        while (pUdp->UDP_CSR[CDC_EP_IN] & AT91C_UDP_TXCOMP);
        pUdp->UDP_CSR[CDC_EP_IN] |= AT91C_UDP_TXPKTRDY;
    }
    // Wait for the end of transfer
    while ( !(pUdp->UDP_CSR[CDC_EP_IN] & AT91C_UDP_TXCOMP) )
        if ( !AT91F_UDP_IsConfigured(pUsbdev) ) return length;
    pUdp->UDP_CSR[CDC_EP_IN] &= ~(AT91C_UDP_TXCOMP);
    while (pUdp->UDP_CSR[CDC_EP_IN] & AT91C_UDP_TXCOMP);

    return length;
}

//*----------------------------------------------------------------------------
//* \fn    AT91F_HID_SendCoordinates
//* \brief Send Data through endpoint 1
//*----------------------------------------------------------------------------
static void AT91F_HID_SendReport(AT91PS_USBDEV pHid, char *report, int reportSize)
{
    AT91PS_UDP pUdp = pHid->pUdp;

    // Send report to the host
    while (reportSize--)
        *((volatile char *) (pUdp->UDP_FDR + HID_EP_IN)) = *report++;
    pUdp->UDP_CSR[HID_EP_IN] |= AT91C_UDP_TXPKTRDY;

    // Wait for the end of transmission
    while ( !(pUdp->UDP_CSR[HID_EP_IN] & AT91C_UDP_TXCOMP) )
        pHid->IsConfigured(pHid);

    // Clear AT91C_UDP_TXCOMP flag for endpoint 1
    if (pUdp->UDP_CSR[HID_EP_IN] & AT91C_UDP_TXCOMP) {
        pUdp->UDP_CSR[HID_EP_IN] &= ~AT91C_UDP_TXCOMP;
        // then wait for it to be recognized as cleared
        while (pUdp->UDP_CSR[HID_EP_IN] & AT91C_UDP_TXCOMP);
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
static void setup_packet_handler(AT91PS_USBDEV pUsbdev)
{
    AT91PS_UDP pUDP = pUsbdev->pUdp;
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
        if (wValue == 0x100) {     // Return Device Descriptor
            AT91F_USB_SendData(pUDP, devDescriptor,
                               MIN(devDescriptorSize, wLength));
        }
        else if (wValue == 0x200) { // Return Configuration Descriptor
            AT91F_USB_SendData(pUDP, cfgDescriptor,
                               MIN(cfgDescriptorSize, wLength));
        }

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
        pUsbdev->currentConfiguration = wValue;
        AT91F_USB_SendZlp(pUDP);
        pUDP->UDP_GLBSTATE  = (wValue) ? AT91C_UDP_CONFG : AT91C_UDP_FADDEN;
        if (pUsbdev->cdcOrHid) {
            // HID -> only one endpoint
            pUDP->UDP_CSR[1] = (wValue) ? (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_IN) : 0;
        } else {
            // CDC -> three endpoints
            pUDP->UDP_CSR[1] = (wValue) ? (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_OUT) : 0;
            pUDP->UDP_CSR[2] = (wValue) ? (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_IN)  : 0;
            pUDP->UDP_CSR[3] = (wValue) ? (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_INT_IN)   : 0;
        }
        break;
    case STD_GET_CONFIGURATION:
        AT91F_USB_SendData(pUDP, (char *) &(pUsbdev->currentConfiguration),
                           sizeof(pUsbdev->currentConfiguration));
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
                pUDP->UDP_CSR[1] = (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_OUT);
            else if (wIndex == 2)
                pUDP->UDP_CSR[2] = (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_BULK_IN);
            else if (wIndex == 3) {
                if (pUsbdev->cdcOrHid)
                    pUDP->UDP_CSR[3] = (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_ISO_IN);
                else
                    pUDP->UDP_CSR[3] = (AT91C_UDP_EPEDS | AT91C_UDP_EPTYPE_INT_IN);
            }
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

        // handle CDC class requests
    case SET_LINE_CODING:
        while ( !(pUDP->UDP_CSR[0] & AT91C_UDP_RX_DATA_BK0) );
        pUDP->UDP_CSR[0] &= ~(AT91C_UDP_RX_DATA_BK0);
        AT91F_USB_SendZlp(pUDP);
        break;
    case GET_LINE_CODING:
        AT91F_USB_SendData(pUDP, (char *) &line, MIN(sizeof(line), wLength));
        break;
    case SET_CONTROL_LINE_STATE:
        pUsbdev->currentConnection = wValue;
        AT91F_USB_SendZlp(pUDP);
        break;

    default:
        AT91F_USB_SendStall(pUDP);
        break;
    }
}
