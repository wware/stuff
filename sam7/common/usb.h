//*----------------------------------------------------------------------------
//*      ATMEL Microcontroller Software Support  -  ROUSSET  -
//*----------------------------------------------------------------------------
//* The software is delivered "AS IS" without warranty or condition of any
//* kind, either express, implied or statutory. This includes without
//* limitation any warranty or condition with respect to merchantability or
//* fitness for any particular purpose, or against the infringements of
//* intellectual property rights of others.
//*----------------------------------------------------------------------------
//* File Name           : usb.h
//* Object              : Handle HID or CDC enumeration, and other USB stuff
//*----------------------------------------------------------------------------
#ifndef HID_ENUMERATE_H
#define HID_ENUMERATE_H

typedef struct _AT91S_USBDEV
{
    // Private members
    AT91PS_UDP pUdp;
    unsigned char cdcOrHid;  // 1 -> HID, 0 -> CDC/ACM
    unsigned char currentConfiguration;
    unsigned char currentConnection;
    unsigned int  currentRcvBank;
    // Public Methods:
    unsigned char (*IsConfigured)(struct _AT91S_USBDEV *pUsbdev);
    // CDC
    unsigned int  (*Write) (struct _AT91S_USBDEV *pUsbdev, const char *pData, unsigned int length);
    unsigned int  (*Read)  (struct _AT91S_USBDEV *pUsbdev, char *pData, unsigned int length);
    // HID
    void (*SendReport)(struct _AT91S_USBDEV *pUsbdev, char *report, int reportSize);
} AT91S_USBDEV, *AT91PS_USBDEV;

//* external function description

extern AT91PS_USBDEV AT91F_USBDEV_Open(AT91PS_USBDEV pUsbdev,
                                       AT91PS_UDP pUdp,
                                       unsigned char cdcHid);

#endif // HID_ENUMERATE_H

