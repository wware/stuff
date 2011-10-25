#ifndef __USB_H__
#define __USB_H__




#include "AT91SAM7S64.h"

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;





/* There's plenty of braindamage here...
**
** These bits are *cleared* by writing *0* to them (what the hell):
** TXCOMP, RX_DATA_BK0, RXSETUP, STALLSENT, RX_DATA_BK1
**
** These bits are read/write by the driver and not modified
** by the hardware.  Safe for RMW access:
** FORCESTALL, DIR, EPTYPE, EPEDS
**
** These bits are setable by driver and cleared by hardware:
** TXPKTRDY
**
** These bits are read-only:
** DTGLE, RXBYTECNT
**
*/

#define ZERO_CLEAR_BITS (UDP_TXCOMP | UDP_RXSETUP | UDP_STALLSENT | \
                         UDP_RX_DATA_BK0 | UDP_RX_DATA_BK1)

#define ACK_EVENT(reg, bit) (reg = ((reg | ZERO_CLEAR_BITS) & (~(bit))))




typedef struct _AT91S_USBDEV {
    // Private members
    AT91PS_UDP pUdp;
    unsigned char currentConfiguration;
    unsigned char currentConnection;
    unsigned int currentRcvBank;
    // Public Methods:
    unsigned char (*IsConfigured) (struct _AT91S_USBDEV * pUsbdev);
    // CDC
    unsigned int (*Write) (struct _AT91S_USBDEV * pUsbdev,
			   const char *pData, unsigned int length);
    unsigned int (*Read) (struct _AT91S_USBDEV * pUsbdev, char *pData,
			  unsigned int length);
    // HID
    void (*SendReport) (struct _AT91S_USBDEV * pUsbdev, char *report,
			int reportSize);
    // internals
    void (*setConfiguration) (struct _AT91S_USBDEV * pUsbdev,
			      ushort wValue);
    void (*clearFeatureEndpoint) (struct _AT91S_USBDEV * pUsbdev,
				  ushort wValue, ushort wIndex);
} AT91S_USBDEV, *AT91PS_USBDEV;

//* external function description

#if 0
extern AT91PS_USBDEV AT91F_USBDEV_Open
    (AT91PS_USBDEV pUsbdev,
     AT91PS_UDP pUdp,
     void (*setConfiguration) (struct _AT91S_USBDEV * pUsbdev,
			       ushort wValue),
     void (*clearFeatureEndpoint) (struct _AT91S_USBDEV * pUsbdev,
				   ushort wValue, ushort wIndex));
#endif





typedef volatile unsigned int vu4;

typedef struct 
{
    vu4 SCER;
    vu4 SCDR;
    vu4 SCSR;
    vu4 __0;
    vu4 PCER;
    vu4 PCDR;
    vu4 PCSR;
    vu4 __1;
    vu4 MOR;
    vu4 MCFR;
    vu4 __2;
    vu4 PLLR;
    vu4 MCKR;
    vu4 __3[2];
    vu4 PCK0;
    vu4 PCK1;
    vu4 PCK2;
} AT91PMC;

#define AT91PMC_ADDR ((AT91PMC*) 0xfffffc00)

/* PMC_SCER/SCDR */
#define PMC_PCK      0x00000001
#define PMC_UDP      0x00000080
#define PMC_PCK0     0x00000100
#define PMC_PCK1     0x00000200
#define PMC_PCK2     0x00000400

typedef struct 
{
    vu4 FRM_NUM;
    vu4 GLB_STAT;
    vu4 FADDR;
    vu4 __0;
    vu4 IER;
    vu4 IDR;
    vu4 IMR;
    vu4 ISR;
    vu4 ICR;
    vu4 __1;
    vu4 RST_EP;
    vu4 __2;
    vu4 CSR0;
    vu4 CSR1;
    vu4 CSR2;
    vu4 CSR3;
    vu4 __3[4];
    vu4 FDR0;
    vu4 FDR1;
    vu4 FDR2;
    vu4 FDR3;
    vu4 __4[5];
    vu4 TXVC;
} AT91UDP;

#define AT91UDP_ADDR ((AT91UDP*) 0xfffb0000)

// GLB_STAT bits
#define UDP_FADDEN    0x00000001
#define UDP_CONFG     0x00000002
#define UDP_ESR       0x00000004
#define UDP_RSMINPR   0x00000008
#define UDP_RMWUPE    0x00000010

// FADDR bits
#define UDP_FEN       0x00000100

// interrupt bits
#define UDP_EP0INT    0x00000001
#define UDP_EP1INT    0x00000002
#define UDP_EP2INT    0x00000004
#define UDP_EP3INT    0x00000008
#define UDP_RXSUSP    0x00000100
#define UDP_RXRSM     0x00000200
#define UDP_EXTRSM    0x00000400
#define UDP_SOFINT    0x00000800
#define UDP_ENDBUSRES 0x00001000
#define UDP_WAKEUP    0x00002000

// RST_EP bits
#define UDP_EP0       0x00000001
#define UDP_EP1       0x00000002
#define UDP_EP2       0x00000004
#define UDP_EP3       0x00000008

// CSR bits
#define UDP_TXCOMP         0x00000001
#define UDP_RX_DATA_BK0    0x00000002
#define UDP_RXSETUP        0x00000004
#define UDP_STALLSENT      0x00000008
#define UDP_ISOERROR       0x00000008
#define UDP_TXPKTRDY       0x00000010
#define UDP_FORCESTALL     0x00000020
#define UDP_RX_DATA_BK1    0x00000040
#define UDP_DIR            0x00000080

#define UDP_DTGL           0x00000800
#define UDP_EPEDS          0x00008000

#define UDP_TYPE_CONTROL     0x00000000
#define UDP_TYPE_ISOCH_OUT   0x00000100
#define UDP_TYPE_BULK_OUT    0x00000200
#define UDP_TYPE_INT_OUT     0x00000300
#define UDP_TYPE_ISOCH_IN    0x00000500
#define UDP_TYPE_BULK_IN     0x00000600
#define UDP_TYPE_INT_IN      0x00000700

#define PID_AIC_FIQ    0
#define PID_SYSIRQ     1
#define PID_PIOA       2
#define PID_ADC        4
#define PID_SPI        5
#define PID_USART      6
#define PID_SSC        8
#define PID_TWI        9
#define PID_PWMC       10
#define PID_UDP        11
#define PID_TC0        12
#define PID_TC1        13
#define PID_TC2        14
#define PID_AIC_IRQ    30




typedef struct {
    /* the control endpoint is only 8 bytes deep,
       so we need to track our state across multiple
       IN packet transactions */
    unsigned char *ctl_out_data;
    unsigned ctl_out_count;

    /* when ack'ing a SET_ADDRESS, we need to cache
       the new address to apply it once the transaction
       is complete */
    unsigned new_address;

    unsigned char recv[64];
    unsigned char xmit[64];
} usb_info;



extern void usb_init(void);
extern void usb_disconnect(void);
extern void usb_poll(void);
extern void usb_send_ready(void);
extern void reset_endpoints(void);

/* called when usb is configured or goes offline */
void usb_status(unsigned online);

#if 0
/* process up to 64 bytes of data. return non-zero
   there is data to send in response*/
unsigned usb_bulk_recv(void *data, unsigned len);


#ifdef WITH_FASTER_USB
unsigned usb_bulk_send(unsigned char **ptr);
#else
/* fill the send buffer with up to 64 bytes of data
   and return the number of bytes to send. 0 == stop
   sending data */
unsigned usb_bulk_send(void *data);
#endif
#endif





extern void usbSetConfiguration(AT91UDP * udp, ushort wValue);

#endif
