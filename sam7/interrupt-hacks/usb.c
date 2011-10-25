/* usb.c -- AT91SAM7 usb core
**
** Copyright 2006, Brian Swetland.  All rights reserved.
** See provided LICENSE file or http://frotz.net/LICENSE for details.
*/

#include "Board.h"
#include "descriptors.h"
#include "usb.h"


static usb_info USB = {
    .ctl_out_data = 0,
    .ctl_out_count = 0,
    .new_address = 0,
};

void reset_endpoints(void)
{
    AT91UDP *udp = AT91UDP_ADDR;
    /* reset endpoints */
    //udp->RST_EP = UDP_EP0 | UDP_EP1 | UDP_EP2 | UDP_EP3;
    udp->RST_EP = UDP_EP0 | UDP_EP1;
    udp->RST_EP = 0;

    USB.ctl_out_count = 0;
    USB.new_address = 0;

    udp->CSR0 = UDP_TYPE_CONTROL | UDP_EPEDS;

    /* disable non-control endpoints */
    udp->CSR1 = 0;
    udp->CSR2 = 0;
    udp->CSR3 = 0;
}


/* PLL must be configured to provide 48MHz USBCLK */
void usb_init()
{
    AT91UDP *udp = AT91UDP_ADDR;
    AT91PMC *pmc = AT91PMC_ADDR;
    //usb_info *usb = &USB;

    /* enable the usb bus and peripheral clocks */
    pmc->SCER = PMC_UDP;
    pmc->PCER = (1 << PID_UDP);

    /* unconfigured, address = 0, function disabled */
    udp->GLB_STAT = 0;
    udp->FADDR = 0;

    reset_endpoints();

    /* enable transciever */
    udp->TXVC = 0;
    udp->IDR = 0xffffffff;
    udp->IER = (UDP_ENDBUSRES | UDP_EP0INT | UDP_EP1INT | UDP_EP3INT);

#if 0
    pio_set(PIN_USB_PULLUP, 0);
    for (n = 0; n < 100; n++);
    pio_set(PIN_USB_PULLUP, 1);
#endif
}

static inline void ctl_ack(AT91UDP * udp)
{
    udp->CSR0 |= UDP_TXPKTRDY;
}

static inline void ctl_stall(AT91UDP * udp)
{
    udp->CSR0 |= UDP_FORCESTALL;
}

static void ctl_send_packet(usb_info * usb, AT91UDP * udp)
{
    unsigned len = usb->ctl_out_count > 8 ? 8 : usb->ctl_out_count;

    usb->ctl_out_count -= len;
    while (len--)
	udp->FDR0 = *usb->ctl_out_data++;
    udp->CSR0 |= UDP_TXPKTRDY;
}

static void ctl_send(usb_info * usb, AT91UDP * udp,
		     unsigned char *data, unsigned len, unsigned reqlen)
{
    if (len > reqlen)
	len = reqlen;
    if (len > 64)
	len = 64;

    usb->ctl_out_data = data;
    usb->ctl_out_count = len;

    ctl_send_packet(usb, udp);
}

typedef struct {
    unsigned char type;
    unsigned char request;
    unsigned short value;
    unsigned short index;
    unsigned short length;
} __attribute__ ((packed)) setup_msg;

#define STD_SET_ADDRESS               0x0500
#define STD_GET_DESCRIPTOR            0x0680
#define STD_GET_CONFIGURATION         0x0880
#define STD_SET_CONFIGURATION         0x0900
#define STD_CLEAR_FEATURE_ENDPOINT    0x0102
#define STD_SET_INTERFACE             0x0B01

static void usb_ep0(usb_info * usb, AT91UDP * udp)
{
    unsigned n, i;
    setup_msg setup;

    n = udp->CSR0;

    if (n & UDP_RX_DATA_BK1) {
	return;
    }
    if (n & UDP_RX_DATA_BK0) {
	ACK_EVENT(udp->CSR0, UDP_RX_DATA_BK0);
	return;
    }
    if (n & UDP_TXCOMP) {
	ACK_EVENT(udp->CSR0, UDP_TXCOMP);
	while ((udp->CSR0 & UDP_TXCOMP));
	if (usb->new_address) {
	    udp->FADDR = UDP_FEN | usb->new_address;	// BUG overflow?
	    udp->GLB_STAT = UDP_FADDEN;
	    usb->new_address = 0;
	}
	if (usb->ctl_out_count) {
	    ctl_send_packet(usb, udp);
	}
	return;
    }
    if (n & UDP_STALLSENT) {
	ACK_EVENT(udp->CSR0, UDP_STALLSENT | UDP_FORCESTALL);
	while ((udp->CSR0 & (UDP_STALLSENT | UDP_FORCESTALL)));
	return;
    }
    if (n & UDP_RXSETUP) {
	unsigned char *x = (unsigned char *) &setup;
	for (i = 0; i < 8; i++)
	    *x++ = udp->FDR0;

	if (setup.type & 0x80) {
	    udp->CSR0 |= UDP_DIR;
	    while (!(udp->CSR0 & UDP_DIR));	// BUG race?
	}

	ACK_EVENT(udp->CSR0, UDP_RXSETUP);
	while ((udp->CSR0 & UDP_RXSETUP));	// BUG race?

	switch ((setup.request << 8) | setup.type) {
	case STD_SET_ADDRESS:
	    if ((setup.value == 0) || (setup.value > 127)) {
		ctl_stall(udp);
	    } else {
		ctl_ack(udp);
		usb->new_address = setup.value;
	    }
	    break;

	case STD_SET_CONFIGURATION:
	    ctl_ack(udp);  // send a zero length packet
	    if (setup.value) {
		udp->GLB_STAT = UDP_CONFG;
                /* reset endpoints */
                // TODO: some kind of endpoint mask
                //udp->RST_EP = UDP_EP0 | UDP_EP1 | UDP_EP2 | UDP_EP3;
                udp->RST_EP = UDP_EP0 | UDP_EP1;
                udp->RST_EP = 0;
		usb_status(1);
	    } else {
		udp->GLB_STAT = UDP_FADDEN;
		usb_status(0);
	    }
            usbSetConfiguration(udp, setup.value);
	    break;

	case STD_SET_INTERFACE:
	    if ((setup.value != 0) || (setup.index != 0)) {
		ctl_stall(udp);
	    } else {
		udp->RST_EP = UDP_EP1 | UDP_EP3;
		udp->RST_EP = 0;
		ctl_ack(udp);
	    }
	    break;

	case STD_GET_DESCRIPTOR:
	    if (setup.value == 0x100) {
		ctl_send(usb, udp, devDescriptor, devDescriptorSize,
			 setup.length);
	    } else if (setup.value == 0x200) {
		ctl_send(usb, udp, cfgDescriptor, cfgDescriptorSize,
			 setup.length);
	    } else {
		ctl_stall(udp);
	    }
	    break;

	default:
	    ctl_stall(udp);
	    return;
	}
	return;
    }
}

extern void usb_ep1(usb_info * usb, AT91UDP * udp);
extern void usb_ep2(usb_info * usb, AT91UDP * udp);
extern void usb_ep3(usb_info * usb, AT91UDP * udp);

void usb_poll()
{
    AT91UDP *udp = AT91UDP_ADDR;
    usb_info *usb = &USB;
    unsigned n;

    /* ignore SOF, suspend, resume, wake, etc */
    for (;;) {
	n = udp->
	    ISR & (UDP_ENDBUSRES | UDP_EP0INT | UDP_EP1INT | UDP_EP2INT | UDP_EP3INT);
	if (n == 0)
	    return;

	if (n & UDP_EP1INT) {
	    usb_ep1(usb, udp);
	    continue;
	}
	if (n & UDP_EP2INT) {
	    usb_ep2(usb, udp);
	    continue;
	}
	if (n & UDP_EP3INT) {
	    usb_ep3(usb, udp);
	    continue;
	}

	/* we handle ep0 *after* the data endpoints because
	   we want to make sure we empty the data FIFOs before
	   doing something like resetting the data toggle
	   with a SET_INTERFACE request (as linux does when it
	   closes a usb device).
	 */
	if (n & UDP_EP0INT) {
	    usb_ep0(usb, udp);
	    continue;
	}
	if (n & UDP_ENDBUSRES) {
	    /* done with reset, enable the part for io */
	    reset_endpoints();
	    udp->FADDR = UDP_FEN;
	    udp->ICR = UDP_ENDBUSRES;
	    udp->IDR = 0xffffffff;
	    udp->IER =
		(UDP_ENDBUSRES | UDP_EP0INT | UDP_EP1INT | UDP_EP3INT);
	    continue;
	}
    }

}
