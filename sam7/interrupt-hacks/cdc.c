#include "Board.h"
#include "usb.h"
#include "descriptors.h"

const char *hidReportDescriptor = (void*) 0;
const int hidReportDescriptorSize = 0;
const int hidReportSize = 0;

// Check http://www.usb.org/developers/hidpage/#Class_Definition
static const char dev_desc_cdc[] = {
    /* Device descriptor */
    0x12,   // bLength
    0x01,   // bDescriptorType
    0x10,   // bcdUSBL
    0x01,   //
    0x02,   // bDeviceClass:  CDC
    0x00,   // bDeviceSubclass:
    0x00,   // bDeviceProtocol:
    0x08,   // bMaxPacketSize0
    // VID:PID = 03EB:6283
    0xEB,   // idVendorL
    0x03,   // idVendorH
    0x83,   // idProductL
    0x62,   // idProductH
    0x01,   // bcdDeviceL
    // These four indices (0, 1, 2, 3) are hard-coded
    // elsewhere, so always use these values in this order.
    0x00,   // language (en-US)
    0x01,   // iManufacturer
    0x02,   // iProduct
    0x03,   // iSerial
    0x01    // bNumConfigs
};
const char *devDescriptor = dev_desc_cdc;
const int devDescriptorSize = sizeof(dev_desc_cdc);

static const char cfg_desc_cdc[] = {
    /* ============== CONFIGURATION 1 =========== */
    /* Configuration 1 descriptor */
    0x09,   // CbLength
    0x02,   // CbDescriptorType
    0x43,   // CwTotalLength 2 EP + Control
    0x00,
    0x02,   // CbNumInterfaces
    0x01,   // CbConfigurationValue
    0x00,   // CiConfiguration
    0xC0,   // CbmAttributes 0xA0
    0x00,   // CMaxPower

    /* Communication Class Interface Descriptor Requirement */
    0x09, // bLength
    0x04, // bDescriptorType
    0x00, // bInterfaceNumber
    0x00, // bAlternateSetting
    0x01, // bNumEndpoints
    0x02, // bInterfaceClass
    0x02, // bInterfaceSubclass
    0x01, // bInterfaceProtocol
    0x00, // iInterface

    /* Header Functional Descriptor */
    0x05, // bFunction Length
    0x24, // bDescriptor type: CS_INTERFACE
    0x00, // bDescriptor subtype: Header Func Desc
    0x10, // bcdCDC:1.1
    0x01,

    /* ACM Functional Descriptor */
    0x04, // bFunctionLength
    0x24, // bDescriptor Type: CS_INTERFACE
    0x02, // bDescriptor Subtype: ACM Func Desc
    0x00, // bmCapabilities

    /* Union Functional Descriptor */
    0x05, // bFunctionLength
    0x24, // bDescriptorType: CS_INTERFACE
    0x06, // bDescriptor Subtype: Union Func Desc
    0x00, // bMasterInterface: Communication Class Interface
    0x01, // bSlaveInterface0: Data Class Interface

    /* Call Management Functional Descriptor */
    0x05, // bFunctionLength
    0x24, // bDescriptor Type: CS_INTERFACE
    0x01, // bDescriptor Subtype: Call Management Func Desc
    0x00, // bmCapabilities: D1 + D0
    0x01, // bDataInterface: Data Class Interface 1

    /* Endpoint 1 descriptor */
    0x07,   // bLength
    0x05,   // bDescriptorType
    0x83,   // bEndpointAddress, Endpoint 03 - IN
    0x03,   // bmAttributes      INT
    0x08,   // wMaxPacketSize
    0x00,
    0xFF,   // bInterval

    /* Data Class Interface Descriptor Requirement */
    0x09, // bLength
    0x04, // bDescriptorType
    0x01, // bInterfaceNumber
    0x00, // bAlternateSetting
    0x02, // bNumEndpoints
    0x0A, // bInterfaceClass
    0x00, // bInterfaceSubclass
    0x00, // bInterfaceProtocol
    0x00, // iInterface

    /* First alternate setting */
    /* Endpoint 1 descriptor */
    0x07,   // bLength
    0x05,   // bDescriptorType
    0x01,   // bEndpointAddress, Endpoint 01 - OUT
    0x02,   // bmAttributes      BULK
    CDC_EP_OUT_SIZE,   // wMaxPacketSize
    0x00,
    0x00,   // bInterval

    /* Endpoint 2 descriptor */
    0x07,   // bLength
    0x05,   // bDescriptorType
    0x82,   // bEndpointAddress, Endpoint 02 - IN
    0x02,   // bmAttributes      BULK
    CDC_EP_IN_SIZE,   // wMaxPacketSize
    0x00,
    0x00    // bInterval
};
const char *cfgDescriptor = cfg_desc_cdc;
const int cfgDescriptorSize = sizeof(cfg_desc_cdc);

// en_US = 0409
static const char lang_desc[] = {
    // Language ID
    4,      // bLength
    0x03,   // bDescriptorType = string
    0x09, 0x04, // little-endian
};
const char *languageStringDescriptor = lang_desc;
const int languageStringDescriptorSize = sizeof(lang_desc);

static const char vendor_desc[] = {
    12,     // bLength
    0x03,   // bDescriptorType = string
    'W', 0, // unicode is two bytes
    'w', 0,
    'a', 0,
    'r', 0,
    'e', 0,
};
const char *vendorStringDescriptor = vendor_desc;
const int vendorStringDescriptorSize = sizeof(vendor_desc);

static const char product_desc[] = {
    26,     // bLength
    0x03,   // bDescriptorType = string
    'C', 0,
    'D', 0,
    'C', 0,
    '/', 0,
    'A', 0,
    'C', 0,
    'M', 0,
    ' ', 0,
    'h', 0,
    'a', 0,
    'c', 0,
    'k', 0,
};
const char *productStringDescriptor = product_desc;
const int productStringDescriptorSize = sizeof(product_desc);

// serial numbers are NOT allowed to include periods
static const char serial_desc[] = {
    14,     // bLength
    0x03,   // bDescriptorType = string
    '3', 0,
    '1', 0,
    '4', 0,
    '1', 0,
    '5', 0,
    '9', 0,
};
const char *serialStringDescriptor = serial_desc;
const int serialStringDescriptorSize = sizeof(serial_desc);

void usb_open(struct _AT91S_USBDEV *usbdev)
{
    AT91F_USBDEV_Open(usbdev, AT91C_BASE_UDP, 0);
}

#define BUFSIZE 255
static char ch[BUFSIZE];
static int n;

void main_loop_iteration(struct _AT91S_USBDEV *usbDevice)
{
    /*
     * If you type:
     *
     *   echo 1 > /dev/ttyUSB0
     *
     * then what happens next is that the Read function returns 2,
     * indicating that the board has received two characters, and the
     * first two characters in ch[] are "1\n" (ascii: 0x31 0x0A).
     * That's lovely, except the Read function gets stuck if you
     * don't alternate it with Write function calls. I dunno why.
     */
    if (1) {
        n = usbDevice->Read(usbDevice, ch, BUFSIZE);
        usbDevice->Write(usbDevice, ch, n);
    } else {
        usbDevice->Read(usbDevice, ch, BUFSIZE);
        usbDevice->Write(usbDevice, "Here is a log statement!\n", 25);
    }
}
