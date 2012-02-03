#include "Board.h"
#include "usb.h"
#include "descriptors.h"

/*
 * Check directories in /sys/bus/usb/devices to find the one with
 * the correct VID and PID. In my case it's 3-1:1.0.
 *
 * cat /sys/bus/usb/devices/3-1:1.0/modalias
 * usb:v03EBp6283d0001dc00dsc00dp00ic03isc01ip02
 *      ^^^^ ^^^^
 *
 * Then to see the HID report, type:
 * sudo bash -c "echo -n 3-1:1.1 >/sys/bus/usb/drivers/usbhid/unbind"
 * sudo lsusb -vvv
 *
 * Report Descriptor: (length is 50)
 *   Item(Global): Usage Page, data= [ 0x01 ] 1
 *                   Generic Desktop Controls
 *   Item(Local ): Usage, data= [ 0x02 ] 2
 *                   Mouse
 *   Item(Main  ): Collection, data= [ 0x01 ] 1
 *                   Application
 *   Item(Local ): Usage, data= [ 0x01 ] 1
 *                   Pointer
 *   Item(Main  ): Collection, data= [ 0x00 ] 0
 *                   Physical
 *   Item(Global): Usage Page, data= [ 0x09 ] 9
 *                   Buttons
 *   Item(Local ): Usage Minimum, data= [ 0x01 ] 1
 *                   Button 1 (Primary)
 *   Item(Local ): Usage Maximum, data= [ 0x03 ] 3
 *                   Button 3 (Tertiary)
 *   Item(Global): Logical Minimum, data= [ 0x00 ] 0
 *   Item(Global): Logical Maximum, data= [ 0x01 ] 1
 *   Item(Global): Report Count, data= [ 0x03 ] 3
 *   Item(Global): Report Size, data= [ 0x01 ] 1
 *   Item(Main  ): Input, data= [ 0x02 ] 2
 *                   Data Variable Absolute No_Wrap Linear
 *                   Preferred_State No_Null_Position Non_Volatile Bitfield
 *   Item(Global): Report Count, data= [ 0x01 ] 1
 *   Item(Global): Report Size, data= [ 0x05 ] 5
 *   Item(Main  ): Input, data= [ 0x01 ] 1
 *                   Constant Array Absolute No_Wrap Linear
 *                   Preferred_State No_Null_Position Non_Volatile Bitfield
 *   Item(Global): Usage Page, data= [ 0x01 ] 1
 *                   Generic Desktop Controls
 *   Item(Local ): Usage, data= [ 0x30 ] 48
 *                   Direction-X
 *   Item(Local ): Usage, data= [ 0x31 ] 49
 *                   Direction-Y
 *   Item(Global): Logical Minimum, data= [ 0x81 ] 129
 *   Item(Global): Logical Maximum, data= [ 0x7f ] 127
 *   Item(Global): Report Size, data= [ 0x08 ] 8
 *   Item(Global): Report Count, data= [ 0x02 ] 2
 *   Item(Main  ): Input, data= [ 0x06 ] 6
 *                   Data Variable Relative No_Wrap Linear
 *                   Preferred_State No_Null_Position Non_Volatile Bitfield
 *   Item(Main  ): End Collection, data=none
 *   Item(Main  ): End Collection, data=none
 */

static const short hid_report[] = {
    0x0105, // Usage Page (Generic Desktop)
    0x0209, // Usage (Mouse)
    0x01A1, // Collection (Application)
    0x0109, //  Usage (Pointer)
    0x00A1, //  Collection (Physical)
    0x0905, //    Usage Page (Buttons)
    0x0119, //    Usage Minimum (01)
    0x0329, //    Usage Maximum (03)
    0x0015, //    Logical Minimum (0)
    0x0125, //    Logical Maximum (1)
    0x0395, //    Report Count (3)
    0x0175, //    Report Size (1)
    0x0281, //    3 Button bits
    0x0195, //    Report Count (1)
    0x0575, //    Report Size (5)
    0x0381, //    5 bits of padding
    0x0105, //    Generic desktop
    0x3009, //    Usage (X)
    0x3109, //    Usage(Y)
    0x8115, //    Logical Minimum (-127)
    0x7F25, //    Logical Maximum (127)
    0x0875, //    Report Size (8)
    0x0295, //    Report Count (2)
    0x0681, //    2 position bytes
    0xC0C0
};
const char *hidReportDescriptor = (const char *) hid_report;
const int hidReportDescriptorSize = sizeof(hid_report);
const int hidReportSize = 3;

// Check http://www.usb.org/developers/hidpage/#Class_Definition
static const char dev_desc_hid[] = {
    /* Device descriptor */
    0x12,   // bLength
    0x01,   // bDescriptorType
    0x10,   // bcdUSBL
    0x01,   //
    0x00,   // bDeviceClass:  HID
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
const char *devDescriptor = dev_desc_hid;
const int devDescriptorSize = sizeof(dev_desc_hid);

static const char cfg_desc_hid[] = {
    /* ============== CONFIGURATION 1 =========== */
    /*
     * Does this mean there could be more than one configuration?
     * Could the same gadget have a HID config and a CDC/ACM config?
     * Curious.
     */
    /* Configuration 1 descriptor */
    0x09,   // CbLength
    0x02,   // CbDescriptorType
    0x22,   // CwTotalLength 2 EP + Control
    0x00,
    0x01,   // CbNumInterfaces
    0x01,   // CbConfigurationValue
    0x00,   // CiConfiguration
    0xA0,   // CbmAttributes Bus powered + Remote Wakeup
    0x32,   // CMaxPower: 100mA

    /* Mouse Interface Descriptor Requirement */
    0x09, // bLength
    0x04, // bDescriptorType
    0x00, // bInterfaceNumber
    0x00, // bAlternateSetting
    0x01, // bNumEndpoints
    0x03, // bInterfaceClass: HID code
    0x01, // bInterfaceSubclass
    0x02, // bInterfaceProtocol: Mouse
    0x00, // iInterface

    /* HID Descriptor */
    0x09, // bLength
    0x21, // bDescriptor type: HID Descriptor Type
    0x00, // bcdHID
    0x01,
    0x00, // bCountryCode
    0x01, // bNumDescriptors
    0x22, // bDescriptorType
    sizeof(hid_report), // wItemLength
    0x00,

    /* Endpoint 1 descriptor */
    0x07,   // bLength
    0x05,   // bDescriptorType
    0x81,   // bEndpointAddress, Endpoint 01 - IN
    0x03,   // bmAttributes      INT
    0x03,   // wMaxPacketSize: 3 bytes (button, x, y)
    0x00,
    0x0A    // bInterval
};
const char *cfgDescriptor = cfg_desc_hid;
const int cfgDescriptorSize = sizeof(cfg_desc_hid);

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
    20,     // bLength
    0x03,   // bDescriptorType = string
    'H', 0,
    'I', 0,
    'D', 0,
    ' ', 0,
    'M', 0,
    'o', 0,
    'u', 0,
    's', 0,
    'e', 0,
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
    AT91F_USBDEV_Open(usbdev, AT91C_BASE_UDP, 1);
}

static int count = 0;

void main_loop_iteration(struct _AT91S_USBDEV *usbDevice)
{
    char x;
    if (count & 64) {
        // LED on, mouse moving left
        AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1);
        x = -2;
    } else {
        // LED off, mouse moving right
        AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1);
        x = 2;
    }
    count++;
    char buttons = 0;
    char hid_report_bytes[] = { buttons, x, 0 };  // three bytes as promised
    usbDevice->SendReport(usbDevice, hid_report_bytes, hidReportSize);
}
