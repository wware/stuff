#ifndef USB_DESCRIPTORS_H
#define USB_DESCRIPTORS_H


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

/* CDC Class Specific Request Code */
#define GET_LINE_CODING               0x21A1
#define SET_LINE_CODING               0x2021
#define SET_CONTROL_LINE_STATE        0x2221

// endpoint for HID
#define HID_EP_IN 1

// endpoints for CDC/ACM
#define CDC_EP_OUT 1
#define CDC_EP_OUT_SIZE 64
#define CDC_EP_IN  2
#define CDC_EP_IN_SIZE 64

// #define HID_REPORT_SIZE 3
extern const int hidReportSize;
extern const char *hidReportDescriptor;
extern const int hidReportDescriptorSize;

extern const char *cfgDescriptor;
extern const int cfgDescriptorSize;
extern const char *devDescriptor;
extern const int devDescriptorSize;
extern const char *languageStringDescriptor;
extern const char *vendorStringDescriptor;
extern const char *productStringDescriptor;
extern const char *serialStringDescriptor;

extern const int languageStringDescriptorSize;
extern const int vendorStringDescriptorSize;
extern const int productStringDescriptorSize;
extern const int serialStringDescriptorSize;

#endif // USB_DESCRIPTORS_H

