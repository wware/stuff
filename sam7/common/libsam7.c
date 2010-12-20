#include "board.h"

//*----------------------------------------------------------------------------
//* \fn    LowLevelInit
//* \brief Initialize the SAM7
//*----------------------------------------------------------------------------
void LowLevelInit(void)
{
    //* Set Flash Wait sate
    //  Single Cycle Access at Up to 30 MHz, or 40
    //  if MCK = 48MHz then 48 Cycles for 1 usecond
    //  result: AT91C_MC_FMR = 0x00300100  (MC Flash Mode Register)
    AT91C_BASE_MC->MC_FMR = ((AT91C_MC_FMCN)&(48 <<16)) | AT91C_MC_FWS_1FWS;

    //* Watchdog Disable
    AT91C_BASE_WDTC->WDTC_WDMR= 0x8000;

    //* Startup time, see section 24.3.2 of the datasheet
    // SCK = 1/32768 = 30.51 uSecond
    // Start up time = 8 * 6 / SCK = 56 * 30.51 = 1,46484375 ms
    // result: AT91C_CKGR_MOR = 0x00000601  (Main Oscillator Register)
    AT91C_BASE_PMC->PMC_MOR = (( AT91C_CKGR_OSCOUNT & (0x06 <<8) | AT91C_CKGR_MOSCEN ));
    // Wait the startup time
    while(!(AT91C_BASE_PMC->PMC_SR & AT91C_PMC_MOSCS));

    // PMC Clock Generator PLL Register setup
    //
    // The following settings are used:  DIV = 14
    //                                   MUL = 72
    //                                   PLLCOUNT = 10
    //
    // Main Clock (MAINCK from crystal oscillator) = 18432000 hz (see AT91SAM7-EK schematic)
    // MAINCK / DIV = 18432000/14 = 1316571 hz
    // PLLCK = 1316571 * (MUL + 1) = 1316571 * (72 + 1) = 1316571 * 73 = 96109683 hz
    //
    // PLLCOUNT = number of slow clock cycles before the LOCK bit is set
    //            in PMC_SR after CKGR_PLLR is written.
    //
    // PLLCOUNT = 10
    //
    // OUT = 0 (not used)
    // result: AT91C_CKGR_PLLR = 0x00000000480A0E   (PLL Register)
    // writes to FFFFFC2C
    AT91C_BASE_PMC->PMC_PLLR = ((AT91C_CKGR_DIV & 14) |
                      (AT91C_CKGR_PLLCOUNT & (10<<8)) |
                      (AT91C_CKGR_MUL & (72<<16)));
    // Wait the startup time (until PMC Status register LOCK bit is set)
    while(!(AT91C_BASE_PMC->PMC_SR & AT91C_PMC_LOCK));

    // PMC Master Clock (MCK) Register setup
    //
    // CSS  = 3  (PLLCK clock selected)
    //
    // PRES = 1  (MCK = PLLCK / 2) = 96109683/2 = 48054841 hz
    //
    // Note: Master Clock  MCK = 48054841 hz  (this is the CPU clock speed)
    // result:  AT91C_PMC_MCKR = 0x00000007  (Master Clock Register)
    AT91C_BASE_PMC->PMC_MCKR = AT91C_PMC_CSS_PLL_CLK | AT91C_PMC_PRES_CLK_2;
}

void set_up_interrupts(void (*spurious_handler)(void),
                       void (*irq_handler)(void),
                       void (*fiq_handler)(void))
{
    int i;
    // Set up the default interrupts handler vectors
    AT91C_BASE_AIC->AIC_SVR[0] = (int) fiq_handler;
    for (i = 1;i < 31; i++) {
        AT91C_BASE_AIC->AIC_SVR[i] = (int) irq_handler;
    }
    AT91C_BASE_AIC->AIC_SPU  = (int) spurious_handler;
}

void usb_init(void)
{
    // Set the PLL USB Divider
    // writes to FFFFFC2C, divider should divide by 2
    AT91C_BASE_CKGR->CKGR_PLLR |= AT91C_CKGR_USBDIV_1 ;

    // Specific Chip USB Initialisation
    // Enables the 48MHz USB clock UDPCK and System Peripheral USB Clock
    AT91C_BASE_PMC->PMC_SCER = AT91C_PMC_UDP;
    AT91C_BASE_PMC->PMC_PCER = (1 << AT91C_ID_UDP);

    // Enable UDP PullUp on D+ line
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, USB_DP_PUP);
    AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, USB_DP_PUP);
#ifdef USB_DM_PUP
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, USB_DM_PUP);
    AT91F_PIO_SetOutput(AT91C_BASE_PIOA, USB_DM_PUP);
#endif
}

static void delay(unsigned char y)
{
    volatile long x;
    while (y--)
        for (x = 0; x < 0x80000; x++);
}

static unsigned char led_state;

// LED1 is green, LED2 is yellow

void led_init(void)
{
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, LED_MASK);
    AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED_MASK);
    led_state = 0;
}

void morse_code_hex(unsigned char digits, int number)
{
    unsigned char i, j;
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, LED_MASK);
    for (i = 0; i < digits; i++) {
        for (j = 0; j < 4; j++) {
            if ((number >> 4 * (digits - 1 - i)) & (1 << (3 - j))) {
                AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
                delay(4);
                AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
            } else {
                AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
                delay(1);
                AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
            }
            delay(1);
        }
        delay(5);
    }
    // recommend delay(15) between calls
}

void blink_number(unsigned char x, int y)
{
    unsigned char i;
    AT91F_PIO_CfgOutput(AT91C_BASE_PIOA, LED_MASK);
    while (1) {
        for (i = 0; i < x; i++) {
            AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
            delay(1);
            AT91F_PIO_SetOutput(AT91C_BASE_PIOA, LED1|LED2);
            delay(1);
        }
        delay(5);
        morse_code_hex(8, y);
        delay(15);
    }
}

void advance_leds(void)
{
    switch (led_state) {
    case 0:
        AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1);
        AT91F_PIO_SetOutput  (AT91C_BASE_PIOA, LED2);
        led_state = 1;
        break;
    case 1:
        AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED1|LED2);
        led_state = 2;
        break;
    case 2:
        AT91F_PIO_ClearOutput(AT91C_BASE_PIOA, LED2);
        AT91F_PIO_SetOutput  (AT91C_BASE_PIOA, LED1);
        led_state = 3;
        break;
    case 3:
        AT91F_PIO_SetOutput  (AT91C_BASE_PIOA, LED1|LED2);
        led_state = 0;
        break;
    }
}
