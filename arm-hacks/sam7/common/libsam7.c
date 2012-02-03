#include "board.h"
#include "libsam7.h"

//*----------------------------------------------------------------------------
//* \fn    LowLevelInit
//* \brief Initialize the SAM7
//*----------------------------------------------------------------------------
void LowLevelInit(void)
{
    // Set Flash Wait sate
    // Single Cycle Access at Up to 30 MHz, or 40
    // if MCK = 48MHz then 48 Cycles for 1 usecond
    // result: AT91C_MC_FMR = 0x00300100  (MC Flash Mode Register)
    AT91C_BASE_MC->MC_FMR = ((AT91C_MC_FMCN)&(48 <<16)) | AT91C_MC_FWS_1FWS;

    // Watchdog Disable
    AT91C_BASE_WDTC->WDTC_WDMR= 0x8000;

    // Startup time, see section 24.3.2 of the datasheet
    // SCK = 32768 kHz
    // Start up time = 8 * 6 / SCK = 48 / 32768 kHz = 1.464 msecs
    // result: AT91C_CKGR_MOR = 0x00000601  (Main Oscillator Register)
    AT91C_BASE_PMC->PMC_MOR =
        (AT91C_CKGR_OSCOUNT & (0x06 << 8)) | AT91C_CKGR_MOSCEN;

    // Wait the startup time
    while(!(AT91C_BASE_PMC->PMC_SR & AT91C_PMC_MOSCS));

    // PMC Clock Generator PLL Register setup
    // The following settings are used:  DIV = 24
    //                                   MUL = 124
    //                                   PLLCOUNT = 10
    // Main Clock (MAINCK from crystal oscillator) = 18432000 hz
    // (true for all Sparkfun SAM7 boards)
    // MAINCK / DIV = 18432000/24 = 768 kHz
    // PLLCK = 768 kHz * (MUL + 1) = 768000 * (124 + 1) = 96 MHz
    // PLLCOUNT = number of slow clock cycles before the LOCK bit is set
    //            in PMC_SR after CKGR_PLLR is written.
    // PLLCOUNT = 10
    // OUT = 0 (not used)
    // AT91C_CKGR_PLLR = 0x000000007C0A18   (PLL Register)
    AT91C_BASE_PMC->PMC_PLLR = ((AT91C_CKGR_DIV & 24) |
                      (AT91C_CKGR_PLLCOUNT & (10<<8)) |
                      (AT91C_CKGR_MUL & (124<<16)));
    // Wait the startup time (until PMC Status register LOCK bit is set)
    while(!(AT91C_BASE_PMC->PMC_SR & AT91C_PMC_LOCK));

    // PMC Master Clock (MCK) Register setup
    // CSS  = 3  (PLLCK clock selected)
    // PRES = 1  (MCK = PLLCK / 2) = 96/2 = 48 MHz
    // Master Clock  MCK = 48 MHz  (this is the CPU clock speed)
    // result:  AT91C_PMC_MCKR = 0x00000007  (Master Clock Register)
    AT91C_BASE_PMC->PMC_MCKR = AT91C_PMC_CSS_PLL_CLK | AT91C_PMC_PRES_CLK_2;
}

void set_up_interrupts(voidfunc spurious_handler, voidfunc irq_handler, voidfunc fiq_handler)
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

// Olimex proto board: LED1 is green, LED2 is yellow
// Olimex header board: LED1 is green, no LED2

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

/*************************************************************************
 * Serial port stuff for SAM7
 *
 * Helpful info here, though the writing style grates on the retina:
 * http://www.sparkfun.com/datasheets/DevTools/SAM7/at91sam7%20serial%20communications.pdf
 */

static volatile AT91S_USART *pUSART = AT91C_BASE_US0;

#define USE_UART_INTERRUPTS 0

#if USE_UART_INTERRUPTS
// external global variables
extern char Buffer[]; // holds received characters
extern unsigned long nChars; // counts number of received chars
extern char *pBuffer; // pointer into Buffer
#endif

#define JIM_LYNCH 0
// I started by following Jim Lynch's work, which is confusing
// Martin Thomas is much clearer:
// http://gandalf.arubi.uni-kl.de/avr_projects/arm_projects/index_at91.html

#if JIM_LYNCH
void uart0_init(int baud_rate)
{
    volatile AT91PS_PIO pPIO = AT91C_BASE_PIOA;

    int brd = (MCK / 16) / baud_rate;

    /* enable USART0 peripheral clock */
    AT91C_BASE_PMC->PMC_PCER = (1 << AT91C_ID_US0);

    pPIO->PIO_PDR =
        AT91C_PA5_RXD0 |          /* Enable RxD0 Pin */
        AT91C_PA6_TXD0;           /* Enalbe TxD0 Pin */

    pPIO->PIO_ASR =
        AT91C_PIO_PA5 |
        AT91C_PIO_PA6;

    pUSART->US_CR =
        AT91C_US_RSTRX |          /* Reset Receiver      */
        AT91C_US_RSTTX |          /* Reset Transmitter   */
        AT91C_US_RXDIS |          /* Receiver Disable    */
        AT91C_US_TXDIS;           /* Transmitter Disable */

    pUSART->US_MR =
        AT91C_US_USMODE_NORMAL |  /* Normal Mode */
        AT91C_US_CLKS_CLOCK    |  /* Clock = MCK */
        AT91C_US_CHRL_8_BITS   |  /* 8-bit Data  */
        AT91C_US_PAR_NONE      |  /* No Parity   */
        AT91C_US_NBSTOP_1_BIT;    /* 1 Stop Bit  */

    pUSART->US_IER = 0x0000;      /* no usart0 interrupts enabled */
    pUSART->US_IDR = 0xFFFF;      /* all usart0 interrupts disabled */

    pUSART->US_BRGR = brd;        /* Baud Rate Divisor */

    pUSART->US_RTOR = 0;          /* receiver time-out (disabled) */
    pUSART->US_TTGR = 0;          /* transmitter timeguard (disabled) */
    pUSART->US_FIDI = 0;          /* FI over DI Ratio Value (disabled) */
    pUSART->US_IF = 0;            /* IrDA Filter value (disabled) */

#if USE_UART_INTERRUPTS
    /* Set up the Advanced Interrupt Controller (AIC)  registers for USART0 */
    void Usart0IrqHandler(void); /* function prototype for USART0 handler */

    volatile AT91PS_AIC  pAIC = AT91C_BASE_AIC;   /* pointer to AIC data structure */
    pAIC->AIC_IDCR = (1<<AT91C_ID_US0);      /* Disable USART0 interrupt in AIC */
    pAIC->AIC_SVR[AT91C_ID_US0] =
        (unsigned int)Usart0IrqHandler; /* Set the USART0 IRQ handler address in AIC  */
    /* Source Vector Register[6] */
    pAIC->AIC_SMR[AT91C_ID_US0] =
        (AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL | 0x4 ); /* Set the int source type and pri  */
    /* in AIC Source Mode Register[6] */
    pAIC->AIC_IECR = (1<<AT91C_ID_US0);  /* Enable the USART0 interrupt in AIC */
#endif

    pUSART->US_CR =
        AT91C_US_RXEN  |          /* Receiver Enable     */
        AT91C_US_TXEN;            /* Transmitter Enable  */

#if USE_UART_INTERRUPTS
    /* enable the USART0 receive interrupt */
    pUSART0->US_IER = AT91C_US_RXRDY; /* enable RXRDY usart0 receive interrupt */
    pUSART0->US_IDR = ~AT91C_US_RXRDY; /* disable all other interrupts except RXRDY */
    /* set up buffer pointer and character counter */
    pBuffer = &Buffer[0];
    nChars = 0;
    /* enable IRQ interrupts */
    enableIRQ();
#endif
}
#endif  // JIM_LYNCH

void uart0_init(int baud_rate)
{
    int brd = (MCK / 16) / baud_rate;

    *AT91C_PIOA_PDR =
        AT91C_PA5_RXD0 |          /* Enable RxD0 Pin */
        AT91C_PA6_TXD0;           /* Enalbe TxD0 Pin */

    pUSART->US_CR =
        AT91C_US_RSTRX |          /* Reset Receiver      */
        AT91C_US_RSTTX |          /* Reset Transmitter   */
        AT91C_US_RXDIS |          /* Receiver Disable    */
        AT91C_US_TXDIS;           /* Transmitter Disable */

    pUSART->US_MR =
        AT91C_US_USMODE_NORMAL |  /* Normal Mode */
        AT91C_US_CLKS_CLOCK    |  /* Clock = MCK */
        AT91C_US_CHRL_8_BITS   |  /* 8-bit Data  */
        AT91C_US_PAR_NONE      |  /* No Parity   */
        AT91C_US_NBSTOP_1_BIT;    /* 1 Stop Bit  */

    pUSART->US_BRGR = brd;        /* Baud Rate Divisor */

    pUSART->US_CR =
        AT91C_US_RXEN  |          /* Receiver Enable     */
        AT91C_US_TXEN;            /* Transmitter Enable  */
}

int uart0_txready(void)
{
    return (pUSART->US_CSR & AT91C_US_TXRDY);
}

void uart0_putc(int ch)
{
    while (!uart0_txready());
    pUSART->US_THR = ch;                          /* Transmit Character */
}

static void uart0_putchar (int ch)   /* Write Character to Serial Port */
{

    if (ch == '\n')  {                            /* Check for LF */
        uart0_putc('\r');                         /* Output CR */
    }
    uart0_putc(ch);                               /* Transmit Character */
}

void uart0_puts (char* s)
{
    while (*s) uart0_putchar(*s++);
}

/* returns true if character in receive buffer */
int uart0_kbhit(void)
{
    return (pUSART->US_CSR & AT91C_US_RXRDY) != 0;
}

/* Read Character from Serial Port */
int uart0_getc(void)
{
    if (!uart0_kbhit()) return -1;                /* -1 if no char available to read */
    return pUSART->US_RHR;                        /* Read Character */
}
