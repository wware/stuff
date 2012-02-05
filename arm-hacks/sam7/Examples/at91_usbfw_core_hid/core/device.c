/* ----------------------------------------------------------------------------
 *         ATMEL Microcontroller Software Support  -  ROUSSET  -
 * ----------------------------------------------------------------------------
 * Copyright (c) 2006, Atmel Corporation

 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the disclaiimer below.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the disclaimer below in the documentation and/or
 * other materials provided with the distribution.
 *
 * Atmel's name may not be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * DISCLAIMER: THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE
 * DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ----------------------------------------------------------------------------
 */

/*
$Id: device.c 108 2006-10-16 08:33:33Z jjoannic $
*/

/* modified by Martin Thomas - search for mthomas to find the modifications */

//------------------------------------------------------------------------------
//      Includes
//------------------------------------------------------------------------------

#include "common.h"
#include "device.h"
#include "board.h"

//------------------------------------------------------------------------------
//      Exported functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// \brief   Low-level initialization of the chip.
//------------------------------------------------------------------------------
void DEV_Init(void)
{
    // PIO Controller initialization for low-power consumption

#if defined(AT91SAM7SEK) || defined(AT91SAM7SEEK) || defined(AT91SAM7XEK)
    // PIOA
    // Set to 1 output PIO
    AT91F_PIO_SetOutput( AT91C_BASE_PIOA, 0xFFFFFFFF );
    // Enable PIO in output mode
    AT91F_PIO_CfgOutput( AT91C_BASE_PIOA, 0xFFFFFFFF );
    // Disable pullup on PIO
    AT91F_PIO_CfgPullup( AT91C_BASE_PIOA, 0x00000000 );
#endif

#if defined(AT91SAM7SEEK) || defined (AT91SAM7XEK)
    // PIOB
    // Set to 1 output PIO
    AT91F_PIO_SetOutput( AT91C_BASE_PIOB, 0xFFFFFFFF );
    // Enable PIO in output mode
    AT91F_PIO_CfgOutput( AT91C_BASE_PIOB, 0xFFFFFFFF );
    // Disable pullup on PIO
    AT91F_PIO_CfgPullup( AT91C_BASE_PIOB, 0x00000000 );
#endif

#if defined(AT91SAM7SEEK)
    // PIOC
    // Set to 1 output PIO
    AT91F_PIO_SetOutput( AT91C_BASE_PIOC, 0xFFFFFFFF );
    // Enable PIO in output mode
    AT91F_PIO_CfgOutput( AT91C_BASE_PIOC, 0xFFFFFFFF );
    // Disable pullup on PIO
    AT91F_PIO_CfgPullup( AT91C_BASE_PIOC, 0x00000000 );
#endif

#if !defined(AT91RM9200)
    // Disable the watchdog
    AT91C_BASE_WDTC->WDTC_WDMR = AT91C_WDTC_WDDIS;

    // Enable user reset: assertion length programmed to 1ms
    AT91C_BASE_RSTC->RSTC_RMR = AT91C_RSTC_URSTEN | (0x4 << 8)
                                | ((unsigned int) 0xA5 << 24);
#endif

    // Start clocks
    DEV_Resume();
}

//------------------------------------------------------------------------------
// \brief   Puts the device back into a normal operation mode
//------------------------------------------------------------------------------
void DEV_Resume(void)
{

#if defined(AT91SAM7SEK) || defined(AT91SAM7SEEK) \
    || defined(AT91SAM7XEK) || defined(AT91SAM7A3EK)

    // Enable Main Oscillator
    // Main Oscillator startup time is board specific:
    // Main Oscillator Startup Time (18432KHz) corresponds to 1.5ms
    // (0x08 for AT91C_CKGR_OSCOUNT field)
    #if defined(AT91SAM7SEK) || defined(AT91SAM7SEEK) || defined(AT91SAM7XEK)

    // mthomas start - line added according to information from the WinARM support-forum
    AT91C_BASE_MC->MC_FMR = AT91C_MC_FWS_1FWS;
    // mthomas end

    AT91C_BASE_PMC->PMC_MOR = (AT91C_CKGR_OSCOUNT & (0x8 << 8))
                              | AT91C_CKGR_MOSCEN;
    #elif defined(AT91SAM7A3EK)
    AT91C_BASE_PMC->PMC_MOR = (AT91C_CKGR_OSCOUNT & (0x40 << 8))
                              | AT91C_CKGR_MOSCEN;
    #endif

    // Wait until the oscillator is stabilized
    while (!ISSET(AT91C_BASE_PMC->PMC_SR, AT91C_PMC_MOSCS));

    // Set PLL to 96MHz (96,109MHz) and UDP Clock to 48MHz
    // PLL Startup time depends on PLL RC filter
    // UDP Clock (48,058MHz) is compliant with the Universal Serial Bus
    // Specification (+/- 0.25% for full speed)
    AT91C_BASE_PMC->PMC_PLLR = AT91C_CKGR_USBDIV_1
                               | AT91C_CKGR_OUT_0
                               | (AT91C_CKGR_PLLCOUNT & (0x28 << 8))
                               | (AT91C_CKGR_MUL & (0x48 << 16))
                               | (AT91C_CKGR_DIV & 0xE);

    // Wait until the PLL is stabilized
    while(!ISSET(AT91C_BASE_PMC->PMC_SR, AT91C_PMC_LOCK));

    // Selection of Master Clock MCK (equal to Processor Clock PCK) equal to
    // PLL/2 = 48MHz
    // The PMC_MCKR register must not be programmed in a single write operation
    // (see. Product Errata Sheet)
    AT91C_BASE_PMC->PMC_MCKR = AT91C_PMC_PRES_CLK_2;
    while (!ISSET(AT91C_BASE_PMC->PMC_SR, AT91C_PMC_MCKRDY));

    SET(AT91C_BASE_PMC->PMC_MCKR, AT91C_PMC_CSS_PLL_CLK);
    while (!ISSET(AT91C_BASE_PMC->PMC_SR, AT91C_PMC_MCKRDY));

#elif defined(AT91SAM9261EK)
    AT91C_BASE_PMC->PMC_IDR = 0xFFFFFFFF;

    // Setup the PLL A
    AT91C_BASE_CKGR->CKGR_PLLAR = 0x2060BF09; // crystal= 18.432MHz

    while (!(*AT91C_PMC_SR & AT91C_PMC_LOCKA));

    // Switch Master Clock from PLLB to PLLA/2
    AT91C_BASE_PMC->PMC_MCKR = AT91C_PMC_CSS_PLLA_CLK | AT91C_PMC_PRES_CLK
                             | AT91C_PMC_MDIV_2;
    while (!(*AT91C_PMC_SR & AT91C_PMC_MCKRDY));

#elif defined(AT91RM9200)

    // Checking the Main Oscillator Frequency (Optional)
    volatile char    tmp    = 0;

    AT91F_PIOD_CfgPMC();  // VBUS
    // Checking the Main Oscillator Frequency (Optional)
    // Determine the main clock frequency
    while(!(AT91C_BASE_CKGR->CKGR_MCFR & AT91C_CKGR_MAINRDY) && (tmp++ < 100));

#endif
}

//------------------------------------------------------------------------------
// \brief   Puts the device into low-power mode.
//------------------------------------------------------------------------------
void DEV_Suspend()
{
#if defined(AT91SAM7SEK) || defined(AT91SAM7SEEK) || defined(AT91SAM7XEK)

    // Voltage regulator in standby mode : Enable VREG Low Power Mode
    AT91C_BASE_VREG->VREG_MR |= AT91C_VREG_PSTDBY;

    // Set the master clock on slow clock
    AT91F_PMC_CfgMCKReg(AT91C_BASE_PMC, AT91C_PMC_PRES_CLK_2);
    while (!ISSET(AT91C_BASE_PMC->PMC_SR, AT91C_PMC_MCKRDY));

    AT91F_PMC_CfgMCKReg(AT91C_BASE_PMC, AT91C_PMC_CSS_SLOW_CLK);
    while (!ISSET(AT91C_BASE_PMC->PMC_SR, AT91C_PMC_MCKRDY));

    // Disable the PLL
    AT91F_CKGR_CfgPLLReg(AT91C_BASE_CKGR, 0);

    // Disable the main Oscillator
    AT91C_BASE_PMC->PMC_MOR = 0;

#elif defined(AT91SAM7A3) || defined(AT91RM9200) || defined(AT91SAM9260EK) \
   || defined(AT91SAM9261EK)|| defined(AT91SAM9263EK)

#endif
}

