/*----------------------------------------------------------------------------
*         ATMEL Microcontroller Software Support  -  ROUSSET  -
*----------------------------------------------------------------------------
* The software is delivered "AS IS" without warranty or condition of any
* kind, either express, implied or statutory. This includes without
* limitation any warranty or condition with respect to merchantability or
* fitness for any particular purpose, or against the infringements of
* intellectual property rights of others.
*----------------------------------------------------------------------------
* File Name           : Board.h
* Object              : AT91SAM7S Evaluation Board Features Definition File.
*
* Creation            : JPP   16/Jun/2004
* V 1.0 21/Feb/05 JPP : Define __ramfunc
* V 1.1 21/Feb/05 JPP : add Lib definition
* V 1.2 22/Feb/05 JPP : Add DBGU inline definition
* V 1.3 14/Oct/05 JPP : Change MCK
* slightly modified for the WinARM example - M.Thomas (not Atmel)
*----------------------------------------------------------------------------
*/
#ifndef Board_h
#define Board_h

#include "AT91SAM7S64.h"

// mt:
// define ERAM to place the ISRs in RAM
#define ERAM (1)

#if 0
#ifdef ERAM
#warning "ISRs in RAM activated"
#endif
#endif


extern unsigned int AT91F_PIO_GetInput( AT91PS_PIO pPio);
extern int AT91F_PIO_IsInputSet( AT91PS_PIO pPio, unsigned int flag);
extern void AT91F_PIO_SetOutput(AT91PS_PIO pPio, unsigned int flag);
extern void AT91F_PIO_ClearOutput(AT91PS_PIO pPio, unsigned int flag);
extern void AT91F_PIO_CfgInput( AT91PS_PIO pPio,unsigned int inputEnable);
extern void AT91F_PIO_OutputEnable(AT91PS_PIO pPio,unsigned int flag);
extern unsigned int AT91F_PIO_GetCfgPullup( AT91PS_PIO pPio);
extern void AT91F_PIO_InterruptEnable( AT91PS_PIO pPio,unsigned int flag);
extern void AT91F_PMC_EnablePeriphClock (AT91PS_PMC pPMC, unsigned int periphIds);
extern void AT91F_PIO_CfgPeriph(AT91PS_PIO pPio, unsigned int periphAEnable,unsigned int periphBEnable);
extern void AT91F_PIO_CfgOutput(AT91PS_PIO pPio, unsigned int pioEnable);
// Interrupt
extern unsigned int AT91F_AIC_ConfigureIt (	AT91PS_AIC pAic, unsigned int irq_id, unsigned int priority, unsigned int src_type, void (*newHandler) (void) );
extern void AT91F_AIC_EnableIt (AT91PS_AIC pAic, unsigned int irq_id );
extern void AT91F_AIC_Trig ( AT91PS_AIC pAic, unsigned int irq_id);
// USART
extern void AT91F_US_EnableIt (AT91PS_USART pUSART,unsigned int flag);
extern unsigned int AT91F_US_SendFrame( AT91PS_USART pUSART, char *pBuffer,	unsigned int szBuffer,	char *pNextBuffer,	unsigned int szNextBuffer );
extern int AT91F_US_GetChar (const AT91PS_USART pUSART);
extern void AT91F_US_PutChar (AT91PS_USART pUSART, int character );
extern void AT91F_US_Configure (AT91PS_USART pUSART,unsigned int mainClock,unsigned int mode ,	unsigned int baudRate ,	unsigned int timeguard );
extern void AT91F_DBGU_CfgPIO (void);
extern unsigned int AT91F_US_TxReady (AT91PS_USART pUSART);
extern unsigned int AT91F_US_RxReady (AT91PS_USART pUSART);
#define AT91C_US_ASYNC_MODE ( AT91C_US_USMODE_NORMAL + AT91C_US_NBSTOP_1_BIT + AT91C_US_PAR_NONE + AT91C_US_CHRL_8_BITS + AT91C_US_CLKS_CLOCK )

// mt - addded to export function from "lib"
extern void AT91F_RSTSetMode( AT91PS_RSTC pRSTC, unsigned int mode);


#define true	-1
#define false	0

/*-------------------------------*/
/* SAM7Board Memories Definition */
/*-------------------------------*/
// The AT91SAM7S64 embeds a 16-Kbyte SRAM bank, and 64 K-Byte Flash

#define  INT_SRAM           0x00200000
#define  INT_SRAM_REMAP	    0x00000000

#define  INT_FLASH          0x00000000
#define  INT_FLASH_REMAP    0x01000000

#define  FLASH_PAGE_NB	    512
#define  FLASH_PAGE_LOCK    32
#define  FLASH_PAGE_SIZE    128

/*-----------------*/
/* Leds Definition */
/*-----------------*/
#define LED1            AT91C_PIO_PA8
#define LED2            0  /* no second LED */
#define LED_MASK        (LED1|LED2)
#define NB_LEB			1

/*------------------*/
/* USART Definition */
/*------------------*/
/* SUB-D 9 points J3 DBGU*/
#define DBGU_RXD		AT91C_PA9_DRXD	  /* JP11 must be closed */
#define DBGU_TXD		AT91C_PA10_DTXD	  /* JP12 must be closed */
#define AT91C_DBGU_BAUD	   115200   // Baud rate

#define US_RXD_PIN		AT91C_PA5_RXD0    /* JP9 must be closed */
#define US_TXD_PIN		AT91C_PA6_TXD0    /* JP7 must be closed */
#define US_RTS_PIN		AT91C_PA7_RTS0    /* JP8 must be closed */
#define US_CTS_PIN		AT91C_PA8_CTS0    /* JP6 must be closed */

/*--------------*/
/* Master Clock */
/*--------------*/

#define EXT_OSC         18432000   // External oscillator MAINCK
#define MCK             47923200   // MCK
#define MCKKHz          (MCK/1000)

#endif /* Board_h */
