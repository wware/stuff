/******************************************************************************//*                                                                            */
/*                                                                            */
/*  EXTINT.C:  External Interrupt                                             */
/*                                                                            */
/******************************************************************************/
/*  ported to arm-elf-gcc / WinARM by Martin Thomas, KL, .de                  */
/*  <eversmith@heizung-thomas.de>                                             */
/*  modifications Copyright Martin Thomas 2005                                */
/*                                                                            */
/*  Based on file that has been a part of the uVision/ARM development         */
/*  tools, Copyright KEIL ELEKTRONIK GmbH 2002-2004                           */
/******************************************************************************/

/* 
  - changed ISR "in" and "out" for gcc
  - adapted "ramfunc" to gcc
*/

#include <AT91SAM7S64.h>                    /* AT91SAMT7S64 definitions */
#include "Board.h"
#include "interrupt_utils.h"

#ifdef ERAM   /* Fast IRQ functions Run in RAM  - see board.h */
#define ATTR RAMFUNC
#else
#define ATTR
#endif


#if 0
// mt: assembler code ported to gcc in interrupt_utils.h
// Keil original:
// Macros for Interrupt Nesting
#define IENABLE                             /* Nested Interrupts Entry */   \
  __asm { MRS     LR, SPSR      }           /* Copy SPSR_irq to LR     */   \
  __asm { STMFD   SP!, {LR}     }           /* Save SPSR_irq           */   \
  __asm { MSR     CPSR_c, #0x1F }           /* Enable IRQ (Sys Mode)   */   \
  __asm { STMFD   SP!, {LR}     }           /* Save LR                 */ 

#define IDISABLE                            /* Nested Interrupts Exit  */   \
  __asm { LDMFD   SP!, {LR}     }           /* Restore LR              */   \
  __asm { MSR     CPSR_c, #0x92 }           /* Disable IRQ (IRQ Mode)  */   \
  __asm { LDMFD   SP!, {LR}     }           /* Restore SPSR_irq to LR  */   \
  __asm { MSR     SPSR_cxsf, LR }           /* Copy LR to SPSR_irq     */ 
#endif
 

extern volatile AT91S_PIO * pPIO;                    /* Global Pointer to PIO */


// void irq0_int (void) __irq __atr {          /* IRQ0 (Push button SW2)    */
//void INTFUNC ATTR irq0_int (void) {          /* IRQ0 (Push button SW2)    */
void NACKEDFUNC ATTR irq0_int (void) {          /* IRQ0 (Push button SW2)    */

  ISR_STORE(); 
  ISR_ENABLE_NEST();                        /* Enable Interrupt nesting  */
  if ((pPIO->PIO_PDSR & SW2) == 0) {       /* Check if SW2 is pressed   */
    pPIO->PIO_CODR = LED2;                  /* Turn On LED2              */
    while ((pPIO->PIO_PDSR & SW2) == 0);   /* Wait until SW2 is released */
    pPIO->PIO_SODR = LED2;                  /* Turn Off LED2             */
  }
  ISR_DISABLE_NEST();                       /* Disable Interrupt nesting */
  *AT91C_AIC_EOICR = 0;                     /* End of Interrupt          */
  ISR_RESTORE();
}


void init_extint (void) {                   /* Setup IRQ 0 Interrupt */
  volatile AT91S_AIC * pAIC = AT91C_BASE_AIC;

  /* Setup IRQ0 Interrupt Mode and Vector with Priority 0 and Enable it */
  // mt: pAIC->AIC_SMR[AT91C_ID_IRQ0] = AT91C_AIC_SRCTYPE_INT_EDGE_TRIGGERED | 0;
  pAIC->AIC_SMR[AT91C_ID_IRQ0] = AT91C_AIC_SRCTYPE_INT_POSITIVE_EDGE | 0;
 
  pAIC->AIC_SVR[AT91C_ID_IRQ0] = (unsigned long) irq0_int;
  pAIC->AIC_IECR = (1 << AT91C_ID_IRQ0);
}
