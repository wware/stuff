/* ****************************************************************************************************** */
/*											crt.s		  */
/*													  */
/*			 Assembly Language Startup Code for Atmel AT91SAM7S256				  */
/*													  */
/*													  */
/*													  */
/*													  */
/* Author:  James P Lynch      May 12, 2007								  */
/* ****************************************************************************************************** */

/* Stack Sizes */
.set  UND_STACK_SIZE, 0x00000010	/* stack for "undefined instruction" interrupts is 16 bytes */
.set  ABT_STACK_SIZE, 0x00000010	/* stack for "abort" interrupts is 16 bytes */
.set  FIQ_STACK_SIZE, 0x00000080	/* stack for "FIQ" interrupts  is 128 bytes */
.set  IRQ_STACK_SIZE, 0X00000080	/* stack for "IRQ" normal interrupts is 128 bytes */
.set  SVC_STACK_SIZE, 0x00000080	/* stack for "SVC" supervisor mode is 128 bytes */

/* Standard definitions of Mode bits and Interrupt (I & F) flags in PSRs (program status registers) */
.set  ARM_MODE_USR, 0x10		/* Normal User Mode */
.set  ARM_MODE_FIQ, 0x11		/* FIQ Processing Fast Interrupts Mode */
.set  ARM_MODE_IRQ, 0x12		/* IRQ Processing Standard Interrupts Mode */
.set  ARM_MODE_SVC, 0x13		/* Supervisor Processing Software Interrupts Mode */
.set  ARM_MODE_ABT, 0x17		/* Abort Processing memory Faults Mode */
.set  ARM_MODE_UND, 0x1B		/* Undefined Processing Undefined Instructions Mode */
.set  ARM_MODE_SYS, 0x1F		/* System Running Priviledged Operating System Tasks  Mode */
.set  I_BIT, 0x80			/* when I bit is set, IRQ is disabled (program status registers) */
.set  F_BIT, 0x40			/* when F bit is set, FIQ is disabled (program status registers) */

/* Addresses and offsets of AIC */
.set  AT91C_BASE_AIC, 0xFFFFF000	/* (AIC) Base Address */
.set  AT91C_AIC_IVR, 0xFFFFF100		/* (AIC) IRQ Interrupt Vector Register */
.set  AT91C_AIC_FVR, 0xFFFFF104		/* (AIC) FIQ Interrupt Vector Register */
.set  AIC_IVR, 256			/* IRQ Vector Register offset from base above */
.set  AIC_FVR, 260			/* FIQ Vector Register offset from base above */
.set  AIC_EOICR, 304			/* End of Interrupt Command Register */

/* Addresses and offsets of PIO */
.set  AT91C_PIOA_PER, 0xFFFFF400	/* PIO Enable Register */
.set  AT91C_PIOA_PDR, 0xFFFFF404	/* PIO Disable Register */
.set  AT91C_PIOA_PSR, 0xFFFFF408	/* PIO Status Register */

.set  AT91C_PIOA_OER, 0xFFFFF410	/* PIO Output Enable Register */
.set  AT91C_PIOA_ODR, 0xFFFFF414	/* PIO Output Disable Register */
.set  AT91C_PIOA_OSR, 0xFFFFF418	/* PIO Output Status Register */

.set  AT91C_PIOA_IFER, 0xFFFFF420	/* PIO Input Filter Enable Register */
.set  AT91C_PIOA_IFDR, 0xFFFFF424	/* PIO Input Filter Disable Register */
.set  AT91C_PIOA_IFSR, 0xFFFFF428	/* PIO Input Filter Status Register */

.set  AT91C_PIOA_SODR, 0xFFFFF430	/* PIO Set Output Data Register */
.set  AT91C_PIOA_CODR, 0xFFFFF434	/* PIO Clear Output Data Register */
.set  AT91C_PIOA_ODSR, 0xFFFFF438	/* PIO Output Data Status Register */
.set  AT91C_PIOA_PDSR, 0xFFFFF43C	/* PIO Pin Data Status Register */

.set  AT91C_PIOA_IER, 0xFFFFF440	/* PIO Interrupt Enable Register */
.set  AT91C_PIOA_IDR, 0xFFFFF444	/* PIO Interrupt Disable Register */
.set  AT91C_PIOA_IMR, 0xFFFFF448	/* PIO Interrupt Status Register */
.set  AT91C_PIOA_ISR, 0xFFFFF44C	/* PIO Interrupt Status Register */

.set  AT91C_PIOA_MDER, 0xFFFFF450	/* PIO Multi-driver Enable Register */
.set  AT91C_PIOA_MDDR, 0xFFFFF454	/* PIO Multi-driver Disable Register */
.set  AT91C_PIOA_MDSR, 0xFFFFF458	/* PIO Multi-driver Status Register */

.set  AT91C_PIOA_PPUDR, 0xFFFFF460	/* PIO Pull-up Disable Register */
.set  AT91C_PIOA_PPUER, 0xFFFFF464	/* PIO Pull-up Enable Register */
.set  AT91C_PIOA_PPUSR, 0xFFFFF468	/* PIO Pull-up Status Register */

/* identify all GLOBAL symbols */
.global _vec_reset
.global _vec_undef
.global _vec_swi
.global _vec_pabt
.global _vec_dabt
.global _vec_rsv
.global _vec_irq
.global _vec_fiq
.global AT91F_Irq_Handler
.global AT91F_Fiq_Handler
.global AT91F_Default_FIQ_handler
.global AT91F_Default_IRQ_handler
.global AT91F_Spurious_handler
.global AT91F_Dabt_Handler
.global AT91F_Pabt_Handler
.global AT91F_Undef_Handler


/* GNU assembler controls */
.text			/* all assembler code that follows will go into .text section */
.arm			/* compile for 32-bit ARM instruction set */
.align			/* align section on 32-bit boundary */

/* ============================================================ */
/*			VECTOR TABLE				*/
/*								*/
/*	Must be located in FLASH at address 0x00000000		*/
/*								*/
/*	Easy to do if this file crt.s is first in the list	*/
/*	for the linker step in the makefile, e.g.		*/
/*								*/
/*	    $(LD) $(LFLAGS) -o main.out	 crt.o main.o		*/
/*								*/
/* ============================================================ */

_vec_reset:	b	_init_reset		/* RESET vector - must be at 0x00000000 */
_vec_undef:	b	AT91F_Undef_Handler	/* Undefined Instruction vector */
_vec_swi:	b	_vec_swi		/* Software Interrupt vector */
_vec_pabt:	b	AT91F_Pabt_Handler	/* Prefetch abort vector */
_vec_dabt:	b	AT91F_Dabt_Handler	/* Data abort vector */
_vec_rsv:	nop				/* Reserved vector */
_vec_irq:	b	AT91F_Irq_Handler	/* Interrupt Request (IRQ) vector */
_vec_fiq:					/* Fast interrupt request (FIQ) vector	*/

/* ==================================================================== */
/* Function:			AT91F_Fiq_Handler			*/
/*									*/
/* The FIQ interrupt asserts when switch SW1 is pressed.		*/
/*									*/
/* This simple FIQ handler turns on LED3 (Port PA2). The LED3 will be	*/
/* turned off by the background loop in main() thus giving a visual	*/
/* indication that the interrupt has occurred.				*/
/*									*/
/* This FIQ_Handler supports non-nested FIQ interrupts (a FIQ interrupt */
/* cannot itself be interrupted).					*/
/*									*/
/* The Fast Interrupt Vector Register (AIC_FVR) is read to clear the	*/
/* interrupt.								*/
/*									*/
/* A global variable FiqCount is also incremented.			*/
/*									*/
/* Remember that switch SW1 is not debounced, so the FIQ interrupt may	*/
/* occur more than once for a single button push.			*/
/*									*/
/* Programmer: James P Lynch						*/
/* ==================================================================== */
AT91F_Fiq_Handler:

	/* Adjust LR_irq */
	sub	lr, lr, #4

	/* Read the AIC Fast Interrupt Vector register to clear the interrupt */
	ldr	r12, =AT91C_AIC_FVR
	ldr	r11, [r12]

	/* Turn on LED3 (write 0x0008 to PIOA_CODR at 0xFFFFF434) */
	ldr	r12, =AT91C_PIOA_CODR
	mov	r11, #0x04
	str	r11, [r12]

	/* Increment the _FiqCount variable */
	/*
	ldr	r12, =FiqCount
	ldr	r11, [r12]
	add	r11, r11, #1
	str	r11, [r12]
	*/

	/* Return from Fiq interrupt */
	movs	pc, lr


/* ==================================================================================== */
/*				  _init_reset Handler					*/
/*											*/
/*	 RESET vector 0x00000000 branches to here.					*/
/*											*/
/*	 ARM microprocessor begins execution after RESET at address 0x00000000		*/
/*   in Supervisor mode with interrupts disabled!					*/
/*											*/
/*	 _init_reset handler:  creates a stack for each ARM mode.			*/
/*			       sets up a stack pointer for each ARM mode.		*/
/*			       turns off interrupts in each mode.			*/
/*						   leaves CPU in SYS (System) mode.	*/
/*											*/
/*			       block copies the initializers to .data section		*/
/*						   clears the .bss section to zero	*/
/*											*/
/*						   branches to main( )			*/
/* ==================================================================================== */
.text	/* all assembler code that follows will go into .text section */
.align	/* align section on 32-bit boundary */
.global _init_reset
_init_reset:

	/* Setup a stack for each mode with interrupts initially disabled. */
	ldr   r0, =_stack_end				/* r0 = top-of-stack  */

	msr   CPSR_c, #ARM_MODE_UND|I_BIT|F_BIT		/* switch to Undefined Instruction Mode	 */
	mov   sp, r0					/* set stack pointer for UND mode  */
	sub   r0, r0, #UND_STACK_SIZE			/* adjust r0 past UND stack  */

	msr   CPSR_c, #ARM_MODE_ABT|I_BIT|F_BIT		/* switch to Abort Mode */
	mov   sp, r0					/* set stack pointer for ABT mode  */
	sub   r0, r0, #ABT_STACK_SIZE			/* adjust r0 past ABT stack  */

	msr   CPSR_c, #ARM_MODE_FIQ|I_BIT|F_BIT		/* switch to FIQ Mode */
	mov   sp, r0					/* set stack pointer for FIQ mode  */
	sub   r0, r0, #FIQ_STACK_SIZE			/* adjust r0 past FIQ stack  */

	msr   CPSR_c, #ARM_MODE_IRQ|I_BIT|F_BIT		/* switch to IRQ Mode */
	mov   sp, r0					/* set stack pointer for IRQ mode  */
	sub   r0, r0, #IRQ_STACK_SIZE			/* adjust r0 past IRQ stack  */

	msr   CPSR_c, #ARM_MODE_SVC|I_BIT|F_BIT		/* switch to Supervisor Mode */
	mov   sp, r0					/* set stack pointer for SVC mode  */
	sub   r0, r0, #SVC_STACK_SIZE			/* adjust r0 past SVC stack  */

	msr   CPSR_c, #ARM_MODE_SYS|I_BIT|F_BIT		/* switch to System Mode */
	mov   sp, r0					/* set stack pointer for SYS mode  */

	/* Start execution in SYSTEM mode.  This is exactly like USER mode */
	/* (same stack) but SYSTEM mode has more privileges. */

	/* copy initialized variables .data section  (Copy from ROM to RAM) */
	ldr	R1, =_etext
	ldr	R2, =_data
	ldr	R3, =_edata
1:	cmp	R2, R3
	ldrlo	R0, [R1], #4
	strlo	R0, [R2], #4
	blo	1b

	/* Clear uninitialized variables .bss section (Zero init)  */
	mov	R0, #0
	ldr	R1, =_bss_start
	ldr	R2, =_bss_end
2:	cmp	R1, R2
	strlo	R0, [R1], #4
	blo	2b

	/* Enter the C code  */
	b	main


/* ============================================================================ */
/* Function:			AT91F_Irq_Handler				*/
/*										*/
/* This IRQ_Handler supports nested interrupts (an IRQ interrupt can itself	*/
/* be interrupted).								*/
/*										*/
/* This handler re-enables interrupts and switches to "Supervisor" mode to	*/
/* prevent any corruption to the link and IP registers.				*/
/*										*/
/* The Interrupt Vector Register (AIC_IVR) is read to determine the address	*/
/* of the required interrupt service routine. The ISR routine can be a		*/
/* standard C function since this handler minds all the save/restore		*/
/* protocols.									*/
/*										*/
/*										*/
/* Programmers:									*/
/*------------------------------------------------------------------------------*/
/*	   ATMEL Microcontroller Software Support  -  ROUSSET  -		*/
/*------------------------------------------------------------------------------*/
/* DISCLAIMER:	THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS	*/
/* OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED		*/
/* WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND		*/
/* NON-INFRINGEMENT ARE DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR	*/
/* ANY DIRECT, INDIRECT,    INCIDENTAL, SPECIAL, EXEMPLARY, OR			*/
/* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT	LIMITED TO, PROCUREMENT		*/
/* OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,	  OR PROFITS; OR	*/
/* BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF	 LIABILITY,	*/
/* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING	NEGLIGENCE	*/
/* OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,		*/
/* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.				*/
/* File source		: Cstartup.s79						*/
/* Object		: Generic CStartup to AT91SAM7S256			*/
/* 1.0 09/May/06 JPP	: Creation						*/
/*										*/
/*										*/
/* Note: taken from Atmel web site (www.at91.com)				*/
/*		 Keil example project:	AT91SAM7S-Interrupt_SAM7S		*/
/* ============================================================================ */

AT91F_Irq_Handler:
	/* Manage Exception Entry */
	/* Adjust and save LR_irq in IRQ stack */
	sub	lr, lr, #4
	stmfd	sp!, {lr}

	/* Save r0 and SPSR (need to be saved for nested interrupt)  */
	mrs	r14, SPSR
	stmfd	sp!, {r0,r14}

	/* Write in the IVR to support Protect Mode				*/
	/* No effect in Normal Mode								*/
	/* De-assert the NIRQ and clear the source in Protect Mode  */
	ldr	r14, =AT91C_BASE_AIC
	ldr	r0 , [r14, #AIC_IVR]
	str	r14, [r14, #AIC_IVR]

	/* Enable Interrupt and Switch in Supervisor Mode  */
	msr	CPSR_c, #ARM_MODE_SVC

	/* Save scratch/used registers and LR in User Stack  */
	stmfd	sp!, { r1-r3, r12, r14}

	/* Branch to the routine pointed by the AIC_IVR	 */
	mov	r14, pc
	bx	r0

	/* Manage Exception Exit								  */
	/* Restore scratch/used registers and LR from User Stack  */
	ldmia	sp!, { r1-r3, r12, r14}

	/* Disable Interrupt and switch back in IRQ mode  */
	msr	CPSR_c, #I_BIT | ARM_MODE_IRQ

	/* Mark the End of Interrupt on the AIC	 */
	ldr	r14, =AT91C_BASE_AIC
	str	r14, [r14, #AIC_EOICR]

	/* Restore SPSR_irq and r0 from IRQ stack  */
	ldmia	sp!, {r0,r14}
	msr	SPSR_cxsf, r14

	/* Restore adjusted  LR_irq from IRQ stack directly in the PC  */
	ldmia	sp!, {pc}^


/*
 * What makes sense with these abort handlers and other troubles is to do five
 * sets of flashes. The first set shows which trouble it is. The next four are
 * the four low hex digits of the location where the trouble happened. Maybe
 * that should be six hex digits? And they should be done like Morse code, so
 * a 7 is short-long-long-long and a C is long-long-short-short.
 *
 * How do I get that address? It would be in the link register, right? In fact
 * the link register is 8 bytes ahead of the address where the problem occurred
 * so subtract 8 first, then pass it into a function that does the blinking.
 */


/* ============================================================================ */
/* Function:			AT91F_Dabt_Handler				*/
/*										*/
/* Entered on Data Abort exception.						*/
/* Enters blink routine	 (3 blinks followed by a pause)				*/
/* processor hangs in the blink loop forever					*/
/*										*/
/* ============================================================================ */
AT91F_Dabt_Handler:
	mov	R0, #3
	mov	R1, lr
	b	blink_number


/* ============================================================================ */
/* Function:			AT91F_Pabt_Handler				*/
/*										*/
/* Entered on Prefetch Abort exception.						*/
/* Enters blink routine	 (2 blinks followed by a pause)				*/
/* processor hangs in the blink loop forever					*/
/*										*/
/* ============================================================================ */
AT91F_Pabt_Handler:
	mov	R0, #2
	mov	R1, lr
	b	blink_number


/* ============================================================================ */
/* Function:			AT91F_Undef_Handler				*/
/*										*/
/* Entered on Undefined Instruction exception.					*/
/* Enters blink routine	 (1 blinks followed by a pause)				*/
/* processor hangs in the blink loop forever					*/
/*										*/
/* ============================================================================ */
AT91F_Undef_Handler:
	mov	R0, #1
	mov	R1, lr
	b	blink_number


AT91F_Default_FIQ_handler:
	mov	pc, lr

AT91F_Default_IRQ_handler:
	mov	pc, lr

AT91F_Spurious_handler:
	mov	pc, lr
.end
