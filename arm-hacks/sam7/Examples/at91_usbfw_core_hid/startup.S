@ * ----------------------------------------------------------------------------
@ *         ATMEL Microcontroller Software Support  -  ROUSSET  -
@ * ----------------------------------------------------------------------------
@ * Copyright (c) 2006, Atmel Corporation
@
@ * All rights reserved.
@ *
@ * Redistribution and use in source and binary forms, with or without
@ * modification, are permitted provided that the following conditions are met:
@ *
@ * - Redistributions of source code must retain the above copyright notice,
@ * this list of conditions and the disclaiimer below.
@ *
@ * - Redistributions in binary form must reproduce the above copyright notice,
@ * this list of conditions and the disclaimer below in the documentation and/or
@ * other materials provided with the distribution.
@ *
@ * Atmel s name may not be used to endorse or promote products derived from
@ * this software without specific prior written permission.
@ *
@ * DISCLAIMER: THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS OR
@ * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
@ * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE
@ * DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR ANY DIRECT, INDIRECT,
@ * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
@ * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES@ LOSS OF USE, DATA,
@ * OR PROFITS@ OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
@ * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
@ * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
@ * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@ * ----------------------------------------------------------------------------

@ $Id: startup.s 106 2006-10-13 15:50:54Z jjoannic $

@ modified by Martin Thomas for GNU arm-elf-as compatiblity
@ this must be processed by the preprocessor before passed
@ to gnu-as (done automaticly thru compiler-frontend - see makefile)

@------------------------------------------------------------------------------
@       Includes
@------------------------------------------------------------------------------

#if 1
#include "AT91SAM7S64_inc.h"
#elif defined(AT91SAM7S321)
#include "AT91SAM7S321_inc.h"
#elif defined(AT91SAM7S64)
// #warning "jep"
#include "AT91SAM7S64_inc.h"
#elif defined(AT91SAM7S128)
#include "AT91SAM7S128_inc.h"
#elif defined(AT91SAM7S256)
#include "AT91SAM7S256_inc.h"
#elif defined(AT91SAM7S512)
#include "AT91SAM7S512_inc.h"

#elif defined(AT91SAM7SE32)
#include "AT91SAM7SE32_inc.h"
#elif defined(AT91SAM7SE256)
#include "AT91SAM7SE256_inc.h"
#elif defined(AT91SAM7SE512)
#include "AT91SAM7SE512_inc.h"

#elif defined(AT91SAM7X128)
#include "AT91SAM7X128_inc.h"
#elif defined(AT91SAM7X256)
#include "AT91SAM7X256_inc.h"
#elif defined(AT91SAM7X512)
#include "AT91SAM7X512_inc.h"

#elif defined(AT91SAM7A3)
#include "AT91SAM7A3_inc.h"

#elif defined(AT91RM9200)
#include "AT91RM9200_inc.h"

#elif defined(AT91SAM9260)
#include "AT91SAM9260_inc.h"
#elif defined(AT91SAM9261)
#include "AT91SAM9261_inc.h"
#elif defined(AT91SAM9263)
#include "AT91SAM9263_inc.h"
#elif defined(AT91SAM9265)
#include "AT91SAM9265_inc.h"
#elif defined(AT91SAM926C)
#include "AT91SAM926C_inc.h"
#else
#error "no defintion for target"
#endif

@-------------------------------------------------------------------------------
@       Constants
@-------------------------------------------------------------------------------

@-- ARM processor modes
.equ ARM_MODE_USER,                0x10
.equ ARM_MODE_FIQ,                 0x11
.equ ARM_MODE_IRQ,                 0x12
.equ ARM_MODE_SVC,                 0x13
.equ ARM_MODE_ABORT,               0x17
.equ ARM_MODE_UNDEF,               0x1B
.equ ARM_MODE_SYS,                 0x1F

@-- Status register bits
.equ I_BIT,                        0x80
.equ F_BIT,                        0x40

@-- Stack sizes
.equ IRQ_STACK_SIZE,               (3*8*4)        @( 3 stacks 8 vectors 4 bytes)
.equ FIQ_STACK_SIZE,               0x004
.equ ABT_STACK_SIZE,               0x004
.equ UND_STACK_SIZE,               0x004
.equ SVC_STACK_SIZE,               0x800
.equ SYS_STACK_SIZE,               0x400

@-------------------------------------------------------------------------------
@       Entry point
@-------------------------------------------------------------------------------
@@    AREA        reset, CODE, READONLY
.section .init, "ax"

@@    EXPORT    __ENTRY
@@__ENTRY
.global __ENTRY
__ENTRY:

@-------------------------------------------------------------------------------
@- Exception vectors ( before Remap )
@-------------------------------------------------------------------------------
@- These vectors are read at address 0.
@- They absolutely requires to be in relative addresssing mode in order to
@- guarantee a valid jump. For the moment, all are just looping (what may be
@- dangerous in a final system). If an exception occurs before remap, this
@- would result in an infinite loop.
@-------------------------------------------------------------------------------

                B           Reset               @ 0x00 Reset handler
undefvec:
                B           undefvec            @ 0x04 Undefined Instruction
swivec:
                B           swivec              @ 0x08 Software Interrupt
pabtvec:
                B           pabtvec             @ 0x0C Prefetch Abort
dabtvec:
                B           dabtvec             @ 0x10 Data Abort
rsvdvec:
                B           rsvdvec             @ 0x14 reserved
irqvec:
                B           IRQ_Handler         @ 0x18 IRQ : read the AIC
fiqvec:
                B           fiqvec              @ 0x1C FIQ

@-------------------------------------------------------------------------------
@       Reset routine
@-------------------------------------------------------------------------------
Reset:

@---- Stack setup
@---- End of RAM (start of stack) address in r1

@    IF  :DEF:AT91SAM9261
#if defined(AT91SAM9261)

      ldr     r1, =AT91C_IRAM
      add     r1, r1, #AT91C_IRAM_SIZE

@    ELSE
@    IF  :DEF:AT91SAM9260
#elif defined(AT91SAM9260)

      ldr     r1, =AT91C_IRAM_1
      add     r1, r1, #AT91C_IRAM_1_SIZE
@    ELSE
#else
      ldr     r1, =AT91C_ISRAM
      add     r1, r1, #AT91C_ISRAM_SIZE
@    ENDIF
@    ENDIF
#endif

@---- Interrupt mode stack setup
@    msr     CPSR_c, #ARM_MODE_IRQ:OR:I_BIT:OR:F_BIT
    msr     CPSR_c, #ARM_MODE_IRQ | I_BIT | F_BIT
    mov     sp, r1
    sub     r1, r1, #IRQ_STACK_SIZE

@---- Fast interrupt mode stack setup
@    msr     CPSR_c, #ARM_MODE_FIQ:OR:I_BIT:OR:F_BIT
    msr     CPSR_c, #ARM_MODE_FIQ | I_BIT | F_BIT
    mov     sp, r1
    sub     r1, r1, #FIQ_STACK_SIZE

@---- Abort mode stack setup
    msr     CPSR_c, #ARM_MODE_ABORT | I_BIT | F_BIT
    mov     sp, r1
    sub     r1, r1, #ABT_STACK_SIZE

@---- Undefined instruction mode stack setup
    msr     CPSR_c, #ARM_MODE_UNDEF | I_BIT | F_BIT
    mov     sp, r1
    sub     r1, r1, #UND_STACK_SIZE

@---- User/System mode stack setup
    msr     CPSR_c, #ARM_MODE_SYS | I_BIT | F_BIT
    mov     sp, r1
    sub     r1, r1, #SYS_STACK_SIZE

@---- Supervisor mode stack setup
    msr     CPSR_c, #ARM_MODE_SVC | F_BIT
    mov     sp, r1
    sub     r1, r1, #SVC_STACK_SIZE

@-------------------------------------------------------------------------------
@       Low-level init
@-------------------------------------------------------------------------------

@    IMPORT  DEV_Init
.extern DEV_Init

    ldr     r0, =DEV_Init
    mov     lr, pc
    bx      r0

@-------------------------------------------------------------------------------
@       Remap
@-------------------------------------------------------------------------------

@    IF  :DEF:REMAP
#if defined(REMAP)

@---- copy the flash code to RAM
@---- Start of RAM in r0, end of stack space in r1, current address in r2
    ldr     r0, =AT91C_ISRAM
    add     r1, r0, #AT91C_ISRAM_SIZE
    ldr     r2, =AT91C_IFLASH

Remap_copy:
    ldr     r3, [r2], #4
    str     r3, [r0], #4
    cmp     r0, r1
    bne     Remap_copy

@---- Perform remap operation
    ldr     r0, =AT91C_MC_RCR
    mov     r1, #1
    str     r1, [r0]

#else

@-------------------------------------------------------------------------------
@       RW data preinitialization
@-------------------------------------------------------------------------------

@    IF :DEF:DEBUG
#if defined(DEBUG)

@    ELSE
#else

@---- Load addresses
    add     r0, pc, #-(8+.-RW_addresses)
    ldmia   r0, {r1, r2, r3}

@---- Initialize RW data
RW_loop:
    cmp     r2, r3
    ldrne   r0, [r1], #4
    strne   r0, [r2], #4
    bne     RW_loop
    b       RW_end

RW_addresses:
@    IMPORT  |Image$$RO$$Limit|      @ End of ROM code
@    IMPORT  |Image$$RW$$Base|       @ Start of RAM data
@    IMPORT  |Image$$RW$$Limit|      @ End of RAM data

@    DCD     |Image$$RO$$Limit|
@    DCD     |Image$$RW$$Base|
@    DCD     |Image$$RW$$Limit|
     .word _etext
     .word __data_start
     .word _edata

RW_end:

@    ENDIF
@    ENDIF
#endif
#endif

@-------------------------------------------------------------------------------
@       ZI data preinitialization
@-------------------------------------------------------------------------------

@---- Load addresses
    add     r0, pc, #-(8+.-ZI_addresses)
    ldmia   r0, {r1, r2}
    mov     r0, #0

@---- Initialize ZI data
ZI_loop:
    cmp     r1, r2
    strcc   r0, [r1], #4
    bcc     ZI_loop
    b       ZI_end

ZI_addresses:
@    IMPORT  |Image$$ZI$$Base|       @ Base and limit of area
@    IMPORT  |Image$$ZI$$Limit|      @ Top of zero init segment

@    DCD     |Image$$ZI$$Base|
@    DCD     |Image$$ZI$$Limit|
    .word __bss_start__
    .word __bss_end__ 
ZI_end:

@-------------------------------------------------------------------------------
@       Branch on C code Main function (with interworking)
@-------------------------------------------------------------------------------
@ Branch must be performed by an interworking call as either an ARM or Thumb
@ main C function must be supported. This makes the code not position-
@ independant. A Branch with link would generate errors
@-------------------------------------------------------------------------------
@    IMPORT  main
.extern main

    ldr     r0, =main
    mov     lr, pc
    bx      r0

@---- Endless loop
End:
    b       End

@------------------------------------------------------------------------------
@- Function             : IRQ_Handler_Entry
@- Treatments           : IRQ Controller Interrupt Handler.
@- Called Functions     : AIC_IVR[interrupt]
@------------------------------------------------------------------------------

IRQ_Handler:

@---- Adjust and save return address on the stack
    sub     lr, lr, #4
    stmfd   sp!, {lr}

@---- Save r0 and SPSR on the stack
    mrs     r14, SPSR
    stmfd   sp!, {r0, r14}

@---- Write in the IVR to support Protect mode
@---- No effect in Normal Mode
@---- De-assert NIRQ and clear the source in Protect mode
    ldr     r14, =AT91C_BASE_AIC
    ldr     r0, [r14, #AIC_IVR]
    str     r14, [r14, #AIC_IVR]

@---- Enable nested interrupts and switch to Supervisor mode
    msr     CPSR_c, #ARM_MODE_SVC

@---- Save scratch/used registers and LR on the stack
    stmfd   sp!, {r1-r3, r12, r14}

@---- Branch to the routine pointed by AIC_IVR
    mov     r14, pc
    bx      r0

@---- Restore scratch/used registers and LR from the stack
    ldmia   sp!, {r1-r3, r12, r14}

@---- Disable nested interrupts and switch back to IRQ mode
    msr     CPSR_c, #I_BIT | ARM_MODE_IRQ

@---- Acknowledge interrupt by writing AIC_EOICR
    ldr     r14, =AT91C_BASE_AIC
    str     r14, [r14, #AIC_EOICR]

@---- Restore SPSR and r0 from the stack
    ldmia   sp!, {r0, r14}
    msr     SPSR_cxsf, r14

@---- Return from interrupt handler
    ldmia   sp!, {pc}^

@    END
.end

