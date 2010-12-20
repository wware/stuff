/* Startup Code for Atmel AT91SAM7S256
   by Adam Pierce <adam@doctort.org>
   Cut down from a more complex example by Jim Lynch
*/

.global _init

.text
.arm   /* Compile for 32bit instruction set */
.align /* align to 32-bit boundary */

_init:
	/* Copy initialized variables from .text to .data section */
	ldr     R1, =_etext
        ldr     R2, =_data
        ldr     R3, =_edata
1:
	cmp     R2, R3
        ldrlo   R0, [R1], #4
        strlo   R0, [R2], #4
        blo     1b

	/* Clear uninitialized variables (.bss section) */
        mov     R0, #0
        ldr     R1, =_bss_start
        ldr     R2, =_bss_end
2:
	cmp     R1, R2
        strlo   R0, [R1], #4
        blo     2b

	/* Run the C code */
        b       main

.end
