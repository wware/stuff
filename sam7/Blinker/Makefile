# Our tools.
CC = arm-elf-gcc
LD = arm-elf-ld
AS = arm-elf-as
CP = arm-elf-objcopy
OD = arm-elf-objdump

# Commandline options for each tool.
#CFLAGS  = -I. -I../common -c -fno-common -O0 -g
CFLAGS  = -I. -I../common -c -fno-common -O2
AFLAGS  = -ahls -mapcs-32
CPFLAGS = --output-target=ihex
ODFLAGS	= -x --syms

# Our target.
all: ram-blink.hex flash-blink.hex

prog: flash-blink.hex
	Sam_I_Am set ramwriteallow 0xffffff64 4 , \
		writew ffffff64 5a000004 , \
		writew ffffff64 5a004004 , \
		flash flash-blink.hex , \
		writew ffffff64 5a00020b

run-from-ram: ram-blink.hex
	Sam_I_Am shell sleep 1 , \
		send ram-blink.hex , \
		shell sleep 1 , \
		go 0x202000 , \
		exit

clean:
	rm -f *.o *.hex *.elf *.dmp *.map *.lst core *~ main.s

flash-blink.hex flash-blink.dmp: flash-blink.elf
	@ echo "...copying"
	$(CP) $(CPFLAGS) --change-addresses +0x100000 flash-blink.elf flash-blink.hex
	$(OD) $(ODFLAGS) flash-blink.elf > flash-blink.dmp

ram-blink.hex ram-blink.dmp: ram-blink.elf
	@ echo "...copying"
	$(CP) $(CPFLAGS) ram-blink.elf ram-blink.hex
	$(OD) $(ODFLAGS) ram-blink.elf > ram-blink.dmp

ram-blink.elf: ram-crt.o main.o libsam7.o
	$(LD) -v -Map ram-blink.map \
		-L/opt/gnu-arm/lib/gcc/arm-elf/4.4.0 \
		-L/opt/gnu-arm/arm-elf/lib \
		-T../common/ram-ln.cmd -o ram-blink.elf \
		ram-crt.o main.o libsam7.o -lgcc

flash-blink.elf: flash-crt.o main.o libsam7.o
	$(LD) -v -Map flash-blink.map \
		-L/opt/gnu-arm/lib/gcc/arm-elf/4.4.0 \
		-L/opt/gnu-arm/arm-elf/lib \
		-T../common/flash-ln.cmd -o flash-blink.elf \
		flash-crt.o main.o libsam7.o -lgcc

ram-crt.o: ../common/ram-crt.s
	$(AS) $(AFLAGS) -o ram-crt.o ../common/ram-crt.s > ram-crt.lst

flash-crt.o: ../common/flash-crt.s
	$(AS) $(AFLAGS) -o flash-crt.o ../common/flash-crt.s > flash-crt.lst

main.s: main.c
	$(CC) $(CFLAGS) -S main.c
main.lst: main.s
	$(AS) $(AFLAGS) -o /dev/null main.s > main.lst

main.o: main.c
	$(CC) $(CFLAGS) -c main.c
libsam7.o: ../common/libsam7.c
	$(CC) $(CFLAGS) -c ../common/libsam7.c
