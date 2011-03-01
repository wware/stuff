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
all: flash-serial.hex

prog: flash-serial.hex
	Sam_I_Am set ramwriteallow 0xffffff64 4 , \
		writew ffffff64 5a000004 , \
		writew ffffff64 5a002004 , \
		flash flash-serial.hex , \
		writew ffffff64 5a00020b

run-from-ram: ram-serial.hex
	Sam_I_Am shell sleep 1 , \
		send ram-serial.hex , \
		shell sleep 1 , \
		go 0x202000 , \
		exit

clean:
	rm -f *.o *.hex *.elf *.dmp *.map *.lst core *~ main.s

ram-serial.hex ram-serial.dmp: ram-serial.elf
	@ echo "...copying"
	$(CP) $(CPFLAGS) ram-serial.elf ram-serial.hex
	$(OD) $(ODFLAGS) ram-serial.elf > ram-serial.dmp

ram-serial.elf: ram-crt.o main.o libsam7.o
	$(LD) -v -Map ram-serial.map \
		-L/opt/gnu-arm/lib/gcc/arm-elf/4.4.0 \
		-L/opt/gnu-arm/arm-elf/lib \
		-T../common/ram-ln.cmd -o ram-serial.elf \
		ram-crt.o main.o libsam7.o -lgcc

flash-serial.hex flash-serial.dmp: flash-serial.elf
	@ echo "...copying"
	$(CP) $(CPFLAGS) --change-addresses +0x100000 flash-serial.elf flash-serial.hex
	$(OD) $(ODFLAGS) flash-serial.elf > flash-serial.dmp

flash-serial.elf: flash-crt.o main.o libsam7.o
	$(LD) -v -Map flash-serial.map \
		-L/opt/gnu-arm/lib/gcc/arm-elf/4.4.0 \
		-L/opt/gnu-arm/arm-elf/lib \
		-T../common/flash-ln.cmd -o flash-serial.elf \
		flash-crt.o main.o libsam7.o -lgcc

ram-crt.o: ../common/ram-crt.s
	$(AS) $(AFLAGS) -o ram-crt.o ../common/ram-crt.s > ram-crt.lst
flash-crt.o: ../common/flash-crt.s
	$(AS) $(AFLAGS) -o flash-crt.o ../common/flash-crt.s > flash-crt.lst
libsam7.o: ../common/libsam7.c
	$(CC) $(CFLAGS) -c ../common/libsam7.c -o libsam7.o

main.s: main.c
	$(CC) $(CFLAGS) -S main.c
main.lst: main.s
	$(AS) $(AFLAGS) -o /dev/null main.s > main.lst

main.o: main.c
	$(CC) $(CFLAGS) -c main.c
