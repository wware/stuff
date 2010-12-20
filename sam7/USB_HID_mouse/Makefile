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
all: flash-mouse.hex

prog: flash-mouse.hex
	Sam_I_Am set ramwriteallow 0xffffff64 4 , \
		writew ffffff64 5a000004 , \
		writew ffffff64 5a004004 , \
		flash flash-mouse.hex , \
		writew ffffff64 5a00020b

clean:
	rm -f *.o *.hex *.elf *.dmp *.map *.lst core *~ main.s

flash-mouse.hex flash-mouse.dmp: flash-mouse.elf
	@ echo "...copying"
	$(CP) $(CPFLAGS) --change-addresses +0x100000 flash-mouse.elf flash-mouse.hex
	$(OD) $(ODFLAGS) flash-mouse.elf > flash-mouse.dmp

flash-mouse.elf: flash-crt.o main.o hid_enumerate.o usb_descriptors.o libsam7.o
	$(LD) -v -Map flash-mouse.map \
		-L/opt/gnu-arm/lib/gcc/arm-elf/4.4.0 \
		-L/opt/gnu-arm/arm-elf/lib \
		-T../common/flash-ln.cmd -o flash-mouse.elf \
		flash-crt.o main.o hid_enumerate.o usb_descriptors.o libsam7.o -lgcc

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
hid_enumerate.o: hid_enumerate.c
	$(CC) $(CFLAGS) -c hid_enumerate.c
usb_descriptors.o: usb_descriptors.c
	$(CC) $(CFLAGS) -c usb_descriptors.c
dbgu.o: ../common/dbgu.c
	$(CC) $(CFLAGS) -c ../common/dbgu.c -o dbgu.o
