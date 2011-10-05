CC = arm-elf-gcc
LD = arm-elf-ld
AS = arm-elf-as
CP = arm-elf-objcopy
OD = arm-elf-objdump

#CFLAGS  = -I. -I../common -c -fno-common -O0 -g
CFLAGS  = -Wall -I. -I../common -c -fno-common -O2
AFLAGS  = -ahls -mapcs-32 -mfpu=softfpa
CPFLAGS = --output-target=ihex
ODFLAGS	= -x --syms
LDFLAGS = -v \
	-L/opt/gnu-arm/lib/gcc/arm-elf/4.4.0 \
	-L/opt/gnu-arm/arm-elf/lib \
	-T../common/flash-ln.cmd
LDFLAGS2 = -lgcc

.SUFFIXES: .c .h .o .dmp .hex .elf .rst .html .s .lst ;

.elf.hex:
	$(CP) $(CPFLAGS) --change-addresses +0x100000 $(@:.hex=.elf) $@

.elf.dmp:
	$(OD) $(ODFLAGS) $(@:.dmp=.elf) > $@

.c.o:
	$(CC) $(CFLAGS) -c $(@:.o=.c) -o $@

.c.s:
	$(CC) $(CFLAGS) -S $(@:.s=.c)

.s.o:
	$(AS) $(AFLAGS) -o $@ $(@:.o=.s) > $(@:.o=.lst)

.s.lst:
	$(AS) $(AFLAGS) -o $(@:.lst=.o) $(@:.lst=.s) > $@

.rst.html:
	rst2html $(@:.html=.rst) $@

##################################################

all: flash-cdc.hex flash-hid.hex flash-cdc.dmp flash-hid.dmp

clean:
	rm -f *.o *.hex *.elf *.dmp *.map *.lst core *~ main.s *.dmp *.hex \
		../common/*.o ../common/*.lst

doc: README.html
	x-www-browser README.html &

flash-hid.hex flash-hid.dmp: flash-hid.elf
	@ echo "...copying"
	$(CP) $(CPFLAGS) --change-addresses +0x100000 flash-hid.elf flash-hid.hex
	$(OD) $(ODFLAGS) flash-hid.elf > flash-hid.dmp

prog: flash-cdc.hex
	Sam_I_Am set ramwriteallow 0xffffff64 4 , shell sleep 1 , \
		writew ffffff64 5a000004 , shell sleep 1 , \
		writew ffffff64 5a002004 , shell sleep 1 , \
		flash flash-cdc.hex , shell sleep 1 , \
		writew ffffff64 5a00020b

progm: flash-hid.hex
	Sam_I_Am set ramwriteallow 0xffffff64 4 , shell sleep 1 , \
		writew ffffff64 5a000004 , shell sleep 1 , \
		writew ffffff64 5a002004 , shell sleep 1 , \
		flash flash-hid.hex , shell sleep 1 , \
		writew ffffff64 5a00020b

# The above is for the AT91SAM7S64. For the AT91SAM7S256, that 5a002004 becomes 5a004004
# as per http://claymore.engineer.gvsu.edu/~steriana/Software/Sam_I_Am/commands.html.

# Everything except main.o
# NOTE: assembly language objects MUST come first
OBJS = ../common/flash-crt.o \
	../common/usb.o \
	../common/libsam7.o

flash-cdc.elf: main.o cdc.o $(OBJS)
	$(LD) $(LDFLAGS) \
		-Map flash-cdc.map -o flash-cdc.elf $(OBJS) main.o cdc.o \
		$(LDFLAGS2)

flash-hid.elf: main.o hid.o $(OBJS)
	$(LD) $(LDFLAGS) \
		-Map flash-hid.map -o flash-hid.elf $(OBJS) main.o hid.o \
		$(LDFLAGS2)

main.lst: main.s
