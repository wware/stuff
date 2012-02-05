This is a GNU/gcc-Port of the Atmel AppNote example source:
AT91 USB Framework - Core 1.01 + HID 1.0.zip 
and       -"-                  + CDC 1.0.zip

gcc-Port by Martin Thomas, Kaiserslautern, Germany
http://www.siwawi.arubi.uni-kl.de/avr_projects

############## THE GNU-PORT IS IN AN EARLY STATE 
############## work in progress

Tested o.k.
HID/Keyboard - non-debug (flash), trace on/off, SAM7S64, SAM7EK
HID/Mouse    - non-debug (flash), trace on/off, SAM7S64, SAM7EK

Tested with problems:
CDC - SAM7S64, SAM7EK - detection o.k., installation of usbser o.k.
      plug-in dectection and init seems o.k. (LEDs, tracelog)
      but Terminal-Software locks up when connecting to the virt. port
      --> TODO

changelog mthomas
 20070310: 
 * To workaround the "sh.exe" problem which is basicly a problem of an old make version
 - included a new version of make.exe. The included version is 3.81. The version
   in WinARM 20060606 is 3.80 which does not handle the shell and the command
   line to recursive calls as expected. There is a batch-file in the top level
   which calls the 3.81 version so this should rather be easy to handle. 
   Just type mtmake all or mtmake clean etc.
 *  from http://en.mikrocontroller.net/topic/98204:
 - root Makefile: ifneq ($POWER,BUS)->ifneq ($(POWER),BUS) thanks to ECC Exelty
 - device.c function DEV_Resume: added line AT91C_BASE_MC->MC_FMR = AT91C_MC_FWS_1FWS;
   thanks to Jonathan Dumarjo


 ----------------------------------------------------------------------------
        ATMEL Microcontroller Software Support  -  ROUSSET  -
 ----------------------------------------------------------------------------
 Copyright (c) 2006, Atmel Corporation
 All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 
 - Redistributions of source code must retain the above copyright notice,
 this list of conditions and the disclaiimer below.
 
 - Redistributions in binary form must reproduce the above copyright notice,
 this list of conditions and the disclaimer below in the documentation and/or
 other materials provided with the distribution. 
 
 Atmel's name may not be used to endorse or promote products derived from
 this software without specific prior written permission. 
 
 DISCLAIMER: THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE
 DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ----------------------------------------------------------------------------

 ----------------------------------------------------------------------------
	AT91 USB HID Drivers
 ----------------------------------------------------------------------------

 This software package contains the USB framework core developped by ATMEL,
 as well as two HID drivers for a mouse and a keyboard.
 The following files are included :
   - core/
      	-> Source code for the framework core
      	-> Makefile for the core
   - hid/
	-> Source code for the HID drivers
	-> Makefile for the HID drivers
   - lib/
	-> Lib v3 files for every supported chips
   - bin/
      	-> Compiled binaries for every supported chips
   - ./
	-> Makefile for the framework
	-> Startup file

 Compilation is done by using the root makefile (not the one in /core or /hid).
 Please refer to the corresponding application note for more information on
 this step.
