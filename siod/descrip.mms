! VMS MAKEFILE (using MMS) for SIOD.
! Author: George Carrette, gjc@world.std.com
!
! [Notes] Alpha OpenVMS: you need to edit the linker options files.
!         Footprint: The $CODE segment of the shared library 70kbytes (VAX)
!         Databases: Sybase/Oracle/Rdb compile/link instructions not included
!                    Refer to your database support vendor for help.
!
!
! use MMS/MACRO=("RELEASE=1") for no debugging.
! use MMS/MACRO=("STRIP=1") for no debugging, no traceback symbols.
! use MMS/MACRO=("PCA=1") for PCA
!
! Default is no optimization and full debug info.
! STRIP provides the smallest binary sizes.

LINKMAP=/MAP=$(mms$target_name).MAP/FULL

.ifdef STRIP
CFLAGS = /NODEBUG/OPTIMIZE/LIST/SHOW=(NOSOURCE)
LINK_EXE = /notraceback/exe=$(mms$target_name).exe$(LINKMAP)
LINK_LIB = /notraceback/share=$(mms$target_name).exe$(LINKMAP)
.else
.ifdef RELEASE
CFLAGS = /DEBUG=TRACEBACK/OPTIMIZE/LIST/SHOW=(NOSOURCE)
LINK_EXE = /traceback/exe=$(mms$target_name).exe$(LINKMAP)
LINK_LIB = /traceback/share=$(mms$target_name).exe$(LINKMAP)
.else
.ifdef PCA
CFLAGS = /DEBUG/OPTIMIZE=NOINLINE/LIST/SHOW=(NOSOURCE)
LINK_EXE = /debug=SYS$LIBRARY:PCA$OBJ.OBJ/exe=$(mms$target_name).exe$(LINKMAP)
LINK_LIB = /debug/share=$(mms$target_name).exe$(LINKMAP)
.else
CFLAGS = /DEBUG/NOOPTIMIZE/LIST/SHOW=(NOSOURCE)
LINK_EXE = /debug/exe=$(mms$target_name).exe$(LINKMAP)
LINK_LIB = /debug/share=$(mms$target_name).exe$(LINKMAP)
.endif
.endif
.endif

all depends_on siod.exe,tar.exe,ss.exe,parser_pratt.exe
 !(ALL DONE)

help depends_on descrip.mms
 ! MMS/MACRO=X=1, where X = STRIP, RELEASE, or LINK_PCA

clean depends_on descrip.mms
 delete *.obj.*,*.exe.*,*.map.*,*.lis.*

SHR_OBJS = slib.obj,sliba.obj,trace.obj,slibu.obj,md5.obj

siod.exe depends_on siod.obj,siodshr.exe,siod.opt
 link$(LINK_EXE) siod.obj,siod.opt/opt
 ! re-execute the next line in your superior process:
 siod == "$" + f$env("DEFAULT") + "SIOD"

siodshr.exe depends_on $(SHR_OBJS),siodshr.opt
 define/job SIODSHR "''F$ENV("DEFAULT")'SIODSHR.EXE"
 define/job SIOD_LIB "''F$ENV("DEFAULT")'"
 link$(LINK_LIB) $(SHR_OBJS),siodshr.opt/opt

tar.exe depends_on tar.obj,tar.opt,siodshr.exe
  link$(LINK_LIB) tar.obj,tar.opt/opt

ss.exe depends_on ss.obj,ss.opt,siodshr.exe
  link$(LINK_LIB) ss.obj,ss.opt/opt

parser_pratt.exe depends_on parser_pratt.obj,parser_pratt.opt,siodshr.exe
  link$(LINK_LIB) parser_pratt.obj,parser_pratt.opt/opt

siod.obj  depends_on siod.c,siod.h
slib.obj  depends_on slib.c,siod.h,siodp.h
sliba.obj depends_on sliba.c,siod.h,siodp.h
trace.obj depends_on trace.c,siod.h,siodp.h
slibu.obj depends_on slibu.c,siod.h,siodp.h,md5.h
ss.obj    depends_on ss.c,siod.h,siodp.h,ss.h
tar.obj   depends_on tar.c,siod.h

