# Microsoft Developer Studio Generated NMAKE File, Format Version 40001
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=libsiod - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to libsiod - Win32 Debug.
!ENDIF

!IF "$(CFG)" != "libsiod - Win32 Release" && "$(CFG)" !=\
 "libsiod - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE
!MESSAGE NMAKE /f "libsiod.mak" CFG="libsiod - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "libsiod - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libsiod - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE
!ERROR An invalid configuration is specified.
!ENDIF

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF
################################################################################
# Begin Project
# PROP Target_Last_Scanned "libsiod - Win32 Debug"
MTL=mktyplib.exe
RSC=rc.exe
CPP=cl.exe

!IF  "$(CFG)" == "libsiod - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\libsiod.dll"

CLEAN :
	-@erase ".\Release\libsiod.dll"
	-@erase ".\Release\md5.obj"
	-@erase ".\Release\slibu.obj"
	-@erase ".\Release\slib.obj"
	-@erase ".\Release\trace.obj"
	-@erase ".\Release\sliba.obj"
	-@erase ".\Release\libsiod.lib"
	-@erase ".\Release\libsiod.exp"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/libsiod.pch" /YX /Fo"$(INTDIR)/" /c
CPP_OBJS=.\Release/
CPP_SBRS=
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/libsiod.bsc"
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/libsiod.pdb" /machine:I386 /def:".\libsiod.def"\
 /out:"$(OUTDIR)/libsiod.dll" /implib:"$(OUTDIR)/libsiod.lib"
DEF_FILE= \
	".\libsiod.def"
LINK32_OBJS= \
	"$(INTDIR)/md5.obj" \
	"$(INTDIR)/slibu.obj" \
	"$(INTDIR)/slib.obj" \
	"$(INTDIR)/trace.obj" \
	"$(INTDIR)/sliba.obj"

"$(OUTDIR)\libsiod.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libsiod - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\libsiod.dll"

CLEAN :
	-@erase ".\Debug\vc40.pdb"
	-@erase ".\Debug\vc40.idb"
	-@erase ".\Debug\libsiod.dll"
	-@erase ".\Debug\slibu.obj"
	-@erase ".\Debug\slib.obj"
	-@erase ".\Debug\trace.obj"
	-@erase ".\Debug\md5.obj"
	-@erase ".\Debug\sliba.obj"
	-@erase ".\Debug\libsiod.ilk"
	-@erase ".\Debug\libsiod.lib"
	-@erase ".\Debug\libsiod.exp"
	-@erase ".\Debug\libsiod.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/libsiod.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c
CPP_OBJS=.\Debug/
CPP_SBRS=
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/libsiod.bsc"
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)/libsiod.pdb" /debug /machine:I386 /def:".\libsiod.def"\
 /out:"$(OUTDIR)/libsiod.dll" /implib:"$(OUTDIR)/libsiod.lib"
DEF_FILE= \
	".\libsiod.def"
LINK32_OBJS= \
	"$(INTDIR)/slibu.obj" \
	"$(INTDIR)/slib.obj" \
	"$(INTDIR)/trace.obj" \
	"$(INTDIR)/md5.obj" \
	"$(INTDIR)/sliba.obj"

"$(OUTDIR)\libsiod.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<

################################################################################
# Begin Target

# Name "libsiod - Win32 Release"
# Name "libsiod - Win32 Debug"

!IF  "$(CFG)" == "libsiod - Win32 Release"

!ELSEIF  "$(CFG)" == "libsiod - Win32 Debug"

!ENDIF

################################################################################
# Begin Source File

SOURCE=trace.c
DEP_CPP_TRACE=\
	"siod.h"\
	"siodp.h"\


"$(INTDIR)\trace.obj" : $(SOURCE) $(DEP_CPP_TRACE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=slib.c
DEP_CPP_SLIB_=\
	"siod.h"\
	"siodp.h"\
	{$(INCLUDE)}"\sys\Types.h"\


"$(INTDIR)\slib.obj" : $(SOURCE) $(DEP_CPP_SLIB_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=sliba.c
DEP_CPP_SLIBA=\
	"siod.h"\
	"siodp.h"\


"$(INTDIR)\sliba.obj" : $(SOURCE) $(DEP_CPP_SLIBA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=slibu.c
DEP_CPP_SLIBU=\
	{$(INCLUDE)}"\sys\Types.h"\
	{$(INCLUDE)}"\sys\Stat.h"\
	"siod.h"\
	"siodp.h"\
	"md5.h"\


"$(INTDIR)\slibu.obj" : $(SOURCE) $(DEP_CPP_SLIBU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=md5.c
DEP_CPP_MD5_C=\
	"md5.h"\


"$(INTDIR)\md5.obj" : $(SOURCE) $(DEP_CPP_MD5_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\libsiod.def

!IF  "$(CFG)" == "libsiod - Win32 Release"

!ELSEIF  "$(CFG)" == "libsiod - Win32 Debug"

!ENDIF

# End Source File
# End Target
# End Project
################################################################################
