rem -
rem OpenOCD environment-information
rem by Martin Thomas, www.siwawi.arubi.uni-kl.de/avr_projects
rem -
rem The following has been tested with the precompiled binaries 
rem from Michael Fischer (www.yagarto.de) based on the OpenOCD
rem sources SVN-version 100. Plattform: PC running Windows-2000 
rem (german), JTAG-interfaces tested: WinARM-JTAG, JTAGKey, 
rem Wiggler-clone
rem -

rem - 
rem Installation directory (where the .exe(s) is/are)
rem -
SET OOCD_INSTALLDIR=C:\Programme\openocd-2006re100\bin

rem - 
rem The used interface either FTDI(=WinARM-JTAG, JTAGKEY etc.) or PP(="Wiggler")
rem -
SET OOCD_INTERFACE=FTDI
rem set OOCD_INTERFACE=PP

rem - 
rem Set if OpenOCD verbose logging is needed
rem -
rem SET OOCD_DBG=-d 3

if %OOCD_INTERFACE% == PP goto LAB_PP
if %OOCD_INTERFACE% == FTDI goto LAB_FTDI
echo ERROR - Unknown interface
goto LAB_END


rem -
rem Executables and configuration files
rem -
:LAB_PP
rem For the "PP"-Interface
SET OOCD_EXE=%OOCD_INSTALLDIR%\openocd-pp.exe
SET OOCD_CFG_DBG=openocd_at91sam7s_dbg_wiggler.cfg
SET OOCD_CFG_FLASH=openocd_at91sam7s_flash_wiggler.cfg
goto LAB_END

:LAB_FTDI
rem For the "FTDI"-Interface
SET OOCD_EXE=%OOCD_INSTALLDIR%\openocd-ftd2xx.exe
SET OOCD_CFG_DBG=openocd_at91sam7s_dbg_ftdi.cfg
SET OOCD_CFG_FLASH=openocd_at91sam7s_flash_ftdi.cfg

:LAB_END


