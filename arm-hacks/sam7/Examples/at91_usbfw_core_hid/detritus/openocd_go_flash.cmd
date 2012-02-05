@echo off

call openocd_env_info.cmd

%OOCD_EXE% %OOCD_DBG% -f %OOCD_CFG_FLASH%

rem pause