@ECHO OFF

REM Path to fpc binaries
SET "FPCBIN=D:\Lazarus\fpc\3.2.0\bin\x86_64-win64"

IF NOT EXIST %FPCBIN% GOTO FAILENVIRONMENT

REM Extend PATH
SET "PATH=%PATH%;%FPCBIN%;%FPCBIN%\..\..\..\.."

SET "INSTANTFPCOPTIONS=-Fu%FPCBIN%\..\..\..\..\lcl\units\x86_64-win64\win32 -Fu%FPCBIN%\..\..\..\..\lcl\units\x86_64-win64 -Fu%FPCBIN%\..\..\..\..\components\lazutils\lib\x86_64-win64"

instantfpc BuildImages.pas

IF ERRORLEVEL 1 GOTO FAIL

ECHO.
ECHO Build finished
ECHO.
GOTO END

:FAILENVIRONMENT
  ECHO.
  ECHO FPCBIN does not exist, please adjust variable
  ECHO.
  PAUSE
  GOTO END

:FAIL
  ECHO.
  ECHO Build failed
  ECHO.
  PAUSE

:END
