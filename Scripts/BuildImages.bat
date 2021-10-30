@ECHO OFF

call SetEnvironment.bat
if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

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
