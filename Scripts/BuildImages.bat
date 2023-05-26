@ECHO OFF

call SetEnvironment.bat
if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

instantfpc BuildImages.pas
if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

ECHO.
ECHO Build finished
ECHO.
