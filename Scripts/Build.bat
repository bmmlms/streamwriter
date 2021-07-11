@ECHO OFF

REM Path to fpc binaries
SET "FPCBIN=D:\Lazarus\fpc\3.2.0\bin\x86_64-win64"

set "ZIP=D:\7-Zip\7z.exe"
set "INNO=D:\Inno Setup\ISCC.exe"
set "CURL=C:\Program Files\curl.exe"
set "CWD=%cd%"
set "APPNAME=streamwriter"
set "PROJECTDIR=%CWD%\.."
set "SOURCEDIR=%PROJECTDIR%\streamwriter"
set "OUTDIR=%PROJECTDIR%\Build"
set "PUBLISHDIR=%PROJECTDIR%\Build\Publish"
set "ZIPFILES=%APPNAME%.exe"
set "UPLOADURL=https://streamwriter.org/de/downloads/svnbuild/?download=67&filename=%APPNAME%"

IF NOT EXIST %FPCBIN% (
  ECHO.
  ECHO FPCBIN does not exist, please adjust variable
  ECHO.
  PAUSE
  exit /b 1
)

REM Extend PATH
SET "PATH=%PATH%;%FPCBIN%;%FPCBIN%\..\..\..\.."

call :main
echo(
if %ERRORLEVEL% EQU 0 (
  echo Ok
) else (
  echo Error
  pause
)
echo(
goto end

:getgitsha
  cd "%PROJECTDIR%"
  for /f "tokens=1" %%r in ('git rev-parse --short HEAD') do set GITSHA=%%r
  exit /b 0

:modifybuildnumber
  powershell -Command "(Get-Content "%SOURCEDIR%\AppData.pas") -replace '"FGitSHA :=.*;"', 'FGitSHA := ''%GITSHA%'';' | Out-File -Encoding UTF8 "%SOURCEDIR%\AppData.pas""

  exit /b %ERRORLEVEL%

:build
  cd "%SOURCEDIR%"

  REM Build executables
  lazbuild --build-all --cpu=i386 --os=Win32 --build-mode=Release streamWriter.lpi
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  REM Build addons
  for /R "..\Addons" %%f in (*.lpi) do (
    cd "%%~dpf"

    lazbuild --build-all --cpu=i386 --os=Win32 --build-mode=Release "%%~nxf"
    if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  )
  
  exit /b 0

:zip
  cd "%OUTDIR%"

  "%ZIP%" a -mx9 %APPNAME%.zip %ZIPFILES%
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  exit /b 0

:setup
  cd "%PROJECTDIR%\Setup"

  "%INNO%" /O"%OUTDIR%" %APPNAME%.iss
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  exit /b 0

:copyfiles
  if not exist "%PUBLISHDIR%" mkdir "%PUBLISHDIR%"

  copy /Y "%OUTDIR%\%APPNAME%.zip" "%PUBLISHDIR%\%APPNAME%.zip"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  copy /Y "%OUTDIR%\%APPNAME%_setup.exe" "%PUBLISHDIR%\%APPNAME%_setup.exe"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  exit /b 0

:upload
  cd "%PUBLISHDIR%"

  "%CURL%" -k -f -S -o nul -F "file=@%APPNAME%.zip" "%UPLOADURL%&gitsha=%GITSHA%"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:main
  call :getgitsha
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :modifybuildnumber
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :build
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :zip
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :setup
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :copyfiles
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :upload
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  exit /b 0

:end
  cd "%CWD%"
