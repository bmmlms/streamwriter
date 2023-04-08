@ECHO OFF

if "%FPCBIN%" == "" (
  call SetEnvironment.bat
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
)

SET "SCRIPTSDIR=%~dp0"
SET "APPNAME=streamwriter"
SET "PROJECTDIR=%SCRIPTSDIR%\.."
SET "SOURCEDIR=%PROJECTDIR%\Source"
SET "OUTDIR=%PROJECTDIR%\Build"
SET "PUBLISHDIR=%PROJECTDIR%\Build\Publish"
SET "ZIPFILES=%APPNAME%.exe"
SET "UPLOADURL=https://streamwriter.org/downloads/svnbuild?download=67&filename=%APPNAME%"

call :main %1
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

:build
  if exist "%OUTDIR%\" (
    rmdir /s /q "%OUTDIR%"
  )

  REM Build libraries
  copy /y "%PROJECTDIR%\SubModules\mbedtls_config.h" "%PROJECTDIR%\SubModules\mbedtls\include\mbedtls\mbedtls_config.h"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  call "%MSYS2%" -defterm -no-start -where "%PROJECTDIR%\SubModules\mbedtls" -mingw32 -c "make clean && make -j lib && exit"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  cd "%SOURCEDIR%"

  instantfpc "%SCRIPTSDIR%\SetGitVersion.pas" streamwriter.lpi streamwriter_gitsha.lpi
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  REM Build executables
  lazbuild --build-all --cpu=i386 --os=Win32 --build-mode=Release --quiet --quiet streamwriter_gitsha.lpi
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  
  del streamwriter_gitsha.lpi
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  instantfpc "%SCRIPTSDIR%\RemoveResources.pas" "%OUTDIR%\streamwriter.exe" "%SCRIPTSDIR%\RemoveResources.txt"

  REM Build addons
  for /R "..\Addons" %%f in (*.lpi) do (
    cd "%%~dpf"

    lazbuild --build-all --cpu=i386 --os=Win32 --build-mode=Release --quiet --quiet "%%~nxf"
    if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  )

  cd "%OUTDIR%"

  for /f %%i in ('git rev-parse --short HEAD') do set GITSHA=%%i
  move "%APPNAME%.dbg" "%APPNAME%_%GITSHA%.dbg"

  for %%f in (*.*) do (
    type "%%f" | "%PLINK%" -batch gaia osslsigncode-sign.sh > "%%f-signed"
    if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
    move /y "%%f-signed" "%%f"
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

  type "%OUTDIR%\%APPNAME%_setup.exe" | "%PLINK%" -batch gaia osslsigncode-sign.sh > "%OUTDIR%\%APPNAME%_setup-signed.exe"
  IF ERRORLEVEL 1 GOTO FAIL
  move /y "%OUTDIR%\%APPNAME%_setup-signed.exe" "%OUTDIR%\%APPNAME%_setup.exe"
  IF ERRORLEVEL 1 GOTO FAIL

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

  call :build
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :zip
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :setup
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  call :copyfiles
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  if "%1"=="upload" (
    call :upload
    if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%
  )

  exit /b 0

:end
  cd "%SCRIPTSDIR%"
