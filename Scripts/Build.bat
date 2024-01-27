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
SET "CPUS=i386 x86_64"

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
  cd "%SOURCEDIR%"

  instantfpc "%SCRIPTSDIR%\SetGitVersion.pas" streamwriter.lpi streamwriter-%~1.lpi
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  powershell -Command "(gc streamwriter-%~1.lpi) -replace 'x86_64', '%~1' | Out-File streamwriter-%~1.lpi"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  REM Build executables
  lazbuild --build-all --cpu=%~1 --build-mode=Release --quiet --quiet streamwriter-%~1.lpi
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  del streamwriter-%~1.lpi
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  REM Build addons
  for /R "..\Addons" %%f in (*.lpi) do (
    cd "%%~dpf"

    lazbuild --build-all --cpu=%~1 --build-mode=Release --quiet --quiet "%%~nxf"
    if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  )

  cd "%OUTDIR%\%~1"

  if exist "%APPNAME%.dbg" (
    move "%APPNAME%.dbg" "%APPNAME%-%GITSHA%.dbg"
  )

  REM for %%f in (*.exe *.dll) do (
  REM   type "%%f" | "%PLINK%" -batch gaia osslsigncode-sign.sh > "%%f-signed"
  REM   if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  REM   move /y "%%f-signed" "%%f"
  REM   if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  REM )

:zip
  cd "%OUTDIR%\%~1"
  "%ZIP%" a -mx9 %APPNAME%.zip %APPNAME%.exe
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  exit /b 0

:setup
  cd "%PROJECTDIR%\Setup"

  powershell -Command "(gc %APPNAME%.iss) -replace 'x86_64', '%~1' | Out-File -encoding ASCII %APPNAME%-%~1.iss"

  if "%~1"=="i386" (
    powershell -Command "(gc %APPNAME%-%~1.iss) -replace 'ArchitecturesAllowed=x64', '' | Out-File -encoding ASCII %APPNAME%-%~1.iss"
    if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

    powershell -Command "(gc %APPNAME%-%~1.iss) -replace 'ArchitecturesInstallIn64BitMode=x64', '' | Out-File -encoding ASCII %APPNAME%-%~1.iss"
    if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%
  )

  "%INNO%" /O"%OUTDIR%\%~1" %APPNAME%-%~1.iss
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  del %APPNAME%-%~1.iss
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  REM type "%OUTDIR%\%~1\%APPNAME%_setup.exe" | "%PLINK%" -batch gaia osslsigncode-sign.sh > "%OUTDIR%\%~1\%APPNAME%_setup-signed.exe"
  REM if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  REM move /y "%OUTDIR%\%~1\%APPNAME%_setup-signed.exe" "%OUTDIR%\%~1\%APPNAME%_setup.exe"
  REM if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  exit /b 0

:upload
  cd "%OUTDIR%\%~1"

  type "%APPNAME%.zip" | "%PLINK%" -batch ares streamwriter-update-build "%GITSHA%"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:main
  if exist "%OUTDIR%\" (
    rmdir /s /q "%OUTDIR%"
  )

  call :getgitsha
  if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

  (for %%C in (%CPUS%) do (
    call :build %%C
    if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

    call :zip %%C
    if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%

    call :setup %%C
    if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%
  ))

  if "%1"=="upload" (
    call :upload i386
    if %ERRORLEVEL% GEQ 1 exit /b %ERRORLEVEL%
  )

  exit /b 0

:end
  cd "%SCRIPTSDIR%"
