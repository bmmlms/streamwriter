@ECHO OFF

REM required to get ERRORLEVEL in for loop
setlocal enabledelayedexpansion

if "%FPCBIN%" == "" (
  call SetEnvironment.bat
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
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
if !ERRORLEVEL! EQU 0 (
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

  instantfpc "%SCRIPTSDIR%\SetGitVersion.pas" "streamwriter.lpi" "streamwriter-git.lpi" "AddBuildNr"
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  REM Build executables
  lazbuild --build-all --cpu=%~1 --build-mode=Release --quiet --quiet "streamwriter-git.lpi"
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  del "streamwriter-git.lpi"
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  REM Build addons
  for /R "..\Addons" %%f in (*.lpi) do (
    cd "%%~dpf"

    instantfpc "%SCRIPTSDIR%\SetGitVersion.pas" "%%~nxf" "%%~nf-git.lpi"
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

    lazbuild --build-all --cpu=%~1 --build-mode=Release --quiet --quiet "%%~nf-git.lpi"
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

    del "%%~nf-git.lpi"
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
  )

  cd "%OUTDIR%\%~1"

  if exist "%APPNAME%.dbg" (
    move "%APPNAME%.dbg" "%APPNAME%-%GITSHA%-%~1.dbg"
  )

  for %%f in (*.exe *.dll) do (
    ssh gaia osslsigncode-sign.sh < "%%f" > "%%f-signed"
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
    move /y "%%f-signed" "%%f"
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
  )

  exit /b 0

:zip
  cd "%OUTDIR%\%~1"
  "%ZIP%" a -mx9 %APPNAME%.zip %APPNAME%.exe
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  exit /b 0

:setup
  cd "%PROJECTDIR%\Setup"

  powershell -Command "(gc -encoding UTF8 '%APPNAME%.iss') -replace '^(#define AppCpu\s+)\"x86_64\"$', '$1\"%~1\"' | Out-File -encoding UTF8 '%APPNAME%-%~1.iss'"

  if "%~1"=="i386" (
    powershell -Command "(gc -encoding UTF8 '%APPNAME%-%~1.iss') -replace '^ArchitecturesAllowed=x64$', '' | Out-File -encoding UTF8 '%APPNAME%-%~1.iss'"
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

    powershell -Command "(gc -encoding UTF8 '%APPNAME%-%~1.iss') -replace '^ArchitecturesInstallIn64BitMode=x64$', '' | Out-File -encoding UTF8 '%APPNAME%-%~1.iss'"
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
  )

  "%INNO%" /O"%OUTDIR%\%~1" "%APPNAME%-%~1.iss"
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  del "%APPNAME%-%~1.iss"
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  ssh gaia osslsigncode-sign.sh < "%OUTDIR%\%~1\%APPNAME%_setup.exe" > "%OUTDIR%\%~1\%APPNAME%_setup-signed.exe"
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
  move /y "%OUTDIR%\%~1\%APPNAME%_setup-signed.exe" "%OUTDIR%\%~1\%APPNAME%_setup.exe"
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  exit /b 0

:upload
  cd "%OUTDIR%\%~1"

  ssh ares streamwriter-update-build "%GITSHA%" "%~1" < "%APPNAME%.zip"
  if !ERRORLEVEL! GEQ 1 exit /b 1

  exit /b 0

:main
  if exist "%OUTDIR%\" (
    rmdir /s /q "%OUTDIR%"
  )

  call :getgitsha
  if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

  (for %%C in (%CPUS%) do (
    call :build %%C
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

    call :zip %%C
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!

    call :setup %%C
    if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
  ))

  if "%1"=="upload" (
    (for %%C in (%CPUS%) do (
      call :upload %%C
      if !ERRORLEVEL! GEQ 1 exit /b !ERRORLEVEL!
    ))
  )

  exit /b 0

:end
  cd "%SCRIPTSDIR%"
