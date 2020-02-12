@echo off

set "RSVARS=D:\Embarcadero\Studio\20.0\bin\rsvars.bat"
set "MSBUILD=c:\Windows\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
set "MADEXCEPTPATCH=E:\Entwicklung\Lib\madCollection\madExcept\Tools\madExceptPatch.exe"
set "ZIP=D:\7-Zip\7z.exe"
set "INNO=D:\Inno Setup\ISCC.exe"
set "CURL=C:\Program Files\curl.exe"
set "CWD=%cd%"
set "APPNAME=streamwriter"
set "BUILDDIR=%CWD%\build"
set "APPDIR=%BUILDDIR%\app"
set "OUTDIR=%CWD%\bin"
set "ZIPFILES=%APPNAME%.exe"
set "UPLOADURL=https://streamwriter.org/de/downloads/svnbuild/?download=67&filename=%APPNAME%"

call %rsvars%

call :main
echo(
if %ERRORLEVEL% EQU 0 (
  echo Ok
) else (
  echo Fehler
)
echo(
pause
goto end

:refresh
  rmdir /s /q "%BUILDDIR%"

  mkdir "%BUILDDIR%"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  cd %BUILDDIR%

  SVN checkout svn://mistake.ws/common common
  if %ERRORLEVEL% GEQ 1 exit /B 1

  SVN checkout svn://mistake.ws/%APPNAME% app
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:getrevision
  cd "%APPDIR%"
  for /f "tokens=2" %%r in ('svn info -r HEAD ^| find "Revision: "') do set REVISION=%%r
  exit /b 0

:modifybuildnumber
  powershell -Command "(Get-Content "%APPDIR%\%APPNAME%\AppData.pas") -replace '"FBuildNumber :=.*;"', 'FBuildNumber := %REVISION%;' | Out-File "%APPDIR%\%APPNAME%\AppData.pas""

  exit /b %ERRORLEVEL%

:build
  cd "%BUILDDIR%"

  for /f "delims=" %%f in ('dir /s /b buildres.bat ^| find /i /v ".svn"') do (
    cd %%~df%%~pf
    call buildres.bat
    if %ERRORLEVEL% GEQ 1 exit /B 1
  )

  cd "%APPDIR%"

  "%MSBUILD%" /t:build /p:config=Release
  if %ERRORLEVEL% GEQ 1 exit /B 1

  "%MADEXCEPTPATCH%" "%APPDIR%\bin\%APPNAME%.exe" "%APPDIR%\%APPNAME%\%APPNAME%.mes"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:beforezip
  exit /b 0

:zip
  cd "%APPDIR%\bin"

  "%ZIP%" a -mx9 %APPNAME%.zip %ZIPFILES%
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:setup
  cd "%APPDIR%\setup"

  "%INNO%" /Q %APPNAME%.iss
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:copyfiles
  if not exist "%OUTDIR%" mkdir "%OUTDIR%"

  copy /Y "%APPDIR%\bin\%APPNAME%.zip" "%OUTDIR%\%APPNAME%.zip"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  copy /Y "%APPDIR%\setup\output\%APPNAME%_setup.exe" "%OUTDIR%\%APPNAME%_setup.exe"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:upload
  cd "%OUTDIR%"

  "%CURL%" -k -f -S -o nul -F "file=@%APPNAME%.zip" "%UPLOADURL%&revision=%REVISION%"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:main
  call :refresh
  if %ERRORLEVEL% GEQ 1 exit /B 1

  call :getrevision
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :modifybuildnumber
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :build
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :beforezip
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :zip
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :setup
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :copyfiles
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :upload
  if %ERRORLEVEL% GEQ 1 exit /b 1

  exit /b 0

:end
