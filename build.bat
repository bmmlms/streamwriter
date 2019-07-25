@echo off

set "RSVARS=D:\Embarcadero\Studio\19.0\bin\rsvars.bat"
set "MSBUILD=c:\Windows\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
set "MADEXCEPTPATCH=E:\Entwicklung\Lib\madCollection\madExcept\Tools\madExceptPatch.exe"
set "ZIP=D:\7-Zip\7z.exe"
set "INNO=D:\Inno Setup\ISCC.exe"
set "CURL=C:\Program Files\curl.exe"
set "CWD=%cd%"
set "BUILDDIR=%CWD%\build"
set "SWDIR=%BUILDDIR%\streamwriter"

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

  SVN checkout svn://mistake.ws/streamwriter streamwriter
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:getrevision
  cd "%SWDIR%"
  for /f "tokens=2" %%r in ('svn info -r HEAD ^| find "Revision: "') do set REVISION=%%r
  exit /b 0

:modifybuildnumber
  powershell -Command "(Get-Content "%SWDIR%\streamwriter\AppData.pas") -replace '"FBuildNumber :=.*;"', 'FBuildNumber := %REVISION%;' | Out-File "%SWDIR%\streamwriter\AppData.pas""

  exit /b %ERRORLEVEL%

:build
  cd "%BUILDDIR%"

  for /f "delims=" %%f in ('dir /s /b buildres.bat ^| find /i /v ".svn"') do (
    cd %%~df%%~pf
    call buildres.bat
    if %ERRORLEVEL% GEQ 1 exit /B 1
  )

  cd "%SWDIR%"

  "%MSBUILD%" /t:build /p:config=Release
  if %ERRORLEVEL% GEQ 1 exit /B 1

  "%MADEXCEPTPATCH%" "%SWDIR%\bin\streamwriter.exe" "%SWDIR%\streamwriter\streamwriter.mes"
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:zip
  cd "%SWDIR%\bin"

  "%ZIP%" a -mx9 streamwriter.zip streamwriter.exe 
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:setup
  cd "%SWDIR%\setup"

  "%INNO%" /Q streamwriter.iss
  if %ERRORLEVEL% GEQ 1 exit /B 1

  exit /b 0

:upload
  cd "%SWDIR%\bin"

  "%CURL%" -k -f -S -o nul -F "file=@streamwriter.zip" "https://streamwriter.org/de/downloads/svnbuild/?download=67&revision=%REVISION%"
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

  call :zip
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :setup
  if %ERRORLEVEL% GEQ 1 exit /b 1

  call :upload
  if %ERRORLEVEL% GEQ 1 exit /b 1

  exit /b 0

:end
