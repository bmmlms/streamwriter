@ECHO OFF

call SetEnvironment.bat
if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

SET "SCRIPTSDIR2=%~dp0"
SET "TEMPDIR=%TEMP%\sw_build"
SET "APPNAME=streamwriter"

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

:main
  IF EXIST "%SCRIPTSDIR2%\..\Build" rmdir /s /q "%SCRIPTSDIR2%\..\Build"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  
  IF EXIST "%TEMPDIR%" rmdir /s /q "%TEMPDIR%"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  mkdir "%TEMPDIR%"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

  cd /D "%TEMPDIR%"

  git clone --recurse-submodules https://github.com/bmmlms/streamwriter.git
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  
  cd "streamwriter\Scripts"
  
  call build.bat
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%
  
  xcopy /s "..\Build" "%SCRIPTSDIR2%\..\Build\"
  if %ERRORLEVEL% GEQ 1 exit /B %ERRORLEVEL%

:end
  cd /D "%SCRIPTSDIR2%"
