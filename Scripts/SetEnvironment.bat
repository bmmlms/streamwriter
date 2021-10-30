REM Path to fpc binaries
SET "FPCBIN=D:\Lazarus\fpc\3.2.2\bin\x86_64-win64"

IF NOT EXIST "%FPCBIN%" (
  ECHO.
  ECHO FPCBIN does not exist, please adjust variable
  ECHO.
  PAUSE
  EXIT /B 1
)

REM Extend PATH
SET "PATH=%PATH%;%FPCBIN%;%FPCBIN%\..\..\..\.."

SET "INSTANTFPCOPTIONS=-Fu%FPCBIN%\..\..\..\..\lcl\units\x86_64-win64\win32 -Fu%FPCBIN%\..\..\..\..\lcl\units\x86_64-win64 -Fu%FPCBIN%\..\..\..\..\components\lazutils\lib\x86_64-win64"

SET "ZIP=D:\7-Zip\7z.exe"
SET "INNO=D:\Inno Setup\ISCC.exe"
SET "CURL=C:\Program Files\curl.exe"
