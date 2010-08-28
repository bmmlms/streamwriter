unit DynBASS;

interface

uses
  Windows, SysUtils;

const
  BASS_STREAM_DECODE = $200000;
  BASS_FILEPOS_CURRENT    = 0;

type
  QWORD = Int64;
  HSTREAM = DWORD;

var
  BassDLLPath: string;
  DLLHandle: Cardinal;
  BassLoaded: Boolean;

  BASSInit: function(device: LongInt; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; stdcall;
  BASSStreamCreateFile: function(mem: BOOL; f: Pointer; offset, length: QWORD; flags: DWORD): HSTREAM; stdcall;
  BASSChannelIsActive: function(handle: DWORD): DWORD; stdcall;
  BASSStreamGetFilePosition: function(handle: HSTREAM; mode: DWORD): QWORD; stdcall;
  BASSChannelBytes2Seconds: function(handle: DWORD; pos: QWORD): Double; stdcall;
  BASSChannelGetLevel: function(handle: DWORD): DWORD; stdcall;
  BASSStreamFree: function(handle: HSTREAM): BOOL; stdcall;

implementation

initialization
begin
  BassLoaded := False;
  BassDLLPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'bass.dll';

  DLLHandle := LoadLibrary(PChar(BassDLLPath));
  if DLLHandle <> 0 then
  begin
    BASSInit := GetProcAddress(DLLHandle, 'BASS_Init');
    BASSStreamCreateFile := GetProcAddress(DLLHandle, 'BASS_StreamCreateFile');
    BASSChannelIsActive := GetProcAddress(DLLHandle, 'BASS_ChannelIsActive');
    BASSStreamGetFilePosition := GetProcAddress(DLLHandle, 'BASS_StreamGetFilePosition');
    BASSChannelBytes2Seconds := GetProcAddress(DLLHandle, 'BASS_ChannelBytes2Seconds');
    BASSChannelGetLevel := GetProcAddress(DLLHandle, 'BASS_ChannelGetLevel');
    BASSStreamFree := GetProcAddress(DLLHandle, 'BASS_StreamFree');

    if BASSInit(-1, 44100, 0, 0, nil) then
      BassLoaded := True
    else
      FreeLibrary(DLLHandle);
  end;
end;

finalization
  if DLLHandle <> 0 then
    FreeLibrary(DLLHandle);

end.
