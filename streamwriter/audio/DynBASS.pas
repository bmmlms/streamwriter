unit DynBASS;

interface

uses
  Windows, SysUtils;

const
  STREAMFILE_BUFFER = 1;
  STREAMFILE_BUFFERPUSH = 2;
  BASS_STREAM_DECODE = $200000;
  BASS_UNICODE = $80000000;
  BASS_POS_BYTE = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_FILEPOS_CURRENT = 0;
  BASS_STREAM_PRESCAN = $20000;
  BASS_STREAM_BLOCK = $100000;
  BASS_FILEPOS_END = 2;
  BASS_FILEPOS_START = 3;
  BASS_SYNC_POS = 0;
  BASS_SYNC_END = 2;
  BASS_SYNC_MIXTIME = $40000000;
  BASS_SYNC_SLIDE = 5;

type
  QWORD = Int64;
  HSTREAM = DWORD;
  HSYNC = DWORD;
  FILECLOSEPROC = procedure(user: Pointer); stdcall;
  FILELENPROC = function(user: Pointer): QWORD; stdcall;
  FILEREADPROC = function(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;
  FILESEEKPROC = function(offset: QWORD; user: Pointer): BOOL; stdcall;
  BASS_FILEPROCS = record
    close: FILECLOSEPROC;
    length: FILELENPROC;
    read: FILEREADPROC;
    seek: FILESEEKPROC;
  end;
  SYNCPROC = procedure(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;

var
  BassDLLPath: string;
  DLLHandle: Cardinal;
  BassLoaded: Boolean;

  BASSInit: function(device: LongInt; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; stdcall;
  BASSStreamCreateFile: function(mem: BOOL; f: Pointer; offset, length: QWORD; flags: DWORD): HSTREAM; stdcall;
  BASSStreamCreateFileUser: function(system, flags: DWORD; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; stdcall;
  BASSChannelIsActive: function(handle: DWORD): DWORD; stdcall;
  BASSStreamGetFilePosition: function(handle: HSTREAM; mode: DWORD): QWORD; stdcall;
  BASSChannelGetPosition: function(handle, mode: DWORD): QWORD; stdcall;
  BASSChannelGetLength: function(handle, mode: DWORD): QWORD; stdcall;
  BASSChannelBytes2Seconds: function(handle: DWORD; pos: QWORD): Double; stdcall;
  BASSChannelSeconds2Bytes: function(handle: DWORD; pos: Double): QWORD; stdcall;
  BASSChannelGetLevel: function(handle: DWORD): DWORD; stdcall;
  BASSChannelPlay: function(handle: DWORD; restart: BOOL): BOOL; stdcall;
  BASSChannelStop: function(handle: DWORD): BOOL; stdcall;
  BASSChannelSetPosition: function(handle: DWORD; pos: QWORD; mode: DWORD): BOOL; stdcall;
  BASSChannelSetSync: function(handle: DWORD; type_: DWORD; param: QWORD; proc: SYNCPROC; user: Pointer): HSYNC; stdcall;
  BASSChannelRemoveSync: function(handle: DWORD; sync: HSYNC): BOOL; stdcall;
  BASSChannelSetAttribute: function(handle, attrib: DWORD; value: Single): BOOL; stdcall;
  BASSChannelSlideAttribute: function(handle, attrib: DWORD; value: Single; time: DWORD): BOOL; stdcall;
  BASSStreamFree: function(handle: HSTREAM): BOOL; stdcall;
  BASSStreamPutFileData: function(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; stdcall;
  BASSErrorGetCode: function: LongInt; stdcall;
  BASSSetConfig: function(option, value: DWORD): BOOL; stdcall;

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
    BASSStreamCreateFileUser := GetProcAddress(DLLHandle, 'BASS_StreamCreateFileUser');
    BASSChannelIsActive := GetProcAddress(DLLHandle, 'BASS_ChannelIsActive');
    BASSStreamGetFilePosition := GetProcAddress(DLLHandle, 'BASS_StreamGetFilePosition');
    BASSChannelGetPosition := GetProcAddress(DLLHandle, 'BASS_ChannelGetPosition');
    BASSChannelGetLength := GetProcAddress(DLLHandle, 'BASS_ChannelGetLength');
    BASSChannelBytes2Seconds := GetProcAddress(DLLHandle, 'BASS_ChannelBytes2Seconds');
    BASSChannelSeconds2Bytes := GetProcAddress(DLLHandle, 'BASS_ChannelSeconds2Bytes');
    BASSChannelGetLevel := GetProcAddress(DLLHandle, 'BASS_ChannelGetLevel');
    BASSChannelPlay := GetProcAddress(DLLHandle, 'BASS_ChannelPlay');
    BASSChannelStop := GetProcAddress(DLLHandle, 'BASS_ChannelStop');
    BASSChannelSetPosition := GetProcAddress(DLLHandle, 'BASS_ChannelSetPosition');
    BASSChannelSetSync := GetProcAddress(DLLHandle, 'BASS_ChannelSetSync');
    BASSChannelRemoveSync := GetProcAddress(DLLHandle, 'BASS_ChannelRemoveSync');
    BASSChannelSetAttribute := GetProcAddress(DLLHandle, 'BASS_ChannelSetAttribute');
    BASSChannelSlideAttribute := GetProcAddress(DLLHandle, 'BASS_ChannelSlideAttribute');
    BASSStreamFree := GetProcAddress(DLLHandle, 'BASS_StreamFree');
    BASSStreamPutFileData := GetProcAddress(DLLHandle, 'BASS_StreamPutFileData');
    BASSErrorGetCode := GetProcAddress(DLLHandle, 'BASS_ErrorGetCode');
    BASSSetConfig := GetProcAddress(DLLHandle, 'BASS_SetConfig');

    if BASSInit(-1, 44100, 0, 0, nil) then
    begin
      BassLoaded := True;
    end else
      FreeLibrary(DLLHandle);
  end;
end;

finalization
  if DLLHandle <> 0 then
    FreeLibrary(DLLHandle);

end.

