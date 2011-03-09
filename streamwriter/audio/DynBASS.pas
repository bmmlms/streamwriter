unit DynBASS;

interface

uses
  Windows, SysUtils, Classes, Functions, GUIFunctions, AppData;

const
  STREAMFILE_BUFFER = 1;
  STREAMFILE_BUFFERPUSH = 2;
  BASS_STREAM_DECODE = $200000;
  BASS_UNICODE = $80000000;
  BASS_POS_BYTE = 0;
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED  = 3;
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

  TBassLoader = class
  private
    BassDLLPath: string;
    BassAACDLLPath: string;
    DLLHandle: Cardinal;
    AACDLLHandle: Cardinal;

    procedure UninitializeBass;
  public
    BassLoaded: Boolean;

    destructor Destroy; override;
    function InitializeBass: Boolean;
  end;

var
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
  BASSChannelPause: function(handle: DWORD): BOOL; stdcall;
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
  BASSPluginLoad: function(filename: PChar; flags: DWORD): DWORD; stdcall;
  BASSPluginFree: function(handle: DWORD): BOOL; stdcall;

var
  Bass: TBassLoader;

implementation

{
function GetTempFile(SubDir, Name: string): string;
var
  Dir, TmpDir, DataDir, AppDir: string;
  Test: TMemoryStream;
begin
  Result := '';
  Dir := '';

  Test := TMemoryStream.Create;
  try
    try
      TmpDir := GetTempDir;
      if not DirectoryExists(TmpDir) then
        ForceDirectories(TmpDir);
      Test.SaveToFile(TmpDir + 'justatestfile');
      DeleteFile(TmpDir + IntToStr(Rnd));
      Dir := TmpDir;
    except
      Exit;
    end;
  finally
    Test.Free;
  end;

  if Dir = '' then
    Exit;

  Result := Dir + Name;
end;
}

{ TBassLoader }

destructor TBassLoader.Destroy;
begin
  UninitializeBass;
  inherited;
end;

function TBassLoader.InitializeBass: Boolean;
var
  Res: TResourceStream;
  T: Cardinal;
begin
  Result := False;

  BassDLLPath := AppGlobals.TempDir + 'bass.dll';
  BassAACDLLPath := AppGlobals.TempDir + 'bass_aac.dll';

  Res := TResourceStream.Create(0, 'BASS', MakeIntResource(RT_RCDATA));
  try
    try
      Res.SaveToFile(BassDLLPath);
    except end;
  finally
    Res.Free;
  end;

  Res := TResourceStream.Create(0, 'BASS_AAC', MakeIntResource(RT_RCDATA));
  try
    try
      Res.SaveToFile(BassAACDLLPath);
    except end;
  finally
    Res.Free;
  end;

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
    BASSChannelPause := GetProcAddress(DLLHandle, 'BASS_ChannelPause');
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
    BASSPluginLoad := GetProcAddress(DLLHandle, 'BASS_PluginLoad');
    BASSPluginFree := GetProcAddress(DLLHandle, 'BASS_PluginFree');

    T := GetTickCount;
    while True do
    begin
      // Manchmal klappt BASSInit nicht, mit nem ErrorCode von -1 ...
      // in der Doku steht "some other mysterious error occured"...
      // Deshalb diese komische Schleife hier.
      if BASSInit(-1, 44100, 0, 0, nil) then
      begin
        BassLoaded := True;
        Break;
      end else
      begin
        if (T + 5000 < GetTickCount) then
          Break;
        Sleep(200);
      end;
    end;

    if not BassLoaded then    
    begin
      FreeLibrary(DLLHandle);
      Exit;
    end;

    AACDLLHandle := BASSPluginLoad(PChar(BassAACDLLPath), BASS_UNICODE);
    if AACDLLHandle = 0 then
    begin
      BassLoaded := False;
      FreeLibrary(DLLHandle);
      DLLHandle := 0;
    end;

    Result := BassLoaded;
  end;
end;

procedure TBassLoader.UninitializeBass;
begin
  try
    if AACDLLHandle <> 0 then
      BASSPluginFree(AACDLLHandle);
    if DLLHandle <> 0 then
    begin
      FreeLibrary(DLLHandle);
    end;
    if BassDLLPath <> '' then
      DeleteFile(BassDLLPath);
    if BassAACDLLPath <> '' then
      DeleteFile(BassAACDLLPath);
  except
  end;
end;

end.

