{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    ------------------------------------------------------------------------
}

{ This unit contains functionality to handle the BASS library }
unit DynBASS;

interface

uses
  Windows, SysUtils, Classes, AppData, Generics.Collections;

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
  BASS_DEVICE_ENABLED = 1;
  BASS_DEVICE_DEFAULT = 2;

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
  BASS_DEVICEINFO = record
    name: PAnsiChar;
    driver: PAnsiChar;
    flags: DWORD;
  end;
  BASS_CHANNELINFO = record
    freq: DWORD;        // default playback rate
    chans: DWORD;       // channels
    flags: DWORD;       // BASS_SAMPLE/STREAM/MUSIC/SPEAKER flags
    ctype: DWORD;       // type of channel
    origres: DWORD;     // original resolution
    plugin: DWORD;    // plugin
    sample: DWORD;    // sample
    filename: PChar;    // filename
  end;

  TBassDevice = class
  private
    FID: Cardinal;
    FName: string;
    FIsDefault: Boolean;
  public
    constructor Create(ID: Cardinal; Name: string; IsDefault: Boolean);

    property ID: Cardinal read FID;
    property Name: string read FName;
    property IsDefault: Boolean read FIsDefault;
  end;

  TBassLoader = class
  private
    BassDLLPath: string;
    BassAACDLLPath: string;
    DLLHandle: Cardinal;
    AACDLLHandle: Cardinal;

    FDevices: TList<TBassDevice>;

    procedure EnumDevices;
    procedure UninitializeBass;
  public
    BassLoaded: Boolean;
    DeviceAvailable: Boolean;

    constructor Create;
    destructor Destroy; override;
    function InitializeBass(Handle: THandle): Boolean;

    property Devices: TList<TBassDevice> read FDevices;
  end;

var
  BASSInit: function(device: LongInt; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; stdcall;
  BASSGetDeviceInfo: function(device: DWORD; var info: BASS_DEVICEINFO): BOOL; stdcall;
  BASSSetDevice: function(device: DWORD): BOOL; stdcall;
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
  BASSChannelGetInfo: function(handle: DWORD; var info: BASS_CHANNELINFO):BOOL; stdcall;
  BASSChannelGetAttribute: function(handle, attrib: DWORD; var value: Single): BOOL; stdcall;
  BASSChannelGetData: function(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; stdcall;

var
  Bass: TBassLoader;

implementation

{ TBassLoader }

constructor TBassLoader.Create;
begin
  inherited;

  FDevices := TList<TBassDevice>.Create();
end;

destructor TBassLoader.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDevices.Count - 1 do
    FDevices[i].Free;
  FDevices.Free;
  UninitializeBass;
  inherited;
end;

procedure TBassLoader.EnumDevices;
var
  i: Integer;
  Info: BASS_DEVICEINFO;
begin
  i := 1;
  while BASSGetDeviceInfo(i, Info) do
  begin
    if (Info.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED then
    begin
      FDevices.Add(TBassDevice.Create(i, Info.name,
        (Info.flags and BASS_DEVICE_DEFAULT) = BASS_DEVICE_DEFAULT));
    end;
    Inc(i);
  end;
end;

function TBassLoader.InitializeBass(Handle: THandle): Boolean;
var
  i: Integer;
  Found: Boolean;
  Res: TResourceStream;
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
    BASSGetDeviceInfo := GetProcAddress(DLLHandle, 'BASS_GetDeviceInfo');
    BASSSetDevice := GetProcAddress(DLLHandle, 'BASS_SetDevice');
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
    BASSChannelGetInfo := GetProcAddress(DLLHandle, 'BASS_ChannelGetInfo');
    BASSChannelGetAttribute := GetProcAddress(DLLHandle, 'BASS_ChannelGetAttribute');
    BASSChannelGetData := GetProcAddress(DLLHandle, 'BASS_ChannelGetData');

    BassLoaded := False;
    DeviceAvailable := False;

    EnumDevices;

    Found := False;
    for i := 0 to FDevices.Count - 1 do
      if FDevices[i].ID = AppGlobals.SoundDevice then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      for i := 0 to FDevices.Count - 1 do
        if FDevices[i].IsDefault then
        begin
          AppGlobals.SoundDevice := FDevices[i].ID;
          Break;
        end;
    end;

    for i := 0 to FDevices.Count - 1 do
      if BASSInit(FDevices[i].ID, 44100, 0, Handle, nil) then
      begin
        BassLoaded := True;
        DeviceAvailable := True;
      end;
    if not BassLoaded then
      if BassInit(0, 44100, 0, Handle, nil) then
        BassLoaded := True;

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

{ TBassDevice }

constructor TBassDevice.Create(ID: Cardinal; Name: string; IsDefault: Boolean);
begin
  FID := ID;
  FName := Name;
  FIsDefault := IsDefault;
end;

end.

