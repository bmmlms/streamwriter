{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2020 Alexander Nottelmann

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
  BASS_INFO = record
    flags: DWORD;       // device capabilities (DSCAPS_xxx flags)
    hwsize: DWORD;      // size of total device hardware memory
    hwfree: DWORD;      // size of free device hardware memory
    freesam: DWORD;     // number of free sample slots in the hardware
    free3d: DWORD;      // number of free 3D sample slots in the hardware
    minrate: DWORD;     // min sample rate supported by the hardware
    maxrate: DWORD;     // max sample rate supported by the hardware
    eax: BOOL;          // device supports EAX? (always FALSE if BASS_DEVICE_3D was not used)
    minbuf: DWORD;      // recommended minimum buffer length in ms (requires BASS_DEVICE_LATENCY)
    dsver: DWORD;       // DirectSound version
    latency: DWORD;     // delay (in ms) before start of playback (requires BASS_DEVICE_LATENCY)
    initflags: DWORD;   // BASS_Init "flags" parameter
    speakers: DWORD;    // number of speakers available
    freq: DWORD;        // current output rate (OSX only)
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
  BASS_DX8_PARAMEQ = record
    fCenter: Single;
    fBandwidth: Single;
    fGain: Single;
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
    FBassDLLPath: string;
    FBassAACDLLPath: string;
    FDLLHandle: Cardinal;
    FAACDLLHandle: Cardinal;
    FEffectsAvailable: Boolean;

    FDevices: TList<TBassDevice>;

    procedure EnumDevices;
    procedure UninitializeBass;
  public
    BassLoaded: Boolean;
    DeviceAvailable: Boolean;

    constructor Create;
    destructor Destroy; override;
    function InitializeBass(Handle: THandle): Boolean;

    property EffectsAvailable: Boolean read FEffectsAvailable;
    property Devices: TList<TBassDevice> read FDevices;
  end;

var
  BASSInit: function(device: LongInt; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; stdcall;
  BASSFree: procedure; stdcall;
  BASSGetInfo: function(var info: BASS_INFO): BOOL; stdcall;
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
  BASSFXSetParameters: function(handle: DWORD; par: Pointer): BOOL; stdcall;
  BASSChannelSetFX: function(handle, type_: DWORD; priority: LongInt): DWORD; stdcall;
  BASSChannelRemoveFX: function(handle: DWORD; fx: DWORD): BOOL; stdcall;

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
  UninitializeBass;
  for i := 0 to FDevices.Count - 1 do
    FDevices[i].Free;
  FDevices.Free;

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
  BassInfo: BASS_INFO;
begin
  Result := False;

  FBassDLLPath := AppGlobals.TempDir + 'bass.dll';
  FBassAACDLLPath := AppGlobals.TempDir + 'bass_aac.dll';

  Res := TResourceStream.Create(0, 'BASS', MakeIntResource(RT_RCDATA));
  try
    try
      Res.SaveToFile(FBassDLLPath);
    except end;
  finally
    Res.Free;
  end;

  Res := TResourceStream.Create(0, 'BASS_AAC', MakeIntResource(RT_RCDATA));
  try
    try
      Res.SaveToFile(FBassAACDLLPath);
    except end;
  finally
    Res.Free;
  end;

  FDLLHandle := LoadLibrary(PChar(FBassDLLPath));

  if FDLLHandle <> 0 then
  begin
    BASSInit := GetProcAddress(FDLLHandle, 'BASS_Init');
    BASSFree := GetProcAddress(FDLLHandle, 'BASS_Free');
    BASSGetInfo := GetProcAddress(FDLLHandle, 'BASS_GetInfo');
    BASSGetDeviceInfo := GetProcAddress(FDLLHandle, 'BASS_GetDeviceInfo');
    BASSSetDevice := GetProcAddress(FDLLHandle, 'BASS_SetDevice');
    BASSStreamCreateFile := GetProcAddress(FDLLHandle, 'BASS_StreamCreateFile');
    BASSStreamCreateFileUser := GetProcAddress(FDLLHandle, 'BASS_StreamCreateFileUser');
    BASSChannelIsActive := GetProcAddress(FDLLHandle, 'BASS_ChannelIsActive');
    BASSStreamGetFilePosition := GetProcAddress(FDLLHandle, 'BASS_StreamGetFilePosition');
    BASSChannelGetPosition := GetProcAddress(FDLLHandle, 'BASS_ChannelGetPosition');
    BASSChannelGetLength := GetProcAddress(FDLLHandle, 'BASS_ChannelGetLength');
    BASSChannelBytes2Seconds := GetProcAddress(FDLLHandle, 'BASS_ChannelBytes2Seconds');
    BASSChannelSeconds2Bytes := GetProcAddress(FDLLHandle, 'BASS_ChannelSeconds2Bytes');
    BASSChannelGetLevel := GetProcAddress(FDLLHandle, 'BASS_ChannelGetLevel');
    BASSChannelPlay := GetProcAddress(FDLLHandle, 'BASS_ChannelPlay');
    BASSChannelPause := GetProcAddress(FDLLHandle, 'BASS_ChannelPause');
    BASSChannelStop := GetProcAddress(FDLLHandle, 'BASS_ChannelStop');
    BASSChannelSetPosition := GetProcAddress(FDLLHandle, 'BASS_ChannelSetPosition');
    BASSChannelSetSync := GetProcAddress(FDLLHandle, 'BASS_ChannelSetSync');
    BASSChannelRemoveSync := GetProcAddress(FDLLHandle, 'BASS_ChannelRemoveSync');
    BASSChannelSetAttribute := GetProcAddress(FDLLHandle, 'BASS_ChannelSetAttribute');
    BASSChannelSlideAttribute := GetProcAddress(FDLLHandle, 'BASS_ChannelSlideAttribute');
    BASSStreamFree := GetProcAddress(FDLLHandle, 'BASS_StreamFree');
    BASSStreamPutFileData := GetProcAddress(FDLLHandle, 'BASS_StreamPutFileData');
    BASSErrorGetCode := GetProcAddress(FDLLHandle, 'BASS_ErrorGetCode');
    BASSSetConfig := GetProcAddress(FDLLHandle, 'BASS_SetConfig');
    BASSPluginLoad := GetProcAddress(FDLLHandle, 'BASS_PluginLoad');
    BASSPluginFree := GetProcAddress(FDLLHandle, 'BASS_PluginFree');
    BASSChannelGetInfo := GetProcAddress(FDLLHandle, 'BASS_ChannelGetInfo');
    BASSChannelGetAttribute := GetProcAddress(FDLLHandle, 'BASS_ChannelGetAttribute');
    BASSChannelGetData := GetProcAddress(FDLLHandle, 'BASS_ChannelGetData');
    BASSFXSetParameters := GetProcAddress(FDLLHandle, 'BASS_FXSetParameters');
    BASSChannelSetFX := GetProcAddress(FDLLHandle, 'BASS_ChannelSetFX');
    BASSChannelRemoveFX := GetProcAddress(FDLLHandle, 'BASS_ChannelRemoveFX');

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
      Exit;

    FAACDLLHandle := BASSPluginLoad(PChar(FBassAACDLLPath), BASS_UNICODE);
    if FAACDLLHandle = 0 then
      BassLoaded := False;

    if BassLoaded then
    begin
       BASSGetInfo(BassInfo);
       FEffectsAvailable := BassInfo.dsver >= 8;
    end;

    Result := BassLoaded;
  end;
end;

procedure TBassLoader.UninitializeBass;
var
  i: Integer;
begin
  try
    if FAACDLLHandle <> 0 then
      BASSPluginFree(FAACDLLHandle);
    if FDLLHandle <> 0 then
    begin
      for i := 0 to FDevices.Count - 1 do
      begin
        BASSSetDevice(FDevices[i].FID);
        BASSFree;
      end;
      FreeLibrary(FDLLHandle);
    end;
    if FBassDLLPath <> '' then
      DeleteFile(FBassDLLPath);
    if FBassAACDLLPath <> '' then
      DeleteFile(FBassAACDLLPath);
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

