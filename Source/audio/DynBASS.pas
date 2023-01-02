{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2023 Alexander Nottelmann

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
  AppData,
  Classes,
  Generics.Collections,
  SysUtils,
  Windows;

const
  // BASS
  DW_ERROR = LongWord(-1);
  QW_ERROR = Int64(-1);
  STREAMFILE_BUFFER = 1;
  STREAMFILE_BUFFERPUSH = 2;
  BASS_STREAM_DECODE = $200000;
  BASS_UNICODE = $80000000;
  BASS_POS_BYTE = 0;
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED = 3;
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
  BASS_SAMPLE_FLOAT = 256; // 32-bit floating-point
  STREAMPROC_PUSH = Pointer(-1); // push stream
  BASS_CONFIG_DEV_DEFAULT = 36;

  // BASS WASAPI
  BASS_DEVICE_LOOPBACK = 8;
  BASS_DEVICE_INPUT = 16;

  // BASS Mixer

  // BASS Enc
  BASS_ENCODE_PAUSE = 32;  // start encording paused
  BASS_ENCODE_PCM = 64; // write PCM sample data (no encoder)


type
  // BASS
  QWORD = Int64;
  HSTREAM = DWORD;
  HSYNC = DWORD;
  FILECLOSEPROC = procedure(user: Pointer); stdcall;
  FILELENPROC = function(user: Pointer): QWORD; stdcall;
  FILEREADPROC = function(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;
  FILESEEKPROC = function(offset: QWORD; user: Pointer): BOOL; stdcall;

  BASS_FILEPROCS = record
    Close: FILECLOSEPROC;
    length: FILELENPROC;
    Read: FILEREADPROC;
    seek: FILESEEKPROC;
  end;
  SYNCPROC = procedure(handle: HSYNC; channel, Data: DWORD; user: Pointer); stdcall;
  STREAMPROC = function(handle: HSTREAM; buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;

  BASS_DEVICEINFO = record
    Name: PAnsiChar;
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

  // BASS WASAPI
  // Device info structure
  BASS_WASAPI_INFO = record
    initflags: DWORD;
    freq: DWORD;
    chans: DWORD;
    format: DWORD;
    buflen: DWORD;
    volmax: Single;
    volmin: Single;
    volstep: Single;
  end;

  BASS_WASAPI_DEVICEINFO = record
    Name: PAnsiChar;
    id: PAnsiChar;
    &type: DWORD;
    flags: DWORD;
    minperiod: Single;
    defperiod: Single;
    mixfreq: DWORD;
    mixchans: DWORD;
  end;
  WASAPIPROC = function(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;

  // BASS Mixer

  // BASS Enc
  ENCODEPROC = procedure(handle: DWORD; channel: DWORD; buffer: Pointer; length: DWORD; user: Pointer); stdcall;

  TBassDevice = class
  private
    FIndex: Cardinal;
    FID: string;
    FName: string;
    FIsDefault: Boolean;
    FIsLoopback: Boolean;
  public
    constructor Create(const Index: Cardinal; const ID, Name: string; const IsDefault, IsLoopback: Boolean);

    property Index: Cardinal read FIndex;
    property ID: string read FID;
    property Name: string read FName;
    property IsDefault: Boolean read FIsDefault;
    property IsLoopback: Boolean read FIsLoopback;
  end;

  { TBassLoader }

  TBassLoader = class
  private
    FBassDLLPath: string;
    FBassAACDLLPath: string;
    FBassWASAPIDLLPath: string;
    FBassMixerDLLPath: string;
    FBassEncDLLPath: string;

    FDLLHandle: Cardinal;
    FAACDLLHandle: Cardinal;
    FWASAPIDLLHandle: Cardinal;
    FMixerDLLHandle: Cardinal;
    FEncDLLHandle: Cardinal;

    FEffectsAvailable: Boolean;

    FDevices: TList<TBassDevice>;
    FWASAPIDevices: TList<TBassDevice>;

    FWASAPIInfo: BASS_WASAPI_INFO;

    procedure EnumWASAPIDevices;
    procedure UninitializeBass;
  public
    BassLoaded: Boolean;
    DeviceAvailable: Boolean;

    constructor Create;
    destructor Destroy; override;
    function InitializeBass(Handle: THandle; LoadAAC, LoadMixer, LoadEnc, LoadWASAPI: Boolean): Boolean;
    function InitializeWASAPIDevice(Device: Integer; InputProc: WASAPIPROC; User: Pointer): Boolean;
    procedure EnumDevices;
    procedure SetDevice(ID: string);

    property EffectsAvailable: Boolean read FEffectsAvailable;
    property Devices: TList<TBassDevice> read FDevices;
    property WASAPIDevices: TList<TBassDevice> read FWASAPIDevices;
    property WASAPIInfo: BASS_WASAPI_INFO read FWASAPIInfo;
  end;

var
  // BASS
  BASSInit: function(device: LongInt; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; stdcall;
  BASSFree: procedure; stdcall;
  BASSGetInfo: function(var info: BASS_INFO): BOOL; stdcall;
  BASSGetDeviceInfo: function(device: DWORD; var info: BASS_DEVICEINFO): BOOL; stdcall;
  BASSSetDevice: function(device: DWORD): BOOL; stdcall;

  BASSStreamCreate: function(freq, chans, flags: DWORD; proc: STREAMPROC; user: Pointer): HSTREAM; stdcall;
  BASSStreamCreateFile: function(mem: BOOL; f: Pointer; offset, length: QWORD; flags: DWORD): HSTREAM; stdcall;
  BASSStreamCreateFileUser: function(system, flags: DWORD; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; stdcall;
  BASSStreamPutData: function(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; stdcall;
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
  BASSChannelSetAttribute: function(handle, attrib: DWORD; Value: Single): BOOL; stdcall;
  BASSChannelSlideAttribute: function(handle, attrib: DWORD; Value: Single; time: DWORD): BOOL; stdcall;
  BASSStreamFree: function(handle: HSTREAM): BOOL; stdcall;
  BASSStreamPutFileData: function(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; stdcall;
  BASSErrorGetCode: function: LongInt; stdcall;
  BASSSetConfig: function(option, Value: DWORD): BOOL; stdcall;
  BASSPluginLoad: function(filename: PChar; flags: DWORD): DWORD; stdcall;
  BASSPluginFree: function(handle: DWORD): BOOL; stdcall;
  BASSChannelGetInfo: function(handle: DWORD; var info: BASS_CHANNELINFO): BOOL; stdcall;
  BASSChannelGetAttribute: function(handle, attrib: DWORD; var Value: Single): BOOL; stdcall;
  BASSChannelGetData: function(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; stdcall;
  BASSFXSetParameters: function(handle: DWORD; par: Pointer): BOOL; stdcall;
  BASSChannelSetFX: function(handle, type_: DWORD; priority: LongInt): DWORD; stdcall;
  BASSChannelRemoveFX: function(handle: DWORD; fx: DWORD): BOOL; stdcall;

  // BASS WASAPI
  BASSWASAPIInit: function(device: Integer; freq, chans, flags: DWORD; buffer, period: Single; proc: WASAPIPROC; user: Pointer): BOOL; stdcall;
  BASSWASAPIGetDeviceInfo: function(device: DWORD; var info: BASS_WASAPI_DEVICEINFO): BOOL; stdcall;
  BASSWASAPIGetInfo: function(var info: BASS_WASAPI_INFO): BOOL; stdcall;
  BASSWASAPIStop: function(reset: BOOL): BOOL; stdcall;
  BASSWASAPIStart: function: BOOL; stdcall;
  BASSWASAPISetDevice: function(device: DWORD): BOOL; stdcall;
  BASSWASAPIFree: function: BOOL; stdcall;

  // BASS Mixer
  BASSMixerStreamCreate: function(freq, chans, flags: DWORD): HSTREAM; stdcall;
  BASSMixerStreamAddChannel: function(handle: HSTREAM; channel, flags: DWORD): BOOL; stdcall;

  // BASS Enc
  BASSEncodeStart: function(handle: DWORD; cmdline: PChar; flags: DWORD; proc: ENCODEPROC; user: Pointer): DWORD; stdcall;
  BASSEncodeSetPaused: function(handle: DWORD; paused: BOOL): BOOL; stdcall;
  BASSEncodeStop: function(handle: DWORD): BOOL; stdcall;

  Bass: TBassLoader;

implementation

{ TBassLoader }

constructor TBassLoader.Create;
begin
  inherited;

  FDevices := TList<TBassDevice>.Create;
  FWASAPIDevices := TList<TBassDevice>.Create;
end;

destructor TBassLoader.Destroy;
var
  i: Integer;
begin
  UninitializeBass;

  for i := 0 to FDevices.Count - 1 do
    FDevices[i].Free;
  FDevices.Free;

  for i := 0 to FWASAPIDevices.Count - 1 do
    FWASAPIDevices[i].Free;
  FWASAPIDevices.Free;

  inherited;
end;

procedure TBassLoader.EnumWASAPIDevices;
var
  i: Integer;
  Info: BASS_WASAPI_DEVICEINFO;
begin
  for i := 0 to FWASAPIDevices.Count - 1 do
    FWASAPIDevices[i].Free;
  FWASAPIDevices.Clear;

  i := 0;
  while BASSWASAPIGetDeviceInfo(i, Info) do
  begin
    if (Info.flags and BASS_DEVICE_INPUT > 0) and (Info.flags and BASS_DEVICE_ENABLED > 0) then
      FWASAPIDevices.Add(TBassDevice.Create(i, Info.id, Info.Name, (Info.flags and BASS_DEVICE_DEFAULT) = BASS_DEVICE_DEFAULT, (Info.flags and BASS_DEVICE_LOOPBACK) = BASS_DEVICE_LOOPBACK));
    Inc(i);
  end;
end;

function TBassLoader.InitializeBass(Handle: THandle; LoadAAC, LoadMixer, LoadEnc, LoadWASAPI: Boolean): Boolean;
var
  i: Integer;
  Res: TResourceStream;
  BassInfo: BASS_INFO;
begin
  Result := False;

  FBassDLLPath := ConcatPaths([AppGlobals.TempDir, 'bass.dll']);
  FBassAACDLLPath := ConcatPaths([AppGlobals.TempDir, 'bass_aac.dll']);
  FBassWASAPIDLLPath := ConcatPaths([AppGlobals.TempDir, 'basswasapi.dll']);
  FBassMixerDLLPath := ConcatPaths([AppGlobals.TempDir + 'bassmix.dll']);
  FBassEncDLLPath := ConcatPaths([AppGlobals.TempDir + 'bassenc.dll']);

  Res := TResourceStream.Create(0, 'BASS', Windows.RT_RCDATA);
  try
    try
      Res.SaveToFile(FBassDLLPath);
    except
    end;
  finally
    Res.Free;
  end;

  if LoadAAC then
  begin
    Res := TResourceStream.Create(0, 'BASS_AAC', Windows.RT_RCDATA);
    try
      try
        Res.SaveToFile(FBassAACDLLPath);
      except
      end;
    finally
      Res.Free;
    end;
  end;

  if LoadWASAPI then
  begin
    Res := TResourceStream.Create(0, 'BASS_WASAPI', Windows.RT_RCDATA);
    try
      try
        Res.SaveToFile(FBassWASAPIDLLPath);
      except
      end;
    finally
      Res.Free;
    end;
  end;

  if LoadMixer then
  begin
    Res := TResourceStream.Create(0, 'BASS_MIXER', Windows.RT_RCDATA);
    try
      try
        Res.SaveToFile(FBassMixerDLLPath);
      except
      end;
    finally
      Res.Free;
    end;
  end;

  if LoadEnc then
  begin
    Res := TResourceStream.Create(0, 'BASS_ENC', Windows.RT_RCDATA);
    try
      try
        Res.SaveToFile(FBassEncDLLPath);
      except
      end;
    finally
      Res.Free;
    end;
  end;

  FDLLHandle := LoadLibrary(PChar(FBassDLLPath));

  if FDLLHandle <> 0 then
  begin
    BASSInit := GetProcAddress(FDLLHandle, 'BASS_Init');
    BASSFree := GetProcAddress(FDLLHandle, 'BASS_Free');
    BASSGetInfo := GetProcAddress(FDLLHandle, 'BASS_GetInfo');
    BASSGetDeviceInfo := GetProcAddress(FDLLHandle, 'BASS_GetDeviceInfo');
    BASSSetDevice := GetProcAddress(FDLLHandle, 'BASS_SetDevice');
    BASSStreamCreate := GetProcAddress(FDLLHandle, 'BASS_StreamCreate');
    BASSStreamCreateFile := GetProcAddress(FDLLHandle, 'BASS_StreamCreateFile');
    BASSStreamCreateFileUser := GetProcAddress(FDLLHandle, 'BASS_StreamCreateFileUser');
    BASSStreamPutData := GetProcAddress(FDLLHandle, 'BASS_StreamPutData');
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

    BASSSetConfig(BASS_CONFIG_DEV_DEFAULT, 1);

    EnumDevices;

    for i := 0 to FDevices.Count - 1 do
      if BASSInit(FDevices[i].Index, 44100, 0, Handle, nil) then
      begin
        BassLoaded := True;
        DeviceAvailable := True;
      end;

    if not BassLoaded then
      if BassInit(0, 44100, 0, Handle, nil) then
        BassLoaded := True;

    if BassLoaded and LoadAAC then
    begin
      FAACDLLHandle := BASSPluginLoad(PChar(FBassAACDLLPath), 0);
      if FAACDLLHandle = 0 then
        BassLoaded := False;
    end;

    if BassLoaded and LoadWASAPI then
    begin
      FWASAPIDLLHandle := LoadLibrary(PChar(FBassWASAPIDLLPath));
      if FWASAPIDLLHandle = 0 then
        BassLoaded := False
      else
      begin
        BASSWASAPIInit := GetProcAddress(FWASAPIDLLHandle, 'BASS_WASAPI_Init');
        BASSWASAPIGetDeviceInfo := GetProcAddress(FWASAPIDLLHandle, 'BASS_WASAPI_GetDeviceInfo');
        BASSWASAPIGetInfo := GetProcAddress(FWASAPIDLLHandle, 'BASS_WASAPI_GetInfo');
        BASSWASAPIStop := GetProcAddress(FWASAPIDLLHandle, 'BASS_WASAPI_Stop');
        BASSWASAPIStart := GetProcAddress(FWASAPIDLLHandle, 'BASS_WASAPI_Start');
        BASSWASAPISetDevice := GetProcAddress(FWASAPIDLLHandle, 'BASS_WASAPI_SetDevice');
        BASSWASAPIFree := GetProcAddress(FWASAPIDLLHandle, 'BASS_WASAPI_Free');

        if not BASSWASAPIInit(-1, 0, 0, 0, 0.4, 0.05, nil, nil) then
          raise Exception.Create('BASSWASAPIInit() failed');

        if not BASSWASAPIGetInfo(FWASAPIInfo) then
          raise Exception.Create('BASSWASAPIGetInfo() failed');

        EnumWASAPIDevices;
      end;
    end;

    if BassLoaded and LoadMixer then
    begin
      FMixerDLLHandle := LoadLibrary(PChar(FBassMixerDLLPath));
      if FMixerDLLHandle = 0 then
        BassLoaded := False
      else
      begin
        BASSMixerStreamCreate := GetProcAddress(FMixerDLLHandle, 'BASS_Mixer_StreamCreate');
        BASSMixerStreamAddChannel := GetProcAddress(FMixerDLLHandle, 'BASS_Mixer_StreamAddChannel');
      end;
    end;

    if BassLoaded and LoadEnc then
    begin
      FEncDLLHandle := LoadLibrary(PChar(FBassEncDLLPath));
      if FEncDLLHandle = 0 then
        BassLoaded := False
      else
      begin
        BASSEncodeStart := GetProcAddress(FEncDLLHandle, 'BASS_Encode_Start');
        BASSEncodeSetPaused := GetProcAddress(FEncDLLHandle, 'BASS_Encode_SetPaused');
        BASSEncodeStop := GetProcAddress(FEncDLLHandle, 'BASS_Encode_Stop');
      end;
    end;

    if BassLoaded then
    begin
      BASSGetInfo(BassInfo);
      FEffectsAvailable := BassInfo.dsver >= 8;
    end;

    Result := BassLoaded;
  end;
end;

function TBassLoader.InitializeWASAPIDevice(Device: Integer; InputProc: WASAPIPROC; User: Pointer): Boolean;
begin
  if not BASSWASAPIInit(Device, 0, 0, 0, 1, 0.1, InputProc, User) then
    if BASSErrorGetCode <> 14 then
      raise Exception.Create('BASSErrorGetCode <> 14');

  Result := True;
end;

procedure TBassLoader.EnumDevices;
var
  i: Integer;
  FoundDefault: Boolean = False;
  Info: BASS_DEVICEINFO;
begin
  for i := 0 to FDevices.Count - 1 do
    FDevices[i].Free;
  FDevices.Clear;

  i := 1;
  while BASSGetDeviceInfo(i, Info) do
  begin
    if (Info.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED then
    begin
      FDevices.Add(TBassDevice.Create(i, IfThen<string>(Info.driver = '', 'Default', Info.driver) , Info.Name, (not FoundDefault) and ((Info.flags and BASS_DEVICE_DEFAULT) = BASS_DEVICE_DEFAULT), False));
      if not FoundDefault then
        FoundDefault := FDevices.Last.IsDefault;
    end;
    Inc(i);
  end;
end;

procedure TBassLoader.SetDevice(ID: string);
var
  Device: TBassDevice;
begin
  for Device in FDevices do
    if Device.ID = ID then
    begin
      BASSSetDevice(Device.Index);
      Exit;
    end;

  BASSSetDevice(0);
end;

procedure TBassLoader.UninitializeBass;
var
  i: Integer;
begin
  try
    if FAACDLLHandle <> 0 then
      BASSPluginFree(FAACDLLHandle);
    if FMixerDLLHandle <> 0 then
      FreeLibrary(FMixerDLLHandle);
    if FEncDLLHandle <> 0 then
      FreeLibrary(FEncDLLHandle);
    if FWASAPIDLLHandle <> 0 then
      FreeLibrary(FWASAPIDLLHandle);

    if FDLLHandle <> 0 then
    begin
      for i := 0 to FDevices.Count - 1 do
      begin
        BASSSetDevice(FDevices[i].Index);
        BASSFree;
      end;
      FreeLibrary(FDLLHandle);
    end;

    if FBassDLLPath <> '' then
      SysUtils.DeleteFile(FBassDLLPath);
    if FBassAACDLLPath <> '' then
      SysUtils.DeleteFile(FBassAACDLLPath);
    if FBassMixerDLLPath <> '' then
      SysUtils.DeleteFile(FBassMixerDLLPath);
    if FBassEncDLLPath <> '' then
      SysUtils.DeleteFile(FBassEncDLLPath);
    if FBassWASAPIDLLPath <> '' then
      SysUtils.DeleteFile(FBassWASAPIDLLPath);
  except
  end;
end;

{ TBassDevice }

constructor TBassDevice.Create(const Index: Cardinal; const ID, Name: string; const IsDefault, IsLoopback: Boolean);
begin
  inherited Create;

  FIndex := Index;
  FID := ID;
  FName := Name;
  FIsDefault := IsDefault;
  FIsLoopback := IsLoopback;
end;

end.
