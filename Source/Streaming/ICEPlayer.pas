{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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

{ This unit handles playback of ICEStreams }
unit ICEPlayer;

interface

uses
  AppData,
  AudioFunctions,
  AudioStream,
  Classes,
  DynBASS,
  Logging,
  PlayerManager,
  StreamHelper,
  SyncObjs,
  SysUtils;

type
  TICEPlayer = class
  private
    FMem: TMemoryStream;
    FLock: TCriticalSection;
    FPlayer: DWORD;
    FPausing, FStopping: Boolean;
    FPlayStartBuffer: Cardinal;
    FDataRead: Boolean;

    FEQEnabled: Boolean;
    FBandData: array[0..9] of TBandData;

    procedure FreeStream(Player: Cardinal);

    function FGetPlaying: Boolean;
    function FGetPaused: Boolean;
    procedure FSetEQEnabled(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play;
    procedure Pause(NoFadeOut: Boolean = False);
    procedure Stop;
    procedure SetVolume(Vol: Integer);
    procedure SetEQ(Value, Freq: Integer);

    procedure PushData(Buf: Pointer; Len: Integer);

    property EQEnabled: Boolean read FEQEnabled write FSetEQEnabled;
    property Playing: Boolean read FGetPlaying;
    property Paused: Boolean read FGetPaused;
    property Pausing: Boolean read FPausing;
    property Stopping: Boolean read FStopping;
    property Mem: TMemoryStream read FMem;
    property DataRead: Boolean read FDataRead;
  end;

implementation

procedure EndSyncProc(handle: HSYNC; channel, Data: DWORD; user: Pointer); stdcall;
var
  P: TICEPlayer;
begin
  P := TICEPlayer(user);
  if P.FPausing then
    BASSChannelPause(channel)
  else if P.Stopping then
    P.FreeStream(channel);
  TICEPlayer(user).FPausing := False;
  TICEPlayer(user).FStopping := False;
end;

procedure DevFailSyncProc(handle: HSYNC; channel, Data: DWORD; user: Pointer); stdcall;
var
  P: TICEPlayer;
begin
  P := TICEPlayer(user);
  P.FreeStream(channel);
  TICEPlayer(user).FPausing := False;
  TICEPlayer(user).FStopping := False;
end;

procedure BASSClose(user: Pointer); stdcall;
begin

end;

function BASSLen(user: Pointer): QWORD; stdcall;
begin
  Result := 0;
end;

function BASSRead(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;
var
  Mem: TMemoryStream;
  CopyLen: Cardinal;
  Tries: Integer;
begin
  Tries := 0;
  Result := 0;

  while not TICEPlayer(user).FLock.TryEnter do
  begin
    Inc(Tries);
    Sleep(10);

    if Tries > 20 then
      Exit;
  end;

  try
    Mem := TICEPlayer(user).FMem;

    Tries := 0;
    while Mem.Size = 0 do
    begin
      Inc(Tries);
      Sleep(10);

      if Tries > 100 then
        Exit;
    end;

    CopyLen := Mem.Size;
    if Length < CopyLen then
      CopyLen := Length;

    Move(Mem.Memory^, Buffer^, CopyLen);
    Result := CopyLen;

    Mem.RemoveRange(0, CopyLen);
  finally
    TICEPlayer(user).FLock.Leave;
  end;
end;

function BASSSeek(offset: QWORD; user: Pointer): longbool; stdcall;
begin
  Result := longbool(0);
end;

{ TPlayThread }

constructor TICEPlayer.Create;
begin
  inherited Create;

  FMem := TAudioStreamMemory.Create;
  FLock := TCriticalSection.Create;
  FPlayer := 0;
  FPlayStartBuffer := 0;
end;

destructor TICEPlayer.Destroy;
begin
  if FPlayer > 0 then
  begin
    FreeStream(FPlayer);
    FPlayer := 0;
  end;
  FMem.Free;
  FLock.Free;

  inherited;
end;

function TICEPlayer.FGetPlaying: Boolean;
begin
  Result := (FPlayer > 0) and (FStopping or (BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING) or (BASSChannelIsActive(FPlayer) = BASS_ACTIVE_STALLED));
end;

function TICEPlayer.FGetPaused: Boolean;
begin
  Result := (FPlayer > 0) and ((BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PAUSED) or FPausing);
end;

procedure TICEPlayer.Play;
var
  Funcs: BASS_FILEPROCS;
  R: Integer;
begin
  BASSStart;

  if (FPlayStartBuffer = 0) or (not Paused and (FMem.Size < FPlayStartBuffer)) then
    Exit;

  if not Playing then
  begin
    AppGlobals.Lock;
    try
      Bass.SetDevice(AppGlobals.SoundDevice);
    finally
      AppGlobals.Unlock;
    end;

    Funcs.Close := BASSClose;
    Funcs.length := BASSLen;
    Funcs.seek := BASSSeek;
    Funcs.Read := BASSRead;

    if FPlayer > 0 then
      FreeStream(FPlayer);
    FPlayer := BASSStreamCreateFileUser(STREAMFILE_BUFFER, 0, Funcs, Self);
    EQEnabled := Players.EQEnabled;

    SetVolume(Players.Volume);

    BASSChannelPlay(FPlayer, False);

    R := BASSErrorGetCode;

    if R <> 0 then
    begin
      FreeStream(FPlayer);
      FPlayer := 0;
    end;
  end;
end;

procedure TICEPlayer.Pause(NoFadeOut: Boolean = False);
begin
  if FPlayer = 0 then
    Exit;

  if NoFadeOut then
    BASSChannelPause(FPlayer)
  else if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
  begin
    FPausing := True;
    BASSChannelSlideAttribute(FPlayer, 2, 0, 300);
    BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, EndSyncProc, Self);
  end;
end;

procedure TICEPlayer.Stop;
var
  State: Cardinal;
begin
  if FPlayer = 0 then
    Exit;

  State := BASSChannelIsActive(FPlayer);
  if State = BASS_ACTIVE_PLAYING then
  begin
    FStopping := True;
    BASSChannelSlideAttribute(FPlayer, 2, 0, 300);
    BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, EndSyncProc, Self);
  end else
    FreeStream(FPlayer);

  FPlayer := 0;
end;

procedure TICEPlayer.SetVolume(Vol: Integer);
begin
  if FPlayer > 0 then
    BASSChannelSetAttribute(FPlayer, 2, Vol / 100);
end;

procedure TICEPlayer.PushData(Buf: Pointer; Len: Integer);
var
  AudioInfo: TAudioInfo;
const
  MAX_BUFFER_SIZE = 1048576;
begin
  FLock.Enter;
  try
    FMem.Seek(0, soFromEnd);
    FMem.Write(Buf^, Len);

    while FMem.Size > MAX_BUFFER_SIZE do
      FMem.RemoveRange(0, 65536); // Puffer "rotieren"

    if FMem.Size < 8192 then
      Exit;

    if (not Playing) and (not Paused) then
    begin
      AudioInfo.GetAudioInfo(FMem);
      if AudioInfo.Success then
        FPlayStartBuffer := AudioInfo.Bitrate * 1100
      else
        raise Exception.Create('');
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TICEPlayer.FreeStream(Player: Cardinal);
var
  i: Integer;
begin
  for i := 0 to High(FBandData) do
  begin
    BASSChannelRemoveFX(Player, FBandData[i].Handle);
    FBandData[i].Handle := 0;
  end;
  BASSStreamFree(Player);
end;

procedure TICEPlayer.FSetEQEnabled(Value: Boolean);
var
  i: Integer;
begin
  FEQEnabled := Value;

  if Value then
  begin
    for i := 0 to High(FBandData) do
    begin
      if FPlayer > 0 then
        FBandData[i].Handle := BASSChannelSetFX(FPlayer, 7, 0);
      AppGlobals.Lock;
      try
        SetEQ(AppGlobals.EQGain[i], i);
      finally
        AppGlobals.Unlock;
      end;
    end;
  end else
    for i := 0 to High(FBandData) do
    begin
      if FBandData[i].Handle > 0 then
        BASSChannelRemoveFX(FPlayer, FBandData[i].Handle);
      FBandData[i].Handle := 0;
    end;
end;

procedure TICEPlayer.SetEQ(Value, Freq: Integer);
var
  S: BASS_DX8_PARAMEQ;
begin
  FBandData[Freq].Gain := Value;

  if FEQEnabled and (FPlayer > 0) and (FBandData[Freq].Handle > 0) then
  begin
    S.fCenter := BandToFreq(Freq);
    S.fBandwidth := 12;
    S.fGain := Value;

    BASSFXSetParameters(FBandData[Freq].Handle, @S);
  end;
end;

end.
