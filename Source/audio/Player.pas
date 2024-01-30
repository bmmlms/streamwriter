{
    ------------------------------------------------------------------------
    streamWriter
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

{ This unit contains everything to playback saved files from harddisk }
unit Player;

interface

uses
  AppData,
  AppMessages,
  AudioFunctions,
  Classes,
  DynBass,
  FileTagger,
  InterfaceBase,
  Logging,
  MessageBus,
  SysUtils,
  Windows;

type
  TPlayer = class
  private
    FPlayer: Cardinal;
    FSyncSlide, FSyncPos, FSyncEnd: Cardinal;
    FPaused: Boolean;
    FStopped: Boolean;
    FFree: Boolean;
    FFilename: string;
    FVolume: Integer;
    FPosToReach: Cardinal;
    FEndPos: Cardinal;
    FShowTitle: Boolean;
    FMessageHWnd: HWND;

    FEQEnabled: Boolean;
    FBandData: array[0..9] of TBandData;
    FTag: TTagData;

    FOnPosReached: TNotifyEvent;
    FOnEndReached: TNotifyEvent;
    FOnPlay: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnStateChange: TNotifyEvent;

    procedure CreatePlayer;
    procedure FreeStream(Player: Cardinal);

    procedure WndMethod(var Msg: TMessage);

    procedure FSetEQEnabled(Value: Boolean);
    function FGetPaused: Boolean;
    function FGetStopped: Boolean;
    function FGetPlaying: Boolean;
    procedure FSetVolume(Value: Integer);
    function FGetMaxByte: Int64;
    function FGetMaxTime: Double;
    function FGetPositionByte: Int64;
    function FGetPositionTime: Double;
    procedure FSetFilename(Value: string);
    procedure FSetPosToReach(Value: Cardinal);
    procedure FSetPositionByte(Value: Int64);
    procedure FSetPositionTime(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play;
    procedure Pause(NoFadeOut: Boolean = False);
    procedure Stop(Free: Boolean; NoFadeOut: Boolean = False);
    procedure SetEQ(Value, Freq: Integer);

    property EQEnabled: Boolean read FEQEnabled write FSetEQEnabled;
    property Paused: Boolean read FGetPaused;
    property Stopped: Boolean read FGetStopped;
    property Playing: Boolean read FGetPlaying;
    property Filename: string read FFilename write FSetFilename;
    property Volume: Integer read FVolume write FSetVolume;
    property MaxByte: Int64 read FGetMaxByte;
    property MaxTime: Double read FGetMaxTime;
    property PositionByte: Int64 read FGetPositionByte write FSetPositionByte;
    property PositionTime: Double read FGetPositionTime write FSetPositionTime;
    property PosToReach: Cardinal read FPosToReach write FSetPosToReach;
    property Tag: TTagData read FTag;
    property ShowTitle: Boolean read FShowTitle write FShowTitle;

    property OnPosReached: TNotifyEvent read FOnPosReached write FOnPosReached;
    property OnEndReached: TNotifyEvent read FOnEndReached write FOnEndReached;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

implementation

uses
  PlayerManager;

procedure SlideEndSyncProc(handle: HSYNC; channel, Data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  BASSChannelRemoveSync(channel, handle);

  P := TPlayer(user);
  if P.FPaused then
    BASSChannelPause(channel)
  else if P.FStopped then
  begin
    BASSChannelStop(channel);
    if P.FFree then
      P.FreeStream(channel);
  end;

  P.FPaused := False;
  P.FStopped := False;
end;

procedure PosSyncProc(handle: HSYNC; channel, Data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  P := TPlayer(user);

  PostMessage(P.FMessageHWnd, WM_USER + BASS_SYNC_POS, 0, 0);
end;

procedure EndSyncProc(handle: HSYNC; channel, Data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  P := TPlayer(user);

  P.FPaused := False;
  P.FStopped := False;

  PostMessage(P.FMessageHWnd, WM_USER + BASS_SYNC_END, 0, 0);
end;

procedure DevFailSyncProc(handle: HSYNC; channel, Data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  P := TPlayer(user);

  P.FPaused := False;
  P.FStopped := False;

  PostMessage(P.FMessageHWnd, WM_USER + BASS_SYNC_DEV_FAIL, 0, 0);
end;

{ TPlayer }

constructor TPlayer.Create;
var
  i: Integer;
begin
  inherited;

  FShowTitle := True;
  FMessageHWnd := WidgetSet.AllocateHWnd(WndMethod);

  for i := 0 to High(FBandData) do
    FBandData[i].Handle := 0;
end;

procedure TPlayer.CreatePlayer;
var
  FT: TFileTagger;
begin
  AppGlobals.Lock;
  try
    Bass.SetDevice(AppGlobals.SoundDevice);
  finally
    AppGlobals.Unlock;
  end;

  FPlayer := BASSStreamCreateFile(False, PChar(FFilename), 0, 0, 0);
  if FPlayer = 0 then
    raise Exception.Create('');

  FSyncEnd := BASSChannelSetSync(FPlayer, BASS_SYNC_END, 0, EndSyncProc, Self);
  BASSChannelSetSync(FPlayer, BASS_SYNC_DEV_FAIL, 0, DevFailSyncProc, Self);

  EQEnabled := Players.EQEnabled;

  if FTag <> nil then
    FreeAndNil(FTag);
  FT := TFileTagger.Create;
  try
    if FT.Read(FFilename) then
      FTag := FT.Tag.Copy;
  finally
    FT.Free;
  end;
end;

destructor TPlayer.Destroy;
begin
  WidgetSet.DeallocateHWnd(FMessageHWnd);

  Players.RemovePlayer(Self);

  FreeStream(FPlayer);
  FreeAndNil(FTag);

  inherited;
end;

function TPlayer.FGetMaxByte: Int64;
begin
  Result := 0;
  if FPlayer > 0 then
    Result := BASSChannelGetLength(FPlayer, BASS_POS_BYTE);
end;

function TPlayer.FGetMaxTime: Double;
begin
  Result := 0;
  if FPlayer > 0 then
    Result := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetLength(FPlayer, BASS_POS_BYTE));
end;

function TPlayer.FGetPaused: Boolean;
begin
  Result := False;
  if FPlayer > 0 then
    Result := BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PAUSED;
end;

function TPlayer.FGetPlaying: Boolean;
begin
  Result := False;
  if FPlayer > 0 then
    Result := BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING;
end;

function TPlayer.FGetPositionByte: Int64;
begin
  Result := 0;
  if FPlayer > 0 then
    Result := BASSChannelGetPosition(FPlayer, BASS_POS_BYTE);
end;

function TPlayer.FGetPositionTime: Double;
begin
  Result := 0;
  if FPlayer > 0 then
    Result := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetPosition(FPlayer, BASS_POS_BYTE));
end;

function TPlayer.FGetStopped: Boolean;
begin
  Result := FPlayer = 0;
end;

procedure TPlayer.FreeStream(Player: Cardinal);
var
  i: Integer;
begin
  if FPlayer = 0 then
    Exit;

  for i := 0 to High(FBandData) do
  begin
    if FBandData[i].Handle = 0 then
      Continue;

    BASSChannelRemoveFX(Player, FBandData[i].Handle);
    FBandData[i].Handle := 0;
  end;
  BASSStreamFree(FPlayer);
end;

procedure TPlayer.FSetEQEnabled(Value: Boolean);
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

procedure TPlayer.FSetFilename(Value: string);
begin
  if FPlayer > 0 then
    if Value <> FFilename then
      Stop(True);

  if FPlayer = 0 then
  begin
    FFilename := Value;
    CreatePlayer;
  end;
end;

procedure TPlayer.FSetPositionByte(Value: Int64);
begin
  if FPlayer > 0 then
    if Value = MaxByte then
    begin
      Stop(False);
      if Assigned(FOnEndReached) then
        FOnEndReached(Self);
    end else
      BASSChannelSetPosition(FPlayer, Value, BASS_POS_BYTE);
end;

procedure TPlayer.FSetPositionTime(Value: Double);
begin
  if FPlayer > 0 then
    FSetPositionByte(BASSChannelSeconds2Bytes(FPlayer, FGetMaxTime - 5));
end;

procedure TPlayer.FSetPosToReach(Value: Cardinal);
begin
  if FSyncPos > 0 then
    BASSChannelRemoveSync(FPlayer, FSyncPos);
  if Value > 0 then
    FSyncPos := BASSChannelSetSync(FPlayer, BASS_SYNC_POS, Value, PosSyncProc, Self);
end;

procedure TPlayer.FSetVolume(Value: Integer);
begin
  FVolume := Value;
  if FPlayer > 0 then
    BASSChannelSetAttribute(FPlayer, 2, Value / 100);
end;

procedure TPlayer.Pause(NoFadeOut: Boolean = False);
var
  Pos, Len: Double;
  MillisecondsLeft: Cardinal;
begin
  if FPlayer > 0 then
  begin
    if Assigned(FOnStateChange) then
      FOnStateChange(Self);

    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
      FPaused := True;

    Pos := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetPosition(FPlayer, BASS_FILEPOS_CURRENT));
    Len := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetLength(FPlayer, BASS_POS_BYTE));
    MillisecondsLeft := Trunc((Len - Pos) * 1000);

    if NoFadeOut or (MillisecondsLeft < 100) then
      BASSChannelPause(FPlayer)
    else
    begin
      FSyncSlide := BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, SlideEndSyncProc, Self);
      BASSChannelSlideAttribute(FPlayer, 2, 0, Min(MillisecondsLeft, 300));
      while BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING do
        Sleep(50);
    end;

    if Assigned(FOnPause) then
      FOnPause(Self);

    MsgBus.SendMessage(TPlayingObjectStopped.Create(Self));
  end;
end;

procedure TPlayer.Play;
begin
  BASSStart;

  if Assigned(FOnStateChange) then
    FOnStateChange(Self);

  if FPlayer = 0 then
    CreatePlayer;

  BASSChannelSetAttribute(FPlayer, 2, FVolume / 100);
  BASSChannelPlay(FPlayer, False);

  if FTag <> nil then
    MsgBus.SendMessage(TPlayingObjectChangedMsg.Create(Self, FTag.Artist, FTag.Title, '', FFilename))
  else
    MsgBus.SendMessage(TPlayingObjectChangedMsg.Create(Self, '', '', '', FFilename));

  if Assigned(FOnPlay) then
    FOnPlay(Self);
end;

procedure TPlayer.SetEQ(Value, Freq: Integer);
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

procedure TPlayer.Stop(Free: Boolean; NoFadeOut: Boolean = False);
var
  Pos, Len: Double;
  MillisecondsLeft: Cardinal;
begin
  if FPlayer > 0 then
  begin
    if Assigned(FOnStateChange) then
      FOnStateChange(Self);

    FStopped := True;
    FFree := Free;

    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
    begin
      Pos := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetPosition(FPlayer, BASS_FILEPOS_CURRENT));
      Len := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetLength(FPlayer, BASS_POS_BYTE));
      MillisecondsLeft := Trunc((Len - Pos) * 1000);

      if NoFadeOut or (MillisecondsLeft < 100) then
      begin
        BASSChannelStop(FPlayer);
        if Free then
          FreeStream(FPlayer);
      end else
      begin
        FSyncSlide := BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, SlideEndSyncProc, Self);
        BASSChannelSlideAttribute(FPlayer, 2, 0, Min(MillisecondsLeft, 300));
        while BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING do
          Sleep(50);
        if Free then
          FreeStream(FPlayer);
      end;
      if Free then
        FPlayer := 0;
    end else
    begin
      BASSChannelStop(FPlayer);
      if Free then
      begin
        FreeStream(FPlayer);
        FPlayer := 0;
      end;
    end;

    if Assigned(FOnStop) then
      FOnStop(Self);

    MsgBus.SendMessage(TPlayingObjectStopped.Create(Self));
  end;
end;

procedure TPlayer.WndMethod(var Msg: TMessage);
var
  Handled: Boolean;
begin
  Handled := True;
  case Msg.Msg of
    WM_USER + BASS_SYNC_END:
      if Assigned(OnEndReached) then
        OnEndReached(Self);
    WM_USER + BASS_SYNC_POS:
      if Assigned(OnPosReached) then
        OnPosReached(Self);
    WM_USER + BASS_SYNC_DEV_FAIL:
      Stop(True, True);
    else
      Handled := False;
  end;

  if Handled then
    Msg.Result := 0
  else
    Msg.Result := DefWindowProc(FMessageHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.
