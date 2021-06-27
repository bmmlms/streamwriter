{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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
  Windows, SysUtils, Messages, Classes, DynBass, Math, Logging, AppData,
  AudioFunctions, FileTagger, MessageBus, AppMessages;

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
//    FMessageHWnd: HWND;

    FEQEnabled: Boolean;
    FBandData: array[0..9] of TBandData;
    FTag: TTagData;

    FOnPosReached: TNotifyEvent;
    FOnEndReached: TNotifyEvent;
    FOnPlay: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnStop: TNotifyEvent;

    procedure CreatePlayer;
    procedure FreeStream(Player: Cardinal);

    procedure WndMethod(var Msg: TMessage);

    procedure FSetEQEnabled(Value: Boolean);
    function FGetPaused: Boolean;
    function FGetStopped: Boolean;
    function FGetPlaying: Boolean;
    procedure FSetVolume(Value: Integer);
    function FGetMaxByte: Cardinal;
    function FGetMaxTime: Double;
    function FGetPositionByte: Cardinal;
    function FGetPositionTime: Double;
    procedure FSetFilename(Value: string);
    procedure FSetPosToReach(Value: Cardinal);
    procedure FSetEndPos(Value: Cardinal);
    procedure FSetPositionByte(Value: Cardinal);
    procedure FSetPositionTime(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play;
    procedure Pause;
    procedure Stop(Free: Boolean; NoFadeOut: Boolean = False);
    procedure SetEQ(Value, Freq: Integer);

    property EQEnabled: Boolean read FEQEnabled write FSetEQEnabled;
    property Paused: Boolean read FGetPaused;
    property Stopped: Boolean read FGetStopped;
    property Playing: Boolean read FGetPlaying;
    property Filename: string read FFilename write FSetFilename;
    property Volume: Integer read FVolume write FSetVolume;
    property MaxByte: Cardinal read FGetMaxByte;
    property MaxTime: Double read FGetMaxTime;
    property PositionByte: Cardinal read FGetPositionByte write FSetPositionByte;
    property PositionTime: Double read FGetPositionTime write FSetPositionTime;
    property PosToReach: Cardinal read FPosToReach write FSetPosToReach;
    property EndPos: Cardinal read FEndPos write FSetEndPos;
    property Tag: TTagData read FTag;
    property ShowTitle: Boolean read FShowTitle write FShowTitle;

    property OnPosReached: TNotifyEvent read FOnPosReached write FOnPosReached;
    property OnEndReached: TNotifyEvent read FOnEndReached write FOnEndReached;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

uses
  PlayerManager;

procedure SlideEndSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  BASSChannelRemoveSync(channel, handle);

  P := TPlayer(user);
  if P.FPaused then
  begin
    BASSChannelPause(channel)
  end else if P.FStopped then
  begin
    BASSChannelStop(channel);
    if P.FFree then
      P.FreeStream(channel);
  end;

  P.FPaused := False;
  P.FStopped := False;
end;

procedure PosSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  P := TPlayer(user);

//  PostMessage(P.FMessageHWnd, 1001, 0, 0);
end;

procedure EndSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  P := TPlayer(user);

  P.FPaused := False;
  P.FStopped := False;

//  PostMessage(P.FMessageHWnd, 1000, 0, 0);
end;

{ TPlayer }

constructor TPlayer.Create;
var
  i: Integer;
begin
  inherited;

  FShowTitle := True;
//  FMessageHWnd := AllocateHWnd(WndMethod);     // TODO:

  for i := 0 to High(FBandData) do
  begin
    FBandData[i].Handle := 0;
  end;
end;

procedure TPlayer.CreatePlayer;
var
  FT: TFileTagger;
begin
  AppGlobals.Lock;
  try
    BASSSetDevice(AppGlobals.SoundDevice);
  finally
    AppGlobals.Unlock;
  end;

  FPlayer := BASSStreamCreateFile(False, PChar(FFilename), 0, 0, 0);
  if FPlayer = 0 then
    raise Exception.Create('');
  FSyncEnd := BASSChannelSetSync(FPlayer, BASS_SYNC_END, 0, EndSyncProc, Self);

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
//  DeallocateHWnd(FMessageHWnd);

  // Crashed bei Programmende, deshalb try..except. Ist nötig wegen dem SavedTab,
  // wenn man hier nicht freigibt, kann er nicht speichern.
  Players.RemovePlayer(Self);
  try
    // TODO:
    // FreeStream(FPlayer);
  except end;

  FreeAndNil(FTag);

  inherited;
end;

function TPlayer.FGetMaxByte: Cardinal;
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

function TPlayer.FGetPositionByte: Cardinal;
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
  for i := 0 to High(FBandData) do
  begin
    BASSChannelRemoveFX(Player, FBandData[i].Handle);
    FBandData[i].Handle := 0;
  end;
  BASSStreamFree(Player);
end;

procedure TPlayer.FSetEndPos(Value: Cardinal);
begin
  if FSyncPos > 0 then
    BASSChannelRemoveSync(FPlayer, FSyncEnd);
  if Value > 0 then
    FSyncEnd := BASSChannelSetSync(FPlayer, BASS_SYNC_POS, Value, EndSyncProc, Self);
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
      begin
        FBandData[i].Handle := BASSChannelSetFX(FPlayer, 7, 0);
      end;

      AppGlobals.Lock;
      try
        SetEQ(AppGlobals.EQGain[i], i);
      finally
        AppGlobals.Unlock;
      end;
    end;
  end else
  begin
    for i := 0 to High(FBandData) do
    begin
      if FBandData[i].Handle > 0 then
        BASSChannelRemoveFX(FPlayer, FBandData[i].Handle);
      FBandData[i].Handle := 0;
    end;
  end;
end;

procedure TPlayer.FSetFilename(Value: string);
begin
  if FPlayer > 0 then
  begin
    if Value <> FFilename then
    begin
      Stop(True);
    end;
  end;

  if FPlayer = 0 then
  begin
    FFilename := Value;
    CreatePlayer;
  end;
end;

procedure TPlayer.FSetPositionByte(Value: Cardinal);
begin
  if FPlayer > 0 then
  begin
    if Value = MaxByte then
    begin
      Stop(False);
      if Assigned(FOnEndReached) then
        FOnEndReached(Self);
    end else
      BASSChannelSetPosition(FPlayer, Value, BASS_POS_BYTE);
  end;
end;

procedure TPlayer.FSetPositionTime(Value: Double);
begin
  if FPlayer > 0 then
  begin
    FSetPositionByte(BASSChannelSeconds2Bytes(FPlayer, FGetMaxTime - 5));
  end;
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

procedure TPlayer.Pause;
var
  Pos, Len: Double;
begin
  if FPlayer > 0 then
  begin
    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
      FPaused := True;

    Pos := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetPosition(FPlayer, BASS_FILEPOS_CURRENT));
    Len := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetLength(FPlayer, BASS_POS_BYTE));

    if Len - Pos < 0.300 then
    begin
      BASSChannelPause(FPlayer);
    end else
    begin
      FSyncSlide := BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, SlideEndSyncProc, Self);
      BASSChannelSlideAttribute(FPlayer, 2, 0, Trunc(Min(Len - Pos - 10, 300)));
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
  if FPlayer = 0 then
  begin
    CreatePlayer;
  end;

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
begin
  if FPlayer > 0 then
  begin
    FStopped := True;
    FFree := Free;

    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
    begin
      Pos := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetPosition(FPlayer, BASS_FILEPOS_CURRENT));
      Len := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetLength(FPlayer, BASS_POS_BYTE));

      if (Len - Pos < 0.300) or NoFadeOut then
      begin
        BASSChannelStop(FPlayer);
        if Free then
          FreeStream(FPlayer);
      end else
      begin
        FSyncSlide := BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, SlideEndSyncProc, Self);
        BASSChannelSlideAttribute(FPlayer, 2, 0, Min(Trunc(Len - Pos - 10), 300));
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
    1000:
      if Assigned(OnEndReached) then
        OnEndReached(Self);
    1001:
      if Assigned(OnPosReached) then
        OnPosReached(Self);
    else
      Handled := False;
  end;

  if Handled then
    Msg.Result := 0
//  else
//    Msg.Result := DefWindowProc(FMessageHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.


