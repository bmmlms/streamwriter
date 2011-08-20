{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit Player;

interface

uses
  Windows, SysUtils, Classes, DynBass, Math, Logging, AppData;

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

    FOnPosReached: TNotifyEvent;
    FOnEndReached: TNotifyEvent;
    FOnPlay: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnStop: TNotifyEvent;

    procedure CreatePlayer;

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
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play;
    procedure Pause;
    procedure Stop(Free: Boolean);

    property Paused: Boolean read FGetPaused;
    property Stopped: Boolean read FGetStopped;
    property Playing: Boolean read FGetPlaying;
    property Filename: string read FFilename write FSetFilename;
    property Volume: Integer read FVolume write FSetVolume;
    property MaxByte: Cardinal read FGetMaxByte;
    property MaxTime: Double read FGetMaxTime;
    property PositionByte: Cardinal read FGetPositionByte write FSetPositionByte;
    property PositionTime: Double read FGetPositionTime;
    property PosToReach: Cardinal read FPosToReach write FSetPosToReach;
    property EndPos: Cardinal read FEndPos write FSetEndPos;

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
      BASSStreamFree(channel);
  end;

  P.FPaused := False;
  P.FStopped := False;
end;

procedure PosSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  P := TPlayer(user);

  if Assigned(P.FOnPosReached) then
    P.FOnPosReached(P);
end;

procedure EndSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  P := TPlayer(user);

  P.FPaused := False;
  P.FStopped := False;

  if Assigned(P.OnEndReached) then
    P.OnEndReached(P);
end;

{ TPlayer }

constructor TPlayer.Create;
begin
  inherited;

end;

procedure TPlayer.CreatePlayer;
begin
  BASSSetDevice(AppGlobals.SoundDevice);
  FPlayer := BASSStreamCreateFile(False, PChar(FFilename), 0, 0, {$IFDEF UNICODE}BASS_UNICODE{$ENDIF});
  if FPlayer = 0 then
    raise Exception.Create('');
  FSyncEnd := BASSChannelSetSync(FPlayer, BASS_SYNC_END, 0, EndSyncProc, Self);
end;

destructor TPlayer.Destroy;
begin
  // Crashed bei Programmende, deshalb try..except. Ist nötig wegen dem SavedTab,
  // wenn man hier nicht freigibt, kann er nicht speichern.
  Players.RemovePlayer(Self);
  try
    BASSStreamFree(FPlayer);
  except end;

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

procedure TPlayer.FSetEndPos(Value: Cardinal);
begin
  if FSyncPos > 0 then
    BASSChannelRemoveSync(FPlayer, FSyncEnd);
  if Value > 0 then
    FSyncEnd := BASSChannelSetSync(FPlayer, BASS_SYNC_POS, Value, EndSyncProc, Self);
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
      FOnPause(Self)
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

  if Assigned(FOnPlay) then
    FOnPlay(Self);
end;

procedure TPlayer.Stop(Free: Boolean);
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

      if Len - Pos < 0.300 then
      begin
        BASSChannelStop(FPlayer);
        if Free then
          BASSStreamFree(FPlayer);
      end else
      begin
        FSyncSlide := BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, SlideEndSyncProc, Self);
        BASSChannelSlideAttribute(FPlayer, 2, 0, Min(Trunc(Len - Pos - 10), 300));
        while BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING do
          Sleep(50);
        if Free then
          BASSStreamFree(FPlayer);
      end;
      if Free then
        FPlayer := 0;
    end else
    begin
      BASSChannelStop(FPlayer);
      if Free then
      begin
        BASSStreamFree(FPlayer);
        FPlayer := 0;
      end;
    end;

    if Assigned(FOnStop) then
      FOnStop(Self);
  end;
end;

end.


