{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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

{ This unit contains everything to control audio-players application-wide }
unit PlayerManager;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Logging, MessageBus, AppMessages,
  AppData;

type
  TPlayerManager = class
  private
    FCS: TCriticalSection;
    FPlayers: TList;
    FVolume: Integer;
    FVolumeBeforeMute: Integer;
    FLastPlayer: TObject;
    FEQEnabled: Boolean;

    procedure Play(Player: TObject);

    procedure FSetVolume(Value: Integer);
    function FGetAllStoppedOrPaused: Boolean;
    function FGetAnyPlayingOrPaused: Boolean;
    function FGetAnyPlaying: Boolean;
    procedure FSetEQEnabled(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure PauseAll;
    procedure StopAll;

    procedure AddPlayer(Player: TObject);
    procedure RemovePlayer(Player: TObject);

    procedure IncreaseVolume;
    procedure DecreaseVolume;
    procedure Mute;
    procedure SetEQ(Value, Freq: Integer);
    procedure GetPlayingInfo(var Artist, Title, Stream, Filename: string);

    property Volume: Integer read FVolume write FSetVolume;
    property VolumeBeforeMute: Integer read FVolumeBeforeMute write FVolumeBeforeMute;
    property AllStoppedOrPaused: Boolean read FGetAllStoppedOrPaused;
    property AnyPlayingOrPaused: Boolean read FGetAnyPlayingOrPaused;
    property AnyPlaying: Boolean read FGetAnyPlaying;
    property LastPlayer: TObject read FLastPlayer write FLastPlayer;
    property EQEnabled: Boolean read FEQEnabled write FSetEQEnabled;
  end;

var
  Players: TPlayerManager;

procedure InitPlayerManager;

implementation

uses
  Player, ICEClient;

{ TPlayerManager }

procedure TPlayerManager.AddPlayer(Player: TObject);
begin
  FPlayers.Add(Player);
end;

constructor TPlayerManager.Create;
begin
  FCS := TCriticalSection.Create;
  FPlayers := TList.Create;

  FVolume := AppGlobals.PlayerVolume;
  FVolumeBeforeMute := AppGlobals.PlayerVolumeBeforeMute;
end;

procedure TPlayerManager.IncreaseVolume;
begin
  Volume := FVolume + 10;
end;

procedure TPlayerManager.Mute;
begin
  if Volume = 0 then
    Volume := FVolumeBeforeMute
  else
    Volume := 0;
end;

procedure TPlayerManager.DecreaseVolume;
begin
  Volume := FVolume - 10;
end;

destructor TPlayerManager.Destroy;
begin
  FPlayers.Free;
  FCS.Free;
  inherited;
end;

function TPlayerManager.FGetAllStoppedOrPaused: Boolean;
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  Result := True;
  for i := 0 to FPlayers.Count - 1 do
    if TObject(FPlayers[i]) is TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      if P.Playing and not P.Paused then
      begin
        Result := False;
        Exit;
      end;
    end else if TObject(FPlayers[i]) is TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      if IP.Playing and not IP.Paused then
      begin
        Result := False;
        Exit;
      end;
    end;
end;

function TPlayerManager.FGetAnyPlaying: Boolean;
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  Result := False;
  for i := 0 to FPlayers.Count - 1 do
    if TObject(FPlayers[i]) is TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      if P.Playing and (not P.Paused) then
      begin
        Result := True;
        Exit;
      end;
    end else if TObject(FPlayers[i]) is TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      if IP.Playing and (not IP.Paused) then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

function TPlayerManager.FGetAnyPlayingOrPaused: Boolean;
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  Result := False;
  for i := 0 to FPlayers.Count - 1 do
    if TObject(FPlayers[i]) is TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      if P.Playing or P.Paused then
      begin
        Result := True;
        Exit;
      end;
    end else if TObject(FPlayers[i]) is TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      if IP.Playing or IP.Paused then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

procedure TPlayerManager.FSetEQEnabled(Value: Boolean);
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  if Value = FEQEnabled then
    Exit;

  FEQEnabled := Value;

  for i := 0 to FPlayers.Count - 1 do
  begin
    if TObject(FPlayers[i]).ClassType = TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      P.EQEnabled := Value;
    end else if TObject(FPlayers[i]).ClassType = TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      IP.EQEnabled := Value;
    end;
  end;
end;

procedure TPlayerManager.FSetVolume(Value: Integer);
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  if Value > 100 then
    Value := 100;
  if Value < 0 then
    Value := 0;

  if (Value = 0) and (Volume > 0) then
    FVolumeBeforeMute := Volume;

  FVolume := Value;

  for i := 0 to FPlayers.Count - 1 do
  begin
    if TObject(FPlayers[i]).ClassType = TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      P.Volume := Value;
    end else if TObject(FPlayers[i]).ClassType = TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      IP.SetVolume(Value);
    end;
  end;

  MsgBus.SendMessage(TVolumeChangedMsg.Create(Value));
end;

procedure TPlayerManager.GetPlayingInfo(var Artist, Title, Stream,
  Filename: string);
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  Artist := '';
  Title := '';
  Stream := '';
  Filename := '';

  for i := 0 to FPlayers.Count - 1 do
  begin
    if TObject(FPlayers[i]) is TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      if P.Playing and P.ShowTitle then
      begin
        if P.Tag <> nil then
        begin
          Artist := P.Tag.Artist;
          Title := P.Tag.Title;
        end;
        Filename := P.Filename;
        Exit;
      end;
    end else if TObject(FPlayers[i]) is TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      if IP.Playing and (not IP.Paused or IP.PlayingStarted) and (not IP.PlayingPaused) then
      begin
        Stream := IP.Entry.Name;
        Title := IP.DisplayTitle;
        Exit;
      end;
    end;
  end;
end;

procedure TPlayerManager.PauseAll;
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  // Wenn alles pausiert ist, den letzten pausierten wieder starten,
  // ansonsten eben alle pausieren.

  if AllStoppedOrPaused then
  begin
    if FLastPlayer <> nil then
      Play(FLastPlayer);
  end else
  begin
    for i := 0 to FPlayers.Count - 1 do
    begin
      if TObject(FPlayers[i]) is TPlayer then
      begin
        P := TPlayer(FPlayers[i]);
        if P.Playing then
        begin
          FLastPlayer := P;
          P.Pause;
        end;
      end else if TObject(FPlayers[i]) is TICEClient then
      begin
        IP := TICEClient(FPlayers[i]);
        if IP.Playing and (not IP.Paused) then
        begin
          FlastPlayer := IP;
          IP.PausePlay;
        end;
      end;
    end;
  end;
end;

procedure TPlayerManager.Play(Player: TObject);
begin
  if TObject(Player) is TPlayer then
    TPlayer(Player).Play
  else if TObject(Player) is TICEClient then
  begin
    TICEClient(Player).StartPlay(True);
  end;
end;

procedure TPlayerManager.RemovePlayer(Player: TObject);
begin
  if FLastPlayer = Player then
    FLastPlayer := nil;
  FPlayers.Remove(Player);
end;

procedure TPlayerManager.SetEQ(Value, Freq: Integer);
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  for i := 0 to FPlayers.Count - 1 do
    if TObject(FPlayers[i]) is TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      P.SetEQ(Value, Freq);
    end else if TObject(FPlayers[i]) is TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      IP.SetEQ(Value, Freq);
    end;
  FLastPlayer := nil;
end;

procedure TPlayerManager.StopAll;
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  for i := 0 to FPlayers.Count - 1 do
    if TObject(FPlayers[i]) is TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      P.Stop(False);
    end else if TObject(FPlayers[i]) is TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      IP.StopPlay;
    end;
  FLastPlayer := nil;
end;

procedure InitPlayerManager;
begin
  Players := TPlayerManager.Create;
end;

initialization

finalization
  FreeAndNil(Players);

end.
