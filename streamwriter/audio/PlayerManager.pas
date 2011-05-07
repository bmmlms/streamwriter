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
unit PlayerManager;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Logging;

type
  TPlayerManager = class
  private
    FCS: TCriticalSection;
    FPlayers: TList;
    FVolume: Cardinal;
    FLastPlayer: TObject;

    procedure Play(Player: TObject);

    procedure FSetVolume(Value: Cardinal);
    function FGetAllStoppedOrPaused: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PauseAll;
    procedure StopAll;

    procedure AddPlayer(Player: TObject);
    procedure RemovePlayer(Player: TObject);

    property Volume: Cardinal read FVolume write FSetVolume;
    property AllStoppedOrPaused: Boolean read FGetAllStoppedOrPaused;
    property LastPlayer: TObject read FLastPlayer write FLastPlayer;
  end;

var
  Players: TPlayerManager;

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

procedure TPlayerManager.FSetVolume(Value: Cardinal);
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  FVolume := Value;
  for i := 0 to FPlayers.Count - 1 do
    if TPlayer(FPlayers[i]) is TPlayer then
    begin
      P := TPlayer(FPlayers[i]);
      P.Volume := Value;
    end else if TICEClient(FPlayers[i]) is TICEClient then
    begin
      IP := TICEClient(FPlayers[i]);
      IP.SetVolume(Value);
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
    TICEClient(Player).StartPlay;
end;

procedure TPlayerManager.RemovePlayer(Player: TObject);
begin
  if FLastPlayer = Player then
    FLastPlayer := nil;
  FPlayers.Remove(Player);
end;

procedure TPlayerManager.StopAll;
var
  i: Integer;
  P: TPlayer;
  IP: TICEClient;
begin
  // Wenn alles pausiert ist, den letzten pausierten wieder starten,
  // ansonsten eben alle pausieren.
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

initialization
  Players := TPlayerManager.Create;
finalization
  Players.Free;

end.
