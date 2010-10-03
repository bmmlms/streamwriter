{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010 Alexander Nottelmann

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
unit ClientManager;

interface

uses
  SysUtils, Windows, Classes, Generics.Collections, ICEClient, RelayServer,
  Functions;

type
  TClientManager = class;

  TClientEnum = class
  private
    FIndex: Integer;
    FClients: TClientManager;
  public
    constructor Create(Clients: TClientManager);
    function GetCurrent: TICEClient;
    function MoveNext: Boolean;
    property Current: TICEClient read GetCurrent;
  end;

  TClientList = TList<TICEClient>;

  TSongSavedEvent = procedure(Sender: TObject; Filename, Title: string; Filesize: UInt64; WasCut: Boolean) of object;
  TTitleChangedEvent = procedure(Sender: TObject; Title: string) of object;
  TICYReceivedEvent = procedure(Sender: TObject; Received: Integer) of object;

  TClientManager = class
  private
    FClients: TClientList;
    FRelayServer: TRelayServer;
    FSongsSaved: Integer;

    FOnClientDebug: TNotifyEvent;
    FOnClientRefresh: TNotifyEvent;
    FOnClientAddRecent: TNotifyEvent;
    FOnClientAdded: TNotifyEvent;
    FOnClientRemoved: TNotifyEvent;
    FOnClientSongSaved: TSongSavedEvent;
    FOnClientTitleChanged: TTitleChangedEvent;
    FOnClientICYReceived: TICYReceivedEvent;
    FOnClientTitleAllowed: TTitleAllowedEvent;

    function FGetItem(Index: Integer): TICEClient;
    function FGetCount: Integer;

    function FGetActive: Boolean;

    procedure ClientDebug(Sender: TObject);
    procedure ClientRefresh(Sender: TObject);
    procedure ClientAddRecent(Sender: TObject);
    procedure ClientSongSaved(Sender: TObject; Filename, Title: string; Filesize: UInt64; WasCut: Boolean);
    procedure ClientTitleChanged(Sender: TObject; Title: string);
    procedure ClientDisconnected(Sender: TObject);
    procedure ClientICYReceived(Sender: TObject; Bytes: Integer);
    procedure ClientURLsReceived(Sender: TObject);
    procedure ClientTitleAllowed(Sender: TObject; Title: string; var Allowed: Boolean);

    procedure RelayGetStream(Sender: TObject);
    procedure RelayEnded(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TClientEnum;

    function AddClient(URL: string): TICEClient; overload;
    function AddClient(Name, StartURL: string): TICEClient; overload;
    function AddClient(Name, StartURL: string; URLs: TStringList;
      SkipShort: Boolean; SongsSaved: Cardinal): TICEClient; overload;
    procedure RemoveClient(Client: TICEClient);
    procedure Terminate;

    procedure SetupClient(Client: TICEClient);

    property Items[Index: Integer]: TICEClient read FGetItem; default;
    property Count: Integer read FGetCount;

    function GetClient(Name, URL: string; URLs: TStringList): TICEClient;

    property Active: Boolean read FGetActive;
    property RelayServer: TRelayServer read FRelayServer;
    property SongsSaved: Integer read FSongsSaved;

    property OnClientDebug: TNotifyEvent read FOnClientDebug write FOnClientDebug;
    property OnClientRefresh: TNotifyEvent read FOnClientRefresh write FOnClientRefresh;
    property OnClientAddRecent: TNotifyEvent read FOnClientAddRecent write FOnClientAddRecent;
    property OnClientAdded: TNotifyEvent read FOnClientAdded write FOnClientAdded;
    property OnClientRemoved: TNotifyEvent read FOnClientRemoved write FOnClientRemoved;
    property OnClientSongSaved: TSongSavedEvent read FOnClientSongSaved write FOnClientSongSaved;
    property OnClientTitleChanged: TTitleChangedEvent read FOnClientTitleChanged write FOnClientTitleChanged;
    property OnClientICYReceived: TICYReceivedEvent read FOnClientICYReceived write FOnClientICYReceived;
    property OnClientTitleAllowed: TTitleAllowedEvent read FOnClientTitleAllowed write FOnClientTitleAllowed;
  end;

implementation

{ TClientManager }

function TClientManager.AddClient(URL: string): TICEClient;
var
  C: TICEClient;
begin
  C := TICEClient.Create(URL);
  SetupClient(C);
  Result := C;
end;

function TClientManager.AddClient(Name, StartURL: string): TICEClient;
var
  C: TICEClient;
begin
  C := TICEClient.Create(Name, StartURL);
  SetupClient(C);
  Result := C;
end;

function TClientManager.AddClient(Name, StartURL: string; URLs: TStringList;
  SkipShort: Boolean; SongsSaved: Cardinal): TICEClient;
var
  C: TICEClient;
begin
  C := TICEClient.Create(Name, StartURL, URLs, SkipShort, SongsSaved);
  SetupClient(C);
  Result := C;
end;

procedure TClientManager.RemoveClient(Client: TICEClient);
begin
  if Client.Active then
    Client.Kill
  else
  begin
    if Assigned(FOnClientRemoved) then
      FOnClientRemoved(Client);
    FClients.Remove(Client);
    Client.Free;
  end;
end;

procedure TClientManager.SetupClient(Client: TICEClient);
begin
  FClients.Add(Client);
  Client.OnDebug := ClientDebug;
  Client.OnRefresh := ClientRefresh;
  Client.OnAddRecent := ClientAddRecent;
  Client.OnSongSaved := ClientSongSaved;
  Client.OnTitleChanged := ClientTitleChanged;
  Client.OnDisconnected := ClientDisconnected;
  Client.OnICYReceived := ClientICYReceived;
  Client.OnURLsReceived := ClientURLsReceived;
  Client.OnTitleAllowed := ClientTitleAllowed;
  if Assigned(FOnClientAdded) then
    FOnClientAdded(Client);
end;

procedure TClientManager.Terminate;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    RemoveClient(FClients[i]);
  if FRelayServer <> nil then
    FreeAndNil(FRelayServer);
end;

constructor TClientManager.Create;
begin
  inherited;
  FSongsSaved := 0;
  FClients := TClientList.Create;
  FRelayServer := TRelayServer.Create;
  FRelayServer.OnGetStream := RelayGetStream;
  FRelayServer.OnEnded := RelayEnded;
end;

destructor TClientManager.Destroy;
begin
  FClients.Free;
  inherited;
end;

function TClientManager.FGetActive: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FClients.Count - 1 do
    if FClients[i].Active then
    begin
      Result := True;
      Exit;
    end;
end;

function TClientManager.FGetCount: Integer;
begin
  Result := FClients.Count;
end;

function TClientManager.FGetItem(Index: Integer): TICEClient;
begin
  Result := FClients[Index];
end;

function TClientManager.GetEnumerator: TClientEnum;
begin
  Result := TClientEnum.Create(Self);
end;

procedure TClientManager.ClientDebug(Sender: TObject);
begin
  if Assigned(FOnClientDebug) then
    FOnClientDebug(Sender);
end;

procedure TClientManager.ClientRefresh(Sender: TObject);
begin
  if Assigned(FOnClientRefresh) then
    FOnClientRefresh(Sender);
end;

procedure TClientManager.ClientAddRecent(Sender: TObject);
begin
  if Assigned(FOnClientAddRecent) then
    FOnClientAddRecent(Sender);
end;

function TClientManager.GetClient(Name, URL: string;
  URLs: TStringList): TICEClient;
var
  i: Integer;
  n: Integer;
  Client: TICEClient;
begin
  Name := Trim(Name);
  URL := Trim(URL);

  Result := nil;
  for Client in FClients do
  begin
    if Name <> '' then
      if LowerCase(Client.StreamName) = LowerCase(Name) then
      begin
        Result := Client;
        Exit;
      end;

    if URL <> '' then
      if LowerCase(Client.StartURL) = LowerCase(URL) then
      begin
        Result := Client;
        Exit;
      end;

    if URLs <> nil then
      for i := 0 to Client.URLs.Count - 1 do
        for n := 0 to URLs.Count - 1 do
          if (LowerCase(Client.URLs[i]) = LowerCase(URL)) or
             (LowerCase(Client.URLs[i]) = LowerCase(URLs[n])) then
          begin
            Result := Client;
            Exit;
          end;
  end;
end;

procedure TClientManager.ClientSongSaved(Sender: TObject; Filename, Title: string; Filesize: UInt64; WasCut: Boolean);
begin
  Inc(FSongsSaved);
  //if Assigned(FOnClientRefresh) then
  //  FOnClientRefresh(Sender);
  if Assigned(FOnClientSongSaved) then
    FOnClientSongSaved(Sender, Filename, Title, Filesize, WasCut);
end;

procedure TClientManager.ClientTitleChanged(Sender: TObject;
  Title: string);
begin
  if Assigned(FOnClientTitleChanged) then
    FOnClientTitleChanged(Sender, Title);
end;

procedure TClientManager.ClientURLsReceived(Sender: TObject);
var
  i, n: Integer;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;
  for i := 0 to FClients.Count - 1 do
    if FClients[i] <> Client then
      for n := 0 to Client.URLs.Count - 1 do
        if FClients[i].URLs.IndexOf(Client.URLs[n]) > -1 then
        begin
          Client.Kill;
          Break;
        end;
end;

procedure TClientManager.ClientTitleAllowed(Sender: TObject; Title: string;
  var Allowed: Boolean);
begin
  FOnClientTitleAllowed(Sender, Title, Allowed);
end;

procedure TClientManager.ClientDisconnected(Sender: TObject);
var
  Client: TICEClient;
begin
  // Die Funktion hier wird nich nur aufgerufen, wenn der ICE-Thread stirbt, sondern auch,
  // wenn alle Plugins abgearbeitet wurden und es keinen ICE-Thread gibt.
  if Assigned(FOnClientRefresh) then
    FOnClientRefresh(Sender);
  Client := Sender as TICEClient;
  if Client.Killed and (Client.ProcessingList.Count = 0) then
  begin
    if Assigned(FOnClientRemoved) then
      FOnClientRemoved(Sender);
    FClients.Remove(Client);
    Client.Free;
  end;
end;

procedure TClientManager.ClientICYReceived(Sender: TObject;
  Bytes: Integer);
begin
  if Assigned(FOnClientICYReceived) then
    FOnClientICYReceived(Sender, Bytes);
end;

procedure TClientManager.RelayGetStream(Sender: TObject);
var
  T: TRelayThread;
  URL: string;
  i: Integer;
begin
  T := TRelayThread(Sender);
  URL := T.RecvStream.RequestedURL;
  if Length(URL) > 1 then
    URL := Copy(URL, 2, Length(URL) - 1);
  for i := 0 to FClients.Count - 1 do
    if ((StripURL(string(FClients[i].StreamName)) = URL) or
        (StripURL(string(FClients[i].StartURL)) = URL)) and
        (FClients[i].Active) then
    begin
      FClients[i].AddRelayThread(T);
      T.StationName := FClients[i].StreamName;
      T.ContentType := FClients[i].ContentType;
      Exit;
    end;
  T.Terminate;
end;

procedure TClientManager.RelayEnded(Sender: TObject);
var
  T: TRelayThread;
  i: Integer;
begin
  T := TRelayThread(Sender);
  for i := 0 to FClients.Count - 1 do
  begin
    FClients[i].RemoveRelayThread(T);
    FClients[i].WriteDebug('Relay disconnected');
  end;
end;

{ TClientEnum }

constructor TClientEnum.Create(Clients: TClientManager);
begin
  inherited Create;
  FClients := Clients;
  FIndex := -1;
end;

function TClientEnum.GetCurrent: TICEClient;
begin
  Result := FClients[FIndex];
end;

function TClientEnum.MoveNext: Boolean;
begin
  Result := FIndex < Pred(FClients.Count);
  if Result then
    Inc(FIndex);
end;

end.

