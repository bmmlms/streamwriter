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
unit ClientManager;

interface

uses
  SysUtils, Windows, Classes, Generics.Collections, ICEClient,
  Functions, AppData, DataManager, HomeCommunication, PlayerManager;

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

  TSongSavedEvent = procedure(Sender: TObject; Filename, Title: string; Filesize, Length: UInt64; WasCut, FullTitle: Boolean) of object;
  TTitleChangedEvent = procedure(Sender: TObject; Title: string) of object;
  TICYReceivedEvent = procedure(Sender: TObject; Received: Integer) of object;

  TClientManager = class
  private
    FClients: TClientList;
    FSongsSaved: Integer;
    FLists: TDataLists;

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
    procedure ClientSongSaved(Sender: TObject; Filename, Title: string; Filesize, Length: UInt64; WasCut, FullTitle: Boolean);
    procedure ClientTitleChanged(Sender: TObject; Title: string);
    procedure ClientDisconnected(Sender: TObject);
    procedure ClientICYReceived(Sender: TObject; Bytes: Integer);
    procedure ClientURLsReceived(Sender: TObject);
    procedure ClientTitleAllowed(Sender: TObject; Title: string; var Allowed: Boolean; var Match: string; var Filter: Integer);

    procedure ClientPlay(Sender: TObject);
    procedure ClientPause(Sender: TObject);
    procedure ClientStop(Sender: TObject);

    procedure HomeCommTitleChanged(Sender: TObject; StreamName, Title, CurrentURL, Format, TitlePattern: string; Kbps: Cardinal);
  public
    constructor Create(Lists: TDataLists);
    destructor Destroy; override;

    function GetEnumerator: TClientEnum;

    function AddClient(URL: string): TICEClient; overload;
    function AddClient(Name, StartURL: string; IsAuto: Boolean = False): TICEClient; overload;
    function AddClient(Entry: TStreamEntry): TICEClient; overload;
    procedure RemoveClient(Client: TICEClient);
    procedure Terminate;

    procedure SetupClient(Client: TICEClient);

    property Items[Index: Integer]: TICEClient read FGetItem; default;
    property Count: Integer read FGetCount;

    function GetClient(Name, URL, Title: string; URLs: TStringList): TICEClient;

    property Active: Boolean read FGetActive;
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

function TClientManager.AddClient(Name, StartURL: string; IsAuto: Boolean = False): TICEClient;
var
  C: TICEClient;
begin
  C := TICEClient.Create(Name, StartURL);
  // Ist hier, damit das ClientView das direkt weiß und passig in die Kategorie packt
  if IsAuto then
    C.AutoRemove := True;
  SetupClient(C);
  Result := C;
end;

function TClientManager.AddClient(Entry: TStreamEntry): TICEClient;
var
  C: TICEClient;
begin
  C := TICEClient.Create(Entry);
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
  Client.OnPlay := ClientPlay;
  Client.OnPause := ClientPause;
  Client.OnStop := ClientStop;
  if Assigned(FOnClientAdded) then
    FOnClientAdded(Client);
end;

procedure TClientManager.Terminate;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    RemoveClient(FClients[i]);
end;

constructor TClientManager.Create(Lists: TDataLists);
begin
  inherited Create;
  FSongsSaved := 0;
  FClients := TClientList.Create;
  FLists := Lists;
  HomeComm.OnTitleChanged := HomeCommTitleChanged;
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

procedure TClientManager.HomeCommTitleChanged(Sender: TObject; StreamName, Title,
  CurrentURL, Format, TitlePattern: string; Kbps: Cardinal);
var
  i: Integer;
  AutoTuneInMinKbps: Cardinal;
  Client: TICEClient;
begin
  AutoTuneInMinKbps := 0;

  case AppGlobals.AutoTuneInMinKbps of
    0: AutoTuneInMinKbps := 0;
    1: AutoTuneInMinKbps := 64;
    2: AutoTuneInMinKbps := 96;
    3: AutoTuneInMinKbps := 128;
    4: AutoTuneInMinKbps := 160;
    5: AutoTuneInMinKbps := 192;
    6: AutoTuneInMinKbps := 224;
    7: AutoTuneInMinKbps := 256;
    8: AutoTuneInMinKbps := 320;
    9: AutoTuneInMinKbps := 384;
  end;

  if Kbps < AutoTuneInMinKbps then
    Exit;
  if (Format = 'mp3') and (AppGlobals.AutoTuneInFormat = 2) then
    Exit;
  if (Format = 'aac') and (AppGlobals.AutoTuneInFormat = 1) then
    Exit;
  if (Format = '') and (AppGlobals.AutoTuneInFormat > 0) then
    Exit;

  for i := 0 to FLists.StreamBlacklist.Count - 1 do
    if FLists.StreamBlacklist[i] = StreamName then
      Exit;

  for i := 0 to FLists.SaveList.Count - 1 do
  begin
    if Like(Title, FLists.SaveList[i].Pattern) then
    begin
      // csStopping wird extra abgefragt. Stellen wir uns vor es gibt 2 Wunschlieder,
      // die auf dem selben Stream hintereinander gespielt werden. Der RecordTitle ist
      // dann das erste Lied. Während der Stream noch in der Liste ist und in ein paar MS
      // die Verbindung kappen wird und noch Plugins laufen oder so würde er hier sonst
      // nicht reingehen, um das zweite Wunschlied aufzunehmen.
      Client := GetClient(StreamName, CurrentURL, Title, nil);
      if not ((Client <> nil) and (Client.AutoRemove) and (Client.State <> csStopping) and (Client.Entry.Settings.SeparateTracks)) then
      begin
        Client := AddClient(StreamName, CurrentURL, True);
        Client.Entry.Settings.Filter := ufNone;
        Client.Entry.Settings.SaveToMemory := True;
        Client.Entry.Settings.SeparateTracks := True;
        Client.Entry.Settings.OnlySaveFull := False;
        Client.Entry.Settings.DeleteStreams := False;
        Client.Entry.Settings.MaxRetries := 0;
        Client.Entry.Settings.RetryDelay := 0;
        if Trim(TitlePattern) <> '' then
          Client.Entry.Settings.TitlePattern := TitlePattern;
        Client.RecordTitle := Title;
        Client.StartRecording;
      end;
      Break;
    end;
  end;
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

function TClientManager.GetClient(Name, URL, Title: string;
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
      if LowerCase(Client.Entry.Name) = LowerCase(Name) then
      begin
        Result := Client;
        Exit;
      end;

    if URL <> '' then
      if LowerCase(Client.Entry.StartURL) = LowerCase(URL) then
      begin
        Result := Client;
        Exit;
      end;

    if Title <> '' then
      if LowerCase(Client.Title) = LowerCase(Title) then
      begin
        Result := Client;
        Exit;
      end;

    if URLs <> nil then
      for i := 0 to Client.Entry.URLs.Count - 1 do
        for n := 0 to URLs.Count - 1 do
          if (LowerCase(Client.Entry.URLs[i]) = LowerCase(URL)) or
             (LowerCase(Client.Entry.URLs[i]) = LowerCase(URLs[n])) then
          begin
            Result := Client;
            Exit;
          end;
  end;
end;

procedure TClientManager.ClientSongSaved(Sender: TObject; Filename, Title: string; Filesize, Length: UInt64; WasCut, FullTitle: Boolean);
begin
  Inc(FSongsSaved);
  if Assigned(FOnClientSongSaved) then
    FOnClientSongSaved(Sender, Filename, Title, Filesize, Length, WasCut, FullTitle);
end;

procedure TClientManager.ClientStop(Sender: TObject);
begin
  if Players.LastPlayer = Sender then
    Players.LastPlayer := nil;
  if Assigned(FOnClientRefresh) then
    FOnClientRefresh(Sender);
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
      for n := 0 to Client.Entry.URLs.Count - 1 do
        if (FClients[i].Entry.StartURL = Client.Entry.StartURL) or
           (FClients[i].Entry.URLs.IndexOf(Client.Entry.URLs[n]) > -1) then
        begin
          if not FClients[i].Killed then
          begin
            if Client.Playing then
              FClients[i].StartPlay;
            if Client.Recording then
              FClients[i].StartRecording;
            Client.Kill;
          end;
          Break;
        end;
end;

procedure TClientManager.ClientTitleAllowed(Sender: TObject; Title: string;
  var Allowed: Boolean; var Match: string; var Filter: Integer);
begin
  FOnClientTitleAllowed(Sender, Title, Allowed, Match, Filter);
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

procedure TClientManager.ClientPause(Sender: TObject);
begin
  Players.LastPlayer := TICEClient(Sender);
  if Assigned(FOnClientRefresh) then
    FOnClientRefresh(Sender);
end;

procedure TClientManager.ClientPlay(Sender: TObject);
begin
  if Assigned(FOnClientRefresh) then
    FOnClientRefresh(Sender);
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

