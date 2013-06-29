{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  SysUtils, Windows, Classes, Generics.Collections, ICEClient, Logging,
  Functions, AppData, DataManager, HomeCommunication, PlayerManager,
  AudioFunctions, SWFunctions, TypeDefs, MessageBus, AppMessages;

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

  TTitleChangedEvent = procedure(Sender: TObject; Title: string) of object;
  TICYReceivedEvent = procedure(Sender: TObject; Received: Integer) of object;

  TClientManager = class
  private
    FClients: TClientList;
    FMonitorClients: TClientList;
    FSongsSaved: Integer;
    FLists: TDataLists;
    FErrorShown: Boolean;

    FOnClientDebug: TNotifyEvent;
    FOnClientRefresh: TNotifyEvent;
    FOnClientAddRecent: TNotifyEvent;
    FOnClientAdded: TNotifyEvent;
    FOnClientRemoved: TNotifyEvent;
    FOnClientSongSaved: TSongSavedEvent;
    FOnClientTitleChanged: TTitleChangedEvent;
    FOnClientICYReceived: TICYReceivedEvent;
    FOnClientTitleAllowed: TTitleAllowedEvent;
    FOnShowErrorMessage: TShowErrorMessageEvent;
    FOnPlaybackStarted: TNotifyEvent;
    FOnClientSecondsReceived: TNotifyEvent;

    function FGetItem(Index: Integer): TICEClient;
    function FGetCount: Integer;

    function FGetActive: Boolean;

    procedure ClientDebug(Sender: TObject);
    procedure ClientRefresh(Sender: TObject);
    procedure ClientAddRecent(Sender: TObject);
    procedure ClientSongSaved(Sender: TObject; Filename, Title, SongArtist, SongTitle: string; Filesize, Length, Bitrate: UInt64;
      VBR, WasCut, FullTitle, IsStreamFile: Boolean; ServerTitleHash, ServerArtistHash: Cardinal);
    procedure ClientTitleChanged(Sender: TObject; Title: string);
    procedure ClientDisconnected(Sender: TObject);
    procedure ClientICYReceived(Sender: TObject; Bytes: Integer);
    procedure ClientURLsReceived(Sender: TObject);
    procedure ClientTitleAllowed(Sender: TObject; Title: string; var Allowed: Boolean; var Match: string; var Filter: Integer);
    procedure ClientPlaybackStarted(Sender: TObject);
    procedure ClientSecondsReceived(Sender: TObject);

    procedure ClientPlay(Sender: TObject);
    procedure ClientPause(Sender: TObject);
    procedure ClientStop(Sender: TObject);

    procedure HomeCommTitleChanged(Sender: TObject; ID: Cardinal; Name, Title, CurrentURL, TitleRegEx: string;
      Format: TAudioTypes; Kbps: Cardinal; ServerHash, ServerArtistHash: Cardinal);
    procedure HomeCommMonitorStreamsReceived(Sender: TObject; StreamIDs: TIntArray);
  public
    constructor Create(Lists: TDataLists);
    destructor Destroy; override;

    function GetEnumerator: TClientEnum;

    function AddClient(URL: string): TICEClient; overload;
    function AddClient(ID, Bitrate: Cardinal; Name, StartURL: string; IsAuto: Boolean = False): TICEClient; overload;
    function AddClient(Entry: TStreamEntry): TICEClient; overload;
    procedure RemoveClient(Client: TICEClient);
    procedure RemoveMonitorClient(Client: TICEClient);
    procedure Stop;
    procedure Terminate;

    property Monitors: TClientList read FMonitorClients;
    procedure StopMonitors;

    procedure SetupClient(Client: TICEClient);

    property Items[Index: Integer]: TICEClient read FGetItem; default;
    property Count: Integer read FGetCount;

    function MatchesClient(Client: TICEClient; ID: Integer; Name, URL, Title: string;
      URLs: TStringList): Boolean;
    function GetClient(ID: Integer; Name, URL, Title: string; URLs: TStringList): TICEClient;
    function GetUsedBandwidth(Bitrate, Speed: Int64; ClientToAdd: TICEClient = nil): Integer;

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
    property OnShowErrorMessage: TShowErrorMessageEvent read FOnShowErrorMessage write FOnShowErrorMessage;
    property OnPlaybackStarted: TNotifyEvent read FOnPlaybackStarted write FOnPlaybackStarted;
    property OnClientSecondsReceived: TNotifyEvent read FOnClientSecondsReceived write FOnClientSecondsReceived;
  end;

implementation

{ TClientManager }

function TClientManager.AddClient(URL: string): TICEClient;
var
  C: TICEClient;
begin
  C := TICEClient.Create(Self, URL);
  SetupClient(C);
  Result := C;
end;

function TClientManager.AddClient(ID, Bitrate: Cardinal; Name, StartURL: string; IsAuto: Boolean = False): TICEClient;
var
  C: TICEClient;
begin
  C := TICEClient.Create(Self, ID, Bitrate, Name, StartURL);
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
  C := TICEClient.Create(Self, Entry);
  SetupClient(C);
  Result := C;
end;

function TClientManager.GetUsedBandwidth(Bitrate, Speed: Int64; ClientToAdd: TICEClient = nil): Integer;
var
  Client: TICEClient;
  UsedKBs: Integer;
begin
  UsedKBs := -1;

  for Client in FClients do
  begin
    if ClientToAdd = Client then
      if Client.Recording or Client.Playing then
      begin
        Result := 0;
        Exit;
      end;
  end;

  if UsedKBs = -1 then
    if (Bitrate = 0) and (Speed = 0) then
      UsedKBs := 18
    else if Bitrate > 0 then
      UsedKBs := Bitrate div 8
    else
      UsedKBs := Speed;

  for Client in FClients do
  begin
    if ClientToAdd <> Client then
      if Client.Recording or Client.Playing then
      begin
        if Client.Entry.Bitrate >= 64 then
          UsedKBs := UsedKBs + (Integer(Client.Entry.Bitrate) div 8) + 3
        else
          UsedKBs := UsedKBs + Client.Speed div 1024;
      end;
  end;

  Result := UsedKBs;
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

procedure TClientManager.RemoveMonitorClient(Client: TICEClient);
begin
  if Client.Active then
    Client.Kill
  else
  begin
    FMonitorClients.Remove(Client);
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
  Client.OnPlaybackStarted := ClientPlaybackStarted;
  Client.OnSecondsReceived := ClientSecondsReceived;
  Client.OnPlay := ClientPlay;
  Client.OnPause := ClientPause;
  Client.OnStop := ClientStop;
  if Assigned(FOnClientAdded) then
    FOnClientAdded(Client);
end;

procedure TClientManager.Stop;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    FClients[i].Entry.WasRecording := FClients[i].Recording;
    FClients[i].Stop;
  end;

  for i := FMonitorClients.Count - 1 downto 0 do
    RemoveMonitorClient(FMonitorClients[i]);
end;

procedure TClientManager.StopMonitors;
var
  i: Integer;
begin
  for i := FMonitorClients.Count - 1 downto 0 do
    RemoveMonitorClient(FMonitorClients[i]);
  FMonitorClients.Clear;
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
  FMonitorClients := TClientList.Create;
  FLists := Lists;
  HomeComm.OnNetworkTitleChangedReceived := HomeCommTitleChanged;
  HomeComm.OnMonitorStreamsReceived := HomeCommMonitorStreamsReceived;
end;

destructor TClientManager.Destroy;
begin
  FClients.Free;
  FMonitorClients.Free;

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

  for i := 0 to FMonitorClients.Count - 1 do
    if FMonitorClients[i].Active then
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

procedure TClientManager.HomeCommMonitorStreamsReceived(Sender: TObject;
  StreamIDs: TIntArray);
var
  i, n: Integer;
  Client: TICEClient;
begin
  for i := 0 to FMonitorClients.Count - 1 do
    RemoveClient(FMonitorClients[i]);

  StopMonitors;
  for i := 0 to High(StreamIDs) do
  begin
    for n := 0 to FLists.BrowserList.Count - 1 do
      if FLists.BrowserList[n].ID = StreamIDs[i] then
      begin
        Client := TICEClient.Create(Self, StreamIDs[i], 128, 'Monitor' + IntToStr(StreamIDs[i]), FLists.BrowserList[n].URL);
        Client.Entry.Settings.MaxRetries := 0;
        Client.Entry.Settings.RetryDelay := 30;
        Client.Entry.Settings.SaveToMemory := True;
        Client.OnDisconnected := ClientDisconnected;
        Client.StartMonitoring;

        FMonitorClients.Add(Client);
      end;
  end;
end;

procedure TClientManager.HomeCommTitleChanged(Sender: TObject; ID: Cardinal;
  Name, Title, CurrentURL, TitleRegEx: string; Format: TAudioTypes; Kbps: Cardinal;
  ServerHash, ServerArtistHash: Cardinal);
var
  i, n: Integer;
  AutoTuneInMinKbps: Cardinal;
  Client: TICEClient;
  Res: TMayConnectResults;
  Found: Boolean;
begin
  AutoTuneInMinKbps := GetAutoTuneInMinKbps(TAudioTypes(Format), AppGlobals.AutoTuneInMinQuality);

  if Kbps < AutoTuneInMinKbps then
    Exit;

  if (AppGlobals.AutoTuneInFormat > 0) and (TAudioTypes(AppGlobals.AutoTuneInFormat) <> Format) then
    Exit;

  for i := 0 to FLists.StreamBlacklist.Count - 1 do
    if FLists.StreamBlacklist[i] = Name then
      Exit;

  for i := 0 to FLists.SaveList.Count - 1 do
  begin
    if ((FLists.SaveList[i].ServerHash > 0) and (ServerHash = FLists.SaveList[i].ServerHash)) or
       ((FLists.SaveList[i].ServerArtistHash > 0) and (ServerArtistHash = FLists.SaveList[i].ServerArtistHash)) then
    begin
      if AppGlobals.AutoTuneInConsiderIgnore then
      begin
        for n := 0 to FLists.IgnoreList.Count - 1 do
        begin
          if Like(Title, FLists.IgnoreList[n].Pattern) then
          begin
            Exit;
          end;
        end;
      end;

      Res := TICEClient.MayConnect(False, GetUsedBandwidth(Kbps, 0));
      if Res <> crOk then
      begin
        if (not FErrorShown) and (Res = crNoFreeSpace) then
        begin
          OnShowErrorMessage(nil, Res, True, False);
          FErrorShown := True;
        end;
        Exit;
      end;
      FErrorShown := False;

      Found := False;
      for Client in Self.FClients do
      begin
        if MatchesClient(Client, ID, Name, CurrentURL, Title, nil) then
        begin
          if (Client.AutoRemove and (Client.RecordTitle = Title)) or (Client.Recording) then
          begin
            Found := True;
            Break;
          end;
        end;
      end;

      if not Found then
      begin
        Client := AddClient(0, 0, Name, CurrentURL, True);
        Client.Entry.Settings.Filter := ufNone;
        Client.Entry.Settings.SaveToMemory := True;
        Client.Entry.Settings.SeparateTracks := True;
        Client.Entry.Settings.OnlySaveFull := False;
        Client.Entry.Settings.DeleteStreams := False;
        Client.Entry.Settings.MaxRetries := 0;
        Client.Entry.Settings.RetryDelay := 0;
        Client.Entry.Settings.AddSavedToIgnore := AppGlobals.AutoTuneInAddToIgnore;
        Client.Entry.Settings.RemoveSavedFromWishlist := AppGlobals.AutoRemoveSavedFromWishlist;
        Client.Entry.Settings.AddSavedToStreamIgnore := False;

        Client.Entry.Settings.SilenceLevel := 5;
        Client.Entry.Settings.SilenceLength := 100;
        Client.Entry.Settings.SongBuffer := 10000;
        Client.Entry.Settings.SilenceBufferSecondsStart := 15;
        Client.Entry.Settings.SilenceBufferSecondsEnd := 15;
        Client.Entry.Settings.AutoDetectSilenceLevel := True;

        Client.Entry.Bitrate := Kbps;
        if Trim(TitleRegEx) <> '' then
          Client.Entry.Settings.TitlePattern := TitleRegEx;
        Client.RecordTitle := Title;
        Client.RecordTitleHash := ServerHash;
        Client.RecordArtistHash := ServerArtistHash;
        Client.StartRecording(False);
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

function TClientManager.MatchesClient(Client: TICEClient; ID: Integer; Name, URL, Title: string;
  URLs: TStringList): Boolean;
var
  i, n: Integer;
begin
  if ID > 0 then
    if Client.Entry.ID = ID then
      Exit(True);

  if Name <> '' then
    if LowerCase(Client.Entry.Name) = LowerCase(Name) then
      Exit(True);

  if URL <> '' then
    if LowerCase(Client.Entry.StartURL) = LowerCase(URL) then
      Exit(True);

  if Title <> '' then
    if LowerCase(Client.Title) = LowerCase(Title) then
      Exit(True);

  if URLs <> nil then
    for i := 0 to Client.Entry.URLs.Count - 1 do
      for n := 0 to URLs.Count - 1 do
        if (LowerCase(Client.Entry.URLs[i]) = LowerCase(URL)) or
           (LowerCase(Client.Entry.URLs[i]) = LowerCase(URLs[n])) then
        begin
          Exit(True);
        end;
  Exit(False);
end;

function TClientManager.GetClient(ID: Integer; Name, URL, Title: string;
  URLs: TStringList): TICEClient;
var
  Client: TICEClient;
begin
  Name := Trim(Name);
  URL := Trim(URL);

  Result := nil;
  for Client in FClients do
  begin
    if MatchesClient(Client, ID, Name, URL, Title, URLs) then
      Exit(Client);
  end;
end;

procedure TClientManager.ClientSecondsReceived(Sender: TObject);
begin
  if Assigned(FOnClientSecondsReceived) then
    FOnClientSecondsReceived(Sender);
end;

procedure TClientManager.ClientSongSaved(Sender: TObject; Filename, Title, SongArtist, SongTitle: string;
  Filesize, Length, Bitrate: UInt64; VBR, WasCut, FullTitle, IsStreamFile: Boolean;
  ServerTitleHash, ServerArtistHash: Cardinal);
begin
  Inc(FSongsSaved);
  if not IsStreamFile then
    MsgBus.SendMessage(TSongSavedMsg.Create(Sender, ServerTitleHash, ServerArtistHash));
  if Assigned(FOnClientSongSaved) then
    FOnClientSongSaved(Sender, Filename, Title, SongArtist, SongTitle, Filesize, Length, Bitrate, VBR, WasCut, FullTitle,
      IsStreamFile, ServerTitleHash, ServerArtistHash);
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

  // Wenn der neue Client eine automatische Aufnahme ist raus hier!
  if Client.AutoRemove then
    Exit;

  for i := 0 to FClients.Count - 1 do
    if FClients[i] <> Client then
      for n := 0 to Client.Entry.URLs.Count - 1 do
        if (FClients[i].Entry.StartURL = Client.Entry.StartURL) or
           (FClients[i].Entry.URLs.IndexOf(Client.Entry.URLs[n]) > -1) then
        begin
          if not FClients[i].Killed then
          begin
            if Client.Playing then
              FClients[i].StartPlay(False);
            if Client.Recording then
              FClients[i].StartRecording(False);
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
  // wenn alle PostProcessors abgearbeitet wurden und es keinen ICE-Thread gibt.
  if Assigned(FOnClientRefresh) then
    FOnClientRefresh(Sender);
  Client := Sender as TICEClient;

  if Client.Killed and (not AppGlobals.PostProcessManager.WorkingForClient(Sender)) then
  begin
    if Assigned(FOnClientRemoved) then
      FOnClientRemoved(Sender);

    FClients.Remove(Client);
    FMonitorClients.Remove(Client);
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

procedure TClientManager.ClientPlaybackStarted(Sender: TObject);
begin
  if Assigned(FOnPlaybackStarted) then
    FOnPlaybackStarted(Sender);
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



