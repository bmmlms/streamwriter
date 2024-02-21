{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2024 Alexander Nottelmann

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
  AppData,
  AppMessages,
  AudioFunctions,
  Classes,
  DataManager,
  Functions,
  Generics.Collections,
  HomeCommunication,
  ICEClient,
  LanguageObjects,
  Logging,
  MessageBus,
  PlayerManager,
  Scheduler,
  SWFunctions,
  SysUtils,
  TypeDefs;

type
  TClientManager = class;

  TClientEnum = class
  private
    FIndex: Integer;
    FClientManager: TClientManager;
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
    FScheduler: TScheduler;
    FMonitorClients: TClientList;
    FSongsSaved: Integer;
    FNoFreeSpaceErrorShown: Boolean;
    FDirDoesNotExistErrorShown: Boolean;

    FOnClientLog: TNotifyEvent;
    FOnClientRefresh: TNotifyEvent;
    FOnClientAddRecent: TNotifyEvent;
    FOnClientAdded: TNotifyEvent;
    FOnClientRemoved: TNotifyEvent;
    FOnClientSongSaved: TSongSavedEvent;
    FOnClientTitleChanged: TTitleChangedEvent;
    FOnClientICYReceived: TICYReceivedEvent;
    FOnClientTitleAllowed: TTitleAllowedEvent;
    FOnShowErrorMessage: TStringEvent;
    FOnPlaybackStarted: TNotifyEvent;
    FOnClientSecondsReceived: TNotifyEvent;

    function FGetItem(Index: Integer): TICEClient;
    function FGetCount: Integer;

    function FGetActive: Boolean;

    procedure ClientLog(Sender: TObject);
    procedure ClientRefresh(Sender: TObject);
    procedure ClientAddRecent(Sender: TObject);
    procedure ClientSongSaved(Sender: TObject; Filename, Title, SongArtist, SongTitle: string; Filesize: Int64; Length, Bitrate: Cardinal; VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
      ServerTitleHash, ServerArtistHash: Cardinal);
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

    procedure SchedulerLog(Text, Data: string);
    procedure SchedulerSchedule(IsStart: Boolean; Schedule: TSchedule);

    procedure HomeCommTitleChanged(Sender: TObject; ID: Cardinal; Name, Title, ParsedTitle, CurrentURL: string; RegExes: TStringList; AudioType: TAudioTypes; Kbps: Cardinal; ServerHash, ServerArtistHash: Cardinal);
    procedure HomeCommMonitorStreamsReceived(Sender: TObject; StreamIDs: TIntArray);
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TClientEnum;

    function AddClient(URL: string): TICEClient; overload;
    function AddClient(ID, Bitrate: Cardinal; Name, StartURL: string; IsAuto: Boolean = False): TICEClient; overload;
    function AddClient(Entry: TStreamEntry): TICEClient; overload;
    function GetErrorText(Msg: TMayConnectResults; Data: string; WasAuto, WasScheduled, ForLog: Boolean): string;
    procedure RemoveClient(Client: TICEClient);
    procedure RemoveMonitorClient(Client: TICEClient);
    procedure Stop;
    procedure Terminate;

    property Monitors: TClientList read FMonitorClients;
    procedure StopMonitors;

    procedure SetupClient(Client: TICEClient);

    procedure RefreshScheduler;

    property Items[Index: Integer]: TICEClient read FGetItem; default;
    property Count: Integer read FGetCount;

    property Scheduler: TScheduler read FScheduler;

    function MatchesClient(Client: TICEClient; ID: Integer; Name, URL: string; URLs: TStringList): Boolean;
    function GetClient(ID: Integer; Name, URL: string; URLs: TStringList): TICEClient;
    function GetUsedBandwidth(Bitrate, Speed: Int64; ClientToAdd: TICEClient = nil): Integer;

    property Active: Boolean read FGetActive;
    property SongsSaved: Integer read FSongsSaved;

    property OnClientLog: TNotifyEvent read FOnClientLog write FOnClientLog;
    property OnClientRefresh: TNotifyEvent read FOnClientRefresh write FOnClientRefresh;
    property OnClientAddRecent: TNotifyEvent read FOnClientAddRecent write FOnClientAddRecent;
    property OnClientAdded: TNotifyEvent read FOnClientAdded write FOnClientAdded;
    property OnClientRemoved: TNotifyEvent read FOnClientRemoved write FOnClientRemoved;
    property OnClientSongSaved: TSongSavedEvent read FOnClientSongSaved write FOnClientSongSaved;
    property OnClientTitleChanged: TTitleChangedEvent read FOnClientTitleChanged write FOnClientTitleChanged;
    property OnClientICYReceived: TICYReceivedEvent read FOnClientICYReceived write FOnClientICYReceived;
    property OnClientTitleAllowed: TTitleAllowedEvent read FOnClientTitleAllowed write FOnClientTitleAllowed;
    property OnShowErrorMessage: TStringEvent read FOnShowErrorMessage write FOnShowErrorMessage;
    property OnPlaybackStarted: TNotifyEvent read FOnPlaybackStarted write FOnPlaybackStarted;
    property OnClientSecondsReceived: TNotifyEvent read FOnClientSecondsReceived write FOnClientSecondsReceived;
  end;

implementation

{ TClientManager }

function TClientManager.AddClient(URL: string): TICEClient;
var
  C: TICEClient;
begin
  {$IFNDEF NOSSL}
  if Copy(LowerCase(URL), 1, 23) = 'http://streamwriter.org' then
    URL := 'https://' + Copy(URL, 8, Length(URL));
  {$ENDIF}

  C := TICEClient.Create(Self, URL);
  SetupClient(C);
  Result := C;
end;

function TClientManager.AddClient(ID, Bitrate: Cardinal; Name, StartURL: string; IsAuto: Boolean = False): TICEClient;
var
  C: TICEClient;
begin
  {$IFNDEF NOSSL}
  if Copy(LowerCase(StartURL), 1, 23) = 'http://streamwriter.org' then
    StartURL := 'https://' + Copy(StartURL, 8, Length(StartURL));
  {$ENDIF}

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
  if Copy(LowerCase(Entry.StartURL), 1, 23) = 'http://streamwriter.org' then
    Entry.StartURL := 'https://' + Copy(Entry.StartURL, 8, Length(Entry.StartURL));

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
    if ClientToAdd = Client then
      if Client.Recording or Client.Playing then
      begin
        Result := 0;
        Exit;
      end;

  if UsedKBs = -1 then
    if (Bitrate = 0) and (Speed = 0) then
      UsedKBs := 18
    else if Bitrate > 0 then
      UsedKBs := Bitrate div 8
    else
      UsedKBs := Speed;

  for Client in FClients do
    if ClientToAdd <> Client then
      if Client.Recording or Client.Playing then
        if Client.Entry.Bitrate >= 64 then
          UsedKBs := UsedKBs + (Integer(Client.Entry.Bitrate) div 8) + 3
        else
          UsedKBs := UsedKBs + Client.Speed div 1024;

  Result := UsedKBs;
end;

procedure TClientManager.RefreshScheduler;
var
  i, n: Integer;
  Lst: TList<TSchedule>;
begin
  Scheduler.SetSchedules(nil);
  Lst := TList<TSchedule>.Create;
  try
    for i := 0 to FClients.Count - 1 do
      for n := 0 to FClients[i].Entry.Schedules.Count - 1 do
        Lst.Add(FClients[i].Entry.Schedules[n]);
    Scheduler.SetSchedules(Lst);
  finally
    Lst.Free;
  end;
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

procedure TClientManager.SchedulerLog(Text, Data: string);
begin
  MsgBus.SendMessage(TLogMsg.Create(nil, lsGeneral, ltSchedule, llDebug, _('Scheduler'), Text));
end;

procedure TClientManager.SchedulerSchedule(IsStart: Boolean; Schedule: TSchedule);
var
  i, n: Integer;
  Client: TICEClient;
  Res: TMayConnectResults;
begin
  Client := nil;
  for i := 0 to FClients.Count - 1 do
    for n := 0 to FClients[i].Entry.Schedules.Count - 1 do
      if FClients[i].Entry.Schedules[n] = Schedule then
      begin
        Client := FClients[i];
        Break;
      end;

  if Client = nil then
    Exit;

  if IsStart then
  begin
    if Client.Recording then
      Client.WriteLog(_('Skipping scheduled recording because recording is already active'), '', ltSchedule, llInfo)
    else
    begin
      Client.WriteLog(_('Starting scheduled recording'), ltSchedule, llInfo);

      Res := Client.StartRecording(True);
      if Res <> crOk then
      begin
        Client.WriteLog(GetErrorText(Res, Client.Entry.CustomName, False, True, True), ltSchedule, llWarning);
        FOnShowErrorMessage(Client, GetErrorText(Res, Client.Entry.CustomName, False, True, False));
      end else
      begin
        Client.ScheduledRecording := True;
        Client.WriteLog(Format(_('Scheduled recording ends at %s'), [TimeToStr(Schedule.GetEndTime(Schedule.GetStartTime(False)))]), ltSchedule, llInfo);
      end;
    end;
  end else if Client.ScheduledRecording then
  begin
    if Client.IsCurrentTimeInSchedule(Schedule) then
      Client.WriteLog(_('Not stopping scheduled recording because another schedule is active'), ltSchedule, llInfo)// Another schedule "surrounds" the triggered schedule, so do nothing
    else
    begin
      Client.WriteLog(_('Stopping scheduled recording'), ltSchedule, llInfo);
      Client.StopRecording;
    end;

    if Schedule.AutoRemove then
    begin
      Scheduler.SetSchedules(nil);

      Client.Entry.Schedules.Remove(Schedule);
      Schedule.Free;

      if Assigned(FOnClientRefresh) then
        FOnClientRefresh(Client);

      RefreshScheduler;
    end;
  end;
end;

procedure TClientManager.SetupClient(Client: TICEClient);
begin
  FClients.Add(Client);
  Client.OnLog := ClientLog;
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
  FScheduler.Stop;

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
  FOnClientLog := nil;
  FOnClientRefresh := nil;
  FOnClientAddRecent := nil;
  FOnClientAdded := nil;
  FOnClientRemoved := nil;
  FOnClientSongSaved := nil;
  FOnClientTitleChanged := nil;
  FOnClientICYReceived := nil;
  FOnClientTitleAllowed := nil;
  FOnShowErrorMessage := nil;
  FOnPlaybackStarted := nil;
  FOnClientSecondsReceived := nil;

  for i := Count - 1 downto 0 do
    RemoveClient(FClients[i]);
end;

constructor TClientManager.Create;
begin
  inherited Create;

  FSongsSaved := 0;
  FClients := TClientList.Create;
  FMonitorClients := TClientList.Create;
  HomeComm.OnNetworkTitleChangedReceived := HomeCommTitleChanged;
  HomeComm.OnMonitorStreamsReceived := HomeCommMonitorStreamsReceived;

  FScheduler := TScheduler.Create;
  FScheduler.OnLog := SchedulerLog;
  FScheduler.OnSchedule := SchedulerSchedule;
  FScheduler.Start;
end;

destructor TClientManager.Destroy;
begin
  FScheduler.Free;
  FClients.Free;
  FMonitorClients.Free;

  FOnClientLog := nil;
  FOnClientRefresh := nil;
  FOnClientAddRecent := nil;
  FOnClientAdded := nil;
  FOnClientRemoved := nil;
  FOnClientSongSaved := nil;
  FOnClientTitleChanged := nil;
  FOnClientICYReceived := nil;
  FOnClientTitleAllowed := nil;
  FOnShowErrorMessage := nil;
  FOnPlaybackStarted := nil;
  FOnClientSecondsReceived := nil;

  inherited;
end;

function TClientManager.FGetActive: Boolean;
var
  i: Integer;
begin
  if FScheduler.Active then
    Exit(True);

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

function TClientManager.GetErrorText(Msg: TMayConnectResults; Data: string; WasAuto, WasScheduled, ForLog: Boolean): string;
begin
  if WasAuto then
    case Msg of
      crNoFreeSpace:
        if ForLog then
          Result := Format(_('Automatic recording of "%s" won''t be started because available disk space is below the set limit'), [Data])
        else
          Result := _('Automatic recording will be stopped as long as available disk space is below the set limit.');
      crNoBandwidth:
        if ForLog then
          Result := Format(_('Automatic recording of "%s" won''t be started because it would exceed the maximum available bandwidth'), [Data]);
      crDirDoesNotExist:
        if ForLog then
          Result := Format(_('Automatic recording of "%s" won''t be started because the folder for automatic recordings does not exist'), [Data])
        else
          Result := _('Automatic recording will be stopped as long as the folder for automatically saved songs does not exist.');
    end else if WasScheduled then
    case Msg of
      crNoFreeSpace:
        if ForLog then
          Result := _('Scheduled recording will not start because available disk space is below the set limit')
        else
          Result := Format(_('Scheduled recording of "%s" will not start because available disk space is below the set limit.'), [Data]);
      crNoBandwidth:
        if ForLog then
          Result := _('Scheduled recording will not start because it would exceed the maximum available bandwidth')
        else
          Result := Format(_('Scheduled recording of "%s" will not start because it would exceed the maximum available bandwidth.'), [Data]);
      crDirDoesNotExist:
        if ForLog then
          Result := _('Scheduled recording will not start because the folder for saved songs does not exist')
        else
          Result := Format(_('Scheduled recording of "%s" will not start because the folder for saved songs does not exist.'), [Data]);
    end else
    case Msg of
      crNoFreeSpace:
      begin
        Result := _('No connection will be established because available disk space is below the set limit');
        if not ForLog then
          Result := Result + '.';
      end;
      crNoBandwidth:
      begin
        Result := _('No connection will be established because it would exceed the maximum available bandwidth');
        if not ForLog then
          Result := Result + '.';
      end;
      crDirDoesNotExist:
      begin
        Result := _('No connection will be established because the folder for saved songs does not exist');
        if not ForLog then
          Result := Result + '.';
      end;
    end;
end;

procedure TClientManager.HomeCommMonitorStreamsReceived(Sender: TObject; StreamIDs: TIntArray);
var
  i, n: Integer;
  Client: TICEClient;
begin
  for i := 0 to FMonitorClients.Count - 1 do
    RemoveClient(FMonitorClients[i]);

  StopMonitors;

  AppGlobals.Lock;
  try
    for i := 0 to High(StreamIDs) do
      for n := 0 to AppGlobals.Data.BrowserList.Count - 1 do
        if AppGlobals.Data.BrowserList[n].ID = StreamIDs[i] then
        begin
          Client := TICEClient.Create(Self, StreamIDs[i], 128, 'Monitor' + IntToStr(StreamIDs[i]), AppGlobals.Data.BrowserList[n].URL);
          Client.Entry.Settings.MaxRetries := 5;
          Client.Entry.Settings.RetryDelay := 30;
          Client.Entry.Settings.SaveToMemory := True;
          Client.OnDisconnected := ClientDisconnected;
          Client.StartMonitoring;

          FMonitorClients.Add(Client);

          Break;
        end;
  finally
    AppGlobals.Unlock;
  end;
end;

procedure TClientManager.HomeCommTitleChanged(Sender: TObject; ID: Cardinal; Name, Title, ParsedTitle, CurrentURL: string; RegExes: TStringList; AudioType: TAudioTypes; Kbps: Cardinal; ServerHash, ServerArtistHash: Cardinal);
var
  i, n: Integer;
  AutoTuneInMinKbps: Cardinal;
  Client: TICEClient;
  Res: TMayConnectResults;
  SaveListTitle: TTitleInfo;
  SaveListArtist: TTitleInfo;
  Text: string;
begin
  SaveListTitle := nil;
  SaveListArtist := nil;
  AutoTuneInMinKbps := GetAutoTuneInMinKbps(TAudioTypes(AudioType), AppGlobals.AutoTuneInMinQuality);

  // Das hier sind harte Abbruchbedingungen. Das muss nicht ins Log.
  for Client in Self.FClients do
    if MatchesClient(Client, ID, Name, CurrentURL, nil) then
      if (Client.AutoRemove and (Client.RecordTitle = Title)) or (Client.Recording) then
        Exit;

  for i := 0 to AppGlobals.Data.SaveList.Count - 1 do
  begin
    if (AppGlobals.Data.SaveList[i].ServerHash > 0) and (ServerHash = AppGlobals.Data.SaveList[i].ServerHash) then
      SaveListTitle := AppGlobals.Data.SaveList[i];
    if (AppGlobals.Data.SaveList[i].ServerHash = 0) and (AppGlobals.Data.SaveList[i].ServerArtistHash > 0) and (ServerArtistHash = AppGlobals.Data.SaveList[i].ServerArtistHash) then
      SaveListArtist := AppGlobals.Data.SaveList[i];
  end;

  if (not Assigned(SaveListTitle)) and (not Assigned(SaveListArtist)) then
    Exit;

  // Ab hier wird protokolliert
  MsgBus.SendMessage(TLogMsg.Create(Self, lsGeneral, ltGeneral, llDebug, _('Automatic recording'), Format('Title "%s" detected on "%s"', [ParsedTitle, Name])));

  Res := TICEClient.MayConnect(False, GetUsedBandwidth(Kbps, 0), True);
  if Res <> crOk then
  begin
    MsgBus.SendMessage(TLogMsg.Create(Self, lsGeneral, ltGeneral, llWarning, _('Automatic recording'), GetErrorText(Res, ParsedTitle, True, False, True)));
    if (Res = crDirDoesNotExist) and (not FDirDoesNotExistErrorShown) then
    begin
      OnShowErrorMessage(Self, GetErrorText(Res, ParsedTitle, True, False, False));
      FDirDoesNotExistErrorShown := True;
    end;
    if (Res = crNoFreeSpace) and (not FNoFreeSpaceErrorShown) then
    begin
      OnShowErrorMessage(Self, GetErrorText(Res, ParsedTitle, True, False, False));
      FNoFreeSpaceErrorShown := True;
    end;
    Exit;
  end;
  FDirDoesNotExistErrorShown := False;
  FNoFreeSpaceErrorShown := False;

  if Kbps < AutoTuneInMinKbps then
  begin
    Text := Format(_('Automatic recording of "%s" won''t be started because the bitrate is too low (%d Kbps)'), [ParsedTitle, Kbps]);
    MsgBus.SendMessage(TLogMsg.Create(Self, lsGeneral, ltGeneral, llWarning, _('Automatic recording'), Text));
    Exit;
  end;

  if (AppGlobals.AutoTuneInFormat > 0) and (TAudioTypes(AppGlobals.AutoTuneInFormat) <> AudioType) then
  begin
    Text := Format(_('Automatic recording of "%s" won''t be started because the audio format is not allowed'), [ParsedTitle]);
    MsgBus.SendMessage(TLogMsg.Create(Self, lsGeneral, ltGeneral, llWarning, _('Automatic recording'), Text));
    Exit;
  end;

  for i := 0 to AppGlobals.Data.StreamBlacklist.Count - 1 do
    if AppGlobals.Data.StreamBlacklist[i] = Name then
    begin
      Text := Format(_('Automatic recording of "%s" won''t be started because the stream is on the blacklist'), [ParsedTitle]);
      MsgBus.SendMessage(TLogMsg.Create(Self, lsGeneral, ltGeneral, llWarning, _('Automatic recording'), Text));
      Exit;
    end;

  if AppGlobals.AutoTuneInConsiderIgnore then
    for n := 0 to AppGlobals.Data.IgnoreList.Count - 1 do
      if TFunctions.Like(Title, AppGlobals.Data.IgnoreList[n].Pattern) then
      begin
        Text := Format(_('Automatic recording of "%s" won''t be started because it matches "%s" on the ignorelist'), [ParsedTitle, AppGlobals.Data.IgnoreList[n].Pattern]);
        MsgBus.SendMessage(TLogMsg.Create(Self, lsGeneral, ltGeneral, llWarning, _('Automatic recording'), Text));
        Exit;
      end;

  MsgBus.SendMessage(TLogMsg.Create(Self, lsGeneral, ltGeneral, llInfo, _('Automatic recording'), Format(_('Starting automatic recording of "%s"'), [ParsedTitle])));

  Client := AddClient(0, 0, Name, CurrentURL, True);
  Client.Entry.Settings.Assign(AppGlobals.Data.AutoRecordSettings);

  Client.Entry.Bitrate := Kbps;
  if RegExes <> nil then
    Client.Entry.Settings.RegExes.Assign(RegExes);

  Client.RecordTitle := Title;
  Client.ParsedRecordTitle := ParsedTitle;
  Client.RecordServerTitle := Title;
  Client.RecordTitleHash := ServerHash;
  Client.RecordArtistHash := ServerArtistHash;
  Client.RecordBecauseArtist := SaveListArtist <> nil;
  Client.StartRecording(False);
end;

procedure TClientManager.ClientLog(Sender: TObject);
begin
  if Assigned(FOnClientLog) then
    FOnClientLog(Sender);
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

function TClientManager.MatchesClient(Client: TICEClient; ID: Integer; Name, URL: string; URLs: TStringList): Boolean;
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

  if URLs <> nil then
    for i := 0 to Client.Entry.URLs.Count - 1 do
      for n := 0 to URLs.Count - 1 do
        if (LowerCase(Client.Entry.URLs[i]) = LowerCase(URL)) or (LowerCase(Client.Entry.URLs[i]) = LowerCase(URLs[n])) then
          Exit(True);
  Exit(False);
end;

function TClientManager.GetClient(ID: Integer; Name, URL: string; URLs: TStringList): TICEClient;
var
  Client: TICEClient;
begin
  Name := Trim(Name);
  URL := Trim(URL);

  Result := nil;
  for Client in FClients do
    if MatchesClient(Client, ID, Name, URL, URLs) then
      Exit(Client);
end;

procedure TClientManager.ClientSecondsReceived(Sender: TObject);
begin
  if Assigned(FOnClientSecondsReceived) then
    FOnClientSecondsReceived(Sender);
end;

procedure TClientManager.ClientSongSaved(Sender: TObject; Filename, Title, SongArtist, SongTitle: string; Filesize: Int64; Length, Bitrate: Cardinal; VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
  ServerTitleHash, ServerArtistHash: Cardinal);
begin
  if not IsStreamFile then
  begin
    MsgBus.SendMessage(TSongSavedMsg.Create(Sender, ServerTitleHash, ServerArtistHash));
    Inc(FSongsSaved);
    AppGlobals.Data.SongsSaved := AppGlobals.Data.SongsSaved + 1;
  end;
  if Assigned(FOnClientSongSaved) then
    FOnClientSongSaved(Sender, Filename, Title, SongArtist, SongTitle, Filesize, Length, Bitrate,
      VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist, ServerTitleHash, ServerArtistHash);
end;

procedure TClientManager.ClientStop(Sender: TObject);
begin
  if Players.LastPlayer = Sender then
    Players.LastPlayer := nil;
  if Assigned(FOnClientRefresh) then
    FOnClientRefresh(Sender);
end;

procedure TClientManager.ClientTitleChanged(Sender: TObject; Title: string);
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
        if (FClients[i].Entry.StartURL = Client.Entry.StartURL) or (FClients[i].Entry.URLs.IndexOf(Client.Entry.URLs[n]) > -1) then
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

procedure TClientManager.ClientTitleAllowed(Sender: TObject; Title: string; var Allowed: Boolean; var Match: string; var Filter: Integer);
begin
  if Assigned(FOnClientTitleAllowed) then
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

procedure TClientManager.ClientICYReceived(Sender: TObject; Bytes: Integer);
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
  FClientManager := Clients;
  FIndex := -1;
end;

function TClientEnum.GetCurrent: TICEClient;
begin
  Result := FClientManager[FIndex];
end;

function TClientEnum.MoveNext: Boolean;
begin
  Result := FIndex < Pred(FClientManager.Count);
  if Result then
    Inc(FIndex);
end;

end.
