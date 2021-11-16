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

unit HomeCommunication;

interface

uses
  AppData,
  AppMessages,
  AudioFunctions,
  Classes,
  Commands,
  Communication,
  DataManager,
  DateUtils,
  ExtendedStream,
  Functions,
  Generics.Collections,
  HomeCommands,
  LanguageObjects,
  MessageBus,
  Protocol,
  Sockets,
  SysUtils,
  TypeDefs;

type
  TCommErrors = (ceUnknown, ceAuthRequired, ceNotification, ceOneTimeNotification);

  THomeThread = class(TCommandThreadBase)
  private
    FAuthenticated: Boolean;
    FIsAdmin: Boolean;

    FHandshakeSuccess: Boolean;
    FServerTime: Cardinal;

    FNetworkTitleChanged: TCommandNetworkTitleChangedResponse;

    FServerInfoClientCount: Cardinal;
    FServerInfoRecordingCount: Cardinal;

    FErrorID: TCommErrors;
    FErrorMsg: string;

    FStreamIDs: TIntArray;

    FSearchReceivedChartsSuccess: Boolean;
    FSearchReceivedCharts: TChartList;

    FConvertManualToAutomaticFoundTitles: TConvertManualToAutomaticArray;
    FConvertManualToAutomaticNotFoundTitles: TStringArray;

    FGetStreamDataLastTitles: TStringArray;
    FGetStreamDataOtherUserRegExps: TStringArray;
    FGetStreamDataUserRegExps: TStringArray;

    FOnHandshakeReceived: TSocketEvent;
    FOnLogInReceived: TSocketEvent;
    FOnLogOutReceived: TSocketEvent;
    FOnServerDataReceived: TSocketEvent;
    FOnNetworkTitleChangedReceived: TSocketEvent;
    FOnServerInfoReceived: TSocketEvent;
    FOnErrorReceived: TSocketEvent;
    FOnMonitorStreamsReceived: TSocketEvent;
    FOnSearchChartsReceived: TSocketEvent;
    FOnWishlistUpgradeReceived: TSocketEvent;
    FOnConvertManualToAutomaticReceived: TSocketEvent;
    FOnGetStreamDataReceived: TSocketEvent;

    procedure DoHandshakeReceived(CommandHeader: TCommandHeader; Command: TCommandHandshakeResponse);
    procedure DoLogInReceived(CommandHeader: TCommandHeader; Command: TCommandLogInResponse);
    procedure DoLogOutReceived(CommandHeader: TCommandHeader; Command: TCommandLogOutResponse);
    procedure DoServerDataReceived(CommandHeader: TCommandHeader; Command: TCommandGetServerDataResponse);
    procedure DoNetworkTitleChanged(CommandHeader: TCommandHeader; Command: TCommandNetworkTitleChangedResponse);
    procedure DoServerInfoReceived(CommandHeader: TCommandHeader; Command: TCommandServerInfoResponse);
    procedure DoMessageReceived(CommandHeader: TCommandHeader; Command: TCommandMessageResponse);
    procedure DoMonitorStreamsReceived(CommandHeader: TCommandHeader; Command: TCommandGetMonitorStreamsResponse);
    procedure DoSearchChartsReceived(CommandHeader: TCommandHeader; Command: TCommandSearchChartsResponse);
    procedure DoConvertManualToAutomaticReceived(CommandHeader: TCommandHeader; Command: TCommandConvertManualToAutomaticResponse);
    procedure DoGetStreamDataReceived(CommandHeader: TCommandHeader; Command: TCommandGetStreamDataResponse);
  protected
    procedure DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand); override;
    procedure DoException(E: Exception); override;
    procedure DoEnded; override;
    procedure DoStuff; override;
    procedure DoConnected; override;
    procedure DoSecured; override;
  public
    constructor Create;
    destructor Destroy; override;

    property OnHandshakeReceived: TSocketEvent read FOnHandshakeReceived write FOnHandshakeReceived;
    property OnLogInReceived: TSocketEvent read FOnLogInReceived write FOnLogInReceived;
    property OnLogOutReceived: TSocketEvent read FOnLogOutReceived write FOnLogOutReceived;
    property OnServerDataReceived: TSocketEvent read FOnServerDataReceived write FOnServerDataReceived;
    property OnNetworkTitleChangedReceived: TSocketEvent read FOnNetworkTitleChangedReceived write FOnNetworkTitleChangedReceived;
    property OnServerInfoReceived: TSocketEvent read FOnServerInfoReceived write FOnServerInfoReceived;
    property OnErrorReceived: TSocketEvent read FOnErrorReceived write FOnErrorReceived;
    property OnMonitorStreamsReceived: TSocketEvent read FOnMonitorStreamsReceived write FOnMonitorStreamsReceived;
    property OnSearchChartsReceived: TSocketEvent read FOnSearchChartsReceived write FOnSearchChartsReceived;
    property OnWishlistUpgradeReceived: TSocketEvent read FOnWishlistUpgradeReceived write FOnWishlistUpgradeReceived;
    property OnConvertManualToAutomaticReceived: TSocketEvent read FOnConvertManualToAutomaticReceived write FOnConvertManualToAutomaticReceived;
    property OnGetStreamDataReceived: TSocketEvent read FOnGetStreamDataReceived write FOnGetStreamDataReceived;
  end;

  TBooleanEvent = procedure(Sender: TObject; Value: Boolean) of object;
  TStreamsReceivedEvent = procedure(Sender: TObject) of object;
  TChartsReceivedEvent = procedure(Sender: TObject; Success: Boolean; Charts: TChartList) of object;
  TTitleChangedEvent = procedure(Sender: TObject; ID: Cardinal; Name, Title, ParsedTitle, CurrentURL: string; RegExes: TStringList; Format: TAudioTypes; Kbps: Cardinal; ServerHash, ServerArtistHash: Cardinal) of object;
  TServerInfoEvent = procedure(Sender: TObject; ClientCount, RecordingCount: Cardinal) of object;
  TErrorEvent = procedure(Sender: TObject; ID: TCommErrors; Msg: string) of object;
  TIntArrayEvent = procedure(Sender: TObject; IntArr: TIntArray) of object;
  TConvertManualToAutomaticEvent = procedure(Sender: TObject; FoundTitles: TConvertManualToAutomaticArray; NotFoundTitles: TStringArray) of object;
  TGetStreamDataEvent = procedure(Sender: TObject; LastTitles: TStringArray; OtherUserRegExps: TStringArray; UserRegExps: TStringArray) of object;
  TCardinalEvent = procedure(Sender: TObject; Data: Cardinal) of object;

  THomeCommunication = class
  private
    FDisabled: Boolean;
    FThread: THomeThread;
    FRaisedException: Exception;

    FAuthenticated, FIsAdmin, FWasConnected, FConnected, FNotifyTitleChanges, FSecured, FCommunicationEstablished: Boolean;
    FTitleNotificationsSet: Boolean;

    FOnStateChanged: TNotifyEvent;
    FOnTitleNotificationsChanged: TNotifyEvent;
    FOnBytesTransferred: TTransferProgressEvent;

    FOnHandshakeReceived: TBooleanEvent;
    FOnLogInReceived: TBooleanEvent;
    FOnLogOutReceived: TNotifyEvent;
    FOnServerDataReceived: TNotifyEvent;
    FOnNetworkTitleChangedReceived: TTitleChangedEvent;
    FOnServerInfoReceived: TServerInfoEvent;
    FOnErrorReceived: TErrorEvent;
    FOnMonitorStreamsReceived: TIntArrayEvent;
    FOnSearchChartsReceived: TChartsReceivedEvent;
    FOnConvertManualToAutomaticReceived: TConvertManualToAutomaticEvent;
    FOnGetStreamDataReceived: TGetStreamDataEvent;
    FOnException: TNotifyEvent;

    function FGetThreadAlive: Boolean;

    procedure HomeThreadConnected(Sender: TSocketThread);
    procedure HomeThreadBeforeEnded(Sender: TSocketThread);
    procedure HomeThreadEnded(Sender: TSocketThread);
    procedure HomeThreadBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader; Transferred: UInt64);

    procedure HomeThreadHandshakeReceived(Sender: TSocketThread);
    procedure HomeThreadLogInReceived(Sender: TSocketThread);
    procedure HomeThreadLogOutReceived(Sender: TSocketThread);
    procedure HomeThreadServerDataReceived(Sender: TSocketThread);
    procedure HomeThreadNetworkTitleChangedReceived(Sender: TSocketThread);
    procedure HomeThreadServerInfoReceived(Sender: TSocketThread);
    procedure HomeThreadErrorReceived(Sender: TSocketThread);
    procedure HomeThreadMonitorStreamsReceived(Sender: TSocketThread);
    procedure HomeThreadSearchChartsReceived(Sender: TSocketThread);
    procedure HomeThreadConvertManualToAutomaticReceived(Sender: TSocketThread);
    procedure HomeThreadGetStreamDataReceived(Sender: TSocketThread);
    procedure HomeThreadLog(Sender: TSocketThread);
    procedure HomeThreadSecured(Sender: TSocketThread);
    procedure HomeThreadCommunicationEstablished(Sender: TSocketThread);
    procedure HomeThreadException(Sender: TSocketThread);

    procedure HomeThreadTerminate(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Terminate;
    function SendCommand(Cmd: TCommand): Boolean;
    procedure SendHandshake;
    procedure SendLogIn(User, Pass: string);
    procedure SendLogOut;
    function SendGetServerData: Boolean;
    procedure SendUpdateStats(List: TList<Cardinal>; RecordingCount: Cardinal);
    procedure SendSetSettings(TitleNotifications: Boolean);
    procedure SendClientStats(Auto: Boolean);
    procedure SendSubmitStream(URL, StreamName: string);
    function SendSetStreamData(StreamID: Cardinal; Rating: Byte): Boolean; overload;
    function SendSetStreamData(StreamID: Cardinal; RegExps: TStringArray): Boolean; overload;
    procedure SendTitleChanged(StreamID: Cardinal; StreamName, Title, CurrentURL, URL: string; Format: TAudioTypes; Kbps: Cardinal; URLs: TStringList);
    procedure SendGetMonitorStreams(Count: Cardinal);
    procedure SendSyncWishlist; overload;
    procedure SendSyncWishlist(SyncType: TSyncWishlistTypes; Hashes: TSyncWishlistRecordArray); overload;
    procedure SendSyncWishlist(SyncType: TSyncWishlistTypes; Hash: Cardinal; IsArtist: Boolean); overload;
    procedure SendSearchCharts(Top: Boolean; Term: string);
    procedure SendStreamAnalyzationData(StreamID: Cardinal; Data: TExtendedStream);
    procedure SendConvertManualToAutomatic(Titles: TStringList);
    procedure SendGetStreamData(StreamID: Integer);

    property Disabled: Boolean read FDisabled write FDisabled;
    property WasConnected: Boolean read FWasConnected;
    property Connected: Boolean read FConnected;
    property Authenticated: Boolean read FAuthenticated;
    property NotifyTitleChanges: Boolean read FNotifyTitleChanges;
    property Secured: Boolean read FSecured;
    property IsAdmin: Boolean read FIsAdmin;
    property ThreadAlive: Boolean read FGetThreadAlive;
    property RaisedException: Exception read FRaisedException;
    property CommunicationEstablished: Boolean read FCommunicationEstablished;

    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnTitleNotificationsChanged: TNotifyEvent read FOnTitleNotificationsChanged write FOnTitleNotificationsChanged;
    property OnBytesTransferred: TTransferProgressEvent read FOnBytesTransferred write FOnBytesTransferred;

    property OnHandshakeReceived: TBooleanEvent read FOnHandshakeReceived write FOnHandshakeReceived;
    property OnLogInReceived: TBooleanEvent read FOnLogInReceived write FOnLogInReceived;
    property OnLogOutReceived: TNotifyEvent read FOnLogOutReceived write FOnLogOutReceived;
    property OnServerDataReceived: TNotifyEvent read FOnServerDataReceived write FOnServerDataReceived;
    property OnNetworkTitleChangedReceived: TTitleChangedEvent read FOnNetworkTitleChangedReceived write FOnNetworkTitleChangedReceived;
    property OnServerInfoReceived: TServerInfoEvent read FOnServerInfoReceived write FOnServerInfoReceived;
    property OnErrorReceived: TErrorEvent read FOnErrorReceived write FOnErrorReceived;
    property OnMonitorStreamsReceived: TIntArrayEvent read FOnMonitorStreamsReceived write FOnMonitorStreamsReceived;
    property OnSearchChartsReceived: TChartsReceivedEvent read FOnSearchChartsReceived write FOnSearchChartsReceived;
    property OnConvertManualToAutomaticReceived: TConvertManualToAutomaticEvent read FOnConvertManualToAutomaticReceived write FOnConvertManualToAutomaticReceived;
    property OnGetStreamDataReceived: TGetStreamDataEvent read FOnGetStreamDataReceived write FOnGetStreamDataReceived;
    property OnException: TNotifyEvent read FOnException write FOnException;
  end;

var
  HomeComm: THomeCommunication;

implementation

{ THomeThread }

constructor THomeThread.Create;
begin
  // Wenn für 30 Sekunden nichts kommt ist Feierabend, das Timeout wird vom Server in HandshakeReceived überschrieben.
  FDataTimeout := 30000;

  {$IFDEF NOSSL}
  inherited Create('streamwriter.org', 7085, TSocketStream.Create, False, AppGlobals.CheckCertificate);
  {$ELSE}
  inherited Create('streamwriter.org', 7086, TSocketStream.Create, True, AppGlobals.CheckCertificate);
  {$ENDIF}

  UseSynchronize := True;
end;

destructor THomeThread.Destroy;
begin

  inherited;
end;

procedure THomeThread.DoConnected;
begin
  inherited;

end;

procedure THomeThread.DoEnded;
begin
  inherited;

  if not Terminated then
    Sleep(5000);
end;

procedure THomeThread.DoMessageReceived(CommandHeader: TCommandHeader; Command: TCommandMessageResponse);
begin
  FErrorID := TCommErrors(Command.MessageID);
  FErrorMsg := Command.MessageMsg;

  if Assigned(FOnErrorReceived) then
    Sync(FOnErrorReceived);
end;

procedure THomeThread.DoMonitorStreamsReceived(CommandHeader: TCommandHeader; Command: TCommandGetMonitorStreamsResponse);
begin
  FStreamIDs := Command.StreamIDs;

  if Assigned(FOnMonitorStreamsReceived) then
    Sync(FOnMonitorStreamsReceived);
end;

procedure THomeThread.DoException(E: Exception);
begin
  if E.ClassType = EExceptionParams then
    WriteLog(Format(_(E.Message), EExceptionParams(E).Args), slError)
  else if E.Message <> '' then
    WriteLog(Format(_('%s'), [_(E.Message)]), slError);

  inherited;
end;

procedure THomeThread.DoGetStreamDataReceived(CommandHeader: TCommandHeader; Command: TCommandGetStreamDataResponse);
begin
  FGetStreamDataLastTitles := Command.LastTitles;
  FGetStreamDataOtherUserRegExps := Command.OtherUserRegExps;
  FGetStreamDataUserRegExps := Command.UserRegExps;

  if Assigned(FOnGetStreamDataReceived) then
    Sync(FOnGetStreamDataReceived);
end;

procedure THomeThread.DoHandshakeReceived(CommandHeader: TCommandHeader; Command: TCommandHandshakeResponse);
begin
  FHandshakeSuccess := Command.Success;
  FServerTime := Command.ServerTime;
  FDataTimeout := Command.CommunicationTimeout;

  if Assigned(FOnHandshakeReceived) then
    Sync(FOnHandshakeReceived);
end;

procedure THomeThread.DoLogInReceived(CommandHeader: TCommandHeader; Command: TCommandLogInResponse);
begin
  FAuthenticated := Command.Success;
  FIsAdmin := Command.IsAdmin;

  if Assigned(FOnLogInReceived) then
    Sync(FOnLogInReceived);
end;

procedure THomeThread.DoLogOutReceived(CommandHeader: TCommandHeader; Command: TCommandLogOutResponse);
begin
  FAuthenticated := False;

  if Assigned(FOnLogOutReceived) then
    Sync(FOnLogOutReceived);
end;

procedure THomeThread.DoNetworkTitleChanged(CommandHeader: TCommandHeader; Command: TCommandNetworkTitleChangedResponse);
begin
  AppGlobals.Lock;
  try
    if not AppGlobals.AutoTuneIn then
      Exit;
  finally
    AppGlobals.Unlock;
  end;

  FNetworkTitleChanged := Command;

  if (FNetworkTitleChanged.StreamName <> '') and (FNetworkTitleChanged.StreamTitle <> '') and (FNetworkTitleChanged.CurrentURL <> '') then
    if Assigned(FOnNetworkTitleChangedReceived) then
      Sync(FOnNetworkTitleChangedReceived);
end;

procedure THomeThread.DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand);
var
  HandShake: TCommandHandshakeResponse absolute Command;
  LogIn: TCommandLogInResponse absolute Command;
  LogOut: TCommandLogOutResponse absolute Command;
  GetServerData: TCommandGetServerDataResponse absolute Command;
  NetworkTitleChanged: TCommandNetworkTitleChangedResponse absolute Command;
  ServerInfo: TCommandServerInfoResponse absolute Command;
  Error: TCommandMessageResponse absolute Command;
  MonitorStreams: TCommandGetMonitorStreamsResponse absolute Command;
  SearchCharts: TCommandSearchChartsResponse absolute Command;
  ConvertManualToAutomatic: TCommandConvertManualToAutomaticResponse absolute Command;
  GetStreamData: TCommandGetStreamDataResponse absolute Command;
begin
  inherited;

  case CommandHeader.CommandType of
    ctHandshakeResponse:
      DoHandshakeReceived(CommandHeader, HandShake);
    ctLoginResponse:
      DoLogInReceived(CommandHeader, LogIn);
    ctLogoutResponse:
      DoLogOutReceived(CommandHeader, LogOut);
    ctGetServerDataResponse:
      DoServerDataReceived(CommandHeader, GetServerData);
    ctNetworkTitleChangedResponse:
      DoNetworkTitleChanged(CommandHeader, NetworkTitleChanged);
    ctServerInfoResponse:
      DoServerInfoReceived(CommandHeader, ServerInfo);
    ctMessageResponse:
      DoMessageReceived(CommandHeader, Error);
    ctGetMonitorStreamsResponse:
      DoMonitorStreamsReceived(CommandHeader, MonitorStreams);
    ctSearchChartsResponse:
      DoSearchChartsReceived(CommandHeader, SearchCharts);
    ctConvertManualToAutomaticResponse:
      DoConvertManualToAutomaticReceived(CommandHeader, ConvertManualToAutomatic);
    ctGetStreamDataResponse:
      DoGetStreamDataReceived(CommandHeader, GetStreamData);
  end;
end;

procedure THomeThread.DoSearchChartsReceived(CommandHeader: TCommandHeader; Command: TCommandSearchChartsResponse);
var
  i: Integer;
  Count: Cardinal;
  Stream: TExtendedStream;
begin
  Stream := TExtendedStream(Command.Stream);

  FSearchReceivedChartsSuccess := Command.Success;
  FSearchReceivedCharts := nil;

  if Command.Success then
  begin
    FSearchReceivedCharts := TChartList.Create;
    try
      // Charts laden
      Stream.Read(Count);
      for i := 0 to Count - 1 do
        FSearchReceivedCharts.Add(TChartEntry.LoadFromHome(Stream, CommandHeader.Version));
    except
      for i := 0 to FSearchReceivedCharts.Count - 1 do
        FSearchReceivedCharts[i].Free;
      FSearchReceivedCharts.Free;
      FSearchReceivedCharts := nil;

      FSearchReceivedChartsSuccess := False;
    end;
  end;

  if Assigned(FOnSearchChartsReceived) then
    Sync(FOnSearchChartsReceived);
end;

procedure THomeThread.DoSecured;
begin
  inherited;

end;

procedure THomeThread.DoServerDataReceived(CommandHeader: TCommandHeader; Command: TCommandGetServerDataResponse);
var
  i: Integer;
  Count: Cardinal;
  Stream: TExtendedStream;
  StreamEntry, StreamEntry2: TStreamBrowserEntry;
  Genre: TGenre;
  Genres: TGenreList;
  Streams: TStreamBrowserList;
begin
  Stream := TExtendedStream(Command.Stream);

  Genres := TGenreList.Create;
  Streams := TStreamBrowserList.Create;
  try
    // Genres laden
    Stream.Read(Count);
    for i := 0 to Count - 1 do
      Genres.Add(TGenre.LoadFromHome(Stream, CommandHeader.Version));

    AppGlobals.Lock;
    try
      // Streams laden und OwnRating synchronisieren
      Stream.Read(Count);
      for i := 0 to Count - 1 do
      begin
        StreamEntry := TStreamBrowserEntry.LoadFromHome(Stream, CommandHeader.Version);

        for StreamEntry2 in AppGlobals.Data.BrowserList do
          if StreamEntry.ID = StreamEntry2.ID then
          begin
            StreamEntry.OwnRating := StreamEntry2.OwnRating;
            Break;
          end;
        Streams.Add(StreamEntry);
      end;

      // Wenn alles erfolgreich geladen wurde alte Listen leeren.
      // Falls hier jetzt eine Exception kommt wird es bitter...
      for Genre in AppGlobals.Data.GenreList do
        Genre.Free;
      AppGlobals.Data.GenreList.Clear;
      for StreamEntry in AppGlobals.Data.BrowserList do
        StreamEntry.Free;
      AppGlobals.Data.BrowserList.Clear;

      // Der Liste alle Sachen wieder hinzufügen
      try
        for Genre in Genres do
          AppGlobals.Data.GenreList.Add(Genre);
      except
        for i := 0 to Genres.Count - 1 do
          Genres[i].Free;
        AppGlobals.Data.GenreList.Clear;
      end;

      try
        for StreamEntry in Streams do
          AppGlobals.Data.BrowserList.Add(StreamEntry);
      except
        for i := 0 to Streams.Count - 1 do
          Streams[i].Free;
        AppGlobals.Data.BrowserList.Clear;
      end;
    finally
      AppGlobals.Unlock;
    end;
  finally
    Genres.Free;
    Streams.Free;
  end;

  if Assigned(FOnServerDataReceived) then
    Sync(FOnServerDataReceived);
end;

procedure THomeThread.DoServerInfoReceived(CommandHeader: TCommandHeader; Command: TCommandServerInfoResponse);
begin
  FServerInfoClientCount := Command.ClientCount;
  FServerInfoRecordingCount := Command.RecordingCount;

  if Assigned(FOnServerInfoReceived) then
    Sync(FOnServerInfoReceived);
end;

procedure THomeThread.DoConvertManualToAutomaticReceived(CommandHeader: TCommandHeader; Command: TCommandConvertManualToAutomaticResponse);
begin
  FConvertManualToAutomaticFoundTitles := Command.FoundTitles;
  FConvertManualToAutomaticNotFoundTitles := Command.NotFoundTitles;

  if Assigned(FOnConvertManualToAutomaticReceived) then
    Sync(FOnConvertManualToAutomaticReceived);
end;

procedure THomeThread.DoStuff;
begin
  inherited;

end;

{ THomeCommunication }

procedure THomeCommunication.HomeThreadBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Sender, Direction, CommandID, CommandHeader, Transferred);
end;

constructor THomeCommunication.Create;
begin
  inherited Create;

  FTitleNotificationsSet := False;
end;

destructor THomeCommunication.Destroy;
begin

  inherited;
end;

function THomeCommunication.FGetThreadAlive: Boolean;
begin
  Result := FThread <> nil;
end;

procedure THomeCommunication.SendLogIn(User, Pass: string);
begin
  if not FCommunicationEstablished then
    Exit;

  FThread.SendCommand(TCommandLogIn.Create(User, Pass));
end;

procedure THomeCommunication.SendLogOut;
begin
  if not FCommunicationEstablished then
    Exit;

  FThread.SendCommand(TCommandLogOut.Create);
end;

procedure THomeCommunication.SendSearchCharts(Top: Boolean; Term: string);
begin
  if not FCommunicationEstablished then
    Exit;

  FThread.SendCommand(TCommandSearchCharts.Create(Top, Term));
end;

procedure THomeCommunication.SendSetSettings(TitleNotifications: Boolean);
begin
  if not FCommunicationEstablished then
    Exit;

  FNotifyTitleChanges := TitleNotifications;

  FThread.SendCommand(TCommandSetSettings.Create(TitleNotifications));

  if Assigned(FOnTitleNotificationsChanged) then
    FOnTitleNotificationsChanged(Self);
end;

function THomeCommunication.SendSetStreamData(StreamID: Cardinal; Rating: Byte): Boolean;
var
  Cmd: TCommandSetStreamData;
begin
  Result := True;
  if (not FCommunicationEstablished) or (not FAuthenticated) then
    Exit(False);

  Cmd := TCommandSetStreamData.Create;
  Cmd.StreamID := StreamID;
  Cmd.Rating := Rating;
  Cmd.SetRegExps := False;

  FThread.SendCommand(Cmd);
end;

function THomeCommunication.SendSetStreamData(StreamID: Cardinal; RegExps: TStringArray): Boolean;
var
  Cmd: TCommandSetStreamData;
begin
  Result := True;
  if (not FCommunicationEstablished) or (not FAuthenticated) then
    Exit(False);

  Cmd := TCommandSetStreamData.Create;
  Cmd.StreamID := StreamID;
  Cmd.Rating := 0;
  Cmd.SetRegExps := True;
  Cmd.RegExps := RegExps;

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.SendStreamAnalyzationData(StreamID: Cardinal; Data: TExtendedStream);
begin
  if not FCommunicationEstablished then
    Exit;

  FThread.SendCommand(TCommandStreamAnalyzationData.Create(StreamID, Data));
end;

procedure THomeCommunication.SendSubmitStream(URL, StreamName: string);
begin
  if not FCommunicationEstablished then
    Exit;

  FThread.SendCommand(TCommandSubmitStream.Create(URL, StreamName));
end;

procedure THomeCommunication.SendSyncWishlist;
var
  i: Integer;
  ItemsFound: Integer;
  Hashes: TSyncWishlistRecordArray;
begin
  if not FCommunicationEstablished then
    Exit;

  SetLength(Hashes, AppGlobals.Data.SaveList.Count);
  ItemsFound := 0;
  for i := 0 to AppGlobals.Data.SaveList.Count - 1 do
    if AppGlobals.Data.SaveList[i].ServerHash > 0 then
    begin
      Hashes[ItemsFound] := TSyncWishlistRecord.Create(AppGlobals.Data.SaveList[i].ServerHash, False);
      Inc(ItemsFound);
    end else if AppGlobals.Data.SaveList[i].ServerArtistHash > 0 then
    begin
      Hashes[ItemsFound] := TSyncWishlistRecord.Create(AppGlobals.Data.SaveList[i].ServerArtistHash, True);
      Inc(ItemsFound);
    end;
  SetLength(Hashes, ItemsFound);

  SendSyncWishlist(swSync, Hashes);
end;

procedure THomeCommunication.SendSyncWishlist(SyncType: TSyncWishlistTypes; Hashes: TSyncWishlistRecordArray);
begin
  if not FCommunicationEstablished then
    Exit;

  if Length(Hashes) = 0 then
    Exit;

  FThread.SendCommand(TCommandSyncWishlist.Create(SyncType, Hashes));
end;

procedure THomeCommunication.SendSyncWishlist(SyncType: TSyncWishlistTypes; Hash: Cardinal; IsArtist: Boolean);
var
  Hashes: TSyncWishlistRecordArray;
begin
  if (not FCommunicationEstablished) or (Hash = 0) or (SyncType = swSync) then
    Exit;

  SetLength(Hashes, 1);
  Hashes[0] := TSyncWishlistRecord.Create(Hash, IsArtist);

  SendSyncWishlist(SyncType, Hashes);
end;

procedure THomeCommunication.SendTitleChanged(StreamID: Cardinal; StreamName, Title, CurrentURL, URL: string; Format: TAudioTypes; Kbps: Cardinal; URLs: TStringList);
begin
  if not FCommunicationEstablished then
    Exit;

  if (StreamID = 0) or (Trim(StreamName) = '') or (Trim(URL) = '') or (Length(Title) <= 3) then
    Exit;

  FThread.SendCommand(TCommandTitleChanged.Create(StreamID, StreamName, Title, CurrentURL, URL, Format, Kbps, URLs.Text));
end;

procedure THomeCommunication.SendUpdateStats(List: TList<Cardinal>; RecordingCount: Cardinal);
var
  i: Integer;
  Cmd: TCommandUpdateStats;
  Stream: TExtendedStream;
begin
  if not FCommunicationEstablished then
    Exit;

  Cmd := TCommandUpdateStats.Create;
  Stream := TExtendedStream(Cmd.Stream);

  Stream.Write(Cardinal(List.Count));
  for i := 0 to List.Count - 1 do
    Stream.Write(List[i]);

  Stream.Write(RecordingCount);

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.Terminate;
begin
  FOnStateChanged := nil;
  FOnTitleNotificationsChanged := nil;
  FOnBytesTransferred := nil;
  FOnHandshakeReceived := nil;
  FOnLogInReceived := nil;
  FOnLogOutReceived := nil;
  FOnNetworkTitleChangedReceived := nil;
  FOnErrorReceived := nil;
  FOnMonitorStreamsReceived := nil;
  FOnSearchChartsReceived := nil;
  FOnServerDataReceived := nil;
  FOnServerInfoReceived := nil;
  FOnConvertManualToAutomaticReceived := nil;
  FOnGetStreamDataReceived := nil;
  FOnException := nil;

  if FThread <> nil then
    FThread.Terminate;
end;

procedure THomeCommunication.SendClientStats(Auto: Boolean);
begin
  if not FCommunicationEstablished then
    Exit;

  if Auto then
    FThread.SendCommand(TCommandClientStats.Create(csAutoSave))
  else
    FThread.SendCommand(TCommandClientStats.Create(csSave));
end;

function THomeCommunication.SendCommand(Cmd: TCommand): Boolean;
begin
  Result := True;
  if not FCommunicationEstablished then
    Exit(False);

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.SendConvertManualToAutomatic(Titles: TStringList);
var
  Arr: TStringArray;
  i: Integer;
begin
  if not FCommunicationEstablished then
    Exit;

  if Titles.Count = 0 then
    Exit;

  SetLength(Arr, Titles.Count);
  for i := 0 to Titles.Count - 1 do
    Arr[i] := Titles[i];

  FThread.SendCommand(TCommandConvertManualToAutomatic.Create(Arr));
end;

procedure THomeCommunication.SendGetMonitorStreams(Count: Cardinal);
begin
  if not FCommunicationEstablished then
    Exit;

  FThread.SendCommand(TCommandGetMonitorStreams.Create(Count));
end;

function THomeCommunication.SendGetServerData: Boolean;
begin
  Result := True;
  if not FCommunicationEstablished then
    Exit(False);

  FThread.SendCommand(TCommandGetServerData.Create);
end;

procedure THomeCommunication.SendGetStreamData(StreamID: Integer);
begin
  if not FCommunicationEstablished then
    Exit;

  FThread.SendCommand(TCommandGetStreamData.Create(StreamID));
end;

procedure THomeCommunication.HomeThreadCommunicationEstablished(Sender: TSocketThread);
begin
  SendHandshake;
end;

procedure THomeCommunication.HomeThreadConnected(Sender: TSocketThread);
begin
  inherited;

  MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llInfo, _('Server'), _('Connected to streamWriter server')));

  FConnected := True;
end;

procedure THomeCommunication.HomeThreadConvertManualToAutomaticReceived(Sender: TSocketThread);
begin
  if Assigned(FOnConvertManualToAutomaticReceived) then
    FOnConvertManualToAutomaticReceived(Self, THomeThread(Sender).FConvertManualToAutomaticFoundTitles, THomeThread(Sender).FConvertManualToAutomaticNotFoundTitles);
end;

procedure THomeCommunication.HomeThreadBeforeEnded(Sender: TSocketThread);
begin
  if FConnected then
    MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llWarning, _('Server'), _('Disconnected from streamWriter server')));

  FConnected := False;
  FAuthenticated := False;
  FIsAdmin := False;
  FTitleNotificationsSet := False;
  FSecured := False;
  FCommunicationEstablished := False;
  //FThread := nil;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure THomeCommunication.HomeThreadEnded(Sender: TSocketThread);
begin
  if THomeThread(Sender).Terminated then
    Exit;

  Connect;
end;

procedure THomeCommunication.HomeThreadErrorReceived(Sender: TSocketThread);
begin
  if Assigned(FOnErrorReceived) then
    FOnErrorReceived(Self, FThread.FErrorID, FThread.FErrorMsg);
end;

procedure THomeCommunication.HomeThreadHandshakeReceived(Sender: TSocketThread);
begin
  FDisabled := not THomeThread(Sender).FHandshakeSuccess;

  if not FDisabled then
  begin
    MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llDebug, _('Server'), _('Server accepted handshake')));

    FCommunicationEstablished := True;

    if AppGlobals.UserWasSetup and (AppGlobals.User <> '') and (AppGlobals.Pass <> '') then
      SendLogIn(AppGlobals.User, AppGlobals.Pass);

    FWasConnected := False;
    if Assigned(FOnStateChanged) then
      FOnStateChanged(Self);
    FWasConnected := True;
  end;

  if FDisabled then
  begin
    MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llError, _('Server'), _('Server rejected handshake')));
    Sender.Terminate;
  end;

  if Assigned(FOnHandshakeReceived) then
    FOnHandshakeReceived(Self, THomeThread(Sender).FHandshakeSuccess);
end;

procedure THomeCommunication.HomeThreadLog(Sender: TSocketThread);
begin
  MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llError, _('Server'), _(FThread.LogMsg)));
end;

procedure THomeCommunication.HomeThreadLogInReceived(Sender: TSocketThread);
begin
  FAuthenticated := THomeThread(Sender).FAuthenticated;
  FIsAdmin := THomeThread(Sender).FIsAdmin;

  if FAuthenticated then
    MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llInfo, _('Server'), _('User logged in')));

  if Assigned(FOnLogInReceived) then
    FOnLogInReceived(Self, FAuthenticated);
end;

procedure THomeCommunication.HomeThreadLogOutReceived(Sender: TSocketThread);
begin
  MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llInfo, _('Server'), _('User logged out')));

  FAuthenticated := THomeThread(Sender).FAuthenticated;
  FIsAdmin := THomeThread(Sender).FIsAdmin;

  if Assigned(FOnLogOutReceived) then
    FOnLogOutReceived(Self);
end;

procedure THomeCommunication.HomeThreadMonitorStreamsReceived(Sender: TSocketThread);
begin
  if Assigned(FOnMonitorStreamsReceived) then
    FOnMonitorStreamsReceived(Self, THomeThread(Sender).FStreamIDs);
end;

procedure THomeCommunication.HomeThreadSearchChartsReceived(Sender: TSocketThread);
begin
  if Assigned(FOnSearchChartsReceived) then
    FOnSearchChartsReceived(Self, THomeThread(Sender).FSearchReceivedChartsSuccess, THomeThread(Sender).FSearchReceivedCharts);
end;

procedure THomeCommunication.HomeThreadSecured(Sender: TSocketThread);
begin
  FSecured := True;
  MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltSecure, llInfo, _('Server'), _('Connection secured')));
end;

procedure THomeCommunication.HomeThreadServerDataReceived(Sender: TSocketThread);
begin
  if Assigned(FOnServerDataReceived) then
    FOnServerDataReceived(Self);
end;

procedure THomeCommunication.HomeThreadServerInfoReceived(Sender: TSocketThread);
begin
  if Assigned(FOnServerInfoReceived) then
    FOnServerInfoReceived(Self, THomeThread(Sender).FServerInfoClientCount, THomeThread(Sender).FServerInfoRecordingCount);
end;

procedure THomeCommunication.HomeThreadException(Sender: TSocketThread);
begin
  FRaisedException := Sender.RaisedException;
  if Assigned(FOnException) then
    FOnException(Self);
end;

procedure THomeCommunication.HomeThreadGetStreamDataReceived(Sender: TSocketThread);
begin
  if Assigned(FOnGetStreamDataReceived) then
    FOnGetStreamDataReceived(Self, THomeThread(Sender).FGetStreamDataLastTitles, THomeThread(Sender).FGetStreamDataOtherUserRegExps, THomeThread(Sender).FGetStreamDataUserRegExps);
end;

procedure THomeCommunication.HomeThreadTerminate(Sender: TObject);
begin
  if (FThread <> nil) and (Sender = FThread) then
    FThread := nil;
end;

procedure THomeCommunication.HomeThreadNetworkTitleChangedReceived(Sender: TSocketThread);
begin
  if Assigned(FOnNetworkTitleChangedReceived) then
    FOnNetworkTitleChangedReceived(Self, THomeThread(Sender).FNetworkTitleChanged.StreamID, THomeThread(Sender).FNetworkTitleChanged.StreamName,
      THomeThread(Sender).FNetworkTitleChanged.StreamTitle, THomeThread(Sender).FNetworkTitleChanged.StreamParsedTitle,
      THomeThread(Sender).FNetworkTitleChanged.CurrentURL, THomeThread(Sender).FNetworkTitleChanged.RegExes,
      THomeThread(Sender).FNetworkTitleChanged.Format, THomeThread(Sender).FNetworkTitleChanged.Bitrate,
      THomeThread(Sender).FNetworkTitleChanged.ServerHash, THomeThread(Sender).FNetworkTitleChanged.ServerArtistHash);
end;

procedure THomeCommunication.SendHandshake;
var
  Cmd: TCommandHandshake;
begin
  if not Connected then
    Exit;

  Cmd := TCommandHandshake.Create;
  Cmd.ID := AppGlobals.ID;
  Cmd.VersionMajor := AppGlobals.AppVersion.Major;
  Cmd.VersionMinor := AppGlobals.AppVersion.Minor;
  Cmd.VersionRevision := AppGlobals.AppVersion.Revision;
  Cmd.VersionBuild := AppGlobals.AppVersion.Build;
  Cmd.Build := 900; // AppGlobals.BuildNumber;  // TODO: !!!
  Cmd.Language := Language.CurrentLanguage.ID;
  Cmd.ProtoVersion := 8;

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.Connect;
begin
  if FDisabled then
    Exit;

  FThread := THomeThread.Create;
  FThread.OnConnected := HomeThreadConnected;
  FThread.OnEnded := HomeThreadEnded;
  FThread.OnBeforeEnded := HomeThreadBeforeEnded;
  FThread.OnBytesTransferred := HomeThreadBytesTransferred;
  FThread.OnLog := HomeThreadLog;

  FThread.OnHandshakeReceived := HomeThreadHandshakeReceived;
  FThread.OnLogInReceived := HomeThreadLogInReceived;
  FThread.OnLogOutReceived := HomeThreadLogOutReceived;
  FThread.OnServerDataReceived := HomeThreadServerDataReceived;

  FThread.OnServerInfoReceived := HomeThreadServerInfoReceived;
  FThread.OnErrorReceived := HomeThreadErrorReceived;

  FThread.OnNetworkTitleChangedReceived := HomeThreadNetworkTitleChangedReceived;
  FThread.OnMonitorStreamsReceived := HomeThreadMonitorStreamsReceived;
  FThread.OnSearchChartsReceived := HomeThreadSearchChartsReceived;

  FThread.OnConvertManualToAutomaticReceived := HomeThreadConvertManualToAutomaticReceived;

  FThread.OnGetStreamDataReceived := HomeThreadGetStreamDataReceived;

  FThread.OnSecured := HomeThreadSecured;
  FThread.OnCommunicationEstablished := HomeThreadCommunicationEstablished;
  FThread.OnException := HomeThreadException;

  FThread.OnTerminate := HomeThreadTerminate;

  FThread.Start;
end;

initialization
  TCommand.RegisterCommand(ctHandshakeResponse, TCommandHandshakeResponse);
  TCommand.RegisterCommand(ctLogInResponse, TCommandLogInResponse);
  TCommand.RegisterCommand(ctLogOutResponse, TCommandLogOutResponse);
  TCommand.RegisterCommand(ctGetServerDataResponse, TCommandGetServerDataResponse);
  TCommand.RegisterCommand(ctServerInfoResponse, TCommandServerInfoResponse);
  TCommand.RegisterCommand(ctMessageResponse, TCommandMessageResponse);
  TCommand.RegisterCommand(ctNetworkTitleChangedResponse, TCommandNetworkTitleChangedResponse);
  TCommand.RegisterCommand(ctGetMonitorStreamsResponse, TCommandGetMonitorStreamsResponse);
  TCommand.RegisterCommand(ctSearchChartsResponse, TCommandSearchChartsResponse);
  TCommand.RegisterCommand(ctConvertManualToAutomaticResponse, TCommandConvertManualToAutomaticResponse);
  TCommand.RegisterCommand(ctGetStreamDataResponse, TCommandGetStreamDataResponse);

  HomeComm := nil;

finalization
  TCommand.UnregisterCommands;
  HomeComm.Free;

end.
