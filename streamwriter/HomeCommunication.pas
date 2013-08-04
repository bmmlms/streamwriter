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

unit HomeCommunication;

interface

uses
  Windows, SysUtils, Classes, HTTPThread, StrUtils, Generics.Collections,
  Sockets, WinSock, Communication, Protocol, Commands, ExtendedStream,
  HomeCommands, DataManager, AppData, AudioFunctions, LanguageObjects,
  TypeDefs;

type
  TCommErrors = (ceUnknown, ceAuthRequired, ceNotification, ceOneTimeNotification);

  THomeThread = class(TCommandThreadBase)
  private
    FLists: TDataLists;

    FAuthenticated: Boolean;
    FIsAdmin: Boolean;

    FHandshakeSuccess: Boolean;

    FNetworkTitleChanged: TCommandNetworkTitleChangedResponse;

    FServerInfoClientCount: Cardinal;
    FServerInfoRecordingCount: Cardinal;

    FErrorID: TCommErrors;
    FErrorMsg: string;

    FStreamIDs: TIntArray;

    FSearchReceivedChartsSuccess: Boolean;
    FSearchReceivedCharts: TChartList;

    FWishlistUpgradeTitles: TWishlistUpgradeList;

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

    procedure DoHandshakeReceived(CommandHeader: TCommandHeader; Command: TCommandHandshakeResponse);
    procedure DoLogInReceived(CommandHeader: TCommandHeader; Command: TCommandLogInResponse);
    procedure DoLogOutReceived(CommandHeader: TCommandHeader; Command: TCommandLogOutResponse);
    procedure DoServerDataReceived(CommandHeader: TCommandHeader; Command: TCommandGetServerDataResponse);
    procedure DoNetworkTitleChanged(CommandHeader: TCommandHeader; Command: TCommandNetworkTitleChangedResponse);
    procedure DoServerInfoReceived(CommandHeader: TCommandHeader; Command: TCommandServerInfoResponse);
    procedure DoMessageReceived(CommandHeader: TCommandHeader; Command: TCommandMessageResponse);
    procedure DoMonitorStreamsReceived(CommandHeader: TCommandHeader; Command: TCommandGetMonitorStreamsResponse);
    procedure DoSearchChartsReceived(CommandHeader: TCommandHeader; Command: TCommandSearchChartsResponse);
    procedure DoWishlistUpgradeReceived(CommandHeader: TCommandHeader; Command: TCommandGetWishlistUpgradeResponse);
  protected
    procedure DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand); override;
    procedure DoException(E: Exception); override;
    procedure DoEnded; override;
  public
    constructor Create(Lists: TDataLists);
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
  end;

  TBooleanEvent = procedure(Sender: TObject; Value: Boolean) of object;
  TStreamsReceivedEvent = procedure(Sender: TObject) of object;
  TChartsReceivedEvent = procedure(Sender: TObject; Success: Boolean; Charts: TChartList) of object;
  TTitleChangedEvent = procedure(Sender: TObject; ID: Cardinal; Name, Title, CurrentURL, TitlePattern: string;
    Format: TAudioTypes; Kbps: Cardinal; ServerHash, ServerArtistHash: Cardinal) of object;
  TServerInfoEvent = procedure(Sender: TObject; ClientCount, RecordingCount: Cardinal) of object;
  TErrorEvent = procedure(Sender: TObject; ID: TCommErrors; Msg: string) of object;
  TIntArrayEvent = procedure(Sender: TObject; IntArr: TIntArray) of object;
  TWishlistUpgradeEvent = procedure(Sender: TObject; WishlistUpgrade: TWishlistUpgradeList) of object;

  THomeCommunication = class
  private
    FDisabled: Boolean;
    FThread: THomeThread;

    FLists: TDataLists;

    FAuthenticated, FIsAdmin, FWasConnected, FConnected, FNotifyTitleChanges: Boolean;
    FTitleNotificationsSet: Boolean;

    FOnStateChanged: TNotifyEvent;
    FOnTitleNotificationsChanged: TNotifyEvent;
    FOnBytesTransferred: TTransferProgressEvent;

    FOnHandshakeReceived: TBooleanEvent;
    FOnLogInReceived: TBooleanEvent;
    FOnLogOutReceived: TNotifyEvent;
    FOnStreamsReceived: TStreamsReceivedEvent;
    FOnServerDataReceived: TNotifyEvent;
    FOnNetworkTitleChangedReceived: TTitleChangedEvent;
    FOnServerInfoReceived: TServerInfoEvent;
    FOnErrorReceived: TErrorEvent;
    FOnMonitorStreamsReceived: TIntArrayEvent;
    FOnSearchChartsReceived: TChartsReceivedEvent;
    FOnWishlistUpgradeReceived: TWishlistUpgradeEvent;

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
    procedure HomeThreadWishlistUpgradeReceived(Sender: TSocketThread);

    procedure HomeThreadTerminate(Sender: TObject);
  public
    constructor Create(Lists: TDataLists);
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
    procedure SendSubmitStream(URL: string);
    procedure SendSetStreamData(StreamID: Cardinal; Rating: Byte);
    procedure SendTitleChanged(StreamID: Cardinal; StreamName, Title, CurrentURL, URL: string; Format: TAudioTypes;
      Kbps: Cardinal; URLs: TStringList);
    procedure SendGetMonitorStreams(Count: Cardinal);
    procedure SendSyncWishlist; overload;
    procedure SendSyncWishlist(SyncType: TSyncWishlistTypes; Hashes: TSyncWishlistRecordArray); overload;
    procedure SendSyncWishlist(SyncType: TSyncWishlistTypes; Hash: Cardinal; IsArtist: Boolean); overload;
    procedure SendSearchCharts(Top: Boolean; Term: string);
    procedure SendGetWishlistUpgrade(Titles: TStringList);
    procedure SendStreamAnalyzationData(StreamID: Cardinal; Data: TExtendedStream);

    property Disabled: Boolean read FDisabled;
    property WasConnected: Boolean read FWasConnected;
    property Connected: Boolean read FConnected;
    property Authenticated: Boolean read FAuthenticated;
    property NotifyTitleChanges: Boolean read FNotifyTitleChanges;
    property IsAdmin: Boolean read FIsAdmin;
    property ThreadAlive: Boolean read FGetThreadAlive;

    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnTitleNotificationsChanged: TNotifyEvent read FOnTitleNotificationsChanged write FOnTitleNotificationsChanged;
    property OnBytesTransferred: TTransferProgressEvent read FOnBytesTransferred write FOnBytesTransferred;

    property OnHandshakeReceived: TBooleanEvent read FOnHandshakeReceived write FOnHandshakeReceived;
    property OnLogInReceived: TBooleanEvent read FOnLogInReceived write FOnLogInReceived;
    property OnLogOutReceived: TNotifyEvent read FOnLogOutReceived write FOnLogOutReceived;
    property OnStreamsReceived: TStreamsReceivedEvent read FOnStreamsReceived write FOnStreamsReceived;
    property OnServerDataReceived: TNotifyEvent read FOnServerDataReceived write FOnServerDataReceived;
    property OnNetworkTitleChangedReceived: TTitleChangedEvent read FOnNetworkTitleChangedReceived write FOnNetworkTitleChangedReceived;
    property OnServerInfoReceived: TServerInfoEvent read FOnServerInfoReceived write FOnServerInfoReceived;
    property OnErrorReceived: TErrorEvent read FOnErrorReceived write FOnErrorReceived;
    property OnMonitorStreamsReceived: TIntArrayEvent read FOnMonitorStreamsReceived write FOnMonitorStreamsReceived;
    property OnSearchChartsReceived: TChartsReceivedEvent read FOnSearchChartsReceived write FOnSearchChartsReceived;
    property OnWishlistUpgradeReceived: TWishlistUpgradeEvent read FOnWishlistUpgradeReceived write FOnWishlistUpgradeReceived;
  end;

var
  HomeComm: THomeCommunication;

implementation

{ THomeThread }

constructor THomeThread.Create(Lists: TDataLists);
begin
  FLists := Lists;

  inherited Create('streamwriter.org', 7085, TSocketStream.Create);
  //inherited Create('gaia', 7085, TSocketStream.Create);

  UseSynchronize := True;
end;

destructor THomeThread.Destroy;
begin

  inherited;
end;

procedure THomeThread.DoEnded;
begin
  inherited;

  if not Terminated then
    Sleep(3000);
end;                    // TODO: charts suchen. ist im status "suche". verbindung geht, kommt wieder. alles bleibt disabled in chartsansicht.

procedure THomeThread.DoMessageReceived(CommandHeader: TCommandHeader;
  Command: TCommandMessageResponse);
begin
  FErrorID := TCommErrors(Command.MessageID);
  FErrorMsg := Command.MessageMsg;

  if Assigned(FOnErrorReceived) then
    Sync(FOnErrorReceived);
end;

procedure THomeThread.DoMonitorStreamsReceived(
  CommandHeader: TCommandHeader; Command: TCommandGetMonitorStreamsResponse);
begin
  FStreamIDs := Command.StreamIDs;

  if Assigned(FOnMonitorStreamsReceived) then
    Sync(FOnMonitorStreamsReceived);
end;

procedure THomeThread.DoException(E: Exception);
begin
  inherited;

end;

procedure THomeThread.DoHandshakeReceived(CommandHeader: TCommandHeader;
  Command: TCommandHandshakeResponse);
begin
  FHandshakeSuccess := Command.Success;

  if Assigned(FOnHandshakeReceived) then
    Sync(FOnHandshakeReceived);
end;

procedure THomeThread.DoLogInReceived(CommandHeader: TCommandHeader;
  Command: TCommandLogInResponse);
begin
  FAuthenticated := Command.Success;
  FIsAdmin := Command.IsAdmin;

  if Assigned(FOnLogInReceived) then
    Sync(FOnLogInReceived);
end;

procedure THomeThread.DoLogOutReceived(CommandHeader: TCommandHeader;
  Command: TCommandLogOutResponse);
begin
  FAuthenticated := False;

  if Assigned(FOnLogOutReceived) then
    Sync(FOnLogOutReceived);
end;

procedure THomeThread.DoNetworkTitleChanged(CommandHeader: TCommandHeader;
  Command: TCommandNetworkTitleChangedResponse);
begin
  if not AppGlobals.AutoTuneIn then
    Exit;

  FNetworkTitleChanged := Command;

  if (FNetworkTitleChanged.StreamName <> '') and (FNetworkTitleChanged.Title <> '') and (FNetworkTitleChanged.CurrentURL <> '') then
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
  WishlistUpgrade: TCommandGetWishlistUpgradeResponse absolute Command;
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
    ctGetWishlistUpgradeResponse:
      DoWishlistUpgradeReceived(CommandHeader, WishlistUpgrade);
  end;
end;

procedure THomeThread.DoSearchChartsReceived(CommandHeader: TCommandHeader;
  Command: TCommandSearchChartsResponse);
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
    Sync(FOnSearchChartsReceived)
end;

procedure THomeThread.DoServerDataReceived(CommandHeader: TCommandHeader;
  Command: TCommandGetServerDataResponse);
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

    // Streams laden und OwnRating synchronisieren
    Stream.Read(Count);
    for i := 0 to Count - 1 do
    begin
      StreamEntry := TStreamBrowserEntry.LoadFromHome(Stream, CommandHeader.Version);
      for StreamEntry2 in FLists.BrowserList do
        if StreamEntry.ID = StreamEntry2.ID then
        begin
          StreamEntry.OwnRating := StreamEntry2.OwnRating;
          Break;
        end;
      Streams.Add(StreamEntry);
    end;

    // Wenn alles erfolgreich geladen wurde alte Listen leeren.
    // Falls hier jetzt eine Exception kommt wird es bitter...
    for Genre in FLists.GenreList do
      Genre.Free;
    FLists.GenreList.Clear;
    for StreamEntry in FLists.BrowserList do
      StreamEntry.Free;
    FLists.BrowserList.Clear;

    // Der Liste alle Sachen wieder hinzufügen
    for Genre in Genres do
      FLists.GenreList.Add(Genre);
    for StreamEntry in Streams do
      FLists.BrowserList.Add(StreamEntry);
  except
    for i := 0 to Genres.Count - 1 do
      Genres[i].Free;
    for i := 0 to Streams.Count - 1 do
      Streams[i].Free;

    Genres.Free;
    Streams.Free;
  end;

  if Assigned(FOnServerDataReceived) then
    Sync(FOnServerDataReceived);
end;

procedure THomeThread.DoServerInfoReceived(CommandHeader: TCommandHeader;
  Command: TCommandServerInfoResponse);
begin
  FServerInfoClientCount := Command.ClientCount;
  FServerInfoRecordingCount := Command.RecordingCount;

  if Assigned(FOnServerInfoReceived) then
    Sync(FOnServerInfoReceived);
end;

procedure THomeThread.DoWishlistUpgradeReceived(
  CommandHeader: TCommandHeader;
  Command: TCommandGetWishlistUpgradeResponse);
begin
  FWishlistUpgradeTitles := Command.Titles;

  if Assigned(FOnWishlistUpgradeReceived) then
    Sync(FOnWishlistUpgradeReceived)
end;

{ THomeCommunication }

procedure THomeCommunication.HomeThreadBytesTransferred(Sender: TObject;
  Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader;
  Transferred: UInt64);
begin
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Sender, Direction, CommandID, CommandHeader, Transferred);
end;

constructor THomeCommunication.Create(Lists: TDataLists);
begin
  inherited Create;

  FLists := Lists;
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
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandLogIn.Create(User, Pass));
end;

procedure THomeCommunication.SendLogOut;
begin
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandLogOut.Create)
end;

procedure THomeCommunication.SendSearchCharts(Top: Boolean; Term: string);
begin
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandSearchCharts.Create(Top, Term));
end;

procedure THomeCommunication.SendSetSettings(TitleNotifications: Boolean);
begin
  if not FConnected then
    Exit;

  FNotifyTitleChanges := TitleNotifications;

  FThread.SendCommand(TCommandSetSettings.Create(TitleNotifications));

  if Assigned(FOnTitleNotificationsChanged) then
    FOnTitleNotificationsChanged(Self);
end;

procedure THomeCommunication.SendSetStreamData(StreamID: Cardinal;
  Rating: Byte);
var
  Cmd: TCommandSetStreamData;
begin
  if not FConnected then
    Exit;

  Cmd := TCommandSetStreamData.Create;
  Cmd.StreamID := StreamID;
  Cmd.Rating := Rating;

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.SendStreamAnalyzationData(
  StreamID: Cardinal; Data: TExtendedStream);
begin
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandStreamAnalyzationData.Create(StreamID, Data));
end;

procedure THomeCommunication.SendSubmitStream(URL: string);
begin
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandSubmitStream.Create(URL));
end;

procedure THomeCommunication.SendSyncWishlist;
var
  i: Integer;
  ItemsFound: Integer;
  Hashes: TSyncWishlistRecordArray;
begin
  if not FConnected then
    Exit;

  SetLength(Hashes, FLists.SaveList.Count);
  ItemsFound := 0;
  for i := 0 to FLists.SaveList.Count - 1 do
    if FLists.SaveList[i].ServerHash > 0 then
    begin
      Hashes[ItemsFound] := TSyncWishlistRecord.Create(FLists.SaveList[i].ServerHash, False);
      Inc(ItemsFound);
    end else if FLists.SaveList[i].ServerArtistHash > 0 then
    begin
      Hashes[ItemsFound] := TSyncWishlistRecord.Create(FLists.SaveList[i].ServerArtistHash, True);
      Inc(ItemsFound);
    end;
  SetLength(Hashes, ItemsFound);

  SendSyncWishlist(swSync, Hashes);
end;

procedure THomeCommunication.SendSyncWishlist(SyncType: TSyncWishlistTypes;
  Hashes: TSyncWishlistRecordArray);
begin
  if not FConnected then
    Exit;

  if Length(Hashes) = 0 then
    Exit;

  FThread.SendCommand(TCommandSyncWishlist.Create(SyncType, Hashes));
end;

procedure THomeCommunication.SendSyncWishlist(SyncType: TSyncWishlistTypes;
  Hash: Cardinal; IsArtist: Boolean);
var
  Hashes: TSyncWishlistRecordArray;
begin
  if (not Connected) or (Hash = 0) or (SyncType = swSync) then
    Exit;

  SetLength(Hashes, 1);
  Hashes[0] := TSyncWishlistRecord.Create(Hash, IsArtist);

  SendSyncWishlist(SyncType, Hashes);
end;

procedure THomeCommunication.SendTitleChanged(StreamID: Cardinal;
  StreamName, Title, CurrentURL, URL: string; Format: TAudioTypes; Kbps: Cardinal;
  URLs: TStringList);
begin
  if not FConnected then
    Exit;

  if (StreamID = 0) or (Trim(StreamName) = '') or (Trim(URL) = '') or (Length(Title) <= 3) then
    Exit;

  FThread.SendCommand(TCommandTitleChanged.Create(StreamID, StreamName, Title,
    CurrentURL, URL, Format, Kbps, URLs.Text));
end;

procedure THomeCommunication.SendUpdateStats(List: TList<Cardinal>;
  RecordingCount: Cardinal);
var
  i: Integer;
  Cmd: TCommandUpdateStats;
  Stream: TExtendedStream;
begin
  if not FConnected then
    Exit;

  Cmd := TCommandUpdateStats.Create;
  Stream := TExtendedStream(Cmd.Stream);

  Stream.Write(Cardinal(List.Count));
  for i := 0 to List.Count - 1 do
  begin
    Stream.Write(List[i]);
  end;

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
  FOnStreamsReceived := nil;
  FOnNetworkTitleChangedReceived := nil;
  FOnErrorReceived := nil;
  FOnMonitorStreamsReceived := nil;
  FOnSearchChartsReceived := nil;

  if FThread <> nil then
    FThread.Terminate;
end;

procedure THomeCommunication.SendClientStats(Auto: Boolean);
begin
  if not FConnected then
    Exit;

  if Auto then
    FThread.SendCommand(TCommandClientStats.Create(csAutoSave))
  else
    FThread.SendCommand(TCommandClientStats.Create(csSave));
end;

function THomeCommunication.SendCommand(Cmd: TCommand): Boolean;
begin
  Result := True;
  if not FConnected then
    Exit(False);

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.SendGetMonitorStreams(Count: Cardinal);
begin
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandGetMonitorStreams.Create(Count))
end;

function THomeCommunication.SendGetServerData: Boolean;
begin
  Result := True;
  if not FConnected then
    Exit(False);

  FThread.SendCommand(TCommandGetServerData.Create)
end;

procedure THomeCommunication.SendGetWishlistUpgrade(Titles: TStringList);
begin
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandGetWishlistUpgrade.Create(Titles));
end;

procedure THomeCommunication.HomeThreadConnected(Sender: TSocketThread);
begin
  inherited;

  FConnected := True;

  SendHandshake;
end;

procedure THomeCommunication.HomeThreadBeforeEnded(Sender: TSocketThread);
begin
  FConnected := False;
  FAuthenticated := False;
  FIsAdmin := False;
  FTitleNotificationsSet := False;
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

procedure THomeCommunication.HomeThreadErrorReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnErrorReceived) then
    FOnErrorReceived(Self, FThread.FErrorID, FThread.FErrorMsg);
end;

procedure THomeCommunication.HomeThreadHandshakeReceived(
  Sender: TSocketThread);
begin
  FDisabled := not THomeThread(Sender).FHandshakeSuccess;

  if not FDisabled then
  begin
    if AppGlobals.UserWasSetup and (AppGlobals.User <> '') and (AppGlobals.Pass <> '') then
      SendLogIn(AppGlobals.User, AppGlobals.Pass);

    FWasConnected := False;
    if Assigned(FOnStateChanged) then
      FOnStateChanged(Self);
    FWasConnected := True;
  end;

  if FDisabled then
    Sender.Terminate;

  if Assigned(FOnHandshakeReceived) then
    FOnHandshakeReceived(Self, THomeThread(Sender).FHandshakeSuccess);
end;

procedure THomeCommunication.HomeThreadLogInReceived(Sender: TSocketThread);
begin
  FAuthenticated := THomeThread(Sender).FAuthenticated;
  FIsAdmin := THomeThread(Sender).FIsAdmin;

  if Assigned(FOnLogInReceived) then
    FOnLogInReceived(Self, FAuthenticated);
end;

procedure THomeCommunication.HomeThreadLogOutReceived(Sender: TSocketThread);
begin
  FAuthenticated := THomeThread(Sender).FAuthenticated;
  FIsAdmin := THomeThread(Sender).FIsAdmin;

  if Assigned(FOnLogOutReceived) then
    FOnLogOutReceived(Self);
end;

procedure THomeCommunication.HomeThreadMonitorStreamsReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnMonitorStreamsReceived) then
    FOnMonitorStreamsReceived(Self, THomeThread(Sender).FStreamIDs);
end;

procedure THomeCommunication.HomeThreadSearchChartsReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnSearchChartsReceived) then
    FOnSearchChartsReceived(Self, THomeThread(Sender).FSearchReceivedChartsSuccess, THomeThread(Sender).FSearchReceivedCharts);
end;

procedure THomeCommunication.HomeThreadServerDataReceived(Sender: TSocketThread);
begin
  if Assigned(FOnStreamsReceived) then
    FOnStreamsReceived(Self);

  if Assigned(FOnServerDataReceived) then
    FOnServerDataReceived(Self);
end;

procedure THomeCommunication.HomeThreadServerInfoReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnServerInfoReceived) then
    FOnServerInfoReceived(Self, THomeThread(Sender).FServerInfoClientCount, THomeThread(Sender).FServerInfoRecordingCount);
end;

procedure THomeCommunication.HomeThreadTerminate(Sender: TObject);
begin
  if (FThread <> nil) and (Sender = FThread) then
    FThread := nil;
end;

procedure THomeCommunication.HomeThreadWishlistUpgradeReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnWishlistUpgradeReceived) then
    FOnWishlistUpgradeReceived(Self, THomeThread(Sender).FWishlistUpgradeTitles);
end;

procedure THomeCommunication.HomeThreadNetworkTitleChangedReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnNetworkTitleChangedReceived) then
    FOnNetworkTitleChangedReceived(Self,  THomeThread(Sender).FNetworkTitleChanged.StreamID, THomeThread(Sender).FNetworkTitleChanged.StreamName,
      THomeThread(Sender).FNetworkTitleChanged.Title, THomeThread(Sender).FNetworkTitleChanged.CurrentURL,
      THomeThread(Sender).FNetworkTitleChanged.TitleRegEx, THomeThread(Sender).FNetworkTitleChanged.Format,
      THomeThread(Sender).FNetworkTitleChanged.Bitrate, THomeThread(Sender).FNetworkTitleChanged.ServerHash,
      THomeThread(Sender).FNetworkTitleChanged.ServerArtistHash);
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
  Cmd.Build := AppGlobals.BuildNumber;
  Cmd.Language := Language.CurrentLanguage.ID;
  Cmd.ProtoVersion := 2;

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.Connect;
begin
  if FDisabled then
    Exit;

  FThread := THomeThread.Create(FLists);
  FThread.OnConnected := HomeThreadConnected;
  FThread.OnEnded := HomeThreadEnded;
  FThread.OnBeforeEnded := HomeThreadBeforeEnded;
  FThread.OnBytesTransferred := HomeThreadBytesTransferred;

  FThread.OnHandshakeReceived := HomeThreadHandshakeReceived;
  FThread.OnLogInReceived := HomeThreadLogInReceived;
  FThread.OnLogOutReceived := HomeThreadLogOutReceived;
  FThread.OnServerDataReceived := HomeThreadServerDataReceived;

  FThread.OnServerInfoReceived := HomeThreadServerInfoReceived;
  FThread.OnErrorReceived := HomeThreadErrorReceived;

  FThread.OnNetworkTitleChangedReceived := HomeThreadNetworkTitleChangedReceived;
  FThread.OnMonitorStreamsReceived := HomeThreadMonitorStreamsReceived;
  FThread.OnSearchChartsReceived := HomeThreadSearchChartsReceived;
  FThread.OnWishlistUpgradeReceived := HomeThreadWishlistUpgradeReceived;

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
  TCommand.RegisterCommand(ctGetWishlistUpgradeResponse, TCommandGetWishlistUpgradeResponse);

  HomeComm := nil;

finalization
  TCommand.UnregisterCommands;
  HomeComm.Free;

end.
