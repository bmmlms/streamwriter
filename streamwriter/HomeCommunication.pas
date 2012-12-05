{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
  Sockets, WinSock, ZLib, Communication, Protocol, Commands, ExtendedStream,
  HomeCommands, DataManager, AppData, AudioFunctions;

type
  TCommErrors = (ceUnknown, ceAuthRequired, ceNotification, ceOneTimeNotification);

  THomeThread = class(TCommandThreadBase)
  private
    FC: Cardinal;

    FAuthenticated: Boolean;
    FIsAdmin: Boolean;
    FGenres: TList<TGenre>;
    FCharts: TList<TChartEntry>;
    FStreams: TStreamBrowserList;

    FNetworkTitleChanged: TCommandNetworkTitleChangedResponse;

    FServerInfoClientCount: Cardinal;
    FServerInfoRecordingCount: Cardinal;

    FErrorID: TCommErrors;
    FErrorMsg: string;

    {
    FChangedStreamID: Cardinal;
    FChangedStreamName: string;
    FChangedTitle: string;
    FChangedCurrentURL: string;
    FChangedKbps: Cardinal;
    FChangedFormat: string;
    FChangedTitlePattern: string;
    }
    //FLogInReceived: TCommandLogInResponse;
    //FLogOutReceived: TCommandLogOutResponse;

    FOnLogInReceived: TSocketEvent;
    FOnLogOutReceived: TSocketEvent;
    FOnServerDataReceived: TSocketEvent;
    FOnNetworkTitleChangedReceived: TSocketEvent;
    FOnServerInfoReceived: TSocketEvent;
    FOnErrorReceived: TSocketEvent;

    procedure DoLogInReceived(CommandHeader: TCommandHeader; Command: TCommandLogInResponse);
    procedure DoLogOutReceived(CommandHeader: TCommandHeader; Command: TCommandLogOutResponse);
    procedure DoServerDataReceived(CommandHeader: TCommandHeader; Command: TCommandGetServerDataResponse);
    procedure DoNetworkTitleChanged(CommandHeader: TCommandHeader; Command: TCommandNetworkTitleChangedResponse);
    procedure DoServerInfoReceived(CommandHeader: TCommandHeader; Command: TCommandServerInfoResponse);
    procedure DoErrorReceived(CommandHeader: TCommandHeader; Command: TCommandErrorResponse);
  protected
    procedure DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand); override;
    procedure DoException(E: Exception); override;
    procedure DoEnded; override;
  public
    constructor Create;
    destructor Destroy; override;

    property OnLogInReceived: TSocketEvent read FOnLogInReceived write FOnLogInReceived;
    property OnLogOutReceived: TSocketEvent read FOnLogOutReceived write FOnLogOutReceived;
    property OnServerDataReceived: TSocketEvent read FOnServerDataReceived write FOnServerDataReceived;
    property OnNetworkTitleChangedReceived: TSocketEvent read FOnNetworkTitleChangedReceived write FOnNetworkTitleChangedReceived;
    property OnServerInfoReceived: TSocketEvent read FOnServerInfoReceived write FOnServerInfoReceived;
    property OnErrorReceived: TSocketEvent read FOnErrorReceived write FOnErrorReceived;
  end;

  TBooleanEvent = procedure(Sender: TObject; Value: Boolean) of object;
  TStreamsReceivedEvent = procedure(Sender: TObject; Genres: TList<TGenre>; Streams: TList<TStreamBrowserEntry>) of object;
  TChartsReceivedEvent = procedure(Sender: TObject; Charts: TList<TChartEntry>) of object;
  TTitleChangedEvent = procedure(Sender: TObject; ID: Cardinal; Name, Title, CurrentURL, TitlePattern: string; Format: TAudioTypes; Kbps: Cardinal) of object;
  TServerInfoEvent = procedure(Sender: TObject; ClientCount, RecordingCount: Cardinal) of object;
  TErrorEvent = procedure(Sender: TObject; ID: TCommErrors; Msg: string) of object;

  THomeCommunication = class
  private
    FThread: THomeThread;

    FAuthenticated, FIsAdmin, FWasConnected, FConnected: Boolean;
    FTitleNotificationsSet: Boolean;

    FOnStateChanged: TNotifyEvent;
    FOnBytesTransferred: TTransferProgressEvent;

    FOnLogInReceived: TBooleanEvent;
    FOnLogOutReceived: TNotifyEvent;
    FOnStreamsReceived: TStreamsReceivedEvent;
    FOnChartsReceived: TChartsReceivedEvent;
    FOnNetworkTitleChangedReceived: TTitleChangedEvent;
    FOnServerInfoReceived: TServerInfoEvent;
    FOnErrorReceived: TErrorEvent;

    procedure HomeThreadConnected(Sender: TSocketThread);
    procedure HomeThreadEnded(Sender: TSocketThread);
    procedure HomeThreadBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader; Transferred: UInt64);

    procedure HomeThreadLogInReceived(Sender: TSocketThread);
    procedure HomeThreadLogOutReceived(Sender: TSocketThread);
    procedure HomeThreadServerDataReceived(Sender: TSocketThread);
    procedure HomeThreadNetworkTitleChangedReceived(Sender: TSocketThread);
    procedure HomeThreadServerInfoReceived(Sender: TSocketThread);
    procedure HomeThreadErrorReceived(Sender: TSocketThread);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Terminate;
    procedure SendHandshake;
    procedure SendLogIn(User, Pass: string);
    procedure SendLogOut;
    function SendGetServerData: Boolean;
    procedure SendUpdateStats(List: TList<Cardinal>; RecordingCount: Cardinal);
    procedure SendSetSettings(TitleNotifications: Boolean);

    property WasConnected: Boolean read FWasConnected;
    property Connected: Boolean read FConnected;
    property Authenticated: Boolean read FAuthenticated;
    property IsAdmin: Boolean read FIsAdmin;

    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnBytesTransferred: TTransferProgressEvent read FOnBytesTransferred write FOnBytesTransferred;

    property OnLogInReceived: TBooleanEvent read FOnLogInReceived write FOnLogInReceived;
    property OnLogOutReceived: TNotifyEvent read FOnLogOutReceived write FOnLogOutReceived;
    property OnStreamsReceived: TStreamsReceivedEvent read FOnStreamsReceived write FOnStreamsReceived;
    property OnChartsReceived: TChartsReceivedEvent read FOnChartsReceived write FOnChartsReceived;
    property OnNetworkTitleChangedReceived: TTitleChangedEvent read FOnNetworkTitleChangedReceived write FOnNetworkTitleChangedReceived;
    property OnServerInfoReceived: TServerInfoEvent read FOnServerInfoReceived write FOnServerInfoReceived;
    property OnErrorReceived: TErrorEvent read FOnErrorReceived write FOnErrorReceived;
  end;

var
  HomeComm: THomeCommunication;

implementation

{ THomeThread }

constructor THomeThread.Create;
begin
  inherited Create('mistake.ws', 7085, TSocketStream.Create);

  UseSynchronize := True;
  FGenres := TList<TGenre>.Create;
  FCharts := TList<TChartEntry>.Create;
  FStreams := TStreamBrowserList.Create;
end;

destructor THomeThread.Destroy;
begin
  FGenres.Free;
  FCharts.Free;
  FStreams.Free;

  inherited;
end;

procedure THomeThread.DoEnded;
begin
  inherited;

end;

procedure THomeThread.DoErrorReceived(CommandHeader: TCommandHeader;
  Command: TCommandErrorResponse);
begin
  FErrorID := TCommErrors(Command.ErrorID);
  FErrorMsg := Command.ErrorMsg;

  if Assigned(FOnErrorReceived) then
    Sync(FOnErrorReceived);
end;

procedure THomeThread.DoException(E: Exception);
begin
  inherited;

  // TODO: bei ner exception hammert der client massiv auf den server. der muss immer etwas warten. gleichzeitig muss in statusleiste stehen "Verbinde..."
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
  LogIn: TCommandLogInResponse absolute Command;
  LogOut: TCommandLogOutResponse absolute Command;
  GetServerData: TCommandGetServerDataResponse absolute Command;
  NetworkTitleChanged: TCommandNetworkTitleChangedResponse absolute Command;
  ServerInfo: TCommandServerInfoResponse absolute Command;
  Error: TCommandErrorResponse absolute Command;
  MS: TExtendedStream;
  Count: Cardinal;
  i: Integer;
begin
  inherited;

  case CommandHeader.CommandType of
    ctHandshakeResponse: ;
    ctLoginResponse:
      DoLogInReceived(CommandHeader, LogIn);
    ctLogout: ;
    ctLogoutResponse:
      DoLogOutReceived(CommandHeader, LogOut);
    ctGetServerDataResponse:
      DoServerDataReceived(CommandHeader, GetServerData);
    ctNetworkTitleChangedResponse:
      DoNetworkTitleChanged(CommandHeader, NetworkTitleChanged);
    ctServerInfoResponse:
      DoServerInfoReceived(CommandHeader, ServerInfo);
    ctErrorResponse:
      DoErrorReceived(CommandHeader, Error);
  end;
end;

procedure THomeThread.DoServerDataReceived(CommandHeader: TCommandHeader;
  Command: TCommandGetServerDataResponse);
var
  i: Integer;
  Count: Cardinal;
  Stream: TExtendedStream;
begin
  try
    Stream := TExtendedStream(Command.Stream);

    Stream.Read(Count);
    for i := 0 to Count - 1 do
      FGenres.Add(TGenre.LoadFromHome(Stream, CommandHeader.Version));

    Stream.Read(Count);
    for i := 0 to Count - 1 do
      FStreams.Add(TStreamBrowserEntry.LoadFromHome(Stream, CommandHeader.Version));
    FStreams.CreateDict;

    Stream.Read(Count);
    for i := 0 to Count - 1 do
      FCharts.Add(TChartEntry.LoadFromHome(Stream, nil, CommandHeader.Version, FStreams));

    if Assigned(FOnServerDataReceived) then
      Sync(FOnServerDataReceived);
  finally
    FStreams.ClearDict;
    FGenres.Clear;
    FCharts.Clear;
    FStreams.Clear;
  end;
end;

procedure THomeThread.DoServerInfoReceived(CommandHeader: TCommandHeader;
  Command: TCommandServerInfoResponse);
begin
  FServerInfoClientCount := Command.ClientCount;
  FServerInfoRecordingCount := Command.RecordingCount;

  if Assigned(FOnServerInfoReceived) then
    Sync(FOnServerInfoReceived);
end;

{ THomeCommunication }

procedure THomeCommunication.HomeThreadBytesTransferred(Sender: TObject;
  Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader;
  Transferred: UInt64);
begin
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Sender, Direction, CommandID, CommandHeader, Transferred);
end;

constructor THomeCommunication.Create;
begin
  inherited;

  //FTitleNotificationsEnabled := False; todo
end;

destructor THomeCommunication.Destroy;
begin

  inherited;
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

procedure THomeCommunication.SendSetSettings(TitleNotifications: Boolean);
begin
  if not FConnected then
    Exit;

  FThread.SendCommand(TCommandSetSettings.Create(TitleNotifications));
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
  // TODO: bei bedarf erweitern!
  FOnStateChanged := nil;
  FOnBytesTransferred := nil;
  FOnLogInReceived := nil;
  FOnLogOutReceived := nil;
  FOnStreamsReceived := nil;
  FOnChartsReceived := nil;
  FOnNetworkTitleChangedReceived := nil;
  FOnErrorReceived := nil;

  if FThread <> nil then
    FThread.Terminate;
end;

function THomeCommunication.SendGetServerData: Boolean;
begin
  Result := True;
  if not FConnected then
    Exit(False);

  FThread.SendCommand(TCommandGetServerData.Create)
end;

procedure THomeCommunication.HomeThreadConnected(Sender: TSocketThread);
begin
  inherited;

  FConnected := True;

  SendHandshake;

  if AppGlobals.UserWasSetup and (AppGlobals.User <> '') and (AppGlobals.Pass <> '') then
    SendLogIn(AppGlobals.User, AppGlobals.Pass);

  FWasConnected := False;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
  FWasConnected := True;
end;

procedure THomeCommunication.HomeThreadEnded(Sender: TSocketThread);
begin
  FConnected := False;
  FAuthenticated := False;
  FIsAdmin := False;
  FTitleNotificationsSet := False;
  FThread := nil;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);

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

procedure THomeCommunication.HomeThreadServerDataReceived(Sender: TSocketThread);
begin
  if Assigned(FOnStreamsReceived) then
    FOnStreamsReceived(Self, FThread.FGenres, FThread.FStreams);

  if Assigned(FOnChartsReceived) then
    FOnChartsReceived(Self, FThread.FCharts);
end;

procedure THomeCommunication.HomeThreadServerInfoReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnServerInfoReceived) then
    FOnServerInfoReceived(Self, THomeThread(Sender).FServerInfoClientCount, THomeThread(Sender).FServerInfoRecordingCount);
end;

procedure THomeCommunication.HomeThreadNetworkTitleChangedReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnNetworkTitleChangedReceived) then
    FOnNetworkTitleChangedReceived(Self,  THomeThread(Sender).FNetworkTitleChanged.StreamID, THomeThread(Sender).FNetworkTitleChanged.StreamName,
      THomeThread(Sender).FNetworkTitleChanged.Title, THomeThread(Sender).FNetworkTitleChanged.CurrentURL,
      THomeThread(Sender).FNetworkTitleChanged.TitleRegEx, THomeThread(Sender).FNetworkTitleChanged.Format,
      THomeThread(Sender).FNetworkTitleChanged.Bitrate);
end;

procedure THomeCommunication.SendHandshake;
var
  Cmd: TCommandHandshake;
begin
  if not Connected then
    Exit;

  Cmd := TCommandHandshake.Create;
  Cmd.VersionMajor := AppGlobals.AppVersion.Major;
  Cmd.VersionMinor := AppGlobals.AppVersion.Minor;
  Cmd.VersionRevision := AppGlobals.AppVersion.Revision;
  Cmd.VersionBuild := AppGlobals.AppVersion.Build;
  Cmd.Build := AppGlobals.BuildNumber;
  Cmd.Language := AppGlobals.Language;
  Cmd.ProtoVersion := 1;

  FThread.SendCommand(Cmd);
end;

procedure THomeCommunication.Connect;
begin
  if FThread <> nil then
    Exit;

  FThread := THomeThread.Create;
  FThread.OnConnected := HomeThreadConnected;
  FThread.OnEnded := HomeThreadEnded;
  FThread.OnBytesTransferred := HomeThreadBytesTransferred;

  FThread.OnLogInReceived := HomeThreadLogInReceived;
  FThread.OnLogOutReceived := HomeThreadLogOutReceived;
  FThread.OnServerDataReceived := HomeThreadServerDataReceived;

  FThread.OnServerInfoReceived := HomeThreadServerInfoReceived;
  FThread.OnErrorReceived := HomeThreadErrorReceived;

  FThread.OnNetworkTitleChangedReceived := HomeThreadNetworkTitleChangedReceived;

  FThread.Start;
end;

initialization
  TCommand.RegisterCommand(ctHandshakeResponse, TCommandHandshakeResponse);
  TCommand.RegisterCommand(ctLogInResponse, TCommandLogInResponse);
  TCommand.RegisterCommand(ctLogOutResponse, TCommandLogOutResponse);
  TCommand.RegisterCommand(ctGetServerDataResponse, TCommandGetServerDataResponse);
  TCommand.RegisterCommand(ctServerInfoResponse, TCommandServerInfoResponse);
  TCommand.RegisterCommand(ctErrorResponse, TCommandErrorResponse);
  TCommand.RegisterCommand(ctNetworkTitleChangedResponse, TCommandNetworkTitleChangedResponse);

  HomeComm := THomeCommunication.Create;

finalization
  HomeComm.Free;

end.
