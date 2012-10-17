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
  Windows, SysUtils, Classes, Functions, HTTPThread, Base64, XMLLib,
  StrUtils, Generics.Collections, Sockets, WinSock, Int32Protocol,
  Logging, ZLib, DataManager, AudioFunctions;

type
  TStreamInfo = record
    ID: Integer;
    Name: string;
    Genre: string;
    URL: string;
    Website: string;
    BitRate: Integer;
    StreamType: string;
    Downloads: Integer;
    MetaData: Boolean;
    ChangesTitleInSong: Boolean;
    Rating: Integer;
    RecordingOkay: Boolean;
    RegEx: string;
  end;
  TStreamInfoArray = array of TStreamInfo;

  TCommErrors = (ceUnknown, ceAuthRequired, ceNotification, ceOneTimeNotification);

  THomeThread = class(TInt32SocketThread)
  private
    FDataLists: TDataLists;

    FAuthAuthenticated: Boolean;
    FIsAdmin: Boolean;
    FCharts: TList<TChartEntry>;
    FChartCategories: TList<TChartCategory>;
    FChartGenres: TList<TGenre>;
    FStreams: TList<TStreamBrowserEntry>;
    FGenres: TList<TGenre>;

    FChangedStreamID: Cardinal;
    FChangedStreamName: string;
    FChangedTitle: string;
    FChangedCurrentURL: string;
    FChangedKbps: Cardinal;
    FChangedFormat: string;
    FChangedTitlePattern: string;

    FServerInfoClientCount: Cardinal;
    FServerInfoRecordingCount: Cardinal;

    FErrorID: TCommErrors;
    FErrorMsg: string;

    FOnLoggedOn: TSocketEvent;
    FOnLoggedOff: TSocketEvent;
    FOnStreamsReceived: TSocketEvent;
    FOnChartsReceived: TSocketEvent;
    FOnChartGenresReceived: TSocketEvent;
    FOnTitleChanged: TSocketEvent;
    FOnServerInfo: TSocketEvent;
    FOnError: TSocketEvent;

    function XMLGet(T: string): TXMLLib;
    function ZDecompressStr(const s: AnsiString): AnsiString;
  protected
    procedure DoStuff; override;
    procedure DoConnected; override;
    procedure DoReceivedString(D: AnsiString); override;
    procedure DoLoggedOn(Version: Integer; Header, Data: TXMLNode);
    procedure DoLoggedOff(Version: Integer; Header, Data: TXMLNode);
    procedure DoStreamsReceived(Version: Integer; Header, Data: TXMLNode);
    procedure DoChartsReceived(Version: Integer; Header, Data: TXMLNode);
    procedure DoTitleChanged(Version: Integer; Header, Data: TXMLNode);
    procedure DoServerInfo(Version: Integer; Header, Data: TXMLNode);
    procedure DoError(Version: Integer; Header, Data: TXMLNode);
    procedure DoEnded; override;
  public
    constructor Create(DataLists: TDataLists);
    destructor Destroy; override;

    property OnLoggedOn: TSocketEvent read FOnLoggedOn write FOnLoggedOn;
    property OnLoggedOff: TSocketEvent read FOnLoggedOff write FOnLoggedOff;
    property OnStreamsReceived: TSocketEvent read FOnStreamsReceived write FOnStreamsReceived;
    property OnChartsReceived: TSocketEvent read FOnChartsReceived write FOnChartsReceived;
    property OnChartGenresReceived: TSocketEvent read FOnChartGenresReceived write FOnChartGenresReceived;
    property OnTitleChanged: TSocketEvent read FOnTitleChanged write FOnTitleChanged;
    property OnServerInfo: TSocketEvent read FOnServerInfo write FOnServerInfo;
    property OnError: TSocketEvent read FOnError write FOnError;
  end;

  TBooleanEvent = procedure(Sender: TObject; Value: Boolean) of object;
  TTitleChangedEvent = procedure(Sender: TObject; ID: Cardinal; Name, Title, CurrentURL, Format, TitlePattern: string; Kbps: Cardinal) of object;
  TServerInfoEvent = procedure(Sender: TObject; ClientCount, RecordingCount: Cardinal) of object;
  TErrorEvent = procedure(Sender: TObject; ID: TCommErrors; Msg: string) of object;
  TStreamsReceivedEvent = procedure(Sender: TObject; Genres: TList<TGenre>; Streams: TList<TStreamBrowserEntry>) of object;
  TChartsReceivedEvent = procedure(Sender: TObject; Categories: TList<TChartCategory>; Charts: TList<TChartEntry>) of object;
  TChartGenresReceivedEvent = procedure(Sender: TObject; List: TList<TGenre>) of object;

  THomeCommunication = class
  private
    FLastUpdateXML: string;
    FClient: THomeThread;
    FConnected: Boolean;
    FWasConnected: Boolean;
    FAuthenticated: Boolean;
    FTitleNotificationsEnabled: Boolean;
    FTitleNotificationsSet: Boolean;
    FIsAdmin: Boolean;

    FOnStreamsReceived: TStreamsReceivedEvent;
    FOnChartsReceived: TChartsReceivedEvent;
    FOnChartGenresReceived: TChartGenresReceivedEvent;
    FOnTitleChanged: TTitleChangedEvent;
    FOnServerInfo: TServerInfoEvent;
    FOnError: TErrorEvent;
    FOnStateChanged: TNotifyEvent;

    procedure ClientConnected(Sender: TSocketThread);
    procedure ClientEnded(Sender: TSocketThread);
    procedure ClientLoggedOn(Sender: TSocketThread);
    procedure ClientLoggedOff(Sender: TSocketThread);
    procedure ClientStreamsReceived(Sender: TSocketThread);
    procedure ClientChartsReceived(Sender: TSocketThread);
    procedure ClientChartGenresReceived(Sender: TSocketThread);
    procedure ClientTitleChanged(Sender: TSocketThread);
    procedure ClientServerInfo(Sender: TSocketThread);
    procedure ClientError(Sender: TSocketThread);

    function ZCompressStr(const s: AnsiString): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure SubmitStream(Stream: string);
    function GetStreams: Boolean;
    function GetCharts: Boolean;

    procedure LogOn(User, Pass: string);
    procedure LogOff;
    procedure TitleChanged(ID: Cardinal; StreamName, Title, CurrentURL, URL, Format: string; Kbps: Cardinal; URLs: TStringList);
    procedure SendClientInfo;
    procedure UpdateStats(List: TList<Cardinal>; RecordingCount: Cardinal);
    procedure RateStream(ID, Rating: Integer);
    procedure SetDataOkay(ID: Integer; RecordingOkay: Boolean); overload;
    procedure SetDataTitlePattern(ID: Integer; RegEx: string); overload;
    procedure SetDataIgnoreTracks(ID: Integer; Lst: TStringList); overload;
    procedure SetTitleNotifications(Enable: Boolean);
    procedure RebuildIndex;
    procedure SendClientStats(Auto: Boolean);

    procedure Terminate;

    property Connected: Boolean read FConnected;
    property WasConnected: Boolean read FWasConnected;
    property Authenticated: Boolean read FAuthenticated;
    property IsAdmin: Boolean read FIsAdmin;
    property OnStreamsReceived: TStreamsReceivedEvent read FOnStreamsReceived write FOnStreamsReceived;
    property OnChartsReceived: TChartsReceivedEvent read FOnChartsReceived write FOnChartsReceived;
    property OnChartGenresReceived: TChartGenresReceivedEvent read FOnChartGenresReceived write FOnChartGenresReceived;
    property OnTitleChanged: TTitleChangedEvent read FOnTitleChanged write FOnTitleChanged;
    property OnServerInfo: TServerInfoEvent read FOnServerInfo write FOnServerInfo;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

var
  HomeComm: THomeCommunication;

implementation

uses
  AppData;

{ THomeCommunication }

procedure THomeCommunication.ClientServerInfo(Sender: TSocketThread);
begin
  if Assigned(FOnServerInfo) then
    FOnServerInfo(Self, THomeThread(Sender).FServerInfoClientCount, THomeThread(Sender).FServerInfoRecordingCount);

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure THomeCommunication.ClientStreamsReceived(Sender: TSocketThread);
begin
  if Assigned(FOnStreamsReceived) then
    FOnStreamsReceived(Self, THomeThread(Sender).FGenres, THomeThread(Sender).FStreams);
end;

procedure THomeCommunication.ClientTitleChanged(Sender: TSocketThread);
begin
  if Assigned(FOnTitleChanged) then
    FOnTitleChanged(Self, THomeThread(Sender).FChangedStreamID, THomeThread(Sender).FChangedStreamName,
      THomeThread(Sender).FChangedTitle, THomeThread(Sender).FChangedCurrentURL,
      THomeThread(Sender).FChangedFormat, THomeThread(Sender).FChangedTitlePattern,
      THomeThread(Sender).FChangedKbps);
end;

procedure THomeCommunication.ClientLoggedOn(Sender: TSocketThread);
begin
  FAuthenticated := THomeThread(Sender).FAuthAuthenticated;
  FIsAdmin := THomeThread(Sender).FIsAdmin;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure THomeCommunication.ClientLoggedOff(Sender: TSocketThread);
begin
  FAuthenticated := THomeThread(Sender).FAuthAuthenticated;
  FIsAdmin := THomeThread(Sender).FIsAdmin;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure THomeCommunication.Connect;
begin
  if FClient <> nil then
    Exit;

  FClient := THomeThread.Create(FDataLists);
  FClient.OnConnected := ClientConnected;
  FClient.OnLoggedOn := ClientLoggedOn;
  FClient.OnLoggedOff := ClientLoggedOff;
  FClient.OnStreamsReceived := ClientStreamsReceived;
  FClient.OnChartsReceived := ClientChartsReceived;
  FClient.OnChartGenresReceived := ClientChartGenresReceived;
  FClient.OnTitleChanged := ClientTitleChanged;
  FClient.OnServerInfo := ClientServerInfo;
  FClient.OnError := ClientError;
  FClient.OnEnded := ClientEnded;
  FClient.Resume;
end;

constructor THomeCommunication.Create;
begin
  FLastUpdateXML := '';
  FTitleNotificationsEnabled := False;
end;

destructor THomeCommunication.Destroy;
begin
  Terminate;
  inherited;
end;

function THomeCommunication.GetCharts: Boolean;
var
  XMLDocument: TXMLLib;
  XML: AnsiString;
begin
  Result := False;
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('getcharts');
  try
    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
    Result := True;
  finally
    XMLDocument.Free;
  end;
end;

function THomeCommunication.GetStreams;
var
  XMLDocument: TXMLLib;
  XML: AnsiString;
begin
  Result := False;
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('getstreams');
  try
    XMLDocument.SaveToString(XML);

    FClient.Write(ZCompressStr(XML));
    Result := True;
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.RateStream(ID, Rating: Integer);
var
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('ratestream');
  try
    Data := XMLDocument.Root.GetNode('data');

    Node := TXMLNode.Create(Data);
    Node.Name := 'id';
    Node.Value.AsInteger := ID;
    Node := TXMLNode.Create(Data);
    Node.Name := 'rating';
    Node.Value.AsInteger := Rating;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.RebuildIndex;
var
  XMLDocument: TXMLLib;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('rebuildindex');
  try
    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.SendClientInfo;
var
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
  XML: AnsiString;
begin
  if not Connected then
    Exit;

  XMLDocument := FClient.XMLGet('clientinfo');
  try
    Data := XMLDocument.Root.GetNode('data');

    Node := TXMLNode.Create(Data);
    Node.Name := 'id';
    Node.Value.AsInteger := AppGlobals.ID;

    Node := TXMLNode.Create(Data);
    Node.Name := 'version';
    Node.Value.AsString := AppGlobals.AppVersion.AsString;

    Node := TXMLNode.Create(Data);
    Node.Name := 'build';
    Node.Value.AsInteger := AppGlobals.BuildNumber;

    Node := TXMLNode.Create(Data);
    Node.Name := 'language';
    Node.Value.AsString := AppGlobals.Language;

    Node := TXmlNode.Create(Data);
    Node.Name := 'compression';
    Node.Value.AsBoolean := True;

    Node := TXmlNode.Create(Data);
    Node.Name := 'protoversion';
    Node.Value.AsInteger := 2;

    XMLDocument.SaveToString(XML);
    FClient.Write(XML);
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.SendClientStats(Auto: Boolean);
var
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
  XML: AnsiString;
begin
  if not Connected then
    Exit;

  XMLDocument := FClient.XMLGet('clientstats');
  try
    Data := XMLDocument.Root.GetNode('data');

    Node := TXMLNode.Create(Data);
    if Auto then
      Node.Name := 'autosaved'
    else
      Node.Name := 'saved';
    Node.Value.AsInteger := 1;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.SetDataOkay(ID: Integer; RecordingOkay: Boolean);
var
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('setdata');
  try
    Data := XMLDocument.Root.GetNode('data');

    Node := TXMLNode.Create(Data);
    Node.Name := 'id';
    Node.Value.AsInteger := ID;

    Node := TXMLNode.Create(Data);
    Node.Name := 'recordingokay';
    Node.Value.AsBoolean := RecordingOkay;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.SetDataTitlePattern(ID: Integer; RegEx: string);
var
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('setdata');
  try
    Data := XMLDocument.Root.GetNode('data');

    Node := TXMLNode.Create(Data);
    Node.Name := 'id';
    Node.Value.AsInteger := ID;

    Node := TXMLNode.Create(Data);
    Node.Name := 'regex';
    Node.Value.AsString := RegEx;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.SetDataIgnoreTracks(ID: Integer; Lst: TStringList);
var
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('setdata');
  try
    Data := XMLDocument.Root.GetNode('data');

    Node := TXMLNode.Create(Data);
    Node.Name := 'id';
    Node.Value.AsInteger := ID;

    Node := TXMLNode.Create(Data);
    Node.Name := 'ignoretitles';
    Node.Value.AsString := Lst.Text;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.SetTitleNotifications(Enable: Boolean);
var
  XMLDocument: TXMLLib;
  Data: TXMLNode;
  Attr: TXMLAttribute;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  if (Enable = FTitleNotificationsEnabled) and FTitleNotificationsSet then
    Exit;

  FTitleNotificationsEnabled := Enable;
  FTitleNotificationsSet := True;

  XMLDocument := FClient.XMLGet('settitlenotifications');
  try
    Data := XMLDocument.Root.GetNode('data');

    Attr := Data.Attributes.Add;
    Attr.Name := 'enable';
    Attr.Value.AsBoolean := Enable;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.SubmitStream(Stream: string);
var
  XMLDocument: TXMLLib;
  Data: TXMLNode;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('submitstream');
  try
    if Stream[Length(Stream)] = '/' then
      Stream := Copy(Stream, 1, Length(Stream) - 1);

    Data := XMLDocument.Root.GetNode('data');
    Data.Value.AsString := Stream;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.Terminate;
begin
  FOnStreamsReceived := nil;
  FOnTitleChanged := nil;
  FOnServerInfo := nil;
  FOnError := nil;
  FOnStateChanged := nil;

  if FClient <> nil then
    FClient.Terminate;
end;

procedure THomeCommunication.LogOn(User, Pass: string);
var
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('logon');
  try
    Data := XMLDocument.Root.GetNode('data');

    Node := TXMLNode.Create(Data);
    Node.Name := 'user';
    //Node.Value.UseCDATA := False;
    Node.Value.AsString := User;

    Node := TXMLNode.Create(Data);
    Node.Name := 'pass';
    //Node.Value.UseCDATA := False;
    Node.Value.AsString := Pass;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.ClientChartGenresReceived(
  Sender: TSocketThread);
begin
  if Assigned(FOnChartGenresReceived) then
    FOnChartGenresReceived(Self, FClient.FChartGenres);
end;

procedure THomeCommunication.ClientChartsReceived(Sender: TSocketThread);
begin
  if Assigned(FOnChartsReceived) then
    FOnChartsReceived(Self, FClient.FChartCategories, FClient.FCharts);
end;

procedure THomeCommunication.ClientConnected(Sender: TSocketThread);
begin
  FConnected := True;
  FLastUpdateXML := '';

  SendClientInfo;

  if AppGlobals.UserWasSetup and (AppGlobals.User <> '') and (AppGlobals.Pass <> '') then
    LogOn(AppGlobals.User, AppGlobals.Pass);

  FWasConnected := False;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
  FWasConnected := True;
end;

procedure THomeCommunication.ClientEnded(Sender: TSocketThread);
var
  Thread: THomeThread;
begin
  FConnected := False;
  FAuthenticated := False;
  FIsAdmin := False;
  FTitleNotificationsSet := False;
  FClient := nil;
  Thread := THomeThread(Sender);

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);

  if Thread.Terminated then
    Exit;

  Connect;
end;

procedure THomeCommunication.ClientError(Sender: TSocketThread);
begin
  if Assigned(FOnError) then
    FOnError(Self, FClient.FErrorID, FClient.FErrorMsg);
end;

procedure THomeCommunication.TitleChanged(ID: Cardinal; StreamName, Title, CurrentURL, URL, Format: string;
  Kbps: Cardinal; URLs: TStringList);
var
  i: Integer;
  XMLDocument: TXMLLib;
  N, N2, N3: TXMLNode;
  XML: AnsiString;
  TmpURL: AnsiString;
begin
  if not FConnected then
    Exit;

  if Trim(Title) = '' then
    Exit;

  XMLDocument := FClient.XMLGet('fulltitlechange');
  try
    N := XMLDocument.Root.Nodes.GetNode('data');

    N2 := TXMLNode.Create(N);
    N2.Name := 'id';
    N2.Value.AsLongWord := ID;
    N2 := TXMLNode.Create(N);
    N2.Name := 'streamname';
    N2.Value.AsString := StreamName;
    N2 := TXMLNode.Create(N);
    N2.Name := 'title';
    N2.Value.AsString := Title;
    N2 := TXMLNode.Create(N);
    N2.Name := 'currenturl';
    N2.Value.AsString := CurrentURL;
    N2 := TXMLNode.Create(N);
    N2.Name := 'url';
    N2.Value.AsString := URL;

    N2 := TXMLNode.Create(N);
    N2.Name := 'format';
    N2.Value.AsString := Format;
    N2 := TXMLNode.Create(N);
    N2.Name := 'kbps';
    N2.Value.AsLongWord := Kbps;

    if URLs <> nil then
    begin
      N3 := TXMLNode.Create(N);
      N3.Name := 'urls';
      for i := 0 to URLs.Count - 1 do
      begin
        N2 := TXMLNode.Create(N3);
        N2.Name := 'url';
        TmpURL := AnsiString(URLs[i]);
        if TmpURL[Length(TmpURL)] = '/' then
          TmpURL := Copy(TmpURL, 1, Length(TmpURL) - 1);
        N2.Value.AsString := TmpURL;
      end;
    end;

    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.LogOff;
var
  XMLDocument: TXMLLib;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('logoff');
  try
    XMLDocument.SaveToString(XML);
    FClient.Write(ZCompressStr(XML));
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.UpdateStats(List: TList<Cardinal>; RecordingCount: Cardinal);
var
  i: Integer;
  XMLDocument: TXMLLib;
  Data, Data2: TXMLNode;
  XML: AnsiString;
begin
  if not FConnected then
    Exit;

  XMLDocument := FClient.XMLGet('updatestats');
  try
    XMLDocument.Root.GetNode('header').Attributes.AttributeByName['version'].Value.AsString := '2';

    Data := XMLDocument.Root.GetNode('data');

    for i := 0 to List.Count - 1 do
    begin
      Data2 := TXMLNode.Create(Data);
      Data2.Name := 'stream';
      Data2.Value.AsLongWord := List[i];
    end;

    Data2 := TXMLNode.Create(Data);
    Data2.Name := 'unknowncount';
    Data2.Value.AsLongWord := RecordingCount;

    XMLDocument.SaveToString(XML);
    if FLastUpdateXML <> XML then
      FClient.Write(ZCompressStr(XML));
    FLastUpdateXML := XML;
  finally
    XMLDocument.Free;
  end;
end;

function THomeCommunication.ZCompressStr(const s: AnsiString): AnsiString;
var
  buffer: Pointer;
  size: Integer;
begin
  ZCompress(PAnsiChar(s), Length(s), buffer, size, zcMax);
  SetLength(result, size);
  Move(buffer^, pointer(result)^, size);
  FreeMem(buffer);
end;

{ THomeThread }

constructor THomeThread.Create(DataLists: TDataLists);
begin
  {$IFDEF DEBUG}
  //inherited Create('gaia', 8007);
  inherited Create('streamwriter.org', 8007);
  {$ELSE}
  inherited Create('streamwriter.org', 8007);
  {$ENDIF}

  UseSynchronize := True;
  FDataLists := DataLists;
  FCharts := TList<TChartEntry>.Create;
  FChartGenres := TList<TGenre>.Create;
  FChartCategories := TList<TChartCategory>.Create;
  FStreams := TList<TStreamBrowserEntry>.Create;
  FGenres := TList<TGenre>.Create;
end;

destructor THomeThread.Destroy;
var
  i: Integer;
begin
  FCharts.Free;
  FChartGenres.Free;

  for i := 0 to FChartCategories.Count - 1 do
    FChartCategories[i].Free;
  FChartCategories.Free;

  for i := 0 to FStreams.Count - 1 do
    FStreams[i].Free;
  FStreams.Free;

  for i := 0 to FGenres.Count - 1 do
    FGenres[i].Free;
  FGenres.Free;

  inherited;
end;

procedure THomeThread.DoLoggedOn(Version: Integer; Header, Data: TXMLNode);
begin
  FAuthAuthenticated := Data.Nodes.GetNode('success').Value.AsBoolean;
  FIsAdmin := Data.Nodes.GetNode('isadmin').Value.AsBoolean;
  if Assigned(FOnLoggedOn) then
    Sync(FOnLoggedOn);
end;

procedure THomeThread.DoChartsReceived(Version: Integer; Header,
  Data: TXMLNode);
var
  i: Integer;
  Node, Node2: TXMLNode;
  Categories: TIntArray;
begin
  for Node in Data.Nodes.GetNode('categories').Nodes do
    FChartCategories.Add(TChartCategory.Create(Node.Attributes.AttributeByName['id'].Value.AsLongWord, Node.Value.AsString));

  for Node in Data.Nodes.GetNode('charts').Nodes do
  begin
    SetLength(Categories, 0);
    for Node2 in Node.Nodes do
    begin
      if Node2.Name = 'cat' then
      begin
        SetLength(Categories, Length(Categories) + 1);
        Categories[High(Categories)] := Node2.Value.AsInteger;
      end;
    end;

    FCharts.Add(TChartEntry.Create(Node.Value.AsString, Node.Attributes.AttributeByName['chance'].Value.AsInteger, Categories));
  end;

  if Assigned(FOnChartsReceived) then
    Sync(FOnChartsReceived);

  for i := 0 to FChartCategories.Count - 1 do
    FChartCategories[i].Free;
  for i := 0 to FCharts.Count - 1 do
    FCharts[i].Free;
  FChartCategories.Clear;
  FCharts.Clear;
end;

procedure THomeThread.DoConnected;
begin
  inherited;

end;

procedure THomeThread.DoEnded;
begin
  inherited;

  Sleep(1000);
end;

procedure THomeThread.DoError(Version: Integer; Header, Data: TXMLNode);
begin
  FErrorID := TCommErrors(Data.Nodes.GetNode('id').Value.AsInteger);
  FErrorMsg := Data.Nodes.GetNode('message').Value.AsString;
  if Assigned(FOnError) then
    Sync(FOnError);
end;

procedure THomeThread.DoReceivedString(D: AnsiString);
var
  Version: Integer;
  XMLDocument: TXMLLib;
  Header, Data: TXMLNode;
  T: string;
begin
  inherited;

  D := ZDecompressStr(D);

  try
    XMLDocument := TXMLLib.Create;
    try
      XMLDocument.LoadFromString(D);

      Header := XMLDocument.Root.Nodes.GetNode('header');
      Data := XMLDocument.Root.Nodes.GetNode('data');

      Version := Header.Attributes.AttributeByName['version'].Value.AsInteger;
      T := Header.Attributes.AttributeByName['type'].Value.AsString;

      if Header.Attributes.AttributeByName['type'].Value.AsString = 'logon' then
      begin
        DoLoggedOn(Version, Header, Data);
      end;

      if Header.Attributes.AttributeByName['type'].Value.AsString = 'logoff' then
      begin
        DoLoggedOff(Version, Header, Data);
      end;

      if Header.Attributes.AttributeByName['type'].Value.AsString = 'getstreams' then
      begin
        DoStreamsReceived(Version, Header, Data);
      end;

      if Header.Attributes.AttributeByName['type'].Value.AsString = 'getcharts' then
      begin
        DoChartsReceived(Version, Header, Data);
      end;

      if Header.Attributes.AttributeByName['type'].Value.AsString = 'fulltitlechange' then
      begin
        DoTitleChanged(Version, Header, Data);
      end;

      if Header.Attributes.AttributeByName['type'].Value.AsString = 'serverinfo' then
      begin
        DoServerInfo(Version, Header, Data);
      end;

      if Header.Attributes.AttributeByName['type'].Value.AsString = 'error' then
      begin
        DoError(Version, Header, Data);
      end;
    finally
      XMLDocument.Free;
    end;
  except
    raise Exception.Create('Invalid data received');
  end;
end;

procedure THomeThread.DoServerInfo(Version: Integer; Header,
  Data: TXMLNode);
begin
  FServerInfoClientCount := Data.Nodes.GetNode('clientcount').Value.AsLongWord;
  FServerInfoRecordingCount := Data.Nodes.GetNode('recordingcount').Value.AsLongWord;

  if Assigned(FOnServerInfo) then
    Sync(FOnServerInfo);
end;

procedure THomeThread.DoStreamsReceived(Version: Integer; Header, Data: TXMLNode);
var
  i: Integer;
  T: string;
  Node: TXMLNode;
  Genre: TGenre;
  Entry: TStreamBrowserEntry;
begin
  for Node in Data.Nodes.GetNode('genres').Nodes do
  begin
    Genre := TGenre.Create;
    Genre.ID := Node.Attributes.AttributeByName['id'].Value.AsLongWord;
    Genre.Name := Node.Value.AsString;
    Genre.ChartCount := Node.Attributes.AttributeByName['chartcount'].Value.AsLongWord;
    FGenres.Add(Genre);
  end;

  for Node in Data.Nodes.GetNode('streams').Nodes do
  begin
    Entry := TStreamBrowserEntry.Create;
    FStreams.Add(Entry);
    Entry.ID := Node.Attributes.AttributeByName['id'].Value.AsInteger;
    Entry.Name := Node.Attributes.AttributeByName['name'].Value.AsString;
    Entry.Genre := Node.Attributes.AttributeByName['genre'].Value.AsString;
    Entry.URL := Node.Value.AsString;
    Entry.Website := Node.Attributes.AttributeByName['website'].Value.AsString;
    Entry.BitRate := Node.Attributes.AttributeByName['bitrate'].Value.AsInteger;
    T := Node.Attributes.AttributeByName['type'].Value.AsString;
    if T = 'mpeg' then
      Entry.AudioType := atMPEG
    else if T = 'aacp' then
      Entry.AudioType := atAAC
    else
      Entry.AudioType := atNone;
    Entry.MetaData := Node.Attributes.AttributeByName['metadata'].Value.AsBoolean;
    Entry.ChangesTitleInSong := Node.Attributes.AttributeByName['changestitleinsong'].Value.AsBoolean;
    Entry.Rating := Node.Attributes.AttributeByName['rating'].Value.AsInteger;
    Entry.RecordingOkay := Node.Attributes.AttributeByName['recordingokay'].Value.AsBoolean;
    Entry.RegEx := Node.Attributes.AttributeByName['regex'].Value.AsString;
    Entry.IgnoreTitles.Text := Node.GetNode('ignoretitles').Value.AsString;
  end;

  if Assigned(FOnStreamsReceived) then
    Sync(FOnStreamsReceived);

  for i := 0 to FGenres.Count - 1 do
    FGenres[i].Free;
  for i := 0 to FStreams.Count - 1 do
    FStreams[i].Free;
  FGenres.Clear;
  FStreams.Clear;
end;

procedure THomeThread.DoStuff;
begin
  inherited;

  if (FLastTimeReceived > 0) and (FLastTimeReceived < GetTickCount - 40000) then
    raise Exception.Create('No data/ping received');
end;

procedure THomeThread.DoTitleChanged(Version: Integer; Header, Data: TXMLNode);
begin
  if not AppGlobals.AutoTuneIn then
    Exit;

  FChangedStreamID := Data.Nodes.GetNode('id').Value.AsLongWord;
  FChangedStreamName := Trim(Data.Nodes.GetNode('streamname').Value.AsString);
  FChangedTitle := Data.Nodes.GetNode('title').Value.AsString;
  FChangedCurrentURL := Data.Nodes.GetNode('currenturl').Value.AsString;
  FChangedKbps := Data.Nodes.GetNode('kbps').Value.AsLongWord;
  FChangedFormat := Data.Nodes.GetNode('format').Value.AsString;
  FChangedTitlePattern := Data.Nodes.GetNode('regex').Value.AsString;

  if (FChangedStreamName <> '') and (FChangedTitle <> '') and (FChangedCurrentURL <> '') then
    if Assigned(FOnTitleChanged) then
      Sync(FOnTitleChanged);
end;

procedure THomeThread.DoLoggedOff(Version: Integer; Header,
  Data: TXMLNode);
begin
  FAuthAuthenticated := False;
  FIsAdmin := False;
  if Assigned(FOnLoggedOff) then
    Sync(FOnLoggedOff);
end;

function THomeThread.XMLGet(T: string): TXMLLib;
var
  Root, Header, Data: TXMLNode;
begin
  Result := TXMLLib.Create;
  Result.Options.WriteStandardEOL := True;
  Result.Options.OutputLevelCharIndent := 0;

  Root := TXMLNode.Create();
  Root.Name := 'request';
  Result.Root := Root;

  Header := TXMLNode.Create(Root);
  Header.Name := 'header';
  Header.Attributes.SimpleAdd('version', '1');
  Header.Attributes.SimpleAdd('type', T);

  Data := TXMLNode.Create(Root);
  Data.Name := 'data';
end;

function THomeThread.ZDecompressStr(const s: AnsiString): AnsiString;
var
  buffer: Pointer;
  size: Integer;
begin
  ZDecompress(Pointer(s), Length(s), buffer, size);
  SetLength(result, size);
  Move(buffer^, pointer(result)^, size);
  FreeMem(buffer);
end;

initialization
  HomeComm := THomeCommunication.Create;

finalization
  HomeComm.Free;

end.
