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
unit HomeCommunication;

interface

uses
  Windows, SysUtils, Classes, Functions, HTTPThread, Base64, XMLLib,
  StrUtils, Generics.Collections, Sockets, WinSock, Int32Protocol;

type
  TStreamInfo = record
    Name: string;
    Genre: string;
    URL: string;
    Website: string;
    BitRate: Integer;
    StreamType: string;
    Downloads: Integer;
  end;
  TStreamInfoArray = array of TStreamInfo;

  THomeThread = class(TInt32SocketThread)
  private
    FGenres: TStringList;

    FCount: Integer;
    FPacketCount: Integer;
    FStreams: TStreamInfoArray;

    FChangedStreamName: string;
    FChangedTitle: string;
    FChangedCurrentURL: string;

    FServerInfoClientCount: Cardinal;
    FServerInfoRecordingCount: Cardinal;

    FErrorMsg: string;

    FOnGenresReceived: TSocketEvent;
    FOnStreamsReceived: TSocketEvent;
    FOnTitleChanged: TSocketEvent;
    FOnServerInfo: TSocketEvent;
    FOnError: TSocketEvent;

    function XMLGetLogin: AnsiString;
    function XMLGet(T: string): TXMLLib;
  protected
    procedure DoConnected; override;
    procedure DoReceivedString(D: string); override;
    procedure DoGenresReceived(Version: Integer; Header, Data: TXMLNode);
    procedure DoStreamsReceived(Version: Integer; Header, Data: TXMLNode);
    procedure DoTitleChanged(Version: Integer; Header, Data: TXMLNode);
    procedure DoServerInfo(Version: Integer; Header, Data: TXMLNode);
    procedure DoError(Version: Integer; Header, Data: TXMLNode);
    procedure DoEnded; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure TitleChanged(StreamName, Title, CurrentURL, URL: string; URLs: TStringList);
    property Genres: TStringList read FGenres write FGenres;

    property OnGenresReceived: TSocketEvent read FOnGenresReceived write FOnGenresReceived;
    property OnStreamsReceived: TSocketEvent read FOnStreamsReceived write FOnStreamsReceived;
    property OnTitleChanged: TSocketEvent read FOnTitleChanged write FOnTitleChanged;
    property OnServerInfo: TSocketEvent read FOnServerInfo write FOnServerInfo;
    property OnError: TSocketEvent read FOnError write FOnError;
  end;

  TBooleanEvent = procedure(Sender: TObject; Value: Boolean) of object;
  TGenresReceivedEvent = procedure(Sender: TObject; Genres: TStringList) of object;
  TStreamsReceivedEvent = procedure(Sender: TObject; Streams: TStreamInfoArray; Count: Integer) of object;
  TTitleChangedEvent = procedure(Sender: TObject; Name, Title, CurrentURL: string) of object;
  TServerInfoEvent = procedure(Sender: TObject; ClientCount, RecordingCount: Cardinal) of object;
  TErrorEvent = procedure(Sender: TObject; Msg: string) of object;

  THomeCommunication = class
  private
    FClient: THomeThread;
    FConnected: Boolean;

    FOnUserAuthenticated: TBooleanEvent;
    FOnGenresReceived: TGenresReceivedEvent;
    FOnStreamsReceived: TStreamsReceivedEvent;
    FOnTitleChanged: TTitleChangedEvent;
    //FOnReceiveError: TNotifyEvent;
    FOnServerInfo: TServerInfoEvent;
    FOnError: TErrorEvent;
    FOnOldVersion: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;

    procedure ClientConnected(Sender: TSocketThread);
    procedure ClientEnded(Sender: TSocketThread);
    procedure ClientGenresReceived(Sender: TSocketThread);
    procedure ClientStreamsReceived(Sender: TSocketThread);
    procedure ClientTitleChanged(Sender: TSocketThread);
    procedure ClientServerInfo(Sender: TSocketThread);
    procedure ClientError(Sender: TSocketThread);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure SubmitStream(Stream: string);
    function GetGenres: Boolean;
    function GetStreams(Count, Offset: Integer; Search, Genre: string; Kbps: Integer;
      StreamType: string; ReplaceQuery: Boolean): Boolean;

    procedure TitleChanged(StreamName, Title, CurrentURL, URL: string; URLs: TStringList);
    procedure UpdateInfo(RecordingCount: Cardinal);

    procedure Terminate;

    property Connected: Boolean read FConnected;
    property OnUserAuthenticated: TBooleanEvent read FOnUserAuthenticated write FOnUserAuthenticated;
    property OnGenresReceived: TGenresReceivedEvent read FOnGenresReceived write FOnGenresReceived;
    property OnStreamsReceived: TStreamsReceivedEvent read FOnStreamsReceived write FOnStreamsReceived;
    property OnTitleChanged: TTitleChangedEvent read FOnTitleChanged write FOnTitleChanged;
    //property OnReceiveError: TNotifyEvent read FOnReceiveError write FOnReceiveError;
    property OnServerInfo: TServerInfoEvent read FOnServerInfo write FOnServerInfo;
    property OnError: TErrorEvent read FOnError write FOnError;
    //property OnOldVersion: TNotifyEvent read FOnOldVersion write FOnOldVersion;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

var
  HomeComm: THomeCommunication;

implementation

uses
  AppData;

{ TStreamSubmit }

procedure THomeCommunication.ClientGenresReceived(Sender: TSocketThread);
begin
  if Assigned(FOnGenresReceived) then
    FOnGenresReceived(Self, THomeThread(Sender).Genres);
end;

procedure THomeCommunication.ClientServerInfo(Sender: TSocketThread);
begin
  if Assigned(FOnServerInfo) then
    FOnServerInfo(Self, THomeThread(Sender).FServerInfoClientCount, THomeThread(Sender).FServerInfoRecordingCount);
end;

procedure THomeCommunication.ClientStreamsReceived(Sender: TSocketThread);
begin
  if Assigned(FOnStreamsReceived) then
    FOnStreamsReceived(Self, THomeThread(Sender).FStreams, THomeThread(Sender).FCount);
end;

procedure THomeCommunication.ClientTitleChanged(Sender: TSocketThread);
begin
  if Assigned(FOnTitleChanged) then
    FOnTitleChanged(Self, THomeThread(Sender).FChangedStreamName, THomeThread(Sender).FChangedTitle,
      THomeThread(Sender).FChangedCurrentURL);
end;

procedure THomeCommunication.Connect;
begin
  if FClient <> nil then
    Exit;

  FClient := THomeThread.Create;
  FClient.OnConnected := ClientConnected;
  FClient.OnGenresReceived := ClientGenresReceived;
  FClient.OnStreamsReceived := ClientStreamsReceived;
  FClient.OnTitleChanged := ClientTitleChanged;
  FClient.OnServerInfo := ClientServerInfo;
  FClient.OnError := ClientError;
  FClient.OnEnded := ClientEnded;
  FClient.Resume;
end;

constructor THomeCommunication.Create;
begin

end;

destructor THomeCommunication.Destroy;
begin
  Terminate;
  inherited;
end;

function THomeCommunication.GetGenres: Boolean;
var
  XMLDocument: TXMLLib;
  XML: AnsiString;
begin
  Result := False;
  if not Connected then
    Exit;

  XMLDocument := FClient.XMLGet('getgenres');
  try
    XMLDocument.SaveToString(XML);

    FClient.Write(XML);
    Result := True;
  finally
    XMLDocument.Free;
  end;
end;

function THomeCommunication.GetStreams(Count, Offset: Integer; Search, Genre: string;
  Kbps: Integer; StreamType: string; ReplaceQuery: Boolean): Boolean;
var
  XMLDocument: TXMLLib;
  Root, Header, Data: TXMLNode;
  XML: AnsiString;
begin
  Result := False;
  if not Connected then
    Exit;

  XMLDocument := TXMLLib.Create;
  try
    Root := TXMLNode.Create();
    Root.Name := 'request';
    XMLDocument.Root := Root;

    Header := TXMLNode.Create(Root);
    Header.Name := 'header';
    Header.Attributes.SimpleAdd('version', '3');
    Header.Attributes.SimpleAdd('type', 'getstreams');

    Data := TXMLNode.Create(Root);
    Data.Name := 'data';

    Data.Nodes.SimpleAdd('count', IntToStr(Count));
    Data.Nodes.SimpleAdd('offset', IntToStr(Offset));
    Data.Nodes.SimpleAdd('search', EncodeU(Search));
    Data.Nodes.SimpleAdd('genre', EncodeU(Genre));
    Data.Nodes.SimpleAdd('kbps', IntToStr(Kbps));
    Data.Nodes.SimpleAdd('type', StreamType);
    Data.Nodes.SimpleAdd('format', EncodeU('m3u'));

    XMLDocument.SaveToString(XML);

    FClient.Write(XML);
    Result := True;
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
  if not Connected then
    Exit;

  XMLDocument := FClient.XMLGet('submitstream');
  try
    if Stream[Length(Stream)] = '/' then
      Stream := Copy(Stream, 1, Length(Stream) - 1);

    Data := XMLDocument.Root.GetNode('data');
    Data.Value.AsString := Stream;

    XMLDocument.SaveToString(XML);
    FClient.Write(XML);
  finally
    XMLDocument.Free;
  end;
end;

procedure THomeCommunication.Terminate;
begin
  if FClient <> nil then
    FClient.Terminate;
end;

procedure THomeCommunication.ClientConnected(Sender: TSocketThread);
begin
  FConnected := True;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure THomeCommunication.ClientEnded(Sender: TSocketThread);
var
  Thread: THomeThread;
begin
  FConnected := False;
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
    FOnError(Self, FClient.FErrorMsg);
end;

procedure THomeCommunication.TitleChanged(StreamName, Title, CurrentURL, URL: string; URLs: TStringList);
begin
  if Trim(Title) <> '' then
    if FClient <> nil then
    begin
      FClient.TitleChanged(StreamName, Title, CurrentURL, URL, URLs);
    end;
end;

procedure THomeCommunication.UpdateInfo(RecordingCount: Cardinal);
var
  XMLDocument: TXMLLib;
  Data, Data2: TXMLNode;
  XML: AnsiString;
begin
  if not Connected then
    Exit;

  XMLDocument := FClient.XMLGet('clientinfo');
  try
    Data := XMLDocument.Root.GetNode('data');
    Data2 := TXMLNode.Create(Data);
    Data2.Name := 'recordingcount';
    Data2.Value.AsLongWord := RecordingCount;

    XMLDocument.SaveToString(XML);
    FClient.Write(XML);
  finally
    XMLDocument.Free;
  end;
end;

{ THomeThread }

constructor THomeThread.Create;
begin
  {$IFDEF DEBUG}
  //inherited Create('gaia', 8007);
  inherited Create('streamwriter.org', 8007);
  {$ELSE}
  inherited Create('streamwriter.org', 8007);
  {$ENDIF}

  UseSynchronize := True;
  FGenres := TStringList.Create;
end;

destructor THomeThread.Destroy;
begin
  FGenres.Free;
  inherited;
end;

procedure THomeThread.DoConnected;
begin
  inherited;

  Write(XMLGetLogin);
end;

procedure THomeThread.DoEnded;
begin
  inherited;

  Sleep(1000);
end;

procedure THomeThread.DoError(Version: Integer; Header, Data: TXMLNode);
begin
  FErrorMsg := Data.Value.AsString;
  if Assigned(FOnError) then
    Sync(FOnError);
end;

procedure THomeThread.DoGenresReceived(Version: Integer; Header, Data: TXMLNode);
var
  i: Integer;
begin
  FGenres.Clear;
  for i := 0 to Data.Nodes.GetNode('genres').Nodes.Count - 1 do
  begin
    Genres.Add(Data.Nodes.GetNode('genres').Nodes[i].Value.AsString);
  end;

  if Assigned(FOnGenresReceived) then
    Sync(FOnGenresReceived);
end;

procedure THomeThread.DoReceivedString(D: string);
var
  Version: Integer;
  XMLDocument: TXMLLib;
  Header, Data: TXMLNode;
  T: string;
begin
  inherited;
    try
      XMLDocument := TXMLLib.Create;
      try
        XMLDocument.LoadFromString(D);

        Header := XMLDocument.Root.Nodes.GetNode('header');
        Data := XMLDocument.Root.Nodes.GetNode('data');

        Version := Header.Attributes.AttributeByName['version'].Value.AsInteger;
        T := Header.Attributes.AttributeByName['type'].Value.AsString;

        if Header.Attributes.AttributeByName['type'].Value.AsString = 'getgenres' then
        begin
          DoGenresReceived(Version, Header, Data);
        end;

        if Header.Attributes.AttributeByName['type'].Value.AsString = 'getstreams' then
        begin
          DoStreamsReceived(Version, Header, Data);
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
  Node: TXMLNode;
begin
  SetLength(FStreams, 0);

  FCount := Data.Nodes.GetNode('count').Value.AsInteger;
  FPacketCount := Data.Nodes.GetNode('packetcount').Value.AsInteger;

  for i := 0 to Data.Nodes.GetNode('streams').Nodes.Count - 1 do
  begin
    Node := Data.Nodes.GetNode('streams').Nodes[i];
    SetLength(FStreams, Length(FStreams) + 1);
    FStreams[High(FStreams)].Name := Node.Attributes.AttributeByName['name'].Value.AsString;
    FStreams[High(FStreams)].Genre := Node.Attributes.AttributeByName['genre'].Value.AsString;
    FStreams[High(FStreams)].URL := Node.Value.AsString;
    FStreams[High(FStreams)].Website := Node.Attributes.AttributeByName['website'].Value.AsString;
    FStreams[High(FStreams)].BitRate := Node.Attributes.AttributeByName['bitrate'].Value.AsInteger;
    FStreams[High(FStreams)].StreamType := Node.Attributes.AttributeByName['type'].Value.AsString;
    FStreams[High(FStreams)].Downloads := Node.Attributes.AttributeByName['downloads'].Value.AsInteger;
  end;

  if Assigned(FOnStreamsReceived) then
    Sync(FOnStreamsReceived);
end;

procedure THomeThread.DoTitleChanged(Version: Integer; Header, Data: TXMLNode);
begin
  if not AppGlobals.NetworkActive then
    Exit;

  FChangedStreamName := Data.Nodes.GetNode('streamname').Value.AsString;
  FChangedTitle := Data.Nodes.GetNode('title').Value.AsString;
  FChangedCurrentURL := Data.Nodes.GetNode('currenturl').Value.AsString;

  if (FChangedStreamName <> '') and (FChangedTitle <> '') and (FChangedCurrentURL <> '') then
    if Assigned(FOnTitleChanged) then
      Sync(FOnTitleChanged);
end;

procedure THomeThread.TitleChanged(StreamName, Title, CurrentURL, URL: string; URLs: TStringList);
var
  i: Integer;
  XML: TXMLLib;
  N, N2, N3: TXMLNode;
  S: AnsiString;
  TmpURL: AnsiString;
begin
  XML := XMLGet('fulltitlechange');
  try
    N := XML.Root.Nodes.GetNode('data');

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

    if URLs <> nil then
    begin
      N3 := TXMLNode.Create(N);
      N3.Name := 'urls';
      for i := 0 to URLs.Count - 1 do
      begin
        N2 := TXMLNode.Create(N3);
        N2.Name := 'url';
        TmpURL := URLs[i];
        if TmpURL[Length(TmpURL)] = '/' then
          TmpURL := Copy(TmpURL, 1, Length(TmpURL) - 1);
        N2.Value.AsString := TmpURL;
      end;
    end;

    XML.SaveToString(S);
    Write(S);
  finally
    XML.Free;
  end;
end;

function THomeThread.XMLGet(T: string): TXMLLib;
var
  Root, Header, Data: TXMLNode;
begin
  Result := TXMLLib.Create;

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

function THomeThread.XMLGetLogin: AnsiString;
var
  XML: TXMLLib;
  D: TXMLNode;
begin
  XML := XMLGet('login');
  try
    D := TXMLNode.Create(XML.Root.Nodes.GetNode('data'));
    D.Name := 'asdf';
    D.Value.AsString := 'lol';
    XML.SaveToString(Result);
  finally
    XML.Free;
  end;
end;

initialization
  HomeComm := THomeCommunication.Create;

finalization
  HomeComm.Free;

end.
