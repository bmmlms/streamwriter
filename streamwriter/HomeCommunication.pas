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
  Windows, SysUtils, Classes, Functions, HTTPThread, AppData, Base64,
  StrUtils, Generics.Collections, XMLLib;

type
  TStreamInfo = record
    Name: string;
    Genre: string;
    URL: string;
    BitRate: Integer;
    Downloads: Integer;
  end;
  TStreamInfoArray = array of TStreamInfo;
  
  THomeThread = class(THTTPThread)
  private
    FKilled: Boolean;
    FSuccess: Boolean;
  public
    property Killed: Boolean read FKilled write FKilled;
    property Success: Boolean read FSuccess write FSuccess;

    constructor Create(URL: string); override;
  end;

  TSubmitThread = class(THomeThread)
  protected
    procedure DoEnded; override;
  end;

  TGetGenresThread = class(THomeThread)
  protected
    procedure DoDisconnected; override;
    procedure DoException(E: Exception); override;
    procedure DoEnded; override;
  public
    Genres: TStringList;

    constructor Create(URL: string); override;
    destructor Destroy; override;
  end;

  TGetStreamsThread = class(THomeThread)
  protected
    procedure DoDisconnected; override;
    procedure DoException(E: Exception); override;
    procedure DoEnded; override;
  public
    Count: Integer;
    PacketCount: Integer;
    Offset: Integer;
    Search: string;
    Streams: TStreamInfoArray;

    constructor Create(URL: string); override;
    destructor Destroy; override;
  end;

  TGenresReceivedEvent = procedure(Sender: TObject; Genres: TStringList) of object;
  TStreamsReceivedEvent = procedure(Sender: TObject; Streams: TStreamInfoArray; Count: Integer) of object;

  THomeCommunication = class
  private
    FClients: TList<THomeThread>;
    FURL: string;

    FOnGenresReceived: TGenresReceivedEvent;
    FOnStreamsReceived: TStreamsReceivedEvent;
    FOnReceiveError: TNotifyEvent;
    FOnOldVersion: TNotifyEvent;

    function FGetCount: Integer;
    procedure InitThread(Thread: THomeThread);
    procedure ThreadEnded(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SubmitStream(Stream: string);
    procedure GetGenres;
    procedure GetStreams(Count, Offset: Integer; Search, Genre: string; Kbps: Integer; ReplaceQuery: Boolean);
    procedure Terminate;

    property Count: Integer read FGetCount;
    property OnGenresReceived: TGenresReceivedEvent read FOnGenresReceived write FOnGenresReceived;
    property OnStreamsReceived: TStreamsReceivedEvent read FOnStreamsReceived write FOnStreamsReceived;
    property OnReceiveError: TNotifyEvent read FOnReceiveError write FOnReceiveError;
    property OnOldVersion: TNotifyEvent read FOnOldVersion write FOnOldVersion;
  end;

implementation

{ TStreamSubmit }

constructor THomeCommunication.Create;
begin
  {$IFDEF DEBUG}
  //FURL := 'http://streamwriter.gaia/en/streamdb/';
  FURL := 'http://streamwriter.org/en/streamdb/';
  {$ELSE}
  FURL := 'http://streamwriter.org/en/streamdb/';
  {$ENDIF}

  FClients := TList<THomeThread>.Create;
end;

procedure THomeCommunication.InitThread(Thread: THomeThread);
begin
  Thread.OnEnded := ThreadEnded;
  if AppGlobals.ProxyEnabled then
  begin
    Thread.ProxyEnabled := True;
    Thread.ProxyHost := AppGlobals.ProxyHost;
    Thread.ProxyPort := AppGlobals.ProxyPort;
  end;
  Thread.UserAgent := AnsiString(AppGlobals.AppName) + ' v' + AppGlobals.AppVersion.AsString;
  FClients.Add(Thread);
end;

destructor THomeCommunication.Destroy;
begin
  FClients.Free;
  inherited;
end;

function THomeCommunication.FGetCount: Integer;
begin
  Result := FClients.Count;
end;

procedure THomeCommunication.GetGenres;
var
  URL: string;
  Thread: TGetGenresThread;
  XMLDocument: TXMLLib;
  Root, Header: TXMLNode;
  XML: AnsiString;
begin
  URL := FURL + 'getgenres/';
  Thread := TGetGenresThread.Create(URL);
  InitThread(Thread);

  XMLDocument := TXMLLib.Create;
  try
    Root := TXMLNode.Create();
    Root.Name := 'request';
    XMLDocument.Root := Root;

    Header := TXMLNode.Create(Root);
    Header.Name := 'header';
    Header.Nodes.SimpleAdd('version', '1');
    Header.Nodes.SimpleAdd('type', 'getgenres');

    XMLDocument.SaveToString(XML);
  finally
    XMLDocument.Free;
  end;

  Thread.PostData := 'data=' + XML;
  Thread.Resume;
end;

procedure THomeCommunication.GetStreams(Count, Offset: Integer; Search, Genre: string;
  Kbps: Integer; ReplaceQuery: Boolean);
var
  i: Integer;
  URL: string;
  Thread: TGetStreamsThread;
  XMLDocument: TXMLLib;
  Root, Header, Data: TXMLNode;
  XML: AnsiString;
begin
  if not ReplaceQuery then
  begin
    // Beim Scrollen darf es immer nur einen Thread für neue Daten geben
    for i := 0 to FClients.Count - 1 do
      if FClients[i] is TGetStreamsThread then
        Exit;
  end else
  begin
    // Beim Suchen einfach den alten töten.
    for i := 0 to FClients.Count - 1 do
      if FClients[i] is TGetStreamsThread then
      begin
        TGetStreamsThread(FClients[i]).Killed := True;
        FClients[i].Terminate;
        Break;
      end;
  end;

  URL := FURL + 'getstreams/';
  Thread := TGetStreamsThread.Create(URL);
  InitThread(Thread);
  Thread.Count := Count;
  Thread.Offset := Offset;
  Thread.Search := Search;
  Thread.Killed := False;

  XMLDocument := TXMLLib.Create;
  try
    Root := TXMLNode.Create();
    Root.Name := 'request';
    XMLDocument.Root := Root;

    Header := TXMLNode.Create(Root);
    Header.Name := 'header';
    Header.Nodes.SimpleAdd('version', '3');
    Header.Nodes.SimpleAdd('type', 'getstreams');

    Data := TXMLNode.Create(Root);
    Data.Name := 'data';

    Data.Nodes.SimpleAdd('count', IntToStr(Count));
    Data.Nodes.SimpleAdd('offset', IntToStr(Offset));
    Data.Nodes.SimpleAdd('search', EncodeU(Search));
    Data.Nodes.SimpleAdd('genre', EncodeU(Genre));
    Data.Nodes.SimpleAdd('kbps', IntToStr(Kbps));
    Data.Nodes.SimpleAdd('format', EncodeU('m3u'));

    XMLDocument.SaveToString(XML);
  finally
    XMLDocument.Free;
  end;

  Thread.PostData := 'data=' + XML;
  Thread.Resume;
end;

procedure THomeCommunication.SubmitStream(Stream: string);
var
  URL: string;
  Thread: TSubmitThread;
  XMLDocument: TXMLLib;
  Root, Header, Data: TXMLNode;
  XML: AnsiString;
begin
  if not AppGlobals.SubmitStreams then
    Exit;

  // Nur submitten, wenn er nicht von uns selber kommt
  if (Pos('mistake.ws', LowerCase(Stream)) > 0) or
     (Pos('streamwriter.org', LowerCase(Stream)) > 0) then
  begin
    Exit;
  end;

  URL := FURL + 'submitstream/';
  Thread := TSubmitThread.Create(URL);
  InitThread(Thread);

  XMLDocument := TXMLLib.Create;
  try
    Root := TXMLNode.Create;
    Root.Name := 'request';
    XMLDocument.Root := Root;

    Header := TXMLNode.Create(Root);
    Header.Name := 'header';
    Header.Nodes.SimpleAdd('version', '2');
    Header.Nodes.SimpleAdd('type', 'submitstream');

    Data := TXMLNode.Create(Root);
    Data.Name := 'data';
    Data.Value.AsString := EncodeU(Stream);

    XMLDocument.SaveToString(XML);
  finally
    XMLDocument.Free;
  end;

  Thread.PostData := 'data=' + XML;
  Thread.Resume;
end;

procedure THomeCommunication.Terminate;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if FClients[i] is TGetStreamsThread then    
      TGetStreamsThread(FClients[i]).Killed := True;
    FClients[i].Terminate;
  end;
end;

procedure THomeCommunication.ThreadEnded(Sender: TObject);
var
  Thread: THomeThread;
begin
  Thread := THomeThread(Sender);

  FClients.Remove(Thread);

  if Thread.Killed then
    Exit;

  if Thread.Success then
  begin
    if Thread is TGetGenresThread then
      if Assigned(FOnGenresReceived) then
        FOnGenresReceived(Self, TGetGenresThread(Thread).Genres);

    if Thread is TGetStreamsThread then
      if Assigned(FOnStreamsReceived) then
        FOnStreamsReceived(Self, TGetStreamsThread(Thread).Streams, TGetStreamsThread(Thread).Count);
  end else
  begin
    if Thread.RecvDataStream.ToString = 'OLDVER' then
    begin
      if Assigned(FOnOldVersion) then
        FOnOldVersion(Self);
    end else
    begin
      // Es kann sein, dass am Anfang GetGenres und GetStreams aktiv ist.
      // Deshalb alles tot machen, so dass die Fehlermeldung anstehen bleibt.
      for Thread in FClients do
      begin
        if not (Thread is TSubmitThread) then
        begin
          Thread.Killed := True;
          Thread.Terminate;
        end;
      end;

      if Assigned(FOnReceiveError) then
        FOnReceiveError(Self);
    end;
  end;
end;

{ TGetStreamsThread }

constructor TGetStreamsThread.Create(URL: string);
begin
  inherited;
  SetLength(Streams, 0);
end;

destructor TGetStreamsThread.Destroy;
begin

  inherited;
end;

procedure TGetStreamsThread.DoDisconnected;
var
  i: Integer;
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
begin
  inherited;
  
  if Killed then
    Exit;

  if Length(RecvDataStream.ToString) = 0 then
  begin
    raise Exception.Create('No data received');
  end else if RecvDataStream.ToString = 'ERROR' then
  begin
    raise Exception.Create('Server-side error');
  end else
  begin
    try
      SetLength(Streams, 0);
      XMLDocument := TXMLLib.Create;
      try
        XMLDocument.LoadFromString(RecvDataStream.ToString);

        Data := XMLDocument.Root.Nodes.GetNode('data');

        Count := Data.Nodes.GetNode('count').Value.AsInteger;
        PacketCount := Data.Nodes.GetNode('packetcount').Value.AsInteger;

        for i := 0 to Data.Nodes.GetNode('streams').Nodes.Count - 1 do
        begin
          Node := Data.Nodes.GetNode('streams').Nodes[i];
          SetLength(Streams, Length(Streams) + 1);
          Streams[High(Streams)].Name := Node.Attributes.AttributeByName['name'].Value.AsString;
          Streams[High(Streams)].Genre := Node.Attributes.AttributeByName['genre'].Value.AsString;
          Streams[High(Streams)].URL := Node.Value.AsString;
          Streams[High(Streams)].BitRate := Node.Attributes.AttributeByName['bitrate'].Value.AsInteger;
          Streams[High(Streams)].Downloads := Node.Attributes.AttributeByName['downloads'].Value.AsInteger;
        end;
      finally
        XMLDocument.Free;
      end;
    except
      raise Exception.Create('No data received');
    end;
    FSuccess := True;
  end;
end;

procedure TGetStreamsThread.DoEnded;
begin
  inherited;
  if RecvDataStream.ToString = '' then
    Sleep(1000);
end;

procedure TGetStreamsThread.DoException(E: Exception);
begin
  SetLength(Streams, 0);
  Sleep(100);
  inherited;
end;

{ TGetGenresThread }

constructor TGetGenresThread.Create(URL: string);
begin
  inherited;
  Genres := TStringList.Create;
end;

destructor TGetGenresThread.Destroy;
begin
  Genres.Free;
  inherited;
end;

procedure TGetGenresThread.DoDisconnected;
var
  i: Integer;
  XMLDocument: TXMLLib;
  Data, Node: TXMLNode;
begin
  inherited;

  if Length(RecvDataStream.ToString) = 0 then
  begin
    raise Exception.Create('No data received');
  end else if RecvDataStream.ToString = 'ERROR' then
  begin
    raise Exception.Create('Server-side error');
  end else
  begin
    try
      XMLDocument := TXMLLib.Create;
      try
        XMLDocument.LoadFromString(RecvDataStream.ToString);

        Data := XMLDocument.Root.Nodes.GetNode('data');

        for i := 0 to Data.Nodes.GetNode('genres').Nodes.Count - 1 do
        begin
          Node := Data.Nodes.GetNode('genres').Nodes[i];
          Genres.Add(Node.Value.AsString);
        end;
      finally
        XMLDocument.Free;
      end;
    except
      raise Exception.Create('No data received');
    end;
    FSuccess := True;
  end;
end;

procedure TGetGenresThread.DoEnded;
begin
  inherited;
  if RecvDataStream.ToString = '' then
    Sleep(1000);
end;

procedure TGetGenresThread.DoException(E: Exception);
begin
  inherited;
  Sleep(100);
end;

{ TSubmitThread }

procedure TSubmitThread.DoEnded;
begin
  // Wichtig, damit das StreamBrowserView nicht sagt "Fehler"
  FSuccess := True;
  inherited;
end;

{ THomeThread }

constructor THomeThread.Create(URL: string);
begin
  inherited;
  Killed := False;
  Success := False;
end;

end.
