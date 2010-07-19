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
  end;
  TStreamInfoArray = array of TStreamInfo;
  
  THomeThread = class(THTTPThread);

  TSubmitThread = class(THomeThread);
  
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
    Killed: Boolean;
    Streams: TStreamInfoArray;
    
    constructor Create(URL: string); override;
    destructor Destroy; override;
  end;

  TStreamsReceivedEvent = procedure(Sender: TObject; Streams: TStreamInfoArray; Count: Integer) of object;

  THomeCommunication = class
  private
    FClients: TList<THTTPThread>;
    FURL: string;

    FOnStreamsReceived: TStreamsReceivedEvent;

    function FGetCount: Integer;
    procedure InitThread(Thread: THTTPThread);
    procedure ThreadEnded(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SubmitStream(Stream: string);
    procedure GetStreams(Count, Offset: Integer; Search: string; ReplaceQuery: Boolean);
    procedure Terminate;

    property Count: Integer read FGetCount;
    property OnStreamsReceived: TStreamsReceivedEvent read FOnStreamsReceived write FOnStreamsReceived;
  end;

implementation

{ TStreamSubmit }

constructor THomeCommunication.Create;
begin
  {$IFDEF DEBUG}
  FURL := 'http://mistake.gaia/en/streamdb/';


  //FURL := 'http://mistake.ws/en/streamdb/';
  {$ELSE}
  FURL := 'http://mistake.ws/en/streamdb/';
  {$ENDIF}
  FClients := TList<THTTPThread>.Create;
end;

procedure THomeCommunication.InitThread(Thread: THTTPThread);
begin
  Thread.OnEnded := ThreadEnded;
  if AppGlobals.ProxyEnabled then
  begin
    Thread.ProxyEnabled := True;
    Thread.ProxyHost := AppGlobals.ProxyHost;
    Thread.ProxyPort := AppGlobals.ProxyPort;
  end;
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

procedure THomeCommunication.GetStreams(Count, Offset: Integer; Search: string; ReplaceQuery: Boolean);
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
        
  URL := FURL + 'getstreams/?count=' + IntToStr(Count) + '&offset=' + IntToStr(Offset) + '&search=' + Encode(AnsiString(Search));
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
    Header.Nodes.SimpleAdd('version', '1');
    Header.Nodes.SimpleAdd('type', 'getstreams');

    Data := TXMLNode.Create(Root);
    Data.Name := 'data';

    Data.Nodes.SimpleAdd('count', IntToStr(Count));
    Data.Nodes.SimpleAdd('offset', IntToStr(Offset));
    Data.Nodes.SimpleAdd('search', Search);

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
    Header.Nodes.SimpleAdd('version', '1');
    Header.Nodes.SimpleAdd('type', 'submitstream');

    Data := TXMLNode.Create(Root);
    Data.Name := 'data';
    Data.Value.AsString := Stream;

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
  Thread: THTTPThread;
  GetStreamsThread: TGetStreamsThread;
begin
  Thread := THTTPThread(Sender);

  FClients.Remove(Thread);

  if Thread is TGetStreamsThread then
  begin
    GetStreamsThread := TGetStreamsThread(Thread);
    
    if GetStreamsThread.Killed then
      Exit;
    
    if GetStreamsThread.RecvDataStream.ToString = '' then
    begin
      GetStreams(GetStreamsThread.Count, GetStreamsThread.Offset, GetStreamsThread.Search, True);
    end else
    begin
      if Assigned(FOnStreamsReceived) then
        FOnStreamsReceived(Self, GetStreamsThread.Streams, GetStreamsThread.Count);
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
  end else
  begin
    try
      SetLength(Streams, 0);
      XMLDocument := TXMLLib.Create;
      try
        XMLDocument.LoadFromString(string(RecvDataStream.ToString));

        //Header := XMLDocument.Root.Nodes.GetNode('header');
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
        end;
      finally
        XMLDocument.Free;
      end;
    except
      raise Exception.Create('No data received');
    end;
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

end.
