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
unit RelayServer;

interface

uses
  SysUtils, Windows, Classes, SocketThread, ServerSocketThread,
  HTTPServerStream, Generics.Collections, AppData;

type
  TRelayThread = class;

  TClientList = class(TList<TRelayThread>);

  TRelayServerThread = class(TServerSocketThread)
  private
    FClients: TClientList;

    FOnClientConnected: TNotifyEvent;
  protected
    procedure DoClientConnected(Handle: Cardinal); override;
    procedure DoEnded; override;
  public
    constructor Create;
    property OnClientConnected: TNotifyEvent read FOnClientConnected write FOnClientConnected;
  end;

  TRelayThread = class(TSocketThread)
  private
    FTypedStream: THTTPServerStream;
    FResponded: Boolean;
    FStationName: string;
    FContentType: string;

    FOnGetStream: TNotifyEvent;
  protected
    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
  public
    constructor Create(Handle: Cardinal);

    property StationName: string read FStationName write FStationName;
    property ContentType: string read FContentType write FContentType;
    property RecvStream: THTTPServerStream read FTypedStream;
    property OnGetStream: TNotifyEvent read FOnGetStream write FOnGetStream;
  end;

  TRelayServer = class
  private
    FAcceptThread: TRelayServerThread;
    FOnGetStream: TNotifyEvent;
    FOnEnded: TNotifyEvent;

    procedure RelayClientConnected(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property OnGetStream: TNotifyEvent read FOnGetStream write FOnGetStream;
    property OnEnded: TNotifyEvent read FOnEnded write FOnEnded;
  end;

implementation

{ TRelayServer }

constructor TRelayServer.Create;
begin
  inherited;
end;

destructor TRelayServer.Destroy;
begin
  Stop;
  inherited;
end;

procedure TRelayServer.RelayClientConnected(Sender: TObject);
var
  C: TRelayThread;
begin
  C := TRelayThread(Sender);
  C.FOnGetStream := FOnGetStream;
  C.OnEnded := FOnEnded;
end;

procedure TRelayServer.Start;
begin
  Stop;
  FAcceptThread := TRelayServerThread.Create;
  FAcceptThread.OnClientConnected := RelayClientConnected;
  FAcceptThread.Start;
end;

procedure TRelayServer.Stop;
begin
  if FAcceptThread <> nil then
  begin
    FAcceptThread.Terminate;
    FAcceptThread := nil;
  end;
end;

{ TRelayServerThread }

constructor TRelayServerThread.Create;
begin
  inherited;
  FClients := TClientList.Create;
end;

procedure TRelayServerThread.DoClientConnected(Handle: Cardinal);
var
  T: TRelayThread;
begin
  T := TRelayThread.Create(Handle);
  T.Start;
  FClients.Add(T);
  Sync(FOnClientConnected, T);
end;

procedure TRelayServerThread.DoEnded;
begin
  inherited;
  FClients.Free;
end;

{ TRelayThread }

constructor TRelayThread.Create(Handle: Cardinal);
var
  S: THTTPServerStream;
begin
  S := THTTPServerStream.Create;
  inherited Create(Handle, S);
  FTypedStream := S;
  FResponded := False;
end;

procedure TRelayThread.DoReceivedData(Buf: Pointer; Len: Integer);
var
  Response: AnsiString;
begin
  inherited;
  if (FTypedStream.RequestProcessed) and (FTypedStream.RequestedURL <> '') and
     (not FResponded) then
  begin
    Sync(FOnGetStream);

    Response := 'ICY 200 OK'#13#10 +
                'icy-name: ' + AnsiString(FStationName) + #13#10 +
                'content-type: ' + AnsiString(FContentType) + #13#10 +
                #13#10;
    FSendStream.Add(Response);
  end else
    raise Exception.Create('Ungültige Anfrage an Server empfangen');
end;

end.
