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
unit ICEThread;

interface

uses
  SysUtils, Windows, WinSock, Classes, HTTPThread, ExtendedStream, ICEStream,
  Functions, SocketThread, SyncObjs, AudioStream, Generics.Collections,
  AppData, PlayThread;

type
  TICEThreadStates = (tsRecording, tsRetrying, tsIOError);

  TRelayInfo = record
    Thread: TSocketThread;
    FirstSent: Boolean;
  end;
  PRelayInfo = ^TRelayInfo;
  TRelayInfoList = TList<PRelayInfo>;

  TICEThread = class(THTTPThread)
  private
    FTitle: string;
    FState: TICEThreadStates;
    FPlayThread: TPlayThread;
    FRecording: Boolean;
    FPlaying: Boolean;

    FRelayThreads: TRelayInfoList;

    FOnTitleChanged: TNotifyEvent;
    FOnSongSaved: TNotifyEvent;
    FOnNeedSettings: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FOnAddRecent: TNotifyEvent;
    FOnTitleAllowed: TNotifyEvent;

    FTypedStream: TICEStream;
    FRelayLock, FPlayLock: TCriticalSection;
    FPlayBuffer: TAudioStreamMemory;

    procedure StreamTitleChanged(Sender: TObject);
    procedure StreamSongSaved(Sender: TObject);
    procedure StreamNeedSettings(Sender: TObject);
    procedure StreamChunkReceived(Buf: Pointer; Len: Integer);
    procedure StreamIOError(Sender: TObject);
    procedure StreamTitleAllowed(Sender: TObject);

    procedure PlayThreadTerminated(Sender: TObject);
  protected
    procedure Execute; override;

    procedure DoHeaderRemoved; override;
    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoDisconnected; override;
    procedure DoEnded; override;
    procedure DoSpeedChange; override;
    procedure DoException(E: Exception); override;
  public
    constructor Create(URL: string); reintroduce;
    destructor Destroy; override;

    procedure SetSettings(SkipShort: Boolean);

    procedure StartPlay;
    procedure StopPlay;
    procedure StartRecording;
    procedure StopRecording;

    procedure LockRelay;
    procedure UnlockRelay;

    property RecvStream: TICEStream read FTypedStream;
    property Title: string read FTitle;
    property State: TICEThreadStates read FState;

    property Recording: Boolean read FRecording;
    property Playing: Boolean read FPlaying;
    property RelayThreads: TRelayInfoList read FRelayThreads;

    property OnTitleChanged: TNotifyEvent read FOnTitleChanged write FOnTitleChanged;
    property OnSongSaved: TNotifyEvent read FOnSongSaved write FOnSongSaved;
    property OnNeedSettings: TNotifyEvent read FOnNeedSettings write FOnNeedSettings;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnAddRecent: TNotifyEvent read FOnAddRecent write FOnAddRecent;
    property OnTitleAllowed: TNotifyEvent read FOnTitleAllowed write FOnTitleAllowed;
  end;

implementation

{ TICEThread }

procedure TICEThread.SetSettings(SkipShort: Boolean);
begin
  FTypedStream.SkipShort := SkipShort;
end;

procedure TICEThread.StartPlay;
var
  P: Integer;
begin
  FPlaying := True;
  if FPlayThread = nil then
  begin
    FPlayThread := TPlayThread.Create;
    FPlayThread.OnTerminate := PlayThreadTerminated;

    if FPlayBuffer <> nil then
    begin
      FRelayLock.Enter;
      try
        P := FPlayBuffer.GetFrame(0, False);
        if P = -1 then
          P := 0;
        FPlayBuffer.Seek(P, soFromBeginning);
        FPlayThread.PushData(Pointer(Integer(FPlayBuffer.Memory) + P), FPlayBuffer.Size - P);
      finally
        FRelayLock.Leave;
      end;
    end;

    FPlayThread.Resume;
  end;
end;

procedure TICEThread.StopPlay;
begin
  FPlaying := False;
  if FPlayThread <> nil then
    FPlayThread.Terminate;
end;

procedure TICEThread.StartRecording;
begin
  FRecording := True;
  FTypedStream.StartRecording;
end;

procedure TICEThread.StopRecording;
begin
  FRecording := False;
  FTypedStream.StopRecording;
end;

procedure TICEThread.StreamNeedSettings(Sender: TObject);
begin
  Sync(FOnNeedSettings);
end;

procedure TICEThread.StreamChunkReceived(Buf: Pointer; Len: Integer);
var
  RemoveTo: Int64;
  Thread: PRelayInfo;
const
  MAX_BUFFER_SIZE = 512000;
begin
  if FPlayThread <> nil then
  begin
    FPlayLock.Enter;
    FPlayThread.PushData(Buf, Len);
    FPlayLock.Leave;
  end;

  //FPlayThread.Resume;

  if FPlayBuffer = nil then
    Exit;
  FRelayLock.Enter;
  try
    FPlayBuffer.Seek(0, soFromEnd);
    FPlayBuffer.WriteBuffer(Buf^, Len);

    WriteDebug(Format('Playbuffer size: %d bytes', [FPlayBuffer.Size]));

    if FPlayBuffer.Size > MAX_BUFFER_SIZE then
    begin
      // Puffer "rotieren"
      RemoveTo := FPlayBuffer.GetFrame(16384, False);
      FPlayBuffer.RemoveRange(0, RemoveTo - 1);
      WriteDebug(Format('Playbuffer size after remove: %d bytes', [FPlayBuffer.Size]));
    end;

    for Thread in FRelayThreads do
    begin
      // Wenn schon was gesendet wurde, die neuen Daten schicken
      if Thread.FirstSent then
      begin
        Thread.Thread.SendLock.Enter;
        try
          Thread.Thread.SendStream.Seek(0, soFromEnd);
          Thread.Thread.SendStream.Write(Buf^, Len);

          WriteDebug(Format('Wrote %d bytes to relaythread, new size is %d', [Len, Thread.Thread.SendStream.Size]));
        finally
          Thread.Thread.SendLock.Leave;
        end;
      end;
    end;
  finally
    FRelayLock.Leave;
  end;
end;

procedure TICEThread.StreamIOError(Sender: TObject);
begin
  FState := tsIOError;
  Sync(FOnStateChanged);
end;

procedure TICEThread.StreamTitleAllowed(Sender: TObject);
begin
  Sync(FOnTitleAllowed);
end;

procedure TICEThread.PlayThreadTerminated(Sender: TObject);
begin
  FPlayLock.Enter;
  FPlayThread := nil;
  if FPlaying then
    StartPlay;
  FPlayLock.Leave;
end;

procedure TICEThread.StreamSongSaved(Sender: TObject);
begin
  Sync(FOnSongSaved);
end;

procedure TICEThread.StreamTitleChanged(Sender: TObject);
begin
  Sync(FOnTitleChanged);
  FState := tsRecording;
  Sync(FOnStateChanged);
end;

procedure TICEThread.DoDisconnected;
begin
  inherited;
  Sleep(100);
end;

procedure TICEThread.DoEnded;
var
  Thread: PRelayInfo;
begin
  inherited;
  FRelayLock.Enter;
  for Thread in FRelayThreads do
    Thread.Thread.Terminate;
  FRelayLock.Leave;
end;

procedure TICEThread.DoException(E: Exception);
var
  StartTime: Cardinal;
  Delay: Cardinal;
begin
  inherited;
  AppGlobals.Lock;
  Delay := AppGlobals.RetryDelay * 1000;
  AppGlobals.Unlock;
  if FState <> tsIOError then
  begin
    FState := tsRetrying;
    Sync(FOnStateChanged);
    StartTime := GetTickCount;
    while StartTime > GetTickCount - Delay do
    begin
      Sleep(500);
      if Terminated then
        Exit;
    end;
  end;
end;

procedure TICEThread.DoHeaderRemoved;
begin
  inherited;

  case FTypedStream.AudioType of
    atMPEG:
      FPlayBuffer := TMPEGStreamMemory.Create;
    atAAC:
      FPlayBuffer := TAACStreamMemory.Create;
  end;

  if (FTypedStream.HeaderType = 'icy') and
     (FTypedStream.StreamName <> '') then
    Sync(FOnAddRecent);
end;

procedure TICEThread.DoReceivedData(Buf: Pointer; Len: Integer);
var
  P: Int64;
  Thread: PRelayInfo;
begin
  inherited;

  if FPlayBuffer <> nil then
  begin
    FRelayLock.Enter;
    try
      for Thread in FRelayThreads do
      begin
        if not Thread.FirstSent then
        begin
          Thread.Thread.SendLock.Enter;
          try
            P := FPlayBuffer.GetFrame(0, False);
            if P = -1 then
              P := 0;
            FPlayBuffer.Seek(P, soFromBeginning);
            Thread.Thread.SendStream.Seek(0, soFromEnd);
            Thread.Thread.SendStream.CopyFrom(FPlayBuffer, FPlayBuffer.Size - P);
            Thread.FirstSent := True;
          finally
            Thread.Thread.SendLock.Leave;
          end;
        end;
      end;
    finally
      FRelayLock.Leave;
    end;
  end;
end;

procedure TICEThread.DoSpeedChange;
begin
  inherited;

end;

procedure TICEThread.Execute;
begin
  inherited;

end;

procedure TICEThread.LockRelay;
begin
  FRelayLock.Enter;
end;

procedure TICEThread.UnlockRelay;
begin
  FRelayLock.Leave;
end;

constructor TICEThread.Create(URL: string);
var
  Host, Data: string;
  SendData: AnsiString;
  Port: Integer;
begin
  inherited Create(URL, TICEStream.Create);

  FRecording := False;
  FPlaying := False;

  AppGlobals.Lock;
  ProxyEnabled := AppGlobals.ProxyEnabled;
  ProxyHost := AppGlobals.ProxyHost;
  ProxyPort := AppGlobals.ProxyPort;
  AppGlobals.Unlock;

  FRelayLock := TCriticalSection.Create;
  FPlayLock := TCriticalSection.Create;
  FRelayThreads := TRelayInfoList.Create;
  FTitle := '';

  FUserAgent := AnsiString(AppGlobals.AppName) + ' v' + AppGlobals.AppVersion.AsString;

  ParseURL(URL, Host, Port, Data);

  FTypedStream := TICEStream(FRecvStream);
  FTypedStream.OnTitleChanged := StreamTitleChanged;
  FTypedStream.OnSongSaved := StreamSongSaved;
  FTypedStream.OnNeedSettings := StreamNeedSettings;
  FTypedStream.OnChunkReceived := StreamChunkReceived;
  FTypedStream.OnIOError := StreamIOError;
  FTypedStream.OnTitleAllowed := StreamTitleAllowed;

  if ProxyEnabled then
    SendData := 'GET ' + AnsiString(URL) + ' HTTP/1.1'#13#10
  else
    SendData := 'GET ' + AnsiString(Data) + ' HTTP/1.1'#13#10;
  SendData := SendData + 'Host: ' + AnsiString(Host) + #13#10;
  SendData := SendData + 'Accept: */*'#13#10;
  SendData := SendData + 'User-Agent: mhttplib/' + FUserAgent + #13#10;
  SendData := SendData + 'Icy-MetaData:1'#13#10;
  SendData := SendData + 'Connection: close'#13#10;
  SendData := SendData + #13#10;
  FSendStream.SetData(SendData);
end;

destructor TICEThread.Destroy;
var
  i: Integer;
begin
  if FPlayThread <> nil then
  begin
    FPlayThread.OnTerminate := nil;
    FPlayThread.Terminate;
  end;
  if FPlayBuffer <> nil then
    FPlayBuffer.Free;
  FRelayLock.Free;
  FPlayLock.Free;
  for i := 0 to FRelayThreads.Count - 1 do
  begin
    Dispose(FRelayThreads[i]);
  end;
  FRelayThreads.Free;
  inherited;
end;

end.

