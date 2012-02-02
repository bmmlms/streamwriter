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

{ This unit handles everything needed for streaming... By using a sub-class of
  TAudioStreamFile/TAudioStreamMemory it does it stuff }
unit ICEThread;

interface

uses
  SysUtils, Windows, WinSock, Classes, HTTPThread, ExtendedStream, ICEStream,
  Functions, Sockets, SyncObjs, AudioStream, Generics.Collections,
  AppData, ICEPlayer, LanguageObjects, PlayerManager, Logging, TypeDefs;

type
  TICEThreadStates = (tsRecording, tsRetrying, tsIOError);

  TICEThread = class(THTTPThread)
  private
    FTitle: string;
    FState: TICEThreadStates;
    FPlayer: TICEPlayer;
    FRecording: Boolean;
    FRecordingStarted: Boolean;
    FPlaying: Boolean;
    FPlayingStarted: Boolean;
    FPlayingPaused: Boolean;
    FPaused: Boolean;
    FSleepTime: Integer;

    FOnTitleChanged: TSocketEvent;
    FOnSongSaved: TSocketEvent;
    FOnNeedSettings: TSocketEvent;
    FOnStateChanged: TSocketEvent;
    FOnAddRecent: TSocketEvent;
    FOnTitleAllowed: TSocketEvent;
    FOnRefreshInfo: TSocketEvent;
    FOnRecordingStopped: TSocketEvent;
    FOnPlaybackStarted: TSocketEvent;

    FTypedStream: TICEStream;
    FPlayBufferLock: TCriticalSection;
    FPlayBuffer: TAudioStreamMemory;

    FPlaybackStarted: Boolean;

    function FGetPaused: Boolean;

    procedure StartRecordingInternal;
    procedure StopRecordingInternal;
    procedure StartPlayInternal;
    procedure StopPlayInternal;

    procedure StreamTitleChanged(Sender: TObject);
    procedure StreamSongSaved(Sender: TObject);
    procedure StreamNeedSettings(Sender: TObject);
    procedure StreamChunkReceived(Buf: Pointer; Len: Integer);
    procedure StreamIOError(Sender: TObject);
    procedure StreamTitleAllowed(Sender: TObject);
    procedure StreamRefreshInfo(Sender: TObject);
  protected
    procedure Execute; override;

    procedure DoStuff; override;
    procedure DoHeaderRemoved; override;
    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoConnecting; override;
    procedure DoConnected; override;
    procedure DoDisconnected; override;
    procedure DoEnded; override;
    procedure DoSpeedChange; override;
    procedure DoException(E: Exception); override;
  public
    constructor Create(URL: string); reintroduce;
    destructor Destroy; override;

    procedure SetSettings(Settings: TStreamSettings; AutoRemove, StopAfterSong: Boolean; RecordTitle: string);

    procedure StartPlay;
    procedure PausePlay;
    procedure StopPlay;
    procedure StartRecording;
    procedure StopRecording;
    procedure Pause;
    procedure SetVolume(Vol: Integer);

    procedure LockRelay;
    procedure UnlockRelay;

    property RecvStream: TICEStream read FTypedStream;
    property Title: string read FTitle;
    property State: TICEThreadStates read FState;

    property Recording: Boolean read FRecordingStarted;
    property Playing: Boolean read FPlayingStarted;
    property Paused: Boolean read FGetPaused;
    property SleepTime: Integer read FSleepTime write FSleepTime;

    property OnTitleChanged: TSocketEvent read FOnTitleChanged write FOnTitleChanged;
    property OnSongSaved: TSocketEvent read FOnSongSaved write FOnSongSaved;
    property OnNeedSettings: TSocketEvent read FOnNeedSettings write FOnNeedSettings;
    property OnStateChanged: TSocketEvent read FOnStateChanged write FOnStateChanged;
    property OnAddRecent: TSocketEvent read FOnAddRecent write FOnAddRecent;
    property OnTitleAllowed: TSocketEvent read FOnTitleAllowed write FOnTitleAllowed;
    property OnRefreshInfo: TSocketEvent read FOnRefreshInfo write FOnRefreshInfo;
    property OnRecordingStopped: TSocketEvent read FOnRecordingStopped write FOnRecordingStopped;
    property OnPlaybackStarted: TSocketEvent read FOnPlaybackStarted write FOnPlaybackStarted;
  end;

implementation

{ TICEThread }

procedure TICEThread.SetSettings(Settings: TStreamSettings; AutoRemove, StopAfterSong: Boolean; RecordTitle: string);
begin
  // Das hier wird nur gesynct aus dem Mainthread heraus aufgerufen.
  FTypedStream.Settings.Assign(Settings);
  FTypedStream.RecordTitle := RecordTitle;
  FTypedStream.StopAfterSong := StopAfterSong;
end;

procedure TICEThread.SetVolume(Vol: Integer);
begin
  FPlayer.SetVolume(Vol);
end;

procedure TICEThread.StartPlay;
begin
  FPlayingStarted := True;
  if FPlayingPaused then
    FPlayingPaused := False;
end;

procedure TICEThread.StartPlayInternal;
var
  P: TPosRect;
begin
  FPlaying := True;
  if FPlayBuffer = nil then
    Exit;

  if (not FPlayer.Playing) and (not FPlayer.Paused) then
  begin
    FPlayer.Mem.Clear;

    FPlayBufferLock.Enter;
    try
      P := FPlayBuffer.GetFrame(0, FPlayBuffer.Size);
      if P.DataStart = -1 then
        P.DataStart := 0;
      if P.DataEnd = -1 then
        P.DataEnd := FPlayBuffer.Size;
      FPlayBuffer.Seek(P.DataStart, soFromBeginning);
      try
        if not FPlaybackStarted then
        begin
          FPlaybackStarted := True;
          Sync(FOnPlaybackStarted);
        end;
        FPlayer.PushData(Pointer(Integer(FPlayBuffer.Memory) + P.DataStart), FPlayBuffer.Size - P.DataStart);
      except
        // Unbekannte Daten (kein MP3/AAC) - ende.
        FPlayingStarted := False;
        FPlaying := False;
        WriteDebug(_('Stream cannot be played because format is unknown'), 3, 0);
      end;
    finally
      FPlayBufferLock.Leave;
    end;
  end else
  begin

  end;
  FPlayer.Play;
  Sync(FOnStateChanged);
end;

procedure TICEThread.PausePlay;
begin
  FPlayingPaused := not FPlayingPaused;
end;

procedure TICEThread.StopPlay;
begin
  FPlayingStarted := False;
end;

procedure TICEThread.StopPlayInternal;
begin
  FPlaying := False;
  FPlayer.Stop;
  DoStuff; // Das muss so, damit der Thread aufs Fadeout-Ende wartet!

  Sync(FOnStateChanged);
end;

procedure TICEThread.StartRecording;
begin
  FRecordingStarted := True;
end;

procedure TICEThread.StartRecordingInternal;
begin
  FRecording := True;
  FTypedStream.StartRecording;

  Sync(FOnStateChanged);
end;

procedure TICEThread.StopRecording;
begin
  FRecordingStarted := False;
end;

procedure TICEThread.StopRecordingInternal;
begin
  FRecording := False;
  FTypedStream.StopRecording;

  Sync(FOnStateChanged);
end;

procedure TICEThread.StreamNeedSettings(Sender: TObject);
begin
  Sync(FOnNeedSettings);
end;

procedure TICEThread.StreamRefreshInfo(Sender: TObject);
begin
  Sync(FOnRefreshInfo);
end;

procedure TICEThread.StreamChunkReceived(Buf: Pointer; Len: Integer);
var
  RemoveTo: TPosRect;
const
  MAX_BUFFER_SIZE = 1048576;
begin

  if FPlaying then
  begin
    try
      if not FPlaybackStarted then
      begin
        Sync(FOnPlaybackStarted);
        FPlaybackStarted := True;
      end;

      FPlayer.PushData(Buf, Len);

      if (not FPaused) and (FPlaying and (not FPlayer.Playing)) then
        FPlayer.Play;
    except
      // Unbekannte Daten (kein MP3/AAC) - Ende.
      FPlayingStarted := False;
      FPlaying := False;
      WriteDebug(_('Stream cannot be played because format is unknown'), 3, 0);

      Sync(FOnStateChanged);
    end;
    //WriteDebug(Format('Playbuffer size: %d', [FPlayer.Mem.Size]));
  end;

  if FPlayBuffer = nil then
    Exit;
  FPlayBufferLock.Enter;
  try
    FPlayBuffer.Seek(0, soFromEnd);
    FPlayBuffer.WriteBuffer(Buf^, Len);

    //WriteDebug(Format('Playbuffer size: %d bytes', [FPlayBuffer.Size]));

    while FPlayBuffer.Size > MAX_BUFFER_SIZE do
    begin
      // Puffer "rotieren"
      RemoveTo := FPlayBuffer.GetFrame(65536, FPlayBuffer.Size);
      FPlayBuffer.RemoveRange(0, RemoveTo.DataStart - 1);
      //WriteDebug(Format('Playbuffer size after remove: %d bytes', [FPlayBuffer.Size]));
    end;
  finally
    FPlayBufferLock.Leave;
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

procedure TICEThread.DoConnecting;
begin
  inherited;
  WriteDebug(Format(_('Connecting to %s:%d...'), [Host, Port]), 0, 0);
end;

procedure TICEThread.DoConnected;
begin
  WriteDebug(_('Connected'), 0, 0);
  inherited;
end;

procedure TICEThread.DoDisconnected;
begin
  inherited;

  if FClosed then
    if (FTypedStream.AudioType <> atNone) then
      raise Exception.Create(_('Connection closed'));

  Sleep(100);
end;

procedure TICEThread.DoEnded;
begin
  inherited;

  FPlaying := False;
  FPlayer.Stop;

  // Noch schön ausfaden lassen
  while FPlayer.Playing or FPlayer.Pausing or FPlayer.Stopping do
  begin
    Sleep(100);
  end;

  if FTypedStream.StopAfterSong then
  begin
    if Assigned(FOnRecordingStopped) then
      Sync(FOnRecordingStopped);
  end;

  // Das hier ist die Schlaf-Zeit, die von aussen festgelegt wird. Nicht Retry-Delay!
  Sleep(FSleepTime * 1000);
end;

procedure TICEThread.DoException(E: Exception);
var
  StartTime: Cardinal;
  Delay: Cardinal;
begin
  inherited;

  FPlaying := False;
  FPlayer.Stop;

  if E.ClassType = ENoDataReceivedSentException then
    WriteDebug(Format(_('No data received/sent for more than %d seconds'), [ENoDataReceivedSentException(E).Timeout]), '', 3, 0)
  else
    WriteDebug(Format(_('%s'), [_(E.Message)]), '', 3, 0);

  Delay := FTypedStream.Settings.RetryDelay * 1000;
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

procedure TICEThread.DoStuff;
begin
  inherited;

  if FTypedStream.RemoveClient then
  begin
    FPlayer.Stop;
    Terminate;
  end;

  if FTypedStream.ClientStopRecording then
  begin
    StopRecordingInternal;
    FTypedStream.ClientStopRecording := False;

    if (not FRecording) and (not FPlaying) then
      Terminate
    else
      if Assigned(FOnRecordingStopped) then
        Sync(FOnRecordingStopped);
  end;

  while FPlayer.Pausing or FPlayer.Stopping do
    Sleep(20);

  if FRecordingStarted and (not FRecording) then
  begin
    StartRecordingInternal;
    FRecording := True;
  end;

  if FPlayingPaused and (not FPaused) then
  begin
    FPlayer.Pause;
    FPaused := True;
  end;

  if (not FPlayingPaused) and FPaused then
  begin
    StartPlayInternal;
    FPaused := False;
  end;

  if (not FRecordingStarted) and FRecording then
  begin
    StopRecordingInternal;
    FRecording := False;
  end;

  if FPlayingStarted and (not FPlaying) then
  begin
    StartPlayInternal;
  end;
  if (not FPlayingStarted) and FPlaying then
  begin
    StopPlayInternal;
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
    atOGG:
      FPlayBuffer := TOGGStreamMemory.Create;
  end;

  if (FTypedStream.HeaderType = 'icy') and
     (FTypedStream.StreamName <> '') then
  begin
    Sync(FOnRefreshInfo);
    Sync(FOnAddRecent);
  end;
end;

procedure TICEThread.DoReceivedData(Buf: Pointer; Len: Integer);
begin
  inherited;

end;

procedure TICEThread.DoSpeedChange;
begin
  inherited;

end;

procedure TICEThread.Execute;
begin
  inherited;

end;

function TICEThread.FGetPaused: Boolean;
begin
  Result := FPaused or FPlayingPaused;
end;

procedure TICEThread.LockRelay;
begin
  FPlayBufferLock.Enter;
end;

procedure TICEThread.Pause;
begin
  FPaused := not FPaused;
end;

procedure TICEThread.UnlockRelay;
begin
  FPlayBufferLock.Leave;
end;

constructor TICEThread.Create(URL: string);
var
  Host, Data: string;
  SendData: AnsiString;
  Port: Integer;
begin
  inherited Create(URL, TICEStream.Create);

  UseSynchronize := True;
  FRecording := False;
  FRecordingStarted := False;
  FPlaying := False;
  FPlayingStarted := False;
  FSleepTime := 0;

  AppGlobals.Lock;
  ProxyEnabled := AppGlobals.ProxyEnabled;
  ProxyHost := AppGlobals.ProxyHost;
  ProxyPort := AppGlobals.ProxyPort;
  AppGlobals.Unlock;

  FPlayBufferLock := TCriticalSection.Create;
  FTitle := '';
  FPlayer := TICEPlayer.Create;

  //FUserAgent := AnsiString(AppGlobals.AppName) + ' v' + AppGlobals.AppVersion.AsString;

  ParseURL(URL, Host, Port, Data);

  FTypedStream := TICEStream(FRecvStream);
  FTypedStream.OnTitleChanged := StreamTitleChanged;
  FTypedStream.OnSongSaved := StreamSongSaved;
  FTypedStream.OnNeedSettings := StreamNeedSettings;
  FTypedStream.OnChunkReceived := StreamChunkReceived;
  FTypedStream.OnIOError := StreamIOError;
  FTypedStream.OnTitleAllowed := StreamTitleAllowed;
  FTypedStream.OnRefreshInfo := StreamRefreshInfo;

  if ProxyEnabled then
    SendData := 'GET ' + AnsiString(URL) + ' HTTP/1.1'#13#10
  else
    SendData := 'GET ' + AnsiString(Data) + ' HTTP/1.1'#13#10;
  SendData := SendData + 'Host: ' + AnsiString(Host) + #13#10;
  SendData := SendData + 'Accept: */*'#13#10;
  //SendData := SendData + 'User-Agent: mhttplib/' + FUserAgent + #13#10;
  SendData := SendData + 'Icy-MetaData:1'#13#10;
  SendData := SendData + 'Connection: close'#13#10;
  SendData := SendData + #13#10;
  FSendStream.SetData(SendData);
end;

destructor TICEThread.Destroy;
begin
  FPlayer.Free;
  if FPlayBuffer <> nil then
    FPlayBuffer.Free;
  FPlayBufferLock.Free;
  inherited;
end;

end.



