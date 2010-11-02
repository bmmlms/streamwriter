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
unit ICEClient;

interface

uses
  SysUtils, Windows, StrUtils, Classes, ICEThread, ICEStream, AppData,
  Generics.Collections, Functions, SocketThread, Plugins;

type
  // Vorsicht: Das hier bestimmt die Sortierreihenfolge im MainForm.
  TICEClientStates = (csConnecting, csConnected, csStopping, csStopped, csRetrying, csIOError);

  TICEClient = class;

  TDebugEntry = class
  public
    Text: string;
    Data: string;
    Time: TDateTime;
    constructor Create(Text: string; Data: string);
  end;

  TURLList = class(TStringList)
  public
    function Add(const S: string): Integer; override;
  end;

  TDebugLog = class(TObjectList<TDebugEntry>)
  protected
    procedure Notify(const Item: TDebugEntry; Action: TCollectionNotification); override;
  end;

  TIntegerEvent = procedure(Sender: TObject; Data: Integer) of object;
  TStringEvent = procedure(Sender: TObject; Data: string) of object;
  //TString2Event = procedure(Sender: TObject; Data, Data2: string) of object;
  TSongSavedEvent = procedure(Sender: TObject; Filename, Title: string; Filesize: UInt64; WasCut: Boolean) of object;
  TTitleAllowedEvent = procedure(Sender: TObject; Title: string; var Allowed: Boolean) of object;

  TICEClient = class
  private
    FDebugLog: TDebugLog;
    FICEThread: TICEThread;
    FProcessingList: TProcessingList;

    FURLsIndex: Integer;
    FURLs: TURLList;
    FState: TICEClientStates;
    FStartURL: string;
    FRedirectedURL: string;
    FStreamName: string;
    FStreamURL: string;
    FBitRate: Cardinal;
    FGenre: string;
    FTitle: string;
    FReceived: UInt64;
    FSongsSaved: Integer;
    FSpeed: Integer;
    FContentType: string;
    FFilename: string;
    FUseFilter: TUseFilters;

    FSkipShort: Boolean;
    FKilled: Boolean;
    FRetries: Integer;

    FOnDebug: TNotifyEvent;
    FOnRefresh: TNotifyEvent;
    FOnSongSaved: TSongSavedEvent;
    FOnTitleChanged: TStringEvent;
    FOnDisconnected: TNotifyEvent;
    FOnAddRecent: TNotifyEvent;
    FOnICYReceived: TIntegerEvent;
    FOnURLsReceived: TNotifyEvent;
    FOnTitleAllowed: TTitleAllowedEvent;

    procedure Connect;
    procedure Disconnect;

    procedure Initialize;
    procedure Start;
    function FGetActive: Boolean;
    function FGetRecording: Boolean;
    function FGetPlaying: Boolean;
    function ParsePlaylist: Boolean;
    function GetURL: string;
    function FGetRelayURL: string;

    procedure ThreadDebug(Sender: TObject);
    procedure ThreadAddRecent(Sender: TObject);
    procedure ThreadSpeedChanged(Sender: TObject);
    procedure ThreadTitleChanged(Sender: TObject);
    procedure ThreadSongSaved(Sender: TObject);
    procedure ThreadStateChanged(Sender: TObject);
    procedure ThreadNeedSettings(Sender: TObject);
    procedure ThreadTitleAllowed(Sender: TObject);
    procedure ThreadEnded(Sender: TObject);
    procedure ThreadTerminated(Sender: TObject);

    procedure PluginThreadTerminate(Sender: TObject);
  public
    constructor Create(StartURL: string); overload;
    constructor Create(Name, StartURL: string); overload;
    constructor Create(Name, StartURL: string; URLs: TStringList; SkipShort: Boolean; UseFilter: TUseFilters; SongsSaved: Cardinal); overload;
    destructor Destroy; override;

    procedure WriteDebug(Text, Data: string); overload;
    procedure WriteDebug(Text: string); overload;

    procedure AddRelayThread(Thread: TSocketThread);
    procedure RemoveRelayThread(Thread: TSocketThread);

    procedure StartPlay;
    procedure StopPlay;
    procedure StartRecording;
    procedure StopRecording;

    procedure Kill;
    procedure SetSettings(SkipShort: Boolean);

    property DebugLog: TDebugLog read FDebugLog;
    property Active: Boolean read FGetActive;
    property Recording: Boolean read FGetRecording;
    property Playing: Boolean read FGetPlaying;
    property Killed: Boolean read FKilled;
    property State: TICEClientStates read FState;
    property StartURL: string read FStartURL;
    property StreamName: string read FStreamName;
    property StreamURL: string read FStreamURL;
    property BitRate: Cardinal read FBitRate;
    property Genre: string read FGenre;
    property Title: string read FTitle;
    property Received: UInt64 read FReceived write FReceived;
    property SongsSaved: Integer read FSongsSaved write FSongsSaved;
    property Speed: Integer read FSpeed;
    property URLs: TURLList read FURLs;
    property ContentType: string read FContentType;
    property Filename: string read FFilename;
    property RelayURL: string read FGetRelayURL;
    property UseFilter: TUseFilters read FUseFilter write FUseFilter;

    property SkipShort: Boolean read FSkipShort;
    property ProcessingList: TProcessingList read FProcessingList;

    property OnDebug: TNotifyEvent read FOnDebug write FOnDebug;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnAddRecent: TNotifyEvent read FOnAddRecent write FOnAddRecent;
    property OnSongSaved: TSongSavedEvent read FOnSongSaved write FOnSongSaved;
    property OnTitleChanged: TStringEvent read FOnTitleChanged write FOnTitleChanged;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnICYReceived: TIntegerEvent read FOnICYReceived write FOnICYReceived;
    property OnURLsReceived: TNotifyEvent read FOnURLsReceived write FOnURLsReceived;
    property OnTitleAllowed: TTitleAllowedEvent read FOnTitleAllowed write FOnTitleAllowed;
  end;

implementation

{ TICEClient }

constructor TICEClient.Create(StartURL: string);
begin
  inherited Create;
  Initialize;
  FStartURL := Trim(StartURL);
  WriteDebug('Client created using primary URL ' +  FStartURL, FURLs.Text);
end;

constructor TICEClient.Create(Name, StartURL: string);
begin
  Initialize;
  FStartURL := Trim(StartURL);
  FStreamName := Trim(Name);
  WriteDebug('Client created using primary URL ' +  FStartURL, FURLs.Text);
end;

constructor TICEClient.Create(Name, StartURL: string; URLs: TStringList;
  SkipShort: Boolean; UseFilter: TUseFilters; SongsSaved: Cardinal);
var
  s: string;
begin
  Create(Name, StartURL);
  if URLs <> nil then
    for s in URLs do
      FURLs.Add(s);
  FSkipShort := SkipShort;
  FUseFilter := UseFilter;
  FSongsSaved := SongsSaved;
end;

procedure TICEClient.Initialize;
begin
  FDebugLog := TDebugLog.Create;
  FProcessingList := TProcessingList.Create;

  FKilled := False;
  FState := csStopped;
  FTitle := '';
  FReceived := 0;
  FSongsSaved := 0;
  FSpeed := 0;
  FContentType := '';
  FFilename := '';
  FStartURL := '';
  FBitRate := 0;
  FRedirectedURL := '';
  FURLs := TURLList.Create;
  FURLsIndex := -1;
  FRetries := 0;

  AppGlobals.Lock;
  FSkipShort := AppGlobals.SkipShort;
  FUseFilter := AppGlobals.DefaultFilter;
  AppGlobals.Unlock;
end;

procedure TICEClient.Kill;
begin
  FKilled := True;
  Disconnect;
end;

procedure TICEClient.AddRelayThread(Thread: TSocketThread);
var
  Info: PRelayInfo;
begin
  if FICEThread <> nil then
  begin
    WriteDebug('Adding relay-thread ' + IntToStr(Thread.Handle));
    New(Info);
    Info.Thread := Thread;
    Info.FirstSent := False;
    FICEThread.LockRelay;
    try
      FICEThread.RelayThreads.Add(Info);
    finally
      FICEThread.UnlockRelay;
    end;
  end;
end;

procedure TICEClient.RemoveRelayThread(Thread: TSocketThread);
var
  i: Integer;
begin
  if FICEThread <> nil then
  begin
    FICEThread.LockRelay;
    try
      for i := 0 to FICEThread.RelayThreads.Count - 1 do
        if FICEThread.RelayThreads[i].Thread = Thread then
        begin
          WriteDebug('Removing relay-thread ' + IntToStr(Thread.Handle));
          Dispose(FICEThread.RelayThreads[i]);
          FICEThread.RelayThreads.Delete(i);
          Break;
        end;
    finally
      FICEThread.UnlockRelay;
    end;
  end;
end;

procedure TICEClient.StartPlay;
begin
  Connect;

  if FICEThread <> nil then
    FICEThread.StartPlay;
end;

procedure TICEClient.StopPlay;
begin
  if FICEThread <> nil then
  begin
    FICEThread.StopPlay;

    if (not FICEThread.Recording) and (not FICEThread.Playing) then
    begin
      Disconnect;
    end;
  end;
end;

procedure TICEClient.StartRecording;
begin
  Connect;

  if FICEThread <> nil then
    FICEThread.StartRecording;
end;

procedure TICEClient.StopRecording;
begin
  FFilename := '';
  if FICEThread <> nil then
  begin
    FICEThread.StopRecording;

    if (not FICEThread.Recording) and (not FICEThread.Playing) then
    begin
      Disconnect;
    end;
  end;
end;

procedure TICEClient.Connect;
begin
  FRetries := 0;
  Start;
end;

procedure TICEClient.Start;
begin
  if FICEThread <> nil then
    Exit;

  FState := csConnecting;
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);

  FICEThread := TICEThread.Create(GetURL);
  FICEThread.OnDebug := ThreadDebug;
  FICEThread.OnTitleChanged := ThreadTitleChanged;
  FICEThread.OnSongSaved := ThreadSongSaved;
  FICEThread.OnNeedSettings := ThreadNeedSettings;
  FICEThread.OnStateChanged := ThreadStateChanged;
  FICEThread.OnSpeedChanged := ThreadSpeedChanged;
  FICEThread.OnEnded := ThreadEnded;
  FICEThread.OnTerminate := ThreadTerminated;
  FICEThread.OnAddRecent := ThreadAddRecent;
  FICEThread.OnTitleAllowed := ThreadTitleAllowed;
  FICEThread.Start;
end;

destructor TICEClient.Destroy;
begin
  FURLs.Free;
  FDebugLog.Free;
  FProcessingList.Free;
  inherited;
end;

procedure TICEClient.Disconnect;
var
  i: Integer;
begin
  if FICEThread = nil then
    Exit;

  FRetries := 0;
  FICEThread.Terminate;
  FState := csStopping;
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

function TICEClient.FGetActive: Boolean;
var
  C: Integer;
begin
  C := 0;
  if FICEThread <> nil then
  begin
    FICEThread.LockRelay;
    C := FICEThread.RelayThreads.Count;
    FICEThread.UnlockRelay;
  end;

  Result := ((FState <> csStopped) and (FState <> csIOError)) or (FProcessingList.Count > 0) or (C > 0);
end;

function TICEClient.FGetRecording: Boolean;
begin
  Result := False;
  if FICEThread = nil then
    Exit;

  Result := ((FState <> csStopped) and (FState <> csIOError)) and FICEThread.Recording;
end;

function TICEClient.FGetPlaying: Boolean;
begin
  Result := False;
  if FICEThread = nil then
    Exit;

  Result := ((FState <> csStopped) and (FState <> csIOError)) and FICEThread.Playing;
end;

function TICEClient.FGetRelayURL: string;
begin
  Result := 'http://127.0.0.1:6000/' + StripURL(FStreamName);
end;

function TICEClient.GetURL: string;
begin
  if FRedirectedURL <> '' then
  begin
    Result := FRedirectedURL;
    FRedirectedURL := '';
    Exit;
  end;

  if (FURLsIndex = -1) and (FStartURL <> '') then
  begin
    Result := FStartURL;
    FURLsIndex := 0;
    Exit;
  end;

  if FURLs.Count > 0 then
  begin
    if FURLsIndex >= FURLs.Count then
    begin
      FURLsIndex := -1;
      Result := GetURL;
      Exit;
    end;
    if FURLsIndex = -1 then
      FURLsIndex := 0;
    Result := FURLs[FURLsIndex];
    Inc(FURLsIndex);
  end else
    Result := FStartURL;
end;

procedure TICEClient.ThreadAddRecent(Sender: TObject);
begin
  FStreamName := FICEThread.RecvStream.StreamName;
  FStreamURL := FICEThread.RecvStream.StreamURL;

  if FStreamName = '' then
    FStreamName := FStartURL;

  FContentType := FICEThread.RecvStream.ContentType;
  if FICEThread.RecvStream.BitRate > 0 then
    FBitRate := FICEThread.RecvStream.BitRate;
  if FICEThread.RecvStream.Genre <> '' then
    FGenre := FICEThread.RecvStream.Genre;

  if Assigned(FOnAddRecent) then
    FOnAddRecent(Self);
end;

procedure TICEClient.ThreadDebug(Sender: TObject);
begin
  WriteDebug(FICEThread.DebugMsg, FICEThread.DebugData);
end;

procedure TICEClient.ThreadEnded(Sender: TObject);
begin
  inherited;
  try
    if FICEThread.RecvStream.HeaderType = 'http' then
    begin
      if FICEThread.RecvStream.RedirURL <> '' then
      begin
        FURLs.Insert(0, FICEThread.RecvStream.RedirURL);
        FURLsIndex := 0;
      end else
      if FICEThread.RecvStream.RecvStream.Size > 0 then
      begin
        // Playlist
        if ParsePlaylist then
        begin
          WriteDebug('Playlist parsed', FURLs.Text);

          // ClientManager prüft, ob es in einem anderen Client schon eine der URLs gibt.
          // Wenn ja, tötet der ClientManager den neu hinzugefügten Client.
          if Assigned(FOnURLsReceived) then
            FOnURLsReceived(Self);
        end else
        begin
          raise Exception.Create('Playlist could not be parsed:'#13#10 + string(FICEThread.RecvStream.ToString(0, FICEThread.RecvStream.Size)));
        end;
      end else
      begin
        raise Exception.Create('Response was HTTP, but without playlist or redirect');
      end;
    end else
    begin
      // Am Ende noch die Bytes die nicht mitgeteilt wurden durchreichen
      FReceived := FReceived + FICEThread.Speed;
      if Assigned(FOnICYReceived) then
        FOnICYReceived(Self, FICEThread.Speed);
    end;
  except
    on E: Exception do
      FDebugLog.Add(TDebugEntry.Create('Exception: ' + E.Message, ''));
  end;
end;

procedure TICEClient.ThreadNeedSettings(Sender: TObject);
begin
  FICEThread.SetSettings(SkipShort);
  FICEThread.RecvStream.SongsSaved := FSongsSaved;
end;

procedure TICEClient.ThreadSongSaved(Sender: TObject);
var
  Data: TPluginProcessInformation;
  Entry: TProcessingEntry;
begin
  Inc(FSongsSaved);

  try
    // Pluginbearbeitung starten
    Data.Filename := FICEThread.RecvStream.SavedFilename;
    Data.Station := StreamName;
    Data.Title := FICEThread.RecvStream.SavedTitle;
    Data.TrackNumber := SongsSaved;
    Data.Filesize := FICEThread.RecvStream.SavedSize;
    Data.WasCut := FICEThread.RecvStream.SavedWasCut;

    if not FKilled then
    begin
      Entry := AppGlobals.PluginManager.ProcessFile(Data);
      if Entry <> nil then
      begin
        WriteDebug(Format('Plugin "%s" starting.', [Entry.ActiveThread.Plugin.Name]));

        Entry.ActiveThread.OnTerminate := PluginThreadTerminate;
        Entry.ActiveThread.Resume;
        FProcessingList.Add(Entry);
      end;
    end;

    if FProcessingList.Count = 0 then
    begin
      // Wenn kein Plugin die Verarbeitung übernimmt, gilt die Datei
      // jetzt schon als gespeichert. Ansonsten macht das PluginThreadTerminate.
      if Assigned(FOnSongSaved) then
        FOnSongSaved(Self, FICEThread.RecvStream.SavedFilename, FICEThread.RecvStream.SavedTitle,
          FICEThread.RecvStream.SavedSize, FICEThread.RecvStream.SavedWasCut);
      if Assigned(FOnRefresh) then
        FOnRefresh(Self);
    end;
  except
    on E: Exception do
    begin
      WriteDebug(Format('Error in ThreadSongSaved: %s', [E.Message]));
      raise;
    end;
  end;
end;

procedure TICEClient.PluginThreadTerminate(Sender: TObject);
var
  i: Integer;
  Processed: Boolean;
  Entry: TProcessingEntry;
begin
  for i := 0 to FProcessingList.Count - 1 do
  begin
    if FProcessingList[i].ActiveThread = Sender then
    begin
      Entry := FProcessingList[i];

      if Entry.ActiveThread.Result = arWin then
      begin
        WriteDebug(Format('Plugin "%s" successfully finished.', [Entry.ActiveThread.Plugin.Name]));
      end
      else
        WriteDebug(Format('Plugin "%s" failed.', [Entry.ActiveThread.Plugin.Name]));

      if FKilled then
      begin
        FProcessingList.Delete(i);
        Entry.Free;
        if FProcessingList.Count = 0 then
          if Assigned(FOnDisconnected) and (FICEThread = nil) then
            FOnDisconnected(Self);
        Exit;
      end;

      Processed := AppGlobals.PluginManager.ProcessFile(Entry);
      if Processed then
      begin
        WriteDebug(Format('Plugin "%s" starting.', [Entry.ActiveThread.Plugin.Name]));

        Entry.ActiveThread.OnTerminate := PluginThreadTerminate;
        Entry.ActiveThread.Resume;
      end else
      begin
        WriteDebug('All plugins done');

        if Assigned(FOnSongSaved) then
          FOnSongSaved(Self, Entry.Data.Filename, Entry.Data.Title, Entry.Data.Filesize, Entry.Data.WasCut);
        if Assigned(FOnRefresh) then
          FOnRefresh(Self);

        Entry.Free;
        FProcessingList.Delete(i);
      end;
      Break;
    end;
  end;
end;

procedure TICEClient.ThreadSpeedChanged(Sender: TObject);
begin
  if FICEThread.RecvStream.HeaderType = 'icy' then
  begin
    FReceived := FReceived + FICEThread.Speed;
    FSpeed := FICEThread.Speed;

    if Assigned(FOnICYReceived) then
      FOnICYReceived(Self, FICEThread.Speed);
  end;
end;

procedure TICEClient.ThreadTitleAllowed(Sender: TObject);
var
  A: Boolean;
begin
  if Assigned(FOnTitleAllowed) then
  begin
    A := True;
    FOnTitleAllowed(Self, FICEThread.RecvStream.SaveAllowedTitle, A);
    FICEThread.RecvStream.SaveAllowed := A;
  end;
end;

procedure TICEClient.ThreadTitleChanged(Sender: TObject);
begin
  FTitle := FICEThread.RecvStream.Title;
  if Assigned(FOnTitleChanged) then
    FOnTitleChanged(Self, FICEThread.RecvStream.Title);
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

procedure TICEClient.ThreadStateChanged(Sender: TObject);
begin
  if FState <> csStopping then
  begin
    case FICEThread.State of
      tsRecording:
        begin
          FFilename := FICEThread.RecvStream.Filename;
          FRetries := 0;
          FState := csConnected;
        end;
      tsRetrying:
        begin
          FState := csRetrying;
        end;
      tsIOError:
        begin
          FState := csIOError;
        end;
    end;
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);
  end;
end;

procedure TICEClient.ThreadTerminated(Sender: TObject);
var
  MaxRetries: Integer;
  DiedThread: TICEThread;
begin
  if FICEThread <> Sender then
    Exit;

  DiedThread := TICEThread(Sender);

  FICEThread := nil;
  FTitle := '';
  FSpeed := 0;
  FFilename := '';
  AppGlobals.Lock;
  MaxRetries := AppGlobals.MaxRetries;
  AppGlobals.Unlock;

  if (FState <> csStopping) and (FState <> csIOError) then
  begin
    if (FRetries >= MaxRetries) and (MaxRetries > 0) then
    begin
      WriteDebug('Retried ' + IntToStr(MaxRetries) + ' times, stopping');
      FState := csStopped;
    end else
    begin
      Start;
      if DiedThread.Playing then
        FICEThread.StartPlay;
      if DiedThread.Recording then
        FICEThread.StartRecording;
    end;
    if FRedirectedURL = '' then
      Inc(FRetries);
  end else if FState = csStopping then
    FState := csStopped;

  if Assigned(FOnRefresh) then
    FOnRefresh(Self);

  if Assigned(FOnDisconnected) and (FICEThread = nil) then
    FOnDisconnected(Self);
end;

procedure TICEClient.WriteDebug(Text, Data: string);
begin
  FDebugLog.Add(TDebugEntry.Create(Text, Data));
  if Assigned(FOnDebug) then
    FOnDebug(Self);
end;

procedure TICEClient.WriteDebug(Text: string);
begin
  WriteDebug(Text, '');
end;

function TICEClient.ParsePlaylist: Boolean;
var
  Offset, Offset2: Integer;
  Host, URLData: string;
  Port: Integer;
  Data, Line: string;
  PortDetected: Boolean;
begin
  FURLs.Clear;
  Offset := 1;
  Data := string(FICEThread.RecvStream.RecvStream.ToString);
  if Copy(LowerCase(Data), 1, 10) = '[playlist]' then // .pls
  begin
    while True do
    begin
      Offset2 := PosEx(#10, Data, Offset);
      if Offset2 > 0 then
        Line := Trim(Copy(Data, Offset, Offset2 - Offset))
      else
        Line := Trim(Copy(Data, Offset, Length(Data)));

      Offset := Offset2 + 1;

      if Copy(LowerCase(Line), 1, 4) = 'file' then
      begin
        Offset2 := Pos('=', Line);
        if Offset2 > 0 then
        begin
          Line := Trim(Copy(Line, Offset2 + 1, Length(Line) - Offset2));
          if (Line <> '') then
          begin
            if ParseURL(Line, Host, Port, URLData, PortDetected) then
            begin
              if not PortDetected then
                FURLs.Add(Host + ':6666' + URLData)
              else
                FURLs.Add(Host + ':' + IntToStr(Port) + URLData);
            end;
          end;
        end;
      end;

      if Offset2 = 0 then
        Break;
    end;
  end else if LowerCase(Copy(Data, 1, 7)) = '#extm3u' then // .m3u
  begin
    while True do
    begin
      Offset2 := PosEx(#10, Data, Offset);
      if Offset2 > 0 then
        Line := Trim(Copy(Data, Offset, Offset2 - Offset))
      else
        Line := Trim(Copy(Data, Offset, Length(Data)));

      Offset := Offset2 + 1;

      if (Length(Line) >= 1) and (Line[1] <> '#') then
      begin
        if ParseURL(Line, Host, Port, URLData, PortDetected) then
        begin
          if not PortDetected then
            FURLs.Add(Host + ':6666' + URLData)
          else
            FURLs.Add(Host + ':' + IntToStr(Port) + URLData);
        end;
      end;

      if Offset2 = 0 then
        Break;
    end;
  end else
  begin
    // Im Notfall alles was empfangen wurde als URLs interpretieren.
    // Siehe z.B. http://www.rockantenne.de/webradio/rockantenne.m3u
    if FICEThread.RecvStream.Size < 102400 then
    begin
      while True do
      begin
        Offset2 := PosEx(#10, Data, Offset);
        if Offset2 > 0 then
          Line := Trim(Copy(Data, Offset, Offset2 - Offset))
        else
          Line := Trim(Copy(Data, Offset, Length(Data)));

        Offset := Offset2 + 1;

        if ParseURL(Line, Host, Port, URLData, PortDetected) then
        begin
          if not PortDetected then
            FURLs.Add(Host + ':6666' + URLData)
          else
            FURLs.Add(Host + ':' + IntToStr(Port) + URLData);
        end;

        if Offset2 = 0 then
          Break;
      end;
    end;
  end;
  Result := FURLs.Count > 0;
  FURLsIndex := 0;
end;

procedure TICEClient.SetSettings(SkipShort: Boolean);
begin
  FSkipShort := SkipShort;
end;

{ TDebugEntry }

constructor TDebugEntry.Create(Text, Data: string);
begin
  Self.Text := Text;
  Self.Data := Data;
  Self.Time := Now;
end;

{ TURLList }

function TURLList.Add(const S: string): Integer;
var
  i, Port: Integer;
  Host, Data: string;
  S2: string;
begin
  Result := -1;
  try
    S2 := s;
    if not ParseURL(S2, Host, Port, Data) then // REMARK: Der Check auf True ist hier neu.
      Exit;
  except
    Exit;
  end;
  if Length(Host) <= 3 then
    Exit;
  for i := 0 to Count - 1 do
    if LowerCase(S2) = LowerCase(Self[i]) then
      Exit;
  Result := inherited;
end;

{ TDebugLog }

procedure TDebugLog.Notify(const Item: TDebugEntry;
  Action: TCollectionNotification);
var
  i: Integer;
begin
  inherited;
  if (Action = cnAdded) and (Count > 1000) then
    for i := Count - 500 downto 0 do
      Delete(i);
end;

end.
