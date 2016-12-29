{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2017 Alexander Nottelmann

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

{ This unit contains the ICEClient which connects to a stream and records it.
  It makes heavy use of ICEThread which uses TICEThread to record songs }
unit ICEClient;

interface

uses
  SysUtils, Windows, StrUtils, Classes, ICEThread, ICEStream, AppData,
  Generics.Collections, Functions, Sockets, LanguageObjects,
  DataManager, HomeCommunication, PlayerManager, Notifications,
  Logging, PlaylistHandler, AudioFunctions, TypeDefs, MessageBus,
  AppMessages;

type
  // Vorsicht: Das hier bestimmt die Sortierreihenfolge im MainForm.
  TICEClientStates = (csConnecting, csConnected, csStopping, csStopped, csRetrying, csIOError);

  TMayConnectResults = (crOk, crNoFreeSpace, crNoBandwidth, crDirDoesNotExist);
  TMayConnectResultsSet = set of TMayConnectResults;

  TICEClient = class;

  TIntegerEvent = procedure(Sender: TObject; Data: Integer) of object;
  TSongSavedEvent = procedure(Sender: TObject; Filename, Title, SongArtist, SongTitle: string;
    Filesize, Length, Bitrate: UInt64; VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
    ServerTitleHash, ServerArtistHash: Cardinal) of object;
  TTitleAllowedEvent = procedure(Sender: TObject; Title: string; var Allowed: Boolean; var Match: string; var Filter: Integer) of object;

  TLogEntry = class
  public
    Text: string;
    Data: string;
    T: TLogType;
    Level: TLogLevel;
    Time: TDateTime;
    constructor Create(Text, Data: string; T: TLogType; Level: TLogLevel);
  end;

  TURLList = class(TStringList)
  public
    function Add(const S: string): Integer; override;
  end;

  TLog = class(TObjectList<TLogEntry>)
  protected
    procedure Notify(const Item: TLogEntry; Action: TCollectionNotification); override;
  end;

  TICEClient = class
  private
    FManager: TObject;
    FEntry: TStreamEntry;

    FDebugLog: TLog;
    FICEThread: TICEThread;

    FURLsIndex: Integer;
    FCurrentURL: string;
    FState: TICEClientStates;
    FRedirectedURL: string;
    FGenre: string;
    FTitle: string;
    FDisplayTitle: string;
    FSpeed: Integer;
    FContentType: string;
    FFilename: string;
    FScheduledRecording: Boolean;
    FIsMonitoring: Boolean;

    FAutoRemove: Boolean;
    FRecordTitle: string;
    FParsedRecordTitle: string;
    FRecordServerTitle: string;
    FRecordTitleHash: Cardinal;
    FRecordArtistHash: Cardinal;
    FRecordBecauseArtist: Boolean;
    FStopAfterSong: Boolean;
    FStopped: Boolean;
    FKilled: Boolean;
    FRetries: Integer;

    FEQEnabled: Boolean;

    FOnLog: TNotifyEvent;
    FOnRefresh: TNotifyEvent;
    FOnSongSaved: TSongSavedEvent;
    FOnTitleChanged: TStringEvent;
    FOnDisplayTitleChanged: TStringEvent;
    FOnDisconnected: TNotifyEvent;
    FOnAddRecent: TNotifyEvent;
    FOnICYReceived: TIntegerEvent;
    FOnURLsReceived: TNotifyEvent;
    FOnTitleAllowed: TTitleAllowedEvent;
    FOnPlaybackStarted: TNotifyEvent;
    FOnSecondsReceived: TNotifyEvent;

    FOnPlay: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnStop: TNotifyEvent;

    procedure Connect;
    procedure Disconnect;

    procedure Initialize;
    procedure Start;
    function FGetActive: Boolean;
    function FGetRecording: Boolean;
    function FGetPaused: Boolean;
    function FGetPlayingPaused: Boolean;
    function FGetPlayingStarted: Boolean;
    function FGetPlaying: Boolean;
    procedure FSetEQEnabled(Value: Boolean);
    function ParsePlaylist: Boolean;
    function GetURL: string;

    procedure ThreadLog(Sender: TSocketThread);
    procedure ThreadAddRecent(Sender: TSocketThread);
    procedure ThreadSpeedChanged(Sender: TSocketThread);
    procedure ThreadTitleChanged(Sender: TSocketThread);
    procedure ThreadDisplayTitleChanged(Sender: TSocketThread);
    procedure ThreadSongSaved(Sender: TSocketThread);
    procedure ThreadStateChanged(Sender: TSocketThread);
    procedure ThreadNeedSettings(Sender: TSocketThread);
    procedure ThreadTitleAllowed(Sender: TSocketThread);
    procedure ThreadRefreshInfo(Sender: TSocketThread);
    procedure ThreadRecordingStopped(Sender: TSocketThread);
    procedure ThreadPlaybackStarted(Sender: TSocketThread);
    procedure ThreadMilliSecondsReceived(Sender: TSocketThread);
    procedure ThreadBeforeEnded(Sender: TSocketThread);
    procedure ThreadTerminated(Sender: TObject);
    procedure ThreadExtLog(Sender: TSocketThread);
    //procedure ThreadMonitorAnalyzerAnalyzed(Sender: TSocketThread);
  public
    constructor Create(Manager: TObject; StartURL: string); overload;
    constructor Create(Manager: TObject; ID, Bitrate: Cardinal; Name, StartURL: string); overload;
    constructor Create(Manager: TObject; Entry: TStreamEntry); overload;
    destructor Destroy; override;

    procedure WriteLog(Text, Data: string; T: TLogType; Level: TLogLevel); overload;
    procedure WriteLog(Text: string; T: TLogType; Level: TLogLevel); overload;

    function StartPlay(CheckConditions: Boolean): TMayConnectResults;
    procedure PausePlay;
    procedure StopPlay;
    class function MayConnect(PlayOnly: Boolean; UsedBandwidth: Integer; IsAuto: Boolean): TMayConnectResults;
    function StartRecording(CheckConditions: Boolean): TMayConnectResults;
    procedure StartMonitoring;
    procedure StopRecording;
    procedure SetVolume(Vol: Integer);
    procedure SetEQ(Value, Freq: Integer);
    procedure PostProcessingFinished(Filename, Title, SongArtist, SongTitle: string;
      Filesize, Length, Bitrate: UInt64; VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
      ServerTitleHash, ServerArtistHash: Cardinal);
    function IsCurrentTimeInSchedule(ExcludeSchedule: TSchedule = nil): Boolean;

    procedure Stop;
    procedure Kill;

    property AutoRemove: Boolean read FAutoRemove write FAutoRemove;
    property RecordTitle: string read FRecordTitle write FRecordTitle;
    property ParsedRecordTitle: string read FParsedRecordTitle write FParsedRecordTitle;
    property RecordServerTitle: string read FRecordServerTitle write FRecordServerTitle;
    property RecordTitleHash: Cardinal read FRecordTitleHash write FRecordTitleHash;
    property RecordArtistHash: Cardinal read FRecordArtistHash write FRecordArtistHash;
    property RecordBecauseArtist: Boolean read FRecordBecauseArtist write FRecordBecauseArtist;
    property StopAfterSong: Boolean read FStopAfterSong write FStopAfterSong;

    property Entry: TStreamEntry read FEntry;

    property DebugLog: TLog read FDebugLog;
    property Active: Boolean read FGetActive;
    property Recording: Boolean read FGetRecording;
    property Playing: Boolean read FGetPlaying;
    property Paused: Boolean read FGetPaused;
    property PlayingPaused: Boolean read FGetPlayingPaused;
    property PlayingStarted: Boolean read FGetPlayingStarted;
    property Killed: Boolean read FKilled;
    property State: TICEClientStates read FState;
    property Genre: string read FGenre;
    property Title: string read FTitle;
    property DisplayTitle: string read FDisplayTitle;
    property Speed: Integer read FSpeed;
    property ContentType: string read FContentType;
    property Filename: string read FFilename;
    property ScheduledRecording: Boolean read FScheduledRecording write FScheduledRecording;

    property EQEnabled: Boolean read FEQEnabled write FSetEQEnabled;

    property OnLog: TNotifyEvent read FOnLog write FOnLog;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnAddRecent: TNotifyEvent read FOnAddRecent write FOnAddRecent;
    property OnSongSaved: TSongSavedEvent read FOnSongSaved write FOnSongSaved;
    property OnTitleChanged: TStringEvent read FOnTitleChanged write FOnTitleChanged;
    property OnDisplayTitleChanged: TStringEvent read FOnDisplayTitleChanged write FOnDisplayTitleChanged;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnICYReceived: TIntegerEvent read FOnICYReceived write FOnICYReceived;
    property OnURLsReceived: TNotifyEvent read FOnURLsReceived write FOnURLsReceived;
    property OnTitleAllowed: TTitleAllowedEvent read FOnTitleAllowed write FOnTitleAllowed;
    property OnPlaybackStarted: TNotifyEvent read FOnPlaybackStarted write FOnPlaybackStarted;
    property OnSecondsReceived: TNotifyEvent read FOnSecondsReceived write FOnSecondsReceived;

    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

uses
  ClientManager, PostProcess;

{ TICEClient }

constructor TICEClient.Create(Manager: TObject; StartURL: string);
begin
  inherited Create;
  FManager := Manager;
  Initialize;
  FEntry.StartURL := Trim(StartURL);
end;

constructor TICEClient.Create(Manager: TObject; ID, Bitrate: Cardinal; Name, StartURL: string);
begin
  FManager := Manager;
  Initialize;
  FEntry.ID := ID;
  FEntry.Bitrate := Bitrate;
  FEntry.StartURL := Trim(StartURL);
  FEntry.Name := Trim(Name);
  FEntry.CustomName := Trim(Name);
end;

constructor TICEClient.Create(Manager: TObject; Entry: TStreamEntry);
begin
  FManager := Manager;
  Initialize;
  FEntry.Assign(Entry);
end;

procedure TICEClient.Initialize;
begin
  Players.AddPlayer(Self);

  FDebugLog := TLog.Create;

  FEntry := TStreamEntry.Create;
  FEntry.Settings.Assign(AppGlobals.Data.StreamSettings);

  FStopped := False;
  FKilled := False;
  FState := csStopped;
  FTitle := '';
  FDisplayTitle := '';
  FSpeed := 0;
  FContentType := '';
  FFilename := '';
  FRedirectedURL := '';
  FURLsIndex := -1;
  FRetries := 0;
end;

procedure TICEClient.Kill;
begin
  FKilled := True;
  Disconnect;
end;

function TICEClient.StartPlay(CheckConditions: Boolean): TMayConnectResults;
begin
  Result := crOK;

  if CheckConditions then
  begin
    Result := MayConnect(True, TClientManager(FManager).GetUsedBandwidth(FEntry.Bitrate, FSpeed, Self), False);

    if Result <> crOk then
      Exit;
  end;

  Connect;

  if FICEThread <> nil then
  begin
    FICEThread.StartPlay;

    if Assigned(FOnPlay) then
      FOnPlay(Self);

    MsgBus.SendMessage(TPlayingObjectChangedMsg.Create(Self, '', FTitle, FEntry.Name, ''));
  end;
end;

procedure TICEClient.PausePlay;
begin
  if FICEThread <> nil then
  begin
    FICEThread.PausePlay;

    if Assigned(FOnPause) then
      FOnPause(Self);

    if FICEThread.PlayingPaused then
      MsgBus.SendMessage(TPlayingObjectStopped.Create(Self))
    else
      MsgBus.SendMessage(TPlayingObjectChangedMsg.Create(Self, '', FTitle, FEntry.Name, ''));
  end;
end;

procedure TICEClient.PostProcessingFinished(Filename, Title, SongArtist,
  SongTitle: string; Filesize, Length, Bitrate: UInt64; VBR, WasCut, FullTitle,
  IsStreamFile, RecordBecauseArtist: Boolean; ServerTitleHash, ServerArtistHash: Cardinal);
begin
  if Assigned(FOnSongSaved) then
    FOnSongSaved(Self, Filename, Title, SongArtist, SongTitle, Filesize, Length, Bitrate,
      VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist, ServerTitleHash, ServerArtistHash);
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);

  if (FAutoRemove or FKilled) and Assigned(FOnDisconnected) and (FICEThread = nil) then
  begin
    Kill;
    FOnDisconnected(Self);
  end;
end;

procedure TICEClient.Stop;
begin
  FStopped := True;
  Disconnect;
end;

procedure TICEClient.StopPlay;
begin
  if FICEThread <> nil then
  begin
    FICEThread.StopPlay;

    MsgBus.SendMessage(TPlayingObjectStopped.Create(Self));

    if Assigned(FOnStop) then
      FOnStop(Self);

    if (not FICEThread.Recording) and (not FICEThread.Playing) and (not FICEThread.Paused) then
    begin
      Disconnect;
    end;
  end;
end;

class function TICEClient.MayConnect(PlayOnly: Boolean; UsedBandwidth: Integer; IsAuto: Boolean): TMayConnectResults;
var
  Dir: string;
begin
  Result := crOk;

  if not PlayOnly then
  begin
    if IsAuto then
      Dir := AppGlobals.DirAuto
    else
      Dir := AppGlobals.Dir;

    if (Dir = '') or (not DirectoryExists(Dir)) then
      Exit(crDirDoesNotExist);

    if not DiskSpaceOkay(Dir, AppGlobals.MinDiskSpace) then
      Exit(crNoFreeSpace);
  end;

  if AppGlobals.LimitSpeed and (AppGlobals.MaxSpeed > 0) then
    if Cardinal(UsedBandwidth) > AppGlobals.MaxSpeed then
      Result := crNoBandwidth;
end;

function TICEClient.StartRecording(CheckConditions: Boolean): TMayConnectResults;
begin
  Result := crOk;

  if CheckConditions then
  begin
    Result := MayConnect(False, TClientManager(FManager).GetUsedBandwidth(FEntry.Bitrate, FSpeed, Self), FAutoRemove);
    if Result <> crOk then
      Exit;
  end;

  Connect;

  if FICEThread <> nil then
    FICEThread.StartRecording;
end;

procedure TICEClient.StopRecording;
begin
  FStopAfterSong := False;

  FFilename := '';
  if FICEThread <> nil then
  begin
    FICEThread.StopRecording;

    if (not FICEThread.Recording) and (not FICEThread.Playing) then
      Disconnect;
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

  FCurrentURL := GetURL;
  FICEThread := TICEThread.Create(FCurrentURL);
  FICEThread.OnLog := ThreadLog;
  FICEThread.OnTitleChanged := ThreadTitleChanged;
  FICEThread.OnDisplayTitleChanged := ThreadDisplayTitleChanged;
  FICEThread.OnSongSaved := ThreadSongSaved;
  FICEThread.OnNeedSettings := ThreadNeedSettings;
  FICEThread.OnStateChanged := ThreadStateChanged;
  FICEThread.OnSpeedChanged := ThreadSpeedChanged;
  FICEThread.OnBeforeEnded := ThreadBeforeEnded;
  FICEThread.OnTerminate := ThreadTerminated;
  FICEThread.OnAddRecent := ThreadAddRecent;
  FICEThread.OnTitleAllowed := ThreadTitleAllowed;
  FICEThread.OnRefreshInfo := ThreadRefreshInfo;
  FICEThread.OnRecordingStopped := ThreadRecordingStopped;
  FICEThread.OnPlaybackStarted := ThreadPlaybackStarted;
  FICEThread.OnMilliSecondsReceived := ThreadMilliSecondsReceived;
  FICEThread.OnExtLog := ThreadExtLog;
  //FICEThread.OnMonitorAnalyzerAnalyzed := ThreadMonitorAnalyzerAnalyzed;

  // Das muss hier so früh sein, wegen z.B. RetryDelay - das hat der Stream nämlich nicht,
  // wenn z.B. beim Verbinden was daneben geht.
  ThreadNeedSettings(FICEThread);

  FICEThread.Start;
end;

procedure TICEClient.StartMonitoring;
begin
  FIsMonitoring := True;

  Connect;

  if FICEThread <> nil then
    FICEThread.StartMonitoring;
end;

destructor TICEClient.Destroy;
begin
  Players.RemovePlayer(Self);
  FEntry.Free;
  FDebugLog.Free;

  inherited;
end;

procedure TICEClient.Disconnect;
begin
  if FICEThread = nil then
    Exit;

  FScheduledRecording := False;

  FState := csStopping;

  FRetries := 0;
  FICEThread.StopPlay;

  // Weil das Signal nicht mehr durchkommt, machen wir das hier per Hand.
  MsgBus.SendMessage(TPlayingObjectStopped.Create(Self));

  FICEThread.Terminate;
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

function TICEClient.FGetActive: Boolean;
begin
  Result := ((FState <> csStopped) and (FState <> csIOError)) or (AppGlobals.PostProcessManager.WorkingForClient(Self));
end;

function TICEClient.IsCurrentTimeInSchedule(ExcludeSchedule: TSchedule = nil): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Entry.Schedules.Count - 1 do
    if Entry.Schedules[i] <> ExcludeSchedule then
      if (Entry.Schedules[i].GetStartTime(False) < Now) and (Entry.Schedules[i].GetEndTime(Entry.Schedules[i].GetStartTime(False)) > Now) then
        Exit(True);
end;

function TICEClient.FGetRecording: Boolean;
begin
  Result := False;
  if FICEThread = nil then
    Exit;

  Result := ((FState <> csStopped) and (FState <> csIOError)) and FICEThread.Recording;
end;

procedure TICEClient.FSetEQEnabled(Value: Boolean);
begin
  FEQEnabled := Value;

  if FICEThread <> nil then
  begin
    FICEThread.SetEQEnabled(Value);
  end;
end;

function TICEClient.FGetPaused: Boolean;
begin
  Result := False;
  if FICEThread = nil then
    Exit;

  Result := ((FState <> csStopped) and (FState <> csIOError)) and FICEThread.Paused;
end;

function TICEClient.FGetPlaying: Boolean;
begin
  Result := False;
  if FICEThread = nil then
    Exit;

  Result := ((FState <> csStopped) and (FState <> csIOError)) and FICEThread.Playing;
end;

function TICEClient.FGetPlayingPaused: Boolean;
begin
  Result := False;
  if FICEThread = nil then
    Exit;

  Result := FICEThread.PlayingPaused;
end;

function TICEClient.FGetPlayingStarted: Boolean;
begin
  Result := False;
  if FICEThread = nil then
    Exit;

  Result := FICEThread.PlayingStarted;
end;

function TICEClient.GetURL: string;
begin
  if FRedirectedURL <> '' then
  begin
    Result := FRedirectedURL;
    FRedirectedURL := '';
    Exit;
  end;

  if (FURLsIndex = -1) and (FEntry.StartURL <> '') then
  begin
    Result := FEntry.StartURL;
    FURLsIndex := 0;
    Exit;
  end;

  if FEntry.URLs.Count > 0 then
  begin
    if FURLsIndex >= FEntry.URLs.Count then
    begin
      if (FEntry.StartURL <> '') and (Pos('streamwriter.', LowerCase(FEntry.StartURL)) = 0) then
      begin
        Result := FEntry.StartURL;
        FURLsIndex := 0;
        Exit;
      end else
        FURLsIndex := 0;
    end;
    if FURLsIndex = -1 then
      FURLsIndex := 0;
    Result := FEntry.URLs[FURLsIndex];
    Inc(FURLsIndex);
  end else
    Result := FEntry.StartURL;
end;

procedure TICEClient.ThreadAddRecent(Sender: TSocketThread);
begin
  if Assigned(FOnAddRecent) then
    FOnAddRecent(Self);
end;

procedure TICEClient.ThreadLog(Sender: TSocketThread);
var
  Level: TLogLevel;
begin
  case FICEThread.LogLevel of
    slError: Level := llError;
    slWarning: Level := llWarning;
    slInfo: Level := llInfo;
    slDebug: Level := llDebug;
    else raise Exception.Create('Unknown FICEThread.LogLevel');
  end;

  WriteLog(FICEThread.LogMsg, FICEThread.LogData, ltGeneral, Level);
end;

procedure TICEClient.ThreadMilliSecondsReceived(Sender: TSocketThread);
begin
  Entry.SecondsReceived := Entry.SecondsReceived + 1;

  if Assigned(FOnSecondsReceived) then
    FOnSecondsReceived(Self);
end;

//procedure TICEClient.ThreadMonitorAnalyzerAnalyzed(Sender: TSocketThread);
//begin
//  HomeComm.SendStreamAnalyzationData(FEntry.ID, TICEThread(Sender).RecvStream.MonitorAnalyzer.WaveDataStream);
//end;

procedure TICEClient.ThreadBeforeEnded(Sender: TSocketThread);
begin
  inherited;

  if FICEThread.Error then
    Exit;

  try
    // Das hier kann bei HTTP und bei ICY kommen.
    if FICEThread.RecvStream.RedirURL <> '' then
    begin
      WriteLog(_('Redirection found'), ltGeneral, llInfo);
      FRedirectedURL := FICEThread.RecvStream.RedirURL;
      Exit;
    end;

    if FICEThread.RecvStream.HeaderType = 'http' then
    begin
      if ParsePlaylist then
      begin
        {$IFDEF DEBUG}
        WriteLog(_('Playlist parsed'), FEntry.URLs.Text, ltGeneral, llInfo);
        {$ELSE}
        WriteLog(_('Playlist parsed'), ltGeneral, llInfo);
        {$ENDIF}

        // ClientManager prüft, ob es in einem anderen Client schon eine der URLs gibt.
        // Wenn ja, tötet der ClientManager den neu hinzugefügten Client.
        if Assigned(FOnURLsReceived) then
          FOnURLsReceived(Self);
      end else
      begin
        if FICEThread.RecvStream.ResponseCode = 404 then
          raise Exception.Create(_('HTTP error 404, document not found'))
        else
          if IntToStr(FICEThread.RecvStream.ResponseCode)[1] <> '2' then
            raise Exception.Create(Format(_('HTTP error %d'),
              [FICEThread.RecvStream.ResponseCode]))
          else
            raise Exception.Create(_('Response was HTTP but without supported playlist or redirect'));
      end;
    end else
    begin
      // Am Ende noch die Bytes die nicht mitgeteilt wurden durchreichen
      FEntry.BytesReceived := FEntry.BytesReceived + FICEThread.Speed;
      if Assigned(FOnICYReceived) then
        FOnICYReceived(Self, FICEThread.Speed);
    end;
  except
    on E: Exception do
    begin
      // Dann geht der Thread in Retrying und wartet etwas...
      FICEThread.Error := True;

      WriteLog(Format(_('Error: %s'), [E.Message]), '', ltGeneral, llError);

      if Assigned(FOnRefresh) then
        FOnRefresh(Self);
    end;
  end;
end;

procedure TICEClient.ThreadNeedSettings(Sender: TSocketThread);
begin
  FICEThread.SetSettings(FEntry.Settings, FAutoRemove, FStopAfterSong, FKilled, FRecordTitle, FParsedRecordTitle, FEntry.SongsSaved, FEntry.CustomName);
end;

procedure TICEClient.ThreadPlaybackStarted(Sender: TSocketThread);
begin
  if Assigned(FOnPlaybackStarted) then
    FOnPlaybackStarted(Self);
end;

procedure TICEClient.ThreadRecordingStopped(Sender: TSocketThread);
begin
  StopRecording;
  FStopAfterSong := False;
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

procedure TICEClient.ThreadRefreshInfo(Sender: TSocketThread);
var
  KeepCustom: Boolean;
begin
  KeepCustom := False;
  if FEntry.Name <> FEntry.CustomName then
    KeepCustom := True;

  FEntry.Name := FICEThread.RecvStream.StreamName;
  if not KeepCustom then
    FEntry.CustomName := FEntry.Name;
  if FEntry.Name = '' then
  begin
    FEntry.Name := FEntry.StartURL;
    if not KeepCustom then
      FEntry.CustomName := FEntry.Name;
  end;

  FEntry.StreamURL := FICEThread.RecvStream.StreamURL;

  FContentType := FICEThread.RecvStream.ContentType;
  if FICEThread.RecvStream.AudioInfo.Bitrate > 0 then
    FEntry.Bitrate := FICEThread.RecvStream.AudioInfo.Bitrate;
  if FICEThread.RecvStream.Genre <> '' then
    FEntry.Genre := FICEThread.RecvStream.Genre;

  FEntry.VBR := FICEThread.RecvStream.AudioInfo.VBR;

  FEntry.AudioType := FICEThread.RecvStream.AudioType;
end;

procedure TICEClient.ThreadSongSaved(Sender: TSocketThread);
var
  Data: TPostProcessInformation;
begin
  if not FICEThread.RecvStream.SavedIsStreamFile then
  begin
    FEntry.SongsSaved := FEntry.SongsSaved + 1;

    if not FAutoRemove then
      FRecordServerTitle := FICEThread.RecvStream.OriginalStreamTitle;

    if FKilled or FStopped then
    begin
      if Assigned(FOnSongSaved) then
        FOnSongSaved(Self, FICEThread.RecvStream.SavedFilename, FICEThread.RecvStream.SavedStreamTitle,
          FICEThread.RecvStream.SavedArtist, FICEThread.RecvStream.SavedTitle, FICEThread.RecvStream.SavedSize,
          FICEThread.RecvStream.SavedLength, FICEThread.RecvStream.AudioInfo.Bitrate, FICEThread.RecvStream.AudioInfo.VBR,
          FICEThread.RecvStream.SavedWasCut, FICEThread.RecvStream.SavedFullTitle, False, FRecordBecauseArtist,
          FRecordTitleHash, FRecordArtistHash);
    end else
    begin
      Data.Filename := FICEThread.RecvStream.SavedFilename;
      Data.FilenameConverted := FICEThread.RecvStream.SavedFilenameConverted;
      Data.WorkFilename := '';
      Data.Station := FEntry.Name;
      Data.Artist := FICEThread.RecvStream.SavedArtist;
      Data.Title := FICEThread.RecvStream.SavedTitle;
      Data.Album := FICEThread.RecvStream.SavedAlbum;
      Data.TrackNumber := FEntry.SongsSaved;
      Data.Filesize := FICEThread.RecvStream.SavedSize;
      Data.Length := FICEThread.RecvStream.SavedLength;
      Data.WasCut := FICEThread.RecvStream.SavedWasCut;
      Data.FullTitle := FICEThread.RecvStream.SavedFullTitle;
      Data.StreamTitle := FICEThread.RecvStream.SavedStreamTitle;
      Data.Bitrate := FICEThread.RecvStream.AudioInfo.Bitrate;
      Data.ServerTitleHash := FRecordTitleHash;
      Data.ServerArtistHash := FRecordArtistHash;
      Data.RecordBecauseArtist := FRecordBecauseArtist;
      Data.VBR := FICEThread.RecvStream.AudioInfo.VBR;

      if Entry.Settings.OutputFormat = atNone then
      begin
        Data.EncoderSettings := Entry.Settings.EncoderSettings.Find(FICEThread.RecvStream.AudioType).Copy;
        TEncoderSettings(Data.EncoderSettings).AudioType := atNone;

        // Falls zwischendurch nach WAVE konvertiert wird, würde die Datei danach mit Default-Einstellungen neu enkodiert werden.
        // Das will man aber nicht, wenn der Stream 320kbps eigentlich hat. Deshalb das folgende.
        if Data.VBR then
          TEncoderSettings(Data.EncoderSettings).BitrateType := brVBR
        else
          TEncoderSettings(Data.EncoderSettings).BitrateType := brCBR;
        TEncoderSettings(Data.EncoderSettings).CBRBitrate := Data.Bitrate;
        TEncoderSettings(Data.EncoderSettings).VBRQuality := GuessVBRQuality(Data.Bitrate, FICEThread.RecvStream.AudioType);
      end else
        Data.EncoderSettings := Entry.Settings.EncoderSettings.Find(Entry.Settings.OutputFormat).Copy;

      if not AppGlobals.PostProcessManager.ProcessFile(Self, Data) then
      begin
        if Assigned(FOnSongSaved) then
          FOnSongSaved(Self, FICEThread.RecvStream.SavedFilename, FICEThread.RecvStream.SavedStreamTitle,
            FICEThread.RecvStream.SavedArtist, FICEThread.RecvStream.SavedTitle, FICEThread.RecvStream.SavedSize,
            FICEThread.RecvStream.SavedLength, FICEThread.RecvStream.AudioInfo.Bitrate, FICEThread.RecvStream.AudioInfo.VBR,
            FICEThread.RecvStream.SavedWasCut, FICEThread.RecvStream.SavedFullTitle, False, FRecordBecauseArtist,
            FRecordTitleHash, FRecordArtistHash);
        if Assigned(FOnRefresh) then
          FOnRefresh(Self);
      end;
    end;

    if FAutoRemove then
    begin
      Kill;
    end;
  end else
  begin
    if Assigned(FOnSongSaved) then
      FOnSongSaved(Self, FICEThread.RecvStream.SavedFilename, FICEThread.RecvStream.SavedStreamTitle,
        FICEThread.RecvStream.SavedArtist, FICEThread.RecvStream.SavedTitle, FICEThread.RecvStream.SavedSize,
        FICEThread.RecvStream.SavedLength, FICEThread.RecvStream.AudioInfo.Bitrate, FICEThread.RecvStream.AudioInfo.VBR,
        FICEThread.RecvStream.SavedWasCut, FICEThread.RecvStream.SavedFullTitle, True, FRecordBecauseArtist, 0, 0);
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);
  end;

  if FICEThread.RecvStream.SavedFullTitle then
    HomeComm.SendClientStats(AutoRemove);
end;

procedure TICEClient.ThreadSpeedChanged(Sender: TSocketThread);
begin
  if FICEThread.RecvStream.HeaderType = 'icy' then
  begin
    FEntry.BytesReceived := FEntry.BytesReceived + FICEThread.Speed;
    FSpeed := FICEThread.Speed;

    if Assigned(FOnICYReceived) then
      FOnICYReceived(Self, FICEThread.Speed);
  end;
end;

procedure TICEClient.ThreadTitleAllowed(Sender: TSocketThread);
var
  A: Boolean;
  M: string;
  F: Integer;
begin
  if Assigned(FOnTitleAllowed) then
  begin
    A := True;
    FOnTitleAllowed(Self, FICEThread.RecvStream.SaveAllowedTitle, A, M, F);
    FICEThread.RecvStream.SaveAllowed := A;
    FICEThread.RecvStream.SaveAllowedMatch := M;
    FICEThread.RecvStream.SaveAllowedFilter := F;
  end;
end;

procedure TICEClient.ThreadTitleChanged(Sender: TSocketThread);
begin
  FTitle := FICEThread.RecvStream.Title;

  if (FEntry.ID > 0) and (FICEThread.RecvStream.FullTitleFound) and (not FAutoRemove) and (FRecordTitle = '') and (AppGlobals.SubmitStats) then
  begin
    HomeComm.SendTitleChanged(Entry.ID, Entry.Name, FTitle, FCurrentURL, Entry.StartURL, FICEThread.RecvStream.AudioType,
      Entry.Bitrate, Entry.URLs);
  end;
end;

procedure TICEClient.ThreadDisplayTitleChanged(Sender: TSocketThread);
begin
  FDisplayTitle := FICEThread.RecvStream.DisplayTitle;

  if (FDisplayTitle <> '') and Playing and (not Paused) then
  begin
    if AppGlobals.DisplayPlayNotifications then
      TfrmNotification.Act(FDisplayTitle, FEntry.Name);

    MsgBus.SendMessage(TPlayingObjectChangedMsg.Create(Self, '', FICEThread.RecvStream.DisplayTitle, FEntry.Name, ''));
  end;

  if Assigned(FOnTitleChanged) then
    FOnTitleChanged(Self, FDisplayTitle);
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

procedure TICEClient.ThreadExtLog(Sender: TSocketThread);
begin
  WriteLog(FICEThread.ExtLogMsg, FICEThread.ExtLogType, FICEThread.ExtLogLevel);
end;

procedure TICEClient.ThreadStateChanged(Sender: TSocketThread);
begin
  if FState <> csStopping then
  begin
    case FICEThread.State of
      tsRecording:
        begin
          FFilename := FICEThread.RecvStream.Filename;
          FState := csConnected;
        end;
      tsRetrying:
        begin
          FTitle := '';
          FDisplayTitle := '';
          FState := csRetrying;
        end;
      tsIOError:
        begin
          FState := csIOError;
          FScheduledRecording := False;
        end;
    end;
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);
  end;

  // Das muss, damit bei Fehlern mit Daten, die BASS nicht parsen kann, beendet wird.
  // Der ICEPlayer wirft bei PushData() eine Exception wenn das so ist.
  if (not FICEThread.Recording) and (not FICEThread.Playing) and (not FICEThread.Monitoring) then
  begin
    Disconnect;
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
  FDisplayTitle := '';
  FSpeed := 0;
  FFilename := '';
  MaxRetries := FEntry.Settings.MaxRetries;

  if DiedThread.RecvStream.RemoveClient or AutoRemove then
  begin
    if not AppGlobals.PostProcessManager.WorkingForClient(Self) then
      Kill
    else
      Disconnect;
    if Assigned(FOnDisconnected) and (FICEThread = nil) and (not AppGlobals.PostProcessManager.WorkingForClient(Self)) then
      FOnDisconnected(Self);
    Exit;
  end;

  if FStopAfterSong then
  begin
    StopRecording;
    if not DiedThread.Playing then
    begin
      if Assigned(FOnDisconnected) and (FICEThread = nil) and (not AppGlobals.PostProcessManager.WorkingForClient(Self)) then
        FOnDisconnected(Self);
      FState := csStopping;
    end;
  end;

  if (FState <> csStopping) and (FState <> csIOError) then
  begin
    if (FRetries >= MaxRetries) and (MaxRetries > 0) then
    begin
      WriteLog(Format(_('Retried %d times, stopping'), [MaxRetries]), ltGeneral, llError);
      FState := csStopped;
    end else
    begin
      Start;
      if DiedThread.Playing then
        FICEThread.StartPlay;
      if DiedThread.Paused then
        FICEThread.PausePlay;
      if DiedThread.Recording then
        FICEThread.StartRecording;
      if DiedThread.Monitoring then
        FICEThread.StartMonitoring;
    end;
    if FRedirectedURL = '' then
      Inc(FRetries);
  end else
    FState := csStopped;

  if (FState = csStopped) or (FState = csIOError) then
    FScheduledRecording := False;

  if Assigned(FOnRefresh) and (FICEThread <> nil) then
    FOnRefresh(Self);

  if Assigned(FOnDisconnected) and (FICEThread = nil) then
    FOnDisconnected(Self);
end;

procedure TICEClient.WriteLog(Text, Data: string; T: TLogType; Level: TLogLevel);
var
  LS: TLogSource;
begin
  {$IFNDEF DEBUG}
  if Level = llDebug then
    Exit;
  {$ENDIF}

  if FIsMonitoring then
    Exit;

  LS := lsStream;
  if FAutoRemove then
    LS := lsAutomatic;

  MsgBus.SendMessage(TLogMsg.Create(Self, LS, T, Level, FEntry.CustomName, Text));

  FDebugLog.Add(TLogEntry.Create(Text, Data, T, Level));
  if Assigned(FOnLog) then
    FOnLog(Self);
end;

procedure TICEClient.WriteLog(Text: string; T: TLogType; Level: TLogLevel);
begin
  WriteLog(Text, '', T, Level);
end;

function TICEClient.ParsePlaylist: Boolean;
 var
  Data: string;
  PH: TPlaylistHandler;
begin
  Result := False;

  Data := string(FICEThread.RecvStream.RecvStream.ToString);

  PH := TPlaylistHandler.Create;
  try
    Data := string(FICEThread.RecvStream.RecvStream.ToString);
    if (Copy(LowerCase(Data), 1, 10) = '[playlist]') or
       (Pos('audio/x-scpls', FICEThread.RecvStream.ContentType) > 0) or
       (Pos('application/pls+xml', FICEThread.RecvStream.ContentType) > 0) then // .pls
    begin
      Result := PH.ParsePlaylist(Data, ptPLS);
    end else if (LowerCase(Copy(Data, 1, 7)) = '#extm3u') or
                (Pos('audio/x-mpegurl', FICEThread.RecvStream.ContentType) > 0) or
                (Pos('audio/mpegurl', FICEThread.RecvStream.ContentType) > 0) then // .m3u
    begin
      Result := PH.ParsePlaylist(Data, ptM3U);
    end else if Pos('application/octet-stream', FICEThread.RecvStream.ContentType) > 0 then
      Result := PH.ParsePlaylist(Data, ptUnknown);

    if Result then
    begin
      Entry.URLs.Assign(PH.URLs);
      FURLsIndex := 0;
    end;
  finally
    PH.Free;
  end;
end;

procedure TICEClient.SetEQ(Value, Freq: Integer);
begin
  if FICEThread <> nil then
    FICEThread.SetEQ(Value, Freq);
end;

procedure TICEClient.SetVolume(Vol: Integer);
begin
  if FICEThread <> nil then
    FICEThread.SetVolume(Vol);
end;

{ TDebugEntry }

constructor TLogEntry.Create(Text, Data: string; T: TLogType; Level: TLogLevel);
begin
  Self.Text := Text;
  Self.Data := Data;
  Self.T := T;
  Self.Level := Level;
  Self.Time := Now;
end;

{ TURLList }

function TURLList.Add(const S: string): Integer;
var
  i: Integer;
  Host, S2: string;
begin
  Result := -1;
  try
    S2 := s;
    if not ParseURL(S2).Success then
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

procedure TLog.Notify(const Item: TLogEntry;
  Action: TCollectionNotification);
begin
  inherited;

  if Action = cnAdded then
    while Count > 1000 do
      Delete(0);
end;

end.

