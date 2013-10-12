{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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

{ This unit.... is the master of streamWriter.
  It handles everything regarding recording, saving, etc... }
unit ICEStream;

interface

uses
  SysUtils, Windows, StrUtils, Classes, HTTPStream, ExtendedStream, AudioStream,
  AppData, LanguageObjects, Functions, DynBASS, WaveData, Generics.Collections,
  Math, PerlRegEx, Logging, WideStrUtils, AudioFunctions, PostProcessMP4Box,
  AddonMP4Box, SWFunctions, MonitorAnalyzer, Generics.Defaults;

type
  TDebugEvent = procedure(Text, Data: string) of object;
  TChunkReceivedEvent = procedure(Buf: Pointer; Len: Integer) of object;

  TStreamTrack = class
  public
    S, E: Int64;
    Title: string;
    FullTitle: Boolean;
    constructor Create(S, E: Int64; Title: string; FullTitle: Boolean);
  end;

  TStreamTracks = class(TList<TStreamTrack>)
  private
    FRecordTitle: string;
    FOnDebug: TDebugEvent;
  public
    destructor Destroy; override;
    procedure Clear; reintroduce;

    procedure FoundTitle(Offset: Int64; Title: string; BytesPerSec: Cardinal; FullTitle: Boolean);
    function Find(const Title: string): TStreamTrack;

    property RecordTitle: string read FRecordTitle write FRecordTitle;
  end;

  TCheckResults = (crSave, crDiscard, crDiscardExistingIsLarger, crOverwrite);

  TTitleStates = (tsFull, tsIncomplete, tsAuto, tsStream);

  TFileChecker = class
  private
    FSettings: TStreamSettings;
    FResult: TCheckResults;
    FStreamname: string;
    FSaveDir: string;
    FFilename: string;
    FFilenameConverted: string;
    FSongsSaved: Cardinal;

    function LimitToMaxPath(Filename: string): string;
    function GetValidFilename(Name: string): string;
    function GetAppendNumber(Dir, Filename: string): Integer;
    function InfoToFilename(Artist, Title, Album, StreamTitle: string; TitleState: TTitleStates; Patterns: string): string;
  public
    constructor Create(Streamname, Dir: string; SongsSaved: Cardinal; Settings: TStreamSettings);

    procedure GetStreamFilename(Name: string; AudioType: TAudioTypes);
    procedure GetFilename(Filesize: UInt64; Artist, Title, Album, StreamTitle: string; AudioType: TAudioTypes;
      TitleState: TTitleStates; Killed: Boolean);

    property Result: TCheckResults read FResult;
    property SaveDir: string read FSaveDir;
    property Filename: string read FFilename;
    property FilenameConverted: string read FFilenameConverted;
  end;

  TICEStream = class(THTTPStream)
  private
    FSettings: TStreamSettings;
    FRecordTitle: string;
    FStopAfterSong: Boolean;
    FKilled: Boolean;

    FMetaInt: Integer;
    FNextMetaInt: Integer;
    FSongsSaved: Cardinal;
    FStreamName: string;
    FStreamURL: string;
    FBitRate: Cardinal;
    FGenre: string;

    FSaveDir: string;
    FSaveDirAuto: string;

    FMetaCounter: Integer;
    FRecordingSessionMetaCounter: Integer;

    FTitle: string;
    FDisplayTitle: string;
    FSavedFilename: string;
    FSavedFilenameConverted: string;
    FSavedArtist: string;
    FSavedTitle: string;
    FSavedAlbum: string;
    FSavedSize: UInt64;
    FSavedLength: UInt64;
    FSavedStreamTitle: string;
    FSavedIsStreamFile: Boolean;
    FFilename: string;
    FSavedWasCut: Boolean;
    FSavedFullTitle: Boolean;
    FBytesPerSec: Integer;
    FBytesPerMSec: Extended;
    FOriginalStreamTitle: string;

    FSaveAllowedTitle: string;
    FSaveAllowed: Boolean;
    FSaveAllowedMatch: string;
    FSaveAllowedFilter: Integer;
    FRecording: Boolean;
    FRecordingStarted: Boolean;
    FFullTitleFound: Boolean;
    FRecordingTitleFound: Boolean;
    FMonitoring: Boolean;
    FMonitoringStarted: Boolean;
    FRemoveClient: Boolean;
    FClientStopRecording: Boolean;
    FLastGetSettings: Cardinal;

    FStreamTracks: TStreamTracks;
    //FMonitorAnalyzer: TMonitorAnalyzer;

    FAudioStream: TStream;
    FAudioType: TAudioTypes;

    FOnTitleChanged: TNotifyEvent;
    FOnSongSaved: TNotifyEvent;
    FOnNeedSettings: TNotifyEvent;
    FOnChunkReceived: TChunkReceivedEvent;
    FOnIOError: TNotifyEvent;
    FOnTitleAllowed: TNotifyEvent;
    FOnRefreshInfo: TNotifyEvent;
    //FOnMonitorAnalyzerAnalyzed: TNotifyEvent;

    function AdjustDisplayTitle(Title: string): string;
    function GetFileLength(Filename: string; Filesize: Int64; var Length: UInt64): Boolean;
    function CalcAdjustment(Offset: Int64): Int64;
    procedure CalcBytesPerSec;
    procedure DataReceived(CopySize: Integer);
    function GetBestRegEx(Title: string): string;
    procedure SaveData(S, E: UInt64; Title: string; FullTitle: Boolean);
    procedure TrySave;
    procedure ProcessData(Received: Cardinal);
    procedure GetSettings;
    procedure StreamTracksDebug(Text, Data: string);
    procedure FreeAudioStream;
    function StartRecordingInternal: Boolean;
    procedure StopRecordingInternal;

    //procedure MonitorAnalyzerAnalyzed(Sender: TObject);

    function CleanTitle(Title: string): string;
    procedure ParseTitle(S, Pattern: string; var Artist: string; var Title: string; var Album: string);

    procedure FSetRecordTitle(Value: string);
  protected
    procedure DoHeaderRemoved; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(Received: Cardinal); override;

    procedure StartRecording;
    procedure StopRecording;
    procedure StartMonitoring;
    procedure Disconnected; override;

    property Settings: TStreamSettings read FSettings;
    property RecordTitle: string read FRecordTitle write FSetRecordTitle;
    property StopAfterSong: Boolean read FStopAfterSong write FStopAfterSong;
    property Killed: Boolean read FKilled write FKilled;

    property MetaCounter: Integer read FMetaCounter;
    //property MonitorAnalyzer: TMonitorAnalyzer read FMonitorAnalyzer;

    property StreamName: string read FStreamName;
    property StreamURL: string read FStreamURL;
    property BitRate: Cardinal read FBitRate;
    property Genre: string read FGenre;
    property Title: string read FTitle;
    property DisplayTitle: string read FDisplayTitle;
    property SavedFilename: string read FSavedFilename;
    property SavedFilenameConverted: string read FSavedFilenameConverted;
    property SavedArtist: string read FSavedArtist;
    property SavedTitle: string read FSavedTitle;
    property SavedAlbum: string read FSavedAlbum;
    property SavedSize: UInt64 read FSavedSize;
    property SavedLength: UInt64 read FSavedLength;
    property SavedStreamTitle: string read FSavedStreamTitle;
    property SavedIsStreamFile: Boolean read FSavedIsStreamFile;
    property SongsSaved: Cardinal read FSongsSaved write FSongsSaved;
    property Filename: string read FFilename;
    property SavedWasCut: Boolean read FSavedWasCut;
    property SavedFullTitle: Boolean read FSavedFullTitle;
    property OriginalStreamTitle: string read FOriginalStreamTitle;

    property FullTitleFound: Boolean read FFullTitleFound write FFullTitleFound;
    property RecordingTitleFound: Boolean read FRecordingTitleFound write FRecordingTitleFound;
    property RemoveClient: Boolean read FRemoveClient;
    property ClientStopRecording: Boolean read FClientStopRecording write FClientStopRecording;

    property SaveAllowedTitle: string read FSaveAllowedTitle;
    property SaveAllowed: Boolean read FSaveAllowed write FSaveAllowed;
    property SaveAllowedMatch: string read FSaveAllowedMatch write FSaveAllowedMatch;
    property SaveAllowedFilter: Integer read FSaveAllowedFilter write FSaveAllowedFilter;

    property AudioType: TAudioTypes read FAudioType;

    property OnTitleChanged: TNotifyEvent read FOnTitleChanged write FOnTitleChanged;
    property OnSongSaved: TNotifyEvent read FOnSongSaved write FOnSongSaved;
    property OnNeedSettings: TNotifyEvent read FOnNeedSettings write FOnNeedSettings;
    property OnChunkReceived: TChunkReceivedEvent read FOnChunkReceived write FOnChunkReceived;
    property OnIOError: TNotifyEvent read FOnIOError write FOnIOError;
    property OnTitleAllowed: TNotifyEvent read FOnTitleAllowed write FOnTitleAllowed;
    property OnRefreshInfo: TNotifyEvent read FOnRefreshInfo write FOnRefreshInfo;
    //property OnMonitorAnalyzerAnalyzed: TNotifyEvent read FOnMonitorAnalyzerAnalyzed write FOnMonitorAnalyzerAnalyzed;
  end;

implementation

{ TICEStream }

function TICEStream.AdjustDisplayTitle(Title: string): string;
var
  i: Integer;
  C: Char;
  NextUpper: Boolean;
begin
  // Das hier ist genau so im Server. Ändert man an einem Programm was
  // muss es im anderen Programm nachgezogen werden!
  // ´ durch ' ersetzen
  Title := Functions.RegExReplace('´', '''', Title);
  // _ durch ' ' ersetzen
  Title := Functions.RegExReplace('_', ' ', Title);
  // Featuring-dinge fitmachen
  Title := Functions.RegExReplace(' ft ', ' Feat. ', Title);
  Title := Functions.RegExReplace(' ft\. ', ' Feat. ', Title);
  Title := Functions.RegExReplace(' feat ', ' Feat. ', Title);
  // Mehrere ' zu einem machen
  Title := Functions.RegExReplace('''+(?='')', '', Title);
  // Mehrere leertasten hintereinander zu einer machen
  Title := Functions.RegExReplace(' +(?= )', '', Title);
  // Leertasten nach Klammer auf bzw. vor Klammer zu entfernen
  Title := Functions.RegExReplace('\( ', '(', Title);
  Title := Functions.RegExReplace(' \)', ')', Title);

  Result := '';
  NextUpper := True;
  for i := 1 to Length(Title) do
  begin
    if NextUpper then
    begin
      C := UpperCase(Title[i])[1];
      NextUpper := False;
    end else
      C := LowerCase(Title[i])[1];

    if (c = ' ') or (c = '-') or (c = '_') or (c = ':') or (c = '(') or (c = '\') or (c = '/') then
      NextUpper := True;
    Result := Result + C;
  end;
end;

function TICEStream.CalcAdjustment(Offset: Int64): Int64;
begin
  Result := Offset;
  if (FBytesPerMSec > 0) and (FSettings.AdjustTrackOffset) and (FSettings.AdjustTrackOffsetMS > 0) then
  begin
    if FSettings.AdjustTrackOffsetDirection = toForward then
      Result := Result + Trunc(FSettings.AdjustTrackOffsetMS * FBytesPerMSec)
    else
      Result := Result - Trunc(FSettings.AdjustTrackOffsetMS * FBytesPerMSec);

    if Result < 0 then
      Result := 0;
  end;
end;

procedure TICEStream.CalcBytesPerSec;
var
  TempPlayer: Cardinal;
  Time: Double;
  BufLen: Int64;
  Size: Int64;
begin
  if FAudioStream.InheritsFrom(TAudioStreamFile) then
  begin
    Size := TAudioStreamFile(FAudioStream).Size;
    if Size = 0 then
      raise Exception.Create('');
    TempPlayer := BASSStreamCreateFile(False, PChar(TAudioStreamFile(FAudioStream).FileName), 0, 0, BASS_STREAM_DECODE or BASS_STREAM_PRESCAN or BASS_UNICODE);
  end else
  begin
    Size := TAudioStreamMemory(FAudioStream).Size;
    if Size = 0 then
      raise Exception.Create('');
    TempPlayer := BASSStreamCreateFile(True, TAudioStreamMemory(FAudioStream).Memory, 0, Size, BASS_STREAM_DECODE or BASS_STREAM_PRESCAN);
  end;

  if TempPlayer = 0 then
    raise Exception.Create('');
  try
    BASSChannelSetPosition(TempPlayer, Size, BASS_POS_BYTE);
    Time := BASSChannelBytes2Seconds(TempPlayer, BASSChannelGetLength(TempPlayer, BASS_POS_BYTE));
    BufLen := BASSStreamGetFilePosition(TempPlayer, BASS_FILEPOS_END);
    if BufLen = -1 then
      raise Exception.Create('');
    FBytesPerSec := Trunc((BufLen / (125 * Time) + 0.5) * 125);
    FBytesPerMSec := FBytesPerSec / 1000;
    FBitRate := Trunc(BufLen / Floor(((125 * Time)) + 0.5));

    FBitrate := RoundBitrate(FBitrate);

    if FBytesPerSec <= 10 then
      raise Exception.Create('');
  finally
    BASSStreamFree(TempPlayer);
  end;
end;

function TICEStream.CleanTitle(Title: string): string;
var
  i: Integer;
begin
  Result := '';
  if Length(Title) > 0 then
    for i := 1 to Length(Title) do
    begin
      if (Ord(Title[i]) >= 32) then
        Result := Result + Title[i];
    end;
end;

constructor TICEStream.Create;
begin
  inherited Create;

  FSettings := TStreamSettings.Create;

  FMetaInt := -1;
  FMetaCounter := 0;
  FRecordingSessionMetaCounter := 0;
  FSongsSaved := 0;
  FFilename := '';
  FBitRate := 0;
  FGenre := '';
  FTitle := '';
  FSavedFilename := '';
  FSavedArtist := '';
  FSavedTitle := '';
  FSavedAlbum := '';
  FOriginalStreamTitle := '';
  FRecording := False;
  FRecordingStarted := False;
  FAudioType := atNone;
  FStreamTracks := TStreamTracks.Create;
  FStreamTracks.FOnDebug := StreamTracksDebug;
end;

destructor TICEStream.Destroy;
begin
  FreeAudioStream;

  FreeAndNil(FStreamTracks);
  FSettings.Free;

  //FreeAndNil(FMonitorAnalyzer);

  inherited;
end;

procedure TICEStream.Disconnected;
var
  Track: TStreamTrack;
begin
  // Falls erlaubt, versuchen, das Empfangene wegzuspeichern..
  if (not FMonitoring) and (FAudioStream <> nil) and (FStreamTracks.Count > 0) and
     (not FSettings.OnlySaveFull) and (FRecordTitle = '') then
  begin
    Track := FStreamTracks[0];
    Track.E := FAudioStream.Size;
    if Track.S - FSettings.SongBuffer * FBytesPerMSec >= 0 then
      Track.S := Track.S - Trunc(FSettings.SongBuffer * FBytesPerMSec);
    SaveData(Track.S, Track.E, Track.Title, False);
    FStreamTracks.Clear;
  end;

  // Und noch die dicke Stream-Datei melden
  if (not FMonitoring) and (FAudioStream <> nil) and FAudioStream.InheritsFrom(TAudioStreamFile) and
      not (FSettings.SeparateTracks and FSettings.DeleteStreams) and (FRecordTitle = '') and
     (FAudioStream.Size > 0) then
  begin
    FSavedFilename := TAudioStreamFile(FAudioStream).FileName;
    FSavedSize := TAudioStreamFile(FAudioStream).Size;
    FSavedFullTitle := False;
    FSavedStreamTitle := TAudioStreamFile(FAudioStream).FileName;
    FSavedIsStreamFile := True;
    if FBytesPerSec > 0 then
      FSavedLength := Trunc(FSavedSize / FBytesPerSec)
    else
      FSavedLength := 0;
    if Assigned(FOnSongSaved) then
      FOnSongSaved(Self);
  end;
end;

procedure TICEStream.DataReceived(CopySize: Integer);
var
  Buf: Pointer;
begin
  //if FMonitoring and (FMonitorAnalyzer <> nil) and (FMetaInt > 0) and (FMonitorAnalyzer.Active) then
  //begin
  //  try
  //    FMonitorAnalyzer.Append(RecvStream, CopySize);
  //    RecvStream.Position := RecvStream.Position - CopySize;
  //  except
  //    FreeAndNil(FMonitorAnalyzer);
  //  end;
  //end;

  if (FAudioStream <> nil) and (not FMonitoring) then
  begin
    FAudioStream.Seek(0, soFromEnd);
    FAudioStream.CopyFrom(RecvStream, CopySize);
  end else
    Seek(CopySize, soFromCurrent);

  if Assigned(FOnChunkReceived) and (not FMonitoring) then
  begin
    GetMem(Buf, CopySize);
    CopyMemory(Buf, Pointer(Integer(RecvStream.Memory) + (RecvStream.Position - CopySize)), CopySize);
    FOnChunkReceived(Buf, CopySize);
    FreeMem(Buf);
  end;
end;

procedure TICEStream.DoHeaderRemoved;
var
  Dir: string;
begin
  inherited;
  GetSettings;

  // Wenn es ein Redirect ist dann die Standard-HTTP-Ausgabe nehmen.
  // Ein paar Streams haben als Header ICY mit Code 302.. das würde sonst im Block
  // hier drunter zur Exception führen.
  if RedirURL <> '' then
  begin
    WriteDebug(_('HTTP response detected'), 1, 1);
    Exit;
  end;

  if (HeaderType = 'icy') or (ContentType = 'audio/mpeg') or (ContentType = 'audio/aacp') or
     (Pos(#10'icy-metaint:', LowerCase(FHeader)) > 0) or (Pos(#10'icy-name:', LowerCase(FHeader)) > 0) then
  begin
    WriteDebug(_('Audio-data response detected'), 1, 1);
    FHeaderType := 'icy';

    if ResponseCode = 200 then
    begin
      try
        FMetaInt := StrToInt(GetHeaderData('icy-metaint'));
        FNextMetaInt := FMetaInt;
      except
        WriteDebug(_('Meta-interval could not be found'), 1, 1);
      end;

      FStreamName := GetHeaderData('icy-name');
      FStreamURL := GetHeaderData('icy-url');
      FBitRate := StrToIntDef(GetHeaderData('icy-br'), 0);
      FGenre := GetHeaderData('icy-genre');

      Dir := FSaveDir;

      if (LowerCase(ContentType) = 'audio/mpeg') or
         ((ContentType = '') and ((FStreamName <> '') or (FStreamURL <> '')))
      then
        FAudioType := atMPEG
      else if (LowerCase(ContentType) = 'audio/aacp') or (LowerCase(ContentType) = 'audio/aac') then
        FAudioType := atAAC
      //else if LowerCase(ContentType) = 'application/ogg' then
      //  FAudioType := atOGG
      else
        raise Exception.Create(_('Unknown content-type'));

      if FRecording then
        StartRecording;

      if FMonitoring then
        StartMonitoring;

      if Assigned(FOnTitleChanged) then
        FOnTitleChanged(Self);
    end else
      raise Exception.Create(Format(_('Invalid responsecode (%d)'), [ResponseCode]));
  end else if HeaderType = 'http' then
  begin
    WriteDebug(_('HTTP response detected'), 1, 1);
  end else
    raise Exception.Create(_('Unknown header-type'));
end;

procedure TICEStream.FreeAudioStream;
var
  Filename: string;
  LowerDir: string;
begin
  Filename := '';
  FFilename := '';
  if FAudioStream <> nil then
  begin
    if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
    begin
      Filename := TAudioStreamFile(FAudioStream).FileName;
    end;
    FreeAndNil(FAudioStream);
  end;

  if (FSettings.SeparateTracks) and (FSettings.DeleteStreams and (Filename <> '')) then
  begin
    DeleteFile(PChar(Filename));

    LowerDir := LowerCase(IncludeTrailingBackslash(ExtractFilePath(Filename)));
    if (LowerDir <> LowerCase(IncludeTrailingBackslash(AppGlobals.Dir))) and (LowerDir <> LowerCase(IncludeTrailingBackslash(AppGlobals.DirAuto))) then
      Windows.RemoveDirectory(PChar(ExtractFilePath(Filename)));
  end;
end;

procedure TICEStream.FSetRecordTitle(Value: string);
begin
  if Value <> FRecordTitle then
    WriteDebug(Format(_('Recording "%s"'), [Value]), 1, 0);
  FRecordTitle := Value;
  FStreamTracks.RecordTitle := Value;
end;

function TICEStream.GetFileLength(Filename: string; Filesize: Int64; var Length: UInt64): Boolean;
var
  TempPlayer: Cardinal;
  Time: Double;
  BufLen: Int64;
  Bitrate: Cardinal;
  Len: QWORD;
  OldBitrate: Cardinal;
begin
  TempPlayer := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE or BASS_UNICODE or BASS_STREAM_PRESCAN);

  if TempPlayer = 0 then
    Exit(False);

  try
    Len := BASSChannelGetLength(TempPlayer, BASS_POS_BYTE);
    BASSChannelSetPosition(TempPlayer, Len, BASS_POS_BYTE);
    Time := BASSChannelBytes2Seconds(TempPlayer, Len);
    BufLen := BASSStreamGetFilePosition(TempPlayer, BASS_FILEPOS_END);
    if BufLen = -1 then
      Exit(False);
    Bitrate := Trunc(BufLen / Floor(((125 * Time)) + 0.5));

    Length := Trunc(Time);

    Bitrate := RoundBitrate(Bitrate);

    // Wir setzen auch diese beiden Stream-Eigenschaften, weil die Datei ja aussagekräftiger
    // ist, als die paar Bytes aus CalcBytesPerSec()
    FBytesPerSec := Trunc((BufLen / (125 * Time) + 0.5) * 125);
    FBytesPerMSec := FBytesPerSec / 1000;

    OldBitrate := FBitRate;
    FBitrate := Bitrate;

    if FBitrate <> OldBitrate then
      FOnRefreshInfo(Self);
  finally
    BASSStreamFree(TempPlayer);
  end;

  Result := True;
end;

procedure TICEStream.GetSettings;
begin
  if Assigned(FOnNeedSettings) then
    FOnNeedSettings(Self);

  AppGlobals.Lock;
  FSaveDir := AppGlobals.Dir;
  FSaveDirAuto := AppGlobals.DirAuto;
  AppGlobals.Unlock;
end;

//procedure TICEStream.MonitorAnalyzerAnalyzed(Sender: TObject);
//begin
//  if Assigned(FOnMonitorAnalyzerAnalyzed) then
//    FOnMonitorAnalyzerAnalyzed(Self);
//end;

function TICEStream.GetBestRegEx(Title: string): string;
type
  TRegExData = record
    RegEx: string;
    BadWeight: Integer;
  end;
var
  i, n: Integer;
  R: TPerlRegEx;
  MArtist, MTitle, MAlbum: string;
  RED: TRegExData;
  REDs: TList<TRegExData>;
const
  BadChars: array[0..2] of string = (':', '-', '|');
begin
  Result := '(?P<a>.*) - (?P<t>.*)';

  REDs := TList<TRegExData>.Create;
  try
    for i := 0 to FSettings.RegExes.Count - 1 do
    begin
      RED.RegEx := FSettings.RegExes[i];
      RED.BadWeight := 0;

      MArtist := '';
      MTitle := '';
      MAlbum := '';

      R := TPerlRegEx.Create;
      try
        R.Options := R.Options + [preCaseLess];
        R.Subject := Title;
        R.RegEx := RED.RegEx;
        try
          if R.Match then
          begin
            try
              if R.NamedGroup('a') > 0 then
              begin
                MArtist := Trim(R.Groups[R.NamedGroup('a')]);
                for n := 0 to High(BadChars) do
                  if Pos(BadChars[n], MArtist) > 0 then
                    RED.BadWeight := RED.BadWeight + 1;
                if ContainsRegEx('(\d{2})', MArtist) then
                  RED.BadWeight := RED.BadWeight + 1;
              end
                else RED.BadWeight := RED.BadWeight + 2;
            except end;
            try
              if R.NamedGroup('t') > 0 then
              begin
                MTitle := Trim(R.Groups[R.NamedGroup('t')]);
                for n := 0 to High(BadChars) do
                  if Pos(BadChars[n], MTitle) > 0 then
                    RED.BadWeight := RED.BadWeight + 1;
                if ContainsRegEx('(\d{2})', MTitle) then
                  RED.BadWeight := RED.BadWeight + 1;
              end
                else RED.BadWeight := RED.BadWeight + 2;
            except end;
            try
              if R.NamedGroup('l') > 0 then
              begin
                RED.BadWeight := RED.BadWeight - 3;
                MAlbum := Trim(R.Groups[R.NamedGroup('l')]);
                for n := 0 to High(BadChars) do
                  if Pos(BadChars[n], MAlbum) > 0 then
                    RED.BadWeight := RED.BadWeight + 1;
              end;
            except end;

            if MAlbum = '' then
              RED.BadWeight := RED.BadWeight + 10;

            REDs.Add(RED);
          end else
            RED.BadWeight := RED.BadWeight + 50;
        except end;
      finally
        R.Free;
      end;
    end;

    REDs.Sort(TComparer<TRegExData>.Construct(
      function (const L, R: TRegExData): integer
      begin
        Result := CmpInt(L.BadWeight, R.BadWeight);
      end
    ));

    if REDs.Count > 0 then
      Result := REDs[0].RegEx;
  finally
    REDs.Free;
  end;
end;

procedure TICEStream.SaveData(S, E: UInt64; Title: string; FullTitle: Boolean);
  procedure RemoveData;
  var
    i: Integer;
    BufLen, RemoveLen: Int64;
  begin
    // Für das nächste Lied einen Puffer daüberlassen, Rest abschneiden. Puffer ist der größere
    // der beiden Werte, weil wir nicht wissen, ob für das nächste Lied Stille gefunden wird,
    // oder der normale Puffer genutzt wird.
    if FAudioStream.ClassType.InheritsFrom(TAudioStreamMemory) then
    begin
      BufLen := Max(FBytesPerSec * FSettings.SilenceBufferSecondsStart, Trunc(FBytesPerMSec * FSettings.SongBuffer));

      if FSettings.AdjustTrackOffset and (FSettings.AdjustTrackOffsetDirection = toBackward) then
        BufLen := BufLen + Trunc(FSettings.AdjustTrackOffsetMS * FBytesPerMSec);

      if FStreamTracks.Count > 0 then
      begin
        RemoveLen := FStreamTracks[FStreamTracks.Count - 1].S - BufLen;

        if RemoveLen <= 0 then
          Exit;

        // Weil wir gleich abschneiden, müssen wir eventuell vorgemerkte Tracks anfassen
        for i := 1 to FStreamTracks.Count - 1 do
        begin
          if FStreamTracks[i].S > -1 then
            FStreamTracks[i].S := FStreamTracks[i].S - RemoveLen;
          if FStreamTracks[i].E > -1 then
            FStreamTracks[i].E := FStreamTracks[i].E - RemoveLen;
        end;

        TAudioStreamMemory(FAudioStream).RemoveRange(0, RemoveLen);
      end;
    end;
  end;
var
  Dir, Filename, FilenameConverted, RegEx: string;
  FileCheck: TFileChecker;
  P: TPosRect;
  Error: Cardinal;
  TitleState: TTitleStates;
begin
  try
    if FClientStopRecording then
      Exit;

    if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
      P := TAudioStreamFile(FAudioStream).GetFrame(S, E)
    else
      P := TAudioStreamMemory(FAudioStream).GetFrame(S, E);

    if FBytesPerSec = 0 then
    begin
      RemoveData;
      Exit;
    end;

    if (P.DataStart <= -1) or (P.DataEnd <= -1) then
      raise Exception.Create(_('Error in audio data'));

    WriteDebug(Format(_('Saving from %d to %d'), [S, E]), 1, 1);

    if FSettings.SkipShort and (P.DataEnd - P.DataStart < FBytesPerSec * FSettings.ShortLengthSeconds) then
    begin
      WriteDebug(Format(_('Skipping "%s" because it''s too small (%d bytes)'), [Title, P.DataEnd - P.DataStart]), 1, 0);
      RemoveData;
      Exit;
    end;

    // GetSettings ist hier, um FKilled zu bekommen. Wenn es True ist, findet keine Nachbearbeitung
    // statt, und FileCheck.GetFileName liefert die Original-Dateierweiterung zurück.
    // Ausserdem muss es FSongsSaved aktualisieren.
    GetSettings;

    // Wird hier temporär nur für das Speichern hier erhöht. Wird später vom ICEClient
    // wieder über GetSettings() geholt.
    Inc(FSongsSaved);
    try
      FSaveAllowedTitle := Title;
      FSaveAllowed := True;

      RegEx := GetBestRegEx(Title);
      ParseTitle(Title, RegEx, FSavedArtist, FSavedTitle, FSavedAlbum);

      if (FSavedArtist <> '') and (FSavedTitle <> '') then
        FSaveAllowedTitle := FSavedArtist + ' - ' + FSavedTitle;

      if (FSavedArtist = '') or (FSavedTitle = '') then
      begin
        FSavedArtist := _('Unknown artist');
        FSavedTitle := _('Unknown title');
      end;

      if FRecordTitle <> '' then
        FileCheck := TFileChecker.Create(FStreamName, FSaveDirAuto, FSongsSaved, FSettings)
      else
        FileCheck := TFileChecker.Create(FStreamName, FSaveDir, FSongsSaved, FSettings);

      try
        if FRecordTitle <> '' then
          TitleState := tsAuto
        else
        begin
          if FullTitle then
            TitleState := tsFull
          else
            TitleState := tsIncomplete;
        end;

        FileCheck.GetFilename(E - S, FSavedArtist, FSavedTitle, FSavedAlbum, Title, FAudioType, TitleState, FKilled);
        if (FileCheck.Result in [crSave, crOverwrite]) and (FileCheck.FFilename <> '') then
        begin
          Dir := FileCheck.SaveDir;
          Filename := FileCheck.Filename;
          FilenameConverted := FileCheck.FilenameConverted;
        end else if (FileCheck.Result <> crDiscard) and (FileCheck.Result <> crDiscardExistingIsLarger) then
          raise Exception.Create(_('Could not determine filename for title'));

        if FileCheck.Result = crDiscard then
        begin
          WriteDebug(Format(_('Skipping "%s" - file already exists'), [Title]), 1, 0);
          RemoveData;
          Exit;
        end else if FileCheck.Result = crDiscardExistingIsLarger then
        begin
          WriteDebug(Format(_('Skipping "%s" - existing file is larger'), [Title]), 1, 0);
          RemoveData;
          Exit;
        end else if (FileCheck.Result <> crOverwrite) and (FRecordTitle = '') then
        begin
          if Assigned(FOnTitleAllowed) then
            FOnTitleAllowed(Self);
          if not FSaveAllowed then
          begin
            if FSaveAllowedFilter = 0 then
              WriteDebug(Format(_('Skipping "%s" - not on wishlist'), [Title]), 1, 0)
            else if FSaveAllowedFilter = 1 then
              WriteDebug(Format(_('Skipping "%s" - on global ignorelist (matches "%s")'), [Title, SaveAllowedMatch]), 1, 0)
            else
              WriteDebug(Format(_('Skipping "%s" - on stream ignorelist (matches "%s")'), [Title, SaveAllowedMatch]), 1, 0);

            RemoveData;
            Exit;
          end;
        end else if FileCheck.Result = crOverwrite then
        begin
          WriteDebug(Format(_('Saving "%s" - it overwrites a smaller same named file'), [Title]), 1, 0)
        end;
      finally
        FileCheck.Free;
      end;

      if Length(Title) > 0 then
        WriteDebug(Format('Saving title "%s"', [Title]), 1, 1)
      else
        WriteDebug('Saving unnamed title', 1, 1);

      try
        ForceDirectories(Dir);
      except
        raise Exception.Create(Format(_('Folder for saved tracks "%s" could not be created'), [Dir]));
      end;

      try
        if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
          TAudioStreamFile(FAudioStream).SaveToFile(Dir + Filename, P.DataStart, P.DataEnd - P.DataStart)
        else
        begin
          TAudioStreamMemory(FAudioStream).SaveToFile(Dir + Filename, P.DataStart, P.DataEnd - P.DataStart);
        end;
      except
        Error := GetLastError;
        if (Error = 3) and (Length(Dir + Filename) > MAX_PATH - 2) then
          raise Exception.Create(_('Could not save file because it exceeds the maximum path length'))
        else
          raise Exception.Create(_('Could not save file'));
      end;

      RemoveData;

      try
        FSavedFilename := Dir + Filename;
        FSavedFilenameConverted := Dir + FilenameConverted;
        FSavedSize := P.DataEnd - P.DataStart;
        FSavedFullTitle := FullTitle;
        FSavedStreamTitle := Title;
        FSavedIsStreamFile := False;

        // Wenn kleiner als 20MB wird FSavedLength ausgegeben, sonst selber rechnen
        if not ((FSavedSize < 20971520) and (GetFileLength(Dir + Filename, P.DataEnd - P.DataStart, FSavedLength))) then
        begin
          if FBytesPerSec > 0 then
            FSavedLength := Trunc(FSavedSize / FBytesPerSec)
          else
            FSavedLength := 0;
        end;

        FOriginalStreamTitle := Title;

        if Assigned(FOnSongSaved) then
          FOnSongSaved(Self);

        if FullTitle then
          WriteDebug(Format(_('Saved song "%s"'), [ExtractFilename(Filename)]), '', 4, 0)
        else
          WriteDebug(Format(_('Saved incomplete song "%s"'), [ExtractFilename(Filename)]), '', 4, 0);
      except
        on E: Exception do
        begin
          WriteDebug(Format(_('Error after successful save: %s'), [E.Message]), 3, 0);
          raise;
        end;
      end;
    except
      on E: Exception do
      begin
        WriteDebug(Format(_('Error while saving "%s": %s'), [ExtractFilename(Filename), E.Message]), 3, 0);
      end;
    end;
  finally
    if (Title = FRecordTitle) and (FRecordTitle <> '') then
      FRemoveClient := True;

    if StopAfterSong then
      FClientStopRecording := True;
  end;
end;

procedure TICEStream.StartMonitoring;
begin
  //if FMonitorAnalyzer = nil then
  //begin
  //  FMonitorAnalyzer := TMonitorAnalyzer.Create;
  //  FMonitorAnalyzer.OnAnalyzed := MonitorAnalyzerAnalyzed;
  //end;
  FMonitoringStarted := True;
end;

procedure TICEStream.StartRecording;
begin
  FRecordingStarted := True;
  if FMetaCounter > 0 then
    FRecordingSessionMetaCounter := 1
  else
    FRecordingSessionMetaCounter := 0;
end;

procedure TICEStream.StopRecording;
begin
  FRecordingStarted := False;
end;

function TICEStream.StartRecordingInternal: Boolean;
var
  Dir, Filename: string;
  FileCheck: TFileChecker;
begin
  Result := False;
  Filename := '';

  if (FAudioStream = nil) and (FAudioType <> atNone) then
  begin
    Dir := FSaveDir;

    if not FSettings.SaveToMemory then
    begin
      FileCheck := TFileChecker.Create(FStreamName, FSaveDir, FSongsSaved, FSettings);
      try
        FileCheck.GetStreamFilename(FStreamName, FAudioType);
        Dir := FileCheck.SaveDir;
        Filename := FileCheck.Filename;
      finally
        FileCheck.Free;
      end;
    end;

    try
      ForceDirectories(Dir);
    except
      if Assigned(FOnIOError) then
        FOnIOError(Self);
      raise Exception.Create(Format(_('Folder for saved tracks "%s" could not be created'), [Dir]));
    end;

    try
      case FAudioType of
        atMPEG:
          begin
            if FSettings.SaveToMemory then
              FAudioStream := TMPEGStreamMemory.Create
            else
            begin
              FAudioStream := TMPEGStreamFile.Create(Dir + Filename, fmCreate or fmShareDenyWrite);
              FFilename := Dir + Filename;
            end;
          end;
        atAAC:
          begin
            if FSettings.SaveToMemory then
              FAudioStream := TAACStreamMemory.Create
            else
            begin
              FAudioStream := TAACStreamFile.Create(Dir + Filename, fmCreate or fmShareDenyWrite);
              FFilename := Dir + Filename;
            end;
          end;
        atOGG:
          begin
            if FSettings.SaveToMemory then
              FAudioStream := TOGGStreamMemory.Create
            else
            begin
              FAudioStream := TOGGStreamFile.Create(Dir + Filename, fmCreate or fmShareDenyWrite);
              FFilename := Dir + Filename;
            end;
          end;
      end;

      if FAudioStream <> nil then
        if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
          WriteDebug('Saving stream to "' + Filename + '"', 1, 1);
    except
      if Assigned(FOnIOError) then
        FOnIOError(Self);

      raise Exception.Create(Format(_('Could not create "%s"'), [Filename]));
    end;


    FRecordingTitleFound := False;
    FStreamTracks.Clear;

    // Falls schon abgespielt wurde, jetzt aufgenommen wird und 'nur ganze Lieder' speichern aus ist,
    // können wir hier direkt mit der Aufnahme anfangen.
    // Achtung: Der Block hier ist so ähnlich in ProcessData() nochmal!
    if (not FSettings.OnlySaveFull) and (FAudioStream <> nil) and (FMetaCounter >= 1) and (FTitle <> '') then
    begin
      FRecordingTitleFound := True;
      FStreamTracks.FoundTitle(0, FTitle, FBytesPerSec, False);
    end;

    Result := True;
  end;
end;

procedure TICEStream.StopRecordingInternal;
begin
  // Das hier wird nur aufgerufen, wenn Play noch aktiv ist, also
  // die Verbindung nicht beendet wird. Dann müssen wir hier am
  // Disconnect aufrufen, damit er ein halbes Lied speichert falls gewünscht.
  Disconnected;

  FreeAudioStream;
end;

procedure TICEStream.StreamTracksDebug(Text, Data: string);
begin
  WriteDebug(Text, Data, 1, 1);
end;

procedure TICEStream.TrySave;
var
  R: TPosRect;
  i: Integer;
  TrackStart, TrackEnd, MaxPeaks: Int64;
  Track: TStreamTrack;
begin
  FSavedWasCut := False;

  for i := FStreamTracks.Count - 1 downto 0 do
  begin
    Track := FStreamTracks[i];

    if (Track.S > -1) and (Track.E > -1) then
    begin
      TrackStart := CalcAdjustment(Track.S);
      TrackEnd := CalcAdjustment(Track.E);

      if FSettings.SearchSilence then
      begin
        if FAudioStream.Size > TrackEnd + FBytesPerSec * FSettings.SilenceBufferSecondsEnd then
        begin
          WriteDebug(Format('Searching for silence using search range of %d/%d bytes...', [FBytesPerSec * FSettings.SilenceBufferSecondsStart,
            FBytesPerSec * FSettings.SilenceBufferSecondsEnd]), 1, 1);

          if FSettings.AutoDetectSilenceLevel then
            MaxPeaks := -1
          else
            MaxPeaks := FSettings.SilenceLevel;

          if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
            R := TAudioStreamFile(FAudioStream).SearchSilence(TrackStart, TrackEnd, FBytesPerSec * FSettings.SilenceBufferSecondsStart,
              FBytesPerSec * FSettings.SilenceBufferSecondsEnd, MaxPeaks, FSettings.SilenceLength)
          else
            R := TAudioStreamMemory(FAudioStream).SearchSilence(TrackStart, TrackEnd, FBytesPerSec * FSettings.SilenceBufferSecondsStart,
              FBytesPerSec * FSettings.SilenceBufferSecondsEnd, MaxPeaks, FSettings.SilenceLength);

          if (R.DataStart > -1) or (R.DataEnd > -1) then
          begin
            if (R.DataStart > -1) and (R.DataEnd > -1) then
              FSavedWasCut := True;

            if R.DataStart = -1 then
            begin
              WriteDebug('No silence at SongStart could be found, using configured buffer', 1, 1);
              R.DataStart := TrackStart - Trunc(FSettings.SongBuffer * FBytesPerMSec);
              if R.DataStart < FAudioStream.Size then
                R.DataStart := TrackStart;
            end else
              WriteDebug('Silence at SongStart found', 1, 1);

            if R.DataEnd = -1 then
            begin
              WriteDebug('No silence at SongEnd could be found', 1, 1);
              R.DataEnd := TrackEnd + Trunc(FSettings.SongBuffer * FBytesPerMSec);
              if R.DataEnd > FAudioStream.Size then
              begin
                WriteDebug('Stream is too small, waiting for more data...', 1, 1);
                Exit;
              end else
              begin
                WriteDebug('Using configured buffer...', 1, 1);
              end;
            end else
              WriteDebug('Silence at SongEnd found', 1, 1);

            WriteDebug(Format('Scanned song start/end: %d/%d', [R.DataStart, R.DataEnd]), 1, 1);

            if (FRecordTitle = '') or ((FRecordTitle <> '') and (FRecordTitle = Track.Title)) then
            begin
              if FRecordTitle <> '' then
                SaveData(R.DataStart, R.DataEnd, Track.Title, True)
              else
                SaveData(R.DataStart, R.DataEnd, Track.Title, Track.FullTitle);
            end else
              WriteDebug('Skipping title because it is not the title to be saved', 1, 1);

            Track.Free;
            FStreamTracks.Delete(i);
            WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]), 1, 1);
          end else
          begin
            if (FAudioStream.Size >= TrackStart + (TrackEnd - TrackStart) + ((FSettings.SongBuffer * FBytesPerMSec) * 2)) and
               (FAudioStream.Size > TrackEnd + (FSettings.SongBuffer * FBytesPerMSec) * 2) then
            begin
              WriteDebug(Format('No silence found, saving using buffer of %d bytes...', [Trunc(FSettings.SongBuffer * FBytesPerMSec)]), 1, 1);

              if (FRecordTitle = '') or ((FRecordTitle <> '') and (FRecordTitle = Track.Title)) then
              begin
                if FRecordTitle <> '' then
                  SaveData(TrackStart - Trunc(FSettings.SongBuffer * FBytesPerMSec), TrackEnd + Trunc(FSettings.SongBuffer * FBytesPerMSec), Track.Title, True)
                else
                  SaveData(TrackStart - Trunc(FSettings.SongBuffer * FBytesPerMSec), TrackEnd + Trunc(FSettings.SongBuffer * FBytesPerMSec), Track.Title, Track.FullTitle);
              end else
                WriteDebug('Skipping title because it is not the title to be saved', 1, 1);

              Track.Free;
              FStreamTracks.Delete(i);
              WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]), 1, 1);
            end else
            begin
              WriteDebug('Waiting for full buffer because no silence found...', 1, 1);
            end;
          end;
        end else
        begin
          // WriteDebug(Format('Waiting to save "%s" because stream is too small', [Track.Title]));
        end;
      end else
      begin
        if (FAudioStream.Size >= TrackStart + (TrackEnd - TrackStart) + ((FSettings.SongBuffer * FBytesPerMSec) * 2)) and
           (FAudioStream.Size > TrackEnd + FSettings.SongBuffer * FBytesPerMSec) then
        begin
          WriteDebug(Format('Saving using buffer of %d bytes...', [Trunc(FSettings.SongBuffer * FBytesPerMSec)]), 1, 1);

          if (FRecordTitle = '') or ((FRecordTitle <> '') and (FRecordTitle = Track.Title)) then
          begin
            if FRecordTitle <> '' then
              SaveData(TrackStart - Trunc(FSettings.SongBuffer * FBytesPerMSec), TrackEnd + Trunc(FSettings.SongBuffer * FBytesPerMSec), Track.Title, True)
            else
              SaveData(TrackStart - Trunc(FSettings.SongBuffer * FBytesPerMSec), TrackEnd + Trunc(FSettings.SongBuffer * FBytesPerMSec), Track.Title, Track.FullTitle);
          end else
            WriteDebug('Skipping title because it is not the title to be saved', 1, 1);

          Track.Free;
          FStreamTracks.Delete(i);
          WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]), 1, 1);
        end else
        begin
          WriteDebug('Waiting for full buffer...', 1, 1);
        end;
      end;
    end;
  end;
end;

procedure TICEStream.ProcessData(Received: Cardinal);
var
  TitleChanged, IgnoreTitle: Boolean;
  i, MetaLen, P, DataCopied: Integer;
  AutoTuneInMinKbps: Cardinal;
  Title: string;
  ParsedArtist, ParsedTitle, ParsedAlbum: string;
  MetaData: AnsiString;
  Buf: Byte;
begin
  RecvStream.Seek(0, soFromBeginning);

  // Falls Einstellungen vom User geändert wurde, die nicht zu unserem Stream-Typ passen, müssen
  // diese rückgängig gemacht werden. Beim nächsten Aufnahmestart müsstes dann passen.
  if FAudioStream <> nil then
  begin
    if (FAudioStream.InheritsFrom(TAudioStreamMemory)) then
    begin
      FSettings.DeleteStreams := False;
      FSettings.SeparateTracks := True;
    end;
    if not FSettings.SeparateTracks then
      FSettings.DeleteStreams := False;

    if (FBytesPerSec = 0) and (FAudioStream.Size > 8192) and (FAudioStream.Size < 131072) then
    begin
      try
        CalcBytesPerSec;

        AutoTuneInMinKbps := GetAutoTuneInMinKbps(Self.FAudioType, AppGlobals.AutoTuneInMinQuality);

        if (FRecordTitle <> '') and (FBitRate < AutoTuneInMinKbps) then
        begin
          WriteDebug(_('Stream will be removed because bitrate does not match.'), 2, 0);
          FRemoveClient := True;
        end;

        FOnRefreshInfo(Self);
      except
        raise Exception.Create(_('Bytes per second could not be calculated'));
      end;
    end;

    // Wenn der Stream im Speicher sitzt und größer als 200MB ist, dann wird der Stream hier geplättet.
    if (FAudioStream.InheritsFrom(TAudioStreamMemory)) and (FAudioStream.Size > 204800000) then
    begin
      WriteDebug(_('Clearing recording buffer because size exceeds 200MB'), 2, 0);
      FStreamTracks.Clear;
      TAudioStreamMemory(FAudioStream).Clear;
    end;
  end;

  if FMetaInt = -1 then
  begin
    // Wenn MonitorMode aber keine Meta-Daten, dann Ende
    if FMonitoring then
      FKilled := True;

    DataReceived(RecvStream.Size);
    RecvStream.Clear;
  end else
  begin
    TitleChanged := False;

    if (not FMonitoring) and (FSettings.SeparateTracks) then
      TrySave;

    if RecordTitle <> '' then
      if ((FBytesPerSec > 0) and (FAudioStream.Size > FBytesPerSec * 60) and (FTitle <> FRecordTitle) and (FStreamTracks.Find(FRecordTitle) = nil)) or
         (FMetaCounter > 5) then
      begin
        WriteDebug(_('Stream will be removed because wished title was not found.'), 2, 0);
        FRemoveClient := True;
      end;

    while RecvStream.Size > 0 do
    begin
      if (FNextMetaInt > FMetaInt) or (FNextMetaInt < 0) then
        raise Exception.Create('Sync failed');

      if FNextMetaInt > 0 then
      begin
        DataCopied := Min(FNextMetaInt, RecvStream.Size - RecvStream.Position);
        if DataCopied = 0 then
          Break;
        DataReceived(DataCopied);

        FNextMetaInt := FNextMetaInt - DataCopied;
      end;

      if FNextMetaInt = 0 then
      begin
        if RecvStream.Position < RecvStream.Size - 4081 then // 4081 wegen 255*16+1 (Max-MetaLen)
        begin
          FNextMetaInt := FMetaInt;

          RecvStream.Read(Buf, 1);
          if Buf > 0 then
          begin
            MetaLen := Buf * 16;
            MetaData := AnsiString(Trim(RecvStream.ToString(RecvStream.Position, MetaLen)));
            RecvStream.Seek(MetaLen, soFromCurrent);
            P := PosEx(''';', MetaData, 14);
            MetaData := AnsiString(Trim(Copy(MetaData, 14, P - 14)));
            if IsUTF8String(MetaData) then
              Title := CleanTitle(UTF8ToString(MetaData))
            else
              Title := CleanTitle(MetaData);

            IgnoreTitle := Title = '';

            //if FMonitoring and (FMonitorAnalyzer <> nil) then
            //begin
            //  try
            //    FMonitorAnalyzer.TitleChanged;
            //    if not FMonitorAnalyzer.Active then
            //      FreeAndNil(FMonitorAnalyzer);
            //  except
            //    FreeAndNil(FMonitorAnalyzer);
            //  end;
            //end;

            for i := 0 to FSettings.IgnoreTrackChangePattern.Count - 1 do
              if Like(Title, FSettings.IgnoreTrackChangePattern[i]) then
              begin
                IgnoreTitle := True;
                Break;
              end;

            if not IgnoreTitle then
            begin
              if Title <> FTitle then
              begin
                WriteDebug(Format(_('"%s" now playing'), [Title]), 2, 0);
                TitleChanged := True;
                Inc(FMetaCounter);
                Inc(FRecordingSessionMetaCounter);

                // Ist nur dafür da, um dem Server zu sagen "hier läuft jetzt ein volles Lied"
                if (FMetaCounter >= 2) then
                  FFullTitleFound := True;

                // Wenn eh nur ganze gespeichert werden sollen, dann jetzt schon raus,
                // sonst wird nie ins SaveData gegangen, wo das auch gemacht wird.
                if FStopAfterSong and (FRecordingSessionMetaCounter = 2) and FSettings.OnlySaveFull then
                begin
                  FClientStopRecording := True;
                end;
              end;

              if (not FMonitoring) and FSettings.SeparateTracks then
                if (AnsiLowerCase(Title) <> AnsiLowerCase(FTitle)) and (FRecordingTitleFound) then
                begin
                  if FAudioStream <> nil then
                    FStreamTracks.FoundTitle(FAudioStream.Size, Title, FBytesPerSec, True);
                end else if Title = FTitle then
                begin

                end else
                begin
                  // Achtung: Der Block hier ist so ähnlich in StartRecordingInternal() nochmal!
                  if not FRecordingTitleFound then
                    if (FMetaCounter >= 2) or ((FMetaCounter = 1) and (not FSettings.OnlySaveFull)) then
                    begin
                      if FAudioStream <> nil then
                      begin
                        FRecordingTitleFound := True;

                        if FMetaCounter >= 2 then
                          FStreamTracks.FoundTitle(FAudioStream.Size, Title, FBytesPerSec, True)
                        else
                          FStreamTracks.FoundTitle(FAudioStream.Size, Title, FBytesPerSec, False);
                      end;
                    end;
                end;

              FTitle := Title;
              FDisplayTitle := FTitle;

              ParseTitle(FTitle, GetBestRegEx(FTitle), ParsedArtist, ParsedTitle, ParsedAlbum);
              if (ParsedArtist <> '') and (ParsedTitle <> '') and (ParsedAlbum <> '') then
                FDisplayTitle := ParsedArtist + ' - ' + ParsedTitle + ' - ' + ParsedAlbum
              else if (ParsedArtist <> '') and (ParsedTitle <> '') then
                FDisplayTitle := ParsedArtist + ' - ' + ParsedTitle;
              FDisplayTitle := AdjustDisplayTitle(FDisplayTitle);

              if TitleChanged then
                if Assigned(FOnTitleChanged) then
                  FOnTitleChanged(Self);
            end;
          end;
        end else
          Break;
      end;
    end;
    RecvStream.RemoveRange(0, Position);
    RecvStream.Seek(0, soFromBeginning);
  end;
end;

procedure TICEStream.ParseTitle(S, Pattern: string; var Artist: string; var Title: string; var Album: string);
  function MyUpperCase(C: Char): Char;
  begin
    if CharInSet(C, ['ä', 'Ä', 'ö', 'Ö', 'ü', 'Ü']) then
      Result := AnsiUpperCase(C)[1]
    else
      Result := UpperCase(C)[1];
  end;
  function MyLowerCase(C: Char): Char;
  begin
    if CharInSet(C, ['ä', 'Ä', 'ö', 'Ö', 'ü', 'Ü']) then
      Result := AnsiLowerCase(C)[1]
    else
      Result := LowerCase(C)[1];
  end;
  function NormalizeText(Text: string): string;
  var
    i: Integer;
    LastChar: Char;
  begin
    Result := '';
    LastChar := #0;
    Text := StringReplace(Text, '_', ' ', [rfReplaceAll]);

    for i := 1 to Length(Text) do
    begin
      if (LastChar = #0) or (LastChar = ' ') or (LastChar = '.') or (LastChar = '-') or (LastChar = '/') or (LastChar = '(') then
        Result := Result + MyUpperCase(Text[i])
      else
        Result := Result + MyLowerCase(Text[i]);
      LastChar := Text[i];
    end;
  end;
var
  R: TPerlRegEx;
begin
  Artist := '';
  Title := '';
  Album := '';

  if (S <> '') and (Pattern <> '') then
  begin
    R := TPerlRegEx.Create;
    R.Options := R.Options + [preCaseLess];
    try
      R.Subject := S;
      R.RegEx := Pattern;
      try
        if R.Match then
        begin
          try
            if R.NamedGroup('a') > 0 then
              Artist := Trim(R.Groups[R.NamedGroup('a')]);
          except end;
          try
            if R.NamedGroup('t') > 0 then
              Title := Trim(R.Groups[R.NamedGroup('t')]);
          except end;
          try
            if R.NamedGroup('l') > 0 then
              Album := Trim(R.Groups[R.NamedGroup('l')]);
          except end;
        end;
      except end;
    finally
      R.Free;
    end;
  end;

  if (Artist = '') and (Title = '') and (Pattern <> '(?P<a>.*) - (?P<t>.*)') then
  begin
    // Wenn nichts gefunden wurde, Fallback mit normalem Muster..
    ParseTitle(S, '(?P<a>.*) - (?P<t>.*)', Artist, Title, Album);
  end;

  if FSettings.NormalizeVariables then
  begin
    Artist := NormalizeText(Artist);
    Title := NormalizeText(Title);
    Album := NormalizeText(Album);
  end;
end;

procedure TICEStream.Process(Received: Cardinal);
begin
  inherited;
  if not HeaderRemoved then
    Exit;
  if HeaderType = 'icy'  then
  begin
    if (HeaderRemoved) and (Size > 8192) then
    begin
      if Cardinal(FLastGetSettings + 2000) < GetTickCount then
      begin
        GetSettings;
        FLastGetSettings := GetTickCount;
      end;

      if FMonitoringStarted then
      begin
        FMonitoring := True;
      end;

      if FRecordingStarted and (not FRecording) then
      begin
        if StartRecordingInternal then
          FRecording := True;
      end;
      if (not FRecordingStarted) and FRecording then
      begin
        StopRecordingInternal;
        FRecording := False;
      end;

      ProcessData(Received);
    end;
  end else if HeaderType = 'http' then
  begin
    if Size > 512000 then
      raise Exception.Create(_('Too many bytes in HTTP-response'))
  end else if HeaderRemoved then
    raise Exception.Create(_('Unknown header-type'));
end;

{ TStreamTrack }

constructor TStreamTrack.Create(S, E: Int64; Title: string; FullTitle: Boolean);
begin
  Self.S := S;
  Self.E := E;
  Self.Title := Title;
  Self.FullTitle := FullTitle;
end;

{ TStreamTracks }

procedure TStreamTracks.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited;
end;

destructor TStreamTracks.Destroy;
begin
  Clear;
  inherited;
end;

function TStreamTracks.Find(const Title: string): TStreamTrack;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Title = Title then
      Exit(Items[i]);
  Exit(nil);
end;

procedure TStreamTracks.FoundTitle(Offset: Int64; Title: string; BytesPerSec: Cardinal; FullTitle: Boolean);
begin
  if Count > 0 then
  begin
    Items[Count - 1].E := Offset;
    //if Assigned(FOnDebug) then
    //  FOnDebug(Format('Setting SongEnd of "%s" to %d', [Items[Count - 1].Title, Offset]), '');
  end;

  // Wenn wir automatisch aufnehmen und der Titel der Titel ist, den wir haben wollen
  // und dabei der erste empfangene ist, dann nutzen wir nicht den Start bei
  // empfangener Meta-Länge, sondern den Start des Streams.
  // Vielleicht gibt es da noch 1-2 Sekunden mehr vom Song...
  if (FRecordTitle <> '') and (Title = FRecordTitle) and (Count = 0) then
  begin
    Offset := Offset - BytesPerSec * 10;
    if Offset < 0 then
      Offset := 0;
  end;

  Add(TStreamTrack.Create(Offset, -1, Title, FullTitle));
  //if Assigned(FOnDebug) then
  //  FOnDebug(Format('Added "%s" with SongStart %d', [Items[Count - 1].Title, Offset]), '');
end;

{ TFileChecker }

constructor TFileChecker.Create(Streamname, Dir: string; SongsSaved: Cardinal; Settings: TStreamSettings);
begin
  inherited Create;

  FStreamname := Streamname;
  FSaveDir := IncludeTrailingBackslash(Dir);
  FSongsSaved := SongsSaved;
  FSettings := Settings;
end;

function TFileChecker.GetAppendNumber(Dir, Filename: string): Integer;
  function AnyFileExists(Filename: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to Ord(High(TAudioTypes)) do
      if TAudioTypes(i) <> atNone then
        if FileExists(Filename + FormatToFiletype(TAudioTypes(i))) then
          Exit(True);
  end;
var
  Append: Integer;
  FilenameTmp: string;
begin
  Result := -1;

  Append := 0;
  FilenameTmp := Filename;

  while AnyFileExists(Dir + FilenameTmp) do
  begin
    Inc(Append);
    FilenameTmp := Filename + ' (' + IntToStr(Append) + ')';
  end;

  if Append > 0 then
    Result := Append;
end;

procedure TFileChecker.GetFilename(Filesize: UInt64; Artist, Title, Album, StreamTitle: string; AudioType: TAudioTypes;
  TitleState: TTitleStates; Killed: Boolean);

  function AnyFileExists(Filename: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to Ord(High(TAudioTypes)) do
      if TAudioTypes(i) <> atNone then
        if FileExists(Filename + FormatToFiletype(TAudioTypes(i))) then
          Exit(True);
  end;
var
  Filename, Ext, Patterns: string;
begin
  FResult := crSave;

  Ext := FormatToFiletype(AudioType);

  case TitleState of
    tsAuto:
      Patterns := 'atlusdi';
    tsStream:
      Patterns := 'sdi';
    else
      Patterns := 'atlusndi';
  end;

  Filename := InfoToFilename(Artist, Title, Album, StreamTitle, TitleState, Patterns);
  Filename := GetValidFilename(Filename);

  if AnyFileExists(FSaveDir + Filename) then
  begin
    if FSettings.DiscardAlways then
    begin
      FResult := crDiscard;
    end else if FSettings.OverwriteSmaller and (GetFileSize(FSaveDir + Filename + Ext) < Filesize) then
    begin
      FResult := crOverwrite;
      FFilename := Filename + Ext;
    end else if FSettings.DiscardSmaller and (GetFileSize(FSaveDir + Filename + Ext) >= Filesize) then
    begin
      FResult := crDiscardExistingIsLarger;
    end else
    begin
      FFilename := FixPathName(Filename + ' (' + IntToStr(GetAppendNumber(FSaveDir, Filename)) + ')' + Ext);
    end;
  end else
  begin
    FResult := crSave;
    FFilename := FixPathName(Filename + Ext);
  end;

  if FSettings.OutputFormat <> atNone then
    FFilenameConverted := RemoveFileExt(FFilename) + FormatToFiletype(FSettings.OutputFormat)
  else
    FFilenameConverted := FFilename;

  FFilename := LimitToMaxPath(FFilename);
  FFilenameConverted := LimitToMaxPath(FFilenameConverted);
end;

procedure TFileChecker.GetStreamFilename(Name: string; AudioType: TAudioTypes);
var
  i: Integer;
  Ext: string;
begin
  case AudioType of
    atNone:
      raise Exception.Create('');
    atMPEG:
      Ext := '.mp3';
    atAAC:
      Ext := '.aac';
    atOGG:
      Ext := '.ogg';
  end;

  repeat
    for i := 1 to Length(FSettings.RemoveChars) do
      Name := StringReplace(Name, FSettings.RemoveChars[i], '', [rfReplaceAll]);

    if Trim(Name) = '' then
    begin
      Name := _('Unknown stream');
    end;

    Name := InfoToFilename('', '', '', '', tsStream, 'sdi');
    FFilename := GetValidFilename(Name);

    if FileExists(FSaveDir + Filename + Ext) then
    begin
      FFilename := Filename + ' (' + IntToStr(GetAppendNumber(FSaveDir, Filename)) + ')' + Ext;
    end else
      FFilename := Filename + Ext;

    if Length(FSaveDir + FFilename) > MAX_PATH - 2 then
      if Length(Name) = 1 then
        raise Exception.Create(_('Could not save file because it exceeds the maximum path length'))
      else
        Name := Copy(Name, 1, Length(Name) - 1);

  until Length(FSaveDir + FFilename) <= MAX_PATH - 2;

  FFilename := FixPathName(FFilename);
end;

function TFileChecker.GetValidFilename(Name: string): string;
begin
  Result := Name;
  Result := StringReplace(Result, '\', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '/', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, ':', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '*', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '"', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '?', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '<', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '>', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '|', ' ', [rfReplaceAll]);
end;

function TFileChecker.InfoToFilename(Artist, Title, Album, StreamTitle: string; TitleState: TTitleStates; Patterns: string): string;
var
  i: Integer;
  Dir, StreamName: string;
  Replaced: string;
  Arr: TPatternReplaceArray;
begin
  inherited;

  for i := 1 to Length(FSettings.RemoveChars) do
    Artist := StringReplace(Artist, FSettings.RemoveChars[i], '', [rfReplaceAll]);
  for i := 1 to Length(FSettings.RemoveChars) do
    Title := StringReplace(Title, FSettings.RemoveChars[i], '', [rfReplaceAll]);
  for i := 1 to Length(FSettings.RemoveChars) do
    Album := StringReplace(Album, FSettings.RemoveChars[i], '', [rfReplaceAll]);

  Dir := '';

  StreamName := Trim(GetValidFileName(FStreamname));
  if Length(StreamName) > 80 then
    StreamName := Copy(StreamName, 1, 80);

  Artist := GetValidFilename(Artist);
  Title := GetValidFilename(Title);

  if StreamName = '' then
    StreamName := _('Unknown stream');

  SetLength(Arr, Length(Patterns));
  for i := 0 to Length(Patterns) - 1 do
  begin
    Arr[i].C := Patterns[i + 1];
    case Arr[i].C of
      'a':
        Arr[i].Replace := Artist;
      't':
        Arr[i].Replace := Title;
      'l':
        Arr[i].Replace := Album;
      'u':
        Arr[i].Replace := StreamTitle;
      's':
        Arr[i].Replace := Trim(StreamName);
      'n':
        Arr[i].Replace := Format('%.*d', [FSettings.FilePatternDecimals, FSongsSaved]);
      'd':
        Arr[i].Replace := FormatDateTime('dd.mm.yy', Now);
      'i':
        Arr[i].Replace := FormatDateTime('hh.nn.ss', Now);
    end;
  end;

  case TitleState of
    tsFull:
      Replaced := PatternReplace(FSettings.FilePattern, Arr);
    tsIncomplete:
      Replaced := PatternReplace(FSettings.IncompleteFilePattern, Arr);
    tsAuto:
      begin
        AppGlobals.Lock;
        Replaced := PatternReplace(AppGlobals.AutomaticFilePattern, Arr);
        AppGlobals.Unlock;
      end;
    tsStream:
      Replaced := PatternReplace(FSettings.StreamFilePattern, Arr);
  end;

  Replaced := FixPatternFilename(Replaced);

  FSaveDir := FixPathName(IncludeTrailingBackslash(ExtractFilePath(FSaveDir + Replaced)));
  Result := ExtractFileName(Replaced);
end;

function TFileChecker.LimitToMaxPath(Filename: string): string;
var
  D, F, E: string;
begin
  Result := Filename;
  // Überall MAX_PATH-2.... -1 funzt nicht immer. Ich bin angetrunken und habe keine Lust das zu untersuchen!
  if (Length(Filename) > 0) and (Length(IncludeTrailingPathDelimiter(FSaveDir) + Filename) > MAX_PATH - 2) then
  begin
    D := IncludeTrailingPathDelimiter(FSaveDir);
    E := ExtractFileExt(Filename);
    F := RemoveFileExt(Filename);

    if Length(D + E) < MAX_PATH - 2 then
    begin
      Result := Copy(F, 1, MAX_PATH - 2 - Length(D + E)) + E;
    end;
  end;
end;

end.
