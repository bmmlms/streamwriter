{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit ICEStream;

interface

uses
  SysUtils, Windows, StrUtils, Classes, HTTPStream, ExtendedStream, AudioStream,
  AppData, LanguageObjects, Functions, DynBASS, WaveData, Generics.Collections,
  Math, PerlRegEx, Logging, WideStrUtils, TypeDefs;

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
    FOnDebug: TDebugEvent;
  public
    destructor Destroy; override;
    procedure Clear; reintroduce;

    procedure FoundTitle(Offset: Int64; Title: string; FullTitle: Boolean);
  end;

  TCheckResults = (crSave, crDiscard, crOverwrite);

  TFileChecker = class
  private
    FSettings: TStreamSettings;
    FResult: TCheckResults;
    FStreamname: string;
    FSaveDir: string;
    FFilename: string;
    FSongsSaved: Cardinal;

    function GetValidFilename(Name: string): string;
    function GetAppendNumber(Dir, Filename, Extension: string): Integer;
    function TitleInfoToFilename(Artist, Title: string; FullTitle: Boolean): string;
  public
    constructor Create(Streamname, Dir: string; SongsSaved: Cardinal; Settings: TStreamSettings);

    procedure GetStreamFilename(Name: string; AudioType: TAudioTypes);
    procedure GetFilename(Filesize: UInt64; Artist, Title: string; AudioType: TAudioTypes; FullTitle: Boolean);

    property Result: TCheckResults read FResult;
    property SaveDir: string read FSaveDir;
    property Filename: string read FFilename;
  end;

  TICEStream = class(THTTPStream)
  private
    FSettings: TStreamSettings;
    FRecordTitle: string;
    FStopAfterSong: Boolean;

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

    FTitle: string;
    FSavedFilename: string;
    FSavedArtist: string;
    FSavedTitle: string;
    FSavedSize: UInt64;
    FSavedLength: UInt64;
    FSavedStreamTitle: string;
    FFilename: string;
    FSavedWasCut: Boolean;
    FSavedFullTitle: Boolean;
    FBytesPerSec: Integer;

    FSaveAllowedTitle: string;
    FSaveAllowed: Boolean;
    FSaveAllowedMatch: string;
    FSaveAllowedFilter: Integer;
    FRecording: Boolean;
    FRecordingStarted: Boolean;
    FFullTitleFound: Boolean;
    FRecordingTitleFound: Boolean;
    FHaltClient: Boolean;

    FStreamTracks: TStreamTracks;

    FAudioStream: TStream;
    FAudioType: TAudioTypes;

    FOnTitleChanged: TNotifyEvent;
    FOnSongSaved: TNotifyEvent;
    FOnNeedSettings: TNotifyEvent;
    FOnChunkReceived: TChunkReceivedEvent;
    FOnIOError: TNotifyEvent;
    FOnTitleAllowed: TNotifyEvent;
    FOnRefreshInfo: TNotifyEvent;

    procedure CalcBytesPerSec;
    procedure DataReceived(CopySize: Integer);
    procedure SaveData(S, E: UInt64; Title: string; FullTitle: Boolean);
    procedure TrySave;
    procedure ProcessData(Received: Cardinal);
    procedure GetSettings;
    procedure StreamTracksDebug(Text, Data: string);
    procedure FreeAudioStream;
    function StartRecordingInternal: Boolean;
    procedure StopRecordingInternal;

    function CleanTitle(Title: string): string;
    procedure ParseTitle(S, Pattern: string; var Artist: string; var Title: string);

    procedure FSetRecordTitle(Value: string);
  protected
    procedure DoHeaderRemoved; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(Received: Cardinal); override;

    procedure StartRecording;
    procedure StopRecording;
    procedure Disconnected; override;

    property Settings: TStreamSettings read FSettings;
    property RecordTitle: string read FRecordTitle write FSetRecordTitle;
    property StopAfterSong: Boolean read FStopAfterSong write FStopAfterSong;

    property MetaCounter: Integer read FMetaCounter;

    property StreamName: string read FStreamName;
    property StreamURL: string read FStreamURL;
    property BitRate: Cardinal read FBitRate;
    property Genre: string read FGenre;
    property Title: string read FTitle;
    property SavedFilename: string read FSavedFilename;
    property SavedArtist: string read FSavedArtist;
    property SavedTitle: string read FSavedTitle;
    property SavedSize: UInt64 read FSavedSize;
    property SavedLength: UInt64 read FSavedLength;
    property SavedStreamTitle: string read FSavedStreamTitle;
    property SongsSaved: Cardinal read FSongsSaved write FSongsSaved;
    property Filename: string read FFilename;
    property SavedWasCut: Boolean read FSavedWasCut;
    property SavedFullTitle: Boolean read FSavedFullTitle;

    property FullTitleFound: Boolean read FFullTitleFound write FFullTitleFound;
    property RecordingTitleFound: Boolean read FRecordingTitleFound write FRecordingTitleFound;
    property HaltClient: Boolean read FHaltClient;

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
  end;

implementation

{ TICEStream }

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
    TempPlayer := BASSStreamCreateFile(False, PChar(TAudioStreamFile(FAudioStream).FileName), 0, 0, BASS_STREAM_DECODE or BASS_UNICODE);
  end else
  begin
    Size := TAudioStreamMemory(FAudioStream).Size;
    if Size = 0 then
      raise Exception.Create('');
    TempPlayer := BASSStreamCreateFile(True, TAudioStreamMemory(FAudioStream).Memory, 0, Size, BASS_STREAM_DECODE);
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
    FBitRate := Trunc(BufLen / floor(((125 * Time)) + 0.5));

    if FBitRate < 40 then
      FBitRate := 32
    else if FBitRate < 70 then
      FBitRate := 64
    else if FBitRate < 100 then
      FBitRate := 96
    else if FBitRate < 130 then
      FBitRate := 128
    else if FBitRate < 170 then
      FBitRate := 160
    else if FBitRate < 200 then
      FBitRate := 192
    else if FBitRate < 230 then
      FBitRate := 224
    else if FBitRate < 260 then
      FBitRate := 256
    else if FBitRate < 330 then
      FBitRate := 320
    else
      FBitRate := 384;

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
      if (Ord(Title[i]) >= 32) then // and (Ord(Title[i]) < 126) then
        Result := Result + Title[i];
    end;
end;

constructor TICEStream.Create;
begin
  inherited Create;

  FSettings := TStreamSettings.Create;

  FMetaInt := -1;
  FMetaCounter := 0;
  FSongsSaved := 0;
  FFilename := '';
  FBitRate := 0;
  FGenre := '';
  FTitle := '';
  FSavedFilename := '';
  FSavedArtist := '';
  FSavedTitle := '';
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

  inherited;
end;

procedure TICEStream.Disconnected;
var
  Track: TStreamTrack;
begin
  // Falls erlaubt, versuchen, das Empfangene wegzuspeichern..
  if (FAudioStream <> nil) and (FStreamTracks.Count > 0) and (not FSettings.OnlySaveFull) and (FRecordTitle = '') then
  begin
    Track := FStreamTracks[0];
    Track.E := FAudioStream.Size;
    if Track.S - FSettings.SongBufferSeconds * FBytesPerSec >= 0 then
      Track.S := Track.S - FSettings.SongBufferSeconds * FBytesPerSec;
    SaveData(Track.S, Track.E, Track.Title, False);
    FStreamTracks.Clear;
  end;
end;

procedure TICEStream.DataReceived(CopySize: Integer);
var
  Buf: Pointer;
begin
  if FAudioStream <> nil then
  begin
    FAudioStream.Seek(0, soFromEnd);
    FAudioStream.CopyFrom(RecvStream, CopySize);
  end else
    Seek(CopySize, soFromCurrent);

  if Assigned(FOnChunkReceived) then
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

  if HeaderType = 'icy' then
  begin
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

      if LowerCase(ContentType) = 'audio/mpeg' then
        FAudioType := atMPEG
      else if LowerCase(ContentType) = 'audio/aacp' then
        FAudioType := atAAC
      //else if LowerCase(ContentType) = 'application/ogg' then
      //  FAudioType := atOGG
      else
        raise Exception.Create(_('Unknown content-type'));

      if FRecording then
        StartRecording;

      if Assigned(FOnTitleChanged) then
        FOnTitleChanged(Self);
    end else
      raise Exception.Create(Format(_('Invalid responsecode (%d)'), [ResponseCode]));
  end else if HeaderType = 'http' then
  begin
    // Wenn wir hier drin sind, müsste es eigentlich eine Playlist sein.
    // Es gibt aber Stationen (z.B. http://stream.laut.fm/disco), die
    // bei einem ICY-Stream einen HTTP-Header liefern...
    if (Pos(#10'icy-metaint:', LowerCase(FHeader)) > 0) or (Pos(#10'icy-name:', LowerCase(FHeader)) > 0) then
    begin
      WriteDebug(_('HTTP header detected but icy fields found - treating as icy header'), 1, 1);
      FHeaderType := 'icy';
      DoHeaderRemoved;
    end;
  end else
    raise Exception.Create(_('Unknown header-type'));
end;

procedure TICEStream.FreeAudioStream;
var
  Filename: string;
begin
  Filename := '';
  FFilename := '';
  if FAudioStream <> nil then
  begin
    if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
      Filename := TAudioStreamFile(FAudioStream).FileName;
    FreeAndNil(FAudioStream);
  end;

  if (FSettings.SeparateTracks) and (FSettings.DeleteStreams and (Filename <> '')) then
    DeleteFile(PChar(Filename));
end;

procedure TICEStream.FSetRecordTitle(Value: string);
begin
  if Value <> FRecordTitle then
    WriteDebug(Format(_('Recording "%s"'), [Value]), 1, 0);
  FRecordTitle := Value;
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

procedure TICEStream.SaveData(S, E: UInt64; Title: string; FullTitle: Boolean);
  procedure RemoveData;
  var
    i: Integer;
    BufLen: Int64;
  begin
    // Für das nächste Lied einen Puffer daüberlassen, Rest abschneiden. Puffer ist der größere
    // der beiden Werte, weil wir nicht wissen, ob für das nächste Lied Stille gefunden wird,
    // oder der normale Puffer genutzt wird.
    if FAudioStream.ClassType.InheritsFrom(TAudioStreamMemory) then
    begin
      BufLen := Max(FBytesPerSec * FSettings.SilenceBufferSeconds, FBytesPerSec * FSettings.SongBufferSeconds);
      if FStreamTracks.Count > 1 then
      begin
        BufLen := FStreamTracks[FStreamTracks.Count - 1].S - BufLen;

        // Weil wir gleich alles abschneiden, müssen wir eventuell vorgemerkte Tracks anfassen
        for i := 1 to FStreamTracks.Count - 1 do
        begin
          if FStreamTracks[i].S > -1 then
            FStreamTracks[i].S := FStreamTracks[i].S - BufLen;
          if FStreamTracks[i].E > -1 then
            FStreamTracks[i].E := FStreamTracks[i].E - BufLen;
        end;
      end else
        BufLen := TAudioStreamMemory(FAudioStream).Size;

      TAudioStreamMemory(FAudioStream).RemoveRange(0, BufLen);
    end;
  end;
var
  Saved: Boolean;
  Dir, Filename: string;
  FileCheck: TFileChecker;
  P: TPosRect;
  Error: Cardinal;
begin
  Saved := False;

  try
    if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
      P := TAudioStreamFile(FAudioStream).GetFrame(S, E)
    else
      P := TAudioStreamMemory(FAudioStream).GetFrame(S, E);

    if FBytesPerSec = 0 then
    begin
      RemoveData;
      Exit;
    end;

    if (P.A <= -1) or (P.B <= -1) then
      raise Exception.Create(_('Error in audio data'));

    WriteDebug(Format(_('Saving from %d to %d'), [S, E]), 1, 1);

    if FSettings.SkipShort and (P.B - P.A < FBytesPerSec * FSettings.ShortLengthSeconds) then
    begin
      WriteDebug(Format(_('Skipping "%s" because it''s too small (%d bytes)'), [Title, P.B - P.A]), 1, 0);
      RemoveData;
      Exit;
    end;

    Inc(FSongsSaved);
    try
      FSaveAllowedTitle := Title;
      FSaveAllowed := True;

      ParseTitle(Title, FSettings.TitlePattern, FSavedArtist, FSavedTitle);

      if FRecordTitle <> '' then
        FileCheck := TFileChecker.Create(FStreamName, FSaveDirAuto, FSongsSaved, FSettings)
      else
        FileCheck := TFileChecker.Create(FStreamName, FSaveDir, FSongsSaved, FSettings);

      try
        FileCheck.GetFilename(E - S, FSavedArtist, FSavedTitle, FAudioType, FullTitle);
        if (FileCheck.Result in [crSave, crOverwrite]) and (FileCheck.FFilename <> '') then
        begin
          Dir := FileCheck.SaveDir;
          Filename := FileCheck.Filename;
        end else if FileCheck.Result <> crDiscard then
          raise Exception.Create(_('Could not determine filename for title'));

        if FileCheck.Result = crDiscard then
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
            else
              WriteDebug(Format(_('Skipping "%s" - on ignorelist (matches "%s")'), [Title, SaveAllowedMatch]), 1, 0);
            Dec(FSongsSaved);
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
          TAudioStreamFile(FAudioStream).SaveToFile(Dir + Filename, P.A, P.B - P.A)
        else
        begin
          TAudioStreamMemory(FAudioStream).SaveToFile(Dir + Filename, P.A, P.B - P.A);
        end;

        Error := GetLastError;

        RemoveData;
      except
        if (Error = 3) and (Length(Dir + Filename) > MAX_PATH - 1) then
          raise Exception.Create('Could not save file because it exceeds the maximum path length (259)')
        else
          raise Exception.Create(_('Could not save file'));
      end;

      Saved := True;

      try
        FSavedFilename := Dir + Filename;
        FSavedSize := P.B - P.A;
        FSavedFullTitle := FullTitle;
        FSavedStreamTitle := Title;
        if FBytesPerSec > 0 then
          FSavedLength := Trunc(FSavedSize / FBytesPerSec)
        else
          FSavedLength := 0;
        if Assigned(FOnSongSaved) then
          FOnSongSaved(Self);

        if FullTitle then
          WriteDebug(Format(_('Saved song "%s"'), [ExtractFilename(Filename)]), '', 1, 0)
        else
          WriteDebug(Format(_('Saved incomplete song "%s"'), [ExtractFilename(Filename)]), '', 1, 0);
      except
        on E: Exception do
        begin
          WriteDebug(Format('Error after successful save: %s', [E.Message]), 1, 0);
          raise;
        end;
      end;
    except
      on E: Exception do
      begin
        if not Saved then
          Dec(FSongsSaved);
        WriteDebug(Format(_('Error while saving "%s": %s'), [ExtractFilename(Filename), E.Message]), 3, 0);
      end;
    end;
  finally
    if (Title = FRecordTitle) and (FRecordTitle <> '') then
      FHaltClient := True;
  end;
end;

procedure TICEStream.StartRecording;
begin
  //I := Integer(@FRecordingStarted);
  //InterlockedExchange(I, Integer(True));
  FRecordingStarted := True;
end;

procedure TICEStream.StopRecording;
begin
  //I := Integer(@FRecordingStarted);
  //InterlockedExchange(I, Integer(False));
  FRecordingStarted := False;
end;

function TICEStream.StartRecordingInternal: Boolean;
var
  Dir, Filename: string;
  Error: Cardinal;
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
    if (not FSettings.OnlySaveFull) and (FAudioStream <> nil) and (FMetaCounter >= 1) and (Title <> '') then
    begin
      FRecordingTitleFound := True;
      FStreamTracks.FoundTitle(0, Title, False);
    end;

    // Damit Der ICEStream sich FFilename wieder setzt, so dass aussen das Menü-Item fürs Play an ist.
    if Assigned(FOnTitleChanged) then
      FOnTitleChanged(Self);

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
  Track: TStreamTrack;
begin
  FSavedWasCut := False;

  for i := FStreamTracks.Count - 1 downto 0 do
  begin
    Track := FStreamTracks[i];

    if (Track.S > -1) and (Track.E > -1) then
    begin
      if FSettings.SearchSilence then
      begin
        if FAudioStream.Size > Track.E + FBytesPerSec * FSettings.SilenceBufferSeconds then
        begin
          WriteDebug(Format('Searching for silence using search range of %d bytes...', [FBytesPerSec * FSettings.SilenceBufferSeconds]), 1, 1);

          if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
            R := TAudioStreamFile(FAudioStream).SearchSilence(Track.S, Track.E, FBytesPerSec * FSettings.SilenceBufferSeconds, FSettings.SilenceLevel, FSettings.SilenceLength)
          else
            R := TAudioStreamMemory(FAudioStream).SearchSilence(Track.S, Track.E, FBytesPerSec * FSettings.SilenceBufferSeconds, FSettings.SilenceLevel, FSettings.SilenceLength);

          if (R.A > -1) or (R.B > -1) then
          begin
            if (R.A > -1) and (R.B > -1) then
              FSavedWasCut := True;

            if R.A = -1 then
            begin
              WriteDebug('No silence at SongStart could be found, using configured buffer', 1, 1);
              R.A := Track.S - FSettings.SongBufferSeconds * FBytesPerSec;
              if R.A < FAudioStream.Size then
                R.A := Track.S;
            end else
              WriteDebug('Silence at SongStart found', 1, 1);

            if R.B = -1 then
            begin
              WriteDebug('No silence at SongEnd could be found', 1, 1);
              R.B := Track.E + FSettings.SongBufferSeconds * FBytesPerSec;
              if R.B > FAudioStream.Size then
              begin
                WriteDebug('Stream is too small, waiting for more data...', 1, 1);
                Exit;
              end else
              begin
                WriteDebug('Using configured buffer...', 1, 1);
              end;
            end else
              WriteDebug('Silence at SongEnd found', 1, 1);

            WriteDebug(Format('Scanned song start/end: %d/%d', [R.A, R.B]), 1, 1);

            if (FRecordTitle = '') or ((FRecordTitle <> '') and (FRecordTitle = Track.Title)) then
            begin
              if FRecordTitle <> '' then
                SaveData(R.A, R.B, Track.Title, True)
              else
                SaveData(R.A, R.B, Track.Title, Track.FullTitle);
            end else
              WriteDebug('Skipping title because it is not the title to be saved', 1, 1);

            Track.Free;
            FStreamTracks.Delete(i);
            WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]), 1, 1);
          end else
          begin
            if (FAudioStream.Size >= Track.S + (Track.E - Track.S) + ((FSettings.SongBufferSeconds * FBytesPerSec) * 2)) and
               (FAudioStream.Size > Track.E + (FSettings.SongBufferSeconds * FBytesPerSec) * 2) then
            begin
              WriteDebug(Format('No silence found, saving using buffer of %d bytes...', [FSettings.SongBufferSeconds * FBytesPerSec]), 1, 1);

              if (FRecordTitle = '') or ((FRecordTitle <> '') and (FRecordTitle = Track.Title)) then
              begin
                if FRecordTitle <> '' then
                  SaveData(Track.S - FSettings.SongBufferSeconds * FBytesPerSec, Track.E + FSettings.SongBufferSeconds * FBytesPerSec, Track.Title, True)
                else
                  SaveData(Track.S - FSettings.SongBufferSeconds * FBytesPerSec, Track.E + FSettings.SongBufferSeconds * FBytesPerSec, Track.Title, Track.FullTitle);
              end else
                WriteDebug('Skipping title because it is not the title to be saved', 1, 1);

              Track.Free;
              FStreamTracks.Delete(i);
              WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]), 1, 1);
            end else
            begin
              // WriteDebug('Waiting for full buffer because no silence found...');
            end;
          end;
        end else
        begin
          // WriteDebug(Format('Waiting to save "%s" because stream is too small', [Track.Title]));
        end;
      end else
      begin
        if (FAudioStream.Size >= Track.S + (Track.E - Track.S) + ((FSettings.SongBufferSeconds * FBytesPerSec) * 2)) and
           (FAudioStream.Size > Track.E + FSettings.SongBufferSeconds * FBytesPerSec) then
        begin
          WriteDebug(Format('Saving using buffer of %d bytes...', [FSettings.SongBufferSeconds * FBytesPerSec]), 1, 1);

          if (FRecordTitle = '') or ((FRecordTitle <> '') and (FRecordTitle = Track.Title)) then
          begin
            if FRecordTitle <> '' then
              SaveData(Track.S - FSettings.SongBufferSeconds * FBytesPerSec, Track.E + FSettings.SongBufferSeconds * FBytesPerSec, Track.Title, True)
            else
              SaveData(Track.S - FSettings.SongBufferSeconds * FBytesPerSec, Track.E + FSettings.SongBufferSeconds * FBytesPerSec, Track.Title, Track.FullTitle);
          end else
            WriteDebug('Skipping title because it is not the title to be saved', 1, 1);

          Track.Free;
          FStreamTracks.Delete(i);
          WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]), 1, 1);
        end else
        begin
          // WriteDebug('Waiting for full buffer...');
        end;
      end;
    end;
  end;
end;

procedure TICEStream.ProcessData(Received: Cardinal);
var
  TitleChanged, IgnoreTitle: Boolean;
  i, MetaLen, P, DataCopied: Integer;
  Title: string;
  MetaData: AnsiString;
  Buf: Byte;
  Track: TStreamTrack;
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

    if (FBytesPerSec = 0) and (FAudioStream.Size > 32768) then
    begin
      try
        CalcBytesPerSec;
        FOnRefreshInfo(Self);;
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
    DataReceived(RecvStream.Size);
    RecvStream.Clear;
  end else
  begin
    TitleChanged := False;

    if FSettings.SeparateTracks then
      TrySave;

    if (RecordTitle <> '') and (FBytesPerSec > 0) and (FTitle <> FRecordTitle) then
    begin
      if FStreamTracks.Count > 0 then
      begin
        Track := FStreamTracks[FStreamTracks.Count - 1];

        if (FMetaCounter > 2) and (Track.E > -1) and (FAudioStream.Size > Track.E + Max(FBytesPerSec * (FSettings.SilenceBufferSeconds + 10) * 2, FBytesPerSec * (FSettings.SongBufferSeconds + 10) * 2)) then
        begin
          // Zuviel empfangen und Titel war nicht dabei
          FHaltClient := True;
        end;
      end;

      if (FAudioStream.Size > FBytesPerSec * 30) and (FMetaCounter = 1) then
      begin
        // Titel scheint nicht zu kommen..
        FHaltClient := True;
      end;

      // Paranoid, I is it.
      if FMetaCounter > 4 then
        FHaltClient := True;
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
            // TODO: Das ist so nicht Threadsicher.
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

                // Ist nur dafür da, um dem Server zu sagen "hier läuft jetzt ein volles Lied"
                if (FMetaCounter >= 2) then
                  FFullTitleFound := True;
              end;

              if FSettings.SeparateTracks then
                if (Title <> FTitle) and (FRecordingTitleFound) then
                begin
                  if FAudioStream <> nil then
                    FStreamTracks.FoundTitle(FAudioStream.Size, Title, True);
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
                        if FAudioStream.InheritsFrom(TAudioStreamMemory) then
                        begin
                          // Stream sauber machen.
                          {
                          if FSettings.SearchSilence then
                            TAudioStreamMemory(FAudioStream).RemoveRange(0, FAudioStream.Size - (FBytesPerSec * FSettings.SilenceBufferSeconds))
                          else
                            TAudioStreamMemory(FAudioStream).RemoveRange(0, FAudioStream.Size - (FBytesPerSec * FSettings.SongBufferSeconds));
                          }
                        end;

                        if FMetaCounter >= 2 then
                          FStreamTracks.FoundTitle(FAudioStream.Size, Title, True)
                        else
                          FStreamTracks.FoundTitle(FAudioStream.Size, Title, False);
                      end;
                    end;
                end;

              FTitle := Title;

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

procedure TICEStream.ParseTitle(S, Pattern: string; var Artist: string; var Title: string);
var
  R: TPerlRegEx;
begin
  Artist := '';
  Title := '';

  if (S <> '') and (Pattern <> '') then
  begin
    R := TPerlRegEx.Create;
    try
      R.Subject := S;
      R.RegEx := Pattern;
      try
        if R.Match then
        begin
          try
            Artist := Trim(R.Groups[R.NamedGroup('a')]);
          except end;
          try
            Title := Trim(R.Groups[R.NamedGroup('t')]);
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
    ParseTitle(S, '(?P<a>.*) - (?P<t>.*)', Artist, Title);
  end;

  if (Artist = '') or (Title = '') then
  begin
    Artist := _('Unknown artist');
    Title := S;
    if Title = '' then
      Title := _('Unknown title');
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
      GetSettings;

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

procedure TStreamTracks.FoundTitle(Offset: Int64; Title: string; FullTitle: Boolean);
begin
  if Count > 0 then
  begin
    Items[Count - 1].E := Offset;
    //if Assigned(FOnDebug) then
    //  FOnDebug(Format('Setting SongEnd of "%s" to %d', [Items[Count - 1].Title, Offset]), '');
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

function TFileChecker.GetAppendNumber(Dir, Filename, Extension: string): Integer;
var
  Append: Integer;
  FilenameTmp: string;
begin
  Result := -1;

  Append := 0;
  FilenameTmp := Filename;

  while FileExists(Dir + FilenameTmp + Extension) do
  begin
    Inc(Append);
    FilenameTmp := Filename + ' (' + IntToStr(Append) + ')';
  end;

  if Append > 0 then
    Result := Append;
end;

procedure TFileChecker.GetFilename(Filesize: UInt64; Artist, Title: string; AudioType: TAudioTypes; FullTitle: Boolean);
var
  Filename, Ext: string;
begin
  FResult := crSave;

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

  Filename := TitleInfoToFilename(Artist, Title, FullTitle);
  Filename := GetValidFilename(Filename);

  if FileExists(FSaveDir + Filename + Ext) then
  begin
    if FSettings.OverwriteSmaller and (GetFileSize(FSaveDir + Filename + Ext) < Filesize) then
    begin
      FResult := crOverwrite;
      FFilename := Filename + Ext;
      Exit;
    end else if FSettings.DiscardSmaller and (GetFileSize(FSaveDir + Filename + Ext) >= Filesize) then
    begin
      FResult := crDiscard;
      Exit;
    end else
    begin
      FFilename := Filename + ' (' + IntToStr(GetAppendNumber(FSaveDir, Filename, Ext)) + ')' + Ext;
    end;
  end else
  begin
    FResult := crSave;
    FFilename := Filename + Ext;
  end;
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
    // REMARK: Zugriff ist nicht Threadsicher!
    for i := 1 to Length(FSettings.RemoveChars) do
      Name := StringReplace(Name, FSettings.RemoveChars[i], '', [rfReplaceAll]);

    if Trim(Name) = '' then
    begin
      Name := _('Unknown stream');
    end;

    Name := Trim(Name);

    FFilename := GetValidFilename(Name);

    if FileExists(FSaveDir + Filename + Ext) then
    begin
      FFilename := Filename + ' (' + IntToStr(GetAppendNumber(FSaveDir, Filename, Ext)) + ')' + Ext;
    end else
      FFilename := Filename + Ext;

    if Length(FSaveDir + FFilename) > MAX_PATH - 1 then
      if Length(Name) = 1 then
        raise Exception.Create('Could not save file because it exceeds the maximum path length (259)')
      else
        Name := Copy(Name, 1, Length(Name) - 1);

  until Length(FSaveDir + FFilename) <= MAX_PATH - 1;
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

function TFileChecker.TitleInfoToFilename(Artist, Title: string; FullTitle: Boolean): string;
var
  i: Integer;
  Dir, StreamName: string;
  Replaced: string;
  Arr: TPatternReplaceArray;
begin
  inherited;

  // REMARK: Zugriff ist nicht Threadsicher!
  for i := 1 to Length(FSettings.RemoveChars) do
    Artist := StringReplace(Artist, FSettings.RemoveChars[i], '', [rfReplaceAll]);
  for i := 1 to Length(FSettings.RemoveChars) do
    Title := StringReplace(Title, FSettings.RemoveChars[i], '', [rfReplaceAll]);

  Dir := '';

  StreamName := Trim(GetValidFileName(FStreamname));
  if Length(StreamName) > 80 then
    StreamName := Copy(StreamName, 1, 80);

  Artist := GetValidFilename(Artist);
  Title := GetValidFilename(Title);

  if StreamName = '' then
    StreamName := _('Unknown stream');

  SetLength(Arr, 6);
  Arr[0].C := 'a';
  Arr[0].Replace := Artist;
  Arr[1].C := 't';
  Arr[1].Replace := Title;
  Arr[2].C := 's';
  Arr[2].Replace := Trim(StreamName);
  Arr[3].C := 'n';
  Arr[3].Replace := Format('%.*d', [FSettings.FilePatternDecimals, FSongsSaved]);
  Arr[4].C := 'd';
  Arr[4].Replace := FormatDateTime('dd.mm.yy', Now);
  Arr[5].C := 'i';
  Arr[5].Replace := FormatDateTime('hh.nn.ss', Now);

  if FullTitle then
    Replaced := PatternReplace(FSettings.FilePattern, Arr)
  else
    Replaced := PatternReplace(FSettings.IncompleteFilePattern, Arr);

  // REMARK: Das folgende ist so genau gleich auch im Settings-Fenster.. wegen DRY..
  // Aneinandergereihte \ entfernen
  i := 1;
  if Length(Replaced) > 0 then
    while True do
    begin
      if i = Length(Replaced) then
        Break;
      if Replaced[i] = '\' then
        if Replaced[i + 1] = '\' then
        begin
          Replaced := Copy(Replaced, 1, i) + Copy(Replaced, i + 2, Length(Replaced) - i);
          Continue;
        end;
      Inc(i);
    end;
  // Ungültige Zeichen entfernen
  Replaced := StringReplace(Replaced, '/', '_', [rfReplaceAll]);
  Replaced := StringReplace(Replaced, ':', '_', [rfReplaceAll]);
  Replaced := StringReplace(Replaced, '*', '_', [rfReplaceAll]);
  Replaced := StringReplace(Replaced, '"', '_', [rfReplaceAll]);
  Replaced := StringReplace(Replaced, '?', '_', [rfReplaceAll]);
  Replaced := StringReplace(Replaced, '<', '_', [rfReplaceAll]);
  Replaced := StringReplace(Replaced, '>', '_', [rfReplaceAll]);
  Replaced := StringReplace(Replaced, '|', '_', [rfReplaceAll]);
  // Sicherstellen, dass am Anfang/Ende kein \ steht
  if Length(Replaced) > 0 then
    if Replaced[1] = '\' then
      Replaced := Copy(Replaced, 2, Length(Replaced) - 1);
  if Length(Replaced) > 0 then
    if Replaced[Length(Replaced)] = '\' then
      Replaced := Copy(Replaced, 1, Length(Replaced) - 1);

  FSaveDir := IncludeTrailingBackslash(ExtractFilePath(FSaveDir + Replaced));
  Result := ExtractFileName(Replaced);
end;

end.
