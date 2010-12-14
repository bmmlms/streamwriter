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
unit ICEStream;

interface

uses
  SysUtils, Windows, StrUtils, Classes, HTTPStream, ExtendedStream, AudioStream,
  AppData, LanguageObjects, Functions, DynBASS, WaveData, Generics.Collections;

type
  TDebugEvent = procedure(Text, Data: string) of object;
  TChunkReceivedEvent = procedure(Buf: Pointer; Len: Integer) of object;

  TAudioTypes = (atMPEG, atAAC, atNone);

  TStreamTrack = class
  public
    S, E: Int64;
    Title: string;
    constructor Create(S, E: Int64; Title: string);
  end;

  TStreamTracks = class(TList<TStreamTrack>)
  private
    FOnDebug: TDebugEvent;
  public
    destructor Destroy; override;
    procedure Clear; reintroduce;

    procedure FoundTitle(Offset: Int64; Title: string);
  end;

  TICEStream = class(THTTPStream)
  private
    FMetaInt: Integer;
    FSongsSaved: Cardinal;
    FStreamName: string;
    FStreamURL: string;
    FBitRate: Cardinal;
    FGenre: string;

    FSkipShort: Boolean;
    FSearchSilence: Boolean;
    FSilenceLevel: Cardinal;
    FSilenceLength: Cardinal;

    FFilePattern: string;
    FSaveDir: string;
    FDeleteStreams: Boolean;
    FShortSize: Integer;
    FSongBuffer: Integer;

    FMetaCounter: Integer;

    FTitle: string;
    FSavedFilename: string;
    FSavedTitle: string;
    FSavedSize: UInt64;
    FFilename: string;
    FSavedWasCut: Boolean;

    FSaveAllowedTitle: string;
    FSaveAllowed: Boolean;
    FSaveAllowedMatch: string;
    FSaveAllowedFilter: Integer;
    FRecording: Boolean;
    FRecordingStarted: Boolean;

    FStreamTracks: TStreamTracks;

    FAudioStream: TAudioStreamFile;
    FAudioType: TAudioTypes;

    FOnTitleChanged: TNotifyEvent;
    FOnSongSaved: TNotifyEvent;
    FOnNeedSettings: TNotifyEvent;
    FOnChunkReceived: TChunkReceivedEvent;
    FOnIOError: TNotifyEvent;
    FOnTitleAllowed: TNotifyEvent;

    procedure DataReceived(CopySize: Integer);
    procedure SaveData(S, E: UInt64; Title: string);
    procedure TrySave;
    procedure ProcessData;
    function GetFilename(var Dir: string; Name: string): string;
    function GetFilenameTitle(StreamTitle: string; var Dir: string): string;
    function GetValidFilename(Name: string): string;
    procedure GetSettings;
    procedure StreamTracksDebug(Text, Data: string);
    procedure FreeAudioStream;
    function StartRecordingInternal: Boolean;
    procedure StopRecordingInternal;
  protected
    procedure DoHeaderRemoved; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process; override;

    procedure StartRecording;
    procedure StopRecording;

    property MetaCounter: Integer read FMetaCounter;

    property StreamName: string read FStreamName;
    property StreamURL: string read FStreamURL;
    property BitRate: Cardinal read FBitRate;
    property Genre: string read FGenre;
    property Title: string read FTitle;
    property SavedFilename: string read FSavedFilename;
    property SavedTitle: string read FSavedTitle;
    property SavedSize: UInt64 read FSavedSize;
    property SongsSaved: Cardinal read FSongsSaved write FSongsSaved;
    property Filename: string read FFilename;
    property SavedWasCut: Boolean read FSavedWasCut;

    property SaveAllowedTitle: string read FSaveAllowedTitle;
    property SaveAllowed: Boolean read FSaveAllowed write FSaveAllowed;
    property SaveAllowedMatch: string read FSaveAllowedMatch write FSaveAllowedMatch;
    property SaveAllowedFilter: Integer read FSaveAllowedFilter write FSaveAllowedFilter;

    property AudioType: TAudioTypes read FAudioType;

    property SkipShort: Boolean read FSkipShort write FSkipShort;

    property OnTitleChanged: TNotifyEvent read FOnTitleChanged write FOnTitleChanged;
    property OnSongSaved: TNotifyEvent read FOnSongSaved write FOnSongSaved;
    property OnNeedSettings: TNotifyEvent read FOnNeedSettings write FOnNeedSettings;
    property OnChunkReceived: TChunkReceivedEvent read FOnChunkReceived write FOnChunkReceived;
    property OnIOError: TNotifyEvent read FOnIOError write FOnIOError;
    property OnTitleAllowed: TNotifyEvent read FOnTitleAllowed write FOnTitleAllowed;
  end;

const
  SILENCE_SEARCH_BUFFER = 75000;

implementation

{ TICEStream }

constructor TICEStream.Create;
begin
  inherited Create;
  FMetaInt := -1;
  FMetaCounter := 0;
  FSongsSaved := 0;
  FFilename := '';
  FBitRate := 0;
  FGenre := '';
  FTitle := '';
  FSavedFilename := '';
  FSavedTitle := '';
  FRecording := False;
  FRecordingStarted := False;
  FAudioType := atNone;
  FDeleteStreams := False;
  FStreamTracks := TStreamTracks.Create;
  FStreamTracks.FOnDebug := StreamTracksDebug;
end;

destructor TICEStream.Destroy;
begin
  FreeAudioStream;

  FreeAndNil(FStreamTracks);

  inherited;
end;

procedure TICEStream.DataReceived(CopySize: Integer);
var
  Buf: Pointer;
begin
  if FAudioStream <> nil then
  begin
    FAudioStream.Seek(0, soFromEnd);
    FAudioStream.CopyFrom(Self, CopySize);
  end else
    Seek(CopySize, soFromCurrent);

  if Assigned(FOnChunkReceived) then
  begin
    GetMem(Buf, CopySize);
    CopyMemory(Buf, Pointer(Integer(Memory) + (Position - CopySize)), CopySize);
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
      except
        WriteDebug(_('Meta-interval could not be found'), 1, 1);
      end;

      FStreamName := GetHeaderData('icy-name');
      FStreamURL := GetHeaderData('icy-url');
      FBitRate := StrToIntDef(GetHeaderData('icy-br'), 0);
      FGenre := GetHeaderData('icy-genre');

      Dir := FSaveDir;

      if LowerCase(ContentType) = 'audio/mpeg' then
      begin
        FAudioType := atMPEG;
      end else if LowerCase(ContentType) = 'audio/aacp' then
      begin
        FAudioType := atAAC;
      end else
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
    if Pos(#10'icy-metaint:', LowerCase(FHeader)) > 0 then
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
  if FAudioStream <> nil then
  begin
    Filename := FAudioStream.FileName;
    FreeAndNil(FAudioStream);
  end;
  if FDeleteStreams and (Filename <> '') then
    DeleteFile(PChar(Filename));
end;

procedure TICEStream.GetSettings;
begin
  if Assigned(FOnNeedSettings) then
    FOnNeedSettings(Self);
  AppGlobals.Lock;
  FFilePattern := AppGlobals.FilePattern;
  FSaveDir := AppGlobals.Dir;
  FDeleteStreams := AppGlobals.DeleteStreams;
  FShortSize := AppGlobals.ShortSize;
  FSongBuffer := AppGlobals.SongBuffer;
  FSearchSilence := AppGlobals.SearchSilence;
  FSilenceLevel := AppGlobals.SilenceLevel;
  FSilenceLength := AppGlobals.SilenceLength;
  AppGlobals.Unlock;
end;

procedure TICEStream.SaveData(S, E: UInt64; Title: string);
var
  Saved: Boolean;
  RangeBegin, RangeEnd: Int64;
  Dir, Filename: string;
begin
  Saved := False;

  RangeBegin := FAudioStream.GetFrame(S, False);
  RangeEnd := FAudioStream.GetFrame(E, True);

  WriteDebug(Format(_('Saving from %d to %d'), [S, E]), 1, 1);

  if (RangeEnd <= -1) or (RangeBegin <= -1) then
    raise Exception.Create(_('Error in audio data'));

  if SkipShort and (RangeEnd - RangeBegin < FShortSize * 1024) then
  begin
    WriteDebug(Format(_('Skipping "%s" because it''s too small (%d bytes)'), [Title, RangeEnd - RangeBegin]), 1, 0);
    Exit;
  end;

  Inc(FSongsSaved);
  try
    Filename := GetFilenameTitle(Title, Dir);

    FSaveAllowedTitle := Title;
    FSaveAllowed := True;
    if Assigned(FOnTitleAllowed) then
      FOnTitleAllowed(Self);
    if not FSaveAllowed then
    begin
      if FSaveAllowedFilter = 0 then
        WriteDebug(Format(_('Skipping "%s" - not on wishlist'), [Title]), 1, 0)
      else
        WriteDebug(Format(_('Skipping "%s" - on ignorelist ("%s")'), [Title, SaveAllowedMatch]), 1, 0);
      Dec(FSongsSaved);
      Exit;
    end;

    if Length(Title) > 0 then
      WriteDebug(Format('Saving title "%s"', [Title]), 1, 1)
    else
      WriteDebug('Saving unnamed title', 1, 1);

    try
      ForceDirectories(Dir);
    except
      raise Exception.Create(Format(_('Folder for saved tracks "%s" could not be created.'), [Dir]));
    end;

    try
      FAudioStream.SaveToFile(Filename, RangeBegin, RangeEnd - RangeBegin);
    except
      raise Exception.Create(_('Could not save file'));
    end;

    Saved := True;

    try
      FSavedFilename := Filename;
      FSavedTitle := Title;
      FSavedSize := RangeEnd - RangeBegin;
      if Assigned(FOnSongSaved) then
        FOnSongSaved(Self);
      WriteDebug(Format(_('Saved song "%s"'), [ExtractFilename(Filename)]), '', 1, 0);
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
      //WriteDebug(Format('Error while saving to "%s": %s', [Filename, E.Message]));
      WriteDebug(Format(_('Error while saving "%s": %s'), [ExtractFilename(Filename), E.Message]), 3, 0);
    end;
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
begin
  Result := False;

  if (FAudioStream = nil) and (FAudioType <> atNone) then
  begin
    Dir := FSaveDir;
    Filename := GetFilename(Dir, FStreamName);
    try
      ForceDirectories(Dir);
    except
      if Assigned(FOnIOError) then
        FOnIOError(Self);
      raise Exception.Create(_('Folder for saved tracks could not be created.'));
    end;

    try
      WriteDebug('Saving stream to "' + Filename + '"', 1, 1);
      case FAudioType of
        atMPEG:
          FAudioStream := TMPEGStreamFile.Create(Filename, fmCreate or fmShareDenyWrite);
        atAAC:
          FAudioStream := TAACStreamFile.Create(Filename, fmCreate or fmShareDenyWrite)
      end;
    except
      if Assigned(FOnIOError) then
        FOnIOError(Self);
      raise Exception.Create(Format(_('Could not create "%s"'), [Filename]));
    end;

    if FMetaCounter > 0 then
      FMetaCounter := 1;
    FStreamTracks.Clear;

    // Damit Der ICEStream sich FFilename wieder setzt, so dass aussen das Menü-Item fürs Play an ist.
    if Assigned(FOnTitleChanged) then
      FOnTitleChanged(Self);

    FFilename := Filename;

    Result := True;
  end;
end;

procedure TICEStream.StopRecordingInternal;
begin
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
      if FSearchSilence and (FAudioStream is TMPEGStreamFile) then
      begin
        if FAudioStream.Size > Track.E + SILENCE_SEARCH_BUFFER then
        begin
          WriteDebug(Format('Searching for silence using search range of %d bytes...', [SILENCE_SEARCH_BUFFER]), 1, 1);

          R := FAudioStream.SearchSilence(Track.S, Track.E, SILENCE_SEARCH_BUFFER, FSilenceLevel, FSilenceLength);

          if (R.A > -1) or (R.B > -1) then
          begin
            if (R.A > -1) and (R.B > -1) then
              FSavedWasCut := True;

            if R.A = -1 then
            begin
              WriteDebug('No silence at SongStart could be found, using configured buffer', 1, 1);
              R.A := Track.S - FSongBuffer * 1024;
              if R.A < FAudioStream.Size then
                R.A := Track.S;
            end else
              WriteDebug('Silence at SongStart found', 1, 1);

            if R.B = -1 then
            begin
              WriteDebug('No silence at SongEnd could be found', 1, 1);
              R.B := Track.E + FSongBuffer * 1024;
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

            SaveData(R.A, R.B, Track.Title);

            Track.Free;
            FStreamTracks.Delete(i);
            WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]), 1, 1);
          end else
          begin
            if (FAudioStream.Size >= Track.S + (Track.E - Track.S) + ((FSongBuffer * 2) * 1024)) and
               (FAudioStream.Size > Track.E + FSongBuffer * 1024) then
            begin
              WriteDebug(Format('No silence found, saving using buffer of %d bytes...', [FSongBuffer * 1024]), 1, 1);

              SaveData(Track.S - FSongBuffer * 1024, Track.E + FSongBuffer * 1024, Track.Title);

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
        if (FAudioStream.Size >= Track.S + (Track.E - Track.S) + ((FSongBuffer * 2) * 1024)) and
           (FAudioStream.Size > Track.E + FSongBuffer * 1024) then
        begin
          if FSearchSilence and (not (FAudioStream is TMPEGStreamFile)) then
            WriteDebug(Format('Saving using buffer of %d bytes because stream is not mpeg...', [FSongBuffer * 1024]), 1, 1)
          else
            WriteDebug(Format('Saving using buffer of %d bytes...', [FSongBuffer * 1024]), 1, 1);

          SaveData(Track.S - FSongBuffer * 1024, Track.E + FSongBuffer * 1024, Track.Title);

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

procedure TICEStream.ProcessData;
var
  TitleChanged: Boolean;
  MetaLen, P: Integer;
  Title, MetaData: string;
  Buf: Byte;
begin
  Seek(0, soFromBeginning);
  if FMetaInt = -1 then
  begin
    DataReceived(Size);
    Clear;
  end else
  begin
    TitleChanged := False;
    while Position < Size - FMetaInt - 4081 do // 4081 wegen 255*16+1 (Max-MetaLen)
    begin
      DataReceived(FMetaInt);

      if FAudioStream <> nil then
        TrySave;

      Read(Buf, 1);
      if Buf > 0 then
      begin
        MetaLen := Buf * 16;

        MetaData := Trim(string(ToString(Position, MetaLen)));
        Seek(MetaLen, soFromCurrent);
        P := PosEx(''';', MetaData, 14);
        Title := Trim(Copy(MetaData, 14, P - 14));

        if Title <> FTitle then
        begin
          WriteDebug(Format(_('"%s" now playing'), [Title]), 2, 0);
          TitleChanged := True;
          Inc(FMetaCounter);
        end;

        if (Title <> FTitle) and (FMetaCounter >= 3) then
        begin
          if FAudioStream <> nil then
            FStreamTracks.FoundTitle(FAudioStream.Size, Title);
        end else if Title = FTitle then
        begin

        end else
        begin
          if FMetaCounter = 2 then
          begin
            WriteDebug(Format(_('Recording of first song starting'), []), 1, 0);
            if FAudioStream <> nil then
              FStreamTracks.FoundTitle(FAudioStream.Size, Title);
          end;
        end;

        FTitle := Title;

        if TitleChanged then
          if Assigned(FOnTitleChanged) then
            FOnTitleChanged(Self);
      end;
    end;

    RemoveRange(0, Position);
  end;
end;

procedure TICEStream.Process;
begin
  inherited;
  if not HeaderRemoved then
    Exit;
  if HeaderType = 'icy'  then
  begin
    if (HeaderRemoved) and (Size > 32768) then
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

      ProcessData;
    end;
  end else if HeaderType = 'http' then
  begin
    if Size > 512000 then
      raise Exception.Create(_('Too many bytes in HTTP-response'))
  end else if HeaderRemoved then
    raise Exception.Create(_('Unknown header-type'));
end;

function TICEStream.GetFilename(var Dir: string; Name: string): string;
var
  Filename, Ext: string;
  Append: Integer;
begin
  inherited;

  if Dir <> '' then
    Dir := IncludeTrailingBackslash(Dir);

  if Trim(Name) = '' then
  begin
    Name := _('[Unnamed title]');
  end;

  Filename := GetValidFilename(Name);

  case FAudioType of
    atMPEG:
      Ext := '.mp3';
    atAAC:
      Ext := '.aac';
    atNone:
      raise Exception.Create('Error');
  end;

  if FileExists(Dir + Filename + Ext) then
  begin
    Append := 1;
    while FileExists(Dir + Filename + ' (' + IntToStr(Append) + ')' + Ext) do
      Inc(Append);
    Filename := Filename + ' (' + IntToStr(Append) + ')' + Ext;
  end else
    Filename := Filename + Ext;
  Result := Dir + Filename;
end;

function TICEStream.GetFilenameTitle(StreamTitle: string; var Dir: string): string;
var
  p: Integer;
  Artist, Title, StreamName, SaveTitle: string;

  Replaced: string;
  Arr: TPatternReplaceArray;
begin
  inherited;

  Dir := '';

  Artist := '';
  Title := '';
  StreamName := GetValidFileName(Trim(Self.StreamName));
  SaveTitle := GetValidFileName(Trim(StreamTitle));

  p := Pos(' - ', SaveTitle);
  if p > 0 then
  begin
    Artist := Trim(Copy(SaveTitle, 1, p - 1));
    Title := Trim(Copy(SaveTitle, p + 3, Length(SaveTitle)));
  end;

  if (Artist = '') or (Title = '') then
  begin
    Artist := _('Unknown artist');
    Title := SaveTitle;
    if Title = '' then
      Title := _('Unknown title');
  end;

  if StreamName = '' then
    StreamName := _('Unknown stream');

  SetLength(Arr, 6);
  Arr[0].C := 'a';
  Arr[0].Replace := Artist;
  Arr[1].C := 't';
  Arr[1].Replace := Title;
  Arr[2].C := 's';
  Arr[2].Replace := StreamName;
  Arr[3].C := 'n';
  Arr[3].Replace := IntToStr(FSongsSaved);
  Arr[4].C := 'd';
  Arr[4].Replace := FormatDateTime('dd.mm.yy', Now);
  Arr[5].C := 'i';
  Arr[5].Replace := FormatDateTime('hh.nn.ss', Now);

  Replaced := PatternReplace(FFilePattern, Arr);
  Dir := IncludeTrailingBackslash(ExtractFilePath(FSaveDir + Replaced));

  Result := GetFilename(Dir, ExtractFileName(Replaced));
end;

function TICEStream.GetValidFilename(Name: string): string;
begin
  Name := StringReplace(Name, '\', '_', [rfReplaceAll]);
  Name := StringReplace(Name, '/', '_', [rfReplaceAll]);
  Name := StringReplace(Name, ':', '_', [rfReplaceAll]);
  Name := StringReplace(Name, '*', '_', [rfReplaceAll]);
  Name := StringReplace(Name, '"', '_', [rfReplaceAll]);
  Name := StringReplace(Name, '?', '_', [rfReplaceAll]);
  Name := StringReplace(Name, '<', '_', [rfReplaceAll]);
  Name := StringReplace(Name, '>', '_', [rfReplaceAll]);
  Name := StringReplace(Name, '|', '_', [rfReplaceAll]);
  Result := Name;
end;

{ TStreamTrack }

constructor TStreamTrack.Create(S, E: Int64; Title: string);
begin
  Self.S := S;
  Self.E := E;
  Self.Title := Title;
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

procedure TStreamTracks.FoundTitle(Offset: Int64; Title: string);
begin
  if Count > 0 then
  begin
    Items[Count - 1].E := Offset;
    //if Assigned(FOnDebug) then
    //  FOnDebug(Format('Setting SongEnd of "%s" to %d', [Items[Count - 1].Title, Offset]), '');
  end;
  Add(TStreamTrack.Create(Offset, -1, Title));
  //if Assigned(FOnDebug) then
  //  FOnDebug(Format('Added "%s" with SongStart %d', [Items[Count - 1].Title, Offset]), '');
end;

end.
