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
  AppData, LanguageObjects, Functions, DynBASS, WaveData, Generics.Collections,
  Math;

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
    FSettings: TStreamSettings;
    FRecordTitle: string;

    FMetaInt: Integer;
    FSongsSaved: Cardinal;
    FStreamName: string;
    FStreamURL: string;
    FBitRate: Cardinal;
    FGenre: string;

    FSaveDir: string;

    FMetaCounter: Integer;

    FTitle: string;
    FSavedFilename: string;
    FSavedTitle: string;
    FSavedSize: UInt64;
    FFilename: string;
    FSavedWasCut: Boolean;
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

    procedure CalcBytesPerSec;
    procedure DataReceived(CopySize: Integer);
    procedure SaveData(S, E: UInt64; Title: string);
    procedure TrySave;
    procedure ProcessData;
    function GetFilename(var Dir: string; Name: string; Filesize: Int64): string;
    function GetFilenameTitle(StreamTitle: string; var Dir: string; Filesize: Int64): string;
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

    property Settings: TStreamSettings read FSettings;
    property RecordTitle: string read FRecordTitle write FRecordTitle;

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

    if FBytesPerSec <= 10 then
      raise Exception.Create('');
  finally
    BASSStreamFree(TempPlayer);
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
    if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
      Filename := TAudioStreamFile(FAudioStream).FileName;
    FreeAndNil(FAudioStream);
  end;

  if (FSettings.SeparateTracks) and (FSettings.DeleteStreams and (Filename <> '')) then
    DeleteFile(PChar(Filename));
end;

procedure TICEStream.GetSettings;
begin
  if Assigned(FOnNeedSettings) then
    FOnNeedSettings(Self);

  AppGlobals.Lock;
  FSaveDir := AppGlobals.Dir;
  AppGlobals.Unlock;
end;

procedure TICEStream.SaveData(S, E: UInt64; Title: string);
var
  Saved, Kill: Boolean;
  RangeBegin, RangeEnd, BufLen: Int64;
  Dir, Filename: string;
  i: Integer;
begin
  Saved := False;
  Kill := Title = FRecordTitle;

  if FAudioStream.ClassType.InheritsFrom(TAudioStreamFile) then
  begin
    RangeBegin := TAudioStreamFile(FAudioStream).GetFrame(S, False);
    RangeEnd := TAudioStreamFile(FAudioStream).GetFrame(E, True);
  end else
  begin
    RangeBegin := TAudioStreamMemory(FAudioStream).GetFrame(S, False);
    RangeEnd := TAudioStreamMemory(FAudioStream).GetFrame(E, True);
  end;

  WriteDebug(Format(_('Saving from %d to %d'), [S, E]), 1, 1);

  if (RangeEnd <= -1) or (RangeBegin <= -1) then
    raise Exception.Create(_('Error in audio data'));

  if FSettings.SkipShort and (RangeEnd - RangeBegin < FBytesPerSec * FSettings.ShortLengthSeconds) then
  begin
    WriteDebug(Format(_('Skipping "%s" because it''s too small (%d bytes)'), [Title, RangeEnd - RangeBegin]), 1, 0);
    if Kill then
      FHaltClient := True;
    Exit;
  end;

  Inc(FSongsSaved);
  try
    FSaveAllowedTitle := Title;
    FSaveAllowed := True;

    Filename := GetFilenameTitle(Title, Dir, E - S);

    // Wenn die Option zum Überschreiben vorhandener Dateien aktiv ist und die
    // Datei schon existiert, übergehen wir den Filter.
    if not (FSettings.OverwriteSmaller and FileExists(Filename) and (GetFileSize(Filename) < E - S)) then
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
        Exit;
      end;
    end else
    begin
      WriteDebug(Format(_('Saving "%s" - it overwrites a smaller same named file'), [Title]), 1, 0)
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
        TAudioStreamFile(FAudioStream).SaveToFile(Filename, RangeBegin, RangeEnd - RangeBegin)
      else
      begin
        TAudioStreamMemory(FAudioStream).SaveToFile(Filename, RangeBegin, RangeEnd - RangeBegin);

        // Für das nächste Lied einen Puffer daüberlassen, Rest abschneiden. Puffer ist der größere
        // der beiden Werte, weil wir nicht wissen, ob für das nächste Lied Stille gefunden wird,
        // oder der normale Puffer genutzt wird.
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

    if Kill then
      FHaltClient := True;
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
  i: Integer;
  Dir, Filename: string;
begin
  Result := False;
  Filename := '';

  if (FAudioStream = nil) and (FAudioType <> atNone) then
  begin
    Dir := FSaveDir;

    if (not FSettings.SaveToMemory) then
      if not (FSettings.SeparateTracks) then
        Filename := GetFilenameTitle(FStreamName, Dir, 0)
      else
        Filename := GetFilename(Dir, FStreamName, 0);

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
              FAudioStream := TMPEGStreamFile.Create(Filename, fmCreate or fmShareDenyWrite);
          end;
        atAAC:
          begin
            if FSettings.SaveToMemory then
              FAudioStream := TAACStreamMemory.Create
            else
              FAudioStream := TAACStreamFile.Create(Filename, fmCreate or fmShareDenyWrite);
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

    // Falls schon abgespielt wird, und jetzt aufgenommen wir, eventuell da aufsetzen und nicht von vorne anfangen.
    // Funzt leider nicht, weil FStreamTracks nur befüllt wird, wenn FAudioStream <> nil ist, und das ist nur
    // der Fall, wenn die Aufnahme schon aktiv ist. Also nicht, während nur abgespielt wird...
    {
    if (((FMetaCounter = 1) and (not FSettings.OnlySaveFull))) and FSettings.SeparateTracks then
    begin
      FRecordingTitleFound := True;
      if FStreamTracks.Count > 1 then
        for i := FStreamTracks.Count - 2 downto 0 do
        begin
          FStreamTracks[i].Free;
          FStreamTracks.Delete(i);
        end;
    end else
    }
    FStreamTracks.Clear;

    FFilename := Filename;

    // Damit Der ICEStream sich FFilename wieder setzt, so dass aussen das Menü-Item fürs Play an ist.
    if Assigned(FOnTitleChanged) then
      FOnTitleChanged(Self);

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
              SaveData(R.A, R.B, Track.Title)
            else
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
                SaveData(Track.S - FSettings.SongBufferSeconds * FBytesPerSec, Track.E + FSettings.SongBufferSeconds * FBytesPerSec, Track.Title)
              else
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
            SaveData(Track.S - FSettings.SongBufferSeconds * FBytesPerSec, Track.E + FSettings.SongBufferSeconds * FBytesPerSec, Track.Title)
          else
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

procedure TICEStream.ProcessData;
var
  TitleChanged: Boolean;
  MetaLen, P: Integer;
  Title, MetaData: string;
  Buf: Byte;
  Track: TStreamTrack;
begin
  Seek(0, soFromBeginning);

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
    DataReceived(Size);
    Clear;
  end else
  begin
    TitleChanged := False;
    while Position < Size - FMetaInt - 4081 do // 4081 wegen 255*16+1 (Max-MetaLen)
    begin
      DataReceived(FMetaInt);

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
        end else
        begin
          // Keine Tracks in der Liste..
          //FHaltClient := True;
        end;

        if (FAudioStream.Size > FBytesPerSec * 30) and (FMetaCounter = 1) then
        begin
          // Titel scheint nicht zu kommen..
          FHaltClient := True;
        end;
      end;

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

          if (FMetaCounter = 2) then
            FFullTitleFound := True;
        end;

        if FSettings.SeparateTracks then
          if (Title <> FTitle) and (FRecordingTitleFound) then
          begin
            if (FAudioStream <> nil) and FSettings.SeparateTracks then
              FStreamTracks.FoundTitle(FAudioStream.Size, Title);
          end else if Title = FTitle then
          begin

          end else
          begin
            if (((FMetaCounter = 2) and FSettings.OnlySaveFull) or ((FMetaCounter = 1) and (not FSettings.OnlySaveFull))) then
            begin
              if (FAudioStream <> nil) and FSettings.SeparateTracks then
              begin
                FRecordingTitleFound := True;
                if FAudioStream.InheritsFrom(TAudioStreamMemory) then
                begin
                  // Stream sauber machen.
                  if FSettings.SearchSilence then
                    TAudioStreamMemory(FAudioStream).RemoveRange(0, FAudioStream.Size - (FBytesPerSec * FSettings.SilenceBufferSeconds))
                  else
                    TAudioStreamMemory(FAudioStream).RemoveRange(0, FAudioStream.Size - (FBytesPerSec * FSettings.SongBufferSeconds));
                end;
                FStreamTracks.FoundTitle(FAudioStream.Size, Title);
              end;
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

function TICEStream.GetFilename(var Dir: string; Name: string; Filesize: Int64): string;
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

  // TODO: krass testen. das mit dem überschreiben, und nicht überschreiben, dem anschließenden applien des filters, etc...
  // Zahl anhängen, wenn Datei existiert und
  // Filesize = 0 oder
  // keine kleineren überschreiben oder
  // nicht (kleinere überschreiben und neues größer als bestehendes File)
  if FileExists(Dir + Filename + Ext) then
    if (Filesize = 0) or (not FSettings.OverwriteSmaller) or (not (FSettings.OverwriteSmaller and (GetFileSize(Dir + Filename + Ext) < Filesize))) then
    begin
      Append := 1;
      while FileExists(Dir + Filename + ' (' + IntToStr(Append) + ')' + Ext) do
        Inc(Append);
      Filename := Filename + ' (' + IntToStr(Append) + ')' + Ext;
    end else
      Filename := Filename + Ext
  else
    Filename := Filename + Ext;

  Result := Dir + Filename;
end;

function TICEStream.GetFilenameTitle(StreamTitle: string; var Dir: string; Filesize: Int64): string;
var
  i, p: Integer;
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

  Replaced := PatternReplace(FSettings.FilePattern, Arr);



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



  Dir := IncludeTrailingBackslash(ExtractFilePath(FSaveDir + Replaced));
  Result := GetFilename(Dir, ExtractFileName(Replaced), Filesize);
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
