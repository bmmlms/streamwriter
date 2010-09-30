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
  public
    destructor Destroy; override;

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

    FSaveDir: string;
    FFilePattern: string;
    FShortSize: Integer;
    FSongBuffer: Integer;

    FMetaCounter: Integer;
    FNextMetaOffset: Integer;

    //FSaveFrom, FForwardLimit: Int64;
    //FSongStart, FSongEnd: Int64;

    FTitle: string;
    //FSaveTitle: string;
    FSavedFilename: string;
    FSavedTitle: string;
    FSavedSize: UInt64;
    FFilename: string;

    FStreamTracks: TStreamTracks;

    FAudioStream: TAudioStreamFile;
    FAudioType: TAudioTypes;

    FOnTitleChanged: TNotifyEvent;
    FOnSongSaved: TNotifyEvent;
    FOnNeedSettings: TNotifyEvent;
    FOnChunkReceived: TChunkReceivedEvent;
    FOnIOError: TNotifyEvent;

    procedure DataReceived(CopySize: Integer);
    procedure SaveData(S, E: UInt64; Title: string);
    procedure TrySave;
    procedure ProcessData;
    function GetFilename(var Dir: string; Name: string): string;
    function GetFilenameTitle(StreamTitle: string; var Dir: string): string;
    function GetValidFilename(Name: string): string;
    procedure GetSettings;
  protected
    procedure DoHeaderRemoved; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process; override;

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

    property AudioType: TAudioTypes read FAudioType;

    property SkipShort: Boolean read FSkipShort write FSkipShort;

    property OnTitleChanged: TNotifyEvent read FOnTitleChanged write FOnTitleChanged;
    property OnSongSaved: TNotifyEvent read FOnSongSaved write FOnSongSaved;
    property OnNeedSettings: TNotifyEvent read FOnNeedSettings write FOnNeedSettings;
    property OnChunkReceived: TChunkReceivedEvent read FOnChunkReceived write FOnChunkReceived;
    property OnIOError: TNotifyEvent read FOnIOError write FOnIOError;
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
  FNextMetaOffset := -1;
  FBitRate := 0;
  FGenre := '';
  FTitle := '';
  //FSongStart := -1;
  //FSongEnd := -1;
  //FSaveTitle := '';
  FSavedFilename := '';
  FSavedTitle := '';
  FAudioType := atNone;
  FStreamTracks := TStreamTracks.Create;
end;

destructor TICEStream.Destroy;
begin
  FAudioStream.Free;
  FStreamTracks.Free;
  inherited;
end;

procedure TICEStream.DataReceived(CopySize: Integer);
var
  Buf: Pointer;
begin
  if FAudioStream <> nil then
    FAudioStream.CopyFrom(FGetRecvDataStream, CopySize);

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
  Dir, Filename: string;
begin
  inherited;
  GetSettings;
  if HeaderType = 'icy' then
  begin
    if ResponseCode = 200 then
    begin
      try
        FMetaInt := StrToInt(GetHeaderData('icy-metaint'));
        FNextMetaOffset := FMetaInt;
      except
        WriteDebug('Meta-interval could not be found');
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
        raise Exception.Create('Unknown content-type');

      Filename := GetFilename(Dir, FStreamName);
      try
        ForceDirectories(Dir);
      except
        if Assigned(FOnIOError) then
          FOnIOError(Self);
        raise Exception.Create('Folder for saved tracks could not be created.');
      end;

      try
        WriteDebug('Saving stream to "' + Filename + '"');
        case FAudioType of
          atMPEG:
            FAudioStream := TMPEGStreamFile.Create(Filename, fmCreate or fmShareDenyWrite);
          atAAC:
            FAudioStream := TAACStreamFile.Create(Filename, fmCreate or fmShareDenyWrite)
        end;
      except
        if Assigned(FOnIOError) then
          FOnIOError(Self);
        raise Exception.Create('Could not open "' + Filename + '"');
      end;

      FFilename := Filename;

      if Assigned(FOnTitleChanged) then
        FOnTitleChanged(Self);
    end else
      raise Exception.Create(Format('Invalid responsecode (%d)', [ResponseCode]));
  end else if HeaderType = 'http' then
  begin
    // Wenn wir hier drin sind, müsste es eigentlich eine Playlist sein.
    // Es gibt aber Stationen (z.B. http://stream.laut.fm/disco), die
    // bei einem ICY-Stream einen HTTP-Header liefern...
    if Pos(#10'icy-metaint:', LowerCase(FHeader)) > 0 then
    begin
      WriteDebug('HTTP header detected but icy fields found - treating as icy header');
      FHeaderType := 'icy';
      DoHeaderRemoved;
    end;
  end else
    raise Exception.Create('Unknown header-type');
end;

procedure TICEStream.GetSettings;
begin
  if Assigned(FOnNeedSettings) then
    FOnNeedSettings(Self);
  AppGlobals.Lock;
  FSaveDir := AppGlobals.Dir;
  FFilePattern := AppGlobals.FilePattern;
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
  OldPos: Int64;
  RangeBegin, RangeEnd: Int64;
  Dir, Filename: string;
  MemStream: TAudioStreamMemory;
  P: TPosRect;
begin
  Saved := False;

  RangeBegin := -1;
  RangeEnd := -1;

  RangeBegin := FAudioStream.GetFrame(S, False);
  RangeEnd := FAudioStream.GetFrame(E, True);

  WriteDebug(Format('Saving from %d to %d', [S, E]));

  if (RangeEnd <= -1) or (RangeBegin <= -1) then
    raise Exception.Create('Error in audio data');

  if SkipShort and (RangeEnd - RangeBegin < FShortSize) then
  begin
    // TODO: Klappt das hier? wirklich testen. glaub mir.
    WriteDebug(Format('Skipping title "%s" because it''s too small (%d bytes)', [Title, RangeEnd - RangeBegin]));
    Exit;
  end;

  Inc(FSongsSaved);
  try
    Filename := GetFilenameTitle(Title, Dir);

    if Length(Title) > 0 then
      WriteDebug(Format('Saving title "%s"', [Title]))
    else
      WriteDebug('Saving unnamed title');

    try
      ForceDirectories(Dir);
    except
      raise Exception.Create('Folder for saved tracks could not be created.');
    end;

    FAudioStream.SaveToFile(Filename, RangeBegin, RangeEnd - RangeBegin);
    Saved := True;

    FSavedFilename := Filename;
    FSavedTitle := Title;
    FSavedSize := RangeEnd - RangeBegin;
    if Assigned(FOnSongSaved) then
      FOnSongSaved(Self);
  except
    on E: Exception do
    begin
      if not Saved then
        Dec(FSongsSaved);
      WriteDebug(Format('Error while saving to "%s": %s', [Filename, E.Message]));
    end;
  end;
end;

procedure TICEStream.TrySave;
var
  TitleChanged: Boolean;
  MetaLen, P: Integer;
  Title, MetaData: string;
  Buf: Byte;
  WD, WD2: TWaveData;
  M1, M2: TMPEGStreamMemory;
  R: TPosRect;
  i: Integer;
  Track: TStreamTrack;
begin
  for i := FStreamTracks.Count - 1 downto 0 do
  begin
    Track := FStreamTracks[i];

    if (Track.S > -1) and (Track.E > -1) then
    begin
      if FSearchSilence and (FAudioStream is TMPEGStreamFile) then
      begin
        if FAudioStream.Size > Track.E + SILENCE_SEARCH_BUFFER then
        begin
          WriteDebug('Searching for silence...');

          R := FAudioStream.SearchSilence(Track.S, Track.E, SILENCE_SEARCH_BUFFER, FSilenceLevel, FSilenceLength);

          if (R.A > -1) or (R.B > -1) then
          begin
            if R.A = -1 then
            begin
              WriteDebug('No silence at SongStart could be found, using configured buffer');
              R.A := Track.S - FSongBuffer * 1024;
              if R.A < FAudioStream.Size then
                R.A := 0;
            end else
              WriteDebug('Silence at SongStart found');

            if R.B = -1 then
            begin
              WriteDebug('No silence at SongEnd could be found');
              R.B := Track.E + FSongBuffer * 1024;
              if R.B > FAudioStream.Size then
              begin
                WriteDebug('Stream is too small, waiting for more data...');
                Exit;
              end else
              begin
                WriteDebug('Using configured buffer...');
              end;
            end else
              WriteDebug('Silence at SongEnd found');

            WriteDebug(Format('Scanned song start/end: %d/%d', [R.A, R.B]));

            SaveData(R.A, R.B, Track.Title);

            Track.Free;
            FStreamTracks.Delete(i);
            WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]));
          end else
          begin
            if (FAudioStream.Size >= Track.S + (Track.E - Track.S) + ((FSongBuffer * 2) * 1024)) and
               (FAudioStream.Size > Track.E + FSongBuffer * 1024) then
            begin
              WriteDebug(Format('No silence found, saving using buffer...', []));

              // TODO: Alle kombinationen testen. silence detection an/aus, buffer gesetzt 0 oder mehr,
              // wird in beiden situationen unter allen umständen gespeichert, etc... alles testen eben!

              SaveData(Track.S - FSongBuffer * 1024, Track.E + FSongBuffer * 1024, Track.Title);

              Track.Free;
              FStreamTracks.Delete(i);
              WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]));
            end else
              WriteDebug('Waiting for full buffer because no silence found...');
          end;
        end else
        begin
          WriteDebug(Format('Waiting to save "%s" because stream is too small', [Track.Title]));
        end;
      end else
      begin
        if (FAudioStream.Size >= Track.S + (Track.E - Track.S) + ((FSongBuffer * 2) * 1024)) and
           (FAudioStream.Size > Track.E + FSongBuffer * 1024) then
        begin
          if FSearchSilence and (not (FAudioStream is TMPEGStreamFile)) then
            WriteDebug('Saving using buffer because stream is not mpeg...')
          else
            WriteDebug('Saving using buffer...');

          SaveData(Track.S - FSongBuffer * 1024, Track.E + FSongBuffer * 1024, Track.Title);

          Track.Free;
          FStreamTracks.Delete(i);
          WriteDebug(Format('Tracklist count is %d', [FStreamTracks.Count]));
        end else
          WriteDebug('Waiting for full buffer...');
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
  WD, WD2: TWaveData;
  M1, M2: TMPEGStreamMemory;
  R: TPosRect;
  i: Integer;
begin
  Seek(0, soFromBeginning);
  if FMetaInt = -1 then
  begin
    FAudioStream.Seek(0, soFromEnd);
    DataReceived(Size);
    Clear;
  end else
  begin
    TitleChanged := False;
    while Position < Size - FMetaInt - 4081 do // 4081 wegen 255*16+1 (Max-MetaLen)
    begin
      FAudioStream.Seek(0, soFromEnd);

      DataReceived(FMetaInt);

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
          WriteDebug(Format('Track changed, title "%s" now playing', [Title]));
          TitleChanged := True;
          Inc(FMetaCounter);
        end;

        if (Title <> FTitle) and (FMetaCounter >= 3) then
        begin
          WriteDebug(Format('Adding title "%s" to list', [Title]));
          FStreamTracks.FoundTitle(FAudioStream.Size, Title);
        end else if Title = FTitle then
        begin

        end else
        begin
          if FMetaCounter = 2 then
          begin
            FStreamTracks.FoundTitle(FAudioStream.Size, Title);
            WriteDebug(Format('Start of first full song "%s" detected, adding it to list', [Title]));
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
    if (HeaderRemoved) and (Size > 100000) then
    begin
      GetSettings;
      ProcessData;
    end;
  end else if HeaderType = 'http' then
  begin
    if Size > 512000 then
      raise Exception.Create('Too many bytes in HTTP-response')
  end else if HeaderRemoved then
    raise Exception.Create('Unknown header-type');
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

  SetLength(Arr, 4);
  Arr[0].C := 'a';
  Arr[0].Replace := Artist;
  Arr[1].C := 't';
  Arr[1].Replace := Title;
  Arr[2].C := 's';
  Arr[2].Replace := StreamName;
  Arr[3].C := 'n';
  Arr[3].Replace := IntToStr(FSongsSaved);

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

destructor TStreamTracks.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TStreamTrack(Items[i]).Free;
  inherited;
end;

procedure TStreamTracks.FoundTitle(Offset: Int64; Title: string);
begin
  if Count > 0 then
  begin
    Items[Count - 1].E := Offset;
  end;
  Add(TStreamTrack.Create(Offset, -1, Title));
end;

end.
