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
  AppData, LanguageObjects, Functions, DynBASS;

type
  TDebugEvent = procedure(Text, Data: string) of object;
  TChunkReceivedEvent = procedure(Buf: Pointer; Len: Integer) of object;

  TAudioTypes = (atMPEG, atAAC, atNone);

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
    FSaveFrom, FForwardLimit: Int64;

    FTitle: string;
    FSaveTitle: string;
    FSavedFilename: string;
    FSavedTitle: string;
    FFilename: string;

    FAudioStream: TAudioStreamFile;
    FAudioType: TAudioTypes;

    FOnTitleChanged: TNotifyEvent;
    FOnSongSaved: TNotifyEvent;
    FOnNeedSettings: TNotifyEvent;
    FOnChunkReceived: TChunkReceivedEvent;
    FOnIOError: TNotifyEvent;

    procedure DataReceived(CopySize: Integer);
    function ExtractTitle(Title: string): string;
    procedure SaveData;
    procedure ProcessData;
    function GetFilename(var Dir: string; Name: string): string;
    function GetFilenameTitle(var Dir: string): string;
    function GetValidFilename(Name: string): string;
    function SaveSizeOkay(Size: Integer): Boolean;
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
  FSaveFrom := 0;
  FForwardLimit := -1;
  FSaveTitle := '';
  FSavedFilename := '';
  FSavedTitle := '';
  FAudioType := atNone;
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

destructor TICEStream.Destroy;
begin
  FAudioStream.Free;
  inherited;
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
      raise Exception.Create('Invalid responsecode (' + IntToStr(ResponseCode) + ')');
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

function TICEStream.ExtractTitle(Title: string): string;
begin
  Result := Trim(Title);
end;

procedure TICEStream.SaveData;
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

  RangeBegin := FAudioStream.GetFrame(FSaveFrom, False);
  RangeEnd := FAudioStream.GetFrame(FAudioStream.Size, True);

  WriteDebug(Format('FSaveFrom, Begin, End: %d / %d / %d', [FSaveFrom, RangeBegin, RangeEnd]));

  // Eventuell nach Stille suchen
  if FSearchSilence then
  begin
    // TODO: Jede "silence" muss eine mindestlänge haben.
    // TODO: Nur, wenn nicht >20mb oder so.... weil ich extra in memory kopiere!
    if FAudioStream is TMPEGStreamFile then
    begin
      if BassLoaded then
      begin
        WriteDebug('Searching for silence...');

        MemStream := TMPEGStreamMemory.Create;
        try
          // Daten in MemoryStream kopieren
          OldPos := FAudioStream.Position;
          FAudioStream.Seek(RangeBegin, soFromBeginning);
          MemStream.CopyFrom(FAudioStream, RangeEnd - RangeBegin);
          FAudioStream.Seek(OldPos, soFromBeginning);

          //P := MemStream.GetPossibleTitle(FSongBuffer);
          P := MemStream.GetPossibleTitle(FSongBuffer); // TODO: Das ist FAiL. weil das ende könnte auch vor title change sein. also irgendwas substrahieren.

          if (P.A > 0) or (P.B > 0) then
          begin
            RangeBegin := P.A + RangeBegin;
            RangeEnd := P.B + RangeBegin;

            WriteDebug(Format('Silence found, track is from %d to %d', [RangeBegin, RangeEnd]));

            RangeBegin := FAudioStream.GetFrame(RangeBegin, False);
            RangeEnd := FAudioStream.GetFrame(RangeEnd, True);

            WriteDebug(Format('Found MPEG headers from %d to %d', [RangeBegin, RangeEnd]));
          end;
        finally
          MemStream.Free;
        end;
      end else
        WriteDebug('Cannot search for silence because bass library was not loaded.');
    end;
  end;

  if (RangeEnd <= -1) or (RangeBegin <= -1) then
    raise Exception.Create('Error in audio data');

  if not (SkipShort and not (SaveSizeOkay(RangeEnd - RangeBegin))) then
  begin
    // Muss hier, damit das in GetFilenameTitle() fürs Dateinamensmuster da ist
    Inc(FSongsSaved);

    try
      Filename := GetFilenameTitle(Dir);

      if Length(FSaveTitle) > 0 then
        WriteDebug('Saving title "' + FSaveTitle + '"')
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
      FSavedTitle := FSaveTitle;
      if Assigned(FOnSongSaved) then
        FOnSongSaved(Self);
    except
      on E: Exception do
      begin
        if not Saved then
          Dec(FSongsSaved);
        WriteDebug('Error while saving to "' + Filename + '": ' + E.Message);
      end;
    end;
  end else
  begin
    WriteDebug('Skipping title "' + FSaveTitle + '" because it''s too small (' + IntToStr(RangeEnd - RangeBegin) + ' bytes)');
  end;
end;

function TICEStream.SaveSizeOkay(Size: Integer): Boolean;
begin
  Result := Size > FShortSize * 1024 - (FSongBuffer * 1024) * 2;
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
    FAudioStream.Seek(0, soFromEnd);
    DataReceived(Size);
    Clear;
  end else
  begin
    if (FForwardLimit > -1) and (FForwardLimit + FMetaInt <= FAudioStream.Size) then
    begin
      WriteDebug('Saving "' + FSaveTitle + '" because saveoffset reached');
      SaveData;

      // TODO: FSaveFrom muss passend gemacht werden auf das, wo SaveData geschnitten hat => SaveData braucht anderen Rückgabetyp
      FSaveFrom := FAudioStream.Size - (FSongBuffer * 2) * 1024;
      if FSaveFrom < 0 then
        FSaveFrom := 0;

      WriteDebug('New starting offset: ' + IntToStr(FSaveFrom) + ' bytes, buffersize is ' + IntToStr(FAudioStream.Size));
      FForwardLimit := -1;
    end;

    TitleChanged := False;
    while Position < Size - FMetaInt - 4081 do // 4081 wegen 255*16+1 (Max-MetaLen)
    begin
      FAudioStream.Seek(0, soFromEnd);
      DataReceived(FMetaInt);

      Read(Buf, 1);
      if Buf > 0 then
      begin
        MetaLen := Buf * 16;

        MetaData := Trim(string(ToString(Position, MetaLen)));
        Seek(MetaLen, soFromCurrent);
        P := PosEx(''';', MetaData, 14);
        Title := ExtractTitle(Copy(MetaData, 14, P - 14));

        //if (Length(Title) = 0) and MetaOnly then
        //  raise Exception.Create('Empty title in metadata received');

        if Title <> FTitle then
        begin
          WriteDebug('Title "' + Title + '" now playing');
          TitleChanged := True;
          Inc(FMetaCounter);
        end;

        if (Title <> FTitle) and (FMetaCounter >= 3) then
        begin
          if FForwardLimit > -1 then
          begin
            WriteDebug('Saving because new title detected while set saveoffset');
            SaveData;

            // TODO: FSaveFrom muss passend gemacht werden auf das, wo SaveData geschnitten hat => SaveData braucht anderen Rückgabetyp
            FSaveFrom := FAudioStream.Size - (FSongBuffer * 2) * 1024;
            if FSaveFrom < 0 then
              FSaveFrom := 0;

            WriteDebug('New starting offset: ' + IntToStr(FSaveFrom) + ' bytes, buffersize is ' + IntToStr(FAudioStream.Size));
            FForwardLimit := -1;
          end else
          begin
            if not SaveSizeOkay(FAudioStream.Size - FSaveFrom + FSongBuffer) then
            begin
              if FSaveTitle <> '' then
                WriteDebug('New title detected but not enough data received to save "' + FSaveTitle + '"')
              else
                WriteDebug('New title detected but not enough data received to save the previous title');

              FSaveFrom := FAudioStream.Size - FSongBuffer * 1024;
              if FSaveFrom < 0 then
                FSaveFrom := 0;

              WriteDebug('New starting offset: ' + IntToStr(FSaveFrom) + ' bytes, buffersize is ' + IntToStr(FAudioStream.Size));
            end else
            begin
              WriteDebug('New title detected, will save "' + FTitle + '" at ' + IntToStr(FAudioStream.Size + FSongBuffer * 1024) + ' bytes');
              FForwardLimit := FAudioStream.Size + FSongBuffer * 1024;
              FSaveTitle := FTitle;
            end;
          end;
        end else if Title = FTitle then
        begin

        end else
        begin
          if FMetaCounter = 2 then
          begin
            FSaveFrom := FAudioStream.Size - FSongBuffer * 1024;
            if FSaveFrom < 0 then
              FSaveFrom := 0;

            WriteDebug('Start of first full song "' + Title + '" detected');
            WriteDebug('New starting offset: ' + IntToStr(FSaveFrom) + ' bytes, buffersize is ' + IntToStr(FAudioStream.Size));
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
      raise Exception.Create('Fehler');
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

function TICEStream.GetFilenameTitle(var Dir: string): string;
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
  SaveTitle := GetValidFileName(Trim(FSaveTitle));

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

end.
