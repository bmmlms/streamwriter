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
  AppData, LanguageObjects;

type
  TDebugEvent = procedure(Text, Data: string) of object;
  TChunkReceivedEvent = procedure(Buf: Pointer; Len: Integer) of object;

  TAudioTypes = (atMPEG, atAAC, atNone);

  TICEStream = class(THTTPStream)
  private
    FMetaInt: Integer;
    FStreamName: string;
    FStreamURL: string;
    FBitRate: Cardinal;
    FGenre: string;

    FSeperateDirs: Boolean;
    FSkipShort: Boolean;

    FSaveDir: string;
    FShortSize: Integer;
    FSongBuffer: Integer;
    FMaxBufSize: Integer;

    FMetaCounter: Integer;
    FNextMetaOffset: Integer;
    FSaveFrom, FForwardLimit: Int64;

    FTitle: string;
    FSaveTitle: string;
    FSavedFilename: string;
    FSavedTitle: string;
    FSongsSaved: Integer;
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
    procedure SaveData(Title: string);
    procedure ProcessData;
    function GetFilename(var Dir: string; Station, Title: string; SeperateDirs: Boolean): string;
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
    property SongsSaved: Integer read FSongsSaved;
    property Filename: string read FFilename;

    property AudioType: TAudioTypes read FAudioType;

    //property MetaOnly: Boolean read FMetaOnly write FMetaOnly;
    property SeperateDirs: Boolean read FSeperateDirs write FSeperateDirs;
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

      Filename := GetFilename(Dir, '', FStreamName, False);
      ForceDirectories(Dir);

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
  FShortSize := AppGlobals.ShortSize;
  //FMaxBufSize := AppGlobals.MaxBufSize;
  FSongBuffer := AppGlobals.SongBuffer;
  AppGlobals.Unlock;
end;

function TICEStream.ExtractTitle(Title: string): string;
begin
  Result := Trim(Title);
end;

procedure TICEStream.SaveData(Title: string);
var
  p: Integer;
  RangeBegin, RangeEnd: Int64;
  Dir, Filename, Artist, Title2: string;
begin
  RangeBegin := FAudioStream.GetFrame(FSaveFrom, False);
  RangeEnd := FAudioStream.GetFrame(0, True);
  Dir := FSaveDir;

  if (RangeEnd <= -1) or (RangeBegin <= -1) then
    raise Exception.Create('Error in Audio data');

  if not (SkipShort and not (SaveSizeOkay(RangeEnd - RangeBegin))) then
  begin
    if Length(Title) > 0 then
    begin
      Filename := GetFilename(Dir, FStreamName, Title, SeperateDirs);
      WriteDebug('Saving title "' + Title + '"');
    end else
    begin
      Filename := GetFilename(Dir, FStreamName, '', SeperateDirs);
      WriteDebug('Saving unnamed Titel');
    end;

    try
      ForceDirectories(Dir);
      FAudioStream.SaveToFile(Filename, RangeBegin, RangeEnd - RangeBegin);

      FSavedFilename := Filename;
      FSavedTitle := Title;
      Inc(FSongsSaved);
      if Assigned(FOnSongSaved) then
        FOnSongSaved(Self);
    except
      on E: Exception do
      begin
        WriteDebug('Error wrhile saving to "' + Filename + '": ' + E.Message);
      end;
    end;
  end else
  begin
    WriteDebug('Skipping title "' + Title + '" because it''s too small (' + IntToStr(RangeEnd - RangeBegin) + ' bytes)');
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

  if FAudioStream.Size > 1048576 * FMaxBufSize then
  begin
    //WriteDebug('Saving because buffer is full');
    //SaveData(FTitle);
    //FAudioStream.Clear;
    //FForwardLimit := -1;
  end;

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
      SaveData(FSaveTitle);

      FSaveFrom := FAudioStream.Size - (FSongBuffer * 2) * 1024;
      if FSaveFrom < 0 then
        FSaveFrom := 0;
      //FAudioStream.RemoveRange(0, FAudioStream.Size - (FSongBuffer * 2) * 1024);

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
            WriteDebug('New title detected but saveoffset was set');
            SaveData(FSaveTitle);

            FSaveFrom := FAudioStream.Size - (FSongBuffer * 2) * 1024;
            if FSaveFrom < 0 then
              FSaveFrom := 0;
            //FAudioStream.RemoveRange(0, FAudioStream.Size - (FSongBuffer * 2) * 1024);

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
              //FAudioStream.RemoveRange(0, FAudioStream.Size - FSongBuffer * 1024);

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
            //FAudioStream.RemoveRange(0, FAudioStream.Size - FSongBuffer * 1024);

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
  //if TransferEncoding <> teNone then
  //  raise Exception.Create('No transfer-encoding support at this time.');
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

function TICEStream.GetFilename(var Dir: string; Station, Title: string; SeperateDirs: Boolean): string;
var
  Dir2, Filename, Ext: string;
  Append: Integer;
begin
  if SeperateDirs and (Length(Station) > 0) then
  begin
    Dir2 := GetValidFilename(Station);
    if Length(Dir2) > 0 then
    begin
      Dir := Dir + Dir2 + '\';
    end;
  end;

  if Title = '' then
  begin
    Title := _('[Unnamed title]');
  end;

  Filename := GetValidFilename(Title);

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
