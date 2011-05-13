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
unit DataManager;

interface

uses
  Windows, Classes, SysUtils, ExtendedStream, Generics.Collections,
  ComCtrls, AppData, Functions, Logging;

type
  TStreamList = class;
  TDataLists = class;
  TListCategory = class;

  EVersionException = class(Exception);

  TTrackInfo = class
  private
    FTime: TDateTime;
    FFilename: string;
    FStreamname: string;
    FFilesize: UInt64;
    FLength: UInt64;
    FWasCut: Boolean;
    FBitRate: Cardinal;
    FIsAuto: Boolean;
  public
    constructor Create; overload;
    constructor Create(Time: TDateTime; Filename, Streamname: string); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TTrackInfo;
    procedure Save(Stream: TExtendedStream);

    property Time: TDateTime read FTime;
    property Filename: string read FFilename write FFilename;
    property Streamname: string read FStreamname write FStreamname;
    property Filesize: UInt64 read FFilesize write FFilesize;
    property Length: UInt64 read FLength write FLength;
    property WasCut: Boolean read FWasCut write FWasCut;
    property BitRate: Cardinal read FBitRate write FBitRate;
    property IsAuto: Boolean read FIsAuto write FIsAuto;
  end;

  TTitleInfo = class
  private
    FTitle: string;
    FPattern: string;
    FHash: Cardinal;
  public
    constructor Create(Title: string); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TTitleInfo;
    procedure Save(Stream: TExtendedStream);

    property Title: string read FTitle;
    property Pattern: string read FPattern;
    property Hash: Cardinal read FHash;
  end;

  TListCategoryList = TList<TListCategory>;

  TListCategory = class
  private
    FName: string;
    FIndex: Integer;
    FExpanded: Boolean;
    FKilled: Boolean;
    FIsAuto: Boolean;
  public
    constructor Create(Name: string; Idx: Integer); overload;
    class function Load(Stream: TExtendedStream; Version: Integer): TListCategory;
    procedure Save(Stream: TExtendedStream);
    property Name: string read FName write FName;
    property Index: Integer read FIndex write FIndex;
    property Expanded: Boolean read FExpanded write FExpanded;
    property Killed: Boolean read FKilled write FKilled;
    property IsAuto: Boolean read FIsAuto write FIsAuto;
  end;

  TTrackList = class(TList<TTrackInfo>)
  public
    procedure RemoveTrack(Track: TTrackInfo);
  end;

  TRecentEntry = class
  private
    FName: string;
    FStartURL: string;
    FIndex: Cardinal;
  public
    constructor Create(Name, StartURL: string; Index: Cardinal);

    procedure Assign(From: TRecentEntry);
    function Copy: TRecentEntry;
    class function Load(Stream: TExtendedStream; Version: Integer): TRecentEntry;
    procedure Save(Stream: TExtendedStream);

    property Name: string read FName write FName;
    property StartURL: string read FStartURL write FStartURL;
    property Index: Cardinal read FIndex write FIndex;
  end;

  TStreamEntry = class(TObject)
  private
    FSettings: TStreamSettings;

    FParent: TStreamList;

    FName: string;
    FStreamURL: string;
    FStartURL: string;
    FURLs: TStringList;
    FBitrate: Cardinal;
    FAudioType: string;
    FGenre: string;
    FIndex: Integer;
    FCategoryIndex: Integer;
    FWasRecording: Boolean;

    // REMARK: IsInList kann raus. Ist für Updates von Versionen < 6 da. Das Feld ist über,
    // weil in neueren Versionen alles IsInList ist.
    FIsInList: Boolean;
    FSongsSaved: Cardinal;
    FBytesReceived: UInt64;

    FMigrationSubmitted: Boolean;
    FMigrationTrackList: TTrackList;
    FMigrationRecentIndex: Integer;

    procedure FSetName(Value: string);
    procedure FSetIsInList(Value: Boolean);

    procedure FSetGenre(Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(From: TStreamEntry);
    function Copy: TStreamEntry;
    class function Load(Stream: TExtendedStream; Version: Integer): TStreamEntry;
    procedure Save(Stream: TExtendedStream);

    property Settings: TStreamSettings read FSettings;

    property Parent: TStreamList read FParent write FParent;
    property Name: string read FName write FSetName;
    property StreamURL: string read FStreamURL write FStreamURL;
    property StartURL: string read FStartURL write FStartURL;
    property URLs: TStringList read FURLs;
    property Bitrate: Cardinal read FBitrate write FBitrate;
    property AudioType: string read FAudioType write FAudioType;
    property Genre: string read FGenre write FSetGenre;
    property Index: Integer read FIndex write FIndex;
    property CategoryIndex: Integer read FCategoryIndex write FCategoryIndex;
    property WasRecording: Boolean read FWasRecording write FWasRecording;

    property IsInList: Boolean read FIsInList write FSetIsInList;
    property SongsSaved: Cardinal read FSongsSaved write FSongsSaved;
    property BytesReceived: UInt64 read FBytesReceived write FBytesReceived;
  end;

  TStreamList = class(TList<TStreamEntry>)
  private
  public
    //function Add(Name: string; URL: string; URLs: TStringList; BitRate: Cardinal; Genre: string;
    //  SkipShort: Boolean; UseFilter: TUseFilters; SongsSaved: Cardinal): TStreamEntry; overload;
    function Add(Entry: TStreamEntry): TStreamEntry; overload;
    function Get(Name, URL: string; URLs: TStringList): TStreamEntry; overload;
  end;

  TTitleList = class(TList<TTitleInfo>)
  end;

  TRecentList = class(TList<TRecentEntry>)
  end;

  TDataLists = class
  private
    FCategoryList: TListCategoryList;
    FStreamList: TStreamList;
    FTrackList: TTrackList;
    FSaveList: TTitleList;
    FIgnoreList: TTitleList;
    FSubmittedStreamList: TStringList;
    FRecentList: TRecentList;
    FStreamBlacklist: TStringList;
    FLoadError: Boolean;
    FReceived: UInt64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property CategoryList: TListCategoryList read FCategoryList;
    property StreamList: TStreamList read FStreamList;
    property TrackList: TTrackList read FTrackList;
    property SaveList: TTitleList read FSaveList;
    property IgnoreList: TTitleList read FIgnoreList;
    property SubmittedStreamList: TStringList read FSubmittedStreamList;
    property RecentList: TRecentList read FRecentList;
    property StreamBlacklist: TStringList read FStreamBlacklist;

    property LoadError: Boolean read FLoadError write FLoadError;
    property Received: UInt64 read FReceived write FReceived;
  end;

const
  DATAVERSION = 17;

implementation

{ TStreamEntry }

procedure TStreamEntry.Assign(From: TStreamEntry);
begin
  FName := From.FName;
  FStreamURL := From.FStreamURL;
  FIsInList := From.FIsInList;
  FStartURL := From.FStartURL;
  FSongsSaved := From.FSongsSaved;
  FBytesReceived := From.FBytesReceived;
  FIndex := From.FIndex;
  FCategoryIndex := From.CategoryIndex;
  FBitRate := From.BitRate;
  FAudioType := From.AudioType;
  FGenre := From.Genre;
  FWasRecording := From.WasRecording;
  FURLs.Assign(From.FURLs);
  FSettings.Assign(From.FSettings);
end;

function TStreamEntry.Copy: TStreamEntry;
begin
  Result := TStreamEntry.Create;
  Result.Assign(Self);
end;

constructor TStreamEntry.Create;
begin
  FSettings := TStreamSettings.Create;

  FParent := Parent;
  FURLs := TStringList.Create;
  FIsInList := False;
  FSongsSaved := 0;
  FMigrationTrackList := TTrackList.Create;
end;

destructor TStreamEntry.Destroy;
begin
  FURLs.Free;
  FMigrationTrackList.Free;
  FSettings.Free;
  inherited;
end;

procedure TStreamEntry.FSetName(Value: string);
begin
  FName := Value;
end;

class function TStreamEntry.Load(Stream: TExtendedStream; Version: Integer): TStreamEntry;
var
  BTmp: Boolean;
  B: Byte;
  i: Integer;
  Count: Cardinal;
  URL: string;
  TrackInfo: TTrackInfo;
  DTTmp: TDateTime;
begin
  Result := TStreamEntry.Create;

  if Version >= 6 then
  begin
    Result.FSettings.Free;
    Result.FSettings := TStreamSettings.Load(Stream, Version);

    // Das hier sollte eigentlich in TStreamSettings.Load. Der Compiler scheint kaputt zu sein und meint
    // in .Load, dass es kein Result.FRetryDelay gibt... Ich versuche das mal, hier zu machen...
    if Result.FSettings.RetryDelay > 10 then
      Result.FSettings.RetryDelay := 10;
  end else
  begin
    // Defaults benutzen..
    Result.FSettings.Assign(AppGlobals.StreamSettings);
  end;

  Stream.Read(Result.FName);
  if Version >= 8 then
    Stream.Read(Result.FStreamURL);
  Stream.Read(Result.FStartURL);
  Stream.Read(Count);
  for i := 0 to Count - 1 do
  begin
    Stream.Read(URL);
    Result.FURLs.Add(URL);
  end;

  Stream.Read(Result.FBitrate);

  if Version > 10 then
    Stream.Read(Result.FAudioType);

  Stream.Read(Result.FGenre);

  if Version <= 5 then
  begin
    Stream.Read(BTmp);
    Result.FSettings.SkipShort := BTmp;

    if (Version >= 3) then
    begin
      Stream.Read(B);
      Result.FSettings.Filter := TUseFilters(B);
    end;

    Stream.Read(Result.FMigrationSubmitted);
  end;

  Stream.Read(Result.FIsInList);
  if Version >= 5 then
  begin
    Stream.Read(Result.FIndex);
    Stream.Read(Result.FCategoryIndex);
  end;

  if Version < 6 then
    Stream.Read(Result.FMigrationRecentIndex);

  if Version <= 5 then
    Stream.Read(DTTmp);

  if Version <= 5 then
  begin
    Stream.Read(Count);
    for i := 0 to Count - 1 do
    begin
      TrackInfo := TTrackInfo.Create(Now, '', '');
      Stream.Read(TrackInfo.FTime);
      Stream.Read(TrackInfo.FFilename);
      TrackInfo.Streamname := Result.FName;
      if Version > 1 then
      begin
        Stream.Read(TrackInfo.FFilesize);
        Stream.Read(TrackInfo.FWasCut);
      end;
      Result.FMigrationTrackList.Add(TrackInfo);
    end;
  end;
  Stream.Read(Result.FSongsSaved);
  Stream.Read(Result.FBytesReceived);

  if Version >= 17 then
  begin
    Stream.Read(Result.FWasRecording);
  end;
end;

procedure TStreamEntry.Save(Stream: TExtendedStream);
var
  i: Integer;
begin
  FSettings.Save(Stream);

  Stream.Write(FName);
  Stream.Write(FStreamURL);
  Stream.Write(FStartURL);
  Stream.Write(FURLs.Count);
  for i := 0 to FURLs.Count - 1 do
  begin
    Stream.Write(FURLs[i]);
  end;
  Stream.Write(FBitRate);
  Stream.Write(FAudioType);
  Stream.Write(FGenre);

  Stream.Write(FIsInList);
  Stream.Write(FIndex);
  Stream.Write(FCategoryIndex);

  Stream.Write(FSongsSaved);
  Stream.Write(FBytesReceived);

  Stream.Write(FWasRecording);
end;

procedure TStreamEntry.FSetGenre(Value: string);
begin
  FGenre := Value;
end;

procedure TStreamEntry.FSetIsInList(Value: Boolean);
begin
  FIsInList := Value;
end;

{
procedure TStreamEntry.FSetRecentIndex(Value: Integer);
  function RemoveOld: Boolean;
  var
    i: Integer;
    Greatest, GreatestIndex, RecentCount: Integer;
  begin
    Result := False;
    Greatest := -1;
    GreatestIndex := -1;
    RecentCount := 0;
    for i := 0 to FParent.Count - 1 do
    begin
      if FParent[i].RecentIndex > -1 then
        Inc(RecentCount);
      if FParent[i].RecentIndex > GreatestIndex then
      begin
        Greatest := i;
        GreatestIndex := FParent[i].RecentIndex;
      end;
    end;

    if RecentCount > 15 then
    begin
      FParent[Greatest].RecentIndex := -1;
      Result := True;
    end;
  end;
var
  i: Integer;
  HasZero: Boolean;
begin
  FRecentIndex := Value;
  if (FParent <> nil) and (Value = 0) then
  begin
    HasZero := False;
    for i := 0 to FParent.Count - 1 do
    begin
      if (FParent[i].RecentIndex = 0) and (FParent[i] <> Self) then
      begin
        HasZero := True;
        Break;
      end;
    end;

    if HasZero then
      for i := 0 to FParent.Count - 1 do
      begin
        if FParent[i].FRecentIndex > -1 then
          FParent[i].FRecentIndex := FParent[i].FRecentIndex + 1;
      end;

    while RemoveOld do
      RemoveOld;
  end;
end;
}

{ TStreamDataList }

constructor TDataLists.Create;
begin
  inherited;

  FLoadError := False;
  FReceived := 0;
  FCategoryList := TListCategoryList.Create;
  FStreamList := TStreamList.Create;
  FTrackList := TTrackList.Create;
  FSaveList := TTitleList.Create;
  FIgnoreList := TTitleList.Create;
  FSubmittedStreamList := TStringList.Create;
  FRecentList := TRecentList.Create;
  FStreamBlacklist := TStringList.Create;
end;

destructor TDataLists.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCategoryList.Count - 1 do
    FCategoryList[i].Free;
  FCategoryList.Free;

  for i := 0 to FTrackList.Count - 1 do
    FTrackList[i].Free;
  FTrackList.Free;

  for i := 0 to FStreamList.Count - 1 do
    FStreamList[i].Free;
  FStreamList.Free;

  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Free;
  FSaveList.Free;

  for i := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[i].Free;
  FIgnoreList.Free;

  FSubmittedStreamList.Free;

  for i := 0 to FRecentList.Count - 1 do
    FRecentList[i].Free;
  FRecentList.Free;

  FStreamBlackList.Free;

  inherited;
end;

procedure TDataLists.Load;
var
  Entry: TStreamEntry;
  TitleInfo: TTitleInfo;
  TrackInfo: TTrackInfo;
  S: TExtendedStream;
  Str: string;
  Version, CatCount, EntryCount: Integer;
  i, n: Integer;
begin
  if AppGlobals.DataFile = '' then
    Exit;

  S := TExtendedStream.Create;
  try
    try
      S.LoadFromFile(AppGlobals.DataFile);
    except
      Exit;
    end;

    try
      S.Read(Version);

      if Version > DATAVERSION then
        raise EVersionException.Create(AppGlobals.DataFile);

      S.Read(FReceived);

      if Version <= 2 then
      begin
        while S.Position < S.Size do
        begin
          Entry := TStreamEntry.Load(S, Version);
          Entry.FParent := FStreamList;
          FStreamList.Add(Entry);
        end;
      end else
      begin
        if Version >= 5 then
        begin
          S.Read(CatCount);
          for i := 0 to CatCount - 1 do
          begin
            FCategoryList.Add(TListCategory.Load(S, Version));
          end;
        end;

        S.Read(EntryCount);
        for i := 0 to EntryCount - 1 do
        begin
          Entry := TStreamEntry.Load(S, Version);
          Entry.FParent := FStreamList;
          FStreamList.Add(Entry);
        end;

        if Version >= 6 then
        begin
          S.Read(EntryCount);
          for i := 0 to EntryCount - 1 do
          begin
            TrackInfo := TTrackInfo.Load(S, Version);
            FTrackList.Add(TrackInfo);
          end;
        end;

        if Version >= 3 then
        begin
          S.Read(EntryCount);
          for i := 0 to EntryCount - 1 do
          begin
            TitleInfo := TTitleInfo.Load(S, Version);
            if TitleInfo <> nil then
              FSaveList.Add(TitleInfo);
          end;
          S.Read(EntryCount);
          for i := 0 to EntryCount - 1 do
          begin
            TitleInfo := TTitleInfo.Load(S, Version);
            if TitleInfo <> nil then
              FIgnoreList.Add(TitleInfo);
          end;

          if Version >= 6 then
          begin
            S.Read(EntryCount);
            for i := 0 to EntryCount - 1 do
            begin
              S.Read(Str);
              FSubmittedStreamList.Add(Str);
            end;

            S.Read(EntryCount);
            for i := 0 to EntryCount - 1 do
              FRecentList.Add(TRecentEntry.Load(S, Version));

            if Version >= 15 then
            begin
              S.Read(EntryCount);
              for i := 0 to EntryCount - 1 do
              begin
                S.Read(Str);
                FStreamBlacklist.Add(Str);
              end;
            end;
          end;

          // REMARK: Irgendwann raus. Fehler in Version 3... doppelte entfernen!
          if Version = 3 then
          begin
            i := 0;
            while True do
            begin
              for n := FSaveList.Count - 1 downto i + 1 do
                if FSaveList[n].Hash = FSaveList[i].Hash then
                begin
                  FSaveList[n].Free;
                  FSaveList.Delete(n);
                end;
              Inc(i);
              if i > FSaveList.Count - 1 then
                Break;
            end;

            i := 0;
            while True do
            begin
              for n := FIgnoreList.Count - 1 downto i + 1 do
                if FIgnoreList[n].Hash = FIgnoreList[i].Hash then
                begin
                  FIgnoreList[n].Free;
                  FIgnoreList.Delete(n);
                end;
              Inc(i);
              if i > FIgnoreList.Count - 1 then
                Break;
            end;
          end;

        end;
      end;

      if Version < 6 then
      begin
        for i := FStreamList.Count - 1 downto 0 do
        begin
          Entry := FStreamList[i];
          // REMARK: Für das Update von 5 auf 6: Alle gespeicherten Tracks von
          // den Entries in die TrackList übernehmen. Kann irgendwann weg.
          // Natürlich auch das alte FSubmitted übernehmen.
          for n := 0 to Entry.FMigrationTrackList.Count - 1 do
          begin
            FTrackList.Add(Entry.FMigrationTrackList[n]);
          end;
          Entry.FMigrationTrackList.Clear;
          if Entry.FMigrationSubmitted then
            FSubmittedStreamList.Add(Entry.StartURL);
          if Entry.FMigrationRecentIndex > -1 then
            FRecentList.Add(TRecentEntry.Create(Entry.Name, Entry.StartURL, Entry.FMigrationRecentIndex));

          if not Entry.IsInList then
          begin
            FStreamList.Remove(Entry);
            Entry.Free;
          end;
        end;
      end;
    except
      on E: EVersionException do
      begin
        FReceived := 0;
        FLoadError := True;
        raise;
      end;
      on E: Exception do
      begin
        FReceived := 0;
        FLoadError := True;
        raise Exception.Create(AppGlobals.DataFile);
      end;
    end;
  finally
    S.Free;
  end;
end;

procedure TDataLists.Save;
var
  i: Integer;
  S: TExtendedStream;
begin
  if (AppGlobals.SkipSave) or (AppGlobals.DataFile = '') then
  begin
    Exit;
  end;

  if (FCategoryList.Count = 1) and (FStreamList.Count = 0) and (FRecentList.Count = 0) and
     (FIgnoreList.Count = 0) and (FSaveList.Count = 0) and not (FileExists(AppGlobals.DataFile)) then
  begin
    Exit;
  end;

  if not FLoadError then
  begin
    S := TExtendedStream.Create;
    try
      S.Write(Integer(DATAVERSION));

      S.Write(FReceived);

      S.Write(FCategoryList.Count);
      for i := 0 to FCategoryList.Count - 1 do
        FCategoryList[i].Save(S);

      S.Write(FStreamList.Count);
      for i := 0 to FStreamList.Count - 1 do
      begin
        FStreamList[i].Save(S);
      end;

      S.Write(FTrackList.Count);
      for i := 0 to FTrackList.Count - 1 do
        FTrackList[i].Save(S);

      S.Write(FSaveList.Count);
      for i := 0 to FSaveList.Count - 1 do
      begin
        FSaveList[i].Save(S);
      end;

      S.Write(FIgnoreList.Count);
      for i := 0 to FIgnoreList.Count - 1 do
      begin
        FIgnoreList[i].Save(S);
      end;

      S.Write(FSubmittedStreamList.Count);
      for i := 0 to FSubmittedStreamList.Count - 1 do
        S.Write(FSubmittedStreamList[i]);

      S.Write(FRecentList.Count);
      for i := 0 to FRecentList.Count - 1 do
        FRecentList[i].Save(S);

      S.Write(FStreamBlacklist.Count);
      for i := 0 to FStreamBlacklist.Count - 1 do
        S.Write(FStreamBlacklist[i]);

      S.SaveToFile(AppGlobals.DataFile);
    finally
      S.Free;
    end;
  end;
end;

{ TTrackInfo }

constructor TTrackInfo.Create;
begin
  inherited;

end;
constructor TTrackInfo.Create(Time: TDateTime; Filename, Streamname: string);
begin
  inherited Create;

  FTime := Time;
  FFilename := Filename;
  FStreamname := Streamname;
  FWasCut := False;
end;

class function TTrackInfo.Load(Stream: TExtendedStream;
  Version: Integer): TTrackInfo;
begin
  Result := TTrackInfo.Create;

  Stream.Read(Result.FFilename);
  Stream.Read(Result.FStreamname);
  Stream.Read(Result.FFilesize);
  if Version >= 13 then
  begin
    Stream.Read(Result.FLength);
  end;
  Stream.Read(Result.FTime);
  Stream.Read(Result.FWasCut);
  if Version > 10 then
  begin
    Stream.Read(Result.FBitRate);
    Stream.Read(Result.FIsAuto);
  end;
end;

procedure TTrackInfo.Save(Stream: TExtendedStream);
begin
  Stream.Write(FFilename);
  Stream.Write(FStreamname);
  Stream.Write(FFilesize);
  Stream.Write(FLength);
  Stream.Write(FTime);
  Stream.Write(FWasCut);
  Stream.Write(FBitRate);
  Stream.Write(FIsAuto);
end;

{ TStreamList }

function TStreamList.Add(Entry: TStreamEntry): TStreamEntry;
begin
  Result := Get(Entry.Name, Entry.StartURL, Entry.URLs);

  if Result <> nil then
  begin
    Exit;
  end;

  Result := Entry;
  inherited Add(Result);
end;

{
function TStreamList.Add(Name, URL: string;
  URLs: TStringList; BitRate: Cardinal; Genre: string; SkipShort: Boolean; UseFilter: TUseFilters; SongsSaved: Cardinal): TStreamEntry;
var
  Entry: TStreamEntry;
begin
  Result := Get(Name, URL, URLs);

  if Result <> nil then
  begin
    if BitRate > 0 then
      Result.BitRate := BitRate;
    if Genre <> '' then
      Result.Genre := Genre;
    Exit;
  end;

  Entry := TStreamEntry.Create;
  Entry.Name := Name;
  Entry.StartURL := URL;
  Entry.URLs.Assign(URLs);
  //Entry.SkipShort := SkipShort;
  //Entry.BitRate := BitRate;
  //Entry.Genre := Genre;
  Entry.SongsSaved := SongsSaved;
  //Entry.UseFilter := UseFilter;

  Entry.FParent := Self;

  Add(Entry);

  Result := Entry;
end;
}

function TStreamList.Get(Name, URL: string;
  URLs: TStringList): TStreamEntry;
var
  i, n, j: Integer;
begin
  Name := Trim(Name);
  URL := Trim(URL);

  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Name <> '' then
      if LowerCase(Items[i].Name) = LowerCase(Name) then
      begin
        Result := Items[i];
        Exit;
      end;

    if URL <> '' then
      if LowerCase(Items[i].StartURL) = LowerCase(URL) then
      begin
        Result := Items[i];
        Exit;
      end;
    for n := 0 to Items[i].URLs.Count - 1 do
    begin
      if URL <> '' then
        if LowerCase(Items[i].URLs[n]) = LowerCase(URL) then
        begin
          Result := Items[i];
          Exit;
        end;
      if URLs <> nil then
        for j := 0 to URLs.Count - 1 do
          if LowerCase(URL) = LowerCase(URLs[j]) then
          begin
            Result := Items[i];
            Exit;
          end;
    end;
  end;
end;

{ TTitleInfo }

constructor TTitleInfo.Create(Title: string);
var
  NumChars: Integer;
  Hash: Cardinal;
  Pattern: string;
begin
  inherited Create;

  FTitle := Title;

  Pattern := BuildPattern(Title, Hash, NumChars);
  FPattern := Pattern;
  FHash := Hash;
end;

class function TTitleInfo.Load(Stream: TExtendedStream;
  Version: Integer): TTitleInfo;
var
  NumChars: Integer;
  Hash: Cardinal;
  Pattern: string;
begin
  Result := TTitleInfo.Create;
  Stream.Read(Result.FTitle);
  if Version > 3 then
  begin
    Stream.Read(Result.FPattern);
    Stream.Read(Result.FHash);
  end else
  begin
    Pattern := BuildPattern(Result.FTitle, Hash, NumChars);
    Result.FPattern := Pattern;
    Result.FHash := Hash;
  end;
end;

procedure TTitleInfo.Save(Stream: TExtendedStream);
begin
  Stream.Write(FTitle);
  Stream.Write(FPattern);
  Stream.Write(FHash);
end;

{ TListCategory }

constructor TListCategory.Create(Name: string; Idx: Integer);
begin
  inherited Create;
  FName := Name;
  FIndex := Idx;
  FKilled := False;
end;

class function TListCategory.Load(Stream: TExtendedStream;
  Version: Integer): TListCategory;
begin
  Result := TListCategory.Create;
  Result.FKilled := False;
  Stream.Read(Result.FIndex);
  Stream.Read(Result.FName);
  Stream.Read(Result.FExpanded);
  if Version >= 10 then
    Stream.Read(Result.FIsAuto);
end;

procedure TListCategory.Save(Stream: TExtendedStream);
begin
  Stream.Write(FIndex);
  Stream.Write(FName);
  Stream.Write(FExpanded);
  Stream.Write(FIsAuto);
end;

{ TTrackList }

procedure TTrackList.RemoveTrack(Track: TTrackInfo);
begin
  Remove(Track);
  Track.Free;
end;

{ TRecentEntry }

procedure TRecentEntry.Assign(From: TRecentEntry);
begin
  FName := From.FName;
  FStartURL := From.FStartURL;
  FIndex := From.FIndex;
end;

function TRecentEntry.Copy: TRecentEntry;
begin
  Result := TRecentEntry.Create(FName, FStartURL, FIndex);
end;

constructor TRecentEntry.Create(Name, StartURL: string; Index: Cardinal);
begin
  inherited Create;

  FName := Name;
  FStartURL := StartURL;
  FIndex := Index;
end;

class function TRecentEntry.Load(Stream: TExtendedStream;
  Version: Integer): TRecentEntry;
begin
  Result := TRecentEntry.Create('', '', 0);
  Stream.Read(Result.FName);
  Stream.Read(Result.FStartURL);
  Stream.Read(Result.FIndex);
end;

procedure TRecentEntry.Save(Stream: TExtendedStream);
begin
  Stream.Write(FName);
  Stream.Write(FStartURL);
  Stream.Write(FIndex);
end;

end.

