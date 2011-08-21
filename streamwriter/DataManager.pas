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
  ComCtrls, AppData, Functions, Logging, DateUtils, TypeDefs;

type
  TStreamList = class;
  TDataLists = class;
  TListCategory = class;

  EVersionException = class(Exception);

  TStreamBrowserEntry = class
  private
    FID: Integer;
    FName: string;
    FGenre: string;
    FURL: string;
    FWebsite: string;
    FBitRate: Integer;
    FAudioType: TAudioTypes;
    FMetaData: Boolean;
    FChangesTitleInSong: Boolean;
    FOwnRating: Byte;
    FRating: Byte;
    FRecordingOkay: Boolean;
    FRegEx: string;
    FIgnoreTitles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    class function Load(Stream: TExtendedStream; Version: Integer): TStreamBrowserEntry;
    procedure Save(Stream: TExtendedStream);

    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Genre: string read FGenre write FGenre;
    property URL: string read FURL write FURL;
    property Website: string read FWebsite write FWebsite;
    property BitRate: Integer read FBitRate write FBitRate;
    property AudioType: TAudioTypes read FAudioType write FAudioType;
    property MetaData: Boolean read FMetaData write FMetaData;
    property ChangesTitleInSong: Boolean read FChangesTitleInSong write FChangesTitleInSong;
    property OwnRating: Byte read FOwnRating write FOwnRating;
    property Rating: Byte read FRating write FRating;
    property RecordingOkay: Boolean read FRecordingOkay write FRecordingOkay;
    property RegEx: string read FRegEx write FRegEx;
    property IgnoreTitles: TStringList read FIgnoreTitles;
  end;

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
    FIsStreamFile: Boolean;
    FFinalized: Boolean;
  public
    constructor Create; overload;
    constructor Create(Time: TDateTime; Filename, Streamname: string); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TTrackInfo;
    procedure Save(Stream: TExtendedStream);

    property Time: TDateTime read FTime write FTime;
    property Filename: string read FFilename write FFilename;
    property Streamname: string read FStreamname write FStreamname;
    property Filesize: UInt64 read FFilesize write FFilesize;
    property Length: UInt64 read FLength write FLength;
    property WasCut: Boolean read FWasCut write FWasCut;
    property BitRate: Cardinal read FBitRate write FBitRate;
    property IsAuto: Boolean read FIsAuto write FIsAuto;
    property IsStreamFile: Boolean read FIsStreamFile write FIsStreamFile;
    property Finalized: Boolean read FFinalized write FFinalized;
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
    FID: Cardinal;
    FName: string;
    FStartURL: string;
    FIndex: Cardinal;
    FBitrate: Cardinal;
  public
    constructor Create(ID, Bitrate: Cardinal; Name, StartURL: string; Index: Cardinal);

    procedure Assign(From: TRecentEntry);
    function Copy: TRecentEntry;
    class function Load(Stream: TExtendedStream; Version: Integer): TRecentEntry;
    procedure Save(Stream: TExtendedStream);

    property ID: Cardinal read FID write FID;
    property Name: string read FName write FName;
    property StartURL: string read FStartURL write FStartURL;
    property Index: Cardinal read FIndex write FIndex;
    property Bitrate: Cardinal read FBitrate write FBitrate;
  end;

  TRatingEntry = class
  private
    FName: string;
    FURL: string;
    FRating: Integer;
  public
    constructor Create(Name, URL: string; Rating: Integer);

    class function Load(Stream: TExtendedStream; Version: Integer): TRatingEntry;
    procedure Save(Stream: TExtendedStream);

    property Name: string read FName;
    property URL: string read FURL;
    property Rating: Integer read FRating write FRating;
  end;

  TSchedule = class(TObject)
  private
    FActive: Boolean;
    FRecurring: Boolean;
    FInterval: TScheduleInterval;
    FDay: TScheduleDay;
    FDate: TDateTime;
    FAutoRemove: Boolean;
    FStartHour, FStartMinute, FEndHour, FEndMinute: Integer;
    FTriedStart: Boolean;
    FTriedStop: Boolean;

    class function MatchesDay(S: TSchedule; NextDay: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(From: TSchedule);
    function Copy: TSchedule;
    class function Load(Stream: TExtendedStream; Version: Integer): TSchedule;
    procedure Save(Stream: TExtendedStream);

    class function MatchesStart(S: TSchedule): Boolean;
    class function MatchesEnd(S: TSchedule): Boolean;

    property Active: Boolean read FActive write FActive;
    property Recurring: Boolean read FRecurring write FRecurring;
    property Interval: TScheduleInterval read FInterval write FInterval;
    property Day: TScheduleDay read FDay write FDay;
    property Date: TDateTime read FDate write FDate;
    property AutoRemove: Boolean read FAutoRemove write FAutoRemove;
    property StartHour: Integer read FStartHour write FStartHour;
    property StartMinute: Integer read FStartMinute write FStartMinute;
    property EndHour: Integer read FEndHour write FEndHour;
    property EndMinute: Integer read FEndMinute write FEndMinute;
    property TriedStart: Boolean read FTriedStart write FTriedStart;
    property TriedStop: Boolean read FTriedStop write FTriedStop;
  end;

  TScheduleList = TList<TSchedule>;

  TStreamEntry = class(TObject)
  private
    FSettings: TStreamSettings;

    FParent: TStreamList;

    FID: Cardinal;
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

    FSongsSaved: Cardinal;
    FBytesReceived: UInt64;

    FMigrationSubmitted: Boolean;
    FMigrationTrackList: TTrackList;
    FMigrationRecentIndex: Integer;

    FSchedules: TScheduleList;

    FSaveList: TTitleList;
    FIgnoreList: TTitleList;

    procedure FSetName(Value: string);

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
    property ID: Cardinal read FID write FID;
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

    property SongsSaved: Cardinal read FSongsSaved write FSongsSaved;
    property BytesReceived: UInt64 read FBytesReceived write FBytesReceived;

    property Schedules: TScheduleList read FSchedules;

    property SaveList: TTitleList read FSaveList;
    property IgnoreList: TTitleList read FIgnoreList;
  end;

  TStreamList = class(TList<TStreamEntry>)
  private
  public
    function Add(Entry: TStreamEntry): TStreamEntry; overload;
    function Get(Name, URL: string; URLs: TStringList): TStreamEntry; overload;
  end;

  TRecentList = class(TList<TRecentEntry>)
  end;

  TRatingList = class(TList<TRatingEntry>)
  public
    procedure SetRating(Name, URL: string; Rating: Integer);
    function GetRating(Name, URL: string): Integer;
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
    FRatingList: TRatingList;
    FBrowserList: TList<TStreamBrowserEntry>;
    FGenreList: TStringList;
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
    property RatingList: TRatingList read FRatingList;
    property BrowserList: TList<TStreamBrowserEntry> read FBrowserList write FBrowserList;
    property GenreList: TStringList read FGenreList write FGenreList;

    property LoadError: Boolean read FLoadError write FLoadError;
    property Received: UInt64 read FReceived write FReceived;
  end;

const
  DATAVERSION = 30;

implementation

{ TStreamEntry }

procedure TStreamEntry.Assign(From: TStreamEntry);
var
  i: Integer;
  S: TSchedule;
begin
  FID := From.FID;
  FName := From.FName;
  FStreamURL := From.FStreamURL;
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

  for i := 0 to FSchedules.Count - 1 do
    FSchedules[i].Free;
  FSchedules.Clear;
  for i := 0 to From.FSchedules.Count - 1 do
  begin
    S := TSchedule.Create;
    S.Assign(From.FSchedules[i]);
    FSchedules.Add(S);
  end;

  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Free;
  for i := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[i].Free;
  FSaveList.Clear;
  FIgnoreList.Clear;

  for i := 0 to From.FSaveList.Count - 1 do
    FSaveList.Add(From.FSaveList[i].Copy);
  for i := 0 to From.FIgnoreList.Count - 1 do
    FIgnoreList.Add(From.FIgnoreList[i].Copy);
end;

function TStreamEntry.Copy: TStreamEntry;
begin
  Result := TStreamEntry.Create;
  Result.Assign(Self);
end;

constructor TStreamEntry.Create;
begin
  FSettings := TStreamSettings.Create;

  FID := 0;
  FParent := Parent;
  FURLs := TStringList.Create;
  FSongsSaved := 0;
  FMigrationTrackList := TTrackList.Create;

  FSchedules := TScheduleList.Create;

  FSaveList := TTitleList.Create;
  FIgnoreList := TTitleList.Create;
end;

destructor TStreamEntry.Destroy;
var
  i: Integer;
begin
  FURLs.Free;
  FMigrationTrackList.Free;
  FSettings.Free;
  for i := 0 to FSchedules.Count - 1 do
    FSchedules[i].Free;
  FSchedules.Free;

  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Free;
  FSaveList.Free;
  for i := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[i].Free;
  FIgnoreList.Free;

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

  if Version >= 24 then
    Stream.Read(Result.FID);

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
      if Version > 26 then
        Result.FSettings.Filter := TUseFilters(B)
      else
      begin
        if B = 0 then
          Result.FSettings.Filter := ufNone
        else if B = 1 then
          Result.FSettings.Filter := ufWish
        else if B = 2 then
          Result.FSettings.Filter := ufIgnoreBoth
        else if B = 3 then
          Result.FSettings.Filter := ufBoth
        else
          Result.FSettings.Filter := ufNone;
      end;
    end;

    Stream.Read(Result.FMigrationSubmitted);
  end;

  if Version <= 20 then
    Stream.Read(BTmp);
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

  if Version >= 18 then
  begin
    Stream.Read(Count);
    for i := 0 to Count - 1 do
      Result.FSchedules.Add(TSchedule.Load(Stream, Version));
  end;

  if Version >= 28 then
  begin
    Stream.Read(Count);
    for i := 0 to Count - 1 do
      Result.FSaveList.Add(TTitleInfo.Load(Stream, Version));
    Stream.Read(Count);
    for i := 0 to Count - 1 do
      Result.FIgnoreList.Add(TTitleInfo.Load(Stream, Version));
  end else if Version = 27 then
  begin
    for i := Result.FSettings.MigrationIgnoreList.Count - 1 downto 0 do
    begin
      Result.FIgnoreList.Add(Result.FSettings.MigrationIgnoreList[i].Copy);
      Result.FSettings.MigrationIgnoreList[i].Free;
      Result.FSettings.MigrationIgnoreList.Delete(i);
    end;
  end;
end;

procedure TStreamEntry.Save(Stream: TExtendedStream);
var
  i: Integer;
begin
  FSettings.Save(Stream);

  Stream.Write(FID);
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

  Stream.Write(FIndex);
  Stream.Write(FCategoryIndex);

  Stream.Write(FSongsSaved);
  Stream.Write(FBytesReceived);

  Stream.Write(FWasRecording);

  Stream.Write(FSchedules.Count);
  for i := 0 to FSchedules.Count - 1 do
    FSchedules[i].Save(Stream);

  Stream.Write(FSaveList.Count);
  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Save(Stream);

  Stream.Write(FIgnoreList.Count);
  for i := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[i].Save(Stream);
end;

procedure TStreamEntry.FSetGenre(Value: string);
begin
  FGenre := Value;
end;

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
  FRatingList := TRatingList.Create;
  FBrowserList := TList<TStreamBrowserEntry>.Create;
  FGenreList := TStringList.Create;
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

  for i := 0 to FRatingList.Count - 1 do
    FRatingList[i].Free;
  FRatingList.Free;

  for i := 0 to FBrowserList.Count - 1 do
    FBrowserList[i].Free;
  FBrowserList.Free;

  FGenreList.Free;

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

            if Version = 22 then
            begin
              S.Read(EntryCount);
              for i := 0 to EntryCount - 1 do
                FRatingList.Add(TRatingEntry.Load(S, Version));
            end;

            if Version >= 23 then
            begin
              S.Read(EntryCount);
              for i := 0 to EntryCount - 1 do
                FBrowserList.Add(TStreamBrowserEntry.Load(S, Version));

              S.Read(EntryCount);
              for i := 0 to EntryCount - 1 do
              begin
                S.Read(Str);
                FGenreList.Add(Str);
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
            FRecentList.Add(TRecentEntry.Create(Entry.ID, Entry.Bitrate, Entry.Name, Entry.StartURL, Entry.FMigrationRecentIndex));
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
     (FIgnoreList.Count = 0) and (FSaveList.Count = 0) and (FBrowserList.Count = 0) and
     not (FileExists(AppGlobals.DataFile)) then
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

      while FSubmittedStreamList.Count > 200 do
        FSubmittedStreamList.Delete(0);

      S.Write(FSubmittedStreamList.Count);
      for i := 0 to FSubmittedStreamList.Count - 1 do
        S.Write(FSubmittedStreamList[i]);

      S.Write(FRecentList.Count);
      for i := 0 to FRecentList.Count - 1 do
        FRecentList[i].Save(S);

      S.Write(FStreamBlacklist.Count);
      for i := 0 to FStreamBlacklist.Count - 1 do
        S.Write(FStreamBlacklist[i]);

      S.Write(FBrowserList.Count);
      for i := 0 to FBrowserList.Count - 1 do
        FBrowserList[i].Save(S);

      S.Write(FGenreList.Count);
      for i := 0 to FGenreList.Count - 1 do
        S.Write(FGenreList[i]);

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
  FIsStreamFile := False;
end;

class function TTrackInfo.Load(Stream: TExtendedStream;
  Version: Integer): TTrackInfo;
begin
  Result := TTrackInfo.Create;

  Stream.Read(Result.FFilename);

  Result.FFilename := TryUnRelativePath(Result.FFilename, True);

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

  if Version >= 30 then
    Stream.Read(Result.FIsStreamFile);

  if Version > 18 then
  begin
    Stream.Read(Result.FFinalized);
  end;
end;

procedure TTrackInfo.Save(Stream: TExtendedStream);
begin
  Stream.Write(TryRelativePath(FFilename, True));

  Stream.Write(FStreamname);
  Stream.Write(FFilesize);
  Stream.Write(FLength);
  Stream.Write(FTime);
  Stream.Write(FWasCut);
  Stream.Write(FBitRate);
  Stream.Write(FIsAuto);
  Stream.Write(FIsStreamFile);
  Stream.Write(FFinalized);
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
  FID := From.FID;
  FBitrate := From.FBitrate;
  FName := From.FName;
  FStartURL := From.FStartURL;
  FIndex := From.FIndex;
end;

function TRecentEntry.Copy: TRecentEntry;
begin
  Result := TRecentEntry.Create(FID, FBitrate, FName, FStartURL, FIndex);
end;

constructor TRecentEntry.Create(ID, Bitrate: Cardinal; Name, StartURL: string; Index: Cardinal);
begin
  inherited Create;

  FID := ID;
  FName := Name;
  FStartURL := StartURL;
  FIndex := Index;
  FBitrate := Bitrate;
end;

class function TRecentEntry.Load(Stream: TExtendedStream;
  Version: Integer): TRecentEntry;
begin
  Result := TRecentEntry.Create(0, 0, '', '', 0);
  if Version >= 24 then
    Stream.Read(Result.FID);
  Stream.Read(Result.FName);
  Stream.Read(Result.FStartURL);
  Stream.Read(Result.FIndex);
  if Version >= 25 then
    Stream.Read(Result.FBitrate);
end;

procedure TRecentEntry.Save(Stream: TExtendedStream);
begin
  Stream.Write(FID);
  Stream.Write(FName);
  Stream.Write(FStartURL);
  Stream.Write(FIndex);
  Stream.Write(FBitrate);
end;

{ TScheduledRecording }

procedure TSchedule.Assign(From: TSchedule);
begin
  FActive := From.FActive;
  FRecurring := From.FRecurring;
  FInterval := From.FInterval;
  FDay := From.FDay;
  FDate := From.FDate;
  FAutoRemove := From.FAutoRemove;
  FStartHour := From.FStartHour;
  FStartMinute := From.FStartMinute;
  FEndHour := From.FEndHour;
  FEndMinute := From.FEndMinute;
end;

function TSchedule.Copy: TSchedule;
begin
  Result := TSchedule.Create;
  Result.Assign(Self);
end;

constructor TSchedule.Create;
begin

end;

destructor TSchedule.Destroy;
begin

  inherited;
end;

class function TSchedule.Load(Stream: TExtendedStream;
  Version: Integer): TSchedule;
var
  B: Byte;
begin
  Result := TSchedule.Create;
  Stream.Read(Result.FActive);
  Stream.Read(Result.FRecurring);
  Stream.Read(B);
  Result.FInterval := TScheduleInterval(B);
  Stream.Read(B);
  Result.FDay := TScheduleDay(B);
  Stream.Read(Result.FDate);
  if Version >= 30 then
    Stream.Read(Result.FAutoRemove);
  Stream.Read(Result.FStartHour);
  Stream.Read(Result.FStartMinute);
  Stream.Read(Result.FEndHour);
  Stream.Read(Result.FEndMinute);
end;

class function TSchedule.MatchesDay(S: TSchedule; NextDay: Boolean): Boolean;
var
  DOW: Word;
begin
  Result := False;
  if S.FRecurring then
  begin
    Result := True;
    if S.FInterval = siWeekly then
    begin
      DOW := DayOfWeek(Now);

      // Weil Sonntag bei DayOfWeek() der erste ist...
      if DOW = 1 then
        DOW := 7
      else
        Dec(DOW);

      if NextDay then
        Inc(DOW);

      if not (DOW = Word(S.FDay) + 1) then
        Result := False;
    end;
  end else
  begin
    if NextDay then
      S.Date := S.Date + 1;

    if Trunc(S.Date) = Trunc(Now) then
      Result := True;
  end;
end;

class function TSchedule.MatchesStart(S: TSchedule): Boolean;
begin
  Result := False;
  if not MatchesDay(S, False) then
    Exit;

  if (S.StartHour = HourOf(Now)) and (S.StartMinute = MinuteOf(Now)) then
    Result := True;
end;

class function TSchedule.MatchesEnd(S: TSchedule): Boolean;
var
  NextDay: Boolean;
  StartTime, EndTime: TDateTime;
begin
  Result := False;

  StartTime := EncodeTime(S.StartHour, S.StartMinute, 0, 0);
  EndTime := EncodeTime(S.EndHour, S.EndMinute, 0, 0);

  NextDay := False;
  if EndTime <= StartTime then
  begin
    // Das hier geht von z.B. 23:00 bis 01:00, also über 24 Uhr hinaus.
    // Für MatchesDay muss dann ein Tag drauf gepackt werden.
    NextDay := True;
  end;

  if not MatchesDay(S, NextDay) then
    Exit;

  if (S.EndHour = HourOf(Now)) and (S.EndMinute = MinuteOf(Now)) then
    Result := True;
end;

procedure TSchedule.Save(Stream: TExtendedStream);
begin
  Stream.Write(FActive);
  Stream.Write(FRecurring);
  Stream.Write(Byte(FInterval));
  Stream.Write(Byte(FDay));
  Stream.Write(FDate);
  Stream.Write(FAutoRemove);
  Stream.Write(FStartHour);
  Stream.Write(FStartMinute);
  Stream.Write(FEndHour);
  Stream.Write(FEndMinute);
end;

{ TRatingEntry }

constructor TRatingEntry.Create(Name, URL: string; Rating: Integer);
begin
  inherited Create;

  FName := Name;
  FURL := URL;
  FRating := Rating;
end;

class function TRatingEntry.Load(Stream: TExtendedStream;
  Version: Integer): TRatingEntry;
begin
  Result := TRatingEntry.Create('', '', 0);
  Stream.Read(Result.FName);
  Stream.Read(Result.FURL);
  Stream.Read(Result.FRating);
end;

procedure TRatingEntry.Save(Stream: TExtendedStream);
begin
  Stream.Write(FName);
  Stream.Write(FURL);
  Stream.Write(FRating);
end;

{ TRatingList }

function TRatingList.GetRating(Name, URL: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  Name := LowerCase(Name);
  URL := LowerCase(URL);
  for i := 0 to Count - 1 do
    if (Items[i].Name = Name) or (Items[i].URL = URL) then
    begin
      Result := Items[i].Rating;
      Exit;
    end;
end;

procedure TRatingList.SetRating(Name, URL: string; Rating: Integer);
var
  i: Integer;
  E: TRatingEntry;
begin
  Name := LowerCase(Name);
  URL := LowerCase(URL);
  for i := 0 to Count - 1 do
    if (Items[i].Name = Name) or (Items[i].URL = URL) then
    begin
      Items[i].Rating := Rating;
      Exit;
    end;
  E := TRatingEntry.Create(Name, URL, Rating);
  Add(E);
end;

{ TStreamBrowserEntry }

constructor TStreamBrowserEntry.Create;
begin
  inherited;

  FIgnoreTitles := TStringList.Create;
end;

destructor TStreamBrowserEntry.Destroy;
begin
  FIgnoreTitles.Free;

  inherited;
end;

class function TStreamBrowserEntry.Load(Stream: TExtendedStream;
  Version: Integer): TStreamBrowserEntry;
var
  i: Integer;
  B: Byte;
  Count: Cardinal;
  E: string;
begin
  Result := TStreamBrowserEntry.Create;
  Stream.Read(Result.FID);
  Stream.Read(Result.FName);
  Stream.Read(Result.FGenre);
  Stream.Read(Result.FURL);
  Stream.Read(Result.FWebsite);
  Stream.Read(Result.FBitRate);
  Stream.Read(B);
  Result.FAudioType := TAudioTypes(B);
  Stream.Read(Result.FMetaData);
  Stream.Read(Result.FChangesTitleInSong);
  Stream.Read(Result.FOwnRating);
  Stream.Read(Result.FRating);
  Stream.Read(Result.FRecordingOkay);
  Stream.Read(Result.FRegEx);

  if Version >= 29 then
  begin
    Stream.Read(Count);
    for i := 0 to Count - 1 do
    begin
      Stream.Read(E);
      Result.FIgnoreTitles.Add(E);
    end;
  end;
end;

procedure TStreamBrowserEntry.Save(Stream: TExtendedStream);
var
  i: Integer;
begin
  Stream.Write(FID);
  Stream.Write(FName);
  Stream.Write(FGenre);
  Stream.Write(FURL);
  Stream.Write(FWebsite);
  Stream.Write(FBitRate);
  Stream.Write(Byte(FAudioType));
  Stream.Write(FMetaData);
  Stream.Write(FChangesTitleInSong);
  Stream.Write(FOwnRating);
  Stream.Write(FRating);
  Stream.Write(FRecordingOkay);
  Stream.Write(FRegEx);

  Stream.Write(Cardinal(FIgnoreTitles.Count));
  for i := 0 to FIgnoreTitles.Count - 1 do
    Stream.Write(FIgnoreTitles[i]);
end;

end.


