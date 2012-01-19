{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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

{ This unit contains everything to load and save data from files containing settings.
  It mainly contains methods to load/save stuff from binary data files }
unit DataManager;

interface

uses
  Windows, Classes, SysUtils, ExtendedStream, Generics.Collections,
  ComCtrls, AppData, Functions, Logging, DateUtils, TypeDefs;

type
  TStreamList = class;
  TDataLists = class;
  TListCategory = class;

  { This exception is raised when streamWriter tries to load a file saved with
    a newer file-format that it does not know (because it is an old version) }
  EVersionException = class(Exception);

  // Contains information about titles in the wishlist/ignorelist
  TTitleInfo = class
  private
    FTitle: string;
    FAdded: TDateTime;
    FIndex: Cardinal;
    FPattern: string;
    FHash: Cardinal;
  public
    constructor Create(Title: string); overload;
    function Copy: TTitleInfo;

    class function Load(Stream: TExtendedStream; Version: Integer): TTitleInfo;
    procedure Save(Stream: TExtendedStream);

    property Title: string read FTitle write FTitle;
    property Added: TDateTime read FAdded write FAdded;
    property Index: Cardinal read FIndex write FIndex;
    property Pattern: string read FPattern;
    property Hash: Cardinal read FHash;
  end;

  TN = procedure(Sender: TObject; const Item: TTitleInfo; Action: TCollectionNotification) of object;

  // A list that might be used as wishlist/ignorelist
  TTitleList = class(TList<TTitleInfo>)
  private
    FNotifications: Boolean;
    FOnChange: TList<TN>;
  protected
    procedure Notify(const Item: TTitleInfo; Action: TCollectionNotification); override;
  public
    constructor Create;
    destructor Destroy; override;

    property OnChange: TList<TN> read FOnChange;
    property Notifications: Boolean read FNotifications write FNotifications;
  end;

  // An entry in the stream-browser
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

    // The unique ID of the stream
    property ID: Integer read FID write FID;
    // The broadcasted name of the stream
    property Name: string read FName write FName;
    // The broadcasted genre(s) of the stream (might be "rock,punk  , gothic")
    property Genre: string read FGenre write FGenre;
    // The URL to the stream
    property URL: string read FURL write FURL;
    // The broadcasted URL to the stream's website
    property Website: string read FWebsite write FWebsite;
    { The broadcasted bitrate of the stream. This value might get overriden
      when streamWriter parses received audio data }
    property BitRate: Integer read FBitRate write FBitRate;
    // The format of the audio-data
    property AudioType: TAudioTypes read FAudioType write FAudioType;
    // When set the stream broadcasts metadata
    property MetaData: Boolean read FMetaData write FMetaData;
    // When set the stream changes it's title within a song (not good for recording)
    property ChangesTitleInSong: Boolean read FChangesTitleInSong write FChangesTitleInSong;
    // The rating for the stream applied by the user
    property OwnRating: Byte read FOwnRating write FOwnRating;
    // The average rating for the stream applied by all users
    property Rating: Byte read FRating write FRating;
    // When set recording from this stream is okay (user rated it this way and it does not change titles in songs (see (ChangesTitleInSong))
    property RecordingOkay: Boolean read FRecordingOkay write FRecordingOkay;
    // The regular expression to extract artist/title/album from stream titles
    property RegEx: string read FRegEx write FRegEx;
    // A list containing titles to ignore for track changes
    property IgnoreTitles: TStringList read FIgnoreTitles;
  end;

  // An entry in the "Saved tracks" tab
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
    FIndex: Cardinal;
  public
    constructor Create; overload;
    constructor Create(Time: TDateTime; Filename, Streamname: string); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TTrackInfo;
    procedure Save(Stream: TExtendedStream);

    // The time the title was saved
    property Time: TDateTime read FTime write FTime;
    // The filename of the title
    property Filename: string read FFilename write FFilename;
    // The name of the stream which led to recording of this title
    property Streamname: string read FStreamname write FStreamname;
    // The size of the file
    property Filesize: UInt64 read FFilesize write FFilesize;
    // The length of the file
    property Length: UInt64 read FLength write FLength;
    // Indicates whether the title was cut (silence on begin/end detected)
    property WasCut: Boolean read FWasCut write FWasCut;
    // The bitrate of the file
    property BitRate: Cardinal read FBitRate write FBitRate;
    // When set the file was automatically recorded from the wishlist
    property IsAuto: Boolean read FIsAuto write FIsAuto;
    // When set this is a stream-file (not a single saved file from a stream with artist/title)
    property IsStreamFile: Boolean read FIsStreamFile write FIsStreamFile;
    // When set the file was post-processed by the user
    property Finalized: Boolean read FFinalized write FFinalized;
    property Index: Cardinal read FIndex write FIndex;
  end;

  TListCategoryList = TList<TListCategory>;

  // A category-entry in client-view
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

    // The name of the category
    property Name: string read FName write FName;
    // The index of the category - it's used when starting up streamWriter to determine the position of the category
    property Index: Integer read FIndex write FIndex;
    // When set the category is expanded
    property Expanded: Boolean read FExpanded write FExpanded;
    // When set all children of the category are "killed" - so remove the category when all children are removed from the tree
    property Killed: Boolean read FKilled write FKilled;
    // When set this is the category for automatic recordings
    property IsAuto: Boolean read FIsAuto write FIsAuto;
  end;

  TTrackList = class(TList<TTrackInfo>)
  public
    procedure RemoveTrack(Track: TTrackInfo);
    function GetTrack(Filename: string): TTrackInfo;
  end;

  // An entry in the stream dropdown-combobox
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

    // The stream's id
    property ID: Cardinal read FID write FID;
    // The name of this recent stream
    property Name: string read FName write FName;
    // The URL
    property StartURL: string read FStartURL write FStartURL;
    // The list index
    property Index: Cardinal read FIndex write FIndex;
    // Bitrate
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

  // Defines an entry in the "scheduled recordings" of a stream
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

    // Does the current time meet the scheduled start-time?
    class function MatchesStart(S: TSchedule): Boolean;
    // Does the current time meet the scheduled end-time?
    class function MatchesEnd(S: TSchedule): Boolean;

    // When set this schedule is active
    property Active: Boolean read FActive write FActive;
    // When set this schedule is recurring
    property Recurring: Boolean read FRecurring write FRecurring;
    // This defines the interval of the schedule
    property Interval: TScheduleInterval read FInterval write FInterval;
    // This defines the day of the schedule
    property Day: TScheduleDay read FDay write FDay;
    // This defines the date of the schedule
    property Date: TDateTime read FDate write FDate;
    // When set the schedule will be removed after recording
    property AutoRemove: Boolean read FAutoRemove write FAutoRemove;
    // Defines the start hour for the schedule
    property StartHour: Integer read FStartHour write FStartHour;
    // Defines the start minute for the schedule
    property StartMinute: Integer read FStartMinute write FStartMinute;
    // Defines the end hour for the schedule
    property EndHour: Integer read FEndHour write FEndHour;
    // Defines the end minute for the schedule
    property EndMinute: Integer read FEndMinute write FEndMinute;

    property TriedStart: Boolean read FTriedStart write FTriedStart;
    property TriedStop: Boolean read FTriedStop write FTriedStop;
  end;

  // A list of items of TSchedule
  TScheduleList = TList<TSchedule>;

  { An entry about a stream that is/was in the stream-list.
    This class contains an instance of TStreamSettings and other data. }
  TStreamEntry = class(TObject)
  private
    // Defines stream-specific settings
    FSettings: TStreamSettings;

    FParent: TStreamList;

    // Unique ID of the stream
    FID: Cardinal;
    // Name of the stream broadcasted by itself
    FName: string;
    // Name of the stream set by the user (this is editable in the clientview)
    FCustomName: string;
    FStreamURL: string;
    FStartURL: string;
    // List of URLs to the stream (excluding the primary URL)
    FURLs: TStringList;
    // Bitrate of the stream
    FBitrate: Cardinal;
    // Audiotype ('mpg', 'aac', 'ogg', ...)
    FAudioType: TAudioTypes;
    // Genre ('punk   , rock, speedmetal')
    FGenre: string;
    // The stream's index on the list
    FIndex: Integer;
    // The category this stream belongs to
    FCategoryIndex: Integer;
    // When set this stream was being recorded when streamWriter exited on the last run
    FWasRecording: Boolean;

    // Number of songs saved
    FSongsSaved: Cardinal;
    // Amount of bytes received
    FBytesReceived: UInt64;

    // List of configured schedules for this stream
    FSchedules: TScheduleList;

    // List of titles to save for this stream
    FSaveList: TTitleList;
    // List of titles to ignore for this stream
    FIgnoreList: TTitleList;
    FIgnoreListIndex: Cardinal;

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
    property CustomName: string read FCustomName write FCustomName;
    property StreamURL: string read FStreamURL write FStreamURL;
    property StartURL: string read FStartURL write FStartURL;
    property URLs: TStringList read FURLs;
    property Bitrate: Cardinal read FBitrate write FBitrate;
    property AudioType: TAudioTypes read FAudioType write FAudioType;
    property Genre: string read FGenre write FSetGenre;
    property Index: Integer read FIndex write FIndex;
    property CategoryIndex: Integer read FCategoryIndex write FCategoryIndex;
    property WasRecording: Boolean read FWasRecording write FWasRecording;

    property SongsSaved: Cardinal read FSongsSaved write FSongsSaved;
    property BytesReceived: UInt64 read FBytesReceived write FBytesReceived;

    property Schedules: TScheduleList read FSchedules;

    property SaveList: TTitleList read FSaveList;
    property IgnoreList: TTitleList read FIgnoreList;
    property IgnoreListIndex: Cardinal read FIgnoreListIndex write FIgnoreListIndex;
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

  TChartCategory = class
  private
    FID: Cardinal;
    FName: string;
  public
    constructor Create; overload;
    constructor Create(ID: Cardinal; Name: string); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TChartCategory;
    procedure Save(Stream: TExtendedStream);

    property ID: Cardinal read FID;
    property Name: string read FName;
  end;

  // An entry within the "Charts"-window
  TChartEntry = class
  private
    FName: string;
    FChance: Integer;
    FCategories: TIntArray;
  public
    constructor Create; overload;
    constructor Create(Name: string; Chance: Integer; Categories: TIntArray); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TChartEntry;
    procedure Save(Stream: TExtendedStream);

    // The name
    property Name: string read FName;
    // The chance of the title to get played
    property Chance: Integer read FChance;
    // Categories this title is included (i.e. "Top 100")
    property Categories: TIntArray read FCategories;
  end;

  TChartList = class(TList<TChartEntry>)
  end;

  TGenre = class
  private
    FID: Cardinal;
    FName: string;
    FChartCount: Cardinal;
  public
    constructor Create; overload;
    constructor Create(Name: string; ID, ChartCount: Cardinal); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TGenre;
    procedure Save(Stream: TExtendedStream);

    property ID: Cardinal read FID write FID;
    property Name: string read FName write FName;
    property ChartCount: Cardinal read FChartCount write FChartCount;
  end;

  TGenreList = class(TList<TGenre>)
  end;

  TChartCategoryList = class(TList<TChartCategory>)
  end;

  { This class contains lists and stuff about everything streamWriter
    needs to know at runtime. This class gets loaded at startup and
    disposed/saved at the end }
  TDataLists = class
  private
    FCategoryList: TListCategoryList;
    FStreamList: TStreamList;
    FTrackList: TTrackList;
    FSaveList: TTitleList;
    FIgnoreList: TTitleList;
    FRecentList: TRecentList;
    FStreamBlacklist: TStringList;
    FRatingList: TRatingList;
    FChartList: TChartList;
    FChartCategoryList: TChartCategoryList;
    FBrowserList: TList<TStreamBrowserEntry>;
    FGenreList: TGenreList;
    FLoadError: Boolean;
    FReceived: UInt64;
  public
    constructor Create;
    destructor Destroy; override;

    // Cleans all lists and frees all their items
    procedure CleanLists;
    procedure Load; overload;
    procedure Load(S: TExtendedStream); overload;
    procedure Save; overload;
    procedure Save(S: TExtendedStream); overload;
    procedure SaveRecover;

    // List that contains all categories
    property CategoryList: TListCategoryList read FCategoryList;
    // List that contains all streams
    property StreamList: TStreamList read FStreamList;
    // List that contains all saved tracks
    property TrackList: TTrackList read FTrackList;
    // List that contains all titles to be saved
    property SaveList: TTitleList read FSaveList;
    // List that contains all titles to be ignored
    property IgnoreList: TTitleList read FIgnoreList;
    // List that contains all recently used streams
    property RecentList: TRecentList read FRecentList;
    // List that contains all streams to blacklist for automatic recordings
    property StreamBlacklist: TStringList read FStreamBlacklist;
    // List that contains all ratings for streams
    property RatingList: TRatingList read FRatingList;
    // List that contains charts
    property ChartList: TChartList read FChartList;
    // List that contains categories for charts
    property ChartCategoryList: TChartCategoryList read FChartCategoryList;
    // List that contains all TStreamBrowserEntries
    property BrowserList: TList<TStreamBrowserEntry> read FBrowserList write FBrowserList;
    // List that contains all genres
    property GenreList: TGenreList read FGenreList write FGenreList;

    // When set an error occured while loading the data-file
    property LoadError: Boolean read FLoadError write FLoadError;
    // Overall amount of data received
    property Received: UInt64 read FReceived write FReceived;
  end;

const
  DATAVERSION = 40;

implementation

{ TTitleInfo }

constructor TTitleInfo.Create(Title: string);
var
  NumChars: Integer;
  Hash: Cardinal;
  Pattern: string;
begin
  inherited Create;

  FTitle := Title;
  FAdded := Now;

  Pattern := BuildPattern(Title, Hash, NumChars, False);
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

  if Version > 31 then
    Stream.Read(Result.FAdded)
  else
    Result.FAdded := Now;

  if Version > 31 then
    Stream.Read(Result.FIndex)
  else
    Result.FIndex := High(Cardinal);

  if Version > 3 then
  begin
    Stream.Read(Result.FPattern);
    Stream.Read(Result.FHash);
  end else
  begin
    Pattern := BuildPattern(Result.FTitle, Hash, NumChars, False);
    Result.FPattern := Pattern;
    Result.FHash := Hash;
  end;
end;

procedure TTitleInfo.Save(Stream: TExtendedStream);
begin
  Stream.Write(FTitle);
  Stream.Write(FAdded);
  Stream.Write(FIndex);
  Stream.Write(FPattern);
  Stream.Write(FHash);
end;

function TTitleInfo.Copy: TTitleInfo;
begin
  Result := TTitleInfo.Create;
  Result.FTitle := FTitle;
  Result.FAdded := FAdded;
  Result.FIndex := FIndex;
  Result.FPattern := FPattern;
  Result.FHash := FHash;
end;

{ TTitleList }

constructor TTitleList.Create;
begin
  inherited;

  FNotifications := True;
  FOnChange := TList<TN>.Create;
end;

destructor TTitleList.Destroy;
begin
  FOnChange.Free;
  FOnChange := nil;

  inherited;
end;

procedure TTitleList.Notify(const Item: TTitleInfo;
  Action: TCollectionNotification);
var
  T: TN;
begin
  if FNotifications and (FOnChange <> nil) then
    for T in FOnChange do
      T(Self, Item, Action);

  inherited;
end;

{ TStreamEntry }

procedure TStreamEntry.Assign(From: TStreamEntry);
var
  i: Integer;
  S: TSchedule;
begin
  FID := From.FID;
  FName := From.FName;
  FCustomName := From.FCustomName;
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

  FIgnoreListIndex := From.FIgnoreListIndex;
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

  FSchedules := TScheduleList.Create;

  FSaveList := TTitleList.Create;
  FIgnoreList := TTitleList.Create;
end;

destructor TStreamEntry.Destroy;
var
  i: Integer;
begin
  FURLs.Free;
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
  i: Integer;
  Count: Cardinal;
  EnumTmp: Byte;
  STmp, URL: string;
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

  if Version >= 38 then
    Stream.Read(Result.FCustomName)
  else
    Result.FCustomName := Result.FName;

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

  if Version < 40 then
  begin
    Stream.Read(STmp);
    Result.FAudioType := atNone;
    if STmp = 'MP3' then
      Result.FAudioType := atMPEG
    else if STmp = 'AAC' then
      Result.FAudioType := atAAC;
  end else
  begin
    Stream.Read(EnumTmp);
    Result.FAudioType := TAudioTypes(EnumTmp);
  end;

  Stream.Read(Result.FGenre);

  if Version <= 20 then
    Stream.Read(BTmp);
  if Version >= 5 then
  begin
    Stream.Read(Result.FIndex);
    Stream.Read(Result.FCategoryIndex);
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
  end;

  if Version > 31 then
    Stream.Read(Result.FIgnoreListIndex)
  else
    Result.FIgnoreListIndex := High(Cardinal);
end;

procedure TStreamEntry.Save(Stream: TExtendedStream);
var
  i: Integer;
begin
  FSettings.Save(Stream);

  Stream.Write(FID);
  Stream.Write(FName);
  Stream.Write(FCustomName);
  Stream.Write(FStreamURL);
  Stream.Write(FStartURL);
  Stream.Write(FURLs.Count);
  for i := 0 to FURLs.Count - 1 do
  begin
    Stream.Write(FURLs[i]);
  end;
  Stream.Write(FBitRate);
  Stream.Write(Byte(FAudioType));
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

  Stream.Write(FIgnoreListIndex);
end;

procedure TStreamEntry.FSetGenre(Value: string);
begin
  FGenre := Value;
end;

{ TStreamDataList }

procedure TDataLists.CleanLists;
var
  i: Integer;
begin
  for i := 0 to FCategoryList.Count - 1 do
    FCategoryList[i].Free;
  FCategoryList.Clear;

  for i := 0 to FTrackList.Count - 1 do
    FTrackList[i].Free;
  FTrackList.Clear;

  for i := 0 to FStreamList.Count - 1 do
    FStreamList[i].Free;
  FStreamList.Clear;

  FSaveList.Notifications := False;
  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Free;
  FSaveList.Clear;
  FSaveList.Notifications := True;

  for i := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[i].Free;
  FIgnoreList.Clear;

  for i := 0 to FRecentList.Count - 1 do
    FRecentList[i].Free;
  FRecentList.Clear;

  FStreamBlackList.Clear;

  for i := 0 to FRatingList.Count - 1 do
    FRatingList[i].Free;
  FRatingList.Clear;

  for i := 0 to FBrowserList.Count - 1 do
    FBrowserList[i].Free;
  FBrowserList.Clear;

  for i := 0 to FChartList.Count - 1 do
    FChartList[i].Free;
  FChartList.Clear;

  for i := 0 to FChartCategoryList.Count - 1 do
    FChartCategoryList[i].Free;
  FChartCategoryList.Clear;

  for i := 0 to FGenreList.Count - 1 do
    FGenreList[i].Free;
  FGenreList.Clear;
end;

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
  FRecentList := TRecentList.Create;
  FStreamBlacklist := TStringList.Create;
  FRatingList := TRatingList.Create;
  FBrowserList := TList<TStreamBrowserEntry>.Create;
  FGenreList := TGenreList.Create;
  FChartList := TChartList.Create;
  FChartCategoryList := TChartCategoryList.Create;
end;

destructor TDataLists.Destroy;
begin
  CleanLists;

  FCategoryList.Free;
  FTrackList.Free;
  FStreamList.Free;
  FSaveList.Free;
  FIgnoreList.Free;
  FRecentList.Free;
  FStreamBlackList.Free;
  FRatingList.Free;
  FBrowserList.Free;
  FGenreList.Free;
  FChartList.Free;
  FChartCategoryList.Free;

  inherited;
end;

procedure TDataLists.Load(S: TExtendedStream);
var
  Entry: TStreamEntry;
  TitleInfo: TTitleInfo;
  TrackInfo: TTrackInfo;
  Str: string;
  Version, CatCount, EntryCount: Integer;
  i: Integer;
begin
  CleanLists;

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
        if Version < 37 then
        begin
          S.Read(EntryCount);
          for i := 0 to EntryCount - 1 do
          begin
            S.Read(Str);
            //FSubmittedStreamList.Add(Str);
          end;
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
            FGenreList.Add(TGenre.Load(S, Version));
        end;
      end;
    end;
  end;

  if Version >= 35 then
  begin
    S.Read(CatCount);
    for i := 0 to CatCount - 1 do
      FChartCategoryList.Add(TChartCategory.Load(S, Version));

    S.Read(CatCount);
    for i := 0 to CatCount - 1 do
      FChartList.Add(TChartEntry.Load(S, Version));
  end;

end;

procedure TDataLists.Load;
var
  S: TExtendedStream;
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
      Load(S);
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

procedure TDataLists.Save(S: TExtendedStream);
var
  i: Integer;
begin
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
    FGenreList[i].Save(S);

  S.Write(FChartCategoryList.Count);
  for i := 0 to FChartCategoryList.Count - 1 do
    FChartCategoryList[i].Save(S);

  S.Write(FChartList.Count);
  for i := 0 to FChartList.Count - 1 do
    FChartList[i].Save(S);
end;

procedure TDataLists.SaveRecover;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    Save(S);
    S.SaveToFile(AppGlobals.RecoveryFile);
  finally
    S.Free;
  end;
end;

procedure TDataLists.Save;
var
  S: TExtendedStream;
begin
  DeleteFile(AppGlobals.RecoveryFile); // TODO: Sollte das nicht ganz am ende bei erfolgreichem speichern erst passieren???

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
      Save(S);
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
    Stream.Read(Result.FLength);

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
    Stream.Read(Result.FFinalized);

  if Version > 31 then
    Stream.Read(Result.FIndex)
  else
    Result.FIndex := High(Cardinal);
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
  Stream.Write(FIndex);
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

function TTrackList.GetTrack(Filename: string): TTrackInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if LowerCase(Items[i].FFilename) = LowerCase(Filename) then
      Exit(Items[i]);
end;

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
  inherited;

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
  SDate: TDateTime;
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
    SDate := S.Date;

    if NextDay then
      SDate := SDate + 1;

    if Trunc(SDate) = Trunc(Now) then
      Result := True;
  end;
end;

class function TSchedule.MatchesStart(S: TSchedule): Boolean;
begin
  Result := False;
  if not MatchesDay(S, False) then
    Exit;

  if (S.StartHour = HourOf(Now)) and (S.StartMinute = MinuteOf(Now)) and
     (SecondOf(Now) = 1) then
  begin
    Result := True;
  end;
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

  if (S.EndHour = HourOf(Now)) and (S.EndMinute = MinuteOf(Now)) and
     (SecondOf(Now) = 0) then
  begin
    Result := True;
  end;
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

{ TChartEntry }

constructor TChartEntry.Create(Name: string; Chance: Integer; Categories: TIntArray);
begin
  inherited Create;

  FName := Name;
  FChance := Chance;
  FCategories := Categories;
end;

constructor TChartEntry.Create;
begin
  inherited;

  SetLength(FCategories, 0);
end;

class function TChartEntry.Load(Stream: TExtendedStream;
  Version: Integer): TChartEntry;
var
  i: Integer;
  C: Cardinal;
begin
  Result := TChartEntry.Create;
  Stream.Read(Result.FName);
  Stream.Read(Result.FChance);

  Stream.Read(C);
  for i := 0 to C - 1 do
  begin
    SetLength(Result.FCategories, Length(Result.FCategories) + 1);
    Stream.Read(Result.FCategories[High(Result.FCategories)]);
  end;
end;

procedure TChartEntry.Save(Stream: TExtendedStream);
var
  i: Integer;
begin
  Stream.Write(FName);
  Stream.Write(FChance);
  Stream.Write(Length(FCategories));
  for i := 0 to High(FCategories) do
    Stream.Write(FCategories[i]);
end;

{ TGenre }

constructor TGenre.Create(Name: string; ID, ChartCount: Cardinal);
begin
  inherited Create;

  FName := Name;
  FID := ID;
  FChartCount := ChartCount;
end;

constructor TGenre.Create;
begin
  inherited;

end;

class function TGenre.Load(Stream: TExtendedStream;
  Version: Integer): TGenre;
begin
  Result := TGenre.Create;

  if Version >= 34 then
  begin
    Stream.Read(Result.FID);
    Stream.Read(Result.FName);
    Stream.Read(Result.FChartCount);
  end else
  begin
    Result.FID := 0;
    Stream.Read(Result.FName);
    Result.FChartCount := 1;
  end;
end;

procedure TGenre.Save(Stream: TExtendedStream);
begin
  Stream.Write(FID);
  Stream.Write(FName);
  Stream.Write(FChartCount);
end;

{ TChartCategory }

constructor TChartCategory.Create(ID: Cardinal; Name: string);
begin
  inherited Create;

  FID := ID;
  FName := Name;
end;

constructor TChartCategory.Create;
begin
  inherited;
end;

class function TChartCategory.Load(Stream: TExtendedStream;
  Version: Integer): TChartCategory;
begin
  Result := TChartCategory.Create;
  Stream.Read(Result.FID);
  Stream.Read(Result.FName);
end;

procedure TChartCategory.Save(Stream: TExtendedStream);
begin
  Stream.Write(FID);
  Stream.Write(FName);
end;

end.


