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

{ This unit contains everything to load and save data from files containing settings.
  It mainly contains methods to load/save stuff from binary data files }
unit DataManager;

interface

uses
  Windows, Classes, SysUtils, ExtendedStream, Generics.Collections,
  ComCtrls, AppData, Functions, Logging, DateUtils, AudioFunctions,
  PowerManagement, Generics.Defaults, ZLib, TypeDefs, winsock;

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
    FServerHash: Cardinal;
    FServerArtistHash: Cardinal;
    FUpdatedToHash: Boolean;
    FSaved: Cardinal;
  public
    constructor Create(ServerHash, ServerArtistHash: Cardinal; Title: string); overload;
    function Copy: TTitleInfo;

    class function Load(Stream: TExtendedStream; Version: Integer): TTitleInfo;
    procedure Save(Stream: TExtendedStream);

    property Title: string read FTitle write FTitle;
    property Added: TDateTime read FAdded write FAdded;
    property Index: Cardinal read FIndex write FIndex;
    property Pattern: string read FPattern;
    property Hash: Cardinal read FHash;
    property ServerHash: Cardinal read FServerHash;
    property ServerArtistHash: Cardinal read FServerArtistHash;
    property UpdatedToHash: Boolean read FUpdatedToHash write FUpdatedToHash;
    property Saved: Cardinal read FSaved write FSaved;
  end;

  TN = procedure(Sender: TObject; const Item: TTitleInfo; Action: TCollectionNotification) of object;

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
    procedure Assign(Source: TStreamBrowserEntry);
    function Copy: TStreamBrowserEntry;

    class function Load(Stream: TExtendedStream; Version: Integer): TStreamBrowserEntry;
    class function LoadFromHome(Stream: TExtendedStream; Version: Integer): TStreamBrowserEntry;
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
    FVBR: Boolean;
    FIndex: Cardinal;
    FServerTitleHash: Cardinal; // TODO: der muss immer gesetzt sein. wird er bei auto-aufnahme über artist gesetzt? sollte so sein, checken!
    FServerArtistHash: Cardinal; // TODO: das selbe gilt hier wie im todo hier drüber, nur umgekehrt.
  public
    constructor Create; overload;
    constructor Create(Time: TDateTime; Filename, Streamname: string; ServerTitleHash, ServerArtistHash: Cardinal); overload;
    procedure Assign(Source: TTrackInfo);
    function Copy: TTrackInfo;

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
    property VBR: Boolean read FVBR write FVBR;
    property ServerTitleHash: Cardinal read FServerTitleHash write FServerTitleHash;
    property ServerArtistHash: Cardinal read FServerArtistHash write FServerArtistHash;
  end;

  TTrackInfoArray = array of TTrackInfo;

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
    FWakeupHandle: Integer;
    FScheduleStarted: TDateTime;

    function GetStartTime: TDateTime;
    function GetEndTime(ScheduleStarted: TDateTime): TDateTime;
    function GetWakeupTime: TDateTime;

    //class function MatchesDay(S: TSchedule; NextDay: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(From: TSchedule);
    function Copy: TSchedule;

    class function Load(Stream: TExtendedStream; Version: Integer): TSchedule;
    procedure Save(Stream: TExtendedStream);

    procedure RemoveWakeup;
    procedure SetWakeup;

    // Does the current time meet the scheduled start-time?
    function MatchesStart: Boolean;
    // Does the current time meet the scheduled end-time?
    function MatchesEnd: Boolean;

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
    property ScheduleStarted: TDateTime read FScheduleStarted write FScheduleStarted;
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
    // Number of seconds the stream was played/recorded
    FSecondsReceived: UInt64;

    // List of configured schedules for this stream
    FSchedules: TScheduleList;

    // List of titles to save for this stream
    FSaveList: TList<TTitleInfo>;
    // List of titles to ignore for this stream
    FIgnoreList: TList<TTitleInfo>;
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
    property SecondsReceived: UInt64 read FSecondsReceived write FSecondsReceived;

    property Schedules: TScheduleList read FSchedules;

    property SaveList: TList<TTitleInfo> read FSaveList;
    property IgnoreList: TList<TTitleInfo> read FIgnoreList;
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
    procedure Assign(Source: TChartCategory);
    function Copy: TChartCategory;

    class function Load(Stream: TExtendedStream; Version: Integer): TChartCategory;
    procedure Save(Stream: TExtendedStream);

    property ID: Cardinal read FID;
    property Name: string read FName;
  end;

  // An entry within TChartEntry.Streams
  TChartStream = class
  private
    FID: Cardinal;
    FStream: TStreamBrowserEntry;
    FPlayedLastDay: Cardinal;
    FPlayedLastWeek: Cardinal;
    FPlayedLast: Cardinal;
  public
    constructor Create(ID, PlayedLastDay, PlayedLastWeek, PlayedLast: Cardinal);
    function Copy: TChartStream;

    class function Load(Stream: TExtendedStream; Version: Integer): TChartStream;

    property ID: Cardinal read FID;
    property Stream: TStreamBrowserEntry read FStream write FStream;
    property PlayedLastDay: Cardinal read FPlayedLastDay;
    property PlayedLastWeek: Cardinal read FPlayedLastWeek;
    property PlayedLast: Cardinal read FPlayedLast;
  end;

  TStreamBrowserList = class(TList<TStreamBrowserEntry>)
  private
    FDict: TDictionary<Cardinal, TStreamBrowserEntry>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CreateDict;
    procedure ClearDict;
    function GetStream(ID: Cardinal): TStreamBrowserEntry;
  end;

  // An entry within the "Charts"-window
  TChartEntry = class
  private
    FName: string;
    FArtist: string;
    FServerHash: Cardinal;
    FServerArtistHash: Cardinal;
    FPlayedLastDay: Cardinal;
    FPlayedLastWeek: Cardinal;
    FPlayedLast: Cardinal;
    FCategories: TIntArray;
    FStreams: TList<TChartStream>;
  public
    constructor Create; overload;
    constructor Create(Name: string; PlayedLastDay, PlayedLastWeek: Cardinal; Categories: TIntArray; Streams: TList<TChartStream>); overload;
    destructor Destroy; override;

    procedure Assign(Source: TChartEntry);
    function Copy: TChartEntry;

    class function Load(Stream: TExtendedStream; Lists: TDataLists; Version: Integer): TChartEntry;
    class function LoadFromHome(Stream: TExtendedStream; Version: Integer): TChartEntry;
    //procedure Save(Stream: TExtendedStream);

    procedure LoadStreams(StreamList: TStreamBrowserList);

    // The name
    property Name: string read FName;
    property Artist: string read FArtist;
    property ServerHash: Cardinal read FServerHash;
    property ServerArtistHash: Cardinal read FServerArtistHash;

    property PlayedLastDay: Cardinal read FPlayedLastDay;
    property PlayedLastWeek: Cardinal read FPlayedLastWeek;
    property PlayedLast: Cardinal read FPlayedLast;

    // Categories this title is included (i.e. "Top 100")
    property Categories: TIntArray read FCategories;
    property Streams: TList<TChartStream> read FStreams;
  end;

  TChartList = class(TList<TChartEntry>)
  end;

  TGenre = class
  private
    FID: Cardinal;
    FName: string;
  public
    constructor Create; overload;
    constructor Create(Name: string; ID: Cardinal); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TGenre;
    class function LoadFromHome(Stream: TExtendedStream; Version: Integer): TGenre;
    procedure Save(Stream: TExtendedStream);

    property ID: Cardinal read FID write FID;
    property Name: string read FName write FName;
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
    FSaveList: TList<TTitleInfo>;
    FIgnoreList: TList<TTitleInfo>;
    FRecentList: TRecentList;
    FStreamBlacklist: TStringList;
    FRatingList: TRatingList;
    //FChartList: TChartList;
    //FChartCategoryList: TChartCategoryList;
    FBrowserList: TStreamBrowserList;
    FGenreList: TGenreList;
    FLoadError: Boolean;
    FReceived: UInt64;
  public
    // TODO: irgendwann sollte das eine property werden...
    SavedTitleHashes: TCardinalArray;

    constructor Create;
    destructor Destroy; override;

    // Cleans all lists and frees all their items
    procedure CleanLists;
    procedure Load; overload;
    procedure Load(var S: TExtendedStream); overload;
    procedure Save; overload;
    procedure Save(S: TExtendedStream; UseCompression: Boolean); overload;
    procedure SaveRecover;

    // List that contains all categories
    property CategoryList: TListCategoryList read FCategoryList;
    // List that contains all streams
    property StreamList: TStreamList read FStreamList;
    // List that contains all saved tracks
    property TrackList: TTrackList read FTrackList;
    // List that contains all titles to be saved
    property SaveList: TList<TTitleInfo> read FSaveList;
    // List that contains all titles to be ignored
    property IgnoreList: TList<TTitleInfo> read FIgnoreList;
    // List that contains all recently used streams
    property RecentList: TRecentList read FRecentList;
    // List that contains all streams to blacklist for automatic recordings
    property StreamBlacklist: TStringList read FStreamBlacklist;
    // List that contains all ratings for streams
    property RatingList: TRatingList read FRatingList;
    // List that contains charts
    //property ChartList: TChartList read FChartList;
    // List that contains categories for charts
    //property ChartCategoryList: TChartCategoryList read FChartCategoryList;
    // List that contains all TStreamBrowserEntries
    property BrowserList: TStreamBrowserList read FBrowserList write FBrowserList;
    // List that contains all genres
    property GenreList: TGenreList read FGenreList write FGenreList;

    // When set an error occured while loading the data-file
    property LoadError: Boolean read FLoadError write FLoadError;
    // Overall amount of data received
    property Received: UInt64 read FReceived write FReceived;
  end;

const
  DATAVERSION = 51;

implementation

{ TTitleInfo }

constructor TTitleInfo.Create(ServerHash, ServerArtistHash: Cardinal; Title: string);
var
  NumChars: Integer;
  Hash: Cardinal;
  Pattern: string;
begin
  inherited Create;

  FServerHash := ServerHash;
  FServerArtistHash := ServerArtistHash;
  FTitle := Title;
  FAdded := Now;

  Pattern := BuildPattern(Title, Hash, NumChars, False);
  FPattern := Pattern;
  FHash := Hash;

  // Default True. Im Load() wird es ggf. auf False gesetzt.
  FUpdatedToHash := True;
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

  if Version > 46 then
    Stream.Read(Result.FServerHash);
  if Version > 49 then
    Stream.Read(Result.FServerArtistHash);

  if Version > 50 then
    Stream.Read(Result.FSaved);

  if Version > 48 then
    Stream.Read(Result.FUpdatedToHash)
  else
    Result.FUpdatedToHash := (Result.ServerHash > 0) or (Result.ServerArtistHash > 0);
end;

procedure TTitleInfo.Save(Stream: TExtendedStream);
begin
  Stream.Write(FTitle);
  Stream.Write(FAdded);
  Stream.Write(FIndex);
  Stream.Write(FPattern);
  Stream.Write(FHash);
  Stream.Write(FServerHash);
  Stream.Write(FServerArtistHash);
  Stream.Write(FSaved);
  Stream.Write(FUpdatedToHash);
end;

function TTitleInfo.Copy: TTitleInfo;
begin
  Result := TTitleInfo.Create;
  Result.FTitle := FTitle;
  Result.FAdded := FAdded;
  Result.FIndex := FIndex;
  Result.FPattern := FPattern;
  Result.FHash := FHash;
  Result.FServerHash := FServerHash;
  Result.FServerArtistHash := FServerArtistHash;
  Result.FUpdatedToHash := FUpdatedToHash;
  Result.Saved := FSaved;
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
  FSecondsReceived := From.FSecondsReceived;
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

  FSaveList := TList<TTitleInfo>.Create;
  FIgnoreList := TList<TTitleInfo>.Create;
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
    if Result.FSettings.RetryDelay > 999 then
      Result.FSettings.RetryDelay := 999;
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

  if Version >= 46 then
    Stream.Read(Result.FSecondsReceived);

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
  Stream.Write(FSecondsReceived);

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

  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Free;
  FSaveList.Clear;

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

  {
  for i := 0 to FChartList.Count - 1 do
    FChartList[i].Free;
  FChartList.Clear;

  for i := 0 to FChartCategoryList.Count - 1 do
    FChartCategoryList[i].Free;
  FChartCategoryList.Clear;
  }

  for i := 0 to FGenreList.Count - 1 do
    FGenreList[i].Free;
  FGenreList.Clear;
end;

constructor TDataLists.Create;
begin
  inherited;

  SetLength(SavedTitleHashes, 0);

  FLoadError := False;
  FReceived := 0;
  FCategoryList := TListCategoryList.Create;
  FStreamList := TStreamList.Create;
  FTrackList := TTrackList.Create;
  FSaveList := TList<TTitleInfo>.Create;
  FIgnoreList := TList<TTitleInfo>.Create;
  FRecentList := TRecentList.Create;
  FStreamBlacklist := TStringList.Create;
  FRatingList := TRatingList.Create;
  FBrowserList := TStreamBrowserList.Create;
  FGenreList := TGenreList.Create;
  //FChartList := TChartList.Create;
  //FChartCategoryList := TChartCategoryList.Create;
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
  //FChartList.Free;
  //FChartCategoryList.Free;

  inherited;
end;

procedure TDataLists.Load(var S: TExtendedStream);
var
  Entry: TStreamEntry;
  TitleInfo: TTitleInfo;
  TrackInfo: TTrackInfo;
  Str: string;
  Version, CatCount, EntryCount: Integer;
  i: Integer;
  CompressedStream: TExtendedStream;
  Compressed: Boolean;
  Chart: TChartEntry;
  ChartCategory: TChartCategory;
  TitleCount: Cardinal;
begin
  CleanLists;

  S.Read(Version);

  if Version > DATAVERSION then
    raise EVersionException.Create(AppGlobals.DataFile);

  // Bei 44 war Kompression immer aktiv, danach optional
  if Version >= 44 then
    Compressed := True
  else
    Compressed := False;

  // Ab 45 für Recoveryfile abgeschaltet, darum als Feld gespeichert
  if Version >= 45 then
    S.Read(Compressed);

  if Compressed then
  begin
    CompressedStream := TExtendedStream.Create;
    try
      ZDecompressStream(S, CompressedStream);

      S.Size := 0;
      CompressedStream.Seek(0, soFromBeginning);
      S.CopyFrom(CompressedStream, CompressedStream.Size);
      S.Seek(0, soFromBeginning);
    finally
      CompressedStream.Free;
    end;
  end;

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
    if Version < 48 then
    begin
      S.Read(CatCount);
      for i := 0 to CatCount - 1 do
      begin
        ChartCategory := TChartCategory.Load(S, Version);
        ChartCategory.Free;
      end;
    end;

    FBrowserList.CreateDict;

    if Version < 48 then
    begin
      S.Read(CatCount);
      for i := 0 to CatCount - 1 do
      begin
        Chart := TChartEntry.Load(S, Self, Version);
        Chart.Free;
      end;
    end;
  end;

  if Version >= 51 then
  begin
    S.Read(TitleCount);
    SetLength(SavedTitleHashes, TitleCount);
    for i := 0 to TitleCount - 1 do
    begin
      S.Read(SavedTitleHashes[i]);
    end;
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

procedure TDataLists.Save(S: TExtendedStream; UseCompression: Boolean);
var
  i: Integer;
  CompressedStream: TExtendedStream;
begin
  S.Write(Integer(DATAVERSION));

  S.Write(UseCompression);

  CompressedStream := TExtendedStream.Create;
  try
    CompressedStream.Write(FReceived);

    CompressedStream.Write(FCategoryList.Count);
    for i := 0 to FCategoryList.Count - 1 do
      FCategoryList[i].Save(CompressedStream);

    CompressedStream.Write(FStreamList.Count);
    for i := 0 to FStreamList.Count - 1 do
    begin
      FStreamList[i].Save(CompressedStream);
    end;

    CompressedStream.Write(FTrackList.Count);
    for i := 0 to FTrackList.Count - 1 do
      FTrackList[i].Save(CompressedStream);

    CompressedStream.Write(FSaveList.Count);
    for i := 0 to FSaveList.Count - 1 do
    begin
      FSaveList[i].Save(CompressedStream);
    end;

    CompressedStream.Write(FIgnoreList.Count);
    for i := 0 to FIgnoreList.Count - 1 do
    begin
      FIgnoreList[i].Save(CompressedStream);
    end;

    CompressedStream.Write(FRecentList.Count);
    for i := 0 to FRecentList.Count - 1 do
      FRecentList[i].Save(CompressedStream);

    CompressedStream.Write(FStreamBlacklist.Count);
    for i := 0 to FStreamBlacklist.Count - 1 do
      CompressedStream.Write(FStreamBlacklist[i]);

    CompressedStream.Write(FBrowserList.Count);
    for i := 0 to FBrowserList.Count - 1 do
      FBrowserList[i].Save(CompressedStream);

    CompressedStream.Write(FGenreList.Count);
    for i := 0 to FGenreList.Count - 1 do
      FGenreList[i].Save(CompressedStream);

    CompressedStream.Write(Cardinal(Length(SavedTitleHashes)));
    for i := 0 to High(SavedTitleHashes) do
      CompressedStream.Write(SavedTitleHashes[i]);

    CompressedStream.Seek(0, soFromBeginning);

    if UseCompression then
    begin
      {$IFDEF DEBUG}
      ZCompressStream(CompressedStream, S, zcNone);
      {$ELSE}
      ZCompressStream(CompressedStream, S, zcDefault);
      {$ENDIF}
    end else
      S.CopyFrom(CompressedStream, CompressedStream.Size);
  finally
    CompressedStream.Free;
  end;
end;

procedure TDataLists.SaveRecover;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    Save(S, False);
    S.SaveToFile(AppGlobals.RecoveryFile);
  finally
    S.Free;
  end;
end;

procedure TDataLists.Save;
var
  S: TExtendedStream;
begin
  DeleteFile(AppGlobals.RecoveryFile);

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
      Save(S, True);
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

procedure TTrackInfo.Assign(Source: TTrackInfo);
begin
  FTime := Source.Time;
  FFilename := Source.Filename;
  FStreamname := Source.Streamname;
  FFilesize := Source.Filesize;
  FLength := Source.Length;
  FWasCut := Source.WasCut;
  FBitRate := Source.BitRate;
  FIsAuto := Source.IsAuto;
  FIsStreamFile := Source.IsStreamFile;
  FFinalized := Source.Finalized;
  FVBR := Source.VBR;
  FIndex := Source.Index;
  FServerTitleHash := Source.ServerTitleHash;
  FServerArtistHash := Source.ServerArtistHash;
end;

function TTrackInfo.Copy: TTrackInfo;
begin
  Result := TTrackInfo.Create;
  Result.Assign(Self);
end;

constructor TTrackInfo.Create(Time: TDateTime; Filename, Streamname: string; ServerTitleHash, ServerArtistHash: Cardinal);
begin
  inherited Create;

  FTime := Time;
  FFilename := Filename;
  FStreamname := Streamname;
  FWasCut := False;
  FIsStreamFile := False;
  FServerTitleHash := ServerTitleHash;
  FServerArtistHash := ServerArtistHash;
end;

class function TTrackInfo.Load(Stream: TExtendedStream;
  Version: Integer): TTrackInfo;
begin
  Result := TTrackInfo.Create;

  Stream.Read(Result.FFilename);

  Result.FFilename := TryUnRelativePath(Result.FFilename);

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

  if Version >= 41 then
    Stream.Read(Result.FVBR);

  if Version > 50 then
  begin
    Stream.Read(Result.FServerTitleHash);
    Stream.Read(Result.FServerArtistHash);
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
  Stream.Write(FIndex);
  Stream.Write(FVBR);
  Stream.Write(FServerTitleHash);
  Stream.Write(FServerArtistHash);
end;

{ TStreamList }

function TStreamList.Add(Entry: TStreamEntry): TStreamEntry;
begin
  // Der Name ist hier leer, da es Streams mit selben Namen
  // aber anderen URLs gibt (z.B. "Die Neue 107.7")...

  // Ausserdem glaube ich, dass das hier über ist. Doppelte Streams
  // lassen sich nämlich nicht der Liste hinzufügen, deshalb
  // dürfte das heir nichts bringen...
  {
  Result := Get('', Entry.StartURL, Entry.URLs);

  if Result <> nil then
  begin
    Exit;
  end;
  }
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
  RemoveWakeup;

  inherited;
end;

function TSchedule.GetEndTime(ScheduleStarted: TDateTime): TDateTime;
begin
  Result := 0;
  if FActive then
  begin
    Result := ScheduleStarted;
    if ((FEndHour < FStartHour) or ((FEndHour = FStartHour) and (FEndMinute < FStartMinute))) then
      Result := IncDay(Result);

    Result := RecodeHour(Result, FEndHour);
    Result := RecodeMinute(Result, FEndMinute);

    if (Result > 0) and (Now > Result) then
      Result := 0;
  end;
end;

function TSchedule.GetStartTime: TDateTime;
var
  N: TDateTime;
begin
  Result := 0;
  if FActive then
  begin
    N := Now;
    case FInterval of
      siDaily:
        begin
          if ((FStartHour = HourOf(N)) and (FStartMinute > MinuteOf(N)) or
              (FStartHour > HourOf(N)))
          then
            Result := EncodeDateTime(YearOf(N), MonthOf(N), DayOf(N), FStartHour, FStartMinute, 0, 0)
          else
          begin
            Result := EncodeDateTime(YearOf(N), MonthOf(N), DayOf(IncDay(N)), FStartHour, FStartMinute, 0, 0);
          end;
        end;
      siWeekly:
        begin
          Result := EncodeDateTime(YearOf(N), MonthOf(N), DayOf(N), FStartHour, FStartMinute, 0, 0);
          if DayOfTheWeek(N) <= Integer(FDay) + 1 then
          begin
            Result := IncDay(Result, (Integer(FDay) + 1 - DayOfTheWeek(N)));
            if Result < N then
              Result := IncWeek(Result);
          end else
          begin
            Result := IncDay(Result, 7 - DayOfTheWeek(N) + Integer(FDay) + 1);
          end;
        end;
      siNone:
        Result := EncodeDateTime(YearOf(FDate), MonthOf(FDate), DayOf(FDate), FStartHour, FStartMinute, 0, 0)
    end;

    if (Result > 0) and (N > Result) then
      Result := 0;
  end;
end;

function TSchedule.GetWakeupTime: TDateTime;
begin
  Result := GetStartTime;
  if (Result > 0) and (Now < IncMinute(Result, -1)) then
    Result := IncMinute(Result, -1)
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

function TSchedule.MatchesEnd: Boolean;
begin
  Result := False;
  if ScheduleStarted > 0 then
    Result := WithinPastSeconds(Now, IncSecond(GetEndTime(ScheduleStarted), -30), 30);
end;

function TSchedule.MatchesStart: Boolean;
begin
  Result := WithinPastSeconds(Now, IncSecond(GetStartTime, 30), 30);
end;

procedure TSchedule.RemoveWakeup;
begin
  if FWakeupHandle > 0 then
  begin
    Power.RemoveWakeup(FWakeupHandle);
    FWakeupHandle := 0;
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

procedure TSchedule.SetWakeup;
var
  WT: TDateTime;
begin
  if FWakeupHandle > 0 then
  begin
    Power.RemoveWakeup(FWakeupHandle);
    FWakeupHandle := 0;
  end;

  WT := GetWakeupTime;
  if WT > 0 then
  begin
    try
      FWakeupHandle := Power.AddWakeup(WT);
    except
    end;
  end;
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

procedure TStreamBrowserEntry.Assign(Source: TStreamBrowserEntry);
begin
  FID := Source.FID;
  FName := Source.FName;
  FGenre := Source.FGenre;
  FURL := Source.FURL;
  FWebsite := Source.FWebsite;
  FBitRate := Source.FBitRate;
  FAudioType := Source.FAudioType;
  FMetaData := Source.FMetaData;
  FChangesTitleInSong := Source.FChangesTitleInSong;
  FOwnRating := Source.FOwnRating;
  FRating := Source.FRating;
  FRecordingOkay := Source.FRecordingOkay;
  FRegEx := Source.FRegEx;
  FIgnoreTitles.Assign(Source.FIgnoreTitles);
end;

function TStreamBrowserEntry.Copy: TStreamBrowserEntry;
begin
  Result := TStreamBrowserEntry.Create;
  Result.Assign(Self);
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

class function TStreamBrowserEntry.LoadFromHome(Stream: TExtendedStream;
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
  Stream.Read(Result.FRating);
  Stream.Read(Result.FRecordingOkay);
  Stream.Read(Result.FRegEx);

  Stream.Read(Count);
  for i := 0 to Count - 1 do
  begin
    Stream.Read(E);
    Result.FIgnoreTitles.Add(E);
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

procedure TChartEntry.Assign(Source: TChartEntry);
var
  i: Integer;
begin
  FName := Source.Name;
  FArtist := Source.Artist;
  FServerHash := Source.ServerHash;
  FServerArtistHash := Source.FServerArtistHash;
  FPlayedLastDay := Source.PlayedLastDay;
  FPlayedLastWeek := Source.PlayedLastWeek;
  FPlayedLast := Source.FPlayedLast;
  FCategories := Source.Categories;

  if FStreams <> nil then
  begin
    for i := 0 to FStreams.Count - 1 do
      FStreams[i].Free;
    FStreams.Clear;
  end else
    FStreams := TList<TChartStream>.Create;
  for i := 0 to Source.Streams.Count - 1 do
    FStreams.Add(Source.Streams[i].Copy);
end;

function TChartEntry.Copy: TChartEntry;
begin
  Result := TChartEntry.Create;
  Result.Assign(Self);
end;

constructor TChartEntry.Create(Name: string; PlayedLastDay, PlayedLastWeek: Cardinal; Categories: TIntArray; Streams: TList<TChartStream>);
begin
  inherited Create;

  FName := Name;
  FPlayedLastDay := PlayedLastDay;
  FPlayedLastWeek := PlayedLastWeek;
  FCategories := Categories;

  // Hier wird direkt zugewiesen - keine Kopie oder so!
  FStreams := Streams;
end;

destructor TChartEntry.Destroy;
var
  i: Integer;
begin
  for i := 0 to FStreams.Count - 1 do
  begin
    FStreams[i].Stream.Free;
    FStreams[i].Free;
  end;
  FStreams.Free;

  inherited;
end;

constructor TChartEntry.Create;
begin
  inherited;

  SetLength(FCategories, 0);
  FStreams := TList<TChartStream>.Create;
end;

// Diese Methode ist ALT! Wird nur noch verwendet, dass der Lesevorgang von alten Daten-Dateien
// ordentlich abläuft!! Iiiiiiiirgendwann kann das wohl raus.
class function TChartEntry.Load(Stream: TExtendedStream;
  Lists: TDataLists; Version: Integer): TChartEntry;
var
  i, Dummy: Integer;
  C: Cardinal;
begin
  Result := TChartEntry.Create;
  Stream.Read(Result.FName);

  if Version > 46 then
    Stream.Read(Result.FServerHash);

  if Version <= 42 then
  begin
    Stream.Read(Dummy);
  end else
  begin
    Stream.Read(Result.FPlayedLastDay);
    Stream.Read(Result.FPlayedLastWeek);
  end;

  Stream.Read(C);
  for i := 0 to C - 1 do
  begin
    SetLength(Result.FCategories, Length(Result.FCategories) + 1);
    Stream.Read(Result.FCategories[High(Result.FCategories)]);
  end;

  if Version > 42 then
  begin
    Stream.Read(C);
    for i := 0 to C - 1 do
    begin
      Result.Streams.Add(TChartStream.Load(Stream, Version));
      Result.LoadStreams(Lists.BrowserList);
    end;
  end;
end;

class function TChartEntry.LoadFromHome(Stream: TExtendedStream; Version: Integer): TChartEntry;
var
  i: Integer;
  C: Cardinal;
begin
  Result := TChartEntry.Create;
  Stream.Read(Result.FServerHash);
  Stream.Read(Result.FServerArtistHash);
  Stream.Read(Result.FName);
  Stream.Read(Result.FArtist);

  Stream.Read(Result.FPlayedLastDay);
  Stream.Read(Result.FPlayedLastWeek);
  Stream.Read(Result.FPlayedLast);

  Stream.Read(C);
  for i := 0 to C - 1 do
    Result.Streams.Add(TChartStream.Load(Stream, Version));
end;

procedure TChartEntry.LoadStreams(StreamList: TStreamBrowserList);
var
  i: Integer;
  Stream: TStreamBrowserEntry;
begin
  for i := Streams.Count - 1 downto 0 do
  begin
    Stream := StreamList.GetStream(Streams[i].FID);
    if Stream <> nil then
      Streams[i].Stream := Stream.Copy
    else
    begin
      Streams[i].Free;
      Streams.Delete(i);
    end;
  end;
end;

{ TGenre }

constructor TGenre.Create(Name: string; ID: Cardinal);
begin
  inherited Create;

  FName := Name;
  FID := ID;
end;

constructor TGenre.Create;
begin
  inherited;

end;

class function TGenre.Load(Stream: TExtendedStream;
  Version: Integer): TGenre;
var
  Dummy: Cardinal;
begin
  Result := TGenre.Create;

  if Version >= 34 then
  begin
    Stream.Read(Result.FID);
    Stream.Read(Result.FName);
    Stream.Read(Dummy); // Das war mal ChartCount. Ich sollte Data version irgendwann erhöhen und das kicken.
  end else
  begin
    Result.FID := 0;
    Stream.Read(Result.FName);
  end;
end;

class function TGenre.LoadFromHome(Stream: TExtendedStream;
  Version: Integer): TGenre;
begin
  Result := TGenre.Create;

  Stream.Read(Result.FID);
  Stream.Read(Result.FName);
end;

procedure TGenre.Save(Stream: TExtendedStream);
begin
  Stream.Write(FID);
  Stream.Write(FName);
  Stream.Write(Cardinal(0));
end;

{ TChartCategory }

procedure TChartCategory.Assign(Source: TChartCategory);
begin
  FID := Source.ID;
  FName := Source.Name;
end;

function TChartCategory.Copy: TChartCategory;
begin
  Result := TChartCategory.Create;
  Result.Assign(Self);
end;

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

{ TChartStream }

function TChartStream.Copy: TChartStream;
begin
  Result := TChartStream.Create(FID, PlayedLastDay, PlayedLastWeek, PlayedLast);
end;

constructor TChartStream.Create(ID, PlayedLastDay, PlayedLastWeek, PlayedLast: Cardinal);
begin
  FID := ID;
  FPlayedLastDay := PlayedLastDay;
  FPlayedLastWeek := PlayedLastWeek;
  FPlayedLast := PlayedLast;
end;

class function TChartStream.Load(Stream: TExtendedStream;
  Version: Integer): TChartStream;
begin
  Result := TChartStream.Create(0, 0, 0, 0);
  Stream.Read(Result.FID);
  Stream.Read(Result.FPlayedLastDay);
  Stream.Read(Result.FPlayedLastWeek);
  if (Version >= 50) or (Version = 1) then
    Stream.Read(Result.FPlayedLast);
end;

{ TStreamBrowserList }

procedure TStreamBrowserList.ClearDict;
begin
  FDict.Clear;
end;

constructor TStreamBrowserList.Create;
begin
  inherited;

  FDict := TDictionary<Cardinal, TStreamBrowserEntry>.Create;
end;

procedure TStreamBrowserList.CreateDict;
var
  i: Integer;
begin
  FDict.Clear;
  for i := 0 to Count - 1 do
    FDict.Add(Items[i].FID, Items[i]);
end;

destructor TStreamBrowserList.Destroy;
begin
  FDict.Free;

  inherited;
end;

function TStreamBrowserList.GetStream(ID: Cardinal): TStreamBrowserEntry;
begin
  Result := nil;

  if FDict.ContainsKey(ID) then
    Result := FDict[ID];
end;

end.


