{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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
  AddonManager,
  AudioFunctions,
  bufstream,
  Classes,
  ComCtrls,
  Constants,
  DateUtils,
  Functions,
  Generics.Collections,
  Generics.Defaults,
  Logging,
  PostProcess,
  PowerManagement,
  StreamHelper,
  SWFunctions,
  SysUtils,
  TypeDefs,
  Windows,
  ZStream;

type
  TStreamList = class;
  TDataLists = class;
  TListCategory = class;

  // Do not change the order of items in the following enums!
  // Definitions for scheduling intervals. siNone has to be the last element!
  TScheduleInterval = (siDaily, siWeekly, siNone);
  // A specific day for a schedule. sdNone has to be the last element!
  TScheduleDay = (sdMonday, sdTuesday, sdWednesday, sdThursday, sdFriday, sdSaturday, sdSunday, sdNone);
  // Set for definition of filters to use
  TUseFilters = (ufNone, ufWish, ufIgnoreGlobal, ufIgnoreLocal, ufIgnoreBoth, ufBoth);
  // Definitions for directions for where to adjust the track-offset
  TTrackOffsetDirection = (toForward, toBackward);

  { This exception is raised when streamWriter tries to load a file saved with
    a newer file-format that it does not know (because it is an old version) }
  EVersionException = class(Exception);

  EUnknownFormatException = class(Exception);

  EUnsupportedFormatException = class(Exception);

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

    class function Load(Stream: TStream; Version: Integer): TTitleInfo;
    procedure Save(Stream: TMemoryStream);

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
    FURLs: TStringList;
    FWebsite: string;
    FBitrate: Integer;
    FAudioType: TAudioTypes;
    FMetaData: Boolean;
    FChangesTitleInSong: Boolean;
    FOwnRating: Byte;
    FRating: Byte;
    FRecordingOkay: Boolean;
    FRegExes: TStringList;
    FIgnoreTitles: TStringList;
    FCanSetRegExps: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TStreamBrowserEntry);
    function Copy: TStreamBrowserEntry;

    class function Load(Stream: TStream; Version: Integer): TStreamBrowserEntry;
    class function LoadFromHome(Stream: TMemoryStream; Version: Integer): TStreamBrowserEntry;
    procedure Save(Stream: TMemoryStream);

    // The unique ID of the stream
    property ID: Integer read FID write FID;
    // The broadcasted name of the stream
    property Name: string read FName write FName;
    // The broadcasted genre(s) of the stream (might be "rock,punk  , gothic")
    property Genre: string read FGenre write FGenre;
    // The URL to the stream
    property URL: string read FURL write FURL;
    property URLs: TStringList read FURLs write FURLs;
    // The broadcasted URL to the stream's website
    property Website: string read FWebsite write FWebsite;
    { The broadcasted bitrate of the stream. This value might get overriden
      when streamWriter parses received audio data }
    property Bitrate: Integer read FBitrate write FBitrate;
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
    property RegExes: TStringList read FRegExes write FRegExes;
    // A list containing titles to ignore for track changes
    property IgnoreTitles: TStringList read FIgnoreTitles;
    property CanSetRegExps: Boolean read FCanSetRegExps;
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
    FBitrate: Cardinal;
    FIsAuto: Boolean;
    FIsStreamFile: Boolean;
    FFinalized: Boolean;
    FVBR: Boolean;
    FIndex: Cardinal;
    FServerTitle: string;
    FSongArtist: string;
    FSongTitle: string;
    FServerTitleHash: Cardinal;
    FServerArtistHash: Cardinal;
    FRecordBecauseArtist: Boolean;

    function FGetParsedTitle: string;
  public
    constructor Create; overload;
    constructor Create(Time: TDateTime; Filename, Streamname, ServerTitle, Artist, Title: string; ServerTitleHash, ServerArtistHash: Cardinal); overload;
    procedure Assign(Source: TTrackInfo);
    function Copy: TTrackInfo;

    class function Load(Stream: TStream; Version: Integer): TTrackInfo;
    procedure Save(Stream: TMemoryStream);

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
    property Bitrate: Cardinal read FBitrate write FBitrate;
    // When set the file was automatically recorded from the wishlist
    property IsAuto: Boolean read FIsAuto write FIsAuto;
    // When set this is a stream-file (not a single saved file from a stream with artist/title)
    property IsStreamFile: Boolean read FIsStreamFile write FIsStreamFile;
    // When set the file was post-processed by the user
    property Finalized: Boolean read FFinalized write FFinalized;
    property Index: Cardinal read FIndex write FIndex;
    property VBR: Boolean read FVBR write FVBR;
    property SongArtist: string read FSongArtist write FSongArtist;
    property SongTitle: string read FSongTitle write FSongTitle;
    property ServerTitle: string read FServerTitle write FServerTitle;
    property ServerTitleHash: Cardinal read FServerTitleHash write FServerTitleHash;
    property ServerArtistHash: Cardinal read FServerArtistHash write FServerArtistHash;
    property RecordBecauseArtist: Boolean read FRecordBecauseArtist write FRecordBecauseArtist;
    property ParsedTitle: string read FGetParsedTitle;
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

    class function Load(Stream: TStream; Version: Integer): TListCategory;
    procedure Save(Stream: TMemoryStream);

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

    class function Load(Stream: TStream; Version: Integer): TRecentEntry;
    procedure Save(Stream: TMemoryStream);

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

    class function Load(Stream: TStream; Version: Integer): TRatingEntry;
    procedure Save(Stream: TMemoryStream);

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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(From: TSchedule);
    function Copy: TSchedule;

    class function Load(Stream: TStream; Version: Integer): TSchedule;
    procedure Save(Stream: TMemoryStream);

    function GetStartTime(ModificationAllowed: Boolean): TDateTime;
    function GetEndTime(ScheduleStarted: TDateTime): TDateTime;

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
  end;

  TEncoderSettings = class
  private
    function FGetHash: Cardinal;
  public
    AudioType: TAudioTypes;
    BitrateType: TBitrates;
    CBRBitrate: Integer;
    VBRQuality: TVBRQualities;

    constructor Create(AudioType: TAudioTypes; BitrateType: TBitrates; VBRQuality: TVBRQualities);
    procedure Load(Stream: TStream; Version: Integer); overload;
    procedure Save(Stream: TMemoryStream); overload;
    procedure Assign(From: TEncoderSettings);
    function Copy: TEncoderSettings;

    property Hash: Cardinal read FGetHash;
  end;

  // A list of items of TSchedule
  TScheduleList = TList<TSchedule>;

  TPostProcessorList = class(TList<TPostProcessBase>)
  private
    function FGetHash: Cardinal;
  public
    function Find(PostProcessor: TPostProcessBase): TPostProcessBase; overload;
    function Find(ClassType: TClass): TPostProcessBase; overload;
    function Find(PostProcessType: TPostProcessTypes): TPostProcessBase; overload;

    property Hash: Cardinal read FGetHash;
  end;

  TEncoderSettingsList = class(TList<TEncoderSettings>)
  private
    function FGetHash: Cardinal;
  public
    function Find(AudioType: TAudioTypes): TEncoderSettings;

    property Hash: Cardinal read FGetHash;
  end;

  { This class defines stream-specific settings. It is used in the settings (AppData) for general
    settings, it is also used in every TStreamEntry which defines configuration of a specific stream }
  TStreamSettings = class
  private
    FFilePattern: string;
    FIncompleteFilePattern: string;
    FStreamFilePattern: string;
    FFilePatternDecimals: Cardinal;
    FRemoveChars: string;
    FNormalizeVariables: Boolean;
    FDeleteStreams: Boolean;
    FAddSavedToIgnore: Boolean;
    FAddSavedToStreamIgnore: Boolean;
    FRemoveSavedFromWishlist: Boolean;
    FSkipShort: Boolean;
    FSearchSilence: Boolean;
    FAutoDetectSilenceLevel: Boolean;
    FSilenceLevel: Cardinal;
    FSilenceLength: Cardinal;
    FSilenceBufferSecondsStart: Integer;
    FSilenceBufferSecondsEnd: Integer;
    FShortLengthSeconds: Integer;
    FSongBuffer: Integer;
    FAdjustTrackOffset: Boolean;
    FAdjustTrackOffsetMS: Cardinal;
    FAdjustTrackOffsetDirection: TTrackOffsetDirection;
    FMaxRetries: Integer;
    FRetryDelay: Cardinal;
    FFilter: TUseFilters;
    FSeparateTracks: Boolean;
    FSaveToMemory: Boolean;
    FOnlySaveFull: Boolean;
    FOverwriteSmaller: Boolean;
    FDiscardSmaller: Boolean;
    FDiscardAlways: Boolean;
    FOutputFormat: TAudioTypes;
    FRegExes: TStringList;
    FIgnoreTrackChangePattern: TStringList;
    FPostProcessors: TPostProcessorList;
    FEncoderSettings: TEncoderSettingsList;

    procedure FSetSaveToMemory(Value: Boolean);
  public
    // Creates a new instance of TStreamSettings class
    constructor Create;
    // Destroys this instance of TStreamSettings
    destructor Destroy; override;

    class function GetDefaults: TStreamSettings;
    class procedure ApplyAutoDefaults(Data: TDataLists; S: TStreamSettings);

    // Loads an instance of TStreamSettings from a stream
    class function Load(Stream: TStream; Version: Integer): TStreamSettings;
    class function LoadAuto(Data: TDataLists; Stream: TStream; Version: Integer): TStreamSettings;
    // Saves this instance of TStreamSettings to a stream
    procedure Save(Stream: TMemoryStream);
    procedure SaveAuto(Stream: TMemoryStream);
    // Assigns this instance of TStreamSettings to From
    procedure Assign(From: TStreamSettings);
    // Copies this instance of TStreamSettings
    function Copy: TStreamSettings;

    // The patterns (Regexes) to detect artist/title/album from broadcasted titles
    property RegExes: TStringList read FRegExes write FRegExes;
    // The pattern for recorded files
    property FilePattern: string read FFilePattern write FFilePattern;
    // The pattern for incompletely recorded files
    property IncompleteFilePattern: string read FIncompleteFilePattern write FIncompleteFilePattern;
    // The pattern for stream-files
    property StreamFilePattern: string read FStreamFilePattern write FStreamFilePattern;
    // Minimum number of decimals for %n (Tracknumber) in filenames
    property FilePatternDecimals: Cardinal read FFilePatternDecimals write FFilePatternDecimals;
    // Chars to remove from filenames of recorded songs
    property RemoveChars: string read FRemoveChars write FRemoveChars;
    // When set variables get normalized (i.e. %artist% = ArTiSt becomes Artist)
    property NormalizeVariables: Boolean read FNormalizeVariables write FNormalizeVariables;
    { When set stream-files are deleted when recording stops.
      This option is only enabled when FSaveToMemory is false }
    property DeleteStreams: Boolean read FDeleteStreams write FDeleteStreams;
    // When set recorded titles are added to ignorelist
    property AddSavedToIgnore: Boolean read FAddSavedToIgnore write FAddSavedToIgnore;
    // When set recorded titles are added to stream-specific ignorelist
    property AddSavedToStreamIgnore: Boolean read FAddSavedToStreamIgnore write FAddSavedToStreamIgnore;
    // When set recorded titles are removed from wishlist
    property RemoveSavedFromWishlist: Boolean read FRemoveSavedFromWishlist write FRemoveSavedFromWishlist;
    // When set short songs will not be saved (see FShortLengthSeconds)
    property SkipShort: Boolean read FSkipShort write FSkipShort;
    // When set silence will be searched to detect cut-positions
    property SearchSilence: Boolean read FSearchSilence write FSearchSilence;
    // When set the level for silence will be automatically detected
    property AutoDetectSilenceLevel: Boolean read FAutoDetectSilenceLevel write FAutoDetectSilenceLevel;
    // Manual silence control: The maximum level of volume to detect as silence
    property SilenceLevel: Cardinal read FSilenceLevel write FSilenceLevel;
    // Manual silence control: The minumum length of set volume level (SilenceLevel) to detect as silence
    property SilenceLength: Cardinal read FSilenceLength write FSilenceLength;
    // Seconds to search for silence before track-change occured
    property SilenceBufferSecondsStart: Integer read FSilenceBufferSecondsStart write FSilenceBufferSecondsStart;
    // Seconds to search for silence after track-change occured
    property SilenceBufferSecondsEnd: Integer read FSilenceBufferSecondsEnd write FSilenceBufferSecondsEnd;
    // When a recorded song is shorter than the defined length it is discarded if SkipShort is set
    property ShortLengthSeconds: Integer read FShortLengthSeconds write FShortLengthSeconds;
    // Use this area of audio data if no silence could be found. It will be appended to the start/end of the song
    property SongBuffer: Integer read FSongBuffer write FSongBuffer;
    // When set the detected track offset will be moved
    property AdjustTrackOffset: Boolean read FAdjustTrackOffset write FAdjustTrackOffset;
    // Milliseconds to move the detected track offset
    property AdjustTrackOffsetMS: Cardinal read FAdjustTrackOffsetMS write FAdjustTrackOffsetMS;
    // The direction to move the adjusted track offset to
    property AdjustTrackOffsetDirection: TTrackOffsetDirection read FAdjustTrackOffsetDirection write FAdjustTrackOffsetDirection;
    // Maximum number of retries on error
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    // The delay between retries (defined by MaxRetries)
    property RetryDelay: Cardinal read FRetryDelay write FRetryDelay;
    // The filter to use
    property Filter: TUseFilters read FFilter write FFilter;
    // When set separated tracks will be saved (not only the stream-file)
    property SeparateTracks: Boolean read FSeparateTracks write FSeparateTracks;
    // When set all data will be saved to memory (instead of harddisk)
    property SaveToMemory: Boolean read FSaveToMemory write FSetSaveToMemory;
    // When set only completely received songs will be saved
    property OnlySaveFull: Boolean read FOnlySaveFull write FOnlySaveFull;
    // When set smaller files with the same name will be overwritten
    property OverwriteSmaller: Boolean read FOverwriteSmaller write FOverwriteSmaller;
    // When set a file will not be saved if it is smaller than an existing same-named song
    property DiscardSmaller: Boolean read FDiscardSmaller write FDiscardSmaller;
    property DiscardAlways: Boolean read FDiscardAlways write FDiscardAlways;
    property OutputFormat: TAudioTypes read FOutputFormat write FOutputFormat;
    { This list defines when to ignore track changes.
      If if contains "Radio XYZ - Greatest Hits!!" the following will happen:
      Artist A - Title A              <- Okay
      Radio XYZ - Greatest Hits!!     <- No track change detection
      Artist B -> Title B             <- Okay, track change is detected here, so Radio XYZ... will not be saved }
    property IgnoreTrackChangePattern: TStringList read FIgnoreTrackChangePattern write FIgnoreTrackChangePattern;
    property PostProcessors: TPostProcessorList read FPostProcessors;
    property EncoderSettings: TEncoderSettingsList read FEncoderSettings;
  end;

  // An array of TStreamSettings
  TStreamSettingsArray = array of TStreamSettings;

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
    FVBR: Boolean;
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
    FWasPlaying: Boolean;

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
    class function Load(Data: TDataLists; Stream: TStream; Version: Integer): TStreamEntry;
    procedure Save(Stream: TMemoryStream);

    property Settings: TStreamSettings read FSettings;

    property Parent: TStreamList read FParent write FParent;
    property ID: Cardinal read FID write FID;
    property Name: string read FName write FSetName;
    property CustomName: string read FCustomName write FCustomName;
    property StreamURL: string read FStreamURL write FStreamURL;
    property StartURL: string read FStartURL write FStartURL;
    property URLs: TStringList read FURLs;
    property Bitrate: Cardinal read FBitrate write FBitrate;
    property VBR: Boolean read FVBR write FVBR;
    property AudioType: TAudioTypes read FAudioType write FAudioType;
    property Genre: string read FGenre write FSetGenre;
    property Index: Integer read FIndex write FIndex;
    property CategoryIndex: Integer read FCategoryIndex write FCategoryIndex;
    property WasRecording: Boolean read FWasRecording write FWasRecording;
    property WasPlaying: Boolean read FWasPlaying write FWasPlaying;

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

    class function Load(Stream: TStream; Version: Integer): TChartCategory;
    procedure Save(Stream: TMemoryStream);

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

    class function Load(Stream: TStream; Version: Integer): TChartStream;

    property ID: Cardinal read FID;
    property Stream: TStreamBrowserEntry read FStream write FStream;
    property PlayedLastDay: Cardinal read FPlayedLastDay;
    property PlayedLastWeek: Cardinal read FPlayedLastWeek;
    property PlayedLast: Cardinal read FPlayedLast write FPlayedLast;
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

    class function LoadFromHome(Stream: TMemoryStream; Version: Integer): TChartEntry;

    procedure LoadStreams;

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
    FStreamCount: Cardinal;
  public
    constructor Create; overload;
    constructor Create(Name: string; ID: Cardinal); overload;

    class function Load(Stream: TStream; Version: Integer): TGenre;
    class function LoadFromHome(Stream: TMemoryStream; Version: Integer): TGenre;
    procedure Save(Stream: TMemoryStream);

    property ID: Cardinal read FID write FID;
    property Name: string read FName write FName;
    property StreamCount: Cardinal read FStreamCount write FStreamCount;
  end;

  TGenreList = class(TList<TGenre>)
  end;

  TChartCategoryList = class(TList<TChartCategory>)
  end;

  TSaveIgnoreList = class(TList<TTitleInfo>)
  private
    function FGetAnyAutomatic: Boolean;
  public
    property AnyAutomatic: Boolean read FGetAnyAutomatic;
  end;

  { This class contains lists and stuff about everything streamWriter
    needs to know at runtime. This class gets loaded at startup and
    disposed/saved at the end }
  TDataLists = class
  private
    FDefaultStreamSettings: TStreamSettings;
    FStreamSettings: TStreamSettings;
    FAutoRecordSettings: TStreamSettings;
    FCategoryList: TListCategoryList;
    FStreamList: TStreamList;
    FTrackList: TTrackList;
    FSaveList: TSaveIgnoreList;
    FIgnoreList: TSaveIgnoreList;
    FRecentList: TRecentList;
    FStreamBlacklist: TStringList;
    FRatingList: TRatingList;
    //FChartList: TChartList;
    //FChartCategoryList: TChartCategoryList;
    FBrowserList: TStreamBrowserList;
    FGenreList: TGenreList;
    FLoadError: Boolean;
    FReceived: UInt64;
    FSongsSaved: UInt64;
    FSavedTitleHashes: TList<Cardinal>;
  public
    constructor Create;
    destructor Destroy; override;

    // Cleans all lists and frees all their items
    procedure CleanLists;
    procedure Load(const Filename: string); overload;
    procedure Load(const Stream: TStream; const Filename: string); overload;
    procedure Save(const Filename: string; const UseCompression: Boolean); overload;
    procedure Save(const S: TStream; const UseCompression: Boolean); overload;
    procedure CheckEncodersAndPostProcessors;

    class procedure VerifyMagic(S: TStream; MinVersion: Cardinal; IsData: Boolean);

    property DefaultStreamSettings: TStreamSettings read FDefaultStreamSettings;
    property StreamSettings: TStreamSettings read FStreamSettings;
    property AutoRecordSettings: TStreamSettings read FAutoRecordSettings;

    // List that contains all categories
    property CategoryList: TListCategoryList read FCategoryList;
    // List that contains all streams
    property StreamList: TStreamList read FStreamList;
    // List that contains all saved tracks
    property TrackList: TTrackList read FTrackList;
    // List that contains all titles to be saved
    property SaveList: TSaveIgnoreList read FSaveList;
    // List that contains all titles to be ignored
    property IgnoreList: TSaveIgnoreList read FIgnoreList;
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
    property SongsSaved: UInt64 read FSongsSaved write FSongsSaved;

    property SavedTitleHashes: TList<Cardinal> read FSavedTitleHashes write FSavedTitleHashes;
  end;

const
  DATAVERSION: Cardinal = 69;
  DATAMAGIC: array[0..3] of Byte = (118, 114, 110, 97);
  EXPORTMAGIC: array[0..3] of Byte = (97, 110, 114, 118);

implementation

uses
  AppData,
  PostProcessConvert,
  PostProcessMP4Box,
  PostProcessSetTags,
  PostProcessSoX;

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

  Pattern := TFunctions.BuildPattern(Title, Hash, NumChars, False);
  FPattern := Pattern;
  FHash := Hash;

  // Default True. Im Load() wird es ggf. auf False gesetzt.
  FUpdatedToHash := True;
end;

class function TTitleInfo.Load(Stream: TStream; Version: Integer): TTitleInfo;
var
  NumChars: Integer;
  Hash: Cardinal;
  Pattern: string;
begin
  Result := TTitleInfo.Create;
  Stream.Read(Result.FTitle, IfThen<Boolean>(Version > 68, True, False));

  if Version > 31 then
    Stream.Read(Result.FAdded)
  else
    Result.FAdded := Now;

  if Version > 31 then
    Stream.Read(Result.FIndex, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FIndex := High(Cardinal);

  if Version > 3 then
  begin
    Stream.Read(Result.FPattern, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FHash, IfThen<Boolean>(Version > 68, True, False));
  end else
  begin
    Pattern := TFunctions.BuildPattern(Result.FTitle, Hash, NumChars, False);
    Result.FPattern := Pattern;
    Result.FHash := Hash;
  end;

  if Version > 46 then
    Stream.Read(Result.FServerHash, IfThen<Boolean>(Version > 68, True, False));
  if Version > 49 then
    Stream.Read(Result.FServerArtistHash, IfThen<Boolean>(Version > 68, True, False));

  if Version > 50 then
    Stream.Read(Result.FSaved, IfThen<Boolean>(Version > 68, True, False));

  if Version > 48 then
    Stream.Read(Result.FUpdatedToHash)
  else
    Result.FUpdatedToHash := (Result.ServerHash > 0) or (Result.ServerArtistHash > 0);
end;

procedure TTitleInfo.Save(Stream: TMemoryStream);
begin
  Stream.Write(FTitle, True);
  Stream.Write(FAdded);
  Stream.Write(FIndex, True);
  Stream.Write(FPattern, True);
  Stream.Write(FHash, True);
  Stream.Write(FServerHash, True);
  Stream.Write(FServerArtistHash, True);
  Stream.Write(FSaved, True);
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
  FBitrate := From.Bitrate;
  FVBR := From.VBR;
  FAudioType := From.AudioType;
  FGenre := From.Genre;
  FWasRecording := From.WasRecording;
  FWasPlaying := From.WasPlaying;
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

class function TStreamEntry.Load(Data: TDataLists; Stream: TStream; Version: Integer): TStreamEntry;
var
  BTmp: Boolean;
  i, Count: Integer;
  EnumTmp: Byte;
  STmp, URL: string;
begin
  Result := TStreamEntry.Create;

  if Version >= 6 then
  begin
    Result.FSettings.Free;
    Result.FSettings := TStreamSettings.Load(Stream, Version);
  end else
    // Defaults benutzen..
    Result.FSettings.Assign(Data.StreamSettings);

  if Version >= 24 then
    Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));

  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 38 then
    Stream.Read(Result.FCustomName, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FCustomName := Result.FName;

  if Version >= 8 then
    Stream.Read(Result.FStreamURL, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FStartURL, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
  for i := 0 to Count - 1 do
  begin
    Stream.Read(URL, IfThen<Boolean>(Version > 68, True, False));
    Result.FURLs.Add(URL);
  end;

  Stream.Read(Result.FBitrate, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 62 then
    Stream.Read(Result.FVBR);

  if Version < 40 then
  begin
    Stream.Read(STmp, IfThen<Boolean>(Version > 68, True, False));
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

  Stream.Read(Result.FGenre, IfThen<Boolean>(Version > 68, True, False));

  if Version <= 20 then
    Stream.Read(BTmp);
  if Version >= 5 then
  begin
    Stream.Read(Result.FIndex, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FCategoryIndex, IfThen<Boolean>(Version > 68, True, False));
  end;

  Stream.Read(Result.FSongsSaved, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FBytesReceived, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 46 then
    Stream.Read(Result.FSecondsReceived, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 17 then
    Stream.Read(Result.FWasRecording);

  if Version >= 65 then
    Stream.Read(Result.FWasPlaying);

  if Version >= 18 then
  begin
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
      Result.FSchedules.Add(TSchedule.Load(Stream, Version));
  end;

  if Version >= 28 then
  begin
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
      Result.FSaveList.Add(TTitleInfo.Load(Stream, Version));
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
      Result.FIgnoreList.Add(TTitleInfo.Load(Stream, Version));
  end;

  if Version > 31 then
    Stream.Read(Result.FIgnoreListIndex, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FIgnoreListIndex := High(Cardinal);
end;

procedure TStreamEntry.Save(Stream: TMemoryStream);
var
  i: Integer;
begin
  FSettings.Save(Stream);

  Stream.Write(FID, True);
  Stream.Write(FName, True);
  Stream.Write(FCustomName, True);
  Stream.Write(FStreamURL, True);
  Stream.Write(FStartURL, True);
  Stream.Write(FURLs.Count, True);
  for i := 0 to FURLs.Count - 1 do
    Stream.Write(FURLs[i], True);
  Stream.Write(FBitrate, True);
  Stream.Write(FVBR);
  Stream.Write(Byte(FAudioType));
  Stream.Write(FGenre, True);

  Stream.Write(FIndex, True);
  Stream.Write(FCategoryIndex, True);

  Stream.Write(FSongsSaved, True);
  Stream.Write(FBytesReceived, True);
  Stream.Write(FSecondsReceived, True);

  Stream.Write(FWasRecording);
  Stream.Write(FWasPlaying);

  Stream.Write(FSchedules.Count, True);
  for i := 0 to FSchedules.Count - 1 do
    FSchedules[i].Save(Stream);

  Stream.Write(FSaveList.Count, True);
  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Save(Stream);

  Stream.Write(FIgnoreList.Count, True);
  for i := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[i].Save(Stream);

  Stream.Write(FIgnoreListIndex, True);
end;

procedure TStreamEntry.FSetGenre(Value: string);
begin
  FGenre := Value;
end;

{ TStreamDataList }

procedure TDataLists.CheckEncodersAndPostProcessors;
var
  i, n: Integer;
begin
  if AppGlobals.AddonManager.CanEncode(FStreamSettings.OutputFormat) <> ceOkay then
    FStreamSettings.OutputFormat := atNone;

  if AppGlobals.AddonManager.CanEncode(FAutoRecordSettings.OutputFormat) <> ceOkay then
    FAutoRecordSettings.OutputFormat := atNone;

  for i := 0 to FStreamList.Count - 1 do
    if AppGlobals.AddonManager.CanEncode(FStreamList[i].Settings.OutputFormat) <> ceOkay then
      FStreamList[i].Settings.OutputFormat := atNone;

  for n := 0 to FStreamSettings.PostProcessors.Count - 1 do
    if FStreamSettings.PostProcessors[n].ClassType.InheritsFrom(TInternalPostProcess) then
      if not TInternalPostProcess(FStreamSettings.PostProcessors[n]).DependenciesMet then
        FStreamSettings.PostProcessors[n].Active := False;

  for n := 0 to FAutoRecordSettings.PostProcessors.Count - 1 do
    if FAutoRecordSettings.PostProcessors[n].ClassType.InheritsFrom(TInternalPostProcess) then
      if not TInternalPostProcess(FAutoRecordSettings.PostProcessors[n]).DependenciesMet then
        FAutoRecordSettings.PostProcessors[n].Active := False;

  for i := 0 to FStreamList.Count - 1 do
    for n := 0 to FStreamList[i].Settings.PostProcessors.Count - 1 do
      if FStreamList[i].Settings.PostProcessors[n].ClassType.InheritsFrom(TInternalPostProcess) then
        if not TInternalPostProcess(FStreamList[i].Settings.PostProcessors[n]).DependenciesMet then
          FStreamList[i].Settings.PostProcessors[n].Active := False;
end;

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

  for i := 0 to FGenreList.Count - 1 do
    FGenreList[i].Free;
  FGenreList.Clear;
end;

constructor TDataLists.Create;
begin
  inherited;

  FDefaultStreamSettings := TStreamSettings.GetDefaults;
  FStreamSettings := TStreamSettings.Create;
  FStreamSettings.Assign(FDefaultStreamSettings);

  FAutoRecordSettings := TStreamSettings.Create;
  FAutoRecordSettings.Assign(FDefaultStreamSettings);
  TStreamSettings.ApplyAutoDefaults(Self, FAutoRecordSettings);

  FLoadError := False;
  FReceived := 0;
  FCategoryList := TListCategoryList.Create;
  FStreamList := TStreamList.Create;
  FTrackList := TTrackList.Create;
  FSaveList := TSaveIgnoreList.Create;
  FIgnoreList := TSaveIgnoreList.Create;
  FRecentList := TRecentList.Create;
  FStreamBlacklist := TStringList.Create;
  FRatingList := TRatingList.Create;
  FBrowserList := TStreamBrowserList.Create;
  FGenreList := TGenreList.Create;
  FSavedTitleHashes := TList<Cardinal>.Create;
end;

destructor TDataLists.Destroy;
begin
  FDefaultStreamSettings.Free;
  FStreamSettings.Free;
  FAutoRecordSettings.Free;

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
  FSavedTitleHashes.Free;
  //FChartList.Free;
  //FChartCategoryList.Free;

  inherited;
end;

procedure TDataLists.Load(const Stream: TStream; const Filename: string);
var
  Entry: TStreamEntry;
  TitleInfo: TTitleInfo;
  TrackInfo: TTrackInfo;
  Str: string;
  i, Version, CatCount, EntryCount: Integer;
  Compressed: Boolean;
  ChartCategory: TChartCategory;
  TitleCount, Hash: Cardinal;
  ReadStream: TStream;
begin
  CleanLists;

  Stream.Read(Version, False);

  if Version > DATAVERSION then
    raise EVersionException.Create(Filename);

  // Bei 44 war Kompression immer aktiv, danach optional
  if Version >= 44 then
    Compressed := True
  else
    Compressed := False;

  // Ab 45 für Recoveryfile abgeschaltet, darum als Feld gespeichert
  if Version >= 45 then
    Stream.Read(Compressed);

  ReadStream := IfThen<TStream>(Compressed, TDecompressionStream.Create(Stream), Stream);
  try
    ReadStream.Read(FReceived, IfThen<Boolean>(Version > 68, True, False));
    if Version >= 56 then
      ReadStream.Read(FSongsSaved, IfThen<Boolean>(Version > 68, True, False));

    if Version <= 2 then
      while ReadStream.Position < ReadStream.Size do
      begin
        Entry := TStreamEntry.Load(Self, ReadStream, Version);
        Entry.FParent := FStreamList;
        FStreamList.Add(Entry);
      end else
    begin
      if Version >= 61 then
      begin
        FStreamSettings.Free;
        FStreamSettings := TStreamSettings.Load(ReadStream, Version);
      end else
        FStreamSettings.Assign(FDefaultStreamSettings);

      if (Version > 58) and (Version < 60) then
      begin
        FAutoRecordSettings.Free;

        // REMARK: Pfusch für Zwischenversion, damit im File gespult wird... für Build 601 von Version 4.9.0.1.
        FAutoRecordSettings := TStreamSettings.Load(ReadStream, Version);
        FAutoRecordSettings.Free;

        FAutoRecordSettings := TStreamSettings.Create;
        FAutoRecordSettings.Assign(DefaultStreamSettings);
        TStreamSettings.ApplyAutoDefaults(Self, FAutoRecordSettings);
      end else if Version >= 60 then
      begin
        FAutoRecordSettings.Free;
        FAutoRecordSettings := TStreamSettings.LoadAuto(Self, ReadStream, Version);
      end;

      if Version >= 5 then
      begin
        ReadStream.Read(CatCount, IfThen<Boolean>(Version > 68, True, False));
        for i := 0 to CatCount - 1 do
          FCategoryList.Add(TListCategory.Load(ReadStream, Version));
      end;

      ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
      for i := 0 to EntryCount - 1 do
      begin
        Entry := TStreamEntry.Load(Self, ReadStream, Version);
        Entry.FParent := FStreamList;
        FStreamList.Add(Entry);
      end;

      if Version >= 6 then
      begin
        ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
        for i := 0 to EntryCount - 1 do
        begin
          TrackInfo := TTrackInfo.Load(ReadStream, Version);
          FTrackList.Add(TrackInfo);
        end;
      end;

      if Version >= 3 then
      begin
        ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
        for i := 0 to EntryCount - 1 do
        begin
          TitleInfo := TTitleInfo.Load(ReadStream, Version);
          if TitleInfo <> nil then
            FSaveList.Add(TitleInfo);
        end;
        ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
        for i := 0 to EntryCount - 1 do
        begin
          TitleInfo := TTitleInfo.Load(ReadStream, Version);
          if TitleInfo <> nil then
            FIgnoreList.Add(TitleInfo);
        end;

        if Version >= 6 then
        begin
          if Version < 37 then
          begin
            ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
            for i := 0 to EntryCount - 1 do
              ReadStream.Read(Str, IfThen<Boolean>(Version > 68, True, False));
          end;

          ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
          for i := 0 to EntryCount - 1 do
            FRecentList.Add(TRecentEntry.Load(ReadStream, Version));

          if Version >= 15 then
          begin
            ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
            for i := 0 to EntryCount - 1 do
            begin
              ReadStream.Read(Str, IfThen<Boolean>(Version > 68, True, False));
              FStreamBlacklist.Add(Str);
            end;
          end;

          if Version = 22 then
          begin
            ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
            for i := 0 to EntryCount - 1 do
              FRatingList.Add(TRatingEntry.Load(ReadStream, Version));
          end;

          if Version >= 23 then
          begin
            ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
            for i := 0 to EntryCount - 1 do
              FBrowserList.Add(TStreamBrowserEntry.Load(ReadStream, Version));

            ReadStream.Read(EntryCount, IfThen<Boolean>(Version > 68, True, False));
            for i := 0 to EntryCount - 1 do
              FGenreList.Add(TGenre.Load(ReadStream, Version));
          end;
        end;
      end;
    end;

    if Version >= 35 then
    begin
      if Version < 48 then
      begin
        ReadStream.Read(CatCount, IfThen<Boolean>(Version > 68, True, False));
        for i := 0 to CatCount - 1 do
        begin
          ChartCategory := TChartCategory.Load(ReadStream, Version);
          ChartCategory.Free;
        end;
      end;

      FBrowserList.CreateDict;
    end;

    if Version >= 51 then
    begin
      ReadStream.Read(TitleCount, IfThen<Boolean>(Version > 68, True, False));
      for i := 0 to TitleCount - 1 do
      begin
        ReadStream.Read(Hash, IfThen<Boolean>(Version > 68, True, False));
        SavedTitleHashes.Add(Hash);
      end;

      // Aufräumen...
      while SavedTitleHashes.Count > 5000 do
        SavedTitleHashes.Delete(0);
    end;

  finally
    if ReadStream <> Stream then
      ReadStream.Free;
  end;
end;

procedure TDataLists.Load(const Filename: string);
var
  Stream: TBufferedFileStream;
begin
  if not FileExists(Filename) then
    Exit;

  Stream := TBufferedFileStream.Create(Filename, fmOpenRead);
  try
    VerifyMagic(Stream, 62, True);
    Load(Stream, Filename);
  finally
    Stream.Free;
  end;
end;

procedure TDataLists.Save(const S: TStream; const UseCompression: Boolean);
var
  i: Integer;
  CompressedStream: TMemoryStream;
begin
  S.Write(Integer(DATAVERSION), False);

  S.Write(UseCompression);

  CompressedStream := TMemoryStream.Create;
  try
    CompressedStream.Write(FReceived, True);
    CompressedStream.Write(FSongsSaved, True);

    FStreamSettings.Save(CompressedStream);
    FAutoRecordSettings.SaveAuto(CompressedStream);

    CompressedStream.Write(FCategoryList.Count, True);
    for i := 0 to FCategoryList.Count - 1 do
      FCategoryList[i].Save(CompressedStream);

    CompressedStream.Write(FStreamList.Count, True);
    for i := 0 to FStreamList.Count - 1 do
      FStreamList[i].Save(CompressedStream);

    CompressedStream.Write(FTrackList.Count, True);
    for i := 0 to FTrackList.Count - 1 do
      FTrackList[i].Save(CompressedStream);

    CompressedStream.Write(FSaveList.Count, True);
    for i := 0 to FSaveList.Count - 1 do
      FSaveList[i].Save(CompressedStream);

    CompressedStream.Write(FIgnoreList.Count, True);
    for i := 0 to FIgnoreList.Count - 1 do
      FIgnoreList[i].Save(CompressedStream);

    CompressedStream.Write(FRecentList.Count, True);
    for i := 0 to FRecentList.Count - 1 do
      FRecentList[i].Save(CompressedStream);

    CompressedStream.Write(FStreamBlacklist.Count, True);
    for i := 0 to FStreamBlacklist.Count - 1 do
      CompressedStream.Write(FStreamBlacklist[i], True);

    CompressedStream.Write(FBrowserList.Count, True);
    for i := 0 to FBrowserList.Count - 1 do
      FBrowserList[i].Save(CompressedStream);

    CompressedStream.Write(FGenreList.Count, True);
    for i := 0 to FGenreList.Count - 1 do
      FGenreList[i].Save(CompressedStream);

    CompressedStream.Write(Cardinal(SavedTitleHashes.Count), True);
    for i := 0 to SavedTitleHashes.Count - 1 do
      CompressedStream.Write(SavedTitleHashes[i], True);

    CompressedStream.Seek(0, soFromBeginning);

    if UseCompression then
    begin
      {$IFDEF DEBUG}
      TFunctions.CompressStream(CompressedStream, S, clDefault);
      {$ELSE}
      TFunctions.CompressStream(CompressedStream, S, clDefault);
      {$ENDIF}
    end else
      S.CopyFrom(CompressedStream, CompressedStream.Size);
  finally
    CompressedStream.Free;
  end;
end;

class procedure TDataLists.VerifyMagic(S: TStream; MinVersion: Cardinal; IsData: Boolean);
var
  Buf: array[0..Length(DATAMAGIC) - 1] of Byte;
  Magic: array[0..Length(DATAMAGIC) - 1] of Byte;
  OtherMagic: array[0..Length(DATAMAGIC) - 1] of Byte;
  Version: Cardinal;
begin
  // Remark: Die Funktion hier kam am 29.12.14 rein.
  //         Muss drin bleiben, aber Parameter "MinVersion" kann irgendwann raus, und dann
  //         kann ich mir hier das lesen der Versionsnummer auch schenken. Entfernen,
  //         wenn es keine Versionen ohne Magicbytes mehr gibt.

  if Length(DATAMAGIC) <> Length(EXPORTMAGIC) then
    raise Exception.Create('Length(DATAMAGIC) <> Length(EXPORTMAGIC)');

  if IsData then
  begin
    Move(DATAMAGIC[0], Magic[0], Length(Magic));
    Move(EXPORTMAGIC[0], OtherMagic[0], Length(Magic));
  end else
  begin
    Move(EXPORTMAGIC[0], Magic[0], Length(Magic));
    Move(DATAMAGIC[0], OtherMagic[0], Length(Magic));
  end;

  S.Read(Version, False);
  if Version > MinVersion then
  begin
    if S.Size >= Length(Magic) then
    begin
      S.Position := 0;
      S.Read(Buf, Length(Buf));

      if CompareMem(@Buf[0], @OtherMagic[0], Length(Buf)) then
        raise EUnsupportedFormatException.Create('')
      else if CompareMem(@Buf[0], @Magic[0], Length(Buf)) then
        S.Position := Length(Buf)
      else
        raise EUnknownFormatException.Create('');
    end else
      raise Exception.Create('');
  end else
    S.Position := 0;
end;

procedure TDataLists.Save(const Filename: string; const UseCompression: Boolean);
var
  WriteFilename: string;
  Stream: TBufferedFileStream;
begin
  if FLoadError or AppGlobals.SkipSave or (AppGlobals.DataFile = '') then
    Exit;

  if (FCategoryList.Count = 1) and (FStreamList.Count = 0) and (FRecentList.Count = 0) and (FIgnoreList.Count = 0) and (FSaveList.Count = 0) and (FBrowserList.Count = 0) and not (FileExists(AppGlobals.DataFile)) then
    Exit;

  WriteFilename := Filename + '.write';

  Stream := TBufferedFileStream.Create(WriteFilename, fmCreate);
  try
    Stream.WriteBuffer(DATAMAGIC[0], Length(DATAMAGIC));
    Save(Stream, UseCompression);
  finally
    Stream.Free;
  end;

  if not MoveFileExA(PChar(WriteFilename), PChar(Filename), MOVEFILE_REPLACE_EXISTING) then
    raise Exception.Create('');
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
  FBitrate := Source.Bitrate;
  FIsAuto := Source.IsAuto;
  FIsStreamFile := Source.IsStreamFile;
  FFinalized := Source.Finalized;
  FVBR := Source.VBR;
  FIndex := Source.Index;
  FServerTitle := Source.ServerTitle;
  FSongArtist := Source.SongArtist;
  FsongTitle := Source.SongTitle;
  FServerTitleHash := Source.ServerTitleHash;
  FServerArtistHash := Source.ServerArtistHash;
  FRecordBecauseArtist := Source.RecordBecauseArtist;
end;

function TTrackInfo.Copy: TTrackInfo;
begin
  Result := TTrackInfo.Create;
  Result.Assign(Self);
end;

constructor TTrackInfo.Create(Time: TDateTime; Filename, Streamname, ServerTitle, Artist, Title: string; ServerTitleHash, ServerArtistHash: Cardinal);
begin
  inherited Create;

  FTime := Time;
  FFilename := Filename;
  FStreamname := Streamname;
  FWasCut := False;
  FIsStreamFile := False;
  FServerTitle := ServerTitle;
  FSongArtist := Artist;
  FSongTitle := Title;
  FServerTitleHash := ServerTitleHash;
  FServerArtistHash := ServerArtistHash;
end;

function TTrackInfo.FGetParsedTitle: string;
begin
  if FIsStreamFile then
    Result := TFunctions.RemoveFileExt(ExtractFileName(FFilename))
  else if (FSongArtist <> '') and (FSongTitle <> '') then
    Result := FSongArtist + ' - ' + FSongTitle
  else if FServerTitle <> '' then
    Result := FServerTitle
  else
    Result := TFunctions.RemoveFileExt(ExtractFileName(FFilename));
end;

class function TTrackInfo.Load(Stream: TStream; Version: Integer): TTrackInfo;
begin
  Result := TTrackInfo.Create;

  Stream.Read(Result.FFilename, IfThen<Boolean>(Version > 68, True, False));

  Result.FFilename := TFunctions.TryUnRelativePath(Result.FFilename);

  Stream.Read(Result.FStreamname, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FFilesize, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 13 then
    Stream.Read(Result.FLength, IfThen<Boolean>(Version > 68, True, False));

  Stream.Read(Result.FTime);
  Stream.Read(Result.FWasCut);
  if Version > 10 then
  begin
    Stream.Read(Result.FBitrate, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FIsAuto);
  end;

  if Version >= 30 then
    Stream.Read(Result.FIsStreamFile);

  if Version > 18 then
    Stream.Read(Result.FFinalized);

  if Version > 31 then
    Stream.Read(Result.FIndex, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FIndex := High(Cardinal);

  if Version >= 41 then
    Stream.Read(Result.FVBR);

  if Version > 51 then
    Stream.Read(Result.FServerTitle, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FServerTitle := TFunctions.RemoveFileExt(ExtractFileName(Result.FFilename));

  if Version > 54 then
  begin
    Stream.Read(Result.FSongArtist, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FSongTitle, IfThen<Boolean>(Version > 68, True, False));
  end;

  if Version > 50 then
  begin
    Stream.Read(Result.FServerTitleHash, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FServerArtistHash, IfThen<Boolean>(Version > 68, True, False));
  end;

  if Version > 53 then
    Stream.Read(Result.FRecordBecauseArtist);
end;

procedure TTrackInfo.Save(Stream: TMemoryStream);
begin
  Stream.Write(TFunctions.TryRelativePath(FFilename, True, True), True);

  Stream.Write(FStreamname, True);
  Stream.Write(FFilesize, True);
  Stream.Write(FLength, True);
  Stream.Write(FTime);
  Stream.Write(FWasCut);
  Stream.Write(FBitrate, True);
  Stream.Write(FIsAuto);
  Stream.Write(FIsStreamFile);
  Stream.Write(FFinalized);
  Stream.Write(FIndex, True);
  Stream.Write(FVBR);
  Stream.Write(FServerTitle, True);
  Stream.Write(FSongArtist, True);
  Stream.Write(FSongTitle, True);
  Stream.Write(FServerTitleHash, True);
  Stream.Write(FServerArtistHash, True);
  Stream.Write(FRecordBecauseArtist);
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

function TStreamList.Get(Name, URL: string; URLs: TStringList): TStreamEntry;
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

class function TListCategory.Load(Stream: TStream; Version: Integer): TListCategory;
begin
  Result := TListCategory.Create;
  Result.FKilled := False;
  Stream.Read(Result.FIndex, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FExpanded);
  if Version >= 10 then
    Stream.Read(Result.FIsAuto);
end;

procedure TListCategory.Save(Stream: TMemoryStream);
begin
  Stream.Write(FIndex, True);
  Stream.Write(FName, True);
  Stream.Write(FExpanded);
  Stream.Write(FIsAuto);
end;

{ TTrackList }

function TTrackList.GetTrack(Filename: string): TTrackInfo;
var
  i: Integer;
begin
  Filename := Filename.ToLower;
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FFilename.ToLower = Filename then
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

class function TRecentEntry.Load(Stream: TStream; Version: Integer): TRecentEntry;
begin
  Result := TRecentEntry.Create(0, 0, '', '', 0);
  if Version >= 24 then
    Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FStartURL, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FIndex, IfThen<Boolean>(Version > 68, True, False));
  if Version >= 25 then
    Stream.Read(Result.FBitrate, IfThen<Boolean>(Version > 68, True, False));
end;

procedure TRecentEntry.Save(Stream: TMemoryStream);
begin
  Stream.Write(FID, True);
  Stream.Write(FName, True);
  Stream.Write(FStartURL, True);
  Stream.Write(FIndex, True);
  Stream.Write(FBitrate, True);
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

function TSchedule.GetEndTime(ScheduleStarted: TDateTime): TDateTime;
begin
  Result := 0;
  if FActive then
  begin
    Result := ScheduleStarted;
    if ((FEndHour < HourOf(Result)) or ((FEndHour = HourOf(Result)) and (FEndMinute <= MinuteOf(Result)))) then
      Result := IncDay(Result);

    Result := RecodeHour(Result, FEndHour);
    Result := RecodeMinute(Result, FEndMinute);
  end;
end;

function TSchedule.GetStartTime(ModificationAllowed: Boolean): TDateTime;
var
  N: TDateTime;
begin
  Result := 0;
  if FActive then
  begin
    N := Now;
    case FInterval of
      siDaily:
        if ModificationAllowed and ((FStartHour < HourOf(N)) or ((FStartHour = HourOf(N)) and (FStartMinute < MinuteOf(N)))) then
          Result := EncodeDateTime(YearOf(N), MonthOf(N), DayOf(IncDay(N)), FStartHour, FStartMinute, 0, 0)
        else
          Result := EncodeDateTime(YearOf(N), MonthOf(N), DayOf(N), FStartHour, FStartMinute, 0, 0);
      siWeekly:
      begin
        Result := EncodeDateTime(YearOf(N), MonthOf(N), DayOf(N), FStartHour, FStartMinute, 0, 0);
        if DayOfTheWeek(N) <= Integer(FDay) + 1 then
        begin
          Result := IncDay(Result, (Integer(FDay) + 1 - DayOfTheWeek(N)));
          if ModificationAllowed and (Result < N) then
            Result := IncWeek(Result);
        end else
          Result := IncDay(Result, 7 - DayOfTheWeek(N) + Integer(FDay) + 1);
      end;
      siNone:
        Result := EncodeDateTime(YearOf(FDate), MonthOf(FDate), DayOf(FDate), FStartHour, FStartMinute, 0, 0);
    end;
  end;
end;

class function TSchedule.Load(Stream: TStream; Version: Integer): TSchedule;
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
  Stream.Read(Result.FStartHour, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FStartMinute, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FEndHour, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FEndMinute, IfThen<Boolean>(Version > 68, True, False));
end;

procedure TSchedule.Save(Stream: TMemoryStream);
begin
  Stream.Write(FActive);
  Stream.Write(FRecurring);
  Stream.Write(Byte(FInterval));
  Stream.Write(Byte(FDay));
  Stream.Write(FDate);
  Stream.Write(FAutoRemove);
  Stream.Write(FStartHour, True);
  Stream.Write(FStartMinute, True);
  Stream.Write(FEndHour, True);
  Stream.Write(FEndMinute, True);
end;

{ TRatingEntry }

constructor TRatingEntry.Create(Name, URL: string; Rating: Integer);
begin
  inherited Create;

  FName := Name;
  FURL := URL;
  FRating := Rating;
end;

class function TRatingEntry.Load(Stream: TStream; Version: Integer): TRatingEntry;
begin
  Result := TRatingEntry.Create('', '', 0);
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FURL, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FRating, IfThen<Boolean>(Version > 68, True, False));
end;

procedure TRatingEntry.Save(Stream: TMemoryStream);
begin
  Stream.Write(FName, True);
  Stream.Write(FURL, True);
  Stream.Write(FRating, True);
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

  FURLs := TStringList.Create;
  FIgnoreTitles := TStringList.Create;
  FRegExes := TStringList.Create;
end;

destructor TStreamBrowserEntry.Destroy;
begin
  FURLs.Free;
  FIgnoreTitles.Free;
  FRegExes.Free;

  inherited;
end;

procedure TStreamBrowserEntry.Assign(Source: TStreamBrowserEntry);
begin
  FID := Source.FID;
  FName := Source.FName;
  FGenre := Source.FGenre;
  FURL := Source.FURL;
  FURLs.Assign(Source.FURLs);
  FWebsite := Source.FWebsite;
  FBitrate := Source.FBitrate;
  FAudioType := Source.FAudioType;
  FMetaData := Source.FMetaData;
  FChangesTitleInSong := Source.FChangesTitleInSong;
  FOwnRating := Source.FOwnRating;
  FRating := Source.FRating;
  FRecordingOkay := Source.FRecordingOkay;
  FRegExes.Assign(Source.FRegExes);
  FIgnoreTitles.Assign(Source.FIgnoreTitles);
end;

function TStreamBrowserEntry.Copy: TStreamBrowserEntry;
begin
  Result := TStreamBrowserEntry.Create;
  Result.Assign(Self);
end;

class function TStreamBrowserEntry.Load(Stream: TStream; Version: Integer): TStreamBrowserEntry;
var
  i: Integer;
  B: Byte;
  Count: Cardinal;
  E: string;
begin
  Result := TStreamBrowserEntry.Create;
  Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FGenre, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FURL, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 66 then
  begin
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
    begin
      Stream.Read(E, IfThen<Boolean>(Version > 68, True, False));
      Result.FURLs.Add(E);
    end;
  end;

  Stream.Read(Result.FWebsite, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FBitrate, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(B);
  Result.FAudioType := TAudioTypes(B);
  Stream.Read(Result.FMetaData);
  Stream.Read(Result.FChangesTitleInSong);
  Stream.Read(Result.FOwnRating);
  Stream.Read(Result.FRating);
  Stream.Read(Result.FRecordingOkay);
  if Version < 58 then
  begin
    Stream.Read(E, IfThen<Boolean>(Version > 68, True, False));
    Result.FRegExes.Add(E);
  end else
  begin
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
    begin
      Stream.Read(E, IfThen<Boolean>(Version > 68, True, False));
      Result.FRegExes.Add(E);
    end;
  end;

  if Version >= 29 then
  begin
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
    begin
      Stream.Read(E, IfThen<Boolean>(Version > 68, True, False));
      Result.FIgnoreTitles.Add(E);
    end;
  end;

  if Version >= 67 then
    Stream.Read(Result.FCanSetRegExps);
end;

class function TStreamBrowserEntry.LoadFromHome(Stream: TMemoryStream; Version: Integer): TStreamBrowserEntry;
var
  i: Integer;
  B: Byte;
  Count: Cardinal;
  E: string;
begin
  Result := TStreamBrowserEntry.Create;
  Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FGenre, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FURL, IfThen<Boolean>(Version > 68, True, False));

  Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
  for i := 0 to Count - 1 do
  begin
    Stream.Read(E, IfThen<Boolean>(Version > 68, True, False));
    Result.URLs.Add(E);
  end;

  Stream.Read(Result.FWebsite, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FBitrate, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(B);
  Result.FAudioType := TAudioTypes(B);
  Stream.Read(Result.FMetaData);
  Stream.Read(Result.FChangesTitleInSong);
  Stream.Read(Result.FRating);
  Stream.Read(Result.FRecordingOkay);

  Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
  for i := 0 to Count - 1 do
  begin
    Stream.Read(E, IfThen<Boolean>(Version > 68, True, False));
    Result.FRegExes.Add(E);
  end;

  Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
  for i := 0 to Count - 1 do
  begin
    Stream.Read(E, IfThen<Boolean>(Version > 68, True, False));
    Result.FIgnoreTitles.Add(E);
  end;

  Stream.Read(Result.FCanSetRegExps);
end;

procedure TStreamBrowserEntry.Save(Stream: TMemoryStream);
var
  i: Integer;
begin
  Stream.Write(FID, True);
  Stream.Write(FName, True);
  Stream.Write(FGenre, True);
  Stream.Write(FURL, True);

  Stream.Write(Cardinal(FURLs.Count), True);
  for i := 0 to FURLs.Count - 1 do
    Stream.Write(FURLs[i], True);

  Stream.Write(FWebsite, True);
  Stream.Write(FBitrate, True);
  Stream.Write(Byte(FAudioType));
  Stream.Write(FMetaData);
  Stream.Write(FChangesTitleInSong);
  Stream.Write(FOwnRating);
  Stream.Write(FRating);
  Stream.Write(FRecordingOkay);

  Stream.Write(Cardinal(FRegExes.Count), True);
  for i := 0 to FRegExes.Count - 1 do
    Stream.Write(FRegExes[i], True);

  Stream.Write(Cardinal(FIgnoreTitles.Count), True);
  for i := 0 to FIgnoreTitles.Count - 1 do
    Stream.Write(FIgnoreTitles[i], True);

  Stream.Write(FCanSetRegExps);
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

class function TChartEntry.LoadFromHome(Stream: TMemoryStream; Version: Integer): TChartEntry;
var
  i: Integer;
  C: Cardinal;
begin
  Result := TChartEntry.Create;
  Stream.Read(Result.FServerHash, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FServerArtistHash, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FArtist, IfThen<Boolean>(Version > 68, True, False));

  Stream.Read(Result.FPlayedLastDay, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FPlayedLastWeek, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FPlayedLast, IfThen<Boolean>(Version > 68, True, False));

  if Result.FPlayedLast > 86400 then
    Result.FPlayedLastDay := 0;
  if Result.FPlayedLast > 604800 then
    Result.FPlayedLastWeek := 0;

  Stream.Read(C, IfThen<Boolean>(Version > 68, True, False));

  for i := 0 to C - 1 do
    Result.Streams.Add(TChartStream.Load(Stream, Version));
end;

procedure TChartEntry.LoadStreams;
var
  i: Integer;
  Stream: TStreamBrowserEntry;
begin
  AppGlobals.Lock;
  try
    for i := Streams.Count - 1 downto 0 do
    begin
      Stream := AppGlobals.Data.BrowserList.GetStream(Streams[i].FID);
      if Stream <> nil then
        Streams[i].Stream := Stream.Copy
      else
      begin
        Streams[i].Free;
        Streams.Delete(i);
      end;
    end;
  finally
    AppGlobals.Unlock;
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

class function TGenre.Load(Stream: TStream; Version: Integer): TGenre;
begin
  Result := TGenre.Create;

  if Version >= 34 then
  begin
    Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FStreamCount, IfThen<Boolean>(Version > 68, True, False));
  end else
  begin
    Result.FID := 0;
    Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  end;
end;

class function TGenre.LoadFromHome(Stream: TMemoryStream; Version: Integer): TGenre;
begin
  Result := TGenre.Create;

  Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FStreamCount, IfThen<Boolean>(Version > 68, True, False));
end;

procedure TGenre.Save(Stream: TMemoryStream);
begin
  Stream.Write(FID, True);
  Stream.Write(FName, True);
  Stream.Write(FStreamCount, True);
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

class function TChartCategory.Load(Stream: TStream; Version: Integer): TChartCategory;
begin
  Result := TChartCategory.Create;
  Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FName, IfThen<Boolean>(Version > 68, True, False));
end;

procedure TChartCategory.Save(Stream: TMemoryStream);
begin
  Stream.Write(FID, True);
  Stream.Write(FName, True);
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

class function TChartStream.Load(Stream: TStream; Version: Integer): TChartStream;
begin
  Result := TChartStream.Create(0, 0, 0, 0);
  Stream.Read(Result.FID, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FPlayedLastDay, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FPlayedLastWeek, IfThen<Boolean>(Version > 68, True, False));

  if (Version >= 50) or (Version = 1) then
  begin
    Stream.Read(Result.FPlayedLast, IfThen<Boolean>(Version > 68, True, False));

    if Result.FPlayedLast > 86400 then
      Result.FPlayedLastDay := 0;
    if Result.FPlayedLast > 604800 then
      Result.FPlayedLastWeek := 0;
  end;
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

{ TStreamSettings }

function TStreamSettings.Copy: TStreamSettings;
begin
  Result := TStreamSettings.Create;
  Result.Assign(Self);
end;

constructor TStreamSettings.Create;
begin
  inherited Create;

  FRegExes := TStringList.Create;
  FIgnoreTrackChangePattern := TStringList.Create;
  FPostProcessors := TPostProcessorList.Create;
  FEncoderSettings := TEncoderSettingsList.Create;

  if FEncoderSettings.Count = 0 then
  begin
    FEncoderSettings.Add(TEncoderSettings.Create(atMPEG, brVBR, vqMedium));
    FEncoderSettings.Add(TEncoderSettings.Create(atAAC, brVBR, vqMedium));
    FEncoderSettings.Add(TEncoderSettings.Create(atOGG, brVBR, vqMedium));
  end;

  if PostProcessors.Count = 0 then
  begin
    // Der Convert muss der erste sein! Greife auf die Liste mal mit [0] zu!
    PostProcessors.Add(TPostProcessConvert.Create);
    PostProcessors.Add(TPostProcessSetTags.Create);
    PostProcessors.Add(TPostProcessSoX.Create);
    PostProcessors.Add(TPostProcessMP4Box.Create);
  end;
end;

destructor TStreamSettings.Destroy;
var
  i: Integer;
begin
  FRegExes.Free;
  FIgnoreTrackChangePattern.Free;

  for i := 0 to FPostProcessors.Count - 1 do
    FPostProcessors[i].Free;
  FPostProcessors.Free;

  for i := 0 to FEncoderSettings.Count - 1 do
    FEncoderSettings[i].Free;
  FEncoderSettings.Free;

  inherited;
end;

procedure TStreamSettings.FSetSaveToMemory(Value: Boolean);
begin
  FSaveToMemory := Value;
  if Value then
    FSeparateTracks := True;
end;

class function TStreamSettings.GetDefaults: TStreamSettings;
begin
  Result := TStreamSettings.Create;

  // Hier vorsichtig sein. Manche Einstellungen werden so auch als Defaults
  // für automatische Aufnahmen benutzt. Immer, wenn ich hier was ändere,
  // muss ich sicherstellen, dass ich dadurch nichts kaputt mache.

  Result.RegExes.Add(DEFAULT_TITLE_REGEXP);

  Result.FFilePattern := '%streamname%\%artist% - %title%';
  Result.FIncompleteFilePattern := '%streamname%\%artist% - %title%';
  Result.FStreamFilePattern := '%streamname%';
  Result.FFilePatternDecimals := 3;
  Result.RemoveChars := '[]{}#$§%~^';
  Result.NormalizeVariables := True;

  Result.FDeleteStreams := False;
  Result.FAddSavedToIgnore := False;
  Result.FAddSavedToStreamIgnore := False;
  Result.FRemoveSavedFromWishlist := False;
  Result.FSkipShort := True;
  Result.FSearchSilence := True;
  Result.FAutoDetectSilenceLevel := True;
  Result.FSilenceLevel := 5;
  Result.FSilenceLength := 100;
  Result.FSilenceBufferSecondsStart := 10;
  Result.FSilenceBufferSecondsEnd := 10;

  Result.FAdjustTrackOffset := False;
  Result.FAdjustTrackOffsetMS := 0;
  Result.FAdjustTrackOffsetDirection := toForward;

  Result.FSaveToMemory := False;
  Result.FOnlySaveFull := False;
  Result.FOverwriteSmaller := False;
  Result.FDiscardSmaller := False;
  Result.FDiscardAlways := False;

  Result.FSilenceLevel := 5;
  Result.FShortLengthSeconds := 45;

  Result.FSongBuffer := 0;

  Result.FMaxRetries := 100;
  Result.FRetryDelay := 5;

  Result.FOutputFormat := atNone;

  Result.FSeparateTracks := True;
  Result.FDeleteStreams := False;

  Result.FFilter := ufNone;
end;

class function TStreamSettings.Load(Stream: TStream; Version: Integer): TStreamSettings;
var
  B: Byte;
  i, Count, FilterTmp, TypeTmp: Integer;
  T: TPostProcessTypes;
  AT: TAudioTypes;
  Tmp: string;
  PP: TPostProcessBase;
  ES: TEncoderSettings;
begin
  Result := TStreamSettings.Create;

  if Version < 15 then
  begin
    Result.FRegExes.Add(DEFAULT_TITLE_REGEXP);
    Stream.Read(Result.FFilePattern, IfThen<Boolean>(Version > 68, True, False));
  end else if Version < 58 then
  begin
    Stream.Read(Tmp, IfThen<Boolean>(Version > 68, True, False));
    Result.FRegExes.Add(Tmp);
  end else
  begin
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
    begin
      Stream.Read(Tmp, IfThen<Boolean>(Version > 68, True, False));
      Result.FRegExes.Add(Tmp);
    end;
  end;

  Stream.Read(Result.FFilePattern, IfThen<Boolean>(Version > 68, True, False));
  if Version < 64 then
    Result.FFilePattern := ConvertPattern(Result.FFilePattern);

  if Version >= 17 then
  begin
    Stream.Read(Result.FIncompleteFilePattern, IfThen<Boolean>(Version > 68, True, False));
    if Result.FIncompleteFilePattern = '' then
      Result.FIncompleteFilePattern := Result.FFilePattern
    else
    begin
      if Version < 64 then
        Result.FIncompleteFilePattern := ConvertPattern(Result.FIncompleteFilePattern);
    end;
  end else
    Result.FIncompleteFilePattern := Result.FFilePattern;

  if Version >= 31 then
  begin
    Stream.Read(Result.FStreamFilePattern, IfThen<Boolean>(Version > 68, True, False));
    if Version < 64 then
      Result.FStreamFilePattern := ConvertPattern(Result.FStreamFilePattern);
    if Result.FStreamFilePattern = '' then
      Result.FStreamFilePattern := '%streamname%';
  end else
    Result.FStreamFilePattern := '%streamname%';

  if Version >= 14 then
    Stream.Read(Result.FFilePatternDecimals, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FFilePatternDecimals := 3;

  if Version >= 20 then
    Stream.Read(Result.FRemoveChars, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FRemoveChars := '[]{}#$§%~^';

  if Version >= 36 then
    Stream.Read(Result.FNormalizeVariables)
  else
    Result.FNormalizeVariables := True;

  Stream.Read(Result.FDeleteStreams);
  Stream.Read(Result.FAddSavedToIgnore);
  if Version >= 27 then
    Stream.Read(Result.FAddSavedToStreamIgnore);

  if Version >= 34 then
    Stream.Read(Result.FRemoveSavedFromWishlist);

  Stream.Read(Result.FSkipShort);
  Stream.Read(Result.FSearchSilence);

  if Version >= 39 then
    Stream.Read(Result.FAutoDetectSilenceLevel)
  else
    Result.AutoDetectSilenceLevel := True;

  Stream.Read(Result.FSilenceLevel, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FSilenceLength, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 33 then
  begin
    Stream.Read(Result.FSilenceBufferSecondsStart, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FSilenceBufferSecondsEnd, IfThen<Boolean>(Version > 68, True, False));
  end else if Version >= 9 then
  begin
    Stream.Read(Result.FSilenceBufferSecondsStart, IfThen<Boolean>(Version > 68, True, False));
    Result.FSilenceBufferSecondsEnd := Result.FSilenceBufferSecondsStart;
  end else
  begin
    Result.FSilenceBufferSecondsStart := 10;
    Result.FSilenceBufferSecondsEnd := 10;
  end;

  if Version >= 9 then
    Stream.Read(Result.FShortLengthSeconds, IfThen<Boolean>(Version > 68, True, False))
  else
  begin
    Stream.Read(FilterTmp, IfThen<Boolean>(Version > 68, True, False));
    Result.FShortLengthSeconds := 45;
  end;

  if Version >= 9 then
  begin
    Stream.Read(Result.FSongBuffer, IfThen<Boolean>(Version > 68, True, False));
    if Version < 42 then
      Result.FSongBuffer := Result.FSongBuffer * 1000;
  end else
  begin
    Stream.Read(FilterTmp, IfThen<Boolean>(Version > 68, True, False));
    Result.FSongBuffer := 0;
  end;

  Stream.Read(Result.FMaxRetries, IfThen<Boolean>(Version > 68, True, False));

  if Version >= 7 then
    Stream.Read(Result.FRetryDelay, IfThen<Boolean>(Version > 68, True, False))
  else
    Result.FRetryDelay := 5;

  if Result.FRetryDelay > 999 then
    Result.FRetryDelay := 999;

  Stream.Read(FilterTmp, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FSeparateTracks);
  Stream.Read(Result.FSaveToMemory);

  if Result.FSaveToMemory then
  begin
    Result.FSeparateTracks := True;
    Result.FDeleteStreams := False;
  end;

  if Version >= 8 then
    Stream.Read(Result.FOnlySaveFull)
  else
    Result.FOnlySaveFull := True;

  if Version >= 9 then
    Stream.Read(Result.FOverwriteSmaller)
  else
    Result.FOverwriteSmaller := True;

  if Version >= 12 then
    Stream.Read(Result.FDiscardSmaller)
  else
    Result.FDiscardSmaller := False;

  if Version >= 53 then
    Stream.Read(Result.FDiscardAlways)
  else
    Result.FDiscardAlways := False;

  if not Result.FSeparateTracks then
    Result.FDeleteStreams := False;

  if (FilterTmp > Ord(High(TUseFilters))) or (FilterTmp < Ord(Low(TUseFilters))) then
    Result.FFilter := ufNone
  else if Version > 26 then
    Result.FFilter := TUseFilters(FilterTmp)
  else if FilterTmp = 0 then
    Result.FFilter := ufNone
  else if FilterTmp = 1 then
    Result.FFilter := ufWish
  else if FilterTmp = 2 then
    Result.FFilter := ufIgnoreBoth
  else if FilterTmp = 3 then
    Result.FFilter := ufBoth
  else
    Result.FFilter := ufNone;

  if Version >= 28 then
  begin
    Stream.Read(Result.FAdjustTrackOffset);
    Stream.Read(Result.FAdjustTrackOffsetMS, IfThen<Boolean>(Version > 68, True, False));

    if Version < 37 then
      Result.FAdjustTrackOffsetMS := Result.FAdjustTrackOffsetMS * 1000;

    Stream.Read(B);
    Result.FAdjustTrackOffsetDirection := TTrackOffsetDirection(B);
  end;

  if Version >= 26 then
  begin
    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
    begin
      Stream.Read(Tmp, IfThen<Boolean>(Version > 68, True, False));
      Result.FIgnoreTrackChangePattern.Add(Tmp);
    end;
  end;

  // Einstellungen laden...
  if Version >= 41 then
  begin
    Stream.Read(TypeTmp, IfThen<Boolean>(Version > 68, True, False));
    Result.FOutputFormat := TAudioTypes(TypeTmp);

    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
    begin
      Stream.Read(TypeTmp, IfThen<Boolean>(Version > 68, True, False));

      T := TPostProcessTypes(TypeTmp);

      if T <> ptExternal then
      begin
        PP := Result.PostProcessors.Find(T);
        PP.Load(Stream, Version);
      end else
      begin
        PP := TExternalPostProcess.Create;
        PP.Load(Stream, Version);
        Result.PostProcessors.Add(PP);
      end;
    end;

    Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
    for i := 0 to Count - 1 do
    begin
      Stream.Read(TypeTmp, IfThen<Boolean>(Version > 68, True, False));

      AT := TAudioTypes(TypeTmp);

      ES := Result.EncoderSettings.Find(AT);

      if ES <> nil then
        ES.Load(Stream, Version);
    end;
  end;
end;

class function TStreamSettings.LoadAuto(Data: TDataLists; Stream: TStream; Version: Integer): TStreamSettings;
var
  i, Count, TypeTmp: Integer;
  T: TPostProcessTypes;
  AT: TAudioTypes;
  PP: TPostProcessBase;
  ES: TEncoderSettings;
begin
  Result := TStreamSettings.Create;

  Result.Assign(Data.DefaultStreamSettings);
  ApplyAutoDefaults(Data, Result);

  if Version >= 61 then
  begin
    Stream.Read(Result.FFilePattern, IfThen<Boolean>(Version > 68, True, False));
    Stream.Read(Result.FAddSavedToIgnore);
    Stream.Read(Result.FRemoveSavedFromWishlist);

    if Version < 64 then
      Result.FFilePattern := ConvertPattern(Result.FFilePattern);
  end;

  Stream.Read(Result.FSearchSilence);
  Stream.Read(Result.FSilenceBufferSecondsStart, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FSilenceBufferSecondsEnd, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FAutoDetectSilenceLevel);
  Stream.Read(Result.FSilenceLevel, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FSilenceLength, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(Result.FSongBuffer, IfThen<Boolean>(Version > 68, True, False));

  Stream.Read(TypeTmp, IfThen<Boolean>(Version > 68, True, False));
  Result.FOutputFormat := TAudioTypes(TypeTmp);

  Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
  for i := 0 to Count - 1 do
  begin
    Stream.Read(TypeTmp, IfThen<Boolean>(Version > 68, True, False));

    T := TPostProcessTypes(TypeTmp);

    if T <> ptExternal then
    begin
      PP := Result.PostProcessors.Find(T);
      PP.Load(Stream, Version);
    end else
    begin
      PP := TExternalPostProcess.Create;
      PP.Load(Stream, Version);
      Result.PostProcessors.Add(PP);
    end;
  end;

  Stream.Read(Count, IfThen<Boolean>(Version > 68, True, False));
  for i := 0 to Count - 1 do
  begin
    Stream.Read(TypeTmp, IfThen<Boolean>(Version > 68, True, False));

    AT := TAudioTypes(TypeTmp);

    ES := Result.EncoderSettings.Find(AT);

    if ES <> nil then
      ES.Load(Stream, Version);
  end;
end;

procedure TStreamSettings.Save(Stream: TMemoryStream);
var
  i: Integer;
  Count: Integer;
begin
  Stream.Write(FRegExes.Count, True);
  for i := 0 to FRegExes.Count - 1 do
    Stream.Write(FRegExes[i], True);

  Stream.Write(FFilePattern, True);
  Stream.Write(FIncompleteFilePattern, True);
  Stream.Write(FStreamFilePattern, True);
  Stream.Write(FFilePatternDecimals, True);
  Stream.Write(FRemoveChars, True);
  Stream.Write(FNormalizeVariables);
  Stream.Write(FDeleteStreams);
  Stream.Write(FAddSavedToIgnore);
  Stream.Write(FAddSavedToStreamIgnore);
  Stream.Write(FRemoveSavedFromWishlist);
  Stream.Write(FSkipShort);
  Stream.Write(FSearchSilence);
  Stream.Write(FAutoDetectSilenceLevel);
  Stream.Write(FSilenceLevel, True);
  Stream.Write(FSilenceLength, True);
  Stream.Write(FSilenceBufferSecondsStart, True);
  Stream.Write(FSilenceBufferSecondsEnd, True);
  Stream.Write(FShortLengthSeconds, True);
  Stream.Write(FSongBuffer, True);
  Stream.Write(FMaxRetries, True);
  Stream.Write(FRetryDelay, True);
  Stream.Write(Integer(FFilter), True);
  Stream.Write(FSeparateTracks);
  Stream.Write(FSaveToMemory);
  Stream.Write(FOnlySaveFull);
  Stream.Write(FOverwriteSmaller);
  Stream.Write(FDiscardSmaller);
  Stream.Write(FDiscardAlways);

  Stream.Write(FAdjustTrackOffset);
  Stream.Write(FAdjustTrackOffsetMS, True);
  Stream.Write(Byte(FAdjustTrackOffsetDirection));

  Stream.Write(FIgnoreTrackChangePattern.Count, True);
  for i := 0 to FIgnoreTrackChangePattern.Count - 1 do
    Stream.Write(FIgnoreTrackChangePattern[i], True);

  Stream.Write(Integer(FOutputFormat), True);

  Count := 0;
  for i := 0 to FPostProcessors.Count - 1 do
    if (not FPostProcessors[i].Hidden) and (FPostProcessors[i].PostProcessType <> ptConvert) then
      Inc(Count);
  Stream.Write(Count, True);
  for i := 0 to FPostProcessors.Count - 1 do
  begin
    if FPostProcessors[i].Hidden or (FPostProcessors[i].PostProcessType = ptConvert) then
      Continue;
    Stream.Write(Integer(FPostProcessors[i].PostProcessType), True);
    FPostProcessors[i].Save(Stream);
  end;

  Stream.Write(FEncoderSettings.Count, True);
  for i := 0 to FEncoderSettings.Count - 1 do
  begin
    Stream.Write(Integer(FEncoderSettings[i].AudioType), True);
    FEncoderSettings[i].Save(Stream);
  end;
end;

procedure TStreamSettings.SaveAuto(Stream: TMemoryStream);
var
  i: Integer;
  Count: Integer;
begin
  Stream.Write(FFilePattern, True);
  Stream.Write(FAddSavedToIgnore);
  Stream.Write(FRemoveSavedFromWishlist);

  Stream.Write(FSearchSilence);
  Stream.Write(FSilenceBufferSecondsStart, True);
  Stream.Write(FSilenceBufferSecondsEnd, True);
  Stream.Write(FAutoDetectSilenceLevel);
  Stream.Write(FSilenceLevel, True);
  Stream.Write(FSilenceLength, True);
  Stream.Write(FSongBuffer, True);

  Stream.Write(Integer(FOutputFormat), True);

  Count := 0;
  for i := 0 to FPostProcessors.Count - 1 do
    if (not FPostProcessors[i].Hidden) and (FPostProcessors[i].PostProcessType <> ptConvert) then
      Inc(Count);
  Stream.Write(Count, True);
  for i := 0 to FPostProcessors.Count - 1 do
  begin
    if FPostProcessors[i].Hidden or (FPostProcessors[i].PostProcessType = ptConvert) then
      Continue;
    Stream.Write(Integer(FPostProcessors[i].PostProcessType), True);
    FPostProcessors[i].Save(Stream);
  end;

  Stream.Write(FEncoderSettings.Count, True);
  for i := 0 to FEncoderSettings.Count - 1 do
  begin
    Stream.Write(Integer(FEncoderSettings[i].AudioType), True);
    FEncoderSettings[i].Save(Stream);
  end;
end;

class procedure TStreamSettings.ApplyAutoDefaults(Data: TDataLists; S: TStreamSettings);
var
  i: Integer;
begin
  S.SearchSilence := True;
  S.SilenceBufferSecondsStart := 15;
  S.SilenceBufferSecondsEnd := 15;
  S.AutoDetectSilenceLevel := True;
  S.SilenceLevel := 5;
  S.SilenceLength := 100;
  S.SongBuffer := 10000;

  S.Filter := ufNone;
  S.SaveToMemory := True;
  S.SeparateTracks := True;
  S.OnlySaveFull := False;
  S.DeleteStreams := False;
  S.MaxRetries := 0;
  S.RetryDelay := 0;
  S.AddSavedToStreamIgnore := False;

  S.RegExes.Assign(Data.DefaultStreamSettings.RegExes);

  S.OutputFormat := atNone;
  for i := 0 to S.PostProcessors.Count - 1 do
    S.PostProcessors[i].Active := False;
end;

procedure TStreamSettings.Assign(From: TStreamSettings);
var
  i: Integer;
begin
  FRegExes.Assign(From.FRegExes);

  FFilePattern := From.FFilePattern;
  FIncompleteFilePattern := From.FIncompleteFilePattern;
  FStreamFilePattern := From.FStreamFilePattern;
  FFilePatternDecimals := From.FilePatternDecimals;
  FRemoveChars := From.RemoveChars;
  FNormalizeVariables := From.NormalizeVariables;
  FDeleteStreams := From.FDeleteStreams;
  FAddSavedToIgnore := From.FAddSavedToIgnore;
  FAddSavedToStreamIgnore := From.FAddSavedToStreamIgnore;
  FRemoveSavedFromWishlist := From.FRemoveSavedFromWishlist;
  FSkipShort := From.FSkipShort;
  FSearchSilence := From.FSearchSilence;
  FAutoDetectSilenceLevel := From.FAutoDetectSilenceLevel;
  FSilenceLevel := From.FSilenceLevel;
  FSilenceLength := From.FSilenceLength;
  FSilenceBufferSecondsStart := From.FSilenceBufferSecondsStart;
  FSilenceBufferSecondsEnd := From.FSilenceBufferSecondsEnd;
  FShortLengthSeconds := From.FShortLengthSeconds;
  FSongBuffer := From.FSongBuffer;
  FMaxRetries := From.FMaxRetries;
  FRetryDelay := From.FRetryDelay;
  FFilter := From.FFilter;
  FSeparateTracks := From.FSeparateTracks;
  FSaveToMemory := From.FSaveToMemory;
  FOnlySaveFull := From.FOnlySaveFull;
  FOverwriteSmaller := From.FOverwriteSmaller;
  FDiscardSmaller := From.FDiscardSmaller;
  FDiscardAlways := From.FDiscardAlways;
  FAdjustTrackOffset := From.FAdjustTrackOffset;
  FAdjustTrackOffsetMS := From.FAdjustTrackOffsetMS;
  FAdjustTrackOffsetDirection := From.FAdjustTrackOffsetDirection;
  FIgnoreTrackChangePattern.Assign(From.FIgnoreTrackChangePattern);

  FOutputFormat := From.FOutputFormat;
  for i := 0 to FPostProcessors.Count - 1 do
    FPostProcessors[i].Free;
  FPostProcessors.Clear;
  for i := 0 to From.PostProcessors.Count - 1 do
    FPostProcessors.Add(From.PostProcessors[i].Copy);

  for i := 0 to FEncoderSettings.Count - 1 do
    FEncoderSettings[i].Free;
  FEncoderSettings.Clear;
  for i := 0 to From.EncoderSettings.Count - 1 do
    FEncoderSettings.Add(From.EncoderSettings[i].Copy);
end;

{ TPostProcessorList }

function TPostProcessorList.Find(PostProcessor: TPostProcessBase): TPostProcessBase;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (PostProcessor is TExternalPostProcess) and (Self[i] is TExternalPostProcess) then
    begin
      if TExternalPostProcess(PostProcessor).Identifier = TExternalPostProcess(Self[i]).Identifier then
      begin
        Result := Self[i];
        Break;
      end;
    end else if PostProcessor.ClassType.InheritsFrom(TInternalPostProcess) and Self[i].ClassType.InheritsFrom(TInternalPostProcess) then
      if PostProcessor.ClassType = Self[i].ClassType then
      begin
        Result := Self[i];
        Break;
      end;
end;

function TPostProcessorList.Find(ClassType: TClass): TPostProcessBase;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Self[i].ClassType = ClassType then
    begin
      Result := Self[i];
      Break;
    end;
end;

function TPostProcessorList.FGetHash: Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].Hash;
end;

function TPostProcessorList.Find(PostProcessType: TPostProcessTypes): TPostProcessBase;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].PostProcessType = PostProcessType then
      Exit(Items[i]);
  Exit(nil);
end;

{ TEncoderSettingsList }

function TEncoderSettingsList.FGetHash: Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].Hash;
end;

function TEncoderSettingsList.Find(AudioType: TAudioTypes): TEncoderSettings;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].AudioType = AudioType then
      Exit(Items[i]);
  Exit(nil);
end;

{ TEncoderSettings }

procedure TEncoderSettings.Assign(From: TEncoderSettings);
begin
  AudioType := From.AudioType;
  BitrateType := From.BitrateType;
  CBRBitrate := From.CBRBitrate;
  VBRQuality := From.VBRQuality;
end;

function TEncoderSettings.Copy: TEncoderSettings;
begin
  Result := TEncoderSettings.Create(atNone, brVBR, vqMedium);
  Result.Assign(Self);
end;

constructor TEncoderSettings.Create(AudioType: TAudioTypes; BitrateType: TBitrates; VBRQuality: TVBRQualities);
begin
  inherited Create;

  Self.AudioType := AudioType;
  Self.BitrateType := BitrateType;
  Self.CBRBitrate := 128;
  Self.VBRQuality := VBRQuality;
end;

function TEncoderSettings.FGetHash: Cardinal;
begin
  Result := TFunctions.HashString(IntToStr(Integer(AudioType)) + IntToStr(Integer(BitrateType)) + IntToStr(CBRBitrate) + IntToStr(Integer(VBRQuality)));
end;

procedure TEncoderSettings.Load(Stream: TStream; Version: Integer);
var
  Tmp: Integer;
begin
  Stream.Read(Tmp, IfThen<Boolean>(Version > 68, True, False));
  BitrateType := TBitrates(Tmp);

  Stream.Read(CBRBitrate, IfThen<Boolean>(Version > 68, True, False));

  Stream.Read(Tmp, IfThen<Boolean>(Version > 68, True, False));
  VBRQuality := TVBRQualities(Tmp);
end;

procedure TEncoderSettings.Save(Stream: TMemoryStream);
begin
  Stream.Write(Integer(BitrateType), True);
  Stream.Write(CBRBitrate, True);
  Stream.Write(Integer(VBRQuality), True);
end;

{ TSaveIgnoreList }

function TSaveIgnoreList.FGetAnyAutomatic: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if (Self[i].FServerHash > 0) or (Self[i].FServerArtistHash > 0) then
      Exit(True);
end;

end.
