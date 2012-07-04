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

{ This unit contains classes to define application-specific settings.
  It contains TAppData, which is a descendand of TAppDataBase.
  This class contains everything related to the application settings.
  TAppDataBase defines more basic elements that all applications
  made by mistake.ws have in common. }
unit AppData;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, Registry, SyncObjs, AppDataBase,
  LanguageObjects, LanguageIcons, ExtendedStream, Forms, Functions, PostProcess,
  AddonManager, PostProcessManager, Logging, Base64, AudioFunctions;

type
  // Actions that can be executed in the stream-view
  TClientActions = (caStartStop, caStreamIntegrated, caStream, caFile);
  // Actions that can be exucuted using the stream-browser
  TBrowserActions = (baStart, baListen, baListenExternal);
  // Set for definition of filters to use
  TUseFilters = (ufNone, ufWish, ufIgnoreGlobal, ufIgnoreLocal, ufIgnoreBoth, ufBoth);
  // Definitions for directions for where to adjust the track-offset
  TTrackOffsetDirection = (toForward, toBackward);
  // Definitions for scheduling intervals. siNone has to be the last element!
  TScheduleInterval = (siDaily, siWeekly, siNone);
  // A specific day for a schedule. sdNone has to be the last element!
  TScheduleDay = (sdMonday, sdTuesday, sdWednesday, sdThursday, sdFriday, sdSaturday, sdSunday, sdNone);
  // An array of integer... what you say??
  TIntArray = array of Integer;

  TPostProcessorList = class(TList<TPostProcessBase>)
  private
    function FGetHash: Cardinal;
  public
    function Find(PostProcessor: TPostProcessBase): TPostProcessBase; overload;
    function Find(ClassType: TClass): TPostProcessBase; overload;
    function Find(PostProcessType: TPostProcessTypes): TPostProcessBase; overload;

    property Hash: Cardinal read FGetHash;
  end;

  TEncoderSettings = class
  private
    function FGetHash: Cardinal;
  public
    AudioType: TAudioTypes;
    BitrateType: TBitRates;
    CBRBitrate: Integer;
    VBRQuality: TVBRQualities;

    constructor Create(AudioType: TAudioTypes; BitrateType: TBitRates; VBRQuality: TVBRQualities);
    procedure Load(Stream: TExtendedStream; Version: Integer); overload;
    procedure Save(Stream: TExtendedStream); overload;
    procedure Load; overload;
    procedure Save; overload;
    procedure Assign(From: TEncoderSettings);
    function Copy: TEncoderSettings;

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
    FTitlePattern: string;
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
    FOutputFormat: TAudioTypes;
    FIgnoreTrackChangePattern: TStringList;
    FPostProcessors: TPostProcessorList;
    FEncoderSettings: TEncoderSettingsList;

    procedure FSetSaveToMemory(Value: Boolean);
  public
    // Creates a new instance of TStreamSettings class
    constructor Create(InitStuff: Boolean = True);
    // Destroys this instance of TStreamSettings
    destructor Destroy; override;

    procedure InitStuff;

    // Loads an instance of TStreamSettings from a stream
    class function Load(Stream: TExtendedStream; Version: Integer): TStreamSettings;
    // Saves this instance of TStreamSettings to a stream
    procedure Save(Stream: TExtendedStream);
    // Assigns this instance of TStreamSettings to From
    procedure Assign(From: TStreamSettings);
    // Copies this instance of TStreamSettings
    function Copy: TStreamSettings;

    // The pattern (Regex) to detect artist/title/album from broadcasted titles
    property TitlePattern: string read FTitlePattern write FTitlePattern;
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
    // When set variables get normalized (i.e. %a = ArTiSt becomes Artist)
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
            // TODO: auch wenn ne gespeicherte datei gespielt wird, das popup anzeigen neben der uhr!!
  // Application-specific settings
  TAppData = class(TAppDataBase)
  private
    FStreamSettings: TStreamSettings;
    FUserLoggedIn: Boolean;
    FID: Integer;
    FDir: string;
    FDirAuto: string;
    FTray: Boolean;
    FTrayOnMinimize: Boolean;
    FSnapMain: Boolean;
    FRememberRecordings: Boolean;
    FDisplayPlayedSong: Boolean;
    FDisplayPlayNotifications: Boolean;
    FShowSplashScreen: Boolean;
    FSidebarWidth: Integer;
    FAutoTuneIn: Boolean;
    FAutoTuneInConsiderIgnore: Boolean;
    FAutoTuneInAddToIgnore: Boolean;
    FAutoRemoveSavedFromWishlist: Boolean;
    FSubmitStreamInfo: Boolean;
    FSubmitStats: Boolean;
    FMinDiskSpace: Integer;
    FDefaultAction: TClientActions;
    FDefaultActionBrowser: TBrowserActions;
    FPlayerVolume: Integer;
    FPlayerVolumeBeforeMute: Integer;
    FAutoScrollLog: Boolean;
    FUserWasSetup: Boolean;
    FUser, FPass: string;
    FSoundDevice: Cardinal;
    FAutoTuneInMinKbps: Cardinal;
    FAutoTuneInFormat: Cardinal;
    FLimitSpeed: Boolean;
    FMaxSpeed: Cardinal;
    FLastBrowserUpdate: Cardinal;
    FAutomaticFilePattern: string;
    //FOutputFormat: TAudioTypes;

    FProjectHelpLinkMain: string;
    FProjectHelpLinkSettings: string;

    FShortcutPlay: Cardinal;
    FShortcutPause: Cardinal;
    FShortcutStop: Cardinal;
    FShortcutNext: Cardinal;
    FShortcutPrev: Cardinal;
    FShortcutVolDown: Cardinal;
    FShortcutVolUp: Cardinal;
    FShortcutMute: Cardinal;

    FClientHeadersLoaded: Boolean;
    FSavedHeadersLoaded: Boolean;
    FClientHeaderWidth: TIntArray;
    FSavedHeaderWidth: TIntArray;
    FClientCols: Integer;
    FSavedCols: Integer;
    FLastUsedDataVersion: Integer;
    FRecoveryFile: string;

    FEQEnabled: Boolean;
    FEQGain: array[0..9] of Integer;

    FAddonManager: TAddonManager;
    FPostProcessManager: TPostProcessManager;
    FLanguageIcons: TLanguageIcons;

    function FGetDataFile: string;

    function FGetEQGain(Idx: Integer): Integer;
    procedure FSetEQGain(Idx: Integer; Value: Integer);
  protected
    // Save ALL the things!
    procedure DoSave; override;
  public
    constructor Create(AppName: string);
    destructor Destroy; override;

    procedure Load; override;
    procedure BuildThanksText; override;
    procedure DeleteUndoFiles;

    property StreamSettings: TStreamSettings read FStreamSettings;
    property UserLoggedIn: Boolean read FUserLoggedIn write FUserLoggedIn;

    // The unique ID generated for this specific client
    property ID: Integer read FID;
    // The directory songs get saved to
    property Dir: string read FDir write FDir;
    // The direcroty automatically recorded songs get saved to
    property DirAuto: string read FDirAuto write FDirAuto;
    // When set streamWriter will have a tray icon
    property Tray: Boolean read FTray write FTray;
    // When set streamWriter will minimize to tray when the window is minimized (will not minimize to tray on close)
    property TrayOnMinimize: Boolean read FTrayOnMinimize write FTrayOnMinimize;
    // When set the main window will snap to screen edges
    property SnapMain: Boolean read FSnapMain write FSnapMain;
    // When set streamWriter will resume recordings on startup if it is closed while recording streams
    property RememberRecordings: Boolean read FRememberRecordings write FRememberRecordings;
    property DisplayPlayedSong: Boolean read FDisplayPlayedSong write FDisplayPlayedSong;
    // When set notifications on the lower right of the screen will be displayed when a title on a stream changes
    property DisplayPlayNotifications: Boolean read FDisplayPlayNotifications write FDisplayPlayNotifications;
    property ShowSplashScreen: Boolean read FShowSplashScreen write FShowSplashScreen;
    // Defines the width of the sidebar (streams/info/log)
    property SidebarWidth: Integer read FSidebarWidth write FSidebarWidth;
    // When set streamWriter automatically records songs from the wishlist
    property AutoTuneIn: Boolean read FAutoTuneIn write FAutoTuneIn;
    // When set streamWriter will not record songs automatically when they are on the ignorelist
    property AutoTuneInConsiderIgnore: Boolean read FAutoTuneInConsiderIgnore write FAutoTuneInConsiderIgnore;
    // When set automatically recorded songs will be added to the global ignorelist
    property AutoTuneInAddToIgnore: Boolean read FAutoTuneInAddToIgnore write FAutoTuneInAddToIgnore;
    // When set automatically recorded songs will be removed from the wishlist
    property AutoRemoveSavedFromWishlist: Boolean read FAutoRemoveSavedFromWishlist write FAutoRemoveSavedFromWishlist;
    // When set information about streams the user records will be sent to the server (only the URL of the stream)
    property SubmitStreamInfo: Boolean read FSubmitStreamInfo write FSubmitStreamInfo;
    // When set some statistics will be sent to the server (number of recoring streams/automatically recording streams)
    property SubmitStats: Boolean read FSubmitStats write FSubmitStats;
    // The minimum amount of free disk space that has to be available in order to record streams
    property MinDiskSpace: Integer read FMinDiskSpace write FMinDiskSpace;
    // The default action to execute when double-clicking a stream in the mainview
    property DefaultAction: TClientActions read FDefaultAction write FDefaultAction;
    // The default action to execute when double-clicking a stream in the streamview
    property DefaultActionBrowser: TBrowserActions read FDefaultActionBrowser write FDefaultActionBrowser;
    // The volume of the player
    property PlayerVolume: Integer read FPlayerVolume write FPlayerVolume;
    // The volume of the player before muting the volume
    property PlayerVolumeBeforeMute: Integer read FPlayerVolumeBeforeMute write FPlayerVolumeBeforeMute;
    // When set the log will scroll automatically
    property AutoScrollLog: Boolean read FAutoScrollLog write FAutoScrollLog;
    // Indicates whether streamWriter was setup successfully
    property UserWasSetup: Boolean read FUserWasSetup write FUserWasSetup;
    // The username to authenticate at streamWriter's server
    property User: string read FUser write FUser;
    // The password to authenticate at streamWriter's server
    property Pass: string read FPass write FPass;
    // The index of the BASS-sound-device to use for playback
    property SoundDevice: Cardinal read FSoundDevice write FSoundDevice;
    // The hotkey to trigger "Play"
    property ShortcutPlay: Cardinal read FShortcutPlay write FShortcutPlay;
    // The hotkey to trigger "Pause"
    property ShortcutPause: Cardinal read FShortcutPause write FShortcutPause;
    // The hotkey to trigger "Stop"
    property ShortcutStop: Cardinal read FShortcutStop write FShortcutStop;
    // The hotkey to trigger "Next"
    property ShortcutNext: Cardinal read FShortcutNext write FShortcutNext;
    // The hotkey to trigger "Previous"
    property ShortcutPrev: Cardinal read FShortcutPrev write FShortcutPrev;
    // The hotkey to trigger "Volume down"
    property ShortcutVolDown: Cardinal read FShortcutVolDown write FShortcutVolDown;
    // The hotkey to trigger "Volume up"
    property ShortcutVolUp: Cardinal read FShortcutVolUp write FShortcutVolUp;
    // The hotkey to trigger "Mute"
    property ShortcutMute: Cardinal read FShortcutMute write FShortcutMute;
    // Minimum Kbps needed for automatic recording of a stream
    property AutoTuneInMinKbps: Cardinal read FAutoTuneInMinKbps write FAutoTuneInMinKbps;
    // Desired format of streams to tune in automatically
    property AutoTuneInFormat: Cardinal read FAutoTuneInFormat write FAutoTuneInFormat;
    // When set the overall speedlimit is active
    property LimitSpeed: Boolean read FLimitSpeed write FLimitSpeed;
    // Overall speedlimit for recording/playback
    property MaxSpeed: Cardinal read FMaxSpeed write FMaxSpeed;
    // Time of last browser update (Browser will be updated automatically in specific intervals)
    property LastBrowserUpdate: Cardinal read FLastBrowserUpdate write FLastBrowserUpdate;
    // The pattern for automatically recorded files
    property AutomaticFilePattern: string read FAutomaticFilePattern write FAutomaticFilePattern;
    //property OutputFormat: TAudioTypes read FOutputFormat write FOutputFormat;

    property ClientHeadersLoaded: Boolean read FClientHeadersLoaded;
    property SavedHeadersLoaded: Boolean read FSavedHeadersLoaded;
    // Widths of column headers of the mainview
    property ClientHeaderWidth: TIntArray read FClientHeaderWidth write FClientHeaderWidth;
    property SavedHeaderWidth: TIntArray read FSavedHeaderWidth write FSavedHeaderWidth;
    // Widths of column headers. Yes, they are stored in a single integer
    property ClientCols: Integer read FClientCols write FClientCols;
    property SavedCols: Integer read FSavedCols write FSavedCols;
    // Last used version of the data-file format
    property LastUsedDataVersion: Integer read FLastUsedDataVersion write FLastUsedDataVersion;
    // Path to the recovery-file (this is set if streamWriter crashed or something)
    property RecoveryFile: string read FRecoveryFile;

    property EQEnabled: Boolean read FEQEnabled write FEQEnabled;
    property EQGain[Idx: Integer]: Integer read FGetEQGain write FSetEQGain;

    // Path to streamWriter's data-file
    property DataFile: string read FGetDataFile;

    // The manager for addons
    property AddonManager: TAddonManager read FAddonManager;

    // The manager for postprocessing
    property PostProcessManager: TPostProcessManager read FPostProcessManager;

    // Icons for languages
    property LanguageIcons: TLanguageIcons read FLanguageIcons;

    property ProjectHelpLinkMain: string read FProjectHelpLinkMain;
    property ProjectHelpLinkSettings: string read FProjectHelpLinkSettings;
  end;

var
  AppGlobals: TAppData;

implementation

uses
  PostProcessSetTags, PostProcessSoX, PostProcessMP4Box, PostProcessConvert;

{ TAppData }

constructor TAppData.Create(AppName: string);
var
  W, H: Integer;
begin
  // Create an instance for global stream-settings
  // (these are used for new streams that do not have user-specified settings)
  FStreamSettings := TStreamSettings.Create(False);

  // Adjust dimensions of the main-form
  W := 900;
  H := 630;
  if Screen.WorkAreaWidth < W then
    W := Screen.WorkAreaWidth - 20;
  if Screen.WorkAreaHeight < H then
    H := Screen.WorkAreaHeight - 20;

  // Adjust amount of column headers
  SetLength(FClientHeaderWidth, 6);
  SetLength(FSavedHeaderWidth, 7);

  // Set some application-specific settings
  SetLength(FProjectUpdateLinks, 2);
  {$IFDEF DEBUG}
  FProjectUpdateLinks[0] := 'http://streamwriter.gaia/';
  FProjectUpdateLinks[1] := 'http://streamwriter.gaia/';
  {$ELSE}
  FProjectUpdateLinks[0] := 'http://streamwriter.org/';
  FProjectUpdateLinks[1] := 'http://streamwriter.de/';
  {$ENDIF}
  FProjectHomepageLink := 'http://streamwriter.org/';
  FProjectLink := 'http://streamwriter.org/';

  FProjectHelpLink := 'http://streamwriter.org/wiki/artikel/help/';
  FProjectHelpLinkMain := 'http://streamwriter.org/wiki/artikel/mainwindow/';
  FProjectHelpLinkSettings := 'http://streamwriter.org/wiki/artikel/settings/';

  FProjectForumLink := 'http://streamwriter.org/forum/';
  FProjectDonateLink := 'http://streamwriter.org/inhalt/donate/';

  // Call the base-constructor with our defined variables
  inherited Create(AppName, True, W, H);

  // Set the name for the recovery-file
  FRecoveryFile := FStorage.DataDir + 'streamwriter_data_recovery.dat';

  // The number of the current build
  FBuildNumber := 391;

  // This builds a large string used to generate the about-window
  BuildThanksText;

  FLanguageIcons := TLanguageIcons.Create;

  DeleteUndoFiles;
end;

procedure TAppData.DeleteUndoFiles;
var
  SR: TSearchRec;
begin
  // Delete any undo-files of a possible previous session
  if FindFirst(TempDir + 'UNDO_*', faAnyFile and not faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
        DeleteFile(TempDir + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

destructor TAppData.Destroy;
begin
  FLanguageIcons.Free;
  FAddonManager.Free;
  FPostProcessManager.Free;
  FStreamSettings.Free;

  DeleteFile(TempDir + 'playlist.m3u');
  DeleteUndoFiles;

  inherited;
end;

// Gets the path to the data file where streamWriter saved it's settings
function TAppData.FGetDataFile;
begin
  Result := FStorage.GetFilePath('data.dat');
end;

function TAppData.FGetEQGain(Idx: Integer): Integer;
begin
  Result := FEQGain[Idx];
end;

procedure TAppData.FSetEQGain(Idx, Value: Integer);
begin
  FEQGain[Idx] := Value;
end;

procedure TAppData.BuildThanksText;
var
  Res: TResourceStream;
  Data: RawByteString;
begin
  inherited;

  Res := TResourceStream.Create(HInstance, 'THANKSTEXT_' + UpperCase(LanguageObjects.Language.CurrentLanguage.ID), RT_RCDATA);
  try
    // 3 wegen UTF-8 Marker
    Res.Position := 3;
    SetLength(Data, Res.Size - 3);
    Res.Read(Data[1], Res.Size - 3);

    FProjectThanksText := UTF8ToString(Data);
  finally
    Res.Free;
  end;
end;

// Loads everything streamWriter needs to know for startup
procedure TAppData.Load;
  // Generates an unique ID (this is only used to know how many people are using streamWriter - seriously!)
  function GetID: Integer;
  var
    Len: Cardinal;
    Name: string;
  begin
    Result := Random(9999) + 1;

    SetLength(Name, 255);
    Len := 255;
    if GetComputerName(@Name[1], Len) then
    begin
      SetLength(Name, Len);
      Result := Result + HashString(Name);
    end;
    Result := Result + GetTickCount;
    if Result < 1 then
      Result := Result * -1;
    if Result = 0 then
      Result := GetID;
  end;
var
  i, DefaultActionTmp, DefaultActionBrowser, DefaultFilterTmp, SilenceBuffer, OutputFormatTmp: Integer;
begin
  inherited;

  FStorage.Read('ID', FID, 0);
  if FID < 1 then
  begin
    FID := GetID;
  end;

  FStorage.Read('LastUsedDataVersion', FLastUsedDataVersion, 0);

  FStreamSettings.FTitlePattern := '(?P<a>.*) - (?P<t>.*)';

  FStorage.Read('FilePattern', FStreamSettings.FFilePattern, '%s\%a - %t');
  FStorage.Read('IncompleteFilePattern', FStreamSettings.FIncompleteFilePattern, '%s\%a - %t');
  FStorage.Read('StreamFilePattern', FStreamSettings.FStreamFilePattern, '%s');
  FStorage.Read('FilePatternDecimals', FStreamSettings.FFilePatternDecimals, 3);
  FStorage.Read('RemoveChars', FStreamSettings.FRemoveChars, '[]{}#$§%~^');
  FStorage.Read('NormalizeVariables', FStreamSettings.FNormalizeVariables, True);

  FStorage.Read('Dir', FDir, '');
  if FDir <> '' then
    FDir := IncludeTrailingBackslash(FDir);
  FDir := TryUnRelativePath(FDir);

  FStorage.Read('DirAuto', FDirAuto, '');
  if FDirAuto = '' then
    FDirAuto := FDir;
  if FDirAuto <> '' then
    FDirAuto := IncludeTrailingBackslash(FDirAuto);
  FDirAuto := TryUnRelativePath(FDirAuto);

  FStorage.Read('DeleteStreams', FStreamSettings.FDeleteStreams, False);
  FStorage.Read('AddSavedToIgnore', FStreamSettings.FAddSavedToIgnore, False);
  FStorage.Read('AddSavedToStreamIgnore', FStreamSettings.FAddSavedToStreamIgnore, False);
  FStorage.Read('RemoveSavedFromWishlist', FStreamSettings.FRemoveSavedFromWishlist, False);
  FStorage.Read('SkipShort', FStreamSettings.FSkipShort, True);
  FStorage.Read('SearchSilence', FStreamSettings.FSearchSilence, True);
  FStorage.Read('AutoDetectSilenceLevel', FStreamSettings.FAutoDetectSilenceLevel, True);
  FStorage.Read('SilenceLevel', FStreamSettings.FSilenceLevel, 5);
  FStorage.Read('SilenceLength', FStreamSettings.FSilenceLength, 100);

  FStorage.Read('SilenceBufferSeconds', SilenceBuffer, 3);
  if SilenceBuffer <> 3 then
  begin
    FStreamSettings.FSilenceBufferSecondsStart := SilenceBuffer;
    FStreamSettings.FSilenceBufferSecondsEnd := SilenceBuffer;
  end else
  begin
    FStorage.Read('SilenceBufferSecondsStart', FStreamSettings.FSilenceBufferSecondsStart, 5);
    FStorage.Read('SilenceBufferSecondsEnd', FStreamSettings.FSilenceBufferSecondsEnd, 5);
  end;

  FStreamSettings.FAdjustTrackOffset := False;
  FStreamSettings.FAdjustTrackOffsetMS := 0;
  FStreamSettings.FAdjustTrackOffsetDirection := toForward;

  FStorage.Read('SaveToMemory', FStreamSettings.FSaveToMemory, False);
  FStorage.Read('OnlySaveFull', FStreamSettings.FOnlySaveFull, True);
  FStorage.Read('OverwriteSmaller', FStreamSettings.FOverwriteSmaller, True);
  FStorage.Read('DiscardSmaller', FStreamSettings.FDiscardSmaller, False);

  if (FStreamSettings.FSilenceLevel < 1) or (FStreamSettings.FSilenceLevel > 100) then
    FStreamSettings.FSilenceLevel := 5;
  if FStreamSettings.FSilenceLength < 20 then
    FStreamSettings.FSilenceLength := 20;

  FStorage.Read('ShortLengthSeconds', FStreamSettings.FShortLengthSeconds, 45);

  // REMARK: Field was renamed, so load it this way... Can be changed later.
  FStorage.Read('SongBuffer', FStreamSettings.FSongBuffer, -1234);
  if FStreamSettings.FSongBuffer = -1234 then
  begin
    FStorage.Read('SongBufferSeconds', FStreamSettings.FSongBuffer, 0);
    FStreamSettings.FSongBuffer := FStreamSettings.FSongBuffer * 1000;
  end;

  FStorage.Read('MaxRetries', FStreamSettings.FMaxRetries, 100);
  FStorage.Read('RetryDelay', FStreamSettings.FRetryDelay, 5);

  FStorage.Read('TrayClose', FTray, False);
  FStorage.Read('TrayOnMinimize', FTrayOnMinimize, False);
  FStorage.Read('SnapMain', FSnapMain, False);
  FStorage.Read('RememberRecordings', FRememberRecordings, False);
  FStorage.Read('DisplayPlayedSong', FDisplayPlayedSong, True);
  FStorage.Read('DisplayPlayNotifications', FDisplayPlayNotifications, True);
  FStorage.Read('ShowSplashScreen', FShowSplashScreen, True);
  FStorage.Read('SidebarWidth', FSidebarWidth, 250);
  FStorage.Read('AutoTuneIn', FAutoTuneIn, True);
  FStorage.Read('AutoTuneInConsiderIgnore', FAutoTuneInConsiderIgnore, False);
  FStorage.Read('AutoTuneInAddToIgnore', FAutoTuneInAddToIgnore, False);
  FStorage.Read('AutoRemoveSavedFromWishlist', FAutoRemoveSavedFromWishlist, False);
  FStorage.Read('SubmitStreamInfo', FSubmitStreamInfo, True);
  FStorage.Read('SubmitStats', FSubmitStats, True);
  FStorage.Read('LimitSpeed', FLimitSpeed, False);
  FStorage.Read('MaxSpeed', FMaxSpeed, 0);
  if FMaxSpeed <= 0 then
    FLimitSpeed := False;
  FStorage.Read('LastBrowserUpdate', FLastBrowserUpdate, Trunc(Now));
  FStorage.Read('AutomaticFilePattern', FAutomaticFilePattern, '%s\%a - %t');

  FStorage.Read('OutputFormat', OutputFormatTmp, 0);
  if (OutputFormatTmp > Ord(High(TAudioTypes))) or
     (OutputFormatTmp < Ord(Low(TAudioTypes))) then
    FStreamSettings.OutputFormat := atNone
  else
    FStreamSettings.OutputFormat := TAudioTypes(OutputFormatTmp);

  FStorage.Read('AutoTuneInMinKbps', FAutoTuneInMinKbps, 3);
  FStorage.Read('AutoTuneInFormat', FAutoTuneInFormat, 0);
  if (FAutoTuneInMinKbps > 9) then
    FAutoTuneInMinKbps := 3;
  if FAutoTuneInFormat > 2 then
    FAutoTuneInFormat := 0;

  if FStreamSettings.FRetryDelay > 999 then
    FStreamSettings.RetryDelay := 999;

  FStorage.Read('SeparateTracks', FStreamSettings.FSeparateTracks, True);
  FStorage.Read('MinDiskSpace', FMinDiskSpace, 5);
  FStorage.Read('DefaultAction', DefaultActionTmp, Integer(caStartStop));
  FStorage.Read('DefaultActionBrowser', DefaultActionBrowser, Integer(baStart));
  FStorage.Read('DefaultFilter', DefaultFilterTmp, Integer(ufNone));
  FStorage.Read('PlayerVolume', FPlayerVolume, 50);
  FStorage.Read('PlayerVolumeBeforeMute', FPlayerVolumeBeforeMute, 50);

  FStorage.Read('AutoScrollLog', FAutoScrollLog, True);
  FStorage.Read('UserWasSetup', FUserWasSetup, False);
  FStorage.Read('User', FUser, '');
  FStorage.Read('Pass', FPass, '');
  if FLastUsedDataVersion >= 29 then
  begin
    // If FPass was empty when reading it, this leads to an exception!
    // We NEED to catch it here so that the startup process does not get interrupted..
    try
      FPass := CryptStr(UTF8ToUnicodeString(Decode(RawByteString(FPass))));
    except
      FPass := '';
    end;
  end else
    FPass := CryptStr(FPass);

  FStorage.Read('SoundDevice', FSoundDevice, 0);

  FStorage.Read('ShortcutPlay', FShortcutPlay, 0);
  FStorage.Read('ShortcutPause', FShortcutPause, 0);
  FStorage.Read('ShortcutStop', FShortcutStop, 0);
  FStorage.Read('ShortcutNext', FShortcutNext, 0);
  FStorage.Read('ShortcutPrev', FShortcutPrev, 0);
  FStorage.Read('ShortcutVolDown', FShortcutVolDown, 0);
  FStorage.Read('ShortcutVolUp', FShortcutVolUp, 0);
  FStorage.Read('ShortcutMute', FShortcutMute, 0);

  // Header of ClientView
  FStorage.Read('HeaderWidth0', i, -1, 'Cols');
  if i > -1 then
  begin
    FClientHeadersLoaded := True;
    for i := 0 to High(FClientHeaderWidth) do
      if i <> 1 then
        FStorage.Read('HeaderWidth' + IntToStr(i), FClientHeaderWidth[i], 130, 'Cols');
  end;
  FStorage.Read('ClientCols', FClientCols, 255, 'Cols');
  FClientCols := FClientCols or (1 shl 0);

  // Header of SavedView
  FStorage.Read('SavedHeaderWidth0', i, -1, 'Cols');
  if i > -1 then
  begin
    FSavedHeadersLoaded := True;
    for i := 0 to High(FSavedHeaderWidth) do
      if i <> 1 then
        FStorage.Read('SavedHeaderWidth' + IntToStr(i), FSavedHeaderWidth[i], 130, 'Cols');
  end;
  FStorage.Read('SavedCols', FSavedCols, 255, 'Cols');
  FSavedCols := FSavedCols or (1 shl 0);

  if (DefaultActionTmp > Ord(High(TClientActions))) or
     (DefaultActionTmp < Ord(Low(TClientActions))) then
    FDefaultAction := caStartStop
  else
    FDefaultAction := TClientActions(DefaultActionTmp);

  if (DefaultActionBrowser > Ord(High(TBrowserActions))) or
     (DefaultActionBrowser < Ord(Low(TBrowserActions))) then
    FDefaultActionBrowser := baStart
  else
    FDefaultActionBrowser := TBrowserActions(DefaultActionBrowser);

  if IsVersionNewer(LastUsedVersion, AppVersion) and (IsVersionNewer(LastUsedVersion, ParseVersion('2.1.0.9'))) then
  begin
    if DefaultFilterTmp = 0 then
      FStreamSettings.Filter := ufNone
    else if DefaultFilterTmp = 1 then
      FStreamSettings.Filter := ufWish
    else if DefaultFilterTmp = 2 then
      FStreamSettings.Filter := ufIgnoreBoth
    else if DefaultFilterTmp = 3 then
      FStreamSettings.Filter := ufBoth
    else
      FStreamSettings.Filter := ufNone;
  end else if (DefaultFilterTmp > Ord(High(TUseFilters))) or
              (DefaultFilterTmp < Ord(Low(TUseFilters))) then
    FStreamSettings.Filter := ufNone
  else
    FStreamSettings.Filter := TUseFilters(DefaultFilterTmp);

  if FStreamSettings.FSaveToMemory then
  begin
    FStreamSettings.FSeparateTracks := True;
    FStreamSettings.FDeleteStreams := False;
  end;

  if not FStreamSettings.FSeparateTracks then
    FStreamSettings.FDeleteStreams := False;

  FStorage.Read('EQEnabled', FEQEnabled, False, 'Equalizer');
  for i := 0 to High(FEQGain) do
  begin
    FStorage.Read('EQBand' + IntToStr(i), FEQGain[i], 15, 'Equalizer');
    FEQGain[i] := FEQGain[i] - 15;
    if (FEQGain[i] > 15) or (FEQGain[i] < -15) then
      FEQGain[i] := 0;
  end;
end;

procedure TAppData.DoSave;
var
  i, n: Integer;
begin
  inherited;

  FStorage.Write('ID', FID);

  FStorage.Write('LastUsedDataVersion', FLastUsedDataVersion);

  FStorage.Write('FilePattern', FStreamSettings.FFilePattern);
  FStorage.Write('IncompleteFilePattern', FStreamSettings.FIncompleteFilePattern);
  FStorage.Write('StreamFilePattern', FStreamSettings.FStreamFilePattern);
  FStorage.Write('FilePatternDecimals', FStreamSettings.FFilePatternDecimals);
  FStorage.Write('RemoveChars', FStreamSettings.FRemoveChars);
  FStorage.Write('NormalizeVariables', FStreamSettings.FNormalizeVariables);

  FStorage.Write('Dir', TryRelativePath(FDir, False));
  FStorage.Write('DirAuto', TryRelativePath(FDirAuto, False));

  FStorage.Write('DeleteStreams', FStreamSettings.FDeleteStreams);
  FStorage.Write('AddSavedToIgnore', FStreamSettings.FAddSavedToIgnore);
  FStorage.Write('AddSavedToStreamIgnore', FStreamSettings.FAddSavedToStreamIgnore);
  FStorage.Write('RemoveSavedFromWishlist', FStreamSettings.FRemoveSavedFromWishlist);
  FStorage.Write('SkipShort', FStreamSettings.FSkipShort);
  FStorage.Write('SearchSilence', FStreamSettings.FSearchSilence);
  FStorage.Write('AutoDetectSilenceLevel', FStreamSettings.FAutoDetectSilenceLevel);
  FStorage.Write('SilenceLevel', FStreamSettings.FSilenceLevel);
  FStorage.Write('SilenceLength', FStreamSettings.FSilenceLength);
  FStorage.Write('SilenceBufferSecondsStart', FStreamSettings.FSilenceBufferSecondsStart);
  FStorage.Write('SilenceBufferSecondsEnd', FStreamSettings.FSilenceBufferSecondsEnd);

  FStorage.Write('SaveToMemory', FStreamSettings.FSaveToMemory);
  FStorage.Write('OnlySaveFull', FStreamSettings.FOnlySaveFull);
  FStorage.Write('ShortLengthSeconds', FStreamSettings.FShortLengthSeconds);
  FStorage.Write('SongBuffer', FStreamSettings.FSongBuffer);
  // REMARK: Das .Delete() kann irgendwann raus.
  FStorage.Delete('SongBufferSeconds');
  FStorage.Write('MaxRetries', FStreamSettings.FMaxRetries);
  FStorage.Write('RetryDelay', FStreamSettings.FRetryDelay);
  FStorage.Write('DefaultFilter', Integer(FStreamSettings.Filter));
  FStorage.Write('SeparateTracks', FStreamSettings.FSeparateTracks);
  FStorage.Write('OverwriteSmaller', FStreamSettings.FOverwriteSmaller);
  FStorage.Write('DiscardSmaller', FStreamSettings.FDiscardSmaller);
  FStorage.Write('OutputFormat', Integer(FStreamSettings.FOutputFormat));

  FStorage.Write('TrayClose', FTray);
  FStorage.Write('TrayOnMinimize', FTrayOnMinimize);
  FStorage.Write('SnapMain', FSnapMain);
  FStorage.Write('RememberRecordings', FRememberRecordings);
  FStorage.Write('DisplayPlayedSong', FDisplayPlayedSong);
  FStorage.Write('DisplayPlayNotifications', FDisplayPlayNotifications);
  FStorage.Write('ShowSplashScreen', FShowSplashScreen);
  FStorage.Write('SidebarWidth', FSidebarWidth);
  FStorage.Write('AutoTuneIn', FAutoTuneIn);
  FStorage.Write('AutoTuneInConsiderIgnore', FAutoTuneInConsiderIgnore);
  FStorage.Write('AutoTuneInAddToIgnore', FAutoTuneInAddToIgnore);
  FStorage.Write('AutoRemoveSavedFromWishlist', FAutoRemoveSavedFromWishlist);
  FStorage.Write('SubmitStats', FSubmitStats);
  FStorage.Write('SubmitStreamInfo', FSubmitStreamInfo);
  FStorage.Write('AutoTuneInMinKbps', FAutoTuneInMinKbps);
  FStorage.Write('AutoTuneInFormat', FAutoTuneInFormat);
  FStorage.Write('LimitSpeed', FLimitSpeed);
  FStorage.Write('MaxSpeed', FMaxSpeed);
  FStorage.Write('LastBrowserUpdate', FLastBrowserUpdate);
  FStorage.Write('AutomaticFilePattern', FAutomaticFilePattern);

  FStorage.Write('MinDiskSpace', FMinDiskSpace);
  FStorage.Write('DefaultAction', Integer(FDefaultAction));
  FStorage.Write('DefaultActionBrowser', Integer(FDefaultActionBrowser));
  FStorage.Write('PlayerVolume', FPlayerVolume);
  FStorage.Write('PlayerVolumeBeforeMute', FPlayerVolumeBeforeMute);
  FStorage.Write('AutoScrollLog', FAutoScrollLog);
  FStorage.Write('UserWasSetup', FUserWasSetup);
  FStorage.Write('User', FUser);
  FStorage.Write('Pass', EncodeU(CryptStr(FPass)));
  FStorage.Write('SoundDevice', FSoundDevice);

  FStorage.Write('ShortcutPlay', FShortcutPlay);
  FStorage.Write('ShortcutPause', FShortcutPause);
  FStorage.Write('ShortcutStop', FShortcutStop);
  FStorage.Write('ShortcutNext', FShortcutNext);
  FStorage.Write('ShortcutPrev', FShortcutPrev);
  FStorage.Write('ShortcutVolDown', FShortcutVolDown);
  FStorage.Write('ShortcutVolUp', FShortcutVolUp);
  FStorage.Write('ShortcutMute', FShortcutMute);

  for i := 0 to High(FClientHeaderWidth) do
    if i <> 1 then
      FStorage.Write('HeaderWidth' + IntToStr(i), FClientHeaderWidth[i], 'Cols');
  FStorage.Write('ClientCols', FClientCols, 'Cols');

  for i := 0 to High(FSavedHeaderWidth) do
    if i <> 1 then
      FStorage.Write('SavedHeaderWidth' + IntToStr(i), FSavedHeaderWidth[i], 'Cols');
  FStorage.Write('SavedCols', FSavedCols, 'Cols');

  FStorage.DeleteKey('Plugins');

  n := 0;
  for i := 0 to FStreamSettings.PostProcessors.Count - 1 do
    if not FStreamSettings.PostProcessors[i].Hidden then
    if (FStreamSettings.PostProcessors[i] is TExternalPostProcess) then
    begin
      FStorage.Write('Active_' + IntToStr(n), TExternalPostProcess(FStreamSettings.PostProcessors[i]).Active, 'Plugins');
      FStorage.Write('Exe_' + IntToStr(n), TExternalPostProcess(FStreamSettings.PostProcessors[i]).Exe, 'Plugins');
      FStorage.Write('Params_' + IntToStr(n), TExternalPostProcess(FStreamSettings.PostProcessors[i]).Params, 'Plugins');
      FStorage.Write('OrderExe_' + IntToStr(n), FStreamSettings.PostProcessors[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + IntToStr(n), FStreamSettings.PostProcessors[i].OnlyIfCut, 'Plugins');
      FStorage.Write('Group_' + IntToStr(n), FStreamSettings.PostProcessors[i].GroupID, 'Plugins');
      Inc(n);
    end else if (FStreamSettings.PostProcessors[i] is TInternalPostProcess) then
    begin
      FStorage.Write('Active_' + FStreamSettings.PostProcessors[i].ClassName, FStreamSettings.PostProcessors[i].Active, 'Plugins');
      if FStreamSettings.PostProcessors[i].GroupID = 1 then
        FStorage.Write('Order_' + FStreamSettings.PostProcessors[i].ClassName, FStreamSettings.PostProcessors[i].Order, 'Plugins')
      else
        FStorage.Write('Order_' + FStreamSettings.PostProcessors[i].ClassName, FStreamSettings.PostProcessors[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + FStreamSettings.PostProcessors[i].ClassName, FStreamSettings.PostProcessors[i].OnlyIfCut, 'Plugins');
      FStreamSettings.PostProcessors[i].Save;
    end;

  for i := 0 to FStreamSettings.EncoderSettings.Count - 1 do
    FStreamSettings.EncoderSettings[i].Save;

  FStorage.Write('EQEnabled', FEQEnabled, 'Equalizer');
  for i := 0 to High(FEQGain) do
    FStorage.Write('EQBand' + IntToStr(i), FEQGain[i] + 15, 'Equalizer');
end;

{ TStreamSettings }

function TStreamSettings.Copy: TStreamSettings;
begin
  Result := TStreamSettings.Create;
  Result.Assign(Self);
end;

constructor TStreamSettings.Create(InitStuff: Boolean = True);
begin
  inherited Create;

  FIgnoreTrackChangePattern := TStringList.Create;
  FPostProcessors := TPostProcessorList.Create;
  FEncoderSettings := TEncoderSettingsList.Create;

  if InitStuff then
    Self.InitStuff;
end;

destructor TStreamSettings.Destroy;
var
  i: Integer;
begin
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

procedure TStreamSettings.InitStuff;
begin
  FEncoderSettings.Add(TEncoderSettings.Create(atMPEG, brVBR, vqMedium));
  FEncoderSettings.Add(TEncoderSettings.Create(atAAC, brVBR, vqMedium));
  FEncoderSettings.Add(TEncoderSettings.Create(atOGG, brVBR, vqMedium));

  // Der Convert muss der erste sein! Greife auf die Liste mal mit [0] zu!
  PostProcessors.Add(TPostProcessConvert.Create);
  PostProcessors.Add(TPostProcessSetTags.Create);
  PostProcessors.Add(TPostProcessSoX.Create);
  PostProcessors.Add(TPostProcessMP4Box.Create);
end;

class function TStreamSettings.Load(Stream: TExtendedStream;
  Version: Integer): TStreamSettings;
var
  B: Byte;
  i, Count, FilterTmp, TypeTmp: Integer;
  T: TPostProcessTypes;
  AT: TAudioTypes;
  IgnoreTmp: string;
  PP: TPostProcessBase;
  ES: TEncoderSettings;
begin
  Result := TStreamSettings.Create;

  if Version < 15 then
  begin
    Result.FTitlePattern := '(?P<a>.*) - (?P<t>.*)';
    Stream.Read(Result.FFilePattern);
  end else
  begin
    Stream.Read(Result.FTitlePattern);
    Stream.Read(Result.FFilePattern);
  end;

  if Version >= 17 then
  begin
    Stream.Read(Result.FIncompleteFilePattern);
    if Result.FIncompleteFilePattern = '' then
      Result.FIncompleteFilePattern := Result.FFilePattern;
  end else
    Result.FIncompleteFilePattern := Result.FFilePattern;

  if Version >= 31 then
  begin
    Stream.Read(Result.FStreamFilePattern);
    if Result.FStreamFilePattern = '' then
      Result.FStreamFilePattern := '%s';
  end else
    Result.FStreamFilePattern := '%s';

  if Version >= 14 then
    Stream.Read(Result.FFilePatternDecimals)
  else
    Result.FFilePatternDecimals := 3;

  if Version >= 20 then
    Stream.Read(Result.FRemoveChars)
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

  Stream.Read(Result.FSilenceLevel);
  Stream.Read(Result.FSilenceLength);

  if Version >= 33 then
  begin
    Stream.Read(Result.FSilenceBufferSecondsStart);
    Stream.Read(Result.FSilenceBufferSecondsEnd);
  end else
  begin
    if Version >= 9 then
    begin
      Stream.Read(Result.FSilenceBufferSecondsStart);
      Result.FSilenceBufferSecondsEnd := Result.FSilenceBufferSecondsStart;
    end else
    begin
      Result.FSilenceBufferSecondsStart := 5;
      Result.FSilenceBufferSecondsEnd := 5;
    end;
  end;

  if Version >= 9 then
    Stream.Read(Result.FShortLengthSeconds)
  else
  begin
    Stream.Read(FilterTmp);
    Result.FShortLengthSeconds := 45;
  end;

  if Version >= 9 then
  begin
    Stream.Read(Result.FSongBuffer);
    if Version < 42 then
      Result.FSongBuffer := Result.FSongBuffer * 1000;
  end else
  begin
    Stream.Read(FilterTmp);
    Result.FSongBuffer := 0;
  end;

  Stream.Read(Result.FMaxRetries);

  if Version >= 7 then
    Stream.Read(Result.FRetryDelay)
  else
    Result.FRetryDelay := AppGlobals.StreamSettings.RetryDelay;
  Stream.Read(FilterTmp);
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

  if not Result.FSeparateTracks then
    Result.FDeleteStreams := False;

  if (FilterTmp > Ord(High(TUseFilters))) or
     (FilterTmp < Ord(Low(TUseFilters))) then
    Result.FFilter := ufNone
  else
    if Version > 26 then
      Result.FFilter := TUseFilters(FilterTmp)
    else
    begin
      if FilterTmp = 0 then
        Result.FFilter := ufNone
      else if FilterTmp = 1 then
        Result.FFilter := ufWish
      else if FilterTmp = 2 then
        Result.FFilter := ufIgnoreBoth
      else if FilterTmp = 3 then
        Result.FFilter := ufBoth
      else Result.FFilter := ufNone;
    end;

  if Version >= 28 then
  begin
    Stream.Read(Result.FAdjustTrackOffset);
    Stream.Read(Result.FAdjustTrackOffsetMS);

    if Version < 37 then
      Result.FAdjustTrackOffsetMS := Result.FAdjustTrackOffsetMS * 1000;

    Stream.Read(B);
    Result.FAdjustTrackOffsetDirection := TTrackOffsetDirection(B);
  end;

  if Version >= 26 then
  begin
    Stream.Read(Count);
    for i := 0 to Count - 1 do
    begin
      Stream.Read(IgnoreTmp);
      Result.FIgnoreTrackChangePattern.Add(IgnoreTmp);
    end;
  end;

  // Einstellungen laden...
  if Version >= 41 then
  begin
    Stream.Read(TypeTmp);
    Result.FOutputFormat := TAudioTypes(TypeTmp);

    Stream.Read(Count);
    for i := 0 to Count - 1 do
    begin
      Stream.Read(TypeTmp);

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

    Stream.Read(Count);
    for i := 0 to Count - 1 do
    begin
      Stream.Read(TypeTmp);

      AT := TAudioTypes(TypeTmp);

      ES := Result.EncoderSettings.Find(AT);

      if ES <> nil then
      begin
        ES.Load(Stream, Version);
      end;
    end;
  end;
end;

procedure TStreamSettings.Save(Stream: TExtendedStream);
var
  i: Integer;
  Count: Integer;
begin
  Stream.Write(FTitlePattern);
  Stream.Write(FFilePattern);
  Stream.Write(FIncompleteFilePattern);
  Stream.Write(FStreamFilePattern);
  Stream.Write(FFilePatternDecimals);
  Stream.Write(FRemoveChars);
  Stream.Write(FNormalizeVariables);
  Stream.Write(FDeleteStreams);
  Stream.Write(FAddSavedToIgnore);
  Stream.Write(FAddSavedToStreamIgnore);
  Stream.Write(FRemoveSavedFromWishlist);
  Stream.Write(FSkipShort);
  Stream.Write(FSearchSilence);
  Stream.Write(FAutoDetectSilenceLevel);
  Stream.Write(FSilenceLevel);
  Stream.Write(FSilenceLength);
  Stream.Write(FSilenceBufferSecondsStart);
  Stream.Write(FSilenceBufferSecondsEnd);
  Stream.Write(FShortLengthSeconds);
  Stream.Write(FSongBuffer);
  Stream.Write(FMaxRetries);
  Stream.Write(FRetryDelay);
  Stream.Write(Integer(FFilter));
  Stream.Write(FSeparateTracks);
  Stream.Write(FSaveToMemory);
  Stream.Write(FOnlySaveFull);
  Stream.Write(FOverwriteSmaller);
  Stream.Write(FDiscardSmaller);

  Stream.Write(FAdjustTrackOffset);
  Stream.Write(FAdjustTrackOffsetMS);
  Stream.Write(Byte(FAdjustTrackOffsetDirection));

  Stream.Write(FIgnoreTrackChangePattern.Count);
  for i := 0 to FIgnoreTrackChangePattern.Count - 1 do
    Stream.Write(FIgnoreTrackChangePattern[i]);

  Stream.Write(Integer(FOutputFormat));

  Count := 0;
  for i := 0 to FPostProcessors.Count - 1 do
    if (not FPostProcessors[i].Hidden) and (FPostProcessors[i].PostProcessType <> ptConvert) then
      Inc(Count);
  Stream.Write(Count);
  for i := 0 to FPostProcessors.Count - 1 do
  begin
    if FPostProcessors[i].Hidden or (FPostProcessors[i].PostProcessType = ptConvert) then
      Continue;
    Stream.Write(Integer(FPostProcessors[i].PostProcessType));
    FPostProcessors[i].Save(Stream);
  end;

  Stream.Write(FEncoderSettings.Count);
  for i := 0 to FEncoderSettings.Count - 1 do
  begin
    Stream.Write(Integer(FEncoderSettings[i].AudioType));
    FEncoderSettings[i].Save(Stream);
  end;
end;

procedure TStreamSettings.Assign(From: TStreamSettings);
var
  i: Integer;
begin
  FTitlePattern := From.FTitlePattern;
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
    begin
      if PostProcessor.ClassType = Self[i].ClassType then
      begin
        Result := Self[i];
        Break;
      end;
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

constructor TEncoderSettings.Create(AudioType: TAudioTypes;
  BitrateType: TBitRates; VBRQuality: TVBRQualities);
begin
  inherited Create;

  Self.AudioType := AudioType;
  Self.BitrateType := BitrateType;
  Self.CBRBitrate := 128;
  Self.VBRQuality := VBRQuality;
end;

function TEncoderSettings.FGetHash: Cardinal;
begin
  Result := HashString(IntToStr(Integer(AudioType)) + IntToStr(Integer(BitrateType)) + IntToStr(CBRBitrate) + IntToStr(Integer(VBRQuality)));
end;

procedure TEncoderSettings.Load;
var
  Tmp: Integer;
begin
  AppGlobals.Storage.Read('CBRBitRate_' + IntToStr(Integer(AudioType)), CBRBitrate, 128, 'Encoders');
  CBRBitRate := RoundBitrate(CBRBitrate);

  AppGlobals.Storage.Read('BitRateType_' + IntToStr(Integer(AudioType)), Tmp, Integer(brVBR), 'Encoders');
  if (Tmp > Ord(High(TBitRates))) or
     (Tmp < Ord(Low(TBitRates))) then
    BitrateType := brVBR
  else
    BitrateType := TBitRates(Tmp);

  AppGlobals.Storage.Read('VBRQuality_' + IntToStr(Integer(AudioType)), Tmp, Integer(vqMedium), 'Encoders');
  if (Tmp > Ord(High(TVBRQualities))) or
     (Tmp < Ord(Low(TVBRQualities))) then
    VBRQuality := vqMedium
  else
    VBRQuality := TVBRQualities(Tmp);
end;

procedure TEncoderSettings.Load(Stream: TExtendedStream; Version: Integer);
var
  Tmp: Integer;
begin
  Stream.Read(Tmp);
  BitrateType := TBitRates(Tmp);

  Stream.Read(CBRBitrate);

  Stream.Read(Tmp);
  VBRQuality := TVBRQualities(Tmp);
end;

procedure TEncoderSettings.Save(Stream: TExtendedStream);
begin
  Stream.Write(Integer(BitrateType));
  Stream.Write(CBRBitrate);
  Stream.Write(Integer(VBRQuality));
end;

procedure TEncoderSettings.Save;
begin
  AppGlobals.Storage.Write('CBRBitRate_' + IntToStr(Integer(AudioType)), CBRBitrate, 'Encoders');
  AppGlobals.Storage.Write('BitRateType_' + IntToStr(Integer(AudioType)), Integer(BitrateType), 'Encoders');
  AppGlobals.Storage.Write('VBRQuality_' + IntToStr(Integer(AudioType)), Integer(VBRQuality), 'Encoders');
end;

initialization
  try
    if Language = nil then
      raise Exception.Create('Language is not initialized');
    AppGlobals := TAppData.Create('streamWriter');

    // PostProcessors etc. laden
    AppGlobals.StreamSettings.InitStuff;

    // AddonManager wird hier erstellt, da erstellte Addon-Items Zugriff
    // auf ein bereits zugewiesenes AppGlobals brauchen.
    AppGlobals.FAddonManager := TAddonManager.Create;
    AppGlobals.FPostProcessManager := TPostProcessManager.Create;

    if AppGlobals.AddonManager.CanEncode(AppGlobals.StreamSettings.OutputFormat) <> ceOkay then
      AppGlobals.StreamSettings.OutputFormat := atNone;
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), PChar(_('Error')), MB_ICONERROR);
      Halt;
    end;
  end;

finalization
  FreeAndNil(AppGlobals);

end.

