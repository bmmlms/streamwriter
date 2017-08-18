{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2017 Alexander Nottelmann

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
  LanguageObjects, LanguageIcons, ExtendedStream, Forms, Functions,
  AddonManager, PostProcessManager, Logging, Base64, AudioFunctions, TypeDefs,
  Messages, DataManager, SWFunctions;

type
  // Do not change the order of items in the following enums!
  // Actions that can be executed in the stream-view.
  TClientActions = (caStartStop, caStreamIntegrated, caStream);

  // Application-specific settings
  TAppData = class(TAppDataBase)
  private
    FData: TDataLists;
    FStreamSettingsObsolete: TStreamSettings;
    FUserLoggedIn: Boolean;
    FID: Integer;
    //FEasyMode: Boolean;
    FDir: string;
    FDirAuto: string;
    FTray: Boolean;
    FTrayOnMinimize: Boolean;
    FSnapMain: Boolean;
    FRememberRecordings: Boolean;
    FRememberPlaying: Boolean;
    FDisplayPlayedSong: Boolean;
    FDisplayPlayNotifications: Boolean;
    FShowSplashScreen: Boolean;
    FCoverPanelAlwaysVisible: Boolean;
    FSidebarWidth: Integer;
    FAutoTuneIn: Boolean;
    FAutoTuneInConsiderIgnore: Boolean;
    FAutoTuneInAddToIgnoreObsolete: Boolean;
    FAutoRemoveSavedFromWishlistObsolete: Boolean;
    FSubmitStreamInfo: Boolean;
    FSubmitStats: Boolean;
    FMonitorMode: Boolean;
    FMonitorCount: Cardinal;
    FMinDiskSpace: Integer;
    FLogFile: string;
    FDefaultAction: TClientActions;
    FDefaultActionBrowser: TStreamOpenActions;
    FPlayerVolume: Integer;
    FPlayerVolumeBeforeMute: Integer;
    FPlayerShuffle: Boolean;
    FUserWasSetup: Boolean;
    FUser, FPass: string;
    FSoundDevice: Cardinal;
    FAutoTuneInMinQuality: Integer;
    FAutoTuneInFormat: Cardinal;
    FLimitSpeed: Boolean;
    FMaxSpeed: Cardinal;
    FLastBrowserUpdate: Cardinal;
    FAutomaticFilePatternObsolete: string;

    FProjectHelpLinkMain: string;
    FProjectHelpLinkSettings: string;
    FProjectHelpLinkAutoSettings: string;
    FProjectHelpLinkStreamSettings: string;

    FShortcutPlay: Cardinal;
    FShortcutPause: Cardinal;
    FShortcutStop: Cardinal;
    FShortcutNext: Cardinal;
    FShortcutPrev: Cardinal;
    FShortcutVolDown: Cardinal;
    FShortcutVolUp: Cardinal;
    FShortcutMute: Cardinal;

    FClientHeaderWidthLoaded: Boolean;
    FClientHeaderPositionLoaded: Boolean;
    FClientHeaderWidth: TIntArray;
    FClientHeaderPosition: TIntArray;
    FClientCols: Integer;

    FChartHeaderWidthLoaded: Boolean;
    FChartHeaderPositionLoaded: Boolean;
    FChartHeaderWidth: TIntArray;
    FChartHeaderPosition: TIntArray;
    FChartCols: Integer;

    FListHeaderWidthLoaded: Boolean;
    FListHeaderPositionLoaded: Boolean;
    FListHeaderWidth: TIntArray;
    FListHeaderPosition: TIntArray;
    FListCols: Integer;

    FSavedHeaderWidthLoaded: Boolean;
    FSavedHeaderPositionLoaded: Boolean;
    FSavedHeaderWidth: TIntArray;
    FSavedHeaderPosition: TIntArray;
    FSavedCols: Integer;

    FLogHeaderWidthLoaded: Boolean;
    FLogHeaderPositionLoaded: Boolean;
    FLogHeaderWidth: TIntArray;
    FLogHeaderPosition: TIntArray;
    FLogCols: Integer;

    FBrowserSortType: Integer;
    FLastUsedDataVersion: Integer;
    FRecoveryFile: string;

    FEQEnabled: Boolean;
    FEQGain: array[0..9] of Integer;

    FApplyEffectFadeoutStart: Integer;
    FApplyEffectFadeoutEnd: Integer;
    FApplyEffectSilenceStart: Integer;
    FApplyEffectSilenceEnd: Integer;
    FApplyEffectNormalize: Boolean;

    FLogFilterTypes: Integer;

    FAddonManager: TAddonManager;
    FPostProcessManager: TPostProcessManager;
    FLanguageIcons: TLanguageIcons;

    FIntroShown: Boolean;

    FMutexHandleExiting: Cardinal;

    function FGetDataFile: string;

    function FGetEQGain(Idx: Integer): Integer;
    procedure FSetEQGain(Idx: Integer; Value: Integer);
  protected
    // Save ALL the things!
    procedure InitOnlyOne; override;
    procedure DoSave; override;
    procedure NotifyRunningInstance(Handle: Cardinal); override;
  public
    constructor Create(AppName: string);
    destructor Destroy; override;

    procedure Load; override;
    procedure BuildThanksText; override;
    procedure DeleteUndoFiles;

    procedure LoadData;
    procedure LoadOldStreamSettings;
    procedure InitPostProcessors;

    property Data: TDataLists read FData;
    property StreamSettingsObsolete: TStreamSettings read FStreamSettingsObsolete;
    property UserLoggedIn: Boolean read FUserLoggedIn write FUserLoggedIn;

    // The unique ID generated for this specific client
    property ID: Integer read FID;
    //property EasyMode: Boolean read FEasyMode write FEasyMode;
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
    property RememberPlaying: Boolean read FRememberPlaying write FRememberPlaying;
    property DisplayPlayedSong: Boolean read FDisplayPlayedSong write FDisplayPlayedSong;
    // When set notifications on the lower right of the screen will be displayed when a title on a stream changes
    property DisplayPlayNotifications: Boolean read FDisplayPlayNotifications write FDisplayPlayNotifications;
    property ShowSplashScreen: Boolean read FShowSplashScreen write FShowSplashScreen;
    property CoverPanelAlwaysVisible: Boolean read FCoverPanelAlwaysVisible write FCoverPanelAlwaysVisible;
    // Defines the width of the sidebar (streams/info/log)
    property SidebarWidth: Integer read FSidebarWidth write FSidebarWidth;
    // When set streamWriter automatically records songs from the wishlist
    property AutoTuneIn: Boolean read FAutoTuneIn write FAutoTuneIn;
    // When set streamWriter will not record songs automatically when they are on the ignorelist
    property AutoTuneInConsiderIgnore: Boolean read FAutoTuneInConsiderIgnore write FAutoTuneInConsiderIgnore;
    // When set automatically recorded songs will be added to the global ignorelist
    property AutoTuneInAddToIgnoreObsolete: Boolean read FAutoTuneInAddToIgnoreObsolete write FAutoTuneInAddToIgnoreObsolete;
    // When set automatically recorded songs will be removed from the wishlist
    property AutoRemoveSavedFromWishlistObsolete: Boolean read FAutoRemoveSavedFromWishlistObsolete write FAutoRemoveSavedFromWishlistObsolete;
    // When set information about streams the user records will be sent to the server (only the URL of the stream)
    property SubmitStreamInfo: Boolean read FSubmitStreamInfo write FSubmitStreamInfo;
    // When set some statistics will be sent to the server (number of recoring streams/automatically recording streams)
    property SubmitStats: Boolean read FSubmitStats write FSubmitStats;
    property MonitorMode: Boolean read FMonitorMode write FMonitorMode;
    property MonitorCount: Cardinal read FMonitorCount write FMonitorCount;
    // The minimum amount of free disk space that has to be available in order to record streams
    property MinDiskSpace: Integer read FMinDiskSpace write FMinDiskSpace;
    property LogFile: string read FLogFile write FLogFile;
    // The default action to execute when double-clicking a stream in the mainview
    property DefaultAction: TClientActions read FDefaultAction write FDefaultAction;
    // The default action to execute when double-clicking a stream in the streamview
    property DefaultActionBrowser: TStreamOpenActions read FDefaultActionBrowser write FDefaultActionBrowser;
    // The volume of the player
    property PlayerVolume: Integer read FPlayerVolume write FPlayerVolume;
    // The volume of the player before muting the volume
    property PlayerVolumeBeforeMute: Integer read FPlayerVolumeBeforeMute write FPlayerVolumeBeforeMute;
    property PlayerShuffle: Boolean read FPlayerShuffle write FPlayerShuffle;
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
    // Minimum quality needed for automatic recording of a stream
    property AutoTuneInMinQuality: Integer read FAutoTuneInMinQuality write FAutoTuneInMinQuality;
    // Desired format of streams to tune in automatically
    property AutoTuneInFormat: Cardinal read FAutoTuneInFormat write FAutoTuneInFormat;
    // When set the overall speedlimit is active
    property LimitSpeed: Boolean read FLimitSpeed write FLimitSpeed;
    // Overall speedlimit for recording/playback
    property MaxSpeed: Cardinal read FMaxSpeed write FMaxSpeed;
    // Time of last browser update (Browser will be updated automatically in specific intervals)
    property LastBrowserUpdate: Cardinal read FLastBrowserUpdate write FLastBrowserUpdate;
    // The pattern for automatically recorded files
    property AutomaticFilePatternObsolete: string read FAutomaticFilePatternObsolete write FAutomaticFilePatternObsolete;

    property ClientHeaderWidthLoaded: Boolean read FClientHeaderWidthLoaded;
    property ClientHeaderPositionLoaded: Boolean read FClientHeaderPositionLoaded;
    property ClientHeaderWidth: TIntArray read FClientHeaderWidth write FClientHeaderWidth;
    property ClientHeaderPosition: TIntArray read FClientHeaderPosition write FClientHeaderPosition;
    property ClientCols: Integer read FClientCols write FClientCols;

    property ChartHeaderWidthLoaded: Boolean read FChartHeaderWidthLoaded;
    property ChartHeaderPositionLoaded: Boolean read FChartHeaderPositionLoaded;
    property ChartHeaderWidth: TIntArray read FChartHeaderWidth write FChartHeaderWidth;
    property ChartHeaderPosition: TIntArray read FChartHeaderPosition write FChartHeaderPosition;
    property ChartCols: Integer read FChartCols write FChartCols;

    property ListHeaderWidthLoaded: Boolean read FListHeaderWidthLoaded;
    property ListHeaderPositionLoaded: Boolean read FListHeaderPositionLoaded;
    property ListHeaderWidth: TIntArray read FListHeaderWidth write FListHeaderWidth;
    property ListHeaderPosition: TIntArray read FListHeaderPosition write FListHeaderPosition;
    property ListCols: Integer read FListCols write FListCols;

    property SavedHeaderWidthLoaded: Boolean read FSavedHeaderWidthLoaded;
    property SavedHeaderPositionLoaded: Boolean read FSavedHeaderPositionLoaded;
    property SavedHeaderWidth: TIntArray read FSavedHeaderWidth write FSavedHeaderWidth;
    property SavedHeaderPosition: TIntArray read FSavedHeaderPosition write FSavedHeaderPosition;
    property SavedCols: Integer read FSavedCols write FSavedCols;

    property LogHeaderWidthLoaded: Boolean read FLogHeaderWidthLoaded;
    property LogHeaderPositionLoaded: Boolean read FLogHeaderPositionLoaded;
    property LogHeaderWidth: TIntArray read FLogHeaderWidth write FLogHeaderWidth;
    property LogHeaderPosition: TIntArray read FLogHeaderPosition write FLogHeaderPosition;
    property LogCols: Integer read FLogCols write FLogCols;

    property BrowserSortType: Integer read FBrowserSortType write FBrowserSortType;
    // Last used version of the data-file format
    property LastUsedDataVersion: Integer read FLastUsedDataVersion write FLastUsedDataVersion;
    // Path to the recovery-file (this is set if streamWriter crashed or something)
    property RecoveryFile: string read FRecoveryFile;

    property EQEnabled: Boolean read FEQEnabled write FEQEnabled;
    property EQGain[Idx: Integer]: Integer read FGetEQGain write FSetEQGain;

    property ApplyEffectFadeoutStart: Integer read FApplyEffectFadeoutStart write FApplyEffectFadeoutStart;
    property ApplyEffectFadeoutEnd: Integer read FApplyEffectFadeoutEnd write FApplyEffectFadeoutEnd;
    property ApplyEffectSilenceStart: Integer read FApplyEffectSilenceStart write FApplyEffectSilenceStart;
    property ApplyEffectSilenceEnd: Integer read FApplyEffectSilenceEnd write FApplyEffectSilenceEnd;
    property ApplyEffectNormalize: Boolean read FApplyEffectNormalize write FApplyEffectNormalize;

    property LogFilterTypes: Integer read FLogFilterTypes write FLogFilterTypes;

    // Path to streamWriter's data-file
    property DataFile: string read FGetDataFile;

    // The manager for addons
    property AddonManager: TAddonManager read FAddonManager;

    // The manager for postprocessing
    property PostProcessManager: TPostProcessManager read FPostProcessManager;

    // Icons for languages
    property LanguageIcons: TLanguageIcons read FLanguageIcons;

    property IntroShown: Boolean read FIntroShown write FIntroShown;

    property MutexHandleExiting: Cardinal read FMutexHandleExiting;

    property ProjectHelpLinkMain: string read FProjectHelpLinkMain;
    property ProjectHelpLinkSettings: string read FProjectHelpLinkSettings;
    property ProjectHelpLinkAutoSettings: string read FProjectHelpLinkAutoSettings;
    property ProjectHelpLinkStreamSettings: string read FProjectHelpLinkStreamSettings;
  end;

procedure CreateAppData;
function InitAppDataStageOne: Boolean;
function InitAppDataStageTwo: Boolean;

const LoadErrorMsg = 'You can delete it to avoid this error when streamWriter starts by clicking "Yes".'#13#10 +
                     'WARNING: When clicking "Yes" all data saved in the file will be lost!'#13#10 +
                     'The file will not be overwritten with new data until it was loaded or deleted.';

var
  AppGlobals: TAppData;

implementation

uses
  PostProcess;

{ TAppData }

constructor TAppData.Create(AppName: string);
var
  i, W, H: Integer;
begin
  // Create an instance for global stream-settings
  // (these are used for new streams that do not have user-specified settings)
  FData := TDataLists.Create;
  FStreamSettingsObsolete := TStreamSettings.Create(False);

  // The number of the current build
  FBuildNumber := 751;
  FCodename := 'Tranquillo';

  // Adjust dimensions of the main-form
  W := 900;
  H := 630;
  if Screen.WorkAreaWidth < W then
    W := Screen.WorkAreaWidth - 20;
  if Screen.WorkAreaHeight < H then
    H := Screen.WorkAreaHeight - 20;

  // Adjust count of column headers and initialize them.
  // Initialization is important because otherwise when finishing the wizard on
  // first run writes "0" to all columns. When streamWriter crashes afterwards,
  // every VirtualTreeView has all columns with Width=0. Initializing it with
  // -1 tells the Load() procedure to skip these columns.
  SetLength(FClientHeaderWidth, 6);
  for i := 0 to High(FClientHeaderWidth) do
    FClientHeaderWidth[i] := -1;

  SetLength(FClientHeaderPosition, 6);
  for i := 0 to High(FClientHeaderPosition) do
    FClientHeaderPosition[i] := -1;

  SetLength(FChartHeaderWidth, 4);
  for i := 0 to High(FChartHeaderWidth) do
    FChartHeaderWidth[i] := -1;

  SetLength(FChartHeaderPosition, 4);
  for i := 0 to High(FChartHeaderPosition) do
    FChartHeaderPosition[i] := -1;

  SetLength(FListHeaderWidth, 3);
  for i := 0 to High(FListHeaderWidth) do
    FListHeaderWidth[i] := -1;

  SetLength(FListHeaderPosition, 3);
  for i := 0 to High(FListHeaderPosition) do
    FListHeaderPosition[i] := -1;

  SetLength(FSavedHeaderWidth, 7);
  for i := 0 to High(FSavedHeaderWidth) do
    FSavedHeaderWidth[i] := -1;

  SetLength(FSavedHeaderPosition, 7);
  for i := 0 to High(FSavedHeaderPosition) do
    FSavedHeaderPosition[i] := -1;

  SetLength(FLogHeaderWidth, 4);
  for i := 0 to High(FLogHeaderWidth) do
    FLogHeaderWidth[i] := -1;

  SetLength(FLogHeaderPosition, 4);
  for i := 0 to High(FLogHeaderPosition) do
    FLogHeaderPosition[i] := -1;

  // Set some application-specific settings
  SetLength(FProjectUpdateLinks, 2);
  {$IFDEF DEBUG}
  FProjectUpdateLinks[0] := 'https://streamwriter.gaia/';
  FProjectUpdateLinks[1] := 'https://streamwriter.gaia/';
  {$ELSE}
  FProjectUpdateLinks[0] := 'https://streamwriter.org/';
  FProjectUpdateLinks[1] := 'https://streamwriter.de/';
  {$ENDIF}
  FProjectHomepageLink := 'https://streamwriter.org/';
  FProjectLink := 'https://streamwriter.org/';

  FProjectHelpLink := 'https://streamwriter.org/wiki/artikel/help/';
  FProjectHelpLinkMain := 'https://streamwriter.org/wiki/artikel/mainwindow/';
  FProjectHelpLinkSettings := 'https://streamwriter.org/wiki/artikel/settings/';
  FProjectHelpLinkAutoSettings := 'https://streamwriter.org/wiki/artikel/autosettings/';
  FProjectHelpLinkStreamSettings := 'https://streamwriter.org/wiki/artikel/streamsettings/';

  FProjectForumLink := 'https://streamwriter.org/forum/';
  FProjectDonateLink := 'https://streamwriter.org/inhalt/donate/';

  // Call the base-constructor with our defined variables
  inherited Create(AppName, True, W, H, alGPL);

  // Set the name for the recovery-file
  FRecoveryFile := FStorage.DataDir + 'streamwriter_data_recovery.dat';

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
  FStreamSettingsObsolete.Free;
  FData.Free;

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

procedure TAppData.InitOnlyOne;
begin
  inherited;

  // This mutex is released when the application exits but did not finish
  // every finializaion (save to datafile, registry, etc)
  // Required to make Inno Setup more comfortable :)
  if MutexHandle > 0 then
    FMutexHandleExiting := CreateMutex(nil, True, PChar(AppName + 'MutexExiting'));
end;

procedure TAppData.InitPostProcessors;
var
  i: Integer;
  App, Params: string;
  EP: TExternalPostProcess;
  Active, OnlyIfCut: Boolean;
  Order, GroupID: Integer;
begin
  StreamSettingsObsolete.InitStuff(True);
  Data.DefaultStreamSettings.InitStuff;
  Data.StreamSettings.InitStuff;
  Data.AutoRecordSettings.InitStuff;

  // REMARK: Das kann raus, wenn DataVersion >= 61 gut verbreitet ist... Nur für Abwärtskompatibilität!
  i := 0;
  repeat
    AppGlobals.Storage.Read('Exe_' + IntToStr(i), App, '', 'Plugins');
    AppGlobals.Storage.Read('Params_' + IntToStr(i), Params, '', 'Plugins');
    AppGlobals.Storage.Read('OrderExe_' + IntToStr(i), Order, 0, 'Plugins');
    AppGlobals.Storage.Read('Active_' + IntToStr(i), Active, False, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + IntToStr(i), OnlyIfCut, False, 'Plugins');
    AppGlobals.Storage.Read('Group_' + IntToStr(i), GroupID, 1, 'Plugins');
    if App <> '' then
    begin
      EP := TExternalPostProcess.Create(App, Params, Active, OnlyIfCut, i, Order, GroupID);
      try
        AppGlobals.StreamSettingsObsolete.PostProcessors.Add(EP);
      except

      end;
    end;
    Inc(i);
  until (App = '');
end;

procedure TAppData.BuildThanksText;
var
  Res: TResourceStream;
  Data: RawByteString;
begin
  inherited;

  try
    Res := TResourceStream.Create(HInstance, 'THANKSTEXT_' + UpperCase(LanguageObjects.Language.CurrentLanguage.ID), RT_RCDATA);
  except
    Res := TResourceStream.Create(HInstance, 'THANKSTEXT_EN', RT_RCDATA);
  end;

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
  i, DefaultActionTmp, DefaultActionBrowser: Integer;
  TmpStr: string;
begin
  inherited;

  FStorage.Read('ID', FID, 0);
  if FID < 1 then
  begin
    FID := GetID;
  end;

  FStorage.Read('LastUsedDataVersion', FLastUsedDataVersion, 0);

  //FStorage.Read('EasyMode', FEasyMode, FLastUsedDataVersion = 0);

  // Remark: Dran denken irgendwann StreamSettingsObsolete zu entfernen!

  FStorage.Read('Dir', FDir, '');
  if FDir <> '' then
    FDir := IncludeTrailingBackslash(FDir);
  FDir := TryUnRelativePath(FDir);
  if not DirectoryExists(FDir) then
    FDir := '';

  FStorage.Read('DirAuto', FDirAuto, '');
  if FDirAuto = '' then
    FDirAuto := FDir;
  if FDirAuto <> '' then
    FDirAuto := IncludeTrailingBackslash(FDirAuto);
  FDirAuto := TryUnRelativePath(FDirAuto);
  if not DirectoryExists(FDirAuto) then
    FDirAuto := '';

  FStorage.Read('TrayClose', FTray, False);
  FStorage.Read('TrayOnMinimize', FTrayOnMinimize, False);
  FStorage.Read('SnapMain', FSnapMain, False);
  FStorage.Read('RememberRecordings', FRememberRecordings, False);
  FStorage.Read('RememberPlaying', FRememberPlaying, False);
  FStorage.Read('DisplayPlayedSong', FDisplayPlayedSong, True);
  FStorage.Read('DisplayPlayNotifications', FDisplayPlayNotifications, True);
  FStorage.Read('ShowSplashScreen', FShowSplashScreen, True);
  FStorage.Read('CoverPanelAlwaysVisible', FCoverPanelAlwaysVisible, False);
  FStorage.Read('SidebarWidth', FSidebarWidth, MulDiv(220, Screen.PixelsPerInch, 96));
  FStorage.Read('AutoTuneIn', FAutoTuneIn, True);
  FStorage.Read('AutoTuneInConsiderIgnore', FAutoTuneInConsiderIgnore, False);
  FStorage.Read('AutoTuneInAddToIgnore', FAutoTuneInAddToIgnoreObsolete, False);
  FStorage.Read('AutoRemoveSavedFromWishlist', FAutoRemoveSavedFromWishlistObsolete, False);
  FStorage.Read('SubmitStreamInfo', FSubmitStreamInfo, True);
  FStorage.Read('SubmitStats', FSubmitStats, True);
  FStorage.Read('MonitorMode', FMonitorMode, True);

  FStorage.Read('MonitorCount', FMonitorCount, 3);
  if FMonitorCount > 999 then
    FMonitorCount := 999;

  FStorage.Read('LimitSpeed', FLimitSpeed, False);
  FStorage.Read('MaxSpeed', FMaxSpeed, 0);
  if FMaxSpeed <= 0 then
    FLimitSpeed := False;
  FStorage.Read('LastBrowserUpdate', FLastBrowserUpdate, Trunc(Now));

  FStorage.Read('AutomaticFilePattern', FAutomaticFilePatternObsolete, '%streamname%\%artist% - %title%');
  if (LastUsedDataVersion > 0) and (LastUsedDataVersion < 64) and (FAutomaticFilePatternObsolete <> '%streamname%\%artist% - %title%') then
    FAutomaticFilePatternObsolete := ConvertPattern(FAutomaticFilePatternObsolete);

  FStorage.Read('AutoTuneInMinQuality', FAutoTuneInMinQuality, 2);
  if (FAutoTuneInMinQuality > 2) or (FAutoTuneInMinQuality < 0) then
    FAutoTuneInMinQuality := 2;

  FStorage.Read('AutoTuneInFormat', FAutoTuneInFormat, 0);
  if FAutoTuneInFormat > 2 then
    FAutoTuneInFormat := 0;

  FStorage.Read('MinDiskSpace', FMinDiskSpace, 5);
  FStorage.Read('LogFile', FLogFile, '');
  FStorage.Read('DefaultAction', DefaultActionTmp, Integer(caStartStop));
  FStorage.Read('DefaultActionBrowser', DefaultActionBrowser, Integer(oaStart));
  FStorage.Read('PlayerVolume', FPlayerVolume, 50);
  FStorage.Read('PlayerVolumeBeforeMute', FPlayerVolumeBeforeMute, 50);
  FStorage.Read('PlayerShuffle', FPlayerShuffle, False);

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

  // Default-Device is now available in BASS, so set it for every user to this device
  // after upgrading...
  if FLastUsedDataVersion < 61 then
    FSoundDevice := 0;

  FStorage.Read('ShortcutPlay', FShortcutPlay, 0);
  FStorage.Read('ShortcutPause', FShortcutPause, 0);
  FStorage.Read('ShortcutStop', FShortcutStop, 0);
  FStorage.Read('ShortcutNext', FShortcutNext, 0);
  FStorage.Read('ShortcutPrev', FShortcutPrev, 0);
  FStorage.Read('ShortcutVolDown', FShortcutVolDown, 0);
  FStorage.Read('ShortcutVolUp', FShortcutVolUp, 0);
  FStorage.Read('ShortcutMute', FShortcutMute, 0);

  // Header of ClientView
  TmpStr := 'ClientHeaderWidth';
  FStorage.Read(TmpStr + '0', i, -1, 'Cols');
  if i = -1 then
  begin
    TmpStr := 'HeaderWidth';
    FStorage.Read('HeaderWidth' + '0', i, -1, 'Cols');
  end;
  if i > -1 then
  begin
    FClientHeaderWidthLoaded := True;
    for i := 0 to High(FClientHeaderWidth) do
      if i <> 1 then
        FStorage.Read(TmpStr + IntToStr(i), FClientHeaderWidth[i], 70, 'Cols');
  end;

  FStorage.Read('ClientHeaderPosition0', i, -1, 'Cols');
  if i > -1 then
  begin
    FClientHeaderPositionLoaded := True;
    for i := 1 to High(FClientHeaderPosition) do
      FStorage.Read('ClientHeaderPosition' + IntToStr(i), FClientHeaderPosition[i], 100, 'Cols');
  end;

  FStorage.Read('ClientCols', FClientCols, 255, 'Cols');
  FClientCols := FClientCols or (1 shl 0);

  // Header of ChartsView
  FStorage.Read('ChartHeaderWidth0', i, -1, 'Cols');
  if i > -1 then
  begin
    FChartHeaderWidthLoaded := True;
    for i := 0 to High(FChartHeaderWidth) do
      FStorage.Read('ChartHeaderWidth' + IntToStr(i), FChartHeaderWidth[i], 120, 'Cols');
  end;

  FStorage.Read('ChartHeaderPosition0', i, -1, 'Cols');
  if i > -1 then
  begin
    FChartHeaderPositionLoaded := True;
    for i := 0 to High(FChartHeaderPosition) do
      FStorage.Read('ChartHeaderPosition' + IntToStr(i), FChartHeaderPosition[i], 100, 'Cols');
  end;

  FStorage.Read('ChartCols', FChartCols, 255, 'Cols');
  FChartCols := FChartCols or (1 shl 0);

  // Header of ListsView
  FStorage.Read('ListHeaderWidth0', i, -1, 'Cols');
  if i > -1 then
  begin
    FListHeaderWidthLoaded := True;
    for i := 0 to High(FListHeaderWidth) do
      FStorage.Read('ListHeaderWidth' + IntToStr(i), FListHeaderWidth[i], 120, 'Cols');
  end;

  FStorage.Read('ListHeaderPosition0', i, -1, 'Cols');
  if i > -1 then
  begin
    FListHeaderPositionLoaded := True;
    for i := 0 to High(FListHeaderWidth) do
      FStorage.Read('ListHeaderPosition' + IntToStr(i), FListHeaderPosition[i], 100, 'Cols');
  end;

  FStorage.Read('ListCols', FListCols, 255, 'Cols');
  FListCols := FListCols or (1 shl 0);

  // Header of SavedView
  FStorage.Read('SavedHeaderWidth0', i, -1, 'Cols');
  if i > -1 then
  begin
    FSavedHeaderWidthLoaded := True;
    for i := 0 to High(FSavedHeaderWidth) do
      FStorage.Read('SavedHeaderWidth' + IntToStr(i), FSavedHeaderWidth[i], 120, 'Cols');
  end;

  FStorage.Read('SavedHeaderPosition0', i, -1, 'Cols');
  if i > -1 then
  begin
    FSavedHeaderPositionLoaded := True;
    for i := 0 to High(FSavedHeaderPosition) do
      FStorage.Read('SavedHeaderPosition' + IntToStr(i), FSavedHeaderPosition[i], 100, 'Cols');
  end;

  FStorage.Read('SavedCols', FSavedCols, 255, 'Cols');
  FSavedCols := FSavedCols or (2 shl 0);

  // Header of LogView
  FStorage.Read('LogHeaderWidth0', i, -1, 'Cols');
  if i > -1 then
  begin
    FLogHeaderWidthLoaded := True;
    for i := 0 to High(FLogHeaderWidth) do
      FStorage.Read('LogHeaderWidth' + IntToStr(i), FLogHeaderWidth[i], 120, 'Cols');
  end;

  FStorage.Read('LogHeaderPosition0', i, -1, 'Cols');
  if i > -1 then
  begin
    FLogHeaderPositionLoaded := True;
    for i := 0 to High(FLogHeaderPosition) do
      FStorage.Read('LogHeaderPosition' + IntToStr(i), FLogHeaderPosition[i], 100, 'Cols');
  end;

  FStorage.Read('LogCols', FLogCols, 255, 'Cols');
  FLogCols := FLogCols or (1 shl 0);
  FLogCols := FLogCols or (1 shl 3);

  FStorage.Read('BrowserSortType', FBrowserSortType, 3);

  if (DefaultActionTmp > Ord(High(TClientActions))) or
     (DefaultActionTmp < Ord(Low(TClientActions))) then
    FDefaultAction := caStartStop
  else
    FDefaultAction := TClientActions(DefaultActionTmp);

  if (DefaultActionBrowser > Ord(High(TStreamOpenActions))) or
     (DefaultActionBrowser < Ord(Low(TStreamOpenActions))) then
    FDefaultActionBrowser := oaStart
  else
    FDefaultActionBrowser := TStreamOpenActions(DefaultActionBrowser);

  FStorage.Read('EQEnabled', FEQEnabled, False, 'Equalizer');
  for i := 0 to High(FEQGain) do
  begin
    FStorage.Read('EQBand' + IntToStr(i), FEQGain[i], 15, 'Equalizer');
    FEQGain[i] := FEQGain[i] - 15;
    if (FEQGain[i] > 15) or (FEQGain[i] < -15) then
      FEQGain[i] := 0;
  end;

  FStorage.Read('ApplyEffectFadeoutStart', FApplyEffectFadeoutStart, 5);
  FStorage.Read('ApplyEffectFadeoutEnd', FApplyEffectFadeoutEnd, 5);
  FStorage.Read('ApplyEffectSilenceStart', FApplyEffectSilenceStart, 3);
  FStorage.Read('ApplyEffectSilenceEnd', FApplyEffectSilenceEnd, 3);
  FStorage.Read('ApplyEffectNormalize', FApplyEffectNormalize, False);

  FStorage.Read('LogFilterTypes', FLogFilterTypes, 7);

  FStorage.Read('IntroShown', FIntroShown, False);
end;

procedure TAppData.LoadData;
var
  Recovered: Boolean;
  Res: Integer;
begin
  Recovered := False;
  {$IFNDEF DEBUG}
  if FileExists(AppGlobals.RecoveryFile) then
  begin
    if (CommandLine.GetParam('-autoloadrecovery') <> nil)
        or (MsgBox(0, _('It seems that streamWriter has not been shutdown correctly, maybe streamWriter or your computer crashed.'#13#10'Do you want to load the latest automatically saved data?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES) then
    begin
      try
        FData.Load(True);
        Recovered := True;
      except
        MsgBox(0, _('Data could not be loaded.'), _('Error'), MB_ICONERROR);
      end;
    end;
  end;
  {$ENDIF}

  try
    if not Recovered then
      FData.Load(False);
  except
    on E: Exception do
    begin
      try
        FData.Free;
      except end;
      FData := TDataLists.Create;
      InitPostProcessors;

      // Damit beim Beenden nichts überschrieben wird.
      FData.LoadError := True;

      if E is EVersionException then
      begin
        Res := MsgBox(0, Format(_('The file "%s" could not be loaded because it was saved with a newer version of streamWriter. ' +
                                  'To use the current file exit streamWriter and use a newer version of the application.') + #13#10 + _(LoadErrorMsg),
                                [E.Message]), _('Info'), MB_YESNO or MB_ICONEXCLAMATION or MB_DEFBUTTON2);
      end else if E is EUnsupportedFormatException then
      begin
        Res := MsgBox(0, Format(_('The file "%s" could not be loaded because it is contains an exported profile and no regular saved data.') + #13#10 + _(LoadErrorMsg),
                                [E.Message]), _('Info'), MB_YESNO or MB_ICONEXCLAMATION or MB_DEFBUTTON2);
      end else if E is EUnknownFormatException then
      begin
        Res := MsgBox(0, Format(_('The file "%s" could not be loaded because it''s format is unknown.') + #13#10 + _(LoadErrorMsg),
                                [E.Message]), _('Info'), MB_YESNO or MB_ICONEXCLAMATION or MB_DEFBUTTON2);
      end else
      begin
        Res := MsgBox(0, Format(_('The file "%s" could not be loaded because it is corrupted.') + #13#10 + _(LoadErrorMsg),
                                [E.Message]), _('Info'), MB_YESNO or MB_ICONEXCLAMATION or MB_DEFBUTTON2);
      end;

      if Res = IDYES then
      begin
        DeleteFile(E.Message);
        FData.LoadError := False;
      end;
    end;
  end;
end;

procedure TAppData.NotifyRunningInstance(Handle: Cardinal);
var
  s: string;
  CDS: TCOPYDATASTRUCT;
begin
  s := GetCommandLineW;
  CDS.dwData := 0;
  CDS.cbData := (Length(s) * SizeOf(Char)) + 2;
  CDS.lpData := PChar(s);
  SendMessage(Handle, WM_COPYDATA, 0, LongInt(@CDS));
end;

procedure TAppData.LoadOldStreamSettings;
var
  TmpStr: string;
  TmpCardinal: Cardinal;
  TmpBoolean: Boolean;
  TmpInteger: Integer;
begin
  if (FLastUsedDataVersion > 0) and (FLastUsedDataVersion < 61) then
  begin
    FStreamSettingsObsolete.RegExes.Assign(FData.DefaultStreamSettings.RegExes);

    FStorage.Read('FilePattern', TmpStr, FData.DefaultStreamSettings.FilePattern);
    FStreamSettingsObsolete.FilePattern := TmpStr;
    FStorage.Read('IncompleteFilePattern', TmpStr, FData.DefaultStreamSettings.IncompleteFilePattern);
    FStreamSettingsObsolete.IncompleteFilePattern := TmpStr;
    FStorage.Read('StreamFilePattern', TmpStr, FData.DefaultStreamSettings.StreamFilePattern);
    FStreamSettingsObsolete.StreamFilePattern := TmpStr;
    FStorage.Read('FilePatternDecimals', TmpCardinal, FData.DefaultStreamSettings.FilePatternDecimals);
    FStreamSettingsObsolete.FilePatternDecimals := TmpCardinal;
    FStorage.Read('RemoveChars', TmpStr, FData.DefaultStreamSettings.RemoveChars);
    FStreamSettingsObsolete.RemoveChars := TmpStr;
    FStorage.Read('NormalizeVariables', TmpBoolean, FData.DefaultStreamSettings.NormalizeVariables);
    FStreamSettingsObsolete.NormalizeVariables := TmpBoolean;

    FStorage.Read('DeleteStreams', TmpBoolean, FData.DefaultStreamSettings.DeleteStreams);
    FStreamSettingsObsolete.DeleteStreams := TmpBoolean;
    FStorage.Read('AddSavedToIgnore', TmpBoolean, FData.DefaultStreamSettings.AddSavedToIgnore);
    FStreamSettingsObsolete.AddSavedToIgnore := TmpBoolean;
    FStorage.Read('AddSavedToStreamIgnore', TmpBoolean, FData.DefaultStreamSettings.AddSavedToStreamIgnore);
    FStreamSettingsObsolete.AddSavedToStreamIgnore := TmpBoolean;
    FStorage.Read('RemoveSavedFromWishlist', TmpBoolean, FData.DefaultStreamSettings.RemoveSavedFromWishlist);
    FStreamSettingsObsolete.RemoveSavedFromWishlist := TmpBoolean;
    FStorage.Read('SkipShort', TmpBoolean, FData.DefaultStreamSettings.SkipShort);
    FStreamSettingsObsolete.SkipShort := TmpBoolean;
    FStorage.Read('SearchSilence', TmpBoolean, FData.DefaultStreamSettings.SearchSilence);
    FStreamSettingsObsolete.SearchSilence := TmpBoolean;
    FStorage.Read('AutoDetectSilenceLevel', TmpBoolean, FData.DefaultStreamSettings.AutoDetectSilenceLevel);
    FStreamSettingsObsolete.AutoDetectSilenceLevel := TmpBoolean;
    FStorage.Read('SilenceLevel', TmpCardinal, FData.DefaultStreamSettings.SilenceLevel);
    FStreamSettingsObsolete.SilenceLevel := TmpCardinal;
    FStorage.Read('SilenceLength', TmpCardinal, FData.DefaultStreamSettings.SilenceLength);
    FStreamSettingsObsolete.SilenceLength := TmpCardinal;

    FStorage.Read('SilenceBufferSeconds', TmpInteger, FData.DefaultStreamSettings.SilenceBufferSecondsStart);
    if TmpInteger <> FData.DefaultStreamSettings.SilenceBufferSecondsStart then
    begin
      FStreamSettingsObsolete.SilenceBufferSecondsStart := TmpInteger;
      FStreamSettingsObsolete.SilenceBufferSecondsEnd := TmpInteger;
    end else
    begin
      FStorage.Read('SilenceBufferSecondsStart', TmpInteger, FData.DefaultStreamSettings.SilenceBufferSecondsStart);
      FStreamSettingsObsolete.SilenceBufferSecondsStart := TmpInteger;
      FStorage.Read('SilenceBufferSecondsEnd', TmpInteger, FData.DefaultStreamSettings.SilenceBufferSecondsEnd);
      FStreamSettingsObsolete.SilenceBufferSecondsEnd := TmpInteger;
    end;

    FStreamSettingsObsolete.AdjustTrackOffset := False;
    FStreamSettingsObsolete.AdjustTrackOffsetMS := 0;
    FStreamSettingsObsolete.AdjustTrackOffsetDirection := toForward;

    FStorage.Read('SaveToMemory', TmpBoolean, FData.DefaultStreamSettings.SaveToMemory);
    FStreamSettingsObsolete.SaveToMemory := TmpBoolean;
    FStorage.Read('OnlySaveFull', TmpBoolean, FData.DefaultStreamSettings.OnlySaveFull);
    FStreamSettingsObsolete.OnlySaveFull := TmpBoolean;
    FStorage.Read('OverwriteSmaller', TmpBoolean, FData.DefaultStreamSettings.OverwriteSmaller);
    FStreamSettingsObsolete.OverwriteSmaller := TmpBoolean;
    FStorage.Read('DiscardSmaller', TmpBoolean, FData.DefaultStreamSettings.DiscardSmaller);
    FStreamSettingsObsolete.DiscardSmaller := TmpBoolean;
    FStorage.Read('DiscardAlways', TmpBoolean, FData.DefaultStreamSettings.DiscardAlways);
    FStreamSettingsObsolete.DiscardAlways := TmpBoolean;

    if (FStreamSettingsObsolete.SilenceLevel < 1) or (StreamSettingsObsolete.SilenceLevel > 100) then
      FStreamSettingsObsolete.SilenceLevel := FData.DefaultStreamSettings.SilenceLevel;
    if FStreamSettingsObsolete.SilenceLength < 20 then
      FStreamSettingsObsolete.SilenceLength := FData.DefaultStreamSettings.SilenceLength;

    FStorage.Read('ShortLengthSeconds', TmpInteger, FData.DefaultStreamSettings.ShortLengthSeconds);
    FStreamSettingsObsolete.ShortLengthSeconds := TmpInteger;

    FStorage.Read('SongBuffer', TmpInteger, FData.DefaultStreamSettings.SongBuffer);
    FStreamSettingsObsolete.SongBuffer := TmpInteger;

    FStorage.Read('MaxRetries', TmpInteger, FData.DefaultStreamSettings.MaxRetries);
    FStreamSettingsObsolete.MaxRetries := TmpInteger;
    FStorage.Read('RetryDelay', TmpCardinal, FData.DefaultStreamSettings.RetryDelay);
    FStreamSettingsObsolete.RetryDelay := TmpCardinal;

    FStorage.Read('SeparateTracks', TmpBoolean, FData.DefaultStreamSettings.SeparateTracks);
    FStreamSettingsObsolete.SeparateTracks := TmpBoolean;

    FStorage.Read('OutputFormat', TmpInteger, Integer(FData.DefaultStreamSettings.OutputFormat));
    if (TmpInteger > Ord(High(TAudioTypes))) or
       (TmpInteger < Ord(Low(TAudioTypes))) then
      FStreamSettingsObsolete.OutputFormat := atNone
    else
      FStreamSettingsObsolete.OutputFormat := TAudioTypes(TmpInteger);

    FStorage.Read('DefaultFilter', TmpInteger, Integer(ufNone));
    if IsVersionNewer(LastUsedVersion, AppVersion) and (IsVersionNewer(LastUsedVersion, ParseVersion('2.1.0.9'))) then
    begin
      if TmpInteger = 0 then
        FStreamSettingsObsolete.Filter := ufNone
      else if TmpInteger = 1 then
        FStreamSettingsObsolete.Filter := ufWish
      else if TmpInteger = 2 then
        FStreamSettingsObsolete.Filter := ufIgnoreBoth
      else if TmpInteger = 3 then
        FStreamSettingsObsolete.Filter := ufBoth
      else
        FStreamSettingsObsolete.Filter := ufNone;
    end else if (TmpInteger > Ord(High(TUseFilters))) or
                (TmpInteger < Ord(Low(TUseFilters))) then
      FStreamSettingsObsolete.Filter := ufNone
    else
      FStreamSettingsObsolete.Filter := TUseFilters(TmpInteger);

    if FStreamSettingsObsolete.SaveToMemory then
    begin
      FStreamSettingsObsolete.SeparateTracks := True;
      FStreamSettingsObsolete.DeleteStreams := False;
    end;

    if not FStreamSettingsObsolete.SeparateTracks then
      FStreamSettingsObsolete.DeleteStreams := False;

    if FStreamSettingsObsolete.RetryDelay > 999 then
      FStreamSettingsObsolete.RetryDelay := 999;
  end;
end;

procedure TAppData.DoSave;
var
  i: Integer;
begin
  inherited;

  FStorage.Write('ID', FID);
  //FStorage.Write('EasyMode', FEasyMode);

  FStorage.Write('LastUsedDataVersion', FLastUsedDataVersion);

  // Alte Einstellungen löschen...
  FStorage.Delete('FilePattern');
  FStorage.Delete('IncompleteFilePattern');
  FStorage.Delete('StreamFilePattern');
  FStorage.Delete('FilePatternDecimals');
  FStorage.Delete('RemoveChars');
  FStorage.Delete('NormalizeVariables');
  FStorage.Delete('DeleteStreams');
  FStorage.Delete('AddSavedToIgnore');
  FStorage.Delete('AddSavedToStreamIgnore');
  FStorage.Delete('RemoveSavedFromWishlist');
  FStorage.Delete('SkipShort');
  FStorage.Delete('SearchSilence');
  FStorage.Delete('AutoDetectSilenceLevel');
  FStorage.Delete('SilenceLevel');
  FStorage.Delete('SilenceLength');
  FStorage.Delete('SilenceBufferSecondsStart');
  FStorage.Delete('SilenceBufferSecondsEnd');
  FStorage.Delete('SaveToMemory');
  FStorage.Delete('OnlySaveFull');
  FStorage.Delete('ShortLengthSeconds');
  FStorage.Delete('SongBuffer');
  FStorage.Delete('MaxRetries');
  FStorage.Delete('RetryDelay');
  FStorage.Delete('DefaultFilter');
  FStorage.Delete('SeparateTracks');
  FStorage.Delete('OverwriteSmaller');
  FStorage.Delete('DiscardSmaller');
  FStorage.Delete('DiscardAlways');
  FStorage.Delete('OutputFormat');

  FStorage.Delete('AutomaticFilePattern');
  FStorage.Delete('AutoTuneInAddToIgnore');
  FStorage.Delete('AutoRemoveSavedFromWishlist');

  FStorage.Delete('AutoScrollLog');


  FStorage.Write('Dir', TryRelativePath(FDir, False, True));
  FStorage.Write('DirAuto', TryRelativePath(FDirAuto, False, True));
  FStorage.Write('TrayClose', FTray);
  FStorage.Write('TrayOnMinimize', FTrayOnMinimize);
  FStorage.Write('SnapMain', FSnapMain);
  FStorage.Write('RememberRecordings', FRememberRecordings);
  FStorage.Write('RememberPlaying', FRememberPlaying);
  FStorage.Write('DisplayPlayedSong', FDisplayPlayedSong);
  FStorage.Write('DisplayPlayNotifications', FDisplayPlayNotifications);
  FStorage.Write('ShowSplashScreen', FShowSplashScreen);
  FStorage.Write('CoverPanelAlwaysVisible', FCoverPanelAlwaysVisible);
  FStorage.Write('SidebarWidth', FSidebarWidth);
  FStorage.Write('AutoTuneIn', FAutoTuneIn);
  FStorage.Write('AutoTuneInConsiderIgnore', FAutoTuneInConsiderIgnore);
  FStorage.Write('SubmitStats', FSubmitStats);
  FStorage.Write('MonitorMode', FMonitorMode);
  FStorage.Write('MonitorCount', FMonitorCount);
  FStorage.Write('SubmitStreamInfo', FSubmitStreamInfo);
  FStorage.Write('AutoTuneInMinQuality', FAutoTuneInMinQuality);
  FStorage.Write('AutoTuneInFormat', FAutoTuneInFormat);
  FStorage.Write('LimitSpeed', FLimitSpeed);
  FStorage.Write('MaxSpeed', FMaxSpeed);
  FStorage.Write('LastBrowserUpdate', FLastBrowserUpdate);

  FStorage.Write('MinDiskSpace', FMinDiskSpace);
  FStorage.Write('LogFile', FLogFile);
  FStorage.Write('DefaultAction', Integer(FDefaultAction));
  FStorage.Write('DefaultActionBrowser', Integer(FDefaultActionBrowser));
  FStorage.Write('PlayerVolume', FPlayerVolume);
  FStorage.Write('PlayerVolumeBeforeMute', FPlayerVolumeBeforeMute);
  FStorage.Write('PlayerShuffle', FPlayerShuffle);
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
    FStorage.Write('ClientHeaderWidth' + IntToStr(i), FClientHeaderWidth[i], 'Cols');
  for i := 0 to High(FClientHeaderPosition) do
    FStorage.Write('ClientHeaderPosition' + IntToStr(i), FClientHeaderPosition[i], 'Cols');
  FStorage.Write('ClientCols', FClientCols, 'Cols');

  for i := 0 to High(FChartHeaderWidth) do
    FStorage.Write('ChartHeaderWidth' + IntToStr(i), FChartHeaderWidth[i], 'Cols');
  for i := 0 to High(FChartHeaderPosition) do
    FStorage.Write('ChartHeaderPosition' + IntToStr(i), FChartHeaderPosition[i], 'Cols');
  FStorage.Write('ChartCols', FChartCols, 'Cols');

  for i := 0 to High(FListHeaderWidth) do
    FStorage.Write('ListHeaderWidth' + IntToStr(i), FListHeaderWidth[i], 'Cols');
  for i := 0 to High(FListHeaderPosition) do
    FStorage.Write('ListHeaderPosition' + IntToStr(i), FListHeaderPosition[i], 'Cols');
  FStorage.Write('ListCols', FListCols, 'Cols');

  for i := 0 to High(FSavedHeaderWidth) do
    FStorage.Write('SavedHeaderWidth' + IntToStr(i), FSavedHeaderWidth[i], 'Cols');
  for i := 0 to High(FSavedHeaderPosition) do
    FStorage.Write('SavedHeaderPosition' + IntToStr(i), FSavedHeaderPosition[i], 'Cols');
  FStorage.Write('SavedCols', FSavedCols, 'Cols');

  for i := 0 to High(FLogHeaderWidth) do
    FStorage.Write('LogHeaderWidth' + IntToStr(i), FLogHeaderWidth[i], 'Cols');
  for i := 0 to High(FLogHeaderPosition) do
    FStorage.Write('LogHeaderPosition' + IntToStr(i), FLogHeaderPosition[i], 'Cols');
  FStorage.Write('LogCols', FLogCols, 'Cols');

  FStorage.Write('BrowserSortType', FBrowserSortType);

  FStorage.DeleteKey('Plugins');
  FStorage.DeleteKey('Encoders');

  FStorage.Write('EQEnabled', FEQEnabled, 'Equalizer');
  for i := 0 to High(FEQGain) do
    FStorage.Write('EQBand' + IntToStr(i), FEQGain[i] + 15, 'Equalizer');

  FStorage.Write('ApplyEffectFadeoutStart', FApplyEffectFadeoutStart);
  FStorage.Write('ApplyEffectFadeoutEnd', FApplyEffectFadeoutEnd);
  FStorage.Write('ApplyEffectSilenceStart', FApplyEffectSilenceStart);
  FStorage.Write('ApplyEffectSilenceEnd', FApplyEffectSilenceEnd);
  FStorage.Write('ApplyEffectNormalize', FApplyEffectNormalize);

  FStorage.Write('LogFilterTypes', FLogFilterTypes);

  FStorage.Write('IntroShown', FIntroShown);
end;

procedure CreateAppData;
begin
  FreeAndNil(AppGlobals);
  AppGlobals := TAppData.Create('streamWriter');
end;

function InitAppDataStageOne: Boolean;
begin
  Result := True;
  try
    if Language = nil then
      raise Exception.Create('Language is not initialized');

    // PostProcessors etc. laden - das kann erst hier passieren, weil AppGlobals zugewiesen sein muss.
    // AppGlobals müssen übrigens nur zugewiesen sein, weil ich alte PostProcess-Einstellungen
    // aus der Registry/Ini laden muss. Wenn ich das nicht mehr machen muss, kann InitPostProcessors()
    // auch direkt im Konstruktor aufgerufen werden bzw. jedes StreamSettings kann mit True im Konstruktor
    // erzeugt werden, damit es sich die PostProcessors selber klarmacht.
    AppGlobals.InitPostProcessors;

    TLogger.SetFilename(AppGlobals.LogFile);

    // Globale Stream-Einstellungen von Vorgängerversion laden
    AppGlobals.LoadOldStreamSettings;
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), PChar(_('Error')), MB_ICONERROR);
      Result := False;
    end;
  end;
end;

function InitAppDataStageTwo: Boolean;
begin
  Result := True;
  try
    // Load settings from datafile
    AppGlobals.LoadData;

    AppGlobals.FAddonManager := TAddonManager.Create;
    AppGlobals.FPostProcessManager := TPostProcessManager.Create;

    AppGlobals.Data.CheckEncodersAndPostProcessors;
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), PChar(_('Error')), MB_ICONERROR);
      Result := False;
    end;
  end;
end;

initialization

finalization
  FreeAndNil(AppGlobals);

end.



