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
unit AppData;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, Registry, SyncObjs, AppDataBase,
  LanguageObjects, LanguageIcons, ExtendedStream, Forms, Functions, Plugins,
  PluginManager, Logging, Base64;

type
  TClientActions = (caStartStop, caStreamIntegrated, caStream, caFile);
  TBrowserActions = (baStart, baListen, baListenExternal);
  TUseFilters = (ufNone, ufWish, ufIgnoreGlobal, ufIgnoreLocal, ufIgnoreBoth, ufBoth);
  TTrackOffsetDirection = (toForward, toBackward);

  // Die ..None-Dinger müssen am Ende stehen!
  TScheduleInterval = (siDaily, siWeekly, siNone);
  TScheduleDay = (sdMonday, sdTuesday, sdWednesday, sdThursday, sdFriday, sdSaturday, sdSunday, sdNone);

  TIntArray = array of Integer;

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
    FSilenceLevel: Cardinal;
    FSilenceLength: Cardinal;
    FSilenceBufferSecondsStart: Integer;
    FSilenceBufferSecondsEnd: Integer;
    FShortLengthSeconds: Integer;
    FSongBufferSeconds: Integer;
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
    FIgnoreTrackChangePattern: TStringList;

    procedure FSetSaveToMemory(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    class function Load(Stream: TExtendedStream; Version: Integer): TStreamSettings;
    procedure Save(Stream: TExtendedStream);
    procedure Assign(From: TStreamSettings);
    function Copy: TStreamSettings;

    property TitlePattern: string read FTitlePattern write FTitlePattern;
    property FilePattern: string read FFilePattern write FFilePattern;
    property IncompleteFilePattern: string read FIncompleteFilePattern write FIncompleteFilePattern;
    property StreamFilePattern: string read FStreamFilePattern write FStreamFilePattern;
    property FilePatternDecimals: Cardinal read FFilePatternDecimals write FFilePatternDecimals;
    property RemoveChars: string read FRemoveChars write FRemoveChars;
    property NormalizeVariables: Boolean read FNormalizeVariables write FNormalizeVariables;
    property DeleteStreams: Boolean read FDeleteStreams write FDeleteStreams;
    property AddSavedToIgnore: Boolean read FAddSavedToIgnore write FAddSavedToIgnore;
    property AddSavedToStreamIgnore: Boolean read FAddSavedToStreamIgnore write FAddSavedToStreamIgnore;
    property RemoveSavedFromWishlist: Boolean read FRemoveSavedFromWishlist write FRemoveSavedFromWishlist;
    property SkipShort: Boolean read FSkipShort write FSkipShort;
    property SearchSilence: Boolean read FSearchSilence write FSearchSilence;
    property SilenceLevel: Cardinal read FSilenceLevel write FSilenceLevel;
    property SilenceLength: Cardinal read FSilenceLength write FSilenceLength;
    property SilenceBufferSecondsStart: Integer read FSilenceBufferSecondsStart write FSilenceBufferSecondsStart;
    property SilenceBufferSecondsEnd: Integer read FSilenceBufferSecondsEnd write FSilenceBufferSecondsEnd;
    property ShortLengthSeconds: Integer read FShortLengthSeconds write FShortLengthSeconds;
    property SongBufferSeconds: Integer read FSongBufferSeconds write FSongBufferSeconds;
    property AdjustTrackOffset: Boolean read FAdjustTrackOffset write FAdjustTrackOffset;
    property AdjustTrackOffsetMS: Cardinal read FAdjustTrackOffsetMS write FAdjustTrackOffsetMS;
    property AdjustTrackOffsetDirection: TTrackOffsetDirection read FAdjustTrackOffsetDirection write FAdjustTrackOffsetDirection;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property RetryDelay: Cardinal read FRetryDelay write FRetryDelay;
    property Filter: TUseFilters read FFilter write FFilter;
    property SeparateTracks: Boolean read FSeparateTracks write FSeparateTracks;
    property SaveToMemory: Boolean read FSaveToMemory write FSetSaveToMemory;
    property OnlySaveFull: Boolean read FOnlySaveFull write FOnlySaveFull;
    property OverwriteSmaller: Boolean read FOverwriteSmaller write FOverwriteSmaller;
    property DiscardSmaller: Boolean read FDiscardSmaller write FDiscardSmaller;
    property IgnoreTrackChangePattern: TStringList read FIgnoreTrackChangePattern write FIgnoreTrackChangePattern;
  end;

  TStreamSettingsArray = array of TStreamSettings;

  TAppData = class(TAppDataBase)
  private
    FStreamSettings: TStreamSettings;
    FUserLoggedIn: Boolean;

    FDir: string;
    FDirAuto: string;
    FTray: Boolean;
    FTrayOnMinimize: Boolean;
    FSnapMain: Boolean;
    FRememberRecordings: Boolean;
    FDisplayPlayNotifications: Boolean;
    FShowSidebar: Boolean;
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

    FShortcutPlay: Cardinal;
    FShortcutPause: Cardinal;
    FShortcutStop: Cardinal;
    FShortcutNext: Cardinal;
    FShortcutPrev: Cardinal;
    FShortcutVolDown: Cardinal;
    FShortcutVolUp: Cardinal;

    FHeaderWidth: TIntArray;
    FClientCols: Integer;
    FLastUsedDataVersion: Integer;
    FRecoveryFile: string;

    FPluginManager: TPluginManager;
    FLanguageIcons: TLanguageIcons;

    function FGetDataFile: string;
  protected
    procedure DoSave; override;
  public
    constructor Create(AppName: String);
    destructor Destroy; override;

    procedure Load; override;
    procedure BuildThanksText; override;

    property StreamSettings: TStreamSettings read FStreamSettings;
    property UserLoggedIn: Boolean read FUserLoggedIn write FUserLoggedIn;

    property Dir: string read FDir write FDir;
    property DirAuto: string read FDirAuto write FDirAuto;
    property Tray: Boolean read FTray write FTray;
    property TrayOnMinimize: Boolean read FTrayOnMinimize write FTrayOnMinimize;
    property SnapMain: Boolean read FSnapMain write FSnapMain;
    property RememberRecordings: Boolean read FRememberRecordings write FRememberRecordings;
    property DisplayPlayNotifications: Boolean read FDisplayPlayNotifications write FDisplayPlayNotifications;
    property ShowSidebar: Boolean read FShowSidebar write FShowSidebar;
    property SidebarWidth: Integer read FSidebarWidth write FSidebarWidth;
    property AutoTuneIn: Boolean read FAutoTuneIn write FAutoTuneIn;
    property AutoTuneInConsiderIgnore: Boolean read FAutoTuneInConsiderIgnore write FAutoTuneInConsiderIgnore;
    property AutoTuneInAddToIgnore: Boolean read FAutoTuneInAddToIgnore write FAutoTuneInAddToIgnore;
    property AutoRemoveSavedFromWishlist: Boolean read FAutoRemoveSavedFromWishlist write FAutoRemoveSavedFromWishlist;
    property SubmitStreamInfo: Boolean read FSubmitStreamInfo write FSubmitStreamInfo;
    property SubmitStats: Boolean read FSubmitStats write FSubmitStats;
    property MinDiskSpace: Integer read FMinDiskSpace write FMinDiskSpace;
    property DefaultAction: TClientActions read FDefaultAction write FDefaultAction;
    property DefaultActionBrowser: TBrowserActions read FDefaultActionBrowser write FDefaultActionBrowser;
    property PlayerVolume: Integer read FPlayerVolume write FPlayerVolume;
    property PlayerVolumeBeforeMute: Integer read FPlayerVolumeBeforeMute write FPlayerVolumeBeforeMute;
    property AutoScrollLog: Boolean read FAutoScrollLog write FAutoScrollLog;
    property UserWasSetup: Boolean read FUserWasSetup write FUserWasSetup;
    property User: string read FUser write FUser;
    property Pass: string read FPass write FPass;
    property SoundDevice: Cardinal read FSoundDevice write FSoundDevice;
    property ShortcutPlay: Cardinal read FShortcutPlay write FShortcutPlay;
    property ShortcutPause: Cardinal read FShortcutPause write FShortcutPause;
    property ShortcutStop: Cardinal read FShortcutStop write FShortcutStop;
    property ShortcutNext: Cardinal read FShortcutNext write FShortcutNext;
    property ShortcutPrev: Cardinal read FShortcutPrev write FShortcutPrev;
    property ShortcutVolDown: Cardinal read FShortcutVolDown write FShortcutVolDown;
    property ShortcutVolUp: Cardinal read FShortcutVolUp write FShortcutVolUp;
    property AutoTuneInMinKbps: Cardinal read FAutoTuneInMinKbps write FAutoTuneInMinKbps;
    property AutoTuneInFormat: Cardinal read FAutoTuneInFormat write FAutoTuneInFormat;
    property LimitSpeed: Boolean read FLimitSpeed write FLimitSpeed;
    property MaxSpeed: Cardinal read FMaxSpeed write FMaxSpeed;
    property LastBrowserUpdate: Cardinal read FLastBrowserUpdate write FLastBrowserUpdate;
    property AutomaticFilePattern: string read FAutomaticFilePattern write FAutomaticFilePattern;

    property HeaderWidth: TIntArray read FHeaderWidth write FHeaderWidth;
    property ClientCols: Integer read FClientCols write FClientCols;
    property LastUsedDataVersion: Integer read FLastUsedDataVersion write FLastUsedDataVersion;
    property RecoveryFile: string read FRecoveryFile;

    property DataFile: string read FGetDataFile;

    property PluginManager: TPluginManager read FPluginManager;
    property LanguageIcons: TLanguageIcons read FLanguageIcons;
  end;

var
  AppGlobals: TAppData;

implementation

{ TAppData }

constructor TAppData.Create(AppName: string);
var
  W, H: Integer;
  SR: TSearchRec;
begin
  FStreamSettings := TStreamSettings.Create;

  W := 900;
  H := 630;
  if Screen.WorkAreaWidth < W then
    W := Screen.WorkAreaWidth - 20;
  if Screen.WorkAreaHeight < H then
    H := Screen.WorkAreaHeight - 20;

  SetLength(FHeaderWidth, 6);

  {$IFDEF DEBUG}
  FProjectUpdateLink := 'http://streamwriter.gaia/';
  {$ELSE}
  FProjectUpdateLink := 'http://streamwriter.org/';
  {$ENDIF}
  FProjectHomepageLink := 'http://streamwriter.org/';
  FProjectLink := 'http://streamwriter.org/';
  FProjectHelpLink := 'http://streamwriter.org/wiki/artikel/help/';
  FProjectForumLink := 'http://streamwriter.org/forum/';
  FProjectDonateLink := 'http://streamwriter.org/inhalt/donate/';

  inherited Create(AppName, True, W, H);

  FRecoveryFile := FStorage.DataDir + 'streamwriter_data_recovery.dat';

  FBuildNumber := 279;

  BuildThanksText;

  FLanguageIcons := TLanguageIcons.Create;

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
  FPluginManager.Free;
  FStreamSettings.Free;

  DeleteFile(TempDir + 'playlist.m3u');

  inherited;
end;

function TAppData.FGetDataFile;
begin
  Result := FStorage.GetFilePath('data.dat');
end;

procedure TAppData.BuildThanksText;
  procedure ShuffleFisherYates(var A: TArray);
  var
    i, j: Integer;
    Tmp: TArrayElement;
  begin
    for i := Low(A) to High(A) do
    begin
      j := i + Random(Length(A) - i + Low(A));
      Tmp := A[j];
      A[j] := A[i];
      A[i] := Tmp;
    end;
  end;
var
  i: Integer;
  FHelpers: TArray;
  Text: TStringList;
begin
  inherited;

  Text := TStringList.Create;
  try
    Text.Add(_('&U&12Thanks go out to...'));

    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10...everybody who donated something'));
    Text.Add('');
    {SetLength(FDonors, 45);
    FDonors[0] := 'Thomas Franke';
    FDonors[1] := '''bastik''';
    FDonors[2] := 'Reto Pitsch';
    FDonors[3] := '''RogerPP''';
    FDonors[4] := 'Gabor Kubik';
    FDonors[5] := '''Peter Parker''';
    FDonors[6] := 'Anita Wimmer';
    FDonors[7] := 'Valentin M.';
    FDonors[8] := '''Rüdi''';
    FDonors[9] := '''Hummer''';
    FDonors[10] := 'Hans Heintz';
    FDonors[11] := 'Thomas Hecker';
    FDonors[12] := 'Michael Schwarz';
    FDonors[13] := 'Markus Bauer';
    FDonors[14] := '''Palm''';
    FDonors[15] := 'Peter Hörth';
    FDonors[16] := 'Roman Roscher';
    FDonors[17] := 'Paul Küpper';
    FDonors[18] := 'Thomas Krössin';
    FDonors[19] := 'Roland Zettier';
    FDonors[20] := '''Roman Regenpfeifer''';
    FDonors[21] := '''brumex''';
    FDonors[22] := '''Taube''';
    FDonors[23] := '''GoFB''';
    FDonors[24] := '''Radiohoerer''';
    FDonors[25] := 'NJOY Radio (Austria)';
    FDonors[26] := 'Yo24hua';
    FDonors[27] := 'Mirko Schal';
    FDonors[28] := 'Alexander Koch';
    FDonors[29] := 'Claus Wien';
    FDonors[30] := 'Hartmut Schiller';
    FDonors[31] := 'Michael Dreßler';
    FDonors[32] := 'Jörn Räuber';
    FDonors[33] := '''Klaus''';
    FDonors[34] := '''bwso''';
    FDonors[35] := 'Jörn Räuber';
    FDonors[36] := 'Sebastian Hein';
    FDonors[37] := 'Johannes Schneider';
    FDonors[38] := 'Edwin de Boer';
    FDonors[39] := 'Alexander Maier (www.kommtel.info)';
    FDonors[40] := 'Bernhard Langheim';
    FDonors[41] := 'Eike R.';
    FDonors[42] := '''Nobbe''';
    FDonors[43] := 'Kay Dubberke';
    FDonors[44] := '''chessbase''';

    ShuffleFisherYates(FDonors);
    for i := 0 to Length(FDonors) - 1 do
      Text.Add(FDonors[i]);

    Text.Add(_('and everyone who does not want to be mentioned'));

    Text.Add('');
    Text.Add('');
    }

    Text.Add(_('&U&10...people who contributed code, documentation,'));
    Text.Add(_('&U&10images or translations'));
    Text.Add('');
    SetLength(FHelpers, 3);
    FHelpers[0] := '''HostedDinner''';
    FHelpers[1] := '''bastik''';
    FHelpers[2] := 'Ralf';
    ShuffleFisherYates(FHelpers);
    for i := 0 to Length(FHelpers) - 1 do
      Text.Add(FHelpers[i]);

    Text.Add('');

    Text.Add(_('&U&10...everyone supporting streamWriter at the board'));
    Text.Add('');

    Text.Add(_('&U&10...and all other nice people I know!'));

    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10Also thanks to some other projects I make use of'#13#10'&U&10to develop streamWriter and it''s website'));
    Text.Add('');
    Text.Add('Apache HTTP Server');
    Text.Add('Bass');
    Text.Add('Delphi-Praxis');
    Text.Add('Django');
    Text.Add('Drag and Drop Component Suite');
    Text.Add('Embarcadero');
    Text.Add('famfamfam');
    Text.Add('FastMM');
    Text.Add('freecsstemplates.org');
    Text.Add('Fugue Icons');
    Text.Add('Gimp');
    Text.Add('Inno Setup');
    Text.Add('jQuery');
    Text.Add('LED icons');
    Text.Add('MySQL');
    Text.Add('Notepad++');
    Text.Add('Mp3FileUtils');
    Text.Add('Python');
    Text.Add('Tango Desktop Project');
    Text.Add('Virtual Treeview');
    Text.Add('XMLLib');

    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');

    Text.Add('&IMG0');
    Text.Add('&IMG1');
    Text.Add('&IMG2');

    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');

    Text.Add('D1734FA178BF7D5AE50CB1AD54442494');

    Text.Add('');
    Text.Add('');
    Text.Add('Korrekt Banze!');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');

    Text.Add(_('Thanks for reading everything...'));

    FProjectThanksText := Text.Text;
  finally
    Text.Free;
  end;
end;

procedure TAppData.Load;
var
  i, DefaultActionTmp, DefaultActionBrowser, DefaultFilterTmp, SilenceBuffer: Integer;
begin
  inherited;

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
  FDir := TryUnRelativePath(FDir, False);

  FStorage.Read('DirAuto', FDirAuto, '');
  if FDirAuto = '' then
    FDirAuto := FDir;
  if FDirAuto <> '' then
    FDirAuto := IncludeTrailingBackslash(FDirAuto);
  FDirAuto := TryUnRelativePath(FDirAuto, False);

  FStorage.Read('DeleteStreams', FStreamSettings.FDeleteStreams, False);
  FStorage.Read('AddSavedToIgnore', FStreamSettings.FAddSavedToIgnore, True);
  FStorage.Read('AddSavedToStreamIgnore', FStreamSettings.FAddSavedToStreamIgnore, False);
  FStorage.Read('RemoveSavedFromWishlist', FStreamSettings.FRemoveSavedFromWishlist, False);
  FStorage.Read('SkipShort', FStreamSettings.FSkipShort, True);
  FStorage.Read('SearchSilence', FStreamSettings.FSearchSilence, True);
  FStorage.Read('SilenceLevel', FStreamSettings.FSilenceLevel, 5);
  FStorage.Read('SilenceLength', FStreamSettings.FSilenceLength, 150);

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
  FStorage.Read('SongBufferSeconds', FStreamSettings.FSongBufferSeconds, 0);
  FStorage.Read('MaxRetries', FStreamSettings.FMaxRetries, 100);
  FStorage.Read('RetryDelay', FStreamSettings.FRetryDelay, 5);

  FShowSidebar := True;
  FStorage.Read('TrayClose', FTray, False);
  FStorage.Read('TrayOnMinimize', FTrayOnMinimize, False);
  FStorage.Read('SnapMain', FSnapMain, False);
  FStorage.Read('RememberRecordings', FRememberRecordings, False);
  FStorage.Read('DisplayPlayNotifications', FDisplayPlayNotifications, True);
  FStorage.Read('SidebarWidth', FSidebarWidth, 250);
  FStorage.Read('AutoTuneIn', FAutoTuneIn, True);
  FStorage.Read('AutoTuneInConsiderIgnore', FAutoTuneInConsiderIgnore, False);
  FStorage.Read('AutoTuneInAddToIgnore', FAutoTuneInAddToIgnore, True);
  FStorage.Read('AutoRemoveSavedFromWishlist', FAutoRemoveSavedFromWishlist, False);
  FStorage.Read('SubmitStreamInfo', FSubmitStreamInfo, True);
  FStorage.Read('SubmitStats', FSubmitStats, True);
  FStorage.Read('LimitSpeed', FLimitSpeed, False);
  FStorage.Read('MaxSpeed', FMaxSpeed, 0);
  if FMaxSpeed <= 0 then
    FLimitSpeed := False;
  FStorage.Read('LastBrowserUpdate', FLastBrowserUpdate, Trunc(Now));
  FStorage.Read('AutomaticFilePattern', FAutomaticFilePattern, '%s\%a - %t');

  FStorage.Read('AutoTuneInMinKbps', FAutoTuneInMinKbps, 3);
  FStorage.Read('AutoTuneInFormat', FAutoTuneInFormat, 0);
  if (FAutoTuneInMinKbps > 9) then
    FAutoTuneInMinKbps := 3;
  if FAutoTuneInFormat > 2 then
    FAutoTuneInFormat := 0;

  // Wenn das zu viel wird, blockiert der Thread zu lange. Und dann kann man
  // Clients nicht mehr so schnell aus der Liste entfernen...
  if FStreamSettings.FRetryDelay > 10 then
    FStreamSettings.RetryDelay := 10;

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
    // Wenn FPass beim Einlesen leer war, gibt das eine Exception!
    // Die MÜSSEN wir hier fangen, sonst geht der Ladevorgang nicht weiter!
    try
      FPass := CryptStr(Decode(RawByteString(FPass)));
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

  FStorage.Read('HeaderWidth0', i, -1, 'Cols');
  if i = -1 then
  begin
    for i := 0 to High(FHeaderWidth) do
      FHeaderWidth[i] := 100;
    FStorage.Read('HeaderWidth0', FHeaderWidth[0], 150, 'Cols');
    FStorage.Read('HeaderWidth2', FHeaderWidth[2], 70, 'Cols');
    FStorage.Read('HeaderWidth3', FHeaderWidth[3], 60, 'Cols');
    FStorage.Read('HeaderWidth4', FHeaderWidth[4], 90, 'Cols');
    FStorage.Read('HeaderWidth5', FHeaderWidth[5], 85, 'Cols');
  end else
  begin
    for i := 0 to High(FHeaderWidth) do
      if i <> 1 then
        FStorage.Read('HeaderWidth' + IntToStr(i), FHeaderWidth[i], 130, 'Cols');
  end;
  FStorage.Read('ClientCols', FClientCols, 255, 'Cols');
  FClientCols := FClientCols or (1 shl 0);

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
end;

procedure TAppData.DoSave;
var
  i, n: Integer;
begin
  inherited;

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
  FStorage.Write('SilenceLevel', FStreamSettings.FSilenceLevel);
  FStorage.Write('SilenceLength', FStreamSettings.FSilenceLength);
  FStorage.Write('SilenceBufferSecondsStart', FStreamSettings.FSilenceBufferSecondsStart);
  FStorage.Write('SilenceBufferSecondsEnd', FStreamSettings.FSilenceBufferSecondsEnd);

  FStorage.Write('SaveToMemory', FStreamSettings.FSaveToMemory);
  FStorage.Write('OnlySaveFull', FStreamSettings.FOnlySaveFull);
  FStorage.Write('ShortLengthSeconds', FStreamSettings.FShortLengthSeconds);
  FStorage.Write('SongBufferSeconds', FStreamSettings.FSongBufferSeconds);
  FStorage.Write('MaxRetries', FStreamSettings.FMaxRetries);
  FStorage.Write('RetryDelay', FStreamSettings.FRetryDelay);
  FStorage.Write('DefaultFilter', Integer(FStreamSettings.Filter));
  FStorage.Write('SeparateTracks', FStreamSettings.FSeparateTracks);
  FStorage.Write('OverwriteSmaller', FStreamSettings.FOverwriteSmaller);
  FStorage.Write('DiscardSmaller', FStreamSettings.FDiscardSmaller);

  FStorage.Write('TrayClose', FTray);
  FStorage.Write('TrayOnMinimize', FTrayOnMinimize);
  FStorage.Write('SnapMain', FSnapMain);
  FStorage.Write('RememberRecordings', FRememberRecordings);
  FStorage.Write('DisplayPlayNotifications', FDisplayPlayNotifications);
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

  for i := 0 to High(FHeaderWidth) do
    if i <> 1 then
      FStorage.Write('HeaderWidth' + IntToStr(i), HeaderWidth[i], 'Cols');
  FStorage.Write('ClientCols', FClientCols, 'Cols');

  FStorage.DeleteKey('Plugins');
  n := 0;
  for i := 0 to FPluginManager.Plugins.Count - 1 do
    if (FPluginManager.Plugins[i] is TExternalPlugin) then
    begin
      FStorage.Write('Active_' + IntToStr(n), TExternalPlugin(FPluginManager.Plugins[i]).Active, 'Plugins');
      FStorage.Write('Exe_' + IntToStr(n), TExternalPlugin(FPluginManager.Plugins[i]).Exe, 'Plugins');
      FStorage.Write('Params_' + IntToStr(n), TExternalPlugin(FPluginManager.Plugins[i]).Params, 'Plugins');
      FStorage.Write('OrderExe_' + IntToStr(n), FPluginManager.Plugins[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + IntToStr(n), FPluginManager.Plugins[i].OnlyIfCut, 'Plugins');
      Inc(n);
    end else if (FPluginManager.Plugins[i] is TDLLPlugin) then
    begin
      FStorage.Write('Active_' + ExtractFileName(TDLLPlugin(FPluginManager.Plugins[i]).Filename), FPluginManager.Plugins[i].Active, 'Plugins');
      FStorage.Write('Order_' + ExtractFileName(TDLLPlugin(FPluginManager.Plugins[i]).Filename), FPluginManager.Plugins[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + ExtractFileName(TDLLPlugin(FPluginManager.Plugins[i]).Filename), FPluginManager.Plugins[i].OnlyIfCut, 'Plugins');
    end else if (FPluginManager.Plugins[i] is TInternalPlugin) then
    begin
      FStorage.Write('Active_' + FPluginManager.Plugins[i].ClassName, FPluginManager.Plugins[i].Active, 'Plugins');
      FStorage.Write('Order_' + FPluginManager.Plugins[i].ClassName, FPluginManager.Plugins[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + FPluginManager.Plugins[i].ClassName, FPluginManager.Plugins[i].OnlyIfCut, 'Plugins');
      FPluginManager.Plugins[i].Save;
    end;
end;

{ TStreamSettings }

function TStreamSettings.Copy: TStreamSettings;
begin
  Result := TStreamSettings.Create;
  Result.Assign(Self);
end;

constructor TStreamSettings.Create;
begin
  inherited;

  FIgnoreTrackChangePattern := TStringList.Create;
end;

destructor TStreamSettings.Destroy;
var
  i: Integer;
begin
  FIgnoreTrackChangePattern.Free;

  inherited;
end;

procedure TStreamSettings.FSetSaveToMemory(Value: Boolean);
begin
  FSaveToMemory := Value;
  if Value then
    FSeparateTracks := True;
end;

class function TStreamSettings.Load(Stream: TExtendedStream;
  Version: Integer): TStreamSettings;
var
  B: Byte;
  i, Count, FilterTmp: Integer;
  IgnoreTmp: string;
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
    Stream.Read(Result.FSongBufferSeconds)
  else
  begin
    Stream.Read(FilterTmp);
    Result.FSongBufferSeconds := 0;
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

  {
  if Version = 27 then
  begin
    Stream.Read(Count);
    for i := 0 to Count - 1 do
    begin
      TitleInfo := TTitleInfo.Load(Stream, Version);
      if TitleInfo <> nil then
        Result.FMigrationIgnoreList.Add(TitleInfo);
    end;
  end;
  }
end;

procedure TStreamSettings.Save(Stream: TExtendedStream);
var
  i: Integer;
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
  Stream.Write(FSilenceLevel);
  Stream.Write(FSilenceLength);
  Stream.Write(FSilenceBufferSecondsStart);
  Stream.Write(FSilenceBufferSecondsEnd);
  Stream.Write(FShortLengthSeconds);
  Stream.Write(FSongBufferSeconds);
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
end;

procedure TStreamSettings.Assign(From: TStreamSettings);
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
  FSilenceLevel := From.FSilenceLevel;
  FSilenceLength := From.FSilenceLength;
  FSilenceBufferSecondsStart := From.FSilenceBufferSecondsStart;
  FSilenceBufferSecondsEnd := From.FSilenceBufferSecondsEnd;
  FShortLengthSeconds := From.FShortLengthSeconds;
  FSongBufferSeconds := From.FSongBufferSeconds;
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
end;

initialization
  try
    if Language = nil then
      raise Exception.Create('Language is not initialized');
    AppGlobals := TAppData.Create('streamWriter');

    // PluginManager wird hier erstellt, da erstellte Plugin-Items Zugriff
    // auf ein bereits zugewiesenes AppGlobals brauchen.
    AppGlobals.FPluginManager := TPluginManager.Create(AppGlobals.AppPath + 'plugins\');
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

