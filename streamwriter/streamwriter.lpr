{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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

program streamwriter;

uses
  MM in '..\..\common\MM.pas',
  Windows,
  Classes,
  Messages,
  SysUtils,
  Forms,
  Interfaces,
  ShlObj,
  Images,
  JwaWinNT,
  Main in 'Main.pas' {frmStreamWriterMain},
  ClientView in 'controls\ClientView.pas',
  DataManager in 'DataManager.pas',
  Settings in 'Settings.pas' {frmSettings},
  AppData in 'AppData.pas',
  Wizard in 'Wizard.pas' {frmWizard},
  ExtendedStream in '..\..\common\ExtendedStream.pas',
  Functions in '..\..\common\Functions.pas',
  AppDataBase in '..\..\common\AppDataBase.pas',
  UpdateClient in '..\..\common\UpdateClient.pas',
  LanguageObjects in '..\..\common\LanguageObjects.pas',
  GUIFunctions in '..\..\common\GUIFunctions.pas',
  Update in '..\..\common\forms\Update.pas' {frmUpdate},
  Changelog in '..\..\common\forms\Changelog.pas' {frmChangeLog},
  LanguageIcons in '..\..\common\LanguageIcons.pas',
  MsgDlg in '..\..\common\forms\MsgDlg.pas' {frmMsgDlg},
  SettingsStorage in '..\..\common\SettingsStorage.pas',
  WizardBase in '..\..\common\forms\WizardBase.pas' {frmSetupWizard},
  AppStartup in '..\..\common\AppStartup.pas',
  ProfileSettings in '..\..\common\forms\ProfileSettings.pas' {frmProfileSettings},
  SettingsBase in '..\..\common\forms\SettingsBase.pas' {frmSettingsBase},
  StreamBrowserView in 'controls\StreamBrowserView.pas',
  StationCombo in 'controls\StationCombo.pas',
  StreamInfoView in 'controls\StreamInfoView.pas',
  StreamDebugView in 'controls\StreamDebugView.pas',
  Base64 in 'Base64.pas',
  AudioStream in 'audio\AudioStream.pas',
  ClientTab in 'controls\ClientTab.pas',
  CutTab in 'controls\CutTab.pas',
  MControls in '..\..\common\MControls.pas',
  Tabs in 'controls\Tabs.pas',
  CutView in 'controls\CutView.pas',
  SavedTab in 'controls\SavedTab.pas',
  WaveData in 'audio\WaveData.pas',
  About in '..\..\common\forms\About.pas' {frmAbout},
  CheckFilesThread in 'CheckFilesThread.pas',
  ListsTab in 'controls\ListsTab.pas',
  UpdatedInfo in '..\..\common\forms\UpdatedInfo.pas' {frmUpdatedInfo},
  SharedControls in 'controls\SharedControls.pas',
  ClientManager in 'streaming\ClientManager.pas',
  ICEClient in 'streaming\ICEClient.pas',
  ICEPlayer in 'streaming\ICEPlayer.pas',
  ICEStream in 'streaming\ICEStream.pas',
  ICEThread in 'streaming\ICEThread.pas',
  MPEG in 'audio\MPEG.pas',
  CommunityLogin in 'CommunityLogin.pas' {frmCommunityLogin},
  Commands in '..\..\common\sockets\Commands.pas',
  Communication in '..\..\common\sockets\Communication.pas',
  Protocol in '..\..\common\sockets\Protocol.pas',
  HTTPStream in '..\..\common\sockets\HTTPStream.pas',
  HTTPThread in '..\..\common\sockets\HTTPThread.pas',
  Sockets in '..\..\common\sockets\Sockets.pas',
  HomeCommunication in 'HomeCommunication.pas',
  Player in 'audio\Player.pas',
  PlayerManager in 'audio\PlayerManager.pas',
  PostProcess in 'postprocess\PostProcess.pas',
  PostProcessSetTags in 'postprocess\PostProcessSetTags.pas',
  PostProcessManager in 'postprocess\PostProcessManager.pas',
  Logging in '..\..\common\Logging.pas',
  Timers in 'Timers.pas' {frmTimers},
  PostProcessSoX in 'postprocess\PostProcessSoX.pas',
  DownloadAddons in 'DownloadAddons.pas' {frmDownloadAddons},
  DownloadClient in '..\..\common\DownloadClient.pas',
  ConfigureSoX in 'postprocess\ConfigureSoX.pas' {frmConfigureSoX},
  Notifications in 'Notifications.pas' {frmNotification},
  TypeDefs in 'TypeDefs.pas',
  CutTabSearchSilence in 'controls\CutTabSearchSilence.pas' {frmCutTabSearchSilence},
  FileWatcher in '..\..\common\FileWatcher.pas',
  ChartsTab in 'controls\ChartsTab.pas',
  ChartsTabAdjustTitleName in 'controls\ChartsTabAdjustTitleName.pas' {frmChartsTabAdjustTitleName},
  ConfigureSetTags in 'postprocess\ConfigureSetTags.pas' {Form1},
  MessageBus in '..\..\common\MessageBus.pas',
  AppMessages in 'AppMessages.pas',
  StatusBar in 'controls\StatusBar.pas',
  PostProcessMP4Box in 'postprocess\PostProcessMP4Box.pas',
  PowerManagement in '..\..\common\PowerManagement.pas',
  FileConvertor in 'audio\FileConvertor.pas',
  Intro in 'Intro.pas' {frmIntro},
  PostProcessConvert in 'postprocess\PostProcessConvert.pas',
  FileTagger in 'audio\FileTagger.pas',
  AudioGenie in 'audio\AudioGenie.pas',
  SavedTabEditTags in 'controls\SavedTabEditTags.pas' {frmEditTags},
  SettingsAddPostProcessor in 'SettingsAddPostProcessor.pas' {frmSettingsAddPostProcessor},
  AddonAudioGenie in 'addon\AddonAudioGenie.pas',
  AddonBase in 'addon\AddonBase.pas',
  AddonFAAC in 'addon\AddonFAAC.pas',
  AddonLAME in 'addon\AddonLAME.pas',
  AddonManager in 'addon\AddonManager.pas',
  AddonMP4Box in 'addon\AddonMP4Box.pas',
  AddonOGGEnc in 'addon\AddonOGGEnc.pas',
  AddonSoX in 'addon\AddonSoX.pas',
  AudioFunctions in 'audio\AudioFunctions.pas',
  ConfigureEncoder in 'ConfigureEncoder.pas' {frmConfigureEncoder},
  PlaylistHandler in 'audio\PlaylistHandler.pas',
  Equalizer in 'Equalizer.pas' {frmEqualizer},
  SWFunctions in 'SWFunctions.pas',
  SplashThread in '..\..\common\SplashThread.pas',
  CommandLine in '..\..\common\CommandLine.pas',
  HomeTest in 'HomeTest.pas' {frmHomeTest},
  HomeCommands in 'HomeCommands.pas',
  SharedData in 'SharedData.pas' {modSharedData: TDataModule},
  MonitorAnalyzer in 'streaming\MonitorAnalyzer.pas',
  DynBASS in 'audio\DynBASS.pas',
  LogTab in 'controls\LogTab.pas',
  Scheduler in 'Scheduler.pas',
  DynOpenSSL in '..\..\common\openssl\DynOpenSSL.pas',
  IdSSLOpenSSLHeadersCustom in '..\..\common\openssl\IdSSLOpenSSLHeadersCustom.pas',
  WindowsFunctions in '..\..\common\WindowsFunctions.pas',
  SetStreamData in 'SetStreamData.pas' {frmSetStreamData},
  Constants in 'Constants.pas';

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$SetPEFlags IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP}
{$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_NX_COMPAT}
{$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE}

{$R *.res}
{$R res\images.rc}
{$R res\language.res}
{$R res\bass.rc}
{$R res\openssl.rc}
{$R res\about.rc}
{$R res\certificates.rc}
{$R ..\..\common\res\language_common.res}
{$R ..\..\common\res\lang_icons.rc}

var
  i: Integer;
  HideMain, Found: Boolean;
  frmStreamWriterMain: TfrmStreamWriterMain;
  frmHomeTest: TfrmHomeTest;

 begin
  IsMultiThread := True;

  Bass := nil;
  OpenSSL := nil;
  try
    {$IFDEF madExcept}
    MESettings.BugReportFile := AnsiString(IncludeTrailingBackslash(GUIFunctions.GetShellFolder(CSIDL_DESKTOP)) + 'streamwriter_bugreport.txt');
    {$ENDIF}

    if not InitWinsock then
      Exit;

    HideMain := False;
    for i := 0 to ParamCount do
    begin
      if ParamStr(i) = '-minimize' then
      begin
        HideMain := True;
        Break;
      end;
    end;

    // Initialize the AppGlobals object without loading any settings.
    // If we need to show the profile selection window, this would make no sense.
    try
      CreateAppData;
    except
      on EAlreadyRunning do
        Exit;
    end;
    MsgBus := TSWMessageBus.Create;

    Application.Title := AppGlobals.AppName;

    if not InitAppStageOne then
      Exit;

    // Basic initialization
    if not InitAppDataStageOne then
      Exit;

    InitPlayerManager;

    if (AppGlobals.ShowSplashScreen) and (AppGlobals.FirstStartShown) and (AppGlobals.WasSetup) and
       (not IsVersionNewer(AppGlobals.LastUsedVersion, AppGlobals.AppVersion)) and (not HideMain)
       and (not AppGlobals.InstallUpdateOnStart)
    then
      TSplashThread.Create('TfrmStreamWriterMain', 'SPLASH', AppGlobals.Codename, AppGlobals.AppVersion.AsString, AppGlobals.BuildNumber,
        AppGlobals.MainLeft, AppGlobals.MainTop, AppGlobals.MainWidth, AppGlobals.MainHeight);

    // Now load everything from datafiles
    if not InitAppDataStageTwo then
      Exit;

    // Initialize BASS, quit application on error
    Bass := TBassLoader.Create;
    if not Bass.InitializeBass(0, True, False, False, False) then
    begin
      MsgBox(0, _('The BASS library or it''s plugins could not be extracted/loaded. Without these libraries streamWriter cannot record/playback streams. Try to get help at streamWriter''s board.'), _('Error'), MB_ICONERROR);
      Exit;
    end;

    OpenSSL := TOpenSSLLoader.Create;
    if not OpenSSL.InitializeOpenSSL(AppGlobals.TempDir) then
    begin
      MsgBox(0, _('The OpenSSL libraries could not be extracted/loaded. Without these libraries streamWriter cannot be run. Try to get help at streamWriter''s board.'), _('Error'), MB_ICONERROR);
      Exit;
    end;

    Found := False;
    for i := 0 to Bass.Devices.Count - 1 do
      if Bass.Devices[i].ID = AppGlobals.SoundDevice then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      for i := 0 to Bass.Devices.Count - 1 do
        if Bass.Devices[i].IsDefault then
        begin
          AppGlobals.SoundDevice := Bass.Devices[i].ID;
          Break;
        end;
    end;

  Application.CreateForm(TmodSharedData, modSharedData);

  // Create the main form if everything is setup
  if InitAppStageTwo(TfrmWizard) and AppGlobals.WasSetup then
    begin
      if AppGlobals.Tray and HideMain then
      begin
        Application.ShowMainForm := False;
      end;

      Application.CreateForm(TfrmStreamWriterMain, frmStreamWriterMain);
      //Application.CreateForm(TfrmHomeTest, frmHomeTest);
    end;

    Application.Run;
  finally
    if Bass <> nil then
      Bass.Free;
    if OpenSSL <> nil then
      OpenSSL.Free;
  end;
end.