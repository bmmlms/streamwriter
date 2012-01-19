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
program streamwriter;

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  MM in '..\..\common\MM.pas',
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Windows,
  SysUtils,
  Forms,
  ShlObj,
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
  DynBASS in 'audio\DynBASS.pas',
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
  Int32Protocol in '..\..\common\sockets\Int32Protocol.pas',
  HomeCommunication in 'HomeCommunication.pas',
  Player in 'audio\Player.pas',
  PlayerManager in 'audio\PlayerManager.pas',
  PostProcess in 'postprocess\PostProcess.pas',
  PostProcessSetTags in 'postprocess\PostProcessSetTags.pas',
  PostProcessManager in 'postprocess\PostProcessManager.pas',
  Logging in '..\..\common\Logging.pas',
  Timers in 'Timers.pas' {frmTimers},
  StreamData in 'StreamData.pas' {frmStreamData},
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
  SystemCritical in '..\..\common\SystemCritical.pas',
  FileConvertor in 'audio\FileConvertor.pas',
  PluginManager in 'plugin\PluginManager.pas',
  PluginLAME in 'plugin\PluginLAME.pas',
  PluginBase in 'plugin\PluginBase.pas',
  PluginFAAC in 'plugin\PluginFAAC.pas',
  PluginSoX in 'plugin\PluginSoX.pas',
  Intro in 'Intro.pas' {frmIntro},
  PostProcessConvert in 'postprocess\PostProcessConvert.pas',
  PluginOGGEnc in 'plugin\PluginOGGEnc.pas',
  PluginMP4Box in 'plugin\PluginMP4Box.pas',
  FileTagger in 'audio\FileTagger.pas',
  PluginAudioGenie in 'plugin\PluginAudioGenie.pas',
  AudioGenie in 'audio\AudioGenie.pas',
  SavedTabEditTags in 'controls\SavedTabEditTags.pas' {frmEditTags};

{$SetPEOptFlags $0140}

{$R *.res}
{$R res\language.res}
{$R res\icons.res}
{$R res\bass.res}
{$R ..\..\common\res\language.res}
{$R ..\..\common\res\lang_icons.res}
{$R ..\..\common\res\icons.res}

var
  frmStreamWriterMain: TfrmStreamWriterMain;
begin
  {$IFDEF madExcept}
  MESettings.BugReportFile := AnsiString(IncludeTrailingBackslash(GUIFunctions.GetShellFolder(CSIDL_DESKTOP)) + 'streamwriter_bugreport.txt');
  {$ENDIF}

  Application.Title := AppGlobals.AppName;
  Application.Icon.Handle := LoadIcon(HInstance, 'A');

  // Initialize BASS, quit application on error
  Bass := TBassLoader.Create;
  if not Bass.InitializeBass(Application.Handle) then
  begin
    MsgBox(0, _('The BASS library or it''s AAC plugin could not be extracted/loaded. Without this library streamWriter cannot record/playback streams. Try to get help at streamWriter''s board.'), _('Error'), MB_ICONERROR);
    Bass.Free;
    Exit;
  end;

  // Create the main form if everything was setup
  if InitApp and AppGlobals.WasSetup then
  begin
    Application.CreateForm(TfrmStreamWriterMain, frmStreamWriterMain);
  end;

  Application.Run;

  Bass.Free;
end.
