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

uses
  MM in '..\..\common\MM.pas',
  Windows,
  Forms,
  Main in 'Main.pas' {frmStreamWriterMain},
  ClientView in 'controls\ClientView.pas',
  RecentManager in 'RecentManager.pas',
  Settings in 'Settings.pas' {frmSettings},
  AppData in 'AppData.pas',
  Wizard in 'Wizard.pas' {frmWizard},
  MPEG in 'MPEG.pas',
  ExtendedStream in '..\..\common\ExtendedStream.pas',
  Functions in '..\..\common\Functions.pas',
  AppDataBase in '..\..\common\AppDataBase.pas',
  UpdateClient in '..\..\common\UpdateClient.pas',
  HTTPThread in '..\..\common\sockets\HTTPThread.pas',
  HTTPStream in '..\..\common\sockets\HTTPStream.pas',
  SocketThread in '..\..\common\sockets\SocketThread.pas',
  SocketStream in '..\..\common\sockets\SocketStream.pas',
  ServerSocketThread in '..\..\common\sockets\ServerSocketThread.pas',
  ClientManager in 'ClientManager.pas',
  RelayServer in 'RelayServer.pas',
  HTTPServerStream in '..\..\common\sockets\HTTPServerStream.pas',
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
  Plugins in 'Plugins.pas',
  HomeCommunication in 'HomeCommunication.pas',
  StreamBrowserView in 'controls\StreamBrowserView.pas',
  StationCombo in 'controls\StationCombo.pas',
  StreamInfoView in 'controls\StreamInfoView.pas',
  StreamDebugView in 'controls\StreamDebugView.pas',
  PluginsShared in 'PluginsShared.pas',
  Base64 in 'Base64.pas',
  DynBASS in 'audio\DynBASS.pas',
  ICEClient in 'ICEClient.pas',
  ICEServerStream in 'ICEServerStream.pas',
  ICEThread in 'ICEThread.pas',
  ICEStream in 'ICEStream.pas',
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
  ICEPlayer in 'ICEPlayer.pas',
  UpdatedInfo in '..\..\common\forms\UpdatedInfo.pas' {frmUpdatedInfo};

{$SetPEOptFlags $0140}

{$R *.res}
{$R res\language.res}
{$R res\icons.res}
{$R ..\..\common\res\language.res}
{$R ..\..\common\res\lang_icons.res}
{$R ..\..\common\res\icons.res}

var
  frmStreamWriterMain: TfrmStreamWriterMain;
begin
  Application.Title := AppGlobals.AppName;
  Application.Icon.Handle := LoadIcon(HInstance, 'A');

  if InitApp and AppGlobals.WasSetup then
  begin
    Application.CreateForm(TfrmStreamWriterMain, frmStreamWriterMain);
  end;

  Application.Run;
end.
