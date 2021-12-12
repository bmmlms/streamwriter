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
  Windows,
  Classes,
  Messages,
  SysUtils,
  Forms,
  Interfaces,
  ShlObj,
  Images,
  JwaWinNT,
  AppData,
  AppDataBase,
  AppMessages,
  AppStartup,
  DynBass,
  DynOpenSSL,
  Functions,
  LanguageObjects,
  Main,
  MessageBus,
  PlayerManager,
  SharedData,
  SplashThread,
  Wizard;

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$SetPEFlags IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP}
{$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_NX_COMPAT}
{$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE}

{$R *.res}
{$R ..\Resources\images.rc}
{$R ..\Resources\language.res}
{$R ..\Resources\bass.rc}
{$R ..\Resources\openssl.rc}
{$R ..\Resources\about.rc}
{$R ..\Resources\certificates.rc}
{$R ..\SubModules\fpc-common\res\language_common.res}
{$R ..\SubModules\fpc-common\res\lang_icons.rc}

var
  i: Integer;
  HideMain, Found: Boolean;
  frmStreamWriterMain: TfrmStreamWriterMain;

begin
  IsMultiThread := True;

  SetErrorMode(SEM_FAILCRITICALERRORS);

  Bass := nil;
  OpenSSL := nil;
  try
    if not InitWinsock then
      Exit;

    HideMain := False;
    for i := 0 to ParamCount do
      if ParamStr(i) = '-minimize' then
      begin
        HideMain := True;
        Break;
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
    Application.Initialize;

    if not InitAppStageOne then
      Exit;

    // Basic initialization
    if not InitAppDataStageOne then
      Exit;

    InitPlayerManager;

    if (AppGlobals.ShowSplashScreen) and (AppGlobals.FirstStartShown) and (AppGlobals.WasSetup) and (not TFunctions.IsVersionNewer(AppGlobals.LastUsedVersion, AppGlobals.AppVersion)) and
      (not HideMain) and (not AppGlobals.InstallUpdateOnStart) then
      TSplashThread.Create('Window', 'SPLASH', AppGlobals.Codename, AppGlobals.AppVersion.AsString, AppGlobals.GitSHA,
        AppGlobals.MainLeft, AppGlobals.MainTop, AppGlobals.MainWidth, AppGlobals.MainHeight);

    // Now load everything from datafiles
    if not InitAppDataStageTwo then
      Exit;

    // Initialize BASS, quit application on error
    Bass := TBassLoader.Create;
    if not Bass.InitializeBass(0, True, False, False, False) then
    begin
      TFunctions.MsgBox(_('The BASS library or it''s plugins could not be extracted/loaded. Without these libraries streamWriter cannot record/playback streams. Try to get help at streamWriter''s board.'), _('Error'), MB_ICONERROR);
      Exit;
    end;

    OpenSSL := TOpenSSLLoader.Create;
    if not OpenSSL.InitializeOpenSSL(AppGlobals.TempDir) then
    begin
      TFunctions.MsgBox(_('The OpenSSL libraries could not be extracted/loaded. Without these libraries streamWriter cannot be run. Try to get help at streamWriter''s board.'), _('Error'), MB_ICONERROR);
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
      for i := 0 to Bass.Devices.Count - 1 do
        if Bass.Devices[i].IsDefault then
        begin
          AppGlobals.SoundDevice := Bass.Devices[i].ID;
          Break;
        end;

    Application.CreateForm(TmodSharedData, modSharedData);

    // Create the main form if everything is setup
    if InitAppStageTwo(TfrmWizard) and AppGlobals.WasSetup then
    begin
      if AppGlobals.Tray and HideMain then
        Application.ShowMainForm := False;

      Application.CreateForm(TfrmStreamWriterMain, frmStreamWriterMain);
    end;

    Application.Run;
  finally
    if Bass <> nil then
      Bass.Free;
    if OpenSSL <> nil then
      OpenSSL.Free;
  end;
end.

