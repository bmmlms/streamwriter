{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2014 Alexander Nottelmann

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

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WinSock, ICEClient, StdCtrls, ExtCtrls, ImgList, Menus,
  XPMan, VirtualTrees, ComCtrls, ToolWin, ClientView, ICEThread,
  Settings, DataManager, ActnList, DragDrop, DropTarget,
  DragDropInternet, DragDropText, DragDropFile, Update, UpdateClient,
  LanguageObjects, AppDataBase, Functions, ClientManager, ShellAPI, DropSource,
  About, MsgDlg, HomeCommunication, StreamBrowserView, Clipbrd,
  StationCombo, GUIFunctions, StreamInfoView, StreamDebugView, MessageBus,
  Buttons, DynBass, ClientTab, CutTab, MControls, Tabs, SavedTab,
  CheckFilesThread, ListsTab, CommCtrl, PngImageList, CommunityLogin,
  PlayerManager, Logging, Timers, Notifications, Generics.Collections,
  ExtendedStream, SettingsStorage, ChartsTab, StatusBar, AudioFunctions,
  PowerManagement, Intro, AddonManager, Equalizer, TypeDefs, SplashThread,
  AppMessages, CommandLine, Protocol, Commands, HomeCommands, SharedData;

const
  WM_UPDATEFOUND = WM_USER + 628;
  WM_AFTERSHOWN = WM_USER + 678;

type
  TfrmStreamWriterMain = class(TForm)
    addXP: TXPManifest;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuSettings: TMenuItem;
    N3: TMenuItem;
    mnuExit: TMenuItem;
    mnuStreams: TMenuItem;
    mnuStartStreaming2: TMenuItem;
    mnuStopStreaming2: TMenuItem;
    mnuRemove2: TMenuItem;
    mnuUpdate: TMenuItem;
    mnuCheckUpdate: TMenuItem;
    ActionList1: TActionList;
    actStart: TAction;
    actStop: TAction;
    actRemove: TAction;
    mnuStreamPopup: TPopupMenu;
    mnuStartStreaming1: TMenuItem;
    mnuStopStreaming1: TMenuItem;
    mnuRemove1: TMenuItem;
    tmrSpeed: TTimer;
    mnuStreamSettings1: TMenuItem;
    mnuStreamSettings2: TMenuItem;
    TrayIcon1: TTrayIcon;
    mnuTray: TPopupMenu;
    mnuShow: TMenuItem;
    N2: TMenuItem;
    Beenden1: TMenuItem;
    mnuTuneIn1: TMenuItem;
    N4: TMenuItem;
    mnuTuneIn2: TMenuItem;
    mnuSavePlaylist2: TMenuItem;
    N6: TMenuItem;
    mnuSavePlaylist1: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    actExit: TAction;
    actSettings: TAction;
    actAbout: TAction;
    N8: TMenuItem;
    N9: TMenuItem;
    View1: TMenuItem;
    mnuShowStreamBrowser: TMenuItem;
    actShowSideBar: TAction;
    actTuneInStream: TAction;
    actTuneInFile: TAction;
    mnuListenToStream2: TMenuItem;
    mnuListenToFile2: TMenuItem;
    mnuListenToStream1: TMenuItem;
    mnuListenToFile1: TMenuItem;
    actSavePlaylistStream: TAction;
    actSavePlaylistFile: TAction;
    actSavePlaylistStream1: TMenuItem;
    actSavePlaylistFile1: TMenuItem;
    Stream1: TMenuItem;
    Stream2: TMenuItem;
    mnuReset1: TMenuItem;
    mnuReset11: TMenuItem;
    actResetData: TAction;
    tbClients: TToolBar;
    cmdStart: TToolButton;
    cmdStop: TToolButton;
    ToolButton3: TToolButton;
    cmdRemove: TToolButton;
    ToolButton2: TToolButton;
    cmdStartPlay: TToolButton;
    cmdStopPlay: TToolButton;
    ToolButton1: TToolButton;
    ToolButton6: TToolButton;
    ToolButton4: TToolButton;
    cmdShowStreamBrowser: TToolButton;
    actCutSave: TAction;
    actCutSaveAs: TAction;
    mnuHelp2: TMenuItem;
    N1: TMenuItem;
    actHelp: TAction;
    actPlay: TAction;
    actStopPlay: TAction;
    mnuStartPlay2: TMenuItem;
    mnuStopPlay2: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    mnuStartPlay1: TMenuItem;
    mnuStopPlay1: TMenuItem;
    actNewCategory: TAction;
    mnuNewCategory1: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    Newcategory1: TMenuItem;
    ToolButton5: TToolButton;
    ToolButton7: TToolButton;
    actStreamSettings: TAction;
    cmdStreamSettings: TToolButton;
    actOpenWebsite: TAction;
    mnuOpenWebsite1: TMenuItem;
    Openwebsite1: TMenuItem;
    cmdPause: TToolButton;
    actPause: TAction;
    Pause1: TMenuItem;
    mnuPause1: TMenuItem;
    tmrRecordings: TTimer;
    Community1: TMenuItem;
    actLogOn: TAction;
    Login1: TMenuItem;
    actLogOff: TAction;
    Logoff1: TMenuItem;
    ToolButton8: TToolButton;
    cmdOpenWebsite: TToolButton;
    mnuMoveToCategory1: TMenuItem;
    actTimers: TAction;
    actStopAfterSong: TAction;
    mnuSetupTimers1: TMenuItem;
    mnuStopAfterSong1: TMenuItem;
    cmdStopAfterSong: TToolButton;
    cmdSetupTimers: TToolButton;
    mnuCurrentTitle1: TMenuItem;
    actCopyTitle: TAction;
    cmdCopyTitle: TToolButton;
    tmrSchedule: TTimer;
    Copytitletoclipboard1: TMenuItem;
    actAddToSaveList: TAction;
    Addtowishlist1: TMenuItem;
    actAddToGlobalIgnoreList: TAction;
    actAddToStreamIgnoreList: TAction;
    Addtoglobalignorelist1: TMenuItem;
    Addtostreamignorelist1: TMenuItem;
    N5: TMenuItem;
    tmrAutoSave: TTimer;
    mnuPlayer: TMenuItem;
    mnuPlayPause: TMenuItem;
    mnuStop: TMenuItem;
    mnuIncreaseVolume: TMenuItem;
    mnuPlayerDecreaseVolume: TMenuItem;
    actPlayerDecreaseVolume: TAction;
    actPlayerIncreaseVolume: TAction;
    N7: TMenuItem;
    actPlayerPlayPause: TAction;
    actPlayerStop: TAction;
    actPlayerMuteVolume: TAction;
    mnuPlayerMuteVolume: TMenuItem;
    mnuRename1: TMenuItem;
    actRename: TAction;
    cmdRename: TToolButton;
    N14: TMenuItem;
    mnuEqualizer: TMenuItem;
    actEqualizer: TAction;
    ToolButton9: TToolButton;
    N15: TMenuItem;
    Settingsforautomaticrecordings1: TMenuItem;
    actAutoSettings: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrSpeedTimer(Sender: TObject);
    procedure actStreamSettingsExecute(Sender: TObject);
    procedure addTrayClick(Sender: TObject);
    procedure mnuCheckUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuShowClick(Sender: TObject);
    procedure mnuStreamPopupPopup(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actAutoSettingsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure mnuStreamSettingsToolbarPopup(Sender: TObject);
    procedure pagSidebarChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actHelpExecute(Sender: TObject);
    procedure tmrRecordingsTimer(Sender: TObject);
    procedure actLogOnExecute(Sender: TObject);
    procedure Community1Click(Sender: TObject);
    procedure actLogOffExecute(Sender: TObject);
    procedure actTimersExecute(Sender: TObject);
    procedure tmrScheduleTimer(Sender: TObject);
    procedure tmrAutoSaveTimer(Sender: TObject);
    procedure actPlayerDecreaseVolumeExecute(Sender: TObject);
    procedure actPlayerIncreaseVolumeExecute(Sender: TObject);
    procedure actPlayerStopExecute(Sender: TObject);
    procedure actPlayerPlayPauseExecute(Sender: TObject);
    procedure actPlayerMuteVolumeExecute(Sender: TObject);
    procedure mnuPlayerClick(Sender: TObject);
    procedure actEqualizerExecute(Sender: TObject);
  private
    FMainCaption: string;

    FCommunityLogin: TfrmCommunityLogin;

    FDataLists: TDataLists;
    FUpdater: TUpdateClient;
    FUpdateOnExit: Boolean;
    FSkipAfterShown: Boolean;

    FClientCount: Cardinal;
    FRecordingCount: Cardinal;

    FWasShown: Boolean;
    FWasMaximized: Boolean;

    FCheckFiles: TCheckFilesThread;
    FClients: TClientManager;
    pagMain: TMainPageControl;
    tabClients: TClientTab;
    tabCharts: TChartsTab;
    tabLists: TListsTab;
    tabSaved: TSavedTab;
    addStatus: TSWStatusBar;

    FEqualizer: TfrmEqualizer;

    FExiting: Boolean;

    procedure OneInstanceMessage(var Msg: TMessage); message WM_USER + 1234;
    procedure AfterShown(var Msg: TMessage); message WM_AFTERSHOWN;
    procedure ReceivedData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure QueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;
    procedure EndSession(var Msg: TMessage); message WM_ENDSESSION;
    procedure SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure Hotkey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure UpdateFound(var Msg: TMessage); message WM_UPDATEFOUND;

    function CanExitApp: Boolean;
    procedure ExitApp(Shutdown: Boolean; ImportFilename: string = '');
    procedure ShowSettings(SettingsType: TSettingsTypes; BrowseDir: Boolean);
    procedure ShowUpdate(Version: string = ''; UpdateURL: string = '');
    procedure UpdateButtons;
    procedure UpdateStatus;
    procedure ToggleWindow(AlwaysShow: Boolean = False);
    procedure UpdaterUpdateFound(Sender: TObject);
    procedure UpdaterNoUpdateFound(Sender: TObject);
    function HandleLoadError(E: Exception): Integer;
    procedure CheckFilesTerminate(Sender: TObject);
    procedure RegisterHotkeys(Reg: Boolean);
    procedure ShowCommunityLogin;
    procedure OpenCut(Filename: string); overload;
    procedure OpenCut(Track: TTrackInfo); overload;
    procedure ProcessCommandLine(Data: string);
    procedure SetWakeups;

    function StartupMessagesNeeded: Boolean;
    procedure ShowStartupMessages;

    procedure CommunityLoginClose(Sender: TObject; var Action: TCloseAction);

    procedure HomeCommStateChanged(Sender: TObject);
    procedure HomeCommBytesTransferred(Sender: TObject;
      Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader;
      Transferred: UInt64);
    procedure HomeCommTitleNotificationsChanged(Sender: TObject);
    procedure HomeCommHandshake(Sender: TObject; Success: Boolean);
    procedure HomeCommLogIn(Sender: TObject; Success: Boolean);
    procedure HomeCommLogOut(Sender: TObject);
    procedure HomeCommServerInfo(Sender: TObject; ClientCount, RecordingCount: Cardinal);
    procedure HomeCommError(Sender: TObject; ID: TCommErrors; Msg: string);
    //procedure HomeCommServerDataReceived(Sender: TObject);

    procedure PreTranslate;
    procedure PostTranslate;

    procedure tabClientsUpdateButtons(Sender: TObject);
    procedure tabClientsTrackAdded(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabClientsTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabClientsAddTitleToList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string);
    procedure tabClientsRemoveTitleFromList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string; ServerTitleHash: Cardinal);
    procedure tabClientsAuthRequired(Sender: TObject);
    procedure tabClientsShowErrorMessage(Sender: TICEClient; Msg: TMayConnectResults; WasAuto, WasScheduled: Boolean);
    procedure tabClientsClientAdded(Sender: TObject);
    procedure tabClientsClientRemoved(Sender: TObject);

    procedure tabSavedCut(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabSavedTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabSavedRefresh(Sender: TObject);
    procedure tabSavedAddTitleToWishlist(Sender: TObject; Title: string; TitleHash: Cardinal);
    procedure tabSavedRemoveTitleFromWishlist(Sender: TObject; Title: string; TitleHash: Cardinal);
    procedure tabSavedAddTitleToIgnorelist(Sender: TObject; Title: string; TitleHash: Cardinal);

    procedure tabCutCutFile(Sender: TObject; Filename: string);
    procedure tabCutSaved(Sender: TObject; AudioInfo: TAudioFileInfo);

    procedure tabPlayStarted(Sender: TObject);

    procedure tabChartsAddToWishlist(Sender: TObject; Arr: TWishlistTitleInfoArray);
    procedure tabChartsRemoveFromWishlist(Sender: TObject; Arr: TWishlistTitleInfoArray);
    procedure tabChartsAddStreams(Sender: TObject; Info: TStartStreamingInfoArray; Action: TStreamOpenActions);
    function tabChartsGetIsStreamOnListEvent(Sender: TObject; Stream: TStreamBrowserEntry): Boolean;

    procedure mnuMoveToCategory(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);

    procedure SettingsSaveForExport(Sender: TObject);
  protected

  public
    constructor Create(AOwner: TComponent); override;

  end;

  TStatusHint = class(TCustomHint)
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetHintSize(HintWindow: TCustomHintWindow); override;
    procedure PaintHint(HintWindow: TCustomHintWindow); override;
  end;

implementation

uses
  AppData, Player;

{$R *.dfm}

procedure TfrmStreamWriterMain.ExitApp(Shutdown: Boolean; ImportFilename: string);
var
  i: Integer;
  Res: Integer;
  StartTime, Version: Cardinal;
  Hard: Boolean;
  S: TExtendedStream;
  Lst: TSettingsList;
begin
  if FExiting then
    Exit;

  if not Shutdown then
    for i := 0 to pagMain.PageCount - 1 do
      if not TMTabSheet(pagMain.Pages[i]).CanClose then
        Exit;

  FExiting := True;

  if ImportFilename = '' then
    AppGlobals.WindowHandle := 0;

  AppGlobals.MainMaximized := WindowState = wsMaximized;
  if not AppGlobals.MainMaximized then
  begin
    AppGlobals.MainLeft := Left;
    AppGlobals.MainTop := Top;
    AppGlobals.MainWidth := Width;
    AppGlobals.MainHeight := Height;
  end;

  AppGlobals.SidebarWidth := tabClients.SideBar.Width;
  AppGlobals.BrowserSortType := Integer(tabClients.SideBar.BrowserView.StreamTree.SelectedSortType);

  for i := 0 to tabClients.ClientView.Header.Columns.Count - 1 do
    AppGlobals.ClientHeaderWidth[i] := tabClients.ClientView.Header.Columns[i].Width;
  for i := 0 to tabSaved.Tree.Header.Columns.Count - 1 do
    AppGlobals.SavedHeaderWidth[i] := tabSaved.Tree.Header.Columns[i].Width;

  try
    // Es ist mir beim Theme-Wechsel passiert, dass der Tray komplett verschwunden ist.
    // Beim Beenden von sW gab es eine Exception. Das hier sollte helfen ;-) ...
    TrayIcon1.Visible := False;
  except
  end;

  tmrSpeed.Enabled := False;
  tmrSchedule.Enabled := False;
  tmrAutoSave.Enabled := False;
  tmrRecordings.Enabled := False;

  TfrmNotification.Stop;

  FEqualizer.Hide;
  Hide;

  Players.StopAll;

  MsgBus.RemoveSubscriber(MessageReceived);

  AppGlobals.PlayerVolume := Players.Volume;
  AppGlobals.PlayerVolumeBeforeMute := Players.VolumeBeforeMute;

  HomeComm.Terminate;
  while (HomeComm.ThreadAlive) and HomeComm.Connected do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;

  tabSaved.StopThreads;

  FUpdater.Kill;

  AppGlobals.LastUsedDataVersion := DataManager.DATAVERSION;
  if not Shutdown then
    AppGlobals.Save(Handle)
  else
    try
      AppGlobals.Save;
    except end;

  // Das muss so, damit der Import von Profilen klappt. Sonst wird beim Hinzufügen
  // in die SaveList das OnChange aufgerufen und es crashed. Also hier lassen.
  //FDataLists.SaveList.OnChange.Clear;

  // Shutdown all postprocessors. Since they get terminated, all postprocessing chains get terminated.
  AppGlobals.PostProcessManager.Terminate;
  // Disconnect all clients. They will save their recordings and will not postprocess any file.
  FClients.Stop;

  if FCheckFiles <> nil then
    FCheckFiles.Terminate;

  Hard := False;
  StartTime := GetTickCount;
  while (HomeComm.ThreadAlive or HomeComm.Connected) or FClients.Active or (FCheckFiles <> nil) or FUpdater.Active do
  begin
    // Wait 30 seconds for threads to finish
    if StartTime < GetTickCount - 30000 then
    begin
      Hard := True;
      Break;
    end;
    Sleep(100);
    Application.ProcessMessages;
  end;

  // Erst Lists updaten, dann Streams!
  tabSaved.Tree.UpdateList;
  tabLists.UpdateLists;
  tabClients.UpdateStreams(FDataLists);

  while True do
  begin
    try
      FDataLists.Save;
      Break;
    except
      if not Shutdown then
      begin
        Res := MsgBox(Handle, Format(_('An error occured while saving the data file. Please make sure you can write to "%s" and that the file is not in use by another application. Click "Yes" to try again, "No" to exit without saving data.'), [AppGlobals.DataFile]), _('Info'), MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON1);
        if Res = IDNO then
          Break;
      end else
        Break;
    end;
  end;

  // Remove all clients from list - then wait and process their last messages
  FClients.Terminate;

  tabClients.ClientView.Clear;

  if FUpdateOnExit then
    FUpdater.RunUpdate(Handle);

  if ImportFilename <> '' then
  begin
    try
      S := TExtendedStream.Create;
      try
        S.LoadFromFile(ImportFilename);
        S.Read(Version);
        Lst := TSettingsList.Load(S);
        try
          AppGlobals.Storage.Assign(Lst);
          FDataLists.Load(S);
          FDataLists.Save;
        finally
          Lst.Free;
        end;
      finally
        S.Free;
      end;
    except
      on E: EVersionException do
        MsgBox(0, _('The file could not be imported because it was exported with a newer version of streamWriter.'), _('Error'), MB_ICONERROR)
      else
        MsgBox(0, _('The file could not be imported.'), _('Error'), MB_ICONERROR)
    end;
    RunProcess('"' + Application.ExeName + '" /profileupdate', False);
  end;

  // We have to close all cut-tabs here because otherwise FreeAndNil(FPlayer) in CutTab.Free()
  // throws an exception which causes that the temporary WAVE-file cannot be deleted...
  for i := pagMain.PageCount - 1 downto 0 do
    if pagMain.Pages[i].ClassType = TCutTab then
      pagMain.Pages[i].Free;

  if Hard then
    Halt
  else
    Application.Terminate;
end;

procedure TfrmStreamWriterMain.actSettingsExecute(Sender: TObject);
begin
  ShowSettings(stApp, False);
end;

procedure TfrmStreamWriterMain.actAboutExecute(Sender: TObject);
var
  F: TfrmAbout;
begin
  F := TfrmAbout.Create(Self, _('About'));
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TfrmStreamWriterMain.actAutoSettingsExecute(Sender: TObject);
begin
  ShowSettings(stAuto, False);
end;

procedure TfrmStreamWriterMain.actEqualizerExecute(Sender: TObject);
begin
  if not FEqualizer.Visible then
  begin
    FEqualizer.Left := Left + 20;
    FEqualizer.Top := Top + Height - 40 - FEqualizer.Height;
  end;
  FEqualizer.Show;
end;

procedure TfrmStreamWriterMain.actExitExecute(Sender: TObject);
begin
  if CanExitApp then
    ExitApp(False);
end;

procedure TfrmStreamWriterMain.actHelpExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(AppGlobals.ProjectHelpLinkMain), '', '', 1);
end;

procedure TfrmStreamWriterMain.actPlayerIncreaseVolumeExecute(Sender: TObject);
begin
  Players.IncreaseVolume;
end;

procedure TfrmStreamWriterMain.actPlayerMuteVolumeExecute(Sender: TObject);
begin
  if Players.Volume > 0 then
  begin
    Players.Volume := 0;
  end else
  begin
    Players.Volume := Players.VolumeBeforeMute;
  end;
end;

procedure TfrmStreamWriterMain.actPlayerPlayPauseExecute(Sender: TObject);
begin
  Players.PauseAll;
end;

procedure TfrmStreamWriterMain.actPlayerStopExecute(Sender: TObject);
begin
  Players.StopAll;
end;

procedure TfrmStreamWriterMain.actLogOnExecute(Sender: TObject);
begin
  ShowCommunityLogin;
end;

procedure TfrmStreamWriterMain.actPlayerDecreaseVolumeExecute(
  Sender: TObject);
begin
  Players.DecreaseVolume;
end;

procedure TfrmStreamWriterMain.actLogOffExecute(Sender: TObject);
begin
  HomeComm.SendLogOut;
end;

procedure TfrmStreamWriterMain.actStreamSettingsExecute(Sender: TObject);
var
  Clients: TClientArray;
  S: TfrmSettings;
  Settings: TStreamSettingsArray;
  i: Integer;
begin
  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClientNoAuto, True));

  if Length(Clients) > 0 then
  begin
    SetLength(Settings, Length(Clients));

    for i := 0 to Length(Clients) - 1 do
      Settings[i] := Clients[i].Entry.Settings;

    S := TfrmSettings.Create(Self, stStream, FDataLists, Settings, False);
    S.ShowModal;

    if S.SaveSettings then
    begin
      for i := 0 to Length(Clients) - 1 do
        if not Clients[i].AutoRemove then
          Clients[i].Entry.Settings.Assign(S.StreamSettings[i]);
    end;
  end;

  // Damit die Entries im Hauptmenü angepasst werden, falls von Popup was geändert wurde.
  UpdateButtons;
end;

procedure TfrmStreamWriterMain.actTimersExecute(Sender: TObject);
var
  Clients: TClientArray;
  T: TfrmTimers;
begin
  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClientNoAuto, True));

  if (Length(Clients) <> 1) and (Clients[0].AutoRemove) then
    Exit;

  T := TfrmTimers.Create(Self, Clients[0].Entry);
  try
    T.ShowModal;
  finally
    T.Free;
  end;
end;

procedure TfrmStreamWriterMain.addTrayClick(Sender: TObject);
begin
  ToggleWindow(False);
end;

procedure TfrmStreamWriterMain.AfterShown(var Msg: TMessage);
var
  FormIntro: TfrmIntro;
begin
  if FSkipAfterShown then
    Exit;

  ShowStartupMessages;

  // If a profile import is active from a call to ShowSettings() in ShowStartupMessages,
  // we need to skip this stuff.
  if not FExiting then
  begin
    if not AppGlobals.FirstStartShown then
    begin
      FormIntro := TfrmIntro.Create(Self);
      try
        FormIntro.ShowModal;
      finally
        FormIntro.Free;
      end;
    end;
    AppGlobals.FirstStartShown := True;

    if StartupMessagesNeeded then
      if (AppGlobals.AutoUpdate) and (AppGlobals.LastUpdateChecked + 1 < Now) then
        FUpdater.Start(uaVersion, True);
  end;
end;

procedure TfrmStreamWriterMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
  if (AppGlobals.Tray) and (not AppGlobals.TrayOnMinimize) then
  begin
    if (Visible) or (IsIconic(Handle)) then
    begin
      TrayIcon1.Visible := True;
      FWasMaximized := WindowState = wsMaximized;
      Hide;
    end;
  end else
  begin
    if CanExitApp then
      ExitApp(False);
  end;
end;

procedure TfrmStreamWriterMain.FormCreate(Sender: TObject);
var
  Recovered: Boolean;
  S: TExtendedStream;
begin
  FMainCaption := 'streamWriter';
  {$IFDEF DEBUG}FMainCaption := FMainCaption + ' --::: DEBUG BUiLD :::--';{$ENDIF}
  Caption := FMainCaption;

  AppGlobals.WindowHandle := Handle;

  if not Bass.EffectsAvailable then
  begin
    actEqualizer.Enabled := False;
    Players.EQEnabled := False;
    AppGlobals.EQEnabled := False;
  end;

  FDataLists := TDataLists.Create;

  HomeComm := THomeCommunication.Create(FDataLists);

  Recovered := False;
  {$IFNDEF DEBUG}
  if FileExists(AppGlobals.RecoveryFile) then
  begin
    if MsgBox(0, _('It seems that streamWriter has not been shutdown correctly, maybe streamWriter or your computer crashed.'#13#10'Do you want to load the latest automatically saved data?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
    begin
      try
        S := TExtendedStream.Create;
        try
          S.LoadFromFile(AppGlobals.RecoveryFile);
          FDataLists.Load(S);
          Recovered := True;
        finally
          S.Free;
        end;
      except
        MsgBox(0, _('Data could not be loaded.'), _('Error'), MB_ICONERROR);
      end;
    end;
  end;
  {$ENDIF}

  try
    if not Recovered then
      FDataLists.Load;
  except
    on E: Exception do
    begin
      try
        FDataLists.Free;
      except end;
      FDataLists := TDataLists.Create;
      // Damit beim Beenden nichts überschrieben wird.
      FDataLists.LoadError := True;

      if HandleLoadError(E) = IDYES then
      begin
        DeleteFile(E.Message);
        FDataLists.LoadError := False;
      end;
    end;
  end;

  addStatus := TSWStatusBar.Create(Self);
  addStatus.Parent := Self;

  FClients := TClientManager.Create(FDataLists);

  pagMain := TMainPageControl.Create(Self);
  pagMain.Parent := Self;
  pagMain.Visible := True;
  pagMain.Align := alClient;
  pagMain.Images := modSharedData.imgImages;

  tabClients := TClientTab.Create(pagMain, tbClients, ActionList1, FClients, FDataLists, mnuStreamPopup);
  tabClients.PageControl := pagMain;
  tabClients.AfterCreate;
  tabClients.AddressBar.Stations.Sort;
  tabClients.AddressBar.Stations.BuildList(FDataLists.RecentList);
  tabClients.OnUpdateButtons := tabClientsUpdateButtons;
  tabClients.OnTrackAdded := tabClientsTrackAdded;
  tabClients.OnTrackRemoved := tabClientsTrackRemoved;
  tabClients.OnAddTitleToList := tabClientsAddTitleToList;
  tabClients.OnRemoveTitleFromList := tabClientsRemoveTitleFromList;
  tabClients.OnPlayStarted := tabPlayStarted;
  tabClients.OnAuthRequired := tabClientsAuthRequired;
  tabClients.OnShowErrorMessage := tabClientsShowErrorMessage;
  tabClients.OnClientAdded := tabClientsClientAdded;
  tabClients.OnClientRemoved := tabClientsClientRemoved;

  tabCharts := TChartsTab.Create(pagMain, FDataLists);
  tabCharts.PageControl := pagMain;
  tabCharts.AfterCreate;
  tabCharts.OnAddToWishlist := tabChartsAddToWishlist;
  tabCharts.OnRemoveTitleFromWishlist := tabChartsRemoveFromWishlist;
  tabCharts.OnAddStreams := tabChartsAddStreams;
  tabCharts.OnGetIsStreamOnListEvent := tabChartsGetIsStreamOnListEvent;

  tabLists := TListsTab.Create(pagMain, FClients, FDataLists);
  tabLists.PageControl := pagMain;
  tabLists.AfterCreate;

  tabSaved := TSavedTab.Create(pagMain, FDataLists);
  tabSaved.PageControl := pagMain;
  tabSaved.AfterCreate;

  tabSaved.OnCut := tabSavedCut;
  tabSaved.OnTrackRemoved := tabSavedTrackRemoved;
  tabSaved.OnRefresh := tabSavedRefresh;
  tabSaved.OnPlayStarted := tabPlayStarted;
  tabSaved.OnAddTitleToWishlist := tabSavedAddTitleToWishlist;
  tabSaved.OnRemoveTitleFromWishlist := tabSavedRemoveTitleFromWishlist;
  tabSaved.OnAddTitleToIgnorelist := tabSavedAddTitleToIgnorelist;

  FWasShown := False;
  FUpdateOnExit := False;

  UpdateStatus;
  TrayIcon1.Visible := AppGlobals.Tray;

  Width := AppGlobals.MainWidth;
  Height := AppGlobals.MainHeight;
  if (AppGlobals.MainLeft = -1) or (AppGlobals.MainTop = -1) then
  begin
    AppGlobals.MainLeft := Screen.Width div 2 - Width div 2;
    AppGlobals.MainTop := Screen.Height div 2 - Height div 2;
  end;
  Left := AppGlobals.MainLeft;
  Top := AppGlobals.MainTop;
  ScreenSnap := AppGlobals.SnapMain;

  addStatus.CustomHint := TStatusHint.Create(Self);

  // Ist nun hier, damit man nicht sieht, wie sich alle Controls resizen.
  if AppGlobals.MainMaximized then
    WindowState := wsMaximized;
  FWasMaximized := WindowState = wsMaximized;

  FEqualizer := TfrmEqualizer.Create(Self);

  FUpdater := TUpdateClient.Create;
  FUpdater.OnNoUpdateFound := UpdaterNoUpdateFound;
  FUpdater.OnUpdateFound := UpdaterUpdateFound;

  HomeComm.OnStateChanged := HomeCommStateChanged;
  HomeComm.OnBytesTransferred := HomeCommBytesTransferred;
  HomeComm.OnTitleNotificationsChanged := HomeCommTitleNotificationsChanged;
  HomeComm.OnHandshakeReceived := HomeCommHandshake;
  HomeComm.OnLogInReceived := HomeCommLogIn;
  HomeComm.OnLogOutReceived := HomeCommLogOut;
  HomeComm.OnServerInfoReceived := HomeCommServerInfo;
  HomeComm.OnErrorReceived := HomeCommError;
  HomeComm.Connect;

  actPlayerIncreaseVolume.Enabled := Bass.DeviceAvailable;
  actPlayerDecreaseVolume.Enabled := Bass.DeviceAvailable;
  actPlayerMuteVolume.Enabled := Bass.DeviceAvailable;
  actEqualizer.Enabled := Bass.DeviceAvailable;

  tabSavedRefresh(nil);

  tmrAutoSave.Enabled := True;
  tmrRecordings.Enabled := True;

  ProcessCommandLine('');

  SetWakeups;

  tmrSpeed.Enabled := True;
  tmrSchedule.Enabled := True;

  RegisterHotkeys(True);

  UpdateButtons;

  actShowSideBar.Checked := tabClients.SideBar.Visible;

  MsgBus.AddSubscriber(MessageReceived);

  Language.Translate(Self);

  if not Application.ShowMainForm then
  begin
    if StartupMessagesNeeded then
    begin
      Show;
    end else
    begin
      FSkipAfterShown := True;
    end;
  end;

  if not StartupMessagesNeeded then
  begin
    if (AppGlobals.AutoUpdate) and (AppGlobals.LastUpdateChecked + 1 < Now) then
      FUpdater.Start(uaVersion, True);
  end;
end;

procedure TfrmStreamWriterMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClients);
  FreeAndNil(FUpdater);
  FreeAndNil(FDataLists);
end;

procedure TfrmStreamWriterMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    actHelp.Execute;
end;

procedure TfrmStreamWriterMain.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if FWasShown then
    Exit;

  if Application.ShowMainForm then
    for i := 0 to ParamCount do
      if (ParamStr(i) = '-minimize') then
        WindowState := wsMinimized;

  FWasShown := True;

  tabClients.ClientView.SetFocus;

  PostMessage(Handle, WM_AFTERSHOWN, 0, 0);
end;

function TfrmStreamWriterMain.HandleLoadError(E: Exception): Integer;
begin
  if E is EVersionException then
    begin
      Result := MsgBox(0, Format(_('The file "%s" could not be loaded because it was saved with a newer version of streamWriter. ' +
                                   'To use the current file, exit streamWriter and use a newer version of the application. ' +
                                   'To delete the file and continue to use this version click "Yes".'#13#10 +
                                   'WARNING: All data saved in the file will be lost!'#13#10 +
                                   'The file will not be overwritten with new data until it was loaded or deleted.'),
                                 [E.Message]),
                                 _('Info'), MB_YESNO or MB_ICONEXCLAMATION or MB_DEFBUTTON2);
    end else
    begin
      Result := MsgBox(0, Format(_('The file "%s" could not be loaded because it is corrupted. ' +
                                   'You can delete it to avoid this error when streamWriter starts by clicking "Yes".'#13#10 +
                                   'WARNING: All data saved in the file will be lost!'#13#10 +
                                   'The file will not be overwritten with new data until it was loaded or deleted.'),
                                 [E.Message]),
                                 _('Info'), MB_YESNO or MB_ICONEXCLAMATION or MB_DEFBUTTON2);
    end;
end;

procedure TfrmStreamWriterMain.HomeCommBytesTransferred(Sender: TObject;
  Direction: TTransferDirection; CommandID: Cardinal;
  CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  if CommandHeader.CommandType = ctGetServerDataResponse then
  begin
    tabClients.SideBar.BrowserView.HomeCommBytesTransferred(CommandHeader, Transferred);
    //tabCharts.HomeCommBytesTransferred(CommandHeader, Transferred);
  end;
end;

procedure TfrmStreamWriterMain.HomeCommError(Sender: TObject; ID: TCommErrors; Msg: string);
const
  Notification = 'A notification from the server was received:'#13#10'%s';
var
  MsgHC: Cardinal;
  MsgHash: Integer;
begin
  case ID of
    ceUnknown:
      MsgBox(Handle, Format(_('An error occured while communicating with the server: '#13#10'%s'), [Msg]), _('Error'), MB_ICONERROR);
    ceAuthRequired:
      begin
        if MsgBox(Handle, _('You need to be logged in to perform that action.'#13#10'Do you want to login now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
          ShowCommunityLogin;
      end;
    ceNotification:
      begin
        TfrmMsgDlg.ShowMsg(Self, Format(_(Notification), [Msg]), mtInformation, [mbOK], mbOK);
      end;
    ceOneTimeNotification:
      begin
        MsgHC := HashString(Msg);
        if MsgHC > MaxInt then
          MsgHash := MsgHC - MaxInt
        else
          MsgHash := MsgHC;

        if MsgHash < 0 then
          MsgHash := MsgHash * -1;
        if MsgHash < 100 then
          MsgHash := MsgHash + 100;

        TfrmMsgDlg.ShowMsg(Self, Format(_(Notification), [Msg]), mtInformation, [mbOK], mbOK, MsgHash);
      end;
  end;
end;

procedure TfrmStreamWriterMain.HomeCommHandshake(Sender: TObject;
  Success: Boolean);
begin
  UpdateStatus;

  if not Success then
    MsgBox(Handle, _('The server did not accept the handshake. Please update streamWriter.'), _('Error'), MB_ICONERROR);
end;

procedure TfrmStreamWriterMain.HomeCommLogIn(Sender: TObject;
  Success: Boolean);
begin
  UpdateStatus;
  if FCommunityLogin <> nil then
    FCommunityLogin.HomeCommLogIn(Sender, Success);
end;

procedure TfrmStreamWriterMain.HomeCommLogOut(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStreamWriterMain.HomeCommServerInfo(Sender: TObject;
  ClientCount, RecordingCount: Cardinal);
begin
  FClientCount := ClientCount;
  FRecordingCount := RecordingCount;
  UpdateStatus;
end;

procedure TfrmStreamWriterMain.HomeCommStateChanged(Sender: TObject);
begin
  UpdateStatus;
  tabCharts.HomeCommStateChanged(Sender);

  if (not HomeComm.WasConnected) and HomeComm.Connected then
  begin
    tmrRecordingsTimer(tmrRecordings);

    FClients.StopMonitors;
    if AppGlobals.SubmitStats and AppGlobals.MonitorMode and (AppGlobals.MonitorCount > 0) then
      HomeComm.SendGetMonitorStreams(AppGlobals.MonitorCount)
    else
      HomeComm.SendGetMonitorStreams(0);

    HomeComm.SendSyncWishlist;

    if not tabCharts.Searched then
      tabCharts.SearchCharts(True, False);

    if (((FDataLists.BrowserList.Count = 0) or (FDataLists.GenreList.Count = 0)) or (AppGlobals.LastBrowserUpdate < Now - 15)) or
       (tabClients.SideBar.BrowserView.Mode = moError) then
    begin
      if HomeComm.SendGetServerData then
      begin
        tabClients.SideBar.BrowserView.SwitchMode(moLoading);
      end;
    end;
  end else if (HomeComm.WasConnected) and (not HomeComm.Connected) then
  begin
    FClients.StopMonitors;

    if tabCharts.State = csSearching then
      tabCharts.SetState(csSearchError);

    if tabClients.SideBar.BrowserView.Mode = moLoading then
      tabClients.SideBar.BrowserView.SwitchMode(moError);
  end;

  HomeComm.SendSetSettings((FDataLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
end;

procedure TfrmStreamWriterMain.HomeCommTitleNotificationsChanged(
  Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStreamWriterMain.Hotkey(var Msg: TWMHotKey);
  procedure StopPlay;
  var
    i: Integer;
    Clients: TClientArray;
  begin
    Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClient, False));
    for i := 0 to Length(Clients) - 1 do
      Clients[i].StopPlay;
  end;
var
  i: Integer;
  NextIsPlaying: Boolean;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  Clients: TClientArray;
  PlayingClient: TICEClient;
  StartPlayClient: TICEClient;
begin
  NextIsPlaying := False;
  PlayingClient := nil;
  StartPlayClient := nil;

  case Msg.HotKey of
    0:
      begin
        Nodes := tabClients.ClientView.GetNodes(ntClient, True);
        if Length(Nodes) > 0 then
        begin
          NodeData := tabClients.ClientView.GetNodeData(Nodes[0]);
          if not NodeData.Client.Playing then
            StopPlay;
          NodeData.Client.StartPlay(True);
        end else
        begin
          Nodes := tabClients.ClientView.GetNodes(ntClient, False);
          if Length(Nodes) > 0 then
          begin
            NodeData := tabClients.ClientView.GetNodeData(Nodes[0]);
            if not NodeData.Client.Playing then
              StopPlay;
            NodeData.Client.StartPlay(True);
          end;
        end;
      end;
    1:
      Players.PauseAll;
    2:
      Players.StopAll;
    3:
      begin
        Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClient, False));
        for i := 0 to Length(Clients) - 1 do
          if Clients[i].Playing then
            PlayingClient := Clients[i];
        if PlayingClient <> nil then
        begin
          for i := 0 to Length(Clients) - 1 do
          begin
            if NextIsPlaying then
            begin
              StartPlayClient := Clients[i];
              Break;
            end;
            if Clients[i].Playing then
              NextIsPlaying := True;
          end;

          if StartPlayClient = nil then
            if Length(Clients) > 0 then
              StartPlayClient := Clients[0];

          if StartPlayClient <> PlayingClient then
          begin
            StopPlay;
            StartPlayClient.StartPlay(True);
          end;
        end;
      end;
    4:
      begin
        Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClient, False));
        for i := 0 to Length(Clients) - 1 do
          if Clients[i].Playing then
            PlayingClient := Clients[i];
        if PlayingClient <> nil then
        begin
          for i := Length(Clients) - 1 downto 0 do
          begin
            if NextIsPlaying then
            begin
              StartPlayClient := Clients[i];
              Break;
            end;
            if Clients[i].Playing then
              NextIsPlaying := True;
          end;

          if StartPlayClient = nil then
            if Length(Clients) > 0 then
              StartPlayClient := Clients[High(Clients)];

          if StartPlayClient <> PlayingClient then
          begin
            StopPlay;
            StartPlayClient.StartPlay(True);
          end;
        end;
      end;
    5:
      begin
        Players.IncreaseVolume;
      end;
    6:
      begin
        Players.DecreaseVolume;
      end;
    7:
      begin
        Players.Mute;
      end;
  end;
end;

procedure TfrmStreamWriterMain.MessageReceived(Msg: TMessageBase);
var
  Artist, Title, Stream, Filename: string;
  NewCaption: string;

  SelectSavedSongsMsg: TSelectSavedSongsMsg absolute Msg;
begin
  if (Msg is TPlayingObjectChangedMsg) or (Msg is TPlayingObjectStopped) then
  begin
    if not AppGlobals.DisplayPlayedSong then
      Exit;

    NewCaption := FMainCaption;
    TrayIcon1.Hint := 'streamWriter';

    PlayerManager.Players.GetPlayingInfo(Artist, Title, Stream, Filename);

    if Filename <> '' then
    begin
      if (Artist <> '') and (Title <> '') then
      begin
        NewCaption := FMainCaption + ' - ' + ShortenString(Artist, 30) + ' - ' + ShortenString(Title, 30);
        TrayIcon1.Hint := 'streamWriter'#13#10 + _('Playing:') + ' ' + Artist + ' - ' + Title;
      end else
      begin
        NewCaption := FMainCaption + ' - ' + ShortenString(RemoveFileExt(ExtractFileName(Filename)), 30);
        TrayIcon1.Hint := 'streamWriter'#13#10 + _('Playing:') + ' ' + RemoveFileExt(ExtractFileName(Filename));
      end;
    end else if Stream <> '' then
    begin
      if Title <> '' then
      begin
        NewCaption := FMainCaption + ' - ' + ShortenString(Title, 30) + ' - ' + ShortenString(Stream, 30);
        TrayIcon1.Hint := 'streamWriter'#13#10 + _('Playing:') + ' ' + Title;
      end else
      begin
        NewCaption := FMainCaption + ' - ' + ShortenString(Stream, 30);
        TrayIcon1.Hint := 'streamWriter'#13#10 + _('Playing:') + ' ' + Stream;
      end;
    end;

    Caption := NewCaption;
  end
  else if Msg is TRefreshServerDataMsg then
  begin
    if HomeComm.SendGetServerData then
    begin
      //tabCharts.SetState(csLoading);
      tabClients.SideBar.BrowserView.SwitchMode(moLoading);
    end;
  end else if Msg is TSelectSavedSongsMsg then
  begin
    pagMain.ActivePage := tabSaved;
  end;
end;

procedure TfrmStreamWriterMain.mnuCheckUpdateClick(Sender: TObject);
begin
  ShowUpdate;
end;

procedure TfrmStreamWriterMain.mnuMoveToCategory(Sender: TObject);
var
  Cats: TNodeArray;
  NodeData: PClientNodeData;
  Cat: PVirtualNode;
  Node: PVirtualNode;
  Item: TMenuItem;
  Nodes: TNodeArray;
begin
  Item := TMenuItem(Sender);

  Nodes := tabClients.ClientView.GetNodes(ntClientNoAuto, True);
  Cats := tabClients.ClientView.GetNodes(ntCategory, False);

  Cat := nil;

  for Node in Cats do
  begin
    NodeData := tabClients.ClientView.GetNodeData(Node);
    if Integer(NodeData) = Item.Tag then
    begin
      Cat := Node;
      Break;
    end;
  end;

  if (Length(Nodes) > 0) and (Cat <> nil) then
  begin
    for Node in Nodes do
      tabClients.ClientView.MoveTo(Node, Cat, amAddChildLast, False);
  end;
end;

procedure TfrmStreamWriterMain.mnuPlayerClick(Sender: TObject);
begin
  actPlayerPlayPause.Enabled := Players.AnyPlayingOrPaused;
  actPlayerStop.Enabled := Players.AnyPlayingOrPaused;
end;

procedure TfrmStreamWriterMain.mnuShowClick(Sender: TObject);
begin
  ToggleWindow(False);
end;

procedure TfrmStreamWriterMain.OneInstanceMessage(var Msg: TMessage);
begin
  if not FExiting then
    ToggleWindow(True);
end;

procedure TfrmStreamWriterMain.OpenCut(Filename: string);
var
  tabCut: TCutTab;
begin
  tabCut := TCutTab.Create(pagMain, nil, Filename);
  tabCut.PageControl := pagMain;
  tabCut.AfterCreate;
  tabCut.OnSaved := tabCutSaved;
  tabCut.OnPlayStarted := tabPlayStarted;

  pagMain.ActivePage := tabCut;

  tabCut.CutView.OnCutFile := tabCutCutFile;
end;

procedure TfrmStreamWriterMain.OpenCut(Track: TTrackInfo);
var
  tabCut: TCutTab;
begin
  tabCut := TCutTab.Create(pagMain, Track);
  tabCut.PageControl := pagMain;
  tabCut.AfterCreate;
  tabCut.OnSaved := tabCutSaved;
  tabCut.OnPlayStarted := tabPlayStarted;

  pagMain.ActivePage := tabCut;

  tabCut.CutView.OnCutFile := tabCutCutFile;
end;

procedure TfrmStreamWriterMain.pagSidebarChange(Sender: TObject);
begin
  // Damit Child-Controls passende Dimensionen in ShowInfo haben
  Application.ProcessMessages;
end;

procedure TfrmStreamWriterMain.PreTranslate;
begin

end;

procedure TfrmStreamWriterMain.ProcessCommandLine(Data: string);
var
  i, Prio: Integer;
  FreeCmdLine: Boolean;
  Param: TCommandLineRecord;
  CmdLine: TCommandLine;
  Titles: TWishlistTitleInfoArray;
begin
  FreeCmdLine := False;

  if Data <> '' then
  begin
    CmdLine := TCommandLine.Create(Data);
    FreeCmdLine := True;
  end else
    CmdLine := AppGlobals.CommandLine;

  Param := CmdLine.GetParam('-r');
  if Param <> nil then
  begin
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StartStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil), oaStart, nil, amNoWhere);
  end;

  Param := CmdLine.GetParam('-p');
  if Param <> nil then
  begin
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StartStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil), oaPlay, nil, amNoWhere);
  end;

  Param := CmdLine.GetParam('-wishadd');
  if Param <> nil then
  begin
    SetLength(Titles, Param.Values.Count);
    for i := 0 to Param.Values.Count - 1 do
    begin
      Titles[i].Hash := 0;
      Titles[i].Title := Param.Values[i];
      Titles[i].IsArtist := False;
    end;
    tabChartsAddToWishlist(nil, Titles);
  end;

  Param := CmdLine.GetParam('-wishremove');
  if Param <> nil then
  begin
    for i := 0 to Param.Values.Count - 1 do
    begin
      tabClientsRemoveTitleFromList(nil, nil, ltSave, Param.Values[i], 0);
    end;
  end;

  Param := CmdLine.GetParam('-priority');
  if Param <> nil then
  begin
    Prio := StrToIntDef(Param.Values[0], -1);
    case Prio of
      0: SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
      1: SetPriorityClass(GetCurrentProcess(), $00004000); // BELOW_...
      2: SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
      3: SetPriorityClass(GetCurrentProcess(), $00008000); // ABOVE_...
      4: SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
    end;
  end;

  if FreeCmdLine then
    CmdLine.Free;
end;

procedure TfrmStreamWriterMain.PostTranslate;
var
  NodeData: PClientNodeData;
begin
  tabClients.SideBar.BrowserView.Translate;
  tabClients.SideBar.InfoView.Translate;
  tabClients.ClientView.Translate;

  tabSaved.Tree.Translate;

  NodeData := tabClients.ClientView.GetNodeData(tabClients.ClientView.AutoNode);
  NodeData.Category.Name := _('Automatic recordings');
  tabClients.ClientView.Invalidate;

  tabLists.PostTranslate;
  tabCharts.PostTranslate;
  tabSaved.PostTranslate;

  addStatus.Invalidate;
end;

procedure TfrmStreamWriterMain.mnuStreamPopupPopup(Sender: TObject);
  function AllClientsInCat(Clients: TNodeArray; Cat: PVirtualNode): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to Length(Clients) - 1 do
      if Clients[i].Parent <> Cat then
      begin
        Result := False;
        Break;
      end;
  end;
var
  Cats: TNodeArray;
  Cat: PClientNodeData;
  Node: PVirtualNode;
  Item: TMenuItem;
  ClientNodes: TNodeArray;
  Clients: TClientArray;
begin
  UpdateButtons;

  ClientNodes := tabClients.ClientView.GetNodes(ntClientNoAuto, True);
  Clients := tabClients.ClientView.NodesToClients(ClientNodes);

  mnuMoveToCategory1.Clear;
  Cats := tabClients.ClientView.GetNodes(ntCategory, False);
  for Node in Cats do
  begin
    Cat := tabClients.ClientView.GetNodeData(Node);
    if (not Cat.Category.IsAuto) and (not AllClientsInCat(ClientNodes, Node)) then
    begin
      Item := mnuStreamPopup.CreateMenuItem;
      Item.Caption := Cat.Category.Name;
      Item.Tag := Integer(Cat);
      Item.OnClick := mnuMoveToCategory;
      mnuMoveToCategory1.Add(Item);
    end;
  end;

  mnuMoveToCategory1.Enabled := (Length(Clients) > 0) and (mnuMoveToCategory1.Count > 0);
end;

procedure TfrmStreamWriterMain.mnuStreamSettingsToolbarPopup(
  Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmStreamWriterMain.QueryEndSession(var Msg: TMessage);
begin
  inherited;
  Msg.Result := 1;
end;

procedure ShortCutToHotKey(HotKey: TShortCut; var Key : Word; var Modifiers: Uint);
var
  Shift: TShiftState;
begin
  ShortCutToKey(HotKey, Key, Shift);
  Modifiers := 0;
  if (ssShift in Shift) then
    Modifiers := Modifiers or MOD_SHIFT;
  if (ssAlt in Shift) then
    Modifiers := Modifiers or MOD_ALT;
  if (ssCtrl in Shift) then
    Modifiers := Modifiers or MOD_CONTROL;
end;

procedure TfrmStreamWriterMain.ReceivedData(var Msg: TWMCopyData);
begin
  ProcessCommandLine(PChar(Msg.CopyDataStruct.lpData));
end;

procedure TfrmStreamWriterMain.RegisterHotkeys(Reg: Boolean);
var
  K: Word;
  M: Cardinal;
begin
  UnregisterHotKey(Handle, 0);
  UnregisterHotKey(Handle, 1);
  UnregisterHotKey(Handle, 2);
  UnregisterHotKey(Handle, 3);
  UnregisterHotKey(Handle, 4);
  UnregisterHotKey(Handle, 5);
  UnregisterHotKey(Handle, 6);
  UnregisterHotKey(Handle, 7);

  if not Reg then
    Exit;

  if AppGlobals.ShortcutPlay > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutPlay, K, M);
    RegisterHotKey(Handle, 0, M, K);
  end;

  if AppGlobals.ShortcutPause > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutPause, K, M);
    RegisterHotKey(Handle, 1, M, K);
  end;

  if AppGlobals.ShortcutStop > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutStop, K, M);
    RegisterHotKey(Handle, 2, M, K);
  end;

  if AppGlobals.ShortcutNext > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutNext, K, M);
    RegisterHotKey(Handle, 3, M, K);
  end;

  if AppGlobals.ShortcutPrev > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutPrev, K, M);
    RegisterHotKey(Handle, 4, M, K);
  end;

  if AppGlobals.ShortcutVolUp > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutVolUp, K, M);
    RegisterHotKey(Handle, 5, M, K);
  end;

  if AppGlobals.ShortcutVolDown > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutVolDown, K, M);
    RegisterHotKey(Handle, 6, M, K);
  end;

  if AppGlobals.ShortcutMute > 0 then
  begin
    ShortCutToHotKey(AppGlobals.ShortcutMute, K, M);
    RegisterHotkey(Handle, 7, M, K);
  end;
end;

procedure TfrmStreamWriterMain.EndSession(var Msg: TMessage);
begin
  if WordBool(Msg.WParam) then
  begin
    Msg.Result := 1;
    ExitApp(True);
  end;
end;

procedure TfrmStreamWriterMain.SettingsSaveForExport(Sender: TObject);
begin
  // Ist hier, damit der Profilexport korrekt funktioniert
  // Erst Lists updaten, dann Streams! Muss so!
  tabSaved.Tree.UpdateList;
  tabLists.UpdateLists;
  tabClients.UpdateStreams(FDataLists);

  // TODO: "Normale" Optionen sollten hier aber auch gespeichert werden? Kompliziert! Letzte Änderungen fehlen evtl hier noch oder?
  //       Also, um es klarzustellen: Änderungen von DIESER SESSION, die NICHT in INI/REGISTRY sind, werden nicht exportiert, oder???
end;

procedure TfrmStreamWriterMain.SetWakeups;
var
  i, n: Integer;
begin
  for i := 0 to FClients.Count - 1 do
    for n := 0 to FClients[i].Entry.Schedules.Count - 1 do
      FClients[i].Entry.Schedules[n].SetWakeup;
end;

procedure TfrmStreamWriterMain.ShowCommunityLogin;
begin
  if FCommunityLogin <> nil then
  begin
    FCommunityLogin.SetFocus;
    Exit;
  end;

  FCommunityLogin := TfrmCommunityLogin.Create(Self);
  FCommunityLogin.OnClose := CommunityLoginClose;
  FCommunityLogin.Show;
end;

// TODO: könnte das AutoRecordSettings-action-ding nicht auch das hier aufrufen? ist ATM nicht so.
procedure TfrmStreamWriterMain.ShowSettings(SettingsType: TSettingsTypes; BrowseDir: Boolean);
var
  S: TfrmSettings;
  OldMonitorCount, NewMonitorCount: Cardinal;
begin
  RegisterHotkeys(False);

  if AppGlobals.SubmitStats and AppGlobals.MonitorMode then
    OldMonitorCount := AppGlobals.MonitorCount
  else
    OldMonitorCount := 0;

  S := TfrmSettings.Create(Self, SettingsType, FDataLists, nil, BrowseDir);
  try
    S.OnSaveForExport := SettingsSaveForExport;
    S.ShowModal;

    // TODO: der Titel des Settings-Fensters sollte sich anpassen je nach aktuellem typ

    if S.SaveSettings then
      case SettingsType of
        stApp:
          begin
            if S.ImportFilename <> '' then
            begin
              ExitApp(False, S.ImportFilename);
              Exit;
            end;

            if not AppGlobals.DisplayPlayedSong then
              Caption := FMainCaption;

            HomeComm.SendSetSettings((FDataLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);


            if AppGlobals.SubmitStats and AppGlobals.MonitorMode then
              NewMonitorCount := AppGlobals.MonitorCount
            else
              NewMonitorCount := 0;
            if NewMonitorCount <> OldMonitorCount then
            begin
              FClients.StopMonitors;
              HomeComm.SendGetMonitorStreams(NewMonitorCount);
            end;


            tabSaved.Tree.SetFileWatcher;

            Language.Translate(Self, PreTranslate, PostTranslate);

            tabClients.AdjustTextSizeDirtyHack;

            tabClients.ShowInfo;

            AppGlobals.PostProcessManager.ReInitPostProcessors;

            TrayIcon1.Visible := AppGlobals.Tray;

            ScreenSnap := AppGlobals.SnapMain;

            RegisterHotkeys(True);

            TLogger.SetFilename(AppGlobals.LogFile);
          end;
        stAuto:
          begin
            FDataLists.AutoRecordSettings.Assign(S.StreamSettings[0]);
          end;
      end;
  finally
    S.Free;
  end;
end;

procedure TfrmStreamWriterMain.ShowStartupMessages;
begin
  if not Bass.DeviceAvailable then
  begin
    TfrmMsgDlg.ShowMsg(Self, _('No sound devices could be detected so playback of streams and files will not be possible.'),
                       mtWarning, [mbOK], mbOK, 7);
  end;

  if not DirectoryExists(AppGlobals.Dir) then
  begin
    MsgBox(Handle, _('The folder for saved songs does not exist.'#13#10'Please select a folder now.'), _('Info'), MB_ICONINFORMATION);
    ShowSettings(stApp, True);
  end;

  if not DirectoryExists(AppGlobals.DirAuto) then
  begin
    MsgBox(Handle, _('The folder for automatically saved songs does not exist.'#13#10'Please select a folder now.'), _('Info'), MB_ICONINFORMATION);
    ShowSettings(stAuto, True);
  end;
end;

procedure TfrmStreamWriterMain.ShowUpdate(Version: string = '';
  UpdateURL: string = '');
var
  S: TfrmUpdate;
begin
  S := TfrmUpdate.Create(Self, Version, UpdateURL);
  S.ShowModal;
  if (S.Updated) and (S.Exit) then
  begin
    S.Free;
    if CanExitApp then
    begin
      FUpdateOnExit := True;
      ExitApp(False);
    end;
  end else if S.Updated then
  begin
    AppGlobals.InstallUpdateOnStart := True;
    mnuCheckUpdate.Enabled := False;
    S.Free;
  end;
end;

function TfrmStreamWriterMain.StartupMessagesNeeded: Boolean;
begin
  if not Bass.DeviceAvailable then
    Exit(True);

  if not DirectoryExists(AppGlobals.Dir) then
    Exit(True);

  // Das erste DirectoryExists() ist da, damit der Settings-Dialog nicht doppelt kommt.
  if DirectoryExists(AppGlobals.Dir) and (not DirectoryExists(AppGlobals.DirAuto)) then
    Exit(True);

  if not AppGlobals.FirstStartShown then
    Exit(True);

  Exit(False);
end;

procedure TfrmStreamWriterMain.SysCommand(var Msg: TWMSysCommand);
begin
  if Msg.CmdType = SC_MINIMIZE then
  begin
    FWasMaximized := WindowState = wsMaximized;

    if (AppGlobals.Tray) and (AppGlobals.TrayOnMinimize) then
    begin
      TrayIcon1.Visible := True;
      Hide;
      Exit;
    end;
  end;
  DefaultHandler(Msg);
end;

procedure TfrmStreamWriterMain.tabCutCutFile(Sender: TObject;
  Filename: string);
var
  tabCut: TCutTab;
  AudioType: TAudioTypes;
begin
  Application.ProcessMessages;

  AudioType := FiletypeToFormat(ExtractFileExt(Filename));

  case AppGlobals.AddonManager.CanEncode(AudioType) of
    ceNoAddon:
      begin
        MsgBox(Handle, _('This filetype is not supported by streamWriter.'), _('Info'), MB_ICONINFORMATION);
        Exit;
      end;
    ceAddonNeeded:
      begin
        if MsgBox(Handle, _('To cut the selected file the required encoder-addon needs to be installed. Do you want to download and install the required addon now?'), _('Question'), MB_ICONINFORMATION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
        begin
          if not AppGlobals.AddonManager.InstallEncoderFor(Self, AudioType) then
            Exit;
        end else
          Exit;
      end;
  end;

  tabCut := TCutTab(Sender);
  if tabCut <> nil then
  begin
    OpenCut(Filename);
  end else
  begin
    pagMain.ActivePage := tabCut;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsClientAdded(Sender: TObject);
var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;
  tabLists.AddClient(Client);
end;

procedure TfrmStreamWriterMain.tabClientsClientRemoved(Sender: TObject);
var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;
  tabLists.RemoveClient(Client);
end;

procedure TfrmStreamWriterMain.tabSavedAddTitleToWishlist(Sender: TObject;
  Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.AddEntry(Title, TitleHash, False, ltSave);
end;

procedure TfrmStreamWriterMain.tabSavedRemoveTitleFromWishlist(
  Sender: TObject; Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.RemoveEntry(Title, TitleHash, ltSave);
end;

procedure TfrmStreamWriterMain.tabSavedAddTitleToIgnorelist(Sender: TObject;
  Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.AddEntry(Title, TitleHash, False, ltIgnore);
end;

procedure TfrmStreamWriterMain.tabSavedCut(Entry: TStreamEntry;
  Track: TTrackInfo);
var
  tabCut: TCutTab;
  AudioType: TAudioTypes;
begin
  AudioType := FiletypeToFormat(ExtractFileExt(Track.Filename));
  if AppGlobals.AddonManager.CanEncode(AudioType) <> ceOkay then
  begin
    if MsgBox(Handle, _('To cut the selected file the required encoder-addon needs to be installed. Do you want to download and install the required addon now?'), _('Question'), MB_ICONINFORMATION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
    begin
      if not AppGlobals.AddonManager.InstallEncoderFor(Self, AudioType) then
        Exit;
    end else
      Exit;
  end;

  tabCut := TCutTab(pagMain.FindCut(Track.Filename));
  if tabCut = nil then
  begin
    OpenCut(Track);
  end else
  begin
    pagMain.ActivePage := tabCut;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsShowErrorMessage(Sender: TICEClient;
  Msg: TMayConnectResults; WasAuto, WasScheduled: Boolean);
var
  Txt: string;
begin
  Txt := '';

  if WasAuto then
    case Msg of
      crNoFreeSpace:
        Txt := _('Automatic recording will be stopped as long as available disk space is below the set limit.');
      crNoBandwidth:
        Exit;
    end
  else if WasScheduled then
    case Msg of
      crNoFreeSpace:
        Txt := Format(_('Scheduled recording of "%s" will not start because available disk space is below the set limit.'), [Sender.Entry.Name]);
      crNoBandwidth:
        Txt := Format(_('Scheduled recording of "%s" will not start because it would exceed the maximum available bandwidth.'), [Sender.Entry.Name]);
    end
  else
    case Msg of
      crNoFreeSpace:
        Txt := _('No connection will be established because available disk space is below the set limit.');
      crNoBandwidth:
        Txt := _('No connection will be established because it would exceed the maximum available bandwidth.');
    end;

  if Txt <> '' then
    TfrmMsgDlg.ShowMsg(Self, Txt, mtInformation, [mbOK], mbOK);
end;

procedure TfrmStreamWriterMain.tabSavedRefresh(Sender: TObject);
var
  i: Integer;
  Files: TList;
begin
  if FCheckFiles <> nil then
    Exit;

  Files := TList.Create;
  try
    for i := 0 to FDataLists.TrackList.Count - 1 do
      Files.Add(TFileEntry.Create(FDataLists.TrackList[i].Filename, FDataLists.TrackList[i].Filesize, eaNone));
    FCheckFiles := TCheckFilesThread.Create(Files);
    FCheckFiles.OnTerminate := CheckFilesTerminate;
    FCheckFiles.Resume;
  finally
    // Wird vom Thread erledigt. Unschön, aber...
    // Files.Free;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsTrackAdded(Entry: TStreamEntry;
  Track: TTrackInfo);
begin
  tabSaved.Tree.AddTrack(Track, True);
end;

procedure TfrmStreamWriterMain.tabClientsTrackRemoved(Entry: TStreamEntry;
  Track: TTrackInfo);
begin
  tabSaved.Tree.RemoveTrack(Track);
end;

procedure TfrmStreamWriterMain.tabChartsAddStreams(Sender: TObject;
  Info: TStartStreamingInfoArray; Action: TStreamOpenActions);
begin
  tabClients.StartStreaming(Info, Action, nil, amAddChildLast);
end;

procedure TfrmStreamWriterMain.tabChartsAddToWishlist(Sender: TObject;
  Arr: TWishlistTitleInfoArray);
var
  i, n, NumChars: Integer;
  Hash: Cardinal;
  Hashes: TSyncWishlistRecordArray;
  Found: Boolean;
  Pattern: string;
  T: TTitleInfo;
begin
  SetLength(Hashes, 0);

  for i := 0 to High(Arr) do
  begin
    Pattern := BuildPattern(Arr[i].Title, Hash, NumChars, True);
    Found := False;

    for n := 0 to FDataLists.SaveList.Count - 1 do
      if ((Arr[i].Hash = 0) and (FDataLists.SaveList[n].ServerHash = 0) and (FDataLists.SaveList[n].Hash = Hash)) or
         ((Arr[i].Hash > 0) and Arr[i].IsArtist and (FDataLists.SaveList[n].ServerArtistHash = Arr[i].Hash)) or
         ((Arr[i].Hash > 0) and (not Arr[i].IsArtist) and (FDataLists.SaveList[n].ServerHash = Arr[i].Hash)) then
      begin
        Found := True;
        Break;
      end;

    if not Found then
    begin
      if Arr[i].Hash > 0 then
      begin
        if Arr[i].IsArtist then
          T := TTitleInfo.Create(0, Arr[i].Hash, Arr[i].Title)
        else
          T := TTitleInfo.Create(Arr[i].Hash, 0, Arr[i].Title);
      end else
        T := TTitleInfo.Create(0, 0, Arr[i].Title);

      FDataLists.SaveList.Add(T);
      tabLists.AddTitle(nil, ltSave, T);

      if Arr[i].Hash > 0 then
      begin
        SetLength(Hashes, Length(Hashes) + 1);
        Hashes[High(Hashes)] := TSyncWishlistRecord.Create(Arr[i].Hash, Arr[i].IsArtist);
      end;
    end;
  end;

  HomeComm.SendSyncWishlist(swAdd, Hashes);
  HomeComm.SendSetSettings((FDataLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
  MsgBus.SendMessage(TListsChangedMsg.Create);
end;

function TfrmStreamWriterMain.tabChartsGetIsStreamOnListEvent(
  Sender: TObject; Stream: TStreamBrowserEntry): Boolean;
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Result := False;
  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClientNoAuto, False));
  for Client in Clients do
    if (Client.Entry.ID = Stream.ID) or (Client.Entry.Name = Stream.Name) then
      Exit(True);
end;

procedure TfrmStreamWriterMain.tabChartsRemoveFromWishlist(Sender: TObject;
  Arr: TWishlistTitleInfoArray);
var
  i, n: Integer;
  Hashes: TSyncWishlistRecordArray;
begin
  SetLength(Hashes, 0);

  for n := 0 to High(Arr) do
    for i := FDataLists.SaveList.Count - 1 downto 0 do
      if (FDataLists.SaveList[i].ServerHash > 0) and (Arr[n].Hash > 0) and (not Arr[n].IsArtist) and
         (FDataLists.SaveList[i].ServerHash = Arr[n].Hash) then
      begin
        tabLists.RemoveTitle(nil, ltSave, FDataLists.SaveList[i]);
        FDataLists.SaveList.Delete(i);

        SetLength(Hashes, Length(Hashes) + 1);
        Hashes[High(Hashes)] := TSyncWishlistRecord.Create(Arr[n].Hash, False);
      end;

  HomeComm.SendSyncWishlist(swRemove, Hashes);
  HomeComm.SendSetSettings((FDataLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
  MsgBus.SendMessage(TListsChangedMsg.Create);
end;

procedure TfrmStreamWriterMain.tabClientsAddTitleToList(Sender: TObject; Client: TICEClient;
  ListType: TListType; Title: string);
var
  i, NumChars: Integer;
  Hash: Cardinal;
  Found: Boolean;
  Pattern: string;
  T: TTitleInfo;
  List: TList<TTitleInfo>;
begin
  if Client = nil then
    if ListType = ltSave then
      List := FDataLists.SaveList
    else
      List := FDataLists.IgnoreList
  else
    if ListType = ltSave then
      List := Client.Entry.SaveList
    else
      List := Client.Entry.IgnoreList;

  Pattern := BuildPattern(Title, Hash, NumChars, True);
  if NumChars > 3 then
  begin
    Found := False;
    for i := 0 to List.Count - 1 do
      if (List[i].ServerHash = 0) and (List[i].Hash = Hash) then
      begin
        Found := True;
        Break;
      end;

    if not Found then
    begin
      T := TTitleInfo.Create(0, 0, Title);

      List.Add(T);
      tabLists.AddTitle(Client, ListType, T);

      HomeComm.SendSetSettings((FDataLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
    end;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsRemoveTitleFromList(
  Sender: TObject; Client: TICEClient; ListType: TListType; Title: string; ServerTitleHash: Cardinal);
var
  i: Integer;
  List: TList<TTitleInfo>;
  T: TTitleInfo;
begin
  if Client = nil then
    if ListType = ltSave then
      List := FDataLists.SaveList
    else
      List := FDataLists.IgnoreList
  else
    if ListType = ltSave then
      List := Client.Entry.SaveList
    else
      List := Client.Entry.IgnoreList;

  for i := List.Count - 1 downto 0 do
  begin
    if ((List[i].ServerHash > 0) and (List[i].ServerHash = ServerTitleHash)) or Like(Title, List[i].Pattern) then
    begin
      tabLists.RemoveTitle(Client, ListType, List[i]);

      T := List[i];
      List.Delete(i);
      T.Free;
    end;
  end;

  MsgBus.SendMessage(TListsChangedMsg.Create);

  HomeComm.SendSetSettings((FDataLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
end;

procedure TfrmStreamWriterMain.tabClientsAuthRequired(Sender: TObject);
begin
  if MsgBox(Handle, _('You need to be logged in to perform that action.'#13#10'Do you want to login now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
    ShowCommunityLogin;
end;

procedure TfrmStreamWriterMain.tabClientsUpdateButtons(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmStreamWriterMain.tabSavedTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
begin

end;

procedure TfrmStreamWriterMain.tabCutSaved(Sender: TObject; AudioInfo: TAudioFileInfo);
var
  i: Integer;
begin
  for i := 0 to FDataLists.TrackList.Count - 1 do
    if LowerCase(FDataLists.TrackList[i].Filename) = LowerCase(TCutTab(Sender).Filename) then
    begin
      FDataLists.TrackList[i].Filesize := AudioInfo.Filesize;
      FDataLists.TrackList[i].Length := Trunc(AudioInfo.Length);

      // Ist mal raus, damit das "geschnitten"-Symbol nur bei automatischen Aufnahmen kommt
      //FDataLists.TrackList[i].WasCut := True;

      FDataLists.TrackList[i].Finalized := True;

      FDataLists.TrackList[i].BitRate := AudioInfo.Bitrate;
      FDataLists.TrackList[i].VBR := AudioInfo.VBR;

      tabSaved.Tree.UpdateTrack(FDataLists.TrackList[i]);

      // Macht den Finalized-Button passig (Down/nicht Down)
      tabSaved.UpdateButtons;

      Exit;
    end;
end;

procedure TfrmStreamWriterMain.tabPlayStarted(Sender: TObject);
var
  i: Integer;
  Tab: TTabSheet;
begin
  for i := 0 to pagMain.PageCount - 1 do
  begin
    Tab := pagMain.Pages[i];
    if Tab <> Sender then
      if Tab is TSavedTab then
      begin
        TSavedTab(Tab).PausePlay;
      end else if Tab is TClientTab then
      begin
        TClientTab(Tab).PausePlay;
      end else if Tab is TCutTab then
      begin
        TCutTab(Tab).PausePlay;
      end;
  end;
end;

procedure TfrmStreamWriterMain.tmrAutoSaveTimer(Sender: TObject);
begin
  if Application.Terminated or AppGlobals.SkipSave or FDataLists.LoadError then
    Exit;

  try
    // Erst Lists updaten, dann Streams!
    tabSaved.Tree.UpdateList;
    tabLists.UpdateLists;
    tabClients.UpdateStreams(FDataLists);
    FDataLists.SaveRecover;

    try
      AppGlobals.Save(0);
    except
    end;
  except
    tmrAutoSave.Enabled := False;
  end;
end;

procedure TfrmStreamWriterMain.tmrRecordingsTimer(Sender: TObject);
var
  i: Integer;
  C: Cardinal;
  L: TList<Cardinal>;
begin
  if Application.Terminated then
    Exit;

  if not AppGlobals.SubmitStats then
    Exit;

  C := 0;

  L := TList<Cardinal>.Create;
  try
    for i := 0 to FClients.Count - 1 do
      if FClients[i].Recording or FClients[i].Playing and not FClients[i].AutoRemove then
      begin
        if FClients[i].Entry.ID > 0 then
          L.Add(FClients[i].Entry.ID)
        else
          Inc(C);
      end;

    for i := 0 to FClients.Monitors.Count - 1 do
      if FClients.Monitors[i].Entry.ID > 0 then
        L.Add(FClients.Monitors[i].Entry.ID);

    HomeComm.SendUpdateStats(L, C);
  finally
    L.Free;
  end;
end;

procedure TfrmStreamWriterMain.tmrScheduleTimer(Sender: TObject);
var
  i: Integer;
  Clients: TClientArray;
  Client: TICEClient;
  Schedule: TSchedule;
  Res: TMayConnectResults;
begin
  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClientNoAuto, False));
  for Client in Clients do
  begin
    for i := Client.Entry.Schedules.Count - 1 downto 0 do
    begin
      Schedule := Client.Entry.Schedules[i];
      if Schedule.Active then
      begin
        if Schedule.MatchesStart and (not Schedule.TriedStart) then
        begin
          // TODO: Übersetzen
          Client.WriteDebug(_('Starting scheduled recording'), dtSchedule, dlNormal);
          Client.WriteDebug(_('Scheduled recording ends at') + ' ' + DateTimeToStr(Schedule.GetEndTime(Schedule.GetStartTime)), dtSchedule, dlNormal);

          Schedule.TriedStart := True;
          Schedule.ScheduleStarted := Schedule.GetStartTime;
          Res := Client.StartRecording(True);
          if Res <> crOk then
            tabClientsShowErrorMessage(Client, Res, False, True);
        end else if not Schedule.MatchesStart then
          Schedule.TriedStart := False;

        if Schedule.MatchesEnd and (not Schedule.TriedStop) then
        begin
          Client.WriteDebug(_('Stopping scheduled recording'), dtSchedule, dlNormal);

          Client.StopRecording;
          Schedule.TriedStop := True;
          Schedule.ScheduleStarted := 0;
          if Schedule.AutoRemove then
          begin
            Client.Entry.Schedules.Remove(Schedule);

            tabClients.ClientView.RefreshClient(Client);

            Schedule.Free;
          end;

          SetWakeups;
        end else if not Schedule.MatchesEnd then
          Schedule.TriedStop := False;
      end;
    end;
  end;
end;

procedure TfrmStreamWriterMain.tmrSpeedTimer(Sender: TObject);
var
  RecordingActive: Boolean;
  PlayingActive: Boolean;
  ScheduleActive: Boolean;
  i: Integer;
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Client2: TICEClient;
  Speed: UInt64;
  OnlyAuto: Boolean;
begin
  Speed := 0;
  Clients := tabClients.ClientView.NodesToData(tabClients.ClientView.GetNodes(ntClient, False));
  for Client in Clients do
  begin
    Speed := Speed + Client.Client.Speed;
    tabClients.ClientView.RefreshClient(Client.Client);
  end;

  for Client2 in FClients.Monitors do
  begin
    Speed := Speed + Client2.Speed;
  end;


  addStatus.Speed := Speed;
  addStatus.BuildSpeedBmp;
  addStatus.CurrentReceived := tabClients.Received;
  addStatus.OverallReceived := FDataLists.Received;

  UpdateStatus;

  tabClients.TimerTick;

  RecordingActive := False;
  PlayingActive := False;
  ScheduleActive := False;
  for i := 0 to FClients.Count - 1 do
  begin
    if FClients[i].Recording then
    begin
      RecordingActive := True;
    end;
    if FClients[i].Playing then
    begin
      PlayingActive := True;
    end;
    if FClients[i].Entry.Schedules.Count > 0 then
    begin
      ScheduleActive := True;
    end;
  end;

  OnlyAuto := True;
  if RecordingActive and not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
  begin
    for i := 0 to FClients.Count - 1 do
      if FClients[i].Recording and (not FClients[i].AutoRemove) then
      begin
        OnlyAuto := False;
        Break;
      end;

    for i := 0 to FClients.Count - 1 do
      if not FClients[i].AutoRemove then
        FClients[i].StopRecording;

    if not OnlyAuto then
    begin
      tmrSpeed.Enabled := False;
      MsgBox(Handle, _('Available disk space is below the set limit, so recording will be stopped.'), _('Info'), MB_ICONINFORMATION);
      tmrSpeed.Enabled := True;
    end;
  end;

  Power.Critical := (PlayingActive or RecordingActive or ScheduleActive) or ((FDataLists.SaveList.Count > 0) and (AppGlobals.AutoTuneIn));
end;

procedure TfrmStreamWriterMain.ToggleWindow(AlwaysShow: Boolean);
begin
  if IsIconic(Handle) then
  begin
    if not Visible then
      Show;
    OpenIcon(Handle);

    if FWasMaximized then
      WindowState := wsMaximized;

    SetForegroundWindow(Handle);
  end else
  begin
    if not Visible then
    begin
      Show;
      SetForegroundWindow(Handle);
    end else
    begin
      if not AlwaysShow then
      begin
        FWasMaximized := WindowState = wsMaximized;
        CloseWindow(Handle);
        Hide;
      end else
        SetForegroundWindow(Handle);
    end;
  end;
end;

procedure TfrmStreamWriterMain.UpdateButtons;
var
  i: Integer;

  OneNormalRecordingWithTitle, OneNormalStopsAfterSong, AllNormalStopsAfterSong: Boolean;
  B, OnlyAutomatedSelected, OnlyAutomatedCatsSelected, OnlyAutomaticRecording: Boolean;
  URLFound, FilenameFound, OneRecording, OneNotRecording, OnePlaying, OnePaused: Boolean;
  OneHasTitle, ClientSchedulesActive: Boolean;
  Clients, AllClients: TClientArray;
  Client: TICEClient;
  CatNodes: TNodeArray;
begin
  // Enabled und so wird hier immer nur gesetzt, wenn sich der Status geändert hat.
  // Das hilft gut gegen flackern, wenn das Popup aufgeklappt ist, während das hier
  // aufgerufen wird. Vielleicht hilft auch nur, kein Default-Item mehr zu setzen.
  // Man weiß es nicht!

  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClient, True));
  AllClients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClient, False));
  CatNodes := tabClients.ClientView.GetNodes(ntCategory, True);

  OnlyAutomaticRecording := True;
  OneNormalRecordingWithTitle := False;
  AllNormalStopsAfterSong := True;
  FilenameFound := False;
  OnlyAutomatedSelected := True;
  OneRecording := False;
  OneNotRecording := False;
  OnePlaying := False;
  OnePaused := False;
  OnlyAutomatedCatsSelected := Length(CatNodes) > 0;
  OneHasTitle := False;

  for i := 0 to High(CatNodes) do
  begin
    if not PClientNodeData(tabClients.ClientView.GetNodeData(CatNodes[i])).Category.IsAuto then
    begin
      OnlyAutomatedCatsSelected := False;
      Break;
    end;
  end;

  for Client in Clients do
  begin
    if not Client.AutoRemove then
    begin
      OnlyAutomatedSelected := False;
      if Client.Recording then
        OnlyAutomaticRecording := False;
      if Client.Recording and (Client.Title <> '') then
      begin
        OneNormalRecordingWithTitle := True;
        if Client.StopAfterSong then
          OneNormalStopsAfterSong := True
        else
          AllNormalStopsAfterSong := False;
      end;
    end;
    if Client.Filename <> '' then
      FilenameFound := True;
    if Client.Title <> '' then
      OneHasTitle := True;
    if Client.Recording then
      OneRecording := True
    else
      OneNotRecording := True;
  end;

  for Client in AllClients do
  begin
    if Client.Playing then
      OnePlaying := True;
    if Client.Paused and Client.Playing then
      OnePaused := True;
  end;

  ClientSchedulesActive := False;
  if Length(Clients) = 1 then
    for i := 0 to Clients[0].Entry.Schedules.Count - 1 do
      if Clients[0].Entry.Schedules[i].ScheduleStarted > 0 then
      begin
        ClientSchedulesActive := True;
        Break;
      end;

  for i := 0 to Length(CatNodes) - 1 do
    if not PClientNodeData(tabClients.ClientView.GetNodeData(CatNodes[i])).Category.IsAuto then
    begin
      OnlyAutomatedSelected := False;
      Break;
    end;

  B := Length(Clients) > 0;
  if actStart.Enabled <> (B and OneNotRecording and not OnlyAutomatedSelected) or ((Length(CatNodes) > 0) and (not OnlyAutomatedCatsSelected)) then
    actStart.Enabled := (B and OneNotRecording and not OnlyAutomatedSelected) or ((Length(CatNodes) > 0) and (not OnlyAutomatedCatsSelected));
  if actStop.Enabled <> (B and OneRecording and (not OnlyAutomaticRecording) and not OnlyAutomatedSelected) or ((Length(CatNodes) > 0) and (not OnlyAutomatedCatsSelected)) then
    actStop.Enabled := (B and OneRecording and (not OnlyAutomaticRecording) and not OnlyAutomatedSelected) or ((Length(CatNodes) > 0) and (not OnlyAutomatedCatsSelected));
  mnuStartStreaming1.Default := False;
  mnuStopStreaming1.Default := False;

  if actRename.Enabled <> (tabClients.ClientView.SelectedCount = 1) and (not OnlyAutomatedSelected) and (not OnlyAutomatedCatsSelected) then
    actRename.Enabled := (tabClients.ClientView.SelectedCount = 1) and (not OnlyAutomatedSelected) and (not OnlyAutomatedCatsSelected);

  if actRemove.Enabled <> B or ((Length(CatNodes) > 0) and not OnlyAutomatedCatsSelected) then
    actRemove.Enabled := B or ((Length(CatNodes) > 0) and not OnlyAutomatedCatsSelected);

  if actStreamSettings.Enabled <> B and (not OnlyAutomatedSelected) then
    actStreamSettings.Enabled := B and (not OnlyAutomatedSelected);

  URLFound := False;
  if Length(Clients) > 0 then
    if Trim(Clients[0].Entry.StreamURL) <> '' then
      URLFound := True;
  if actOpenWebsite.Enabled <> URLFound then
    actOpenWebsite.Enabled := URLFound;

  if mnuTuneIn1.Enabled <> B then
    mnuTuneIn1.Enabled := B;
  if mnuTuneIn2.Enabled <> B then
    mnuTuneIn2.Enabled := B;

  if mnuSavePlaylist1.Enabled <> B then
    mnuSavePlaylist1.Enabled := B;
  if mnuSavePlaylist2.Enabled <> B then
    mnuSavePlaylist2.Enabled := B;

  if actResetData.Enabled <> ((Length(Clients) > 0) and not OnlyAutomatedSelected) then
    actResetData.Enabled := ((Length(Clients) > 0) and not OnlyAutomatedSelected);
  if actTuneInFile.Enabled <> FilenameFound then
    actTuneInFile.Enabled := FilenameFound;
  if actSavePlaylistFile.Enabled <> FilenameFound then
    actSavePlaylistFile.Enabled := FilenameFound;

  if actStopPlay.Enabled <> OnePlaying and Bass.DeviceAvailable then
    actStopPlay.Enabled := OnePlaying and Bass.DeviceAvailable;
  if actPause.Enabled <> OnePlaying and Bass.DeviceAvailable then
    actPause.Enabled := OnePlaying and Bass.DeviceAvailable;

  if actPlay.Enabled <> (Length(Clients) = 1) and (not (Clients[0].AutoRemove and (Clients[0].State <> csConnected))) and Bass.DeviceAvailable then
    actPlay.Enabled := (Length(Clients) = 1) and (not (Clients[0].AutoRemove and (Clients[0].State <> csConnected))) and Bass.DeviceAvailable;

  if actStopAfterSong.Enabled <> OneNormalRecordingWithTitle then
    actStopAfterSong.Enabled := OneNormalRecordingWithTitle;

  if actStopAfterSong.Checked <> OneNormalRecordingWithTitle and AllNormalStopsAfterSong then
    actStopAfterSong.Checked := OneNormalRecordingWithTitle and AllNormalStopsAfterSong;

  if actTimers.Enabled <> (Length(Clients) = 1) and (not Clients[0].AutoRemove) and (not ClientSchedulesActive) then
    actTimers.Enabled := (Length(Clients) = 1) and (not Clients[0].AutoRemove) and (not ClientSchedulesActive);

  if mnuCurrentTitle1.Enabled <> (Length(Clients) > 0) and OneHasTitle then
    mnuCurrentTitle1.Enabled := (Length(Clients) > 0) and OneHasTitle;

  cmdPause.Down := OnePaused;
  
  actCopyTitle.Enabled := (Length(Clients) > 0) and OneHasTitle;
end;

procedure TfrmStreamWriterMain.UpdateFound(var Msg: TMessage);
var
  Res: Integer;
begin
  Res := MsgBox(Handle, _('A new version was found.'#13#10'Do you want to download the update now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1);
  if Res = IDYES then
  begin
    if AppGlobals.RunningFromInstalledLocation then
      ShowUpdate(FUpdater.FoundVersion.AsString, FUpdater.UpdateURL)
    else
      ShellExecute(Handle, 'open', PChar(AppGlobals.ProjectLink), nil, nil, 1);
  end;
end;

procedure TfrmStreamWriterMain.UpdaterNoUpdateFound(Sender: TObject);
begin
  AppGlobals.LastUpdateChecked := Trunc(Now);
end;

procedure TfrmStreamWriterMain.UpdaterUpdateFound(Sender: TObject);
begin
  AppGlobals.LastUpdateChecked := Trunc(Now);

  // Hier war mal die MsgBox mit 'A new version was found...'
  // Das konnte aber zu Deadlocks führen, weil während allem was jetzt kam
  // (z.B. ShowUpdate() mit Modaler Form) der UpdateThread nicht nil gesetzt
  // wurde. Deshalb ist hier nun ein PostMessage.
  PostMessage(Handle, WM_UPDATEFOUND, 0, 0);
end;

procedure TfrmStreamWriterMain.UpdateStatus;
var
  CS: THomeConnectionState;
begin
  if HomeComm.Disabled then
    CS := cshFail
  else if HomeComm.Connected then
    CS := cshConnected
  else
    CS := cshDisconnected;

  addStatus.SetState(CS, HomeComm.Authenticated, HomeComm.NotifyTitleChanges, FClientCount, FRecordingCount,
    FClients.SongsSaved, FDataLists.SongsSaved);
end;

function TfrmStreamWriterMain.CanExitApp: Boolean;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Rec: Boolean;
begin
  Result := True;
  Rec := False;

  Clients := tabClients.ClientView.NodesToData(tabClients.ClientView.GetNodes(ntClient, False));

  for Client in Clients do
    if Client.Client.Recording then
    begin
      Rec := True;
      Break;
    end;

  if Rec then
  begin
    if TfrmMsgDlg.ShowMsg(Self, _('You are recording at least one stream at the moment. Exiting the application will abort streaming.'#13#10'Do you really want to quit?'),
                          mtConfirmation, mbYesNo, mbNo, 1) = mrNo then
    begin
      Result := False;
    end;
  end;
end;

procedure TfrmStreamWriterMain.CheckFilesTerminate(Sender: TObject);
var
  i, n: Integer;
  Track: TTrackInfo;
  E: TFileEntry;
begin
  tabSaved.Tree.BeginUpdate;
  try
    for i := 0 to FCheckFiles.Files.Count - 1 do
    begin
      E := TFileEntry(FCheckFiles.Files[i]);

      if E.Action = eaNone then
        Continue;

      for n := 0 to FDataLists.TrackList.Count - 1 do
        if FDataLists.TrackList[n].Filename = E.Filename then
        begin
          Track := FDataLists.TrackList[n];
          case E.Action of
            eaNone: ;
            eaSize:
              Track.Filesize := E.Size;
            eaRemove:
              begin
                FDataLists.TrackList.Delete(n);
                tabSaved.Tree.RemoveTrack(Track);
                Track.Free;
              end;
          end;
          Break;
        end;
    end;
  finally
    tabSaved.Tree.EndUpdate;
  end;

  FCheckFiles := nil;
end;

procedure TfrmStreamWriterMain.Community1Click(Sender: TObject);
begin
  actLogOn.Enabled := not HomeComm.Authenticated and HomeComm.Connected;
  actLogOff.Enabled := HomeComm.Authenticated and HomeComm.Connected;
end;

procedure TfrmStreamWriterMain.CommunityLoginClose(Sender: TObject; var Action: TCloseAction);
begin
  FCommunityLogin := nil;
end;

constructor TfrmStreamWriterMain.Create(AOwner: TComponent);
begin
  inherited;

  if Screen.PixelsPerInch <> 96 then
  begin
    mnuMain.Images := nil;
    mnuStreamPopup.Images := nil;
    mnuTray.Images := nil;
  end;
end;

{ TStatusHint }

constructor TStatusHint.Create(AOwner: TComponent);
begin
  inherited;

  Delay := 500;
end;

procedure TStatusHint.PaintHint(HintWindow: TCustomHintWindow);
begin

  inherited;
end;

procedure TStatusHint.SetHintSize(HintWindow: TCustomHintWindow);
var
  Pos: TPoint;
  R: TRect;
begin
  inherited;

  Pos := Mouse.CursorPos;
  Pos := TfrmStreamWriterMain(Owner).addStatus.ScreenToClient(Pos);
  TfrmStreamWriterMain(Owner).addStatus.Perform(SB_GETRECT, 1, Integer(@R));
  R.Top := 0;

  if not PtInRect(R, Pos) then
  begin
    HintWindow.Width := 0;
    HintWindow.Height := 0;
  end;
end;

end.

