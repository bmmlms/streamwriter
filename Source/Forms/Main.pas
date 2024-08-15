{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2024 Alexander Nottelmann

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
  About,
  ActnList,
  AddonBase,
  AddonManager,
  AppDataBase,
  AppMessages,
  AudioFunctions,
  Buttons,
  ChartsTab,
  CheckFilesThread,
  Classes,
  ClientManager,
  ClientTab,
  ClientView,
  Clipbrd,
  ComCtrls,
  CommandLine,
  Commands,
  CommCtrl,
  CommunityLogin,
  Controls,
  CutTab,
  DataManager,
  Dialogs,
  DragDrop,
  DragDropFile,
  DragDropInternet,
  DragDropText,
  DynBass,
  Equalizer,
  ExtCtrls,
  Forms,
  Functions,
  Generics.Collections,
  Graphics,
  HomeCommands,
  HomeCommunication,
  ICEClient,
  ImgList,
  Intro,
  LanguageObjects,
  ListsTab,
  Logging,
  LogTab,
  MControlFocuser,
  MControls,
  Menus,
  MessageBus,
  MPageControl,
  MsgDlg,
  MToolbarForcedHorizontal,
  Notifications,
  PlayerManager,
  PowerManagement,
  Protocol,
  SavedTab,
  SetStreamData,
  Settings,
  SettingsStorage,
  SharedData,
  Sockets,
  StationCombo,
  StatusBar,
  StreamBrowserView,
  StreamHelper,
  SysUtils,
  Tabs,
  Timers,
  TypeDefs,
  Update,
  UpdateClient,
  Variants,
  VirtualTrees,
  Windows;

const
  WM_UPDATEFOUND = WM_USER + 628;
  WM_AFTERSHOWN = WM_USER + 678;

type
  TWMHotKey = packed record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    HotKey: WPARAM;
    Unused: LPARAM;
    Result: LRESULT;
  end;

  { TfrmStreamWriterMain }

  TfrmStreamWriterMain = class(TForm)
    actCheckUpdate: TAction;
    actPlay: TAction;
    actPause: TAction;
    actStopPlay: TAction;
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
    addTrayIcon: TTrayIcon;
    mnuTray: TPopupMenu;
    mnuShow: TMenuItem;
    N2: TMenuItem;
    Beenden1: TMenuItem;
    mnuTuneIn1: TMenuItem;
    N4: TMenuItem;
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
    mnuListenToStream2: TMenuItem;
    actSavePlaylistStream: TAction;
    actSavePlaylistStream1: TMenuItem;
    mnuReset1: TMenuItem;
    mnuReset11: TMenuItem;
    actResetData: TAction;
    tbClients: TMToolbarForcedHorizontal;
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
    mnuHelp2: TMenuItem;
    N1: TMenuItem;
    actHelp: TAction;
    mnuStartPlay2: TMenuItem;
    mnuStopPlay2: TMenuItem;
    N10: TMenuItem;
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
    N15: TMenuItem;
    Settingsforautomaticrecordings1: TMenuItem;
    actAutoSettings: TAction;
    N16: TMenuItem;
    ToolButton10: TToolButton;
    cmdAddToSaveList: TToolButton;
    cmdAddToGlobalIgnoreList: TToolButton;
    cmdAddToStreamIgnoreList: TToolButton;
    cmdTuneInStream: TToolButton;
    cmdSavePlaylistStream: TToolButton;
    N11: TMenuItem;
    Stoprecordingaftercurrenttitle1: TMenuItem;
    Rename1: TMenuItem;
    mnuCurrentTitle2: TMenuItem;
    Addtostreamignorelist2: TMenuItem;
    Addtoglobalignorelist2: TMenuItem;
    Addtomanualwishlist1: TMenuItem;
    N17: TMenuItem;
    Copytitletoclipboard2: TMenuItem;
    mnuMoveToCategory2: TMenuItem;
    Setupscheduledrecordings1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrSpeedTimer(Sender: TObject);
    procedure actStreamSettingsExecute(Sender: TObject);
    procedure addTrayClick(Sender: TObject);
    procedure actCheckUpdateExecute(Sender: TObject);
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actHelpExecute(Sender: TObject);
    procedure tmrRecordingsTimer(Sender: TObject);
    procedure actLogOnExecute(Sender: TObject);
    procedure Community1Click(Sender: TObject);
    procedure actLogOffExecute(Sender: TObject);
    procedure actTimersExecute(Sender: TObject);
    procedure tmrAutoSaveTimer(Sender: TObject);
    procedure actPlayerDecreaseVolumeExecute(Sender: TObject);
    procedure actPlayerIncreaseVolumeExecute(Sender: TObject);
    procedure actPlayerStopExecute(Sender: TObject);
    procedure actPlayerPlayPauseExecute(Sender: TObject);
    procedure actPlayerMuteVolumeExecute(Sender: TObject);
    procedure mnuPlayerClick(Sender: TObject);
    procedure actEqualizerExecute(Sender: TObject);
    procedure mnuStreamsClick(Sender: TObject);
    procedure FormShortcut(var Msg: TWMKey; var Handled: Boolean);
  private
    FPrevWndProc: Windows.WNDPROC;

    FCommunityLogin: TfrmCommunityLogin;

    FUpdater: TUpdateClient;
    FUpdateOnExit: Boolean;
    FSkipAfterShown: Boolean;

    FSpeed: Cardinal;
    FClientCount: Cardinal;
    FRecordingCount: Cardinal;
    FDiskSpaceFailCount: Cardinal;

    FWasShown: Boolean;
    FWasMaximized: Boolean;

    FCheckFiles: TCheckFilesThread;
    FClientManager: TClientManager;
    pagMain: TMainPageControl;
    tabClients: TClientTab;
    tabCharts: TChartsTab;
    tabLists: TListsTab;
    tabSaved: TSavedTab;
    tabLog: TLogTab;
    addStatus: TSWStatusBar;

    FEqualizer: TfrmEqualizer;

    FExiting: Boolean;

    class function CustomWndProcWrapper(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; static;

    function CustomWndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

    procedure AfterShown(var Msg: TMessage); message WM_AFTERSHOWN;
    procedure SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure Hotkey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure UpdateFound(var Msg: TMessage); message WM_UPDATEFOUND;
    procedure SetupExitMessage(var Msg: TMessage); message 5432;
    procedure EndSession(Sender: TObject);
    function CanExitApp: Boolean;
    procedure ExitApp(Shutdown: Boolean; ImportFilename: string = '');
    procedure ShowSettings(SettingsType: TSettingsTypes; BrowseDir: Boolean);
    procedure ShowUpdate(Version: string = ''; UpdateURL: string = '');
    procedure UpdateButtons;
    procedure UpdateStatus;
    procedure ToggleWindow(AlwaysShow: Boolean = False);
    procedure UpdaterUpdateFound(Sender: TObject);
    procedure UpdaterNoUpdateFound(Sender: TObject);
    procedure CheckFilesTerminate(Sender: TObject);
    procedure UnregisterHotkeys;
    procedure RegisterHotkeys;
    procedure ShowCommunityLogin;
    procedure OpenCut(Filename: string); overload;
    procedure OpenCut(Track: TTrackInfo); overload;
    procedure ProcessCommandLine(const CmdLine: TCommandLine);
    procedure SetCaptionAndTrayHint;
    procedure BuildMoveToCategoryMenu;
    procedure SetFormDimensions;

    function StartupMessagesNeeded: Boolean;
    procedure ShowStartupMessages;
    procedure PrepareSave;

    procedure CommunityLoginClose(Sender: TObject; var Action: TCloseAction);

    procedure HomeCommStateChanged(Sender: TObject);
    procedure HomeCommBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader; Transferred: Cardinal);
    procedure HomeCommTitleNotificationsChanged(Sender: TObject);
    procedure HomeCommHandshake(Sender: TObject; Success: Boolean);
    procedure HomeCommLogIn(Sender: TObject; Success: Boolean);
    procedure HomeCommLogOut(Sender: TObject);
    procedure HomeCommServerInfo(Sender: TObject; ClientCount, RecordingCount: Cardinal);
    procedure HomeCommError(Sender: TObject; ID: TCommErrors; Msg: string);
    procedure HomeCommException(Sender: TObject);

    procedure tabClientsUpdateButtons(Sender: TObject);
    procedure tabClientsTrackAdded(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabClientsAddTitleToList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string);
    procedure tabClientsRemoveTitleFromList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string; ServerTitleHash: Cardinal);
    procedure tabClientsAuthRequired(Sender: TObject);
    procedure tabClientsShowErrorMessage(Sender: TObject; Data: string);
    procedure tabClientsClientAdded(Sender: TObject);
    procedure tabClientsClientRemoved(Sender: TObject);
    procedure tabClientsBrowserViewStreamsReceived(Sender: TObject);
    procedure tabClientsSetStreamData(Sender: TObject; StreamID: Integer);

    procedure tabSavedCut(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabSavedRefresh(Sender: TObject);
    procedure tabSavedAddTitleToWishlist(Sender: TObject; Title: string; TitleHash: Cardinal);
    procedure tabSavedRemoveTitleFromWishlist(Sender: TObject; Title: string; TitleHash: Cardinal);
    procedure tabSavedAddTitleToIgnorelist(Sender: TObject; Title: string; TitleHash: Cardinal);
    procedure tabSavedRemoveTitleFromIgnorelist(Sender: TObject; Title: string; TitleHash: Cardinal);

    procedure tabCutCutFile(Sender: TObject; Filename: string);
    procedure tabCutSaved(Sender: TObject; AudioInfo: TAudioInfo);

    procedure tabPlayStarted(Sender: TObject);

    procedure tabChartsAddToWishlist(Sender: TObject; Arr: TWishlistTitleInfoArray);
    procedure tabChartsRemoveFromWishlist(Sender: TObject; Arr: TWishlistTitleInfoArray);
    procedure tabChartsAddStreams(Sender: TObject; Info: TStartStreamingInfoArray; Action: TStreamOpenActions);
    function tabChartsGetIsStreamOnListEvent(Sender: TObject; Stream: TStreamBrowserEntry): Boolean;

    procedure mnuMoveToCategory(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);

    procedure SettingsSaveForExport(Sender: TObject);
  protected
    procedure UpdateActions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  AppData;

{$R *.lfm}

procedure TfrmStreamWriterMain.ExitApp(Shutdown: Boolean; ImportFilename: string);
var
  i: Integer;
  Res: Integer;
  Version: Cardinal;
  StartTime, HardTimeout: UInt64;
  S: TMemoryStream;
  Lst: TSettingsList;
begin
  if FExiting then
    Exit;

  if not Shutdown then
    for i := 0 to pagMain.PageCount - 1 do
      if not TMTabSheet(pagMain.Pages[i]).CanClose then
        Exit;

  FExiting := True;
  Hide;

  CloseHandle(AppGlobals.MutexHandleExiting);

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

  for i := 0 to tabClients.ClientView.Header.Columns.Count - 1 do
  begin
    AppGlobals.ClientHeaderWidth[i] := tabClients.ClientView.Header.Columns[i].Width;
    AppGlobals.ClientHeaderPosition[i] := tabClients.ClientView.Header.Columns[i].Position;
  end;

  for i := 0 to tabCharts.ChartsTree.Header.Columns.Count - 1 do
  begin
    AppGlobals.ChartHeaderWidth[i] := tabCharts.ChartsTree.Header.Columns[i].Width;
    AppGlobals.ChartHeaderPosition[i] := tabCharts.ChartsTree.Header.Columns[i].Position;
  end;

  {
  for i := 0 to tabLists.ListsPanel.Tree.Header.Columns.Count - 1 do
  begin
    AppGlobals.ListHeaderWidth[i] := tabLists.ListsPanel.Tree.Header.Columns[i].Width;
    AppGlobals.ListHeaderPosition[i] := tabLists.ListsPanel.Tree.Header.Columns[i].Position;
  end;
  }

  for i := 0 to tabSaved.Tree.Header.Columns.Count - 1 do
  begin
    AppGlobals.SavedHeaderWidth[i] := tabSaved.Tree.Header.Columns[i].Width;
    AppGlobals.SavedHeaderPosition[i] := tabSaved.Tree.Header.Columns[i].Position;
  end;

  for i := 0 to tabLog.LogTree.Header.Columns.Count - 1 do
  begin
    AppGlobals.LogHeaderWidth[i] := tabLog.LogTree.Header.Columns[i].Width;
    AppGlobals.LogHeaderPosition[i] := tabLog.LogTree.Header.Columns[i].Position;
  end;

  try
    // Es ist mir beim Theme-Wechsel passiert, dass der Tray komplett verschwunden ist.
    // Beim Beenden von sW gab es dann eine Exception. Das hier sollte helfen ;-) ...
    addTrayIcon.Visible := False;
  except
  end;

  tmrSpeed.Enabled := False;
  tmrAutoSave.Enabled := False;
  tmrRecordings.Enabled := False;

  TfrmNotification.Hide;

  FEqualizer.Hide;

  Players.StopAll;

  MsgBus.RemoveSubscriber(MessageReceived);

  AppGlobals.PlayerVolume := Players.Volume;
  AppGlobals.PlayerVolumeBeforeMute := Players.VolumeBeforeMute;

  HomeComm.Terminate;

  tabSaved.StopThreads;

  FUpdater.Kill;

  AppGlobals.LastUsedDataVersion := DataManager.DATAVERSION;
  if not Shutdown then
    AppGlobals.Save(Handle)
  else
    try
      AppGlobals.Save;
    except
    end;

  // Shutdown all postprocessors. Since they get terminated, all postprocessing chains get terminated.
  AppGlobals.PostProcessManager.Terminate;
  // Disconnect all clients. They will save their recordings and will not postprocess any file.
  FClientManager.Stop;

  if FCheckFiles <> nil then
    FCheckFiles.Terminate;

  if Shutdown then
    HardTimeout := 2000
  else
    HardTimeout := 10000;

  StartTime := GetTickCount64;
  while (HomeComm.ThreadAlive or HomeComm.Connected) or FClientManager.Active or (FCheckFiles <> nil) or FUpdater.Active do
  begin
    // Wait for threads to finish
    if StartTime < GetTickCount64 - HardTimeout then
      Break;
    Sleep(100);
    Application.ProcessMessages;
  end;

  PrepareSave;

  while True do
    try
      AppGlobals.Data.Save(AppGlobals.DataFile, False);
      Break;
    except
      if not Shutdown then
      begin
        Res := TFunctions.MsgBox(Format(_('An error occured while saving the data file. Please make sure you can write to "%s" and that the file is not in use by another application. Click "Yes" to try again, "No" to exit without saving data.'),
          [AppGlobals.DataFile]), _('Info'), MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON1);
        if Res = IDNO then
          Break;
      end else
        Break;
    end;

  // Remove all clients from list
  FClientManager.Terminate;

  tabClients.ClientView.Clear;

  if FUpdateOnExit and (not Shutdown) then
    FUpdater.RunUpdate(Handle);

  if ImportFilename <> '' then
  begin
    try
      S := TMemoryStream.Create;
      try
        S.LoadFromFile(ImportFilename);

        // Remark: Ich limitiere hier die maximal-Version auf 10. Das liegt daran, dass es manchmal vorkommt,
        //         dass Menschen die Einstellungsdatei importieren. Ich kann dann eine passende Fehlermeldung ausgeben.
        //         Weil es dreckig so gelöst ist, stehen bei neuen exportierten Dateien/gespeicherten Einstellungsdateien
        //         in Zukunft immer "magische Bytes" am Anfang. Wenn es diese neuen Dateiversionen lange genug gibt,
        //         dann kann ich den Versionshack hier auslassen und nur noch die Bytes auswerten.
        //         Mögliche Fälle:
        //          - User importiert alte normale Einstellungsdatei, Version ist > 10, keine EXPORTMAGIC, Exception
        //          - User importiert alte exportierte Datei, Version ist 1, alles cool
        //          - User importiert neue normale Einstellungsdatei (mit Magic), Version ist > 10 durch Magic, keine EXPORTMAGIC, Exception
        //          - User importiert neue exportierte Datei, Version ist > 10 durch Magic, EXPORTMAGIC gefunden, alles cool
        TDataLists.VerifyMagic(S, 10, False);

        S.Read(Version, False);
        Lst := TSettingsList.Load(S);
        try
          AppGlobals.Storage.Assign(Lst);
          // Hier wird das eben gespeicherte neu geladen, damit das anschließende
          // verarbeiten der Datendatei (AppGlobals.Data.Load()) Zugriff auf
          // LastUsedVersion aus den Registry-/Ini-Einstellungen hat.
          AppGlobals.Load;
          AppGlobals.Data.Load(S, ImportFilename);
          AppGlobals.Data.Save(AppGlobals.DataFile, True);
        finally
          Lst.Free;
        end;
      finally
        S.Free;
      end;
    except
      on E: EFOpenError do
        TFunctions.MsgBox(_('The file could not be imported because it could not be opened for reading.'), _('Error'), MB_ICONERROR);
      on E: EVersionException do
        TFunctions.MsgBox(_('The file could not be imported because it was exported with a newer version of streamWriter.'), _('Error'), MB_ICONERROR);
      on E: EUnsupportedFormatException do
        TFunctions.MsgBox(_('The file could not be imported because it contains regular saved data and no exported profile.'), _('Error'), MB_ICONERROR);
      on E: EUnknownFormatException do
        TFunctions.MsgBox(_('The file could not be imported because it''s format is unknown.'), _('Error'), MB_ICONERROR);
      else
        TFunctions.MsgBox(_('The file could not be imported.'), _('Error'), MB_ICONERROR)
    end;

    TFunctions.RunProcess('"' + Application.ExeName + '" /profileupdate', False);
  end;

  // We have to close all cut-tabs here because otherwise FreeAndNil(FPlayer) in CutTab.Free()
  // throws an exception which causes that the temporary WAVE-file cannot be deleted...
  for i := pagMain.PageCount - 1 downto 0 do
    if pagMain.Pages[i].ClassType = TCutTab then
      pagMain.Pages[i].Free;

  TFunctions.ShutdownBlockReasonDestroy(Handle);

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
    FEqualizer.Left := Left + Scale96ToFont(15);
    FEqualizer.Top := Top + Height - addStatus.Height - FEqualizer.Height - Scale96ToFont(5);
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
  TFunctions.ShellExecute(Handle, 'open', AppGlobals.ProjectHelpLinkMain);
end;

procedure TfrmStreamWriterMain.actPlayerIncreaseVolumeExecute(Sender: TObject);
begin
  Players.IncreaseVolume;
end;

procedure TfrmStreamWriterMain.actPlayerMuteVolumeExecute(Sender: TObject);
begin
  if Players.Volume > 0 then
    Players.Volume := 0
  else
    Players.Volume := Players.VolumeBeforeMute;
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

procedure TfrmStreamWriterMain.actPlayerDecreaseVolumeExecute(Sender: TObject);
begin
  Players.DecreaseVolume;
end;

procedure TfrmStreamWriterMain.actLogOffExecute(Sender: TObject);
begin
  AppGlobals.Pass := '';

  HomeComm.SendLogOut;
end;

procedure TfrmStreamWriterMain.actStreamSettingsExecute(Sender: TObject);
begin
  ShowSettings(stStream, False);
end;

procedure TfrmStreamWriterMain.actTimersExecute(Sender: TObject);
var
  i: Integer;
  Clients: TClientArray;
  T: TfrmTimers;
begin
  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClientNoAuto, True));

  if (Length(Clients) <> 1) or ((Length(Clients) = 1) and (Clients[0].AutoRemove)) then
    Exit;

  T := TfrmTimers.Create(Self, Clients[0].Entry);
  try
    T.ShowModal;

    if T.Okay then
    begin
      FClientManager.Scheduler.SetSchedules(nil);

      Clients[0].Entry.Schedules.Clear;
      for i := 0 to T.Entry.Schedules.Count - 1 do
        Clients[0].Entry.Schedules.Add(T.Entry.Schedules[i].Copy);

      if Clients[0].ScheduledRecording and Clients[0].Recording then
        Clients[0].ScheduledRecording := Clients[0].IsCurrentTimeInSchedule;

      FClientManager.RefreshScheduler;
    end;
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
  Addon: TAddonBase;
  FormIntro: TfrmIntro;
  AddonError: Boolean = False;
begin
  if FSkipAfterShown then
    Exit;

  ShowStartupMessages;

  // If a profile import is active from a call to ShowSettings() in ShowStartupMessages,
  // we need to skip this stuff.
  if not FExiting then
  begin
    // Remark: Der Vergleich hier nach dem "and" kann irgendwann raus. Wenn Version > 5.0.0.1 lange genug
    //         veröffentlicht ist, rausmachen.
    if (not AppGlobals.IntroShown) and (AppGlobals.LastUsedDataVersion <= 39) then
    begin
      FormIntro := TfrmIntro.Create(Self);
      try
        FormIntro.ShowModal;
      finally
        FormIntro.Free;
      end;
      AppGlobals.FirstStartShown := True;
      AppGlobals.IntroShown := True;
    end else
      // Remark: Das kann irgendwann raus. Genau dann, wenn der dumme Vergleich hier drüber auch rausfliegt.
      AppGlobals.IntroShown := True;

    if StartupMessagesNeeded then
      if (AppGlobals.AutoUpdate) and (AppGlobals.LastUpdateChecked + 1 < Now) then
        FUpdater.Start(uaVersion, True);

    for Addon in AppGlobals.Data.GetMissingAddons do
      if Assigned(Addon) and not AppGlobals.AddonManager.EnableAddon(Self, Addon, False) then
        AddonError := True;

    if AddonError then
      TFunctions.MsgBox(_('At least one addon could not be downloaded or initialized, encoders or postprocessors requiring any of these missing addons will not work until the required addons have been installed.'), _('Error'), MB_ICONEXCLAMATION);
  end;
end;

procedure TfrmStreamWriterMain.BuildMoveToCategoryMenu;

  function AllClientsInCat(Clients: TNodeArray; Cat: PVirtualNode): Boolean;
  var
    Node: PVirtualNode;
  begin
    Result := True;
    for Node in Clients do
      if Node.Parent <> Cat then
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
  mnuMoveToCategory2.Clear;
  Cats := tabClients.ClientView.GetNodes(ntCategory, False);
  for Node in Cats do
  begin
    Cat := tabClients.ClientView.GetNodeData(Node);
    if (not Cat.Category.IsAuto) and (not AllClientsInCat(ClientNodes, Node)) then
    begin
      Item := TMenuItem.Create(mnuMain);
      Item.Caption := Cat.Category.Name;
      Item.Tag := PtrInt(Cat);
      Item.OnClick := mnuMoveToCategory;
      mnuMoveToCategory1.Add(Item);

      Item := TMenuItem.Create(mnuMain);
      Item.Caption := Cat.Category.Name;
      Item.Tag := PtrInt(Cat);
      Item.OnClick := mnuMoveToCategory;
      mnuMoveToCategory2.Add(Item);
    end;
  end;

  mnuMoveToCategory1.Enabled := (Length(Clients) > 0) and (mnuMoveToCategory1.Count > 0);
  mnuMoveToCategory2.Enabled := (Length(Clients) > 0) and (mnuMoveToCategory2.Count > 0);
end;

procedure TfrmStreamWriterMain.SetFormDimensions;

  procedure ResetForm(const W, H: Integer);
  begin
    Width := Scale96ToFont(Width);
    Height := Scale96ToFont(Height);
    Left := Screen.PrimaryMonitor.WorkareaRect.Width div 2 - Width div 2;
    Top := Screen.PrimaryMonitor.WorkareaRect.Height div 2 - Height div 2;
  end;

var
  F: Boolean = False;
  i, DefW, DefH: Integer;
  R: TRect;
begin
  DefW := Scale96ToFont(Width);
  DefH := Scale96ToFont(Height);

  if (AppGlobals.MainWidth >= Constraints.MinWidth) and (AppGlobals.MainHeight >= Constraints.MinHeight) and
     (AppGlobals.MainWidth <= Screen.WorkAreaWidth) and (AppGlobals.MainHeight <= Screen.WorkAreaHeight) and
     (AppGlobals.MainLeft <> -1) and (AppGlobals.MainTop <> -1) then
  begin
    Width := AppGlobals.MainWidth;
    Height := AppGlobals.MainHeight;
    Left := AppGlobals.MainLeft;
    Top := AppGlobals.MainTop;

    // Wenn Fenster nicht auf Bildschirmen, Position zurücksetzen
    R := Classes.Rect(Left + 20, Top + 20, Left + Width - 40, Top + Height - 40);
    for i := 0 to Screen.MonitorCount - 1 do
      if Screen.Monitors[i].WorkareaRect.IntersectsWith(R) then
      begin
        F := True;
        Break;
      end;

    if not F then
    begin
      AppGlobals.MainMaximized := False;
      ResetForm(DefW, DefH);
    end;
  end else
  begin
    AppGlobals.MainMaximized := False;
    ResetForm(DefW, DefH);
  end;

  // Ist nun hier, damit man nicht sieht, wie sich alle Controls resizen.
  if AppGlobals.MainMaximized then
    WindowState := wsMaximized;
end;

procedure TfrmStreamWriterMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  if (AppGlobals.Tray) and (not AppGlobals.TrayOnMinimize) then
  begin
    if (Visible) or (IsIconic(Handle)) then
    begin
      addTrayIcon.Visible := True;
      FWasMaximized := WindowState = wsMaximized;
      Hide;
    end;
  end else if CanExitApp then
    ExitApp(False);
end;

procedure TfrmStreamWriterMain.FormCreate(Sender: TObject);
begin
  SetCaptionAndTrayHint;

  AppGlobals.WindowHandle := Handle;

  if not Bass.EffectsAvailable then
  begin
    Players.EQEnabled := False;
    AppGlobals.EQEnabled := False;
  end;

  HomeComm := THomeCommunication.Create;

  addStatus := TSWStatusBar.Create(Self);
  addStatus.Parent := Self;

  FClientManager := TClientManager.Create;

  pagMain := TMainPageControl.Create(Self);
  pagMain.Align := alClient;
  pagMain.Images := modSharedData.imgImages;
  pagMain.MaxTabWidth := Scale96ToFont(250);
  pagMain.Parent := Self;

  tabClients := TClientTab.Create(pagMain, tbClients, ActionList1, FClientManager, mnuStreamPopup);
  tabClients.PageControl := pagMain;
  tabClients.AddressBar.Stations.Sort;
  tabClients.AddressBar.Stations.BuildList;
  tabClients.OnUpdateButtons := tabClientsUpdateButtons;
  tabClients.OnTrackAdded := tabClientsTrackAdded;
  tabClients.OnAddTitleToList := tabClientsAddTitleToList;
  tabClients.OnRemoveTitleFromList := tabClientsRemoveTitleFromList;
  tabClients.OnPlayStarted := tabPlayStarted;
  tabClients.OnAuthRequired := tabClientsAuthRequired;
  tabClients.OnShowErrorMessage := tabClientsShowErrorMessage;
  tabClients.OnClientAdded := tabClientsClientAdded;
  tabClients.OnClientRemoved := tabClientsClientRemoved;
  tabClients.OnSetStreamData := tabClientsSetStreamData;
  tabClients.SideBar.BrowserView.OnStreamsReceived := tabClientsBrowserViewStreamsReceived;

  tabCharts := TChartsTab.Create(pagMain);
  tabCharts.PageControl := pagMain;
  tabCharts.OnAddToWishlist := tabChartsAddToWishlist;
  tabCharts.OnRemoveTitleFromWishlist := tabChartsRemoveFromWishlist;
  tabCharts.OnAddStreams := tabChartsAddStreams;
  tabCharts.OnGetIsStreamOnListEvent := tabChartsGetIsStreamOnListEvent;

  tabLists := TListsTab.Create(pagMain, FClientManager);
  tabLists.PageControl := pagMain;

  tabSaved := TSavedTab.Create(pagMain);
  tabSaved.PageControl := pagMain;

  tabSaved.OnCut := tabSavedCut;
  tabSaved.OnRefresh := tabSavedRefresh;
  tabSaved.OnPlayStarted := tabPlayStarted;
  tabSaved.OnAddTitleToWishlist := tabSavedAddTitleToWishlist;
  tabSaved.OnRemoveTitleFromWishlist := tabSavedRemoveTitleFromWishlist;
  tabSaved.OnAddTitleToIgnorelist := tabSavedAddTitleToIgnorelist;
  tabSaved.OnRemoveTitleFromIgnorelist := tabSavedRemoveTitleFromIgnorelist;

  tabLog := TLogTab.Create(pagMain);
  tabLog.PageControl := pagMain;

  FWasShown := False;
  FUpdateOnExit := False;

  UpdateStatus;
  addTrayIcon.Visible := AppGlobals.Tray;

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
  Constraints.MinHeight := Scale96ToFont(Constraints.MinHeight);

  SetFormDimensions;

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
  HomeComm.OnException := HomeCommException;
  HomeComm.Connect;

  actPlayerIncreaseVolume.Enabled := Bass.DeviceAvailable;
  actPlayerDecreaseVolume.Enabled := Bass.DeviceAvailable;
  actPlayerMuteVolume.Enabled := Bass.DeviceAvailable;
  actEqualizer.Enabled := Bass.DeviceAvailable;

  tmrAutoSave.Enabled := True;
  tmrRecordings.Enabled := True;

  ProcessCommandLine(AppGlobals.CommandLine);

  tmrSpeed.Enabled := True;

  RegisterHotkeys;

  UpdateButtons;

  actShowSideBar.Checked := tabClients.SideBar.Visible;

  MsgBus.AddSubscriber(MessageReceived);

  Language.Translate(Self);

  if not Application.ShowMainForm then
    if StartupMessagesNeeded then
      Show
    else
      FSkipAfterShown := True;

  if not StartupMessagesNeeded then
    if (AppGlobals.AutoUpdate) and (AppGlobals.LastUpdateChecked + 1 < Now) then
      FUpdater.Start(uaVersion, True);

  FClientManager.RefreshScheduler;

  Application.OnEndSession := EndSession;
end;

procedure TfrmStreamWriterMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClientManager);
  FreeAndNil(FUpdater);

  inherited;
end;

procedure TfrmStreamWriterMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F1 then
    actHelp.Execute;
end;

procedure TfrmStreamWriterMain.FormShortcut(var Msg: TWMKey; var Handled: Boolean);
begin
  Handled := pagMain.ProcessShortcut(Msg) or TMTabSheet(pagMain.ActivePage).ProcessShortcut(Msg);
end;

procedure TfrmStreamWriterMain.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if FWasShown then
    Exit;

  // Das hier darf erst aufgerufen werden nachdem der SavedTree befüllt wurde
  tabSavedRefresh(nil);

  if Application.ShowMainForm then
    for i := 0 to ParamCount do
      if (ParamStr(i) = '-minimize') then
        WindowState := wsMinimized;

  TFunctions.ShutdownBlockReasonCreate(Handle, _('Stopping recordings and saving settings...'));

  FWasShown := True;

  PostMessage(Handle, WM_AFTERSHOWN, 0, 0);
end;

procedure TfrmStreamWriterMain.HomeCommBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader; Transferred: Cardinal);
begin
  if CommandHeader.CommandType = ctGetServerDataResponse then
    tabClients.SideBar.BrowserView.HomeCommBytesTransferred(CommandHeader, Transferred);
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
      TFunctions.MsgBox(Format(_('An error occured while communicating with the server: '#13#10'%s'), [Msg]), _('Error'), MB_ICONERROR);
    ceAuthRequired:
      if TFunctions.MsgBox(_('You need to be logged in to perform that action.'#13#10'Do you want to login now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
        ShowCommunityLogin;
    ceNotification:
      TfrmMsgDlg.ShowMsg(Self, Format(_(Notification), [Msg]), mtInformation, [mbOK], mbOK);
    ceOneTimeNotification:
    begin
      MsgHC := TFunctions.HashString(Msg);
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

procedure TfrmStreamWriterMain.HomeCommHandshake(Sender: TObject; Success: Boolean);
begin
  UpdateStatus;

  if not Success then
    TFunctions.MsgBox(_('The server did not accept the handshake. Please update streamWriter.'), _('Error'), MB_ICONERROR);
end;

procedure TfrmStreamWriterMain.HomeCommLogIn(Sender: TObject; Success: Boolean);
begin
  if not Success then
  begin
    AppGlobals.User := '';
    AppGlobals.Pass := '';
  end;

  UpdateStatus;
  if FCommunityLogin <> nil then
    FCommunityLogin.HomeCommLogIn(Sender, Success);
end;

procedure TfrmStreamWriterMain.HomeCommLogOut(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStreamWriterMain.HomeCommServerInfo(Sender: TObject; ClientCount, RecordingCount: Cardinal);
begin
  FClientCount := ClientCount;
  FRecordingCount := RecordingCount;
  UpdateStatus;
end;

procedure TfrmStreamWriterMain.HomeCommException(Sender: TObject);
var
  Res: TModalResult;
begin
  if HomeComm.RaisedException is ESSLException then
  begin
    Res := TfrmMsgDlg.ShowMsg(Self, _('The certificate received from streamwriter.org could not be validated, somebody might be intercepting your connection to streamWriter''s server. ' +
      'To abort the insecure connection press "Cancel", to continue using an insecure connection press "OK". ' + 'If you decide to use an insecure connection this setting will be remembered for future connections. ' +
      'You can change it in the settings window in the "General" category.'), mtError, [mbOK, mbCancel], mbCancel);
    if Res = mrOk then
      AppGlobals.CheckCertificate := False
    else
      HomeComm.Disabled := True;
  end;
end;

procedure TfrmStreamWriterMain.HomeCommStateChanged(Sender: TObject);
begin
  UpdateStatus;
  tabCharts.HomeCommStateChanged(Sender);

  if (not HomeComm.WasConnected) and HomeComm.Connected then
  begin
    tmrRecordingsTimer(tmrRecordings);

    FClientManager.StopMonitors;

    AppGlobals.Lock;
    try
      if AppGlobals.SubmitStats and AppGlobals.MonitorMode and (AppGlobals.MonitorCount > 0) and (AppGlobals.Data.BrowserList.Count > 0) then
        HomeComm.SendGetMonitorStreams(AppGlobals.MonitorCount)
      else
        HomeComm.SendGetMonitorStreams(0);
    finally
      AppGlobals.Unlock;
    end;

    HomeComm.SendSyncWishlist;

    if not tabCharts.Searched then
      tabCharts.SearchCharts(True, False);

    AppGlobals.Lock;
    try
      if (((AppGlobals.Data.BrowserList.Count = 0) or (AppGlobals.Data.GenreList.Count = 0)) or (AppGlobals.LastBrowserUpdate < Now - 15)) or (tabClients.SideBar.BrowserView.Mode = moError) then
        if HomeComm.SendGetServerData then
          tabClients.SideBar.BrowserView.SwitchMode(moLoading);
    finally
      AppGlobals.Unlock;
    end;
  end else if (HomeComm.WasConnected) and (not HomeComm.Connected) then
  begin
    FClientManager.StopMonitors;

    if tabCharts.State = csSearching then
      tabCharts.SetState(csSearchError);

    if tabClients.SideBar.BrowserView.Mode = moLoading then
      tabClients.SideBar.BrowserView.SwitchMode(moError);
  end;

  HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
end;

procedure TfrmStreamWriterMain.HomeCommTitleNotificationsChanged(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStreamWriterMain.Hotkey(var Msg: TWMHotKey);

  procedure StopPlay;
  var
    Clients: TClientArray;
    Client: TICEClient;
  begin
    Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClient, False));
    for Client in Clients do
      Client.StopPlay;
  end;

var
  i: Integer;
  NextIsPlaying: Boolean = False;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  Clients: TClientArray;
  Client: TICEClient;
  PlayingClient: TICEClient = nil;
  StartPlayClient: TICEClient = nil;
begin
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
      for Client in Clients do
        if Client.Playing then
          PlayingClient := Client;

      if PlayingClient <> nil then
      begin
        for Client in Clients do
        begin
          if NextIsPlaying then
          begin
            StartPlayClient := Client;
            Break;
          end;
          if Client.Playing then
            NextIsPlaying := True;
        end;

        if StartPlayClient = nil then
          if Length(Clients) > 0 then
            StartPlayClient := Client;

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
      for Client in Clients do
        if Client.Playing then
          PlayingClient := Client;

      if PlayingClient <> nil then
      begin
        for i := High(Clients) downto 0 do
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
      Players.IncreaseVolume;
    6:
      Players.DecreaseVolume;
    7:
      Players.Mute;
    8:
      tabSaved.ToggleShuffle;
  end;
end;

procedure TfrmStreamWriterMain.MessageReceived(Msg: TMessageBase);
var
  SelectSavedSongsMsg: TSelectSavedSongsMsg absolute Msg;
begin
  if (Msg is TPlayingObjectChangedMsg) or (Msg is TPlayingObjectStopped) then
  begin
    if not AppGlobals.DisplayPlayedSong then
      Exit;

    SetCaptionAndTrayHint;
  end else if Msg is TRefreshServerDataMsg then
  begin
    if HomeComm.SendGetServerData then
      tabClients.SideBar.BrowserView.SwitchMode(moLoading);
  end else if Msg is TSelectSavedSongsMsg then
    pagMain.ActivePage := tabSaved;
end;

procedure TfrmStreamWriterMain.actCheckUpdateExecute(Sender: TObject);
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
    if SizeUInt(NodeData) = Item.Tag then
    begin
      Cat := Node;
      Break;
    end;
  end;

  if (Length(Nodes) > 0) and (Cat <> nil) then
    for Node in Nodes do
      tabClients.ClientView.MoveTo(Node, Cat, amAddChildLast, False);
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

procedure TfrmStreamWriterMain.OpenCut(Filename: string);
var
  tabCut: TCutTab;
begin
  tabCut := TCutTab.Create(pagMain, nil, Filename);
  tabCut.PageControl := pagMain;

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

procedure TfrmStreamWriterMain.PrepareSave;
begin
  // Erst Lists, dann Streams. Reihenfolge ist wichtig!
  tabSaved.Tree.UpdateList;
  tabLists.UpdateLists;
  tabClients.UpdateStreams;
end;

procedure TfrmStreamWriterMain.ProcessCommandLine(const CmdLine: TCommandLine);
var
  i, Prio: Integer;
  Param: TCommandLineRecord;
  Titles: TWishlistTitleInfoArray;
begin
  Param := CmdLine.GetParam('-r');
  if Param <> nil then
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StartStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil, nil), oaStart, nil, amNoWhere);

  Param := CmdLine.GetParam('-sr');
  if Param <> nil then
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StopStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil, nil), oaStart);

  Param := CmdLine.GetParam('-p');
  if Param <> nil then
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StartStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil, nil), oaPlay, nil, amNoWhere);

  Param := CmdLine.GetParam('-sp');
  if Param <> nil then
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StopStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil, nil), oaPlay);

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
    for i := 0 to Param.Values.Count - 1 do
      tabClientsRemoveTitleFromList(nil, nil, ltSave, Param.Values[i], 0);

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
end;

procedure TfrmStreamWriterMain.mnuStreamPopupPopup(Sender: TObject);
begin
  BuildMoveToCategoryMenu;
end;

procedure TfrmStreamWriterMain.mnuStreamsClick(Sender: TObject);
begin
  BuildMoveToCategoryMenu;
end;

procedure TfrmStreamWriterMain.mnuStreamSettingsToolbarPopup(Sender: TObject);
begin
  UpdateButtons;
end;

procedure ShortcutToHotKey(const HotKey: TShortcut; out Key: Word; out Modifiers: Uint);
var
  Shift: TShiftState;
begin
  ShortcutToKey(HotKey, Key, Shift);
  Modifiers := 0;
  if (ssShift in Shift) then
    Modifiers := Modifiers or MOD_SHIFT;
  if (ssAlt in Shift) then
    Modifiers := Modifiers or MOD_ALT;
  if (ssCtrl in Shift) then
    Modifiers := Modifiers or MOD_CONTROL;
end;

procedure TfrmStreamWriterMain.UnregisterHotkeys;
begin
  UnregisterHotKey(Handle, 0);
  UnregisterHotKey(Handle, 1);
  UnregisterHotKey(Handle, 2);
  UnregisterHotKey(Handle, 3);
  UnregisterHotKey(Handle, 4);
  UnregisterHotKey(Handle, 5);
  UnregisterHotKey(Handle, 6);
  UnregisterHotKey(Handle, 7);
end;

procedure TfrmStreamWriterMain.RegisterHotkeys;
var
  K: Word;
  M: Cardinal;
begin
  if AppGlobals.ShortcutPlay > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutPlay, K, M);
    RegisterHotKey(Handle, 0, M, K);
  end;

  if AppGlobals.ShortcutPause > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutPause, K, M);
    RegisterHotKey(Handle, 1, M, K);
  end;

  if AppGlobals.ShortcutStop > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutStop, K, M);
    RegisterHotKey(Handle, 2, M, K);
  end;

  if AppGlobals.ShortcutNext > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutNext, K, M);
    RegisterHotKey(Handle, 3, M, K);
  end;

  if AppGlobals.ShortcutPrev > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutPrev, K, M);
    RegisterHotKey(Handle, 4, M, K);
  end;

  if AppGlobals.ShortcutVolUp > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutVolUp, K, M);
    RegisterHotKey(Handle, 5, M, K);
  end;

  if AppGlobals.ShortcutVolDown > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutVolDown, K, M);
    RegisterHotKey(Handle, 6, M, K);
  end;

  if AppGlobals.ShortcutMute > 0 then
  begin
    ShortcutToHotKey(AppGlobals.ShortcutMute, K, M);
    RegisterHotkey(Handle, 7, M, K);
  end;
end;

procedure TfrmStreamWriterMain.EndSession(Sender: TObject);
begin
  ExitApp(True);
end;

procedure TfrmStreamWriterMain.SetCaptionAndTrayHint;
var
  Artist, Title, Stream, Filename, NewCaption, NewHint: string;
  Recordings: Integer;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  Node: PVirtualNode;
begin
  if tabClients <> nil then
    Nodes := tabClients.ClientView.GetNodes(ntClient, False);

  NewCaption := 'streamWriter'{$IFDEF DEBUG}+ ' (Debug build)'{$ENDIF};
  NewHint := 'streamWriter'{$IFDEF DEBUG}+ ' (Debug build)'{$ENDIF};

  PlayerManager.Players.GetPlayingInfo(Artist, Title, Stream, Filename);

  if Filename <> '' then
  begin
    if (Artist <> '') and (Title <> '') then
    begin
      if AppGlobals.DisplayPlayedSong then
        NewCaption := NewCaption + ' - ' + Artist + ' - ' + Title;
      NewHint := NewHint + #13#10 + _('Playing:') + ' ' + Artist + ' - ' + Title;
    end else
    begin
      if AppGlobals.DisplayPlayedSong then
        NewCaption := NewCaption + ' - ' + TFunctions.RemoveFileExt(ExtractFileName(Filename));
      NewHint := NewHint + #13#10 + _('Playing:') + ' ' + TFunctions.RemoveFileExt(ExtractFileName(Filename));
    end;
  end else if Stream <> '' then
    if Title <> '' then
    begin
      if AppGlobals.DisplayPlayedSong then
        NewCaption := NewCaption + ' - ' + Title + ' - ' + Stream;
      NewHint := NewHint + #13#10 + _('Playing:') + ' ' + Title;
    end else
    begin
      if AppGlobals.DisplayPlayedSong then
        NewCaption := NewCaption + ' - ' + Stream;
      NewHint := NewHint + #13#10 + _('Playing:') + ' ' + Stream;
    end;

  Recordings := 0;
  if tabClients <> nil then
    for Node in Nodes do
    begin
      NodeData := tabClients.ClientView.GetNodeData(Node);
      if NodeData.Client.Recording then
        Inc(Recordings);
    end;

  if Recordings = 1 then
    NewHint := NewHint + #13#10 + Format(_('%d active recording'), [Recordings])
  else
    NewHint := NewHint + #13#10 + Format(_('%d active recordings'), [Recordings]);
  NewHint := NewHint + #13#10 + TFunctions.MakeSize(FSpeed) + '/s';

  if Caption <> NewCaption then
    Caption := NewCaption;
  if addTrayIcon.Hint <> NewHint then
    addTrayIcon.Hint := NewHint;
end;

procedure TfrmStreamWriterMain.SettingsSaveForExport(Sender: TObject);
begin
  // Ist hier, damit der Profilexport korrekt funktioniert
  PrepareSave;
end;

procedure TfrmStreamWriterMain.UpdateActions;
begin
  // Do not call base method. When client view is focused everytime a timer fires UpdateAction requires a noticable amount of resources. Should be investigated further...
  // inherited UpdateActions;
end;

procedure TfrmStreamWriterMain.SetupExitMessage(var Msg: TMessage);
begin
  if (Msg.WParam = 6345) and (Msg.LParam = 555) then
    ExitApp(False);
end;

procedure TfrmStreamWriterMain.ShowCommunityLogin;
begin
  if FCommunityLogin <> nil then
  begin
    FCommunityLogin.ApplyFocus;
    Exit;
  end;

  FCommunityLogin := TfrmCommunityLogin.Create(Self);
  FCommunityLogin.OnClose := CommunityLoginClose;
  FCommunityLogin.Show;
end;

procedure TfrmStreamWriterMain.ShowSettings(SettingsType: TSettingsTypes; BrowseDir: Boolean);
var
  i: Integer;
  S: TfrmSettings;
  OldMonitorCount, NewMonitorCount: Cardinal;
  StreamSettings: TStreamSettingsArray = [];
  Client: TICEClient;
  Clients: TClientArray;
begin
  if AppGlobals.SubmitStats and AppGlobals.MonitorMode then
    OldMonitorCount := AppGlobals.MonitorCount
  else
    OldMonitorCount := 0;

  if SettingsType = stApp then
  begin
    UnregisterHotkeys;
    StreamSettings += [AppGlobals.Data.StreamSettings.Copy];
  end else if SettingsType = stAuto then
    StreamSettings += [AppGlobals.Data.AutoRecordSettings.Copy]
  else if SettingsType = stStream then
  begin
    Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClientNoAuto, True));
    if Length(Clients) > 0 then
      for Client in Clients do
        StreamSettings += [Client.Entry.Settings]
    else
      Exit;
  end else
    raise Exception.Create('SettingsType not allowed here');

  S := TfrmSettings.Create(Self, SettingsType, StreamSettings, BrowseDir);
  try
    S.OnSaveForExport := SettingsSaveForExport;
    S.ShowModal;

    if S.ImportFilename <> '' then
    begin
      ExitApp(False, S.ImportFilename);
      Exit;
    end;

    if S.SaveSettings then
      case SettingsType of
        stApp:
        begin
          AppGlobals.Data.StreamSettings.Assign(S.StreamSettings[0]);

          SetCaptionAndTrayHint;

          if AppGlobals.SubmitStats and AppGlobals.MonitorMode then
            NewMonitorCount := AppGlobals.MonitorCount
          else
            NewMonitorCount := 0;
          if NewMonitorCount <> OldMonitorCount then
          begin
            FClientManager.StopMonitors;
            HomeComm.SendGetMonitorStreams(NewMonitorCount);
          end;

          tabSaved.Tree.SetDirectoryWatchers;

          Language.Translate(Self);

          tabClients.ShowInfo;

          addTrayIcon.Visible := AppGlobals.Tray;

          TLogger.SetFilename(AppGlobals.LogFile);
        end;
        stAuto:
        begin
          AppGlobals.Data.AutoRecordSettings.Assign(S.StreamSettings[0]);

          HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);

          tabSaved.Tree.SetDirectoryWatchers;
        end;
        stStream:
          for i := 0 to High(Clients) do
            if not Clients[i].AutoRemove then
              Clients[i].Entry.Settings.Assign(S.StreamSettings[i]);
      end;

    if SettingsType = stApp then
      RegisterHotkeys;
  finally
    S.Free;
  end;
end;

procedure TfrmStreamWriterMain.ShowStartupMessages;
begin
  if not Bass.DeviceAvailable then
    TfrmMsgDlg.ShowMsg(Self, _('No sound devices could be detected so playback of streams and files will not be possible.'),
      mtWarning, [mbOK], mbOK, 7);

  if (AppGlobals.LastUsedDataVersion > 0) and (AppGlobals.LastUsedDataVersion < 60) and (not FExiting) then
    TFunctions.MsgBox(_('Since handling of settings for automatically saved songs changed, these settings were reset to default values. Please see "Settings"->"Settings for automatic recordings..." in the menu to adjust these settings.'),
      _('Info'), MB_ICONINFORMATION);

  if (not DirectoryExists(AppGlobals.Dir)) and (not FExiting) then
  begin
    TFunctions.MsgBox(_('The folder for saved songs does not exist.'#13#10'Please select a folder now.'), _('Info'), MB_ICONINFORMATION);
    ShowSettings(stApp, True);
  end;

  if (not DirectoryExists(AppGlobals.DirAuto)) and (not FExiting) then
  begin
    TFunctions.MsgBox(_('The folder for automatically saved songs does not exist.'#13#10'Please select a folder now.'), _('Info'), MB_ICONINFORMATION);
    ShowSettings(stAuto, True);
  end;
end;

procedure TfrmStreamWriterMain.ShowUpdate(Version: string = ''; UpdateURL: string = '');
var
  S: TfrmUpdate;
begin
  S := TfrmUpdate.Create(Self, Version, UpdateURL);
  try
    S.ShowModal;

    if (S.Updated) and (S.Exit) then
    begin
      if CanExitApp then
      begin
        FUpdateOnExit := True;
        ExitApp(False);
      end;
    end else if S.Updated then
    begin
      AppGlobals.InstallUpdateOnStart := True;
      actCheckUpdate.Enabled := False;
    end;
  finally
    S.Free;
  end;
end;

function TfrmStreamWriterMain.StartupMessagesNeeded: Boolean;
begin
  if not Bass.DeviceAvailable then
    Exit(True);

  if not DirectoryExists(AppGlobals.Dir) then
    Exit(True);

  if (AppGlobals.LastUsedDataVersion > 0) and (AppGlobals.LastUsedDataVersion < 60) then
    Exit(True);

  // Das erste DirectoryExists() ist da, damit der Settings-Dialog nicht doppelt kommt.
  if DirectoryExists(AppGlobals.Dir) and (not DirectoryExists(AppGlobals.DirAuto)) then
    Exit(True);

  if not AppGlobals.FirstStartShown then
    Exit(True);

  if Length(AppGlobals.Data.GetMissingAddons) > 0 then
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
      addTrayIcon.Visible := True;
      Hide;
      Exit;
    end;
  end;

  DefaultHandler(Msg);
end;

procedure TfrmStreamWriterMain.tabCutCutFile(Sender: TObject; Filename: string);
var
  tabCut: TCutTab;
  AudioType: TAudioTypes;
begin
  Application.ProcessMessages;

  AudioType := FilenameToFormat(Filename);

  case AppGlobals.AddonManager.CanEncode(AudioType) of
    ceNoAddon:
    begin
      TFunctions.MsgBox(_('This filetype is not supported by streamWriter.'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;
    ceAddonNeeded:
      if TFunctions.MsgBox(_('To cut the selected file the required encoder-addon needs to be installed. Do you want to download and install the required addon now?'), _('Question'),
        MB_ICONINFORMATION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
      begin
        if not AppGlobals.AddonManager.EnableAddon(Self, AppGlobals.AddonManager.Find(AudioType), True) then
          Exit;
      end else
        Exit;
  end;

  tabCut := TCutTab(Sender);
  if tabCut <> nil then
    OpenCut(Filename)
  else
    pagMain.ActivePage := tabCut;
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

procedure TfrmStreamWriterMain.tabSavedAddTitleToWishlist(Sender: TObject; Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.AddEntry(Title, TitleHash, False, ltSave);
end;

procedure TfrmStreamWriterMain.tabSavedRemoveTitleFromWishlist(Sender: TObject; Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.RemoveEntry(Title, TitleHash, ltSave);
end;

procedure TfrmStreamWriterMain.tabSavedAddTitleToIgnorelist(Sender: TObject; Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.AddEntry(Title, TitleHash, False, ltIgnore);
end;

procedure TfrmStreamWriterMain.tabSavedRemoveTitleFromIgnorelist(Sender: TObject; Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.RemoveEntry(Title, TitleHash, ltIgnore);
end;

procedure TfrmStreamWriterMain.tabSavedCut(Entry: TStreamEntry; Track: TTrackInfo);
var
  tabCut: TCutTab;
  AudioType: TAudioTypes;
begin
  AudioType := FilenameToFormat(Track.Filename);
  if AppGlobals.AddonManager.CanEncode(AudioType) <> ceOkay then
    if TFunctions.MsgBox(_('To cut the selected file the required encoder-addon needs to be installed. Do you want to download and install the required addon now?'), _('Question'),
      MB_ICONINFORMATION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
    begin
      if not AppGlobals.AddonManager.EnableAddon(Self, AppGlobals.AddonManager.Find(AudioType), False) then
        Exit;
    end else
      Exit;

  tabCut := TCutTab(pagMain.FindCut(Track.Filename));
  if tabCut = nil then
    OpenCut(Track)
  else
    pagMain.ActivePage := tabCut;
end;

procedure TfrmStreamWriterMain.tabClientsShowErrorMessage(Sender: TObject; Data: string);
begin
  TfrmMsgDlg.ShowMsg(Self, Data, mtInformation, [mbOK], mbOK);
end;

procedure TfrmStreamWriterMain.tabSavedRefresh(Sender: TObject);
var
  i: Integer;
  Files: TList<TFileEntry>;
begin
  if FCheckFiles <> nil then
    Exit;

  Files := TList<TFileEntry>.Create;
  try
    for i := 0 to AppGlobals.Data.TrackList.Count - 1 do
      Files.Add(TFileEntry.Create(AppGlobals.Data.TrackList[i].Filename, AppGlobals.Data.TrackList[i].Filesize, feaNone));
    FCheckFiles := TCheckFilesThread.Create(Files);
    FCheckFiles.OnTerminate := CheckFilesTerminate;
    FCheckFiles.Start;
  finally
    // Wird vom Thread erledigt. Unschön, aber...
    // Files.Free;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsTrackAdded(Entry: TStreamEntry; Track: TTrackInfo);
begin
  tabSaved.Tree.AddTrack(Track, True);
end;

procedure TfrmStreamWriterMain.tabChartsAddStreams(Sender: TObject; Info: TStartStreamingInfoArray; Action: TStreamOpenActions);
begin
  tabClients.StartStreaming(Info, Action, nil, amAddChildLast);
end;

procedure TfrmStreamWriterMain.tabChartsAddToWishlist(Sender: TObject; Arr: TWishlistTitleInfoArray);
var
  NumChars: Integer;
  Hash: Cardinal;
  Hashes: TSyncWishlistRecordArray = [];
  Found: Boolean;
  T: TTitleInfo;
  WishlistTitle: TWishlistTitleInfo;
begin
  for WishlistTitle in Arr do
  begin
    TFunctions.BuildPattern(WishlistTitle.Title, Hash, NumChars, True);
    Found := False;

    for T in AppGlobals.Data.SaveList do
      if ((WishlistTitle.Hash = 0) and (T.ServerHash = 0) and (T.Hash = Hash)) or ((WishlistTitle.Hash > 0) and WishlistTitle.IsArtist and
        (T.ServerArtistHash = WishlistTitle.Hash)) or ((WishlistTitle.Hash > 0) and (not WishlistTitle.IsArtist) and (T.ServerHash = WishlistTitle.Hash)) then
      begin
        Found := True;
        Break;
      end;

    if not Found then
    begin
      if WishlistTitle.Hash > 0 then
      begin
        if WishlistTitle.IsArtist then
          T := TTitleInfo.Create(0, WishlistTitle.Hash, WishlistTitle.Title)
        else
          T := TTitleInfo.Create(WishlistTitle.Hash, 0, WishlistTitle.Title);
      end else
        T := TTitleInfo.Create(0, 0, WishlistTitle.Title);

      AppGlobals.Data.SaveList.Add(T);
      tabLists.AddTitle(nil, ltSave, T);

      if WishlistTitle.Hash > 0 then
        Hashes += [TSyncWishlistRecord.Create(WishlistTitle.Hash, WishlistTitle.IsArtist)];
    end;
  end;

  if Length(Hashes) > 0 then
  begin
    HomeComm.SendSyncWishlist(swAdd, Hashes);
    HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
    MsgBus.SendMessage(TListsChangedMsg.Create);
  end;
end;

function TfrmStreamWriterMain.tabChartsGetIsStreamOnListEvent(Sender: TObject; Stream: TStreamBrowserEntry): Boolean;
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

procedure TfrmStreamWriterMain.tabChartsRemoveFromWishlist(Sender: TObject; Arr: TWishlistTitleInfoArray);
var
  i, n: Integer;
  Hashes: TSyncWishlistRecordArray = [];
begin
  for n := 0 to High(Arr) do
    for i := AppGlobals.Data.SaveList.Count - 1 downto 0 do
      if (AppGlobals.Data.SaveList[i].ServerHash > 0) and (Arr[n].Hash > 0) and (not Arr[n].IsArtist) and (AppGlobals.Data.SaveList[i].ServerHash = Arr[n].Hash) then
      begin
        tabLists.RemoveTitle(nil, ltSave, AppGlobals.Data.SaveList[i]);
        AppGlobals.Data.SaveList.Delete(i);

        Hashes += [TSyncWishlistRecord.Create(Arr[n].Hash, False)];
      end;

  if Length(Hashes) > 0 then
  begin
    HomeComm.SendSyncWishlist(swRemove, Hashes);
    HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
    MsgBus.SendMessage(TListsChangedMsg.Create);
  end;
end;

procedure TfrmStreamWriterMain.tabClientsAddTitleToList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string);
var
  i, NumChars: Integer;
  Hash: Cardinal;
  Found: Boolean;
  T: TTitleInfo;
  List: TSaveIgnoreList;
begin
  if Client = nil then
    if ListType = ltSave then
      List := AppGlobals.Data.SaveList
    else
      List := AppGlobals.Data.IgnoreList
  else if ListType = ltSave then
    List := Client.Entry.SaveList
  else
    List := Client.Entry.IgnoreList;

  TFunctions.BuildPattern(Title, Hash, NumChars, True);
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

      HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
    end;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsRemoveTitleFromList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string; ServerTitleHash: Cardinal);
var
  i: Integer;
  List: TSaveIgnoreList;
  T: TTitleInfo;
begin
  if Client = nil then
    if ListType = ltSave then
      List := AppGlobals.Data.SaveList
    else
      List := AppGlobals.Data.IgnoreList
  else if ListType = ltSave then
    List := Client.Entry.SaveList
  else
    List := Client.Entry.IgnoreList;

  for i := List.Count - 1 downto 0 do
    if ((List[i].ServerHash > 0) and (List[i].ServerHash = ServerTitleHash)) or TFunctions.Like(Title, List[i].Pattern) then
    begin
      tabLists.RemoveTitle(Client, ListType, List[i]);

      T := List[i];
      List.Delete(i);
      T.Free;
    end;

  HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
  MsgBus.SendMessage(TListsChangedMsg.Create);
end;

procedure TfrmStreamWriterMain.tabClientsAuthRequired(Sender: TObject);
begin
  if TFunctions.MsgBox(_('You need to be logged in to perform that action.'#13#10'Do you want to login now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
    ShowCommunityLogin;
end;

procedure TfrmStreamWriterMain.tabClientsBrowserViewStreamsReceived(Sender: TObject);
begin
  // Nach einer Neuinstallation können wir noch keine Monitors anfragen, weil wir noch keine Streams kennen.
  // Wenn die Streams angekommen sind, dann machen wir das hier klar!
  if AppGlobals.SubmitStats and AppGlobals.MonitorMode and (AppGlobals.MonitorCount > 0) and (AppGlobals.Data.BrowserList.Count > 0) and (FClientManager.Monitors.Count = 0) then
    HomeComm.SendGetMonitorStreams(AppGlobals.MonitorCount);
end;

procedure TfrmStreamWriterMain.tabClientsSetStreamData(Sender: TObject; StreamID: Integer);
var
  Form: TfrmSetStreamData;
begin
  Form := TfrmSetStreamData.Create(Self, StreamID);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsUpdateButtons(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmStreamWriterMain.tabCutSaved(Sender: TObject; AudioInfo: TAudioInfo);
var
  Track: TTrackInfo;
  FileSize: Int64;
begin
  Track := AppGlobals.Data.TrackList.GetTrack(TCutTab(Sender).Filename);

  if not Assigned(Track) then
    Exit;

  if TFunctions.GetFileSize(TCutTab(Sender).Filename, FileSize) then
    Track.Filesize := FileSize;
  Track.Length := Trunc(AudioInfo.Length);

  // Ist mal raus, damit das "geschnitten"-Symbol nur bei automatischen Aufnahmen kommt
  // Track.WasCut := True;

  Track.Finalized := True;

  Track.Bitrate := AudioInfo.Bitrate;
  Track.VBR := AudioInfo.VBR;

  tabSaved.Tree.UpdateTracks([Track]);

  // Macht den Finalized-Button passig (Down/nicht Down)
  tabSaved.UpdateButtons;
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
        TSavedTab(Tab).PausePlay
      else if Tab is TClientTab then
        TClientTab(Tab).PausePlay
      else if Tab is TCutTab then
        TCutTab(Tab).PausePlay;
  end;
end;

procedure TfrmStreamWriterMain.tmrAutoSaveTimer(Sender: TObject);
begin
  if Application.Terminated or AppGlobals.SkipSave or AppGlobals.Data.LoadError then
    Exit;

  try
    PrepareSave;
    AppGlobals.Data.Save(AppGlobals.DataFile, False);
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
  if Application.Terminated or (not AppGlobals.SubmitStats) then
    Exit;

  C := 0;
  L := TList<Cardinal>.Create;
  try
    for i := 0 to FClientManager.Count - 1 do
      if FClientManager[i].Recording or FClientManager[i].Playing and not FClientManager[i].AutoRemove then
        if FClientManager[i].Entry.ID > 0 then
          L.Add(FClientManager[i].Entry.ID)
        else
          Inc(C);

    for i := 0 to FClientManager.Monitors.Count - 1 do
      if FClientManager.Monitors[i].Entry.ID > 0 then
        L.Add(FClientManager.Monitors[i].Entry.ID);

    HomeComm.SendUpdateStats(L, C);
  finally
    L.Free;
  end;
end;

procedure TfrmStreamWriterMain.tmrSpeedTimer(Sender: TObject);
var
  RecordingActive: Boolean;
  PlayingActive: Boolean;
  i: Integer;
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Client2: TICEClient;
  Speed: Cardinal;
  OnlyAuto: Boolean;
begin
  Speed := 0;
  Clients := tabClients.ClientView.NodesToData(tabClients.ClientView.GetNodes(ntClient, False));
  for Client in Clients do
  begin
    Speed := Speed + Client.Client.Speed;
    tabClients.ClientView.RefreshClient(Client.Client);
  end;

  for Client2 in FClientManager.Monitors do
    Speed := Speed + Client2.Speed;

  FSpeed := Speed;
  SetCaptionAndTrayHint;

  addStatus.Speed := Speed;
  addStatus.BuildSpeedBmp;
  addStatus.CurrentReceived := tabClients.Received;
  addStatus.OverallReceived := AppGlobals.Data.Received;

  UpdateStatus;

  tabClients.TimerTick;

  RecordingActive := False;
  PlayingActive := False;
  for i := 0 to FClientManager.Count - 1 do
  begin
    if FClientManager[i].Recording then
      RecordingActive := True;
    if FClientManager[i].Playing then
      PlayingActive := True;
  end;

  OnlyAuto := True;

  if RecordingActive then
  begin
    if not TFunctions.DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
      Inc(FDiskSpaceFailCount)
    else
      FDiskSpaceFailCount := 0;

    if FDiskSpaceFailCount >= 10 then
    begin
      for i := 0 to FClientManager.Count - 1 do
        if FClientManager[i].Recording and (not FClientManager[i].AutoRemove) then
        begin
          FClientManager[i].WriteLog(_('Stopping recording because available disk space is below the set limit'), '', ltGeneral, llWarning);
          FClientManager[i].StopRecording;
          OnlyAuto := False;
        end;

      if not OnlyAuto then
      begin
        tmrSpeed.Enabled := False;

        TfrmMsgDlg.ShowMsg(Self, _('Available disk space is below the set limit, so recordings will be stopped.'), mtInformation, [mbOK], mbOK);

        tmrSpeed.Enabled := True;
      end;
    end;
  end else
    FDiskSpaceFailCount := 0;

  Power.Critical := (PlayingActive or RecordingActive) or (AppGlobals.Data.SaveList.AnyAutomatic and (AppGlobals.AutoTuneIn));
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
  end else if not Visible then
  begin
    Show;
    SetForegroundWindow(Handle);
  end else if not AlwaysShow then
  begin
    FWasMaximized := WindowState = wsMaximized;
    CloseWindow(Handle);
    Hide;
  end else
    SetForegroundWindow(Handle);
end;

procedure TfrmStreamWriterMain.UpdateButtons;
var
  i: Integer;
  OneNormalRecordingWithTitle, AllNormalStopsAfterSong: Boolean;
  B, OnlyAutomatedSelected, OnlyAutomatedCatsSelected, OnlyAutomaticRecording: Boolean;
  URLFound, OneRecording, OneNotRecording, OnePlaying, OnePaused: Boolean;
  OneHasTitle: Boolean;
  Clients, AllClients: TClientArray;
  Client: TICEClient;
  CatNodes: TNodeArray;
  Node: PVirtualNode;
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
  OnlyAutomatedSelected := True;
  OneRecording := False;
  OneNotRecording := False;
  OnePlaying := False;
  OnePaused := False;
  OnlyAutomatedCatsSelected := Length(CatNodes) > 0;
  OneHasTitle := False;

  for Node in CatNodes do
    if not PClientNodeData(tabClients.ClientView.GetNodeData(Node)).Category.IsAuto then
    begin
      OnlyAutomatedCatsSelected := False;
      Break;
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
        if not Client.StopAfterSong then
          AllNormalStopsAfterSong := False;
      end;
    end;
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

  for Node in CatNodes do
    if not PClientNodeData(tabClients.ClientView.GetNodeData(Node)).Category.IsAuto then
    begin
      OnlyAutomatedSelected := False;
      Break;
    end;

  B := Length(Clients) > 0;
  actStart.Enabled := (B and OneNotRecording and not OnlyAutomatedSelected) or ((Length(CatNodes) > 0) and (not OnlyAutomatedCatsSelected));
  actStop.Enabled := (B and OneRecording and (not OnlyAutomaticRecording) and not OnlyAutomatedSelected) or ((Length(CatNodes) > 0) and (not OnlyAutomatedCatsSelected));

  actRename.Enabled := (tabClients.ClientView.SelectedCount = 1) and (not OnlyAutomatedSelected) and (not OnlyAutomatedCatsSelected);
  actRemove.Enabled := B or ((Length(CatNodes) > 0) and not OnlyAutomatedCatsSelected);
  actStreamSettings.Enabled := B and (not OnlyAutomatedSelected);

  URLFound := False;
  if Length(Clients) > 0 then
    if Trim(Clients[0].Entry.StreamURL) <> '' then
      URLFound := True;
  if actOpenWebsite.Enabled <> URLFound then
    actOpenWebsite.Enabled := URLFound;

  actTuneInStream.Enabled := B;
  actSavePlaylistStream.Enabled := B;
  actResetData.Enabled := ((Length(Clients) > 0) and not OnlyAutomatedSelected);
  actStopPlay.Enabled := OnePlaying and Bass.DeviceAvailable;
  actPause.Checked := OnePaused;
  actPause.Enabled := OnePlaying and Bass.DeviceAvailable;
  actPlay.Enabled := (Length(Clients) = 1) and (not (Clients[0].AutoRemove and (Clients[0].State <> csConnected))) and Bass.DeviceAvailable;

  // Das hier muss man vor dem nächsten "Enabled"-Setzen machen. Er muss aus, sonst lässt sich "Checked" nach dem nächsten "Enabled := True" nicht mehr setzen.
  // Der Button malt sich halt nicht passend...
  if not OneNormalRecordingWithTitle then
    actStopAfterSong.Checked := False;

  actStopAfterSong.Enabled := OneNormalRecordingWithTitle;
  actStopAfterSong.Checked := OneNormalRecordingWithTitle and AllNormalStopsAfterSong;
  actTimers.Enabled := (Length(Clients) = 1) and (not Clients[0].AutoRemove);
  mnuCurrentTitle1.Enabled := (Length(Clients) > 0) and OneHasTitle;
  mnuCurrentTitle2.Enabled := (Length(Clients) > 0) and OneHasTitle;
  actAddToSaveList.Enabled := (Length(Clients) > 0) and OneHasTitle;
  actAddToGlobalIgnoreList.Enabled := (Length(Clients) > 0) and OneHasTitle;
  actAddToStreamIgnoreList.Enabled := (Length(Clients) > 0) and OneHasTitle;
  actCopyTitle.Enabled := (Length(Clients) > 0) and OneHasTitle;
end;

procedure TfrmStreamWriterMain.UpdateFound(var Msg: TMessage);
var
  Res: Integer;
begin
  Res := TFunctions.MsgBox(_('A new version of streamWriter was found.'#13#10'Do you want to download the update now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1);
  if Res = IDYES then
    if AppGlobals.RunningFromInstalledLocation then
      ShowUpdate(FUpdater.FoundVersion.AsString, FUpdater.UpdateURL)
    else
      TFunctions.ShellExecute(Handle, 'open', AppGlobals.ProjectLink);
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
  begin
    if HomeComm.Secured then
      CS := cshConnectedSecure
    else
      CS := cshConnected;
  end else
  begin
    CS := cshDisconnected;
    FClientCount := 0;
    FRecordingCount := 0;
  end;

  addStatus.SetState(CS, HomeComm.Authenticated, HomeComm.NotifyTitleChanges, FClientCount,
    FRecordingCount, FClientManager.SongsSaved, AppGlobals.Data.SongsSaved);

  SetCaptionAndTrayHint;
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
    if TfrmMsgDlg.ShowMsg(Self, _('You are recording at least one stream at the moment. Exiting the application will abort streaming.'#13#10'Do you really want to quit?'), mtConfirmation, mbYesNo, mbNo, 1) = mrNo then
      Result := False;
end;

procedure TfrmStreamWriterMain.CheckFilesTerminate(Sender: TObject);
var
  i, n: Integer;
  Track: TTrackInfo;
  E: TFileEntry;
  RemoveTracks: TList<TTrackInfo>;
begin
  tabSaved.Tree.BeginUpdate;
  RemoveTracks := TList<TTrackInfo>.Create;
  try
    for i := 0 to FCheckFiles.Files.Count - 1 do
    begin
      if FExiting then
        Break;

      E := TFileEntry(FCheckFiles.Files[i]);

      if E.Action = feaNone then
        Continue;

      for n := 0 to AppGlobals.Data.TrackList.Count - 1 do
        if AppGlobals.Data.TrackList[n].Filename = E.Filename then
        begin
          Track := AppGlobals.Data.TrackList[n];
          case E.Action of
            feaSize:
              Track.Filesize := E.Size;
            feaRemove:
              RemoveTracks.Add(Track);
          end;
          Break;
        end;
    end;

    tabSaved.Tree.RemoveTracks(RemoveTracks.ToArray);
  finally
    RemoveTracks.Free;
    tabSaved.Tree.EndUpdate;
  end;

  FCheckFiles := nil;
end;

procedure TfrmStreamWriterMain.Community1Click(Sender: TObject);
begin
  actLogOn.Enabled := not HomeComm.Authenticated and HomeComm.CommunicationEstablished;
  actLogOff.Enabled := HomeComm.Authenticated and HomeComm.CommunicationEstablished;
end;

procedure TfrmStreamWriterMain.CommunityLoginClose(Sender: TObject; var Action: TCloseAction);
begin
  FCommunityLogin := nil;
end;

constructor TfrmStreamWriterMain.Create(AOwner: TComponent);
begin
  inherited;

  SetWindowLongPtrW(Handle, GWLP_USERDATA, Windows.HANDLE(Self));

  FPrevWndProc := Pointer(SetWindowLongPtrW(Handle, GWLP_WNDPROC, LONG_PTR(@CustomWndProcWrapper)));
end;

destructor TfrmStreamWriterMain.Destroy;
begin
  SetWindowLongPtrW(Handle, GWLP_WNDPROC, LONG_PTR(@FPrevWndProc));

  inherited;
end;

class function TfrmStreamWriterMain.CustomWndProcWrapper(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := TfrmStreamWriterMain(GetWindowLongPtrW(hwnd, GWLP_USERDATA)).CustomWndProc(hwnd, uMsg, wParam, lParam);
end;

function TfrmStreamWriterMain.CustomWndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  CmdLine: TCommandLine;
begin
  if not FExiting and (uMsg = WM_COPYDATA) then
  begin
    CmdLine := TCommandLine.Create(PChar(PCOPYDATASTRUCT(lParam).lpData));
    try
      if CmdLine.Records.Count = 0 then
        ToggleWindow(True)
      else
        ProcessCommandLine(CmdLine);
    finally
      CmdLine.Free;
    end;

    Exit(0);
  end;

  Result := FPrevWndProc(hwnd, uMsg, wParam, lParam);
end;

end.
