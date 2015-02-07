{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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
  AppMessages, CommandLine, Protocol, Commands, HomeCommands, SharedData,
  LogTab;

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
    procedure tmrAutoSaveTimer(Sender: TObject);
    procedure actPlayerDecreaseVolumeExecute(Sender: TObject);
    procedure actPlayerIncreaseVolumeExecute(Sender: TObject);
    procedure actPlayerStopExecute(Sender: TObject);
    procedure actPlayerPlayPauseExecute(Sender: TObject);
    procedure actPlayerMuteVolumeExecute(Sender: TObject);
    procedure mnuPlayerClick(Sender: TObject);
    procedure actEqualizerExecute(Sender: TObject);
    procedure mnuStreamsClick(Sender: TObject);
  private
    FCommunityLogin: TfrmCommunityLogin;

    FUpdater: TUpdateClient;
    FUpdateOnExit: Boolean;
    FSkipAfterShown: Boolean;

    FSpeed: UInt64;
    FClientCount: Cardinal;
    FRecordingCount: Cardinal;

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

    FSSLErrorShown: Boolean;

    procedure AfterShown(var Msg: TMessage); message WM_AFTERSHOWN;
    procedure ReceivedData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure QueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;
    procedure EndSession(var Msg: TMessage); message WM_ENDSESSION;
    procedure SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure Hotkey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure UpdateFound(var Msg: TMessage); message WM_UPDATEFOUND;
    procedure SetupExitMessage(var Msg: TMessage); message 5432;

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
    procedure RegisterHotkeys;
    procedure ShowCommunityLogin;
    procedure OpenCut(Filename: string); overload;
    procedure OpenCut(Track: TTrackInfo); overload;
    procedure ProcessCommandLine(Data: string);
    procedure SetCaptionAndTrayHint;
    procedure BuildMoveToCategoryMenu;

    function StartupMessagesNeeded: Boolean;
    procedure ShowStartupMessages;
    procedure PrepareSave;

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
    procedure HomeCommSSLError(Sender: TObject);

    procedure PreTranslate;
    procedure PostTranslate;

    procedure tabClientsUpdateButtons(Sender: TObject);
    procedure tabClientsTrackAdded(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabClientsTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabClientsAddTitleToList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string);
    procedure tabClientsRemoveTitleFromList(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string; ServerTitleHash: Cardinal);
    procedure tabClientsAuthRequired(Sender: TObject);
    procedure tabClientsShowErrorMessage(Sender: TObject; Data: string);
    procedure tabClientsClientAdded(Sender: TObject);
    procedure tabClientsClientRemoved(Sender: TObject);
    procedure tabClientsBrowserViewStreamsReceived(Sender: TObject);

    procedure tabSavedCut(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabSavedTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
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
  HardTimeout: Integer;
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

  // Das Hide lassen wir weg. Das ist nämlich gefährlich, wenn Windows herunter
  // gefahren wird. Wenn ein Programm kein sichtbares Top-Level-Fenster mehr hat,
  // dann kann es einfach abgeschossen werden, und darf den Shutdown nicht blockieren!
  //Hide;

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
  AppGlobals.BrowserSortType := Integer(tabClients.SideBar.BrowserView.StreamTree.SelectedSortType);

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

  for i := 0 to tabLists.ListsPanel.Tree.Header.Columns.Count - 1 do
  begin
    AppGlobals.ListHeaderWidth[i] := tabLists.ListsPanel.Tree.Header.Columns[i].Width;
    AppGlobals.ListHeaderPosition[i] := tabLists.ListsPanel.Tree.Header.Columns[i].Position;
  end;

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

  TfrmNotification.Stop;

  FEqualizer.Hide;

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

  Hard := False;
  StartTime := GetTickCount;
  while (HomeComm.ThreadAlive or HomeComm.Connected) or FClientManager.Active or (FCheckFiles <> nil) or FUpdater.Active do
  begin
    // Wait for threads to finish
    if StartTime < GetTickCount - HardTimeout then
    begin
      Hard := True;
      Break;
    end;
    Sleep(100);
    Application.ProcessMessages;
  end;

  PrepareSave;

  while True do
  begin
    try
      AppGlobals.Data.Save;
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

  // Remove all clients from list
  FClientManager.Terminate;

  tabClients.ClientView.Clear;

  if FUpdateOnExit then
    FUpdater.RunUpdate(Handle);

  if ImportFilename <> '' then
  begin
    try
      S := TExtendedStream.Create;
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

        S.Read(Version);
        Lst := TSettingsList.Load(S);
        try
          AppGlobals.Storage.Assign(Lst);
          // Hier wird das eben gespeicherte neu geladen, damit das anschließende
          // verarbeiten der Datendatei (AppGlobals.Data.Load()) Zugriff auf
          // LastUsedVersion aus den Registry-/Ini-Einstellungen hat.
          AppGlobals.Load;
          AppGlobals.LoadOldStreamSettings;
          // Remark: Das hier ist ganz fies, bis zur "Trennlinie" muss das irgendwann raus, wenn jeder Client mindestens DataVersion 61 hat.
          //         Genau dann kann auch StreamSettingsObsolete raus! Das ist hier nur so, dass das InitPostProcessors() was folgt
          //         die Dinger neu lädt mithilfe der importierten Einstellungen aus der Registry. Extrem pfuschig.
          AppGlobals.StreamSettingsObsolete.PostProcessors.Clear;
          AppGlobals.StreamSettingsObsolete.EncoderSettings.Clear;
          AppGlobals.InitPostProcessors;
          // -----------------------------------------------------------
          AppGlobals.Data.Load(S, ImportFilename);
          AppGlobals.Data.Save;
        finally
          Lst.Free;
        end;
      finally
        S.Free;
      end;
    except
      on E: EVersionException do
        MsgBox(0, _('The file could not be imported because it was exported with a newer version of streamWriter.'), _('Error'), MB_ICONERROR);
      on E: EUnsupportedFormatException do
        MsgBox(0, _('The file could not be imported because it contains regular saved data and no exported profile.'), _('Error'), MB_ICONERROR);
      on E: EUnknownFormatException do
        MsgBox(0, _('The file could not be imported because it''s format is unknown.'), _('Error'), MB_ICONERROR);
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
  F := TfrmAbout.Create(Self, _('About'), False);
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
  FormIntro: TfrmIntro;
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
  end;
end;

procedure TfrmStreamWriterMain.BuildMoveToCategoryMenu;
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
  mnuMoveToCategory2.Clear;
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

      Item := mnuMain.CreateMenuItem;
      Item.Caption := Cat.Category.Name;
      Item.Tag := Integer(Cat);
      Item.OnClick := mnuMoveToCategory;
      mnuMoveToCategory2.Add(Item);
    end;
  end;

  mnuMoveToCategory1.Enabled := (Length(Clients) > 0) and (mnuMoveToCategory1.Count > 0);
  mnuMoveToCategory2.Enabled := (Length(Clients) > 0) and (mnuMoveToCategory2.Count > 0);
end;

procedure TfrmStreamWriterMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
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
  end else
  begin
    if CanExitApp then
      ExitApp(False);
  end;
end;

procedure TfrmStreamWriterMain.FormCreate(Sender: TObject);
begin
  SetCaptionAndTrayHint;

  AppGlobals.WindowHandle := Handle;

  if not Bass.EffectsAvailable then
  begin
    actEqualizer.Enabled := False;
    Players.EQEnabled := False;
    AppGlobals.EQEnabled := False;
  end;

  HomeComm := THomeCommunication.Create;

  addStatus := TSWStatusBar.Create(Self);
  addStatus.Parent := Self;

  FClientManager := TClientManager.Create;

  pagMain := TMainPageControl.Create(Self);
  pagMain.Parent := Self;
  pagMain.Visible := True;
  pagMain.Align := alClient;
  pagMain.Images := modSharedData.imgImages;

  tabClients := TClientTab.Create(pagMain, tbClients, ActionList1, FClientManager, mnuStreamPopup);
  tabClients.PageControl := pagMain;
  tabClients.AfterCreate;
  tabClients.AddressBar.Stations.Sort;
  tabClients.AddressBar.Stations.BuildList;
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
  tabClients.SideBar.BrowserView.OnStreamsReceived := tabClientsBrowserViewStreamsReceived;

  tabCharts := TChartsTab.Create(pagMain);
  tabCharts.PageControl := pagMain;
  tabCharts.AfterCreate;
  tabCharts.OnAddToWishlist := tabChartsAddToWishlist;
  tabCharts.OnRemoveTitleFromWishlist := tabChartsRemoveFromWishlist;
  tabCharts.OnAddStreams := tabChartsAddStreams;
  tabCharts.OnGetIsStreamOnListEvent := tabChartsGetIsStreamOnListEvent;

  tabLists := TListsTab.Create(pagMain, FClientManager);
  tabLists.PageControl := pagMain;
  tabLists.AfterCreate;

  tabSaved := TSavedTab.Create(pagMain);
  tabSaved.PageControl := pagMain;
  tabSaved.AfterCreate;

  tabSaved.OnCut := tabSavedCut;
  tabSaved.OnTrackRemoved := tabSavedTrackRemoved;
  tabSaved.OnRefresh := tabSavedRefresh;
  tabSaved.OnPlayStarted := tabPlayStarted;
  tabSaved.OnAddTitleToWishlist := tabSavedAddTitleToWishlist;
  tabSaved.OnRemoveTitleFromWishlist := tabSavedRemoveTitleFromWishlist;
  tabSaved.OnAddTitleToIgnorelist := tabSavedAddTitleToIgnorelist;
  tabSaved.OnRemoveTitleFromIgnorelist  := tabSavedRemoveTitleFromIgnorelist;

  tabLog := TLogTab.Create(pagMain);
  tabLog.PageControl := pagMain;
  tabLog.AfterCreate;

  FWasShown := False;
  FUpdateOnExit := False;

  UpdateStatus;
  addTrayIcon.Visible := AppGlobals.Tray;

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
  HomeComm.OnSSLError := HomeCommSSLError;
  HomeComm.Connect;

  actPlayerIncreaseVolume.Enabled := Bass.DeviceAvailable;
  actPlayerDecreaseVolume.Enabled := Bass.DeviceAvailable;
  actPlayerMuteVolume.Enabled := Bass.DeviceAvailable;
  actEqualizer.Enabled := Bass.DeviceAvailable;

  tabSavedRefresh(nil);

  tmrAutoSave.Enabled := True;
  tmrRecordings.Enabled := True;

  ProcessCommandLine('');

  tmrSpeed.Enabled := True;

  RegisterHotkeys;

  UpdateButtons;

  actShowSideBar.Checked := tabClients.SideBar.Visible;

  MsgBus.AddSubscriber(MessageReceived);

  Language.Translate(Self);

  // This needs to be done for the controls (TLabel in the header) to adjust width
  tabLists.PostTranslate;

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

  FClientManager.RefreshScheduler;
end;

procedure TfrmStreamWriterMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClientManager);
  FreeAndNil(FUpdater);

  inherited;
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

  tabClients.ClientView.ApplyFocus;

  PostMessage(Handle, WM_AFTERSHOWN, 0, 0);
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
  FSSLErrorShown := False;

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

procedure TfrmStreamWriterMain.HomeCommSSLError(Sender: TObject);
begin
  if not FSSLErrorShown then
  begin
    FSSLErrorShown := True;
    TfrmMsgDlg.ShowMsg(Self, _('The certificate received from streamwriter.org could not be validated. ' +
                               'Please go to streamwriter.org and download and install the newest version. If this message continues to pop up afterwards please post to the board.'), mtError, [mbOK], mbOK);
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
      if (((AppGlobals.Data.BrowserList.Count = 0) or (AppGlobals.Data.GenreList.Count = 0)) or (AppGlobals.LastBrowserUpdate < Now - 15)) or
         (tabClients.SideBar.BrowserView.Mode = moError) then
      begin
        if HomeComm.SendGetServerData then
        begin
          tabClients.SideBar.BrowserView.SwitchMode(moLoading);
        end;
      end;
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

  HomeComm.SendSetSettings((AppGlobals.Data.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
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
    8:
      begin
        tabSaved.ToggleShuffle;
      end;
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
  end
  else if Msg is TRefreshServerDataMsg then
  begin
    if HomeComm.SendGetServerData then
      tabClients.SideBar.BrowserView.SwitchMode(moLoading);
  end else if Msg is TSelectSavedSongsMsg then
    pagMain.ActivePage := tabSaved;
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

procedure TfrmStreamWriterMain.PrepareSave;
begin
  // Erst Lists, dann Streams. Reihenfolge ist wichtig!
  tabSaved.Tree.UpdateList;
  tabLists.UpdateLists;
  tabClients.UpdateStreams;
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

  Param := CmdLine.GetParam('-sr');
  if Param <> nil then
  begin
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StopStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil), oaStart);
  end;

  Param := CmdLine.GetParam('-p');
  if Param <> nil then
  begin
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StartStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil), oaPlay, nil, amNoWhere);
  end;

  Param := CmdLine.GetParam('-sp');
  if Param <> nil then
  begin
    for i := 0 to Param.Values.Count - 1 do
      tabClients.StopStreaming(TStartStreamingInfo.Create(0, 0, '', Param.Values[i], nil, nil), oaPlay);
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
  tabClients.SideBar.BrowserView.PostTranslate;
  tabClients.SideBar.InfoView.PostTranslate;
  tabClients.ClientView.PostTranslate;

  NodeData := tabClients.ClientView.GetNodeData(tabClients.ClientView.AutoNode);
  NodeData.Category.Name := _('Automatic recordings');
  tabClients.ClientView.Invalidate;

  tabLists.PostTranslate;
  tabCharts.PostTranslate;
  tabSaved.PostTranslate;
  tabLog.PostTranslate;

  addStatus.Invalidate;
end;

procedure TfrmStreamWriterMain.mnuStreamPopupPopup(Sender: TObject);
begin
  BuildMoveToCategoryMenu;
end;

procedure TfrmStreamWriterMain.mnuStreamsClick(Sender: TObject);
begin
  BuildMoveToCategoryMenu;
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
var
  CmdLine: TCommandLine;
begin
  if not FExiting then
  begin
    CmdLine := TCommandLine.Create(PChar(Msg.CopyDataStruct.lpData));
    if CmdLine.Records.Count = 0 then
      ToggleWindow(True);

    ProcessCommandLine(PChar(Msg.CopyDataStruct.lpData));
  end;
end;

procedure TfrmStreamWriterMain.RegisterHotkeys;
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
    Msg.Result := 0;
    ExitApp(True);
  end;
end;

procedure TfrmStreamWriterMain.SetCaptionAndTrayHint;
var
  NewCaption: string;
  NewHint: string;
  Artist, Title, Stream, Filename: string;
  i, Recordings: Integer;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
begin
  if tabClients <> nil then
    Nodes := tabClients.ClientView.GetNodes(ntClient, False);

  NewCaption := 'streamWriter';
  {$IFDEF DEBUG}NewCaption := NewCaption + ' -: DEBUG BUiLD :- ';{$ENDIF}

  NewHint := 'streamWriter';
  {$IFDEF DEBUG}NewHint := NewHint + ' -: DEBUG BUiLD :- ';{$ENDIF}

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
        NewCaption := NewCaption + ' - ' + RemoveFileExt(ExtractFileName(Filename));
      NewHint := NewHint + #13#10 + _('Playing:') + ' ' + RemoveFileExt(ExtractFileName(Filename));
    end;
  end else if Stream <> '' then
  begin
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
  end;

  Recordings := 0;
  if tabClients <> nil then
    for i := 0 to High(Nodes) do
    begin
      NodeData := tabClients.ClientView.GetNodeData(Nodes[i]);
      if NodeData.Client.Recording then
        Inc(Recordings);
    end;

  if Recordings = 1 then
    NewHint := NewHint + #13#10 + Format(_('%d active recording'), [Recordings])
  else
    NewHint := NewHint + #13#10 + Format(_('%d active recordings'), [Recordings]);
  NewHint := NewHint + #13#10 + MakeSize(FSpeed) + '/s';

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
  StreamSettings: TStreamSettingsArray;
  Clients: TClientArray;
begin
  if AppGlobals.SubmitStats and AppGlobals.MonitorMode then
    OldMonitorCount := AppGlobals.MonitorCount
  else
    OldMonitorCount := 0;

  SetLength(StreamSettings, 1);
  if SettingsType = stApp then
    StreamSettings[0] := AppGlobals.Data.StreamSettings.Copy
  else if SettingsType = stAuto then
    StreamSettings[0] := AppGlobals.Data.AutoRecordSettings.Copy
  else if SettingsType = stStream then
  begin
    Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(ntClientNoAuto, True));
    if Length(Clients) > 0 then
    begin
      SetLength(StreamSettings, Length(Clients));
      for i := 0 to Length(Clients) - 1 do
        StreamSettings[i] := Clients[i].Entry.Settings;
    end else
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

            tabSaved.Tree.SetFileWatcher;

            Language.Translate(Self, PreTranslate, PostTranslate);

            tabClients.AdjustTextSizeDirtyHack;

            tabClients.ShowInfo;

            addTrayIcon.Visible := AppGlobals.Tray;

            ScreenSnap := AppGlobals.SnapMain;

            RegisterHotkeys;

            TLogger.SetFilename(AppGlobals.LogFile);
          end;
        stAuto:
          begin
            AppGlobals.Data.AutoRecordSettings.Assign(S.StreamSettings[0]);

            HomeComm.SendSetSettings((AppGlobals.Data.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
          end;
        stStream:
          begin
            for i := 0 to Length(Clients) - 1 do
              if not Clients[i].AutoRemove then
                Clients[i].Entry.Settings.Assign(S.StreamSettings[i]);
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

  if (AppGlobals.LastUsedDataVersion > 0) and (AppGlobals.LastUsedDataVersion < 60) and (not FExiting) then
    MsgBox(Handle, _('Since handling of settings for automatically saved songs changed, these settings were reset to default values. Please see "Settings"->"Settings for automatic recordings..." in the menu to adjust these settings.'), _('Info'), MB_ICONINFORMATION);

  if (not DirectoryExists(AppGlobals.Dir)) and (not FExiting) then
  begin
    MsgBox(Handle, _('The folder for saved songs does not exist.'#13#10'Please select a folder now.'), _('Info'), MB_ICONINFORMATION);
    ShowSettings(stApp, True);
  end;

  if (not DirectoryExists(AppGlobals.DirAuto)) and (not FExiting) then
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

  if (AppGlobals.LastUsedDataVersion > 0) and (AppGlobals.LastUsedDataVersion < 60) then
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
      addTrayIcon.Visible := True;
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

procedure TfrmStreamWriterMain.tabSavedRemoveTitleFromIgnorelist(
  Sender: TObject; Title: string; TitleHash: Cardinal);
begin
  tabLists.ListsPanel.RemoveEntry(Title, TitleHash, ltIgnore);
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

procedure TfrmStreamWriterMain.tabClientsShowErrorMessage(Sender: TObject; Data: string);
begin
  TfrmMsgDlg.ShowMsg(Self, Data, mtInformation, [mbOK], mbOK);
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
    for i := 0 to AppGlobals.Data.TrackList.Count - 1 do
      Files.Add(TFileEntry.Create(AppGlobals.Data.TrackList[i].Filename, AppGlobals.Data.TrackList[i].Filesize, eaNone));
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

    for n := 0 to AppGlobals.Data.SaveList.Count - 1 do
      if ((Arr[i].Hash = 0) and (AppGlobals.Data.SaveList[n].ServerHash = 0) and (AppGlobals.Data.SaveList[n].Hash = Hash)) or
         ((Arr[i].Hash > 0) and Arr[i].IsArtist and (AppGlobals.Data.SaveList[n].ServerArtistHash = Arr[i].Hash)) or
         ((Arr[i].Hash > 0) and (not Arr[i].IsArtist) and (AppGlobals.Data.SaveList[n].ServerHash = Arr[i].Hash)) then
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

      AppGlobals.Data.SaveList.Add(T);
      tabLists.AddTitle(nil, ltSave, T);

      if Arr[i].Hash > 0 then
      begin
        SetLength(Hashes, Length(Hashes) + 1);
        Hashes[High(Hashes)] := TSyncWishlistRecord.Create(Arr[i].Hash, Arr[i].IsArtist);
      end;
    end;
  end;

  HomeComm.SendSyncWishlist(swAdd, Hashes);
  HomeComm.SendSetSettings((AppGlobals.Data.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
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
    for i := AppGlobals.Data.SaveList.Count - 1 downto 0 do
      if (AppGlobals.Data.SaveList[i].ServerHash > 0) and (Arr[n].Hash > 0) and (not Arr[n].IsArtist) and
         (AppGlobals.Data.SaveList[i].ServerHash = Arr[n].Hash) then
      begin
        tabLists.RemoveTitle(nil, ltSave, AppGlobals.Data.SaveList[i]);
        AppGlobals.Data.SaveList.Delete(i);

        SetLength(Hashes, Length(Hashes) + 1);
        Hashes[High(Hashes)] := TSyncWishlistRecord.Create(Arr[n].Hash, False);
      end;

  HomeComm.SendSyncWishlist(swRemove, Hashes);
  HomeComm.SendSetSettings((AppGlobals.Data.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
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
      List := AppGlobals.Data.SaveList
    else
      List := AppGlobals.Data.IgnoreList
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

      HomeComm.SendSetSettings((AppGlobals.Data.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
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
      List := AppGlobals.Data.SaveList
    else
      List := AppGlobals.Data.IgnoreList
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

  HomeComm.SendSetSettings((AppGlobals.Data.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
end;

procedure TfrmStreamWriterMain.tabClientsAuthRequired(Sender: TObject);
begin
  if MsgBox(Handle, _('You need to be logged in to perform that action.'#13#10'Do you want to login now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
    ShowCommunityLogin;
end;

procedure TfrmStreamWriterMain.tabClientsBrowserViewStreamsReceived(Sender: TObject);
begin
  // Nach einer Neuinstallation können wir noch keine Monitors anfragen, weil wir noch keine Streams kennen.
  // Wenn die Streams angekommen sind, dann machen wir das hier klar!
  if AppGlobals.SubmitStats and AppGlobals.MonitorMode and (AppGlobals.MonitorCount > 0) and (AppGlobals.Data.BrowserList.Count > 0) and (FClientManager.Monitors.Count = 0) then
    HomeComm.SendGetMonitorStreams(AppGlobals.MonitorCount);
end;

procedure TfrmStreamWriterMain.tabClientsUpdateButtons(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmStreamWriterMain.tabSavedTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
begin

end;

procedure TfrmStreamWriterMain.tabCutSaved(Sender: TObject; AudioInfo: TAudioInfo);
var
  i: Integer;
begin
  for i := 0 to AppGlobals.Data.TrackList.Count - 1 do
    if LowerCase(AppGlobals.Data.TrackList[i].Filename) = LowerCase(TCutTab(Sender).Filename) then
    begin
      AppGlobals.Data.TrackList[i].Filesize := GetFileSize(TCutTab(Sender).Filename);
      AppGlobals.Data.TrackList[i].Length := Trunc(AudioInfo.Length);

      // Ist mal raus, damit das "geschnitten"-Symbol nur bei automatischen Aufnahmen kommt
      //FDataLists.TrackList[i].WasCut := True;

      AppGlobals.Data.TrackList[i].Finalized := True;

      AppGlobals.Data.TrackList[i].Bitrate := AudioInfo.Bitrate;
      AppGlobals.Data.TrackList[i].VBR := AudioInfo.VBR;

      tabSaved.Tree.UpdateTrack(AppGlobals.Data.TrackList[i]);

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
  if Application.Terminated or AppGlobals.SkipSave or AppGlobals.Data.LoadError then
    Exit;

  try
    PrepareSave;
    AppGlobals.Data.SaveRecover;
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
      begin
        if FClientManager[i].Entry.ID > 0 then
          L.Add(FClientManager[i].Entry.ID)
        else
          Inc(C);
      end;

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
  if RecordingActive and not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
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

  Power.Critical := (PlayingActive or RecordingActive) or ((AppGlobals.Data.SaveList.Count > 0) and (AppGlobals.AutoTuneIn));
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

  OneNormalRecordingWithTitle, AllNormalStopsAfterSong: Boolean;
  B, OnlyAutomatedSelected, OnlyAutomatedCatsSelected, OnlyAutomaticRecording: Boolean;
  URLFound, OneRecording, OneNotRecording, OnePlaying, OnePaused: Boolean;
  OneHasTitle: Boolean;
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

  if actTuneInStream.Enabled <> B then
    actTuneInStream.Enabled := B;

  if actSavePlaylistStream.Enabled <> B then
    actSavePlaylistStream.Enabled := B;

  if actResetData.Enabled <> ((Length(Clients) > 0) and not OnlyAutomatedSelected) then
    actResetData.Enabled := ((Length(Clients) > 0) and not OnlyAutomatedSelected);

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

  if actTimers.Enabled <> (Length(Clients) = 1) and (not Clients[0].AutoRemove) then
    actTimers.Enabled := (Length(Clients) = 1) and (not Clients[0].AutoRemove);

  if mnuCurrentTitle1.Enabled <> (Length(Clients) > 0) and OneHasTitle then
    mnuCurrentTitle1.Enabled := (Length(Clients) > 0) and OneHasTitle;

  if mnuCurrentTitle2.Enabled <> (Length(Clients) > 0) and OneHasTitle then
    mnuCurrentTitle2.Enabled := (Length(Clients) > 0) and OneHasTitle;

  if actAddToSaveList.Enabled <> (Length(Clients) > 0) and OneHasTitle then
    actAddToSaveList.Enabled := (Length(Clients) > 0) and OneHasTitle;

  if actAddToGlobalIgnoreList.Enabled <> (Length(Clients) > 0) and OneHasTitle then
    actAddToGlobalIgnoreList.Enabled := (Length(Clients) > 0) and OneHasTitle;

  if actAddToStreamIgnoreList.Enabled <> (Length(Clients) > 0) and OneHasTitle then
    actAddToStreamIgnoreList.Enabled := (Length(Clients) > 0) and OneHasTitle;

  cmdPause.Down := OnePaused;
  
  actCopyTitle.Enabled := (Length(Clients) > 0) and OneHasTitle;
end;

procedure TfrmStreamWriterMain.UpdateFound(var Msg: TMessage);
var
  Res: Integer;
begin
  Res := MsgBox(Handle, _('A new version of streamWriter was found.'#13#10'Do you want to download the update now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1);
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

      for n := 0 to AppGlobals.Data.TrackList.Count - 1 do
        if AppGlobals.Data.TrackList[n].Filename = E.Filename then
        begin
          Track := AppGlobals.Data.TrackList[n];
          case E.Action of
            eaNone: ;
            eaSize:
              Track.Filesize := E.Size;
            eaRemove:
              begin
                AppGlobals.Data.TrackList.Delete(n);
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

