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
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WinSock, ICEClient, StdCtrls, ExtCtrls, ImgList, Menus,
  XPMan, VirtualTrees, ComCtrls, ToolWin, ClientView, ICEThread,
  Settings, RecentManager, ActnList, AppData, DragDrop, DropTarget,
  DragDropInternet, DragDropText, DragDropFile, Update, UpdateClient,
  LanguageObjects, AppDataBase, Functions, ClientManager, ShellAPI, DropSource,
  About, MsgDlg, HomeCommunication, StreamBrowserView, Clipbrd,
  StationCombo, GUIFunctions, StreamInfoView, StreamDebugView, Plugins,
  Buttons, DynBass, ClientTab, CutTab, MControls, Tabs, SavedTab,
  CheckFilesThread, ListsTab;

type
  TfrmStreamWriterMain = class(TForm)
    addStatus: TStatusBar;
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
    imgClients: TImageList;
    imgImages: TImageList;
    imgStations: TImageList;
    ActionList1: TActionList;
    actStart: TAction;
    actStop: TAction;
    actRemove: TAction;
    mnuStreamPopup: TPopupMenu;
    mnuStartStreaming1: TMenuItem;
    mnuStopStreaming1: TMenuItem;
    Entfernen1: TMenuItem;
    tmrSpeed: TTimer;
    mnuStreamSettings1: TMenuItem;
    mnuSkipShort: TMenuItem;
    actSkipShort: TAction;
    mnuStreamSettings2: TMenuItem;
    KurzeLiederberspringen1: TMenuItem;
    TrayIcon1: TTrayIcon;
    mnuTray: TPopupMenu;
    mnuShow: TMenuItem;
    N2: TMenuItem;
    Beenden1: TMenuItem;
    actTuneInRelay: TAction;
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
    mnuStreamSettingsToolbar: TPopupMenu;
    Skipshortsongs1: TMenuItem;
    actAbout: TAction;
    N8: TMenuItem;
    N9: TMenuItem;
    View1: TMenuItem;
    mnuShowStreamBrowser: TMenuItem;
    actShowSideBar: TAction;
    actTuneInStream: TAction;
    actTuneInFile: TAction;
    mnuListenToStream2: TMenuItem;
    mnuListenToRelay2: TMenuItem;
    mnuListenToFile2: TMenuItem;
    mnuListenToStream1: TMenuItem;
    mnuListenToRelay1: TMenuItem;
    mnuListenToFile1: TMenuItem;
    actSavePlaylistStream: TAction;
    actSavePlaylistRelay: TAction;
    actSavePlaylistFile: TAction;
    actSavePlaylistStream1: TMenuItem;
    actSavePlaylistRelay1: TMenuItem;
    actSavePlaylistFile1: TMenuItem;
    Stream1: TMenuItem;
    Relay1: TMenuItem;
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
    cmdStreamSettings: TToolButton;
    actCutSave: TAction;
    actCutSaveAs: TAction;
    mnuHelp2: TMenuItem;
    N1: TMenuItem;
    actHelp: TAction;
    N5: TMenuItem;
    mnuWishList1: TMenuItem;
    mnuIgnoreList1: TMenuItem;
    mnuNoList1: TMenuItem;
    N7: TMenuItem;
    mnuNoList2: TMenuItem;
    mnuWishList2: TMenuItem;
    mnuIgnoreList2: TMenuItem;
    actUseNoList: TAction;
    actUseWishlist: TAction;
    actUseIgnoreList: TAction;
    actPlay: TAction;
    actStopPlay: TAction;
    mnuStartPlay2: TMenuItem;
    mnuStopPlay2: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    mnuStartPlay1: TMenuItem;
    mnuStopPlay1: TMenuItem;
    imgLog: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrSpeedTimer(Sender: TObject);
    procedure actStreamSettingsExecute(Sender: TObject);
    procedure addTrayClick(Sender: TObject);
    procedure mnuCheckUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuShowClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mnuStreamPopupPopup(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure mnuStreamSettingsToolbarPopup(Sender: TObject);
    procedure pagSidebarChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actHelpExecute(Sender: TObject);
  private
    FStreams: TDataLists;
    FUpdater: TUpdateClient;
    FUpdateOnExit: Boolean;

    FWasActivated: Boolean;
    FWasShown: Boolean;
    FWasMaximized: Boolean;

    FCheckFiles: TCheckFilesThread;
    FClients: TClientManager;
    FHomeCommunication: THomeCommunication;
    pagMain: TMainPageControl;
    tabClients: TClientTab;
    tabSaved: TSavedTab;
    tabLists: TListsTab;

    procedure OneInstanceMessage(var Msg: TMessage); message WM_USER + 1234;
    procedure QueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;
    procedure EndSession(var Msg: TMessage); message WM_ENDSESSION;
    procedure SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;

    function CanExitApp: Boolean;
    procedure ExitApp(Shutdown: Boolean);
    procedure ShowSettings(BrowseDir: Boolean);
    procedure ShowUpdate(Version: string = ''; UpdateURL: string = '');
    procedure UpdateButtons;
    procedure UpdateStatus;
    procedure ToggleWindow(AlwaysShow: Boolean = False);
    procedure UpdaterUpdateFound(Sender: TObject);
    procedure UpdaterNoUpdateFound(Sender: TObject);
    function HandleLoadError(E: Exception): Integer;
    procedure CheckFilesTerminate(Sender: TObject);

    procedure PreTranslate;
    procedure PostTranslate;

    procedure tabClientsUpdateButtons(Sender: TObject);
    procedure tabClientsCut(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabSavedRefresh(Sender: TObject);

    procedure tabClientsTrackAdded(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabClientsTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
    procedure tabClientsAddIgnoreList(Sender: TObject; Data: string);

    procedure tabSavedTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);

    procedure tabCutSaved(Sender: TObject);
  protected

  public

  end;

implementation

{$R *.dfm}

procedure TfrmStreamWriterMain.ExitApp(Shutdown: Boolean);
var
  i: Integer;
  Res: Integer;
  StartTime: Cardinal;
  Saved, Hard: Boolean;
begin
  AppGlobals.MainMaximized := WindowState = wsMaximized;
  if not AppGlobals.MainMaximized then
  begin
    AppGlobals.MainLeft := Left;
    AppGlobals.MainTop := Top;
    AppGlobals.MainWidth := Width;
    AppGlobals.MainHeight := Height;
  end;

  AppGlobals.ShowSidebar := tabClients.SideBar.Visible;
  AppGlobals.SidebarWidth := tabClients.SideBar.Width;

  for i := 0 to tabClients.ClientView.Header.Columns.Count - 1 do
    AppGlobals.HeaderWidth[i] := tabClients.ClientView.Header.Columns[i].Width;

  TrayIcon1.Visible := False;

  Hide;

  FUpdater.Kill;

  if not Shutdown then
    AppGlobals.Save(Handle)
  else
    try
      AppGlobals.Save;
    except end;

  Saved := False;
  while not Saved do
  begin
    try
      FStreams.Save;
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

  tabClients.ClientView.Clear;

  FClients.Terminate;
  FHomeCommunication.Terminate;

  if FCheckFiles <> nil then
    FCheckFiles.Terminate;

  Hard := False;
  StartTime := GetTickCount;
  while (FClients.Count > 0) or (FHomeCommunication.Count > 0) or (FClients.Active) or (FCheckFiles <> nil) or (FUpdater.Active) do
  begin
    // 5 Sekunden warten, für sauberes beenden
    if StartTime < GetTickCount - 5000 then
    begin
      Hard := True;
      Break;
    end;
    Sleep(100);
    Application.ProcessMessages;
  end;

  if FUpdateOnExit then
    FUpdater.RunUpdate(Handle);

  if Hard then
    Halt
  else
    Application.Terminate;
end;

procedure TfrmStreamWriterMain.actSettingsExecute(Sender: TObject);
begin
  ShowSettings(False);
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

procedure TfrmStreamWriterMain.actExitExecute(Sender: TObject);
begin
  if CanExitApp then
    ExitApp(False);
end;

procedure TfrmStreamWriterMain.actHelpExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(AppGlobals.ProjectHelpLink), '', '', 1);
end;

procedure TfrmStreamWriterMain.actStreamSettingsExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
  R: TStreamEntry;
begin
  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(True));
  for Client in Clients do
  begin
    if Sender = actSkipShort then
      Client.SetSettings(actSkipShort.Checked);

    if Sender = actUseNoList then
      Client.UseFilter := ufNone;
    if Sender = actUseWishlist then
      Client.UseFilter := ufWish;
    if Sender = actUseIgnoreList then
      Client.UseFilter := ufIgnore;

    R := FStreams.StreamList.Get(Client);
    if R <> nil then
    begin
      R.SkipShort := Client.SkipShort;
      R.UseFilter := Client.UseFilter;
    end;
  end;

  UpdateButtons; // Damit die Entries im Hauptmenü angepasst werden, falls von Popup was geändert wurde.
end;

procedure TfrmStreamWriterMain.addTrayClick(Sender: TObject);
begin
  ToggleWindow(False);
end;

procedure TfrmStreamWriterMain.FormActivate(Sender: TObject);
begin
  if FWasActivated then
    Exit;

  FWasActivated := True;

  if not DirectoryExists(AppGlobals.Dir) then
  begin
    MsgBox(Handle, _('The folder for saved songs does not exist.'#13#10'Please select a folder now.'), _('Info'), MB_ICONINFORMATION);
    ShowSettings(True);
  end;

  if not BassLoaded then
  begin
    MsgBox(Handle, _('Bass.dll could not be loaded. Without this library no playback/cutting/searching for silence is available.'), _('Info'), MB_ICONINFORMATION);
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
begin
  FClients := TClientManager.Create;

  FStreams := TDataLists.Create;

  FHomeCommunication := THomeCommunication.Create;

  pagMain := TMainPageControl.Create(Self);
  pagMain.Parent := Self;
  pagMain.Visible := True;
  pagMain.Align := alClient;
  pagMain.Images := imgImages;

  tabClients := TClientTab.Create(pagMain);
  tabClients.PageControl := pagMain;
  tabClients.Setup(tbClients, ActionList1, mnuStreamPopup, imgImages, imgClients,
    FClients, FStreams, FHomeCommunication);
  tabClients.SideBar.BrowserView.StreamTree.Images := imgStations;
  tabClients.AddressBar.Stations.Images := imgStations;
  tabClients.SideBar.DebugView.DebugView.DebugView.Images := imgLog;
  tabClients.OnUpdateButtons := tabClientsUpdateButtons;
  tabClients.OnCut := tabClientsCut;
  tabClients.OnTrackAdded := tabClientsTrackAdded;
  tabClients.OnTrackRemoved := tabClientsTrackRemoved;
  tabClients.OnAddIgnoreList := tabClientsAddIgnoreList;

  tabSaved := TSavedTab.Create(pagMain);
  tabSaved.PageControl := pagMain;
  tabSaved.OnCut := tabClientsCut;
  tabSaved.OnTrackRemoved := tabSavedTrackRemoved;
  tabSaved.OnRefresh := tabSavedRefresh;

  tabLists := TListsTab.Create(pagMain);
  tabLists.PageControl := pagMain;

  //if AppGlobals.Relay then
  //  FClients.RelayServer.Start;

  FWasActivated := False;
  FWasShown := False;
  FUpdateOnExit := False;

  try
    FStreams.Load;
  except
    on E: Exception do
    begin
      try
        FStreams.Free;
      except end;
      FStreams := TDataLists.Create;
      // Damit nichts überschrieben wird.
      FStreams.LoadError := True;

      if HandleLoadError(E) = IDYES then
      begin
        DeleteFile(E.Message);
        FStreams.LoadError := False;
      end;
    end;
  end;

  // Ist hier unten, weil hier erst Tracks geladen wurden
  tabSaved.Setup(FStreams, imgImages);

  tabClients.ClientView.SortItems;
  tabClients.AddressBar.Stations.Sort;

  {$IFDEF DEBUG}Caption := Caption + ' --::: DEBUG BUiLD :::--';{$ENDIF}

  UpdateButtons;
  UpdateStatus;
  tmrSpeed.Enabled := True;
  TrayIcon1.Visible := AppGlobals.Tray;

  FUpdater := TUpdateClient.Create;
  FUpdater.OnNoUpdateFound := UpdaterNoUpdateFound;
  FUpdater.OnUpdateFound := UpdaterUpdateFound;
  if (AppGlobals.AutoUpdate) and (AppGlobals.LastUpdateChecked + 1 < Now) then
    FUpdater.Start(uaVersion);

  Width := AppGlobals.MainWidth;
  Height := AppGlobals.MainHeight;
  if (AppGlobals.MainLeft = -1) or (AppGlobals.MainTop = -1) then
  begin
    AppGlobals.MainLeft := Screen.Width div 2 - Width div 2;
    AppGlobals.MainTop := Screen.Height div 2 - Height div 2;
  end;
  Left := AppGlobals.MainLeft;
  Top := AppGlobals.MainTop;

  Language.Translate(Self);
end;

procedure TfrmStreamWriterMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClients);
  FreeAndNil(FHomeCommunication);
  FreeAndNil(FUpdater);
  FreeAndNil(FStreams);
end;

procedure TfrmStreamWriterMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    actHelp.Execute;
end;

procedure TfrmStreamWriterMain.FormShow(Sender: TObject);
begin
  if FWasShown then
    Exit;

  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);

  FWasShown := True;

  tabSavedRefresh(nil);

  AppGlobals.WindowHandle := Handle;

  if AppGlobals.MainMaximized then
    WindowState := wsMaximized;
  FWasMaximized := WindowState = wsMaximized;

  tabClients.Shown;
  tabLists.Setup(FStreams, imgImages);
  actShowSideBar.Checked := tabClients.SideBar.Visible;
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

procedure TfrmStreamWriterMain.mnuCheckUpdateClick(Sender: TObject);
begin
  ShowUpdate;
end;

procedure TfrmStreamWriterMain.mnuShowClick(Sender: TObject);
begin
  ToggleWindow(False);
end;

procedure TfrmStreamWriterMain.OneInstanceMessage(var Msg: TMessage);
begin
  ToggleWindow(True);
end;

procedure TfrmStreamWriterMain.pagSidebarChange(Sender: TObject);
begin
  // Damit Child-Controls passende Dimensionen in ShowInfo haben
  Application.ProcessMessages;
end;

procedure TfrmStreamWriterMain.PreTranslate;
begin

end;

procedure TfrmStreamWriterMain.PostTranslate;
begin
  tabClients.SideBar.BrowserView.Translate;
  tabClients.SideBar.InfoView.Translate;
end;

procedure TfrmStreamWriterMain.mnuStreamPopupPopup(Sender: TObject);
begin
  UpdateButtons;
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

procedure TfrmStreamWriterMain.EndSession(var Msg: TMessage);
begin
  if WordBool(Msg.WParam) then
  begin
    Msg.Result := 1;
    ExitApp(True);
  end;
end;

procedure TfrmStreamWriterMain.ShowSettings(BrowseDir: Boolean);
var
  S: TfrmSettings;
begin
  S := TfrmSettings.Create(Self, BrowseDir);
  S.ShowModal;
  Language.Translate(Self, PreTranslate, PostTranslate);
  AppGlobals.PluginManager.ReInitPlugins;
  {
  if S.RelayChanged then
  begin
    if AppGlobals.Relay then
      FClients.RelayServer.Start
    else
      FClients.RelayServer.Stop;
  end;
  }
  TrayIcon1.Visible := AppGlobals.Tray;
  S.Free;
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

procedure TfrmStreamWriterMain.tabClientsCut(Entry: TStreamEntry;
  Track: TTrackInfo);
var
  tabCut: TCutTab;
begin
  if LowerCase(ExtractFileExt(Track.Filename)) <> '.mp3' then
  begin
    Exit;
  end;

  tabCut := TCutTab(pagMain.FindCut(Track.Filename));
  if tabCut = nil then
  begin
    tabCut := TCutTab.Create(pagMain);
    tabCut.PageControl := pagMain;
    tabCut.OnSaved := tabCutSaved;

    pagMain.ActivePage := tabCut;

    tabCut.Setup(Track.Filename, imgImages);
  end else
  begin
    pagMain.ActivePageIndex := tabCut.PageIndex;
  end;
end;

procedure TfrmStreamWriterMain.tabSavedRefresh(Sender: TObject);
var
  i, n: Integer;
  Files: TList;
begin
  if FCheckFiles <> nil then
    Exit;

  Files := TList.Create;
  try
    for i := 0 to FStreams.StreamList.Count - 1 do
      for n := 0 to FStreams.StreamList[i].Tracks.Count - 1 do
        Files.Add(TFileEntry.Create(FStreams.StreamList[i].Tracks[n].Filename, FStreams.StreamList[i].Tracks[n].Filesize, eaNone));
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
  tabSaved.AddTrack(Entry, Track);
end;

procedure TfrmStreamWriterMain.tabClientsTrackRemoved(Entry: TStreamEntry;
  Track: TTrackInfo);
begin
  tabSaved.RemoveTrack(Track);
end;

procedure TfrmStreamWriterMain.tabClientsAddIgnoreList(Sender: TObject;
  Data: string);
var
  Ignore: TTitleInfo;
  i, NumChars: Integer;
  Pattern: string;
  Hash: Cardinal;
begin
  if AppGlobals.AddSavedToIgnore then
  begin
    Pattern := BuildPattern(Data, Hash, NumChars);
    if NumChars > 3 then
    begin
      for i := 0 to FStreams.IgnoreList.Count - 1 do
        if FStreams.IgnoreList[i].Hash = Hash then
          Exit;

      Ignore := TTitleInfo.Create(Data);
      FStreams.IgnoreList.Add(Ignore);
      tabLists.AddIgnore(Ignore);
    end;
  end;
end;

procedure TfrmStreamWriterMain.tabClientsUpdateButtons(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmStreamWriterMain.tabSavedTrackRemoved(Entry: TStreamEntry; Track: TTrackInfo);
begin

end;

procedure TfrmStreamWriterMain.tabCutSaved(Sender: TObject);
var
  i, n: Integer;
begin
  for i := 0 to FStreams.StreamList.Count - 1 do
    for n := 0 to FStreams.StreamList[i].Tracks.Count - 1 do
      if LowerCase(FStreams.StreamList[i].Tracks[n].Filename) = LowerCase(TCutTab(Sender).Filename) then
      begin
        FStreams.StreamList[i].Tracks[n].Filesize := GetFileSize(FStreams.StreamList[i].Tracks[n].Filename);
        FStreams.StreamList[i].Tracks[n].WasCut := True;
        Exit;
      end;
end;

procedure TfrmStreamWriterMain.tmrSpeedTimer(Sender: TObject);
var
  Active: Boolean;
  i: Integer;
begin
  UpdateStatus;

  tabClients.TimerTick;

  Active := False;
  for i := 0 to FClients.Count - 1 do
    if FClients[i].Recording then
    begin
      Active := True;
      Break;
    end;

  if Active and not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
  begin
    for i := 0 to FClients.Count - 1 do
      FClients[i].StopRecording;
    tmrSpeed.Enabled := False;
    MsgBox(Handle, _('Available disk space is below the set limit, so recording will be stopped.'), _('Info'), MB_ICONINFORMATION);
    tmrSpeed.Enabled := True;
  end;
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
  B, B4, B5: Boolean;
  UseFilter: TUseFilters;
  Clients: TClientArray;
  Client, Client2: TICEClient;
begin
  Clients := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(True));
  B := Length(Clients) > 0;
  actStart.Enabled := B;
  actStop.Enabled := B;
  mnuStartStreaming1.Default := False;
  mnuStopStreaming1.Default := False;
  actRemove.Enabled := B;
  mnuStreamSettings1.Enabled := B;
  mnuStreamSettings2.Enabled := B;
  cmdStreamSettings.Enabled := B;

  mnuTuneIn1.Enabled := B;
  mnuTuneIn2.Enabled := B;
  mnuSavePlaylist1.Enabled := B;
  mnuSavePlaylist2.Enabled := B;

  actResetData.Enabled := B;
  actTuneInRelay.Enabled := False;
  actTuneInFile.Enabled := False;

  actUseNoList.Checked := False;
  actUseWishlist.Checked := False;
  actUseIgnoreList.Checked := False;

  B4 := BassLoaded;
  for Client in Clients do
  begin
    //if Client.Active then
    //  if AppGlobals.Relay then
    //    actTuneInRelay.Enabled := True;
    if Client.Filename <> '' then
      actTuneInFile.Enabled := True;
    if Client.Playing then
      B4 := True;
  end;

  actPlay.Enabled := False;
  actStopPlay.Enabled := B4;

  if tabClients.ClientView.SelectedCount > 1 then
  begin
    Client2 := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(True))[0];
    UseFilter := Client2.UseFilter;
    B4 := True;
    B5 := True;
    for Client in tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(True)) do
    begin
      if not (Client.SkipShort = Client2.SkipShort) then
        B4 := False;
      if not (Client.UseFilter = Client2.UseFilter) then
        B5 := False;
    end;
    Client := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(True))[0];
    actSkipShort.Checked := Client.SkipShort and B4;
    if B5 then
    begin
      case UseFilter of
        ufNone:
          actUseNoList.Checked := True;
        ufWish:
          actUseWishlist.Checked := True;
        ufIgnore:
          actUseIgnoreList.Checked := True;
      end;
    end;
  end else if tabClients.ClientView.SelectedCount = 1 then
  begin
    Client := tabClients.ClientView.NodesToClients(tabClients.ClientView.GetNodes(True))[0];
    actSkipShort.Checked := Client.SkipShort;

    actPlay.Enabled := BassLoaded;
    //actStopPlay.Enabled := True;

    case Client.UseFilter of
      ufNone:
        actUseNoList.Checked := True;
      ufWish:
        actUseWishlist.Checked := True;
      ufIgnore:
        actUseIgnoreList.Checked := True;
    end;

    case AppGlobals.DefaultAction of
      caStartStop:
        if Client.Recording then
          mnuStopStreaming1.Default := True
        else
          mnuStartStreaming1.Default := True;
      caStreamIntegrated:
        if Client.Playing then
          mnuStopPlay1.Default := True
        else
          mnuStartPlay1.Default := True;
      caStream:
        mnuListenToStream1.Default := True;
      {
      caRelay:
        mnuListenToRelay1.Default := True;
      }
      caFile:
        mnuListenToFile1.Default := True;
    end;
  end;
end;

procedure TfrmStreamWriterMain.UpdaterNoUpdateFound(Sender: TObject);
begin
  AppGlobals.LastUpdateChecked := Trunc(Now);
end;

procedure TfrmStreamWriterMain.UpdaterUpdateFound(Sender: TObject);
var
  Res: Integer;
begin
  AppGlobals.LastUpdateChecked := Trunc(Now);
  Res := MsgBox(Handle, _('A new version was found.'#13#10'Do you want to download the update now?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
  if Res = IDYES then
  begin
    if AppGlobals.RunningFromInstalledLocation then
      ShowUpdate(FUpdater.FoundVersion.AsString, FUpdater.UpdateURL)
    else
      ShellExecute(Handle, 'open', PChar(AppGlobals.ProjectLink), nil, nil, 1);
  end;
end;

procedure TfrmStreamWriterMain.UpdateStatus;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Speed: UInt64;
begin
  Speed := 0;
  Clients := tabClients.ClientView.NodesToData(tabClients.ClientView.GetNodes(False));
  for Client in Clients do
  begin
    Speed := Speed + Client.Client.Speed;
    tabClients.ClientView.RefreshClient(Client.Client);
  end;
  addStatus.Panels[0].Text := MakeSize(Speed) + '/s';
  addStatus.Panels[1].Text := Format(_('%s/%s received'), [MakeSize(tabClients.Received), MakeSize(FStreams.Received)]);
  addStatus.Panels[2].Text := Format(_('%d songs saved'), [FClients.SongsSaved]);
end;

function TfrmStreamWriterMain.CanExitApp: Boolean;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Rec: Boolean;
begin
  Result := True;
  Rec := False;

  Clients := tabClients.ClientView.NodesToData(tabClients.ClientView.GetNodes(False));

  for Client in Clients do
    if Client.Client.Recording then
    begin
      Rec := True;
      Break;
    end;

  if Rec then
  begin
    if TfrmMsgDlg.ShowMsg(Self, _('You are recording at least one stream at the moment. Exiting the application will abort streaming.'#13#10'Do you really want to quit?'), 2, 1) = mtCancel then
    begin
      Result := False;
    end;
  end;
end;

procedure TfrmStreamWriterMain.CheckFilesTerminate(Sender: TObject);
var
  i, n, j: Integer;
  Found: Boolean;
  Track: TTrackInfo;
  E: TFileEntry;
begin
  for i := 0 to FCheckFiles.Files.Count - 1 do
  begin
    E := TFileEntry(FCheckFiles.Files[i]);

    if E.Action = eaNone then
      Continue;

    for n := 0 to FStreams.StreamList.Count - 1 do
    begin
      Found := False;
      for j := FStreams.StreamList[n].Tracks.Count - 1 downto 0 do
      begin
        if FStreams.StreamList[n].Tracks[j].Hash = E.Hash then
        begin
          Track := FStreams.StreamList[n].Tracks[j];
          case E.Action of
            eaNone: ;
            eaSize:
              Track.Filesize := E.Size;
            eaRemove:
              begin
                FStreams.StreamList[n].Tracks.Delete(j);
                tabSaved.RemoveTrack(Track);
                Track.Free;
              end;
          end;
          Found := True;
        end;
        if Found then
          Break;
      end;
      if Found then
        Break;
    end;
  end;

  FCheckFiles := nil;
end;

end.
