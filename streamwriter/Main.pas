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
  Buttons, DynBass;

type
  TfrmStreamWriterMain = class(TForm)
    addStatus: TStatusBar;
    pnlStreams: TPanel;
    pnlTop: TPanel;
    lblTop: TLabel;
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
    DropStations: TDropURLTarget;
    DropList: TDropURLTarget;
    ToolBar2: TToolBar;
    cmdStartStreaming: TToolButton;
    mnuStreamSettings1: TMenuItem;
    asd: TMenuItem;
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
    ToolBar1: TToolBar;
    cmdStart: TToolButton;
    cmdStop: TToolButton;
    cmdRemove: TToolButton;
    actExit: TAction;
    actSettings: TAction;
    cmdStreamSettings: TToolButton;
    mnuStreamSettingsToolbar: TPopupMenu;
    Savetitlestoseperatefolder1: TMenuItem;
    Skipshortsongs1: TMenuItem;
    actAbout: TAction;
    cmdSettings: TToolButton;
    N8: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    N9: TMenuItem;
    View1: TMenuItem;
    mnuShowStreamBrowser: TMenuItem;
    ToolButton4: TToolButton;
    cmdShowStreamBrowser: TToolButton;
    actShowStreamBrowser: TAction;
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
    imgSavedTracks: TImageList;
    mnuReset1: TMenuItem;
    mnuReset11: TMenuItem;
    actResetData: TAction;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton2: TToolButton;
    cmdStartPlay: TToolButton;
    cmdStopPlay: TToolButton;
    pagSidebar: TPageControl;
    tabBrowser: TTabSheet;
    tabInfo: TTabSheet;
    tabDebug: TTabSheet;
    Splitter1: TSplitter;
    procedure cmdStartStreamingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure tmrSpeedTimer(Sender: TObject);
    procedure lstStationsKeyPress(Sender: TObject; var Key: Char);
    procedure DropStationsDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure DropListDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure actStreamSettingsExecute(Sender: TObject);
    procedure addTrayClick(Sender: TObject);
    procedure mnuCheckUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuShowClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actTuneInRelayExecute(Sender: TObject);
    procedure mnuStreamPopupPopup(Sender: TObject);
    procedure lstStationsChange(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure mnuStreamSettingsToolbarPopup(Sender: TObject);
    procedure cmdStreamSettingsClick(Sender: TObject);
    procedure actShowStreamBrowserExecute(Sender: TObject);
    procedure actTuneInStreamExecute(Sender: TObject);
    procedure actTuneInFileExecute(Sender: TObject);
    procedure actSavePlaylistStreamExecute(Sender: TObject);
    procedure actSavePlaylistRelayExecute(Sender: TObject);
    procedure actSavePlaylistFileExecute(Sender: TObject);
    procedure tabInfoResize(Sender: TObject);
    procedure pagSidebarChange(Sender: TObject);
    procedure actResetDataExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FStreams: TStreamDataList;
    FUpdater: TUpdateClient;
    FReceived: UInt64;
    FUpdateOnExit: Boolean;
    FRefreshInfo: Boolean;
    FWasShown: Boolean;
    FWasMaximized: Boolean;

    FClients: TClientManager;
    FHomeCommunication: THomeCommunication;
    lstClients: TMClientView;
    pnlStreamBrowser: TMStreamBrowserView;
    lstStations: TMStationCombo;
    pnlStreamInfo: TMStreamInfoContainer;
    pnlDebug: TMStreamDebugContainer;

    procedure OneInstanceMessage(var Msg: TMessage); message WM_USER + 123;
    procedure QueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;
    procedure EndSession(var Msg: TMessage); message WM_ENDSESSION;
    procedure SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;

    function CanExitApp: Boolean;
    procedure ExitApp(Shutdown: Boolean);
    procedure ShowSettings(BrowseDir: Boolean);
    procedure ShowUpdate(Version: string = ''; UpdateURL: string = '');
    procedure SavePlaylist(Entries: TPlaylistEntryArray; Open: Boolean);
    procedure UpdateButtons;
    procedure UpdateStatus;
    procedure ToggleWindow(AlwaysShow: Boolean = False);
    procedure UpdaterUpdateFound(Sender: TObject);
    procedure UpdaterNoUpdateFound(Sender: TObject);
    function HandleLoadError(E: Exception): Integer;
    function StartStreaming(Name, URL: string): Boolean;
    procedure ShowInfo;
    procedure PreTranslate;
    procedure PostTranslate;

    procedure lstClientsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure lstClientsDblClick(Sender: TObject);
    procedure lstClientsKeyPress(Sender: TObject; var Key: Char);
    procedure lstClientsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure StreamsStreamAdded(Sender: TObject; Stream: TStreamEntry);
    procedure StreamsStreamRemoved(Sender: TObject; Stream: TStreamEntry);
    procedure StreamsStreamChanged(Sender: TObject; Stream: TStreamEntry);

    procedure DebugClear(Sender: TObject);

    procedure ClientManagerDebug(Sender: TObject);
    procedure ClientManagerRefresh(Sender: TObject);
    procedure ClientManagerAddRecent(Sender: TObject);
    procedure ClientManagerClientAdded(Sender: TObject);
    procedure ClientManagerClientRemoved(Sender: TObject);
    procedure ClientManagerSongSaved(Sender: TObject; Filename, Title: string);
    procedure ClientManagerTitleChanged(Sender: TObject; Title: string);
    procedure ClientManagerICYReceived(Sender: TObject; Received: Integer);

    procedure StreamBrowserAction(Sender: TObject; Action: TOpenActions; Streams: TStreamDataArray);
    procedure StreamInfoAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
  protected

  public

  end;

implementation

{$R *.dfm}

procedure TfrmStreamWriterMain.cmdStartStreamingClick(Sender: TObject);
begin
  StartStreaming(lstStations.Text, lstStations.Text);
end;

procedure TfrmStreamWriterMain.cmdStreamSettingsClick(Sender: TObject);
var
  Point: TPoint;
begin
  Point.X := cmdStreamSettings.Left;
  Point.Y := cmdStreamSettings.Top + cmdStreamSettings.Height;
  Point := ClientToScreen(Point);
  cmdStreamSettings.Down := True;
  cmdStreamSettings.DropDownMenu.Popup(Point.X, Point.Y);
  cmdStreamSettings.Down := False;
end;

procedure TfrmStreamWriterMain.DebugClear(Sender: TObject);
var
  Clients: TNodeDataArray;
  i: Integer;
begin
  Clients := lstClients.NodesToData(lstClients.GetNodes(False));
  for i := 0 to Length(Clients) - 1 do
    if Clients[i].Client = pnlDebug.DebugView.Client then
    begin
      Clients[i].Client.DebugLog.Clear;
      Break;
    end;
end;

procedure TfrmStreamWriterMain.DropListDrop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
var
  DropURL: string;
begin
  DropURL := string(DropList.URL);
  StartStreaming('', DropURL);
end;

procedure TfrmStreamWriterMain.DropStationsDrop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
begin
  lstStations.ItemIndex := -1;
  lstStations.Text := string(DropStations.URL);
end;

procedure TfrmStreamWriterMain.ExitApp(Shutdown: Boolean);
var
  i: Integer;
  Res: Integer;
  Clients: TNodeDataArray;
  StartTime: Cardinal;
  Saved: Boolean;
begin
  Clients := lstClients.NodesToData(lstClients.GetNodes(False));

  AppGlobals.MainMaximized := WindowState = wsMaximized;
  if not AppGlobals.MainMaximized then
  begin
    AppGlobals.MainLeft := Left;
    AppGlobals.MainTop := Top;
    AppGlobals.MainWidth := Width;
    AppGlobals.MainHeight := Height;
  end;

  AppGlobals.ShowSidebar := pagSideBar.Visible;
  AppGlobals.SidebarWidth := pagSideBar.Width;

  for i := 0 to lstClients.Header.Columns.Count - 1 do
    AppGlobals.HeaderWidth[i] := lstClients.Header.Columns[i].Width;

  TrayIcon1.Visible := False;

  Hide;

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
      if FStreams.Save then
      begin
        Saved := True;

        // Diese Zeilen sollten irgendwann raus, genau so wie die
        // Variablen in AppGlobals.
        DeleteFile(AppGlobals.ListFile);
        DeleteFile(AppGlobals.RecentFile);
      end;
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

  lstClients.Clear;

  FClients.Terminate;
  FHomeCommunication.Terminate;

  StartTime := GetTickCount;
  while (FClients.Count > 0) or (FHomeCommunication.Count > 0) or (FClients.Active) do
  begin
    // 5 Sekunden warten, für sauberes beenden
    if StartTime < GetTickCount - 5000 then
      Halt;
    Sleep(100);
    Application.ProcessMessages;
  end;

  if FUpdateOnExit then
    FUpdater.RunUpdate(Handle);

  Application.Terminate;
end;

procedure TfrmStreamWriterMain.actSavePlaylistFileExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := lstClients.GetEntries(etFile);
  SavePlaylist(Entries, False);
end;

procedure TfrmStreamWriterMain.actSavePlaylistRelayExecute(
  Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := lstClients.GetEntries(etRelay);
  SavePlaylist(Entries, False);
end;

procedure TfrmStreamWriterMain.actSavePlaylistStreamExecute(
  Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := lstClients.GetEntries(etStream);
  SavePlaylist(Entries, False);
end;

procedure TfrmStreamWriterMain.actSettingsExecute(Sender: TObject);
begin
  ShowSettings(False);
end;

procedure TfrmStreamWriterMain.actShowStreamBrowserExecute(
  Sender: TObject);
begin
  pagSidebar.Visible := not pagSidebar.Visible;
  Splitter1.Visible := not Splitter1.Visible;
  actShowStreamBrowser.Checked := pagSidebar.Visible;
end;

procedure TfrmStreamWriterMain.actStartExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
  Entry: TStreamEntry;
begin
  if not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
  begin
    MsgBox(Handle, _('Available disk space is below the set limit, so recording will not start.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  Clients := lstClients.NodesToClients(lstClients.GetNodes(True));
  for Client in Clients do
  begin
    Entry := FStreams.Get(Client);
    if Entry <> nil then
      Entry.LastTouched := Now;
    Client.Connect;
  end;
end;

procedure TfrmStreamWriterMain.actStopExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := lstClients.NodesToClients(lstClients.GetNodes(True));
  for Client in Clients do
  begin
    Client.Disconnect;
  end;
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

procedure TfrmStreamWriterMain.actStreamSettingsExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
  R: TStreamEntry;
begin
  Clients := lstClients.NodesToClients(lstClients.GetNodes(True));
  for Client in Clients do
  begin
    if Sender = actSkipShort then
      Client.SetSettings(actSkipShort.Checked);

    R := FStreams.Get(Client);
    if R <> nil then
    begin
      R.SkipShort := Client.SkipShort;
    end;
  end;
end;

procedure TfrmStreamWriterMain.actTuneInRelayExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  UpdateButtons;
  if actTuneInRelay.Enabled then
  begin
    Entries := lstClients.GetEntries(etRelay);
    SavePlaylist(Entries, True);
  end;
end;

procedure TfrmStreamWriterMain.actTuneInStreamExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := lstClients.GetEntries(etStream);
  SavePlaylist(Entries, True);
end;

procedure TfrmStreamWriterMain.actTuneInFileExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  UpdateButtons;
  if actTuneInFile.Enabled then
  begin
    Entries := lstClients.GetEntries(etFile);
    SavePlaylist(Entries, True);
  end;
end;

procedure TfrmStreamWriterMain.addTrayClick(Sender: TObject);
begin
  ToggleWindow(False);
end;

procedure TfrmStreamWriterMain.actRemoveExecute(Sender: TObject);
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
begin
  Clients := lstClients.NodesToData(lstClients.GetNodes(True));
  for Client in Clients do
  begin
    FClients.RemoveClient(Client.Client);
  end;
end;

procedure TfrmStreamWriterMain.actResetDataExecute(Sender: TObject);
var
  Res: Integer;
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  R: TStreamEntry;
  i: Integer;
begin
  Res := MsgBox(Handle, _('This will reset the saved song and bytes received counters and information about saved songs.'#13#10 +
                          'The tracknumber of new saved titles will be 1 if you specified the tracknumber in the filename pattern, this number will also be set in ID3 tags.'#13#10 +
                          'Do you want to continue?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
  if Res = IDYES then
  begin
    Clients := lstClients.NodesToData(lstClients.GetNodes(True));
    for Client in Clients do
    begin
      R := FStreams.Get(Client.Client);
      R.SongsSaved := 0;
      R.BytesReceived := 0;
      for i := 0 to R.Tracks.Count - 1 do
        R.Tracks[i].Free;
      R.Tracks.Clear;

      Client.Client.SongsSaved := 0;
      Client.Client.Received := 0;

      lstClients.RefreshClient(Client.Client);
    end;
  end;
  ShowInfo;
end;

procedure TfrmStreamWriterMain.FormActivate(Sender: TObject);
begin
  if not DirectoryExists(AppGlobals.Dir) then
  begin
    MsgBox(Handle, _('The folder for saved songs does not exist.'#13#10'Please select a folder now.'), _('Info'), MB_ICONINFORMATION);
    ShowSettings(True);
  end;
end;

procedure TfrmStreamWriterMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
  if AppGlobals.TrayClose then
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
  Recent: TRecent;
  List: TListList;
  Entry: TStreamEntry;
  i: Integer;
begin
  FClients := TClientManager.Create;
  FClients.OnClientDebug := ClientManagerDebug;
  FClients.OnClientRefresh := ClientManagerRefresh;
  FClients.OnClientAddRecent := ClientManagerAddRecent;
  FClients.OnClientAdded := ClientManagerClientAdded;
  FClients.OnClientRemoved := ClientManagerClientRemoved;
  FClients.OnClientSongSaved := ClientManagerSongSaved;
  FClients.OnClientTitleChanged := ClientManagerTitleChanged;
  FClients.OnClientICYReceived := ClientManagerICYReceived;

  FHomeCommunication := THomeCommunication.Create;

  if AppGlobals.Relay then
    FClients.RelayServer.Start;

  FWasShown := False;
  FReceived := 0;
  FUpdateOnExit := False;
  FRefreshInfo := False;

  lstStations := TMStationCombo.Create(Self);
  lstStations.Parent := pnlTop;
  lstStations.DropDownCount := 15;
  lstStations.Left := lblTop.Left + lblTop.Width + 8;
  lstStations.Images := imgStations;
  lstStations.Top := 4;
  lstStations.Width := pnlTop.ClientWidth - lstStations.Left - cmdStartStreaming.Width - 8;
  lstStations.Anchors := [akLeft, akTop, akRight];
  lstStations.OnKeyPress := lstStationsKeyPress;
  lstStations.OnChange := lstStationsChange;

  lstClients := TMClientView.Create(Self, mnuStreamPopup);
  lstClients.Parent := pnlStreams;
  lstClients.PopupMenu := mnuStreamPopup;
  lstClients.Align := alClient;
  lstClients.Images := imgClients;
  lstClients.OnChange := lstClientsChange;
  lstClients.OnDblClick := lstClientsDblClick;
  lstClients.OnKeyPress := lstClientsKeyPress;
  lstClients.OnKeyDown := lstClientsKeyDown;
  lstClients.Show;

  pnlStreamBrowser := TMStreamBrowserView.Create(tabBrowser, FHomeCommunication);
  pnlStreamBrowser.Parent := tabBrowser;
  pnlStreamBrowser.Left := 0;
  pnlStreamBrowser.Align := alClient;

  pnlStreamBrowser.StreamTree.OnAction := StreamBrowserAction;
  pnlStreamBrowser.StreamTree.Images := imgStations;

  pnlStreamBrowser.Show;

  pnlStreamInfo := TMStreamInfoContainer.Create(Self);
  pnlStreamInfo.InfoView.Tree.OnAction := StreamInfoAction;
  pnlStreamInfo.InfoView.Tree.Images := imgSavedTracks;
  pnlStreamInfo.Parent := tabInfo;
  pnlStreamInfo.Show;

  pnlDebug := TMStreamDebugContainer.Create(Self);
  pnlDebug.Parent := tabDebug;
  pnlDebug.DebugView.OnClear := DebugClear;
  pnlDebug.Show;

  DropStations.Register(lstStations);
  DropList.Register(lstClients);

  pagSidebar.ActivePageIndex := 0;

  FStreams := TStreamDataList.Create;
  FStreams.OnStreamAdded := StreamsStreamAdded;
  FStreams.OnStreamRemoved := StreamsStreamRemoved;
  FStreams.OnStreamChanged := StreamsStreamChanged;

  try
    FStreams.Load;
  except
      on E: Exception do
      begin
        try
          FStreams.Free;
        except end;
        FStreams := TStreamDataList.Create;

        if HandleLoadError(E) = IDYES then
        begin
          DeleteFile(E.Message);
          FStreams.LoadError := False;
        end;
      end;
  end;

  Recent := TRecent.Create;
  try
    try
      Recent.Load;
      for i := 0 to Recent.List.Count - 1 do
      begin
        Entry := FStreams.Add(Recent.List[i].Copy);
        Entry.RecentIndex := i;
        Entry.IsInList := False;
      end;
    except
    end;
  finally
    Recent.Free;
  end;

  List := TListList.Create;
  try
    try
      List.Load;
      for i := 0 to List.List.Count - 1 do
      begin
        Entry := FStreams.Add(List.List[i].Copy);
        Entry.IsInList := True;
      end;
    except
    end;
  finally
    Recent.Free;
  end;

  lstClients.SortItems;
  lstStations.Sort;

  {$IFDEF DEBUG}Caption := Caption + ' --::: DEBUG BUiLD :::--';{$ENDIF}

  UpdateButtons;
  UpdateStatus;
  tmrSpeed.Enabled := True;
  TrayIcon1.Visible := AppGlobals.TrayClose;

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

  cmdStartPlay.Visible := BassLoaded;
  cmdStopPlay.Visible := BassLoaded;
  ToolButton2.Visible := BassLoaded;

  Language.Translate(Self);
end;

procedure TfrmStreamWriterMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClients);
  FreeAndNil(FHomeCommunication);
  FreeAndNil(FUpdater);
  FreeAndNil(FStreams);
end;

procedure TfrmStreamWriterMain.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if FWasShown then
    Exit;
  FWasShown := True;

  AppGlobals.WindowHandle := Handle;

  if AppGlobals.MainMaximized then
    WindowState := wsMaximized;
  FWasMaximized := WindowState = wsMaximized;

  pagSidebar.Visible := AppGlobals.ShowSidebar;
  Splitter1.Visible := AppGlobals.ShowSidebar;
  pagSidebar.Width := AppGlobals.SidebarWidth;
  actShowStreamBrowser.Checked := pagSidebar.Visible;

  if lstClients.RootNodeCount > 0 then
  begin
    lstClients.Selected[lstClients.GetFirst] := True;
    lstClients.FocusedNode := lstclients.GetFirst;
  end;

  for i := 0 to lstClients.Header.Columns.Count - 1 do
    lstClients.Header.Columns[i].Width := AppGlobals.HeaderWidth[i];

  pnlStreamBrowser.Setup;
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
  ShowInfo;
end;

procedure TfrmStreamWriterMain.PreTranslate;
begin

end;

procedure TfrmStreamWriterMain.PostTranslate;
begin
  pnlStreamBrowser.Translate;
  pnlStreamInfo.Translate;
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

procedure TfrmStreamWriterMain.SavePlaylist(Entries: TPlaylistEntryArray;
  Open: Boolean);
  procedure BuildPLS(Entries: TPlaylistEntryArray; List: TStringList);
  var
    i: Integer;
  begin
    List.Clear;
    List.Add('[playlist]');
    List.Add('numberofentries=' + IntToStr(Length(Entries)));
    for i := 0 to Length(Entries) - 1 do
    begin
      List.Add('File' + IntToStr(i + 1) + '=' + Entries[i].URL);
      List.Add('Title' + IntToStr(i + 1) + '=' + Entries[i].Name);
      List.Add('Length' + IntToStr(i + 1) + '=-1');
    end;
  end;
  procedure BuildM3U(Entries: TPlaylistEntryArray; List: TStringList);
  var
    i: Integer;
  begin
    List.Clear;
    List.Add('#EXTM3U');
    for i := 0 to Length(Entries) - 1 do
    begin
      List.Add('#EXTINF:-1,' + Entries[i].Name);
      List.Add(Entries[i].URL);
    end;
  end;
var
  Res: Integer;
  List: TStringList;
  Dlg: TSaveDialog;
begin
  if Length(Entries) = 0 then
    Exit;
  List := TStringList.Create;
  try
    if not Open then
    begin
      Dlg := TSaveDialog.Create(Self);
      try
        Dlg.FileName := '';
        Dlg.Filter := '.M3U Playlist|*.m3u|.PLS Playlist|*.pls';
        Dlg.Options := Dlg.Options + [ofOverwritePrompt, ofPathMustExist];
        if Dlg.Execute(Handle) then
        begin
          try
            if (LowerCase(ExtractFileExt(Dlg.FileName)) <> '.m3u') and
               (LowerCase(ExtractFileExt(Dlg.FileName)) <> '.pls') then
              if Dlg.FilterIndex = 1 then
                Dlg.FileName := Dlg.FileName + '.m3u'
              else
                Dlg.FileName := Dlg.FileName + '.pls';

            if LowerCase(ExtractFileExt(Dlg.FileName)) = '.m3u' then
              BuildM3U(Entries, List)
            else
              BuildPLS(Entries, List);

            List.SaveToFile(Dlg.FileName);
          except
            MsgBox(Handle, Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [ExtractFilePath(Dlg.FileName)]), _('Error'), MB_ICONEXCLAMATION);
          end;
        end;
      finally
        Dlg.Free;
      end;
    end else
    begin
      try
        BuildM3U(Entries, List);
        List.SaveToFile(AppGlobals.TempDir + 'playlist.m3u');
        Res := ShellExecute(Handle, 'open', PChar(AppGlobals.TempDir + 'playlist.m3u'), nil, nil, 1);
        if Res <= 32 then
        begin
          BuildPLS(Entries, List);
          List.SaveToFile(AppGlobals.TempDir + 'playlist.pls');
          Res := ShellExecute(Handle, 'open', PChar(AppGlobals.TempDir + 'playlist.pls'), nil, nil, 1);
          if Res <= 32 then
            ShellExecute(Handle, nil, 'rundll32.exe', PChar('shell32.dll,OpenAs_RunDLL ' + AppGlobals.TempDir + 'playlist.pls'), nil, 1);
        end;
      except
        MsgBox(Handle, Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [AppGlobals.TempDir]), _('Error'), MB_ICONEXCLAMATION);
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmStreamWriterMain.ShowInfo;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Entry: TStreamEntry;
  Entries: TStreamList;
begin
  Clients := lstClients.NodesToData(lstClients.GetNodes(True));

  Entries := TStreamList.Create;
  try
    for Client in Clients do
    begin
      Entry := FStreams.Get(Client.Client);
      if Entry <> nil then
      begin
        Entries.Add(Entry)
      end;
    end;

    if Entries.Count > 0 then
      pnlStreamInfo.ShowInfo(Entries)
    else
      pnlStreamInfo.ShowInfo(nil);
  finally
    Entries.Free;
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
  if S.RelayChanged then
  begin
    if AppGlobals.Relay then
      FClients.RelayServer.Start
    else
      FClients.RelayServer.Stop;
  end;
  TrayIcon1.Visible := AppGlobals.TrayClose;
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

function TfrmStreamWriterMain.StartStreaming(Name, URL: string): Boolean;
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Result := True;
  if not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
  begin
    Result := False;
    MsgBox(Handle, _('Available disk space is below the set limit, so recording will not start.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  URL := Trim(URL);
  if URL <> '' then
  begin
    Entry := FStreams.Get(Name, URL, nil);
    if Entry <> nil then
      Entry.LastTouched := Now;

    // Ist der Client schon in der Liste?
    Client := FClients.GetClient(Name, URL, nil);
    if Client <> nil then
    begin
      Client.Connect;
      Exit;
    end else
    begin
      // Ist der Client schon bekannt?
      if Entry <> nil then
      begin
        Client := FClients.AddClient(Entry.Name, Entry.StartURL, Entry.URLs, Entry.SkipShort, Entry.SongsSaved);
        Client.Connect;
      end else
      begin
        if ValidURL(URL) then
        begin
          Client := FClients.AddClient(Name, URL);
          Client.Connect;
        end else
        begin
          Result := False;
          MsgBox(Handle, _('The stream could not be added to the list because the URL is invalid.'), _('Info'), MB_ICONINFORMATION);
        end;
      end;
    end;
  end;
end;

procedure TfrmStreamWriterMain.StreamBrowserAction(Sender: TObject;
  Action: TOpenActions; Streams: TStreamDataArray);
var
  i: Integer;
  s: string;
  Entries: TPlaylistEntryArray;
begin
  if Action in [oaListen, oaSave] then
  begin
    SetLength(Entries, 0);
    for i := 0 to Length(Streams) - 1 do
    begin
      SetLength(Entries, Length(Entries) + 1);
      Entries[i].Name := Streams[i].Name;
      Entries[i].URL := Streams[i].URL;
    end;
  end;

  case Action of
    oaStart:
      for i := 0 to Length(Streams) - 1 do
        if not StartStreaming(Streams[i].Name, Streams[i].URL) then
          Break;
    oaListen:
      SavePlaylist(Entries, True);
    oaCopy:
      begin
        s := '';
        Clipboard.Clear;
        for i := 0 to Length(Streams) - 1 do
          s := s + Streams[i].URL + #13#10;
        s := Trim(s);
        Clipboard.SetTextBuf(PChar(s));
      end;
    oaSave:
      SavePlaylist(Entries, False);
  end;
end;

procedure TfrmStreamWriterMain.StreamInfoAction(Sender: TObject;
  Action: TTrackActions; Tracks: TTrackInfoArray);
var
  Entries: TPlaylistEntryArray;
  i: Integer;
begin
  case Action of
    taPlay:
      begin
        // Tracks in Playlist konvertieren
        SetLength(Entries, Length(Tracks));
        for i := 0 to Length(Tracks) - 1 do
        begin
          Entries[i].Name := Tracks[i].Filename;
          Entries[i].URL := Tracks[i].Filename;
        end;

        SavePlaylist(Entries, True);
      end;
    taRemove:
      begin
        for i := 0 to Length(Tracks) - 1 do
          FStreams.RemoveTrack(Tracks[i]);
      end;
    taDelete:
      begin
        for i := 0 to Length(Tracks) - 1 do
        begin
          DeleteFile(Tracks[i].Filename);
          FStreams.RemoveTrack(Tracks[i]);
        end;
      end;
    taProperties:
      PropertiesDialog(Tracks[0].Filename) // testen obs lackt.
  end;
end;

procedure TfrmStreamWriterMain.StreamsStreamAdded(Sender: TObject;
  Stream: TStreamEntry);
begin

end;

procedure TfrmStreamWriterMain.StreamsStreamRemoved(Sender: TObject;
  Stream: TStreamEntry);
begin

end;

procedure TfrmStreamWriterMain.SysCommand(var Msg: TWMSysCommand);
begin
  if Msg.CmdType = SC_MINIMIZE then
    FWasMaximized := WindowState = wsMaximized;
  DefaultHandler(Msg);
end;

procedure TfrmStreamWriterMain.tabInfoResize(Sender: TObject);
begin
  // ShowInfo;
end;

procedure TfrmStreamWriterMain.StreamsStreamChanged(Sender: TObject;
  Stream: TStreamEntry);
var
  Item: TComboExItem;
  Client: TICEClient;
begin
  if Stream.IsInList then
  begin
    Client := FClients.GetClient(Stream.Name, Stream.StartURL, Stream.URLs);
    if Client = nil then
      FClients.AddClient(Stream.Name, Stream.StartURL, Stream.URLs, Stream.SkipShort, Stream.SongsSaved);
  end;
  Item := lstStations.Get(Stream.Name, Stream.StartURL, Stream.URLs);
  if (Item = nil) and (Stream.RecentIndex > -1) then
  begin
    Item := lstStations.ItemsEx.Add;
    Item.ImageIndex := 0;
    Item.Caption := Stream.Name;
    Item.Data := Stream;
    if lstStations.ItemIndex > -1 then
      lstStations.ItemIndex := lstStations.ItemIndex + 1;
  end else
  begin
    if (Item <> nil) and (Stream.RecentIndex = -1) then
      lstStations.ItemsEx.Delete(Item.Index);
  end;
  lstStations.Sort;
end;

procedure TfrmStreamWriterMain.tmrSpeedTimer(Sender: TObject);
var
  Active: Boolean;
  i: Integer;
begin
  UpdateStatus;

  if FRefreshInfo then
  begin
    ShowInfo;
    FRefreshInfo := False;
  end;

  Active := False;
  for i := 0 to FClients.Count - 1 do
    if FClients[i].Active then
    begin
      Active := True;
      Break;
    end;

  if Active and not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
  begin
    for i := 0 to FClients.Count - 1 do
      FClients[i].Disconnect;
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
  B, B4: Boolean;
  Clients: TClientArray;
  Client, Client2: TICEClient;
begin
  Clients := lstClients.NodesToClients(lstClients.GetNodes(True));
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

  for Client in Clients do
  begin
    if Client.Active then
      if AppGlobals.Relay then
        actTuneInRelay.Enabled := True;
    if Client.Filename <> '' then
      actTuneInFile.Enabled := True;
  end;

  if lstClients.SelectedCount > 1 then
  begin
    Client2 := lstClients.NodesToClients(lstClients.GetNodes(True))[0];
    B4 := True;
    for Client in lstClients.NodesToClients(lstClients.GetNodes(True)) do
    begin
      if not Client.SkipShort = Client2.SkipShort then
        B4 := False;
    end;
    Client := lstClients.NodesToClients(lstClients.GetNodes(True))[0];
    actSkipShort.Checked := Client.SkipShort and B4;
  end else if lstClients.SelectedCount = 1 then
  begin
    Client := lstClients.NodesToClients(lstClients.GetNodes(True))[0];
    actSkipShort.Checked := Client.SkipShort;

    case AppGlobals.DefaultAction of
      caStartStop:
        if Client.Active then
          mnuStopStreaming1.Default := True
        else
          mnuStartStreaming1.Default := True;
      caStream:
        mnuListenToStream1.Default := True;
      caRelay:
        mnuListenToRelay1.Default := True;
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
  Clients := lstClients.NodesToData(lstClients.GetNodes(False));
  for Client in Clients do
  begin
    Speed := Speed + Client.Client.Speed;
    lstClients.RefreshClient(Client.Client);
  end;
  addStatus.Panels[0].Text := MakeSize(Speed) + '/s';
  addStatus.Panels[1].Text := Format(_('%s/%s received'), [MakeSize(FReceived), MakeSize(FStreams.Received)]);
  addStatus.Panels[2].Text := Format(_('%d songs saved'), [FClients.SongsSaved]);
end;

procedure TfrmStreamWriterMain.lstClientsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    actRemove.Execute;
  end;
end;

procedure TfrmStreamWriterMain.lstClientsKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) or (Key = #32) then
  begin
    lstClientsDblClick(lstClients);
    Key := #0;
  end;
end;

procedure TfrmStreamWriterMain.lstClientsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Clients: TNodeDataArray;
begin
  UpdateButtons;
  ShowInfo;

  if lstClients.SelectedCount = 1 then
  begin
    Clients := lstClients.NodesToData(lstClients.GetNodes(True));
    if Length(Clients) = 1 then
      pnlDebug.ShowDebug(Clients[0].Client);
  end else
    pnlDebug.ShowDebug(nil);
end;

procedure TfrmStreamWriterMain.lstClientsDblClick(Sender: TObject);
var
  Clients: TNodeDataArray;
begin
  Clients := lstClients.NodesToData(lstClients.GetNodes(True));
  if Length(Clients) = 1 then
  begin
    case AppGlobals.DefaultAction of
      caStartStop:
        if Clients[0].Client.Active then
          Clients[0].Client.Disconnect
        else
          Clients[0].Client.Connect;
      caStream:
        actTuneInStream.Execute;
      caRelay:
        actTuneInRelay.Execute;
      caFile:
        actTuneInFile.Execute;
    end;
  end;
end;

procedure TfrmStreamWriterMain.lstStationsChange(Sender: TObject);
begin
  cmdStartStreaming.Enabled := (Length(Trim(lstStations.Text)) > 0) or (lstStations.ItemIndex > -1);
end;

procedure TfrmStreamWriterMain.lstStationsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    cmdStartStreaming.Click;
  end else
  begin
    lstStations.ItemIndex := -1;
  end;
end;

function TfrmStreamWriterMain.CanExitApp: Boolean;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Rec: Boolean;
begin
  Result := True;
  Rec := False;
  Clients := lstClients.NodesToData(lstClients.GetNodes(False));

  for Client in Clients do
    if Client.Client.Active then
    begin
      Rec := True;
      Break;
    end;

  if Rec then
  begin
    if TfrmMsgDlg.ShowMsg(Self, _('At least one connection is active at the moment. Exiting the application will abort streaming.'#13#10'Do you really want to quit?'), 2, 1) = mtCancel then
    begin
      Result := False;
    end;
  end;
end;

procedure TfrmStreamWriterMain.ClientManagerAddRecent(Sender: TObject);
var
  Client: TICEClient;
  Entry: TStreamEntry;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Add(Client.StreamName, Client.StartURL, Client.URLs,
    Client.BitRate, Client.Genre, Client.SkipShort, 0);
  Entry.Name := Client.StreamName;
  Entry.RecentIndex := 0;
  Entry.LastTouched := Now;
  if not Entry.Submitted then
  begin
    FHomeCommunication.SubmitStream(Entry.StartURL);
    Entry.Submitted := True;
  end;

  ShowInfo;
end;

procedure TfrmStreamWriterMain.ClientManagerDebug(Sender: TObject);
begin
  if pnlDebug.DebugView.Client = Sender then
  begin
    pnlDebug.ShowDebug(TICEClient(Sender));
  end;
end;

procedure TfrmStreamWriterMain.ClientManagerICYReceived(Sender: TObject;
  Received: Integer);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  FReceived := FReceived + Received;
  FStreams.Received := FStreams.Received + Received;

  Entry := FStreams.Get(Client);
  if Entry <> nil then
  begin
    Entry.BytesReceived := Entry.BytesReceived + Received;
  end;

  FRefreshInfo := True;
end;

procedure TfrmStreamWriterMain.ClientManagerRefresh(Sender: TObject);
begin
  lstClients.RefreshClient(Sender as TICEClient);
end;

procedure TfrmStreamWriterMain.ClientManagerClientAdded(Sender: TObject);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Add(Client.StreamName, Client.StartURL, Client.URLs,
    Client.BitRate, Client.Genre, Client.SkipShort, 0);
  Entry.LastTouched := Now;
  Entry.IsInList := True;
  Client.Received := Entry.BytesReceived;

  lstClients.AddClient(Client);

  lstClients.SortItems;
end;

procedure TfrmStreamWriterMain.ClientManagerClientRemoved(Sender: TObject);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Get(Client);
  if Entry <> nil then
    Entry.IsInList := False;

  lstClients.RemoveClient(Client);

  if pnlDebug.DebugView.Client = Client then
    pnlDebug.ShowDebug(nil);

  ShowInfo;
end;

procedure TfrmStreamWriterMain.ClientManagerSongSaved(Sender: TObject;
  Filename, Title: string);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Get(Client);
  if Entry <> nil then
  begin
    Entry.Tracks.Add(TTrackInfo.Create(Now, Filename));
    Entry.SongsSaved := Entry.SongsSaved + 1;
  end;

  ShowInfo;
end;

procedure TfrmStreamWriterMain.ClientManagerTitleChanged(Sender: TObject;
  Title: string);
begin

end;

end.
