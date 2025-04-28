{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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

unit ClientTab;

interface

uses
  ActnList,
  AppData,
  AppMessages,
  AudioFunctions,
  Buttons,
  Classes,
  ClientAddressBar,
  ClientManager,
  ClientView,
  Clipbrd,
  ComboEx,
  ComCtrls,
  Controls,
  DataManager,
  Dialogs,
  DragDrop,
  DynBass,
  ExtCtrls,
  Forms,
  Functions,
  Generics.Collections,
  Generics.Defaults,
  Graphics,
  HomeCommunication,
  ICEClient,
  Images,
  ImgList,
  LanguageObjects,
  Logging,
  MControlFocuser,
  MControls,
  Menus,
  MessageBus,
  MsgDlg,
  MToolbarForcedHorizontal,
  MVolumePanel,
  PlayerManager,
  PlaylistHandler,
  SharedControls,
  SharedData,
  StationCombo,
  StdCtrls,
  StreamBrowserView,
  StreamDebugView,
  StreamInfoView,
  SysUtils,
  Tabs,
  TypeDefs,
  VirtualTrees,
  Windows;

type

  { TSidebar }

  TSidebar = class(TPageControl)
  private
    FPage1, FPage2, FPage3: TTabSheet;

    FBrowserView: TMStreamBrowserView;
    FInfoView: TMStreamInfoView;
    FDebugView: TMStreamDebugView;
  public
    constructor Create(TheOwner: TComponent); override;

    property BrowserView: TMStreamBrowserView read FBrowserView;
    property InfoView: TMStreamInfoView read FInfoView;
    property DebugView: TMStreamDebugView read FDebugView;
  end;

  TAddTitleEvent = procedure(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string) of object;
  TAddTitleEventWithServerHash = procedure(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string; ServerTitleHash: Cardinal) of object;

  { TClientTab }

  TClientTab = class(TMainTabSheet)
  private
    FToolbarPanel: TPanel;
    FTimeLabel: TLabel;
    FVolume: TMVolumePanel;
    FToolbar: TToolBar;
    FAddressBar: TClientAddressBar;
    FClientView: TMClientView;
    FSplitter: TSplitter;
    FSideBar: TSideBar;

    FClientManager: TClientManager;
    FHomeCommunication: THomeCommunication;

    FReceived: UInt64;
    FRefreshInfo: Boolean;

    FPlaybackSeconds: Cardinal;
    FPlaybackTimer: TTimer;

    FActionRename: TAction;
    FActionRemove: TAction;
    FActionShowSideBar: TAction;
    FActionStopAfterSong: TAction;
    FActionPlay: TAction;
    FActionPause: TAction;
    FActionStopPlay: TAction;
    FActionTuneInStream: TAction;

    FOnUpdateButtons: TNotifyEvent;
    FOnTrackAdded: TTrackEvent;
    FOnPlayStarted: TNotifyEvent;
    FOnAuthRequired: TNotifyEvent;
    FOnShowErrorMessage: TStringEvent;
    FOnClientAdded: TNotifyEvent;
    FOnClientRemoved: TNotifyEvent;
    FOnAddTitleToList: TAddTitleEvent;
    FOnRemoveTitleFromList: TAddTitleEventWithServerHash;
    FOnSetStreamData: TIntegerEvent;

    procedure ActionNewCategoryExecute(Sender: TObject);
    procedure ActionStartExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionOpenWebsiteExecute(Sender: TObject);
    procedure ActionRenameExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionPlayStopExecute(Sender: TObject);
    procedure ActionResetDataExecute(Sender: TObject);
    procedure ActionShowSideBarExecute(Sender: TObject);
    procedure ActionStopAfterSongExecute(Sender: TObject);
    procedure ActionSavePlaylistStreamExecute(Sender: TObject);
    procedure ActionTuneInStreamExecute(Sender: TObject);
    procedure ActionCopyTitleExecute(Sender: TObject);
    procedure ActionAddToSaveListExecute(Sender: TObject);
    procedure ActionAddToGlobalIgnoreList(Sender: TObject);
    procedure ActionAddToStreamIgnoreList(Sender: TObject);

    procedure ClientManagerLog(Sender: TObject);
    procedure ClientManagerRefresh(Sender: TObject);
    procedure ClientManagerAddRecent(Sender: TObject);
    procedure ClientManagerClientAdded(Sender: TObject);
    procedure ClientManagerClientRemoved(Sender: TObject);
    procedure ClientManagerSongSaved(Sender: TObject; Filename, Title, SongArtist, SongTitle: string; Filesize: Int64; Length, Bitrate: Cardinal; VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
      ServerTitleHash, ServerArtistHash: Cardinal);
    procedure ClientManagerClientTitleChanged(Sender: TObject; Title: string);
    procedure ClientManagerICYReceived(Sender: TObject; Received: Integer);
    procedure ClientManagerTitleAllowed(Sender: TObject; Title: string; var Allowed: Boolean; var Match: string; var Filter: Integer);
    procedure ClientManagerShowErrorMessage(Sender: TObject; Data: string);
    procedure ClientManagerPlaybackStarted(Sender: TObject);
    procedure ClientManagerSecondsReceived(Sender: TObject);

    procedure FClientViewSelectionChange(Sender: TObject);
    procedure FClientViewNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure FClientViewKeyPress(Sender: TObject; var Key: Char);
    procedure FClientViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FClientViewStartStreaming(Sender: TObject; ID, Bitrate: Cardinal; Name, URL: string; URLs, RegExes, IgnoreTitles: TStringList; AddOnly: Boolean; Node: PVirtualNode; Mode: TVTNodeAttachMode);

    procedure StreamBrowserAction(Sender: TObject; Action: TStreamOpenActions; Streams: TStreamDataArray);
    function StreamBrowserIsInClientList(Sender: TObject; ID: Cardinal): Boolean;

    procedure VolumeVolumeChange(Sender: TObject);
    function VolumeGetVolumeBeforeMute(Sender: TObject): Integer;

    procedure AddressBarStart(Sender: TObject);

    procedure DebugClear(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);

    procedure PlaybackTimerTimer(Sender: TObject);
  protected
    procedure CreateHandle; override;
    procedure ShownFirst; override;
  public
    constructor Create(AOwner: TComponent; Toolbar: TMToolbarForcedHorizontal; Actions: TActionList; Clients: TClientManager; Popup: TPopupMenu); reintroduce;
    destructor Destroy; override;

    function StartStreaming(Streams: TStartStreamingInfoArray; Action: TStreamOpenActions; HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean; overload;
    function StartStreaming(Stream: TStartStreamingInfo; Action: TStreamOpenActions; HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean; overload;
    function StopStreaming(Info: TStartStreamingInfo; Action: TStreamOpenActions): Boolean; overload;
    procedure TimerTick;
    procedure UpdateStreams;
    procedure BuildTree;
    procedure PausePlay;
    procedure ShowInfo;

    property AddressBar: TClientAddressBar read FAddressBar;
    property ClientView: TMClientView read FClientView;
    property SideBar: TSideBar read FSideBar;
    property Received: UInt64 read FReceived;

    property OnUpdateButtons: TNotifyEvent read FOnUpdateButtons write FOnUpdateButtons;
    property OnTrackAdded: TTrackEvent read FOnTrackAdded write FOnTrackAdded;
    property OnPlayStarted: TNotifyEvent read FOnPlayStarted write FOnPlayStarted;
    property OnAuthRequired: TNotifyEvent read FOnAuthRequired write FOnAuthRequired;
    property OnShowErrorMessage: TStringEvent read FOnShowErrorMessage write FOnShowErrorMessage;
    property OnClientAdded: TNotifyEvent read FOnClientAdded write FOnClientAdded;
    property OnClientRemoved: TNotifyEvent read FOnClientRemoved write FOnClientRemoved;
    property OnAddTitleToList: TAddTitleEvent read FOnAddTitleToList write FOnAddTitleToList;
    property OnRemoveTitleFromList: TAddTitleEventWithServerHash read FOnRemoveTitleFromList write FOnRemoveTitleFromList;
    property OnSetStreamData: TIntegerEvent read FOnSetStreamData write FOnSetStreamData;
  end;

implementation

{ TClientTab }

procedure TClientTab.ActionAddToGlobalIgnoreList(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    if Assigned(FOnAddTitleToList) then
      FOnAddTitleToList(Self, nil, ltIgnore, Client.Title);
end;

procedure TClientTab.ActionAddToStreamIgnoreList(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    if Assigned(FOnAddTitleToList) then
      FOnAddTitleToList(Self, Client, ltIgnore, Client.Title);
end;

procedure TClientTab.ActionAddToSaveListExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    if Assigned(FOnAddTitleToList) then
      FOnAddTitleToList(Self, nil, ltSave, Client.Title);
end;

procedure TClientTab.ActionCopyTitleExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
  Title: string;
begin
  Title := '';
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    if Client.Title <> '' then
      Title := Title + Client.Title + #13#10;
  Title := Trim(Title);
  if Title <> '' then
    Clipboard.AsText := Title;
end;

procedure TClientTab.ActionNewCategoryExecute(Sender: TObject);
var
  NodeData: PClientNodeData;
begin
  NodeData := FClientView.GetNodeData(FClientView.AddCategory);
  AppGlobals.Data.CategoryList.Add(NodeData.Category);
end;

procedure TClientTab.ActionStartExecute(Sender: TObject);

  function StartClient(Client: TICEClient; ErrorShown: Boolean): Boolean;
  var
    Res: TMayConnectResults;
  begin
    Result := ErrorShown;
    Res := Client.StartRecording(True);
    if Res <> crOk then
    begin
      Client.WriteLog(FClientManager.GetErrorText(Res, '', False, False, True), '', ltGeneral, llWarning);
      if not ErrorShown then
        OnShowErrorMessage(Client, FClientManager.GetErrorText(Res, '', False, False, False));
      Result := True;
    end;
  end;

var
  ErrorShown: Boolean;
  Clients: TClientArray;
  Client: TICEClient;
  Node: PVirtualNode;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
begin
  ErrorShown := False;
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    if not Client.AutoRemove then
      ErrorShown := StartClient(Client, ErrorShown);

  ErrorShown := False;
  Nodes := FClientView.GetNodes(ntCategory, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if not NodeData.Category.IsAuto then
    begin
      Clients := FClientView.NodesToClients(FClientView.GetChildNodes(Node));
      for Client in Clients do
        ErrorShown := StartClient(Client, ErrorShown);
    end;
  end;
end;

procedure TClientTab.ActionStopAfterSongExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    Client.StopAfterSong := FActionStopAfterSong.Checked;
end;

procedure TClientTab.ActionStopExecute(Sender: TObject);

  procedure StopClient(Client: TICEClient);
  begin
    if Client.ScheduledRecording then
      Client.WriteLog(_('Scheduled recording was interrupted by user'), '', ltSchedule, llInfo);
    Client.StopRecording;
  end;

var
  Clients: TClientArray;
  Client: TICEClient;
  Node: PVirtualNode;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    if not Client.AutoRemove then
      StopClient(Client);

  Nodes := FClientView.GetNodes(ntCategory, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if not NodeData.Category.IsAuto then
    begin
      Clients := FClientView.NodesToClients(FClientView.GetChildNodes(Node));
      for Client in Clients do
        StopClient(Client);
    end;
  end;
end;

procedure TClientTab.ActionRemoveExecute(Sender: TObject);
var
  OnlyAutomatic: Boolean;
  Node, ChildNode: PVirtualNode;
  Nodes, ChildNodes: TNodeArray;
  NodeData, ChildNodeData: PClientNodeData;
begin
  Nodes := FClientView.GetNodes(ntClient, True);
  OnlyAutomatic := True;
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if not NodeData.Client.AutoRemove then
    begin
      OnlyAutomatic := False;
      Break;
    end;
  end;

  if not OnlyAutomatic then
    if TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('All selected streams will be removed from the list. This also means that their ' + 'settings and ignorelists get deleted.'#13#10'Are you sure you want to continue?'),
      mtConfirmation, [mbOK, mbCancel], mbCancel, 8) = mrCancel then
      Exit;

  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if NodeData.Client <> nil then
      FClientManager.RemoveClient(NodeData.Client);
  end;

  Nodes := FClientView.GetNodes(ntCategory, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if NodeData.Category <> nil then
      if not NodeData.Category.IsAuto then
      begin
        ChildNodes := FClientView.GetNodes(ntAll, False);
        for ChildNode in ChildNodes do
          if ChildNode.Parent = Node then
          begin
            ChildNodeData := FClientView.GetNodeData(ChildNode);
            FClientManager.RemoveClient(ChildNodeData.Client);
          end;
      end;
  end;

  // Wenn alle Clients weg sind können jetzt Kategorien gekickt werden.
  Nodes := FClientView.GetNodes(ntCategory, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if NodeData.Category.IsAuto then
      Continue;
    if FClientView.ChildCount[Node] = 0 then
    begin
      AppGlobals.Data.CategoryList.Remove(NodeData.Category);
      NodeData.Category.Free;
      FClientView.DeleteNode(Node);
    end else
      NodeData.Category.Killed := True;
  end;
end;

procedure TClientTab.ActionRenameExecute(Sender: TObject);
begin
  FClientView.EditNode(FClientView.GetFirstSelected, 0);
end;

procedure TClientTab.ActionPlayExecute(Sender: TObject);
var
  Res: TMayConnectResults;
  Clients: TNodeDataArray;
  SelectedClient, Client: PClientNodeData;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
  if Length(Clients) <> 1 then
    Exit
  else
    SelectedClient := Clients[0];

  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, False));
  for Client in Clients do
    if Client <> SelectedClient then
      Client.Client.StopPlay;

  Res := SelectedClient.Client.StartPlay(True);
  if Res <> crOk then
  begin
    SelectedClient.Client.WriteLog(FClientManager.GetErrorText(Res, '', False, False, True), '', ltGeneral, llWarning);
    OnShowErrorMessage(SelectedClient.Client, FClientManager.GetErrorText(Res, '', False, False, False));
  end else if Assigned(FOnPlayStarted) then
    FOnPlayStarted(Self);
end;

procedure TClientTab.ActionPauseExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));

  for Client in Clients do
    if Client.Playing and (Client.Paused) then
    begin
      Client.PausePlay;
      if Assigned(FOnPlayStarted) then
        FOnPlayStarted(Self);
    end;

  for Client in Clients do
    if (Client.Playing) and (not Client.Paused) then
    begin
      Client.PausePlay;
      if Assigned(FOnPlayStarted) then
        FOnPlayStarted(Self);
    end;
end;

procedure TClientTab.ActionPlayStopExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
  for Client in Clients do
    Client.StopPlay;
end;

procedure TClientTab.ActionOpenWebsiteExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    TFunctions.ShellExecute(Handle, 'open', Client.Entry.StreamURL);
end;

procedure TClientTab.ActionResetDataExecute(Sender: TObject);
var
  Res: Integer;
  Clients: TNodeDataArray;
  Client: PClientNodeData;
begin
  Res := TFunctions.MsgBox(_('This will reset the saved song and bytes received counters.'#13#10 +
    'The tracknumber of new saved titles will be 1 if you specified the tracknumber in the filename pattern, this number will also be set in ID3 tags.'#13#10 + 'Do you want to continue?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
  if Res = IDYES then
  begin
    Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
    for Client in Clients do
    begin
      if Client.Client.AutoRemove then
        Continue;
      Client.Client.Entry.SongsSaved := 0;
      Client.Client.Entry.BytesReceived := 0;
      Client.Client.Entry.SecondsReceived := 0;

      FClientView.RefreshClient(Client.Client);
    end;
  end;
  ShowInfo;
end;

procedure TClientTab.ActionShowSideBarExecute(Sender: TObject);
begin
  FSideBar.Visible := not FSideBar.Visible;
  FSplitter.Visible := not FSplitter.Visible;
  FActionShowSideBar.Checked := FSideBar.Visible;
end;

procedure TClientTab.ActionSavePlaylistStreamExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etStream);
  SavePlaylist(Entries, False);
end;

procedure TClientTab.ActionTuneInStreamExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etStream);
  SavePlaylist(Entries, True);
end;

constructor TClientTab.Create(AOwner: TComponent; Toolbar: TMToolbarForcedHorizontal; Actions: TActionList; Clients: TClientManager; Popup: TPopupMenu);

  function GetAction(Name: string): TAction;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to Actions.ActionCount - 1 do
      if Actions[i].Name = Name then
      begin
        Result := Actions[i] as TAction;
        Break;
      end;
    if Result = nil then
      raise Exception.Create('');
  end;

begin
  inherited Create(AOwner);

  ShowCloseButton := False;
  ImageIndex := TImages.TRANSMIT;

  FPlaybackTimer := TTimer.Create(Self);
  FPlaybackTimer.Interval := 1000;
  FPlaybackTimer.Enabled := False;
  FPlaybackTimer.OnTimer := PlaybackTimerTimer;

  FRefreshInfo := False;
  FReceived := 0;

  FClientManager := Clients;
  FClientManager.OnClientLog := ClientManagerLog;
  FClientManager.OnClientRefresh := ClientManagerRefresh;
  FClientManager.OnClientAddRecent := ClientManagerAddRecent;
  FClientManager.OnClientAdded := ClientManagerClientAdded;
  FClientManager.OnClientRemoved := ClientManagerClientRemoved;
  FClientManager.OnClientSongSaved := ClientManagerSongSaved;
  FClientManager.OnClientTitleChanged := ClientManagerClientTitleChanged;
  FClientManager.OnClientICYReceived := ClientManagerICYReceived;
  FClientManager.OnClientTitleAllowed := ClientManagerTitleAllowed;
  FClientManager.OnShowErrorMessage := ClientManagerShowErrorMessage;
  FClientManager.OnPlaybackStarted := ClientManagerPlaybackStarted;
  FClientManager.OnClientSecondsReceived := ClientManagerSecondsReceived;

  FHomeCommunication := HomeComm;

  Caption := 'Streams';

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Align := alTop;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Top := -100;
  FToolbarPanel.ChildSizing.TopBottomSpacing := 4;
  FToolbarPanel.ChildSizing.VerticalSpacing := 4;
  FToolbarPanel.AutoSize := True;
  FToolbarPanel.Parent := Self;

  FAddressBar := TClientAddressBar.Create(Self);
  FAddressBar.Align := alTop;
  FAddressBar.Top := 0;
  FAddressBar.AutoSize := True;
  FAddressBar.OnStart := AddressBarStart;
  FAddressBar.Parent := Self;

  FToolbar := Toolbar;
  FToolbar.Align := alClient;
  FToolbar.Parent := FToolbarPanel;

  FVolume := TMVolumePanel.Create(Self);
  FVolume.Align := alRight;
  FVolume.Images := modSharedData.imgImages;
  FVolume.ImageIndexMute := TImages.SOUND_MUTE;
  FVolume.ImageIndexSound := TImages.SOUND;
  FVolume.ImageIndexSoundLow := TImages.SOUND_LOW;
  FVolume.Enabled := Bass.DeviceAvailable;
  FVolume.Volume := Players.Volume;
  FVolume.OnVolumeChange := VolumeVolumeChange;
  FVolume.OnGetVolumeBeforeMute := VolumeGetVolumeBeforeMute;
  FVolume.Parent := FToolbarPanel;

  FTimeLabel := TLabel.Create(Self);
  FTimeLabel.Align := alRight;
  FTimeLabel.Layout := tlCenter;
  FTimeLabel.Alignment := taCenter;
  FTimeLabel.BorderSpacing.Right := Scale96ToFont(4);
  FTimeLabel.Parent := FToolbarPanel;

  FActionPlay := GetAction('actPlay');
  FActionPause := GetAction('actPause');
  FActionStopPlay := GetAction('actStopPlay');
  FActionTuneInStream := GetAction('actTuneInStream');
  FActionRename := GetAction('actRename');
  FActionRemove := GetAction('actRemove');
  FActionShowSideBar := GetAction('actShowSideBar');
  FActionStopAfterSong := GetAction('actStopAfterSong');

  FActionPlay.OnExecute := ActionPlayExecute;
  FActionPause.OnExecute := ActionPauseExecute;
  FActionStopPlay.OnExecute := ActionPlayStopExecute;
  FActionTuneInStream.OnExecute := ActionTuneInStreamExecute;
  FActionRename.OnExecute := ActionRenameExecute;
  FActionRemove.OnExecute := ActionRemoveExecute;
  FActionShowSideBar.OnExecute := ActionShowSideBarExecute;
  FActionStopAfterSong.OnExecute := ActionStopAfterSongExecute;

  GetAction('actNewCategory').Enabled := True;
  GetAction('actNewCategory').OnExecute := ActionNewCategoryExecute;
  GetAction('actStart').OnExecute := ActionStartExecute;
  GetAction('actStop').OnExecute := ActionStopExecute;
  GetAction('actOpenWebsite').OnExecute := ActionOpenWebsiteExecute;
  GetAction('actResetData').OnExecute := ActionResetDataExecute;
  GetAction('actSavePlaylistStream').OnExecute := ActionSavePlaylistStreamExecute;
  GetAction('actCopyTitle').OnExecute := ActionCopyTitleExecute;
  GetAction('actAddToSaveList').OnExecute := ActionAddToSaveListExecute;
  GetAction('actAddToGlobalIgnoreList').OnExecute := ActionAddToGlobalIgnoreList;
  GetAction('actAddToStreamIgnoreList').OnExecute := ActionAddToStreamIgnoreList;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Align := alRight;
  FSplitter.AutoSnap := False;
  FSplitter.ResizeStyle := rsUpdate;
  FSplitter.Parent := Self;

  FSideBar := TSidebar.Create(Self);
  FSideBar.Align := alRight;
  FSideBar.BorderSpacing.Top := Scale96ToFont(4);
  FSideBar.FDebugView.DebugView.OnClear := DebugClear;
  FSideBar.FBrowserView.StreamTree.OnAction := StreamBrowserAction;
  FSideBar.FBrowserView.StreamTree.OnIsInClientList := StreamBrowserIsInClientList;
  FSideBar.FBrowserView.StreamTree.PopupMenu2.Images := modSharedData.imgImages;
  FSideBar.Parent := Self;

  FClientView := TMClientView.Create(Self, Popup, FSideBar.FBrowserView.StreamTree);
  FClientView.Align := alClient;
  FClientView.Images := modSharedData.imgImages;
  FClientView.BorderSpacing.Top := Scale96ToFont(4);
  FClientView.OnSelectionChange := FClientViewSelectionChange;
  FClientView.OnNodeDblClick := FClientViewNodeDblClick;
  FClientView.OnKeyPress := FClientViewKeyPress;
  FClientView.OnKeyDown := FClientViewKeyDown;
  FClientView.OnStartStreaming := FClientViewStartStreaming;
  FClientView.Parent := Self;

  MsgBus.AddSubscriber(MessageReceived);

  FSplitter.Width := Scale96ToFont(4);
  FSplitter.MinSize := Scale96ToFont(220);
  FSplitter.Left := FSideBar.Left - FSplitter.Width - Scale96ToFont(5);
  FSideBar.Width := AppGlobals.SidebarWidth;
end;

procedure TClientTab.AddressBarStart(Sender: TObject);
var
  Entry: TRecentEntry;
begin
  if not Assigned(FAddressBar.Stations.FocusedItemData) then
    StartStreaming(TStartStreamingInfo.Create(0, 0, '', FAddressBar.Stations.Text, nil, nil, nil), AppGlobals.DefaultActionNewStream, nil, amNoWhere)
  else
  begin
    Entry := TRecentEntry(FAddressBar.Stations.FocusedItemData);
    StartStreaming(TStartStreamingInfo.Create(Entry.ID, Entry.Bitrate, Entry.Name, Entry.StartURL, nil, nil, nil), AppGlobals.DefaultActionNewStream, nil, amNoWhere);
  end;
end;

procedure TClientTab.DebugClear(Sender: TObject);
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, False));
  for Client in Clients do
    if Client.Client = FSideBar.FDebugView.DebugView.Client then
    begin
      Client.Client.DebugLog.Clear;
      Break;
    end;
end;

destructor TClientTab.Destroy;
begin
  MsgBus.RemoveSubscriber(MessageReceived);

  FreeAndNil(FPlaybackTimer);

  inherited;
end;

procedure TClientTab.ShowInfo;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Entries: TStreamList;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));

  Entries := TStreamList.Create;
  try
    for Client in Clients do
      Entries.Add(Client.Client.Entry);

    if Entries.Count > 0 then
      FSideBar.InfoView.ShowInfo(Entries)
    else
      FSideBar.InfoView.ShowInfo(nil);
  finally
    Entries.Free;
  end;
end;

procedure TClientTab.ClientManagerAddRecent(Sender: TObject);
var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  if not Client.AutoRemove then
  begin
    FAddressBar.Stations.AddItem(Client.Entry.ID, Client.Entry.Bitrate, Client.Entry.Name, Client.Entry.StartURL);

    if (Client.Entry.ID = 0) and (Trim(Client.Entry.Name) <> '') and (AppGlobals.SubmitStreamInfo) then
      FHomeCommunication.SendSubmitStream(Client.Entry.StartURL, Client.Entry.Name);
  end;

  ShowInfo;
end;

procedure TClientTab.ClientManagerLog(Sender: TObject);
begin
  if FSideBar.FDebugView.DebugView.Client = Sender then
    FSideBar.FDebugView.ShowDebug(TICEClient(Sender));
end;

procedure TClientTab.ClientManagerICYReceived(Sender: TObject; Received: Integer);
var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  FReceived := FReceived + Received;
  AppGlobals.Data.Received := AppGlobals.Data.Received + Received;
  Client.Entry.BytesReceived := Client.Entry.BytesReceived + Received;

  // Das ist raus, weil sowieso jede Sekunde das Event für MilliSecondsReceived kommt
  // FRefreshInfo := True;
end;

procedure TClientTab.ClientManagerPlaybackStarted(Sender: TObject);
begin
  FPlaybackSeconds := 0;
  if FPlaybackTimer <> nil then
  begin
    FPlaybackTimer.Enabled := False;
    FPlaybackTimer.Enabled := True;
  end;

  // Da man "Play" auch über einen Hotkey (Play, Next track, Prev track) machen kann,
  // rufen wir das hier auch nochmal auf. Ansonsten regeln das nämlich die Klicks
  // in der GUI, ganz unschön... könnte man gut über das Messaging-System regeln.
  if Assigned(FOnPlayStarted) then
    FOnPlayStarted(Self);
end;

procedure TClientTab.ClientManagerTitleAllowed(Sender: TObject; Title: string; var Allowed: Boolean; var Match: string; var Filter: Integer);

  function ContainsTitle(List: TSaveIgnoreList; Title: string; var Match: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    Title := LowerCase(Title);
    for i := 0 to List.Count - 1 do
      if TFunctions.Like(Title, List[i].Pattern) then
      begin
        Result := True;
        Match := List[i].Title;
        Exit;
      end;
  end;

var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;
  Allowed := False;
  Match := '';
  if Length(Title) < 1 then
    Exit;

  case Client.Entry.Settings.Filter of
    ufWish:
    begin
      Allowed := ContainsTitle(AppGlobals.Data.SaveList, Title, Match);
      Filter := 0;
    end;
    ufIgnoreGlobal:
    begin
      Allowed := not ContainsTitle(AppGlobals.Data.IgnoreList, Title, Match);
      Filter := 1;
    end;
    ufIgnoreLocal:
    begin
      Allowed := not ContainsTitle(Client.Entry.IgnoreList, Title, Match);
      Filter := 2;
    end;
    ufIgnoreBoth:
    begin
      Allowed := not ContainsTitle(AppGlobals.Data.IgnoreList, Title, Match);
      Filter := 1;

      if Allowed then
      begin
        Allowed := not ContainsTitle(Client.Entry.IgnoreList, Title, Match);
        Filter := 2;
      end;
    end;
    ufBoth:
    begin
      Allowed := ContainsTitle(AppGlobals.Data.SaveList, Title, Match);
      if Allowed then
      begin
        Allowed := not ContainsTitle(AppGlobals.Data.IgnoreList, Title, Match);
        Filter := 1;

        if Allowed then
        begin
          Allowed := not ContainsTitle(Client.Entry.IgnoreList, Title, Match);
          Filter := 2;
        end;
      end else
        Filter := 0;
    end else
    begin
      Allowed := True;
      Exit;
    end;
  end;
end;

procedure TClientTab.ClientManagerRefresh(Sender: TObject);
var
  i: Integer;
  OnePlaying: Boolean;
begin
  FClientView.RefreshClient(Sender as TICEClient);

  OnePlaying := False;
  for i := 0 to FClientManager.Count - 1 do
    if FClientManager[i].Playing and (FClientManager[i].State = csConnected) then
    begin
      OnePlaying := True;
      Break;
    end;

  if OnePlaying then
    FPlaybackTimer.Enabled := True
  else
  begin
    FPlaybackTimer.Enabled := False;
    FTimeLabel.Caption := '';
  end;

  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Sender);
end;

procedure TClientTab.ClientManagerClientAdded(Sender: TObject);
var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;
  FClientView.AddClient(Client);

  if Assigned(FOnClientAdded) then
    FOnClientAdded(Client);
end;

procedure TClientTab.ClientManagerClientRemoved(Sender: TObject);
var
  Client: TICEClient;
  Node, RemoveNode: PVirtualNode;
  NodeData: PClientNodeData;
  FreeCategory: TListCategory;
begin
  Client := Sender as TICEClient;

  FreeCategory := nil;
  RemoveNode := nil;

  // Wenn es zum Client eine Category gibt, die auf Killed = True ist,
  // und es keinen anderen Client mehr gibt, die Category entfernen.
  Node := FClientView.GetClientNode(Client);

  // Node kann irgendwie nil sein bei Programmende..
  if Node = nil then
    Exit;

  if Node.Parent <> FClientView.RootNode then
  begin
    NodeData := FClientView.GetNodeData(Node.Parent);
    if NodeData.Category.Killed and (FClientView.ChildCount[Node.Parent] <= 1) then
    begin
      FreeCategory := NodeData.Category;
      RemoveNode := Node.Parent;
    end;
  end;

  FClientView.RemoveClient(Client);

  if FSidebar.FDebugView.DebugView.Client = Client then
    FSidebar.FDebugView.ShowDebug(nil);

  ShowInfo;

  if RemoveNode <> nil then
  begin
    AppGlobals.Data.CategoryList.Remove(FreeCategory);
    FreeCategory.Free;
    FClientView.DeleteNode(RemoveNode);
  end;

  // Um die Markierung für "Ist in Liste" wegzubekommen
  SideBar.FBrowserView.StreamTree.InvalidateVisible;

  if Assigned(FOnClientRemoved) then
    FOnClientRemoved(Client);
end;

procedure TClientTab.ClientManagerSecondsReceived(Sender: TObject);
var
  Clients: TNodeDataArray;
begin
  FRefreshInfo := True;

  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
  if Length(Clients) = 1 then
    ShowInfo;
end;

procedure TClientTab.ClientManagerShowErrorMessage(Sender: TObject; Data: string);
begin
  if Assigned(FOnShowErrorMessage) then
    FOnShowErrorMessage(Sender, Data);
end;

procedure TClientTab.ClientManagerSongSaved(Sender: TObject; Filename, Title, SongArtist, SongTitle: string; Filesize: Int64; Length, Bitrate: Cardinal; VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
  ServerTitleHash, ServerArtistHash: Cardinal);
var
  Client: TICEClient;
  Track: TTrackInfo;
  i: Integer;
  LowerFilename: string;
  Added: Boolean;
begin
  Client := Sender as TICEClient;

  // Das hier ist NICHT cool, aber alles andere wäre Wahnsinn!
  if SongArtist = _('Unknown artist') then
    SongArtist := '';
  if SongTitle = _('Unknown title') then
    SongTitle := '';

  Added := False;
  Track := nil;
  LowerFilename := LowerCase(Filename);
  for i := 0 to AppGlobals.Data.TrackList.Count - 1 do
    if LowerCase(AppGlobals.Data.TrackList[i].Filename) = LowerFilename then
    begin
      Track := AppGlobals.Data.TrackList[i];
      Track.Time := Now;
      Break;
    end;

  if Track = nil then
  begin
    Track := TTrackInfo.Create(Now, Filename, Client.Entry.DisplayName, Title, SongArtist, SongTitle, ServerTitleHash, ServerArtistHash);
    AppGlobals.Data.TrackList.Add(Track);
    Added := True;
  end;

  Track.Streamname := Client.Entry.DisplayName;
  Track.Filesize := Filesize;
  Track.Length := Length;
  Track.WasCut := WasCut;
  Track.Bitrate := Bitrate;
  Track.IsAuto := Client.AutoRemove;
  Track.RecordBecauseArtist := Client.RecordBecauseArtist;
  Track.IsStreamFile := IsStreamFile;
  Track.VBR := VBR;

  if Added then
    if Assigned(FOnTrackAdded) then
      FOnTrackAdded(Client.Entry, Track);

  if FullTitle and (not IsStreamFile) then
  begin
    if Client.Entry.Settings.AddSavedToIgnore then
      if Assigned(FOnAddTitleToList) then
        FOnAddTitleToList(Self, nil, ltIgnore, Track.ParsedTitle);

    if Client.Entry.Settings.AddSavedToStreamIgnore then
      if Assigned(FOnAddTitleToList) then
        FOnAddTitleToList(Self, Client, ltIgnore, Track.ParsedTitle);

    if Client.Entry.Settings.RemoveSavedFromWishlist then
      if Assigned(FOnRemoveTitleFromList) then
        FOnRemoveTitleFromList(Self, nil, ltSave, Track.ParsedTitle, ServerTitleHash);
  end;

  ShowInfo;
end;

procedure TClientTab.ClientManagerClientTitleChanged(Sender: TObject; Title: string);
begin
  // Ist hier, weil wenn FFilename im Client gesetzt wird, das hier aufgerufen wird.
  // Relativ unschön so, aber Hauptsache es tut..
  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Sender);

  FPlaybackSeconds := 0;
end;

procedure TClientTab.FClientViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    FActionRemove.Execute;
end;

procedure TClientTab.FClientViewStartStreaming(Sender: TObject; ID, Bitrate: Cardinal; Name, URL: string; URLs, RegExes, IgnoreTitles: TStringList; AddOnly: Boolean; Node: PVirtualNode; Mode: TVTNodeAttachMode);
begin
  StartStreaming(TStartStreamingInfo.Create(ID, Bitrate, Name, URL, URLs, RegExes, IgnoreTitles), IfThen<TStreamOpenActions>(AddOnly, oaAdd, AppGlobals.DefaultActionNewStream), Node, Mode);
end;

procedure TClientTab.MessageReceived(Msg: TMessageBase);
var
  VolMsg: TVolumeChangedMsg;
begin
  if Msg is TVolumeChangedMsg then
  begin
    VolMsg := TVolumeChangedMsg(Msg);

    if VolMsg.Volume <> FVolume.Volume then
      FVolume.Volume := TVolumeChangedMsg(Msg).Volume;
  end;
end;

procedure TClientTab.PausePlay;
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
  for Client in Clients do
    if Client.Playing and (not Client.Paused) then
      Client.PausePlay;
end;

procedure TClientTab.PlaybackTimerTimer(Sender: TObject);
begin
  Inc(FPlaybackSeconds);

  FTimeLabel.Caption := BuildTime(FPlaybackSeconds, False);
end;

procedure TClientTab.CreateHandle;
begin
  inherited CreateHandle;

  // A Handle is required since we expand nodes in BuildTree which does not work otherwise.
  BuildTree;
end;

procedure TClientTab.ShownFirst;
begin
  inherited;

  FAddressBar.Stations.ApplyFocus;
end;

procedure TClientTab.FClientViewKeyPress(Sender: TObject; var Key: Char);
var
  Dummy: THitInfo;
begin
  if Key = Char(VK_RETURN) then
  begin
    Dummy.HitPositions := [hiOnItem];
    FClientViewNodeDblClick(FClientView, Dummy);
    Key := #0;
  end;
end;

procedure TClientTab.FClientViewNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  Clients: TNodeDataArray;
  Res: TMayConnectResults;
begin
  if hiOnItemButton in HitInfo.HitPositions then
    Exit;

  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
  if Length(Clients) = 1 then
    case AppGlobals.DefaultAction of
      caStartStop:
      begin
        if Clients[0].Client.AutoRemove then
          Exit;

        if Clients[0].Client.Recording then
          Clients[0].Client.StopRecording
        else
        begin
          Res := Clients[0].Client.StartRecording(True);
          if Res <> crOk then
          begin
            Clients[0].Client.WriteLog(FClientManager.GetErrorText(Res, '', False, False, True), '', ltGeneral, llWarning);
            OnShowErrorMessage(Clients[0].Client, FClientManager.GetErrorText(Res, '', False, False, False));
          end;
        end;
      end;
      caStreamIntegrated:
        if Clients[0].Client.Playing then
          if Clients[0].Client.Paused then
            FActionPlay.Execute
          else
            FActionStopPlay.Execute
        else
          FActionPlay.Execute;
      caStream:
        FActionTuneInStream.Execute;
    end;
end;

procedure TClientTab.FClientViewSelectionChange(Sender: TObject);
var
  Clients: TNodeDataArray;
begin
  if Assigned(OnUpdateButtons) then
    OnUpdateButtons(Self);
  ShowInfo;

  if FClientView.SelectedCount = 1 then
  begin
    Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
    if Length(Clients) = 1 then
      FSideBar.FDebugView.ShowDebug(Clients[0].Client);
  end else
    FSideBar.FDebugView.ShowDebug(nil);
end;

function TClientTab.StartStreaming(Streams: TStartStreamingInfoArray; Action: TStreamOpenActions; HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean;

  procedure UnkillCategory;
  var
    NodeData: PClientNodeData;
  begin
    NodeData := FClientView.GetNodeData(HitNode);
    if Assigned(NodeData) and Assigned(NodeData.Category) then
      NodeData.Category.Killed := False;
  end;

  procedure PlayStarted(Client: TICEClient);
  var
    Clients: TClientArray;
    C: TICEClient;
  begin
    Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
    for C in Clients do
      if C <> Client then
        C.StopPlay;
    if Assigned(FOnPlayStarted) then
      FOnPlayStarted(Self);
  end;

var
  i: Integer;
  Client: TICEClient;
  Node: PVirtualNode;
  Res: TMayConnectResults;
  PH: TPlaylistHandler;
  Entries: TPlaylistEntryArray;
  Info: TStartStreamingInfo;
  MessagesShown: TMayConnectResultsSet = [];
begin
  Result := True;

  // Sonderbehandlung fürs extern abspielen...
  if Action = oaPlayExternal then
  begin
    SetLength(Entries, 0);
    for Info in Streams do
    begin
      SetLength(Entries, Length(Entries) + 1);
      Entries[High(Entries)].URL := Info.URL;
      if Info.Name <> '' then
        Entries[High(Entries)].Name := Info.Name
      else
        Entries[High(Entries)].Name := Info.URL;
    end;
    SavePlaylist(Entries, True);
    Exit;
  end;

  for Info in Streams do
    if Info.URL <> '' then
    begin
      // Falls eine Datei gemeint ist...
      if FileExists(Info.URL) then
      begin
        PH := TPlaylistHandler.Create;
        try
          PH.ParsePlaylist(Info.URL);
          for i := 0 to PH.URLs.Count - 1 do
            StartStreaming(TStartStreamingInfo.Create(Info.ID, Info.Bitrate, Info.Name, PH.URLs[i], nil, Info.RegExes, Info.IgnoreTitles),
              oaAdd, HitNode, Mode);
        finally
          PH.Free;
        end;
        Exit;
      end;

      // Ist der Client schon in der Liste?
      Client := FClientManager.GetClient(Info.ID, '', Info.URL, nil);
      if (Client <> nil) and (not Client.AutoRemove) then
      begin
        case Action of
          oaStart:
            Res := Client.StartRecording(True);
          oaPlay:
          begin
            Res := Client.StartPlay(True);
            if Res = crOk then
              PlayStarted(Client);
          end;
          else
            Res := crOk;
        end;

        if Res = crOk then
          UnkillCategory
        else
        begin
          Client.WriteLog(FClientManager.GetErrorText(Res, '', False, False, True), '', ltGeneral, llWarning);
          if not (Res in MessagesShown) then
          begin
            MessagesShown := MessagesShown + [Res];
            OnShowErrorMessage(Client, FClientManager.GetErrorText(Res, '', False, False, False));
          end;
        end;
      end else if TFunctions.ValidURL(Info.URL) then
      begin
        Client := FClientManager.AddClient(Info.ID, Info.Bitrate, Info.Name, Info.URL);

        if Info.URLs <> nil then
          Client.Entry.URLs.Assign(Info.URLs);

        if Info.RegExes <> nil then
          Client.Entry.Settings.RegExes.Assign(Info.RegExes);

        if Info.IgnoreTitles <> nil then
          Client.Entry.Settings.IgnoreTrackChangePattern.Assign(Info.IgnoreTitles);

        if HitNode <> nil then
        begin
          Node := FClientView.GetClientNode(Client);
          FClientView.MoveTo(Node, HitNode, Mode, False);
        end;

        case Action of
          oaStart:
            Res := Client.StartRecording(True);
          oaPlay:
          begin
            Res := Client.StartPlay(True);
            if Res = crOk then
              PlayStarted(Client);
          end;
          else
            Res := crOk;
        end;

        if Res = crOk then
          UnkillCategory
        else
        begin
          Client.WriteLog(FClientManager.GetErrorText(Res, '', False, False, True), '', ltGeneral, llWarning);
          if not (Res in MessagesShown) then
          begin
            MessagesShown := MessagesShown + [Res];
            OnShowErrorMessage(Client, FClientManager.GetErrorText(Res, '', False, False, False));
          end;
        end;
      end else
      begin
        Result := False;
        TFunctions.MsgBox(_('The stream could not be added to the list because the URL is invalid.'), _('Info'), MB_ICONINFORMATION);
      end;
    end;
end;

function TClientTab.StartStreaming(Stream: TStartStreamingInfo; Action: TStreamOpenActions; HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean;
var
  Arr: TStartStreamingInfoArray;
begin
  SetLength(Arr, 1);
  Arr[0] := Stream;
  Result := StartStreaming(Arr, Action, HitNode, Mode);
end;

function TClientTab.StopStreaming(Info: TStartStreamingInfo; Action: TStreamOpenActions): Boolean;
var
  Client: TICEClient;
begin
  Result := False;

  if Info.URL <> '' then
  begin
    Client := FClientManager.GetClient(Info.ID, '', Info.URL, nil);
    if (Client <> nil) and (not Client.AutoRemove) then
    begin
      case Action of
        oaStart:
          Client.StopRecording;
        oaPlay:
          Client.StopPlay;
      end;
      Result := True;
    end;
  end;
end;

procedure TClientTab.StreamBrowserAction(Sender: TObject; Action: TStreamOpenActions; Streams: TStreamDataArray);

  procedure Rate(R: Integer);
  var
    Node: PVirtualNode;
    ND: PStreamNodeData;
  begin
    if not HomeComm.Authenticated then
      FOnAuthRequired(Self)
    else
    begin
      HomeComm.SendSetStreamData(Streams[0].ID, R);

      Node := FSideBar.FBrowserView.StreamTree.GetNodes(True)[0];
      ND := FSideBar.FBrowserView.StreamTree.GetNodeData(Node);
      if ND <> nil then
      begin
        ND.Data.OwnRating := R;
        FSideBar.FBrowserView.StreamTree.InvalidateNode(Node);
      end;
    end;
  end;

var
  i: Integer;
  s: string;
  Entries: TPlaylistEntryArray;
  Arr: TStartStreamingInfoArray;
  Stream: TStreamData;
begin
  if Action in [oaPlayExternal, oaSave] then
  begin
    SetLength(Entries, Length(Streams));
    for i := 0 to High(Streams) do
    begin
      Entries[i].Name := Streams[i].Name;
      Entries[i].URL := Streams[i].URL;
    end;
  end;

  if Action in [oaStart, oaPlay, oaAdd] then
  begin
    SetLength(Arr, Length(Streams));
    for i := 0 to High(Streams) do
      Arr[i] := TStartStreamingInfo.Create(Streams[i].ID, Streams[i].Bitrate, Streams[i].Name, Streams[i].URL, Streams[i].URLs, Streams[i].RegExes, Streams[i].IgnoreTitles);
    StartStreaming(Arr, Action, nil, amNoWhere);
    Exit;
  end;

  case Action of
    oaPlayExternal:
      SavePlaylist(Entries, True);
    oaOpenWebsite:
      for Stream in Streams do
        TFunctions.ShellExecute(Handle, 'open', Stream.Website);
    oaBlacklist:
      for Stream in Streams do
        if Stream.Name <> '' then
          if AppGlobals.Data.StreamBlacklist.IndexOf(Stream.Name) = -1 then
            AppGlobals.Data.StreamBlacklist.Add(Stream.Name);
    oaCopy:
    begin
      s := '';
      for Stream in Streams do
        s := s + Stream.URL + #13#10;
      s := Trim(s);
      Clipboard.AsText := s;
    end;
    oaSave:
      SavePlaylist(Entries, False);
    oaRefresh:
      FSideBar.FBrowserView.RefreshStreams;
    oaSetData:
      if HomeComm.CommunicationEstablished and HomeComm.Authenticated then
        FOnSetStreamData(Self, Streams[0].ID)
      else if not HomeComm.CommunicationEstablished then
        TFunctions.MsgBox(_('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION)
      else if not HomeComm.Authenticated then
        FOnAuthRequired(Self);
    oaRate1:
      Rate(1);
    oaRate2:
      Rate(2);
    oaRate3:
      Rate(3);
    oaRate4:
      Rate(4);
    oaRate5:
      Rate(5);
  end;
end;

function TClientTab.StreamBrowserIsInClientList(Sender: TObject; ID: Cardinal): Boolean;
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
  for Client in Clients do
    if Client.Entry.ID = ID then
      Exit(True);
  Exit(False);
end;

function TClientTab.VolumeGetVolumeBeforeMute(Sender: TObject): Integer;
begin
  Result := Players.VolumeBeforeMute;
end;

procedure TClientTab.VolumeVolumeChange(Sender: TObject);
begin
  Players.Volume := FVolume.Volume;
  if FVolume.VolumeBeforeDrag > -1 then
    Players.VolumeBeforeMute := FVolume.VolumeBeforeDrag;
end;

procedure TClientTab.TimerTick;
begin
  if FRefreshInfo then
  begin
    ShowInfo;
    FRefreshInfo := False;
  end;
end;

procedure TClientTab.UpdateStreams;
var
  CatIdx: Integer;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  Node: PVirtualNode;
  StreamEntry: TStreamEntry;
  RecentEntry: TRecentEntry;
  CategoryEntry: TListCategory;
  OldCategories: TListCategoryList;
  ComboItem: TCollectionItem;
begin
  CatIdx := 0;

  for StreamEntry in AppGlobals.Data.StreamList do
    StreamEntry.Free;
  AppGlobals.Data.StreamList.Clear;

  for RecentEntry in AppGlobals.Data.RecentList do
    RecentEntry.Free;
  AppGlobals.Data.RecentList.Clear;

  for ComboItem in FAddressBar.Stations.ItemsEx do
    AppGlobals.Data.RecentList.Add(TRecentEntry(TComboExItem(ComboItem).Data).Copy);

  OldCategories := TListCategoryList.Create;
  try
    for CategoryEntry in AppGlobals.Data.CategoryList do
      OldCategories.Add(CategoryEntry);

    Nodes := FClientView.GetNodes(ntAll, False);
    for Node in Nodes do
    begin
      NodeData := FClientView.GetNodeData(Node);

      if NodeData.Client <> nil then
      begin
        if NodeData.Client.AutoRemove then
          Continue;

        StreamEntry := NodeData.Client.Entry.Copy;
        StreamEntry.Index := Node.Index;
        StreamEntry.CategoryIndex := 0;
        if Node.Parent <> FClientView.RootNode then
          StreamEntry.CategoryIndex := CatIdx;
        AppGlobals.Data.StreamList.Add(StreamEntry);
      end else
      begin
        CatIdx := Node.Index + 1;
        CategoryEntry := TListCategory.Create(NodeData.Category.Name, CatIdx);
        CategoryEntry.Expanded := FClientView.Expanded[Node];
        CategoryEntry.IsAuto := NodeData.Category.IsAuto;
        AppGlobals.Data.CategoryList.Add(CategoryEntry);

        // Weil hier nicht mit Kopien gearbeitet wird Referenz ändern
        NodeData.Category := CategoryEntry;
      end;
    end;

    // Alte Kategorien erst hier löschen, weil ich an der Stelle
    // nicht wie bei den StreamEntries mit Kopien arbeite.
    for CategoryEntry in OldCategories do
    begin
      AppGlobals.Data.CategoryList.Remove(CategoryEntry);
      CategoryEntry.Free;
    end;
  finally
    OldCategories.Free;
  end;
end;

procedure TClientTab.BuildTree;
var
  i: Integer;
  Client: TICEClient;
  Cat: TListCategory;
  Node, ParentNode: PVirtualNode;
begin
  for i := 0 to AppGlobals.Data.CategoryList.Count - 1 do
    FClientView.AddCategory(AppGlobals.Data.CategoryList[i]);

  for i := 0 to AppGlobals.Data.StreamList.Count - 1 do
  begin
    Client := FClientManager.AddClient(AppGlobals.Data.StreamList[i]);
    Node := FClientView.GetClientNode(Client);
    if Client <> nil then
    begin
      if AppGlobals.Data.StreamList[i].CategoryIndex > 0 then
      begin
        ParentNode := FClientView.GetCategoryNode(AppGlobals.Data.StreamList[i].CategoryIndex);
        if ParentNode <> nil then
          FClientView.MoveTo(Node, ParentNode, amAddChildLast, False);
      end;
      if AppGlobals.Data.StreamList[i].WasRecording and AppGlobals.RememberRecordings then
        Client.StartRecording(True);
      if AppGlobals.Data.StreamList[i].WasPlaying and AppGlobals.RememberPlaying then
        Client.StartPlay(False);
      Client.Entry.WasRecording := False;
      Client.Entry.WasPlaying := False;
    end;
  end;

  for i := 0 to AppGlobals.Data.CategoryList.Count - 1 do
  begin
    Node := FClientView.GetCategoryNode(AppGlobals.Data.CategoryList[i].Index);
    if AppGlobals.Data.CategoryList[i].Expanded then
      FClientView.Expanded[Node] := True;
  end;

  if FClientView.AutoNode = nil then
  begin
    Cat := TListCategory.Create(_('Automatic recordings'), High(Integer));
    Cat.IsAuto := True;
    FClientView.AddCategory(Cat);
    AppGlobals.Data.CategoryList.Add(Cat);
  end;

  Cat := PClientNodeData(FClientView.GetNodeData(FClientView.AutoNode)).Category;
  Cat.Name := _('Automatic recordings');

  FClientView.SortItems;
end;

{ TSidebar }

constructor TSidebar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FPage1 := TTabSheet.Create(Self);
  FPage1.PageControl := Self;
  FPage1.Caption := 'Browser';

  FPage2 := TTabSheet.Create(Self);
  FPage2.PageControl := Self;
  FPage2.Caption := 'Info';

  FPage3 := TTabSheet.Create(Self);
  FPage3.PageControl := Self;
  FPage3.Caption := 'Log';

  FBrowserView := TMStreamBrowserView.Create(Self);
  FBrowserView.Parent := FPage1;

  FInfoView := TMStreamInfoView.Create(Self);
  FInfoView.Parent := FPage2;

  FDebugView := TMStreamDebugView.Create(Self);
  FDebugView.Parent := FPage3;
end;

end.
