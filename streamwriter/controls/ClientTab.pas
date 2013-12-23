{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, ClientView, StreamBrowserView, StreamDebugView, StreamInfoView,
  LanguageObjects, HomeCommunication, StationCombo, Menus, ActnList, ImgList,
  DataManager, ICEClient, ClientManager, VirtualTrees, Clipbrd, Functions,
  GUIFunctions, AppData, DragDrop, DropTarget, DropComboTarget, Tabs,
  Graphics, SharedControls, Generics.Collections, Generics.Defaults, Math,
  Logging, DynBass, Forms, MsgDlg, TypeDefs, MessageBus, AppMessages,
  PlayerManager, PlaylistHandler, AudioFunctions, SharedData, PngSpeedButton,
  Dialogs, ShellAPI;

type
  TSidebar = class(TPageControl)
  private
    FDataLists: TDataLists;

    FPage1, FPage2, FPage3: TTabSheet;

    FBrowserView: TMStreamBrowserView;
    FInfoView: TMStreamInfoView;
    FDebugView: TMStreamDebugView;
  public
    constructor Create(AOwner: TComponent; DataLists: TDataLists); reintroduce;
    destructor Destroy; override;
    procedure AfterCreate;

    property BrowserView: TMStreamBrowserView read FBrowserView;
    property InfoView: TMStreamInfoView read FInfoView;
    property DebugView: TMStreamDebugView read FDebugView;
  end;

  TClientAddressBar = class(TPanel)
  private
    FLabel: TLabel;
    FStations: TMStationCombo;
    FStart: TPngSpeedButton;
    FDropTarget: TDropComboTarget;

    FOnStart: TNotifyEvent;

    procedure FStationsChange(Sender: TObject);
    procedure FStationsKeyPress(Sender: TObject; var Key: Char);
    procedure FStartClick(Sender: TObject);

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterCreate;

    property Stations: TMStationCombo read FStations;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

  TAddTitleEvent = procedure(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string) of object;
  TAddTitleEventWithServerHash = procedure(Sender: TObject; Client: TICEClient; ListType: TListType; Title: string;
    ServerTitleHash: Cardinal) of object;

  TClientTab = class(TMainTabSheet)
  private
    FToolbarPanel: TPanel;
    FTimeLabel: TLabel;
    FVolume: TVolumePanel;
    FToolbar: TToolBar;
    FAddressBar: TClientAddressBar;
    FClientView: TMClientView;
    FSplitter: TSplitter;
    FSideBar: TSideBar;

    FClients: TClientManager;
    FStreams: TDataLists;
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
    FActionTuneInFile: TAction;
    FActionTuneInStream: TAction;

    FOnUpdateButtons: TNotifyEvent;
    FOnTrackAdded: TTrackEvent;
    FOnTrackRemoved: TTrackEvent;
    FOnPlayStarted: TNotifyEvent;
    FOnAuthRequired: TNotifyEvent;
    FOnShowErrorMessage: TShowErrorMessageEvent;
    FOnClientAdded: TNotifyEvent;
    FOnClientRemoved: TNotifyEvent;
    FOnAddTitleToList: TAddTitleEvent;
    FOnRemoveTitleFromList: TAddTitleEventWithServerHash;

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
    procedure ActionSavePlaylistFileExecute(Sender: TObject);
    procedure ActionTuneInStreamExecute(Sender: TObject);
    procedure ActionTuneInFileExecute(Sender: TObject);
    procedure ActionCopyTitleExecute(Sender: TObject);
    procedure ActionAddToSaveListExecute(Sender: TObject);
    procedure ActionAddToGlobalIgnoreList(Sender: TObject);
    procedure ActionAddToStreamIgnoreList(Sender: TObject);

    procedure ClientManagerDebug(Sender: TObject);
    procedure ClientManagerRefresh(Sender: TObject);
    procedure ClientManagerAddRecent(Sender: TObject);
    procedure ClientManagerClientAdded(Sender: TObject);
    procedure ClientManagerClientRemoved(Sender: TObject);
    procedure ClientManagerSongSaved(Sender: TObject; Filename, Title, SongArtist, SongTitle: string;
      Filesize, Length, Bitrate: UInt64; VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
      ServerTitleHash, ServerArtistHash: Cardinal);
    procedure ClientManagerTitleChanged(Sender: TObject; Title: string);
    procedure ClientManagerICYReceived(Sender: TObject; Received: Integer);
    procedure ClientManagerTitleAllowed(Sender: TObject; Title: string;
      var Allowed: Boolean; var Match: string; var Filter: Integer);
    procedure ClientManagerShowErrorMessage(Sender: TICEClient; Msg: TMayConnectResults; WasAuto, WasScheduled: Boolean);
    procedure ClientManagerPlaybackStarted(Sender: TObject);
    procedure ClientManagerSecondsReceived(Sender: TObject);

    procedure FClientViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FClientViewDblClick(Sender: TObject);
    procedure FClientViewKeyPress(Sender: TObject; var Key: Char);
    procedure FClientViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FClientViewStartStreaming(Sender: TObject; ID, Bitrate: Cardinal; Name, URL: string; RegExes,
      IgnoreTitles: TStringList; Node: PVirtualNode; Mode: TVTNodeAttachMode);

    procedure StreamBrowserAction(Sender: TObject; Action: TStreamOpenActions; Streams: TStreamDataArray);
    function StreamBrowserIsInClientList(Sender: TObject; ID: Cardinal): Boolean;

    procedure VolumeVolumeChange(Sender: TObject);
    function VolumeGetVolumeBeforeMute(Sender: TObject): Integer;

    procedure AddressBarStart(Sender: TObject);

    procedure DebugClear(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);

    procedure PlaybackTimerTimer(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; Toolbar: TToolbar; Actions: TActionList;
      Clients: TClientManager; Streams: TDataLists; Popup: TPopupMenu); reintroduce;
    destructor Destroy; override;
    procedure AfterCreate; override;

    function StartStreaming(Streams: TStartStreamingInfoArray; Action: TStreamOpenActions; HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean; overload;
    function StartStreaming(Stream: TStartStreamingInfo; Action: TStreamOpenActions; HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean; overload;
    procedure TimerTick;
    procedure UpdateStreams(Streams: TDataLists);
    procedure BuildTree(Streams: TDataLists);
    procedure PausePlay;
    procedure ShowInfo;

    procedure AdjustTextSizeDirtyHack;

    property AddressBar: TClientAddressBar read FAddressBar;
    property ClientView: TMClientView read FClientView;
    property SideBar: TSideBar read FSideBar;
    property Received: UInt64 read FReceived;

    property OnUpdateButtons: TNotifyEvent read FOnUpdateButtons write FOnUpdateButtons;
    property OnTrackAdded: TTrackEvent read FOnTrackAdded write FOnTrackAdded;
    property OnTrackRemoved: TTrackEvent read FOnTrackRemoved write FOnTrackRemoved;
    property OnPlayStarted: TNotifyEvent read FOnPlayStarted write FOnPlayStarted;
    property OnAuthRequired: TNotifyEvent read FOnAuthRequired write FOnAuthRequired;
    property OnShowErrorMessage: TShowErrorMessageEvent read FOnShowErrorMessage write FOnShowErrorMessage;
    property OnClientAdded: TNotifyEvent read FOnClientAdded write FOnClientAdded;
    property OnClientRemoved: TNotifyEvent read FOnClientRemoved write FOnClientRemoved;
    property OnAddTitleToList: TAddTitleEvent read FOnAddTitleToList write FOnAddTitleToList;
    property OnRemoveTitleFromList: TAddTitleEventWithServerHash read FOnRemoveTitleFromList write FOnRemoveTitleFromList;
  end;

implementation

{ TClientAddressBar }

constructor TClientAddressBar.Create(AOwner: TComponent);
begin
  inherited;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;

  FStart := TPngSpeedButton.Create(Self);
  FStart.Parent := Self;

  FStations := TMStationCombo.Create(Self);
  FStations.Parent := Self;

  FDropTarget := TDropComboTarget.Create(Self);
end;

destructor TClientAddressBar.Destroy;
begin

  inherited;
end;

procedure TClientAddressBar.DropTargetDrop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
begin
  FStations.ItemIndex := -1;
  if FDropTarget.URL <> '' then
    FStations.Text := string(FDropTarget.URL)
  else if FDropTarget.Text <> '' then
    FStations.Text := string(FDropTarget.Text)
  else if FDropTarget.Files.Count > 0 then
    FStations.Text := string(FDropTarget.Files[0]);
end;

procedure TClientAddressBar.AfterCreate;
begin
  FLabel.Left := 0;
  FLabel.Caption := 'Playlist/Stream-URL:';

  FStart.Width := 24;
  FStart.Height := 22;
  FStart.Left := ClientWidth - 1 - FStart.Width;
  FStart.Anchors := [akRight];
  FStart.Flat := True;
  FStart.Hint := 'Add and start recording';
  FStart.ShowHint := True;
  FStart.NumGlyphs := 1;
  FStart.OnClick := FStartClick;
  FStart.PngImage := modSharedData.imgImages.PngImages[11].PngImage;

  FStations.DropDownCount := 15;
  FStations.Left := FLabel.Left + FLabel.Width + 6;
  FStations.Top := 3;
  FStations.Width := ClientWidth - FStations.Left - FStart.Width - 6;
  FStations.Anchors := [akLeft, akTop, akRight];
  FStations.OnKeyPress := FStationsKeyPress;
  FStations.OnChange := FStationsChange;
  FStations.Images := modSharedData.imgImages;

  FLabel.Top := FStations.Top + FStations.Height div 2 - FLabel.Height div 2;

  FDropTarget.Formats := [mfText, mfURL, mfFile];
  FDropTarget.Register(FStations);
  FDropTarget.OnDrop := DropTargetDrop;

  BevelOuter := bvNone;

  ClientHeight := Max(FLabel.Height + FLabel.Top * 2, FStations.Height + FStations.Top * 2) - 2;

  // Das muss nach dem Setzen der ClientHeight. KA warum, aber die ClientHeight ändert FStart.Top!
  FStart.Top := 3;

  FStart.Enabled := False;
end;

procedure TClientAddressBar.FStationsChange(Sender: TObject);
begin
  FStart.Enabled := (Length(Trim(FStations.Text)) > 0) or (FStations.ItemIndex > -1);
end;

procedure TClientAddressBar.FStationsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    FStart.Click;
  end else
  begin
    FStations.ItemIndex := -1;
  end;
end;

procedure TClientAddressBar.FStartClick(Sender: TObject);
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

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
  begin
    if Client.Title <> '' then
      Title := Title + Client.Title + #13#10;
  end;
  Title := Trim(Title);
  if Title <> '' then
  begin
    Clipboard.SetTextBuf(PChar(Title));
  end;
end;

procedure TClientTab.ActionNewCategoryExecute(Sender: TObject);
var
  NodeData: PClientNodeData;
begin
  NodeData := FClientView.GetNodeData(FClientView.AddCategory);
  FStreams.CategoryList.Add(NodeData.Category);
end;

procedure TClientTab.ActionStartExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
  Node: PVirtualNode;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  Res: TMayConnectResults;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
  begin
    if not Client.AutoRemove then
    begin
      Res := Client.StartRecording(True);
      if Res <> crOk then
      begin
        OnShowErrorMessage(Client, Res, False, False);
        Exit;
      end;
    end;
  end;

  Nodes := FClientView.GetNodes(ntCategory, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if not NodeData.Category.IsAuto then
    begin
      Clients := FClientView.NodesToClients(FClientView.GetChildNodes(Node));
      for Client in Clients do
      begin
        Res := Client.StartRecording(True);
        if Res <> crOk then
        begin
          OnShowErrorMessage(Client, Res, False, False);
          Exit;
        end;
      end;
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
var
  Clients: TClientArray;
  Client: TICEClient;
  Node: PVirtualNode;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
  begin
    if not Client.AutoRemove then
      Client.StopRecording;
  end;

  Nodes := FClientView.GetNodes(ntCategory, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if not NodeData.Category.IsAuto then
    begin
      Clients := FClientView.NodesToClients(FClientView.GetChildNodes(Node));
      for Client in Clients do
      begin
        Client.StopRecording;
      end;
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
    if TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('All selected streams will be removed from the list. This also means that their ' +
                                                 'settings and ignorelists get deleted.'#13#10'Are you sure you want to continue?'),
                                                 mtConfirmation, [mbOK, mbCancel], mbCancel, 8) = mrCancel
    then
      Exit;

  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if NodeData.Client <> nil then
      FClients.RemoveClient(NodeData.Client);
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
        begin
          if ChildNode.Parent = Node then
          begin
            ChildNodeData := FClientView.GetNodeData(ChildNode);
            FClients.RemoveClient(ChildNodeData.Client);
          end;
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
      FStreams.CategoryList.Remove(NodeData.Category);
      NodeData.Category.Free;
      FClientView.DeleteNode(Node);
    end else
    begin
      NodeData.Category.Killed := True;
    end;
  end;
end;

procedure TClientTab.ActionRenameExecute(Sender: TObject);
begin
  FClientView.EditNode(FClientView.GetFirstSelected, 0);
end;

procedure TClientTab.ActionPlayExecute(Sender: TObject);
var
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

  if SelectedClient.Client.StartPlay(True) <> crOk then
    OnShowErrorMessage(SelectedClient.Client, crNoBandwidth, False, False)
  else
    if Assigned(FOnPlayStarted) then
      FOnPlayStarted(Self);

  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Sender);
end;

procedure TClientTab.ActionPauseExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));

  for Client in Clients do
  begin
    if Client.Playing and (Client.Paused) then
    begin
      Client.PausePlay;
      if Assigned(FOnPlayStarted) then
        FOnPlayStarted(Self);
    end;
  end;

  for Client in Clients do
  begin
    if (Client.Playing) and (not Client.Paused) then
    begin
      Client.PausePlay;
      if Assigned(FOnPlayStarted) then
        FOnPlayStarted(Self);
    end;
  end;

  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Sender);
end;

procedure TClientTab.ActionPlayStopExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
  for Client in Clients do
  begin
    Client.StopPlay;
  end;
end;

procedure TClientTab.ActionOpenWebsiteExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
    ShellExecute(Handle, 'open', PChar(Client.Entry.StreamURL), '', '', 1);
end;

procedure TClientTab.ActionResetDataExecute(Sender: TObject);
var
  Res: Integer;
  Clients: TNodeDataArray;
  Client: PClientNodeData;
begin
  Res := MsgBox(GetParentForm(Self).Handle,
                _('This will reset the saved song and bytes received counters.'#13#10 +
                  'The tracknumber of new saved titles will be 1 if you specified the tracknumber in the filename pattern, this number will also be set in ID3 tags.'#13#10 +
                  'Do you want to continue?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
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

procedure TClientTab.ActionSavePlaylistFileExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etFile);
  SavePlaylist(Entries, False);
end;

procedure TClientTab.ActionTuneInStreamExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etStream);
  SavePlaylist(Entries, True);
end;

procedure TClientTab.ActionTuneInFileExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  if FActionTuneInFile.Enabled then
  begin
    Entries := FClientView.GetEntries(etFile);
    SavePlaylist(Entries, True);
  end;
end;

constructor TClientTab.Create(AOwner: TComponent; Toolbar: TToolbar; Actions: TActionList; Clients: TClientManager;
  Streams: TDataLists; Popup: TPopupMenu);
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

  ClientHeight := 345;

  ShowCloseButton := False;
  ImageIndex := 68;

  FPlaybackTimer := TTimer.Create(Self);
  FPlaybackTimer.Interval := 1000;
  FPlaybackTimer.Enabled := False;
  FPlaybackTimer.OnTimer := PlaybackTimerTimer;

  FRefreshInfo := False;
  FReceived := 0;

  FClients := Clients;
  FClients.OnClientDebug := ClientManagerDebug;
  FClients.OnClientRefresh := ClientManagerRefresh;
  FClients.OnClientAddRecent := ClientManagerAddRecent;
  FClients.OnClientAdded := ClientManagerClientAdded;
  FClients.OnClientRemoved := ClientManagerClientRemoved;
  FClients.OnClientSongSaved := ClientManagerSongSaved;
  FClients.OnClientTitleChanged := ClientManagerTitleChanged;
  FClients.OnClientICYReceived := ClientManagerICYReceived;
  FClients.OnClientTitleAllowed := ClientManagerTitleAllowed;
  FClients.OnShowErrorMessage := ClientManagerShowErrorMessage;
  FClients.OnPlaybackStarted := ClientManagerPlaybackStarted;
  FClients.OnClientSecondsReceived := ClientManagerSecondsReceived;

  FStreams := Streams;

  FHomeCommunication := HomeComm;

  Caption := 'Streams';

  FAddressBar := TClientAddressBar.Create(Self);
  FAddressBar.Parent := Self;
  FAddressBar.Align := alTop;
  FAddressBar.Visible := True;
  FAddressBar.OnStart := AddressBarStart;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Align := alTop;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Parent := Self;
  FToolbarPanel.Padding.Top := 1;

  FToolbar := Toolbar;
  FToolbar.Align := alLeft;
  FToolbar.Indent := 0;
  FToolbar.Top := 0;
  FToolbar.Parent := FToolbarPanel;

  FVolume := TVolumePanel.Create(Self);
  FVolume.Parent := FToolbarPanel;
  FVolume.Align := alRight;
  FVolume.Padding.Bottom := 1;
  FVolume.OnVolumeChange := VolumeVolumeChange;
  FVolume.OnGetVolumeBeforeMute := VolumeGetVolumeBeforeMute;

  FTimeLabel := TLabel.Create(Self);
  FTimeLabel.Anchors := [akRight, akTop];
  FTimeLabel.Alignment := taCenter;
  FTimeLabel.Parent := FToolbarPanel;

  FActionPlay := GetAction('actPlay');
  FActionPause := GetAction('actPause');
  FActionStopPlay := GetAction('actStopPlay');
  FActionTuneInStream := GetAction('actTuneInStream');
  FActionTuneInFile := GetAction('actTuneInFile');
  FActionRename := GetAction('actRename');
  FActionRemove := GetAction('actRemove');
  FActionShowSideBar := GetAction('actShowSideBar');
  FActionStopAfterSong := GetAction('actStopAfterSong');

  FActionPlay.OnExecute := ActionPlayExecute;
  FActionPause.OnExecute := ActionPauseExecute;
  FActionStopPlay.OnExecute := ActionPlayStopExecute;
  FActionTuneInStream.OnExecute := ActionTuneInStreamExecute;
  FActionTuneInFile.OnExecute := ActionTuneInFileExecute;
  FActionRename.OnExecute := ActionRenameExecute;
  FActionRemove.OnExecute := ActionRemoveExecute;
  FActionShowSideBar.OnExecute := ActionShowSideBarExecute;
  FActionStopAfterSong.OnExecute := ActionStopAfterSongExecute;

  GetAction('actNewCategory').OnExecute := ActionNewCategoryExecute;
  GetAction('actStart').OnExecute := ActionStartExecute;
  GetAction('actStop').OnExecute := ActionStopExecute;
  GetAction('actOpenWebsite').OnExecute := ActionOpenWebsiteExecute;
  GetAction('actResetData').OnExecute := ActionResetDataExecute;
  GetAction('actSavePlaylistStream').OnExecute := ActionSavePlaylistStreamExecute;
  GetAction('actSavePlaylistFile').OnExecute := ActionSavePlaylistFileExecute;
  GetAction('actCopyTitle').OnExecute := ActionCopyTitleExecute;
  GetAction('actAddToSaveList').OnExecute := ActionAddToSaveListExecute;
  GetAction('actAddToGlobalIgnoreList').OnExecute := ActionAddToGlobalIgnoreList;
  GetAction('actAddToStreamIgnoreList').OnExecute := ActionAddToStreamIgnoreList;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := alRight;
  FSplitter.Visible := True;
  FSplitter.AutoSnap := False;
  FSplitter.ResizeStyle := rsUpdate;

  FSideBar := TSidebar.Create(Self, FStreams);
  FSideBar.Parent := Self;
  FSideBar.Align := alRight;
  FSideBar.Visible := True;


  FSideBar.Visible := True;
  FSplitter.Visible := True;

  FToolbarPanel.Top := 0;
  FAddressBar.Top := 100;

  FClientView := TMClientView.Create(Self, Popup, FSideBar.FBrowserView.StreamTree);

  MsgBus.AddSubscriber(MessageReceived);
end;

procedure TClientTab.AddressBarStart(Sender: TObject);
var
  Entry: TRecentEntry;
begin
  if FAddressBar.FStations.ItemIndex = -1 then
  begin
    StartStreaming(TStartStreamingInfo.Create(0, 0, '', FAddressBar.FStations.Text, nil, nil), AppGlobals.DefaultActionBrowser, nil, amNoWhere)
  end else
  begin
    Entry := TRecentEntry(FAddressBar.FStations.ItemsEx[FAddressBar.FStations.ItemIndex].Data);
    StartStreaming(TStartStreamingInfo.Create(Entry.ID, Entry.Bitrate, Entry.Name, Entry.StartURL, nil, nil), AppGlobals.DefaultActionBrowser, nil, amNoWhere);
  end;
end;

procedure TClientTab.AdjustTextSizeDirtyHack;
begin
  FAddressBar.FStations.Left := FAddressBar.FLabel.Left + FAddressBar.FLabel.Width + 6;
  FAddressBar.FStations.Width := FAddressBar.ClientWidth - FAddressBar.FStations.Left - FAddressBar.FStart.Width - 6;
end;

procedure TClientTab.AfterCreate;
var
  i: Integer;
begin
  inherited;

  BuildTree(FStreams);

  FAddressBar.ClientHeight := Max(FAddressBar.FLabel.Height + FAddressBar.FLabel.Top * 2, FAddressBar.FStations.Height + FAddressBar.FStations.Top * 2) + 1;

  FToolbarPanel.ClientHeight := 24;

  FToolbar.Width := FToolbarPanel.ClientWidth - 250;
  FToolbar.Height := 25;

  FVolume.Setup;
  FVolume.Enabled := Bass.DeviceAvailable;
  FVolume.Width := 140;
  FVolume.Volume := Players.Volume;

  FTimeLabel.Left := FVolume.Left - GetTextSize(FTimeLabel.Caption, FTimeLabel.Font).cx;
  FTimeLabel.Top := FToolbarPanel.ClientHeight div 2 - FTimeLabel.Height div 2;

  FSplitter.Width := MulDiv(4, Screen.PixelsPerInch, 96);
  FSplitter.MinSize := MulDiv(220, Screen.PixelsPerInch, 96);
  FSplitter.Left := FSideBar.Left - FSplitter.Width - 5;

  FSideBar.Width := AppGlobals.SidebarWidth;


  FSideBar.FDebugView.DebugView.OnClear := DebugClear;
  FSideBar.FBrowserView.StreamTree.OnAction := StreamBrowserAction;
  FSideBar.FBrowserView.StreamTree.OnIsInClientList := StreamBrowserIsInClientList;
  if Screen.PixelsPerInch = 96 then
    FSideBar.FBrowserView.StreamTree.PopupMenu2.Images := modSharedData.imgImages;



  // Das ClientView wird erst hier erzeugt, weil es eine Referenz auf FSideBar.FBrowserView.StreamTree braucht!
  FClientView.Parent := Self;
  FClientView.Align := alClient;
  FClientView.Visible := True;
  FClientView.Images := modSharedData.imgClients;
  FClientView.OnChange := FClientViewChange;
  FClientView.OnDblClick := FClientViewDblClick;
  FClientView.OnKeyPress := FClientViewKeyPress;
  FClientView.OnKeyDown := FClientViewKeyDown;
  FClientView.OnStartStreaming := FClientViewStartStreaming;

  if FClientView.RootNodeCount > 0 then
  begin
    FClientView.Selected[FClientView.GetFirst] := True;
    FClientView.FocusedNode := FClientView.GetFirst;
  end;

  if AppGlobals.ClientHeadersLoaded then
    for i := 0 to FClientView.Header.Columns.Count - 1 do
      FClientView.Header.Columns[i].Width := AppGlobals.ClientHeaderWidth[i];


  FToolbarPanel.ClientHeight := 24;

  FToolbar.Width := FToolbarPanel.ClientWidth - 250;

  FVolume.Width := 140;

  FTimeLabel.Left := FVolume.Left - GetTextSize(FTimeLabel.Caption, FTimeLabel.Font).cx;
  FTimeLabel.Top := FToolbarPanel.ClientHeight div 2 - FTimeLabel.Height div 2;

  FSplitter.Width := MulDiv(4, Screen.PixelsPerInch, 96);
  FSplitter.MinSize := MulDiv(220, Screen.PixelsPerInch, 96);
  FSplitter.Left := FSideBar.Left - FSplitter.Width - 5;

  FSideBar.Width := AppGlobals.SidebarWidth;

  //AdjustTextSizeDirtyHack;

  FAddressBar.AfterCreate;
  FClientView.AfterCreate;
  FSideBar.AfterCreate;
end;

procedure TClientTab.DebugClear(Sender: TObject);
var
  Clients: TNodeDataArray;
  i: Integer;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, False));
  for i := 0 to Length(Clients) - 1 do
    if Clients[i].Client = FSideBar.FDebugView.DebugView.Client then
    begin
      Clients[i].Client.DebugLog.Clear;
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

// TODO: Speicherlöcher jagen ;-) .. mal im DebugMode laufen lassen und dann zusehen.

// TODO: Testen, wenn im Tray gestartet wurde, ohne dass das Fenster je sichtbar war:
//  + Auto-Aufnahmen
//  + Speicherplatz gering (kommt die message?)
//  + Notification von HomeComm
//  + Ordner für SavedSongs existiert nicht
//  - Update gefunden (alle Kombinationen Update-Verlauf!)
//  - Update bereit zum Installieren
//  + Alle Meldungen aus Shown/AfterShown
//  + Profilimport testen.
//  - Auf hohen DPI testen.
//  - Fernsteuerung per CommandLine im Tray als auch mit Fenster offen
//  + Fehler in Datendatei
//  + Eine Message vom Server empfangen wird die angezeigt werden muss
//  - Start nach frischer Installation

procedure TClientTab.ClientManagerAddRecent(Sender: TObject);
var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  if not Client.AutoRemove then
  begin
    FAddressBar.Stations.AddItem(Client.Entry.ID, Client.Entry.Bitrate, Client.Entry.Name, Client.Entry.StartURL);

    if Client.Entry.ID = 0 then
      FHomeCommunication.SendSubmitStream(Client.Entry.StartURL);
  end;

  ShowInfo;
end;

procedure TClientTab.ClientManagerDebug(Sender: TObject);
begin
  if FSideBar.FDebugView.DebugView.Client = Sender then
  begin
    FSideBar.FDebugView.ShowDebug(TICEClient(Sender));
  end;
end;

procedure TClientTab.ClientManagerICYReceived(Sender: TObject;
  Received: Integer);
var
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  FReceived := FReceived + Received;
  FStreams.Received := FStreams.Received + Received;
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
end;

procedure TClientTab.ClientManagerTitleAllowed(Sender: TObject; Title: string;
  var Allowed: Boolean; var Match: string; var Filter: Integer);
  function ContainsTitle(List: TList<TTitleInfo>; Title: string; var Match: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    Title := LowerCase(Title);
    for i := 0 to List.Count - 1 do
    begin
      if Like(Title, List[i].Pattern) then
      begin
        Result := True;
        Match := List[i].Title;
        Exit;
      end;
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
        Allowed := ContainsTitle(FStreams.SaveList, Title, Match);
        Filter := 0;
      end;
    ufIgnoreGlobal:
      begin
        Allowed := not ContainsTitle(FStreams.IgnoreList, Title, Match);
        Filter := 1;
      end;
    ufIgnoreLocal:
      begin
        Allowed := not ContainsTitle(Client.Entry.IgnoreList, Title, Match);
        Filter := 2;
      end;
    ufIgnoreBoth:
      begin
        Allowed := not ContainsTitle(FStreams.IgnoreList, Title, Match);
        Filter := 1;

        if Allowed then
        begin
          Allowed := not ContainsTitle(Client.Entry.IgnoreList, Title, Match);
          Filter := 2;
        end;
      end;
    ufBoth:
      begin
        Allowed := ContainsTitle(FStreams.SaveList, Title, Match);
        if Allowed then
        begin
          Allowed := not ContainsTitle(FStreams.IgnoreList, Title, Match);
          Filter := 1;

          if Allowed then
          begin
            Allowed := not ContainsTitle(Client.Entry.IgnoreList, Title, Match);
            Filter := 2;
          end;
        end else
          Filter := 0;
      end
    else
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
  OnePlayingName: string;
begin
  FClientView.RefreshClient(Sender as TICEClient);

  OnePlaying := False;
  for i := 0 to FClients.Count - 1 do
  begin
    if FClients[i].Playing and (FClients[i].State = csConnected) then
    begin
      OnePlaying := True;
      OnePlayingName := FClients[i].Entry.Name;
      Break;
    end;
  end;

  if OnePlaying then
  begin
    FPlaybackTimer.Enabled := True;
  end else
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
    FStreams.CategoryList.Remove(FreeCategory);
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

procedure TClientTab.ClientManagerShowErrorMessage(Sender: TICEClient;
  Msg: TMayConnectResults; WasAuto, WasScheduled: Boolean);
begin
  if Assigned(FOnShowErrorMessage) then
    FOnShowErrorMessage(Sender, Msg, WasAuto, WasScheduled);
end;

procedure TClientTab.ClientManagerSongSaved(Sender: TObject;
  Filename, Title, SongArtist, SongTitle: string; Filesize, Length, Bitrate: UInt64;
  VBR, WasCut, FullTitle, IsStreamFile, RecordBecauseArtist: Boolean;
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
  for i := 0 to FStreams.TrackList.Count - 1 do
    if LowerCase(FStreams.TrackList[i].Filename) = LowerFilename then
    begin
      Track := FStreams.TrackList[i];
      Track.Time := Now;
      Break;
    end;

  if Track = nil then
  begin
    Track := TTrackInfo.Create(Now, Filename, Client.Entry.Name, Title, SongArtist, SongTitle, ServerTitleHash, ServerArtistHash);
    FStreams.TrackList.Add(Track);
    Added := True;
  end;

  Track.Streamname := Client.Entry.Name;
  Track.Filesize := Filesize;
  Track.Length := Length;
  Track.WasCut := WasCut;
  Track.BitRate := Bitrate;
  Track.IsAuto := Client.AutoRemove;
  Track.RecordBecauseArtist := Client.RecordBecauseArtist;
  Track.IsStreamFile := IsStreamFile;
  Track.VBR := VBR;

  if Added then
  begin
    if Assigned(FOnTrackAdded) then
      FOnTrackAdded(Client.Entry, Track);
  end;

  if FullTitle and (not IsStreamFile) then
  begin
    if Client.Entry.Settings.AddSavedToIgnore then
      if Assigned(FOnAddTitleToList) then
        FOnAddTitleToList(Self, nil, ltIgnore, Track.ParsedTitle);

    if Client.Entry.Settings.AddSavedToStreamIgnore then
      if Assigned(FOnAddTitleToList) then
        FOnAddTitleToList(Self, Client, ltIgnore, Track.ParsedTitle);

    if Client.Entry.Settings.RemoveSavedFromWishlist then
    begin
      if Assigned(FOnRemoveTitleFromList) then
        FOnRemoveTitleFromList(Self, nil, ltSave, Track.ParsedTitle, ServerTitleHash);
    end;
  end;

  ShowInfo;
end;

procedure TClientTab.ClientManagerTitleChanged(Sender: TObject;
  Title: string);
begin
  // Ist hier, weil wenn FFilename im Client gesetzt wird, das hier aufgerufen wird.
  // Relativ unschön so, aber Hauptsache es tut..
  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Sender);

  FPlaybackSeconds := 0;
end;

procedure TClientTab.FClientViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    FActionRemove.Execute;
  end;
end;

procedure TClientTab.FClientViewStartStreaming(Sender: TObject;
  ID, Bitrate: Cardinal; Name, URL: string; RegExes, IgnoreTitles: TStringList;
  Node: PVirtualNode; Mode: TVTNodeAttachMode);
begin
  StartStreaming(TStartStreamingInfo.Create(ID, Bitrate, Name, URL, RegExes, IgnoreTitles), AppGlobals.DefaultActionBrowser, Node, Mode);
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
  begin
    if Client.Playing and (not Client.Paused) then
      Client.PausePlay;
  end;
end;

procedure TClientTab.PlaybackTimerTimer(Sender: TObject);
begin
  Inc(FPlaybackSeconds);

  FTimeLabel.Caption := BuildTime(FPlaybackSeconds, False);
  FTimeLabel.Left := FVolume.Left - GetTextSize(FTimeLabel.Caption, FTimeLabel.Font).cx - 6;
end;

procedure TClientTab.Resize;
begin
  inherited;

{
  if FToolbarPanel <> nil then
    FToolbarPanel.Width := FToolbar.Width + 10;
  if FTimeLabel <> nil then
    FTimeLabel.Left := FVolume.Left - GetTextSize(FTimeLabel.Caption, FTimeLabel.Font).cx - 8;
}
end;

procedure TClientTab.FClientViewKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) or (Key = #32) then
  begin
    FClientViewDblClick(FClientView);
    Key := #0;
  end;
end;

procedure TClientTab.FClientViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
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

procedure TClientTab.FClientViewDblClick(Sender: TObject);
var
  Clients: TNodeDataArray;
  Res: TMayConnectResults;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
  if Length(Clients) = 1 then
  begin
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
              OnShowErrorMessage(Clients[0].Client, Res, False, False);
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
      caFile:
        FActionTuneInFile.Execute;
    end;
  end;
end;

function TClientTab.StartStreaming(Streams: TStartStreamingInfoArray; Action: TStreamOpenActions; HitNode: PVirtualNode;
  Mode: TVTNodeAttachMode): Boolean;
  procedure UnkillCategory;
  var
    NodeData: PClientNodeData;
  begin
    if HitNode <> nil then
    begin
      NodeData := FClientView.GetNodeData(HitNode);
      if NodeData.Category <> nil then
        NodeData.Category.Killed := False;
    end;
  end;
  procedure PlayStarted(Client: TICEClient);
  var
    Clients: TClientArray;
    C: TICEClient;
  begin
    Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
    for C in Clients do
    begin
      if C <> Client then
        C.StopPlay;
    end;
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
  MessagesShown: TMayConnectResultsSet;
begin
  Result := True;

  // Sonderbehandlung fürs Extern abspielen...
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
  begin
    if Info.URL <> '' then
    begin
      // Falls eine Datei gemeint ist...
      if FileExists(Info.URL) then
      begin
        PH := TPlaylistHandler.Create;
        try
          PH.ParsePlaylist(Info.URL);
          for i := 0 to PH.URLs.Count - 1 do
            StartStreaming(TStartStreamingInfo.Create(Info.ID, Info.Bitrate, Info.Name, PH.URLs[i], Info.RegExes, Info.IgnoreTitles),
              oaAdd, HitNode, Mode);
        finally
          PH.Free;
        end;
        Exit;
      end;

      // Ist der Client schon in der Liste?
      Client := FClients.GetClient(Info.ID, '', Info.URL, '', nil);
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
          if not (Res in MessagesShown) then
          begin
            MessagesShown := MessagesShown + [Res];
            OnShowErrorMessage(Client, Res, False, False);
          end;
        end;
      end else
      begin
        if ValidURL(Info.URL) then
        begin
          Client := FClients.AddClient(Info.ID, Info.Bitrate, Info.Name, Info.URL);
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
            if not (Res in MessagesShown) then
            begin
              MessagesShown := MessagesShown + [Res];
              OnShowErrorMessage(Client, Res, False, False);
            end;
          end;
        end else
        begin
          Result := False;
          MsgBox(GetParentForm(Self).Handle, _('The stream could not be added to the list because the URL is invalid.'), _('Info'), MB_ICONINFORMATION);
        end;
      end;
    end;
  end;
end;

function TClientTab.StartStreaming(Stream: TStartStreamingInfo;
  Action: TStreamOpenActions; HitNode: PVirtualNode;
  Mode: TVTNodeAttachMode): Boolean;
var
  Arr: TStartStreamingInfoArray;
begin
  SetLength(Arr, 1);
  Arr[0] := Stream;
  Result := StartStreaming(Arr, Action, HitNode, Mode);
end;

procedure TClientTab.StreamBrowserAction(Sender: TObject; Action: TStreamOpenActions;
  Streams: TStreamDataArray);
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
begin
  if Action in [oaPlayExternal, oaSave] then
  begin
    SetLength(Entries, 0);
    for i := 0 to Length(Streams) - 1 do
    begin
      SetLength(Entries, Length(Entries) + 1);
      Entries[i].Name := Streams[i].Name;
      Entries[i].URL := Streams[i].URL;
    end;
  end;

  if Action in [oaStart, oaPlay, oaAdd] then
  begin
    SetLength(Arr, 0);
    for i := 0 to Length(Streams) - 1 do
    begin
      SetLength(Arr, Length(Arr) + 1);
      Arr[High(Arr)] := TStartStreamingInfo.Create(Streams[i].ID, Streams[i].Bitrate, Streams[i].Name, Streams[i].URL, Streams[i].RegExes, Streams[i].IgnoreTitles);
    end;
    StartStreaming(Arr, Action, nil, amNoWhere);
    Exit;
  end;

  case Action of
    oaPlayExternal:
      SavePlaylist(Entries, True);
    oaOpenWebsite:
      for i := 0 to Length(Streams) - 1 do
        ShellExecute(Handle, 'open', PChar(Streams[i].Website), '', '', 1);
    oaBlacklist:
      for i := 0 to Length(Streams) - 1 do
        if Streams[i].Name <> '' then
          if FStreams.StreamBlacklist.IndexOf(Streams[i].Name) = -1 then
            FStreams.StreamBlacklist.Add(Streams[i].Name);
    oaCopy:
      begin
        s := '';
        for i := 0 to Length(Streams) - 1 do
          s := s + Streams[i].URL + #13#10;
        s := Trim(s);
        Clipboard.Clear;
        Clipboard.SetTextBuf(PChar(s));
      end;
    oaSave:
      SavePlaylist(Entries, False);
    oaRefresh:
      FSideBar.FBrowserView.RefreshStreams;
    oaSetData:
      begin
        if HomeComm.Connected then
        begin
          FSideBar.BrowserView.SetStreamDataID := Streams[0].ID;
          HomeComm.SendGenerateAuthToken;
        end else
          MsgBox(Handle, _('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION);
      end;
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
  begin
    if Client.Entry.ID = ID then
      Exit(True);
  end;
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

procedure TClientTab.UpdateStreams(Streams: TDataLists);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  E: TStreamEntry;
  C: TListCategory;
  CatIdx: Integer;
  OldCategories: TListCategoryList;
begin
  CatIdx := 0;

  for i := 0 to Streams.StreamList.Count - 1 do
    Streams.StreamList[i].Free;
  Streams.StreamList.Clear;

  for i := 0 to Streams.RecentList.Count - 1 do
    Streams.RecentList[i].Free;
  Streams.RecentList.Clear;


  for i := 0 to FAddressBar.Stations.ItemsEx.Count - 1 do
  begin
    Streams.RecentList.Add(TRecentEntry(FAddressBar.Stations.ItemsEx[i].Data).Copy);
  end;

  OldCategories := TListCategoryList.Create;
  try
    for i := 0 to Streams.CategoryList.Count - 1 do
      OldCategories.Add(Streams.CategoryList[i]);

    Nodes := FClientView.GetNodes(ntAll, False);
    for i := 0 to Length(Nodes) - 1 do
    begin
      NodeData := FClientView.GetNodeData(Nodes[i]);

      if NodeData.Client <> nil then
      begin
        if NodeData.Client.AutoRemove then
          Continue;

        E := NodeData.Client.Entry.Copy;
        E.Index := Nodes[i].Index;
        E.CategoryIndex := 0;
        if Nodes[i].Parent <> FClientView.RootNode then
        begin
          E.CategoryIndex := CatIdx;
        end;
        FStreams.StreamList.Add(E);
      end else
      begin
        CatIdx := Nodes[i].Index + 1;
        C := TListCategory.Create(NodeData.Category.Name, CatIdx);
        C.Expanded := FClientView.Expanded[Nodes[i]];
        C.IsAuto := NodeData.Category.IsAuto;
        Streams.CategoryList.Add(C);

        // Weil hier nicht mit Kopien gearbeitet wird Referenz ändern
        NodeData.Category := C;
      end;
    end;

    // Alte Kategorien erst hier löschen, weil ich an der Stelle
    // nicht wie bei den StreamEntries mit Kopien arbeite.
    for i := 0 to OldCategories.Count - 1 do
    begin
      Streams.CategoryList.Remove(OldCategories[i]);
      OldCategories[i].Free;
    end;
  finally
    OldCategories.Free;
  end;
end;

procedure TClientTab.BuildTree(Streams: TDataLists);
var
  i: Integer;
  Client: TICEClient;
  Cat: TListCategory;
  Node, ParentNode: PVirtualNode;
begin
  for i := 0 to Streams.CategoryList.Count - 1 do
    FClientView.AddCategory(Streams.CategoryList[i]);

  for i := 0 to Streams.StreamList.Count - 1 do
  begin
    Client := FClients.AddClient(Streams.StreamList[i]);
    Node := FClientView.GetClientNode(Client);
    if Client <> nil then
    begin
      if Streams.StreamList[i].CategoryIndex > 0 then
      begin
        ParentNode := FClientView.GetCategoryNode(Streams.StreamList[i].CategoryIndex);
        if ParentNode <> nil then
          FClientView.MoveTo(Node, ParentNode, amAddChildLast, False);
      end;
      if Streams.StreamList[i].WasRecording and AppGlobals.RememberRecordings then
        Client.StartRecording(True);
      Client.Entry.WasRecording := False;
    end;
  end;

  for i := 0 to Streams.CategoryList.Count - 1 do
  begin
    Node := FClientView.GetCategoryNode(Streams.CategoryList[i].Index);
    if Streams.CategoryList[i].Expanded then
      FClientView.Expanded[Node] := True;
  end;

  if FClientView.AutoNode = nil then
  begin
    Cat := TListCategory.Create(_('Automatic recordings'), High(Integer));
    Cat.IsAuto := True;
    FClientView.AddCategory(Cat);
    Streams.CategoryList.Add(Cat);
  end;

  Cat := PClientNodeData(FClientView.GetNodeData(FClientView.AutoNode)).Category;
  Cat.Name := _('Automatic recordings');

  FClientView.SortItems;
end;

{ TSidebar }

procedure TSidebar.AfterCreate;
begin
  FPage1.PageControl := Self;
  FPage2.PageControl := Self;
  FPage3.PageControl := Self;

  FInfoView.Parent := FPage2;
  FDebugView.Parent := FPage3;

  FBrowserView.AfterCreate;
end;

constructor TSidebar.Create(AOwner: TComponent; DataLists: TDataLists);
begin
  inherited Create(AOwner);

  FDataLists := DataLists;

  FPage1 := TTabSheet.Create(Self);
  FPage1.Caption := 'Browser';

  FPage2 := TTabSheet.Create(Self);
  FPage2.Caption := 'Info';

  FPage3 := TTabSheet.Create(Self);
  FPage3.Caption := 'Log';

  FBrowserView := TMStreamBrowserView.Create(Self, FDataLists);
  FBrowserView.Parent := FPage1;

  FInfoView := TMStreamInfoView.Create(Self);
  FDebugView := TMStreamDebugView.Create(Self);
end;

destructor TSidebar.Destroy;
begin

  inherited;
end;

end.



