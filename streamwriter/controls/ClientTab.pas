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
unit ClientTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, ClientView, StreamBrowserView, StreamDebugView, StreamInfoView,
  LanguageObjects, HomeCommunication, StationCombo, Menus, ActnList, ImgList,
  RecentManager, ICEClient, ClientManager, VirtualTrees, Clipbrd, Functions,
  GUIFunctions, AppData, DragDrop, DropTarget, DropComboTarget, ShellAPI, Tabs,
  Graphics, SharedControls, Generics.Collections, Generics.Defaults;

type
  TSidebar = class(TPageControl)
  private
    FPage1, FPage2, FPage3: TTabSheet;

    FBrowserView: TMStreamBrowserView;
    FInfoView: TMStreamInfoView;
    FDebugView: TMStreamDebugView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(HomeCommunication: THomeCommunication);

    property BrowserView: TMStreamBrowserView read FBrowserView;
    property InfoView: TMStreamInfoView read FInfoView;
    property DebugView: TMStreamDebugView read FDebugView;
  end;

  TClientAddressBar = class(TPanel)
  private
    FLabel: TLabel;
    FStations: TMStationCombo;
    FStart: TSpeedButton;
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

    procedure Setup;

    property Stations: TMStationCombo read FStations;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

  TClientTab = class(TMainTabSheet)
  private
    FToolbarPanel: TPanel;
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

    FActionRemove: TAction;
    FActionShowSideBar: TAction;
    FActionPlay: TAction;
    FActionStopPlay: TAction;
    //FActionTuneInRelay: TAction;
    FActionTuneInFile: TAction;
    FActionTuneInStream: TAction;

    FOnUpdateButtons: TNotifyEvent;
    FOnCut: TTrackEvent;
    FOnTrackAdded: TTrackEvent;
    FOnTrackRemoved: TTrackEvent;
    FOnAddIgnoreList: TStringEvent;
    FOnSetVolume: TIntegerEvent;

    procedure ShowInfo;

    procedure ActionNewCategoryExecute(Sender: TObject);
    procedure ActionStartExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionPlayStopExecute(Sender: TObject);
    procedure ActionResetDataExecute(Sender: TObject);
    procedure ActionShowSideBarExecute(Sender: TObject);
    procedure ActionSavePlaylistStreamExecute(Sender: TObject);
    procedure ActionSavePlaylistRelayExecute(Sender: TObject);
    procedure ActionSavePlaylistFileExecute(Sender: TObject);
    procedure ActionTuneInStreamExecute(Sender: TObject);
    //procedure ActionTuneInRelayExecute(Sender: TObject);
    procedure ActionTuneInFileExecute(Sender: TObject);

    procedure ClientManagerDebug(Sender: TObject);
    procedure ClientManagerRefresh(Sender: TObject);
    procedure ClientManagerAddRecent(Sender: TObject);
    procedure ClientManagerClientAdded(Sender: TObject);
    procedure ClientManagerClientRemoved(Sender: TObject);
    procedure ClientManagerSongSaved(Sender: TObject; Filename, Title: string; Filesize: UInt64; WasCut: Boolean);
    procedure ClientManagerTitleChanged(Sender: TObject; Title: string);
    procedure ClientManagerICYReceived(Sender: TObject; Received: Integer);
    procedure ClientManagerTitleAllowed(Sender: TObject; Title: string;
      var Allowed: Boolean; var Match: string; var Filter: Integer);

    procedure FClientViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FClientViewDblClick(Sender: TObject);
    procedure FClientViewKeyPress(Sender: TObject; var Key: Char);
    procedure FClientViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FClientViewStartStreaming(Sender: TObject; URL: string; Node: PVirtualNode; Mode: TVTNodeAttachMode);

    procedure StationsStreamChanged(Sender: TObject; Stream: TStreamEntry);

    procedure StreamBrowserAction(Sender: TObject; Action: TOpenActions; Streams: TStreamDataArray);

    procedure VolumeTrackbarChange(Sender: TObject);

    procedure AddressBarStart(Sender: TObject);

    procedure DebugClear(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Setup(Toolbar: TToolbar; Actions: TActionList; Popup: TPopupMenu; MenuImages,
      ClientImages: TImageList; Clients: TClientManager;
      Streams: TDataLists; HomeCommunication: THomeCommunication);
    procedure Shown;
    function StartStreaming(Name, URL: string; StartPlay: Boolean;
      HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean;
    procedure TimerTick;
    procedure UpdateStreams(Streams: TDataLists);
    procedure BuildTree(Streams: TDataLists);

    property AddressBar: TClientAddressBar read FAddressBar;
    property ClientView: TMClientView read FClientView;
    property SideBar: TSideBar read FSideBar;
    property Received: UInt64 read FReceived;

    property OnUpdateButtons: TNotifyEvent read FOnUpdateButtons write FOnUpdateButtons;
    property OnCut: TTrackEvent read FOnCut write FOnCut;
    property OnTrackAdded: TTrackEvent read FOnTrackAdded write FOnTrackAdded;
    property OnTrackRemoved: TTrackEvent read FOnTrackRemoved write FOnTrackRemoved;
    property OnAddIgnoreList: TStringEvent read FOnAddIgnoreList write FOnAddIgnoreList;
    property OnSetVolume: TIntegerEvent read FOnSetVolume write FOnSetVolume;
  end;

implementation

{ TClientAddressBar }

constructor TClientAddressBar.Create(AOwner: TComponent);
begin
  inherited;

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
    FStations.Text := string(FDropTarget.URL);
  if FDropTarget.Text <> '' then
    FStations.Text := string(FDropTarget.Text);
  if FDropTarget.Files.Count > 0 then
    FStations.Text := string(FDropTarget.Files[0]);
end;

procedure TClientAddressBar.Setup;
begin
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Left := 4;
  FLabel.Top := 6;
  FLabel.Caption := _('Playlist/Stream-URL:');

  FStart := TSpeedButton.Create(Self);
  FStart.Parent := Self;
  FStart.Width := 24;
  FStart.Height := 24;
  FStart.Top := 6;
  FStart.Left := ClientWidth - 4 - FStart.Width;
  FStart.Anchors := [akRight];
  FStart.Flat := True;
  FStart.Hint := _('Add and start recording');
  FStart.ShowHint := True;
  FStart.NumGlyphs := 2;
  FStart.OnClick := FStartClick;

  GetBitmap('ADD', 2, FStart.Glyph);

  FStations := TMStationCombo.Create(Self);
  FStations.Parent := Self;
  FStations.DropDownCount := 15;
  FStations.Left := FLabel.Left + FLabel.Width + 8;

  FStations.Top := 2;
  FStations.Width := ClientWidth - FStations.Left - FStart.Width - 8;
  FStations.Anchors := [akLeft, akTop, akRight];
  FStations.OnKeyPress := FStationsKeyPress;
  FStations.OnChange := FStationsChange;
  Height := FStations.Top + FStations.Height + FStations.Top;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfText, mfURL, mfFile];
  FDropTarget.Register(FStations);
  FDropTarget.OnDrop := DropTargetDrop;

  BevelOuter := bvNone;
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

procedure TClientTab.ActionNewCategoryExecute(Sender: TObject);
begin
  FClientView.AddCategory;
end;

procedure TClientTab.ActionStartExecute(Sender: TObject);
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

  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
  begin
    Entry := FStreams.StreamList.Get(Client);
    if Entry <> nil then
      Entry.LastTouched := Now;
    Client.StartRecording;
  end;
end;

procedure TClientTab.ActionStopExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, True));
  for Client in Clients do
  begin
    Client.StopRecording;
  end;
end;

procedure TClientTab.ActionRemoveExecute(Sender: TObject);
var
  Clients: TNodeDataArray;
  Node, ChildNode: PVirtualNode;
  Nodes, ChildNodes: TNodeArray;
  NodeData, ChildNodeData: PClientNodeData;
begin
  Nodes := FClientView.GetNodes(ntAll, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    if NodeData.Category <> nil then
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
    end else
      FClients.RemoveClient(NodeData.Client);
  end;

  // Wenn alle Clients weg sind können jetzt Kategorien gekickt werden.
  Nodes := FClientView.GetNodes(ntCategory, True);
  for Node in Nodes do
  begin
    NodeData := FClientView.GetNodeData(Node);
    FStreams.CategoryList.Remove(NodeData.Category);
    NodeData.Category.Free;
    FClientView.DeleteNode(Node);
  end;
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

  SelectedClient.Client.StartPlay;
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

procedure TClientTab.ActionResetDataExecute(Sender: TObject);
var
  Res: Integer;
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  R: TStreamEntry;
begin
  Res := MsgBox(Handle, _('This will reset the saved song and bytes received counters.'#13#10 +
                          'The tracknumber of new saved titles will be 1 if you specified the tracknumber in the filename pattern, this number will also be set in ID3 tags.'#13#10 +
                          'Do you want to continue?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
  if Res = IDYES then
  begin
    Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
    for Client in Clients do
    begin
      R := FStreams.StreamList.Get(Client.Client);
      R.SongsSaved := 0;
      R.BytesReceived := 0;
      {
      for i := 0 to R.Tracks.Count - 1 do
        R.Tracks[i].Free;
      R.Tracks.Clear;
      }

      Client.Client.SongsSaved := 0;
      Client.Client.Received := 0;

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

procedure TClientTab.ActionSavePlaylistRelayExecute(Sender: TObject);
//var
//  Entries: TPlaylistEntryArray;
begin
//  Entries := FClientView.GetEntries(etRelay);
//  SavePlaylist(Entries, False);
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

{
procedure TClientTab.ActionTuneInRelayExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Self);
  if FActionTuneInRelay.Enabled then
  begin
    Entries := FClientView.GetEntries(etRelay);
    SavePlaylist(Entries, True);
  end;
end;
}

procedure TClientTab.ActionTuneInFileExecute(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Self);
  if FActionTuneInFile.Enabled then
  begin
    Entries := FClientView.GetEntries(etFile);
    SavePlaylist(Entries, True);
  end;
end;

constructor TClientTab.Create(AOwner: TComponent);
begin
  inherited;

  ShowCloseButton := False;
  ImageIndex := 16;
end;

procedure TClientTab.AddressBarStart(Sender: TObject);
begin
  StartStreaming(FAddressBar.FStations.Text, FAddressBar.FStations.Text, False, nil, amNoWhere);
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

  inherited;
end;

procedure TClientTab.Setup(Toolbar: TToolbar; Actions: TActionList;
  Popup: TPopupMenu; MenuImages,
  ClientImages: TImageList; Clients: TClientManager; Streams: TDataLists;
  HomeCommunication: THomeCommunication);
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

  FStreams := Streams;
  FStreams.StreamList.OnStreamChanged := StationsStreamChanged; //genau sowas brauche ich auch für categories. OnCategoriesChanged. Die dann zuerst adden und dann passig die streams!!

  FHomeCommunication := HomeCommunication;

  Caption := _('Streams');

  FAddressBar := TClientAddressBar.Create(Self);
  FAddressBar.Parent := Self;
  FAddressBar.Align := alTop;
  FAddressBar.Visible := True;
  FAddressBar.Setup;
  FAddressBar.OnStart := AddressBarStart;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Align := alTop;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Parent := Self;
  FToolbarPanel.ClientHeight := 24;

  FToolbar := Toolbar;
  FToolbar.Align := alLeft;
  FToolbar.Width := FToolbarPanel.ClientWidth - 130;
  FToolbar.Height := 24;
  FToolbar.Parent := FToolbarPanel;

  FVolume := TVolumePanel.Create(Self);
  FVolume.Parent := FToolbarPanel;
  FVolume.Width := 140;
  FVolume.Align := alRight;
  FVolume.Setup;
  FVolume.TrackBar.Position := AppGlobals.PlayerVolume;
  FVolume.TrackBar.OnChange := VolumeTrackbarChange;

  //FActionTuneInRelay := GetAction('actTuneInRelay');
  //FActionTuneInRelay.OnExecute := ActionTuneInRelayExecute;

  FActionPlay := GetAction('actPlay');
  FActionStopPlay := GetAction('actStopPlay');
  FActionTuneInStream := GetAction('actTuneInStream');
  FActionTuneInFile := GetAction('actTuneInFile');
  FActionRemove := GetAction('actRemove');
  FActionShowSideBar := GetAction('actShowSideBar');

  FActionPlay.OnExecute := ActionPlayExecute;
  FActionStopPlay.OnExecute := ActionPlayStopExecute;
  FActionTuneInStream.OnExecute := ActionTuneInStreamExecute;
  FActionTuneInFile.OnExecute := ActionTuneInFileExecute;
  FActionRemove.OnExecute := ActionRemoveExecute;
  FActionShowSideBar.OnExecute := ActionShowSideBarExecute;

  GetAction('actNewCategory').OnExecute := ActionNewCategoryExecute;
  GetAction('actStart').OnExecute := ActionStartExecute;
  GetAction('actStop').OnExecute := ActionStopExecute;
  GetAction('actResetData').OnExecute := ActionResetDataExecute;
  GetAction('actSavePlaylistStream').OnExecute := ActionSavePlaylistStreamExecute;
  GetAction('actSavePlaylistRelay').OnExecute := ActionSavePlaylistRelayExecute;
  GetAction('actSavePlaylistFile').OnExecute := ActionSavePlaylistFileExecute;

  FClientView := TMClientView.Create(Self, Popup);
  FClientView.Parent := Self;
  FClientView.Align := alClient;
  FClientView.Visible := True;
  FClientView.PopupMenu := Popup;
  FClientView.Images := ClientImages;
  FClientView.OnChange := FClientViewChange;
  FClientView.OnDblClick := FClientViewDblClick;
  FClientView.OnKeyPress := FClientViewKeyPress;
  FClientView.OnKeyDown := FClientViewKeyDown;
  FClientView.OnStartStreaming := FClientViewStartStreaming;
  FClientView.Show;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := alRight;
  FSplitter.Visible := True;
  FSplitter.Width := 4;
  FSplitter.MinSize := 220;
  FSplitter.AutoSnap := False;
  FSplitter.ResizeStyle := rsUpdate;

  FSideBar := TSidebar.Create(Self);
  FSideBar.Parent := Self;
  FSideBar.Align := alRight;
  FSideBar.Visible := True;
  FSideBar.Init(HomeCommunication);

  FSideBar.FDebugView.DebugView.OnClear := DebugClear;
  FSideBar.FBrowserView.StreamTree.OnAction := StreamBrowserAction;
  //FSideBar.FInfoView.InfoView.Tree.OnAction := StreamInfoAction;

  FSplitter.Left := FSideBar.Left - 5;

  FSideBar.Visible := AppGlobals.ShowSidebar;
  FSplitter.Visible := AppGlobals.ShowSidebar;
  FSideBar.Width := AppGlobals.SidebarWidth;
end;

procedure TClientTab.ShowInfo;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Entry: TStreamEntry;
  Entries: TStreamList;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));

  Entries := TStreamList.Create;
  try
    for Client in Clients do
    begin
      Entry := FStreams.StreamList.Get(Client.Client);
      if Entry <> nil then
      begin
        Entries.Add(Entry)
      end;
    end;

    if Entries.Count > 0 then
      FSideBar.InfoView.ShowInfo(Entries)
    else
      FSideBar.InfoView.ShowInfo(nil);
  finally
    Entries.Free;
  end;
end;

procedure TClientTab.Shown;
var
  i: Integer;
begin
  FSideBar.FBrowserView.Setup;

  if FClientView.RootNodeCount > 0 then
  begin
    FClientView.Selected[FClientView.GetFirst] := True;
    FClientView.FocusedNode := FClientView.GetFirst;
  end;

  for i := 0 to FClientView.Header.Columns.Count - 1 do
    FClientView.Header.Columns[i].Width := AppGlobals.HeaderWidth[i];
end;

procedure TClientTab.ClientManagerAddRecent(Sender: TObject);
var
  Client: TICEClient;
  Entry: TStreamEntry;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.StreamList.Add(Client.StreamName, Client.StartURL, Client.URLs,
    Client.BitRate, Client.Genre, Client.SkipShort, Client.UseFilter, 0);
  if Entry.Name <> Client.StreamName then
  begin
    Entry.Name := Client.StreamName;
    // Ist nötig, weil zuerst bei geaddeter Playlist (manuell ohne Browser)
    // "http://" drin steht und Titel sich hier ändert
    //FClientView.SortItems;
  end;
  Entry.RecentIndex := 0;
  Entry.LastTouched := Now;
  if not Entry.Submitted then
  begin
    FHomeCommunication.SubmitStream(Entry.StartURL);
    Entry.Submitted := True;
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
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  FReceived := FReceived + Received;
  FStreams.Received := FStreams.Received + Received;

  Entry := FStreams.StreamList.Get(Client);
  if Entry <> nil then
  begin
    Entry.BytesReceived := Entry.BytesReceived + Received;
  end;

  FRefreshInfo := True;
end;

procedure TClientTab.ClientManagerTitleAllowed(Sender: TObject; Title: string;
  var Allowed: Boolean; var Match: string; var Filter: Integer);
var
  i: Integer;
  List: TTitleList;
begin
  Filter := 1000;
  Match := '';
  if Length(Title) < 1 then
    Exit;

  case TICEClient(Sender).UseFilter of
    ufWish:
      begin
        Allowed := False;
        Filter := 0;
        List := FStreams.SaveList;
      end;
    ufIgnore:
      begin
        Allowed := True;
        Filter := 1;
        List := FStreams.IgnoreList;
      end
    else
      begin
        Allowed := True;
        Exit;
      end;
  end;

  Title := LowerCase(Title);
  for i := 0 to List.Count - 1 do
  begin
    if Like(Title, List[i].Pattern) then
    begin
      Allowed := not Allowed;
      Match := List[i].Title;
      Exit;
    end;
  end;
end;

procedure TClientTab.ClientManagerRefresh(Sender: TObject);
begin
  FClientView.RefreshClient(Sender as TICEClient);
end;

procedure TClientTab.ClientManagerClientAdded(Sender: TObject);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.StreamList.Add(Client.StreamName, Client.StartURL, Client.URLs,
    Client.BitRate, Client.Genre, Client.SkipShort, Client.UseFilter, 0);
  Entry.LastTouched := Now;
  //Entry.IsInList := True;
  Client.Received := Entry.BytesReceived;

  FClientView.AddClient(Client);

  //FClientView.SortItems;
end;

procedure TClientTab.ClientManagerClientRemoved(Sender: TObject);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.StreamList.Get(Client);
  //if Entry <> nil then
  //  Entry.IsInList := False;

  FClientView.RemoveClient(Client);

  if FSidebar.FDebugView.DebugView.Client = Client then
    FSidebar.FDebugView.ShowDebug(nil);

  ShowInfo;
end;

procedure TClientTab.ClientManagerSongSaved(Sender: TObject;
  Filename, Title: string; Filesize: UInt64; WasCut: Boolean);
var
  Entry: TStreamEntry;
  Client: TICEClient;
  Track: TTrackInfo;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.StreamList.Get(Client);
  if Entry <> nil then
  begin
    Track := TTrackInfo.Create(Now, Filename);
    Track.Filesize := Filesize;
    Track.WasCut := WasCut;
    Entry.Tracks.Add(Track);
    Entry.SongsSaved := Entry.SongsSaved + 1;
    if Assigned(FOnTrackAdded) then
      FOnTrackAdded(Entry, Track);
  end;

  if Assigned(FOnAddIgnoreList) then
    FOnAddIgnoreList(Self, Title);

  ShowInfo;
end;

procedure TClientTab.ClientManagerTitleChanged(Sender: TObject;
  Title: string);
begin

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
  URL: string; Node: PVirtualNode; Mode: TVTNodeAttachMode);
begin
  StartStreaming('', URL, False, Node, Mode);
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
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(ntClient, True));
  if Length(Clients) = 1 then
  begin
    case AppGlobals.DefaultAction of
      caStartStop:
        if Clients[0].Client.Recording then
          Clients[0].Client.StopRecording
        else
        begin
          if not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
            MsgBox(Handle, _('Available disk space is below the set limit, so recording will not start.'), _('Info'), MB_ICONINFORMATION)
          else
            Clients[0].Client.StartRecording;
        end;
      caStreamIntegrated:
        if Clients[0].Client.Playing then
          FActionStopPlay.Execute
        else
          FActionPlay.Execute;
      caStream:
        FActionTuneInStream.Execute;
      //caRelay: ;
        //FActionTuneInRelay.Execute;
      caFile:
        FActionTuneInFile.Execute;
    end;
  end;
end;

function TClientTab.StartStreaming(Name, URL: string; StartPlay: Boolean;
  HitNode: PVirtualNode; Mode: TVTNodeAttachMode): Boolean;
var
  Clients: TClientArray;
  Client: TICEClient;
  Entry: TStreamEntry;
  Node: PVirtualNode;
begin
  Result := True;

  // Wenn versucht wird, einen Relay zu einem Stream, der schon in der Liste ist,
  // der Liste hinzuzufügen, quasi eine Aufnahme von einer Aufnahme startet
  // (kann durch D&D passieren), dann raus hier.
  //for Client in FClients do
  //  if URL = Client.RelayURL then
  //    Exit;

  if StartPlay then
  begin
    Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
    for Client in Clients do
    begin
      Client.StopPlay;
    end;
  end;

  if (not StartPlay) and (not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace)) then
  begin
    Result := False;
    MsgBox(Handle, _('Available disk space is below the set limit, so recording will not start.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  URL := Trim(URL);
  if URL <> '' then
  begin
    Entry := FStreams.StreamList.Get(Name, URL, nil);
    if Entry <> nil then
      Entry.LastTouched := Now;

    // Ist der Client schon in der Liste?
    Client := FClients.GetClient(Name, URL, nil);
    if Client <> nil then
    begin
      if StartPlay then
        Client.StartPlay
      else
        Client.StartRecording;
      Exit;
    end else
    begin
      // Ist der Client schon bekannt?
      if Entry <> nil then
      begin
        Client := FClients.AddClient(Entry.Name, Entry.StartURL, Entry.URLs, Entry.SkipShort, Entry.UseFilter, Entry.SongsSaved);

        if HitNode <> nil then
        begin
          Node := FClientView.GetClientNode(Client);
          FClientView.MoveTo(Node, HitNode, Mode, False);
        end;

        if StartPlay then
          Client.StartPlay
        else
          Client.StartRecording;
      end else
      begin
        if ValidURL(URL) then
        begin
          Client := FClients.AddClient(Name, URL);

          if HitNode <> nil then
          begin
            Node := FClientView.GetClientNode(Client);
            FClientView.MoveTo(Node, HitNode, Mode, False);
          end;

          if StartPlay then
            Client.StartPlay
          else
            Client.StartRecording;
        end else
        begin
          Result := False;
          MsgBox(Handle, _('The stream could not be added to the list because the URL is invalid.'), _('Info'), MB_ICONINFORMATION);
        end;
      end;
    end;
  end;
end;

procedure TClientTab.StationsStreamChanged(Sender: TObject;
  Stream: TStreamEntry);
var
  Item: TComboExItem;
  Client: TICEClient;
begin
  {
  if Stream.IsInList then
  begin
    Client := FClients.GetClient(Stream.Name, Stream.StartURL, Stream.URLs);
    if Client = nil then
    begin
      // Client nach laden von Daten hinzufügen.
      //FClients.AddClient(Stream.Name, Stream.StartURL, Stream.URLs, Stream.SkipShort, Stream.UseFilter, Stream.SongsSaved);
      Exit;
    end;
  end;
  }

  Item := FAddressBar.FStations.Get(Stream.Name, Stream.StartURL, Stream.URLs);
  if (Item = nil) and (Stream.RecentIndex > -1) then
  begin
    Item := FAddressBar.FStations.ItemsEx.Add;
    Item.ImageIndex := 0;
    Item.Caption := Stream.Name;
    Item.Data := Stream;
    if FAddressBar.FStations.ItemIndex > -1 then
      FAddressBar.FStations.ItemIndex := FAddressBar.FStations.ItemIndex + 1;
  end else
  begin
    if (Item <> nil) and (Stream.RecentIndex = -1) then
      FAddressBar.FStations.ItemsEx.Delete(Item.Index);
  end;
  FAddressBar.FStations.Sort;
end;

procedure TClientTab.StreamBrowserAction(Sender: TObject; Action: TOpenActions;
  Streams: TStreamDataArray);
var
  i: Integer;
  s: string;
  Entries: TPlaylistEntryArray;
begin
  if Action in [oaOpen, oaSave] then
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
        if not StartStreaming(Streams[i].Name, Streams[i].URL, False, nil, amNoWhere) then
          Break;
    oaPlay:
      for i := 0 to Length(Streams) - 1 do
        StartStreaming(Streams[i].Name, Streams[i].URL, True, nil, amNoWhere);
    oaOpen:
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

procedure TClientTab.VolumeTrackbarChange(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  AppGlobals.PlayerVolume := FVolume.Trackbar.Position;
  Clients := FClientView.NodesToClients(FClientView.GetNodes(ntClient, False));
  for Client in Clients do
  begin
    Client.SetVolume(FVolume.Trackbar.Position);
  end;
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
  CatNodes: TNodeArray;
  Nodes: TNodeArray;
  Clients: TClientArray;
  Client: TICEClient;
  Entry: TStreamEntry;
  Node: PVirtualNode;
  NodeData: PClientNodeData;
  ItemIdx, ChildStreamIdx: Integer;
begin
  ItemIdx := 0;
  ChildStreamIdx := 0;

  for i := 0 to Streams.StreamList.Count - 1 do
  begin
    Streams.StreamList[i].IsInList := False;
    Streams.StreamList[i].Index := 0;
    Streams.StreamList[i].CategoryIndex := 0;
  end;

  // Evtl. gelöschte Kategorien freigeben
  for i := Streams.CategoryList.Count - 1 downto 0 do
  begin
    Node := FClientView.GetCategoryNode(Streams.CategoryList[i].Index);
    if Node = nil then
    begin
      Streams.CategoryList[i].Free;
      Streams.CategoryList.Delete(i);
    end;
  end;

  Nodes := FClientView.GetNodes(ntAll, False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := FClientView.GetNodeData(Nodes[i]);

    if NodeData.Client = nil then
    begin
      Inc(ItemIdx);
      NodeData.Category.Index := ItemIdx;
      NodeData.Category.Expanded := FClientView.Expanded[Nodes[i]];
      // Wenn es eine neue Kategorie ist, hinzufügen
      if not Streams.CategoryList.Contains(NodeData.Category) then
        Streams.CategoryList.Add(NodeData.Category);
      ChildStreamIdx := 0;
    end else
    begin
      Entry := Streams.StreamList.Get(NodeData.Client);
      if Entry <> nil then
      begin
        if FClientView.GetNodeLevel(Nodes[i]) = 0 then
        begin
          Inc(ItemIdx);
          Entry.Index := ItemIdx;
        end else
        begin
          Inc(ChildStreamIdx);
          Entry.Index := ChildStreamIdx;

          // Eltern Node holen und Category setzen
          Node := Nodes[i].Parent;
          NodeData := FClientView.GetNodeData(Node);
          Entry.CategoryIndex := NodeData.Category.Index;
        end;
        Entry.IsInList := True;
      end;
    end;
  end;

  {
  Streams.CategoryList.Sort(TComparer<TListCategory>.Construct(
    function (const L, R: TListCategory): Integer
    begin
      Result := CmpInt(L.Index, R.Index);
    end
  ));
  Streams.StreamList.Sort(TComparer<TStreamEntry>.Construct(
    function (const L, R: TStreamEntry): Integer
    begin
      Result := CmpInt(L.Index, R.Index);
    end
  ));
  }
end;

procedure TClientTab.BuildTree(Streams: TDataLists);
var
  i: Integer;
  Client: TICEClient;
  Node, ParentNode: PVirtualNode;
begin
  for i := 0 to Streams.CategoryList.Count - 1 do
  begin
    Node := FClientView.AddCategory(Streams.CategoryList[i]);
  end;

  for i := 0 to Streams.StreamList.Count - 1 do
  begin
    if not Streams.StreamList[i].IsInList then
      Continue;

    //Client := FClients.AddClient(Streams.StreamList[i].Name, Streams.StreamList[i].StartURL, Streams.StreamList[i].URLs,
    //  Streams.StreamList[i].SkipShort, Streams.StreamList[i].UseFilter, Streams.StreamList[i].SongsSaved);
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
    end;
  end;

  for i := 0 to Streams.CategoryList.Count - 1 do
  begin
    Node := FClientView.GetCategoryNode(Streams.CategoryList[i].Index);
    if Streams.CategoryList[i].Expanded then
      FClientView.Expanded[Node] := True;
  end;

  FClientView.SortItems;
end;

{ TSidebar }

constructor TSidebar.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSidebar.Destroy;
begin

  inherited;
end;

procedure TSidebar.Init(HomeCommunication: THomeCommunication);
begin
  FPage1 := TTabSheet.Create(Self);
  FPage1.PageControl := Self;
  FPage1.Caption := _('Browser');

  FPage2 := TTabSheet.Create(Self);
  FPage2.PageControl := Self;
  FPage2.Caption := _('Info');

  FPage3 := TTabSheet.Create(Self);
  FPage3.PageControl := Self;
  FPage3.Caption := _('Log');

  FBrowserView := TMStreamBrowserView.Create(Self, HomeCommunication);
  FInfoView := TMStreamInfoView.Create(Self);
  FDebugView := TMStreamDebugView.Create(Self);

  FBrowserView.Parent := FPage1;
  FInfoView.Parent := FPage2;
  FDebugView.Parent := FPage3;
end;

end.

