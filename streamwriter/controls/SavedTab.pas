{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit SavedTab;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, MControls, LanguageObjects, Tabs, VirtualTrees, DataManager,
  ImgList, Functions, DragDropFile, GUIFunctions, StreamInfoView, DynBASS,
  Menus, Math, Forms, Player, SharedControls, AppData, Graphics, Themes;

type
  TSavedTree = class;

  TSavedNodeData = record
    Track: TTrackInfo;
  end;
  PSavedNodeData = ^TSavedNodeData;

  TTrackActions = (taRefresh, taCut, taRemove, taRecycle, taDelete, taShowFile, taProperties);

  TTrackInfoArray = array of TTrackInfo;

  TTrackActionEvent = procedure(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray) of object;

  TSavedTracksPopup = class(TPopupMenu)
  private
    FItemRefresh: TMenuItem;
    FItemPlay: TMenuItem;
    FItemPause: TMenuItem;
    FItemStop: TMenuItem;
    FItemCut: TMenuItem;
    FItemRemove: TMenuItem;
    FItemRecycle: TMenuItem;
    FItemDelete: TMenuItem;
    FItemShowFile: TMenuItem;
    FItemProperties: TMenuItem;
  protected
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable, Playing: Boolean);

    property ItemRefresh: TMenuItem read FItemRefresh;
    property ItemPlay: TMenuItem read FItemPlay;
    property ItemPause: TMenuItem read FItemPause;
    property ItemStop: TMenuItem read FItemStop;
    property ItemCut: TMenuItem read FItemCut;
    property ItemRemove: TMenuItem read FItemRemove;
    property ItemRecycle: TMenuItem read FItemRecycle;
    property ItemDelete: TMenuItem read FItemDelete;
    property ItemShowFile: TMenuItem read FItemShowFile;
    property ItemProperties: TMenuItem read FItemProperties;
  end;

  TSavedToolBar = class(TToolBar)
  private
    FRefresh: TToolButton;
    FSep1: TToolButton;
    FPlay: TToolButton;
    FPause: TToolButton;
    FStop: TToolButton;
    FSep2: TToolButton;
    FCut: TToolButton;
    FSep3: TToolButton;
    FRemove: TToolButton;
    FRecycle: TToolButton;
    FDelete: TToolButton;
    FSep4: TToolButton;
    FShowFile: TToolButton;
    FProperties: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable, Playing: Boolean);

    procedure Setup;
  end;

  TSearchBar = class(TPanel)
  private
    FLabel: TLabel;
    FSearch: TEdit;

    procedure FSearchChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Setup;
  end;

  TSavedTab = class(TMainTabSheet)
  private
    FPositionTimer: TTimer;

    FToolbarPanel: TPanel;
    FToolbar: TSavedToolBar;
    FVolume: TVolumePanel;
    FSeek: TSeekBar;
    FSearchBar: TSearchBar;
    FSavedTree: TSavedTree;
    FStreams: TDataLists;

    FOnCut: TTrackEvent;
    FOnTrackRemoved: TTrackEvent;
    FOnRefresh: TNotifyEvent;

    procedure BuildTree;
    procedure SavedTreeAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
    procedure ToolBarClick(Sender: TObject);
    procedure SearchTextChange(Sender: TObject);
    procedure VolumeTrackbarChange(Sender: TObject);
    procedure SeekChange(Sender: TObject);
    procedure PositionTimer(Sender: TObject);

    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Setup(Streams: TDataLists; Images: TImageList);

    property Tree: TSavedTree read FSavedTree;
    property OnCut: TTrackEvent read FOnCut write FOnCut;
    property OnTrackRemoved: TTrackEvent read FOnTrackRemoved write FOnTrackRemoved;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

  TSavedTree = class(TVirtualStringTree)
  private
    FPlayer: TPlayer;
    FDragSource: TDropFileSource;
    FTab: TSavedTab;
    FTrackList: TTrackList;

    FOnAction: TTrackActionEvent;

    FPopupMenu: TSavedTracksPopup;

    FColImages: TVirtualTreeColumn;
    FColFilename: TVirtualTreeColumn;
    FColSize: TVirtualTreeColumn;
    FColStream: TVirtualTreeColumn;
    FColSaved: TVirtualTreeColumn;
    FColBitRate: TVirtualTreeColumn;

    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    function GetSelected: TTrackInfoArray;
    function TrackMatchesPattern(Track: TTrackInfo): Boolean;

    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);

    procedure PlayerEndReached(Sender: TObject);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
    procedure DoDragging(P: TPoint); override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: UnicodeString); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure Change(Node: PVirtualNode); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo;
      ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoEdit; override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; Text: string;
      CellRect: TRect; DrawFormat: Cardinal); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Translate;

    procedure AddTrack(Track: TTrackInfo; FromFilter: Boolean);
    procedure RemoveTrack(Track: TTrackInfo); overload;
    procedure DeleteTrack(Track: TTrackInfo);
    procedure Filter(S: string);

    property Player: TPlayer read FPlayer;
    property OnAction: TTrackActionEvent read FOnAction write FOnAction;
  end;

implementation

{ TSavedTracksPopup }

constructor TSavedTracksPopup.Create(AOwner: TComponent);
var
  ItemTmp: TMenuItem;
begin
  inherited;

  FItemRefresh := CreateMenuItem;
  FItemRefresh.Caption := _('Re&fresh');
  FItemRefresh.ImageIndex := 23;
  Items.Add(FItemRefresh);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemPlay := CreateMenuItem;
  FItemPlay.Caption := _('&Play');
  FItemPlay.ImageIndex := 33;
  Items.Add(FItemPlay);

  FItemPause := CreateMenuItem;
  FItemPause.Caption := _('Pa&use');
  FItemPause.ImageIndex := 39;
  Items.Add(FItemPause);

  FItemStop := CreateMenuItem;
  FItemStop.Caption := _('St&op');
  FItemStop.ImageIndex := 1;
  Items.Add(FItemStop);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemCut := CreateMenuItem;
  FItemCut.Caption := _('&Cut');
  FItemCut.ImageIndex := 17;
  Items.Add(FItemCut);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemRemove := CreateMenuItem;
  FItemRemove.Caption := _('&Remove from list');
  FItemRemove.ImageIndex := 21;
  Items.Add(FItemRemove);

  FItemRecycle := CreateMenuItem;
  FItemRecycle.Caption := _('R&ecycle files');
  FItemRecycle.ImageIndex := 24;
  Items.Add(FItemRecycle);

  FItemDelete := CreateMenuItem;
  FItemDelete.Caption := _('&Delete files');
  FItemDelete.ImageIndex := 2;
  Items.Add(FItemDelete);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemShowFile := CreateMenuItem;
  FItemShowFile.Caption := _('&Show in explorer');
  FItemShowFile.ImageIndex := 28;
  Items.Add(FItemShowFile);

  FItemProperties := CreateMenuItem;
  FItemProperties.Caption := _('Pr&operties');
  FItemProperties.ImageIndex := 22;
  Items.Add(FItemProperties);
end;

procedure TSavedTracksPopup.EnableItems(Enable, Playing: Boolean);
begin
  FItemRefresh.Enabled := Enable;
  FItemPlay.Enabled := Enable;
  FItemPause.Enabled := Playing;
  FItemStop.Enabled := Playing;
  FItemCut.Enabled := Enable;
  FItemRemove.Enabled := Enable;
  FItemRecycle.Enabled := Enable;
  FItemDelete.Enabled := Enable;
  ItemShowFile.Enabled := Enable;
  FItemProperties.Enabled := Enable;
end;

{ TSavedToolBar }

constructor TSavedToolBar.Create(AOwner: TComponent);
begin
  inherited;

  ShowHint := True;
  Transparent := True;
end;

procedure TSavedToolBar.EnableItems(Enable, Playing: Boolean);
begin
  FRefresh.Enabled := Enable;
  FPlay.Enabled := Enable;
  FPause.Enabled := Playing;
  FStop.Enabled := Playing;
  FCut.Enabled := Enable;
  FRemove.Enabled := Enable;
  FRecycle.Enabled := Enable;
  FDelete.Enabled := Enable;
  FShowFile.Enabled := Enable;
  FProperties.Enabled := Enable;
end;

procedure TSavedToolBar.Setup;
begin
  FProperties := TToolButton.Create(Self);
  FProperties.Parent := Self;
  FProperties.Hint := _('Properties');
  FProperties.ImageIndex := 22;

  FShowFile := TToolButton.Create(Self);
  FShowFile.Parent := Self;
  FShowFile.Hint := _('Show in explorer');
  FShowFile.ImageIndex := 28;

  FSep3 := TToolButton.Create(Self);
  FSep3.Parent := Self;
  FSep3.Style := tbsSeparator;
  FSep3.Width := 8;

  FDelete := TToolButton.Create(Self);
  FDelete.Parent := Self;
  FDelete.Hint := 'Delete files';
  FDelete.ImageIndex := 2;

  FRecycle := TToolButton.Create(Self);
  FRecycle.Parent := Self;
  FRecycle.Hint := 'Recycle files';
  FRecycle.ImageIndex := 24;

  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := 'Remove from list';
  FRemove.ImageIndex := 21;

  FSep3 := TToolButton.Create(Self);
  FSep3.Parent := Self;
  FSep3.Style := tbsSeparator;
  FSep3.Width := 8;

  FCut := TToolButton.Create(Self);
  FCut.Parent := Self;
  FCut.Hint := 'Cut';
  FCut.ImageIndex := 17;

  FSep2 := TToolButton.Create(Self);
  FSep2.Parent := Self;
  FSep2.Style := tbsSeparator;
  FSep2.Width := 8;

  FStop := TToolButton.Create(Self);
  FStop.Parent := Self;
  FStop.Hint := 'Stop';
  FStop.ImageIndex := 1;

  FPause := TToolButton.Create(Self);
  FPause.Parent := Self;
  FPause.Hint := 'Pause';
  FPause.ImageIndex := 39;

  FPlay := TToolButton.Create(Self);
  FPlay.Parent := Self;
  FPlay.Hint := 'Play';
  FPlay.ImageIndex := 33;

  FSep1 := TToolButton.Create(Self);
  FSep1.Parent := Self;
  FSep1.Style := tbsSeparator;
  FSep1.Width := 8;

  FRefresh := TToolButton.Create(Self);
  FRefresh.Parent := Self;
  FRefresh.Hint := 'Refresh';
  FRefresh.ImageIndex := 23;
end;

{ TSavedTab }

constructor TSavedTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ShowCloseButton := False;
  ImageIndex := 14;

  FPositionTimer := TTimer.Create(Self);
  FPositionTimer.Interval := 210;
  FPositionTimer.Enabled := True;
  FPositionTimer.OnTimer := PositionTimer;

  FSavedTree := TSavedTree.Create(Self);
  FSavedTree.Parent := Self;
  FSavedTree.Align := alClient;
  FSavedTree.OnAction := SavedTreeAction;
end;

destructor TSavedTab.Destroy;
begin

  inherited;
end;

procedure TSavedTab.PositionTimer(Sender: TObject);
begin
  FSeek.Position := Tree.Player.PositionByte;
end;

procedure TSavedTab.BuildTree;
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  for i := 0 to FStreams.TrackList.Count - 1 do
  begin
    FSavedTree.AddTrack(FStreams.TrackList[i], False);
  end;

  FSavedTree.Sort(nil, FSavedTree.Header.SortColumn, FSavedTree.Header.SortDirection);

  FSavedTree.Change(nil);
end;

procedure TSavedTab.SavedTreeAction(Sender: TObject; Action: TTrackActions;
  Tracks: TTrackInfoArray);
var
  Entries: TPlaylistEntryArray;
  i: Integer;
  Error: Boolean;
begin
  case Action of
    taRefresh:
      begin
        if Assigned(FOnRefresh) then
          FOnRefresh(Self);
      end;
    {
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
    }
    taCut:
      begin
        if Assigned(FOnCut) then
          for i := 0 to Length(Tracks) - 1 do
            FOnCut(nil, Tracks[i]);
      end;
    taRemove:
      begin
        for i := 0 to Length(Tracks) - 1 do
        begin
          FStreams.TrackList.RemoveTrack(Tracks[i]);
          FSavedTree.DeleteTrack(Tracks[i]);
          if Assigned(FOnTrackRemoved) then
            FOnTrackRemoved(nil, Tracks[i]);
        end;
      end;
    taRecycle:
      begin
        for i := 0 to Length(Tracks) - 1 do
        begin
          if Recycle(Handle, Tracks[i].Filename) then
          begin
            FSavedTree.DeleteTrack(Tracks[i]);
            FStreams.TrackList.RemoveTrack(Tracks[i]);
            if Assigned(FOnTrackRemoved) then
              FOnTrackRemoved(nil, Tracks[i]);
          end;
        end;
      end;
    taDelete:
      begin
        if MsgBox(GetParentForm(Self).Handle, _('Do you really want to delete all selected files?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
          Exit;
        Error := False;
        for i := 0 to Length(Tracks) - 1 do
        begin
          if Windows.DeleteFile(PChar(Tracks[i].Filename)) or (GetLastError = ERROR_FILE_NOT_FOUND) then
          begin
            FSavedTree.DeleteTrack(Tracks[i]);
            FStreams.TrackList.RemoveTrack(Tracks[i]);
            if Assigned(FOnTrackRemoved) then
              FOnTrackRemoved(nil, Tracks[i]);
          end else
            Error := True;
        end;
        if Error then
          MsgBox(Handle, _('Some files could not be deleted.'#13#10'Please make sure they are not in use by another application.'), _('Info'), MB_ICONINFORMATION);
      end;
    taShowFile:
      RunProcess('explorer.exe /select,"' + Tracks[0].Filename + '"');
    taProperties:
      PropertiesDialog(Tracks[0].Filename);
  end;

  //FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
  FSavedTree.Change(nil);
end;

procedure TSavedTab.ToolBarClick(Sender: TObject);
begin
  if Sender = FToolbar.FRefresh then
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);

  if Sender = FToolbar.FPlay then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPlay);
  if Sender = FToolbar.FPause then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPause);
  if Sender = FToolbar.FStop then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemStop);
  if Sender = FToolbar.FCut then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemCut);
  if Sender = FToolbar.FRemove then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemRemove);
  if Sender = FToolbar.FRecycle then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemRecycle);
  if Sender = FToolbar.FDelete then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemDelete);
  if Sender = FToolbar.FShowFile then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemShowFile);
  if Sender = FToolbar.FProperties then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemProperties);
end;

procedure TSavedTab.UpdateButtons;
begin
  FToolbar.FPause.Enabled := FSavedTree.Player.Playing or FSavedTree.Player.Paused;
  FToolbar.FStop.Enabled := FSavedTree.Player.Playing or FSavedTree.Player.Paused;
  FSeek.GripperVisible := FSavedTree.Player.Playing or FSavedTree.Player.Paused;
  FSavedTree.Invalidate;
end;

procedure TSavedTab.VolumeTrackbarChange(Sender: TObject);
begin
  AppGlobals.SavedPlayerVolume := FVolume.Volume;
  FSavedTree.Player.Volume := FVolume.Volume;
end;

procedure TSavedTab.SearchTextChange(Sender: TObject);
begin
  FSavedTree.Filter(FSearchBar.FSearch.Text);
end;

procedure TSavedTab.SeekChange(Sender: TObject);
begin
  FSavedTree.FPlayer.SetPosition(FSeek.Position);
  UpdateButtons;
end;

procedure TSavedTab.Setup(Streams: TDataLists; Images: TImageList);
begin
  Caption := 'Saved songs';

  FStreams := Streams;

  FSavedTree.Images := Images;
  FSavedTree.StateImages := Images;
  FSavedTree.FPopupMenu.Images := Images;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Align := alTop;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Parent := Self;
  FToolbarPanel.ClientHeight := 24;

  FSearchBar := TSearchBar.Create(Self);
  FSearchBar.Parent := Self;
  FSearchBar.Align := alTop;
  FSearchBar.Setup;
  FSearchBar.FSearch.OnChange := SearchTextChange;

  FToolBar := TSavedToolBar.Create(Self);
  FToolBar.Align := alLeft;
  FToolBar.AutoSize := True;
  FToolBar.Parent := FToolbarPanel;
  FToolBar.Height := 24;
  FToolBar.Images := Images;
  FToolBar.Setup;

  FSeek := TSeekBar.Create(Self);
  FSeek.Parent := FToolbarPanel;
  FSeek.Align := alRight;
  FSeek.Left := FToolbar.Left + FToolbar.Width + 10;
  //FSeek.TickStyle := tsNone;
  FSeek.Width := 150;
  //FSeek.OnChange := SeekChange;
  FSeek.OnPositionChanged := SeekChange;

  FVolume := TVolumePanel.Create(Self);
  FVolume.Parent := FToolbarPanel;
  FVolume.Width := 140;
  FVolume.Align := alRight;
  FVolume.Setup;
  FVolume.OnVolumeChange := VolumeTrackbarChange;
  FVolume.Volume := AppGlobals.SavedPlayerVolume;
  FVolume.Padding.Left := 10;

  FVolume.Left := 99999999999;

  FToolbarPanel.Top := 0;
  FSearchBar.Top := FToolBar.Height + 20;

  FToolBar.FRefresh.OnClick := ToolBarClick;
  FToolBar.FPlay.OnClick := ToolBarClick;
  FToolBar.FPause.OnClick := ToolBarClick;
  FToolBar.FStop.OnClick := ToolBarClick;
  FToolBar.FCut.OnClick := ToolBarClick;
  FToolBar.FRemove.OnClick := ToolBarClick;
  FToolBar.FRecycle.OnClick := ToolBarClick;
  FToolBar.FDelete.OnClick := ToolBarClick;
  FToolBar.FShowFile.OnClick := ToolBarClick;
  FToolBar.FProperties.OnClick := ToolBarClick;

  BuildTree;
end;

{ TSavedTree }

constructor TSavedTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPlayer := TPlayer.Create;
  FPlayer.OnEndReached := PlayerEndReached;
  FTrackList := TTrackList.Create;

  FTab := TSavedTab(AOwner);

  NodeDataSize := SizeOf(TSavedNodeData);
  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.Options := Header.Options - [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 1;
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FDragSource := TDropFileSource.Create(Self);

  FPopupMenu := TSavedTracksPopup.Create(Self);
  FPopupMenu.ItemRefresh.OnClick := PopupMenuClick;
  FPopupMenu.ItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.ItemPause.OnClick := PopupMenuClick;
  FPopupMenu.ItemStop.OnClick := PopupMenuClick;
  FPopupMenu.ItemCut.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemove.OnClick := PopupMenuClick;
  FPopupMenu.ItemRecycle.OnClick := PopupMenuClick;
  FPopupMenu.ItemDelete.OnClick := PopupMenuClick;
  FPopupMenu.ItemShowFile.OnClick := PopupMenuClick;
  FPopupMenu.ItemProperties.OnClick := PopupMenuClick;
  FPopupMenu.OnPopup := PopupMenuPopUp;

  PopupMenu := FPopupMenu;

  FColImages := Header.Columns.Add;
  FColImages.Text := '';
  FColImages.Width := 56;
  FColImages.Options := FColImages.Options - [coResizable];
  FColFilename := Header.Columns.Add;
  FColFilename.Text := _('Filename');
  FColSize := Header.Columns.Add;
  FColSize.Text := _('Size');
  FColSize.Width := 70;
  FColBitRate := Header.Columns.Add;
  FColBitRate.Text := _('Bitrate');
  FColBitRate.Width := 70;
  FColStream := Header.Columns.Add;
  FColStream.Text := _('Stream');
  FColStream.Width := 250;
  FColSaved := Header.Columns.Add;
  FColSaved.Text := _('Time');
  FColSaved.Width := 130;

  Header.Options := Header.Options + [hoAutoResize];

  Header.SortColumn := 1;
  Header.SortDirection := sdAscending;
end;

destructor TSavedTree.Destroy;
begin
  FPlayer.Free;
  FTrackList.Free;
  FDragSource.Free;

  inherited;
end;

function TSavedTree.GetNodes(SelectedOnly: Boolean): TNodeArray;
var
  i: Integer;
  Node: PVirtualNode;
  Nodes: TNodeArray;
begin
  SetLength(Result, 0);
  if not SelectedOnly then begin
    Node := GetFirst;
    while Node <> nil do begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Node;
      Node := GetNext(Node);
    end;
  end else begin
    SetLength(Result, 0);
    Nodes := GetSortedSelection(True);
    for i := 0 to Length(Nodes) - 1 do begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Nodes[i];
    end;
  end;
end;

function TSavedTree.GetSelected: TTrackInfoArray;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
begin
  SetLength(Result, 0);
  Nodes := GetNodes(True);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := NodeData.Track;
  end;
end;

procedure TSavedTree.DeleteTrack(Track: TTrackInfo);
var
  i: Integer;
  NodeData: PSavedNodeData;
  Nodes: TNodeArray;
begin
  Nodes := GetNodes(False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if Track = NodeData.Track then
    begin
      DeleteNode(Nodes[i]);
      Break;
    end;
  end;
  FTrackList.Remove(Track);
end;

procedure TSavedTree.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  L: Integer;
  NodeData: PSavedNodeData;
begin
  if PaintInfo.Column = 0 then
  begin
    NodeData := GetNodeData(PaintInfo.Node);
    L := PaintInfo.ImageInfo[ImageInfoIndex].XPos;

    if LowerCase(FPlayer.Filename) = LowerCase(NodeData.Track.Filename) then
    begin
      if FPlayer.Playing then
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 33)
      else if FPlayer.Paused then
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 39)
    end else
      Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 20);

    if NodeData.Track.WasCut then
    begin
      Images.Draw(PaintInfo.Canvas, L + 16, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 17);
    end;
    if NodeData.Track.IsAuto then
      Images.Draw(PaintInfo.Canvas, L + 32, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 49);
  end;
end;

procedure TSavedTree.PlayerEndReached(Sender: TObject);
begin
  FTab.UpdateButtons;
end;

procedure TSavedTree.PopupMenuClick(Sender: TObject);
var
  Action: TTrackActions;
  Tracks: TTrackInfoArray;
begin
  Tracks := GetSelected;

  if Sender = FPopupMenu.ItemRefresh then
    Exit;

  if Sender = FPopupMenu.ItemPause then
  begin
    FPlayer.Pause;
    FTab.UpdateButtons;
    Exit;
  end
  else if Sender = FPopupMenu.ItemStop then
  begin
    FPlayer.Stop(True);
    FTab.UpdateButtons;
    Exit;
  end;

  if Length(Tracks) = 0 then
    Exit;

  if Sender = FPopupMenu.ItemPlay then
  begin
    FPlayer.Play(Tracks[0].Filename, FTab.FSeek.Position);

    FTab.FSeek.Max := Player.MaxByte;
    FTab.FSeek.Position := Player.PositionByte;
  end else if Sender = FPopupMenu.ItemCut then
    Action := taCut
  else if Sender = FPopupMenu.ItemRemove then
  begin
    Action := taRemove;
  end else if Sender = FPopupMenu.ItemRecycle then
  begin
    Action := taRecycle;
  end else if Sender = FPopupMenu.ItemDelete then
  begin
    Action := taDelete;
  end else if Sender = FPopupMenu.ItemShowFile then
    Action := taShowFile
  else if Sender = FPopupMenu.ItemProperties then
    Action := taProperties
  else
    raise Exception.Create('');

  if Length(Tracks) > 0 then
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Tracks);
end;

procedure TSavedTree.PopupMenuPopup(Sender: TObject);
begin
  FPopupMenu.FItemPause.Enabled := FPlayer.Playing;
  FPopupMenu.FItemStop.Enabled := FPlayer.Playing;
end;

procedure TSavedTree.AddTrack(Track: TTrackInfo; FromFilter: Boolean);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  if not FromFilter then
    FTrackList.Add(Track);

  if not TrackMatchesPattern(Track) then
    Exit;

  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Track := Track;

  Change(nil);
end;

procedure TSavedTree.RemoveTrack(Track: TTrackInfo);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
begin
  FTrackList.Remove(Track);

  Nodes := GetNodes(False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if NodeData.Track = Track then
    begin
      DeleteNode(Nodes[i]);
      Break;
    end;
  end;

  Change(nil);
end;

function TSavedTree.TrackMatchesPattern(Track: TTrackInfo): Boolean;
var
  Hash: Cardinal;
  Chars: Integer;
  P: string;
begin
  Result := True;
  P := BuildPattern(FTab.FSearchBar.FSearch.Text, Hash, Chars);
  if P = '' then
    Exit;
  if (not Like(LowerCase(Track.Filename), LowerCase(P))) and (not Like(LowerCase(Track.Streamname), LowerCase(P))) then
    Result := False;
end;

procedure TSavedTree.Translate;
begin
  FColFilename.Text := _('Filename');
  FColSize.Text := _('Size');
  FColStream.Text := _('Stream');
  FColSaved.Text := _('Time');
  FColBitRate.Text := _('Bitrate');
end;

procedure TSavedTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PSavedNodeData;
begin
  inherited;

  Text := '';

  if TextType = ttNormal then
  begin
    NodeData := GetNodeData(Node);
    case Column of
      1: Text := ExtractFileName(NodeData.Track.Filename);
      2:
        Text := MakeSize(NodeData.Track.Filesize);
      3:
        if NodeData.Track.BitRate > 0 then
          Text := IntToStr(NodeData.Track.BitRate);
      4:
        Text := NodeData.Track.Streamname;
      5:
        begin
          if Trunc(NodeData.Track.Time) = Trunc(Now) then
            Text := TimeToStr(NodeData.Track.Time)
          else
            Text := DateTimeToStr(NodeData.Track.Time);
        end;
    end;
  end;
end;

function TSavedTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
var
  NodeData: PSavedNodeData;
begin
  Result := inherited;

  NodeData := GetNodeData(Node);

  if (Column = 0) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    Index := 0;

  {
  if Column = 0 then
    if Kind = ikState then
    begin
      Index := 20
    end else
    begin
      if NodeData.Track.WasCut then
        Index := 17
      else
        Index := -1;
    end;
  }
end;

procedure TSavedTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if Header.SortColumn <> HitInfo.Column then
    begin
      Header.SortColumn := HitInfo.Column;
      if (HitInfo.Column = 0) or (HitInfo.Column = 2) or (HitInfo.Column = 3) or (HitInfo.Column = 5) then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end else
    begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end;
    Sort(nil, HitInfo.Column, Header.SortDirection);
  end;
end;

function TSavedTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  s: string;
  NodeData: PSavedNodeData;
begin
  Result := 0;
  S := Text;
  NodeData := GetNodeData(Node);
  if NodeData = nil then
    Exit;
  Result := StrLIComp(PChar(s), PChar(ExtractFileName(NodeData.Track.Filename)), Min(Length(s), Length(ExtractFileName(NodeData.Track.Filename))));
end;

procedure TSavedTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  Text: UnicodeString);
var
  NodeData: PSavedNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);
  if RenameFile(IncludeTrailingBackslash(ExtractFilePath(NodeData.Track.Filename)) + ExtractFileName(NodeData.Track.Filename),
    IncludeTrailingBackslash(ExtractFilePath(NodeData.Track.Filename)) + Text) then
  begin
    NodeData.Track.Filename := IncludeTrailingBackslash(ExtractFilePath(NodeData.Track.Filename)) + Text;
  end;
end;

procedure TSavedTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  NodeData: PSavedNodeData;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  inherited;
end;

procedure TSavedTree.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  Text: string; CellRect: TRect; DrawFormat: Cardinal);
var
  NodeData: PSavedNodeData;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  if FPlayer.Playing or FPlayer.Paused then
    if not Selected[PaintInfo.Node] then
      if (FPlayer.Playing or FPlayer.Paused) and (LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename)) then
      begin
        PaintInfo.Canvas.Font.Color := HTML2Color('#0078ff');
      end
    else
      if (FPlayer.Playing or FPlayer.Paused) and (LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename)) then
      begin
        PaintInfo.Canvas.Font.Color := PaintInfo.Canvas.Font.Color - 100;
      end;

  inherited;
end;

procedure TSavedTree.Filter(S: string);
var
  i: Integer;
begin
  BeginUpdate;
  Clear;

  for i := 0 to FTrackList.Count - 1 do
    AddTrack(FTrackList[i], True);

  EndUpdate;
end;

procedure TSavedTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  inherited;

  Allowed := True;
end;

procedure TSavedTree.Change(Node: PVirtualNode);
var
  Tracks: TTrackInfoArray;
begin
  inherited;

  Tracks := GetSelected;
  FPopupMenu.EnableItems(Length(Tracks) > 0, FPlayer.Playing);
  FTab.FToolbar.EnableItems(Length(Tracks) > 0, FPlayer.Playing or FPlayer.Paused);

  FPopupMenu.ItemCut.Enabled := Bass.BassLoaded and (Length(Tracks) > 0);
  FTab.FToolbar.FCut.Enabled := Bass.BassLoaded and (Length(Tracks) > 0);

  FPopupMenu.ItemRefresh.Enabled := RootNodeCount > 0;
  FTab.FToolbar.FRefresh.Enabled := RootNodeCount > 0;

  FPopupMenu.ItemShowFile.Enabled := Length(Tracks) = 1;
  FTab.FToolbar.FShowFile.Enabled := Length(Tracks) = 1;
  FPopupMenu.ItemProperties.Enabled := Length(Tracks) = 1;
  FTab.FToolbar.FProperties.Enabled := Length(Tracks) = 1;

  Invalidate;

  FTab.UpdateButtons;
end;

function TSavedTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
  function CmpTime(a, b: TDateTime): Integer;
  begin
    if a > b then
      Result := 1
    else if a < b then
      Result := -1
    else
      Result := 0;
  end;
var
  I1, I2: Integer;
  Data1, Data2: PSavedNodeData;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Column of
    0:
      begin
        I1 := 0;
        I2 := 0;

        if Data1.Track.WasCut then
          I1 := I1 + 2;
        if Data2.Track.WasCut then
          I2 := I2 + 2;

        if Data1.Track.IsAuto then
          I1 := I1 + 1;
        if Data2.Track.IsAuto then
          I2 := I2 + 1;

        Result := CmpInt(I1, I2);
      end;
    1: Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
    2: Result := CmpInt(Data1.Track.Filesize, Data2.Track.Filesize);
    3: Result := CmpInt(Data1.Track.BitRate, Data2.Track.BitRate);
    4: Result := CompareText(Data1.Track.Streamname, Data2.Track.Streamname);
    5: Result := CmpTime(Data1.Track.Time, Data2.Track.Time);
  end;
end;

procedure TSavedTree.HandleMouseDblClick(var Message: TWMMouse;
  const HitInfo: THitInfo);
var
  Tracks: TTrackInfoArray;
begin
  inherited;
  if HitInfo.HitNode <> nil then
  begin
    Tracks := GetSelected;
    if Length(Tracks) = 1 then
      FPopupMenu.FItemPlay.Click;
  end;
end;

procedure TSavedTree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = VK_DELETE then
  begin
    FPopupMenu.FItemDelete.Click;
  end;
end;

procedure TSavedTree.DoDragging(P: TPoint);
var
  i: Integer;
  Tracks: TTrackInfoArray;
begin
  if FDragSource.DragInProgress then
    Exit;

  FDragSource.Files.Clear;
  Tracks := GetSelected;
  for i := 0 to Length(Tracks) - 1 do
    FDragSource.Files.Add(Tracks[i].Filename);

  if FDragSource.Files.Count = 0 then
    Exit;

  DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending]);
  FDragSource.Execute(True);
end;

procedure TSavedTree.DoEdit;
begin
  EditColumn := 1;

  inherited;
end;

{ TSearchBar }

constructor TSearchBar.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSearchBar.Destroy;
begin

  inherited;
end;

procedure TSearchBar.FSearchChange(Sender: TObject);
begin

end;

procedure TSearchBar.Setup;
begin
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Left := 4;
  FLabel.Top := 6;
  FLabel.Caption := _('Search:');

  FSearch := TEdit.Create(Self);
  FSearch.Parent := Self;
  FSearch.Left := FLabel.Left + FLabel.Width + 8;

  FSearch.Top := 2;
  FSearch.Width := 200;
  Height := FSearch.Top + FSearch.Height + FSearch.Top + 3;

  BevelOuter := bvNone;
end;

end.
