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
unit SavedTab;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, MControls, LanguageObjects, Tabs, VirtualTrees, RecentManager,
  ImgList, Functions, DragDropFile, GUIFunctions, StreamInfoView, DynBASS,
  Menus, Math;

type
  TSavedTree = class;

  TSavedNodeData = record
    //Stream: TStreamEntry;
    Track: TTrackInfo;
  end;
  PSavedNodeData = ^TSavedNodeData;

  TTrackActions = (taRefresh, taPlay, taCut, taRemove, taRecycle, taDelete, taShowFile, taProperties);

  TTrackInfoArray = array of TTrackInfo;

  TTrackActionEvent = procedure(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray) of object;

  TSavedTracksPopup = class(TPopupMenu)
  private
    FItemRefresh: TMenuItem;
    FItemPlay: TMenuItem;
    FItemCut: TMenuItem;
    FItemRemove: TMenuItem;
    FItemRecycle: TMenuItem;
    FItemDelete: TMenuItem;
    FItemShowFile: TMenuItem;
    FItemProperties: TMenuItem;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable: Boolean);

    property ItemRefresh: TMenuItem read FItemRefresh;
    property ItemPlay: TMenuItem read FItemPlay;
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
    FCut: TToolButton;
    FSep2: TToolButton;
    FRemove: TToolButton;
    FRecycle: TToolButton;
    FDelete: TToolButton;
    FSep3: TToolButton;
    FShowFile: TToolButton;
    FProperties: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable: Boolean);

    procedure Setup;
  end;

  TSavedTab = class(TMainTabSheet)
  private
    FToolbar: TSavedToolBar;
    FSavedTree: TSavedTree;
    FStreams: TDataLists;

    FOnCut: TTrackEvent;
    FOnTrackRemoved: TTrackEvent;
    FOnRefresh: TNotifyEvent;

    procedure BuildTree;
    procedure SavedTreeAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
    procedure ToolBarClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup(Streams: TDataLists; Images: TImageList);

    procedure AddTrack(Entry: TStreamEntry; Track: TTrackInfo);
    procedure RemoveTrack(Track: TTrackInfo); overload;

    property Tree: TSavedTree read FSavedTree;
    property OnCut: TTrackEvent read FOnCut write FOnCut;
    property OnTrackRemoved: TTrackEvent read FOnTrackRemoved write FOnTrackRemoved;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

  TSavedTree = class(TVirtualStringTree)
  private
    FDragSource: TDropFileSource;
    FTab: TSavedTab;

    FOnAction: TTrackActionEvent;

    FSortColumn: Integer;
    FSortDirection: TSortDirection;

    FPopupMenu: TSavedTracksPopup;

    FColFilename: TVirtualTreeColumn;
    FColSize: TVirtualTreeColumn;
    FColStream: TVirtualTreeColumn;
    FColSaved: TVirtualTreeColumn;

    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    function GetSelected: TTrackInfoArray;

    procedure PopupMenuClick(Sender: TObject);
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DeleteTrack(Track: TTrackInfo);

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

procedure TSavedTracksPopup.EnableItems(Enable: Boolean);
begin
  FItemRefresh.Enabled := Enable;
  FItemPlay.Enabled := Enable;
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

procedure TSavedToolBar.EnableItems(Enable: Boolean);
begin
  FRefresh.Enabled := Enable;
  FPlay.Enabled := Enable;
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
  FDelete.Hint := _('Delete files');
  FDelete.ImageIndex := 2;

  FRecycle := TToolButton.Create(Self);
  FRecycle.Parent := Self;
  FRecycle.Hint := _('Recycle files');
  FRecycle.ImageIndex := 24;

  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := _('Remove from list');
  FRemove.ImageIndex := 21;

  FSep2 := TToolButton.Create(Self);
  FSep2.Parent := Self;
  FSep2.Style := tbsSeparator;
  FSep2.Width := 8;

  FCut := TToolButton.Create(Self);
  FCut.Parent := Self;
  FCut.Hint := _('Cut');
  FCut.ImageIndex := 17;

  FPlay := TToolButton.Create(Self);
  FPlay.Parent := Self;
  FPlay.Hint := _('Play');
  FPlay.ImageIndex := 33;

  FSep1 := TToolButton.Create(Self);
  FSep1.Parent := Self;
  FSep1.Style := tbsSeparator;
  FSep1.Width := 8;

  FRefresh := TToolButton.Create(Self);
  FRefresh.Parent := Self;
  FRefresh.Hint := _('Refresh');
  FRefresh.ImageIndex := 23;
end;

{ TSavedTab }

constructor TSavedTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ShowCloseButton := False;
  ImageIndex := 14;

  FSavedTree := TSavedTree.Create(Self);
  FSavedTree.Parent := Self;
  FSavedTree.Align := alClient;
  FSavedTree.OnAction := SavedTreeAction;
end;

procedure TSavedTab.BuildTree;
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  for i := 0 to FStreams.TrackList.Count - 1 do
  begin
    Node := FSavedTree.AddChild(nil);
    NodeData := FSavedTree.GetNodeData(Node);
    //NodeData.Stream := FStreams.StreamList[i];
    NodeData.Track := FStreams.TrackList[i];
  end;

  FSavedTree.Sort(nil, FSavedTree.FSortColumn, FSavedTree.FSortDirection);

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
        if MsgBox(0, _('Do you really want to delete all selected files?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
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

procedure TSavedTab.Setup(Streams: TDataLists; Images: TImageList);
begin
  Caption := _('Saved songs');

  FStreams := Streams;

  FSavedTree.Images := Images;
  FSavedTree.StateImages := Images;
  FSavedTree.FPopupMenu.Images := Images;

  FToolBar := TSavedToolBar.Create(Self);
  FToolBar.Parent := Self;
  FToolBar.Height := 24;
  FToolBar.Images := Images;
  FToolBar.Setup;

  FToolBar.FRefresh.OnClick := ToolBarClick;
  FToolBar.FPlay.OnClick := ToolBarClick;
  FToolBar.FCut.OnClick := ToolBarClick;
  FToolBar.FRemove.OnClick := ToolBarClick;
  FToolBar.FRecycle.OnClick := ToolBarClick;
  FToolBar.FDelete.OnClick := ToolBarClick;
  FToolBar.FShowFile.OnClick := ToolBarClick;
  FToolBar.FProperties.OnClick := ToolBarClick;

  BuildTree;
end;

procedure TSavedTab.AddTrack(Entry: TStreamEntry; Track: TTrackInfo);
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  Node := FSavedTree.AddChild(nil);
  NodeData := FSavedTree.GetNodeData(Node);
  //NodeData.Stream := Entry;
  NodeData.Track := Track;

  FSavedTree.Change(nil);
  //FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
end;

procedure TSavedTab.RemoveTrack(Track: TTrackInfo);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
begin
  Nodes := FSavedTree.GetNodes(False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := FSavedTree.GetNodeData(Nodes[i]);
    if NodeData.Track = Track then
    begin
      FSavedTree.DeleteNode(Nodes[i]);
      Break;
    end;
  end;

  FSavedTree.Change(nil);
  //FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
end;

{ TSavedTree }

constructor TSavedTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTab := TSavedTab(AOwner);

  NodeDataSize := SizeOf(TSavedNodeData);
  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.Options := Header.Options + [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 0;
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FDragSource := TDropFileSource.Create(Self);

  FPopupMenu := TSavedTracksPopup.Create(Self);
  FPopupMenu.ItemRefresh.OnClick := PopupMenuClick;
  FPopupMenu.ItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.ItemCut.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemove.OnClick := PopupMenuClick;
  FPopupMenu.ItemRecycle.OnClick := PopupMenuClick;
  FPopupMenu.ItemDelete.OnClick := PopupMenuClick;
  FPopupMenu.ItemShowFile.OnClick := PopupMenuClick;
  FPopupMenu.ItemProperties.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;

  FSortColumn := 0;
  FSortDirection := sdAscending;

  FColFilename := Header.Columns.Add;
  FColFilename.Text := _('Filename');
  FColSize := Header.Columns.Add;
  FColSize.Text := _('Size');
  FColSize.Width := 70;
  FColStream := Header.Columns.Add;
  FColStream.Text := _('Stream');
  FColStream.Width := 300;
  FColSaved := Header.Columns.Add;
  FColSaved.Text := _('Time');
  FColSaved.Width := 130;
end;

destructor TSavedTree.Destroy;
begin
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
      Exit;
    end;
  end;
end;

procedure TSavedTree.PopupMenuClick(Sender: TObject);
var
  Action: TTrackActions;
  Tracks: TTrackInfoArray;
begin
  Tracks := GetSelected;

  if Sender = FPopupMenu.ItemRefresh then
  begin
    if Assigned(FOnAction) then
      FOnAction(Self, taRefresh, Tracks);
    Exit;
  end;

  if Length(Tracks) = 0 then
    Exit;

  if Sender = FPopupMenu.ItemPlay then
    Action := taPlay
  else if Sender = FPopupMenu.ItemCut then
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

procedure TSavedTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PSavedNodeData;
begin
  inherited;

  if TextType = ttNormal then
  begin
    NodeData := GetNodeData(Node);
    case Column of
      0: Text := ExtractFileName(NodeData.Track.Filename);
      1:
        Text := MakeSize(NodeData.Track.Filesize);
      2:
        Text := NodeData.Track.Streamname;
      3:
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
end;

procedure TSavedTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if FSortColumn <> HitInfo.Column then
    begin
      FSortColumn := HitInfo.Column;
      FSortDirection := sdAscending;
    end else
    begin
      if FSortDirection = sdAscending then
        FSortDirection := sdDescending
      else
        FSortDirection := sdAscending;
    end;
    Sort(nil, HitInfo.Column, FSortDirection);
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

procedure TSavedTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  inherited;

  Allowed := False;
  if Column = 0 then
    Allowed := True;
end;

procedure TSavedTree.Change(Node: PVirtualNode);
var
  FoundMP3: Boolean;
  i: Integer;
  Tracks: TTrackInfoArray;
begin
  inherited;

  Tracks := GetSelected;
  FPopupMenu.EnableItems(Length(Tracks) > 0);
  FTab.FToolbar.EnableItems(Length(Tracks) > 0);

  FoundMP3 := False;
  if BassLoaded then
    for i := 0 to Length(Tracks) - 1 do
      if LowerCase(ExtractFileExt(Tracks[i].Filename)) = '.mp3' then
      begin
        FoundMP3 := True;
        Break;
      end;
  FPopupMenu.ItemCut.Enabled := FoundMP3;
  FTab.FToolbar.FCut.Enabled := FoundMP3;

  FPopupMenu.ItemRefresh.Enabled := RootNodeCount > 0;
  FTab.FToolbar.FRefresh.Enabled := RootNodeCount > 0;

  FPopupMenu.ItemShowFile.Enabled := Length(Tracks) = 1;
  FTab.FToolbar.FShowFile.Enabled := Length(Tracks) = 1;
  FPopupMenu.ItemProperties.Enabled := Length(Tracks) = 1;
  FTab.FToolbar.FProperties.Enabled := Length(Tracks) = 1;
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
  Data1, Data2: PSavedNodeData;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Column of
    0: Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
    1: Result := CmpInt(Data1.Track.Filesize, Data2.Track.Filesize);
    2: Result := CompareText(Data1.Track.Streamname, Data2.Track.Streamname);
    3: Result := CmpTime(Data1.Track.Time, Data2.Track.Time);
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
    //if (Length(Tracks) > 0) and Assigned(FOnAction) then
    //  FOnAction(Self, taPlay, Tracks);
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

end.
