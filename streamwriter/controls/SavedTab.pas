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
    Stream: TStreamEntry;
    Track: TTrackInfo;
  end;
  PSavedNodeData = ^TSavedNodeData;

  TTrackActions = (taPlay, taCut, taRemove, taRecycle, taDelete, taShowFile, taProperties);

  TTrackInfoArray = array of TTrackInfo;

  TTrackActionEvent = procedure(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray) of object;

  TSavedTracksPopup = class(TPopupMenu)
  private
    FItemPlay: TMenuItem;
    FItemCut: TMenuItem;
    FItemRemove: TMenuItem;
    FItemRecycle: TMenuItem;
    FItemDelete: TMenuItem;
    FItemShowFile: TMenuItem;
    FItemProperties: TMenuItem;
  public
    constructor Create(AOwner: TComponent);

    procedure EnableItems(Enable: Boolean);

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
  public
    constructor Create(AOwner: TComponent);
    procedure Setup;
  end;

  TSavedTab = class(TMainTabSheet)
  private
    FToolbar: TSavedToolBar;
    FSavedTree: TSavedTree;
    FStreams: TStreamDataList;

    FOnCut: TTrackEvent;
    FOnTrackRemoved: TTrackEvent;
    FOnRefresh: TNotifyEvent;

    procedure BuildTree;
    procedure SavedTreeAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
    procedure RefreshClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup(Streams: TStreamDataList; Images: TImageList);

    procedure AddTrack(Entry: TStreamEntry; Track: TTrackInfo);
    procedure RemoveTrack(Track: TTrackInfo); overload;
    procedure RemoveTrack(Track: string); overload;

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

    procedure PopupMenuPopup(Sender: TObject);
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

  FItemPlay := CreateMenuItem;
  FItemPlay.Caption := _('&Play');
  FItemPlay.ImageIndex := 0;
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
  Items.Add(FItemShowFile);

  FItemProperties := CreateMenuItem;
  FItemProperties.Caption := _('Pr&operties');
  FItemProperties.ImageIndex := 22;
  Items.Add(FItemProperties);
end;

procedure TSavedTracksPopup.EnableItems(Enable: Boolean);
begin
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

procedure TSavedToolBar.Setup;
begin
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
  i, n: Integer;
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  for i := 0 to FStreams.Count - 1 do
    for n := 0 to FStreams[i].Tracks.Count - 1 do
    begin
      Node := FSavedTree.AddChild(nil);
      NodeData := FSavedTree.GetNodeData(Node);
      NodeData.Stream := FStreams[i];
      NodeData.Track := FStreams[i].Tracks[n];
    end;

  FSavedTree.Sort(nil, FSavedTree.FSortColumn, FSavedTree.FSortDirection);

  FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
end;

procedure TSavedTab.SavedTreeAction(Sender: TObject; Action: TTrackActions;
  Tracks: TTrackInfoArray);
var
  Entries: TPlaylistEntryArray;
  SL: TStringList;
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
          FStreams.RemoveTrack(Tracks[i]);
          FSavedTree.DeleteTrack(Tracks[i]);
          if Assigned(FOnTrackRemoved) then
            FOnTrackRemoved(nil, Tracks[i]);
        end;
      end;
    taRecycle:
      begin
        for i := 0 to Length(Tracks) - 1 do
        begin
          if Recycle(Tracks[i].Filename) then
          begin
            FSavedTree.DeleteTrack(Tracks[i]);
            FStreams.RemoveTrack(Tracks[i]);
            if Assigned(FOnTrackRemoved) then
              FOnTrackRemoved(nil, Tracks[i]);
          end else
          begin
            // TODO: ?
          end;
        end;
      end;
    taDelete:
      begin
        if MsgBox(0, _('Do you really want to delete all selected files?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
          Exit;
        for i := 0 to Length(Tracks) - 1 do
        begin
          if DeleteFile(Tracks[i].Filename) then
          begin
            FSavedTree.DeleteTrack(Tracks[i]);
            FStreams.RemoveTrack(Tracks[i]);
            if Assigned(FOnTrackRemoved) then
              FOnTrackRemoved(nil, Tracks[i]);
          end else
          begin
            // TODO: ?
          end;
        end;
      end;
    taShowFile:
      RunProcess('explorer.exe /select,"' + Tracks[0].Filename + '"');
    taProperties:
      PropertiesDialog(Tracks[0].Filename);
  end;

  FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
end;

procedure TSavedTab.RefreshClick(Sender: TObject);
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

procedure TSavedTab.Setup(Streams: TStreamDataList; Images: TImageList);
begin
  Caption := _('Saved songs');

  FStreams := Streams;

  FSavedTree.Images := Images;
  FSavedTree.FPopupMenu.Images := Images;

  FToolBar := TSavedToolBar.Create(Self);
  FToolBar.Parent := Self;
  FToolBar.Images := Images;
  FToolBar.Setup;

  FToolBar.FRefresh.OnClick := RefreshClick;

  BuildTree;
end;

procedure TSavedTab.AddTrack(Entry: TStreamEntry; Track: TTrackInfo);
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  Node := FSavedTree.AddChild(nil);
  NodeData := FSavedTree.GetNodeData(Node);
  NodeData.Stream := Entry;
  NodeData.Track := Track;

  FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
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

  FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
end;

procedure TSavedTab.RemoveTrack(Track: string);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
begin
  Nodes := FSavedTree.GetNodes(False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := FSavedTree.GetNodeData(Nodes[i]);
    if LowerCase(NodeData.Track.Filename) = LowerCase(Track) then
    begin
      FSavedTree.DeleteNode(Nodes[i]);
      Break;
    end;
  end;

  FToolbar.FRefresh.Enabled := FSavedTree.RootNodeCount > 0;
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
  FPopupMenu.ItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.ItemCut.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemove.OnClick := PopupMenuClick;
  FPopupMenu.ItemRecycle.OnClick := PopupMenuClick;
  FPopupMenu.ItemDelete.OnClick := PopupMenuClick;
  FPopupMenu.ItemShowFile.OnClick := PopupMenuClick;
  FPopupMenu.ItemProperties.OnClick := PopupMenuClick;
  FPopupMenu.OnPopup := PopupMenuPopup;

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
  i, n: Integer;
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
  {
  Nodes := GetNodes(False);
  for n := 0 to Length(Tracks) - 1 do
  begin
    for i := 0 to Length(Nodes) - 1 do
    begin
      NodeData := GetNodeData(Nodes[i]);
      if Tracks[n] = NodeData.Track then
      begin
        DeleteNode(Nodes[i]);
      end;
    end;
  end;
  }
end;

procedure TSavedTree.PopupMenuPopup(Sender: TObject);
var
  FoundMP3: Boolean;
  i: Integer;
  Tracks: TTrackInfoArray;
begin
  Tracks := GetSelected;
  FPopupMenu.EnableItems(Length(Tracks) > 0);

  FoundMP3 := False;
  if BassLoaded then
    for i := 0 to Length(Tracks) - 1 do
      if LowerCase(ExtractFileExt(Tracks[i].Filename)) = '.mp3' then
      begin
        FoundMP3 := True;
        Break;
      end;
  FPopupMenu.ItemCut.Enabled := FoundMP3;
end;

procedure TSavedTree.PopupMenuClick(Sender: TObject);  // TODO: Das Popup hier braucht auch ne toolbar.
var
  Action: TTrackActions;
  Tracks: TTrackInfoArray;
begin
  Tracks := GetSelected;

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
      2: Text := NodeData.Stream.Name;
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
begin
  Result := inherited;

  if Column = 0 then
    Index := 20;
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
    2: Result := CompareText(Data1.Stream.Name, Data2.Stream.Name);
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
