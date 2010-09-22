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
  ImgList, Functions, DragDropFile, StreamInfoView, GUIFunctions;

type
  TSavedTree = class;

  TSavedNodeData = record
    Stream: TStreamEntry;
    Track: TTrackInfo;
  end;
  PSavedNodeData = ^TSavedNodeData;

  TSavedTab = class(TMainTabSheet)
  private
    FSavedTree: TSavedTree;
    FStreams: TStreamDataList;

    FOnCut: TTrackEvent;
    FOnTrackRemoved: TTrackEvent;

    procedure BuildTree;
    procedure SavedTreeAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup(Streams: TStreamDataList; Images: TImageList);

    property Tree: TSavedTree read FSavedTree;
    property OnCut: TTrackEvent read FOnCut write FOnCut;
    property OnTrackRemoved: TTrackEvent read FOnTrackRemoved write FOnTrackRemoved;
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
    procedure DeleteTracks(Tracks: TTrackInfoArray);

    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure KeyPress(var Key: Char); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
    procedure DoDragging(P: TPoint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddTrack(Entry: TStreamEntry; Track: TTrackInfo);
    procedure RemoveTrack(Track: TTrackInfo);

    property OnAction: TTrackActionEvent read FOnAction write FOnAction;
  end;

implementation

{ TCutTab }

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
end;

procedure TSavedTab.SavedTreeAction(Sender: TObject; Action: TTrackActions;
  Tracks: TTrackInfoArray);
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
          if Assigned(FOnTrackRemoved) then
            FOnTrackRemoved(nil, Tracks[i]);
        end;
      end;
    taDelete:
      begin
        for i := 0 to Length(Tracks) - 1 do
        begin
          DeleteFile(Tracks[i].Filename);
          FStreams.RemoveTrack(Tracks[i]);
          if Assigned(FOnTrackRemoved) then
            FOnTrackRemoved(nil, Tracks[i]);
        end;
      end;
    taProperties:
      PropertiesDialog(Tracks[0].Filename);
  end;
end;

constructor TSavedTab.Create(AOwner: TComponent);
begin
  inherited;

  ShowCloseButton := False;
  ImageIndex := 14;

  FSavedTree := TSavedTree.Create(Self);
  FSavedTree.Parent := Self;
  FSavedTree.Align := alClient;
  FSavedTree.OnAction := SavedTreeAction;
end;

procedure TSavedTab.Setup(Streams: TStreamDataList; Images: TImageList);
begin
  Caption := _('Saved songs');

  FStreams := Streams;

  FSavedTree.Images := Images;

  BuildTree;
end;

{ TSavedTree }

constructor TSavedTree.Create(AOwner: TComponent);
begin
  inherited;

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
  FPopupMenu.ItemDelete.OnClick := PopupMenuClick;
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
  FColSaved.Text := _('Gespeichert');
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

procedure TSavedTree.AddTrack(Entry: TStreamEntry; Track: TTrackInfo);
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Stream := Entry;
  NodeData.Track := Track;
end;

procedure TSavedTree.DeleteTracks(Tracks: TTrackInfoArray);
var
  i, n: Integer;
  NodeData: PSavedNodeData;
  Nodes: TNodeArray;
begin
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
  for i := 0 to Length(Tracks) - 1 do
    if LowerCase(ExtractFileExt(Tracks[i].Filename)) = '.mp3' then
    begin
      FoundMP3 := True;
      Break;
    end;
  FPopupMenu.ItemCut.Enabled := FoundMP3;
end;

procedure TSavedTree.PopupMenuClick(Sender: TObject);
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
    DeleteTracks(Tracks);
  end
  else if Sender = FPopupMenu.ItemDelete then
  begin
    Action := taDelete;
    DeleteTracks(Tracks);
  end else if Sender = FPopupMenu.ItemProperties then
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
    Index := 16;
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
    1:
      Result := CmpInt(Data1.Track.Filesize, Data2.Track.Filesize);
    3: Result := CompareText(Data1.Stream.Name, Data2.Stream.Name);
    2: Result := CmpTime(Data1.Track.Time, Data2.Track.Time);
  end;
end;

procedure TSavedTree.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSavedTree.RemoveTrack(Track: TTrackInfo);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
begin
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
