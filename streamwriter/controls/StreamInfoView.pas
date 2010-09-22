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
unit StreamInfoView;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ImgList,
  RecentManager, VirtualTrees, LanguageObjects, GUIFunctions,
  Generics.Collections, Graphics, Forms, Menus, Messages, DragDrop,
  DragDropFile, Functions;

type
  TSavedHistoryNodeData = record
    TrackInfo: TTrackInfo;
  end;
  PSavedHistoryNodeData = ^TSavedHistoryNodeData;

  TTrackActions = (taPlay, taCut, taRemove, taDelete, taProperties);

  TTrackInfoArray = array of TTrackInfo;

  TTrackActionEvent = procedure(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray) of object;

  TSavedTracksPopup = class(TPopupMenu)
  private
    FItemPlay: TMenuItem;
    FItemCut: TMenuItem;
    FItemRemove: TMenuItem;
    FItemDelete: TMenuItem;
    FItemProperties: TMenuItem;
  public
    constructor Create(AOwner: TComponent);

    procedure EnableItems(Enable: Boolean);

    property ItemPlay: TMenuItem read FItemPlay;
    property ItemCut: TMenuItem read FItemCut;
    property ItemRemove: TMenuItem read FItemRemove;
    property ItemDelete: TMenuItem read FItemDelete;
    property ItemProperties: TMenuItem read FItemProperties;
  end;

  TSavedTracksTree = class(TVirtualStringTree)
  private
    FDragSource: TDropFileSource;

    FSortColumn: Integer;
    FSortDirection: TSortDirection;

    FDisplayedTracks: TList;

    FPopupMenu: TSavedTracksPopup;

    FOnAction: TTrackActionEvent;

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
    procedure DoFreeNode(Node: PVirtualNode); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    function ShowTracks(Tracks: TList<TTrackInfo>; EntriesChanged: Boolean): TTrackInfoArray;

    property OnAction: TTrackActionEvent read FOnAction write FOnAction;
  end;

  TMStreamInfoViewPanel = class(TPanel)
  private
    FEntries: TStreamList;
    FResized: Boolean;
    FTopPanel: TPanel;
    FSplitter: TSplitter;
    FName: TLabel;
    FInfo: TMemo;
    FSavedTracks: TSavedTracksTree;

    procedure ShowInfo(Entries: TStreamList; ChangedOverride: Boolean = False);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property Tree: TSavedTracksTree read FSavedTracks;
  end;

  TMStreamInfoView = class(TPanel)
  private
    FInfoView: TMStreamInfoViewPanel;
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure Translate;
    procedure ShowInfo(ChangedOverride: Boolean = False); overload;
    procedure ShowInfo(Entries: TStreamList); overload;

    property InfoView: TMStreamInfoViewPanel read FInfoView;
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
  Items.Add(FItemPlay);

  FItemCut := CreateMenuItem;
  FItemCut.Caption := _('&Cut');
  Items.Add(FItemCut);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemRemove := CreateMenuItem;
  FItemRemove.Caption := _('&Remove');
  Items.Add(FItemRemove);

  FItemDelete := CreateMenuItem;
  FItemDelete.Caption := _('&Delete');
  Items.Add(FItemDelete);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemProperties := CreateMenuItem;
  FItemProperties.Caption := _('Pr&operties');
  Items.Add(FItemProperties);
end;

{ TStreamInfoTree }

constructor TSavedTracksTree.Create(AOwner: TComponent);
var
  C1, C2: TVirtualTreeColumn;
begin
  inherited Create(AOwner);

  NodeDataSize := SizeOf(TSavedHistoryNodeData);

  FDragSource := TDropFileSource.Create(Self);
  FDisplayedTracks := TList.Create;

  C1 := Header.Columns.Add;
  C1.Text := _('Time');
  C2 := Header.Columns.Add;
  C2.Text := _('Filename');
  C1.Width := 80;
  Indent := 2;

  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FPopupMenu := TSavedTracksPopup.Create(Self);
  FPopupMenu.ItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.ItemCut.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemove.OnClick := PopupMenuClick;
  FPopupMenu.ItemDelete.OnClick := PopupMenuClick;
  FPopupMenu.ItemProperties.OnClick := PopupMenuClick;
  FPopupMenu.OnPopup := PopupMenuPopup;

  PopupMenu := FPopupMenu;

  FSortColumn := 0;
  FSortDirection := sdDescending;

  Header.AutoSizeIndex := 1;
  Header.Options := [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
end;

procedure TSavedTracksTree.DeleteTracks(Tracks: TTrackInfoArray);
var
  i, n: Integer;
  NodeData: PSavedHistoryNodeData;
  Nodes: TNodeArray;
begin
  Nodes := GetNodes(False);
  for n := 0 to Length(Tracks) - 1 do
  begin
    for i := 0 to Length(Nodes) - 1 do
    begin
      NodeData := GetNodeData(Nodes[i]);
      if Tracks[n] = NodeData.TrackInfo then
      begin
        DeleteNode(Nodes[i]);
      end;
    end;
  end;
end;

destructor TSavedTracksTree.Destroy;
begin
  FDisplayedTracks.Free;

  inherited;
end;

function TSavedTracksTree.DoCompare(Node1, Node2: PVirtualNode;
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
  Data1, Data2: PSavedHistoryNodeData;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Column of
    0: Result := CmpTime(Data1.TrackInfo.Time, Data2.TrackInfo.Time);
    1: Result := CompareText(Data1.TrackInfo.Filename, Data2.TrackInfo.Filename);
  end;
end;

procedure TSavedTracksTree.DoDragging(P: TPoint);
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

procedure TSavedTracksTree.DoFreeNode(Node: PVirtualNode);
begin
  inherited;

end;

function TSavedTracksTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;
  if Column = 0 then
    Index := 0;
end;

procedure TSavedTracksTree.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PSavedHistoryNodeData;
begin
  inherited;
  if TextType = ttNormal then
  begin
    NodeData := GetNodeData(Node);
    case Column of
      0:
        begin
          if Trunc(NodeData.TrackInfo.Time) = Trunc(Now) then
            Text := TimeToStr(NodeData.TrackInfo.Time)
          else
            Text := DateTimeToStr(NodeData.TrackInfo.Time);
        end;
      1: Text := ExtractFileName(NodeData.TrackInfo.Filename);
    end;
  end;
end;

procedure TSavedTracksTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if FSortColumn <> HitInfo.Column then
    begin
      FSortColumn := HitInfo.Column;
      if (HitInfo.Column <> 0) and (HitInfo.Column <> 1) then
        FSortDirection := sdDescending
      else
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

function TSavedTracksTree.GetNodes(SelectedOnly: Boolean): TNodeArray;
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

function TSavedTracksTree.GetSelected: TTrackInfoArray;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedHistoryNodeData;
begin
  SetLength(Result, 0);
  Nodes := GetNodes(True);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := NodeData.TrackInfo;
  end;
end;

procedure TSavedTracksTree.HandleMouseDblClick(var Message: TWMMouse;
  const HitInfo: THitInfo);
var
  Tracks: TTrackInfoArray;
begin
  inherited;
  if HitInfo.HitNode <> nil then
  begin
    Tracks := GetSelected;
    if (Length(Tracks) > 0) and Assigned(FOnAction) then
      FOnAction(Self, taPlay, Tracks);
  end;
end;

procedure TSavedTracksTree.KeyPress(var Key: Char);
var
  Tracks: TTrackInfoArray;
begin
  inherited;
  if Key = #13 then
  begin
    Tracks := GetSelected;
    if (Length(Tracks) > 0) and Assigned(FOnAction) then
      FOnAction(Self, taPlay, Tracks);
    Key := #0;
  end;
end;

procedure TSavedTracksTree.PopupMenuPopup(Sender: TObject);
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

procedure TSavedTracksTree.PopupMenuClick(Sender: TObject);
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

function TSavedTracksTree.ShowTracks(Tracks: TList<TTrackInfo>; EntriesChanged: Boolean): TTrackInfoArray;
var
  i: Integer;
  ReSort: Boolean;
  Node: PVirtualNode;
  NodeData: PSavedHistoryNodeData;
begin
  ReSort := False;
  SetLength(Result, 0);
  BeginUpdate;
  try
    if EntriesChanged or (Tracks.Count = 0) then
    begin
      Clear;
      FDisplayedTracks.Clear;
    end;

    Node := GetFirst;
    while Node <> nil do
    begin
      NodeData := GetNodeData(Node);
      FDisplayedTracks.Add(NodeData.TrackInfo);
      Node := GetNext(Node);
    end;

    for i := Tracks.Count - 1 downto 0 do
    begin
      if FDisplayedTracks.IndexOf(Tracks[i]) = -1 then
        if FileExists(Tracks[i].Filename) then
        begin
          ReSort := True;
          Node := AddChild(nil);
          NodeData := GetNodeData(Node);
          NodeData.TrackInfo := Tracks[i];
        end else
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Tracks[i];
        end;
    end;
  finally
    EndUpdate;
  end;
  if ReSort then
    Sort(nil, FSortColumn, FSortDirection);
end;

{ TStreamInfoView }

constructor TMStreamInfoViewPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEntries := TStreamList.Create;
  FResized := False;

  BevelOuter := bvNone;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 110;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Visible := True;

  FName := TLabel.Create(Self);
  FName.Parent := FTopPanel;
  FName.Align := alTop;
  FName.Font.Size := 10;
  FName.Font.Style := [fsBold];
  FName.Visible := True;

  FInfo := TMemo.Create(Self);
  FInfo.Parent := FTopPanel;
  FInfo.Align := alClient;
  FInfo.BorderStyle := bsNone;
  FInfo.Color := clWindow;
  FInfo.ScrollBars := ssVertical;
  FInfo.ReadOnly := True;
  FInfo.Visible := True;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := alTop;
  FSplitter.ResizeStyle := rsUpdate;
  FSplitter.Visible := True;
  FSplitter.Top := FTopPanel.Top + FTopPanel.Height;

  FSavedTracks := TSavedTracksTree.Create(Self);
  FSavedTracks.Parent := Self;
  FSavedTracks.Align := alClient;;
  FSavedTracks.Visible := True;

  Align := alClient;
end;

destructor TMStreamInfoViewPanel.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TMStreamInfoViewPanel.Resize;
begin
  inherited;
  ShowInfo(FEntries);
end;

procedure TMStreamInfoViewPanel.ShowInfo(Entries: TStreamList; ChangedOverride: Boolean = False);
var
  i, n: Integer;
  SongsSaved: Cardinal;
  Received: UInt64;
  EntriesChanged: Boolean;
  Title, Info, Genres, BitRates: string;
  Entry: TStreamEntry;
  TrackList: TList<TTrackInfo>;
  Del: TTrackInfoArray;
  EntriesNew: TStreamList;
begin
  if Entries = nil then
  begin
    FEntries.Clear;

  end else
  begin
    EntriesChanged := False;
    for Entry in Entries do
      if not FEntries.Contains(Entry) then
      begin
        EntriesChanged := True;
        Break;
      end;
    for Entry in FEntries do
      if not Entries.Contains(Entry) then
      begin
        EntriesChanged := True;
        Break;
      end;

    TrackList := TList<TTrackInfo>.Create;
    EntriesNew := TStreamList.Create;
    try
      Genres := '';
      BitRates := '';
      SongsSaved := 0;
      Received := 0;
      for Entry in Entries do
      begin
        EntriesNew.Add(Entry);

        Title := Title + Entry.Name;
        if Entry.Genre <> '' then
        begin
          if Genres <> '' then
            Genres := Genres + ' / ';
          Genres := Genres + Entry.Genre;
        end;
        if Entry.BitRate > 0 then
        begin
          if BitRates <> '' then
            BitRates := BitRates + ' / ';
          BitRates := BitRates + IntToStr(Entry.BitRate);
        end;
        SongsSaved := SongsSaved + Entry.SongsSaved;
        Received := Received + Entry.BytesReceived;

        for i := 0 to Entry.Tracks.Count - 1 do
          TrackList.Add(Entry.Tracks[i]);
      end;

      Title := TruncateText(Title, FName.Parent.Width, FName.Canvas.Font);
      if Title <> FName.Caption then
        FName.Caption := Title;

      Info := '';
      if Genres <> '' then
        Info := Info + Genres + #13#10;
      if BitRates <> '' then
        Info := Info + Bitrates + 'kbps' + #13#10;
      Info := Info + IntToStr(SongsSaved) + _(' songs saved') + #13#10;
      Info := Info + MakeSize(Received) + _(' received');
      if Info <> FInfo.Text then
        FInfo.Text := Info;

      if ChangedOverride then
        EntriesChanged := True;

      Del := FSavedTracks.ShowTracks(TrackList, EntriesChanged);
      for i := 0 to Length(Del) - 1 do
      begin
        for n := 0 to Entries.Count - 1 do
        begin
          Entries[n].Tracks.Remove(Del[i]);
        end;
        Del[i].Free;
      end;

      FEntries.Free;
      FEntries := EntriesNew;
    finally
      TrackList.Free;
    end;
  end;
end;

{ TMStreamInfoContainer }

constructor TMStreamInfoView.Create(AOwner: TComponent);
begin
  inherited;

  Caption := _('Please select at least one stream.');
  BevelOuter := bvNone;
  Align := alClient;

  FInfoView := TMStreamInfoViewPanel.Create(Self);
  FInfoView.Parent := Self;
  FInfoView.Visible := False;
end;

procedure TMStreamInfoView.ShowInfo(ChangedOverride: Boolean = False);
begin
  FInfoView.ShowInfo(FInfoView.FEntries, ChangedOverride);
end;

procedure TMStreamInfoView.ShowInfo(Entries: TStreamList);
begin
  FInfoView.ShowInfo(Entries);
  FInfoView.Visible := Entries <> nil;
end;

procedure TMStreamInfoView.Translate;
begin

end;

procedure TSavedTracksPopup.EnableItems(Enable: Boolean);
begin
  FItemPlay.Enabled := Enable;
  FItemCut.Enabled := Enable;
  FItemRemove.Enabled := Enable;
  FItemDelete.Enabled := Enable;
  FItemProperties.Enabled := Enable;
end;

end.

