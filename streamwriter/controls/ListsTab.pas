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
unit ListsTab;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, MControls, LanguageObjects, Tabs, VirtualTrees, RecentManager,
  ImgList, Functions, GUIFunctions, Menus, Math;

type
  TTitleTree = class;

  TTitleNodeData = record
    Title: TTitleInfo;
  end;
  PTitleNodeData = ^TTitleNodeData;

  TTitleToolbar = class(TToolbar)
  private
    FAdd: TToolButton;
    FRemove: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable: Boolean);

    procedure Setup;
  end;

  TTitlePanel = class(TPanel)
  private
    FLabel: TLabel;
    FToolbarPanel: TPanel;
    FAddEdit: TEdit;
    FToolbar: TTitleToolbar;
    FTree: TTitleTree;
    FList: TTitleList;

    procedure BuildTree;

    procedure AddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure AddEditKeyPress(Sender: TObject; var Key: Char);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    procedure Setup(List: TTitleList; Images: TImageList; Title: string);
  end;

  TListsTab = class(TMainTabSheet)
  private
    FWishPanel, FIgnorePanel: TTitlePanel;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup(Streams: TDataLists; Images: TImageList);
    procedure AddIgnore(Title: TTitleInfo);
  end;

  TTitleTree = class(TVirtualStringTree)
  private
    FTab: TListsTab;

    FType: Integer;
    FSortColumn: Integer;
    FSortDirection: TSortDirection;

    FColTitle: TVirtualTreeColumn;

    //function GetNodes(SelectedOnly: Boolean): TNodeArray;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TListsTab }

constructor TListsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ShowCloseButton := False;
  ImageIndex := 30;

  FWishPanel := TTitlePanel.Create(Self);
  FWishPanel.Parent := Self;

  FIgnorePanel := TTitlePanel.Create(Self);
  FIgnorePanel.Parent := Self;
end;

procedure TListsTab.Resize;
begin
  inherited;

  FWishPanel.Top := 0;
  FWishPanel.Left := 0;
  FWishPanel.Height := ClientHeight;
  FWishPanel.Width := ClientWidth div 2 - 4;

  FIgnorePanel.Top := 0;
  FIgnorePanel.Left := FWishPanel.Left + FWishPanel.Width + 4;
  FIgnorePanel.Height := ClientHeight;
  FIgnorePanel.Width := ClientWidth div 2;
end;

procedure TListsTab.Setup(Streams: TDataLists; Images: TImageList);
begin
  Caption := _('Filters');

  FWishPanel.Setup(Streams.SaveList, Images, _('Wish list'));
  FIgnorePanel.Setup(Streams.IgnoreList, Images, _('Ignore list'));

  FWishPanel.FTree.FType := 0;
  FIgnorePanel.FTree.FType := 1;
end;

procedure TListsTab.AddIgnore(Title: TTitleInfo);
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Node := FIgnorePanel.FTree.AddChild(nil);
  NodeData := FIgnorePanel.FTree.GetNodeData(Node);
  NodeData.Title := Title;
end;

{ TTitleTree }

constructor TTitleTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTab := TListsTab(AOwner);

  NodeDataSize := SizeOf(TTitleNodeData);
  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.Options := Header.Options + [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 0;
  DragMode := dmManual;
  ShowHint := True;
  HintMode := hmTooltip;

  FSortColumn := 0;
  FSortDirection := sdAscending;

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Title');
end;

destructor TTitleTree.Destroy;
begin

  inherited;
end;

{
function TTitleTree.GetNodes(SelectedOnly: Boolean): TNodeArray;
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
}

procedure TTitleTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PTitleNodeData;
begin
  inherited;

  if TextType = ttNormal then
  begin
    NodeData := GetNodeData(Node);
    case Column of
      0: Text := NodeData.Title.StreamTitle;
    end;
  end;
end;

function TTitleTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  if Column = 0 then
    if FType = 0 then
      Index := 31
    else
      Index := 32;
end;

procedure TTitleTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
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

function TTitleTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  s: string;
  NodeData: PTitleNodeData;
begin
  Result := 0;
  S := Text;
  NodeData := GetNodeData(Node);
  if NodeData = nil then
    Exit;
  Result := StrLIComp(PChar(s), PChar(NodeData.Title.StreamTitle), Min(Length(s), Length(NodeData.Title.StreamTitle)));
end;

function TTitleTree.DoCompare(Node1, Node2: PVirtualNode;
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
  Data1, Data2: PTitleNodeData;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Column of
    0: Result := CompareText(Data1.Title.StreamTitle, Data2.Title.StreamTitle);
  end;
end;

procedure TTitleTree.DoFreeNode(Node: PVirtualNode);
begin

  inherited;
end;

{ TTitlePanel }

procedure TTitlePanel.AddClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Title: TTitleInfo;
begin
  if Trim(FAddEdit.Text) <> '' then
  begin
    Node := FTree.AddChild(nil);
    NodeData := FTree.GetNodeData(Node);

    Title := TTitleInfo.Create(Trim(FAddEdit.Text));
    NodeData.Title := Title;
    FList.Add(Title);
    FAddEdit.Text := '';
  end else
    MsgBox(Handle, _('Please enter a string to add to list.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TTitlePanel.RemoveClick(Sender: TObject);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Remove: TNodeArray;
begin
  SetLength(Remove, 0);
  Node := FTree.GetFirst;
  while Node <> nil do
  begin
    if FTree.Selected[Node] then
    begin
      SetLength(Remove, Length(Remove) + 1);
      Remove[High(Remove)] := Node;
    end;
    Node := FTree.GetNext(Node);
  end;

  for i := 0 to High(Remove) do
  begin
    NodeData := FTree.GetNodeData(Remove[i]);
    FList.Remove(NodeData.Title);
    NodeData.Title.Free;
    FTree.DeleteNode(Remove[i]);
  end;
end;

procedure TTitlePanel.BuildTree;
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Node := FTree.AddChild(nil);
    NodeData := FTree.GetNodeData(Node);
    NodeData.Title := FList[i];
  end;
  FTree.Sort(nil, 0, FTree.FSortDirection);
end;

procedure TTitlePanel.Setup(List: TTitleList; Images: TImageList; Title: string);
var
  P: TPanel;
begin
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := Title;
  FLabel.Align := alTop;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Parent := Self;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Align := alTop;
  FToolbarPanel.Height := 24;

  FAddEdit := TEdit.Create(Self);
  FAddEdit.Parent := FToolbarPanel;
  FAddEdit.OnKeyPress := AddEditKeyPress;
  FAddEdit.Width := 150;
  FAddEdit.Left := 0;
  FAddEdit.Top := 0;

  FToolbar := TTitleToolbar.Create(Self);
  FToolbar.Parent := FToolbarPanel;
  FToolbar.Align := alNone;
  FToolbar.Left := FAddEdit.Left + FAddEdit.Width + 8;
  FToolbar.Width := 100;
  FToolbar.Images := Images;
  FToolbar.Height := 24;
  FToolbar.Setup;
  FToolbar.FAdd.OnClick := AddClick;
  FToolbar.FRemove.OnClick := RemoveClick;

  // Ist nur für den Abstand zwischen Toolbar-Panel und Tree
  P := TPanel.Create(Self);
  P.Parent := Self;
  P.BevelOuter := bvNone;
  P.Height := 4;
  P.Top := FToolbarPanel.Top + 5;
  P.Align := alTop;

  FTree := TTitleTree.Create(Self);
  FTree.Parent := Self;
  FTree.Images := Images;
  FTree.Align := alClient;
  FTree.OnChange := TreeChange;

  FList := List;
  BuildTree;

  FToolbar.FRemove.Enabled := FTree.SelectedCount > 0;

  BevelOuter := bvNone;
end;

procedure TTitlePanel.AddEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    FToolbar.FAdd.Click;
    Key := #0;
  end;
end;

procedure TTitlePanel.TreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  FToolbar.FRemove.Enabled := FTree.SelectedCount > 0;
end;

{ TTitleToolbar }

constructor TTitleToolbar.Create(AOwner: TComponent);
begin
  inherited;

  ShowHint := True;
  Transparent := True;
end;

procedure TTitleToolbar.EnableItems(Enable: Boolean);
begin

end;

procedure TTitleToolbar.Setup;
begin
  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := _('Remove');
  FRemove.ImageIndex := 2;

  FAdd := TToolButton.Create(Self);
  FAdd.Parent := Self;
  FAdd.Hint := _('Add');
  FAdd.ImageIndex := 11;
end;

end.
