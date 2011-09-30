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
unit ChartsTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, LanguageObjects, Tabs, Functions, AppData, Logging, VirtualTrees,
  HomeCommunication, DataManager, ImgList, Graphics, Math, Generics.Collections,
  Menus, ChartsTabAdjustTitleName, Forms;

type
  TCategoryTypes = (ctAll, ctGenre);

  TCategoryNodeData = record
    CatType: TCategoryTypes;
    Genre: TGenre;
  end;
  PCategoryNodeData = ^TCategoryNodeData;

  TChartNodeData = record
    Chart: TChartEntry;
    IsOnWishlist: Boolean;
  end;
  PChartNodeData = ^TChartNodeData;

  TChartsPopup = class(TPopupMenu)
  private
    FItemAddToWishlist: TMenuItem;
    FItemEditAndAddToWishlist: TMenuItem;
  protected
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(SelectedCount: Integer; AllOnList: Boolean);

    property ItemAddToWishlist: TMenuItem read FItemAddToWishlist;
    property ItemEditAndAddToWishlist: TMenuItem read FItemEditAndAddToWishlist;
  end;

  TCategoryChangedEvent = procedure(Data: PCategoryNodeData) of object;

  TCategoryTree = class(TVirtualStringTree)
  private
    FLists: TDataLists;

    FGenreNode: PVirtualNode;

    FChangeEnabled: Boolean;

    FOnCategoryChanged: TCategoryChangedEvent;

    procedure HomeCommChartGenresReceived(Sender: TObject; List: TList<TGenre>);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
    function DoCollapsing(Node: PVirtualNode): Boolean; override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean;
      var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoChange(Node: PVirtualNode); override;
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode;
      Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
  public
    constructor Create(AOwner: TComponent);

    procedure Setup(Lists: TDataLists; Images: TImageList);
    procedure PostTranslate;

    property OnCategoryChanged: TCategoryChangedEvent read FOnCategoryChanged write FOnCategoryChanged;
  end;

  TSearchPanel = class(TPanel)
  private
    FTopPanel: TPanel;
    FLabel: TLabel;
    FSearchPanel: TPanel;
    FSearch: TEdit;
    FSearchButton: TSpeedButton;
    FCategories: TCategoryTree;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent);

    procedure Setup;
  end;

  TChartArray = array of TChartEntry;
  TChartStates = (csNormal, csLoading, csError);

  TChartsTree = class(TVirtualStringTree)
  private
    FPopupMenu: TChartsPopup;

    FColImages: TVirtualTreeColumn;
    FColTitle: TVirtualTreeColumn;
    FColChance: TVirtualTreeColumn;

    FState: TChartStates;

    FLists: TDataLists;

    procedure PopupMenuClick(Sender: TObject);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean;
      var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode;
      Column: TColumnIndex): Integer; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure DoChange(Node: PVirtualNode); override;
    procedure KeyPress(var Key: Char); override;
    function GetSelected: TChartArray;
    procedure Paint; override;
    procedure DblClick; override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo;
      ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists);

    procedure BuildTree(List: TList<TChartEntry>);
  end;

  TAddToWishlistEvent = procedure(Sender: TObject; List: TStringList) of object;

  TChartsTab = class(TMainTabSheet)
  private
    FLists: TDataLists;
    FSearchPanel: TSearchPanel;
    FChartsTree: TChartsTree;

    FLastSearch: string;
    FLastGenreID: Cardinal;

    FOnAddToWishlist: TAddToWishlistEvent;

    procedure GetCharts;
    procedure SetState(State: TChartStates);

    procedure SearchEditKeyPress(Sender: TObject; var Key: Char);
    procedure SearchButtonClick(Sender: TObject);

    procedure HomeCommChartsReceived(Sender: TObject; GenreID: Cardinal; Search: string; List: TList<TChartEntry>);
    procedure CategoriesCategoryChanged(Data: PCategoryNodeData);
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists);
    destructor Destroy; override;

    procedure Setup(Images: TImageList);
    procedure Translate;

    procedure HomeCommStateChanged(Sender: TObject);

    property OnAddToWishlist: TAddToWishlistEvent read FOnAddToWishlist write FOnAddToWishlist;
  end;

const
  GENRENODETEXT = 'Search all genres';

implementation

{ TChartsTab }

procedure TChartsTab.CategoriesCategoryChanged(Data: PCategoryNodeData);
begin
  GetCharts;
end;

constructor TChartsTab.Create(AOwner: TComponent; Lists: TDataLists);
begin
  inherited Create(AOwner);

  FLists := Lists;

  FSearchPanel := TSearchPanel.Create(Self);
  FSearchPanel.Parent := Self;
  FSearchPanel.Align := alLeft;

  FChartsTree := TChartsTree.Create(Self, FLists);
  FChartsTree.Parent := Self;
  FChartsTree.Align := alClient;

  HomeComm.OnChartsReceived := HomeCommChartsReceived;

  ImageIndex := 68;
  ShowCloseButton := False;

  FSearchPanel.FSearch.OnKeyPress := SearchEditKeyPress;
  FSearchPanel.FSearchButton.OnClick := SearchButtonClick;

  FSearchPanel.FCategories.OnCategoryChanged := CategoriesCategoryChanged;

  FLastGenreID := High(Cardinal);
end;

destructor TChartsTab.Destroy;
begin

  inherited;
end;

procedure TChartsTab.GetCharts;
var
  Node: PVirtualNode;
  NodeData: PCategoryNodeData;
  P: string;
  Hash: Cardinal;
  Chars: Integer;
  Res: Boolean;
begin
  Node := FSearchPanel.FCategories.GetFirstSelected;
  if Node <> nil then
  begin
    NodeData := FSearchPanel.FCategories.GetNodeData(Node);

    P := BuildPattern(FSearchPanel.FSearch.Text, Hash, Chars, False);

    if P = '*' then
      P := '';

    if (P = FLastSearch) and (NodeData.Genre.ID = FLastGenreID) then
    begin
      Exit;
    end;

    if (Trim(FSearchPanel.FSearch.Text) <> '') and (Chars < 3) then
    begin
      MsgBox(GetParentForm(Self).Handle, _('Please enter at least three characters to search for.'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    FChartsTree.Clear;

    if HomeComm.Connected then
    begin
      Res := False;
      case NodeData.CatType of
        ctAll:
            Res := HomeComm.GetCharts(P, 0);
        ctGenre:
            Res := HomeComm.GetCharts(P, NodeData.Genre.ID);
      end;

      if Res then
      begin
        SetState(csLoading);

        FLastSearch := P;
        FLastGenreID := NodeData.Genre.ID;
      end;
    end else
      SetState(csError);
  end;
end;

procedure TChartsTab.HomeCommChartsReceived(Sender: TObject; GenreID: Cardinal; Search: string; List: TList<TChartEntry>);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PCategoryNodeData;
begin
  Node := FSearchPanel.FCategories.FocusedNode;
  if Node <> nil then
  begin
    NodeData := FSearchPanel.FCategories.GetNodeData(Node);

    if (FLastGenreID = GenreID) and (FLastSearch = Search) then
    begin
      FChartsTree.BuildTree(List);

      SetState(csNormal);
    end else
    begin
      // Wenn wir die Charts nicht hinzufügen, werden sie nicht freigegeben. Also hier machen.
      for i := 0 to List.Count - 1 do
        List[i].Free;
    end;
  end else
    for i := 0 to List.Count - 1 do
      List[i].Free;
end;

procedure TChartsTab.HomeCommStateChanged(Sender: TObject);
var
  K: Char;
begin
  if (not HomeComm.WasConnected) and (HomeComm.Connected) then
  begin
    GetCharts;
    if FSearchPanel.FCategories.FGenreNode.ChildCount = 0 then
      HomeComm.GetChartGenres;
  end;

  if (HomeComm.WasConnected) and (not HomeComm.Connected) then
  begin
    if FChartsTree.FState <> csNormal then
      SetState(csError);
  end;
end;

procedure TChartsTab.Translate;
begin
  FChartsTree.FColImages.Text := _('State');
  FChartsTree.FColTitle.Text := _('Name');
  FChartsTree.FColChance.Text := _('Chance');

  FSearchPanel.FLabel.Caption := _('Search:');
  FSearchPanel.FCategories.PostTranslate;
end;

procedure TChartsTab.SearchButtonClick(Sender: TObject);
begin
  GetCharts;
end;

procedure TChartsTab.SearchEditKeyPress(Sender: TObject; var Key: Char);
var
  Node: PVirtualNode;
  NodeData: PCategoryNodeData;
begin
  if Key <> #13 then
    Exit;

  GetCharts;
  Key := #0;
end;

procedure TChartsTab.SetState(State: TChartStates);
begin
  if FChartsTree.FState <> State then
  begin
    FChartsTree.FState := State;
    FChartsTree.Invalidate;
  end;
end;

procedure TChartsTab.Setup(Images: TImageList);
begin
  FSearchPanel.Setup;

  FSearchPanel.FCategories.Setup(FLists, Images);

  FChartsTree.Images := Images;

  FChartsTree.PopupMenu.Images := Images;

  Caption := _('Charts');
end;

{ TChartsTree }

procedure TChartsTree.BuildTree(List: TList<TChartEntry>);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PChartNodeData;
begin
  BeginUpdate;
  try
    Clear;

    for i := 0 to List.Count - 1 do
    begin
      Node := AddChild(nil);
      NodeData := GetNodeData(Node);
      NodeData.Chart := List[i];
    end;
  finally
    SortTree(Header.SortColumn, Header.SortDirection);

    EndUpdate;
  end;
end;

constructor TChartsTree.Create(AOwner: TComponent; Lists: TDataLists);
begin
  inherited Create(AOwner);

  FLists := Lists;

  NodeDataSize := SizeOf(TChartNodeData);

  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  Header.Options := Header.Options - [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 1;

  FColImages := Header.Columns.Add;
  FColImages.Text := _('State');
  FColImages.Width := 50;
  FColImages.Options := FColImages.Options - [coResizable];

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Name');

  FColChance := Header.Columns.Add;
  FColChance.Text := _('Chance');
  FColChance.Width := 200;

  Header.Options := Header.Options + [hoAutoResize];

  FPopupMenu := TChartsPopup.Create(Self);
  FPopupMenu.ItemAddToWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemEditAndAddToWishlist.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;

  Header.SortColumn := 2;
  Header.SortDirection := sdDescending;
end;

procedure TChartsTree.DblClick;
var
  i: Integer;
  Tracks: TChartArray;
begin
  inherited;

  FPopupMenu.FItemAddToWishlist.Click;
end;

procedure TChartsTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  R: TRect;
  DrawWidth, MaxWidth, TextWidth: Integer;
  NodeData: PChartNodeData;
begin
  inherited;

  if Column = 2 then
  begin
    Canvas.Brush.Color := HTML2Color('#005fb0');
    if Selected[Node] and Focused then
      Canvas.Brush.Color := HTML2Color('#d2d2d2');

    NodeData := GetNodeData(Node);

    TextWidth := Canvas.TextWidth(IntToStr(NodeData.Chart.Chance) + '%');
    MaxWidth := CellRect.Right - CellRect.Left - 8 - TextWidth;
    DrawWidth := Trunc((NodeData.Chart.Chance / 100) * MaxWidth) - 2;

    R.Left := CellRect.Left + 2;
    R.Top := CellRect.Top + 2;
    R.Right := R.Left + DrawWidth;
    R.Bottom := CellRect.Bottom - 2;

    Canvas.FillRect(R);

    SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.TextOut(R.Right + 2, R.Top, IntToStr(NodeData.Chart.Chance) + '%');
  end;
end;

procedure TChartsTree.DoChange(Node: PVirtualNode);
var
  i: Integer;
  AllOnList: Boolean;
  N: PVirtualNode;
  NodeData: PChartNodeData;
begin
  inherited;

  AllOnList := True;

  N := GetFirst;
  while N <> nil do
  begin
    if Selected[N] then
    begin
      NodeData := GetNodeData(N);
      if not NodeData.IsOnWishlist then
      begin
        AllOnList := False;
        Break;
      end;
    end;
    N := GetNext(N);
  end;

  FPopupMenu.EnableItems(SelectedCount, AllOnList);
end;

function TChartsTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  C1, C2: Integer;
  Data1, Data2: PChartNodeData;
begin
  Result := 0;

  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Header.SortColumn of
    0:
      begin
        C1 := 0;
        C2 := 0;
        if Data1.IsOnWishlist then
          C1 := C1 + 1;
        if Data2.IsOnWishlist then
          C2 := C2 + 1;

        Result := CmpInt(C1, C2);
      end;
    1:
      Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
    2:
      Result := CmpInt(Data1.Chart.Chance, Data2.Chart.Chance);
  end;
end;

procedure TChartsTree.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PChartNodeData;
begin
  NodeData := GetNodeData(Node);
  NodeData.Chart.Free;

  inherited;
end;

function TChartsTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  // Wir müssen irgendeinen Index setzen, damit PaintImage() getriggert wird
  if (Column = 0) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    Index := 0;
end;

procedure TChartsTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
var
  NodeData: PChartNodeData;
begin
  inherited;

  Text := '';

  NodeData := GetNodeData(Node);
  case Column of
    1:
      Text := NodeData.Chart.Name;
  end;
end;

 procedure TChartsTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if Header.SortColumn <> HitInfo.Column then
    begin
      Header.SortColumn := HitInfo.Column;
      case HitInfo.Column of
        0: Header.SortDirection := sdDescending;
        1: Header.SortDirection := sdAscending;
        2: Header.SortDirection := sdDescending;
      end;
    end else
    begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end;
    SortTree(Header.SortColumn, Header.SortDirection);
  end;
end;

function TChartsTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  CmpTxt: string;
  NodeData: PChartNodeData;
begin
  NodeData := GetNodeData(Node);
  Result := StrLIComp(PChar(Text), PChar(NodeData.Chart.Name), Min(Length(Text), Length(NodeData.Chart.Name)));
end;

function TChartsTree.GetSelected: TChartArray;
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PChartNodeData;
begin
  SetLength(Result, 0);
  Node := GetFirst;
  while Node <> nil do
  begin
    if Selected[Node] then
    begin
      NodeData := GetNodeData(Node);
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := NodeData.Chart;
    end;
    Node := GetNext(Node);
  end;
end;

procedure TChartsTree.KeyPress(var Key: Char);
var
  i: Integer;
  Tracks: TChartArray;
begin
  inherited;

  if (Key = #13) or (Key = #32) then
  begin
    Key := #0;
    FPopupMenu.FItemAddToWishlist.Click;
  end;
end;

procedure TChartsTree.Paint;
var
  Msg: string;
begin
  inherited;

  SetBkMode(Canvas.Handle, TRANSPARENT);

  if RootNodeCount = 0 then
    case FState of
      csNormal: ;
      csLoading:
        begin
          Msg := _('Loading...');
          Canvas.TextOut(ClientWidth div 2 - Canvas.TextWidth(MSg) div 2, ClientHeight div 2 - Canvas.TextHeight(Msg), Msg);
        end;
      csError:
        begin
          Msg := _('You need to be connected to the server.');
          Canvas.TextOut(ClientWidth div 2 - Canvas.TextWidth(Msg) div 2, ClientHeight div 2 - Canvas.TextHeight(Msg), Msg);
        end;
    end;
end;

procedure TChartsTree.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  L, i: Integer;
  Found: Boolean;
  NodeData: PChartNodeData;
begin
  if PaintInfo.Column = 0 then
  begin
    NodeData := GetNodeData(PaintInfo.Node);

    L := PaintInfo.ImageInfo[ImageInfoIndex].XPos;

    Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 20);

    Found := False;
    for i := 0 to FLists.SaveList.Count - 1 do
      if LowerCase(FLists.SaveList[i].Title) = LowerCase(NodeData.Chart.Name) then
      begin
        Found := True;
        Break;
      end;

    NodeData.IsOnWishlist := Found;

    if Found then
      Images.Draw(PaintInfo.Canvas, L + 16, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 31);
  end;
end;
                  // TODO: erste spalte darf nicht resized werden!!!
procedure TChartsTree.PopupMenuClick(Sender: TObject);
var
  i: Integer;
  Titles: TStringList;
  Charts: TChartArray;
  P: TControl;
  Pattern: string;
  F: TfrmChartsTabAdjustTitleName;
begin
  Titles := TStringList.Create;

  P := Parent;
  while not (P.ClassType = TChartsTab) do
    P := P.Parent;

  Charts := GetSelected;
  try
    if Sender = FPopupMenu.ItemAddToWishlist then
    begin
      for i := 0 to High(Charts) do
      begin
        Titles.Add(Charts[i].Name);
      end;

      if Titles.Count > 0 then
        TChartsTab(P).FOnAddToWishlist(Self, Titles)
    end else if Sender = FPopupMenu.ItemEditAndAddToWishlist then
    begin
      if Length(Charts) <> 1 then
        Exit;

      F := TfrmChartsTabAdjustTitleName.Create(GetParentForm(Self), Charts[0].Name);
      try
        F.ShowModal;

        if F.Okay then
        begin
          Titles.Add(F.TitleName);
          TChartsTab(P).FOnAddToWishlist(Self, Titles);
        end;
      finally
        F.Free;
      end;
    end;
  finally
    Titles.Free;
  end;

  Invalidate;
end;

{ TSearchPanel }

constructor TSearchPanel.Create(AOwner: TComponent);
var
  I: TIcon;
  B: TBitmap;
begin
  inherited;

  BevelOuter := bvNone;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Align := alTop;
  FTopPanel.Padding.Top := 0;
  FTopPanel.Padding.Left := 0;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := FTopPanel;
  FLabel.Caption := _('Search:');
  FLabel.Align := alTop;

  FSearchPanel := TPanel.Create(Self);
  FSearchPanel.Parent := FTopPanel;
  FSearchPanel.BevelOuter := bvNone;
  FSearchPanel.Align := alTop;
  FSearchPanel.Height := 24;
  FSearchPanel.Padding.Top := 2;

  FSearch := TEdit.Create(Self);
  FSearch.Parent := FSearchPanel;
  FSearch.Left := 0;
  FSearch.Top := 2;

  FSearchButton := TSpeedButton.Create(Self);
  FSearchButton.Parent := FSearchPanel;
  FSearchButton.Anchors := [akRight, akTop];
  FSearchButton.Flat := True;
  FSearchButton.Hint := 'Search';
  FSearchButton.ShowHint := True;

  I := TIcon.Create;
  I.LoadFromResourceName(HInstance, 'SEARCH');
  B := TBitmap.Create;
  B.Width := 32;
  B.Height := 32;
  B.Canvas.Draw(0, 0, I);
  B.Canvas.StretchDraw(Rect(0, 0, 16, 16), B);
  B.Width := 16;
  B.Height := 16;
  FSearchButton.Glyph := B;
  FSearchButton.Glyph.PixelFormat := pf24bit;
  B.Free;
  I.Free;

  FCategories := TCategoryTree.Create(Self);
  FCategories.Align := alClient;
  FCategories.Parent := Self;
end;

procedure TSearchPanel.Resize;
begin
  inherited;

  FSearchButton.Left := ClientWidth - 4 - FSearchButton.Width;
  FSearch.Width := FTopPanel.ClientWidth - 8 - FSearchButton.Width;
end;

procedure TSearchPanel.Setup;
begin
  FTopPanel.Height := FLabel.Height + FSearchPanel.Height + 2;

  FSearchButton.Width := 24;
  FSearchButton.Height := 24;
  FSearchButton.Top := 0;
end;

{ TCategoryTree }

constructor TCategoryTree.Create(AOwner: TComponent);
begin
  inherited;

  NodeDataSize := SizeOf(TCategoryNodeData);

  IncrementalSearch := isVisibleOnly;
  TreeOptions.SelectionOptions := [toDisableDrawSelection, toFullRowSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop] + [toFullRowDrag];

  FChangeEnabled := True;

  HomeComm.OnChartGenresReceived := HomeCommChartGenresReceived;
end;

procedure TCategoryTree.DoChange(Node: PVirtualNode);
begin
  inherited;

  if Node = nil then
    Exit;

  if Assigned(FOnCategoryChanged) then
    FOnCategoryChanged(PCategoryNodeData(GetNodeData(Node)));
end;

function TCategoryTree.DoCollapsing(Node: PVirtualNode): Boolean;
begin
  Result := False;
end;

function TCategoryTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  ND1, ND2: PCategoryNodeData;
begin
  ND1 := GetNodeData(Node1);
  ND2 := GetNodeData(Node2);

  Result := CompareText(ND1.Genre.Name, ND2.Genre.Name);
end;

procedure TCategoryTree.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PCategoryNodeData;
begin
  NodeData := GetNodeData(Node);

  if NodeData.Genre <> nil then
    NodeData.Genre.Free;

  inherited;
end;

function TCategoryTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
var
  NodeData: PCategoryNodeData;
begin
  Result := inherited;

  NodeData := GetNodeData(Node);
  case NodeData.CatType of
    ctAll:
      Index := 28;
    ctGenre:
      Index := 16;
  end;
end;

procedure TCategoryTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
var
  NodeData: PCategoryNodeData;
begin
  inherited;

  if TextType = ttStatic then
    Exit;

  NodeData := GetNodeData(Node);

  Text := NodeData.Genre.Name;
end;

function TCategoryTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  CmpTxt: string;
  NodeData: PCategoryNodeData;
begin
  NodeData := GetNodeData(Node);
  Result := StrLIComp(PChar(Text), PChar(NodeData.Genre.Name), Min(Length(Text), Length(NodeData.Genre.Name)));
end;

procedure TCategoryTree.HomeCommChartGenresReceived(Sender: TObject;
  List: TList<TGenre>);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PCategoryNodeData;
begin
  BeginUpdate;
  try
    DeleteChildren(FGenreNode);

    for i := 0 to List.Count - 1 do
    begin
      if List[i].ChartCount = 0 then
        Continue;

      Node :=  AddChild(FGenreNode);
      NodeData := GetNodeData(Node);
      NodeData.Genre := List[i];
      NodeData.CatType := ctGenre;
    end;

    Sort(FGenreNode, 0, sdAscending);

    Expanded[FGenreNode] := True;

    ScrollIntoView(GetFirst, False)
  finally
    EndUpdate;
  end;
end;

procedure TCategoryTree.PostTranslate;
var
  NodeData: PCategoryNodeData;
begin
  NodeData := GetNodeData(FGenreNode);
  NodeData.Genre.Name := _(GENRENODETEXT);
end;

procedure TCategoryTree.Setup(Lists: TDataLists; Images: TImageList);
var
  i: Integer;
  NodeGenres, Node: PVirtualNode;
  NodeData: PCategoryNodeData;
begin
  FLists := Lists;
  Self.Images := Images;

  Clear;

  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  // TODO: Das hier muss ins POST-TRANSLATE!!!
  NodeData.Genre := TGenre.Create(_(GENRENODETEXT), 0, 0);
  NodeData.CatType := ctAll;
  FGenreNode := Node;

  FocusedNode := GetFirst;
  Selected[GetFirst] := True;
end;

{ TChartsPopup }

constructor TChartsPopup.Create(AOwner: TComponent);
begin
  inherited;

  FItemAddToWishlist := CreateMenuItem;
  FItemAddToWishlist.Caption := '&Add to wishlist';
  FItemAddToWishlist.ImageIndex := 31;
  FItemAddToWishlist.Default := True;
  Items.Add(FItemAddToWishlist);

  FItemEditAndAddToWishlist := CreateMenuItem;
  FItemEditAndAddToWishlist.Caption := '&Edit and add to wishlist';
  FItemEditAndAddToWishlist.ImageIndex := 30;
  Items.Add(FItemEditAndAddToWishlist);
end;

procedure TChartsPopup.EnableItems(SelectedCount: Integer; AllOnList: Boolean);
begin
  FItemAddToWishlist.Enabled := (SelectedCount > 0) and (not AllOnList);
  FItemEditAndAddToWishlist.Enabled := SelectedCount = 1;
end;

end.

