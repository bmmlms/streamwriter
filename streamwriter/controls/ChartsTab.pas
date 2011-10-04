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

  TCategoryCombo = class(TComboBoxEx)
  private
  public
    procedure LoadCategories(Categories: TList<TChartCategory>);
    procedure PostTranslate;
  end;

  TSearchPanel = class(TPanel)
  private
    FLabel: TLabel;
    FSearch: TEdit;
    FCategories: TCategoryCombo;
    FToolbar: TToolBar;

    FButtonReload: TToolButton;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent);

    procedure Setup(Images: TImageList);
  end;

  TChartArray = array of TChartEntry;
  TChartStates = (csNormal, csLoading, csError);

  TChartsTree = class(TVirtualStringTree)
  private
    FTimer: TTimer;
    FDots: string;
    FTextLeft: Integer;

    FPopupMenu: TChartsPopup;

    FColImages: TVirtualTreeColumn;
    FColTitle: TVirtualTreeColumn;
    FColChance: TVirtualTreeColumn;

    FState: TChartStates;

    FLists: TDataLists;

    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);

    procedure OnSaveListNotify(Sender: TObject; const Item: TTitleInfo; Action: TCollectionNotification);

    procedure TimerOnTimer(Sender: TObject);

    procedure FSetState(Value: TChartStates);
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
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists);
    destructor Destroy; override;

    procedure BuildTree(List: TList<TChartEntry>);

    property State: TChartStates read FState write FSetState;
  end;

  TAddToWishlistEvent = procedure(Sender: TObject; List: TStringList) of object;

  TChartsTab = class(TMainTabSheet)
  private
    FLists: TDataLists;
    FSearchPanel: TSearchPanel;
    FChartsTree: TChartsTree;
    FResultLabel: TLabel;

    FOnAddToWishlist: TAddToWishlistEvent;

    procedure GetCharts;
    procedure ShowCharts;
    procedure SetState(State: TChartStates);

    procedure SearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure HomeCommChartsReceived(Sender: TObject; CategoryList: TList<TChartCategory>;
      Genres: TList<TGenre>; ChartList: TList<TChartEntry>);
    procedure CategoriesChange(Sender: TObject);
    procedure ButtonReloadClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists);
    destructor Destroy; override;

    procedure Setup(Images: TImageList);
    procedure PostTranslate;

    procedure HomeCommStateChanged(Sender: TObject);

    property OnAddToWishlist: TAddToWishlistEvent read FOnAddToWishlist write FOnAddToWishlist;
  end;

const
  TEXT_LOADING = 'Loading charts';
  TEXT_ERROR = 'You need to be connected to the server.';
  TEXT_EVERYSONG = 'Every song';
  TEXT_RESULTS = '%d songs found';

implementation

{ TChartsTab }

procedure TChartsTab.ButtonReloadClick(Sender: TObject);
begin
  GetCharts;
end;

procedure TChartsTab.CategoriesChange(Sender: TObject);
begin
  ShowCharts;
end;

constructor TChartsTab.Create(AOwner: TComponent; Lists: TDataLists);
begin
  inherited Create(AOwner);

  FLists := Lists;

  FSearchPanel := TSearchPanel.Create(Self);
  FSearchPanel.Parent := Self;
  FSearchPanel.Align := alTop;

  FChartsTree := TChartsTree.Create(Self, FLists);
  FChartsTree.Parent := Self;
  FChartsTree.Align := alClient;

  FResultLabel := TLabel.Create(Self);
  FResultLabel.Parent := Self;
  FResultLabel.Align := alBottom;

  HomeComm.OnChartsReceived := HomeCommChartsReceived;

  ImageIndex := 68;
  ShowCloseButton := False;

  FSearchPanel.FSearch.OnKeyUp := SearchKeyUp;
  FSearchPanel.FCategories.OnChange := CategoriesChange;
end;

destructor TChartsTab.Destroy;
begin

  inherited;
end;

procedure TChartsTab.GetCharts;
begin
  if HomeComm.GetCharts then
    SetState(csLoading)
  else
    SetState(csError);
end;

procedure TChartsTab.HomeCommChartsReceived(Sender: TObject; CategoryList: TList<TChartCategory>;
  Genres: TList<TGenre>; ChartList: TList<TChartEntry>);
var
  i: Integer;
  Node: PVirtualNode;
begin
  SetState(csNormal);

  FChartsTree.Clear;

  for i := 0 to FLists.ChartCategoryList.Count - 1 do
    FLists.ChartCategoryList[i].Free;
  FLists.ChartCategoryList.Clear;

  for i := 0 to CategoryList.Count - 1 do
    FLists.ChartCategoryList.Add(CategoryList[i]);

  for i := 0 to FLists.ChartList.Count - 1 do
    FLists.ChartList[i].Free;
  FLists.ChartList.Clear;

  for i := 0 to ChartList.Count - 1 do
    FLists.ChartList.Add(ChartList[i]);

  FSearchPanel.FCategories.LoadCategories(FLists.ChartCategoryList);

  ShowCharts;
end;

procedure TChartsTab.HomeCommStateChanged(Sender: TObject);
var
  K: Char;
begin
  FSearchPanel.FButtonReload.Enabled := HomeComm.Connected;

  if (HomeComm.Connected) and
      (((FLists.ChartCategoryList.Count = 0) or (FLists.ChartList.Count = 0)) or (FChartsTree.FState <> csNormal)) then
  begin
    GetCharts;
  end else
    if (not HomeComm.Connected) and (FChartsTree.FState = csLoading) then
      SetState(csError);
end;

procedure TChartsTab.PostTranslate;
begin
  FChartsTree.FColImages.Text := _('State');
  FChartsTree.FColTitle.Text := _('Name');
  FChartsTree.FColChance.Text := _('Chance');

  FSearchPanel.FLabel.Caption := _('Search:');
  FSearchPanel.FCategories.PostTranslate;

  FResultLabel.Caption := Format(_(TEXT_RESULTS), [FChartsTree.RootNodeCount]);
end;

procedure TChartsTab.SearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ShowCharts;
end;

procedure TChartsTab.SetState(State: TChartStates);
begin
  if FChartsTree.FState <> State then
  begin
    FResultLabel.Enabled := State = csNormal;

    FChartsTree.BeginUpdate;
    FChartsTree.Clear;
    FChartsTree.EndUpdate;

    FChartsTree.State := State;
    FChartsTree.Invalidate;

    FSearchPanel.FSearch.Enabled := State = csNormal;
    FSearchPanel.FCategories.Enabled := State = csNormal;
    FSearchPanel.FToolbar.Enabled := State = csNormal;

    FSearchPanel.FButtonReload.Enabled := State = csNormal;
  end;
end;

procedure TChartsTab.Setup(Images: TImageList);
begin
  FSearchPanel.Setup(Images);

  FChartsTree.Images := Images;

  FChartsTree.PopupMenu.Images := Images;

  FSearchPanel.FButtonReload.OnClick := ButtonReloadClick;

  Caption := _('Charts');

  if ((FLists.ChartCategoryList.Count = 0) or (FLists.ChartList.Count = 0)) and (not HomeComm.Connected) then
  begin
    SetState(csError);
  end;

  if (FChartsTree.FState = csNormal) and (FLists.ChartList.Count > 0) and (FLists.CategoryList.Count > 0) then
  begin
    FSearchPanel.FCategories.LoadCategories(FLists.ChartCategoryList);
    FSearchPanel.FCategories.ItemIndex := 0;
    ShowCharts;
  end;
end;

procedure TChartsTab.ShowCharts;
var
  i, n: Integer;
  Add: Boolean;
  Node: PVirtualNode;
  NodeData: PChartNodeData;
  CatData: TChartCategory;

  P: string;
  Hash: Cardinal;
  Chars: Integer;
  Res: Boolean;

  CatMatch: Boolean;
  SearchMatch: Boolean;
begin
  if FSearchPanel.FCategories.ItemIndex = -1 then
    Exit;

  P := BuildPattern(FSearchPanel.FSearch.Text, Hash, Chars, False);

  try
    FChartsTree.BeginUpdate;
    FChartsTree.Clear;

    CatData := TChartCategory(FSearchPanel.FCategories.ItemsEx[FSearchPanel.FCategories.ItemIndex].Data);

    for i := 0 to FLists.ChartList.Count - 1 do
    begin
      CatMatch := False;
      SearchMatch := False;

      if CatData = nil then
      begin
        CatMatch := True
      end else
      begin
        for n := 0 to High(FLists.ChartList[i].Categories) do
          if FLists.ChartList[i].Categories[n] = CatData.ID then
          begin
            CatMatch := True;
            Break;
          end;
      end;

      SearchMatch := Like(FLists.ChartList[i].Name, P);

      if CatMatch and SearchMatch then
      begin
        Node := FChartsTree.AddChild(nil);
        NodeData := FChartsTree.GetNodeData(Node);
        NodeData.Chart := FLists.ChartList[i];

        for n := 0 to FLists.SaveList.Count - 1 do
          if LowerCase(FLists.SaveList[n].Title) = LowerCase(NodeData.Chart.Name) then
          begin
            NodeData.IsOnWishlist := True;
            Break;
          end;
      end;
    end;

    FChartsTree.SortTree(FChartsTree.Header.SortColumn, FChartsTree.Header.SortDirection);
  finally
    FChartsTree.EndUpdate;
  end;

  FResultLabel.Caption := Format(_(TEXT_RESULTS), [FChartsTree.RootNodeCount]);
end;

{ TChartsTree }

procedure TChartsTree.BuildTree(List: TList<TChartEntry>);
var
  i, n: Integer;
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
      NodeData.IsOnWishlist := False;

      for n := 0 to FLists.SaveList.Count - 1 do
        if LowerCase(FLists.SaveList[n].Title) = LowerCase(NodeData.Chart.Name) then
        begin
          NodeData.IsOnWishlist := True;
          Break;
        end;
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

  FLists.SaveList.OnChange.Add(OnSaveListNotify);

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerOnTimer;

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
  FPopupMenu.OnPopup := PopupMenuPopup;
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

destructor TChartsTree.Destroy;
begin

  inherited;
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

function TChartsTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  C1, C2: Integer;
  Data1, Data2: PChartNodeData;
begin
  Result := 0;

  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Column of
    0:
      begin
        C1 := 0;
        C2 := 0;
        if Data1.IsOnWishlist then
          C1 := C1 + 1;
        if Data2.IsOnWishlist then
          C2 := C2 + 1;

        Result := CmpInt(C1, C2);
        if Result = 0 then
        begin
          Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
          if Header.SortDirection = sdDescending then
            Result := Result * -1;
        end;
      end;
    1:
      Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
    2:
      begin
        Result := CmpInt(Data1.Chart.Chance, Data2.Chart.Chance);
        if Result = 0 then
        begin
          Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
          if Header.SortDirection = sdDescending then
            Result := Result * -1;
        end;
      end;
  end;
end;

procedure TChartsTree.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PChartNodeData;
begin

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

procedure TChartsTree.FSetState(Value: TChartStates);
begin
  FDots := '';

  FTimer.Enabled := False;

  case Value of
    csNormal:
      Enabled := True;
    csLoading:
      begin
        Enabled := False;
        FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_LOADING) + '..') div 2;
        FTimer.Enabled := True;
        Invalidate;
      end;
    csError:
      begin
        Enabled := False;
        FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_ERROR)) div 2;
        Invalidate;
      end;
  end;

  FState := Value;
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

procedure TChartsTree.OnSaveListNotify(Sender: TObject;
  const Item: TTitleInfo; Action: TCollectionNotification);
var
  Node: PVirtualNode;
  NodeData: PChartNodeData;
begin
  if Sender = FLists.SaveList then
  begin
    if (Action = cnAdded) or (Action = cnRemoved) then
    begin
      Node := GetFirst;
      while Node <> nil do
      begin
        NodeData := GetNodeData(Node);
        if LowerCase(NodeData.Chart.Name) = LowerCase(Item.Title) then
        begin
          NodeData.IsOnWishlist := Action = cnAdded;
          InvalidateNode(Node);
          Break;
        end;
        Node := GetNext(Node);
      end;
    end;
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
          Msg := _(TEXT_LOADING) + FDots;
          Canvas.TextOut(FTextLeft, ClientHeight div 2 - Canvas.TextHeight(Msg), Msg);
        end;
      csError:
        begin
          Msg := _(TEXT_ERROR);
          Canvas.TextOut(FTextLeft, ClientHeight div 2 - Canvas.TextHeight(Msg), Msg);
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

    if NodeData.IsOnWishlist then
      Images.Draw(PaintInfo.Canvas, L + 16, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 31);
  end;
end;

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

procedure TChartsTree.PopupMenuPopup(Sender: TObject);
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

procedure TChartsTree.Resize;
begin
  inherited;

  case FState of
    csLoading:
      FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_LOADING) + '..') div 2;
    csError:
      FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_ERROR)) div 2;
  end;
end;

procedure TChartsTree.TimerOnTimer(Sender: TObject);
begin
  FDots := FDots + '.';

  if Length(FDots) = 4 then
    FDots := '';

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

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := _('Search:');

  FSearch := TEdit.Create(Self);
  FSearch.Parent := Self;

  FCategories := TCategoryCombo.Create(Self);
  FCategories.Style := csExDropDownList;
  FCategories.Parent := Self;

  FToolbar := TToolBar.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.ShowHint := True;
  FToolbar.Align := alCustom;
end;

procedure TSearchPanel.Resize;
begin
  inherited;

end;

procedure TSearchPanel.Setup(Images: TImageList);
begin
  FCategories.Left := 0;
  FCategories.Top := 2;

  FLabel.Top := 7;
  FLabel.Left := FCategories.Left + FCategories.Width + 8;

  FSearch.Width := 200;
  FSearch.Top := FCategories.Top + 1;
  FSearch.Left := FLabel.Left + FLabel.Width + 4;

  ClientHeight := FSearch.Top + 6 + FSearch.Height;

  FToolbar.Images := Images;

  FButtonReload := TToolButton.Create(FToolbar);
  FButtonReload.Parent := FToolbar;
  FButtonReload.Hint := _('Reload');
  FButtonReload.ImageIndex := 23;

  //FToolbar.Top := 2;
  //FToolbar.Left := ClientWidth - FButtonReload.Width - 2;
  FToolbar.Padding.Top := 2;
  FToolbar.Align := alRight;
  FToolbar.Width := FButtonReload.Width + 2;
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

{ TCategoryCombo }

procedure TCategoryCombo.LoadCategories(Categories: TList<TChartCategory>);
var
  i: Integer;
  ComboItem: TComboExItem;
begin
  ItemsEx.Clear;

  ComboItem := ItemsEx.Add;
  ComboItem.Caption := _(TEXT_EVERYSONG);
  ComboItem.Data := nil;

  for i := 0 to Categories.Count - 1 do
  begin
    ComboItem := ItemsEx.Add;
    ComboItem.Caption := Categories[i].Name;
    ComboItem.Data := Categories[i];
    //ComboItem.ImageIndex := AppGlobals.LanguageIcons.GetIconIndex(LanguageList[i].ID);
  end;

  ItemIndex := 0;
end;

procedure TCategoryCombo.PostTranslate;
var
  OldIdx: Integer;
begin
  if ItemsEx.Count > 0 then
  begin
    OldIdx := ItemIndex;
    if OldIdx = -1 then
      OldIdx := 0;

    ItemIndex := -1;
    ItemsEx[0].Caption := _(TEXT_EVERYSONG);

    // Ja, das sieht doof aus, aber muss, damit sich die Caption übersetzt!
    Application.ProcessMessages;
    ItemIndex := OldIdx;
    Application.ProcessMessages;
  end;
end;

end.

