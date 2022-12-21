{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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

{ This unit is for showing radio-charts in the main-window }

unit ChartsTab;

interface

uses
  AppData,
  AppMessages,
  Buttons,
  ChartsPopup,
  ChartsTabAdjustTitleName,
  Classes,
  ComboEx,
  ComCtrls,
  Controls,
  DataManager,
  DateUtils,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  HomeCommunication,
  Images,
  ImgList,
  LanguageObjects,
  Logging,
  MControls,
  Menus,
  MessageBus,
  SharedControls,
  SharedData,
  StdCtrls,
  SysUtils,
  Tabs,
  TypeDefs,
  VirtualTrees,
  Windows;

type
  TNodeTypes = (ntChart, ntStream, ntAll);

  TChartNodeData = record
    Chart: TChartEntry;
    Stream: TChartStream;
    IsOnWishlist: Boolean;
    IsArtistOnWishlist: Boolean;
  end;
  PChartNodeData = ^TChartNodeData;

  TChartDataArray = array of PChartNodeData;

  { TSearchPanel }

  TSearchPanel = class(TPanel)
  private
    FLabel: TLabel;
    FSearch: TComboBoxExEditable;
    FToolbar: TToolbarForcedHorizontal;

    FButtonAddToWishlist: TToolButton;
    FButtonRemoveFromWishlist: TToolButton;
    FButtonAddArtistToWishlist: TToolButton;
    FButtonEditAndAddToWishlist: TToolButton;
    FButtonStartStreaming: TToolButton;
    FButtonPlayStream: TToolButton;
    FButtonPlayStreamExternal: TToolButton;
    FButtonAddStream: TToolButton;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure InsertSearchItem(Search: string);

    procedure PostTranslate;
  end;

  TChartArray = array of TChartEntry;
  TChartStates = (csNormal, csSearching, csSearchError);

  { TChartsTree }

  TChartsTree = class(TVirtualStringTree)
  private
    FTimer: TTimer;
    FDots: string;
    FTextLeft: Integer;
    FProgressBar: TProgressBar;
    FPopupMenu: TChartsPopup;
    FColTitle: TVirtualTreeColumn;
    FColImages: TVirtualTreeColumn;
    FColLastPlayed: TVirtualTreeColumn;
    FColChance: TVirtualTreeColumn;
    FHeaderDragSourcePosition: Cardinal;
    FState: TChartStates;

    procedure FitColumns;
    procedure MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
    procedure MessageReceived(Msg: TMessageBase);
    procedure PopupMenuClick(Sender: TObject);
    procedure TimerOnTimer(Sender: TObject);
    procedure FSetState(Value: TChartStates);
    procedure ExecDefaultAction;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure Resize; override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    function DoHeaderDragging(Column: TColumnIndex): Boolean; override;
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); override;
    procedure DoNodeDblClick(const HitInfo: THitInfo); override;
    function DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean; override;
    procedure DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    function GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
    function NodesToData(Nodes: TNodeArray): TChartDataArray;

    property State: TChartStates read FState write FSetState;
  end;

  TAddToWishlistEvent = procedure(Sender: TObject; Arr: TWishlistTitleInfoArray) of object;
  TAddStreamsEvent = procedure(Sender: TObject; Info: TStartStreamingInfoArray; Action: TStreamOpenActions) of object;
  TGetIsStreamOnListEvent = function(Sender: TObject; Stream: TStreamBrowserEntry): Boolean of object;

  { TChartsTab }

  TChartsTab = class(TMainTabSheet)
  private
    FSearchPanel: TSearchPanel;
    FChartsTree: TChartsTree;
    FResultLabel: TLabel;
    FState: TChartStates;
    FCharts: TChartList;
    FSearched: Boolean;

    FOnAddToWishlist: TAddToWishlistEvent;
    FOnRemoveTitleFromWishlist: TAddToWishlistEvent;
    FOnAddStreams: TAddStreamsEvent;
    FOnGetIsStreamOnListEvent: TGetIsStreamOnListEvent;

    procedure ShowCharts;
    procedure UpdateButtons;
    procedure SearchKeyPress(Sender: TObject; var Key: Char);
    procedure SearchSelect(Sender: TObject);
    procedure HomeCommSearchChartsReceived(Sender: TObject; Success: Boolean; Charts: TChartList);
    procedure ButtonClick(Sender: TObject);
    procedure ChartsTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure PostTranslate;
    procedure SetState(State: TChartStates);
    procedure SearchCharts(Top, ShowMessages: Boolean);
    procedure HomeCommStateChanged(Sender: TObject);

    property ChartsTree: TChartsTree read FChartsTree;
    property State: TChartStates read FState;
    property Searched: Boolean read FSearched;
    property OnAddToWishlist: TAddToWishlistEvent read FOnAddToWishlist write FOnAddToWishlist;
    property OnRemoveTitleFromWishlist: TAddToWishlistEvent read FOnRemoveTitleFromWishlist write FOnRemoveTitleFromWishlist;
    property OnAddStreams: TAddStreamsEvent read FOnAddStreams write FOnAddStreams;
    property OnGetIsStreamOnListEvent: TGetIsStreamOnListEvent read FOnGetIsStreamOnListEvent write FOnGetIsStreamOnListEvent;
  end;

const
  TEXT_SEARCHING = 'Searching titles';
  TEXT_SEARCH_ERROR = 'Error searching titles.';
  TEXT_RESULTS = '%d songs found';
  SEARCH_TOP = '[Most played]';

implementation

{ TChartsTab }

procedure TChartsTab.ButtonClick(Sender: TObject);
begin
  if Sender = FSearchPanel.FButtonAddToWishlist then
    FChartsTree.FPopupMenu.ItemAddToWishlist.Click
  else if Sender = FSearchPanel.FButtonRemoveFromWishlist then
    FChartsTree.FPopupMenu.ItemRemoveFromWishlist.Click
  else if Sender = FSearchPanel.FButtonAddArtistToWishlist then
    FChartsTree.FPopupMenu.ItemAddArtistToWishlist.Click
  else if Sender = FSearchPanel.FButtonEditAndAddToWishlist then
    FChartsTree.FPopupMenu.ItemEditAndAddToWishlist.Click
  else if Sender = FSearchPanel.FButtonStartStreaming then
    FChartsTree.FPopupMenu.ItemStartStreaming.Click
  else if Sender = FSearchPanel.FButtonPlayStream then
    FChartsTree.FPopupMenu.ItemPlayStream.Click
  else if Sender = FSearchPanel.FButtonPlayStreamExternal then
    FChartsTree.FPopupMenu.ItemPlayStreamExternal.Click
  else if Sender = FSearchPanel.FButtonAddStream then
    FChartsTree.FPopupMenu.ItemAddStream.Click;
end;

procedure TChartsTab.ChartsTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateButtons;
end;

constructor TChartsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSearchPanel := TSearchPanel.Create(Self);
  FSearchPanel.Parent := Self;
  FSearchPanel.Align := alTop;

  FChartsTree := TChartsTree.Create(Self);
  FChartsTree.Parent := Self;
  FChartsTree.Align := alClient;
  FChartsTree.OnChange := ChartsTreeChange;

  FResultLabel := TLabel.Create(Self);
  FResultLabel.Parent := Self;
  FResultLabel.Align := alBottom;
  FResultLabel.Caption := Format(_(TEXT_RESULTS), [0]);

  HomeComm.OnSearchChartsReceived := HomeCommSearchChartsReceived;

  ImageIndex := TImages.FIND;
  ShowCloseButton := False;

  FSearchPanel.FSearch.OnKeyPress := SearchKeyPress;
  FSearchPanel.FSearch.OnSelect := SearchSelect;

  FChartsTree.Images := modSharedData.imgImages;

  if Screen.PixelsPerInch = 96 then
    FChartsTree.PopupMenu.Images := modSharedData.imgImages;

  FSearchPanel.FButtonAddToWishlist.OnClick := ButtonClick;
  FSearchPanel.FButtonRemoveFromWishlist.OnClick := ButtonClick;
  FSearchPanel.FButtonAddArtistToWishlist.OnClick := ButtonClick;
  FSearchPanel.FButtonEditAndAddToWishlist.OnClick := ButtonClick;
  FSearchPanel.FButtonStartStreaming.OnClick := ButtonClick;
  FSearchPanel.FButtonPlayStream.OnClick := ButtonClick;
  FSearchPanel.FButtonPlayStreamExternal.OnClick := ButtonClick;
  FSearchPanel.FButtonAddStream.OnClick := ButtonClick;

  Caption := 'Title search';

  UpdateButtons;
end;

destructor TChartsTab.Destroy;
var
  i: Integer;
begin
  if FCharts <> nil then
  begin
    for i := 0 to FCharts.Count - 1 do
      FCharts[i].Free;
    FreeAndNil(FCharts);
  end;

  inherited;
end;

procedure TChartsTab.HomeCommSearchChartsReceived(Sender: TObject; Success: Boolean; Charts: TChartList);
var
  i: Integer;
begin
  FSearched := True;

  if FCharts <> nil then
  begin
    for i := 0 to FCharts.Count - 1 do
      FCharts[i].Free;
    FCharts.Free;
  end;

  FCharts := Charts;

  FChartsTree.BeginUpdate;
  try
    FChartsTree.Clear;
  finally
    FChartsTree.EndUpdate;
  end;

  if Success then
    SetState(csNormal)
  else
    SetState(csSearchError);

  ShowCharts;

  FSearchPanel.FSearch.SelectAll;
  FSearchPanel.FSearch.ApplyFocus;
end;

procedure TChartsTab.HomeCommStateChanged(Sender: TObject);
begin

end;

procedure TChartsTab.PostTranslate;
begin
  FChartsTree.FColImages.Text := _('State');
  FChartsTree.FColTitle.Text := _('Name');
  FChartsTree.FColChance.Text := _('Played last day/week');

  FSearchPanel.PostTranslate;

  FResultLabel.Caption := Format(_(TEXT_RESULTS), [FChartsTree.RootNodeCount]);
end;

procedure TChartsTab.SearchCharts(Top, ShowMessages: Boolean);
var
  Tmp: string;
  Abort: Boolean;
  SL: TStringList;
begin
  if not HomeComm.CommunicationEstablished then
  begin
    if ShowMessages then
      TFunctions.MsgBox(_('streamWriter needs to be connected to the server in order to search.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if Top then
  begin
    HomeComm.SendSearchCharts(True, '');
    SetState(csSearching);
  end else
  begin
    Abort := False;

    if FSearchPanel.FSearch.ItemIndex = -1 then
      Tmp := Trim(FSearchPanel.FSearch.Text)
    else
      Tmp := Trim(FSearchPanel.FSearch.ItemsEx[FSearchPanel.FSearch.ItemIndex].Caption);

    if (Pos('"', Tmp) > 0) and (TFunctions.OccurenceCount('"', Tmp) mod 2 <> 0) then
    begin
      TFunctions.MsgBox(_('When using quotes every opening quote needs a closing quote.'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if not Abort then
    begin
      SL := TStringList.Create;
      try
        TFunctions.Explode(' ', Tmp, SL);

        if SL.Count = 0 then
          Abort := True;
      finally
        SL.Free;
      end;
    end;

    if Abort then
      TFunctions.MsgBox(_('You need to specify at least one word to search for. Special chars (+-*()<>~'') are not allowed.'), _('Info'), MB_ICONINFORMATION)
    else
    begin
      FSearchPanel.InsertSearchItem(Tmp);

      HomeComm.SendSearchCharts(False, Tmp);
      SetState(csSearching);
    end;
  end;
end;

procedure TChartsTab.SearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if FSearchPanel.FSearch.ItemIndex = 0 then
    begin
      SearchCharts(True, True);
      FSearchPanel.FSearch.ItemIndex := 0;
    end else
      SearchCharts(False, True);
    Key := #0;
  end;
end;

procedure TChartsTab.SearchSelect(Sender: TObject);
begin
  if FSearchPanel.FSearch.ItemIndex = 0 then
    SearchCharts(True, True)
  else
    SearchCharts(False, True);
end;

procedure TChartsTab.SetState(State: TChartStates);
begin
  FState := State;
  if FChartsTree.FState <> State then
  begin
    FChartsTree.BeginUpdate;
    FChartsTree.Clear;
    FChartsTree.EndUpdate;

    FChartsTree.State := State;
    FChartsTree.Invalidate;

    FChartsTree.Enabled := not (State = csSearching);
    FSearchPanel.Enabled := not (State = csSearching);
    FSearchPanel.FLabel.Enabled := not (State = csSearching);
    FSearchPanel.FSearch.Enabled := not (State = csSearching);
    FResultLabel.Enabled := not (State = csSearching);

    if State = csSearching then
      FResultLabel.Caption := Format(_(TEXT_RESULTS), [0]);

    UpdateButtons;
  end;
end;

procedure TChartsTab.ShowCharts;
var
  i, n: Integer;
  Node, NodeStream: PVirtualNode;
  NodeData, NodeDataStream: PChartNodeData;
begin
  if FCharts <> nil then
  begin
    FChartsTree.BeginUpdate;
    try
      for i := 0 to FCharts.Count - 1 do
      begin
        Node := FChartsTree.AddChild(nil);
        NodeData := FChartsTree.GetNodeData(Node);
        NodeData.Chart := FCharts[i];

        NodeData.IsOnWishlist := False;
        NodeData.IsArtistOnWishlist := False;

        NodeData.Chart.LoadStreams;

        for n := 0 to NodeData.Chart.Streams.Count - 1 do
        begin
          NodeStream := FChartsTree.AddChild(Node);
          NodeDataStream := FChartsTree.GetNodeData(NodeStream);
          NodeDataStream.Stream := NodeData.Chart.Streams[n];
        end;

        for n := 0 to AppGlobals.Data.SaveList.Count - 1 do
        begin
          if (AppGlobals.Data.SaveList[n].ServerHash > 0) and (AppGlobals.Data.SaveList[n].ServerHash = NodeData.Chart.ServerHash) then
            NodeData.IsOnWishlist := True;
          if (AppGlobals.Data.SaveList[n].ServerArtistHash > 0) and (AppGlobals.Data.SaveList[n].ServerArtistHash = NodeData.Chart.ServerArtistHash) then
            NodeData.IsArtistOnWishlist := True;

          if NodeData.IsOnWishlist and NodeData.IsArtistOnWishlist then
            Break;
        end;
      end;

      FChartsTree.SortTree(FChartsTree.Header.SortColumn, FChartsTree.Header.SortDirection);
    finally
      FChartsTree.EndUpdate;
    end;
  end;

  FResultLabel.Caption := Format(_(TEXT_RESULTS), [FChartsTree.RootNodeCount]);
end;

procedure TChartsTab.UpdateButtons;
var
  AllOnWishlist, AllArtistsOnList, AtLeastOneOnWishlist: Boolean;
  OneSelectedChart, ManySelectedCharts: Boolean;
  OneSelectedStream, ManySelectedStreams: Boolean;
  AtLeastOneArtistSelected: Boolean;
  N: PVirtualNode;
  NodeData: PChartNodeData;
begin
  inherited;

  AllOnWishlist := True;
  AtLeastOneOnWishlist := False;
  AllArtistsOnList := True;
  OneSelectedChart := False;
  ManySelectedCharts := False;
  OneSelectedStream := False;
  ManySelectedStreams := False;
  AtLeastOneArtistSelected := False;

  N := FChartsTree.GetFirstSelected;
  while N <> nil do
  begin
    if FChartsTree.Selected[N] then
    begin
      NodeData := FChartsTree.GetNodeData(N);

      if NodeData.Chart <> nil then
      begin
        if OneSelectedChart then
        begin
          ManySelectedCharts := True;
          OneSelectedChart := False;
        end else if (not OneSelectedChart) and (not ManySelectedCharts) then
          OneSelectedChart := True;

        if not NodeData.IsOnWishlist then
          AllOnWishlist := False;
        if not NodeData.IsArtistOnWishlist then
          AllArtistsOnList := False;

        if NodeData.Chart.ServerArtistHash > 0 then
          AtLeastOneArtistSelected := True;
        if NodeData.IsOnWishlist then
          AtLeastOneOnWishlist := True;
      end;

      if NodeData.Stream <> nil then
        if OneSelectedStream then
        begin
          ManySelectedStreams := True;
          OneSelectedStream := False;
        end else if (not OneSelectedStream) and (not ManySelectedStreams) then
          OneSelectedStream := True;
    end;
    N := FChartsTree.GetNextSelected(N);
  end;

  FChartsTree.FPopupMenu.ItemAddToWishlist.Enabled := (not AllOnWishlist) and (OneSelectedChart or ManySelectedCharts) and (State = csNormal);
  FChartsTree.FPopupMenu.ItemRemoveFromWishlist.Enabled := AtLeastOneOnWishlist and (OneSelectedChart or ManySelectedCharts) and (State = csNormal);
  FChartsTree.FPopupMenu.ItemAddArtistToWishlist.Enabled := (not AllArtistsOnList) and (AtLeastOneArtistSelected or ManySelectedCharts) and (State = csNormal);
  FChartsTree.FPopupMenu.ItemEditAndAddToWishlist.Enabled := (OneSelectedChart) and (State = csNormal);
  FChartsTree.FPopupMenu.ItemStartStreaming.Enabled := (OneSelectedStream or ManySelectedStreams) and (State = csNormal);
  FChartsTree.FPopupMenu.ItemPlayStream.Enabled := (OneSelectedStream) and (State = csNormal);
  FChartsTree.FPopupMenu.ItemPlayStreamExternal.Enabled := (OneSelectedStream) and (State = csNormal);
  FChartsTree.FPopupMenu.ItemAddStream.Enabled := (OneSelectedStream or ManySelectedStreams) and (State = csNormal);

  FSearchPanel.FButtonAddToWishlist.Enabled := FChartsTree.FPopupMenu.ItemAddToWishlist.Enabled;
  FSearchPanel.FButtonRemoveFromWishlist.Enabled := FChartsTree.FPopupMenu.ItemRemoveFromWishlist.Enabled;
  FSearchPanel.FButtonAddArtistToWishlist.Enabled := FChartsTree.FPopupMenu.ItemAddArtistToWishlist.Enabled;
  FSearchPanel.FButtonEditAndAddToWishlist.Enabled := FChartsTree.FPopupMenu.ItemEditAndAddToWishlist.Enabled;
  FSearchPanel.FButtonStartStreaming.Enabled := FChartsTree.FPopupMenu.ItemStartStreaming.Enabled;
  FSearchPanel.FButtonPlayStream.Enabled := FChartsTree.FPopupMenu.ItemPlayStream.Enabled;
  FSearchPanel.FButtonPlayStreamExternal.Enabled := FChartsTree.FPopupMenu.ItemPlayStreamExternal.Enabled;
  FSearchPanel.FButtonAddStream.Enabled := FChartsTree.FPopupMenu.ItemAddStream.Enabled;
end;

{ TChartsTree }

constructor TChartsTree.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  MsgBus.AddSubscriber(MessageReceived);

  FState := csNormal;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerOnTimer;

  NodeDataSize := SizeOf(TChartNodeData);

  IncrementalSearch := isVisibleOnly;

  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  Header.Options := [hoColumnResize, hoDrag, hoAutoResize, hoHotTrack, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleOnDblClick];
  ShowHint := True;
  HintMode := hmTooltip;

  Header.AutoSizeIndex := 0;

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Name');
  FColTitle.Options := FColTitle.Options - [coDraggable];
  FColImages := Header.Columns.Add;
  FColImages.Text := _('State');
  FColImages.Options := FColImages.Options - [coResizable];
  FColLastPlayed := Header.Columns.Add;
  FColLastPlayed.Text := _('Last played');
  FColChance := Header.Columns.Add;
  FColChance.Text := _('Played last day/week');
  FColChance.Alignment := taRightJustify;

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);
  TMTreeColumnPopup(Header.PopupMenu).OnAction := MenuColsAction;

  FPopupMenu := TChartsPopup.Create(Self);
  FPopupMenu.ItemAddToWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemoveFromWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemAddArtistToWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemEditAndAddToWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemStartStreaming.OnClick := PopupMenuClick;
  FPopupMenu.ItemPlayStream.OnClick := PopupMenuClick;
  FPopupMenu.ItemPlayStreamExternal.OnClick := PopupMenuClick;
  FPopupMenu.ItemAddStream.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;

  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Parent := Self;
  FProgressBar.Width := 200;
  FProgressBar.Height := 20;
  FProgressBar.Style := pbstMarquee;
  FProgressBar.Visible := False;

  Header.SortColumn := 3;
  Header.SortDirection := sdDescending;

  for i := 1 to Header.Columns.Count - 1 do
    if not ((AppGlobals.ChartCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];

  FitColumns;
end;

destructor TChartsTree.Destroy;
begin
  MsgBus.RemoveSubscriber(MessageReceived);

  inherited;
end;

procedure TChartsTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect);
var
  C: Extended;
  Chance: Integer;
  R: TRect;
  DrawWidth, MaxWidth, TextWidth: Integer;
  NodeData: PChartNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);
  if (Column = 3) and (NodeData.Chart <> nil) then
  begin
    C := ((NodeData.Chart.PlayedLastWeek) / 14) * 24;
    if C > 100 then
      C := 100;
    Chance := Trunc(C);

    Canvas.Brush.Color := TFunctions.HTML2Color('#005fb0');
    if Selected[Node] and Focused then
      Canvas.Brush.Color := TFunctions.HTML2Color('#d2d2d2');

    TextWidth := Canvas.TextWidth('1000 / 1000');
    MaxWidth := CellRect.Right - CellRect.Left - 8 - TextWidth;
    DrawWidth := Trunc((Chance / 100) * MaxWidth) - 2;

    if DrawWidth < 1 then
      Exit;

    R.Left := CellRect.Left + 2;
    R.Top := CellRect.Top + 2;
    R.Right := R.Left + DrawWidth;
    R.Bottom := CellRect.Bottom - 2;

    Canvas.FillRect(R);
  end;
end;

function TChartsTree.DoCompare(Node1: PVirtualNode; Node2: PVirtualNode; Column: TColumnIndex): Integer;
var
  i: Integer;
  C1, C2: Integer;
  Data1, Data2: PChartNodeData;
begin
  Result := 0;

  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  if (Data1.Chart <> nil) and (Data2.Chart <> nil) then
  begin
    case Column of
      0:
        Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
      1:
      begin
        C1 := 0;
        C2 := 0;

        for i := 0 to AppGlobals.Data.SavedTitleHashes.Count - 1 do
        begin
          if (Data1.Chart <> nil) and (Data1.Chart.ServerHash = AppGlobals.Data.SavedTitleHashes[i]) then
            C1 := C1 + 1;
          if (Data2.Chart <> nil) and (Data2.Chart.ServerHash = AppGlobals.Data.SavedTitleHashes[i]) then
            C2 := C2 + 1;
        end;

        if Data1.IsArtistOnWishlist then
          C1 := C1 + 2;
        if Data2.IsArtistOnWishlist then
          C2 := C2 + 2;

        if Data1.IsOnWishlist then
          C1 := C1 + 3;
        if Data2.IsOnWishlist then
          C2 := C2 + 3;

        Result := TFunctions.CmpInt(C1, C2);
        if Result = 0 then
        begin
          Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
          if Header.SortDirection = sdDescending then
            Result := Result * -1;
        end;
      end;
      2:
        Result := TFunctions.CmpInt(Data1.Chart.PlayedLast, Data2.Chart.PlayedLast);
      3:
      begin
        Result := TFunctions.CmpInt(Data1.Chart.PlayedLastWeek, Data2.Chart.PlayedLastWeek);
        if Result = 0 then
        begin
          Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
          if Header.SortDirection = sdDescending then
            Result := Result * -1;
        end;
      end;
    end;
  end else if (Data1.Stream <> nil) and (Data2.Stream <> nil) then
    case Column of
      0, 1:
      begin
        Result := CompareText(Data1.Stream.Stream.Name, Data2.Stream.Stream.Name);
        if (Header.SortDirection = sdDescending) then
          Result := Result * -1;
      end;
      2:
      begin
        Result := TFunctions.CmpInt(Data2.Stream.PlayedLast, Data1.Stream.PlayedLast);
        if (Header.SortDirection = sdAscending) then
          Result := Result * -1;
      end;
      3:
      begin
        Result := TFunctions.CmpInt(Data1.Stream.PlayedLastWeek, Data2.Stream.PlayedLastWeek);
        if (Header.SortDirection = sdAscending) then
          Result := Result * -1;
      end;
    end;
end;

function TChartsTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  // Wir müssen irgendeinen Index setzen, damit PaintImage() getriggert wird
  if ((Column = 0) or (Column = 1)) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    Index := 0;
end;

procedure TChartsTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
var
  Val: Int64;
  NodeData: PChartNodeData;
begin
  inherited;

  Val := 1;
  Text := '';

  NodeData := GetNodeData(Node);
  case Column of
    0:
      if NodeData.Chart <> nil then
        Text := NodeData.Chart.Name
      else
        Text := NodeData.Stream.Stream.Name;
    2:
    begin
      if NodeData.Chart <> nil then
        Val := NodeData.Chart.PlayedLast
      else if NodeData.Stream <> nil then
        Val := NodeData.Stream.PlayedLast;

      if Val < 1 then
        Val := 1;

      if Val >= 86400 then
      begin
        if Val div 86400 = 1 then
          Text := Format(_('%d day ago'), [Val div 86400])
        else
          Text := Format(_('%d days ago'), [Val div 86400]);
      end else if Val >= 3600 then
      begin
        if Val div 3600 = 1 then
          Text := Format(_('%d hour ago'), [Val div 3600])
        else
          Text := Format(_('%d hours ago'), [Val div 3600]);
      end else if Val >= 60 then
      begin
        if Val div 60 = 1 then
          Text := Format(_('%d minute ago'), [Val div 60])
        else
          Text := Format(_('%d minutes ago'), [Val div 60]);
      end else if Val = 1 then
        Text := Format(_('%d second ago'), [Val])
      else
        Text := Format(_('%d seconds ago'), [Val]);
    end;
    3:
      if NodeData.Chart <> nil then
        Text := Format('%d / %d', [NodeData.Chart.PlayedLastDay, NodeData.Chart.PlayedLastWeek])
      else
        Text := Format('%d / %d', [NodeData.Stream.PlayedLastDay, NodeData.Stream.PlayedLastWeek]);
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
        0: Header.SortDirection := sdAscending;
        1: Header.SortDirection := sdDescending;
        2: Header.SortDirection := sdAscending;
        3: Header.SortDirection := sdDescending;
      end;
    end else if Header.SortDirection = sdAscending then
      Header.SortDirection := sdDescending
    else
      Header.SortDirection := sdAscending;
    SortTree(Header.SortColumn, Header.SortDirection);
  end;
end;

procedure TChartsTree.DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition);
begin
  inherited;

  if Header.Columns[Column].Position = 0 then
    Header.Columns[Column].Position := FHeaderDragSourcePosition;
end;

function TChartsTree.DoHeaderDragging(Column: TColumnIndex): Boolean;
begin
  if Column = -1 then
    Exit(False);

  Result := inherited;

  FHeaderDragSourcePosition := Header.Columns[Column].Position;
end;

function TChartsTree.DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer;
var
  NodeData: PChartNodeData;
begin
  NodeData := GetNodeData(Node);
  if NodeData.Chart <> nil then
    Result := StrLIComp(PChar(Text), PChar(NodeData.Chart.Name), Min(Length(Text), Length(NodeData.Chart.Name)))
  else
    Result := StrLIComp(PChar(Text), PChar(NodeData.Stream.Stream.Name), Min(Length(Text), Length(NodeData.Stream.Stream.Name)));
end;

procedure TChartsTree.DoNodeDblClick(const HitInfo: THitInfo);
begin
  inherited;

  if hiOnItemButton in HitInfo.HitPositions then
    Exit;

  if (SelectedCount = 1) and (FocusedNode <> nil) then
    ExecDefaultAction;
end;

function TChartsTree.DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean;
begin
  Result := inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Result := True;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(R);
end;

procedure TChartsTree.DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(ItemRect);
end;

procedure TChartsTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  if Focused and Selected[Node] then
    Canvas.Font.Color := AppGlobals.NodeTextColorSelectedFocused
  else if Selected[Node] then
    Canvas.Font.Color := AppGlobals.NodeTextColorSelected
  else
    Canvas.Font.Color := AppGlobals.NodeTextColor;
end;

procedure TChartsTree.ExecDefaultAction;
var
  i: Integer;
  AllOnWishlist: Boolean;
  P: TControl;
  Nodes: TNodeArray;
  NodesData: TChartDataArray;
  Titles: TWishlistTitleInfoArray;
  Info: TStartStreamingInfoArray;
begin
  P := Parent;
  while not (P.ClassType = TChartsTab) do
    P := P.Parent;

  AllOnWishlist := True;
  Nodes := GetNodes(ntAll, True);
  NodesData := NodesToData(Nodes);

  SetLength(Titles, 0);
  SetLength(Info, 0);
  for i := 0 to Length(NodesData) - 1 do
    if NodesData[i].Chart <> nil then
    begin
      if not NodesData[i].IsOnWishlist then
        AllOnWishlist := False;

      SetLength(Titles, Length(Titles) + 1);
      Titles[High(Titles)] := TWishlistTitleInfo.Create(NodesData[i].Chart.ServerHash, NodesData[i].Chart.Name, False);
    end else if NodesData[i].Stream <> nil then
    begin
      SetLength(Info, Length(Info) + 1);
      Info[High(Info)] := TStartStreamingInfo.Create(NodesData[i].Stream.ID, NodesData[i].Stream.Stream.Bitrate, NodesData[i].Stream.Stream.Name, NodesData[i].Stream.Stream.URL,
        NodesData[i].Stream.Stream.URLs, NodesData[i].Stream.Stream.RegExes, NodesData[i].Stream.Stream.IgnoreTitles);
    end;

  case AppGlobals.DefaultActionBrowser of
    oaStart:
      TChartsTab(P).FOnAddStreams(Self, Info, oaStart);
    oaPlay:
      TChartsTab(P).FOnAddStreams(Self, Info, oaPlay);
    oaPlayExternal:
      TChartsTab(P).FOnAddStreams(Self, Info, oaPlayExternal);
    oaAdd:
      TChartsTab(P).FOnAddStreams(Self, Info, oaAdd)
  end;

  if Length(Titles) > 0 then
    if AllOnWishlist then
      TChartsTab(P).FOnRemoveTitleFromWishlist(Self, Titles)
    else
      TChartsTab(P).FOnAddToWishlist(Self, Titles);

  for i := 0 to Length(Nodes) - 1 do
    InvalidateNode(Nodes[i]);
end;

procedure TChartsTree.FitColumns;
var
  i: Integer;
begin
  if (Header.Columns.Count <> Length(AppGlobals.ChartHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.ChartHeaderPosition)) then
    raise Exception.Create('(Header.Columns.Count <> Length(AppGlobals.ChartHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.ChartHeaderPosition))');

  if AppGlobals.ChartHeaderWidthLoaded then
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Width := AppGlobals.ChartHeaderWidth[i]
  else
  begin
    FColLastPlayed.Width := TFunctions.GetTextSize(FColLastPlayed.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);
    FColChance.Width := TFunctions.GetTextSize(FColChance.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);
  end;

  FColImages.Width := Max(Margin * 2 + 3 * 16 + 2 * 2, TFunctions.GetTextSize(FColImages.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96));

  if AppGlobals.ChartHeaderPositionLoaded then
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.ChartHeaderPosition[i];
end;

procedure TChartsTree.FSetState(Value: TChartStates);
begin
  FDots := '';

  FTimer.Enabled := False;

  case Value of
    csNormal:
      FProgressBar.Visible := False;

    csSearching:
    begin
      // Marquee wieder zurücksetzen, dass es links anfängt...
      FProgressBar.Style := pbstNormal;
      FProgressBar.Style := pbstMarquee;

      FProgressBar.Visible := True;
      FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_SEARCHING) + '..') div 2;
      FTimer.Enabled := True;
      Invalidate;
    end;
    csSearchError:
    begin
      FProgressBar.Visible := False;
      FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_SEARCH_ERROR)) div 2;
      Invalidate;
    end;
  end;

  FState := Value;
end;

function TChartsTree.GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
var
  Node: PVirtualNode;
  NodeData: PChartNodeData;
begin
  SetLength(Result, 0);
  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);

    if SelectedOnly and (not Selected[Node]) then
    begin
      Node := GetNext(Node);
      Continue;
    end;

    if ((NodeTypes = ntChart) and (NodeData.Chart = nil)) or ((NodeTypes = ntStream) and (NodeData.Stream = nil)) then
    begin
      Node := GetNext(Node);
      Continue;
    end;

    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := Node;
    Node := GetNext(Node);
  end;
end;

function TChartsTree.NodesToData(Nodes: TNodeArray): TChartDataArray;
var
  i: Integer;
  Data: PChartNodeData;
begin
  SetLength(Result, Length(Nodes));
  for i := 0 to Length(Nodes) - 1 do
  begin
    Data := GetNodeData(Nodes[i]);
    Result[i] := Data;
  end;
end;

procedure TChartsTree.KeyPress(var Key: Char);
begin
  inherited;

  if (SelectedCount > 0) and ((Key = #13) or (Key = #32)) then
  begin
    ExecDefaultAction;
    Key := #0;
  end;
end;

procedure TChartsTree.MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
var
  Show: Boolean;
begin
  Show := True;
  if coVisible in Header.Columns[Index].Options then
    Show := False;

  if Show then
    Header.Columns[Index].Options := Header.Columns[Index].Options + [coVisible]
  else
    Header.Columns[Index].Options := Header.Columns[Index].Options - [coVisible];

  AppGlobals.ChartCols := AppGlobals.ChartCols xor (1 shl Index);
end;

procedure TChartsTree.MessageReceived(Msg: TMessageBase);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PChartNodeData;
  SongSavedMsg: TSongSavedMsg absolute Msg;
begin
  BeginUpdate;
  try
    if Msg is TListsChangedMsg then
    begin
      Node := GetFirst;
      while Node <> nil do
      begin
        NodeData := GetNodeData(Node);

        NodeData.IsOnWishlist := False;
        NodeData.IsArtistOnWishlist := False;
        for i := 0 to AppGlobals.Data.SaveList.Count - 1 do
        begin
          if not NodeData.IsOnWishlist then
            NodeData.IsOnWishlist := (NodeData.Chart <> nil) and (AppGlobals.Data.SaveList.Items[i].ServerHash > 0) and (NodeData.Chart.ServerHash = AppGlobals.Data.SaveList.Items[i].ServerHash);
          if not NodeData.IsArtistOnWishlist then
            NodeData.IsArtistOnWishlist := (NodeData.Chart <> nil) and (AppGlobals.Data.SaveList.Items[i].ServerArtistHash > 0) and (NodeData.Chart.ServerArtistHash = AppGlobals.Data.SaveList.Items[i].ServerArtistHash);
          if NodeData.IsOnWishlist and NodeData.IsArtistOnWishlist then
            Break;
        end;

        Node := GetNext(Node);
      end;

      Invalidate;
    end else if Msg is TSongSavedMsg then
    begin
      if not AppGlobals.Data.SavedTitleHashes.Contains(SongSavedMsg.ServerTitleHash) then
        AppGlobals.Data.SavedTitleHashes.Add(SongSavedMsg.ServerTitleHash);

      Node := GetFirst;
      while Node <> nil do
      begin
        NodeData := GetNodeData(Node);

        if (NodeData.Chart <> nil) and (NodeData.Chart.ServerHash = SongSavedMsg.ServerTitleHash) then
        begin
          InvalidateNode(Node);
          Break;
        end;

        Node := GetNext(Node);
      end;
    end;
  finally
    EndUpdate;
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
      csSearching:
      begin
        Msg := _(TEXT_SEARCHING) + FDots;
        Canvas.TextOut(FTextLeft, FProgressBar.Top - TFunctions.GetTextSize('Wyg', Font).cy - MulDiv(2, Screen.PixelsPerInch, 96), Msg);
      end;
      csSearchError:
      begin
        Msg := _(TEXT_SEARCH_ERROR);
        Canvas.TextOut(FTextLeft, ClientHeight div 2 - Canvas.TextHeight(Msg), Msg);
      end;
    end;
end;

procedure TChartsTree.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  i: Integer;
  NodeData: PChartNodeData;
  P: TControl;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  case PaintInfo.Column of
    0:
      if NodeData.Chart <> nil then
        Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.MUSIC)
      else
        Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.TRANSMIT);
    1:
      if NodeData.Chart <> nil then
      begin
        if NodeData.IsOnWishlist then
          Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.SCRIPT_BRICKS);
        if NodeData.IsArtistOnWishlist then
          Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos + 18, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.SCRIPT_USER_GRAY_COOL);

        for i := 0 to AppGlobals.Data.SavedTitleHashes.Count - 1 do
          if AppGlobals.Data.SavedTitleHashes[i] = NodeData.Chart.ServerHash then
            Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos + 36, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.DRIVE);
      end else
      begin
        P := Parent;
        while not (P.ClassType = TChartsTab) do
          P := P.Parent;
        if TChartsTab(P).FOnGetIsStreamOnListEvent(Self, NodeData.Stream.Stream) then
          Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.TRANSMIT_ADD);
      end;
  end;
end;

procedure TChartsTree.PopupMenuClick(Sender: TObject);
var
  i: Integer;
  Nodes: TChartDataArray;
  Titles: TWishlistTitleInfoArray;
  P: TControl;
  F: TfrmChartsTabAdjustTitleName;
  Info: TStartStreamingInfoArray;
begin
  P := Parent;
  while not (P.ClassType = TChartsTab) do
    P := P.Parent;

  Nodes := NodesToData(GetNodes(ntAll, True));

  SetLength(Titles, 0);
  SetLength(Info, 0);
  try
    for i := 0 to Length(Nodes) - 1 do
      if Nodes[i].Chart <> nil then
      begin
        if (Sender = FPopupMenu.ItemAddToWishlist) or (Sender = FPopupMenu.ItemRemoveFromWishlist) then
        begin
          SetLength(Titles, Length(Titles) + 1);
          Titles[High(Titles)] := TWishlistTitleInfo.Create(Nodes[i].Chart.ServerHash, Nodes[i].Chart.Name, False);
        end else if Sender = FPopupMenu.ItemAddArtistToWishlist then
        begin
          if Nodes[i].Chart.ServerArtistHash > 0 then
          begin
            SetLength(Titles, Length(Titles) + 1);
            Titles[High(Titles)] := TWishlistTitleInfo.Create(Nodes[i].Chart.ServerArtistHash, Nodes[i].Chart.Artist, True);
          end;
        end else if Sender = FPopupMenu.ItemEditAndAddToWishlist then
        begin
          F := TfrmChartsTabAdjustTitleName.Create(GetParentForm(Self), Nodes[i].Chart.Name);
          try
            F.ShowModal;

            if F.Okay then
            begin
              SetLength(Titles, Length(Titles) + 1);
              Titles[High(Titles)] := TWishlistTitleInfo.Create(0, F.TitleName, False);
            end;
          finally
            F.Free;
          end;
        end;
      end else
      begin
        SetLength(Info, Length(Info) + 1);
        Info[High(Info)] := TStartStreamingInfo.Create(Nodes[i].Stream.ID, Nodes[i].Stream.Stream.Bitrate, Nodes[i].Stream.Stream.Name, Nodes[i].Stream.Stream.URL, Nodes[i].Stream.Stream.URLs,
          Nodes[i].Stream.Stream.RegExes, Nodes[i].Stream.Stream.IgnoreTitles);
      end;

    if Sender = FPopupMenu.ItemStartStreaming then
      TChartsTab(P).FOnAddStreams(Self, Info, oaStart)
    else if Sender = FPopupMenu.ItemPlayStream then
      TChartsTab(P).FOnAddStreams(Self, Info, oaPlay)
    else if Sender = FPopupMenu.ItemPlayStreamExternal then
      TChartsTab(P).FOnAddStreams(Self, Info, oaPlayExternal)
    else if Sender = FPopupMenu.ItemAddStream then
      TChartsTab(P).FOnAddStreams(Self, Info, oaAdd);

    if Length(Titles) > 0 then
      if Sender = FPopupMenu.ItemRemoveFromWishlist then
        TChartsTab(P).FOnRemoveTitleFromWishlist(Self, Titles)
      else
        TChartsTab(P).FOnAddToWishlist(Self, Titles);
  finally

  end;

  TChartsTab(P).UpdateButtons;

  Invalidate;
end;

procedure TChartsTree.Resize;
begin
  inherited;

  if not Assigned(FProgressBar) then
    Exit;

  FProgressBar.Left := Trunc(ClientWidth / 2 - FProgressBar.Width / 2);
  FProgressBar.Top := ClientHeight div 2 - Canvas.TextHeight('Wy') + 15;

  case FState of
    csSearching:
      FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_SEARCHING) + '..') div 2;
    csSearchError:
      FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_SEARCH_ERROR)) div 2;
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

procedure TSearchPanel.CreateHandle;
begin
  inherited CreateHandle;

  FSearch.ItemIndex := 0;
end;

constructor TSearchPanel.Create(AOwner: TComponent);
var
  Sep: TToolButton;
begin
  inherited;

  BevelOuter := bvNone;
  AutoSize := True;

  FLabel := TLabel.Create(Self);
  FLabel.Align := alLeft;
  FLabel.Layout := tlCenter;
  FLabel.Parent := Self;
  FLabel.Caption := 'Search:';
  FLabel.Left := -100;

  FSearch := TComboBoxExEditable.Create(Self);
  FSearch.Align := alLeft;
  FSearch.Parent := Self;
  FSearch.Images := modSharedData.imgImages;
  FSearch.Width := 200;
  FSearch.DropDownCount := 16;

  FSearch.ItemsEx.AddItem(_(SEARCH_TOP), 0);

  FToolbar := TToolbarForcedHorizontal.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align := alRight;
  FToolbar.Images := modSharedData.imgImages;

  FButtonAddToWishlist := TToolButton.Create(FToolbar);
  FButtonAddToWishlist.Parent := FToolbar;
  FButtonAddToWishlist.Hint := 'Add title to automatic wishlist';
  FButtonAddToWishlist.ImageIndex := TImages.SCRIPT_BRICKS_ADD;

  FButtonRemoveFromWishlist := TToolButton.Create(FToolbar);
  FButtonRemoveFromWishlist.Parent := FToolbar;
  FButtonRemoveFromWishlist.Hint := 'Remove title from automatic wishlist';
  FButtonRemoveFromWishlist.ImageIndex := TImages.SCRIPT_BRICKS_DELETE;

  FButtonAddArtistToWishlist := TToolButton.Create(FToolbar);
  FButtonAddArtistToWishlist.Parent := FToolbar;
  FButtonAddArtistToWishlist.Hint := 'Add artist to automatic wishlist';
  FButtonAddArtistToWishlist.ImageIndex := TImages.SCRIPT_USER_GRAY_COOL_ADD;

  Sep := TToolButton.Create(FToolbar);
  Sep.Parent := FToolbar;
  Sep.Style := tbsSeparator;

  FButtonEditAndAddToWishlist := TToolButton.Create(FToolbar);
  FButtonEditAndAddToWishlist.Parent := FToolbar;
  FButtonEditAndAddToWishlist.Hint := 'Edit and add to manual wishlist';
  FButtonEditAndAddToWishlist.ImageIndex := TImages.SCRIPT_HEART_ADD;

  Sep := TToolButton.Create(FToolbar);
  Sep.Parent := FToolbar;
  Sep.Style := tbsSeparator;

  FButtonStartStreaming := TToolButton.Create(FToolbar);
  FButtonStartStreaming.Parent := FToolbar;
  FButtonStartStreaming.Hint := 'Start recording';
  FButtonStartStreaming.ImageIndex := TImages.RECORD_RED;

  FButtonPlayStream := TToolButton.Create(FToolbar);
  FButtonPlayStream.Parent := FToolbar;
  FButtonPlayStream.Hint := 'Play stream';
  FButtonPlayStream.ImageIndex := TImages.PLAY_BLUE;

  FButtonPlayStreamExternal := TToolButton.Create(FToolbar);
  FButtonPlayStreamExternal.Parent := FToolbar;
  FButtonPlayStreamExternal.Hint := 'Play stream (external player)';
  FButtonPlayStreamExternal.ImageIndex := TImages.PLAY_GO;

  FButtonAddStream := TToolButton.Create(FToolbar);
  FButtonAddStream.Parent := FToolbar;
  FButtonAddStream.Hint := 'Add stream';
  FButtonAddStream.ImageIndex := TImages.TRANSMIT_ADD;

  PostTranslate;
end;

procedure TSearchPanel.PostTranslate;
begin
  FSearch.ItemsEx[0].Caption := _(SEARCH_TOP);
end;

procedure TSearchPanel.InsertSearchItem(Search: string);
var
  i: Integer;
  ComboItem: TComboExItem;
begin
  Search := Search.Trim;

  ComboItem := FSearch.ItemsEx.Insert(1);
  ComboItem.Caption := Search;
  ComboItem.ImageIndex := 1;

  for i := FSearch.ItemsEx.Count - 1 downto 2 do
    if LowerCase(FSearch.ItemsEx[i].Caption) = LowerCase(Search) then
      FSearch.ItemsEx.Delete(i);

  while FSearch.ItemsEx.Count > 9 do
    FSearch.ItemsEx.Delete(FSearch.ItemsEx.Count - 1);

  FSearch.ItemIndex := 1;
end;

end.
