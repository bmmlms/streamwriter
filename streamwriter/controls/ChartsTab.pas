{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2014 Alexander Nottelmann

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
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, LanguageObjects, Tabs, Functions, AppData, Logging, VirtualTrees,
  HomeCommunication, DataManager, ImgList, Graphics, Math, Generics.Collections,
  Menus, ChartsTabAdjustTitleName, Forms, TypeDefs, MessageBus, AppMessages,
  HomeCommands, Commands, GUIFunctions, SharedData, PerlRegEx, Messages,
  DateUtils;

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

  TChartsPopup = class(TPopupMenu)
  private
    FItemAddToWishlist: TMenuItem;
    FItemAddArtistToWishlist: TMenuItem;
    FItemEditAndAddToWishlist: TMenuItem;
    FItemStartStreaming: TMenuItem;
    FItemPlayStream: TMenuItem;
    FItemPlayStreamExternal: TMenuItem;
    FItemAddStream: TMenuItem;
  protected

  public
    constructor Create(AOwner: TComponent); override;

    property ItemAddToWishlist: TMenuItem read FItemAddToWishlist;
    property ItemAddArtistToWishlist: TMenuItem read FItemAddArtistToWishlist;
    property ItemEditAndAddToWishlist: TMenuItem read FItemEditAndAddToWishlist;
    property ItemStartStreaming: TMenuItem read FItemStartStreaming;
    property ItemPlayStream: TMenuItem read FItemPlayStream;
    property ItemPlayStreamExternal: TMenuItem read FItemPlayStreamExternal;
    property ItemAddStream: TMenuItem read FItemAddStream;
  end;

  TMyComboBox = class(TComboBox)
  private
    FShouldSelect: Boolean;
  protected
    function MouseActivate(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer; HitTest: Integer): TMouseActivate; override;
    procedure WndProc(var Message: TMessage); override;
  public
  published
  end;

  TSearchPanel = class(TPanel)
  private
    FLabel: TLabel;
    FSearch: TMyComboBox;
    FToolbar: TToolBar;

    FButtonAddToWishlist: TToolButton;
    FButtonAddArtistToWishlist: TToolButton;
    FButtonEditAndAddToWishlist: TToolButton;
    FButtonStartStreaming: TToolButton;
    FButtonPlayStream: TToolButton;
    FButtonPlayStreamExternal: TToolButton;
    FButtonAddStream: TToolButton;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    procedure AfterCreate;

    procedure RebuildSearchItems(NewEntry: string);

    procedure PostTranslate;
  end;

  TChartArray = array of TChartEntry;
  TChartStates = (csNormal, csSearching, csSearchError);

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

    FState: TChartStates;

    FLists: TDataLists;

    procedure MessageReceived(Msg: TMessageBase);

    procedure PopupMenuClick(Sender: TObject);

    procedure TimerOnTimer(Sender: TObject);

    procedure FSetState(Value: TChartStates);

    procedure ExecDefaultAction;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean;
      var Index: Integer): TCustomImageList; override;
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode;
      Column: TColumnIndex): Integer; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    procedure DblClick; override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
    procedure Resize; override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect); override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo;
      ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists); reintroduce;
    destructor Destroy; override;
    procedure AfterCreate;

    function GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
    function NodesToData(Nodes: TNodeArray): TChartDataArray;

    property State: TChartStates read FState write FSetState;
  end;

  TAddToWishlistEvent = procedure(Sender: TObject; Arr: TWishlistTitleInfoArray) of object;
  TAddStreamsEvent = procedure(Sender: TObject; Info: TStartStreamingInfoArray; Action: TStreamOpenActions) of object;
  TGetIsStreamOnListEvent = function(Sender: TObject; Stream: TStreamBrowserEntry): Boolean of object;

  TChartsTab = class(TMainTabSheet)
  private
    FLists: TDataLists;
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
  protected
    procedure DoEnter; override;

  public
    constructor Create(AOwner: TComponent; Lists: TDataLists); reintroduce;
    destructor Destroy; override;
    procedure AfterCreate; override;

    procedure PostTranslate;
    procedure SetState(State: TChartStates);
    procedure SearchCharts(Top, ShowMessages: Boolean);

    procedure HomeCommStateChanged(Sender: TObject);

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

procedure TChartsTab.AfterCreate;
begin
  inherited;

  FChartsTree.AfterCreate;
  FSearchPanel.AfterCreate;

  FChartsTree.Images := modSharedData.imgImages;

  if Screen.PixelsPerInch = 96 then
    FChartsTree.PopupMenu.Images := modSharedData.imgImages;

  FSearchPanel.FButtonAddToWishlist.OnClick := ButtonClick;
  FSearchPanel.FButtonAddArtistToWishlist.OnClick := ButtonClick;
  FSearchPanel.FButtonEditAndAddToWishlist.OnClick := ButtonClick;
  FSearchPanel.FButtonStartStreaming.OnClick := ButtonClick;
  FSearchPanel.FButtonPlayStream.OnClick := ButtonClick;
  FSearchPanel.FButtonPlayStreamExternal.OnClick := ButtonClick;
  FSearchPanel.FButtonAddStream.OnClick := ButtonClick;

  Caption := 'Title search';

  UpdateButtons;
end;

procedure TChartsTab.ButtonClick(Sender: TObject);
begin
  if Sender = FSearchPanel.FButtonAddToWishlist then
    FChartsTree.FPopupMenu.FItemAddToWishlist.Click
  else if Sender = FSearchPanel.FButtonAddArtistToWishlist then
    FChartsTree.FPopupMenu.FItemAddArtistToWishlist.Click
  else if Sender = FSearchPanel.FButtonEditAndAddToWishlist then
    FChartsTree.FPopupMenu.FItemEditAndAddToWishlist.Click
  else if Sender = FSearchPanel.FButtonStartStreaming then
    FChartsTree.FPopupMenu.FItemStartStreaming.Click
  else if Sender = FSearchPanel.FButtonPlayStream then
    FChartsTree.FPopupMenu.FItemPlayStream.Click
  else if Sender = FSearchPanel.FButtonPlayStreamExternal then
    FChartsTree.FPopupMenu.FItemPlayStreamExternal.Click
  else if Sender = FSearchPanel.FButtonAddStream then
    FChartsTree.FPopupMenu.FItemAddStream.Click;
end;

procedure TChartsTab.ChartsTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateButtons;
end;

constructor TChartsTab.Create(AOwner: TComponent; Lists: TDataLists);
begin
  inherited Create(AOwner);

  FLists := Lists;

  FSearchPanel := TSearchPanel.Create(Self);
  FSearchPanel.Parent := Self;
  FSearchPanel.Align := alTop;
  FSearchPanel.Padding.Top := 1;

  FChartsTree := TChartsTree.Create(Self, FLists);
  FChartsTree.Parent := Self;
  FChartsTree.Align := alClient;
  FChartsTree.OnChange := ChartsTreeChange;

  FResultLabel := TLabel.Create(Self);
  FResultLabel.Parent := Self;
  FResultLabel.Align := alBottom;
  FResultLabel.Caption := Format(_(TEXT_RESULTS), [0]);

  HomeComm.OnSearchChartsReceived := HomeCommSearchChartsReceived;

  ImageIndex := 89;
  ShowCloseButton := False;

  FSearchPanel.FSearch.OnKeyPress := SearchKeyPress;
  FSearchPanel.FSearch.OnSelect := SearchSelect;
end;

destructor TChartsTab.Destroy;
var
  i: Integer;
begin
  if FCharts <> nil then
  begin
    for i := 0 to FCharts.Count - 1 do
    begin
      FCharts[i].Free;
    end;
    FCharts.Free;
  end;

  inherited;
end;

procedure TChartsTab.DoEnter;
begin
  inherited;

  if FSearchPanel.Enabled and FSearchPanel.FSearch.Enabled then
    FSearchPanel.FSearch.SetFocus;
end;

procedure TChartsTab.HomeCommSearchChartsReceived(Sender: TObject;
  Success: Boolean; Charts: TChartList);
var
  i: Integer;
begin
  FSearched := True;

  if FCharts <> nil then
  begin
    for i := 0 to FCharts.Count - 1 do
    begin
      FCharts[i].Free;
    end;
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
  if FSearchPanel.FSearch.CanFocus then
    FSearchPanel.FSearch.SetFocus;
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
  if not HomeComm.Connected then
  begin
    if ShowMessages then
      MsgBox(GetParentForm(Self).Handle, _('streamWriter needs to be connected to the server in order to search.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if Top then
  begin
    HomeComm.SendSearchCharts(True, '');
    SetState(csSearching);
  end else
  begin
    Abort := False;

    Tmp := Trim(FSearchPanel.FSearch.Text);

    {
    if (Pos('+', Tmp) > 0) or (Pos('-', Tmp) > 0) or (Pos('*', Tmp) > 0) or (Pos('(', Tmp) > 0) or (Pos(')', Tmp) > 0) or
       (Pos('<', Tmp) > 0) or (Pos('>', Tmp) > 0) or (Pos('~', Tmp) > 0) or (Pos('''', Tmp) > 0) then
    begin
      Abort := True;
    end;
    }

    if (Pos('"', Tmp) > 0) and (OccurenceCount('"', Tmp) mod 2 <> 0) then
    begin
      MsgBox(GetParentForm(Self).Handle, _('When using quotes every opening quote needs a closing quote.'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if not Abort then
    begin
      SL := TStringList.Create;
      try
        Explode(' ', Tmp, SL);

        if SL.Count = 0 then
          Abort := True;
      finally
        SL.Free;
      end;
    end;

    if Abort then
    begin
      MsgBox(GetParentForm(Self).Handle, _('You need to specify at least one word to search for. Special chars (+-*()<>~'') are not allowed.'), _('Info'), MB_ICONINFORMATION);
    end else
    begin
      FSearchPanel.RebuildSearchItems(Tmp);

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
  if FSearchPanel.FSearch.Items.Objects[FSearchPanel.FSearch.ItemIndex] <> nil then
    SearchCharts(True, True)
  else
  begin
    FSearchPanel.RebuildSearchItems(FSearchPanel.FSearch.Text);
    SearchCharts(False, True);
  end;
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
    begin
      FResultLabel.Caption := Format(_(TEXT_RESULTS), [0]);
    end;

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

        NodeData.Chart.LoadStreams(FLists.BrowserList);

        for n := 0 to NodeData.Chart.Streams.Count - 1 do
        begin
          NodeStream := FChartsTree.AddChild(Node);
          NodeDataStream := FChartsTree.GetNodeData(NodeStream);
          NodeDataStream.Stream := NodeData.Chart.Streams[n];
        end;

        for n := 0 to FLists.SaveList.Count - 1 do
        begin
          if (FLists.SaveList[n].ServerHash > 0) and (FLists.SaveList[n].ServerHash = NodeData.Chart.ServerHash) then
            NodeData.IsOnWishlist := True;
          if (FLists.SaveList[n].ServerArtistHash > 0) and (FLists.SaveList[n].ServerArtistHash = NodeData.Chart.ServerArtistHash) then
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
  AllOnList, AllArtistsOnList: Boolean;
  OneSelectedChart, ManySelectedCharts: Boolean;
  OneSelectedStream, ManySelectedStreams: Boolean;
  AtLeastOneArtistSelected: Boolean;
  N: PVirtualNode;
  NodeData: PChartNodeData;
begin
  inherited;

  AllOnList := True;
  AllArtistsOnList := True;
  OneSelectedChart := False;
  ManySelectedCharts := False;
  OneSelectedStream := False;
  ManySelectedStreams := False;
  AtLeastOneArtistSelected := False;

  N := FChartsTree.GetFirst;
  while N <> nil do
  begin
    if FChartsTree.Selected[N] then
    begin
      NodeData := FChartsTree.GetNodeData(N);
      if not NodeData.IsOnWishlist then
        AllOnList := False;
      if not NodeData.IsArtistOnWishlist then
        AllArtistsOnList := False;

      if NodeData.Chart <> nil then
      begin
        if OneSelectedChart then
        begin
          ManySelectedCharts := True;
          OneSelectedChart := False;
        end else if (not OneSelectedChart) and (not ManySelectedCharts) then
          OneSelectedChart := True;

        if NodeData.Chart.ServerArtistHash > 0 then
          AtLeastOneArtistSelected := True;
      end;

      if NodeData.Stream <> nil then
      begin
        if OneSelectedStream then
        begin
          ManySelectedStreams := True;
          OneSelectedStream := False;
        end else if (not OneSelectedStream) and (not ManySelectedStreams) then
          OneSelectedStream := True;
      end;
    end;
    N := FChartsTree.GetNext(N);
  end;

  FChartsTree.FPopupMenu.FItemAddToWishlist.Enabled := (not AllOnList) and (OneSelectedChart or ManySelectedCharts) and (State = csNormal);
  FChartsTree.FPopupMenu.FItemAddArtistToWishlist.Enabled := (not AllArtistsOnList) and (AtLeastOneArtistSelected or ManySelectedCharts) and (State = csNormal);
  FChartsTree.FPopupMenu.FItemEditAndAddToWishlist.Enabled := (OneSelectedChart) and (State = csNormal);
  FChartsTree.FPopupMenu.FItemStartStreaming.Enabled := (OneSelectedStream or ManySelectedStreams) and (State = csNormal);
  FChartsTree.FPopupMenu.FItemPlayStream.Enabled := (OneSelectedStream) and (State = csNormal);
  FChartsTree.FPopupMenu.FItemPlayStreamExternal.Enabled := (OneSelectedStream) and (State = csNormal);
  FChartsTree.FPopupMenu.FItemAddStream.Enabled := (OneSelectedStream or ManySelectedStreams) and (State = csNormal);

  FSearchPanel.FButtonAddToWishlist.Enabled := FChartsTree.FPopupMenu.FItemAddToWishlist.Enabled;
  FSearchPanel.FButtonAddArtistToWishlist.Enabled := (not AllArtistsOnList) and (AtLeastOneArtistSelected or ManySelectedCharts) and (State = csNormal);
  FSearchPanel.FButtonEditAndAddToWishlist.Enabled := FChartsTree.FPopupMenu.FItemEditAndAddToWishlist.Enabled;
  FSearchPanel.FButtonStartStreaming.Enabled := FChartsTree.FPopupMenu.FItemStartStreaming.Enabled;
  FSearchPanel.FButtonPlayStream.Enabled := FChartsTree.FPopupMenu.FItemPlayStream.Enabled;
  FSearchPanel.FButtonPlayStreamExternal.Enabled := FChartsTree.FPopupMenu.FItemPlayStreamExternal.Enabled;
  FSearchPanel.FButtonAddStream.Enabled := FChartsTree.FPopupMenu.FItemAddStream.Enabled;
end;

{ TChartsTree }

procedure TChartsTree.AfterCreate;
begin
  FColImages.Width := GetTextSize(FColImages.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);
  FColLastPlayed.Width := GetTextSize(FColLastPlayed.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);
  FColChance.Width := GetTextSize(FColChance.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);
end;

constructor TChartsTree.Create(AOwner: TComponent; Lists: TDataLists);
begin
  inherited Create(AOwner);

  MsgBus.AddSubscriber(MessageReceived);

  FLists := Lists;

  FState := csNormal;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerOnTimer;

  NodeDataSize := SizeOf(TChartNodeData);

  IncrementalSearch := isVisibleOnly;

  Header.Height := GetTextSize('Wyg', Font).cy + 5;
  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  Header.Options := [hoColumnResize, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleOnDblClick];
  Header.Options := Header.Options - [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];

  Header.AutoSizeIndex := 0;

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Name');

  FColImages := Header.Columns.Add;
  FColImages.Text := _('State');

  FColLastPlayed := Header.Columns.Add;
  FColLastPlayed.Text := _('Last played');

  FColChance := Header.Columns.Add;
  FColChance.Text := _('Played last day/week');
  FColChance.Alignment := taRightJustify;

  Header.Options := Header.Options + [hoAutoResize];

  FPopupMenu := TChartsPopup.Create(Self);
  FPopupMenu.ItemAddToWishlist.OnClick := PopupMenuClick;
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
end;

procedure TChartsTree.DblClick;
begin
  inherited;

  if (SelectedCount = 1) and (FocusedNode <> nil) then
    ExecDefaultAction;
end;

destructor TChartsTree.Destroy;
begin

  inherited;
end;

procedure TChartsTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
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

    Canvas.Brush.Color := HTML2Color('#005fb0');
    if Selected[Node] and Focused then
      Canvas.Brush.Color := HTML2Color('#d2d2d2');

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

function TChartsTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
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

          for i := 0 to FLists.SavedTitleHashes.Count - 1 do
          begin
            if (Data1.Chart <> nil) and (Data1.Chart.ServerHash = FLists.SavedTitleHashes[i]) then
              C1 := C1 + 1;
            if (Data2.Chart <> nil) and (Data2.Chart.ServerHash = FLists.SavedTitleHashes[i]) then
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

          Result := CmpInt(C1, C2);
          if Result = 0 then
          begin
            Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
            if Header.SortDirection = sdDescending then
              Result := Result * -1;
          end;
        end;
      2:
        Result := CmpInt(Data1.Chart.PlayedLast, Data2.Chart.PlayedLast);
      3:
        begin
          Result := CmpInt(Data1.Chart.PlayedLastWeek, Data2.Chart.PlayedLastWeek);
          if Result = 0 then
          begin
            Result := CompareText(Data1.Chart.Name, Data2.Chart.Name);
            if Header.SortDirection = sdDescending then
              Result := Result * -1;
          end;
        end;
    end;
  end else if (Data1.Stream <> nil) and (Data2.Stream <> nil) then
  begin
    case Column of
      0, 1:
        begin
          Result := CompareText(Data1.Stream.Stream.Name, Data2.Stream.Stream.Name);
          if (Header.SortDirection = sdDescending) then
            Result := Result * -1;
        end;
      2:
        begin
          Result := CmpInt(Data1.Stream.PlayedLast, Data2.Stream.PlayedLast);
          if (Header.SortDirection = sdAscending) then
            Result := Result * -1;
        end;
      3:
        begin
          Result := CmpInt(Data1.Stream.PlayedLastWeek, Data2.Stream.PlayedLastWeek);
          if (Header.SortDirection = sdAscending) then
            Result := Result * -1;
        end;
    end;
  end;
end;

function TChartsTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  // Wir müssen irgendeinen Index setzen, damit PaintImage() getriggert wird
  if ((Column = 0) or (Column = 1)) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    Index := 0;
end;

procedure TChartsTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
var
  Val: Int64;
  NodeData, ParentNodeData: PChartNodeData;
begin
  inherited;

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
        begin
          if Node.PrevSibling = nil then
          begin
            // REMARK: Das hier ist ein Hack. Die Berechnung hier drunter ergibt nicht das,
            //         was im PlayedLast des Charts steht.. sieht doof aus. Darum das hier!
            ParentNodeData := GetNodeData(Node.Parent);
            Val := ParentNodeData.Chart.PlayedLast;
          end else
            Val := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)) - NodeData.Stream.PlayedLast + HomeComm.ServerTimeDiff;
        end;

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
        end else
        begin
          if Val = 1 then
            Text := Format(_('%d second ago'), [Val])
          else
            Text := Format(_('%d seconds ago'), [Val]);
        end;
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
  NodeData: PChartNodeData;
begin
  NodeData := GetNodeData(Node);
  if NodeData.Chart <> nil then
    Result := StrLIComp(PChar(Text), PChar(NodeData.Chart.Name), Min(Length(Text), Length(NodeData.Chart.Name)))
  else
    Result := StrLIComp(PChar(Text), PChar(NodeData.Stream.Stream.Name), Min(Length(Text), Length(NodeData.Stream.Stream.Name)))
end;

procedure TChartsTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;

  NodeHeight := GetTextSize('Wyg', Font).cy + 5;
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
  try
    for i := 0 to Length(NodesData) - 1 do
    begin
      if NodesData[i].Chart <> nil then
      begin
        if not NodesData[i].IsOnWishlist then
          AllOnWishlist := False;

        SetLength(Titles, Length(Titles) + 1);
        Titles[High(Titles)] := TWishlistTitleInfo.Create(NodesData[i].Chart.ServerHash, NodesData[i].Chart.Name, False);
      end else if NodesData[i].Stream <> nil then
      begin
        SetLength(Info, Length(Info) + 1);
        Info[High(Info)] := TStartStreamingInfo.Create(NodesData[i].Stream.ID, NodesData[i].Stream.Stream.Bitrate,
          NodesData[i].Stream.Stream.Name, NodesData[i].Stream.Stream.URL, NodesData[i].Stream.Stream.RegExes,
          NodesData[i].Stream.Stream.IgnoreTitles);
      end;
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
  finally

  end;
end;

procedure TChartsTree.FSetState(Value: TChartStates);
begin
  FDots := '';

  FTimer.Enabled := False;

  case Value of
    csNormal:
      begin
        FProgressBar.Visible := False;
        //Enabled := True;
      end;
    csSearching:
      begin
        // Marquee wieder zurücksetzen, dass es links anfängt...
        FProgressBar.Style := pbstNormal;
        FProgressBar.Style := pbstMarquee;

        FProgressBar.Visible := True;
        //Enabled := False;
        FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_SEARCHING) + '..') div 2;
        FTimer.Enabled := True;
        Invalidate;
      end;
    csSearchError:
      begin
        FProgressBar.Visible := False;
        //Enabled := False;
        FTextLeft := ClientWidth div 2 - Canvas.TextWidth(_(TEXT_SEARCH_ERROR)) div 2;
        Invalidate;
      end;
  end;

  FState := Value;
end;

function TChartsTree.GetNodes(NodeTypes: TNodeTypes;
  SelectedOnly: Boolean): TNodeArray;
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

    if ((NodeTypes = ntChart) and (NodeData.Chart = nil)) or
       ((NodeTypes = ntStream) and (NodeData.Stream = nil)) then
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
        for i := 0 to FLists.SaveList.Count - 1 do
        begin
          if not NodeData.IsOnWishlist then
            NodeData.IsOnWishlist := (NodeData.Chart <> nil) and (FLists.SaveList.Items[i].ServerHash > 0) and (NodeData.Chart.ServerHash = FLists.SaveList.Items[i].ServerHash);
          if not NodeData.IsArtistOnWishlist then
            NodeData.IsArtistOnWishlist := (NodeData.Chart <> nil) and (FLists.SaveList.Items[i].ServerArtistHash > 0) and (NodeData.Chart.ServerArtistHash = FLists.SaveList.Items[i].ServerArtistHash);
          if NodeData.IsOnWishlist and NodeData.IsArtistOnWishlist then
            Break;
        end;

        Node := GetNext(Node);
      end;

      Invalidate;
    end else if Msg is TSongSavedMsg then
    begin
      if not FLists.SavedTitleHashes.Contains(SongSavedMsg.ServerTitleHash) then
        FLists.SavedTitleHashes.Add(SongSavedMsg.ServerTitleHash);

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
          Canvas.TextOut(FTextLeft, FProgressBar.Top - GetTextSize('Wyg', Font).cy - MulDiv(2, Screen.PixelsPerInch, 96), Msg);
        end;
      csSearchError:
        begin
          Msg := _(TEXT_SEARCH_ERROR);
          Canvas.TextOut(FTextLeft, ClientHeight div 2 - Canvas.TextHeight(Msg), Msg);
        end;
    end;
end;

procedure TChartsTree.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  i: Integer;
  NodeData: PChartNodeData;
  P: TControl;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  case PaintInfo.Column of
    0:
      begin
        if NodeData.Chart <> nil then
          Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 20)
        else
          Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 68);
      end;
    1:
      begin
        if NodeData.Chart <> nil then
        begin
          if NodeData.IsOnWishlist then
          begin
            Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 77);
          end;
          if NodeData.IsArtistOnWishlist then
          begin
            Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos + 16, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 86);
          end;

          for i := 0 to FLists.SavedTitleHashes.Count - 1 do
          begin
            if FLists.SavedTitleHashes[i] = NodeData.Chart.ServerHash then
            begin
              Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos + 32, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 14);
            end;
          end;
        end else
        begin
          P := Parent;
          while not (P.ClassType = TChartsTab) do
            P := P.Parent;
          if TChartsTab(P).FOnGetIsStreamOnListEvent(Self, NodeData.Stream.Stream) then
            Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 80);
        end;
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
    begin
      if Nodes[i].Chart <> nil then
      begin
        if Sender = FPopupMenu.ItemAddToWishlist then
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
        Info[High(Info)] := TStartStreamingInfo.Create(Nodes[i].Stream.ID, Nodes[i].Stream.Stream.Bitrate,
          Nodes[i].Stream.Stream.Name, Nodes[i].Stream.Stream.URL, Nodes[i].Stream.Stream.RegExes,
          Nodes[i].Stream.Stream.IgnoreTitles);
      end;
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
      TChartsTab(P).FOnAddToWishlist(Self, Titles)
  finally

  end;

  TChartsTab(P).UpdateButtons;

  Invalidate;
end;

procedure TChartsTree.Resize;
begin
  inherited;

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

constructor TSearchPanel.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := _('Search:');

  FSearch := TMyComboBox.Create(Self);
  FSearch.Parent := Self;
  FSearch.AutoComplete := False;

  FToolbar := TToolBar.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.ShowHint := True;
end;

procedure TSearchPanel.PostTranslate;
begin
  FLabel.Caption := _('Search:');
  FSearch.Left := FLabel.Left + FLabel.Width + 6;

  RebuildSearchItems('');
end;

procedure TSearchPanel.RebuildSearchItems(NewEntry: string);
var
  SL: TStringList;
  i, OldIndex: Integer;
begin
  OldIndex := FSearch.ItemIndex;
  SL := TStringList.Create;
  try
    SL.Assign(FSearch.Items);

    for i := SL.Count - 1 downto 0 do
      if (AnsiLowerCase(SL[i]) = AnsiLowerCase(NewEntry)) or (SL.Objects[i] <> nil) then
        SL.Delete(i);
    while SL.Count > 9 do
      SL.Delete(SL.Count - 1);

    FSearch.Items.Clear;
    FSearch.AddItem(_(SEARCH_TOP), FSearch);
    if Trim(NewEntry) <> '' then
      FSearch.Items.Add(NewEntry);

    for i := 0 to SL.Count - 1 do
      FSearch.Items.Add(SL[i]);

    if NewEntry <> '' then
    begin
      for i := 0 to FSearch.Items.Count - 1 do
        if FSearch.Items[i] = NewEntry then
        begin
          FSearch.ItemIndex := i;
          Break;
        end;
    end else
      if (OldIndex = -1) and (FSearch.Items.Count > 0) then
        FSearch.ItemIndex := 0
      else
        FSearch.ItemIndex := OldIndex;
  finally
    SL.Free;
  end;
end;

procedure TSearchPanel.Resize;
begin
  inherited;

end;

procedure TSearchPanel.AfterCreate;
var
  Sep: TToolButton;
begin
  FToolbar.Images := modSharedData.imgImages;

  FButtonAddStream := TToolButton.Create(FToolbar);
  FButtonAddStream.Parent := FToolbar;
  FButtonAddStream.Hint := _('Add stream');
  FButtonAddStream.ImageIndex := 80;

  FButtonPlayStreamExternal := TToolButton.Create(FToolbar);
  FButtonPlayStreamExternal.Parent := FToolbar;
  FButtonPlayStreamExternal.Hint := _('Play stream (external player)');
  FButtonPlayStreamExternal.ImageIndex := 82;

  FButtonPlayStream := TToolButton.Create(FToolbar);
  FButtonPlayStream.Parent := FToolbar;
  FButtonPlayStream.Hint := _('Play stream');
  FButtonPlayStream.ImageIndex := 33;

  FButtonStartStreaming := TToolButton.Create(FToolbar);
  FButtonStartStreaming.Parent := FToolbar;
  FButtonStartStreaming.Hint := _('Start recording');
  FButtonStartStreaming.ImageIndex := 0;

  Sep := TToolButton.Create(FToolbar);
  Sep.Parent := FToolbar;
  Sep.Style := tbsSeparator;
  Sep.Width := 8;

  FButtonEditAndAddToWishlist := TToolButton.Create(FToolbar);
  FButtonEditAndAddToWishlist.Parent := FToolbar;
  FButtonEditAndAddToWishlist.Hint := _('Edit and add to manual wishlist');
  FButtonEditAndAddToWishlist.ImageIndex := 30;

  Sep := TToolButton.Create(FToolbar);
  Sep.Parent := FToolbar;
  Sep.Style := tbsSeparator;
  Sep.Width := 8;

  FButtonAddArtistToWishlist := TToolButton.Create(FToolbar);
  FButtonAddArtistToWishlist.Parent := FToolbar;
  FButtonAddArtistToWishlist.Hint := _('Add artist to automatic wishlist');
  FButtonAddArtistToWishlist.ImageIndex := 86;

  FButtonAddToWishlist := TToolButton.Create(FToolbar);
  FButtonAddToWishlist.Parent := FToolbar;
  FButtonAddToWishlist.Hint := _('Add title to automatic wishlist');
  FButtonAddToWishlist.ImageIndex := 77;

  //FToolbar.Padding.Top := 6;
  FToolbar.Align := alRight;
  FToolbar.AutoSize := True;

  RebuildSearchItems('');

  PostTranslate;

  FLabel.Left := 0;
  FSearch.Width := 200;
  FSearch.Top := 1;

  FLabel.Top := (FSearch.Top + FSearch.Height div 2 - FLabel.Height div 2);

  ClientHeight := FSearch.Top * 2 + FSearch.Height + MulDiv(3, Screen.PixelsPerInch, 96);
end;

{ TChartsPopup }

constructor TChartsPopup.Create(AOwner: TComponent);
var
  Sep: TMenuItem;
begin
  inherited;

  Sep := CreateMenuItem;
  Sep.Caption := '-';
  Items.Add(Sep);

  FItemAddToWishlist := CreateMenuItem;
  FItemAddToWishlist.Caption := '&Add title to automatic wishlist';
  FItemAddToWishlist.ImageIndex := 77;
  Items.Add(FItemAddToWishlist);

  FItemAddArtistToWishlist := CreateMenuItem;
  FItemAddArtistToWishlist.Caption := '&Add artist to automatic wishlist';
  FItemAddArtistToWishlist.ImageIndex := 86;
  Items.Add(FItemAddArtistToWishlist);

  Sep := CreateMenuItem;
  Sep.Caption := '-';
  Items.Add(Sep);

  FItemEditAndAddToWishlist := CreateMenuItem;
  FItemEditAndAddToWishlist.Caption := '&Edit and add to manual wishlist';
  FItemEditAndAddToWishlist.ImageIndex := 30;
  Items.Add(FItemEditAndAddToWishlist);

  Sep := CreateMenuItem;
  Sep.Caption := '-';
  Items.Add(Sep);

  FItemStartStreaming := CreateMenuItem;
  FItemStartStreaming.Caption := '&Start recording';
  FItemStartStreaming.ImageIndex := 0;
  Items.Add(FItemStartStreaming);

  FItemPlayStream := CreateMenuItem;
  FItemPlayStream.Caption := '&Play stream';
  FItemPlayStream.ImageIndex := 33;
  Items.Add(FItemPlayStream);

  FItemPlayStreamExternal := CreateMenuItem;
  FItemPlayStreamExternal.Caption := 'P&lay stream (external player)';
  FItemPlayStreamExternal.ImageIndex := 82;
  Items.Add(FItemPlayStreamExternal);

  FItemAddStream := CreateMenuItem;
  FItemAddStream.Caption := '&Add stream';
  FItemAddStream.ImageIndex := 80;
  Items.Add(FItemAddStream);
end;

{ TMyComboBox }

function TMyComboBox.MouseActivate(Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer): TMouseActivate;
begin
  Result := inherited;

  if GetParentForm(Self).Handle <> GetForegroundWindow then
    SetTimer(Handle, 0, 10, nil);
end;

procedure TMyComboBox.WndProc(var Message: TMessage);
begin
  inherited;

  if Message.Msg = WM_TIMER then
  begin
    KillTimer(Handle, 0);
    SelectAll;
  end;
end;

end.

