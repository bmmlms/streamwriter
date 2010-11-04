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
unit StreamBrowserView;

interface

uses
  Windows, SysUtils, Classes, Messages, ComCtrls, ActiveX, Controls, Buttons,
  StdCtrls, Menus, ImgList, Math, VirtualTrees, LanguageObjects,
  Graphics, DragDrop, DragDropFile, Functions, AppData, ExtCtrls,
  HomeCommunication;

type
  TModes = (moShow, moLoading, moError, moOldVersion);

  TMStreamTree = class;

  TStreamData = record
    Name: string;
    URL: string;
  end;
  TStreamDataArray = array of TStreamData;

  TStreamNodeData = record
    Name: string;
    Genre: string;
    URL: string;
    BitRate: Integer;
    Downloads: Integer;
    HasData: Boolean;
  end;
  PStreamNodeData = ^TStreamNodeData;

  TOpenActions = (oaStart, oaPlay, oaOpen, oaCopy, oaSave);

  TNeedDataEvent = procedure(Sender: TObject; Offset, Count: Integer) of object;
  TAddStreamEvent = procedure(Sender: TObject; URL, Name: string) of object;
  TActionEvent = procedure(Sender: TObject; Action: TOpenActions; Streams: TStreamDataArray) of object;

  TScrollDirection = (sdUp, sdDown);

  TMLoadingPanel = class(TPanel)
  private
    FLabel: TLabel;
    FBtnRetry: TButton;
    FDots: string;
    FTimer: TTimer;
    procedure TimerOnTimer(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TMStreamSearchPanel = class(TPanel)
  private
    FSearchLabel: TLabel;
    FGenreLabel: TLabel;
    FKbpsLabel: TLabel;
    FSearchEdit: TEdit;
    FGenreList: TComboBox;
    FKbpsList: TComboBox;
    FSearchButton: TSpeedButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Setup;
  end;

  TMStreamBrowserView = class(TPanel)
  private
    FSearch: TMStreamSearchPanel;
    FStreamTree: TMStreamTree;
    FCountLabel: TLabel;
    FLoadingPanel: TMLoadingPanel;

    FCurrentSearch: string;
    FCurrentGenre: string;
    FCurrentKbps: Integer;

    FLoading: Boolean;

    FHomeCommunication: THomeCommunication;

    procedure ListsChange(Sender: TObject);
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char);
    procedure SearchButtonClick(Sender: TObject);
    procedure BtnRetryClick(Sender: TObject);

    procedure GetStreams; overload;
    procedure GetStreams(Search, Genre: string; Kbps: Integer); overload;
    procedure SwitchMode(Mode: TModes);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; HomeCommunication: THomeCommunication); reintroduce;
    destructor Destroy; override;

    procedure Setup;
    procedure Translate;

    procedure StreamBrowserNeedData(Sender: TObject; Offset, Count: Integer);
    procedure HomeCommunicationStreamsReceived(Sender: TObject; Streams: TStreamInfoArray;
      Count: Integer);
    procedure HomeCommunicationGenresReceived(Sender: TObject; Genres: TStringList);
    procedure HomeCommunicationReceiveError(Sender: TObject);
    procedure HomeCommunicationOldVersion(Sender: TObject);

    property CurrentSearch: string read FCurrentSearch write FCurrentSearch;
    property CurrentGenre: string read FCurrentGenre write FCurrentGenre;
    property CurrentKbps: Integer read FCurrentKbps write FCurrentKbps;

    property StreamTree: TMStreamTree read FStreamTree;
  end;

  TMStreamTree = class(TVirtualStringTree)
  private
    FDragSource: TDropFileSource;

    FColName: TVirtualTreeColumn;
    FDisplayCount: Integer;
    FUnloadedVisible: Boolean;

    FIsLoading: Boolean;

    FLastScrollTick: Cardinal;
    FScrollDirection: TScrollDirection;
    FLastScrollY: Integer;

    FTimerScroll: TTimer;

    FPopupMenu: TPopupMenu;
    FItemStart: TMenuItem;
    FItemPlay: TMenuItem;
    FItemOpen: TMenuItem;
    FItemCopy: TMenuItem;
    FItemSave: TMenuItem;

    FTimer: TTimer;
    FDots: string;
    FLoadOffset: Integer;

    FOnNeedData: TNeedDataEvent;
    FOnAction: TActionEvent;

    procedure FSetIsLoading(Value: Boolean);

    procedure FitColumns;
    function AddStream(Node: PVirtualNode; Name, Genre, URL: string;
      BitRate, Downloads: Integer; HasData: Boolean): PVirtualNode;
    procedure GetLoadDataNodes(var FirstVisibleNoData, LastVisibleNoData: PVirtualNode);
    function GetSelected: TStreamDataArray;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoDragging(P: TPoint); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; Text: UnicodeString; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoScroll(DeltaX, DeltaY: Integer); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
    procedure Resize; override;
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString; override;

    procedure TimerScrollOnTimer(Sender: TObject);
    procedure TimerOnTimer(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure Setup;

    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    procedure AddStreams(Streams: TStreamInfoArray; Count: Integer);
    procedure ReceiveError;
    procedure ClearStreams;

    property IsLoading: Boolean read FIsLoading write FSetIsLoading;

    property DisplayCount: Integer read FDisplayCount;
    property LoadOffset: Integer read FLoadOffset write FLoadOffset;
    property OnNeedData: TNeedDataEvent read FOnNeedData write FOnNeedData;
    property OnAction: TActionEvent read FOnAction write FOnAction;
  end;

implementation

{ TMStreamView }

function TMStreamTree.AddStream(Node: PVirtualNode; Name, Genre, URL: string;
  BitRate, Downloads: Integer; HasData: Boolean): PVirtualNode;
var
  NodeData: PStreamNodeData;
begin
  if Node = nil then
    Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Name := Name;
  NodeData.Genre := Genre;
  NodeData.URL := URL;
  NodeData.BitRate := BitRate;
  NodeData.Downloads := Downloads;
  NodeData.HasData := HasData;
  Result := Node;
end;

constructor TMStreamTree.Create(AOwner: TComponent);
begin
  inherited;
  NodeDataSize := SizeOf(TStreamNodeData);
  IncrementalSearch := isVisibleOnly;

  FScrollDirection := sdDown;
  FLastScrollY := 0;

  TreeOptions.SelectionOptions := [toDisableDrawSelection, toRightClickSelect, toFullRowSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop] + [toFullRowDrag];
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FDragSource := TDropFileSource.Create(Self);

  FUnloadedVisible := False;
  FIsLoading := False;
  FLoadOffset := 0;

  FColName := Header.Columns.Add;
  FColName.Text := _('Name');
  FitColumns;

  FPopupMenu := TPopupMenu.Create(Self);

  FItemStart := FPopupMenu.CreateMenuItem;
  FItemStart.Caption := _('&Start recording');
  FItemStart.Default := True;
  FItemStart.OnClick := PopupMenuClick;
  FPopupMenu.Items.Add(FItemStart);

  FItemPlay := FPopupMenu.CreateMenuItem;
  FItemPlay.Caption := _('&Play stream');
  FItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.Items.Add(FItemPlay);

  FItemOpen := FPopupMenu.CreateMenuItem;
  FItemOpen.Caption := _('&Play stream (external player)');
  FItemOpen.OnClick := PopupMenuClick;
  FPopupMenu.Items.Add(FItemOpen);

  FItemCopy := FPopupMenu.CreateMenuItem;
  FItemCopy.Caption := _('&Copy URL');
  FItemCopy.OnClick := PopupMenuClick;
  FPopupMenu.Items.Add(FItemCopy);

  FItemSave := FPopupMenu.CreateMenuItem;
  FItemSave.Caption := _('&Save as playlist...');
  FItemSave.OnClick := PopupMenuClick;
  FPopupMenu.Items.Add(FItemSave);


  FTimerScroll := TTimer.Create(Self);
  FTimerScroll.OnTimer := TimerScrollOnTimer;
  FTimerScroll.Interval := 50;
  FTimerScroll.Enabled := True;

  FDots := '';
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerOnTimer;
  FTimer.Interval := 1000;
  FTimer.Enabled := True;
end;

destructor TMStreamTree.Destroy;
begin
  FPopupMenu.Free;
  FDragSource.Free;
  FTimer.Free;

  inherited;
end;

procedure TMStreamTree.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PStreamNodeData;
begin
  NodeData := GetNodeData(Node);
  Finalize(NodeData.Name);
  Finalize(NodeData.Genre);
  Finalize(NodeData.URL);
  inherited;
end;

function TMStreamTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;
  if ((Kind = ikNormal) or (Kind = ikSelected)) and (Column = 0) then
  begin
    Index := 0;
  end;
end;

procedure TMStreamTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PStreamNodeData;
begin
  inherited;
  NodeData := PStreamNodeData(GetNodeData(Node));
  case Column of
    0:
      if NodeData.HasData then
        Text := NodeData.Name
      else
        Text := _('Loading info') + FDots;
  end;
end;

procedure TMStreamTree.DoScroll(DeltaX, DeltaY: Integer);
begin
  inherited;

  if DeltaY < 0 then
    FScrollDirection := sdDown
  else
    FScrollDirection := sdUp;

  FLastScrollTick := GetTickCount;
  Exit;
end;

procedure TMStreamTree.FitColumns;
begin
  Header.AutoSizeIndex := 1;
  Header.Options := Header.Options + [hoAutoResize];
end;

procedure TMStreamTree.GetLoadDataNodes(var FirstVisibleNoData, LastVisibleNoData: PVirtualNode);
var
  Node: PVirtualNode;
  NodeData: PStreamNodeData;
  R, R2: TRect;
  InRect: Boolean;
begin
  inherited;

  FirstVisibleNoData := nil;
  LastVisibleNoData := nil;

  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);

    R := GetDisplayRect(Node,0, False);
    R2 := Self.ClientRect;

    InRect := PtInRect(R2, R.TopLeft);
    if not InRect then
      InRect := PtInRect(R2, R.BottomRight);

    if not NodeData.HasData then
      if InRect then
      begin
        if FirstVisibleNoData = nil then
        begin
          FirstVisibleNoData := Node;
        end;
        LastVisibleNoData := Node;
      end;

    Node := GetNext(Node);
  end;
end;

function TMStreamTree.GetNodes(SelectedOnly: Boolean): TNodeArray;
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

function TMStreamTree.GetSelected: TStreamDataArray;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PStreamNodeData;
begin
  SetLength(Result, 0);
  Nodes := GetNodes(True);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if NodeData.HasData then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].Name := NodeData.Name;
      Result[High(Result)].URL := NodeData.URL;
    end;
  end;
end;

procedure TMStreamTree.HandleMouseDblClick(var Message: TWMMouse;
  const HitInfo: THitInfo);
var
  Entries: TStreamDataArray;
begin
  inherited;
  if HitInfo.HitNode <> nil then
  begin
    Entries := GetSelected;
    if (Length(Entries) > 0) and Assigned(FOnAction) then
      FOnAction(Self, oaStart, Entries);
  end;
end;

procedure TMStreamTree.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then
  begin
    FItemStart.Click;
    Key := #0;
  end;
end;

procedure TMStreamTree.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  Text: UnicodeString; CellRect: TRect; DrawFormat: Cardinal);
var
  Size, Size2: TSize;
  TmpText: string;
  NodeData: PStreamNodeData;
begin
  NodeData := PStreamNodeData(GetNodeData(PaintInfo.Node));

  if NodeData.HasData then
  begin
    GetTextExtentPoint32W(PaintInfo.Canvas.Handle, 'Wl0', 3, Size);

    CellRect.Top := CellRect.Top + 2;
    DrawFormat := DT_TOP or DT_LEFT;
    inherited;

    CellRect.Top := CellRect.Top + 2 + Size.cy;

    Text := '';
    if NodeData.BitRate > 0 then
      Text := IntToStr(NodeData.BitRate) + 'kbps';
    if NodeData.Genre <> '' then
    begin
      if Text <> '' then
        Text := Text + ' / ';
      //Text := Text + IntToStr(NodeData.Downloads) + ' ' + _('Downloads') + ' / ';
      Text := Text + NodeData.Genre;
    end;

    if Text = '' then
      Text := _('No info available');
    inherited;
  end else
  begin
    TmpText := Copy(Text, 1, Length(Text) - Length(FDots));
    GetTextExtentPoint32W(PaintInfo.Canvas.Handle, TmpText, Length(TmpText), Size);
    GetTextExtentPoint32W(PaintInfo.Canvas.Handle, '...', 3, Size2);

    CellRect.Left := (CellRect.Right div 2) - (Size.cx div 2) + (Size2.cx div 2);
    CellRect.Top := (CellRect.Bottom div 2) - (Size.cy div 2);

    DrawFormat := 0;
    inherited;
  end;
end;

procedure TMStreamTree.Paint;
var
  Size: TSize;
  TmpText: string;
  R: TRect;
begin
  inherited;
  if FIsLoading and (RootNodeCount = 0) then
  begin
    TmpText := _('Loading streams');
    GetTextExtentPoint32W(Canvas.Handle, TmpText, Length(TmpText), Size);

    R := ClientRect;
    R.Left := (R.Right div 2) - (Size.cx div 2) - 4;
    R.Top := (R.Bottom div 2) - (Size.cy div 2);

    DrawText(Canvas.Handle, PChar(TmpText + FDots), Length(TmpText) + Length(FDots), R, 0);
  end;
end;

procedure TMStreamTree.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
begin
  PaintInfo.ImageInfo[ImageInfoIndex].XPos := 4;
  PaintInfo.ImageInfo[ImageInfoIndex].YPos := 4;
  inherited;
end;

procedure TMStreamTree.PopupMenuClick(Sender: TObject);
var
  Action: TOpenActions;
  Streams: TStreamDataArray;
begin
  Streams := GetSelected;

  if Sender = FItemStart then
    Action := oaStart
  else if Sender = FItemPlay then
    Action := oaPlay
  else if Sender = FItemOpen then
    Action := oaOpen
  else if Sender = FItemCopy then
    Action := oaCopy
  else if Sender = FItemSave then
    Action := oaSave
  else
    raise Exception.Create('');

  if Length(Streams) > 0 then
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Streams);
end;

procedure TMStreamTree.TimerOnTimer(Sender: TObject);
var
  i: Integer;
  Nodes: TNodeArray;
begin
  FDots := FDots + '.';

  if Length(FDots) = 4 then
    FDots := '';

  if not FIsLoading then
  begin
    Nodes := GetNodes(False);
    for i := 0 to Length(Nodes) - 1 do
      InvalidateNode(Nodes[i]);
  end else
    Invalidate;
end;

procedure TMStreamTree.TimerScrollOnTimer(Sender: TObject);
var
  Offset, Count: Integer;
  FirstVisibleNoData, LastVisibleNoData: PVirtualNode;
begin
  if (FLastScrollTick <> 0) and (GetTickCount > FLastScrollTick + 200) then
  begin
    GetLoadDataNodes(FirstVisibleNoData, LastVisibleNoData);
    if FirstVisibleNoData <> nil then
    begin
      Offset := FirstVisibleNoData.Index - 5;

      if Offset < 0 then
        Offset := 0;

      Count := FDisplayCount + 10;

      FLoadOffset := Offset;

      if Assigned(FOnNeedData) then
        FOnNeedData(Self, Offset, Count);

      FTimer.Enabled := True;
    end;
    FLastScrollTick := 0;
  end;
end;

procedure TMStreamTree.DoDragging(P: TPoint);
var
  i: Integer;
  Entries: TStreamDataArray;
begin
  if FDragSource.DragInProgress then
    Exit;

  FDragSource.Files.Clear;
  Entries := GetSelected;
  for i := 0 to Length(Entries) - 1 do
    FDragSource.Files.Add(Entries[i].URL);

  if FDragSource.Files.Count = 0 then
    Exit;

  DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending]);
  FDragSource.Execute(True);
end;

procedure TMStreamTree.AddStreams(Streams: TStreamInfoArray;
  Count: Integer);
var
  i, n: Integer;
  Node: PVirtualNode;
begin
  if FLoadOffset = 0 then
  begin
    FScrollDirection := sdDown;
    FLastScrollY := 0;

    IsLoading := False;
    BeginUpdate;
    try
      for i := 0 to Length(Streams) - 1 do
        AddStream(nil, Streams[i].Name, Streams[i].Genre, Streams[i].URL, Streams[i].BitRate, Streams[i].Downloads, True);
      for i := RootNodeCount to Count - 1 do
        AddStream(nil, '', '', '', 0, 0, False);
    finally
      EndUpdate;
    end;
  end else
  begin
    i := 0;
    n := 0;
    BeginUpdate;
    try
      Node := GetFirst;
      while Node <> nil do
      begin
        if (i >= FLoadOffset) and (High(Streams) >= n) then
        begin
          AddStream(Node, Streams[n].Name, Streams[n].Genre, Streams[n].URL, Streams[n].BitRate, Streams[n].Downloads, True);
          InvalidateNode(Node);
          Inc(n);
        end;
        Inc(i);
        Node := GetNext(Node);
      end;
    finally
      EndUpdate;
    end;
  end;

  FTimer.Enabled := False;
end;

procedure TMStreamTree.ClearStreams;
begin
  FLoadOffset := 0;
  Clear;
end;

procedure TMStreamTree.FSetIsLoading(Value: Boolean);
begin
  FIsLoading := Value;
  FDots := '';
  FTimer.Enabled := False;
  FTimer.Enabled := True;
  Invalidate;
end;

procedure TMStreamTree.ReceiveError;
begin
  Clear;
end;

procedure TMStreamTree.Resize;
begin
  inherited;
  Setup;
  FLastScrollTick := GetTickCount;
end;

procedure TMStreamTree.Setup;
var
  Size: TSize;
begin
  FColName.Width := ClientWidth;
  GetTextExtentPoint32W(Canvas.Handle, 'Wl0', 3, Size);
  DefaultNodeHeight := Size.cy * 2 + 6;
  FDisplayCount := Ceil(ClientHeight / DefaultNodeHeight);
end;

procedure TMStreamTree.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  if Button = mbRight then
  begin
    if Length(GetSelected) > 0 then
    begin
      P.X := X;
      P.Y := Y;
      FPopupMenu.Popup(ClientToScreen(P).X, ClientToScreen(P).Y);
    end;
  end;
end;

function TMStreamTree.DoGetNodeTooltip(Node: PVirtualNode;
  Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString;
var
  Text: UnicodeString;
  NodeData: PStreamNodeData;
begin
  inherited;
  Text := '';
  LineBreakStyle := hlbForceMultiLine;
  DoGetText(Node, Column, ttNormal, Text);
  NodeData := GetNodeData(Node);
  if NodeData.HasData then
  begin
    if NodeData.Genre <> '' then
      Text := Text + #13#10 + NodeData.Genre;
  end;
  Result := Text;
end;

{ TMStreamView }

procedure TMStreamBrowserView.BtnRetryClick(Sender: TObject);
begin
  SwitchMode(moLoading);
  if FSearch.FGenreList.Items.Count = 0 then
    FHomeCommunication.GetGenres;
  FStreamTree.ClearStreams;
  FHomeCommunication.GetStreams(FStreamTree.DisplayCount, 0, FCurrentSearch, FCurrentGenre, FCurrentKbps, True);
end;

constructor TMStreamBrowserView.Create(AOwner: TComponent; HomeCommunication: THomeCommunication);
begin
  inherited Create(AOwner);

  Align := alClient;
  BevelOuter := bvNone;

  FCurrentKbps := 32;
  FLoading := False;
  FHomeCommunication := HomeCommunication;

  FSearch := TMStreamSearchPanel.Create(Self);
  FSearch.Align := alTop;
  FSearch.Height := 100;
  FSearch.Parent := Self;
  FSearch.Visible := True;

  FStreamTree := TMStreamTree.Create(Self);
  FStreamTree.Align := alClient;
  FStreamTree.Parent := Self;
  FStreamTree.Visible := True;
  FStreamTree.OnNeedData := StreamBrowserNeedData;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Align := alBottom;
  FCountLabel.Parent := Self;
  FCountLabel.Visible := True;

  FLoadingPanel := TMLoadingPanel.Create(Self);
  FLoadingPanel.Align := alClient;
  FLoadingPanel.Parent := Self;
  FLoadingPanel.Visible := False;

  FLoadingPanel.FBtnRetry.OnClick := BtnRetryClick;
end;

destructor TMStreamBrowserView.Destroy;
begin
  inherited;
end;

procedure TMStreamBrowserView.GetStreams(Search, Genre: string; Kbps: Integer);
begin
  if (Search = CurrentSearch) and (Genre = CurrentGenre) and (Kbps = CurrentKbps) then
    Exit;

  FStreamTree.ClearStreams;
  CurrentSearch := Search;
  CurrentGenre := Genre;
  CurrentKbps := Kbps;
  FStreamTree.IsLoading := True;
  FHomeCommunication.GetStreams(FStreamTree.DisplayCount, 0, Search, Genre, Kbps, True);
end;

procedure TMStreamBrowserView.GetStreams;
var
  Search, Genre: string;
  Kbps: Integer;
begin
  Search := Trim(FSearch.FSearchEdit.Text);

  if FSearch.FGenreList.ItemIndex > 0 then
    Genre := FSearch.FGenreList.Text
  else
    Genre := '';

  case FSearch.FKbpsList.ItemIndex of
    0: Kbps := 32;
    1: Kbps := 64;
    2: Kbps := 128;
    3: Kbps := 192;
    else
      raise Exception.Create('');
  end;

  GetStreams(Search, Genre, Kbps);
end;

procedure TMStreamBrowserView.HomeCommunicationGenresReceived(
  Sender: TObject; Genres: TStringList);
var
  i: Integer;
begin
  FSearch.FGenreList.Clear;
  FSearch.FGenreList.Items.Add(_('- No genre -'));
  for i := 0 to Genres.Count - 1 do
    FSearch.FGenreList.Items.Add(Genres[i]);
  if FSearch.FGenreList.Items.Count > 0 then
    FSearch.FGenreList.ItemIndex := 0;
  FSearch.FGenreList.Sorted := True;
  if FStreamTree.RootNodeCount > 0 then
    SwitchMode(moShow);
end;

procedure TMStreamBrowserView.HomeCommunicationOldVersion(Sender: TObject);
begin
  SwitchMode(moOldVersion);
end;

procedure TMStreamBrowserView.HomeCommunicationReceiveError(Sender: TObject);
begin
  SwitchMode(moError);
end;

procedure TMStreamBrowserView.HomeCommunicationStreamsReceived(Sender: TObject;
  Streams: TStreamInfoArray; Count: Integer);
begin
  FStreamTree.AddStreams(Streams, Count);
  if FSearch.FGenreList.Items.Count > 0 then
    SwitchMode(moShow);
  if Count = 1 then
    FCountLabel.Caption := Format(_('%d stream found'), [Count])
  else
    FCountLabel.Caption := Format(_('%d streams found'), [Count]);
end;

procedure TMStreamBrowserView.ListsChange(Sender: TObject);
begin
  GetStreams;
end;

procedure TMStreamBrowserView.Resize;
begin
  inherited;
end;

procedure TMStreamBrowserView.SearchButtonClick(Sender: TObject);
begin
  GetStreams;
end;

procedure TMStreamBrowserView.SearchEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    GetStreams;
    Key := #0;
  end;
end;

procedure TMStreamBrowserView.Setup;
begin
  SwitchMode(moLoading);

  FSearch.Setup;
  FStreamTree.Setup;

  FSearch.FSearchEdit.OnKeyPress := SearchEditKeyPress;
  FSearch.FSearchButton.OnClick := SearchButtonClick;
  FSearch.FGenreList.OnChange := ListsChange;
  FSearch.FKbpsList.OnChange := ListsChange;

  FHomeCommunication.OnGenresReceived := HomeCommunicationGenresReceived;
  FHomeCommunication.OnStreamsReceived := HomeCommunicationStreamsReceived;
  FHomeCommunication.OnReceiveError := HomeCommunicationReceiveError;
  FHomeCommunication.OnOldVersion := HomeCommunicationOldVersion;

  FHomeCommunication.GetGenres;
  FHomeCommunication.GetStreams(FStreamTree.DisplayCount, 0, FCurrentSearch, FCurrentGenre, FCurrentKbps, True);
end;

procedure TMStreamBrowserView.StreamBrowserNeedData(Sender: TObject;
  Offset, Count: Integer);
begin
  FHomeCommunication.GetStreams(Count, Offset, CurrentSearch, CurrentGenre, CurrentKbps, False);
end;

procedure TMStreamBrowserView.SwitchMode(Mode: TModes);
begin
  if (Mode <> moShow) then
  begin
    FSearch.Visible := False;
    FStreamTree.Visible := False;
    FCountLabel.Visible := False;
    FLoadingPanel.Visible := True;
  end else
  begin
    FSearch.Visible := True;
    FStreamTree.Visible := True;
    FCountLabel.Visible := True;
    FLoadingPanel.Visible := False;
  end;

  if Mode = moLoading then
  begin
    FLoadingPanel.FLabel.Caption := _('Loading streams');
    FLoadingPanel.FDots := '';
    FLoadingPanel.FTimer.Enabled := True;
    FLoadingPanel.FBtnRetry.Visible := False;
    FLoading := True;
  end else if Mode = moError then
  begin
    FLoadingPanel.FLabel.Caption := _('Error loading streams.');
    FLoadingPanel.FBtnRetry.Visible := True;
  end else if Mode = moOldVersion then
  begin
    FLoadingPanel.FLabel.Caption := _('Error loading streams.'#13#10'Please update your version of streamWriter.');
    FLoadingPanel.FBtnRetry.Visible := False;
  end;

  if Mode <> moLoading then
  begin
    FLoadingPanel.FTimer.Enabled := False;
    FLoading := False;
  end;

  FLoadingPanel.Resize;
end;

procedure TMStreamBrowserView.Translate;
var
  Idx: Integer;
begin
  if FSearch.FGenreList.Items.Count > 0 then
  begin
    Idx := FSearch.FGenreList.ItemIndex;
    FSearch.FGenreList.Items[0] := _('- No genre -');
    FSearch.FGenreList.ItemIndex := Idx;
  end;
  if FSearch.FKbpsList.Items.Count > 0 then
  begin
    Idx := FSearch.FKbpsList.ItemIndex;
    FSearch.FKbpsList.Items[0] := _('- No kbps -');
    FSearch.FKbpsList.ItemIndex := Idx;
  end;

  if FStreamTree.RootNodeCount = 1 then
    FCountLabel.Caption := Format(_('%d stream found'), [FStreamTree.RootNodeCount])
  else
    FCountLabel.Caption := Format(_('%d streams found'), [FStreamTree.RootNodeCount]);
end;

{ TMStreamSearch }

constructor TMStreamSearchPanel.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;
end;

procedure TMStreamSearchPanel.Setup;
var
  I: TIcon;
  B: TBitmap;
begin
  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Parent := Self;
  FSearchEdit.Left := 50;
  FSearchEdit.Top := 4;
  FSearchEdit.Anchors := [akLeft, akRight, akTop];

  FGenreList := TComboBox.Create(Self);
  FGenreList.Parent := Self;
  FGenreList.Style := csDropDownList;
  FGenreList.Left := 50;
  FGenreList.Top := 30;
  FGenreList.Anchors := [akLeft, akRight, akTop];
  FGenreList.DropDownCount := 16;

  FKbpsList := TComboBox.Create(Self);
  FKbpsList.Parent := Self;
  FKbpsList.Style := csDropDownList;
  FKbpsList.Left := 50;
  FKbpsList.Top := 56;
  FKbpsList.Anchors := [akLeft, akRight, akTop];


  FSearchLabel := TLabel.Create(Self);
  FSearchLabel.Parent := Self;
  FSearchLabel.Left := 4;
  FSearchLabel.Caption := _('Search:');
  FSearchLabel.Top := FSearchEdit.Top + FSearchEdit.Height div 2 - FSearchLabel.Height div 2;

  FGenreLabel := TLabel.Create(Self);
  FGenreLabel.Parent := Self;
  FGenreLabel.Left := 4;
  FGenreLabel.Caption := _('Genre:');
  FGenreLabel.Top := FGenreList.Top + FGenreList.Height div 2 - FGenreLabel.Height div 2;

  FKbpsLabel := TLabel.Create(Self);
  FKbpsLabel.Parent := Self;
  FKbpsLabel.Left := 4;
  FKbpsLabel.Caption := _('Kbps:');
  FKbpsLabel.Top := FKbpsList.Top + FKbpsList.Height div 2 - FKbpsLabel.Height div 2;


  FSearchButton := TSpeedButton.Create(Self);
  FSearchButton.Parent := Self;
  FSearchButton.Width := 24;
  FSearchButton.Height := 24;
  FSearchButton.Top := 3;
  FSearchButton.Left := ClientWidth - 8 - FSearchButton.Width;
  FSearchButton.Anchors := [akRight, akTop];
  FSearchButton.Flat := True;
  FSearchButton.Hint := _('Search');
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


  FSearchEdit.Width := ClientWidth - FSearchEdit.Left - 12 - FSearchButton.Width;
  FGenreList.Width := ClientWidth - FGenreList.Left - 8;
  FKbpsList.Width := ClientWidth - FKbpsList.Left - 8;


  FKbpsList.Items.Add(_('- No kbps -'));
  FKbpsList.Items.Add('>= 64');
  FKbpsList.Items.Add('>= 128');
  FKbpsList.Items.Add('>= 192');
  FKbpsList.ItemIndex := 0;


  ClientHeight := FKbpsList.Top + FKbpsList.Height + 8;
end;

{ TMLoadingPanel }

constructor TMLoadingPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BevelOuter := bvNone;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Visible := True;
  FLabel.Alignment := taCenter;

  FBtnRetry := TButton.Create(Self);
  FBtnRetry.Parent := Self;
  FBtnRetry.Visible := True;
  FBtnRetry.Caption := _('Retry');

  FDots := '';
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerOnTimer;
  FTimer.Interval := 1000;
  FTimer.Enabled := False;
end;

procedure TMLoadingPanel.Resize;
begin
  inherited;

  FLabel.Left := ClientWidth div 2 - FLabel.Width div 2;
  FLabel.Top := ClientHeight div 2 - FLabel.Height;

  FBtnRetry.Left := ClientWidth div 2 - FBtnRetry.Width div 2;
  FBtnRetry.Top := FLabel.Top + FLabel.Height + 8;
end;

procedure TMLoadingPanel.TimerOnTimer(Sender: TObject);
begin
  FDots := FDots + '.';

  if Length(FDots) = 4 then
    FDots := '';

  FLabel.Caption := _('Loading streams') + FDots;
end;

end.

