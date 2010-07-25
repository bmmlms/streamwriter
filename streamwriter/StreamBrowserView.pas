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
    HasData: Boolean;
  end;
  PStreamNodeData = ^TStreamNodeData;

  TOpenActions = (oaStart, oaListen, oaCopy, oaSave);

  TNeedDataEvent = procedure(Sender: TObject; Offset, Count: Integer) of object;
  TAddStreamEvent = procedure(Sender: TObject; URL, Name: string) of object;
  TActionEvent = procedure(Sender: TObject; Action: TOpenActions; Streams: TStreamDataArray) of object;

  TScrollDirection = (sdUp, sdDown);

  TMStreamBrowserView = class(TVirtualStringTree)
  private
    FDragSource: TDropFileSource;

    FColName: TVirtualTreeColumn;
    FDisplayCount: Integer;
    FUnloadedVisible: Boolean;

    FLastScrollTick: Cardinal;
    FScrollDirection: TScrollDirection;
    FLastScrollY: Integer;

    FTimerScroll: TTimer;

    FPopupMenu: TPopupMenu;
    FItemStart: TMenuItem;
    FItemPlay: TMenuItem;
    FItemCopy: TMenuItem;
    FItemSave: TMenuItem;

    FTimer: TTimer;
    FDots: string;
    FIsLoading: Boolean;
    FCurrentSearch: string;
    FLoadOffset: Integer;

    FOnNeedData: TNeedDataEvent;
    FOnAction: TActionEvent;

    procedure FSetIsLoading(Value: Boolean);

    procedure FitColumns;
    function AddStream(Node: PVirtualNode; Name, Genre, URL: string;
      BitRate: Integer; HasData: Boolean): PVirtualNode;
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
    procedure ClearStreams;

    property IsLoading: Boolean read FIsLoading write FSetIsLoading;
    property CurrentSearch: string read FCurrentSearch write FCurrentSearch;
    property LoadOffset: Integer read FLoadOffset write FLoadOffset;
    property DisplayCount: Integer read FDisplayCount;
    property OnNeedData: TNeedDataEvent read FOnNeedData write FOnNeedData;
    property OnAction: TActionEvent read FOnAction write FOnAction;
  end;

implementation

{ TMStreamView }

function TMStreamBrowserView.AddStream(Node: PVirtualNode; Name, Genre, URL: string;
  BitRate: Integer; HasData: Boolean): PVirtualNode;
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
  NodeData.HasData := HasData;
  Result := Node;
end;

constructor TMStreamBrowserView.Create(AOwner: TComponent);
begin
  inherited;
  NodeDataSize := SizeOf(TStreamNodeData);
  IncrementalSearch := isVisibleOnly;

  FScrollDirection := sdDown;
  FLastScrollY := 0;

  TreeOptions.SelectionOptions := [toDisableDrawSelection, toRightClickSelect, toFullRowSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FDragSource := TDropFileSource.Create(Self);

  FUnloadedVisible := False;
  FLoadOffset := 0;

  FColName := Header.Columns.Add;
  FColName.Text := _('Name');
  FitColumns;


  FPopupMenu := TPopupMenu.Create(Self);

  FItemStart := FPopupMenu.CreateMenuItem;
  FItemStart.Caption := _('&Start recording');
  FItemStart.OnClick := PopupMenuClick;
  FPopupMenu.Items.Add(FItemStart);

  FItemPlay := FPopupMenu.CreateMenuItem;
  FItemPlay.Caption := _('&Play stream');
  FItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.Items.Add(FItemPlay);

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

destructor TMStreamBrowserView.Destroy;
begin
  FPopupMenu.Free;
  FDragSource.Free;
  FTimer.Free;
  inherited;
end;

procedure TMStreamBrowserView.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PStreamNodeData;
begin
  NodeData := GetNodeData(Node);
  Finalize(NodeData.Name);
  Finalize(NodeData.Genre);
  Finalize(NodeData.URL);
  inherited;
end;

function TMStreamBrowserView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;
  if ((Kind = ikNormal) or (Kind = ikSelected)) and (Column = 0) then
  begin
    Index := 0;
  end;
end;

procedure TMStreamBrowserView.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
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

procedure TMStreamBrowserView.DoScroll(DeltaX, DeltaY: Integer);
begin
  inherited;

  if DeltaY < 0 then
    FScrollDirection := sdDown
  else
    FScrollDirection := sdUp;

  FLastScrollTick := GetTickCount;
  Exit;
end;

procedure TMStreamBrowserView.FitColumns;
begin
  Header.AutoSizeIndex := 1;
  Header.Options := Header.Options + [hoAutoResize];
end;

procedure TMStreamBrowserView.GetLoadDataNodes(var FirstVisibleNoData, LastVisibleNoData: PVirtualNode);
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

function TMStreamBrowserView.GetNodes(SelectedOnly: Boolean): TNodeArray;
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

function TMStreamBrowserView.GetSelected: TStreamDataArray;
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

procedure TMStreamBrowserView.HandleMouseDblClick(var Message: TWMMouse;
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

procedure TMStreamBrowserView.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then
  begin
    FItemStart.Click;
    Key := #0;
  end;
end;

procedure TMStreamBrowserView.DoTextDrawing(var PaintInfo: TVTPaintInfo;
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

procedure TMStreamBrowserView.Paint;
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

procedure TMStreamBrowserView.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
begin
  PaintInfo.ImageInfo[ImageInfoIndex].XPos := 4;
  PaintInfo.ImageInfo[ImageInfoIndex].YPos := 4;
  inherited;
end;

procedure TMStreamBrowserView.PopupMenuClick(Sender: TObject);
var
  Action: TOpenActions;
  Streams: TStreamDataArray;
begin
  Streams := GetSelected;

  if Sender = FItemStart then
    Action := oaStart
  else if Sender = FItemPlay then
    Action := oaListen
  else if Sender = FItemCopy then
    Action := oaCopy
  else if Sender = FItemSave then
    Action := oaSave
  else
    raise Exception.Create('Fail');

  if Length(Streams) > 0 then
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Streams);
end;

procedure TMStreamBrowserView.TimerOnTimer(Sender: TObject);
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

procedure TMStreamBrowserView.TimerScrollOnTimer(Sender: TObject);
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
    end;
    FLastScrollTick := 0;
  end;
end;

procedure TMStreamBrowserView.DoDragging(P: TPoint);
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

  DoStateChange([], [tsOLEDragPending, tsClearPending]);
  FDragSource.Execute(True);
end;

procedure TMStreamBrowserView.AddStreams(Streams: TStreamInfoArray;
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
        AddStream(nil, Streams[i].Name, Streams[i].Genre, Streams[i].URL, Streams[i].BitRate, True);
      for i := RootNodeCount to Count - 1 do
        AddStream(nil, '', '', '', 0, False);
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
          AddStream(Node, Streams[n].Name, Streams[n].Genre, Streams[n].URL, Streams[n].BitRate, True);
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
end;

procedure TMStreamBrowserView.ClearStreams;
begin
  Clear;
end;

procedure TMStreamBrowserView.FSetIsLoading(Value: Boolean);
begin
  FIsLoading := Value;
  FDots := '';
  FTimer.Enabled := False;
  FTimer.Enabled := True;
  Invalidate;
end;

procedure TMStreamBrowserView.Resize;
begin
  inherited;
  Setup;
  FLastScrollTick := GetTickCount;
end;

procedure TMStreamBrowserView.Setup;
var
  Size: TSize;
begin
  FColName.Width := ClientWidth;
  GetTextExtentPoint32W(Canvas.Handle, 'Wl0', 3, Size);
  DefaultNodeHeight := Size.cy * 2 + 6;
  FDisplayCount := Round(ClientHeight / DefaultNodeHeight);
end;

procedure TMStreamBrowserView.MouseUp(Button: TMouseButton; Shift: TShiftState;
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

function TMStreamBrowserView.DoGetNodeTooltip(Node: PVirtualNode;
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

end.

