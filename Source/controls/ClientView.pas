{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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

{ This unit is for displaying different streams (or TICEClients) in the main-window
  in a separated tab }
unit ClientView;

interface

uses
  ActiveX,
  AppData,
  Buttons,
  Classes,
  ComCtrls,
  ComObj,
  Controls,
  DataManager,
  DragDrop,
  DragDropFile,
  DropSource,
  Forms,
  Functions,
  Graphics,
  ICEClient,
  Images,
  ImgList,
  LanguageObjects,
  Logging,
  MControls,
  Menus,
  SharedControls,
  StdCtrls,
  StreamBrowserView,
  StrUtils,
  SWFunctions,
  SysUtils,
  Tabs,
  VirtualTrees,
  Windows;

type
  TAccessCanvas = class(TCanvas);

  TMClientView = class;

  TClientArray = array of TICEClient;

  TNodeTypes = (ntCategory, ntClient, ntClientNoAuto, ntAll);

  TClientNodeData = record
    Client: TICEClient;
    Category: TListCategory;
  end;
  PClientNodeData = ^TClientNodeData;

  TEntryTypes = (etStream, etFile);

  TNodeDataArray = array of PClientNodeData;

  TStartStreamingEvent = procedure(Sender: TObject; ID, Bitrate: Cardinal; Name, URL: string; URLs, RegExes, IgnoreTitles: TStringList; Node: PVirtualNode; Mode: TVTNodeAttachMode) of object;

  { TMClientView }

  TMClientView = class(TMVirtualStringTree)
  private
    FBrowser: TMStreamTree;

    FDragSource: TDropFileSource;
    FDragNodes: TNodeArray;
    FAutoNode: PVirtualNode;
    FDragTreshold: Integer;

    FInitialSorted: Boolean;

    FColName: TVirtualTreeColumn;
    FColTitle: TVirtualTreeColumn;
    FColRcvd: TVirtualTreeColumn;
    FColSongs: TVirtualTreeColumn;
    FColSpeed: TVirtualTreeColumn;
    FColStatus: TVirtualTreeColumn;

    FHeaderDragSourcePosition: Cardinal;

    FOnStartStreaming: TStartStreamingEvent;

    procedure FitColumns;
    procedure MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoDragging(P: TPoint); override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: LongWord): Boolean; override;
    procedure DoEdit; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); override;
    function DoHeaderDragging(Column: TColumnIndex): Boolean; override;
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean; override;
    procedure DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent; PopupMenu: TPopupMenu; Browser: TMStreamTree); reintroduce;
    destructor Destroy; override;

    function AddClient(Client: TICEClient): PVirtualNode;
    function RefreshClient(Client: TICEClient): Boolean;
    function GetClientNodeData(Client: TICEClient): PClientNodeData;
    function GetClientNode(Client: TICEClient): PVirtualNode;
    function GetCategoryNode(Idx: Integer): PVirtualNode;
    procedure RemoveClient(Client: TICEClient);
    procedure SortItems;
    function AddCategory(Category: TListCategory): PVirtualNode; overload;
    function AddCategory: PVirtualNode; overload;

    function GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
    function GetChildNodes(Parent: PVirtualNode): TNodeArray;
    function NodesToData(Nodes: TNodeArray): TNodeDataArray;
    function NodesToClients(Nodes: TNodeArray): TClientArray;
    function GetEntries(T: TEntryTypes): TPlaylistEntryArray;

    procedure PostTranslate; override;

    procedure MoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode; ChildrenOnly: Boolean); reintroduce;

    property AutoNode: PVirtualNode read FAutoNode write FAutoNode;
    property OnStartStreaming: TStartStreamingEvent read FOnStartStreaming write FOnStartStreaming;
  end;

implementation

{ TMStreamView }

function TMClientView.AddCategory(Category: TListCategory): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Client := nil;
  NodeData.Category := Category;
  if Category.IsAuto then
    FAutoNode := Node;
  Result := Node;
end;

function TMClientView.AddCategory: PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Client := nil;
  NodeData.Category := TListCategory.Create(_('New category'), 0);
  EditNode(Node, 0);
  Result := Node;
end;

function TMClientView.AddClient(Client: TICEClient): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Client := Client;
  NodeData.Category := nil;

  if Client.AutoRemove then
    MoveTo(Node, FAutoNode, amAddChildLast, False);

  Result := Node;
end;

constructor TMClientView.Create(AOwner: TComponent; PopupMenu: TPopupMenu; Browser: TMStreamTree);

var
  i: Integer;
begin
  inherited Create(AOwner);

  FBrowser := Browser;

  FDragTreshold := 6;

  NodeDataSize := SizeOf(TClientNodeData);
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toEditable];
  DragMode := dmAutomatic;

  Header.AutoSizeIndex := 1;

  Self.PopupMenu := PopupMenu;
  FDragSource := TDropFileSource.Create(Self);

  FColName := Header.Columns.Add;
  FColName.Text := _('Name');
  FColName.Options := FColName.Options - [coDraggable];
  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Title');
  FColRcvd := Header.Columns.Add;
  FColRcvd.Alignment := taRightJustify;
  FColRcvd.Text := _('Received');
  FColSongs := Header.Columns.Add;
  FColSongs.Alignment := taRightJustify;
  FColSongs.Text := _('Songs');
  FColSpeed := Header.Columns.Add;
  FColSpeed.Alignment := taRightJustify;
  FColSpeed.Text := _('Speed');
  FColStatus := Header.Columns.Add;
  FColStatus.Text := _('State');

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);
  TMTreeColumnPopup(Header.PopupMenu).OnAction := MenuColsAction;

  for i := 1 to Header.Columns.Count - 1 do
    if not ((AppGlobals.ClientCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];

  FitColumns;
end;

destructor TMClientView.Destroy;
begin
  FDragSource.Free;
  inherited;
end;

procedure TMClientView.DoFreeNode(Node: PVirtualNode);
begin
  inherited;
end;

function TMClientView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
var
  NodeData: PClientNodeData;
  AnyPlaying, AnyRecording: Boolean;
begin
  Result := Images;

  if Kind = ikOverlay then
    Exit;

  NodeData := GetNodeData(Node);
  if NodeData.Client <> nil then
    case Column of
      0:
        if NodeData.Client.Playing and NodeData.Client.Paused and NodeData.Client.Recording then
          Index := TImages.RECORD_PAUSE
        else if NodeData.Client.Playing and NodeData.Client.Recording then
          Index := TImages.RECORD_PLAY
        else if NodeData.Client.Recording then
          Index := TImages.RECORD_RED
        else if NodeData.Client.Playing and NodeData.Client.Paused then
          Index := TImages.PAUSE_BLUE
        else if NodeData.Client.Playing then
          Index := TImages.PLAY_BLUE
        else
          Index := TImages.STOP_BLUE;
    end else if Column = 0 then
    if NodeData.Category.IsAuto then
      Index := TImages.FOLDER_BRICKS
    else
    begin
      AnyPlaying := False;
      AnyRecording := False;

      Node := GetFirstChild(Node);
      while Node <> nil do
      begin
        NodeData := GetNodeData(Node);

        if NodeData.Client.Playing or NodeData.Client.Paused then
          AnyPlaying := True;
        if NodeData.Client.Recording then
          AnyRecording := True;

        Node := GetNextSibling(Node);
      end;

      if AnyPlaying and AnyRecording then
        Index := TImages.FOLDER_TRANSMIT_RECORD_PLAY
      else if AnyRecording then
        Index := TImages.FOLDER_TRANSMIT_RECORD
      else if AnyPlaying then
        Index := TImages.FOLDER_TRANSMIT_PLAY
      else
        Index := TImages.FOLDER_TRANSMIT;
    end;
end;

procedure TMClientView.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
var
  NodeData: PClientNodeData;
begin
  inherited;

  Text := '';
  NodeData := PClientNodeData(GetNodeData(Node));
  if NodeData.Client <> nil then
  begin
    case Column of
      0:
        if NodeData.Client.Entry.CustomName = '' then
          if NodeData.Client.Entry.StartURL = '' then
            Text := _('Unknown')
          else
            Text := NodeData.Client.Entry.StartURL
        else
          Text := NodeData.Client.Entry.CustomName;
      1:
        if NodeData.Client.DisplayTitle = '' then
          if (NodeData.Client.State = csConnected) or (NodeData.Client.State = csConnecting) then
            Text := _('Unknown')
          else
            Text := ''
        else
          Text := NodeData.Client.DisplayTitle;
      2:
        Text := TFunctions.MakeSize(NodeData.Client.Entry.BytesReceived);
      3:
        if NodeData.Client.AutoRemove then
          Text := ''
        else
          Text := IntToStr(NodeData.Client.Entry.SongsSaved);
      4:
        Text := TFunctions.MakeSize(NodeData.Client.Speed) + '/s';
      5:
        case NodeData.Client.State of
          csConnecting:
            Text := _('Connecting...');
          csConnected:
            Text := _('Connected');
          csRetrying:
            Text := _('Waiting...');
          csStopped:
            Text := _('Stopped');
          csStopping:
            Text := _('Stopping...');
          csIOError:
            Text := _('Error creating file');
        end;
    end;
  end else if Column = 0 then
    Text := NodeData.Category.Name + ' (' + IntToStr(Node.ChildCount) + ')';
end;

procedure TMClientView.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
var
  i: Integer;
  Nodes: TNodeArray;
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if Header.SortColumn <> HitInfo.Column then
    begin
      Header.SortColumn := HitInfo.Column;
      if (HitInfo.Column <> 0) and (HitInfo.Column <> 1) then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end else if Header.SortDirection = sdAscending then
      Header.SortDirection := sdDescending
    else
      Header.SortDirection := sdAscending;
    Sort(nil, HitInfo.Column, Header.SortDirection);
    Nodes := GetNodes(ntCategory, False);
    for i := 0 to Length(Nodes) - 1 do
      Sort(Nodes[i], Header.SortColumn, Header.SortDirection);
  end;
end;

procedure TMClientView.DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition);
begin
  inherited;

  if Header.Columns[Column].Position = 0 then
    Header.Columns[Column].Position := FHeaderDragSourcePosition;
end;

function TMClientView.DoHeaderDragging(Column: TColumnIndex): Boolean;
begin
  if Column = -1 then
    Exit(False);

  Result := inherited;

  FHeaderDragSourcePosition := Header.Columns[Column].Position;
end;

function TMClientView.DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer;
var
  NodeData: PClientNodeData;
  CellText: string;
begin
  Result := 0;

  NodeData := GetNodeData(Node);
  if NodeData = nil then
    Exit;

  DoGetText(Node, 0, ttNormal, CellText);

  if not StartsText(Text, CellText) then
    Result := 1;
end;

procedure TMClientView.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string);
var
  NodeData: PClientNodeData;
begin
  inherited;

  if Trim(Text) <> '' then
  begin
    NodeData := GetNodeData(Node);

    if NodeData.Category <> nil then
      NodeData.Category.Name := Text
    else if NodeData.Client <> nil then
      NodeData.Client.Entry.CustomName := Text;
  end;
end;

function TMClientView.DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean;
begin
  Result := inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Result := True;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(R);
end;

procedure TMClientView.DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(ItemRect);
end;

procedure TMClientView.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
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

procedure TMClientView.CreateHandle;
begin
  inherited CreateHandle;

  if RootNodeCount > 0 then
  begin
    Selected[GetFirst] := True;
    FocusedNode := GetFirst;
  end;
end;

procedure TMClientView.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal);
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  if not Selected[PaintInfo.Node] then
    if NodeData.Client <> nil then
    begin
      if NodeData.Client.Playing or NodeData.Client.Paused then
        PaintInfo.Canvas.Font.Color := TFunctions.HTML2Color('#0078ff');
    end else if NodeData.Category <> nil then
    begin
      Node := GetFirstChild(PaintInfo.Node);
      while Node <> nil do
      begin
        NodeData := GetNodeData(Node);

        if NodeData.Client.Playing or NodeData.Client.Paused then
        begin
          PaintInfo.Canvas.Font.Color := TFunctions.HTML2Color('#0078ff');  // TODO: sollte hier und hier drüber nicht hilite color genutzt werden? wie ist es beim savedtab wenn was wiedergegeben wird?
          Break;
        end;

        Node := GetNextSibling(Node);
      end;
    end;

  inherited;
end;

procedure TMClientView.FitColumns;
var
  i: Integer;
begin
  if (Header.Columns.Count <> Length(AppGlobals.ClientHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.ClientHeaderPosition)) then
    raise Exception.Create('(Header.Columns.Count <> Length(AppGlobals.ClientHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.ClientHeaderPosition))');

  if AppGlobals.ClientHeaderWidthLoaded then
  begin
    for i := 0 to Header.Columns.Count - 1 do
      if i <> 1 then
        Header.Columns[i].Width := AppGlobals.ClientHeaderWidth[i];
  end else
  begin
    FColRcvd.Width := Max(TFunctions.GetTextSize(FColRcvd.Text, Font).cx, TFunctions.GetTextSize('111,11 KB', Font).cx) + MulDiv(20, Screen.PixelsPerInch, 96);
    FColSpeed.Width := Max(TFunctions.GetTextSize(FColSpeed.Text, Font).cx, TFunctions.GetTextSize('11,11 KB/s', Font).cx) + MulDiv(20, Screen.PixelsPerInch, 96);
    FColSongs.Width := TFunctions.GetTextSize(FColSongs.Text, Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColStatus.Width := Max(TFunctions.GetTextSize(FColStatus.Text, Font).cx, MulDiv(80, Screen.PixelsPerInch, 96)) + MulDiv(20, Screen.PixelsPerInch, 96);
    FColName.Width := MulDiv(150, Screen.PixelsPerInch, 96);
  end;

  if AppGlobals.ClientHeaderPositionLoaded then
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.ClientHeaderPosition[i];
end;

function TMClientView.DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: LongWord): Boolean;
var
  i, n: Integer;
  Children: TNodeArray;
  HitNode: PVirtualNode;
  NodeData, ParentNodeData: PClientNodeData;
  s: string;
  URLs: TStringArray;
  R: TRect;
begin
  if (Length(FDragNodes) = 0) and (Length(FBrowser.DraggedStreams) = 0) and (not (TFunctions.ReadDataObjectText(VTVDragManager.DataObject, s) and TFunctions.FilterHTTPUrls(s, URLs))) then
    Exit(False);

  Result := True;

  HitNode := GetNodeAt(Pt.X, Pt.Y);
  if HitNode <> nil then
  begin
    NodeData := GetNodeData(HitNode);
    if (NodeData.Category <> nil) and (NodeData.Category.IsAuto) then
    begin
      R := GetDisplayRect(HitNode, 0, False);
      if Expanded[HitNode] then
      begin
        if ChildCount[HitNode] > 0 then
        begin
          if not (Pt.Y < R.Top + FDragTreshold) then
            Exit(False);
        end else
        begin
          if (not (Pt.Y > R.Bottom - FDragTreshold)) and (not (Pt.Y < R.Top + FDragTreshold)) then
            Exit(False);
        end;
      end else if (not (Pt.Y > R.Bottom - FDragTreshold)) and (not (Pt.Y < R.Top + FDragTreshold)) then
        Exit(False); // Man darf in die automatische Kategorie nix reindraggen
    end;

    if (NodeData.Client <> nil) and (GetNodeLevel(HitNode) > 0) then
    begin
      ParentNodeData := GetNodeData(HitNode.Parent);
      if ParentNodeData.Category <> nil then
        if ParentNodeData.Category.IsAuto then
          Exit(False);
    end;

    // Drop darf nur erlaubt sein, wenn Ziel-Node nicht in gedraggten
    // Nodes vorkommt und Ziel-Node kein Kind von Drag-Node ist
    for i := 0 to Length(FDragNodes) - 1 do
    begin
      if HitNode = FDragNodes[i] then
        Exit(False);

      Children := GetNodes(ntClient, False);
      for n := 0 to Length(Children) - 1 do
        if (Children[n] = HitNode) and (HitNode.Parent = FDragNodes[i]) then
          Exit(False);
    end;
  end;
end;

procedure TMClientView.DoEdit;
var
  Edit: TVTEdit;
  NodeData: PClientNodeData;
begin
  inherited;

  if (EditLink <> nil) and (EditLink is TStringEditLink) then
  begin
    NodeData := GetNodeData((EditLink as TStringEditLink).Node);
    Edit := (EditLink as TStringEditLink).Edit;

    if NodeData.Client <> nil then
      Edit.Text := NodeData.Client.Entry.CustomName
    else
      Edit.Text := NodeData.Category.Name;

    Edit.SelectAll;
  end;
end;

function TMClientView.GetClientNodeData(Client: TICEClient): PClientNodeData;
var
  Nodes: TNodeArray;
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Result := nil;
  Nodes := GetNodes(ntClient, False);
  for Node in Nodes do
  begin
    NodeData := GetNodeData(Node);
    if NodeData.Client = Client then
    begin
      Result := NodeData;
      Exit;
    end;
  end;
end;

function TMClientView.GetClientNode(Client: TICEClient): PVirtualNode;
var
  Nodes: TNodeArray;
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Result := nil;
  Nodes := GetNodes(ntClient, False);
  for Node in Nodes do
  begin
    NodeData := GetNodeData(Node);
    if NodeData.Client = Client then
    begin
      Result := Node;
      Exit;
    end;
  end;
end;

function TMClientView.GetCategoryNode(Idx: Integer): PVirtualNode;
var
  Nodes: TNodeArray;
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Result := nil;
  Nodes := GetNodes(ntCategory, False);
  for Node in Nodes do
  begin
    NodeData := GetNodeData(Node);
    if (NodeData.Category <> nil) and (NodeData.Category.Index = Idx) then
    begin
      Result := Node;
      Exit;
    end;
  end;
end;

function TMClientView.GetChildNodes(Parent: PVirtualNode): TNodeArray;
var
  Node: PVirtualNode;
begin
  SetLength(Result, 0);
  Node := GetFirst;
  while Node <> nil do
  begin
    if Node.Parent = Parent then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Node;
    end;
    Node := GetNext(Node);
  end;
end;

function TMClientView.GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
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

    if ((NodeTypes = ntClient) and (NodeData.Client = nil)) or (((NodeTypes = ntClientNoAuto) and (NodeData.Client = nil)) or ((NodeTypes = ntClientNoAuto) and (NodeData.Client <> nil) and (NodeData.Client.AutoRemove))) or
      ((NodeTypes = ntCategory) and (NodeData.Client <> nil)) then
    begin
      Node := GetNext(Node);
      Continue;
    end;

    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := Node;
    Node := GetNext(Node);
  end;
end;

procedure TMClientView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    Key := 0;
  inherited;
end;

procedure TMClientView.KeyPress(var Key: Char);
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
  Nodes: TNodeArray;
begin
  if Key = ' ' then
  begin
    Nodes := GetNodes(ntAll, False);
    for Node in Nodes do
    begin
      NodeData := GetNodeData(Node);
      if (NodeData.Client <> nil) and NodeData.Client.Playing then
      begin
        ClearSelection;
        SelectNodes(Node, Node, False);
        FocusedNode := Node;
        ScrollIntoView(Node, True);
        Exit;
      end;
    end;
  end else
    inherited;
end;

procedure TMClientView.MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
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

  AppGlobals.ClientCols := AppGlobals.ClientCols xor (1 shl Index);
end;

procedure TMClientView.MoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode; ChildrenOnly: Boolean);
var
  NodeData: PClientNodeData;
begin
  inherited;

  if FInitialSorted and (Mode in [amAddChildFirst, amAddChildLast]) then
    if not Expanded[Target] then
    begin
      NodeData := GetNodeData(Source);
      if NodeData.Client <> nil then
        Expanded[Target] := True;
    end;
end;

function TMClientView.RefreshClient(Client: TICEClient): Boolean;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
begin
  Result := False;
  Nodes := GetNodes(ntClient, False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if NodeData.Client = Client then
    begin
      Result := True;
      InvalidateNode(Nodes[i]);
      if (Nodes[i].Parent <> nil) and (Nodes[i].Parent <> RootNode) then
        InvalidateNode(Nodes[i].Parent);
      Break;
    end;
  end;
end;

procedure TMClientView.RemoveClient(Client: TICEClient);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
begin
  Nodes := GetNodes(ntClient, False);
  for i := Length(Nodes) - 1 downto 0 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if NodeData.Client = Client then
    begin
      DeleteNode(Nodes[i]);
      Break;
    end;
  end;
end;

procedure TMClientView.SortItems;
var
  i: Integer;
  Nodes: TNodeArray;
begin
  Sort(nil, -1, sdAscending);
  Nodes := GetNodes(ntCategory, False);
  for i := 0 to Length(Nodes) - 1 do
    Sort(Nodes[i], -1, sdAscending);
  FInitialSorted := True;
end;

procedure TMClientView.PostTranslate;
var
  NodeData: PClientNodeData;
begin
  NodeData := GetNodeData(FAutoNode);
  NodeData.Category.Name := _('Automatic recordings');

  inherited;

  FColName.Text := _('Name');
  FColTitle.Text := _('Title');
  FColRcvd.Text := _('Received');
  FColSongs.Text := _('Songs');
  FColSpeed.Text := _('Speed');
  FColStatus.Text := _('State');
end;

function TMClientView.NodesToData(Nodes: TNodeArray): TNodeDataArray;
var
  i: Integer;
  Data: PClientNodeData;
begin
  SetLength(Result, Length(Nodes));
  for i := 0 to Length(Nodes) - 1 do
  begin
    Data := GetNodeData(Nodes[i]);
    Result[i] := Data;
  end;
end;

procedure TMClientView.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  NodeData: PClientNodeData;
begin
  inherited;

  NodeData := GetNodeData(PaintInfo.Node);

  if (NodeData.Client <> nil) and (NodeData.Client.Entry.Schedules.Count > 0) then
    Images.Resolution[8].Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos + 8, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.TIME);
end;

function TMClientView.NodesToClients(Nodes: TNodeArray): TClientArray;
var
  i: Integer;
  Data: PClientNodeData;
begin
  SetLength(Result, Length(Nodes));
  for i := 0 to Length(Nodes) - 1 do
  begin
    Data := GetNodeData(Nodes[i]);
    Result[i] := Data.Client;
  end;
end;

procedure TMClientView.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  NodeData: PClientNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  // Den Check auf NodeData wegen Bugreport von "Klaus <knatterton_nick@gmx.net>" eingebaut...
  Allowed := (NodeData <> nil) and (((NodeData.Client <> nil) and (not NodeData.Client.AutoRemove) and (NodeData.Client.Entry.CustomName <> '')) or ((NodeData.Category <> nil) and (not NodeData.Category.IsAuto)));
end;

function TMClientView.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
var
  Data1, Data2: PClientNodeData;
  I1, I2: Integer;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  if (Column = -1) and (not FInitialSorted) then
  begin
    // Mit Column -1 heißt nach Programmstart sortieren
    if Data1.Client <> nil then
      I1 := Data1.Client.Entry.Index
    else
      I1 := Data1.Category.Index;

    if Data2.Client <> nil then
      I2 := Data2.Client.Entry.Index
    else
      I2 := Data2.Category.Index;

    Result := TFunctions.CmpInt(I1, I2);
    Exit;
  end;

  if (Data1.Client <> nil) and (Data2.Client <> nil) then
    case Column of
      0: Result := CompareText(Data1.Client.Entry.CustomName, Data2.Client.Entry.CustomName);
      1: Result := CompareText(Data1.Client.Title, Data2.Client.Title);
      2: Result := TFunctions.CmpUInt64(Data1.Client.Entry.BytesReceived, Data2.Client.Entry.BytesReceived);
      3: Result := TFunctions.CmpInt(Data1.Client.Entry.SongsSaved, Data2.Client.Entry.SongsSaved);
      4: Result := TFunctions.CmpInt(Data1.Client.Speed, Data2.Client.Speed);
      5: Result := TFunctions.CmpInt(Integer(Data1.Client.State), Integer(Data2.Client.State), True);
    end else if (Data1.Category <> nil) and (Data2.Category <> nil) then
    if Column = 0 then
      Result := CompareText(Data1.Category.Name, Data2.Category.Name);
end;

procedure TMClientView.DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);

  procedure UnkillCategory(Node: PVirtualNode);
  var
    NodeData: PClientNodeData;
  begin
    if Node <> nil then
    begin
      NodeData := GetNodeData(Node);
      if NodeData.Category <> nil then
        NodeData.Category.Killed := False;
    end;
  end;

var
  Attachmode: TVTNodeAttachMode = amInsertAfter;
  i, n: Integer;
  S: string;
  URLs: TStringArray;
  HitNodeData, DragNodeData: PClientNodeData;
  HI: THitInfo;
  R: TRect;
begin
  if not Assigned(DataObject) then
    Exit;

  Effect := DROPEFFECT_COPY;
  HitNodeData := nil;

  GetHitTestInfoAt(Pt.X, Pt.Y, True, HI);
  if HI.HitNode <> nil then
  begin
    HitNodeData := GetNodeData(HI.HitNode);
    R := GetDisplayRect(HI.HitNode, 0, False);

    if Pt.Y > R.Bottom - FDragTreshold then
      AttachMode := amInsertAfter
    else if Pt.Y < R.Top + FDragTreshold then
      AttachMode := amInsertBefore
    else
      AttachMode := amNoWhere;
  end;

  if Length(FDragNodes) > 0 then
  begin
    if (HI.HitNode <> nil) and (HitNodeData <> nil) then
    begin
      if (HitNodeData.Client = nil) and (((Attachmode = amInsertAfter) and Expanded[HI.HitNode]) or (Attachmode = amNoWhere)) then
      begin
        for i := 0 to Length(FDragNodes) - 1 do
        begin
          DragNodeData := GetNodeData(FDragNodes[i]);
          if DragNodeData.Category = nil then
            MoveTo(FDragNodes[i], HI.HitNode, amAddChildLast, False)
          else
            MoveTo(FDragNodes[i], HI.HitNode, amInsertAfter, False);
          UnkillCategory(HI.HitNode);
        end;
      end else
      begin
        if (HI.HitNode <> nil) and Expanded[HI.HitNode] and (Attachmode <> amInsertBefore) then
          Attachmode := amAddChildLast;
        if AttachMode = amNoWhere then
          AttachMode := amInsertAfter;
        for i := 0 to Length(FDragNodes) - 1 do
        begin
          DragNodeData := GetNodeData(FDragNodes[i]);
          if (DragNodeData.Category <> nil) then
            if GetNodeLevel(HI.HitNode) > 0 then
            begin
              HI.HitNode := HI.HitNode.Parent;
              Attachmode := amInsertAfter;
            end;
          MoveTo(FDragNodes[i], HI.HitNode, Attachmode, False);
          UnkillCategory(HI.HitNode);
        end;
      end;
    end else
      // Nodes ins "nichts" gedraggt
      for i := 0 to Length(FDragNodes) - 1 do
        MoveTo(FDragNodes[i], RootNode, amAddChildLast, False);
  end else if Length(FBrowser.DraggedStreams) > 0 then
  begin
    for i := 0 to High(FBrowser.DraggedStreams) do
    begin
      // Das hier ist das selbe wie hier drunter, nur mit anderer URL/RegEx...
      if ((HI.HitNode <> nil) and (HitNodeData.Client = nil) and (Attachmode = amInsertAfter) and Expanded[HI.HitNode]) or (Attachmode = amNoWhere) then
        if (HitNodeData <> nil) and (HitNodeData.Client <> nil) then
          OnStartStreaming(Self, FBrowser.DraggedStreams[i].ID, FBrowser.DraggedStreams[i].Bitrate, FBrowser.DraggedStreams[i].Name,
            FBrowser.DraggedStreams[i].URL, FBrowser.DraggedStreams[i].URLs, FBrowser.DraggedStreams[i].RegExes, FBrowser.DraggedStreams[i].IgnoreTitles,
            HI.HitNode, amInsertAfter)
        else
          OnStartStreaming(Self, FBrowser.DraggedStreams[i].ID, FBrowser.DraggedStreams[i].Bitrate, FBrowser.DraggedStreams[i].Name,
            FBrowser.DraggedStreams[i].URL, FBrowser.DraggedStreams[i].URLs, FBrowser.DraggedStreams[i].RegExes, FBrowser.DraggedStreams[i].IgnoreTitles,
            HI.HitNode, amAddChildLast)
      else
      begin
        if (HI.HitNode <> nil) and Expanded[HI.HitNode] and (Attachmode <> amInsertBefore) then
          Attachmode := amAddChildLast;
        if AttachMode = amNoWhere then
          AttachMode := amInsertAfter;
        OnStartStreaming(Self, FBrowser.DraggedStreams[i].ID, FBrowser.DraggedStreams[i].Bitrate, FBrowser.DraggedStreams[i].Name,
          FBrowser.DraggedStreams[i].URL, FBrowser.DraggedStreams[i].URLs, FBrowser.DraggedStreams[i].RegExes, FBrowser.DraggedStreams[i].IgnoreTitles,
          HI.HitNode, Attachmode);
      end;
      UnkillCategory(HI.HitNode);
    end;
  end else if TFunctions.ReadDataObjectText(DataObject, S) and TFunctions.FilterHTTPUrls(S, URLs) then
  begin
    for S in URLs do
      // Das selbe wie im Kommentar oben beschrieben...
      if ((HI.HitNode <> nil) and (HitNodeData.Client = nil) and (Attachmode = amInsertAfter) and Expanded[HI.HitNode]) or ((Attachmode = amNoWhere) and (HI.HitNode <> nil) and (HitNodeData.Client = nil)) then
        OnStartStreaming(Self, 0, 0, '', S, nil, nil, nil, HI.HitNode, amAddChildLast)
      else
      begin
        if (HI.HitNode <> nil) and Expanded[HI.HitNode] and (Attachmode <> amInsertBefore) then
          Attachmode := amAddChildLast;
        if AttachMode = amNoWhere then
          AttachMode := amInsertAfter;
        OnStartStreaming(Self, 0, 0, '', S, nil, nil, nil, HI.HitNode, Attachmode);
      end;
    UnkillCategory(HI.HitNode);
  end;
end;

procedure TMClientView.DoDragging(P: TPoint);
var
  i: Integer;
  Entries: TPlaylistEntryArray = [];
  Client: TICEClient;
  Clients: TClientArray;
  Node: PVirtualNode;
  Nodes: TNodeArray;
begin
  if FDragSource.DragInProgress then
    Exit;

  // Raus, wenn nichts markiert ist oder von beiden etwas...
  if ((Length(GetNodes(ntCategory, True)) = 0) and (Length(GetNodes(ntClient, True)) = 0)) or ((Length(GetNodes(ntCategory, True)) > 0) and (Length(GetNodes(ntClient, True)) > 0)) then
    Exit;

  SetLength(FDragNodes, 0);
  FDragSource.Files.Clear;

  Clients := NodesToClients(GetNodes(ntClient, True));
  if Length(Clients) > 0 then
  begin
    for Client in Clients do
    begin
      if Client.AutoRemove then
        Exit;
      SetLength(FDragNodes, Length(FDragNodes) + 1);
      FDragNodes[High(FDragNodes)] := GetClientNode(Client);
    end;

    case AppGlobals.DefaultAction of
      caStream:
        Entries := GetEntries(etStream);
    end;

    if Length(Entries) = 0 then
      Entries := GetEntries(etStream);

    for i := 0 to Length(Entries) - 1 do
      FDragSource.Files.Add(SecureSWURLToInsecure(Entries[i].URL));

    if FDragSource.Files.Count = 0 then
      Exit;
  end else
  begin
    Nodes := GetNodes(ntCategory, True);
    for Node in Nodes do
    begin
      SetLength(FDragNodes, Length(FDragNodes) + 1);
      FDragNodes[High(FDragNodes)] := Node;
    end;
  end;

  DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending]);

  FDragSource.Execute(False);

  SetLength(FDragNodes, 0);
end;

function TMClientView.GetEntries(T: TEntryTypes): TPlaylistEntryArray;
var
  Add: Boolean;
  Name, URL: string;
  Clients: TClientArray;
  Client: TICEClient;
begin
  SetLength(Result, 0);
  Clients := NodesToClients(GetNodes(ntClient, True));
  for Client in Clients do
  begin
    Add := True;
    if Client.Entry.Name = '' then
      Name := Client.Entry.StartURL
    else
      Name := Client.Entry.Name;

    if (T = etFile) and (Client.Filename = '') then
      Add := False;

    if Add then
    begin
      case T of
        etStream: URL := Client.Entry.StartURL;
        etFile: URL := Client.Filename;
      end;

      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].URL := URL;
      Result[High(Result)].Name := Name;
    end;
  end;
end;

end.
