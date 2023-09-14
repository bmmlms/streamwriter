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
  MStringFunctions,
  PlaylistHandler,
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

  TStartStreamingEvent = procedure(Sender: TObject; ID, Bitrate: Cardinal; Name, URL: string; URLs, RegExes, IgnoreTitles: TStringList; AddOnly: Boolean; Node: PVirtualNode; Mode: TVTNodeAttachMode) of object;

  { TMClientView }

  TMClientView = class(TMSWVirtualTree)
  private
    FBrowser: TMStreamTree;

    FDragSource: TDropFileSource;
    FDragNodes: TNodeArray;
    FAutoNode: PVirtualNode;

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
    function GetDropTarget(const Pt: TPoint; out TargetNode: PVirtualNode; out AttachMode: TVTNodeAttachMode): Boolean;
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
  ParentNode: PVirtualNode;
  NodeData: PClientNodeData;
  Nodes: TNodeArray;
begin
  ParentNode := nil;
  Nodes := GetNodes(ntAll, True);
  for Node in Nodes do
    if GetNodeLevel(Node) = 0 then
      ParentNode := Node
    else
      ParentNode := Node.Parent;

  UnselectNodes(GetFirst, GetLast);

  Node := InsertNode(ParentNode, amInsertAfter);
  NodeData := GetNodeData(Node);
  NodeData.Client := nil;
  NodeData.Category := TListCategory.Create(_('New category'), 0);

  Selected[Node] := True;

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
  Nodes: TNodeArray;
  Node: PVirtualNode;
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
    for Node in Nodes do
      Sort(Node, Header.SortColumn, Header.SortDirection);
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
        PaintInfo.Canvas.Font.Color := clHighlight;
    end else if NodeData.Category <> nil then
    begin
      Node := GetFirstChild(PaintInfo.Node);
      while Node <> nil do
      begin
        NodeData := GetNodeData(Node);

        if NodeData.Client.Playing or NodeData.Client.Paused then
        begin
          PaintInfo.Canvas.Font.Color := clHighlight;
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
    FColRcvd.FitColumn('111,11 KB');
    FColSpeed.FitColumn('11,11 KB/s');
    FColSongs.FitColumn;
    FColStatus.FitColumn(_('Stopped'));
    FColName.Width := Scale96ToFont(150);
  end;

  if AppGlobals.ClientHeaderPositionLoaded then
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.ClientHeaderPosition[i];
end;

function TMClientView.DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: LongWord): Boolean;
var
  S: string;
  Node: PVirtualNode;
  Attachmode: TVTNodeAttachMode;
  URLs, Files, OutFiles: TStringArray;
begin
  if not inherited DoDragOver(Source, Shift, State, Pt, Mode, Effect) then
    Exit(False);

  if (Length(FDragNodes) = 0) and (Length(FBrowser.DraggedStreams) = 0) and (not (TFunctions.ReadDataObjectText(VTVDragManager.DataObject, S) and TFunctions.FilterHTTPUrls(S, URLs))) and
    (not (TFunctions.ReadDataObjectFiles(VTVDragManager.DataObject, Files) and TFunctions.FilterEndsWith(Files, ['.m3u', '.m3u8', '.pls'], OutFiles)))
  then
    Exit(False);

  Exit(GetDropTarget(Pt, Node, AttachMode));
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
  Result := [];
  Node := GetFirst;
  while Node <> nil do
  begin
    if Node.Parent = Parent then
      Result += [Node];

    Node := GetNext(Node);
  end;
end;

function TMClientView.GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Result := [];
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

    Result += [Node];

    Node := GetNext(Node);
  end;
end;

procedure TMClientView.KeyDown(var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
  Nodes: TNodeArray;
begin
  if Key = VK_SPACE then
  begin
    Key := 0;

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
        Break;
      end;
    end;
  end;

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

function TMClientView.GetDropTarget(const Pt: TPoint; out TargetNode: PVirtualNode; out AttachMode: TVTNodeAttachMode): Boolean;
var
  DragNode: PVirtualNode;
  TargetNodeData: PClientNodeData = nil;
  TargetParentNodeData: PClientNodeData = nil;
  DraggingCategories: Boolean = False;
begin
  TargetNode := GetNodeAt(Pt);
  if Assigned(TargetNode) then
  begin
    TargetNodeData := GetNodeData(TargetNode);
    TargetParentNodeData := GetNodeData(TargetNode.Parent);
  end;

  for DragNode in FDragNodes do
    if Assigned(PClientNodeData(GetNodeData(DragNode)).Category) then
    begin
      DraggingCategories := True;
      Break;
    end;

  if not Assigned(TargetNode) then
  begin
    TargetNode := RootNode;
    AttachMode := amAddChildLast;
    Exit(True);
  end else if LastDropMode = dmAbove then
    AttachMode := amInsertBefore
  else if LastDropMode = dmBelow then
    AttachMode := IfThen<TVTNodeAttachMode>(Expanded[TargetNode], amAddChildFirst, amInsertAfter)
  else
    AttachMode := IfThen<TVTNodeAttachMode>(Assigned(TargetNodeData.Category), amAddChildLast, amNoWhere);

  if AttachMode = amNoWhere then
    Exit(False);

  // When dropping below the last automatic recording modify target node
  // When dropping categories below the last stream in a category modify target node
  if AttachMode = amInsertAfter then
    if (Assigned(TargetNodeData.Client) and Assigned(TargetParentNodeData) and Assigned(TargetParentNodeData.Category) and TargetParentNodeData.Category.IsAuto and not Assigned(GetNextSibling(TargetNode)))
      or (DraggingCategories and Assigned(TargetNodeData.Client) and Assigned(TargetParentNodeData) and Assigned(TargetParentNodeData.Category) and not Assigned(GetNextSibling(TargetNode))) then
    begin
      TargetNode := TargetNode.Parent;
      TargetNodeData := GetNodeData(TargetNode);
      TargetParentNodeData := GetNodeData(TargetNode.Parent);
    end;

  // Deny adding categories into categories
  if ((AttachMode in [amAddChildFirst, amAddChildLast]) or (GetNodeLevel(TargetNode) > 0)) and DraggingCategories then
    Exit(False);

  // Deny adding children to automatic recording category
  if (AttachMode in [amAddChildFirst, amAddChildLast]) and Assigned(TargetNodeData.Category) and TargetNodeData.Category.IsAuto then
    Exit(False);

  // Deny inserting before/after automatic recording stream
  if (AttachMode = amInsertBefore) and Assigned(TargetNodeData.Client) and Assigned(TargetParentNodeData) and Assigned(TargetParentNodeData.Category) and TargetParentNodeData.Category.IsAuto then
    Exit(False);

  // Deny inserting after automatic recording stream if next stream is automatic as well
  if (AttachMode = amInsertAfter) and Assigned(TargetNodeData.Client) and Assigned(TargetParentNodeData) and Assigned(TargetParentNodeData.Category) and TargetParentNodeData.Category.IsAuto
    and Assigned(GetNextSibling(TargetNode)) and (GetNodeLevel(GetNextSibling(TargetNode)) > 0) then
    Exit(False);

  // Drop darf nur erlaubt sein, wenn Ziel-Node nicht in gedraggten
  // Nodes vorkommt und Ziel-Node kein Kind von Drag-Node ist
  for DragNode in FDragNodes do
    if (TargetNode = DragNode) or (TargetNode.Parent = DragNode) then
      Exit(False);

  Exit(True);
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
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  Node: PVirtualNode;
begin
  Result := False;
  Nodes := GetNodes(ntClient, False);
  for Node in Nodes do
  begin
    NodeData := GetNodeData(Node);
    if NodeData.Client <> Client then
      Continue;

    Result := True;
    InvalidateNode(Node);
    if (Node.Parent <> nil) and (Node.Parent <> RootNode) then
      InvalidateNode(Node.Parent);
    Exit;
  end;
end;

procedure TMClientView.RemoveClient(Client: TICEClient);
var
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
  Node: PVirtualNode;
begin
  Nodes := GetNodes(ntClient, False);
  for Node in Nodes do
  begin
    NodeData := GetNodeData(Node);
    if NodeData.Client <> Client then
      Continue;

    DeleteNode(Node);
    Exit;
  end;
end;

procedure TMClientView.SortItems;
var
  Nodes: TNodeArray;
  Node: PVirtualNode;
begin
  Sort(nil, -1, sdAscending);
  Nodes := GetNodes(ntCategory, False);
  for Node in Nodes do
    Sort(Node, -1, sdAscending);
  FInitialSorted := True;
end;

procedure TMClientView.PostTranslate;
var
  NodeData: PClientNodeData;
begin
  inherited;

  if Assigned(FAutoNode) then
  begin
    NodeData := GetNodeData(FAutoNode);
    NodeData.Category.Name := _('Automatic recordings');
  end;

  FColName.Text := _('Name');
  FColTitle.Text := _('Title');
  FColRcvd.Text := _('Received');
  FColSongs.Text := _('Songs');
  FColSpeed.Text := _('Speed');
  FColStatus.Text := _('State');
end;

function TMClientView.NodesToData(Nodes: TNodeArray): TNodeDataArray;
var
  Data: PClientNodeData;
  Node: PVirtualNode;
begin
  Result := [];
  for Node in Nodes do
  begin
    Data := GetNodeData(Node);
    Result += [Data];
  end;
end;

procedure TMClientView.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  NodeData: PClientNodeData;
begin
  inherited;

  NodeData := GetNodeData(PaintInfo.Node);

  if (NodeData.Client <> nil) and (NodeData.Client.Entry.Schedules.Count > 0) then
    Images.ResolutionForPPI[8, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos + Scale96ToFont(8), PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.TIME);
end;

function TMClientView.NodesToClients(Nodes: TNodeArray): TClientArray;
var
  Data: PClientNodeData;
  Node: PVirtualNode;
begin
  Result := [];
  for Node in Nodes do
  begin
    Data := GetNodeData(Node);
    Result += [Data.Client];
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
    NodeData := GetNodeData(Node);
    if Assigned(NodeData) and Assigned(NodeData.Category) then
      NodeData.Category.Killed := False;
  end;

var
  AttachMode: TVTNodeAttachMode;
  S, S2: string;
  URLs, Files, OutFiles: TStringArray;
  StreamData: TStreamData;
  Node, TargetNode: PVirtualNode;
  PH: TPlaylistHandler;
begin
  if not Assigned(DataObject) or not GetDropTarget(Pt, TargetNode, AttachMode) then
    Exit;

  // Unkill when adding children or when adding before/after children of category
  if (AttachMode in [amAddChildFirst, amAddChildLast]) or (GetNodeLevel(TargetNode) > 0) then
    UnkillCategory(TargetNode);

  if Length(FDragNodes) > 0 then
  begin
    for Node in FDragNodes do
    begin
      MoveTo(Node, TargetNode, AttachMode, False);

      // Update TargetNode to preserve order of dropped nodes
      if (AttachMode = amAddChildFirst) or (AttachMode = amInsertAfter) then
      begin
        TargetNode := Node;
        AttachMode := amInsertAfter;
      end;
    end;
  end else if Length(FBrowser.DraggedStreams) > 0 then
    for StreamData in FBrowser.DraggedStreams do
      OnStartStreaming(Self, StreamData.ID, StreamData.Bitrate, StreamData.Name, StreamData.URL, StreamData.URLs, StreamData.RegExes, StreamData.IgnoreTitles, False, TargetNode, Attachmode)
  else if TFunctions.ReadDataObjectText(DataObject, S) and TFunctions.FilterHTTPUrls(S, URLs) then
    for S in URLs do
      OnStartStreaming(Self, 0, 0, '', S, nil, nil, nil, True, TargetNode, AttachMode)
  else if TFunctions.ReadDataObjectFiles(DataObject, Files) and TFunctions.FilterEndsWith(Files, ['.m3u', '.m3u8', '.pls'], OutFiles) then
  begin
    PH := TPlaylistHandler.Create;
    try
      for S in OutFiles do
        try
          if not PH.ParsePlaylist(S) then
            Continue;

          for S2 in PH.URLs do
            OnStartStreaming(Self, 0, 0, '', S2, nil, nil, nil, True, TargetNode, AttachMode);
        except
        end;
    finally
      PH.Free;
    end;
  end;
end;

procedure TMClientView.DoDragging(P: TPoint);
var
  Entries: TPlaylistEntryArray = [];
  Entry: TPlaylistEntry;
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

  FDragNodes := [];
  FDragSource.Files.Clear;

  Clients := NodesToClients(GetNodes(ntClient, True));
  if Length(Clients) > 0 then
  begin
    for Client in Clients do
    begin
      if Client.AutoRemove then
        Exit;

      FDragNodes += [GetClientNode(Client)];
    end;

    case AppGlobals.DefaultAction of
      caStream:
        Entries := GetEntries(etStream);
    end;

    if Length(Entries) = 0 then
      Entries := GetEntries(etStream);

    for Entry in Entries do
      FDragSource.Files.Add(SecureSWURLToInsecure(Entry.URL));

    if FDragSource.Files.Count = 0 then
      Exit;
  end else
  begin
    Nodes := GetNodes(ntCategory, True);
    for Node in Nodes do
      FDragNodes += [Node];
  end;

  DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending]);

  FDragSource.Execute(False);

  FDragNodes := [];
end;

function TMClientView.GetEntries(T: TEntryTypes): TPlaylistEntryArray;
var
  Add: Boolean;
  Name, URL: string;
  Clients: TClientArray;
  Client: TICEClient;
begin
  Result := [];
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
