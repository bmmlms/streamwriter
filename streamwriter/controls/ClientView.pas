{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
  Windows, SysUtils, Classes, Messages, ComCtrls, ActiveX, Controls, Buttons,
  StdCtrls, Menus, ImgList, Math, ICEClient, VirtualTrees, LanguageObjects,
  Graphics, DragDrop, DragDropFile, Functions, AppData, Tabs, DropComboTarget,
  DropSource, ShlObj, ComObj, ShellAPI, DataManager, StreamBrowserView,
  Logging, PngImage, SharedControls;

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

  TStartStreamingEvent = procedure(Sender: TObject; ID, Bitrate: Cardinal; Name, URL, TitlePattern: string;
    IgnoreTitles: TStringList; Node: PVirtualNode; Mode: TVTNodeAttachMode) of object;

  TMClientView = class(TVirtualStringTree)
  private
    FBrowser: TMStreamTree;

    FPopupMenu: TPopupMenu;
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

    FOnStartStreaming: TStartStreamingEvent;

    procedure FitColumns;
    procedure MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoDragging(P: TPoint); override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
      var Effect: Integer; Mode: TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer): Boolean; override;
    procedure DoEdit; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    function DoEndEdit: Boolean; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: UnicodeString); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo;
      ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
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

    procedure Translate;

    procedure MoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode; ChildrenOnly: Boolean); reintroduce;

    property AutoNode: PVirtualNode read FAutoNode write FAutoNode;
    property OnStartStreaming: TStartStreamingEvent read FOnStartStreaming write FOnStartStreaming;
  end;

implementation

uses
  ClientTab;

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
  begin
    FAutoNode := Node;
  end;
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
  begin
    MoveTo(Node, FAutoNode, amAddChildLast, False);
  end;

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
  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowDropmark, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toAcceptOLEDrop, toEditable];
  Header.Options := Header.Options + [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 1;
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FPopupMenu := PopupMenu;
  FDragSource := TDropFileSource.Create(Self);

  FColName := Header.Columns.Add;
  FColName.Text := _('Name');
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
  begin
    if not ((AppGlobals.ClientCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];
  end;

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

function TMClientView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
var
  NodeData: PClientNodeData;
begin
  Result := inherited;

  if Kind = ikOverlay then
    Exit;

  NodeData := GetNodeData(Node);
  if NodeData.Client <> nil then
    case Column of
      0:
        begin
          if NodeData.Client.Playing and NodeData.Client.Paused and NodeData.Client.Recording then
            Index := 5
          else if NodeData.Client.Playing and NodeData.Client.Recording then
            Index := 2
          else if NodeData.Client.Recording then
            Index := 0
          else if NodeData.Client.Playing and NodeData.Client.Paused then
            Index := 4
          else if NodeData.Client.Playing then
            Index := 1
          else
            Index := 3;
        end;
    end
  else if Column = 0 then
    if NodeData.Category.IsAuto then
      Index := 7
    else
      Index := 6;
end;

function TMClientView.DoGetNodeTooltip(Node: PVirtualNode;
  Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString;
var
  Text: UnicodeString;
begin
  Text := '';
  DoGetText(Node, Column, ttNormal, Text);
  Result := Text;
end;

procedure TMClientView.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
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
        if NodeData.Client.Title = '' then
          if (NodeData.Client.State = csConnected) or (NodeData.Client.State = csConnecting) then
            Text := _('Unknown')
          else
            Text := ''
        else
          Text := NodeData.Client.Title;
      2:
        Text := MakeSize(NodeData.Client.Entry.BytesReceived);
      3:
        if NodeData.Client.AutoRemove then
          Text := ''
        else
          Text := IntToStr(NodeData.Client.Entry.SongsSaved);
      4:
        Text := MakeSize(NodeData.Client.Speed) + '/s';
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
    end
  end else
    if Column = 0 then
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
    end else
    begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end;
    Sort(nil, HitInfo.Column, Header.SortDirection);
    Nodes := GetNodes(ntCategory, False);
    for i := 0 to Length(Nodes) - 1 do
      Sort(Nodes[i], Header.SortColumn, Header.SortDirection);
  end;
end;

function TMClientView.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  s, NodeText: string;
  NodeData: PClientNodeData;
begin
  Result := 0;
  S := Text;
  NodeData := GetNodeData(Node);
  if NodeData = nil then
    Exit;
  DoGetText(Node, 0, ttNormal, NodeText);
  Result := StrLIComp(PChar(s), PChar(NodeText), Min(Length(s), Length(NodeText)));
end;

procedure TMClientView.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  Text: UnicodeString);
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

procedure TMClientView.FitColumns;
  function GetTextWidth(Text: string): Integer;
  var
    Canvas: TAccessCanvas;
  begin
    Canvas := TAccessCanvas.Create;
    try
      Canvas.Handle := GetDC(GetDesktopWindow);
      SelectObject(Canvas.Handle, Header.Font.Handle);
      Result := Canvas.TextWidth(Text) + 20;
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    finally
      Canvas.Free;
    end;
  end;
begin
  FColName.Width := 120;
  FColStatus.Width := 100;
  FColRcvd.Width := GetTextWidth(FColRcvd.Text);
  FColSpeed.Width := Max(GetTextWidth('11,11KB/s'), GetTextWidth(FColSpeed.Text));
  FColSongs.Width := GetTextWidth(FColSongs.Text);
end;

function TMClientView.DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer): Boolean;
var
  i, n: Integer;
  Children: TNodeArray;
  HitNode: PVirtualNode;
  NodeData, ParentNodeData: PClientNodeData;
  R: TRect;
begin
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
          begin
            Result := False;
            Exit;
          end;
        end else
        begin
          if (not (Pt.Y > R.Bottom - FDragTreshold)) and (not (Pt.Y < R.Top + FDragTreshold)) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end else
        if (not (Pt.Y > R.Bottom - FDragTreshold)) and (not (Pt.Y < R.Top + FDragTreshold)) then
        begin
          // Man darf in die automatische Kategorie nix reindraggen
          Result := False;
          Exit;
        end;
    end;

    if (NodeData.Client <> nil) and (GetNodeLevel(HitNode) > 0) then
    begin
      ParentNodeData := GetNodeData(HitNode.Parent);
      if ParentNodeData.Category <> nil then
      begin
        if ParentNodeData.Category.IsAuto then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;

    // Drop darf nur erlaubt sein, wenn Ziel-Node nicht in gedraggten
    // Nodes vorkommt und Ziel-Node kein Kind von Drag-Node ist
    if Length(FDragNodes) > 0 then
    begin
      // Wir sind im Tree am draggen
      for i := 0 to Length(FDragNodes) - 1 do
      begin
        if HitNode = FDragNodes[i] then
        begin
          Result := False;
          Break;
        end;

        Children := GetNodes(ntClient, False);
        for n := 0 to Length(Children) - 1 do
          if (Children[n] = HitNode) and (HitNode.Parent = FDragNodes[i]) then
          begin
            Result := False;
            Exit;
          end;
      end;
    end else
    begin
      // Drag von wo anders (Browser, Streambrowser)

    end;
  end;
end;

procedure TMClientView.DoEdit;
var
  P: Integer;
  Edit: TVTEdit;
begin
  inherited;

  if (EditLink <> nil) and (EditLink is TStringEditLink) then
  begin
    Edit := TStringEditLink(EditLink).Edit;
    P := Pos('(', Edit.Text);
    if P > 0 then
    begin
      Edit.Text := Copy(Edit.Text, 1, P - 2);
      Edit.SelectAll;
    end;
  end;
end;

function TMClientView.DoEndEdit: Boolean;
begin
  Result := inherited;
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

    if ((NodeTypes = ntClient) and (NodeData.Client = nil)) or
       (((NodeTypes = ntClientNoAuto) and (NodeData.Client = nil)) or ((NodeTypes = ntClientNoAuto) and (NodeData.Client <> nil) and (NodeData.Client.AutoRemove))) or
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

procedure TMClientView.MenuColsAction(Sender: TVirtualStringTree; Index: Integer;
  Checked: Boolean);
var
  Show: Boolean;
begin
  Show := True;
  if coVisible in Header.Columns[Index].Options then
    Show := False;

  if Show then
  begin
    Header.Columns[Index].Options := Header.Columns[Index].Options + [coVisible];
  end else
  begin
    Header.Columns[Index].Options := Header.Columns[Index].Options - [coVisible];
  end;

  AppGlobals.ClientCols := AppGlobals.ClientCols xor (1 shl Index);
end;

procedure TMClientView.MoveTo(Source, Target: PVirtualNode;
  Mode: TVTNodeAttachMode; ChildrenOnly: Boolean);
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

procedure TMClientView.Translate;
begin
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

procedure TMClientView.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  NodeData: PClientNodeData;
begin
  inherited;

  NodeData := GetNodeData(PaintInfo.Node);

  if (NodeData.Client <> nil) and (NodeData.Client.Entry.Schedules.Count > 0) then
    Images.Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 8);
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

procedure TMClientView.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var
  NodeData: PClientNodeData;
begin
  inherited;
  NodeData := GetNodeData(Node);
  Allowed := ((NodeData.Client <> nil) and (not NodeData.Client.AutoRemove)) or
             ((NodeData.Category <> nil) and (not NodeData.Category.IsAuto));
end;

function TMClientView.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
  function CmpInt(a, b: Integer): Integer;
  begin
    if a > b then
      Result := 1
    else if a < b then
      Result := -1
    else
      Result := 0;
  end;
  function CmpIntR(a, b: Integer): Integer;
  begin
    if a < b then
      Result := 1
    else if a > b then
      Result := -1
    else
      Result := 0;
  end;
var
  Data1, Data2: PClientNodeData;
  I1, I2: Integer;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  if (Column = -1) and (not FInitialSorted) then
  begin
    // Mit Column -1 heiﬂt nach Programmstart sortieren
    if Data1.Client <> nil then
      I1 := Data1.Client.Entry.Index
    else
      I1 := Data1.Category.Index;

    if Data2.Client <> nil then
      I2 := Data2.Client.Entry.Index
    else
      I2 := Data2.Category.Index;

    Result := CmpInt(I1, I2);
    Exit;
  end;

  if (Data1.Client <> nil) and (Data2.Client <> nil) then
    case Column of
      0: Result := CompareText(Data1.Client.Entry.Name, Data2.Client.Entry.Name);
      1: Result := CompareText(Data1.Client.Title, Data2.Client.Title);
      2: Result := CmpInt(Data1.Client.Entry.BytesReceived, Data2.Client.Entry.BytesReceived);
      3: Result := CmpInt(Data1.Client.Entry.SongsSaved, Data2.Client.Entry.SongsSaved);
      4: Result := CmpInt(Data1.Client.Speed, Data2.Client.Speed);
      5: Result := CmpIntR(Integer(Data1.Client.State), Integer(Data2.Client.State));
    end
  else if (Data1.Category <> nil) and (Data2.Category <> nil) then
    if Column = 0 then
      Result := CompareText(Data1.Category.Name, Data2.Category.Name);
end;

function GetFileListFromObj(const DataObj: IDataObject;
  const FileList: TStrings): Boolean;
var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  FileName: string;
  i, DroppedFileCount, FileNameLength: Integer;
begin
  Result := False;
  try
    FormatEtc.cfFormat := CF_HDROP;
    FormatEtc.ptd := nil;
    FormatEtc.dwAspect := DVASPECT_CONTENT;
    FormatEtc.lindex := -1;
    FormatEtc.tymed := TYMED_HGLOBAL;
    OleCheck(DataObj.GetData(FormatEtc, Medium));
    try
      try
        DroppedFileCount := DragQueryFile(Medium.hGlobal, $FFFFFFFF, nil, 0);
        for i := 0 to Pred(DroppedFileCount) do
        begin
          FileNameLength := DragQueryFile(Medium.hGlobal, i, nil, 0);
          SetLength(FileName, FileNameLength);
          DragQueryFile(Medium.hGlobal, i, PChar(FileName), FileNameLength + 1);
          FileList.Add(FileName);
        end;
      finally
        DragFinish(Medium.hGlobal);
      end;
    finally
      ReleaseStgMedium(Medium);
    end;
    Result := FileList.Count > 0;
  except end;
end;

function GetWideStringFromObj(const DataObject: IDataObject; var S: string): Boolean;
var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  OLEData,
  Head: PWideChar;
  Chars: Integer;
begin
  S := '';

  FormatEtc.cfFormat := CF_UNICODETEXT;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;

  if DataObject.QueryGetData(FormatEtc) = S_OK then
  begin
    if DataObject.GetData(FormatEtc, Medium) = S_OK then
    begin
      OLEData := GlobalLock(Medium.hGlobal);
      if Assigned(OLEData) then
      begin
        Chars := 0;
        Head := OLEData;
        try
          while Head^ <> #0 do
          begin
            Head := Pointer(Integer(Head) + SizeOf(WideChar));
            Inc(Chars);
          end;

          SetString(S, OLEData, Chars);
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      end;
      ReleaseStgMedium(Medium);
    end;
  end;
  Result := S <> '';
end;

procedure TMClientView.DoDragDrop(Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
  var Effect: Integer; Mode: TDropMode);
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
  Attachmode: TVTNodeAttachMode;
  Nodes: TNodeArray;
  i, n: Integer;
  Files: TStringList;
  DropURL: string;
  HitNodeData, DragNodeData: PClientNodeData;
  HI: THitInfo;
  R: TRect;
begin
  inherited;

  Nodes := nil;
  Attachmode := amInsertAfter;
  Effect := DROPEFFECT_COPY;
  HitNodeData := nil;

  GetHitTestInfoAt(Pt.X, Pt.Y, True, HI);
  if Hi.HitNode <> nil then
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

  if DataObject <> nil then
  begin
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
      begin
        // Nodes ins "nichts" gedraggt
        for i := 0 to Length(FDragNodes) - 1 do
          MoveTo(FDragNodes[i], RootNode, amAddChildLast, False);
      end;
      Exit;
    end;

    if Length(FBrowser.DraggedStreams) > 0 then
    begin
      for i := 0 to High(FBrowser.DraggedStreams) do
      begin
        // Das hier ist das selbe wie hier drunter, nur mit anderer URL/RegEx...
        if ((HI.HitNode <> nil) and (HitNodeData.Client = nil) and (Attachmode = amInsertAfter) and Expanded[HI.HitNode]) or (Attachmode = amNoWhere) then
          if (HitNodeData <> nil) and (HitNodeData.Client <> nil) then
            OnStartStreaming(Self, FBrowser.DraggedStreams[i].ID, FBrowser.DraggedStreams[i].Bitrate, FBrowser.DraggedStreams[i].Name,
              FBrowser.DraggedStreams[i].URL, FBrowser.DraggedStreams[i].RegEx, FBrowser.DraggedStreams[i].IgnoreTitles, HI.HitNode, amInsertAfter)
          else
            OnStartStreaming(Self, FBrowser.DraggedStreams[i].ID, FBrowser.DraggedStreams[i].Bitrate, FBrowser.DraggedStreams[i].Name,
              FBrowser.DraggedStreams[i].URL, FBrowser.DraggedStreams[i].RegEx, FBrowser.DraggedStreams[i].IgnoreTitles, HI.HitNode, amAddChildLast)
        else
        begin
          if (HI.HitNode <> nil) and Expanded[HI.HitNode] and (Attachmode <> amInsertBefore) then
            Attachmode := amAddChildLast;
          if AttachMode = amNoWhere then
            AttachMode := amInsertAfter;
          OnStartStreaming(Self, FBrowser.DraggedStreams[i].ID, FBrowser.DraggedStreams[i].Bitrate, FBrowser.DraggedStreams[i].Name,
            FBrowser.DraggedStreams[i].URL, FBrowser.DraggedStreams[i].RegEx, FBrowser.DraggedStreams[i].IgnoreTitles, HI.HitNode, Attachmode);
        end;
        UnkillCategory(HI.HitNode);
      end;
    end else
    begin
      Files := TStringList.Create;
      try
        for n := 0 to High(Formats) do
        begin
          case Formats[n] of
            CF_UNICODETEXT:
              begin
                if GetWideStringFromObj(DataObject, DropURL) then
                begin
                  Files.Add(DropURL);
                  Break;
                end;
              end;
          end;
        end;

        if Files.Count = 0 then
          GetFileListFromObj(DataObject, Files);

        for i := 0 to Files.Count - 1 do
          if (Files[i] <> '') then
            // Das selbe wie im Kommentar oben beschrieben...
            if ((HI.HitNode <> nil) and (HitNodeData.Client = nil) and (Attachmode = amInsertAfter) and Expanded[HI.HitNode]) or (Attachmode = amNoWhere) then
              OnStartStreaming(Self, 0, 0, '', Files[i], '', nil, HI.HitNode, amAddChildLast)
            else
            begin
              if (HI.HitNode <> nil) and Expanded[HI.HitNode] and (Attachmode <> amInsertBefore) then
                Attachmode := amAddChildLast;
              if AttachMode = amNoWhere then
                AttachMode := amInsertAfter;
              OnStartStreaming(Self, 0, 0, '', Files[i], '', nil, HI.HitNode, Attachmode);
            end;
            UnkillCategory(HI.HitNode);
      finally
        Files.Free;
      end;
    end;
  end;
end;

procedure TMClientView.DoDragging(P: TPoint);
var
  i: Integer;
  UseFile: Boolean;
  Entries: TPlaylistEntryArray;
  Client: TICEClient;
  Clients: TClientArray;
  Node: PVirtualNode;
  Nodes: TNodeArray;
begin
  if FDragSource.DragInProgress then
    Exit;

  if ((Length(GetNodes(ntCategory, True)) = 0) and (Length(GetNodes(ntClient, True)) = 0)) or
     ((Length(GetNodes(ntCategory, True)) > 0) and (Length(GetNodes(ntClient, True)) > 0)) then
  begin
    // Raus, wenn nichts markiert ist oder von beiden etwas...
    Exit;
  end;

  UseFile := True;

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
      if not Client.Active then
        UseFile := False;
    end;

    SetLength(Entries, 0);

    case AppGlobals.DefaultAction of
      caStream:
        Entries := GetEntries(etStream);
      caFile:
        if UseFile then
          Entries := GetEntries(etFile);
    end;

    if Length(Entries) = 0 then
      Entries := GetEntries(etStream);

    for i := 0 to Length(Entries) - 1 do
      FDragSource.Files.Add(AnsiString(Entries[i].URL));

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

