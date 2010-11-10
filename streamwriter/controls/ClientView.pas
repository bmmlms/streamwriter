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
unit ClientView;

interface

uses
  Windows, SysUtils, Classes, Messages, ComCtrls, ActiveX, Controls, Buttons,
  StdCtrls, Menus, ImgList, Math, ICEClient, VirtualTrees, LanguageObjects,
  Graphics, DragDrop, DragDropFile, Functions, AppData, Tabs;

type
  TAccessCanvas = class(TCanvas);

  TMClientView = class;

  TClientArray = array of TICEClient;

  TClientNodeData = record
    Client: TICEClient;
  end;
  PClientNodeData = ^TClientNodeData;

  TEntryTypes = (etStream, {etRelay,} etFile);

  TNodeDataArray = array of PClientNodeData;

  TMClientView = class(TVirtualStringTree)
  private
    FPopupMenu: TPopupMenu;
    FDragSource: TDropFileSource;

    FSortColumn: Integer;
    FSortDirection: TSortDirection;

    FColName: TVirtualTreeColumn;
    FColTitle: TVirtualTreeColumn;
    FColRcvd: TVirtualTreeColumn;
    FColSongs: TVirtualTreeColumn;
    FColSpeed: TVirtualTreeColumn;
    FColStatus: TVirtualTreeColumn;

    procedure FitColumns;
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
  public
    constructor Create(AOwner: TComponent; PopupMenu: TPopupMenu); reintroduce;
    destructor Destroy; override;

    function AddClient(Client: TICEClient): PVirtualNode;
    function RefreshClient(Client: TICEClient): Boolean;
    function GetClientNodeData(Client: TICEClient): PClientNodeData;
    procedure RemoveClient(Client: TICEClient);
    procedure SortItems;

    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    function NodesToData(Nodes: TNodeArray): TNodeDataArray;
    function NodesToClients(Nodes: TNodeArray): TClientArray;
    function GetEntries(T: TEntryTypes): TPlaylistEntryArray;
  end;

implementation

{ TMStreamView }

function TMClientView.AddClient(Client: TICEClient): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Client := Client;
  Result := Node;
end;

constructor TMClientView.Create(AOwner: TComponent; PopupMenu: TPopupMenu);
begin
  inherited Create(AOwner);

  NodeDataSize := SizeOf(TClientNodeData);
  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.Options := Header.Options + [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 1;
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FPopupMenu := PopupMenu;
  FDragSource := TDropFileSource.Create(Self);

  FSortColumn := 0;
  FSortDirection := sdAscending;

  FColName := Header.Columns.Add;
  FColName.Text := _('Name');
  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Title');
  FColRcvd := Header.Columns.Add;
  FColRcvd.Text := _('Received');
  FColSongs := Header.Columns.Add;
  FColSongs.Text := _('Songs');
  FColSpeed := Header.Columns.Add;
  FColSpeed.Text := _('Speed');
  FColStatus := Header.Columns.Add;
  FColStatus.Text := _('State');
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
  inherited;
  Result := inherited;
  //if ((Kind = ikNormal) or (Kind = ikSelected)) then
  begin
    NodeData := GetNodeData(Node);
    case Column of
      0:
        begin
          case Kind of
            ikState:
              begin
                if NodeData.Client.Recording {and (NodeData.Client.State = csConnected)} then
                  Index := 0
                else
                  Index := 1;
                {
                Index := 0;
                case NodeData.Client.State of
                  csStopped:
                    Index := 1;
                  csIOError:
                    Index := 1;
                end;
                }
              end;
            ikNormal, ikSelected:
              begin
                if NodeData.Client.Playing {and (NodeData.Client.State = csConnected)} then
                  Index := 2;
              end;
          end;
        end;
    end;
  end;
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
  NodeData := PClientNodeData(GetNodeData(Node));
  case Column of
    0:
      if NodeData.Client.StreamName = '' then
        if NodeData.Client.StartURL = '' then
          Text := _('Unknown')
        else
          Text := NodeData.Client.StartURL
      else
        Text := NodeData.Client.StreamName;
    1:
      if NodeData.Client.Title = '' then
        if NodeData.Client.State = csConnected then
          Text := _('Unknown')
        else
          Text := _('')
      else
        Text := NodeData.Client.Title;
    2:
      Text := MakeSize(NodeData.Client.Received);
    3:
      Text := IntToStr(NodeData.Client.SongsSaved);
    4:
      Text := MakeSize(NodeData.Client.Speed) + '/s';
    5:
      case NodeData.Client.State of
        csConnecting:
          Text := _('Connecting...');
        csConnected:
          Text := _('Connected');
        csRetrying:
          Text := _('Retrying...');
        csStopped:
          Text := _('Stopped');
        csStopping:
          Text := _('Stopping...');
        csIOError:
          Text := _('Error creating file');
      end;
  end;
end;

procedure TMClientView.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if FSortColumn <> HitInfo.Column then
    begin
      FSortColumn := HitInfo.Column;
      if (HitInfo.Column <> 0) and (HitInfo.Column <> 1) then
        FSortDirection := sdDescending
      else
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

function TMClientView.GetClientNodeData(
  Client: TICEClient): PClientNodeData;
var
  Nodes: TNodeArray;
  Node: PVirtualNode;
  NodeData: PClientNodeData;
begin
  Result := nil;
  Nodes := GetNodes(False);
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

function TMClientView.GetNodes(SelectedOnly: Boolean): TNodeArray;
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

function TMClientView.RefreshClient(Client: TICEClient): Boolean;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PClientNodeData;
begin
  Result := False;
  Nodes := GetNodes(False);
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
  Nodes := GetNodes(False);
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
begin
  Sort(nil, FSortColumn, FSortDirection);
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
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Column of
    0: Result := CompareText(Data1.Client.StreamName, Data2.Client.StreamName);
    1: Result := CompareText(Data1.Client.Title, Data2.Client.Title);
    2: Result := CmpInt(Data1.Client.Received, Data2.Client.Received);
    3: Result := CmpInt(Data1.Client.SongsSaved, Data2.Client.SongsSaved);
    4: Result := CmpInt(Data1.Client.Speed, Data2.Client.Speed);
    5: Result := CmpIntR(Integer(Data1.Client.State), Integer(Data2.Client.State));
  end;
end;

procedure TMClientView.DoDragging(P: TPoint);
var
  i: Integer;
  //UseRelay: Boolean;
  UseFile: Boolean;
  Entries: TPlaylistEntryArray;
  Client: TICEClient;
  Clients: TClientArray;
begin
  if FDragSource.DragInProgress then
    Exit;

  //UseRelay := AppGlobals.Relay;
  UseFile := True;

  Clients := NodesToClients(GetNodes(True));
  FDragSource.Files.Clear;
  for Client in Clients do
  begin
    //if AppGlobals.Relay then
    //  if not Client.Active then
    //    UseRelay := False;
    if not Client.Active then
      UseFile := False;
  end;

  SetLength(Entries, 0);

  case AppGlobals.DefaultAction of
    //caStartStop:
      //if UseRelay then
      //  Entries := GetEntries(etRelay);
    caStream:
      Entries := GetEntries(etStream);
    //caRelay:
    //  if UseRelay then
    //    Entries := GetEntries(etRelay);
    caFile:
      if UseFile then
        Entries := GetEntries(etFile);
  end;

  if Length(Entries) = 0 then
    Entries := GetEntries(etStream);

  for i := 0 to Length(Entries) - 1 do
    FDragSource.Files.Add(Entries[i].URL);

  if FDragSource.Files.Count = 0 then
    Exit;

  DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending]);
  FDragSource.Execute(True);
end;

function TMClientView.GetEntries(T: TEntryTypes): TPlaylistEntryArray;
var
  Add: Boolean;
  Name, URL: string;
  Clients: TClientArray;
  Client: TICEClient;
begin
  SetLength(Result, 0);
  Clients := NodesToClients(GetNodes(True));
  for Client in Clients do
  begin
    Add := True;
    if Client.StreamName = '' then
      Name := Client.StartURL
    else
      Name := Client.StreamName;

    //if (T = etRelay) and (not Client.Active) then
    //  Add := False;
    if (T = etFile) and (Client.Filename = '') then
      Add := False;

    if Add then
    begin
      case T of
        etStream: URL := Client.StartURL;
        //etRelay: URL := Client.RelayURL;
        etFile: URL := Client.Filename;
      end;

      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].URL := URL;
      Result[High(Result)].Name := Name;
    end;
  end;
end;

end.

