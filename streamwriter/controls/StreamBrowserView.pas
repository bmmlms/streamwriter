{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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

{ This unit contains everything needed to display the stream-browser }
unit StreamBrowserView;

interface

uses
  Windows, SysUtils, Classes, Messages, ComCtrls, ActiveX, Controls, Buttons,
  StdCtrls, Menus, ImgList, Math, VirtualTrees, LanguageObjects, MControls,
  Graphics, DragDrop, DragDropFile, Functions, AppData, ExtCtrls,
  HomeCommunication, DynBASS, pngimage, PngImageList, Forms, Logging,
  DataManager, DropSource, Types, AudioFunctions, PngSpeedButton,
  Generics.Collections, TypeDefs, MessageBus, AppMessages, Commands,
  GUIFunctions, SharedData;

type
  TModes = (moShow, moLoading, moError);

  TMStreamTree = class;

  TStreamData = record
    ID: Integer;
    Bitrate: Cardinal;
    Name: string;
    URL: string;
    Website: string;
    Rating: Integer;
    RegEx: string;
    RecordingOkay: Boolean;
    IgnoreTitles: TStringList;
  end;
  TStreamDataArray = array of TStreamData;

  TStreamNodeData = record
    Data: TStreamBrowserEntry;
  end;
  PStreamNodeData = ^TStreamNodeData;

  TNeedDataEvent = procedure(Sender: TObject; Offset, Count: Integer) of object;
  TAddStreamEvent = procedure(Sender: TObject; URL, Name: string) of object;
  TActionEvent = procedure(Sender: TObject; Action: TStreamOpenActions; Streams: TStreamDataArray) of object;
  TIsInClientListEvent = function(Sender: TObject; ID: Cardinal): Boolean of object;

  TScrollDirection = (sdUp, sdDown);

  TMStreamSearchPanel = class(TPanel)
  private
    //FShowHideFilters: TMShowHidePanel;

    FSearchLabel: TLabel;
    FGenreLabel: TLabel;
    FKbpsLabel: TLabel;
    FTypeLabel: TLabel;
    FSearchEdit: TEdit;
    FGenreList: TComboBox;
    FKbpsList: TComboBox;
    FTypeList: TComboBox;

    procedure SetVisible(Value: Boolean);

    procedure ExpandButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Setup;
  end;

  TSortTypes = (stName, stBitrate, stType, stRating);

  TMStreamBrowserView = class(TPanel)
  private
    FSearch: TMStreamSearchPanel;
    FStreamTree: TMStreamTree;
    FCountLabel: TLabel;
    FDataLists: TDataLists;
    FMode: TModes;

    FSelectedSortType: TSortTypes;

    FLoading: Boolean;

    FHomeCommunication: THomeCommunication;

    procedure ListsChange(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SortItemClick(Sender: TObject);

    procedure BuildTree(AlwaysBuild: Boolean);
    procedure BuildGenres;
    procedure SortTree;

    procedure StreamBrowserHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

    procedure HomeCommStreamsReceived(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent; DataLists: TDataLists); reintroduce;
    destructor Destroy; override;

    procedure Setup;
    procedure Translate;
    procedure RefreshStreams;
    procedure SwitchMode(Mode: TModes);

    procedure HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: UInt64);

    property Mode: TModes read FMode;
    property StreamTree: TMStreamTree read FStreamTree;
  end;

  TMStreamTreeHeaderPopup = class(TPopupMenu)
  private
    FItemName: TMenuItem;
    FItemKbps: TMenuItem;
    FItemType: TMenuItem;
    FItemRating: TMenuItem;

    procedure Translate;
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TMStreamTree = class(TVirtualStringTree)
  private
    FDragSource: TDropFileSource;
    FDataLists: TDataLists;

    FColName: TVirtualTreeColumn;

    FLastSearch: string;
    FLastGenre: string;
    FLastAudioType: TAudioTypes;
    FLastBitrate: Cardinal;
    FSelectedSortType: TSortTypes;

    FImageMetaData: TPngImage;
    FImageChangesTitle: TPngImage;

    FPopupMenu: TPopupMenu;
    FItemStart: TMenuItem;
    FItemPlay: TMenuItem;
    FItemOpen: TMenuItem;
    FItemAdd: TMenuItem;
    FItemRate: TMenuItem;
    FItemRate1: TMenuItem;
    FItemRate2: TMenuItem;
    FItemRate3: TMenuItem;
    FItemRate4: TMenuItem;
    FItemRate5: TMenuItem;
    FItemRefresh: TMenuItem;
    FItemSetData: TMenuItem;
    FItemOpenWebsite: TMenuItem;
    FItemBlacklist: TMenuItem;
    FItemCopy: TMenuItem;
    FItemSave: TMenuItem;

    FProgressBar: TProgressBar;

    FSortPopupMenu: TMStreamTreeHeaderPopup;
    FMode: TModes;
    FTimer: TTimer;
    FDots: string;
    FButtonPos: TRect;

    FDraggedStreams: TStreamDataArray;

    FOnNeedData: TNeedDataEvent;
    FOnAction: TActionEvent;
    FOnIsInClientList: TIsInClientListEvent;

    function CreateItem(Caption: string; ImageIndex: Integer; Parent: TMenuItem): TMenuItem;

    procedure FitColumns;
    function GetSelected: TStreamDataArray;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoDragging(P: TPoint); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; Text: UnicodeString; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;

    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
      CellRect: TRect; var ContentRect: TRect); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
    procedure Resize; override;
    procedure Paint; override;
    procedure KeyPress(var Key: Char); override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString; override;
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer); override;

    procedure WndProc(var Message: TMessage); override;

    procedure TimerOnTimer(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);

    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  public
    constructor Create(AOwner: TComponent; DataLists: TDataLists); reintroduce;
    destructor Destroy; override;
    procedure Setup;
    procedure InvalidateVisible;

    procedure SwitchMode(Mode: TModes);

    procedure Sort(Node: PVirtualNode; Column: TColumnIndex; SortType: TSortTypes; Direction: TSortDirection); reintroduce;

    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    function Build(AlwaysBuild: Boolean; Search, Genre: string; AudioType: TAudioTypes; Bitrate: Cardinal): Boolean;
    procedure ReceiveError;
    procedure HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: UInt64);

    property PopupMenu2: TPopupMenu read FPopupMenu;
    property DraggedStreams: TStreamDataArray read FDraggedStreams;

    property OnNeedData: TNeedDataEvent read FOnNeedData write FOnNeedData;
    property OnAction: TActionEvent read FOnAction write FOnAction;
    property OnIsInClientList: TIsInClientListEvent read FOnIsInClientList write FOnIsInClientList;
    property SelectedSortType: TSortTypes read FSelectedSortType write FSelectedSortType;
  end;

implementation

{ TMStreamView }

constructor TMStreamTree.Create(AOwner: TComponent; DataLists: TDataLists);
var
  Res: TResourceStream;
begin
  inherited Create(AOwner);

  FMode := moShow;

  FDataLists := DataLists;

  NodeDataSize := SizeOf(TStreamNodeData);
  IncrementalSearch := isVisibleOnly;

  Header.Height := GetTextSize('Wyg', Font).cy + 5;

  TreeOptions.SelectionOptions := [toDisableDrawSelection, toRightClickSelect, toFullRowSelect, toMultiSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop] + [toFullRowDrag];
  Header.Options := Header.Options + [hoShowSortGlyphs, hoVisible, hoOwnerDraw] - [hoDrag];
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  ScrollBarOptions.ScrollBars := ssVertical;
  ScrollBarOptions.AlwaysVisible := True;

  FDragSource := TDropFileSource.Create(Self);

  Res := TResourceStream.Create(HInstance, 'BROWSER_METADATA', MakeIntResource(RT_RCDATA));
  try
    FImageMetaData := TPngImage.Create;
    FImageMetaData.LoadFromStream(Res);
  finally
    Res.Free;
  end;
  Res := TResourceStream.Create(HInstance, 'BROWSER_CHANGESTITLE', MakeIntResource(RT_RCDATA));
  try
    FImageChangesTitle := TPngImage.Create;
    FImageChangesTitle.LoadFromStream(Res);
  finally
    Res.Free;
  end;

  FColName := Header.Columns.Add;
  FColName.Text := _('Rating');
  FitColumns;

  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.AutoHotkeys := maManual;
  FPopupMenu.OnPopup := PopupMenuPopup;

  FItemRefresh := CreateItem('Re&fresh', 23, nil);
  FItemRefresh.OnClick := PopupMenuClick;

  CreateItem('-', -1, nil);

  FItemStart := CreateItem('&Start recording', 0, nil);
  FItemStart.OnClick := PopupMenuClick;

  FItemPlay := CreateItem('&Play stream', 33, nil);
  FItemPlay.OnClick := PopupMenuClick;

  FItemOpen := CreateItem('P&lay stream (external player)', 82, nil);
  FItemOpen.OnClick := PopupMenuClick;

  FItemAdd := CreateItem('&Add stream', 80, nil);
  FItemAdd.OnClick := PopupMenuClick;

  FItemSetData := CreateItem('S&et data...', -1, nil);
  FItemSetData.OnClick := PopupMenuClick;

  FItemRate := CreateItem('&Rate', 64, nil);

  FItemRate5 := CreateItem('&5', 64, FItemRate);
  FItemRate5.OnClick := PopupMenuClick;
  FItemRate5.Tag := 5;
  FItemRate4 := CreateItem('&4', 63, FItemRate);
  FItemRate4.OnClick := PopupMenuClick;
  FItemRate4.Tag := 4;
  FItemRate3 := CreateItem('&3', 62, FItemRate);
  FItemRate3.OnClick := PopupMenuClick;
  FItemRate3.Tag := 3;
  FItemRate2 := CreateItem('&2', 61, FItemRate);
  FItemRate2.OnClick := PopupMenuClick;
  FItemRate2.Tag := 2;
  FItemRate1 := CreateItem('&1', 60, FItemRate);
  FItemRate1.OnClick := PopupMenuClick;
  FItemRate1.Tag := 1;

  CreateItem('-', -1, nil);

  FItemOpenWebsite := CreateItem('Open &website...', 38, nil);
  FItemOpenWebsite.OnClick := PopupMenuClick;

  FItemBlacklist := CreateItem('Add to &blacklist', 51, nil);
  FItemBlacklist.OnClick := PopupMenuClick;

  FItemCopy := CreateItem('&Copy URL', -1, nil);
  FItemCopy.OnClick := PopupMenuClick;

  FItemSave := CreateItem('&Save as playlist...', -1, nil);
  FItemSave.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;

  FDots := '';
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerOnTimer;
  FTimer.Interval := 1000;
  FTimer.Enabled := True;

  Header.SortColumn := 0;
  Header.SortDirection := sdDescending;

  FSortPopupMenu := TMStreamTreeHeaderPopup.Create(Self);
  Header.PopupMenu := FSortPopupMenu;


  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Parent := Self;
  FProgressBar.Width := 150;
  FProgressBar.Height := 20;
  FProgressBar.Visible := False;
  FProgressBar.Max := 100;
  FProgressBar.Min := 0;

  Images := modSharedData.imgImages;
end;

function TMStreamTree.CreateItem(Caption: string; ImageIndex: Integer;
  Parent: TMenuItem): TMenuItem;
begin
  Result := FPopupMenu.CreateMenuItem;
  Result.Caption := Caption;
  Result.ImageIndex := ImageIndex;
  if Parent = nil then
    FPopupMenu.Items.Add(Result)
  else
    Parent.Add(Result);
end;

destructor TMStreamTree.Destroy;
begin
  FDragSource.Free;
  FTimer.Free;

  FImageMetaData.Free;
  FImageChangesTitle.Free;

  inherited;
end;

procedure TMStreamTree.DoFreeNode(Node: PVirtualNode);
begin

  inherited;
end;

function TMStreamTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
var
  NodeData: PStreamNodeData;
begin
  Result := inherited;
  NodeData := PStreamNodeData(GetNodeData(Node));

  if ((Kind = ikNormal) or (Kind = ikSelected)) and (Column = 0) then
  begin
    if NodeData.Data.OwnRating > 0 then
      Index := 59 + NodeData.Data.OwnRating
    else
      if NodeData.Data.Rating > 0 then
        Index := 39 + NodeData.Data.Rating
      else
        Index := 16;
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
      Text := StringReplace(NodeData.Data.Name, '&', '&&', [rfReplaceAll]) // Wegen & und dem Shortcut..
  end;
end;

procedure TMStreamTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;

  NodeHeight := GetTextSize('Wyg', Font).cy + 6 + GetTextSize('Wyg', Font).cy;
end;

procedure TMStreamTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  L: Integer;
  NodeData: PStreamNodeData;
begin
  inherited;

  NodeData := GetNodeData(PaintInfo.Node);

  L := 4;
  if NodeData.Data.MetaData then
  begin
    PaintInfo.Canvas.Draw(L, 21, FImageMetaData);
    L := L + 9;
  end;
  if NodeData.Data.ChangesTitleInSong or (not NodeData.Data.RecordingOkay) then
    PaintInfo.Canvas.Draw(L, 21, FImageChangesTitle);
end;

procedure TMStreamTree.FitColumns;
begin
  Header.AutoSizeIndex := 1;
  Header.Options := Header.Options + [hoAutoResize];
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

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].ID := NodeData.Data.ID;
    Result[High(Result)].Bitrate := NodeData.Data.BitRate;
    Result[High(Result)].Name := NodeData.Data.Name;
    Result[High(Result)].URL := NodeData.Data.URL;
    Result[High(Result)].Website := NodeData.Data.Website;
    Result[High(Result)].Rating := NodeData.Data.Rating;
    Result[High(Result)].RegEx := NodeData.Data.RegEx;
    Result[High(Result)].RecordingOkay := NodeData.Data.RecordingOkay;
    Result[High(Result)].IgnoreTitles := NodeData.Data.IgnoreTitles;
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
      case AppGlobals.DefaultActionBrowser of
        oaStart:
          FOnAction(Self, oaStart, Entries);
        oaPlay:
          FOnAction(Self, oaPlay, Entries);
        oaPlayExternal:
          FOnAction(Self, oaPlayExternal, Entries);
        oaAdd:
          FOnAction(Self, oaAdd, Entries);
      end;
  end;
end;

procedure TMStreamTree.HomeCommBytesTransferred(
  CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  if FProgressBar.Position < 100 then
    FProgressBar.Position := FProgressBar.Position + 1;
  FProgressBar.Position := Trunc((Transferred / CommandHeader.CommandLength) * 100);
end;

procedure TMStreamTree.InvalidateVisible;
var
  Node: PVirtualNode;
  R, R2: TRect;
  FoundStart, InRect: Boolean;
begin
  FoundStart := False;

  R2 := Self.ClientRect;

  Node := GetFirst;
  while Node <> nil do
  begin
    R := GetDisplayRect(Node, 0, False);

    InRect := PtInRect(R2, R.TopLeft);
    if not InRect then
      InRect := PtInRect(R2, R.BottomRight);

    if InRect then
    begin
      FoundStart := True;
      InvalidateNode(Node);
      RepaintNode(Node);
    end else
      if FoundStart then
        Exit;

    Node := GetNext(Node);
  end;
end;

procedure TMStreamTree.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then
  begin
    case AppGlobals.DefaultActionBrowser of
      oaStart:
        FItemStart.Click;
      oaPlay:
        FItemPlay.Click;
      oaPlayExternal:
        FItemOpen.Click;
    end;
    Key := #0;
  end;
end;

procedure TMStreamTree.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  Text: UnicodeString; CellRect: TRect; DrawFormat: Cardinal);
var
  Size: TSize;
  NodeData: PStreamNodeData;
begin
  NodeData := PStreamNodeData(GetNodeData(PaintInfo.Node));

  GetTextExtentPoint32W(PaintInfo.Canvas.Handle, 'Wl0', 3, Size);

  CellRect.Top := CellRect.Top + 2;
  DrawFormat := DT_TOP or DT_LEFT;
  inherited;

  CellRect.Top := CellRect.Top + 2 + Size.cy;

  Text := '';

  if NodeData.Data.AudioType <> atNone then
  begin
    if NodeData.Data.AudioType = atMPEG then
      Text := 'MP3'
    else if NodeData.Data.AudioType = atAAC then
      Text := 'AAC';
  end;

  if NodeData.Data.BitRate > 0 then
  begin
    if Text <> '' then
      Text := Text + ' / ';
    Text := Text + IntToStr(NodeData.Data.BitRate) + 'kbps';
  end;

  if NodeData.Data.Genre <> '' then
  begin
    if Text <> '' then
      Text := Text + ' / ';
    Text := Text + NodeData.Data.Genre;
  end;

  if Text = '' then
    Text := _('No info available')
  else
    Text := StringReplace(Text, '&', '&&', [rfReplaceall]); // Wegen & und dem Shortcut..
  inherited;
end;

procedure TMStreamTree.Paint;
var
  Size: TSize;
  TmpText: string;
  R: TRect;
begin
  inherited;

  if FMode = moError then
  begin
    TmpText := _('No connection to server.');
    GetTextExtentPoint32W(Canvas.Handle, TmpText, Length(TmpText), Size);

    R := ClientRect;
    R.Left := (R.Right div 2) - (Size.cx div 2) - 4;
    R.Top := ClientHeight div 2 - Canvas.TextHeight('Wy');

    DrawText(Canvas.Handle, PChar(TmpText), Length(TmpText), R, 0);
  end;

  if (FMode = moLoading) and (RootNodeCount = 0) then
  begin
    TmpText := _('Loading streams');
    GetTextExtentPoint32W(Canvas.Handle, TmpText, Length(TmpText), Size);

    R := ClientRect;
    R.Left := (R.Right div 2) - (Size.cx div 2) - 4;
    R.Top := FProgressBar.Top - GetTextSize('Wyg', Font).cy - MulDiv(2, Screen.PixelsPerInch, 96);

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
  Action: TStreamOpenActions;
  Streams: TStreamDataArray;
begin
  Action := oaNone;
  Streams := GetSelected;

  if Sender = FItemStart then
    Action := oaStart
  else if Sender = FItemPlay then
  begin
    if Bass.DeviceAvailable then
      Action := oaPlay;
  end else if Sender = FItemOpen then
    Action := oaPlayExternal
  else if Sender = FItemAdd then
    Action := oaAdd
  else if Sender = FItemOpenWebsite then
    Action := oaOpenWebsite
  else if Sender = FItemBlacklist then
    Action := oaBlacklist
  else if Sender = FItemCopy then
    Action := oaCopy
  else if Sender = FItemSave then
    Action := oaSave
  else if Sender = FItemSetData then
    Action := oaSetData
  else if Sender = FItemRefresh then
    Action := oaRefresh
  else if (Sender = FItemRate1) or (Sender = FItemRate2) or (Sender = FItemRate3) or
          (Sender = FItemRate4) or (Sender = FItemRate5) then
    if Length(Streams) = 1 then
    begin
      case TMenuItem(Sender).Tag of
        1: Action := oaRate1;
        2: Action := oaRate2;
        3: Action := oaRate3;
        4: Action := oaRate4;
        5: Action := oaRate5;
      end;
    end
  else
    raise Exception.Create('');

  if (Action <> oaNone) and (Length(Streams) > 0) then
  begin
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Streams);
  end else if Action = oaRefresh then
  begin
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Streams);
  end;
end;

procedure TMStreamTree.PopupMenuPopup(Sender: TObject);
var
  Streams: TStreamDataArray;
begin
  Streams := GetSelected;

  FItemStart.Enabled := Length(Streams) > 0;
  FItemPlay.Enabled := (Length(Streams) = 1) and Bass.DeviceAvailable;
  FItemOpen.Enabled := Length(Streams) = 1;
  FItemOpenWebsite.Enabled := (Length(Streams) > 0) and (Trim(Streams[0].Website) <> '');
  FItemBlacklist.Enabled := Length(Streams) > 0;

  FItemRate.Enabled := HomeComm.Connected and (Length(Streams) = 1);
  FItemRefresh.Enabled := HomeComm.Connected;

  FItemSetData.Enabled := HomeComm.Connected and (Length(Streams) = 1);

  FItemCopy.Enabled := Length(Streams) > 0;
  FItemSave.Enabled := Length(Streams) > 0;
end;

procedure TMStreamTree.TimerOnTimer(Sender: TObject);
begin
  FDots := FDots + '.';

  if Length(FDots) = 4 then
    FDots := '';

  Invalidate;
end;

procedure TMStreamTree.WMKeyDown(var Message: TWMKeyDown);
var
  Shift: TShiftState;
begin
  if (Message.CharCode = Ord('a')) or (Message.CharCode = Ord('A')) then
  begin
    Shift := KeyDataToShiftState(Message.KeyData);
    if ssCtrl in Shift then
      Exit;
  end;

  inherited;
end;

procedure TMStreamTree.WndProc(var Message: TMessage);
  procedure DrawImg(C: TCanvas);
  var
    SBW: Integer;
    R: TRect;
  begin
    Windows.GetClientRect(Handle, R);
    SBW := GetSystemMetrics(SM_CXVSCROLL);
    FButtonPos := Bounds(Width - SBW - 2, 2, SBW + 1, Height - R.Bottom - 5);
    C.Brush.Color := clBtnFace;
    R := FButtonPos;
    C.FillRect(R);
    Images.Draw(C, FButtonPos.Left, FButtonPos.Top, 47, True);
  end;
var
  DC: HDC;
  Flags: DWORD;
  C: TCanvas;
  P: TPoint;
begin
  inherited;

  case Message.Msg of
    WM_NCPAINT:
      begin
        Flags := DCX_CACHE or DCX_CLIPSIBLINGS or DCX_WINDOW or DCX_VALIDATE;
        DC := GetDCEx(Self.Header.Treeview.Handle, 0, Flags);
        if DC <> 0 then
        begin
          C := TCanvas.Create;
          try
            C.Handle := DC;
            DrawImg(C);
          finally
            C.Free;
          end;
        end;
      end;
    WM_NCLBUTTONDOWN:
      begin
        P.X := TWMNCLButtonDown(Message).XCursor;
        P.Y := TWMNCLButtonDown(Message).YCursor + Header.Height;

        if PtInRect(FButtonPos, ScreenToClient(P)) then
        begin
          FSortPopupMenu.Popup(P.X, P.Y - Header.Height);
        end;
      end;
  end;
end;

procedure TMStreamTree.DoBeforeCellPaint(Canvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  NodeData: PStreamNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  if CellPaintMode = cpmPaint then
  begin
    if FOnIsInClientList(Self, NodeData.Data.ID) then
      Canvas.Brush.Color := HTML2Color('c3c1c1')
    else
      case Node.Index mod 2 of
        0:
          Canvas.Brush.Color := clWindow;
        1:
          Canvas.Brush.Color := HTML2Color('f3f3f3');
      end;
    Canvas.FillRect(CellRect);
  end;
end;

function TMStreamTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  Data1, Data2: PStreamNodeData;
  S1, S2: string;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Data1.Data.AudioType of
    atNone: S1 := 'z';
    atMPEG: S1 := 'm';
    atAAC: S1 := 'a';
    atOGG: S1 := 'o';
  end;

  case Data2.Data.AudioType of
    atNone: S2 := 'z';
    atMPEG: S2 := 'm';
    atAAC: S2 := 'a';
    atOGG: S2 := 'o';
  end;

  case FSelectedSortType of
    stName: Result := CompareText(Data1.Data.Name, Data2.Data.Name);
    stBitrate: Result := CmpInt(Data1.Data.BitRate, Data2.Data.BitRate);
    stType: Result := CompareText(S1, S2);
    stRating: Result := CmpInt(Data1.Data.Rating, Data2.Data.Rating)
  end;

  if (Result = 0) and (FSelectedSortType <> stName) then
    Result := CompareText(Data2.Data.Name, Data1.Data.Name);

  if (Result = 0) and (FSelectedSortType <> stRating) then
    Result := CmpInt(Data1.Data.Rating, Data2.Data.Rating)
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
  FDraggedStreams := GetSelected;
  FDragSource.Execute(False);
  SetLength(FDraggedStreams, 0);
end;

function TMStreamTree.Build(AlwaysBuild: Boolean; Search, Genre: string; AudioType: TAudioTypes; Bitrate: Cardinal): Boolean;
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PStreamNodeData;
  Add: Boolean;
  P, P2: string;
  Hash: Cardinal;
  Chars: Integer;
begin
  Result := False;
  P := BuildPattern(Search, Hash, Chars, False);
  P2 := BuildPattern(Genre, Hash, Chars, False);

  if (not AlwaysBuild) and (P = FLastSearch) and (Genre = FLastGenre) and
     (AudioType = FLastAudioType) and (Bitrate = FLastBitrate) then
    Exit;

  Result := True;

  BeginUpdate;
  try
    Clear;

    for i := 0 to FDataLists.BrowserList.Count - 1 do
    begin
      Add := ((P = '*') or Like(LowerCase(FDataLists.BrowserList[i].Name), LowerCase(P))) and
             ((P2 = '*') or Like(LowerCase(FDataLists.BrowserList[i].Genre), LowerCase(P2))) and
             ((AudioType = atNone) or (FDataLists.BrowserList[i].AudioType = AudioType)) and
             ((Bitrate = 0) or (FDataLists.BrowserList[i].BitRate >= Bitrate));
      if Add then
      begin
        Node := AddChild(nil);
        NodeData := GetNodeData(Node);
        NodeData.Data := FDataLists.BrowserList[i];
      end;
    end;

    FLastSearch := P;
    FLastGenre := Genre;
    FLastAudioType := AudioType;
    FLastBitrate := Bitrate;
  finally
    EndUpdate;
  end;
end;

procedure TMStreamTree.ReceiveError;
begin
  Clear;
end;

procedure TMStreamTree.Resize;
begin
  inherited;

  Setup;

  FProgressBar.Left := Trunc(ClientWidth / 2 - FProgressBar.Width / 2);
  FProgressBar.Top := ClientHeight div 2 - Canvas.TextHeight('Wy') + 15;
end;

procedure TMStreamTree.Setup;
begin
  FColName.Width := ClientWidth;
end;

procedure TMStreamTree.Sort(Node: PVirtualNode; Column: TColumnIndex;
  SortType: TSortTypes; Direction: TSortDirection);
begin
  FSelectedSortType := SortType;
  inherited Sort(nil, 0, Direction);
end;

procedure TMStreamTree.SwitchMode(Mode: TModes);
begin
  if Mode = FMode then
    Exit;

  Enabled := Mode = moShow;

  FTimer.Enabled := False;
  if Mode = moLoading then
  begin
    Clear;

    FProgressBar.Position := 0;
    if not FProgressBar.Visible then
      FProgressBar.Visible := True;

    FDots := '';
    FTimer.Enabled := True;
  end else
    if FProgressBar.Visible then
      FProgressBar.Visible := False;

  FMode := Mode;

  Invalidate;
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
  if NodeData.Data.Genre <> '' then
    Text := Text + #13#10 + NodeData.Data.Genre;
  Result := Text;
end;

{ TMStreamView }

procedure TMStreamBrowserView.BuildGenres;
var
  i: Integer;
  LastGenre: string;
begin
  LastGenre := '';
  if FSearch.FGenreList.ItemIndex > -1 then
    LastGenre := FSearch.FGenreList.Text;

  FSearch.FGenreList.Clear;
  FSearch.FGenreList.Items.Add(_('- No genre -'));
  for i := 0 to FDataLists.GenreList.Count - 1 do
    FSearch.FGenreList.Items.Add(FDataLists.GenreList[i].Name);
  if FSearch.FGenreList.Items.Count > 0 then
    FSearch.FGenreList.ItemIndex := 0;
  FSearch.FGenreList.Sorted := True;

  for i := 0 to FSearch.FGenreList.Items.Count - 1 do
    if FSearch.FGenreList.Items[i] = LastGenre then
    begin
      FSearch.FGenreList.ItemIndex := i;
      Break;
    end;
end;

procedure TMStreamBrowserView.BuildTree(AlwaysBuild: Boolean);
var
  Genre: string;
  AudioType: TAudioTypes;
  Bitrate: Cardinal;
begin
  Genre := '';

  if FSearch.FGenreList.ItemIndex > 0 then
    Genre := FSearch.FGenreList.Items[FSearch.FGenreList.ItemIndex];

  case FSearch.FTypeList.ItemIndex of
    0: AudioType := atNone;
    1: AudioType := atMPEG;
    2: AudioType := atAAC;
    else
      raise Exception.Create('');
  end;

  case FSearch.FKbpsList.ItemIndex of
    0: Bitrate := 0;
    1: Bitrate := 64;
    2: Bitrate := 128;
    3: Bitrate := 192;
    4: Bitrate := 256;
    5: Bitrate := 320;
    else
      raise Exception.Create('');
  end;


  if FStreamTree.Build(AlwaysBuild, FSearch.FSearchEdit.Text, Genre, AudioType, Bitrate) then
  begin
    FStreamTree.Sort(nil, 0, FSelectedSortType, FStreamTree.Header.SortDirection);

    if FStreamTree.RootNodeCount = 1 then
      FCountLabel.Caption := Format(_('%d stream found'), [FStreamTree.RootNodeCount])
    else
      FCountLabel.Caption := Format(_('%d streams found'), [FStreamTree.RootNodeCount]);
  end;
end;

constructor TMStreamBrowserView.Create(AOwner: TComponent; DataLists: TDataLists);
begin
  inherited Create(AOwner);

  FDataLists := DataLists;

  Align := alClient;
  BevelOuter := bvNone;

  FLoading := False;
  FHomeCommunication := HomeComm;
  FSelectedSortType := stRating;

  FSearch := TMStreamSearchPanel.Create(Self);
  FSearch.Align := alTop;
  FSearch.Height := 100;
  FSearch.Parent := Self;
  FSearch.Visible := True;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Align := alBottom;
  FCountLabel.Parent := Self;
  FCountLabel.Visible := True;

  FStreamTree := TMStreamTree.Create(Self, FDataLists);
  FStreamTree.Align := alClient;
  FStreamTree.Parent := Self;
  FStreamTree.Visible := True;
  FStreamTree.OnHeaderClick := StreamBrowserHeaderClick;
  FStreamTree.FSortPopupMenu.FItemName.OnClick := SortItemClick;
  FStreamTree.FSortPopupMenu.FItemKbps.OnClick := SortItemClick;
  FStreamTree.FSortPopupMenu.FItemType.OnClick := SortItemClick;
  FStreamTree.FSortPopupMenu.FItemRating.OnClick := SortItemClick;
end;

destructor TMStreamBrowserView.Destroy;
begin

  inherited;
end;

procedure TMStreamBrowserView.HomeCommBytesTransferred(
  CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  FStreamTree.HomeCommBytesTransferred(CommandHeader, Transferred);
end;

procedure TMStreamBrowserView.HomeCommStreamsReceived(Sender: TObject);
begin
  FSearch.FGenreList.Clear;
  FStreamTree.Clear;

  BuildGenres;
  BuildTree(True);

  FDataLists.BrowserList.CreateDict;

  SwitchMode(moShow);

  AppGlobals.LastBrowserUpdate := Trunc(Now);
end;

procedure TMStreamBrowserView.ListsChange(Sender: TObject);
begin
  BuildTree(False);
end;

procedure TMStreamBrowserView.RefreshStreams;
begin
  MsgBus.SendMessage(TRefreshServerDataMsg.Create);
end;

procedure TMStreamBrowserView.SearchEditChange(Sender: TObject);
begin
  BuildTree(False);
end;

procedure TMStreamBrowserView.SortTree;
var
  i: Integer;
  SortDir: TSortDirection;
begin
  SortDir := sdAscending;

  case FSelectedSortType of
    stName:
      begin
        FStreamTree.Header.Columns[0].Text := _('Name');
      end;
    stBitrate:
      begin
        FStreamTree.Header.Columns[0].Text := _('Kbps');
        SortDir := sdDescending;
      end;
    stType:
      begin
        FStreamTree.Header.Columns[0].Text := _('Type');
      end;
    stRating:
      begin
        FStreamTree.Header.Columns[0].Text := _('Rating');
        SortDir := sdDescending;
      end
    else
      raise Exception.Create('');
  end;

  for i := 0 to FStreamTree.FSortPopupMenu.Items.Count - 1 do
    if FStreamTree.FSortPopupMenu.Items[i].Tag = Integer(FSelectedSortType) then
    begin
      FStreamTree.FSortPopupMenu.Items[i].Checked := True;
      Break;
    end;

  FStreamTree.Sort(nil, 0, FSelectedSortType, SortDir);
end;

procedure TMStreamBrowserView.Setup;
begin
  SwitchMode(moLoading);

  FSearch.Setup;
  FStreamTree.Setup;

  FSearch.FSearchEdit.OnChange := SearchEditChange;
  //FSearch.FSearchButton.OnClick := SearchButtonClick;
  FSearch.FGenreList.OnChange := ListsChange;
  FSearch.FKbpsList.OnChange := ListsChange;
  FSearch.FTypeList.OnChange := ListsChange;

  FHomeCommunication.OnStreamsReceived := HomeCommStreamsReceived;

  if (FDataLists.BrowserList.Count > 0) and (FDataLists.GenreList.Count > 0) then
  begin
    BuildGenres;
    BuildTree(True);
    SwitchMode(moShow);
  end;

  FSelectedSortType := TSortTypes(AppGlobals.BrowserSortType);
  SortTree;
end;

procedure TMStreamBrowserView.SortItemClick(Sender: TObject);
var
  i: Integer;
  LastItem: TMenuItem;
begin
  LastItem := nil;

  for i := 0 to FStreamTree.FSortPopupMenu.Items.Count - 1 do
  begin
    if FStreamTree.FSortPopupMenu.Items[i].Checked then
      LastItem := FStreamTree.FSortPopupMenu.Items[i];
    FStreamTree.FSortPopupMenu.Items[i].Checked := False;
  end;
  TMenuItem(Sender).Checked := True;

  if LastItem = TMenuItem(Sender) then
    Exit;

  if Sender = FStreamTree.FSortPopupMenu.FItemName then
  begin
    FSelectedSortType := stName;
  end else if Sender = FStreamTree.FSortPopupMenu.FItemKbps then
  begin
    FSelectedSortType := stBitrate;
  end else if Sender = FStreamTree.FSortPopupMenu.FItemType then
  begin
    FSelectedSortType := stType;
  end else if Sender = FStreamTree.FSortPopupMenu.FItemRating then
  begin
    FSelectedSortType := stRating;
  end else
    raise Exception.Create('');

  SortTree;
end;

procedure TMStreamBrowserView.StreamBrowserHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button = mbLeft then
  begin
    if FStreamTree.Header.SortDirection = sdAscending then
      FStreamTree.Header.SortDirection := sdDescending
    else
      FStreamTree.Header.SortDirection := sdAscending;

    FStreamTree.Sort(nil, 0, FSelectedSortType, FStreamTree.Header.SortDirection);
  end;
end;

procedure TMStreamBrowserView.SwitchMode(Mode: TModes);
begin
  FSearch.FSearchEdit.Enabled := Mode = moShow;
  FSearch.FGenreList.Enabled := Mode = moShow;
  FSearch.FKbpsList.Enabled := Mode = moShow;
  FSearch.FTypeList.Enabled := Mode = moShow;
  //FSearch.FSearchButton.Enabled := Mode = moShow;

  FCountLabel.Enabled := Mode = moShow;

  FMode := Mode;

  FStreamTree.SwitchMode(Mode);
end;

procedure TMStreamBrowserView.Translate;
var
  Idx: Integer;
begin
  FStreamTree.FColName.Text := _('Rating');

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
  if FSearch.FTypeList.Items.Count > 0 then
  begin
    Idx := FSearch.FTypeList.ItemIndex;
    FSearch.FTypeList.Items[0] := _('- No type -');
    FSearch.FTypeList.ItemIndex := Idx;
  end;

  if FStreamTree.RootNodeCount = 1 then
    FCountLabel.Caption := Format(_('%d stream found'), [FStreamTree.RootNodeCount])
  else
   FCountLabel.Caption := Format(_('%d streams found'), [FStreamTree.RootNodeCount]);

  FStreamTree.FSortPopupMenu.Translate;
end;

{ TMStreamSearch }

constructor TMStreamSearchPanel.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FSearchLabel := TLabel.Create(Self);
  FSearchEdit := TEdit.Create(Self);
  FGenreLabel := TLabel.Create(Self);
  FGenreList := TComboBox.Create(Self);
  FKbpsLabel := TLabel.Create(Self);
  FKbpsList := TComboBox.Create(Self);
  FTypeLabel := TLabel.Create(Self);
  FTypeList := TComboBox.Create(Self);
end;

procedure TMStreamSearchPanel.ExpandButtonClick(Sender: TObject);
begin
  SetVisible(not FTypeList.Visible);
end;

procedure TMStreamSearchPanel.Setup;
var
  TopCnt, MaxW: Integer;
begin
  TopCnt := 4;
  MaxW := 0;

  FSearchLabel.Parent := Self;
  FSearchLabel.Left := 4;
  FSearchLabel.Caption := 'Search:';
  if MaxW < FSearchLabel.Width then
    MaxW := FSearchLabel.Width;

  FSearchEdit.Parent := Self;
  FSearchEdit.Top := TopCnt;
  FSearchEdit.Anchors := [akLeft, akRight, akTop];

  FSearchLabel.Top := FSearchEdit.Top + FSearchEdit.Height div 2 - FSearchLabel.Height div 2;

  {
  FSearchButton.Parent := Self;
  FSearchButton.Width := 24;
  FSearchButton.Height := 24;
  FSearchButton.Top := TopCnt - 1;
  FSearchButton.Left := ClientWidth - 8 - FSearchButton.Width;
  FSearchButton.Anchors := [akRight, akTop];
  FSearchButton.Flat := True;
  FSearchButton.Hint := 'Search';
  FSearchButton.ShowHint := True;
  }

  TopCnt := TopCnt + FSearchEdit.Height + 4;

  {
  FShowHideFilters := TMShowHidePanel.Create(Self);
  FShowHideFilters.ShowCaption := _('Show filters');
  FShowHideFilters.HideCaption := _('Hide filters');
  FShowHideFilters.Parent := Self;
  FShowHideFilters.Top := TopCnt;
  FShowHideFilters.Height := 30;
  FShowHideFilters.Width := 100;

  TopCnt := TopCnt + 26;
  }

  FGenreLabel.Parent := Self;
  FGenreLabel.Left := 4;
  FGenreLabel.Caption := _('Genre') + ':';
  if MaxW < FGenreLabel.Width then
    MaxW := FGenreLabel.Width;

  FGenreList.Parent := Self;
  FGenreList.Style := csDropDownList;
  FGenreList.Top := TopCnt;
  FGenreList.Anchors := [akLeft, akRight, akTop];
  FGenreList.DropDownCount := 16;

  FGenreLabel.Top := FGenreList.Top + FGenreList.Height div 2 - FGenreLabel.Height div 2;

  TopCnt := TopCnt + FGenreList.Height + 4;

  FKbpsLabel.Parent := Self;
  FKbpsLabel.Left := 4;
  FKbpsLabel.Caption := _('Kbps') + ':';
  if MaxW < FKbpsLabel.Width then
    MaxW := FKbpsLabel.Width;

  FKbpsList.Parent := Self;
  FKbpsList.Style := csDropDownList;
  FKbpsList.Top := TopCnt;
  FKbpsList.Anchors := [akLeft, akRight, akTop];

  FKbpsLabel.Top := FKbpsList.Top + FKbpsList.Height div 2 - FKbpsLabel.Height div 2;

  TopCnt := TopCnt + FKbpsList.Height + 4;

  FTypeLabel.Parent := Self;
  FTypeLabel.Left := 4;
  FTypeLabel.Caption := _('Type') + ':';
  if MaxW < FTypeLabel.Width then
    MaxW := FTypeLabel.Width;

  FTypeList.Parent := Self;
  FTypeList.Style := csDropDownList;
  FTypeList.Top := TopCnt;
  FTypeList.Anchors := [akLeft, akRight, akTop];

  FTypeLabel.Top := FTypeList.Top + FTypeList.Height div 2 - FTypeLabel.Height div 2;


  {
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
  }

  FSearchEdit.Left := MaxW + 12;
  FGenreList.Left := MaxW + 12;
  FKbpsList.Left := MaxW + 12;
  FTypeList.Left := MaxW + 12;


  FSearchEdit.Width := ClientWidth - FSearchEdit.Left - FSearchLabel.Left;
  FGenreList.Width := ClientWidth - FGenreList.Left - FGenreLabel.Left;
  FKbpsList.Width := ClientWidth - FKbpsList.Left - FKbpsLabel.Left;
  FTypeList.Width := ClientWidth - FTypeList.Left - FTypeLabel.Left;

  FKbpsList.Items.Add(_('- No kbps -'));
  FKbpsList.Items.Add('>= 64');
  FKbpsList.Items.Add('>= 128');
  FKbpsList.Items.Add('>= 192');
  FKbpsList.Items.Add('>= 256');
  FKbpsList.Items.Add('= 320');
  FKbpsList.ItemIndex := 0;

  FTypeList.Items.Add(_('- No type -'));
  FTypeList.Items.Add(_('MP3'));
  FTypeList.Items.Add(_('AAC'));
  FTypeList.ItemIndex := 0;

  ClientHeight := FTypeList.Top + FTypeList.Height + FSearchEdit.Top + 4;

  SetVisible(True);
end;

procedure TMStreamSearchPanel.SetVisible(Value: Boolean);
var
  i: Integer;
begin
  if Value then
  begin
    //FExpandButton.Caption := _('Hide filters');
    //FExpandButton.PngImage := FImageHideFilter;
  end else
  begin
    //FExpandButton.Caption := _('Show filters');
    //FExpandButton.PngImage := FImageShowFilter;
  end;

  for i := 0 to ControlCount - 1 do
    if {(Controls[i] <> FExpandLabel) and (Controls[i] <> FShowHideFilters) and}
       (Controls[i] <> FSearchLabel) and (Controls[i] <> FSearchEdit) then
    begin
      Controls[i].Visible := Value;
    end;
  //if Value then
    ClientHeight := FTypeList.Top + FTypeList.Height + FSearchEdit.Top + 4
  //else
  //  ClientHeight := FShowHideFilters.Top + FShowHideFilters.Height + FSearchEdit.Top + 4;
end;

{ TMStreamTreeHeaderPopup }

constructor TMStreamTreeHeaderPopup.Create(AOwner: TComponent);
begin
  inherited;

  FItemName := CreateMenuItem;
  FItemName.Caption := _('Name');
  FItemName.RadioItem := True;
  FItemName.Tag := Integer(stName);
  Items.Add(FItemName);

  FItemKbps := CreateMenuItem;
  FItemKbps.Caption := _('Kbps');
  FItemKbps.RadioItem := True;
  FItemKbps.Tag := Integer(stBitrate);
  Items.Add(FItemKbps);

  FItemType := CreateMenuItem;
  FItemType.Caption := _('Type');
  FItemType.RadioItem := True;
  FItemType.Tag := Integer(stType);
  Items.Add(FItemType);

  FItemRating := CreateMenuItem;
  FItemRating.Caption := _('Rating');
  FItemRating.RadioItem := True;
  FItemRating.Tag := Integer(stRating);
  Items.Add(FItemRating);
end;

procedure TMStreamTreeHeaderPopup.DoPopup(Sender: TObject);
begin
  inherited;

end;

procedure TMStreamTreeHeaderPopup.Translate;
begin
  FItemName.Caption := _('Name');
  FItemKbps.Caption := _('Kbps');
  FItemType.Caption := _('Type');
  FItemRating.Caption := _('Rating');
end;

end.

