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

{ This unit contains everything needed to display the stream-browser }
unit StreamBrowserView;

interface

uses
  AppData,
  AppMessages,
  AudioFunctions,
  Buttons,
  Classes,
  ComboEx,
  ComCtrls,
  Commands,
  Controls,
  DataManager,
  DragDrop,
  DragDropFile,
  DropSource,
  DynBASS,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  GraphType,
  HomeCommunication,
  Images,
  ImgList,
  LanguageObjects,
  LMessages,
  Logging,
  Menus,
  MessageBus,
  SharedData,
  StdCtrls,
  SysUtils,
  Themes,
  TypeDefs,
  Types,
  VirtualTrees,
  Windows;

type
  TModes = (moShow, moLoading, moError);

  TMStreamTree = class;

  TStreamData = record
    ID: Integer;
    Bitrate: Cardinal;
    Name: string;
    URL: string;
    URLs: TStringList;
    Website: string;
    Rating: Integer;
    RecordingOkay: Boolean;
    RegExes: TStringList;
    IgnoreTitles: TStringList;
    CanSetRegExps: Boolean;
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

  { TMStreamSearchPanel }

  TMStreamSearchPanel = class(TPanel)
  private
    FSearchEdit: TEdit;
    FGenreList: TComboBoxEx;
    FKbpsList: TComboBoxEx;
    FTypeList: TComboBoxEx;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSortTypes = (stName, stBitrate, stType, stRating);

  { TMStreamBrowserView }

  TMStreamBrowserView = class(TPanel)
  private
    FSearch: TMStreamSearchPanel;
    FStreamTree: TMStreamTree;
    FCountLabel: TLabel;
    FMode: TModes;

    FSortType: TSortTypes;

    FLoading: Boolean;

    FOnStreamsReceived: TNotifyEvent;

    procedure ListsChange(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SortItemClick(Sender: TObject);

    procedure BuildTree(AlwaysBuild: Boolean);
    procedure BuildGenres;
    procedure SortTree(ResetSortDir: Boolean);

    procedure StreamBrowserHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

    procedure HomeCommDataReceived(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure PostTranslate;
    procedure RefreshStreams;
    procedure SwitchMode(Mode: TModes);

    procedure HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: UInt64);

    property Mode: TModes read FMode;
    property StreamTree: TMStreamTree read FStreamTree;

    property OnStreamsReceived: TNotifyEvent read FOnStreamsReceived write FOnStreamsReceived;
  end;

  TMStreamTreeHeaderPopup = class(TPopupMenu)
  private
    FItemName: TMenuItem;
    FItemKbps: TMenuItem;
    FItemType: TMenuItem;
    FItemRating: TMenuItem;

    procedure PostTranslate;
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TMStreamTree }

  TMStreamTree = class(TVirtualStringTree)
  private
    FDragSource: TDropFileSource;

    FColName: TVirtualTreeColumn;

    FLastSearch: string;
    FLastGenre: string;
    FLastAudioType: TAudioTypes;
    FLastBitrate: Cardinal;
    FSortType: TSortTypes;

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
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoDragging(P: TPoint); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): string; override;
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    function DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean; override;
    procedure DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo); override;
    procedure Resize; override;
    procedure Paint; override;
    procedure KeyPress(var Key: Char); override;

    procedure WndProc(var Message: TMessage); override;

    procedure TimerOnTimer(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);

    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

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
  end;

implementation

{ TMStreamView }

constructor TMStreamTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMode := moShow;

  NodeDataSize := SizeOf(TStreamNodeData);
  IncrementalSearch := isVisibleOnly;

  TreeOptions.SelectionOptions := [toDisableDrawSelection, toRightClickSelect, toFullRowSelect, toMultiSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop] + [toFullRowDrag, toVariableNodeHeight];
  Header.Options := Header.Options + [hoShowSortGlyphs, hoVisible, hoOwnerDraw] - [hoDrag];
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  ScrollBarOptions.ScrollBars := ssVertical;
  ScrollBarOptions.AlwaysVisible := True;

  FDragSource := TDropFileSource.Create(Self);

  FColName := Header.Columns.Add;
  FColName.Text := _('Rating');
  FitColumns;

  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.OnPopup := PopupMenuPopup;

  FItemRefresh := CreateItem('Re&fresh', TImages.ARROW_REFRESH, nil);
  FItemRefresh.OnClick := PopupMenuClick;

  FPopupMenu.Items.AddSeparator;

  FItemStart := CreateItem('&Start recording', TImages.RECORD_RED, nil);
  FItemStart.OnClick := PopupMenuClick;

  FItemPlay := CreateItem('&Play stream', TImages.PLAY_BLUE, nil);
  FItemPlay.OnClick := PopupMenuClick;

  FItemOpen := CreateItem('P&lay stream (external player)', TImages.PLAY_GO, nil);
  FItemOpen.OnClick := PopupMenuClick;

  FItemAdd := CreateItem('&Add stream', TImages.TRANSMIT_ADD, nil);
  FItemAdd.OnClick := PopupMenuClick;

  FItemSetData := CreateItem('S&et data...', TImages.TRANSMIT_EDIT, nil);
  FItemSetData.OnClick := PopupMenuClick;

  FItemRate := CreateItem('&Rate', TImages.STAR, nil);

  FItemRate5 := CreateItem('&5', TImages.STAR, FItemRate);
  FItemRate5.OnClick := PopupMenuClick;
  FItemRate5.Tag := 5;
  FItemRate4 := CreateItem('&4', TImages.STAR_2, FItemRate);
  FItemRate4.OnClick := PopupMenuClick;
  FItemRate4.Tag := 4;
  FItemRate3 := CreateItem('&3', TImages.STAR_3, FItemRate);
  FItemRate3.OnClick := PopupMenuClick;
  FItemRate3.Tag := 3;
  FItemRate2 := CreateItem('&2', TImages.STAR_4, FItemRate);
  FItemRate2.OnClick := PopupMenuClick;
  FItemRate2.Tag := 2;
  FItemRate1 := CreateItem('&1', TImages.STAR_5, FItemRate);
  FItemRate1.OnClick := PopupMenuClick;
  FItemRate1.Tag := 1;

  FPopupMenu.Items.AddSeparator;

  FItemOpenWebsite := CreateItem('Open &website...', TImages.LINK_GO, nil);
  FItemOpenWebsite.OnClick := PopupMenuClick;

  FItemBlacklist := CreateItem('Add to &blacklist', TImages.PAGE_WHITE_TRANSMIT_ADD, nil);
  FItemBlacklist.OnClick := PopupMenuClick;

  FItemCopy := CreateItem('&Copy URL', 57, nil);
  FItemCopy.OnClick := PopupMenuClick;

  FItemSave := CreateItem('&Save as playlist...', TImages.PAGE_WHITE_MUSIC_OUT, nil);
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
  FProgressBar.AnchorSideLeft.Control := Self;
  FProgressBar.AnchorSideLeft.Side := asrCenter;
  FProgressBar.AnchorSideTop.Control := Self;
  FProgressBar.AnchorSideTop.Side := asrCenter;

  Images := modSharedData.imgImages;
end;

function TMStreamTree.CreateItem(Caption: string; ImageIndex: Integer; Parent: TMenuItem): TMenuItem;
begin
  Result := TMenuItem.Create(FPopupMenu);
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

  inherited;
end;

procedure TMStreamTree.DoFreeNode(Node: PVirtualNode);
begin

  inherited;
end;

function TMStreamTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
var
  NodeData: PStreamNodeData;
begin
  Result := inherited;

  NodeData := PStreamNodeData(GetNodeData(Node));

  if ((Kind = ikNormal) or (Kind = ikSelected)) and (Column = 0) then
    if NodeData.Data.OwnRating > 0 then
      Index := TImages.STAR_BLUE_5 + 1 - NodeData.Data.OwnRating
    else if NodeData.Data.Rating > 0 then
      Index := TImages.STAR_5 + 1 - NodeData.Data.Rating
    else
      Index := TImages.TRANSMIT;
end;

procedure TMStreamTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
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

procedure TMStreamTree.DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;

  NodeHeight := TFunctions.GetTextSize('Wyg', Font).cy + 6 + TFunctions.GetTextSize('Wyg', Font).cy;
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
    Images.Resolution[8].Draw(PaintInfo.Canvas, L, 21, TImages.TAG_GREEN, gdeNormal);
    L := L + 9;
  end;

  if NodeData.Data.ChangesTitleInSong or (not NodeData.Data.RecordingOkay) then
    Images.Resolution[10].Draw(PaintInfo.Canvas, L, 21, TImages.CROSS, gdeNormal);
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
  if not SelectedOnly then
  begin
    Node := GetFirst;
    while Node <> nil do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Node;
      Node := GetNext(Node);
    end;
  end else
  begin
    SetLength(Result, 0);
    Nodes := GetSortedSelection(True);
    for i := 0 to Length(Nodes) - 1 do
    begin
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
    Result[High(Result)].Bitrate := NodeData.Data.Bitrate;
    Result[High(Result)].Name := NodeData.Data.Name;
    Result[High(Result)].URL := NodeData.Data.URL;
    Result[High(Result)].URLs := NodeData.Data.URLs;
    Result[High(Result)].Website := NodeData.Data.Website;
    Result[High(Result)].Rating := NodeData.Data.Rating;
    Result[High(Result)].RegExes := NodeData.Data.RegExes;
    Result[High(Result)].RecordingOkay := NodeData.Data.RecordingOkay;
    Result[High(Result)].IgnoreTitles := NodeData.Data.IgnoreTitles;
    Result[High(Result)].CanSetRegExps := NodeData.Data.CanSetRegExps;
  end;
end;

procedure TMStreamTree.HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo);
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

procedure TMStreamTree.HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: UInt64);
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
    end else if FoundStart then
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

procedure TMStreamTree.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal);
var
  NewText: string;
  Size: TSize;
  NodeData: PStreamNodeData;
begin
  NodeData := PStreamNodeData(GetNodeData(PaintInfo.Node));

  GetTextExtentPoint32W(PaintInfo.Canvas.Handle, 'Wl0', 3, Size);

  CellRect.Top := CellRect.Top + 2;
  DrawFormat := DT_TOP or DT_LEFT;
  inherited;

  CellRect.Top := CellRect.Top + 2 + Size.cy;

  NewText := '';

  if NodeData.Data.AudioType <> atNone then
    if NodeData.Data.AudioType = atMPEG then
      NewText := 'MP3'
    else if NodeData.Data.AudioType = atAAC then
      NewText := 'AAC';

  if NodeData.Data.Bitrate > 0 then
  begin
    if NewText <> '' then
      NewText := NewText + ' / ';
    NewText := NewText + IntToStr(NodeData.Data.Bitrate) + 'kbps';
  end;

  if NodeData.Data.Genre <> '' then
  begin
    if NewText <> '' then
      NewText := NewText + ' / ';
    NewText := NewText + NodeData.Data.Genre;
  end;

  if NewText = '' then
    NewText := _('No info available')
  else
    NewText := StringReplace(NewText, '&', '&&', [rfReplaceall]); // Wegen & und dem Shortcut..

  inherited DoTextDrawing(PaintInfo, NewText, CellRect, DrawFormat);
end;

procedure TMStreamTree.Paint;
var
  TmpText: string;
  R: TRect;
  Style: TTextStyle;
begin
  inherited;

  if FMode = moError then
  begin
    R := ClientRect;

    ZeroMemory(@Style, SizeOf(Style));
    Style.Alignment := taCenter;
    Style.Layout := tlCenter;

    Canvas.TextRect(R, R.Left, R.Top, _('No connection to server.'), Style);
  end;

  if FMode = moLoading then
  begin
    TmpText := _('Loading streams');

    R := ClientRect;
    R.Left := (R.Right div 2) - (Canvas.TextWidth(TmpText) div 2);
    R.Top := FProgressBar.Top - TFunctions.GetTextSize('Wyg', Font).cy - MulDiv(2, Screen.PixelsPerInch, 96);

    Canvas.TextRect(R, R.Left, R.Top, TmpText + FDots);
  end;
end;

procedure TMStreamTree.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
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
  else if (Sender = FItemRate1) or (Sender = FItemRate2) or (Sender = FItemRate3) or (Sender = FItemRate4) or (Sender = FItemRate5) then
    if Length(Streams) = 1 then
      case TMenuItem(Sender).Tag of
        1: Action := oaRate1;
        2: Action := oaRate2;
        3: Action := oaRate3;
        4: Action := oaRate4;
        5: Action := oaRate5;
      end else
      raise Exception.Create('');

  if (Action <> oaNone) and (Length(Streams) > 0) then
  begin
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Streams);
  end else if Action = oaRefresh then
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Streams);
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
  FItemAdd.Enabled := Length(Streams) > 0;
  FItemBlacklist.Enabled := Length(Streams) > 0;

  FItemRate.Enabled := HomeComm.CommunicationEstablished and (Length(Streams) = 1);
  FItemRefresh.Enabled := HomeComm.CommunicationEstablished;

  FItemSetData.Enabled := HomeComm.CommunicationEstablished and (Length(Streams) = 1) and (Streams[0].CanSetRegExps);

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
        FSortPopupMenu.Popup(P.X, P.Y - Header.Height);
    end;
  end;
end;

procedure TMStreamTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  NodeData: PStreamNodeData;
begin
  inherited;

  if AppGlobals.NodeColorsLoaded then
    Exit;

  NodeData := GetNodeData(Node);

  if CellPaintMode = cpmPaint then
  begin
    if FOnIsInClientList(Self, NodeData.Data.ID) then
      Canvas.Brush.Color := TFunctions.HTML2Color('c3c1c1')
    else
      case Node.Index mod 2 of
        0:
          Canvas.Brush.Color := Colors.BackGroundColor;
        1:
          Canvas.Brush.Color := TFunctions.HTML2Color('f3f3f3');
      end;
    Canvas.FillRect(CellRect);
  end;
end;

function TMStreamTree.DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean;
begin
  Result := inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Result := True;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(R);
end;

procedure TMStreamTree.DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(ItemRect);
end;

procedure TMStreamTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
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

function TMStreamTree.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
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

  case FSortType of
    stName: Result := CompareText(Data1.Data.Name, Data2.Data.Name);
    stBitrate: Result := TFunctions.CmpInt(Data1.Data.Bitrate, Data2.Data.Bitrate);
    stType: Result := CompareText(S1, S2);
    stRating: Result := TFunctions.CmpInt(Data1.Data.Rating, Data2.Data.Rating)
  end;

  if (Result = 0) and (FSortType <> stName) then
    Result := CompareText(Data2.Data.Name, Data1.Data.Name);

  if (Result = 0) and (FSortType <> stRating) then
    Result := TFunctions.CmpInt(Data1.Data.Rating, Data2.Data.Rating);
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
  P: string;
  Hash: Cardinal;
  Chars: Integer;
begin
  Result := False;
  P := TFunctions.BuildPattern(Search, Hash, Chars, False);

  if (not AlwaysBuild) and (P = FLastSearch) and (Genre = FLastGenre) and (AudioType = FLastAudioType) and (Bitrate = FLastBitrate) then
    Exit;

  Result := True;

  BeginUpdate;
  try
    Clear;

    AppGlobals.Lock;
    try
      for i := 0 to AppGlobals.Data.BrowserList.Count - 1 do
      begin
        Add := ((P = '*') or TFunctions.Like(LowerCase(AppGlobals.Data.BrowserList[i].Name), LowerCase(P))) and ((Genre = '') or (Pos(LowerCase(Genre), LowerCase(AppGlobals.Data.BrowserList[i].Genre)) > 0)) and
          ((AudioType = atNone) or (AppGlobals.Data.BrowserList[i].AudioType = AudioType)) and ((Bitrate = 0) or (AppGlobals.Data.BrowserList[i].Bitrate >= Bitrate));
        if Add then
        begin
          Node := AddChild(nil);
          NodeData := GetNodeData(Node);
          NodeData.Data := AppGlobals.Data.BrowserList[i];
        end;
      end;
    finally
      AppGlobals.Unlock;
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

  if Assigned(FColName) then
    FColName.Width := ClientWidth;
end;

procedure TMStreamTree.Sort(Node: PVirtualNode; Column: TColumnIndex; SortType: TSortTypes; Direction: TSortDirection);
begin
  FSortType := SortType;

  AppGlobals.BrowserSortType := Cardinal(FSortType);

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
  end else if FProgressBar.Visible then
    FProgressBar.Visible := False;

  FMode := Mode;

  Invalidate;
end;

function TMStreamTree.DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): string;
var
  NodeData: PStreamNodeData;
begin
  inherited;

  LineBreakStyle := hlbForceMultiLine;
  DoGetText(Node, Column, ttNormal, Result);
  NodeData := GetNodeData(Node);
  if NodeData.Data.Genre <> '' then
    Result := Result + #13#10 + NodeData.Data.Genre;
  Result := Result;
end;

{ TMStreamView }

procedure TMStreamBrowserView.BuildGenres;
var
  i: Integer;
begin
  FSearch.FGenreList.Clear;
  FSearch.FGenreList.ItemsEx.AddItem(_('- No genre -'));
  FSearch.FGenreList.ItemIndex := 0;

  AppGlobals.Lock;
  try
    for i := 0 to AppGlobals.Data.GenreList.Count - 1 do
      FSearch.FGenreList.ItemsEx.AddItem(AppGlobals.Data.GenreList[i].Name + ' (' + IntToStr(AppGlobals.Data.GenreList[i].StreamCount) + ')');
  finally
    AppGlobals.Unlock;
  end;

  FSearch.FGenreList.ItemsEx.SortType := stText;

  if AppGlobals.BrowserSearchGenre < FSearch.FGenreList.ItemsEx.Count then
    FSearch.FGenreList.ItemIndex := AppGlobals.BrowserSearchGenre;
end;

procedure TMStreamBrowserView.BuildTree(AlwaysBuild: Boolean);
var
  Genre: string;
  AudioType: TAudioTypes;
  Bitrate: Cardinal;
begin
  Genre := '';

  if FSearch.FGenreList.ItemIndex = -1 then
    Exit
  else if FSearch.FGenreList.ItemIndex > 0 then
  begin
    Genre := FSearch.FGenreList.ItemsEx[FSearch.FGenreList.ItemIndex].Caption;
    Genre := Copy(Genre, 1, Pos(' (', Genre) - 1);
  end;

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
    // Es ist etwas dreckig, dass wir das hier speichern. Aber so ist es am einfachsten...
    AppGlobals.BrowserSearchText := FSearch.FSearchEdit.Text;
    AppGlobals.BrowserSearchGenre := FSearch.FGenreList.ItemIndex;
    AppGlobals.BrowserSearchAudioType := Cardinal(AudioType);
    AppGlobals.BrowserSearchBitrate := FSearch.FKbpsList.ItemIndex;

    FStreamTree.Sort(nil, 0, FSortType, FStreamTree.Header.SortDirection);

    if FStreamTree.RootNodeCount = 1 then
      FCountLabel.Caption := Format(_('%d stream found'), [FStreamTree.RootNodeCount])
    else
      FCountLabel.Caption := Format(_('%d streams found'), [FStreamTree.RootNodeCount]);
  end;
end;

constructor TMStreamBrowserView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BevelOuter := bvNone;
  Align := alClient;

  FSearch := TMStreamSearchPanel.Create(Self);
  FSearch.Parent := Self;
  FSearch.Align := alTop;
  FSearch.AutoSize := True;
  FSearch.FSearchEdit.OnChange := SearchEditChange;
  FSearch.FGenreList.OnChange := ListsChange;
  FSearch.FKbpsList.OnChange := ListsChange;
  FSearch.FTypeList.OnChange := ListsChange;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Align := alBottom;
  FCountLabel.Parent := Self;

  FStreamTree := TMStreamTree.Create(Self);
  FStreamTree.Align := alClient;
  FStreamTree.Parent := Self;
  FStreamTree.OnHeaderClick := StreamBrowserHeaderClick;
  FStreamTree.FSortPopupMenu.FItemName.OnClick := SortItemClick;
  FStreamTree.FSortPopupMenu.FItemKbps.OnClick := SortItemClick;
  FStreamTree.FSortPopupMenu.FItemType.OnClick := SortItemClick;
  FStreamTree.FSortPopupMenu.FItemRating.OnClick := SortItemClick;

  FLoading := False;
  FSortType := TSortTypes(AppGlobals.BrowserSortType);
  FStreamTree.Header.SortDirection := TSortDirection(AppGlobals.BrowserSortDir);
  FSearch.FSearchEdit.Text := AppGlobals.BrowserSearchText;
  if AppGlobals.BrowserSearchAudioType < FSearch.FTypeList.ItemsEx.Count then
    FSearch.FTypeList.ItemIndex := AppGlobals.BrowserSearchAudioType;
  if AppGlobals.BrowserSearchBitrate < FSearch.FKbpsList.ItemsEx.Count then
    FSearch.FKbpsList.ItemIndex := AppGlobals.BrowserSearchBitrate;

  AppGlobals.Lock;
  try
    if (AppGlobals.Data.BrowserList.Count > 0) and (AppGlobals.Data.GenreList.Count > 0) then
    begin
      BuildGenres;
      BuildTree(True);
      SwitchMode(moShow);
    end;
  finally
    AppGlobals.Unlock;
  end;

  SortTree(False);

  HomeComm.OnServerDataReceived := HomeCommDataReceived;
end;

procedure TMStreamBrowserView.HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  FStreamTree.HomeCommBytesTransferred(CommandHeader, Transferred);
end;

procedure TMStreamBrowserView.HomeCommDataReceived(Sender: TObject);
begin
  FSearch.FGenreList.ItemsEx.Clear;
  FStreamTree.Clear;

  BuildGenres;
  BuildTree(True);

  AppGlobals.Data.BrowserList.CreateDict;

  SwitchMode(moShow);

  AppGlobals.LastBrowserUpdate := Trunc(Now);

  if Assigned(FOnStreamsReceived) then
    FOnStreamsReceived(Self);
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

procedure TMStreamBrowserView.SortTree(ResetSortDir: Boolean);
var
  i: Integer;
  SortDir: TSortDirection;
begin
  if ResetSortDir then
    SortDir := sdAscending
  else
    SortDir := TSortDirection(AppGlobals.BrowserSortDir);

  case FSortType of
    stName:
      FStreamTree.Header.Columns[0].Text := _('Name');
    stBitrate:
    begin
      FStreamTree.Header.Columns[0].Text := _('Kbps');
      SortDir := sdDescending;
    end;
    stType:
      FStreamTree.Header.Columns[0].Text := _('Type');
    stRating:
    begin
      FStreamTree.Header.Columns[0].Text := _('Rating');
      SortDir := sdDescending;
    end else
      raise Exception.Create('');
  end;

  for i := 0 to FStreamTree.FSortPopupMenu.Items.Count - 1 do
    if FStreamTree.FSortPopupMenu.Items[i].Tag = Integer(FSortType) then
    begin
      FStreamTree.FSortPopupMenu.Items[i].Checked := True;
      Break;
    end;

  FStreamTree.Sort(nil, 0, FSortType, SortDir);
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
    FSortType := stName
  else if Sender = FStreamTree.FSortPopupMenu.FItemKbps then
    FSortType := stBitrate
  else if Sender = FStreamTree.FSortPopupMenu.FItemType then
    FSortType := stType
  else if Sender = FStreamTree.FSortPopupMenu.FItemRating then
    FSortType := stRating
  else
    raise Exception.Create('');

  SortTree(True);
end;

procedure TMStreamBrowserView.StreamBrowserHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button = mbLeft then
  begin
    if FStreamTree.Header.SortDirection = sdAscending then
      FStreamTree.Header.SortDirection := sdDescending
    else
      FStreamTree.Header.SortDirection := sdAscending;

    AppGlobals.BrowserSortDir := Cardinal(FStreamTree.Header.SortDirection);

    FStreamTree.Sort(nil, 0, FSortType, FStreamTree.Header.SortDirection);
  end;
end;

procedure TMStreamBrowserView.SwitchMode(Mode: TModes);
var
  i: Integer;
begin
  for i := 0 to FSearch.ControlCount - 1 do
    FSearch.Controls[i].Enabled := Mode = moShow;

  FCountLabel.Enabled := Mode = moShow;

  FMode := Mode;

  FStreamTree.SwitchMode(Mode);
end;

procedure TMStreamBrowserView.PostTranslate;
var
  Idx: Integer;
begin
  FStreamTree.FColName.Text := _('Rating');

  if FSearch.FGenreList.ItemsEx.Count > 0 then
  begin
    Idx := FSearch.FGenreList.ItemIndex;
    FSearch.FGenreList.ItemsEx[0].Caption := _('- No genre -');
    FSearch.FGenreList.ItemIndex := Idx;
  end;
  if FSearch.FKbpsList.ItemsEx.Count > 0 then
  begin
    Idx := FSearch.FKbpsList.ItemIndex;
    FSearch.FKbpsList.ItemsEx[0].Caption := _('- No kbps -');
    FSearch.FKbpsList.ItemIndex := Idx;
  end;
  if FSearch.FTypeList.ItemsEx.Count > 0 then
  begin
    Idx := FSearch.FTypeList.ItemIndex;
    FSearch.FTypeList.ItemsEx[0].Caption := _('- No type -');
    FSearch.FTypeList.ItemIndex := Idx;
  end;

  if FStreamTree.RootNodeCount = 1 then
    FCountLabel.Caption := Format(_('%d stream found'), [FStreamTree.RootNodeCount])
  else
    FCountLabel.Caption := Format(_('%d streams found'), [FStreamTree.RootNodeCount]);

  FStreamTree.FSortPopupMenu.PostTranslate;
end;

{ TMStreamSearch }

constructor TMStreamSearchPanel.Create(AOwner: TComponent);

  function CreatePanel(const LabelText: string): TPanel;
  var
    L: TLabel;
  begin
    Result := TPanel.Create(Self);
    Result.Parent := Self;
    Result.Align := alTop;
    Result.AutoSize := True;
    Result.BevelOuter := bvNone;

    L := TLabel.Create(Self);
    L.Parent := Result;
    L.Align := alLeft;
    L.Caption := LabelText;
    L.Layout := tlCenter;
    L.Constraints.MinWidth := 60;
  end;

var
  P: TPanel;
begin
  inherited;

  BevelOuter := bvNone;

  P := CreatePanel(_('Type') + ':');
  FTypeList := TComboBoxEx.Create(Self);
  FTypeList.Parent := P;
  FTypeList.Align := alClient;
  //  FTypeList.Style := csDropDownList;

  FTypeList.ItemsEx.AddItem(_('- No type -'));
  FTypeList.ItemsEx.AddItem(_('MP3'));
  FTypeList.ItemsEx.AddItem(_('AAC'));
  FTypeList.ItemIndex := 0;

  P := CreatePanel(_('Kbps') + ':');
  FKbpsList := TComboBoxEx.Create(Self);
  FKbpsList.Parent := P;
  FKbpsList.Align := alClient;
  //  FKbpsList.Style := csDropDownList;

  FKbpsList.ItemsEx.AddItem(_('- No kbps -'));
  FKbpsList.ItemsEx.AddItem('>= 64');
  FKbpsList.ItemsEx.AddItem('>= 128');
  FKbpsList.ItemsEx.AddItem('>= 192');
  FKbpsList.ItemsEx.AddItem('>= 256');
  FKbpsList.ItemsEx.AddItem('= 320');
  FKbpsList.ItemIndex := 0;

  P := CreatePanel(_('Genre') + ':');
  FGenreList := TComboBoxEx.Create(Self);
  FGenreList.Parent := P;
  FGenreList.Align := alClient;
  //  FGenreList.Style := csDropDownList;
  FGenreList.DropDownCount := 16;

  P := CreatePanel(_('Search') + ':');
  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Parent := P;
  FSearchEdit.Align := alClient;
end;

{ TMStreamTreeHeaderPopup }

constructor TMStreamTreeHeaderPopup.Create(AOwner: TComponent);
begin
  inherited;

  FItemName := TMenuItem.Create(Self);
  FItemName.Caption := _('Name');
  FItemName.RadioItem := True;
  FItemName.Tag := Integer(stName);
  Items.Add(FItemName);

  FItemKbps := TMenuItem.Create(Self);
  FItemKbps.Caption := _('Kbps');
  FItemKbps.RadioItem := True;
  FItemKbps.Tag := Integer(stBitrate);
  Items.Add(FItemKbps);

  FItemType := TMenuItem.Create(Self);
  FItemType.Caption := _('Type');
  FItemType.RadioItem := True;
  FItemType.Tag := Integer(stType);
  Items.Add(FItemType);

  FItemRating := TMenuItem.Create(Self);
  FItemRating.Caption := _('Rating');
  FItemRating.RadioItem := True;
  FItemRating.Tag := Integer(stRating);
  Items.Add(FItemRating);
end;

procedure TMStreamTreeHeaderPopup.DoPopup(Sender: TObject);
begin
  inherited;

end;

procedure TMStreamTreeHeaderPopup.PostTranslate;
begin
  FItemName.Caption := _('Name');
  FItemKbps.Caption := _('Kbps');
  FItemType.Caption := _('Type');
  FItemRating.Caption := _('Rating');
end;

end.
