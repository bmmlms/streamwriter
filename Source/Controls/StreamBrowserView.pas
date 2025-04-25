{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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
  Constants,
  Controls,
  DataManager,
  DragDrop,
  DragDropFile,
  DropSource,
  DynBASS,
  ExtCtrls,
  Forms,
  Functions,
  Generics.Collections,
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
  MStringFunctions,
  regexpr,
  SharedControls,
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

  { TMStreamSearchPanel }

  TMStreamSearchPanel = class(TPanel, IPostTranslatable)
  private
    FSearchEdit: TEdit;
    FGenreList: TComboBoxEx;
    FKbpsList: TComboBoxEx;
    FTypeList: TComboBoxEx;

    FPanelLabels: TList<TLabel>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PostTranslate;
  end;

  TSortTypes = (stName, stBitrate, stType, stRating);

  { TStreamTreeColumns }

  TStreamTreeColumns = class(TVirtualTreeColumns)
  public
    procedure PaintHeader(TargetCanvas: TCanvas; R: TRect; const Target: TPoint; RTLOffset: Integer = 0); overload; override;
    function ColumnFromPosition(const P: TPoint; Relative: Boolean = True): TColumnIndex; overload; override;
  end;

  { TStreamTreeHeader }

  TStreamTreeHeader = class(TVTHeader)
  private
    FSortDown: Boolean;
    FSortHover: Boolean;
    FIgnoreNextButtonDown: Boolean;
  protected
    function GetColumnsClass: TVirtualTreeColumnsClass; override;
    function HandleMessage(var Message: TLMessage): Boolean; override;
  end;

  { TMStreamBrowserView }

  TMStreamBrowserView = class(TPanel, IPostTranslatable)
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

    procedure HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: Cardinal);

    property Mode: TModes read FMode;
    property StreamTree: TMStreamTree read FStreamTree;

    property OnStreamsReceived: TNotifyEvent read FOnStreamsReceived write FOnStreamsReceived;
  end;

  TMStreamTreeHeaderPopup = class(TPopupMenu, IPostTranslatable)
  private
    FItemName: TMenuItem;
    FItemKbps: TMenuItem;
    FItemType: TMenuItem;
    FItemRating: TMenuItem;

    procedure PostTranslate;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TMStreamTree }

  TMStreamTree = class(TMSWVirtualTree)
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
    procedure DoDragging(P: TPoint); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): string; override;
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    function GetHeaderClass: TVTHeaderClass; override;
    procedure HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo); override;
    procedure Resize; override;
    procedure Paint; override;
    procedure KeyPress(var Key: Char); override;

    procedure TimerOnTimer(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);

    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure CreateHandle; override;

    procedure InvalidateVisible;

    procedure SwitchMode(Mode: TModes);

    procedure Sort(Node: PVirtualNode; Column: TColumnIndex; SortType: TSortTypes; Direction: TSortDirection); reintroduce;

    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    function Build(AlwaysBuild: Boolean; Search, Genre: string; AudioType: TAudioTypes; Bitrate: Cardinal): Boolean;
    procedure ReceiveError;
    procedure HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: Cardinal);

    property PopupMenu2: TPopupMenu read FPopupMenu;
    property DraggedStreams: TStreamDataArray read FDraggedStreams;

    property OnNeedData: TNeedDataEvent read FOnNeedData write FOnNeedData;
    property OnAction: TActionEvent read FOnAction write FOnAction;
    property OnIsInClientList: TIsInClientListEvent read FOnIsInClientList write FOnIsInClientList;
  end;

implementation

{ TStreamTreeHeader }

function TStreamTreeHeader.GetColumnsClass: TVirtualTreeColumnsClass;
begin
  Result := TStreamTreeColumns;
end;

function TStreamTreeHeader.HandleMessage(var Message: TLMessage): Boolean;
var
  P: TPoint;
  ColRect, ButtonRect: TRect;
  MouseEvent: TLMMouseEvent absolute Message;
  MouseMove: TLMMouseMove absolute Message;
begin
  case Message.msg of
    LM_LBUTTONDOWN:
    begin
      P := Classes.Point(MouseEvent.X, MouseEvent.Y);

      ColRect := Columns[0].GetRect;
      ButtonRect := TRect.Create(ColRect.Right - Treeview.Scale96ToFont(16 + 2), ColRect.Top, ColRect.Right, ColRect.Bottom);

      if ButtonRect.Contains(P) and not FIgnoreNextButtonDown then
      begin
        FSortDown := True;
        Invalidate(nil);

        P := TPoint.Create(ColRect.Right - Treeview.Scale96ToFont(16 + 2), ColRect.Top + ColRect.Height);
        ClientToScreen(Treeview.Handle, P);
        PopupMenu.PopUp(P.X, P.Y);

        FSortDown := False;
        Invalidate(nil);

        if GetCursorPos(P) then
        begin
          ScreenToClient(Treeview.Handle, P);
          if ButtonRect.Contains(P) then
            FIgnoreNextButtonDown := True
          else
            FSortHover := False;
        end;
      end else
        FIgnoreNextButtonDown := False;
    end;
    LM_RBUTTONUP, LM_RBUTTONDOWN:
    begin
      P := Classes.Point(MouseEvent.X, MouseEvent.Y);

      if InHeader(P) and (Columns.ColumnFromPosition(P) = NoColumn) then
      begin
        Message.Result := 1;
        Exit(True);
      end;
    end;
    CM_MOUSELEAVE:
    begin
      if FSortDown then
        Exit(True);

      FSortHover := False;

      Invalidate(nil);
    end;
    LM_MOUSEMOVE:
      with TLMMouseMove(Message) do
      begin
        P := Classes.Point(MouseMove.XPos, MouseMove.YPos);

        ColRect := Columns[0].GetRect;
        ButtonRect := TRect.Create(ColRect.Right - Treeview.Scale96ToFont(16 + 2), ColRect.Top, ColRect.Right, ColRect.Bottom);

        if (ButtonRect.Contains(P) <> FSortHover) or (not ButtonRect.Contains(P) and FSortDown) then
          Invalidate(nil);

        if FSortDown and not ButtonRect.Contains(P) then
          FSortDown := False;

        FSortHover := ButtonRect.Contains(P);
      end;
  end;

  Result := inherited HandleMessage(Message);
end;

{ TStreamTreeColumns }

procedure TStreamTreeColumns.PaintHeader(TargetCanvas: TCanvas; R: TRect; const Target: TPoint; RTLOffset: Integer);
var
  Details: TThemedElementDetails;
begin
  inherited PaintHeader(TargetCanvas, R, Target, RTLOffset);

  R := TRect.Create(Min(R.Right, TotalWidth) - Header.Treeview.Scale96ToFont(16 + 2), Max(R.Top, 0), Min(R.Right, TotalWidth), Min(R.Bottom, Header.Height));

  if TStreamTreeHeader(Header).FSortDown then
    Details := ThemeServices.GetElementDetails(thHeaderItemPressed)
  else if TStreamTreeHeader(Header).FSortHover then
    Details := ThemeServices.GetElementDetails(thHeaderItemHot)
  else
    Details := ThemeServices.GetElementDetails(thHeaderItemNormal);

  ThemeServices.DrawElement(TargetCanvas.Handle, Details, R, nil);

  if TStreamTreeHeader(Header).FSortDown then
    R.Offset(1, 1);

  modSharedData.imgImages.ResolutionForPPI[16, Header.Font.PixelsPerInch, Header.Treeview.GetCanvasScaleFactor].Draw(TargetCanvas, R.Right - Header.Treeview.Scale96ToFont(16), R.Top + R.Height div 2 - Header.Treeview.Scale96ToFont(16) div 2, TImages.SORT, IfThen<TGraphicsDrawEffect>(Header.Treeview.Enabled, gdeNormal, gdeDisabled));
end;

function TStreamTreeColumns.ColumnFromPosition(const P: TPoint; Relative: Boolean): TColumnIndex;
begin
  if P.X > Items[0].Width - Header.Treeview.Scale96ToFont(16 + 2) then
    Exit(NoColumn);

  Result := inherited ColumnFromPosition(P, Relative);
end;

{ TMStreamView }

constructor TMStreamTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMode := moShow;

  NodeDataSize := SizeOf(TStreamNodeData);
  IncrementalSearch := isVisibleOnly;

  TreeOptions.SelectionOptions := [toDisableDrawSelection, toRightClickSelect, toFullRowSelect, toMultiSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop] + [toFullRowDrag];
  Header.Options := Header.Options + [hoShowSortGlyphs, hoVisible, hoOwnerDraw] - [hoDrag, hoColumnResize];
  DragMode := dmAutomatic;

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

  FItemOpen := CreateItem('Play stream (e&xternal player)', TImages.PLAY_GO, nil);
  FItemOpen.OnClick := PopupMenuClick;

  FItemAdd := CreateItem('&Add stream', TImages.ADD, nil);
  FItemAdd.OnClick := PopupMenuClick;

  FItemSetData := CreateItem('S&et data...', TImages.PENCIL, nil);
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

  FItemCopy := CreateItem('&Copy URL', TImages.LINK_PAGE_WHITE_COPY, nil);
  FItemCopy.OnClick := PopupMenuClick;

  FItemBlacklist := CreateItem('Add to &blacklist', TImages.PAGE_WHITE_TRANSMIT_ADD, nil);
  FItemBlacklist.OnClick := PopupMenuClick;

  FItemSave := CreateItem('&Save as playlist...', TImages.PAGE_WHITE_MUSIC_OUT, nil);
  FItemSave.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;

  FDots := '';
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerOnTimer;
  FTimer.Interval := 1000;
  FTimer.Enabled := False;

  Header.SortColumn := 0;
  Header.SortDirection := sdDescending;

  FSortPopupMenu := TMStreamTreeHeaderPopup.Create(Self);
  Header.PopupMenu := FSortPopupMenu;

  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Parent := Self;
  FProgressBar.Width := Scale96ToFont(150);
  FProgressBar.Height := Scale96ToFont(PROGRESSBAR_HEIGHT);
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

procedure TMStreamTree.CreateHandle;
var
  Node: PVirtualNode;
  NodeHeight: Integer;
  F: TFont;
begin
  inherited;

  F := TFont.Create;
  try
    F.Assign(Font);
    F.Size := Round((Graphics.GetFontData(Font.Handle).Height * 72 / Font.PixelsPerInch) * -1) - 1;
    NodeHeight := TMStringFunctions.GetTextSize(MeasureTextHeightString, Font).Height + TMStringFunctions.GetTextSize(MeasureTextHeightString, F).Height + Scale96ToFont(6);
  finally
    F.Free;
  end;

  if DefaultNodeHeight <> NodeHeight then
  begin
    Node := GetFirst;
    while Node <> nil do
    begin
      Node.NodeHeight := NodeHeight;
      Node.TotalHeight := NodeHeight;
      Node := GetNext(Node);
    end;

    DefaultNodeHeight := NodeHeight;

    RootNode.NodeHeight := NodeHeight;
    RootNode.TotalHeight := NodeHeight * RootNodeCount;
  end;
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

procedure TMStreamTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  L: Integer;
  NodeData: PStreamNodeData;
begin
  inherited;

  NodeData := GetNodeData(PaintInfo.Node);

  L := PaintInfo.ImageInfo[iiNormal].XPos;

  if NodeData.Data.MetaData then
  begin
    Images.ResolutionForPPI[9, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(PaintInfo.Canvas, L, Scale96ToFont(16) + Margin, TImages.TAG_GREEN, gdeNormal);
    L := L + Scale96ToFont(11);
  end;

  if NodeData.Data.ChangesTitleInSong or (not NodeData.Data.RecordingOkay) then
    Images.ResolutionForPPI[9, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(PaintInfo.Canvas, L, Scale96ToFont(16) + Margin, TImages.CROSS, gdeNormal);
end;

procedure TMStreamTree.FitColumns;
begin
  Header.AutoSizeIndex := 1;
  Header.Options := Header.Options + [hoAutoResize];
end;

function TMStreamTree.GetNodes(SelectedOnly: Boolean): TNodeArray;
var
  Node: PVirtualNode;
  Nodes: TNodeArray;
begin
  Result := [];
  if not SelectedOnly then
  begin
    Node := GetFirst;
    while Node <> nil do
    begin
      Result += [Node];
      Node := GetNext(Node);
    end;
  end else
  begin
    Nodes := GetSortedSelection(True);
    for Node in Nodes do
      Result += [Node];
  end;
end;

function TMStreamTree.GetSelected: TStreamDataArray;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PStreamNodeData;
  StreamData: TStreamData;
begin
  Result := [];
  Nodes := GetNodes(True);

  for i := 0 to High(Nodes) do
  begin
    NodeData := GetNodeData(Nodes[i]);

    StreamData.ID := NodeData.Data.ID;
    StreamData.Bitrate := NodeData.Data.Bitrate;
    StreamData.Name := NodeData.Data.Name;
    StreamData.URL := NodeData.Data.URL;
    StreamData.URLs := NodeData.Data.URLs;
    StreamData.Website := NodeData.Data.Website;
    StreamData.Rating := NodeData.Data.Rating;
    StreamData.RegExes := NodeData.Data.RegExes;
    StreamData.RecordingOkay := NodeData.Data.RecordingOkay;
    StreamData.IgnoreTitles := NodeData.Data.IgnoreTitles;
    StreamData.CanSetRegExps := NodeData.Data.CanSetRegExps;

    Result += [StreamData];
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
      case AppGlobals.DefaultActionNewStream of
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

procedure TMStreamTree.HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: Cardinal);
begin
  if FProgressBar.Position < 100 then
    FProgressBar.Position := FProgressBar.Position + 1;

  if CommandHeader.CommandLength > 0 then
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

  if Key = Char(VK_RETURN) then
  begin
    case AppGlobals.DefaultActionNewStream of
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
  NodeData: PStreamNodeData;
  LineHeight, MaxTextWidth: Integer;
begin
  NodeData := PStreamNodeData(GetNodeData(PaintInfo.Node));

  LineHeight := PaintInfo.Canvas.GetTextHeight(MeasureTextHeightString);

  CellRect.Top := CellRect.Top + Scale96ToFont(2);
  DrawFormat := DT_TOP or DT_LEFT;

  MaxTextWidth := ClientWidth - PaintInfo.ContentRect.Left - TextMargin * 2;

  inherited;

  CellRect.Top := CellRect.Top + Scale96ToFont(2) + LineHeight;

  NewText := '';

  if NodeData.Data.AudioType <> atNone then
    if NodeData.Data.AudioType = atMPEG then
      NewText := 'MP3'
    else if NodeData.Data.AudioType = atAAC then
      NewText := 'AAC';

  if NodeData.Data.Bitrate > 0 then
  begin
    if NewText <> '' then
      NewText := NewText + ' ● ';
    NewText := NewText + IntToStr(NodeData.Data.Bitrate) + 'kbps';
  end;

  if NodeData.Data.Genre <> '' then
  begin
    if NewText <> '' then
      NewText := NewText + ' ● ';
    NewText := NewText + NodeData.Data.Genre;
  end;

  if NewText = '' then
    NewText := _('No info available')
  else
    NewText := StringReplace(NewText, '&', '&&', [rfReplaceall]); // Wegen & und dem Shortcut..

  PaintInfo.Canvas.Font.Size := Round((Graphics.GetFontData(PaintInfo.Canvas.Font.Handle).Height * 72 / Font.PixelsPerInch) * -1) - 1;

  if PaintInfo.Canvas.GetTextWidth(NewText) > MaxTextWidth then
    NewText := ShortenString(PaintInfo.Canvas.Handle, NewText, MaxTextWidth, 0);

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
    R.Top := FProgressBar.Top - Canvas.GetTextHeight(MeasureTextHeightString) - Scale96ToFont(2);

    Canvas.TextRect(R, R.Left, R.Top, TmpText + FDots);
  end;
end;

procedure TMStreamTree.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
begin
  PaintInfo.ImageInfo[ImageInfoIndex].XPos := TextMargin;
  PaintInfo.ImageInfo[ImageInfoIndex].YPos := Scale96ToFont(2);

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

procedure TMStreamTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  NodeData: PStreamNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  if CellPaintMode = cpmPaint then
  begin
    if FOnIsInClientList(Self, NodeData.Data.ID) then
      Canvas.Brush.Color := TFunctions.SimilarColor(Colors.BackGroundColor, 30)
    else
      case Node.Index mod 2 of
        0:
          Canvas.Brush.Color := Colors.BackGroundColor;
        1:
          Canvas.Brush.Color := TFunctions.SimilarColor(Colors.BackGroundColor, 5);
      end;
    Canvas.FillRect(CellRect);
  end;
end;

function TMStreamTree.GetHeaderClass: TVTHeaderClass;
begin
  Result := TStreamTreeHeader;
end;

function TMStreamTree.DoCompare(Node1: PVirtualNode; Node2: PVirtualNode; Column: TColumnIndex): Integer;
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
  Entries: TStreamDataArray;
  StreamData: TStreamData;
begin
  if FDragSource.DragInProgress then
    Exit;

  FDragSource.Files.Clear;
  Entries := GetSelected;
  for StreamData in Entries do
    FDragSource.Files.Add(StreamData.URL);

  if FDragSource.Files.Count = 0 then
    Exit;

  DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending]);
  FDraggedStreams := GetSelected;
  FDragSource.Execute(False);
  FDraggedStreams := [];
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
  Result := inherited;

  LineBreakStyle := hlbForceMultiLine;
  DoGetText(Node, Column, ttNormal, Result);
  NodeData := GetNodeData(Node);
  if NodeData.Data.Genre <> '' then
    Result := Result + #13#10 + NodeData.Data.Genre;
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
  FSearch.Align := alTop;
  FSearch.ChildSizing.TopBottomSpacing := 4;
  FSearch.ChildSizing.LeftRightSpacing := 4;
  FSearch.ChildSizing.HorizontalSpacing := 4;
  FSearch.ChildSizing.VerticalSpacing := 4;
  FSearch.AutoSize := True;
  FSearch.FSearchEdit.OnChange := SearchEditChange;
  FSearch.FGenreList.OnChange := ListsChange;
  FSearch.FKbpsList.OnChange := ListsChange;
  FSearch.FTypeList.OnChange := ListsChange;
  FSearch.Parent := Self;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Align := alBottom;
  FCountLabel.BorderSpacing.Top := Scale96ToFont(2);
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

procedure TMStreamBrowserView.HomeCommBytesTransferred(CommandHeader: TCommandHeader; Transferred: Cardinal);
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
    L.BorderSpacing.Right := Scale96ToFont(4);

    FPanelLabels.Add(L);
  end;

var
  P: TPanel;
begin
  inherited;

  FPanelLabels := TList<TLabel>.Create;

  BevelOuter := bvNone;

  P := CreatePanel(_('Type') + ':');
  FTypeList := TComboBoxEx.Create(Self);
  FTypeList.Align := alClient;
  FTypeList.ItemHeight := Scale96ToFont(17);
  FTypeList.Parent := P;

  FTypeList.ItemsEx.AddItem(_('- No type -'));
  FTypeList.ItemsEx.AddItem(_('MP3'));
  FTypeList.ItemsEx.AddItem(_('AAC'));
  FTypeList.ItemIndex := 0;

  P := CreatePanel(_('Kbps') + ':');
  FKbpsList := TComboBoxEx.Create(Self);
  FKbpsList.Align := alClient;
  FKbpsList.ItemHeight := Scale96ToFont(17);
  FKbpsList.Parent := P;

  FKbpsList.ItemsEx.AddItem(_('- No kbps -'));
  FKbpsList.ItemsEx.AddItem('>= 64');
  FKbpsList.ItemsEx.AddItem('>= 128');
  FKbpsList.ItemsEx.AddItem('>= 192');
  FKbpsList.ItemsEx.AddItem('>= 256');
  FKbpsList.ItemsEx.AddItem('= 320');
  FKbpsList.ItemIndex := 0;

  P := CreatePanel(_('Genre') + ':');
  FGenreList := TComboBoxEx.Create(Self);
  FGenreList.Align := alClient;
  FGenreList.DropDownCount := 16;
  FGenreList.ItemHeight := Scale96ToFont(17);
  FGenreList.Parent := P;

  P := CreatePanel(_('Search') + ':');
  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Align := alClient;
  FSearchEdit.Parent := P;
end;

destructor TMStreamSearchPanel.Destroy;
begin
  FPanelLabels.Free;

  inherited Destroy;
end;

procedure TMStreamSearchPanel.PostTranslate;
var
  TextLen: Integer;
  MaxTextLen: Integer = 0;
  Lbl: TLabel;
begin
  for Lbl in FPanelLabels do
  begin
    TextLen := TMStringFunctions.GetTextSize(Lbl.Caption, Lbl.Font).Width;
    if TextLen > MaxTextLen then
      MaxTextLen := TextLen;
  end;

  for Lbl in FPanelLabels do
    Lbl.Constraints.MinWidth := MaxTextLen;
end;

{ TMStreamTreeHeaderPopup }

constructor TMStreamTreeHeaderPopup.Create(AOwner: TComponent);
begin
  inherited;

  FItemName := TMenuItem.Create(Self);
  FItemName.Caption := _('Name');
  FItemName.RadioItem := True;
  FItemName.Tag := PtrInt(stName);
  Items.Add(FItemName);

  FItemKbps := TMenuItem.Create(Self);
  FItemKbps.Caption := _('Kbps');
  FItemKbps.RadioItem := True;
  FItemKbps.Tag := PtrInt(stBitrate);
  Items.Add(FItemKbps);

  FItemType := TMenuItem.Create(Self);
  FItemType.Caption := _('Type');
  FItemType.RadioItem := True;
  FItemType.Tag := PtrInt(stType);
  Items.Add(FItemType);

  FItemRating := TMenuItem.Create(Self);
  FItemRating.Caption := _('Rating');
  FItemRating.RadioItem := True;
  FItemRating.Tag := PtrInt(stRating);
  Items.Add(FItemRating);
end;

procedure TMStreamTreeHeaderPopup.PostTranslate;
begin
  FItemName.Caption := _('Name');
  FItemKbps.Caption := _('Kbps');
  FItemType.Caption := _('Type');
  FItemRating.Caption := _('Rating');
end;

end.
