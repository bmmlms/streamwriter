{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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

unit LogTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, LanguageObjects, Tabs, Functions, AppData, Logging, VirtualTrees,
  HomeCommunication, DataManager, ImgList, Graphics, Math, Generics.Collections,
  Menus, ChartsTabAdjustTitleName, Forms, TypeDefs, MessageBus, AppMessages,
  HomeCommands, Commands, GUIFunctions, SharedData, PerlRegEx, Messages,
  DateUtils, SharedControls, Clipbrd;

type
  TFilterTypes = set of TLogLevel;

  TLogEntry = class
  public
    Text: string;
    TextSource: string;
    Source: TLogSource;
    LogType: TLogType;
    Level: TLogLevel;
    Time: TDateTime;
    constructor Create(Text, TextSource: string; Time: TDateTime; Source: TLogSource; LogType: TLogType; Level: TLogLevel);
  end;

  TLogNodeData = record
    LogEntry: TLogEntry;
  end;
  PLogNodeData = ^TLogNodeData;

  TLogPopup = class(TPopupMenu)
  private
    FItemDebug: TMenuItem;
    FItemInfo: TMenuItem;
    FItemWarning: TMenuItem;
    FItemError: TMenuItem;
    FItemCopy: TMenuItem;
    FItemClear: TMenuItem;
  protected

  public
    constructor Create(AOwner: TComponent); override;

    property ItemDebug: TMenuItem read FItemDebug;
    property ItemInfo: TMenuItem read FItemInfo;
    property ItemWarning: TMenuItem read FItemWarning;
    property ItemError: TMenuItem read FItemError;
    property ItemCopy: TMenuItem read FItemCopy;
    property ItemClear: TMenuItem read FItemClear;
  end;

  TLogPanel = class(TPanel)
  private
    FLabel: TLabel;
    FSearch: TEdit;
    FToolbar: TToolBar;

    FButtonDebug: TToolButton;
    FButtonInfo: TToolButton;
    FButtonWarning: TToolButton;
    FButtonError: TToolButton;
    FButtonCopy: TToolButton;
    FButtonClear: TToolButton;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    procedure AfterCreate;

    procedure PostTranslate;
  end;

  TLogTree = class(TVirtualStringTree)
  private
    FPopupMenu: TLogPopup;

    FColType: TVirtualTreeColumn;
    FColTime: TVirtualTreeColumn;
    FColSource: TVirtualTreeColumn;
    FColText: TVirtualTreeColumn;

    FLog: TList<TLogEntry>;

    FFilterPattern: string;
    FFilterTypes: TFilterTypes;

    FHeaderDragSourcePosition: Cardinal;

    procedure FitColumns;
    procedure MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
    function MatchesFilter(LogEntry: TLogEntry): Boolean;
    procedure Add(LogEntry: TLogEntry); overload;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean;
      var Index: Integer): TCustomImageList; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo;
      ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    function DoHeaderDragging(Column: TColumnIndex): Boolean; override;
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); override;

    procedure MessageReceived(Msg: TMessageBase);
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure AfterCreate;

    procedure PostTranslate;

    procedure Add(Source: TLogSource; LogType: TLogType; LogLevel: TLogLevel; SourceText, Text: string; Time: TDateTime); overload;
    procedure SetFilter(Text: string; FilterTypes: TFilterTypes);
  end;

  TLogTab = class(TMainTabSheet)
  private
    FLogPanel: TLogPanel;
    FLogTree: TLogTree;

    procedure UpdateButtons;

    procedure TextChange(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);
    procedure LogTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure AfterCreate; override;

    procedure PostTranslate;

    property LogTree: TLogTree read FLogTree;
  end;

implementation

{ TLogTab }

procedure TLogTab.AfterCreate;
begin
  inherited;

  FLogTree.AfterCreate;
  FLogPanel.AfterCreate;

  FLogTree.Images := modSharedData.imgImages;

  if Screen.PixelsPerInch = 96 then
    FLogTree.PopupMenu.Images := modSharedData.imgImages;

  FLogPanel.FSearch.OnChange := TextChange;
  FLogPanel.FButtonDebug.OnClick := ButtonClick;
  FLogPanel.FButtonInfo.OnClick := ButtonClick;
  FLogPanel.FButtonWarning.OnClick := ButtonClick;
  FLogPanel.FButtonError.OnClick := ButtonClick;
  FLogPanel.FButtonCopy.OnClick := ButtonClick;
  FLogPanel.FButtonClear.OnClick := ButtonClick;

  Caption := 'Log';
end;

procedure TLogTab.ButtonClick(Sender: TObject);
var
  i: Integer;
  s: string;
  Node: PVirtualNode;
  NodeData: PLogNodeData;
begin
  if Sender = FLogPanel.FButtonDebug then
  begin
    if FLogPanel.FButtonDebug.Down then
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes + [llDebug])
    else
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes - [llDebug]);
  end else if Sender = FLogPanel.FButtonInfo then
  begin
    if FLogPanel.FButtonInfo.Down then
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes + [llInfo])
    else
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes - [llInfo]);
  end else if Sender = FLogPanel.FButtonWarning then
  begin
    if FLogPanel.FButtonWarning.Down then
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes + [llWarning])
    else
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes - [llWarning]);
  end else if Sender = FLogPanel.FButtonError then
  begin
    if FLogPanel.FButtonError.Down then
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes + [llError])
    else
      FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes - [llError]);
  end else if Sender = FLogPanel.FButtonCopy then
  begin
    if FLogTree.RootNodeCount > 0 then
    begin
      s := '';
      Node := FLogTree.GetFirst;
      while Node <> nil do
      begin
        NodeData := FLogTree.GetNodeData(Node);
        if FLogTree.Selected[Node] or (FLogTree.SelectedCount = 0) then
          s := s + TimeToStr(NodeData.LogEntry.Time) + ' - ' + NodeData.LogEntry.TextSource + ' - ' + NodeData.LogEntry.Text + #13#10;
        Node := FLogTree.GetNext(Node)
      end;
      Clipboard.Clear;
      Clipboard.SetTextBuf(PChar(s));
    end;
  end else if Sender = FLogPanel.FButtonClear then
  begin
    FLogTree.Clear;
    for i := 0 to FLogTree.FLog.Count - 1 do
      FLogTree.FLog[i].Free;
    FLogTree.FLog.Clear;
  end;

  FLogPanel.FButtonDebug.Down := llDebug in FLogTree.FFilterTypes;
  FLogPanel.FButtonInfo.Down := llInfo in FLogTree.FFilterTypes;
  FLogPanel.FButtonWarning.Down := llWarning in FLogTree.FFilterTypes;
  FLogPanel.FButtonError.Down := llError in FLogTree.FFilterTypes;

  FLogTree.FPopupMenu.FItemDebug.Checked := llDebug in FLogTree.FFilterTypes;
  FLogTree.FPopupMenu.FItemInfo.Checked := llInfo in FLogTree.FFilterTypes;
  FLogTree.FPopupMenu.FItemWarning.Checked := llWarning in FLogTree.FFilterTypes;
  FLogTree.FPopupMenu.FItemError.Checked := llError in FLogTree.FFilterTypes;

  UpdateButtons;
end;

procedure TLogTab.LogTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateButtons;
end;

constructor TLogTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ImageIndex := 3;

  FLogPanel := TLogPanel.Create(Self);
  FLogPanel.Parent := Self;
  FLogPanel.Align := alTop;
  FLogPanel.Padding.Top := 1;

  FLogTree := TLogTree.Create(Self);
  FLogTree.Parent := Self;
  FLogTree.Align := alClient;
  FLogTree.OnChange := LogTreeChange;

  FLogTree.FPopupMenu.ItemInfo.OnClick := PopupMenuClick;
  FLogTree.FPopupMenu.ItemWarning.OnClick := PopupMenuClick;
  FLogTree.FPopupMenu.ItemError.OnClick := PopupMenuClick;
  FLogTree.FPopupMenu.ItemCopy.OnClick := PopupMenuClick;
  FLogTree.FPopupMenu.ItemClear.OnClick := PopupMenuClick;

  ShowCloseButton := False;
end;

destructor TLogTab.Destroy;
begin

  inherited;
end;

procedure TLogTab.PopupMenuClick(Sender: TObject);
begin
  if (Sender = FLogTree.FPopupMenu.FItemDebug) or (Sender = FLogTree.FPopupMenu.FItemInfo)
    or (Sender = FLogTree.FPopupMenu.FItemWarning) or (Sender = FLogTree.FPopupMenu.FItemError)
  then
  begin
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  end;

  if Sender = FLogTree.FPopupMenu.FItemDebug then
  begin
    FLogPanel.FButtonDebug.Down := TMenuItem(Sender).Checked;
    ButtonClick(FLogPanel.FButtonDebug)
  end else if Sender = FLogTree.FPopupMenu.FItemInfo then
  begin
    FLogPanel.FButtonInfo.Down := TMenuItem(Sender).Checked;
    ButtonClick(FLogPanel.FButtonInfo)
  end else if Sender = FLogTree.FPopupMenu.FItemWarning then
  begin
    FLogPanel.FButtonWarning.Down := TMenuItem(Sender).Checked;
    ButtonClick(FLogPanel.FButtonWarning)
  end else if Sender = FLogTree.FPopupMenu.FItemError then
  begin
    FLogPanel.FButtonError.Down := TMenuItem(Sender).Checked;
    ButtonClick(FLogPanel.FButtonError)
  end else if Sender = FLogTree.FPopupMenu.FItemCopy then
  begin
    ButtonClick(FLogPanel.FButtonCopy)
  end else if Sender = FLogTree.FPopupMenu.FItemClear then
  begin
    ButtonClick(FLogPanel.FButtonClear);
  end;
end;

procedure TLogTab.PostTranslate;
begin
  FLogPanel.PostTranslate;
  FLogTree.PostTranslate;
end;

procedure TLogTab.TextChange(Sender: TObject);
begin
  FLogTree.SetFilter(FLogPanel.FSearch.Text, FLogTree.FFilterTypes);
end;

procedure TLogTab.UpdateButtons;
begin
  FLogPanel.FButtonCopy.Enabled := FLogTree.RootNodeCount > 0;
  FLogPanel.FButtonClear.Enabled := FLogTree.FLog.Count > 0;

  FLogTree.FPopupMenu.FItemCopy.Enabled := FLogPanel.FButtonCopy.Enabled;
  FLogTree.FPopupMenu.FItemClear.Enabled := FLogPanel.FButtonClear.Enabled;
end;

{ TLogTree }

procedure TLogTree.Add(Source: TLogSource; LogType: TLogType; LogLevel: TLogLevel; SourceText, Text: string; Time: TDateTime);
var
  Node: PVirtualNode;
  NodeData: PLogNodeData;
  LogEntry: TLogEntry;
begin
  while FLog.Count > 10000 do
  begin
    if MatchesFilter(FLog[0]) then
    begin
      Node := GetFirst;
      while Node <> nil do
      begin
        NodeData := GetNodeData(Node);
        if NodeData.LogEntry = FLog[0] then
        begin
          DeleteNode(Node);
          Break;
        end;
        Node := GetNext(Node);
      end;
    end;
    FLog[0].Free;
    FLog.Delete(0);
  end;

  LogEntry := TLogEntry.Create(Text, SourceText, Time, Source,  LogType, LogLevel);
  FLog.Add(LogEntry);

  Add(LogEntry);

  // Just to trigger "UpdateButtons" in LogTab..
  OnChange(nil, nil);
end;

procedure TLogTree.Add(LogEntry: TLogEntry);
var
  Node: PVirtualNode;
  NodeData: PLogNodeData;
begin
  if MatchesFilter(LogEntry) then
  begin
    Node := AddChild(nil);
    NodeData := GetNodeData(Node);
    NodeData.LogEntry := LogEntry;
  end;
end;

procedure TLogTree.AfterCreate;
begin
  FitColumns;
end;

constructor TLogTree.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FLog := TList<TLogEntry>.Create;

  MsgBus.AddSubscriber(MessageReceived);

  NodeDataSize := SizeOf(PLogNodeData);

  IncrementalSearch := isNone;

  Indent := 0;

  Header.Height := GetTextSize('Wyg', Font).cy + 6;
  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  Header.Options := [hoColumnResize, hoDrag, hoAutoResize, hoHotTrack, hoVisible, hoShowSortGlyphs];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleOnDblClick];
  ShowHint := True;
  HintMode := hmTooltip;

  Header.AutoSizeIndex := 3;

  FColType := Header.Columns.Add;
  FColType.Text := _('Type');
  FColType.Options := FColType.Options - [coDraggable, coResizable, coAllowClick];
  FColTime := Header.Columns.Add;
  FColTime.Text := _('Time');
  FColSource := Header.Columns.Add;
  FColSource.Text := _('Source');
  FColText := Header.Columns.Add;
  FColText.Text := _('Text');

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);
  TMTreeColumnPopup(Header.PopupMenu).OnAction := MenuColsAction;
  TMTreeColumnPopup(Header.PopupMenu).HideIdx := 3;

  FPopupMenu := TLogPopup.Create(Self);

  PopupMenu := FPopupMenu;

  Header.SortColumn := 1;
  Header.SortDirection := sdDescending;

  FFilterPattern := '*';
  if (AppGlobals.LogFilterTypes and (1 shl Integer(llDebug))) <> 0 then
    FFilterTypes := FFilterTypes + [llDebug];
  if (AppGlobals.LogFilterTypes and (1 shl Integer(llInfo))) <> 0 then
    FFilterTypes := FFilterTypes + [llInfo];
  if (AppGlobals.LogFilterTypes and (1 shl Integer(llWarning))) <> 0 then
    FFilterTypes := FFilterTypes + [llWarning];
  if (AppGlobals.LogFilterTypes and (1 shl Integer(llError))) <> 0 then
    FFilterTypes := FFilterTypes + [llError];

  for i := 1 to Header.Columns.Count - 1 do
  begin
    if not ((AppGlobals.LogCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];
  end;
end;

destructor TLogTree.Destroy;
var
  i: Integer;
begin
  MsgBus.RemoveSubscriber(MessageReceived);

  for i := 0 to FLog.Count - 1 do
    FLog[i].Free;
  FLog.Free;

  inherited;
end;

function TLogTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  // Wir müssen irgendeinen Index setzen, damit PaintImage() getriggert wird
  if (Column = 0) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    Index := 0;
end;

procedure TLogTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
var
  NodeData: PLogNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  Text := '';

  case Column of
    1: Text := TimeToStr(NodeData.LogEntry.Time);
    2: Text := NodeData.LogEntry.TextSource;
    3: Text := NodeData.LogEntry.Text;
  end;
end;

procedure TLogTree.DoHeaderDragged(Column: TColumnIndex;
  OldPosition: TColumnPosition);
begin
  inherited;

  if Header.Columns[Column].Position = 0 then
    Header.Columns[Column].Position := FHeaderDragSourcePosition;
end;

function TLogTree.DoHeaderDragging(Column: TColumnIndex): Boolean;
begin
  if Column = -1 then
    Exit(False);

  Result := inherited;

  FHeaderDragSourcePosition := Header.Columns[Column].Position
end;

procedure TLogTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;

  NodeHeight := GetTextSize('Wyg', Font).cy + 6;
end;

procedure TLogTree.FitColumns;
var
  i: Integer;
begin
  if (Header.Columns.Count <> Length(AppGlobals.LogHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.LogHeaderPosition)) then
    raise Exception.Create('(Header.Columns.Count <> Length(AppGlobals.LogHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.LogHeaderPosition))');

  if AppGlobals.LogHeaderWidthLoaded then
  begin
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Width := AppGlobals.LogHeaderWidth[i];
    FColType.Width := GetTextSize(FColType.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);
  end else
  begin
    FColType.Width := GetTextSize(FColType.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);
    FColType.Width := GetTextSize(FColType.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96);

    FColTime.Width := GetTextSize('00-00-00', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColSource.Width := GetTextSize('wwwwwwwwwwwwwww', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
  end;

  if AppGlobals.LogHeaderPositionLoaded then
  begin
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.LogHeaderPosition[i];
  end;
end;

function TLogTree.MatchesFilter(LogEntry: TLogEntry): Boolean;

begin
  Result := (LogEntry.Level in FFilterTypes)
    and ((FFilterPattern = '*')
      or (Like(LowerCase(LogEntry.Text), FFilterPattern))
      or (Like(LowerCase(LogEntry.TextSource), FFilterPattern))
      or (Like(LowerCase(TimeToStr(LogEntry.Time)), FFilterPattern)));
end;

procedure TLogTree.MenuColsAction(Sender: TVirtualStringTree;
  Index: Integer; Checked: Boolean);
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

  AppGlobals.LogCols := AppGlobals.LogCols xor (1 shl Index);
end;

procedure TLogTree.MessageReceived(Msg: TMessageBase);
var
  R: TRect;
  LogMsg: TLogMsg absolute Msg;
begin
  BeginUpdate;
  try
    if Msg is TLogMsg then
    begin
      Add(LogMsg.Source, LogMsg.LogType, LogMsg.LogLevel, LogMsg.SourceText, LogMsg.Text, LogMsg.Time);

      if (GetLast <> nil) and (GetPrevious(GetLast) <> nil) and (GetPrevious(GetPrevious(GetLast)) <> nil) then
      begin
        R := GetDisplayRect(GetPrevious(GetPrevious(GetLast)), NoColumn, False);
        if R.Top <= ClientHeight then
          PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);
      end;

      Invalidate;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLogTree.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  L: Integer;
  NodeData: PLogNodeData;
begin
  if PaintInfo.Column = 0 then
  begin
    NodeData := GetNodeData(PaintInfo.Node);

    L := PaintInfo.ImageInfo[ImageInfoIndex].XPos;

    case NodeData.LogEntry.Source of
      lsGeneral:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 3);
      lsAutomatic:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 77);
      lsStream:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 68);
      lsHome:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 99);
    end;

    L := L + 16;

    case NodeData.LogEntry.LogType of
      ltGeneral: ;
      ltSong:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 20);
      ltSaved:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 14);
      ltPostProcess:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 56);
      ltSchedule:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 50);
      ltSecure:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 103);
    end;

    L := L + 16;

    case NodeData.LogEntry.Level of
      llError:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 100);
      llWarning:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 97);
      llInfo:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 101);
      llDebug:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 98);
    end;
  end;
end;

procedure TLogTree.PostTranslate;
begin
  FColType.Text := _('Type');
  FColTime.Text := _('Time');
  FColSource.Text := _('Source');
  FColText.Text := _('Text');
end;

procedure TLogTree.Resize;
var
  R: TRect;
begin
  inherited;

  if (GetLast <> nil) and (GetPrevious(GetLast) <> nil) and (GetPrevious(GetPrevious(GetLast)) <> nil) then
  begin
    R := GetDisplayRect(GetPrevious(GetPrevious(GetLast)), NoColumn, False);
    if R.Top <= ClientHeight then
      PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;
end;

procedure TLogTree.SetFilter(Text: string; FilterTypes: TFilterTypes);
var
  i: Integer;
  Hash: Cardinal;
  Chars: Integer;
begin
  FFilterPattern := BuildPattern(Text, Hash, Chars, True);
  FFilterTypes := FilterTypes;

  BeginUpdate;
  try
    Clear;

    for i := 0 to FLog.Count - 1 do
      Add(FLog[i]);
  finally
    EndUpdate;
  end;

  if GetLast <> nil then
    ScrollIntoView(GetLast, False, False);

  AppGlobals.LogFilterTypes := 0;
  if llDebug in FilterTypes then
    AppGlobals.LogFilterTypes := AppGlobals.LogFilterTypes or (1 shl Integer(llDebug));
  if llInfo in FilterTypes then
    AppGlobals.LogFilterTypes := AppGlobals.LogFilterTypes or (1 shl Integer(llInfo));
  if llWarning in FilterTypes then
    AppGlobals.LogFilterTypes := AppGlobals.LogFilterTypes or (1 shl Integer(llWarning));
  if llError in FilterTypes then
    AppGlobals.LogFilterTypes := AppGlobals.LogFilterTypes or (1 shl Integer(llError));
end;

{ TLogPanel }

constructor TLogPanel.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := 'Search:';

  FSearch := TEdit.Create(Self);
  FSearch.Parent := Self;

  FToolbar := TToolBar.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.ShowHint := True;
end;

procedure TLogPanel.PostTranslate;
begin
  FSearch.Left := FLabel.Left + FLabel.Width + 6;
end;

procedure TLogPanel.Resize;
begin
  inherited;

end;

procedure TLogPanel.AfterCreate;
var
  Sep: TToolButton;
begin
  FToolbar.Images := modSharedData.imgImages;

  FButtonClear := TToolButton.Create(FToolbar);
  FButtonClear.Parent := FToolbar;
  FButtonClear.Hint := 'Clear';
  FButtonClear.ImageIndex := 13;

  FButtonCopy := TToolButton.Create(FToolbar);
  FButtonCopy.Parent := FToolbar;
  FButtonCopy.Hint := 'Copy';
  FButtonCopy.ImageIndex := 57;

  Sep := TToolButton.Create(FToolbar);
  Sep.Parent := FToolbar;
  Sep.Style := tbsSeparator;
  Sep.Width := 8;

  FButtonError := TToolButton.Create(FToolbar);
  FButtonError.Parent := FToolbar;
  FButtonError.Hint := 'Error';
  FButtonError.ImageIndex := 100;
  FButtonError.Style := tbsCheck;
  FButtonError.Down := (AppGlobals.LogFilterTypes and (1 shl Integer(llError))) <> 0;

  FButtonWarning := TToolButton.Create(FToolbar);
  FButtonWarning.Parent := FToolbar;
  FButtonWarning.Hint := 'Warning';
  FButtonWarning.ImageIndex := 97;
  FButtonWarning.Style := tbsCheck;
  FButtonWarning.Down := (AppGlobals.LogFilterTypes and (1 shl Integer(llWarning))) <> 0;

  FButtonInfo := TToolButton.Create(FToolbar);
  FButtonInfo.Parent := FToolbar;
  FButtonInfo.Hint := 'Info';
  FButtonInfo.ImageIndex := 101;
  FButtonInfo.Style := tbsCheck;
  FButtonInfo.Down := (AppGlobals.LogFilterTypes and (1 shl Integer(llInfo))) <> 0;

  FButtonDebug := TToolButton.Create(FToolbar);
  FButtonDebug.Parent := FToolbar;
  FButtonDebug.Hint := 'Debug';
  FButtonDebug.ImageIndex := 98;
  FButtonDebug.Style := tbsCheck;
  FButtonDebug.Down := (AppGlobals.LogFilterTypes and (1 shl Integer(llDebug))) <> 0;
  {$IFNDEF DEBUG}
  FButtonDebug.Visible := False;
  {$ENDIF}

  FToolbar.Align := alRight;
  FToolbar.AutoSize := True;

  PostTranslate;

  FLabel.Left := 0;
  FSearch.Width := 200;
  FSearch.Top := 1;

  FLabel.Top := (FSearch.Top + FSearch.Height div 2 - FLabel.Height div 2);

  ClientHeight := FSearch.Top * 2 + FSearch.Height + MulDiv(3, Screen.PixelsPerInch, 96);
end;

{ TLogEntry }

constructor TLogEntry.Create(Text, TextSource: string; Time: TDateTime; Source: TLogSource; LogType: TLogType; Level: TLogLevel);
begin
  inherited Create;

  Self.Text := Text;
  Self.TextSource := TextSource;
  Self.Source := Source;
  Self.LogType := LogType;
  Self.Level := Level;
  Self.Time := Time;
end;

{ TLogPopup }

constructor TLogPopup.Create(AOwner: TComponent);
var
  Sep: TMenuItem;
begin
  inherited;

  FItemDebug := CreateMenuItem;
  FItemDebug.Caption := '&Debug';
  FItemDebug.Checked := (AppGlobals.LogFilterTypes and (1 shl Integer(llDebug))) <> 0;
  Items.Add(FItemDebug);
  {$IFNDEF DEBUG}
  FItemDebug.Visible := False;
  {$ENDIF}

  FItemInfo := CreateMenuItem;
  FItemInfo.Caption := '&Info';
  FItemInfo.Checked := (AppGlobals.LogFilterTypes and (1 shl Integer(llInfo))) <> 0;
  Items.Add(FItemInfo);

  FItemWarning := CreateMenuItem;
  FItemWarning.Caption := '&Warning';
  FItemWarning.Checked := (AppGlobals.LogFilterTypes and (1 shl Integer(llWarning))) <> 0;
  Items.Add(FItemWarning);

  FItemError := CreateMenuItem;
  FItemError.Caption := '&Error';
  FItemError.Checked := (AppGlobals.LogFilterTypes and (1 shl Integer(llError))) <> 0;
  Items.Add(FItemError);

  Sep := CreateMenuItem;
  Sep.Caption := '-';
  Items.Add(Sep);

  FItemCopy := CreateMenuItem;
  FItemCopy.Caption := '&Copy';
  FItemCopy.ImageIndex := 57;
  Items.Add(FItemCopy);

  FItemClear := CreateMenuItem;
  FItemClear.Caption := 'C&lear';
  FItemClear.ImageIndex := 13;
  Items.Add(FItemClear);
end;

end.

