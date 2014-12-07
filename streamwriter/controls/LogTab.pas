{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2014 Alexander Nottelmann

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
    function DoCompare(Node1: PVirtualNode; Node2: PVirtualNode;
      Column: TColumnIndex): Integer; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo;
      ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    function DoHeaderDragging(Column: TColumnIndex): Boolean; override;
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); override;

    procedure MessageReceived(Msg: TMessageBase);
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

    procedure TextChange(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
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

  // TODO: brauchen nen popupmenü!
  //if Screen.PixelsPerInch = 96 then
  //  FChartsTree.PopupMenu.Images := modSharedData.imgImages;

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
end;

procedure TLogTab.LogTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin

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

  ShowCloseButton := False;
end;

destructor TLogTab.Destroy;
begin

  inherited;
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

  MsgBus.AddSubscriber(MessageReceived);    // TODO: RemoveSubscriber? fehlt das noch an anderen stellen?? klar, ist nicht wichtig, aber SAUBER!

  NodeDataSize := SizeOf(PLogNodeData);

  IncrementalSearch := isVisibleOnly;

  Indent := 0;

  Header.Height := GetTextSize('Wyg', Font).cy + 5;
  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  Header.Options := [hoColumnResize, hoDrag, hoAutoResize, hoHotTrack, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleOnDblClick];

  Header.AutoSizeIndex := 3;

  FColType := Header.Columns.Add;
  FColType.Text := _('Type');
  FColType.Options := FColType.Options - [coDraggable];
  FColTime := Header.Columns.Add;
  FColTime.Text := _('Time');
  FColSource := Header.Columns.Add;
  FColSource.Text := _('Source');
  FColText := Header.Columns.Add;
  FColText.Text := _('Text');

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);
  TMTreeColumnPopup(Header.PopupMenu).OnAction := MenuColsAction;
  TMTreeColumnPopup(Header.PopupMenu).HideIdx := 3;

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
  for i := 0 to FLog.Count - 1 do
    FLog[i].Free;
  FLog.Free;

  inherited;
end;

function TLogTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  i: Integer;
  C1, C2: Integer;
//  Data1, Data2: PLogNodeData;
begin
  Result := 0;

//  Data1 := GetNodeData(Node1);
//  Data2 := GetNodeData(Node2);
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

procedure TLogTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;

end;

procedure TLogTree.DoHeaderDragged(Column: TColumnIndex;
  OldPosition: TColumnPosition);
begin
  inherited;

end;

function TLogTree.DoHeaderDragging(Column: TColumnIndex): Boolean;
begin
  Result := inherited;

  FHeaderDragSourcePosition := Header.Columns[Column].Position;
end;

function TLogTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  NodeData: PLogNodeData;
begin
  NodeData := GetNodeData(Node);

  Result := 0;
end;

procedure TLogTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;

  NodeHeight := GetTextSize('Wyg', Font).cy + 5;
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
    FColType.Width := 52;
  end else
  begin
    FColType.Width := 52;

    FColTime.Width := GetTextSize('00-00-00', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColSource.Width := GetTextSize('wwwwwwwwwwwwwww', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
  end;

  if AppGlobals.LogHeaderPositionLoaded then
  begin
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.LogHeaderPosition[i];
  end;

                  // TODO: alles auf hohen DPI testen---
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

      if (GetLast <> nil) and (GetPrevious(GetLast) <> nil) then
      begin
        R := GetDisplayRect(GetPrevious(GetLast), NoColumn, False);
        if not (R.Bottom > ClientHeight) then
          ScrollIntoView(GetLast, False, False);
      end;

      Invalidate;
    end;
  finally
    EndUpdate;
  end;                            // TODO: doppelklick bei titelsuche fügt zur wunschliste hinzu, doppelklick entfernt. warum gibts für entfernen kein popupmenu item???
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
    end;

    L := L + 16;

    case NodeData.LogEntry.Level of
      llError:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 100);
      llWarning:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 97);
      llInfo:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 10);
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

procedure TLogTree.SetFilter(Text: string; FilterTypes: TFilterTypes);
var
  i: Integer;
  Hash: Cardinal;
  Chars: Integer;
begin
  FFilterPattern := BuildPattern(Text, Hash, Chars, True);
  FFilterTypes := FilterTypes;

  BeginUpdate;
  try                          // TODO: tooltips anzeigen wenn text in spalte nicht ganz sichtbar ist. in der titelsuche funzt das auch nicht.
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
  FButtonError.ImageIndex := 2;
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
  FButtonInfo.ImageIndex := 10;
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

end.

