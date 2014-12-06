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

{ This unit is for showing radio-charts in the main-window }
unit LogTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, LanguageObjects, Tabs, Functions, AppData, Logging, VirtualTrees,
  HomeCommunication, DataManager, ImgList, Graphics, Math, Generics.Collections,
  Menus, ChartsTabAdjustTitleName, Forms, TypeDefs, MessageBus, AppMessages,
  HomeCommands, Commands, GUIFunctions, SharedData, PerlRegEx, Messages,
  DateUtils, SharedControls;

type
  TLogNodeData = record
    Source: TLogSource;
    LogType: TDebugTypes;
    LogLevel: TLogLevel;
    Date: TDateTime;
    SourceText: string;
    Text: string;
  end;
  PLogNodeData = ^TLogNodeData;

  TLogPanel = class(TPanel)
  private
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

    FHeaderDragSourcePosition: Cardinal;

    procedure FitColumns;
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
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect); override;
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

    procedure Add(Source: TLogSource; LogType: TDebugTypes; LogLevel: TLogLevel; SourceText, Text: string);
  end;

  TLogTab = class(TMainTabSheet)
  private
    FLogPanel: TLogPanel;
    FLogTree: TLogTree;

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

  Caption := 'Log';
end;
     // TODO: als bling bling überall einbauen, dass die aktuell sortierte spalte eingefärbt wird? oder nur die erste oder so?? das sieht geil aus. wie in mp3freund mit dem grau bei "Dateiname"
procedure TLogTab.LogTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin

end;

constructor TLogTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ImageIndex := 3;

  {
  FLogPanel := TLogPanel.Create(Self);
  FLogPanel.Parent := Self;
  FLogPanel.Align := alTop;
  FLogPanel.Padding.Top := 1;
  }

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

{ TLogTree }

procedure TLogTree.Add(Source: TLogSource; LogType: TDebugTypes; LogLevel: TLogLevel; SourceText, Text: string);
var
  Node: PVirtualNode;
  NodeData: PLogNodeData;
begin
  Node := AddChild(nil);

  NodeData := GetNodeData(Node);
                                                // TODO: log muss automatisch nach unten scrollen, wenn es schon nach unten gescrollt ist. sonst nicht scrollen.
  NodeData.Source := Source;
  NodeData.LogType := LogType;
  NodeData.LogLevel := LogLevel;
  NodeData.Date := Now;
  NodeData.SourceText := SourceText;
  NodeData.Text := Text;
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

  MsgBus.AddSubscriber(MessageReceived);    // TODO: RemoveSubscriber? fehlt das noch an anderen stellen?? klar, ist nicht wichtig, aber SAUBER!

  NodeDataSize := SizeOf(TLogNodeData);

  IncrementalSearch := isVisibleOnly;

  Indent := 0;

  // TODO: passt das???????? WOHL KAUM!
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
  FColTime.Text := _('Time'); // TODO: okay der text?? zeit oder datum?
  FColSource := Header.Columns.Add;
  FColSource.Text := _('Source');
  FColText := Header.Columns.Add; // TODO: spalteneinstellungen speichern etcpp...
  FColText.Text := _('Text');

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);

  Header.SortColumn := 1;
  Header.SortDirection := sdDescending;

  for i := 1 to Header.Columns.Count - 1 do
  begin
    if not ((AppGlobals.ChartCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];
  end;
end;

destructor TLogTree.Destroy;
begin

  inherited;
end;

procedure TLogTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  C: Extended;
  Chance: Integer;
  R: TRect;
  DrawWidth, MaxWidth, TextWidth: Integer;
  NodeData: PLogNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);
end;

function TLogTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  i: Integer;
  C1, C2: Integer;
  Data1, Data2: PLogNodeData;
begin
  Result := 0;

  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);
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
    1: Text := TimeToStr(NodeData.Date);
    2: Text := NodeData.SourceText;
    3: Text := NodeData.Text;
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

// TODO: braucht man das logfile noch? hat es mir je weiter geholfen? lohnt sich der doppelte aufwand, wegen in tree und file loggen?

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
  {
  if (Header.Columns.Count <> Length(AppGlobals.SavedHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.SavedHeaderPosition)) then
    raise Exception.Create('(Header.Columns.Count <> Length(AppGlobals.SavedHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.SavedHeaderPosition))');

  if AppGlobals.SavedHeaderWidthLoaded then
  begin
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Width := AppGlobals.SavedHeaderWidth[i];
    FColImages.Width := 104;
  end else
  begin
    FColImages.Width := 104;
    FColSize.Width := GetTextSize('111,11 KB', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColLength.Width := GetTextSize('00:00', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColBitRate.Width := GetTextSize('320 VBR', font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColStream.Width := MulDiv(200, Screen.PixelsPerInch, 96);
    FColSaved.Width := MulDiv(130, Screen.PixelsPerInch, 96);
  end;

  if AppGlobals.SavedHeaderPositionLoaded then
  begin
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.SavedHeaderPosition[i];
  end;
  }
               // TODO: oben mus nen filter hin - nur clients anzeigen, nur auto-clients (und messages warum abgelehnt!) anzeigen
  // TODO: Das HeaderColumn-Popupmenü funzt noch nicht richtig.
                  // TODO: alles auf hohen DPI testen---
  FColType.Width := 52;
  FColTime.Width := GetTextSize('00-00-00', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
  FColSource.Width := 100 + MulDiv(20, Screen.PixelsPerInch, 96);
end;

procedure TLogTree.MessageReceived(Msg: TMessageBase);
var
  LogMsg: TLogMsg absolute Msg;
begin
  BeginUpdate;
  try
    if Msg is TLogMsg then
    begin
      Add(LogMsg.Source, LogMsg.LogType, LogMsg.LogLevel, LogMsg.SourceText, LogMsg.Text);

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
  begin                          // TODO: icons zwischen LogTab und DebugView (das "Protokoll" zu den streams) vereinheitlichen und in eine imagelist packen!
    NodeData := GetNodeData(PaintInfo.Node);

    L := PaintInfo.ImageInfo[ImageInfoIndex].XPos;

    case NodeData.Source of
      lsGeneral:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 3);
      lsAutomatic:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 77);
      lsStream:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 68);
    end;
                   // TODO: nach 2000 einträgen oder so abschneiden... oder noch mehr. soll das mit in die datei, mit nem "Lösch" knopf in der ansicht, um zu leeren?
    L := L + 16;

    // TODO: brauche über protokoll-ansicht auf jedenfall nen suchfeld.
    // TODO: ALLES ins protokoll tun, was sonst in die datei wandern würde!
    // TODO: LogImageList kicken!
    // TODO: das bezieht sich nur auf die LogImageList...
    case NodeData.LogType of
      dtSocket:;
      dtMessage:;  // TODO: das muss, oder??? // TODO: ist über, durch das level unten!
      dtSong:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 16);
      dtError: ; // TODO: ist über, durch das level unten!
      dtSaved:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 14);
      dtPostProcess:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 56);
      dtSchedule:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 50);
    end;
                 // TODO: Bei Einträgen wie "asdf - ASDF" wird gespielt sollte ich dahinter noch den "ge-regexten-titel" zeigen. also einmal wie der stream es meldet, dann, wie es interpretiert wurde. wir haben viel platz.
    L := L + 16;

    case NodeData.LogLevel of
      llDebug: ;
      llError:
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 65);
      llInfo: ;
//        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 10);
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

{ TLogPanel }

constructor TLogPanel.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;
end;

procedure TLogPanel.PostTranslate;
begin

end;

procedure TLogPanel.Resize;
begin
  inherited;

end;

procedure TLogPanel.AfterCreate;
var
  Sep: TToolButton;
begin
  PostTranslate;

//  ClientHeight := FSearch.Top * 2 + FSearch.Height + MulDiv(3, Screen.PixelsPerInch, 96);
end;

end.

