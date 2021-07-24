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

{ This unit contains everything needed to display the stream-log }
unit StreamDebugView;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ImgList,
  DataManager, VirtualTrees, LanguageObjects, GUIFunctions, Messages,
  Generics.Collections, Graphics, Forms, ICEClient, Clipbrd, AppData,
  Logging, Math, TypeDefs, SharedData, Images;

type
  TDebugView = class(TVirtualStringTree)
  private
    FClient: TICEClient;
    procedure FSetClient(Value: TICEClient);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: String); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure Resize; override;
//    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer); override;
    function DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean; override;
    function DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean; override;
    procedure DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Copy;
    property Client: TICEClient read FClient write FSetClient;
  end;

  TMStreamDebugPanel = class(TPanel)
  private
    FClient: TICEClient;
    FDebug: TDebugView;
    FPanelBottom: TPanel;
    FBtnCopy: TButton;
    FBtnClear: TButton;

    FOnClear: TNotifyEvent;

    procedure ShowDebug(Client: TICEClient);

    procedure BtnCopyClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property Client: TICEClient read FClient;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property DebugView: TDebugView read FDebug;
  end;

  TMStreamDebugView = class(TPanel)
  private
    FDebugView: TMStreamDebugPanel;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    procedure ShowDebug(Client: TICEClient);
    property DebugView: TMStreamDebugPanel read FDebugView;
  end;

implementation

{ TStreamDebugView }

procedure TMStreamDebugPanel.BtnClearClick(Sender: TObject);
begin
  FDebug.Clear;
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

procedure TMStreamDebugPanel.BtnCopyClick(Sender: TObject);
begin
  FDebug.Copy;
end;

constructor TMStreamDebugPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Align := alClient;
  BevelOuter := bvNone;

  FDebug := TDebugView.Create(Self);
  FDebug.Parent := Self;
  FDebug.Align := alClient;

  FPanelBottom := TPanel.Create(Self);
  FPanelBottom.Parent := Self;
  FPanelBottom.Align := alBottom;
  FPanelBottom.BevelOuter := bvNone;
  FPanelBottom.AutoSize := True;

  FBtnCopy := TButton.Create(Self);
  FBtnCopy.Caption := '&Copy';
  FBtnCopy.Align := alRight;
  FBtnCopy.Parent := FPanelBottom;
  FBtnCopy.OnClick := BtnCopyClick;

  FBtnClear := TButton.Create(Self);
  FBtnClear.Caption := 'C&lear';
  FBtnClear.Align := alRight;
  FBtnClear.Parent := FPanelBottom;
  FBtnClear.OnClick := BtnClearClick;
end;

destructor TMStreamDebugPanel.Destroy;
begin

  inherited;
end;

procedure TMStreamDebugPanel.ShowDebug(Client: TICEClient);
begin
  FClient := Client;
  FDebug.Client := Client;
end;

{ TMStreamDebugContainer }

constructor TMStreamDebugView.Create(AOwner: TComponent);
begin
  inherited;

  Caption := 'Please select a stream.';
  BevelOuter := bvNone;
  Align := alClient;

  FDebugView := TMStreamDebugPanel.Create(Self);
  FDebugView.Parent := Self;
  FDebugView.Visible := False;
end;

procedure TMStreamDebugView.ShowDebug(Client: TICEClient);
begin
  FDebugView.ShowDebug(Client);
  FDebugView.Visible := (Client <> nil);
end;

{ TDebugView }

procedure TDebugView.Copy;
var
  s: string;
  Node: PVirtualNode;
begin
  if RootNodeCount > 0 then
  begin
    s := '';

    Node := GetFirst;
    while Node <> nil do
    begin
      if Selected[Node] or (SelectedCount = 0) then
      begin
        if GetNodeLevel(Node) = 1 then
          s := s + '    ' + StringReplace(FClient.DebugLog[Node.Parent.Index].Data, #13#10, #13#10'    ', [rfReplaceAll])
        else
          s := s + TimeToStr(FClient.DebugLog[Node.Index].Time) + ' - ' + FClient.DebugLog[Node.Index].Text;
        s := s + #13#10;
      end;
      Node := GetNext(Node);
    end;

    Clipboard.Clear;
    Clipboard.SetTextBuf(PChar(s));
  end;
end;

constructor TDebugView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  NodeDataSize := SizeOf(Integer);
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toVariableNodeHeight];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines] + [toHideFocusRect];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect, toMultiSelect];
  ScrollBarOptions.ScrollBars := ssVertical;

  Images := modSharedData.imgImages;

  Indent := 0;
  ShowHint := True;
  HintMode := hmTooltip;

  Header.Options := [hoAutoResize];

  Header.Columns.Add;
  Header.Columns[0].MinWidth := GetTextSize('00-00-00', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
  Header.Columns[0].MaxWidth := Header.Columns[0].MinWidth;
  Header.Columns[0].Options := Header.Columns[0].Options - [coResizable];

  Header.Columns.Add;

  Header.AutoSizeIndex := 1;
end;

procedure TDebugView.FSetClient(Value: TICEClient);
var
  R: TRect;
begin
  FClient := Value;
  if FClient <> nil then
  begin
    RootNodeCount := FClient.DebugLog.Count;
  end else
  begin
    RootNodeCount := 0;
  end;

  if (GetLast <> nil) and (GetPrevious(GetLast) <> nil) and (GetPrevious(GetPrevious(GetLast)) <> nil) then
  begin
    R := GetDisplayRect(GetPrevious(GetPrevious(GetLast)), NoColumn, False);
    if R.Top <= ClientHeight then
      PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;

  // Invalidate, weil FClient.DebugLog.Add() eventuell auch alte Einträge entfernt im Notify().
  // Ohne Invalidate bekommt die Ansicht das nicht mit.
  Invalidate;
end;

function TDebugView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin
  Result := Images;

  Index := -1;
  if Column = 1 then
  begin
    Index := TImages.BOOK_OPEN;

    case FClient.DebugLog[Node.Index].Level of
      llError: Index := TImages.EXCLAMATION;
      llWarning: Index := TImages.ERROR;
      llDebug: Index := TImages.BUG;
    end;

    if Index = 3 then
    begin
      case FClient.DebugLog[Node.Index].T of
        ltSong:
          Index := TImages.MUSIC;
        ltSaved:
          Index := TImages.DRIVE;
        ltPostProcess:
          Index := TImages.LIGHTNING;
        ltSchedule:
          Index := TImages.TIME;
        ltSecure:
          Index := TImages.LOCK;
      end;
    end;
  end;
end;

procedure TDebugView.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var Text: String);
begin
  inherited;

  if FClient <> nil then
  begin
    case Column of
      0:
        begin
          Text := TimeToStr(FClient.DebugLog[Node.Index].Time);
        end;
      1:
        begin
          case GetNodeLevel(Node) of
            0:
              begin
                Text := FClient.DebugLog[Node.Index].Text;
                if (FClient.DebugLog[Node.Index].Data <> '') and not HasChildren[Node] then
                  HasChildren[Node] := True;
              end;
            1:
              begin
                MultiLine[Node] := True;
                Text := FClient.DebugLog[Node.Parent.Index].Data;
              end;
          end;
        end;
    end;
  end;
end;

function TDebugView.DoInitChildren(Node: PVirtualNode;
  var ChildCount: Cardinal): Boolean;
begin
  inherited;

  if FClient <> nil then
  begin
    if FClient.DebugLog[Node.Index].Data <> '' then
      ChildCount := 1
    else
      ChildCount := 0;
  end;
  Result := True;
end;

function TDebugView.DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean;
begin
  Result := inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Result := True;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(R);
end;

procedure TDebugView.DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(ItemRect);
end;

procedure TDebugView.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
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

procedure TDebugView.DoFreeNode(Node: PVirtualNode);
begin
  inherited;
end;

procedure TDebugView.Resize;
var
  R: TRect;
begin
  inherited;

  {
  if (GetLast <> nil) and (GetPrevious(GetLast) <> nil) and (GetPrevious(GetPrevious(GetLast)) <> nil) then
  begin
    R := GetDisplayRect(GetPrevious(GetPrevious(GetLast)), NoColumn, False);
    if R.Top <= ClientHeight then
      PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;
  }
end;

end.
