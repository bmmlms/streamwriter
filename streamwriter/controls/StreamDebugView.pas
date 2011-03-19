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
unit StreamDebugView;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ImgList,
  DataManager, VirtualTrees, LanguageObjects, GUIFunctions,
  Generics.Collections, Graphics, Forms, ICEClient, Clipbrd, AppData;

type
  TDebugView = class(TVirtualStringTree)
  private
    FClient: TICEClient;
    procedure FSetClient(Value: TICEClient);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure Resize; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
    procedure DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Copy;
    property Client: TICEClient read FClient write FSetClient;
  end;

  TMStreamDebugPanel = class(TPanel)
  private
    FClient: TICEClient;
    FChkAutoScroll: TCheckBox;
    FDebug: TDebugView;
    FPanelBottom: TPanel;
    FBtnCopy: TButton;
    FBtnClear: TButton;

    FOnClear: TNotifyEvent;

    procedure ShowDebug(Client: TICEClient);

    procedure BtnCopyClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure ChkAutoScrollClick(Sender: TObject);
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

procedure TMStreamDebugPanel.ChkAutoScrollClick(Sender: TObject);
begin
  AppGlobals.AutoScrollLog := FChkAutoScroll.Checked;
end;

constructor TMStreamDebugPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Align := alClient;
  BevelOuter := bvNone;

  FChkAutoScroll := TCheckBox.Create(Self);
  FChkAutoScroll.Caption := _('&Autoscroll');
  FChkAutoScroll.Parent := Self;
  FChkAutoScroll.Align := alTop;
  FChkAutoScroll.Visible := True;
  FChkAutoScroll.Checked := AppGlobals.AutoScrollLog;
  FChkAutoScroll.OnClick := ChkAutoScrollClick;

  FDebug := TDebugView.Create(Self);
  FDebug.Parent := Self;
  FDebug.Align := alClient;
  FDebug.Visible := True;

  FPanelBottom := TPanel.Create(Self);
  FPanelBottom.Parent := Self;
  FPanelBottom.Align := alBottom;
  FPanelBottom.BevelOuter := bvNone;
  FPanelBottom.Visible := True;
  FPanelBottom.Height := 30;
  FPanelBottom.Padding.Top := 4;

  FBtnCopy := TButton.Create(Self);
  FBtnCopy.Caption := _('&Copy');
  FBtnCopy.Align := alRight;
  FBtnCopy.Parent := FPanelBottom;
  FBtnCopy.Visible := True;
  FBtnCopy.OnClick := BtnCopyClick;

  FBtnClear := TButton.Create(Self);
  FBtnClear.Caption := _('C&lear');
  FBtnClear.Align := alRight;
  FBtnClear.Parent := FPanelBottom;
  FBtnClear.Visible := True;
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
  FDebugView.Visible := (Client <> nil) and (not Client.Killed);
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
      if GetNodeLevel(Node) = 1 then
        s := s + '    ' + StringReplace(FClient.DebugLog[Node.Parent.Index].Data, #13#10, #13#10'    ', [rfReplaceAll])
      else
        s := s + TimeToStr(FClient.DebugLog[Node.Index].Time) + ' - ' + FClient.DebugLog[Node.Index].Text;
      s := s + #13#10;
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
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];
  ScrollBarOptions.ScrollBars := ssVertical;

  Indent := 0;
  ShowHint := True;
  HintMode := hmTooltip;

  Header.Columns.Add;
  Header.Columns.Add;
  Header.Columns[0].Margin := 0;
  Header.Columns[1].Margin := 0;
  Header.AutoSizeIndex := 1;
  TextMargin := 0;
end;

procedure TDebugView.DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
begin
  inherited;
  NodeHeight := ComputeNodeHeight(Canvas, Node, 0);
end;

procedure TDebugView.FSetClient(Value: TICEClient);
begin
  FClient := Value;
  if FClient <> nil then
  begin
    RootNodeCount := FClient.DebugLog.Count;
  end else
  begin
    RootNodeCount := 0;
  end;
  if AppGlobals.AutoScrollLog and (GetLast <> nil) then
    ScrollIntoView(GetLast, False, False);
end;

function TDebugView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;
  Index := -1;
  if Column = 1 then
    case FClient.DebugLog[Node.Index].T of
      dtSocket:;
      dtMessage:;
      dtSong:
        Index := 0;
      dtError:
        Index := 1;
    end;
end;

procedure TDebugView.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
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

procedure TDebugView.DoInitChildren(Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  inherited;
  if FClient <> nil then
  begin
    if FClient.DebugLog[Node.Index].Data <> '' then
      ChildCount := 1
    else
      ChildCount := 0;
  end;
end;

procedure TDebugView.DoFreeNode(Node: PVirtualNode);
begin
  inherited;
end;

procedure TDebugView.Resize;
begin
  inherited;
  Header.Columns[0].Width := Canvas.TextWidth(TimeToStr(Now)) + 15;
  Header.Columns[1].Width := ClientWidth - Header.Columns[0].Width;
end;

end.
