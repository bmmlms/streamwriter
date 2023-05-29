{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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
  AppData,
  Classes,
  Clipbrd,
  Controls,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  ICEClient,
  Images,
  ImgList,
  LanguageObjects,
  Logging,
  MStringFunctions,
  SharedControls,
  SharedData,
  StdCtrls,
  SysUtils,
  TypeDefs,
  VirtualTrees,
  Windows;

type

  { TDebugView }

  TDebugView = class(TMSWVirtualTree)
  private
    FClient: TICEClient;
    procedure FSetClient(Value: TICEClient);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    function DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Copy;

    property Client: TICEClient read FClient write FSetClient;
  end;

  { TMStreamDebugPanel }

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

  { TMStreamDebugView }

  TMStreamDebugView = class(TPanel)
  private
    FDebugView: TMStreamDebugPanel;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

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
  FDebug.Align := alClient;
  FDebug.Parent := Self;

  FPanelBottom := TPanel.Create(Self);
  FPanelBottom.Align := alBottom;
  FPanelBottom.BorderSpacing.Top := 4;
  FPanelBottom.BevelOuter := bvNone;
  FPanelBottom.AutoSize := True;
  FPanelBottom.Parent := Self;

  FBtnCopy := TButton.Create(Self);
  FBtnCopy.Caption := '&Copy';
  FBtnCopy.AutoSize := True;
  FBtnCopy.Align := alRight;
  FBtnCopy.OnClick := BtnCopyClick;
  FBtnCopy.Parent := FPanelBottom;

  FBtnClear := TButton.Create(Self);
  FBtnClear.Caption := 'C&lear';
  FBtnClear.AutoSize := True;
  FBtnClear.Align := alRight;
  FBtnClear.OnClick := BtnClearClick;
  FBtnClear.Parent := FPanelBottom;
end;

destructor TMStreamDebugPanel.Destroy;
begin
  inherited Destroy;

  FDebug := nil;
  FPanelBottom := nil;
  FBtnCopy := nil;
  FBtnClear := nil;
end;

procedure TMStreamDebugPanel.ShowDebug(Client: TICEClient);
begin
  FClient := Client;

  if Assigned(FDebug) then
    FDebug.Client := Client;
end;

{ TMStreamDebugContainer }

constructor TMStreamDebugView.Create(AOwner: TComponent);
begin
  inherited;

  Caption := 'Please select a stream.';
  BevelOuter := bvNone;
  WordWrap := True;
  Align := alClient;

  FDebugView := TMStreamDebugPanel.Create(Self);
  FDebugView.Parent := Self;
  FDebugView.Visible := False;
end;

destructor TMStreamDebugView.Destroy;
begin
  inherited Destroy;

  FDebugView := nil;
end;

procedure TMStreamDebugView.ShowDebug(Client: TICEClient);
begin
  if not Assigned(FDebugView) then
    Exit;

  FDebugView.ShowDebug(Client);
  FDebugView.Visible := (Client <> nil);
end;

{ TDebugView }

procedure TDebugView.Copy;
var
  s: string;
  Node: PVirtualNode;
begin
  if RootNodeCount = 0 then
    Exit;

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

  Clipboard.AsText := s;
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
  Header.Options := [hoAutoResize];

  Header.Columns.Add;
  Header.Columns[0].MinWidth := TMStringFunctions.GetTextSize('00-00-00', Font).cx + Scale96ToFont(16 + Margin * 2 + TextMargin);
  Header.Columns[0].MaxWidth := Header.Columns[0].MinWidth;
  Header.Columns[0].Options := Header.Columns[0].Options - [coResizable];
  Header.Columns.Add;

  Header.AutoSizeIndex := 1;
end;

procedure TDebugView.FSetClient(Value: TICEClient);
var
  ClientChanged: Boolean;
  R: TRect;
begin
  ClientChanged := FClient <> Value;

  FClient := Value;
  if FClient <> nil then
    RootNodeCount := FClient.DebugLog.Count
  else
    RootNodeCount := 0;

  if ClientChanged then
  begin
    if GetLast <> nil then
      ScrollIntoView(GetLast, True, False);
  end else if (GetLast <> nil) and (GetPrevious(GetLast) <> nil) and (GetPrevious(GetPrevious(GetLast)) <> nil) then
  begin
    R := GetDisplayRect(GetPrevious(GetPrevious(GetLast)), NoColumn, False);
    if R.Top <= ClientHeight then
      ScrollIntoView(GetLast, True, False);
  end;

  // Invalidate, weil FClient.DebugLog.Add() eventuell auch alte Einträge entfernt im Notify().
  // Ohne Invalidate bekommt die Ansicht das nicht mit.
  Invalidate;
end;

function TDebugView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin
  Result := Images;

  Index := -1;
  if Column = 0 then
  begin
    case FClient.DebugLog[Node.Index].Level of
      llError: Index := TImages.ERROR;
      llWarning: Index := TImages.EXCLAMATION;
      llDebug: Index := TImages.BUG;
    end;

    if Index = -1 then
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
        else
          Index := TImages.INFORMATION;
      end;
  end;
end;

procedure TDebugView.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String);
begin
  inherited;

  if FClient <> nil then
    case Column of
      0:
        Text := TimeToStr(FClient.DebugLog[Node.Index].Time);
      1:
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

function TDebugView.DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean;
begin
  inherited;

  if FClient <> nil then
    if FClient.DebugLog[Node.Index].Data <> '' then
      ChildCount := 1
    else
      ChildCount := 0;
  Result := True;
end;

procedure TDebugView.Resize;
begin
  inherited;

  if HandleAllocated and (GetLast <> nil) and IsVisible[GetLast] then
    ScrollIntoView(GetLast, True, False);
end;

end.
