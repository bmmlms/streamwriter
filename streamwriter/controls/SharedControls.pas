{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2020 Alexander Nottelmann

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

{ This unit contains controls used in this applications, TSeekBar and TVolumePanel }
unit SharedControls;

interface

uses
  Windows, SysUtils, Classes, ComCtrls, ExtCtrls, Controls, Graphics,
  Functions, PngSpeedButton, PngImage, LanguageObjects, Menus,
  Themes, Messages, Math, Buttons, Logging, Forms, VirtualTrees;

type
  TGripperStates = (gsUnknown, gsNormal, gsHot, gsDown);

  TSeekBar = class(TCustomControl)
  private
    FMax: Int64;
    FPosition: Int64;
    FOrientation: TScrollBarKind;

    FPositionBeforeDrag: Int64;

    FGripperPos, FLastGripperPos: Integer;
    FDragFrom: Integer;
    FGripperVisible: Boolean;
    FGripperDown: Boolean;
    FNotifyOnMove: Boolean;
    FNotifyOnDown: Boolean;

    FLastGripperState: TGripperStates;

    FSetting: Boolean;
    FOnPositionChanged: TNotifyEvent;

    procedure PaintBackground(Bmp: TBitmap);
    procedure PaintGripper(Bmp: TBitmap);

    function GetGripperState: TGripperStates;

    procedure FSetPosition(Value: Int64);
    procedure FSetGripperVisible(Value: Boolean);

    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
      override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Max: Int64 read FMax write FMax;
    property Position: Int64 read FPosition write FSetPosition;
    property PositionBeforeDrag: Int64 read FPositionBeforeDrag;
    property Orientation: TScrollBarKind read FOrientation write FOrientation;
    property GripperVisible: Boolean read FGripperDown write FSetGripperVisible;
    property NotifyOnMove: Boolean read FNotifyOnMove write FNotifyOnMove;
    property NotifyOnDown: Boolean read FNotifyOnDown write FNotifyOnDown;
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
  end;

  TOnGetVolumeBeforeMute = function(Sender: TObject): Integer of object;

  TVolumePanel = class(TPanel)
  private
    FTrackBarPanel: TPanel;
    FTrackBar: TSeekBar;
    FMute: TPngSpeedButton;
    FVolume: Integer;
    FVolumeBeforeDrag: Integer;
    FVolumeChange: TNotifyEvent;
    FVolumePng: TPngImage;
    FVolumeMutedPng: TPngImage;

    FOnGetVolumeBeforeMute: TOnGetVolumeBeforeMute;

    procedure MuteClick(Sender: TObject);
    procedure VolumeChange(Sender: TObject);
    procedure RefreshButtonState(DoIt: Boolean);
    procedure FSetVolume(Volume: Integer);
    procedure FSetNotifyOnMove(Value: Boolean);
    function FGetVolume: Integer;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure Setup;

    property OnVolumeChange: TNotifyEvent read FVolumeChange write FVolumeChange;
    property Volume: Integer read FGetVolume write FSetVolume;
    property VolumeBeforeDrag: Integer read FVolumeBeforeDrag;
    property NotifyOnMove: Boolean write FSetNotifyOnMove;
    property OnGetVolumeBeforeMute: TOnGetVolumeBeforeMute read FOnGetVolumeBeforeMute write FOnGetVolumeBeforeMute;
  end;

  TMenuColEvent = procedure(Sender: TVirtualStringTree; Index: Integer; Checken: Boolean) of object;

  TMTreeColumnPopup = class(TPopupMenu)
  private
    FFileView: TVirtualStringTree;
    FOnAction: TMenuColEvent;
    FHideIdx: Integer;

    procedure ColItemsClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    property OnAction: TMenuColEvent read FOnAction write FOnAction;
    property HideIdx: Integer read FHideIdx write FHideIdx;
  end;

implementation

{ TVolumePanel }

procedure TVolumePanel.SetEnabled(Value: Boolean);
begin
  inherited;

  FMute.Enabled := Value;
  FTrackBar.GripperVisible := Value;
end;

procedure TVolumePanel.Setup;
begin
  BevelOuter := bvNone;

  FMute.Hint := 'Mute';
  FMute.ShowHint := True;
  FMute.Flat := True;
  FMute.Align := alLeft;
  FMute.ClientWidth := 22;
  FMute.GroupIndex := 1;
  FMute.AllowAllUp := True;
  FMute.Down := True;
  FMute.OnClick := MuteClick;
  FMute.Parent := Self;

  FTrackBarPanel.Align := alClient;
  FTrackBarPanel.BevelOuter := bvNone;
  FTrackBarPanel.Padding.Left := 4;
  FTrackBarPanel.Padding.Right := 2;
  FTrackBarPanel.Parent := Self;

  FTrackBar.Max := 100;
  FTrackBar.Align := alClient;
  FTrackBar.OnPositionChanged := VolumeChange;
  FTrackBar.Parent := FTrackBarPanel;
  FTrackBar.GripperVisible := True;
  FTrackBar.NotifyOnMove := True;
  FTrackBar.NotifyOnDown := True;

  RefreshButtonState(True);
end;

procedure TVolumePanel.MuteClick(Sender: TObject);
var
  P: Integer;
begin
  if FMute.Down then
  begin
    FTrackBar.FPositionBeforeDrag := FTrackBar.Position;
    FTrackBar.Position := 0;

    FMute.PngImage := FVolumeMutedPng;
    if not FMute.Down then
      FMute.Down := True;
  end else
  begin
    P := FOnGetVolumeBeforeMute(Self);
    FTrackBar.Position := P;
    FMute.PngImage := FVolumePng;
  end;
end;

procedure TVolumePanel.VolumeChange(Sender: TObject);
begin
  RefreshButtonState(False);

  FVolume := FTrackBar.Position;
  FVolumeBeforeDrag := FTrackBar.PositionBeforeDrag;

  if Assigned(OnVolumeChange) then
    OnVolumeChange(Self);
end;

procedure TVolumePanel.RefreshButtonState(DoIt: Boolean);
begin
  if Volume = 0 then
  begin
    if not FMute.Down or DoIt then
    begin
      FMute.Down := True;
      FMute.PngImage := FVolumeMutedPng;
    end;
  end else
  begin
    if FMute.Down or DoIt then
    begin
      FMute.Down := False;
      FMute.PngImage := FVolumePng;
    end;
  end;
end;

procedure TVolumePanel.FSetVolume(Volume: Integer);
begin
  FTrackBar.Position := Volume;
  RefreshButtonState(False);
end;

function TVolumePanel.FGetVolume: Integer;
begin
  Result := FTrackBar.Position;
end;

procedure TVolumePanel.FSetNotifyOnMove(Value: Boolean);
begin
  FTrackBar.NotifyOnMove := Value;
end;

constructor TVolumePanel.Create(AOwner: TComponent);
var
  ResStream: TResourceStream;
begin
  inherited;

  FMute := TPngSpeedButton.Create(Self);
  FTrackBar := TSeekBar.Create(Self);
  FTrackBarPanel := TPanel.Create(Self);

  ResStream := TResourceStream.Create(HInstance, 'VOLUME', RT_RCDATA);
  try
    FVolumePng := TPngImage.Create;
    FVolumePng.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;

  ResStream := TResourceStream.Create(HInstance, 'VOLUME_MUTED', RT_RCDATA);
  try
    FVolumeMutedPng := TPngImage.Create;
    FVolumeMutedPng.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;

destructor TVolumePanel.Destroy;
begin
  inherited;

  FVolumePng.Destroy;
  FVolumeMutedPng.Destroy;
end;

{ TSeekBar }

procedure TSeekBar.Paint;
var
  Bmp: TBitmap;
  R: TRect;
begin
  inherited;

  if not HandleAllocated then
    Exit;

  Bmp := TBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;

    R.Left := 0;
    R.Top := 0;
    R.Right := Bmp.Width;
    R.Bottom := Bmp.Height;

    if not ThemeServices.ThemesEnabled then
    begin
      Bmp.Canvas.Brush.Style := bsSolid;
      Bmp.Canvas.Brush.Color := clBtnFace;
      Bmp.Canvas.FillRect(R);
    end else
    begin
      ThemeServices.DrawParentBackground(Handle, BMP.Canvas.Handle, nil, False);
    end;

    PaintBackground(Bmp);
    PaintGripper(Bmp);

    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TSeekBar.PaintBackground(Bmp: TBitmap);
var
  R: TRect;
begin
  Bmp.Canvas.Brush.Color := clBlack;
  Bmp.Canvas.Pen.Color := clBlack;

  case FOrientation of
    sbHorizontal:
      begin
        // Rand links und oben
        Bmp.Canvas.MoveTo(0, Bmp.Height div 2 + 3); // Unten links
        Bmp.Canvas.LineTo(0, Bmp.Height div 2 - 3); // Nach oben malen
        Bmp.Canvas.LineTo(Bmp.Width - Bmp.Canvas.Pen.Width, Bmp.Height div 2 - 3); // Nach rechts malen
        // Rand rechts und unten
        Bmp.Canvas.Pen.Color := clGray;
        Bmp.Canvas.LineTo(Bmp.Width - Bmp.Canvas.Pen.Width, Bmp.Height div 2 + 3);
        Bmp.Canvas.LineTo(0, Bmp.Height div 2 + 3);

        R.Left := Canvas.Pen.Width;
        R.Top := Bmp.Height div 2 - 3 + Bmp.Canvas.Pen.Width;
        R.Bottom := Bmp.Height div 2 + 3;
        R.Right := Bmp.Width - Bmp.Canvas.Pen.Width;
      end;
    sbVertical:
      begin
        // Rand links und oben
        Bmp.Canvas.MoveTo(Bmp.Width div 2 - 3, Bmp.Height - Bmp.Canvas.Pen.Width);
        Bmp.Canvas.LineTo(Bmp.Width div 2 - 3, 0);
        Bmp.Canvas.LineTo(Bmp.Width div 2 + 3, 0);
        // Rand rechts und unten
        Bmp.Canvas.Pen.Color := clGray;
        Bmp.Canvas.LineTo(Bmp.Width div 2 + 3, Bmp.Height - Bmp.Canvas.Pen.Width);
        Bmp.Canvas.LineTo(Bmp.Width div 2 - 3, Bmp.Height - Bmp.Canvas.Pen.Width);

        R.Left := Bmp.Width div 2 - 3 + Canvas.Pen.Width;
        R.Top := Bmp.Canvas.Pen.Width;
        R.Bottom := Bmp.Height - Bmp.Canvas.Pen.Width;
        R.Right := Bmp.Width div 2 + 3 - Bmp.Canvas.Pen.Width;
      end;
  end;
  Bmp.Canvas.Brush.Color := clBtnFace;
  Bmp.Canvas.FillRect(R);
end;

procedure TSeekBar.PaintGripper(Bmp: TBitmap);
var
  P: Cardinal;
  R: TRect;
  D, D2: TThemedElementDetails;
begin
  if not FGripperVisible then
    Exit;

  if FMax > 0 then
  begin
    if FOrientation = sbHorizontal then
    begin
      P := Trunc((FPosition / FMax) * (ClientWidth - 20));

      R.Top := 2;
      R.Left := P;
      R.Bottom := Bmp.Height - 2;
      R.Right := 20 + R.Left;
    end else
    begin
      P := Trunc((FPosition / FMax) * (ClientHeight - 20));

      R.Top := P;
      R.Left := 2;
      R.Bottom := P + 20;;
      R.Right := Bmp.Width;
    end;

    if ThemeServices.ThemesEnabled then
    begin
      case GetGripperState of
        gsNormal:
          begin
            if FOrientation = sbHorizontal then
            begin
              D := ThemeServices.GetElementDetails(tsThumbBtnHorzNormal);
              D2 := ThemeServices.GetElementDetails(tsGripperHorzNormal);
            end else
            begin
              D := ThemeServices.GetElementDetails(tsThumbBtnVertNormal);
              D2 := ThemeServices.GetElementDetails(tsGripperVertNormal);
            end;
          end;
        gsHot:
          begin
            if FOrientation = sbHorizontal then
            begin
              D := ThemeServices.GetElementDetails(tsThumbBtnHorzHot);
              D2 := ThemeServices.GetElementDetails(tsGripperHorzHot);
            end else
            begin
              D := ThemeServices.GetElementDetails(tsThumbBtnVertHot);
              D2 := ThemeServices.GetElementDetails(tsGripperVertHot);
            end;
          end;
        gsDown:
          begin
            if FOrientation = sbHorizontal then
            begin
              D := ThemeServices.GetElementDetails(tsThumbBtnHorzPressed);
              D2 := ThemeServices.GetElementDetails(tsGripperHorzPressed);
            end else
            begin
              D := ThemeServices.GetElementDetails(tsThumbBtnVertPressed);
              D2 := ThemeServices.GetElementDetails(tsGripperVertPressed);
            end;
          end;
      end;

      ThemeServices.DrawElement(Bmp.Canvas.Handle, D, R);
      ThemeServices.DrawElement(Bmp.Canvas.Handle, D2, R);
    end else
    begin
      case GetGripperState of
        gsNormal:
          DrawButtonFace(Bmp.Canvas, R, 1, bsAutoDetect, True, False, False);
        gsHot:
          DrawButtonFace(Bmp.Canvas, R, 1, bsAutoDetect, True, False, True);
        gsDown:
          DrawButtonFace(Bmp.Canvas, R, 1, bsAutoDetect, True, True, True);
      end;
    end;

    FLastGripperState := GetGripperState;
    FLastGripperPos := FPosition;
  end;
end;

procedure TSeekBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TSeekBar.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  FPosition := FPosition + Trunc(Msg.WheelDelta / 30);

  if FPosition < 0 then
    FPosition := 0;
  if FPosition > FMax then
    FPosition := FMax;

  if FNotifyOnMove then
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Paint;
end;

procedure TSeekBar.WndProc(var Message: TMessage);
begin
  inherited;

  if Message.Msg = CM_MOUSELEAVE then
    Paint;
end;

function TSeekBar.GetGripperState: TGripperStates;
var
  P: Cardinal;
  R: TRect;
begin
  Result := gsUnknown;

  if not FGripperVisible then
    Exit;

  if FOrientation = sbHorizontal then
  begin
    P := Trunc((FPosition / FMax) * (ClientWidth - 20));

    R.Top := 2;
    R.Left := P;
    R.Bottom := ClientHeight;
    R.Right := 20 + R.Left;
  end else
  begin
    P := Trunc((FPosition / FMax) * (ClientHeight - 20));

    R.Top := P;
    R.Left := 2;
    R.Bottom := P + 20;;
    R.Right := ClientWidth;
  end;

  if not FGripperDown and PtInRect(R, ScreenToClient(Mouse.CursorPos)) then
  begin
    Result := gsHot;
  end else
  if FGripperDown then
  begin
    Result := gsDown;
  end else
  begin
    Result := gsNormal;
  end;
end;

constructor TSeekBar.Create(AOwner: TComponent);
begin
  inherited;

  FMax := 0;
  FPositionBeforeDrag := -1;
  FOrientation := sbHorizontal;
end;

procedure TSeekBar.FSetGripperVisible(Value: Boolean);
begin
  if Value <> FGripperVisible then
  begin
    FGripperVisible := Value;
    Paint;
  end;
end;

procedure TSeekBar.FSetPosition(Value: Int64);
begin
  if FSetting then
    Exit;

  FPosition := Value;
  if FMax = 0 then
    FGripperPos := 0
  else
    if FOrientation = sbHorizontal then
      FGripperPos := Trunc((FPosition / FMax) * (ClientWidth - 20))
    else
      FGripperPos := Trunc((FPosition / FMax) * (ClientHeight - 20));

  if FNotifyOnMove then
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Paint;
end;

procedure TSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  V: Integer;
begin
  inherited;

  if not FGripperVisible then
    Exit;

  if Button = mbLeft then
  begin
    FGripperDown := True;

    if FOrientation = sbHorizontal then
    begin
      V := X;
      FGripperPos := Trunc((FPosition / FMax) * (ClientWidth - 20))
    end else
    begin
      V := Y;
      FGripperPos := Trunc((FPosition / FMax) * (ClientHeight - 20));
    end;

    if (V > FGripperPos) and (V < FGripperPos + 20) then
    begin
      FDragFrom := Min(Abs(V - FGripperPos), Abs(FGripperPos - V));
    end else
    begin
      FDragFrom := 10;

      if FPositionBeforeDrag = -1 then
        FPositionBeforeDrag := FPosition;

      if FOrientation = sbHorizontal then
        FPosition := Trunc(((V - FDragFrom) / (ClientWidth - 20)) * Max)
      else
        FPosition := Trunc(((V - FDragFrom) / (ClientHeight - 20)) * Max);
      FGripperPos := V - FDragFrom;

      if FPosition < 0 then
        FPosition := 0;
      if FPosition > FMax then
        FPosition := FMax;

      if FNotifyOnDown then
        if Assigned(FOnPositionChanged) then
          FOnPositionChanged(Self);
    end;

    Paint;

    FSetting := True;
  end;
end;

procedure TSeekBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if ssLeft in Shift then
  begin
    if FPositionBeforeDrag = -1 then
      FPositionBeforeDrag := FPosition;

    if FOrientation = sbHorizontal then
    begin
      FPosition := Trunc(((X - FDragFrom) / (ClientWidth - 20)) * Max);
      FGripperPos := X - FDragFrom;
    end else
    begin
      FPosition := Trunc(((Y - FDragFrom) / (ClientHeight - 20)) * Max);
      FGripperPos := Y - FDragFrom;
    end;

    if FPosition < 0 then
      FPosition := 0;
    if FPosition > FMax then
      FPosition := FMax;

    if FNotifyOnMove then
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self);

    FSetting := True;

    if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
      Paint;
  end;

end;

procedure TSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  SetFocus;

  if Button = mbLeft then
  begin
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

    FPositionBeforeDrag := -1;

    FSetting := False;
    FGripperDown := False;

    Paint;
  end;
end;

{ TMTreeColumnPopup }

procedure TMTreeColumnPopup.ColItemsClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    FOnAction(nil, TVirtualTreeColumn(TMenuItem(Sender).Tag).Index, True);
end;

procedure TMTreeColumnPopup.DoPopup(Sender: TObject);
var
  i: Integer;
  Tree: TVirtualStringTree;
  Item: TMenuItem;
begin
  inherited;

  Items.Clear;

  Tree := TVirtualStringTree(Owner);
  FFileView := Tree;
  for i := 1 to Tree.Header.Columns.Count - 1 do
  begin
    if i = FHideIdx then
      Continue;
    Item := CreateMenuItem;
    Item.Caption := Tree.Header.Columns[i].Text;
    Item.OnClick := ColItemsClick;
    Item.Tag := Integer(Tree.Header.Columns[i]);
    Items.Add(Item);
  end;

  for i := 0 to Items.Count - 1 do
  begin
    Items[i].Checked := coVisible in TVirtualTreeColumn(Items[i].Tag).Options;
  end;
end;

end.

