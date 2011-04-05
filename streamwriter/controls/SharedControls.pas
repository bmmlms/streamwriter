{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit SharedControls;

interface

uses
  Windows, SysUtils, Classes, ComCtrls, ExtCtrls, Controls, Graphics,
  Functions, AppData, PngSpeedButton, PngImage, LanguageObjects,
  Themes, Messages, Math, Buttons;

type
  TGripperStates = (gsUnknown, gsNormal, gsHot, gsDown);

  TSeekChangeEvent = procedure(Sender: TObject; Position: Integer) of object;

  TSeekBar = class(TCustomControl)
  private
    FMax: Int64;
    FPosition: Int64;
    FGripperPos, FLastGripperPos: Cardinal;
    FDragFrom: Int64;
    FGripperVisible: Boolean;
    FGripperDown: Boolean;
    FNotifyOnMove: Boolean;
    FNotifyOnDown: Boolean;

    FLastGripperState: TGripperStates;

    FSetting: Boolean;
    FLastChanged: Cardinal;
    FOnPositionChanged: TNotifyEvent;

    procedure PaintBackground(Bmp: TBitmap);
    procedure PaintGripper(Bmp: TBitmap);

    function GetGripperState: TGripperStates;
    function GetGripperPos(X: Integer): Cardinal;

    procedure FSetPosition(Value: Int64);
    procedure FSetGripperVisible(Value: Boolean);

    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
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
    property GripperVisible: Boolean read FGripperDown write FSetGripperVisible;
    property NotifyOnMove: Boolean read FNotifyOnMove write FNotifyOnMove;
    property NotifyOnDown: Boolean read FNotifyOnDown write FNotifyOnDown;
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
  end;

  TVolumePanel = class(TPanel)
  private
    FTrackBarPanel: TPanel;
    FTrackBar: TSeekBar;
    FMute: TPngSpeedButton;
    FVolume: Integer;
    FVolumeChange: TNotifyEvent;
    FVolumePng: TPngImage;
    FVolumeMutedPng: TPngImage;

    procedure MuteClick(Sender: TObject);
    procedure VolumeChange(Sender: TObject);
    procedure RefreshButtonState(DoIt: Boolean);
    procedure FSetVolume(Volume: Integer);
    procedure FSetNotifyOnMove(Value: Boolean);
    function FGetVolume: Integer;
  public
    procedure Setup;

    property OnVolumeChange: TNotifyEvent read FVolumeChange write FVolumeChange;
    property Volume: Integer read FGetVolume write FSetVolume;
    property NotifyOnMove: Boolean write FSetNotifyOnMove;

    destructor Destroy; override;
  end;

implementation

{ TVolumePanel }

procedure TVolumePanel.Setup;
var
  ResStream: TResourceStream;
begin
  BevelOuter := bvNone;

  FMute := TPngSpeedButton.Create(Self);
  FMute.Hint := 'Mute';
  FMute.ShowHint := True;
  FMute.Flat := True;
  FMute.Align := alLeft;
  FMute.Width := 25;
  FMute.GroupIndex := 1;
  FMute.AllowAllUp := True;
  FMute.Down := True;
  FMute.OnClick := MuteClick;
  FMute.Parent := Self;

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

  FTrackBarPanel := TPanel.Create(Self);
  FTrackBarPanel.Align := alClient;
  FTrackBarPanel.BevelOuter := bvNone;
  FTrackBarPanel.Padding.Left := 4;
  FTrackBarPanel.Padding.Right := 2;
  FTrackBarPanel.Parent := Self;

  FTrackBar := TSeekBar.Create(Self);
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
begin
  if FMute.Down then
  begin
    AppGlobals.PlayerVolumeBeforeMute := FTrackBar.Position;
    FTrackBar.Position := 0;

    FMute.PngImage := FVolumeMutedPng;
    if not FMute.Down then
      FMute.Down := True;
  end else
  begin
    FTrackBar.Position := AppGlobals.PlayerVolumeBeforeMute;
    FMute.PngImage := FVolumePng;
  end;
end;

procedure TVolumePanel.VolumeChange(Sender: TObject);
begin
  RefreshButtonState(False);

  FVolume := FTrackBar.Position;

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
  // Rand links und oben
  Bmp.Canvas.MoveTo(0, Bmp.Height div 2 + 3);
  Bmp.Canvas.LineTo(0, Bmp.Height div 2 - 3);
  Bmp.Canvas.LineTo(Bmp.Width - Bmp.Canvas.Pen.Width, Bmp.Height div 2 - 3);
  // Rand rechts und unten
  Bmp.Canvas.Pen.Color := clGray;
  Bmp.Canvas.LineTo(Bmp.Width - Bmp.Canvas.Pen.Width, Bmp.Height div 2 + 3);
  Bmp.Canvas.LineTo(0, Bmp.Height div 2 + 3);

  R.Left := Canvas.Pen.Width;
  R.Top := Bmp.Height div 2 - 3 + Bmp.Canvas.Pen.Width;
  R.Bottom := Bmp.Height div 2 + 3;
  R.Right := Bmp.Width - Bmp.Canvas.Pen.Width;
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
    P := Trunc((FPosition / FMax) * (ClientWidth - 20));

    R.Top := 2;
    R.Left := P;
    R.Bottom := Bmp.Height - 2;
    R.Right := 20 + R.Left;

    if ThemeServices.ThemesEnabled then
    begin
      case GetGripperState of
        gsNormal:
          begin
            D := ThemeServices.GetElementDetails(tsThumbBtnHorzNormal);
            D2 := ThemeServices.GetElementDetails(tsGripperHorzNormal);
          end;
        gsHot:
          begin
            D := ThemeServices.GetElementDetails(tsThumbBtnHorzHot);
            D2 := ThemeServices.GetElementDetails(tsGripperHorzHot);
          end;
        gsDown:
          begin
            D := ThemeServices.GetElementDetails(tsThumbBtnHorzPressed);
            D2 := ThemeServices.GetElementDetails(tsGripperHorzPressed);
          end;
      end;

      ThemeServices.DrawElement(Bmp.Canvas.Handle, D, R);
      ThemeServices.DrawElement(Bmp.Canvas.Handle, D2, R);
    end else
    begin
      case GetGripperState of
        gsNormal:
          begin
            DrawButtonFace(Bmp.Canvas, R, 1, bsAutoDetect, True, False, False);
          end;
        gsHot:
          begin
            DrawButtonFace(Bmp.Canvas, R, 1, bsAutoDetect, True, False, True);
          end;
        gsDown:
          begin
            DrawButtonFace(Bmp.Canvas, R, 1, bsAutoDetect, True, True, True);
          end;
      end;
    end;

    FLastGripperState := GetGripperState;
    FLastGripperPos := FPosition;
  end;
end;

procedure TSeekBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  {
  if ThemeServices.ThemesEnabled and Assigned(Parent) and (csParentBackground in ControlStyle) then
  begin
    ThemeServices.DrawParentBackground(Handle, Msg.DC, nil, False);
  end
  else
    FillRect(Canvas.Handle, ClientRect, Canvas.Brush.Handle);
  }
  Msg.Result := 1;
end;

procedure TSeekBar.WndProc(var Message: TMessage);
begin
  inherited;

  if Message.Msg = CM_MOUSELEAVE then
    Paint;
end;

function TSeekBar.GetGripperPos(X: Integer): Cardinal;
begin
  FPosition := Trunc(((X - FDragFrom) / (ClientWidth - 20)) * Max);
end;

function TSeekBar.GetGripperState: TGripperStates;
var
  P: Cardinal;
  R: TRect;
  D, D2: TThemedElementDetails;
begin
  Result := gsUnknown;

  if not FGripperVisible then
    Exit;

  P := Trunc((FPosition / FMax) * (ClientWidth - 20));

  R.Top := 2;
  R.Left := P;
  R.Bottom := ClientHeight - 2;
  R.Right := 20 + R.Left;

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
end;

procedure TSeekBar.FSetGripperVisible(Value: Boolean);
begin
  FGripperVisible := Value;
  Paint;
end;

procedure TSeekBar.FSetPosition(Value: Int64);
begin
  if FSetting then
    Exit;

  FPosition := Value;
  if FMax = 0 then
    FGripperPos := 0
  else
    FGripperPos := Trunc((FPosition / FMax) * (ClientWidth - 20));

  if FNotifyOnMove then
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Paint;
end;

procedure TSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if not FGripperVisible then
    Exit;

  if Button = mbLeft then
  begin
    FGripperDown := True;

    FGripperPos := Trunc((FPosition / FMax) * (ClientWidth - 20));

    if (X > FGripperPos) and (X < FGripperPos + 20) then
    begin
      FDragFrom := Min(Abs(X - FGripperPos), Abs(FGripperPos - X));
    end else
    begin
      FDragFrom := 10;

      FPosition := Trunc(((X - FDragFrom) / (ClientWidth - 20)) * Max);
      FGripperPos := X - FDragFrom;

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
var
  XO: Integer;
  P: Cardinal;
  R: TRect;
begin
  inherited;

  if ssLeft in Shift then
  begin
    FPosition := Trunc(((X - FDragFrom) / (ClientWidth - 20)) * Max);
    FGripperPos := X - FDragFrom;

    if FPosition < 0 then
      FPosition := 0;
    if FPosition > FMax then
      FPosition := FMax;

    if FNotifyOnMove then
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self);

    FSetting := True;
  end;

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Paint;
end;

procedure TSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if Assigned(FOnPositionChanged) then
    FOnPositionChanged(Self);

  FSetting := False;
  FGripperDown := False;

  Paint;
end;

end.

