unit SeekBar;

interface

uses
  Windows, SysUtils, Classes, ComCtrls, ExtCtrls, Controls, Graphics,
  Menus, Themes, Messages, Math, Buttons, Forms;

type
  TGripperStates = (gsUnknown, gsNormal, gsHot, gsDown);

  TMSeekBar = class(TCustomControl)
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
    property PositionBeforeDrag: Int64 read FPositionBeforeDrag write FPositionBeforeDrag;
  published
    property Align;
    property GripperVisible: Boolean read FGripperDown write FSetGripperVisible;
    property NotifyOnMove: Boolean read FNotifyOnMove write FNotifyOnMove;
    property NotifyOnDown: Boolean read FNotifyOnDown write FNotifyOnDown;
    property Orientation: TScrollBarKind read FOrientation write FOrientation;
    property Position: Int64 read FPosition write FSetPosition;
    property Max: Int64 read FMax write FMax;

    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
  end;

implementation

{ TSeekBar }

procedure TMSeekBar.Paint;
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

procedure TMSeekBar.PaintBackground(Bmp: TBitmap);
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

procedure TMSeekBar.PaintGripper(Bmp: TBitmap);
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

procedure TMSeekBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TMSeekBar.WndProc(var Message: TMessage);
begin
  inherited;

  if Message.Msg = CM_MOUSELEAVE then
    Paint;
end;

function TMSeekBar.GetGripperState: TGripperStates;
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

constructor TMSeekBar.Create(AOwner: TComponent);
begin
  inherited;

  FMax := 0;
  FPositionBeforeDrag := -1;
  FOrientation := sbHorizontal;
  FGripperVisible := True;
end;

procedure TMSeekBar.FSetGripperVisible(Value: Boolean);
begin
  FGripperVisible := Value;
  Paint;
end;

procedure TMSeekBar.FSetPosition(Value: Int64);
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

procedure TMSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
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

procedure TMSeekBar.MouseMove(Shift: TShiftState; X, Y: Integer);
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
  end;

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Paint;
end;

procedure TMSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

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

end.
