{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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

{ This unit contains the StatusBar streamWriter is showing at it's bottom }
unit StatusBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls, AppData,
  Functions, LanguageObjects, CommCtrl, GUIFunctions, Forms;

type
  THomeConnectionState = (cshUndefined, cshConnected, cshDisconnected, cshFail);

  TSWStatusBar = class(TStatusBar)
  private
    FConnectionState: THomeConnectionState;
    FLoggedIn: Boolean;
    FNotifyTitleChanges: Boolean;
    FClients: Integer;
    FRecordings: Integer;
    FSpeed: UInt64;
    FSongsSaved: Cardinal;
    FCurrentReceived: UInt64;
    FOverallReceived: UInt64;
    FLastPos: Integer;
    FSpace: Integer;

    FSpeedBmp: TBitmap;
    IconConnected, IconDisconnected: TIcon;
    IconLoggedIn, IconLoggedOff: TIcon;
    IconGroupAutoEnabled: TIcon;
    IconGroupAutoDisabled: TIcon;

    procedure PaintPanel(Index: Integer);
    procedure FSetSpeed(Value: UInt64);
    procedure FSetCurrentReceived(Value: UInt64);
    procedure FSetOverallReceived(Value: UInt64);
  protected
    procedure DrawPanel(Panel: TStatusPanel; const R: TRect); override;
    procedure Resize; override;
    procedure CNDrawitem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure SetState(ConnectionState: THomeConnectionState; LoggedIn, NotifyTitleChanges: Boolean; Clients, Recordings: Integer;
      SongsSaved: Cardinal);
    procedure BuildSpeedBmp;
    property Speed: UInt64 read FSpeed write FSetSpeed;
    property CurrentReceived: UInt64 read FCurrentReceived write FSetCurrentReceived;
    property OverallReceived: UInt64 read FOverallReceived write FSetOverallReceived;
  published
  end;

implementation

{ TSWStatusBar }

procedure TSWStatusBar.BuildSpeedBmp;
var
  P: Integer;
  NewBmp: TBitmap;
begin
  NewBmp := TBitmap.Create;
  NewBmp.Width := 35;
  NewBmp.Height := ClientHeight - 4;
  NewBmp.Canvas.Pen.Width := 1;
  NewBmp.Canvas.Brush.Color := clBtnFace;
  NewBmp.Canvas.Pen.Color := clBlack;
  NewBmp.Canvas.FillRect(Rect(0, 0, NewBmp.Width, NewBmp.Height));

  if FSpeedBmp <> nil then
  begin
    NewBmp.Canvas.Draw(-1, 0, FSpeedBmp);
  end;
  FSpeedBmp.Free;
  FSpeedBmp := NewBmp;

  P := 0;
  if AppGlobals.MaxSpeed > 0 then
  begin
    P := Trunc(((FSpeed / 1024) / AppGlobals.MaxSpeed) * NewBmp.Height - 1);
    if P > NewBmp.Height - 1 then
      P := NewBmp.Height - 1;
    if P < 1 then
      P := 1;
  end;

  FSpeedBmp.Canvas.MoveTo(0, FSpeedBmp.Height - 1);
  FSpeedBmp.Canvas.LineTo(FSpeedBmp.Width - 1, FSpeedBmp.Height - 1);

  FSpeedBmp.Canvas.MoveTo(FSpeedBmp.Width - 1, FSpeedBmp.Height - P);
  FSpeedBmp.Canvas.LineTo(FSpeedBmp.Width - 1, FSpeedBmp.Height);

  if MulDiv(P, 100, FSpeedBmp.Height) >= 65 then
  begin
    FSpeedBmp.Canvas.Brush.Color := HTML2Color('4b1616');
    FSpeedBmp.Canvas.Pen.Color := HTML2Color('4b1616');
    FSpeedBmp.Canvas.FillRect(Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(75, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(65, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 75 then
  begin
    FSpeedBmp.Canvas.Brush.Color := HTML2Color('722222');
    FSpeedBmp.Canvas.Pen.Color := HTML2Color('722222');
    FSpeedBmp.Canvas.FillRect(Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(85, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(75, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 85 then
  begin
    FSpeedBmp.Canvas.Brush.Color := HTML2Color('9d2626');
    FSpeedBmp.Canvas.Pen.Color := HTML2Color('9d2626');
    FSpeedBmp.Canvas.FillRect(Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(90, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(85, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 90 then
  begin
    FSpeedBmp.Canvas.Brush.Color := HTML2Color('c42c2c');
    FSpeedBmp.Canvas.Pen.Color := HTML2Color('c42c2c');
    FSpeedBmp.Canvas.FillRect(Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(95, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(90, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 95 then
  begin
    FSpeedBmp.Canvas.Brush.Color := HTML2Color('d71717');
    FSpeedBmp.Canvas.Pen.Color := HTML2Color('d71717');
    FSpeedBmp.Canvas.FillRect(Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(100, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(95, FSpeedBmp.Height, 100)));
  end;

  FLastPos := P;
end;

procedure TSWStatusBar.CNDrawitem(var Message: TWMDrawItem);
begin
  inherited;

  Message.Result := 1;
end;

constructor TSWStatusBar.Create(AOwner: TComponent);
var
  P: TStatusPanel;
  C: TAccessCanvas;
begin
  inherited;

  Height := GetTextSize('Wyg', Font).cy + 4;

  ShowHint := True;

  IconConnected := TIcon.Create;
  IconConnected.Handle := LoadImage(HInstance, 'CONNECT', IMAGE_ICON, 15, 15, LR_DEFAULTCOLOR);
  IconDisconnected := TIcon.Create;
  IconDisconnected.Handle := LoadImage(HInstance, 'DISCONNECT', IMAGE_ICON, 15, 15, LR_DEFAULTCOLOR);
  IconLoggedIn := TIcon.Create;
  IconLoggedIn.Handle := LoadImage(HInstance, 'USER_GO', IMAGE_ICON, 15, 15, LR_DEFAULTCOLOR);
  IconLoggedOff := TIcon.Create;
  IconLoggedOff.Handle := LoadImage(HInstance, 'USER_DELETE', IMAGE_ICON, 15, 15, LR_DEFAULTCOLOR);
  IconGroupAutoEnabled := TIcon.Create;
  IconGroupAutoEnabled.Handle := LoadImage(HInstance, 'GROUP_GO', IMAGE_ICON, 15, 15, LR_DEFAULTCOLOR);
  IconGroupAutoDisabled := TIcon.Create;
  IconGroupAutoDisabled.Handle := LoadImage(HInstance, 'GROUP_DELETE', IMAGE_ICON, 15, 15, LR_DEFAULTCOLOR);

  FSpace := MulDiv(GetTextSize('WWW', Font).cx, Screen.PixelsPerInch, 96);

  P := Panels.Add;
  P.Width := 2 + 38 + GetTextSize(_('Connecting...'), Font).cx + FSpace;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Width := 90;
  P.Width := 2 + 20 + GetTextSize('0000/0000', Font).cx + MulDiv(GetTextSize('W', Font).cx, Screen.PixelsPerInch, 96);
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Width := 2 + GetTextSize(Format(_('%s/%s received'), ['000,00 kb', '000, 00 kb']), Font).cx + FSpace;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Style := psOwnerDraw;
end;

destructor TSWStatusBar.Destroy;
begin
  IconConnected.Free;
  IconDisconnected.Free;
  IconLoggedIn.Free;
  IconLoggedOff.Free;
  IconGroupAutoEnabled.Free;
  IconGroupAutoDisabled.Free;
  FSpeedBmp.Free;

  inherited;
end;

procedure TSWStatusBar.DrawPanel(Panel: TStatusPanel; const R: TRect);
begin
  inherited;

  Hint := _('Users/active streams');
  if (FConnectionState = cshConnected) and FNotifyTitleChanges then
    Hint := Hint + _(' (automatic recordings enabled)')
  else
    Hint := Hint + _(' (automatic recordings disabled)');

  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(R);

  case Panel.Index of
    0:
      begin
        case FConnectionState of
          cshConnected:
            begin
              Canvas.Draw(R.Left, R.Top + (R.Bottom - R.Top) div 2 - IconConnected.Height div 2, IconConnected);
              Canvas.TextOut(R.Left + 38, R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(_('Connected')) div 2, _('Connected'));
            end;
          cshDisconnected:
            begin
              Canvas.Draw(R.Left, R.Top + (R.Bottom - R.Top) div 2 - IconConnected.Height div 2, IconDisconnected);
              Canvas.TextOut(R.Left + 38, R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(_('Connecting...')) div 2, _('Connecting...'));
            end;
          cshFail:
            begin
              Canvas.Draw(R.Left, R.Top + (R.Bottom - R.Top) div 2 - IconConnected.Height div 2, IconDisconnected);
              Canvas.TextOut(R.Left + 38, R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(_('Error')) div 2, _('Error'));
            end;
        end;

        if (FConnectionState = cshConnected) and FLoggedIn then
          Canvas.Draw(R.Left + 18, R.Top + (R.Bottom - R.Top) div 2 - IconConnected.Height div 2, IconLoggedIn)
        else
          Canvas.Draw(R.Left + 18, R.Top + (R.Bottom - R.Top) div 2 - IconConnected.Height div 2, IconLoggedOff);
      end;
    1:
      begin
        if (FConnectionState = cshConnected) and FNotifyTitleChanges then
          Canvas.Draw(R.Left, R.Top + (R.Bottom - R.Top) div 2 - IconConnected.Height div 2, IconGroupAutoEnabled)
        else
          Canvas.Draw(R.Left, R.Top + (R.Bottom - R.Top) div 2 - IconConnected.Height div 2, IconGroupAutoDisabled);

        Canvas.TextOut(R.Left + 20, R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(IntToStr(FClients) + '/' + IntToStr(FRecordings)) div 2, IntToStr(FClients) + '/' + IntToStr(FRecordings));
      end;
    2:
      begin
        Canvas.TextOut(R.Left + 2, R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(MakeSize(FSpeed) + '/s') div 2, MakeSize(FSpeed) + '/s');
        if AppGlobals.LimitSpeed and (AppGlobals.MaxSpeed > 0) then
        begin
          Panels[2].Width := 2 + 35 + GetTextSize(_('0000/KBs'), Font).cx + FSpace;
          if FSpeedBmp <> nil then
            Canvas.Draw(R.Right - FSpeedBmp.Width - 2, R.Bottom - FSpeedBmp.Height, FSpeedBmp);
        end else
          Panels[2].Width := 2 + GetTextSize(_('0000/KBs'), Font).cx + FSpace;
      end;
    3:
      Canvas.TextOut(R.Left + 2, R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(Format(_('%s/%s received'), [MakeSize(FCurrentReceived), MakeSize(FOverallReceived)])) div 2, Format(_('%s/%s received'), [MakeSize(FCurrentReceived), MakeSize(FOverallReceived)]));
    4:
      Canvas.TextOut(R.Left + 2, R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(Format(_('%d songs saved'), [FSongsSaved])) div 2, Format(_('%d songs saved'), [FSongsSaved]));
  end;
end;

procedure TSWStatusBar.FSetCurrentReceived(Value: UInt64);
var
  C: Boolean;
begin
  C := FCurrentReceived <> Value;
  FCurrentReceived := Value;

  if C then
    PaintPanel(3);
end;

procedure TSWStatusBar.FSetOverallReceived(Value: UInt64);
var
  C: Boolean;
begin
  C := FOverallReceived <> Value;
  FOverallReceived := Value;

  if C then
    PaintPanel(3);
end;

procedure TSWStatusBar.FSetSpeed(Value: UInt64);
begin
  FSpeed := Value;
  BuildSpeedBmp;
  PaintPanel(2);
end;

procedure TSWStatusBar.PaintPanel(Index: Integer);
var
  R: TRect;
begin
  Perform(SB_GETRECT, Index, Integer(@R));
  R.Right := R.Right - 2;
  R.Top := R.Top + 1;
  R.Left := R.Left + 2;
  R.Bottom := R.Bottom - 1;
  DrawPanel(Panels[Index], R);
end;

procedure TSWStatusBar.Resize;
begin
  inherited;

  BuildSpeedBmp;
end;

procedure TSWStatusBar.SetState(ConnectionState: THomeConnectionState; LoggedIn, NotifyTitleChanges: Boolean; Clients, Recordings: Integer;
  SongsSaved: Cardinal);
var
  OldConnectionState: THomeConnectionState;
  OldLoggedIn, OldNotifyTitleChanges: Boolean;
  OldClients, OldRecordings: Integer;
  OldSongsSaved: Cardinal;
begin
  OldConnectionState := FConnectionState;
  OldLoggedIn := FLoggedIn;
  OldNotifyTitleChanges := FNotifyTitleChanges;
  OldClients := FClients;
  OldRecordings := FRecordings;
  OldSongsSaved := FSongsSaved;

  FConnectionState := ConnectionState;
  FLoggedIn := LoggedIn;
  FNotifyTitleChanges := NotifyTitleChanges;
  if ConnectionState = cshConnected then
  begin
    FClients := Clients;
    FRecordings := Recordings;
  end else
  begin
    FClients := 0;
    FRecordings := 0;
  end;

  FSongsSaved := SongsSaved;

  if (OldConnectionState <> FConnectionState) or (OldLoggedIn <> FLoggedIn) then
    PaintPanel(0);
  if (OldClients <> FClients) or (OldRecordings <> FRecordings) or (OldNotifyTitleChanges <> FNotifyTitleChanges) then
    PaintPanel(1);

  if OldSongsSaved <> FSongsSaved then
    PaintPanel(4);
end;

procedure TSWStatusBar.WMPaint(var Message: TWMPaint);
var
  i: Integer;
begin
  inherited;

  for i := 0 to Panels.Count - 1 do
  begin
    PaintPanel(i);
  end;
end;

end.
