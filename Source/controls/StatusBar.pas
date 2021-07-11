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

{ This unit contains the StatusBar streamWriter is showing at it's bottom }
unit StatusBar;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ComCtrls, AppData, SharedData,
  Functions, LanguageObjects, CommCtrl, GUIFunctions, Forms, ExtCtrls, Images,
  GraphType;

type
  THomeConnectionState = (cshUndefined, cshConnected, cshConnectedSecure, cshDisconnected, cshFail);

  { TSWStatusBar }

  TSWStatusBar = class(TStatusBar)
  private
    FConnectionState: THomeConnectionState;
    FLoggedIn: Boolean;
    FNotifyTitleChanges: Boolean;
    FClients: Integer;
    FRecordings: Integer;
    FSpeed: UInt64;
    FSongsSaved: Cardinal;
    FOverallSongsSaved: Cardinal;
    FCurrentReceived: UInt64;
    FOverallReceived: UInt64;
    FLastPos: Integer;
    FSpace: Integer;
    FDots: string;

    FTimer: TTimer;
    FSpeedBmp: TBitmap;

    procedure TimerTimer(Sender: TObject);
    procedure FSetSpeed(Value: UInt64);
    procedure FSetCurrentReceived(Value: UInt64);
    procedure FSetOverallReceived(Value: UInt64);
  protected
    procedure DrawPanel(Panel: TStatusPanel; const R: TRect); override;
    procedure Resize; override;
    procedure InvalidatePanel(PanelIndex: integer); overload;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure SetState(ConnectionState: THomeConnectionState; LoggedIn, NotifyTitleChanges: Boolean;
      Clients, Recordings: Integer; SongsSaved, OverallSongsSaved: Cardinal);
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

constructor TSWStatusBar.Create(AOwner: TComponent);
var
  P: TStatusPanel;
begin
  inherited;

  SimplePanel := False;

  Height := GetTextSize('Wyg', Font).cy + 4;

  ShowHint := False;

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerTimer;
  FTimer.Interval := 1000;
  FTimer.Enabled := True;

  FSpace := MulDiv(GetTextSize('WWW', Font).cx, Screen.PixelsPerInch, 96);

  P := Panels.Add;
  P.Width := 2 + 56 + GetTextSize(_('Connecting...'), Font).cx + FSpace;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Width := 18 + 4 + 18 + GetTextSize('00000000', Font).cx + MulDiv(GetTextSize('W', Font).cx, Screen.PixelsPerInch, 96) + 10;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Width := 2 + GetTextSize(Format(_('%s/%s received'), ['000,00 kb', '000,00 kb']), Font).cx + FSpace;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Style := psOwnerDraw;
end;

destructor TSWStatusBar.Destroy;
begin
  FSpeedBmp.Free;

  inherited;
end;

procedure TSWStatusBar.DrawPanel(Panel: TStatusPanel; const R: TRect);
var
  ImageTop, TextTop: Integer;
begin
  inherited;

  ImageTop := R.Top + (R.Bottom - R.Top) div 2 - MulDiv(16, Screen.PixelsPerInch, 96) div 2;
  TextTop := R.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(_('Wyg')) div 2;

  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(R);

  case Panel.Index of
    0:
      begin
        if FConnectionState = cshDisconnected then
          FTimer.Enabled := True
        else
        begin
          FTimer.Enabled := False;
          FDots := '';
        end;

        case FConnectionState of
          cshConnected:
            begin
              modSharedData.imgImages.DrawForControl(Canvas, R.Left, ImageTop, TImages.CONNECT, 16, Self, gdeNormal);
              Canvas.TextOut(R.Left + 56, TextTop, _('Connected'));
            end;
          cshConnectedSecure:
            begin
              modSharedData.imgImages.DrawForControl(Canvas, R.Left, ImageTop, TImages.CONNECT_SECURE, 16, Self, gdeNormal);
              Canvas.TextOut(R.Left + 56, TextTop, _('Connected'));
            end;
          cshDisconnected:
            begin
              modSharedData.imgImages.DrawForControl(Canvas, R.Left, ImageTop, IfThen<Integer>(Length(FDots) mod 2 = 0, TImages.CONNECT, TImages.DISCONNECT), 16, Self, gdeNormal);
              Canvas.TextOut(R.Left + 56, TextTop, _('Connecting') + FDots);
            end;
          cshFail:
            begin
              modSharedData.imgImages.DrawForControl(Canvas, R.Left, ImageTop, TImages.DISCONNECT, 16, Self, gdeNormal);
              Canvas.TextOut(R.Left + 56, TextTop, _('Error'));
            end;
        end;

        modSharedData.imgImages.DrawForControl(Canvas, R.Left + 18, ImageTop, TImages.USER, 16, Self, IfThen<TGraphicsDrawEffect>(FLoggedIn, gdeNormal, gdeDisabled));
        modSharedData.imgImages.DrawForControl(Canvas, R.Left + 36, ImageTop, TImages.BRICKS, 16, Self, IfThen<TGraphicsDrawEffect>(FNotifyTitleChanges, gdeNormal, gdeDisabled));
      end;
    1:
      begin
        if (FConnectionState = cshConnected) or (FConnectionState = cshConnectedSecure) then
        begin
          modSharedData.imgImages.DrawForControl(Canvas, R.Left, ImageTop, TImages.GROUP, 16, Self, gdeNormal);
          Canvas.TextOut(R.Left + 18, TextTop, IntToStr(FClients));

          modSharedData.imgImages.DrawForControl(Canvas, R.Left + 18 + Canvas.TextWidth(IntToStr(FClients)) + 4, ImageTop, TImages.RECORD_RED, 16, Self, gdeNormal);
          Canvas.TextOut(R.Left + 18 + Canvas.TextWidth(IntToStr(FClients)) + 4 + 18, TextTop, IntToStr(FRecordings));
        end else
          Canvas.FillRect(R);
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
      Canvas.TextOut(R.Left + 2, TextTop, _('%s/%s received').Format([MakeSize(FCurrentReceived), MakeSize(FOverallReceived)]));
    4:
      Canvas.TextOut(R.Left + 2, TextTop, _('%d/%d songs saved').Format([FSongsSaved, FOverallSongsSaved]));
  end;
end;

procedure TSWStatusBar.FSetCurrentReceived(Value: UInt64);
var
  C: Boolean;
begin
  C := FCurrentReceived <> Value;
  FCurrentReceived := Value;

  if C then
    InvalidatePanel(3);
end;

procedure TSWStatusBar.FSetOverallReceived(Value: UInt64);
var
  C: Boolean;
begin
  C := FOverallReceived <> Value;
  FOverallReceived := Value;

  if C then
    InvalidatePanel(3);
end;

procedure TSWStatusBar.FSetSpeed(Value: UInt64);
begin
  FSpeed := Value;
  BuildSpeedBmp;
 InvalidatePanel(2);
end;

procedure TSWStatusBar.Resize;
begin
  inherited;

  BuildSpeedBmp;
end;

procedure TSWStatusBar.SetState(ConnectionState: THomeConnectionState; LoggedIn, NotifyTitleChanges: Boolean;
  Clients, Recordings: Integer; SongsSaved, OverallSongsSaved: Cardinal);
var
  OldConnectionState: THomeConnectionState;
  OldLoggedIn, OldNotifyTitleChanges: Boolean;
  OldClients, OldRecordings: Integer;
  OldSongsSaved, OldOverallSongsSaved: Cardinal;
begin
  OldConnectionState := FConnectionState;
  OldLoggedIn := FLoggedIn;
  OldNotifyTitleChanges := FNotifyTitleChanges;
  OldClients := FClients;
  OldRecordings := FRecordings;
  OldSongsSaved := FSongsSaved;
  OldOverallSongsSaved := FOverallSongsSaved;

  FConnectionState := ConnectionState;
  FLoggedIn := LoggedIn;
  FNotifyTitleChanges := NotifyTitleChanges;
  if (ConnectionState = cshConnected) or (ConnectionState = cshConnectedSecure) then
  begin
    FClients := Clients;
    FRecordings := Recordings;
  end else
  begin
    FClients := 0;
    FRecordings := 0;
  end;

  FSongsSaved := SongsSaved;
  FOverallSongsSaved := OverallSongsSaved;

  if (OldConnectionState <> FConnectionState) or (OldLoggedIn <> FLoggedIn) then
  begin
   InvalidatePanel(0);
   InvalidatePanel(1);
  end;

  if (OldClients <> FClients) or (OldRecordings <> FRecordings) or (OldNotifyTitleChanges <> FNotifyTitleChanges) then
  begin
    InvalidatePanel(0);
    InvalidatePanel(1);
  end;

  if (OldSongsSaved <> FSongsSaved) or (OldOverallSongsSaved <> FOverallSongsSaved) then
    InvalidatePanel(4);
end;

procedure TSWStatusBar.TimerTimer(Sender: TObject);
begin
  FDots := FDots + '.';
  if Length(FDots) = 4 then
    FDots := '';
 InvalidatePanel(0);
end;

procedure TSWStatusBar.InvalidatePanel(PanelIndex: integer);
begin
  InvalidatePanel(PanelIndex, [ppText]);
end;

end.
