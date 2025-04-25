{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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
  AppData,
  Classes,
  ComCtrls,
  CommCtrl,
  Controls,
  DDetours,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  GraphType,
  Images,
  LanguageObjects,
  LCLType,
  LMessages,
  MStringFunctions,
  SharedData,
  SysUtils,
  Themes,
  UxTheme,
  Win32Int,
  Win32Proc,
  Win32Themes,
  Win32WSComCtrls,
  Windows;

type
  THomeConnectionState = (cshUndefined, cshConnected, cshConnectedSecure, cshDisconnected, cshFail);

  { TSWStatusBar }

  TSWStatusBar = class(TStatusBar, IPostTranslatable)
  private const
    ICON_SIZE = 16;
    MARGIN = 2;
    SPEEDBMP_WIDTH = 35;
  private
    FConnectionState: THomeConnectionState;
    FLoggedIn: Boolean;
    FNotifyTitleChanges: Boolean;
    FClients: Integer;
    FRecordings: Integer;
    FSpeed: Cardinal;
    FSongsSaved: Cardinal;
    FOverallSongsSaved: Cardinal;
    FCurrentReceived: UInt64;
    FOverallReceived: UInt64;
    FLastPos: Integer;
    FSpace: Integer;
    FDots: string;
    FSpeedColors: array[0..4] of TColor;

    FTimer: TTimer;
    FSpeedBmp: Graphics.TBitmap;

    procedure TimerTimer(Sender: TObject);
    procedure FSetSpeed(Value: Cardinal);
    procedure FSetCurrentReceived(Value: UInt64);
    procedure FSetOverallReceived(Value: UInt64);
  protected
    procedure CreateHandle; override;
    procedure DrawPanel(Panel: TStatusPanel; const R: TRect); override;
    procedure Resize; override;
    procedure InvalidatePanel(PanelIndex: Integer); overload;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure SetState(ConnectionState: THomeConnectionState; LoggedIn, NotifyTitleChanges: Boolean; Clients, Recordings: Integer; SongsSaved, OverallSongsSaved: Cardinal);
    procedure BuildSpeedBmp;
    procedure PostTranslate;

    property Speed: Cardinal read FSpeed write FSetSpeed;
    property CurrentReceived: UInt64 read FCurrentReceived write FSetCurrentReceived;
    property OverallReceived: UInt64 read FOverallReceived write FSetOverallReceived;
  published
  end;

implementation

var
  SetWindowSubclassOld: function(hWnd: HWND; pfnSubclass: SUBCLASSPROC; uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): BOOL; stdcall;

// Copied from uWin32WidgetSetDark with added handling of psOwnerDraw.
function StatusBarWndProc(Window: HWND; Msg: UINT; wParam: Windows.WPARAM; lParam: Windows.LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  DC: HDC;
  X: Integer;
  Index: Integer;
  PS: TPaintStruct;
  LCanvas: TCanvas;
  APanel: TStatusPanel;
  StatusBar: TStatusBar;
  Info: PWin32WindowInfo;
  Detail: TThemedElementDetails;
  Rect: TRect;
  gripSize: TSize;
  DrawItemStruct: LCLType.TDrawItemStruct;
  DrawItemsMsg: TLMDrawItems;
begin
  Info := GetWin32WindowInfo(Window);
  if (Info = nil) or (Info^.WinControl = nil) then
  begin
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    Exit;
  end;

  if Msg = WM_ERASEBKGND then
  begin
    StatusBar := TStatusBar(Info^.WinControl);
    TWin32WSStatusBar.DoUpdate(StatusBar);
  end;

  if ((Msg = WM_PAINT) or (Msg = WM_ERASEBKGND)) then
  begin
    StatusBar := TStatusBar(Info^.WinControl);

    TWin32WSStatusBar.DoUpdate(StatusBar);

    DC := BeginPaint(Window, @ps);

    LCanvas := TCanvas.Create;
    try
      LCanvas.Handle := DC;
      LCanvas.Brush.Color := GetSysColor(COLOR_MENUHILIGHT);
      LCanvas.FillRect(ps.rcPaint);

      X := 1;
      LCanvas.Font.Color := GetSysColor(COLOR_BTNTEXT);
      LCanvas.Pen.Color := GetSysColor(COLOR_GRAYTEXT);
      if StatusBar.SimplePanel then
        LCanvas.TextOut(X + 3, (StatusBar.Height - LCanvas.TextHeight('Ag')) div 2, StatusBar.SimpleText)
      else
        for Index := 0 to StatusBar.Panels.Count - 1 do
        begin
          APanel := StatusBar.Panels[Index];
          if APanel.Width > 0 then
          begin
            if APanel.Style = psText then
              LCanvas.TextOut(X + 1, (StatusBar.Height - LCanvas.TextHeight('Ag')) div 2, APanel.Text)
            else
            begin
              FillChar(DrawItemStruct, SizeOf(DrawItemStruct), #0);
              DrawItemStruct.rcItem.Left := X + 1;
              DrawItemStruct.rcItem.Top := 0;
              DrawItemStruct.rcItem.Width := APanel.Width - 3;
              DrawItemStruct.rcItem.Height := StatusBar.Height;
              DrawItemStruct.itemID := Index;
              DrawItemStruct._hDC := LCanvas.Handle;

              DrawItemsMsg.Msg := LM_DRAWITEM;
              DrawItemsMsg.Ctl := 0;
              DrawItemsMsg.DrawItemStruct := @DrawItemStruct;

              StatusBar.Dispatch(DrawItemsMsg);
            end;

            if Index <> (StatusBar.Panels.Count - 1) then
            begin
              X += APanel.Width;
              LCanvas.Line(x - 2, {ps.rcPaint.Top +} 3, x - 2, {ps.rcPaint.Bottom} StatusBar.Height - 3);
            end;
          end;
        end;
      if StatusBar.SizeGrip then
      begin
        Rect := StatusBar.ClientRect;
        Detail := ThemeServices.GetElementDetails(tsGripper);
        GetThemePartSize(TWin32ThemeServices(ThemeServices).ThemeForPPI[teStatus, 0],
          LCanvas.Handle, SP_GRIPPER, 0, @Rect, TS_DRAW, gripSize);
        Rect.Left := Rect.Right - gripSize.cx;
        Rect.Top := Rect.Bottom - gripSize.cy;
        ThemeServices.DrawElement(LCanvas.Handle, Detail, Rect);
      end;
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;
    EndPaint(Window, @ps);
    Result := 0;
  end else
    Result := DefSubclassProc(Window, Msg, WParam, LParam);
end;

function SetWindowSubclassNew(hWnd: HWND; pfnSubclass: SUBCLASSPROC; uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): BOOL; stdcall;
begin
  Result := SetWindowSubclassOld(hWnd, @StatusBarWndProc, uIdSubclass, dwRefData);
end;

{ TSWStatusBar }

constructor TSWStatusBar.Create(AOwner: TComponent);
var
  P: TStatusPanel;
begin
  inherited;

  SimplePanel := False;

  Height := TMStringFunctions.GetTextSize(MeasureTextHeightString, Font).cy + 4;

  ShowHint := False;

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerTimer;
  FTimer.Interval := 1000;
  FTimer.Enabled := True;

  FSpace := TMStringFunctions.GetTextSize('WW', Font).Width;

  P := Panels.Add;
  P.Width := Scale96ToFont(MARGIN * 2 + (ICON_SIZE + MARGIN) * 3) + Max(TMStringFunctions.GetTextSize(_('Connecting...'), Font).Width, TMStringFunctions.GetTextSize(_('Connected'), Font).Width) + FSpace;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Width := Scale96ToFont(MARGIN * 2 + (ICON_SIZE + MARGIN) * 2) + TMStringFunctions.GetTextSize('00000000', Font).Width + FSpace;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Width := Scale96ToFont(MARGIN * 2) + TMStringFunctions.GetTextSize(Format(_('%s/%s received'), ['000,00 kb', '000,00 kb']), Font).Width + FSpace;
  P.Style := psOwnerDraw;

  P := Panels.Add;
  P.Style := psOwnerDraw;

  FSpeedColors[0] := TFunctions.HTML2Color('4b1616');
  FSpeedColors[1] := TFunctions.HTML2Color('722222');
  FSpeedColors[2] := TFunctions.HTML2Color('9d2626');
  FSpeedColors[3] := TFunctions.HTML2Color('c42c2c');
  FSpeedColors[4] := TFunctions.HTML2Color('d71717');
end;

destructor TSWStatusBar.Destroy;
begin
  FSpeedBmp.Free;

  inherited;
end;

procedure TSWStatusBar.BuildSpeedBmp;
var
  P: Integer;
  NewBmp: Graphics.TBitmap;
begin
  NewBmp := Graphics.TBitmap.Create;
  NewBmp.Transparent := True;
  NewBmp.TransparentColor := clFuchsia;
  NewBmp.Width := Scale96ToFont(SPEEDBMP_WIDTH);
  NewBmp.Height := ClientHeight - Scale96ToFont(MARGIN * 2);
  NewBmp.Canvas.Pen.Width := 1;
  NewBmp.Canvas.Pen.Color := clGray;
  NewBmp.Canvas.Brush.Color := NewBmp.TransparentColor;
  NewBmp.Canvas.FillRect(0, 0, NewBmp.Width, NewBmp.Height);

  if (FSpeedBmp <> nil) and (FSpeedBmp.Height = NewBmp.Height) then
    NewBmp.Canvas.Draw(-1, 0, FSpeedBmp);
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
    FSpeedBmp.Canvas.Brush.Color := FSpeedColors[0];
    FSpeedBmp.Canvas.Pen.Color := FSpeedColors[0];
    FSpeedBmp.Canvas.FillRect(Classes.Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(75, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(65, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 75 then
  begin
    FSpeedBmp.Canvas.Brush.Color := FSpeedColors[1];
    FSpeedBmp.Canvas.Pen.Color := FSpeedColors[1];
    FSpeedBmp.Canvas.FillRect(Classes.Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(85, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(75, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 85 then
  begin
    FSpeedBmp.Canvas.Brush.Color := FSpeedColors[2];
    FSpeedBmp.Canvas.Pen.Color := FSpeedColors[2];
    FSpeedBmp.Canvas.FillRect(Classes.Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(90, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(85, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 90 then
  begin
    FSpeedBmp.Canvas.Brush.Color := FSpeedColors[3];
    FSpeedBmp.Canvas.Pen.Color := FSpeedColors[3];
    FSpeedBmp.Canvas.FillRect(Classes.Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(95, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(90, FSpeedBmp.Height, 100)));
  end;

  if MulDiv(P, 100, FSpeedBmp.Height) >= 95 then
  begin
    FSpeedBmp.Canvas.Brush.Color := FSpeedColors[4];
    FSpeedBmp.Canvas.Pen.Color := FSpeedColors[4];
    FSpeedBmp.Canvas.FillRect(Classes.Rect(FSpeedBmp.Width - 1, FSpeedBmp.Height - MulDiv(100, FSpeedBmp.Height, 100), FSpeedBmp.Width, FSpeedBmp.Height - MulDiv(95, FSpeedBmp.Height, 100)));
  end;

  FLastPos := P;
end;

procedure TSWStatusBar.PostTranslate;
begin
  Invalidate;
end;

procedure TSWStatusBar.DrawPanel(Panel: TStatusPanel; const R: TRect);
var
  ImageTop, TextTop: Integer;
  PanelRect: TRect;
begin
  inherited;

  PanelRect := R;
  PanelRect.Left += Scale96ToFont(MARGIN);

  ImageTop := PanelRect.Top + PanelRect.Height div 2 - 16 div 2;
  TextTop := PanelRect.Top + PanelRect.Height div 2 - Canvas.TextHeight(MeasureTextHeightString) div 2;

  Canvas.Brush.Style := bsClear;

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
          modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left, ImageTop, TImages.CONNECT, ICON_SIZE, Self, gdeNormal);
          Canvas.TextOut(PanelRect.Left + Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), TextTop, TMStringFunctions.TruncateText(_('Connected'), PanelRect.Width - Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), Canvas.Font));
        end;
        cshConnectedSecure:
        begin
          modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left, ImageTop, TImages.CONNECT_SECURE, ICON_SIZE, Self, gdeNormal);
          Canvas.TextOut(PanelRect.Left + Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), TextTop, TMStringFunctions.TruncateText(_('Connected'), PanelRect.Width - Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), Canvas.Font));
        end;
        cshDisconnected:
        begin
          modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left, ImageTop, IfThen<Integer>(Length(FDots) mod 2 = 0, TImages.CONNECT, TImages.DISCONNECT), ICON_SIZE, Self, gdeNormal);
          Canvas.TextOut(PanelRect.Left + Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), TextTop, TMStringFunctions.TruncateText(_('Connecting') + FDots, PanelRect.Width - Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), Canvas.Font));
        end;
        cshFail:
        begin
          modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left, ImageTop, TImages.DISCONNECT, ICON_SIZE, Self, gdeNormal);
          Canvas.TextOut(PanelRect.Left + Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), TextTop, TMStringFunctions.TruncateText(_('Error'), PanelRect.Width - Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 3), Canvas.Font));
        end;
      end;

      modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left + Scale96ToFont(ICON_SIZE + MARGIN), ImageTop, TImages.USER, ICON_SIZE, Self, IfThen<TGraphicsDrawEffect>(FLoggedIn, gdeNormal, gdeDisabled));
      modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left + Scale96ToFont((ICON_SIZE + MARGIN) * 2), ImageTop, TImages.BRICKS, ICON_SIZE, Self, IfThen<TGraphicsDrawEffect>(FNotifyTitleChanges, gdeNormal, gdeDisabled));
    end;
    1:
      if ((FConnectionState = cshConnected) or (FConnectionState = cshConnectedSecure)) and (FClients > 0) then
      begin
        modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left, ImageTop, TImages.GROUP, ICON_SIZE, Self, gdeNormal);
        Canvas.TextOut(PanelRect.Left + Scale96ToFont(ICON_SIZE + MARGIN), TextTop, IntToStr(FClients));

        modSharedData.imgImages.DrawForControl(Canvas, PanelRect.Left + Scale96ToFont(ICON_SIZE + MARGIN) + Canvas.TextWidth(IntToStr(FClients)) + Scale96ToFont(MARGIN * 2), ImageTop, TImages.RECORD_RED, ICON_SIZE, Self, gdeNormal);

        Canvas.TextOut(PanelRect.Left + Scale96ToFont(MARGIN + (ICON_SIZE + MARGIN) * 2) + Canvas.TextWidth(IntToStr(FClients)), TextTop, IntToStr(FRecordings));
      end else
        Canvas.FillRect(PanelRect);
    2:
    begin
      Canvas.TextOut(PanelRect.Left, PanelRect.Top + ((PanelRect.Bottom - PanelRect.Top) div 2) - Canvas.TextHeight(TFunctions.MakeSize(FSpeed) + '/s') div 2, TFunctions.MakeSize(FSpeed) + '/s');
      if AppGlobals.LimitSpeed and (AppGlobals.MaxSpeed > 0) then
      begin
        Panels[2].Width := Scale96ToFont(MARGIN + SPEEDBMP_WIDTH) + Canvas.TextWidth(_('000.00/KBs')) + FSpace;
        if FSpeedBmp <> nil then
          Canvas.Draw(PanelRect.Right - FSpeedBmp.Width - Scale96ToFont(MARGIN), PanelRect.Top + PanelRect.Height div 2 - FSpeedBmp.Height div 2, FSpeedBmp);
      end else
        Panels[2].Width := Scale96ToFont(MARGIN) + Canvas.TextWidth(_('000.00/KBs')) + FSpace;
    end;
    3:
      Canvas.TextOut(PanelRect.Left, TextTop, _('%s/%s received').Format([TFunctions.MakeSize(FCurrentReceived), TFunctions.MakeSize(FOverallReceived)]));
    4:
      Canvas.TextOut(PanelRect.Left, TextTop, _('%d/%d songs saved').Format([FSongsSaved, FOverallSongsSaved]));
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

procedure TSWStatusBar.CreateHandle;
begin
  // MetaDarkStyle uses SetWindowSubclass() for setting a custom WndProc which does not handle owner drawn panels.
  // By intercepting SetWindowSubclass() another WndProc can be set.
  @SetWindowSubclassOld := InterceptCreate(@SetWindowSubclass, @SetWindowSubclassNew);
  try
    inherited CreateHandle;
  finally
    InterceptRemove(@SetWindowSubclassOld);
  end;
end;

procedure TSWStatusBar.FSetSpeed(Value: Cardinal);
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

procedure TSWStatusBar.SetState(ConnectionState: THomeConnectionState; LoggedIn, NotifyTitleChanges: Boolean; Clients, Recordings: Integer; SongsSaved, OverallSongsSaved: Cardinal);
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

procedure TSWStatusBar.InvalidatePanel(PanelIndex: Integer);
begin
  InvalidatePanel(PanelIndex, [ppText]);
end;

end.
