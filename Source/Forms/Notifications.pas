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

unit Notifications;

interface

uses
  Classes,
  Controls,
  DateUtils,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  InterfaceBase,
  LCLType,
  LMessages,
  MStringFunctions,
  StdCtrls,
  SysUtils,
  UxTheme,
  Windows;

type

  { TfrmNotification }

  TfrmNotification = class(TForm)
    tmrFade: TTimer;
    procedure tmrFadeTimer(Sender: TObject);
  type
    TNotificationStates = (nsFadingIn, nsVisible, nsFadingOut);
  private
  const
    FADE_DURATION = 500;
    SHOW_DURATION = 3000;
    REAL_IMAGE_SIZE = 96;
    SMALL_IMAGE_SIZE = 64;
    TITLE_FONT_SIZE = 20;
    STREAM_FONT_SIZE = 12;
    BORDER_SIZE = 6;
  private
    FState: TNotificationStates;
    FFadeStartedAt, FFadeoutAt: TDateTime;
    FTitle, FStream: string;
    FBitmap: Graphics.TBitmap;

    FRedisplayTitle, FRedisplayStream: string;
    FWindow: TfrmNotification; static;

    procedure UpdateBitmap;
    procedure MouseHook(Sender: TObject; Msg: Cardinal);
  protected
    procedure ShowNoActivate;
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure Paint; override;
  public
    class procedure Display(Title, Stream: string);
    class procedure Hide;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DisplayWindow(Title, Stream: string);
    procedure HideWindow;
  end;

implementation

{$R *.lfm}

{ TfrmNotification }

class procedure TfrmNotification.Display(Title, Stream: string);
begin
  if TFunctions.WindowIsFullscreen then
    Exit;

  if FWindow = nil then
    FWindow := TfrmNotification.Create(nil);
  FWindow.DisplayWindow(Title, Stream);
end;

class procedure TfrmNotification.Hide;
begin
  if FWindow <> nil then
    FWindow.HideWindow;
end;

constructor TfrmNotification.Create(AOwner: TComponent);
begin
  inherited;

  FState := nsFadingIn;
  Application.AddOnUserInputHandler(MouseHook);

  FBitmap := Graphics.TBitmap.Create;
  FBitmap.Canvas.Font.Assign(Canvas.Font);
end;

destructor TfrmNotification.Destroy;
begin
  Application.RemoveOnUserInputHandler(MouseHook);
  FWindow := nil;
  FBitmap.Free;

  inherited;
end;

procedure TfrmNotification.DisplayWindow(Title, Stream: string);
begin
  case FState of
    nsFadingIn:
    begin
      FTitle := Title;
      FStream := Stream;

      FFadeStartedAt := Now;

      UpdateBitmap;

      Width := FBitmap.Width;
      Height := FBitmap.Height;
      Left := Screen.PrimaryMonitor.WorkareaRect.Right - Width - Scale96ToFont(15);
      Top := Screen.PrimaryMonitor.WorkareaRect.Bottom - Height - Scale96ToFont(15);

      if Visible then
        Invalidate;

      ShowNoActivate;
    end;
    nsVisible, nsFadingOut:
    begin
      FRedisplayTitle := Title;
      FRedisplayStream := Stream;
    end;
  end;
end;

procedure TfrmNotification.tmrFadeTimer(Sender: TObject);
begin
  case FState of
    nsFadingIn:
      if MilliSecondsBetween(Now, FFadeStartedAt) > FADE_DURATION then
      begin
        AlphaBlendValue := High(Byte);
        FState := nsVisible;
        FFadeoutAt := IncMilliSecond(Now, SHOW_DURATION);
      end else
        AlphaBlendValue := Trunc((MilliSecondsBetween(Now, FFadeStartedAt) / FADE_DURATION) * High(Byte));
    nsVisible:
      if Now > FFadeoutAt then
      begin
        FFadeStartedAt := Now;
        FState := nsFadingOut;
      end;
    nsFadingOut:
      if MilliSecondsBetween(Now, FFadeStartedAt) > FADE_DURATION then
      begin
        AlphaBlendValue := Low(Byte);

        if FRedisplayTitle <> '' then
        begin
          FState := nsFadingIn;
          DisplayWindow(FRedisplayTitle, FRedisplayStream);
          FRedisplayTitle := '';
          FRedisplayStream := '';
        end else
          Close;
      end else
        AlphaBlendValue := High(Byte) - Trunc((MilliSecondsBetween(Now, FFadeStartedAt) / FADE_DURATION) * High(Byte));
  end;
end;

procedure TfrmNotification.UpdateBitmap;
var
  MaxWidth, MaxTextWidth, TextWidth: Integer;
  ImageSize, TitleSize, StreamSize: TSize;
  ScaledBorderSize, ScaledImageSize, TransparentRight, TransparentTop: Integer;
  FormRect, ContentRect, TextRect: TRect;
  TextStyle: TTextStyle;
  TextMetricsTitle, TextMetricsStream: LCLType.TTextMetric;
  Icon: TIcon;
begin
  ScaledBorderSize := Scale96ToFont(BORDER_SIZE);
  ScaledImageSize := Min(REAL_IMAGE_SIZE, Scale96ToFont(SMALL_IMAGE_SIZE));

  Icon := TIcon.Create;
  try
    Icon.SetSize(REAL_IMAGE_SIZE, REAL_IMAGE_SIZE);
    Icon.LoadFromResourceName(HINSTANCE, 'MAINICON');

    TFunctions.GetMaxTransparent(Icon, TransparentTop, TransparentRight);

    TransparentRight := Trunc((ScaledImageSize / REAL_IMAGE_SIZE) * TransparentRight);
    TransparentTop := Trunc((ScaledImageSize / REAL_IMAGE_SIZE) * TransparentTop);

    ImageSize := TSize.Create(TransparentRight, ScaledImageSize - TransparentTop);

    MaxWidth := Trunc(Screen.PrimaryMonitor.WorkareaRect.Width * 0.2);
    MaxTextWidth := MaxWidth - ScaledBorderSize * 2 - ScaledBorderSize - ImageSize.Width;

    FBitmap.Canvas.Font.Size := TITLE_FONT_SIZE;
    WidgetSet.GetTextMetrics(FBitmap.Canvas.Handle, TextMetricsTitle);
    TitleSize := FBitmap.Canvas.TextExtent(FTitle);

    FBitmap.Canvas.Font.Size := STREAM_FONT_SIZE;
    WidgetSet.GetTextMetrics(FBitmap.Canvas.Handle, TextMetricsStream);
    StreamSize := FBitmap.Canvas.TextExtent(FStream);

    TextWidth := Min(Max(TitleSize.Width, StreamSize.Width), MaxTextWidth);

    FormRect := TRect.Create(0, 0, TextWidth + ScaledBorderSize * 2 + ScaledBorderSize + ImageSize.Width, ImageSize.Height + ScaledBorderSize * 2);

    ContentRect := FormRect;
    ContentRect.Inflate(-ScaledBorderSize, -ScaledBorderSize);

    TextRect := ContentRect;
    TextRect.Inflate(-ScaledBorderSize, -ScaledBorderSize);
    TextRect.Right -= ImageSize.Width - ScaledBorderSize;

    FBitmap.SetSize(FormRect.Width, FormRect.Height);

    FBitmap.Canvas.Pen.Color := clBtnShadow;
    FBitmap.Canvas.Rectangle(FormRect);

    FillChar(TextStyle, SizeOf(TextStyle), 0);
    TextStyle.EndEllipsis := True;

    FBitmap.Canvas.Font.Size := TITLE_FONT_SIZE;
    FBitmap.Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top - TextMetricsTitle.tmInternalLeading, FTitle, TextStyle);

    FBitmap.Canvas.Font.Size := STREAM_FONT_SIZE;
    FBitmap.Canvas.TextRect(TextRect, TextRect.Left, TextRect.Bottom - TextMetricsStream.tmHeight, FStream, TextStyle);

    DrawIconEx(FBitmap.Canvas.Handle, ContentRect.Right - ImageSize.Width, ContentRect.Top, Icon.Handle, ImageSize.Width, ImageSize.Height, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

procedure TfrmNotification.MouseHook(Sender: TObject; Msg: Cardinal);
begin
  if (Msg = LM_LBUTTONDOWN) and (PtInRect(Self.BoundsRect, Mouse.CursorPos)) then
    Close;
end;

procedure TfrmNotification.ShowNoActivate;
begin
  AlphaBlend := True;
  AlphaBlendValue := 0;

  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  Visible := True;
end;

procedure TfrmNotification.DoClose(var CloseAction: TCloseAction);
begin
  inherited;

  CloseAction := caFree;
end;

procedure TfrmNotification.Paint;
begin
  Canvas.Draw(0, 0, FBitmap);
end;

procedure TfrmNotification.HideWindow;
begin
  FRedisplayTitle := '';
  FRedisplayStream := '';
end;

end.
