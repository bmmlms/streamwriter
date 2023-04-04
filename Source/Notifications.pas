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
  LMessages,
  MStringFunctions,
  StdCtrls,
  SysUtils,
  UxTheme,
  Windows;

type

  { TfrmNotification }

  TfrmNotification = class(TForm)
    lblStream: TLabel;
    lblTitle: TLabel;
    pbLogo: TPaintBox;
    Panel1: TPanel;
    tmrFade: TTimer;
    procedure pbLogoPaint(Sender: TObject);
    procedure tmrFadeTimer(Sender: TObject);
  type
    TNotificationStates = (nsFadingIn, nsVisible, nsFadingOut);
  private
  const
    FADE_DURATION = 500;
    SHOW_DURATION = 3000;
  private
    FState: TNotificationStates;
    FFadeStartedAt, FFadeoutAt: TDateTime;
    FTitle, FStream: string;
    FRedisplayTitle, FRedisplayStream: string;
    FWindow: TfrmNotification; static;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
  protected
    procedure ShowNoActivate;
    procedure ControlsAligned; override;
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
end;

destructor TfrmNotification.Destroy;
begin
  Application.RemoveOnUserInputHandler(MouseHook);
  FWindow := nil;

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

      if Visible then
        ControlsAligned;

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

procedure TfrmNotification.MouseHook(Sender: TObject; Msg: Cardinal);
begin
  if (Msg = LM_LBUTTONDOWN) and (PtInRect(Self.BoundsRect, Mouse.CursorPos)) then
    Close;
end;

procedure TfrmNotification.pbLogoPaint(Sender: TObject);
var
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.SetSize(96, 96);
    Icon.LoadFromResourceName(HINSTANCE, 'MAINICON');

    DrawIconEx(pbLogo.Canvas.Handle, 0, 0, Icon.Handle, 64, 64, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

procedure TfrmNotification.ShowNoActivate;
begin
  AlphaBlend := True;
  AlphaBlendValue := 0;

  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  Visible := True;
end;

procedure TfrmNotification.ControlsAligned;
begin
  inherited;

  lblTitle.Caption := TMStringFunctions.TruncateText(FTitle, 350 - pbLogo.Width, lblTitle.Font);
  lblStream.Caption := TMStringFunctions.TruncateText(FStream, 350 - pbLogo.Width, lblStream.Font);

  Left := Screen.PrimaryMonitor.WorkareaRect.Right - ClientWidth - 15;
  Top := Screen.PrimaryMonitor.WorkareaRect.Bottom - ClientHeight - 15;
end;

procedure TfrmNotification.DoClose(var CloseAction: TCloseAction);
begin
  inherited;

  CloseAction := caFree;
end;

procedure TfrmNotification.Paint;
begin
  inherited;

  Canvas.Pen.Color := clBtnShadow;
  Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
end;

procedure TfrmNotification.HideWindow;
begin
  FRedisplayTitle := '';
  FRedisplayStream := '';
end;

end.
