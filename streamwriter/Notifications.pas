unit Notifications;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Forms, StdCtrls,
  Graphics, UxTheme, DWMAPI, PngImageList, ImgList, Math,
  GUIFunctions, LanguageObjects, Logging;

type
  TNotificationStates = (nsFadingIn, nsVisible, nsFadingOut);

  TfrmNotification = class(TForm)
    lblTitle: TLabel;
    lblStream: TLabel;
    PngImageList1: TPngImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FState: TNotificationStates;
    FDisplayOnEndTitle: string;
    FDisplayOnEndStream: string;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoShow; override;
    procedure WMMouseActivate(var Message: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    class procedure Act(Title, Stream: string);
    class procedure Stop;

    procedure Display(Title, Stream: string);
    procedure StopDisplay;
  end;

var
  NotificationForm: TfrmNotification;

implementation

{$R *.dfm}

{ TfrmNotification }

class procedure TfrmNotification.Act(Title, Stream: string);
begin
  if NotificationForm = nil then
  begin
    NotificationForm := TfrmNotification.Create(nil);
    NotificationForm.Display(Title, Stream);
  end else
  begin
    NotificationForm.Display(Title, Stream);
  end;
end;

constructor TfrmNotification.Create(AOwner: TComponent);
begin
  inherited;

  FState := nsFadingIn;
  Parent := nil;
  ClientHeight := 55;
end;

procedure TfrmNotification.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := 0;
  Params.Style := WS_POPUP or WS_THICKFRAME or WS_EX_TOPMOST;
  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE;
end;

procedure TfrmNotification.Display(Title, Stream: string);
var
  TextWidth: Integer;
begin
  case FState of
    nsFadingIn:
      begin
        TextWidth := GetTextSize(Title, lblTitle.Font).cx;
        if TextWidth > 350 then
          TextWidth := 350;
        ClientWidth := lblTitle.Left * 2 + 52 + Max(200, TextWidth);

        Left := Screen.PrimaryMonitor.WorkareaRect.Right - ClientWidth - GlassFrame.Right * 2 - 15;
        Top := Screen.PrimaryMonitor.WorkareaRect.Bottom - ClientHeight - GlassFrame.Top * 2 - 15;

        DoShow;
        lblTitle.Caption := TruncateText(Title, lblTitle.Width, lblTitle.Font);
        lblStream.Caption := TruncateText(Format(_('on %s'), [Stream]), lblStream.Width, lblStream.Font);

        ShowWindow(Handle, SW_HIDE);
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
      end;
    nsVisible, nsFadingOut:
      begin
        FDisplayOnEndTitle := Title;
        FDisplayOnEndStream := Stream;
        KillTimer(Handle, 0);
        KillTimer(Handle, 1);
        KillTimer(Handle, 2);
        SetTimer(Handle, 2, 20, nil);
      end;
  end;
end;

procedure TfrmNotification.DoShow;
begin
  AlphaBlend := True;
  AlphaBlendValue := 0;
  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  SetTimer(Handle, 0, 20, nil);
  SetTimer(Handle, 10, 50, nil);
end;

procedure TfrmNotification.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  KillTimer(Handle, 10);
  Action := caFree;
  NotificationForm := nil;
end;

procedure TfrmNotification.Paint;
begin
  inherited;
  PngImageList1.Draw(Canvas, ClientWidth - 52, 4, 0, True);
end;

class procedure TfrmNotification.Stop;
begin
  if NotificationForm <> nil then
    NotificationForm.StopDisplay;
end;

procedure TfrmNotification.StopDisplay;
begin
  FDisplayOnEndTitle := '';
  FDisplayOnEndStream := '';
  KillTimer(Handle, 0);
  KillTimer(Handle, 1);
  KillTimer(Handle, 2);
  SetTimer(Handle, 2, 5, nil);
end;

procedure TfrmNotification.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  Message.Result := MA_NOACTIVATEANDEAT;
end;

procedure TfrmNotification.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTCLIENT;
end;

procedure TfrmNotification.WMTimer(var Message: TWMTimer);
begin
  if Message.TimerID = 0 then
  begin
    if AlphaBlendValue + 20 < 225 then
      AlphaBlendValue := AlphaBlendValue + 20
    else
    begin
      AlphaBlendValue := 225;
      KillTimer(Handle, 0);
      SetTimer(Handle, 1, 4000, nil);
      FState := nsVisible;
    end;
  end else if Message.TimerID = 1 then
  begin
    KillTimer(Handle, 1);
    SetTimer(Handle, 2, 20, nil);
    FState := nsFadingOut;
  end else if Message.TimerID = 2 then
  begin
    if AlphaBlendValue - 20 > 0 then
      AlphaBlendValue := AlphaBlendValue - 20
    else
    begin
      AlphaBlendValue := 0;
      KillTimer(Handle, 2);

      if FDisplayOnEndTitle <> '' then
      begin
        FState := nsFadingIn;
        Display(FDisplayOnEndTitle, FDisplayOnEndStream);
        FDisplayOnEndTitle := '';
        FDisplayOnEndStream := '';
      end else
      begin
        Close;
      end;
    end;
  end;

  if Message.TimerID = 10 then
  begin
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
  end;
end;

end.
