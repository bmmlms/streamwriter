unit CommunityLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, LanguageObjects, ShellAPI, Functions,
  AppData, ComCtrls, HomeCommunication;

type
  TfrmCommunityLogin = class(TForm)
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlConnecting: TPanel;
    lblConnecting: TLabel;
    prgConnecting: TProgressBar;
    pnlConnect: TPanel;
    txtPassword: TLabeledEdit;
    txtUsername: TLabeledEdit;
    txtText: TMemo;
    lblSignup: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lblSignupClick(Sender: TObject);
  private
    procedure ShowConnecting(Show: Boolean);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    procedure HomeCommStateChanged(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TfrmCommunityLogin.btnCancelClick(Sender: TObject);
begin
  if pnlConnecting.Visible then
  begin

  end else
    Close;
end;

procedure TfrmCommunityLogin.btnOKClick(Sender: TObject);
begin
  if (Trim(txtUsername.Text) = '') or (Trim(txtPassword.Text) = '') then
  begin
    MsgBox(Handle, _('You have to enter your username and your password.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if not HomeComm.Connected then
  begin
    MsgBox(Handle, _('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  ShowConnecting(True);

  HomeComm.LogOn(Trim(txtUsername.Text), Trim(txtPassword.Text));
end;

procedure TfrmCommunityLogin.DoClose(var Action: TCloseAction);
var
  Res: Integer;
begin
  if AppGlobals.UserWasSetup then
    Action := caFree
  else
  begin
    //Res := MsgBox(Handle, _('If you cancel logging in, you won''t be asked another time. You can always login from selecting the ''Logon...'' entry in the main menu.'#13#10'Are you sure you want to cancel logging in?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
    //if Res = IDYES then
    begin
      Action := caFree;
      AppGlobals.UserWasSetup := True;
      AppGlobals.User := '';
      AppGlobals.Pass := '';
    end;// else
    //  Action := caNone;
  end;

  if Action = caFree then
    inherited;
end;

procedure TfrmCommunityLogin.FormCreate(Sender: TObject);
begin
  pnlConnecting.BevelOuter := bvNone;
  pnlConnect.BevelOuter := bvNone;
  pnlConnecting.Align := alClient;
  pnlConnect.Align := alClient;

  txtText.Text := _('Logging in to the streamWriter community gives you some more options, for example setting ratings for streams.'#13#10 +
                    'More community features may get introduced in the future. If you don''t have an account yet, click the link below to signup for free.');

  txtUsername.Text := AppGlobals.User;
  txtPassword.Text := AppGlobals.Pass;
end;

procedure TfrmCommunityLogin.FormShow(Sender: TObject);
begin
  Language.Translate(Self);

  txtUsername.SetFocus;
  ShowConnecting(False);
end;

procedure TfrmCommunityLogin.HomeCommStateChanged(Sender: TObject);
begin
  if (not HomeComm.Connected) and (pnlConnecting.Visible) then
  begin
    ShowConnecting(False);
    MsgBox(Handle, _('The connection to the server was close while authenticating.'#13#10'Please try again later.'), _('Error'), MB_ICONERROR);
  end;

  if HomeComm.Connected and pnlConnecting.Visible then
    if HomeComm.Authenticated then
    begin
      if not AppGlobals.UserWasSetup then
        MsgBox(Handle, _('You are now logged in.'#13#10'Your credentials will be saved and streamWriter will try to login automatically next time. You can logoff by using the corresponding item in the main menu.'), _('Info'), MB_ICONINFORMATION);

      AppGlobals.User := txtUsername.Text;
      AppGlobals.Pass := txtPassword.Text;
      AppGlobals.UserWasSetup := True;

      Close;
    end else
    begin
      ShowConnecting(False);

      AppGlobals.User := '';
      AppGlobals.Pass := '';

      MsgBox(Handle, _('You have entered an unknown username or a wrong password.'#13#10'Please try again.'), _('Error'), MB_ICONERROR);
    end;
end;

procedure TfrmCommunityLogin.lblSignupClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('http://streamwriter.org/' + Language.CurrentLanguage.ID + '/benutzer/anmelden/'), '', '', 1);
end;

procedure TfrmCommunityLogin.ShowConnecting(Show: Boolean);
begin
  pnlConnect.Visible := not Show;
  pnlConnecting.Visible := Show;

  btnCancel.Enabled := not Show;
  btnOK.Enabled := not Show;
end;

end.
