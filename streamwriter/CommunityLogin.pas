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
    chkSaveData: TCheckBox;
    txtUsername: TLabeledEdit;
    txtText: TMemo;
    lblSignup: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHomeCommunication: THomeCommunication;

    procedure ShowConnecting(Show: Boolean);

    procedure HomeCommunicationUserAuthenticated(Sender: TObject; Value: Boolean);
  public

  end;

implementation

{$R *.dfm}

procedure TfrmCommunityLogin.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCommunityLogin.btnOKClick(Sender: TObject);
begin
  if (Trim(txtUsername.Text) = '') or (Trim(txtPassword.Text) = '') then
  begin
    Exit;
  end;

  ShowConnecting(True);

  //FHomeCommunication.AuthUser(Trim(txtUsername.Text), Trim(txtPassword.Text));
end;

procedure TfrmCommunityLogin.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  Res: Integer;
begin
  if AppGlobals.UserWasSetup then
    Action := caHide
  else
  begin
    Res := MsgBox(Handle, _('If you cancel logging in, you won''t be asked another time. You can always login from selecting the ''login'' entry in the main menu.'#13#10'Are you sure you want to cancel logging in?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
    if Res = IDYES then
    begin
      Action := caHide;
      AppGlobals.UserWasSetup := True;
    end else
      Action := caNone;
  end;
end;

procedure TfrmCommunityLogin.FormCreate(Sender: TObject);
begin
  FHomeCommunication := THomeCommunication.Create;
  FHomeCommunication.OnUserAuthenticated := HomeCommunicationUserAuthenticated;

  pnlConnecting.BevelOuter := bvNone;
  pnlConnect.BevelOuter := bvNone;
  pnlConnecting.Align := alClient;
  pnlConnect.Align := alClient;

  txtText.Text := _('Logging in to the streamWriter community gives you some more options, for example setting ratings for streams.'#13#10 +
                    'More community features may get introduced in the future. if you don''t have an account yet, click the link below to signup for free.');
end;

procedure TfrmCommunityLogin.FormDestroy(Sender: TObject);
begin
  FHomeCommunication.Destroy;
end;

procedure TfrmCommunityLogin.FormShow(Sender: TObject);
begin
  txtUsername.SetFocus;
  ShowConnecting(False);
end;

procedure TfrmCommunityLogin.HomeCommunicationUserAuthenticated(
  Sender: TObject; Value: Boolean);
begin
  if Value then
  begin
    AppGlobals.User := txtUsername.Text;
    AppGlobals.Pass := txtPassword.Text;
    AppGlobals.UserWasSetup := True;
    ShowMessage('yes');
    Close;
  end else
  begin
    ShowConnecting(False);
    ShowMessage('oh noews');
  end;
end;

procedure TfrmCommunityLogin.ShowConnecting(Show: Boolean);
begin
  pnlConnect.Visible := not Show;
  pnlConnecting.Visible := Show;

  //pnlConnecting.Left := ClientWidth div 2 - pnlConnecting.Width div 2;
  //pnlConnecting.Top := ClientHeight div 2 - pnlConnecting.Height div 2;
  //pnlConnecting.Visible := False;
end;

end.
