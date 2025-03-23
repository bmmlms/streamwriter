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

unit CommunityLogin;

interface

uses
  AppData,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  HomeCommunication,
  Images,
  LanguageObjects,
  Logging,
  MControlFocuser,
  MControls,
  MLabeledEdit,
  SharedData,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type

  { TfrmCommunityLogin }

  TfrmCommunityLogin = class(TForm)
    txtPassword: TMLabeledEdit;
    lblText: TLabel;
    txtUsername: TMLabeledEdit;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlConnecting: TPanel;
    lblConnecting: TLabel;
    prgConnecting: TProgressBar;
    pnlConnect: TPanel;
    lblSignup: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lblSignupClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure ShowConnecting(Show: Boolean);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    procedure HomeCommLogIn(Sender: TObject; Success: Boolean);
  end;

implementation

{$R *.lfm}

procedure TfrmCommunityLogin.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCommunityLogin.btnOKClick(Sender: TObject);
begin
  if (Trim(txtUsername.Control.Text) = '') or (Trim(txtPassword.Control.Text) = '') then
  begin
    TFunctions.MsgBox(_('You have to enter your username and your password.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if not HomeComm.CommunicationEstablished then
  begin
    TFunctions.MsgBox(_('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  ShowConnecting(True);

  HomeComm.SendLogIn(Trim(txtUsername.Control.Text), Trim(txtPassword.Control.Text));
end;

procedure TfrmCommunityLogin.DoClose(var Action: TCloseAction);
begin
  if AppGlobals.UserWasSetup then
    Action := caFree
  else
  begin
    Action := caFree;
    AppGlobals.UserWasSetup := True;
    AppGlobals.User := '';
    AppGlobals.Pass := '';
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

  lblText.Caption := _('Logging in to the streamWriter community gives you some more options, for example setting ratings for streams.'#13#10 +
    'More community features may get introduced in the future. If you don''t have an account yet, click the link below to signup for free.');

  txtUsername.Control.Text := AppGlobals.User;
  txtPassword.Control.Text := AppGlobals.Pass;

  modSharedData.imgImages.GetIcon(TImages.USER, Icon);

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
  Constraints.MinHeight := Scale96ToFont(Constraints.MinHeight);
end;

procedure TfrmCommunityLogin.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmCommunityLogin.FormShow(Sender: TObject);
begin
  Language.Translate(Self);

  txtUsername.Control.ApplyFocus;
  ShowConnecting(False);
end;

procedure TfrmCommunityLogin.HomeCommLogIn(Sender: TObject; Success: Boolean);
begin
  if (not HomeComm.CommunicationEstablished) and (pnlConnecting.Visible) then
  begin
    ShowConnecting(False);
    TFunctions.MsgBox(_('The connection to the server was closed while authenticating.'#13#10'Please try again later.'), _('Error'), MB_ICONERROR);
  end;

  if HomeComm.CommunicationEstablished and pnlConnecting.Visible then
  begin
    ShowConnecting(False);

    if Success then
    begin
      if not AppGlobals.UserWasSetup then
        TFunctions.MsgBox(_('You are now logged in.'#13#10'Your credentials will be saved and streamWriter will try to login automatically next time. You can logoff by using the corresponding item in the main menu.'), _('Info'), MB_ICONINFORMATION);

      AppGlobals.User := txtUsername.Control.Text;
      AppGlobals.Pass := txtPassword.Control.Text;
      AppGlobals.UserWasSetup := True;

      Close;
    end else
      TFunctions.MsgBox(_('You have entered an unknown username or a wrong password.'#13#10'Please try again.'), _('Error'), MB_ICONERROR);
  end;
end;

procedure TfrmCommunityLogin.lblSignupClick(Sender: TObject);
begin
  TFunctions.ShellExecute(0, 'open', 'https://streamwriter.org/benutzer/anmelden/');
end;

procedure TfrmCommunityLogin.ShowConnecting(Show: Boolean);
begin
  if Show then
    prgConnecting.Style := pbstMarquee
  else
  begin
    prgConnecting.Style := pbstNormal;
    prgConnecting.Position := 0;
  end;

  pnlConnect.Visible := not Show;
  pnlConnecting.Visible := Show;

  btnOK.Enabled := not Show;
end;

end.
