{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

    Portions created by Ralf Kruse

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
unit StreamData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, PngSpeedButton, GUIFunctions,
  HomeCommunication, Functions, LanguageObjects, PerlRegEx, Logging;

type
  TfrmStreamData = class(TForm)
    txtTitlePattern: TLabeledEdit;
    optGood: TRadioButton;
    Label1: TLabel;
    optBad: TRadioButton;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    Label2: TLabel;
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    Label3: TLabel;
    btnResetTitlePattern: TPngSpeedButton;
    procedure btnResetTitlePatternClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure optGoodClick(Sender: TObject);
    procedure optBadClick(Sender: TObject);
    procedure txtTitlePatternChange(Sender: TObject);
  private
    FInitialized: Boolean;

    FID: Integer;
    FName: string;
    FRegEx: string;
    FIsOkay: Boolean;

    FIsOkayChanged: Boolean;
    FRegExChanged: Boolean;
  public
    constructor Create(AOwner: TComponent; ID: Integer; Name: string; RegEx: string; IsOkay: Boolean);

    property RecordingOkay: Boolean read FIsOkay;
    property RegEx: string read FRegEx;
    property RegExChanged: Boolean read FRegExChanged;
    property IsOkayChanged: Boolean read FIsOkayChanged;
  end;

implementation

{$R *.dfm}

procedure TfrmStreamData.btnOKClick(Sender: TObject);
var
  R: TPerlRegEx;
  RValid: Boolean;
begin
  if not HomeComm.Connected then
  begin
    MsgBox(Handle, _('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  RValid := False;
  R := TPerlRegEx.Create;
  try
    R.RegEx := txtTitlePattern.Text;
    try
      R.Compile;
    except end;
    RValid := True;
  finally
    R.Free;
  end;

  if (Trim(txtTitlePattern.Text) = '') or (Pos('(?P<a>.*)', txtTitlePattern.Text) = 0) or
    (Pos('(?P<t>.*)', txtTitlePattern.Text) = 0) or not RValid then
  begin
    MsgBox(Handle, _('Please supply a valid regular expression containing the groups (?P<a>.*) and (?P<t>.*). If you don''t have a clue about regular expressions, click the button next to the text field to reset the pattern.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if ((optGood.Checked <> FIsOkay) and FIsOkayChanged) and ((FRegEx <> txtTitlePattern.Text) and FRegExChanged) then
  begin
    HomeComm.SetData(FID, optGood.Checked, txtTitlePattern.Text);
    FIsOkay := optGood.Checked;
    FRegEx := txtTitlePattern.Text;
  end else if (optGood.Checked <> FIsOkay) and (FIsOkayChanged) then
  begin
    HomeComm.SetData(FID, optGood.Checked);
    FIsOkay := optGood.Checked;
  end else if (txtTitlePattern.Text <> FRegEx) and (FRegExChanged) then
  begin
    HomeComm.SetData(FID, txtTitlePattern.Text);
    FRegEx := txtTitlePattern.Text;
  end;

  Close;
end;

procedure TfrmStreamData.btnResetTitlePatternClick(Sender: TObject);
begin
  txtTitlePattern.Text := '(?P<a>.*) - (?P<t>.*)';
end;

constructor TfrmStreamData.Create(AOwner: TComponent; ID: Integer; Name, RegEx: string;
  IsOkay: Boolean);
begin
  inherited Create(AOwner);

  FID := ID;
  FName := Name;
  FRegEx := RegEx;
  FIsOkay := IsOkay;

  optGood.Checked := FIsOkay;
  optBad.Checked := not FIsOkay;
  txtTitlePattern.Text := RegEx;
  if txtTitlePattern.Text = '' then
    txtTitlePattern.Text := '(?P<a>.*) - (?P<t>.*)';

  FInitialized := True;

  Language.Translate(Self);
end;

procedure TfrmStreamData.FormActivate(Sender: TObject);
begin
  lblTop.Caption := TruncateText(lblTop.Caption + ' ' + FName, lblTop.Width, lblTop.Font);
end;

procedure TfrmStreamData.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmStreamData.optBadClick(Sender: TObject);
begin
  if FInitialized then
    FIsOkayChanged := True;
end;

procedure TfrmStreamData.optGoodClick(Sender: TObject);
begin
  if FInitialized then
    FIsOkayChanged := True;
end;

procedure TfrmStreamData.txtTitlePatternChange(Sender: TObject);
begin
  if FInitialized then
    FRegExChanged := True;
end;

end.
