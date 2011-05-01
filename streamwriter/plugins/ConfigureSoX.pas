{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit ConfigureSoX;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, LanguageObjects, Functions;

type
  TfrmConfigureSoX = class(TForm)
    chkFadeoutStart: TCheckBox;
    chkFadeoutEnd: TCheckBox;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    txtFadeoutStart: TLabeledEdit;
    txtFadeoutEnd: TLabeledEdit;
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkFadeoutClick(Sender: TObject);
  private
    FFadeoutStart: Boolean;
    FFadeoutEnd: Boolean;
    FFadeoutStartLength: Integer;
    FFadeoutEndLength: Integer;

    FSaveData: Boolean;
  public
    constructor Create(AOwner: TComponent; FadeoutStart, FadeoutEnd: Boolean; FadeoutStartLength, FadeoutEndLength: Integer);

    property FadeoutStart: Boolean read FFadeoutStart write FFadeoutStart;
    property FadeoutEnd: Boolean read FFadeoutEnd write FFadeoutEnd;
    property FadeoutStartLength: Integer read FFadeoutStartLength write FFadeoutStartLength;
    property FadeoutEndLength: Integer read FFadeoutEndLength write FFadeoutEndLength;
    property SaveData: Boolean read FSaveData;
  end;

implementation

{$R *.dfm}

procedure TfrmConfigureSoX.btnOKClick(Sender: TObject);
begin
  if chkFadeoutStart.Checked and (StrToIntDef(txtFadeoutStart.Text, 0) = 0) then
  begin
    MsgBox(Handle, _('Please enter the length of the fadein in seconds.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end else if (StrToIntDef(txtFadeoutStart.Text, 0) = 0) then
    txtFadeoutStart.Text := '5';

  if chkFadeoutEnd.Checked and (StrToIntDef(txtFadeoutEnd.Text, 0) = 0) then
  begin
    MsgBox(Handle, _('Please enter the length of the fadeout in seconds.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end else if (StrToIntDef(txtFadeoutEnd.Text, 0) = 0) then
    txtFadeoutEnd.Text := '5';

  FFadeoutStart := chkFadeoutStart.Checked;
  FFadeoutEnd := chkFadeoutEnd.Checked;
  FFadeoutStartLength := StrToInt(txtFadeoutStart.Text);
  FFadeoutEndLength := StrToInt(txtFadeoutEnd.Text);

  FSaveData := True;

  Close;
end;

procedure TfrmConfigureSoX.chkFadeoutClick(Sender: TObject);
begin
  txtFadeoutStart.Enabled := chkFadeoutStart.Checked;
  txtFadeoutEnd.Enabled := chkFadeoutEnd.Checked;
end;

constructor TfrmConfigureSoX.Create(AOwner: TComponent; FadeoutStart, FadeoutEnd: Boolean; FadeoutStartLength, FadeoutEndLength: Integer);
begin
  inherited Create(AOwner);

  chkFadeoutStart.Checked := FadeoutStart;
  chkFadeoutEnd.Checked := FadeoutEnd;
  txtFadeoutStart.Text := IntToStr(FadeoutStartLength);
  txtFadeoutEnd.Text := IntToStr(FadeoutEndLength);

  Language.Translate(Self);
end;

procedure TfrmConfigureSoX.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

end.
