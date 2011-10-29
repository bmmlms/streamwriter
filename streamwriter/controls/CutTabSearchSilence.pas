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
unit CutTabSearchSilence;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, LanguageObjects, AppData, Functions;

type
  TfrmCutTabSearchSilence = class(TForm)
    txtSilenceLevel: TEdit;
    Label14: TLabel;
    Label12: TLabel;
    txtSilenceLength: TEdit;
    Label13: TLabel;
    Label10: TLabel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  public
    SilenceLevel: Integer;
    SilenceLength: Integer;
    Okay: Boolean;
  end;

implementation

{$R *.dfm}

procedure TfrmCutTabSearchSilence.btnOKClick(Sender: TObject);
begin
  if (StrToIntDef(txtSilenceLevel.Text, -1) > 100) or (StrToIntDef(txtSilenceLevel.Text, -1) < 1) then
  begin
    MsgBox(Handle, _('Please enter the maximum volume level for silence detection as a value ranging from 1 to 100.'), _('Info'), MB_ICONINFORMATION);
    txtSilenceLevel.SetFocus;
    Exit;
  end;

  if StrToIntDef(txtSilenceLength.Text, -1) < 20 then
  begin
    MsgBox(Handle, _('Please enter the minimum length for silence (at least 20 ms).'), _('Info'), MB_ICONINFORMATION);
    txtSilenceLength.SetFocus;
    Exit;
  end;

  SilenceLevel := StrToInt(txtSilenceLevel.Text);
  SilenceLength := StrToInt(txtSilenceLength.Text);
  Okay := True;

  Close;
end;

procedure TfrmCutTabSearchSilence.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  Okay := False;

  txtSilenceLength.Left := Label12.Left + Label12.Width + 4;
  Label13.Left := txtSilenceLength.Left + txtSilenceLength.Width + 4;

  txtSilenceLevel.Text := IntToStr(AppGlobals.StreamSettings.SilenceLevel);
  txtSilenceLength.Text := IntToStr(AppGlobals.StreamSettings.SilenceLength);
end;

procedure TfrmCutTabSearchSilence.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

end.
