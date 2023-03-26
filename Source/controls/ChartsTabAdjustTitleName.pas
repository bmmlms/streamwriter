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

unit ChartsTabAdjustTitleName;

interface

uses
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  Images,
  LanguageObjects,
  MControlFocuser,
  MControls,
  MLabeledEdit,
  MsgDlg,
  SharedData,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type

  { TfrmChartsTabAdjustTitleName }

  TfrmChartsTabAdjustTitleName = class(TForm)
    txtTitle: TMLabeledEdit;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    TitleName: string;
    Okay: Boolean;

    constructor Create(AOwner: TComponent; Title: string); reintroduce;
  end;

implementation

{$R *.lfm}

procedure TfrmChartsTabAdjustTitleName.btnOKClick(Sender: TObject);
var
  NumChars: Integer;
  Hash: Cardinal;
begin
  TFunctions.BuildPattern(txtTitle.Control.Text, Hash, NumChars, True);

  if NumChars = 0 then
  begin
    TFunctions.MsgBox(_('Please enter a pattern to add to list.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if NumChars <= 3 then
    TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('A short pattern may produce many matches, i.e. using ''a'' records/ignores every song containing an ''a''.'), mtInformation, [mbOK], mbOK, 6);

  TitleName := Trim(txtTitle.Control.Text);
  Okay := True;

  Close;
end;

procedure TfrmChartsTabAdjustTitleName.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  Okay := False;
end;

procedure TfrmChartsTabAdjustTitleName.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;

  inherited;
end;

procedure TfrmChartsTabAdjustTitleName.FormShow(Sender: TObject);
begin
  txtTitle.Control.ApplyFocus;
end;

constructor TfrmChartsTabAdjustTitleName.Create(AOwner: TComponent; Title: string);
begin
  inherited Create(AOwner);

  modSharedData.imgImages.GetIcon(TImages.TEXTFIELD_RENAME, Icon);

  txtTitle.Control.Text := Title;
end;

end.
