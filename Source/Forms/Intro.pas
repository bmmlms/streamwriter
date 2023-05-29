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

unit Intro;

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
  LanguageObjects,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type

  { TfrmIntro }

  TfrmIntro = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lblAutomatic1: TLabel;
    lblAutomaticDesc1: TLabel;
    lblManual1: TLabel;
    lblManualDesc1: TLabel;
    lblTitle: TLabel;
    lblIntro: TLabel;
    pbLogo: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    procedure btnCloseClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure pbLogoPaint(Sender: TObject);
  private
  public
  end;

implementation

{$R *.lfm}

procedure TfrmIntro.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmIntro.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
  Constraints.MinHeight := Scale96ToFont(Constraints.MinHeight);
end;

procedure TfrmIntro.pbLogoPaint(Sender: TObject);
var
  TransparentRight, TransparentTop: Integer;
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.SetSize(96, 96);
    Icon.LoadFromResourceName(HINSTANCE, 'MAINICON');

    TFunctions.GetMaxTransparent(Icon, TransparentTop, TransparentRight);

    DrawIconEx(pbLogo.Canvas.Handle, Trunc(pbLogo.ClientWidth - (64 / 96) * TransparentRight), Trunc(-((64 / 96) * TransparentTop)), Icon.Handle, 64, 64, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

procedure TfrmIntro.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

end.
