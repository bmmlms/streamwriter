{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, LanguageObjects, StdCtrls, Buttons;

type
  TfrmIntro = class(TForm)
    imgLogo: TImage;
    lblTitle: TLabel;
    lblIntro: TLabel;
    lblManual: TLabel;
    lblManualDesc: TLabel;
    lblAutomatic: TLabel;
    lblAutomaticDesc: TLabel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    procedure btnCloseClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TfrmIntro.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmIntro.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);
end;

procedure TfrmIntro.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

end.
