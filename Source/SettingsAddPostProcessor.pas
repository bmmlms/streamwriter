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

unit SettingsAddPostProcessor;

interface

uses
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Images,
  LanguageObjects,
  SharedData,
  StdCtrls,
  SysUtils,
  Variants;

type

  { TfrmSettingsAddPostProcessor }

  TfrmSettingsAddPostProcessor = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    Label1: TLabel;
    optWAVE: TRadioButton;
    optDestinationFormat: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FResult: Integer;
  public
    property Result: Integer read FResult;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

procedure TfrmSettingsAddPostProcessor.btnOKClick(Sender: TObject);
begin
  if optWAVE.Checked then
    FResult := 0
  else if optDestinationFormat.Checked then
    FResult := 1;
  Close;
end;

procedure TfrmSettingsAddPostProcessor.FormCreate(Sender: TObject);
begin
  FResult := 2;

  Language.Translate(Self);
end;

procedure TfrmSettingsAddPostProcessor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

constructor TfrmSettingsAddPostProcessor.Create(AOwner: TComponent);
begin
  inherited;

  modSharedData.imgImages.GetIcon(TImages.LIGHTNING, Icon);
end;

end.
