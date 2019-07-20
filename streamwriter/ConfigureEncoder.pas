{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2019 Alexander Nottelmann

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

unit ConfigureEncoder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, AppData, LanguageObjects,
  AudioFunctions, DataManager;

type
  TfrmConfigureEncoder = class(TForm)
    optCBR: TRadioButton;
    optVBR: TRadioButton;
    lstCBR: TComboBox;
    lstVBR: TComboBox;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure optCBRClick(Sender: TObject);
    procedure optVBRClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FEncoderSettings: TEncoderSettings;
    FSave: Boolean;
  public
    constructor Create(AOwner: TComponent; EncoderSettings: TEncoderSettings); reintroduce;

    property EncoderSettings: TEncoderSettings read FEncoderSettings;
    property Save: Boolean read FSave;
  end;

implementation

{$R *.dfm}

procedure TfrmConfigureEncoder.btnOKClick(Sender: TObject);
begin
  FSave := True;

  if optCBR.Checked then
    FEncoderSettings.BitrateType := brCBR
  else
    FEncoderSettings.BitrateType := brVBR;
  FEncoderSettings.CBRBitrate := StrToInt(lstCBR.Text);
  FEncoderSettings.VBRQuality := TVBRQualities(lstVBR.ItemIndex);

  Close;
end;

constructor TfrmConfigureEncoder.Create(AOwner: TComponent; EncoderSettings: TEncoderSettings);
begin
  inherited Create(AOwner);

  FEncoderSettings := EncoderSettings;

  lstCBR.Items.Add('320');
  lstCBR.Items.Add('256');
  lstCBR.Items.Add('224');
  lstCBR.Items.Add('192');
  lstCBR.Items.Add('160');
  lstCBR.Items.Add('128');
  lstCBR.Items.Add('96');
  lstCBR.Items.Add('64');
  lstCBR.Items.Add('32');

  lstVBR.Items.Add(_('High quality'));
  lstVBR.Items.Add(_('Medium quality'));
  lstVBR.Items.Add(_('Low quality'));

  optCBR.Checked := FEncoderSettings.BitrateType = brCBR;
  optVBR.Checked := FEncoderSettings.BitrateType = brVBR;

  lstCBR.ItemIndex := lstCBR.Items.IndexOf(IntToStr(EncoderSettings.CBRBitrate));
  lstVBR.ItemIndex := Integer(EncoderSettings.VBRQuality);

  lstCBR.Enabled := optCBR.Checked;
  lstVBR.Enabled := optVBR.Checked;

  Language.Translate(Self);
end;

procedure TfrmConfigureEncoder.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmConfigureEncoder.optCBRClick(Sender: TObject);
begin
  lstCBR.Enabled := True;
  lstVBR.Enabled := False;
end;

procedure TfrmConfigureEncoder.optVBRClick(Sender: TObject);
begin
  lstCBR.Enabled := False;
  lstVBR.Enabled := True;
end;

end.
