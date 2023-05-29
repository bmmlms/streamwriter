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

unit ConfigureEncoder;

interface

uses
  AppData,
  AudioFunctions,
  Buttons,
  Classes,
  ComboEx,
  Controls,
  DataManager,
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
  TfrmConfigureEncoder = class(TForm)
    optCBR: TRadioButton;
    optVBR: TRadioButton;
    lstCBR: TComboBoxEx;
    lstVBR: TComboBoxEx;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure optCBRClick(Sender: TObject);
    procedure optVBRClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FEncoderSettings: TEncoderSettings;
    FSave: Boolean;
  public
    constructor Create(AOwner: TComponent; EncoderSettings: TEncoderSettings); reintroduce;

    property EncoderSettings: TEncoderSettings read FEncoderSettings;
    property Save: Boolean read FSave;
  end;

implementation

{$R *.lfm}

procedure TfrmConfigureEncoder.btnOKClick(Sender: TObject);
begin
  FSave := True;

  if optCBR.Checked then
    FEncoderSettings.BitrateType := brCBR
  else
    FEncoderSettings.BitrateType := brVBR;
  FEncoderSettings.CBRBitrate := StrToInt(lstCBR.ItemsEx[lstCBR.ItemIndex].Caption);
  FEncoderSettings.VBRQuality := TVBRQualities(lstVBR.ItemIndex);

  Close;
end;

constructor TfrmConfigureEncoder.Create(AOwner: TComponent; EncoderSettings: TEncoderSettings);
var
  Item: TCollectionItem;
begin
  inherited Create(AOwner);

  modSharedData.imgImages.GetIcon(TImages.COG_GO, Icon);

  FEncoderSettings := EncoderSettings;

  lstCBR.ItemsEx.AddItem('320');
  lstCBR.ItemsEx.AddItem('256');
  lstCBR.ItemsEx.AddItem('224');
  lstCBR.ItemsEx.AddItem('192');
  lstCBR.ItemsEx.AddItem('160');
  lstCBR.ItemsEx.AddItem('128');
  lstCBR.ItemsEx.AddItem('96');
  lstCBR.ItemsEx.AddItem('64');
  lstCBR.ItemsEx.AddItem('32');

  lstVBR.ItemsEx.AddItem(_('High quality'));
  lstVBR.ItemsEx.AddItem(_('Medium quality'));
  lstVBR.ItemsEx.AddItem(_('Low quality'));

  optCBR.Checked := FEncoderSettings.BitrateType = brCBR;
  optVBR.Checked := FEncoderSettings.BitrateType = brVBR;

  for Item in lstCBR.ItemsEx do
    if TComboExItem(Item).Caption = EncoderSettings.CBRBitrate.ToString then
    begin
      lstCBR.ItemIndex := Item.Index;
      Break;
    end;

  lstVBR.ItemIndex := Integer(EncoderSettings.VBRQuality);

  lstCBR.Enabled := optCBR.Checked;
  lstVBR.Enabled := optVBR.Checked;

  Language.Translate(Self);

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
end;

procedure TfrmConfigureEncoder.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
