{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2024 Alexander Nottelmann

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
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  LanguageObjects,
  LCLType,
  Logging,
  MLabeledEdit,
  PostProcessSoX,
  StdCtrls,
  SysUtils,
  Variants;

type

  { TfrmConfigureSoX }

  TfrmConfigureSoX = class(TForm)
    chkFadeoutEnd: TCheckBox;
    chkFadeoutStart: TCheckBox;
    chkNormalize: TCheckBox;
    chkSilenceEnd: TCheckBox;
    chkSilenceStart: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    txtFadeinStart: TMLabeledSpinEdit;
    txtFadeoutEnd: TMLabeledSpinEdit;
    txtSilenceEnd: TMLabeledSpinEdit;
    txtSilenceStart: TMLabeledSpinEdit;
    procedure btnOKClick(Sender: TObject);
    procedure chkChange(Sender: TObject);
  private
    FNormalize: Boolean;
    FFadeoutStart: Boolean;
    FFadeoutEnd: Boolean;
    FFadeoutStartLength: Integer;
    FFadeoutEndLength: Integer;
    FSilenceStart: Boolean;
    FSilenceEnd: Boolean;
    FSilenceStartLength: Integer;
    FSilenceEndLength: Integer;

    FPostProcessor: TPostProcessSoX;
    FTitleLength: Cardinal;

    FSaveData: Boolean;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent; PostProcessor: TPostProcessSoX; TitleLength: Cardinal); reintroduce; overload;
    constructor Create(AOwner: TComponent; PostProcessor: TPostProcessSoX; Normalize, FadeoutStart, FadeoutEnd: Boolean; FadeoutStartLength, FadeoutEndLength: Integer;
      SilenceStart, SilenceEnd: Boolean; SilenceStartLength, SilenceEndLength: Integer; TitleLength: Cardinal); reintroduce; overload;

    property Normalize: Boolean read FNormalize write FNormalize;
    property FadeoutStart: Boolean read FFadeoutStart write FFadeoutStart;
    property FadeoutEnd: Boolean read FFadeoutEnd write FFadeoutEnd;
    property FadeoutStartLength: Integer read FFadeoutStartLength write FFadeoutStartLength;
    property FadeoutEndLength: Integer read FFadeoutEndLength write FFadeoutEndLength;
    property SilenceStart: Boolean read FSilenceStart write FSilenceStart;
    property SilenceEnd: Boolean read FSilenceEnd write FSilenceEnd;
    property SilenceStartLength: Integer read FSilenceStartLength write FSilenceStartLength;
    property SilenceEndLength: Integer read FSilenceEndLength write FSilenceEndLength;

    property SaveData: Boolean read FSaveData;
  end;

implementation

{$R *.lfm}

procedure TfrmConfigureSoX.btnOKClick(Sender: TObject);
begin
  if FTitleLength > 0 then
  begin
    if chkFadeoutStart.Checked and (txtFadeinStart.Control.Value > FTitleLength / 2) then
    begin
      TFunctions.MsgBox(Format(_('The length for fadeout cannot be greater than half the length of the song (%d seconds).'), [FTitleLength div 2]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if chkFadeoutEnd.Checked and (txtFadeoutEnd.Control.Value > FTitleLength / 2) then
    begin
      TFunctions.MsgBox(Format(_('The length for fadein cannot be greater than half the length of the song (%d seconds).'), [FTitleLength div 2]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if chkSilenceStart.Checked and (txtSilenceStart.Control.Value > FTitleLength) then
    begin
      TFunctions.MsgBox(Format(_('The length for silence at the beginning cannot be greater than the length of the song (%d seconds).'), [FTitleLength]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if chkFadeoutEnd.Checked and (txtFadeoutEnd.Control.Value > FTitleLength) then
    begin
      TFunctions.MsgBox(Format(_('The length for silence at the end cannot be greater than the length of the song (%d seconds).'), [FTitleLength]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;
  end;

  FNormalize := chkNormalize.Checked;

  FFadeoutStart := chkFadeoutStart.Checked;
  FFadeoutEnd := chkFadeoutEnd.Checked;
  FFadeoutStartLength := txtFadeinStart.Control.Value;
  FFadeoutEndLength := txtFadeoutEnd.Control.Value;

  FSilenceStart := chkSilenceStart.Checked;
  FSilenceEnd := chkSilenceEnd.Checked;
  FSilenceStartLength := txtSilenceStart.Control.Value;
  FSilenceEndLength := txtSilenceEnd.Control.Value;

  FSaveData := True;

  Close;
end;

procedure TfrmConfigureSoX.chkChange(Sender: TObject);
begin
  txtFadeinStart.Enabled := chkFadeoutStart.Checked;
  txtFadeoutEnd.Enabled := chkFadeoutEnd.Checked;
  txtSilenceStart.Enabled := chkSilenceStart.Checked;
  txtSilenceEnd.Enabled := chkSilenceEnd.Checked;
end;

procedure TfrmConfigureSoX.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;

  inherited;
end;

procedure TfrmConfigureSoX.DoShow;
begin
  inherited;

  Caption := Format(_('Configure "%s"'), [FPostProcessor.Name]);

  chkChange(nil);
end;

constructor TfrmConfigureSoX.Create(AOwner: TComponent; PostProcessor: TPostProcessSoX; TitleLength: Cardinal);
begin
  inherited Create(AOwner);

  FPostProcessor := PostProcessor;
  FTitleLength := TitleLength;

  btnOK.Caption := _('&OK');

  Language.Translate(Self);
end;

constructor TfrmConfigureSoX.Create(AOwner: TComponent; PostProcessor: TPostProcessSoX; Normalize, FadeoutStart, FadeoutEnd: Boolean; FadeoutStartLength, FadeoutEndLength: Integer;
  SilenceStart, SilenceEnd: Boolean; SilenceStartLength, SilenceEndLength: Integer; TitleLength: Cardinal);
begin
  Create(AOwner, PostProcessor, TitleLength);

  chkNormalize.Checked := Normalize;

  chkFadeoutStart.Checked := FadeoutStart;
  chkFadeoutEnd.Checked := FadeoutEnd;
  txtFadeinStart.Control.Value := FadeoutStartLength;
  txtFadeoutEnd.Control.Value := FadeoutEndLength;

  chkSilenceStart.Checked := SilenceStart;
  chkSilenceEnd.Checked := SilenceEnd;
  txtSilenceStart.Control.Value := SilenceStartLength;
  txtSilenceEnd.Control.Value := SilenceEndLength;
end;

end.
