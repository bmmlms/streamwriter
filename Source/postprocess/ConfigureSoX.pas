{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  LanguageObjects,
  LCLType,
  Logging,
  PostProcessSoX,
  StdCtrls,
  SysUtils,
  Variants;

type
  TfrmConfigureSoX = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    pnlConfigure: TPanel;
    pnlSetup: TPanel;
    btnBrowse1: TSpeedButton;
    btnBrowse2: TSpeedButton;
    txtLameDLL: TLabeledEdit;
    txtMadDLL: TLabeledEdit;
    lblInfo: TLabel;
    dlgOpen: TOpenDialog;
    GroupBox1: TGroupBox;
    txtFadeoutStart: TLabeledEdit;
    chkFadeoutStart: TCheckBox;
    txtFadeoutEnd: TLabeledEdit;
    chkFadeoutEnd: TCheckBox;
    GroupBox2: TGroupBox;
    txtSilenceStart: TLabeledEdit;
    chkSilenceStart: TCheckBox;
    txtSilenceEnd: TLabeledEdit;
    chkSilenceEnd: TCheckBox;
    GroupBox3: TGroupBox;
    chkNormalize: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chkClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

    FOnlySetup: Boolean;
    FPostProcessor: TPostProcessSoX;
    FTitleLength: Cardinal;

    FSaveData: Boolean;

    procedure InitForm;
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

procedure TfrmConfigureSoX.btnBrowseClick(Sender: TObject);
begin
  if Sender = btnBrowse1 then
    dlgOpen.Filter := 'lame_enc.dll|*.dll'
  else
    dlgOpen.Filter := 'libmad.dll|*.dll';

  if dlgOpen.Execute then
    if FileExists(dlgOpen.FileName) then
      if Sender = btnBrowse1 then
        txtLameDLL.Text := dlgOpen.FileName
      else
        txtMadDLL.Text := dlgOpen.FileName;
end;

procedure TfrmConfigureSoX.btnOKClick(Sender: TObject);
begin
  {
  if pnlSetup.Visible then
  begin
    if (not FileExists(txtLameDLL.Text)) or (not FileExists(txtMadDLL.Text)) then
    begin
      TFunctions.MsgBox(_('Please browse for "lame_enc.dll" and "libmad.dll".'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if LowerCase(ExtractFileName(txtLameDLL.Text)) <> 'lame_enc.dll' then
    begin
      Res := TFunctions.MsgBox(_('The selected file for "lame_enc.dll" has a different filename.'#13#10'Are you really sure you want to use that file?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
      if Res = IDNO then
        Exit;
    end;

    if LowerCase(ExtractFileName(txtMadDLL.Text)) <> 'libmad.dll' then
    begin
      Res := TFunctions.MsgBox(_('The selected file for "libmad.dll" has a different filename.'#13#10'Are you really sure you want to use that file?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
      if Res = IDNO then
        Exit;
    end;

    if FPostProcessor.EatFiles(txtLameDLL.Text, txtMadDLL.Text) then
    begin
      if FOnlySetup then
      begin
        Close;
      end else
      begin
        pnlSetup.Visible := False;
        pnlConfigure.Visible := True;

        btnOK.Caption := '&OK';
      end;
    end else
    begin
      TFunctions.MsgBox(_('The selected files could not be included to the SoX-Addon. Please make sure the files are readable by streamWriter.'), _('Error'), MB_ICONERROR);
    end;
  end else
  begin
  }

  if chkFadeoutStart.Checked and (StrToIntDef(txtFadeoutStart.Text, 0) = 0) then
  begin
    TFunctions.MsgBox(_('Please enter the length of the fadein in seconds.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end else if (StrToIntDef(txtFadeoutStart.Text, 0) = 0) then
    txtFadeoutStart.Text := '5';

  if chkFadeoutEnd.Checked and (StrToIntDef(txtFadeoutEnd.Text, 0) = 0) then
  begin
    TFunctions.MsgBox(_('Please enter the length of the fadeout in seconds.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end else if (StrToIntDef(txtFadeoutEnd.Text, 0) = 0) then
    txtFadeoutEnd.Text := '5';

  if chkSilenceStart.Checked and (StrToIntDef(txtSilenceStart.Text, 0) = 0) then
  begin
    TFunctions.MsgBox(_('Please enter the length of silence at the beginning in seconds.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end else if (StrToIntDef(txtSilenceStart.Text, 0) = 0) then
    txtSilenceStart.Text := '5';

  if chkSilenceEnd.Checked and (StrToIntDef(txtSilenceEnd.Text, 0) = 0) then
  begin
    TFunctions.MsgBox(_('Please enter the length of silence at the end in seconds.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end else if (StrToIntDef(txtSilenceEnd.Text, 0) = 0) then
    txtSilenceEnd.Text := '5';


  if FTitleLength > 0 then
  begin
    if chkFadeoutStart.Checked and (StrToInt(txtFadeoutStart.Text) > FTitleLength / 2) then
    begin
      TFunctions.MsgBox(Format(_('The length for fadeout cannot be greater than half the length of the song (%d seconds).'), [FTitleLength div 2]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if chkFadeoutEnd.Checked and (StrToInt(txtFadeoutEnd.Text) > FTitleLength / 2) then
    begin
      TFunctions.MsgBox(Format(_('The length for fadein cannot be greater than half the length of the song (%d seconds).'), [FTitleLength div 2]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if chkSilenceStart.Checked and (StrToInt(txtSilenceStart.Text) > FTitleLength) then
    begin
      TFunctions.MsgBox(Format(_('The length for silence at the beginning cannot be greater than the length of the song (%d seconds).'), [FTitleLength]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    if chkFadeoutEnd.Checked and (StrToInt(txtFadeoutEnd.Text) > FTitleLength) then
    begin
      TFunctions.MsgBox(Format(_('The length for silence at the end cannot be greater than the length of the song (%d seconds).'), [FTitleLength]), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;
  end;


  FNormalize := chkNormalize.Checked;

  FFadeoutStart := chkFadeoutStart.Checked;
  FFadeoutEnd := chkFadeoutEnd.Checked;
  FFadeoutStartLength := StrToInt(txtFadeoutStart.Text);
  FFadeoutEndLength := StrToInt(txtFadeoutEnd.Text);

  FSilenceStart := chkSilenceStart.Checked;
  FSilenceEnd := chkSilenceEnd.Checked;
  FSilenceStartLength := StrToInt(txtSilenceStart.Text);
  FSilenceEndLength := StrToInt(txtSilenceEnd.Text);

  FSaveData := True;

  Close;
end;

procedure TfrmConfigureSoX.chkClick(Sender: TObject);
begin
  txtFadeoutStart.Enabled := chkFadeoutStart.Checked;
  txtFadeoutEnd.Enabled := chkFadeoutEnd.Checked;
  txtSilenceStart.Enabled := chkSilenceStart.Checked;
  txtSilenceEnd.Enabled := chkSilenceEnd.Checked;
end;

constructor TfrmConfigureSoX.Create(AOwner: TComponent; PostProcessor: TPostProcessSoX; TitleLength: Cardinal);
begin
  inherited Create(AOwner);

  InitForm;

  FPostProcessor := PostProcessor;
  FTitleLength := TitleLength;

  pnlSetup.Show;
  btnOK.Caption := _('&OK');
  FOnlySetup := True;

  Language.Translate(Self);
end;

constructor TfrmConfigureSoX.Create(AOwner: TComponent; PostProcessor: TPostProcessSoX; Normalize, FadeoutStart, FadeoutEnd: Boolean; FadeoutStartLength, FadeoutEndLength: Integer;
  SilenceStart, SilenceEnd: Boolean; SilenceStartLength, SilenceEndLength: Integer; TitleLength: Cardinal);
begin
  inherited Create(AOwner);

  InitForm;

  FPostProcessor := PostProcessor;
  FTitleLength := TitleLength;

  pnlConfigure.Show;
  btnOK.Caption := _('&OK');

  chkNormalize.Checked := Normalize;

  chkFadeoutStart.Checked := FadeoutStart;
  chkFadeoutEnd.Checked := FadeoutEnd;
  txtFadeoutStart.Text := IntToStr(FadeoutStartLength);
  txtFadeoutEnd.Text := IntToStr(FadeoutEndLength);

  chkSilenceStart.Checked := SilenceStart;
  chkSilenceEnd.Checked := SilenceEnd;
  txtSilenceStart.Text := IntToStr(SilenceStartLength);
  txtSilenceEnd.Text := IntToStr(SilenceEndLength);

  Language.Translate(Self);
end;

procedure TfrmConfigureSoX.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmConfigureSoX.FormShow(Sender: TObject);
begin
  Caption := Format(_('Configure "%s"'), [FPostProcessor.Name]);
end;

procedure TfrmConfigureSoX.InitForm;
begin
  //  btnBrowse1.Image := modSharedData.imgImages.Images[85].Image;
  //  btnBrowse2.Image := modSharedData.imgImages.Images[85].Image;
end;

end.
