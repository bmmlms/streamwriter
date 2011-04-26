unit StreamData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, PngSpeedButton, GUIFunctions,
  HomeCommunication, Functions, LanguageObjects, PerlRegEx;

type
  TfrmStreamData = class(TForm)
    txtTitlePattern: TLabeledEdit;
    optGood: TRadioButton;
    Label1: TLabel;
    optBad: TRadioButton;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    Label2: TLabel;
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    Label3: TLabel;
    btnResetTitlePattern: TPngSpeedButton;
    procedure btnResetTitlePatternClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure optGoodClick(Sender: TObject);
    procedure optBadClick(Sender: TObject);
    procedure txtTitlePatternChange(Sender: TObject);
  private
    FInitialized: Boolean;

    FID: Integer;
    FName: string;
    FRegEx: string;
    FIsOkay: Boolean;

    FIsOkayChanged: Boolean;
    FRegExChanged: Boolean;
  public
    constructor Create(AOwner: TComponent; ID: Integer; Name: string; RegEx: string; IsOkay: Boolean);

    property RecordingOkay: Boolean read FIsOkay;
    property RegEx: string read FRegEx;
    property RegExChanged: Boolean read FRegExChanged;
    property IsOkayChanged: Boolean read FIsOkayChanged;
  end;

implementation

{$R *.dfm}

procedure TfrmStreamData.btnOKClick(Sender: TObject);
var
  R: TPerlRegEx;
  RValid: Boolean;
begin
  if not HomeComm.Connected then
  begin
    MsgBox(Handle, _('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  RValid := False;
  R := TPerlRegEx.Create;
  try
    R.RegEx := txtTitlePattern.Text;
    try
      R.Compile;
    except end;
    RValid := True;
  finally
    R.Free;
  end;

  if (Trim(txtTitlePattern.Text) = '') or (Pos('(?P<a>.*)', txtTitlePattern.Text) = 0) or
    (Pos('(?P<t>.*)', txtTitlePattern.Text) = 0) or not RValid then
  begin
    MsgBox(Handle, 'TODO: !!!', 'TODO: !!!', MB_ICONINFORMATION);
    Exit;
  end;

  if ((optGood.Checked <> FIsOkay) and FIsOkayChanged) and ((FRegEx <> txtTitlePattern.Text) and FRegExChanged) then
  begin
    HomeComm.SetData(FID, optGood.Checked, txtTitlePattern.Text);
    FIsOkay := optGood.Checked;
    FRegEx := txtTitlePattern.Text;
  end else if (optGood.Checked <> FIsOkay) and (FIsOkayChanged) then
  begin
    HomeComm.SetData(FID, optGood.Checked);
    FIsOkay := optGood.Checked;
  end else if (txtTitlePattern.Text <> FRegEx) and (FRegExChanged) then
  begin
    HomeComm.SetData(FID, txtTitlePattern.Text);
    FRegEx := txtTitlePattern.Text;
  end;

  Close;
end;

procedure TfrmStreamData.btnResetTitlePatternClick(Sender: TObject);
begin
  txtTitlePattern.Text := '(?P<a>.*) - (?P<t>.*)';
end;

constructor TfrmStreamData.Create(AOwner: TComponent; ID: Integer; Name, RegEx: string;
  IsOkay: Boolean);
begin
  inherited Create(AOwner);

  FID := ID;
  FName := Name;             // TODO: Hier nen msgdlg zeigen, was das fenster eigentlich macht. nur für server, nix lokales, etcpp.
  FRegEx := RegEx;
  FIsOkay := IsOkay;

  optGood.Checked := FIsOkay;
  optBad.Checked := not FIsOkay;
  txtTitlePattern.Text := RegEx;
  if txtTitlePattern.Text = '' then
    txtTitlePattern.Text := '(?P<a>.*) - (?P<t>.*)';

  FInitialized := True;
end;

procedure TfrmStreamData.FormActivate(Sender: TObject);
begin
  lblTop.Caption := TruncateText(lblTop.Caption + ' ' + FName, lblTop.Width, lblTop.Font);
end;

procedure TfrmStreamData.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmStreamData.optBadClick(Sender: TObject);
begin
  if FInitialized then
    FIsOkayChanged := True;
end;

procedure TfrmStreamData.optGoodClick(Sender: TObject);
begin
  if FInitialized then
    FIsOkayChanged := True;
end;

procedure TfrmStreamData.txtTitlePatternChange(Sender: TObject);
begin
  if FInitialized then
    FRegExChanged := True;
end;

end.
