unit SettingsAddPostProcessor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LanguageObjects, StdCtrls, Buttons, ExtCtrls;

type
  TfrmSettingsAddPostProcessor = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    Label1: TLabel;
    optWAVE: TRadioButton;
    optDestinationFormat: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FResult: Integer;
  public
    property Result: Integer read FResult;
  end;

implementation

{$R *.dfm}

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

end.
