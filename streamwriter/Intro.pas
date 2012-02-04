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
  end;       // TODO: zu nem stream verbinden. und es klappt nicht (timeout) - die log message ist englisch....

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
