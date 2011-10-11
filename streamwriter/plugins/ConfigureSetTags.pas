unit ConfigureSetTags;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, LanguageObjects, SetTags;

type
  TfrmConfigureSetTags = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    txtArtist: TLabeledEdit;
    txtTitle: TLabeledEdit;
    Label1: TLabel;
    txtComment: TMemo;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSaveData: Boolean;

    FPlugin: TSetTagsPlugin;
    FArtist, FTitle, FComment: string;
  public
    constructor Create(AOwner: TComponent; Plugin: TSetTagsPlugin; Artist, Title, Comment: string); overload;

    property Artist: string read FArtist;
    property Title: string read FTitle;
    property Comment: string read FComment;

    property SaveData: Boolean read FSaveData;
  end;

implementation

{$R *.dfm}

procedure TfrmConfigureSetTags.btnOKClick(Sender: TObject);
var
  Res: Integer;
begin
  if Trim(txtArtist.Text) = '' then
  begin
    // TODO: !!!
    Exit;
  end else
    FArtist := Trim(txtArtist.Text);

  if Trim(txtTitle.Text) = '' then
  begin
    // TODO: !!!
    Exit;
  end else
    FTitle := Trim(txtTitle.Text);

  if Trim(txtComment.Text) = '' then
  begin
    // TODO: !!!
    Exit;
  end else
    FComment := Trim(txtComment.Text);

  FArtist := txtArtist.Text;
  FTitle := txtTitle.Text;
  FComment := txtComment.Text;

  FSaveData := True;

  Close;
end;

constructor TfrmConfigureSetTags.Create(AOwner: TComponent;
  Plugin: TSetTagsPlugin; Artist, Title, Comment: string);
begin
  inherited Create(AOwner);

  FPlugin := Plugin;

  FArtist := Artist;
  FTitle := Title;
  FComment := Comment;

  txtArtist.Text := Artist;
  txtTitle.Text := Title;
  txtComment.Text := Comment;

  Language.Translate(Self);
end;

procedure TfrmConfigureSetTags.FormCreate(Sender: TObject);
begin
  // TODO: Hier fehlen noch captions (oben!!!!) und msgdlg texte....
end;

procedure TfrmConfigureSetTags.FormShow(Sender: TObject);
begin
  Caption := Format(_('Configure "%s"'), [FPlugin.Name]);
end;

end.
