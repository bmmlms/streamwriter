unit ConfigureSetTags;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, LanguageObjects, SetTags,
  PngSpeedButton;

type
  TfrmConfigureSetTags = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    txtArtist: TLabeledEdit;
    txtTitle: TLabeledEdit;
    Label1: TLabel;
    txtComment: TMemo;
    btnResetArtistPattern: TPngSpeedButton;
    btnResetTitlePattern: TPngSpeedButton;
    btnResetCommentPattern: TPngSpeedButton;
    lblPattern: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnResetPatternClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
  FArtist := Trim(txtArtist.Text);
  FTitle := Trim(txtTitle.Text);
  FComment := Trim(txtComment.Text);

  FSaveData := True;

  Close;
end;

procedure TfrmConfigureSetTags.btnResetPatternClick(Sender: TObject);
begin
  if Sender = btnResetArtistPattern then
  begin
    txtArtist.Text := '%a';
    txtArtist.SetFocus;
  end else if Sender = btnResetTitlePattern then
  begin
    txtTitle.Text := '%t';
    txtTitle.SetFocus;
  end else if Sender = btnResetCommentPattern then
  begin
    txtComment.Text := '%s / %u / Recorded using streamWriter';
    txtComment.SelectAll;
    txtComment.SetFocus;
  end;
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

procedure TfrmConfigureSetTags.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmConfigureSetTags.FormShow(Sender: TObject);
begin
  Caption := Format(_('Configure "%s"'), [FPlugin.Name]);
end;

end.
