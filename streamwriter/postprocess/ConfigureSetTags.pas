unit ConfigureSetTags;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, LanguageObjects, PostProcess,
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
    txtAlbum: TLabeledEdit;
    btnResetAlbumPattern: TPngSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnResetPatternClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FSaveData: Boolean;

    FPlugin: TPostProcessBase;
    FArtist, FTitle, FAlbum, FComment: string;
  public
    constructor Create(AOwner: TComponent; Plugin: TPostProcessBase; Artist, Title, Album, Comment: string); overload;

    property Artist: string read FArtist;
    property Title: string read FTitle;
    property Album: string read FAlbum;
    property Comment: string read FComment;

    property SaveData: Boolean read FSaveData;
  end;

implementation

{$R *.dfm}

procedure TfrmConfigureSetTags.btnOKClick(Sender: TObject);
begin
  FArtist := Trim(txtArtist.Text);
  FTitle := Trim(txtTitle.Text);
  FAlbum := Trim(txtAlbum.Text);
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
  end else if Sender = btnResetAlbumPattern then
  begin
    txtTitle.Text := '%l';
    txtTitle.SetFocus;
  end else if Sender = btnResetCommentPattern then
  begin
    txtComment.Text := '%s / %u / Recorded using streamWriter';
    txtComment.SelectAll;
    txtComment.SetFocus;
  end;
end;

constructor TfrmConfigureSetTags.Create(AOwner: TComponent;
  Plugin: TPostProcessBase; Artist, Title, Album, Comment: string);
begin
  inherited Create(AOwner);

  FPlugin := Plugin;

  FArtist := Artist;
  FTitle := Title;
  FAlbum := Album;
  FComment := Comment;

  txtArtist.Text := Artist;
  txtTitle.Text := Title;
  txtAlbum.Text := Album;
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
