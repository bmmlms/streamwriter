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

unit ConfigureSetTags;

interface

uses
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  LanguageObjects,
  MControls,
  PostProcess,
  StdCtrls,
  SysUtils,
  Variants;

type
  TfrmConfigureSetTags = class(TForm)
    txtArtist: TLabeledEdit;
    txtTitle: TLabeledEdit;
    Label1: TLabel;
    txtComment: TMemo;
    btnResetArtistPattern: TSpeedButton;
    btnResetTitlePattern: TSpeedButton;
    btnResetCommentPattern: TSpeedButton;
    lblPattern: TLabel;
    txtAlbum: TLabeledEdit;
    btnResetAlbumPattern: TSpeedButton;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    btnResetGenrePattern: TSpeedButton;
    txtGenre: TLabeledEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnResetPatternClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure txtCommentKeyPress(Sender: TObject; var Key: Char);
  private
    FSaveData: Boolean;

    FPostProcessor: TPostProcessBase;
    FArtist, FTitle, FAlbum, FGenre, FComment: string;
  public
    constructor Create(AOwner: TComponent; PostProcessor: TPostProcessBase; Artist, Title, Album, Genre, Comment: string); reintroduce;

    property Artist: string read FArtist;
    property Title: string read FTitle;
    property Album: string read FAlbum;
    property Genre: string read FGenre;
    property Comment: string read FComment;

    property SaveData: Boolean read FSaveData;
  end;

implementation

{$R *.lfm}

procedure TfrmConfigureSetTags.btnOKClick(Sender: TObject);
begin
  FArtist := Trim(txtArtist.Text);
  FTitle := Trim(txtTitle.Text);
  FAlbum := Trim(txtAlbum.Text);
  FGenre := Trim(txtGenre.Text);
  FComment := Trim(txtComment.Text);

  FSaveData := True;

  Close;
end;

procedure TfrmConfigureSetTags.btnResetPatternClick(Sender: TObject);
begin
  if Sender = btnResetArtistPattern then
  begin
    txtArtist.Text := '%artist%';
    txtArtist.ApplyFocus;
  end else if Sender = btnResetTitlePattern then
  begin
    txtTitle.Text := '%title%';
    txtTitle.ApplyFocus;
  end else if Sender = btnResetAlbumPattern then
  begin
    txtAlbum.Text := '%album%';
    txtAlbum.ApplyFocus;
  end else if Sender = btnResetCommentPattern then
  begin
    txtComment.Text := _('%streamname% / %streamtitle% / Recorded using streamWriter');
    txtComment.SelectAll;
    txtComment.ApplyFocus;
  end else if Sender = btnResetGenrePattern then
  begin
    txtGenre.Text := '%genre%';
    txtGenre.ApplyFocus;
  end;
end;

constructor TfrmConfigureSetTags.Create(AOwner: TComponent; PostProcessor: TPostProcessBase; Artist, Title, Album, Genre, Comment: string);
begin
  inherited Create(AOwner);

  FPostProcessor := PostProcessor;

  FArtist := Artist;
  FTitle := Title;
  FAlbum := Album;
  FGenre := Genre;
  FComment := Comment;

  txtArtist.Text := Artist;
  txtTitle.Text := Title;
  txtAlbum.Text := Album;
  txtGenre.Text := Genre;
  txtComment.Text := Comment;

  Language.Translate(Self);
end;

procedure TfrmConfigureSetTags.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmConfigureSetTags.FormShow(Sender: TObject);
begin
  Caption := Format(_('Configure "%s"'), [FPostProcessor.Name]);
end;

procedure TfrmConfigureSetTags.txtCommentKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    Key := #0;
end;

end.
