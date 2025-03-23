{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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
  ExtCtrls,
  Forms,
  Graphics,
  LanguageObjects,
  MControlFocuser,
  MControls,
  MLabeledEdit,
  MSpeedButton,
  PostProcess,
  StdCtrls,
  SysUtils,
  Windows;

type

  { TfrmConfigureSetTags }

  TfrmConfigureSetTags = class(TForm)
    btnResetCommentPattern: TMSpeedButton;
    Label1: TLabel;
    Panel1: TPanel;
    txtComment: TMemo;
    txtGenre: TMLabeledEditButton;
    txtArtist: TMLabeledEditButton;
    txtTitle: TMLabeledEditButton;
    lblPattern: TLabel;
    btnResetAlbumPattern: TMSpeedButton;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    txtAlbum: TMLabeledEditButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnResetPatternClick(Sender: TObject);
    procedure txtCommentKeyPress(Sender: TObject; var Key: Char);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
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
  FArtist := Trim(txtArtist.Control.Text);
  FTitle := Trim(txtTitle.Control.Text);
  FAlbum := Trim(txtAlbum.Control.Text);
  FGenre := Trim(txtGenre.Control.Text);
  FComment := Trim(txtComment.Text);

  FSaveData := True;

  Close;
end;

procedure TfrmConfigureSetTags.btnResetPatternClick(Sender: TObject);
begin
  if Sender = txtArtist.Control then
  begin
    txtArtist.Control.Text := '%artist%';
    txtArtist.Control.ApplyFocus;
  end else if Sender = txtTitle.Control then
  begin
    txtTitle.Control.Text := '%title%';
    txtTitle.Control.ApplyFocus;
  end else if Sender = txtAlbum.Control then
  begin
    txtAlbum.Control.Text := '%album%';
    txtAlbum.Control.ApplyFocus;
  end else if Sender = btnResetCommentPattern then
  begin
    txtComment.Text := _('%streamname% / %streamtitle% / Recorded using streamWriter');
    txtComment.SelectAll;
    txtComment.ApplyFocus;
  end else if Sender = txtGenre.Control then
  begin
    txtGenre.Control.Text := '%genre%';
    txtGenre.Control.ApplyFocus;
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

  txtArtist.Control.Text := Artist;
  txtTitle.Control.Text := Title;
  txtAlbum.Control.Text := Album;
  txtGenre.Control.Text := Genre;
  txtComment.Text := Comment;

  Language.Translate(Self);

  Caption := Format(_('Configure "%s"'), [FPostProcessor.Name]);
end;

procedure TfrmConfigureSetTags.txtCommentKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Char(VK_RETURN) then
    Key := #0;
end;

procedure TfrmConfigureSetTags.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;

  inherited;
end;

end.
