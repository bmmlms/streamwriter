{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, LanguageObjects, PostProcess,
  PngSpeedButton, MControls;

type
  TfrmConfigureSetTags = class(TForm)
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
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnResetPatternClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure txtCommentKeyPress(Sender: TObject; var Key: Char);
  private
    FSaveData: Boolean;

    FPostProcessor: TPostProcessBase;
    FArtist, FTitle, FAlbum, FComment: string;
  public
    constructor Create(AOwner: TComponent; PostProcessor: TPostProcessBase; Artist, Title, Album, Comment: string); reintroduce;

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
  end;
end;

constructor TfrmConfigureSetTags.Create(AOwner: TComponent;
  PostProcessor: TPostProcessBase; Artist, Title, Album, Comment: string);
begin
  inherited Create(AOwner);

  FPostProcessor := PostProcessor;

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
  Caption := Format(_('Configure "%s"'), [FPostProcessor.Name]);
end;

procedure TfrmConfigureSetTags.txtCommentKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    Key := #0;
end;

end.
