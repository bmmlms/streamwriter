{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010 Alexander Nottelmann

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
unit SavedTabEditTags;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LanguageObjects, StdCtrls, Buttons, ExtCtrls, AppData, AudioGenie,
  AddonAudioGenie, FileTagger, Functions;

type
  TfrmEditTags = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    Label1: TLabel;
    txtArtist: TLabeledEdit;
    txtTitle: TLabeledEdit;
    txtComment: TMemo;
    txtAlbum: TLabeledEdit;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
  private
    FAddon: TAddonAudioGenie;
    FFilename: string;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    function EditFile(Filename: string): Boolean;
  end;

implementation

{$R *.dfm}

procedure TfrmEditTags.btnCloseClick(Sender: TObject);
var
  FileTagger: TFileTagger;
begin
  FileTagger := TFileTagger.Create;
  try
    FileTagger.Tag.Artist := Trim(txtArtist.Text);
    FileTagger.Tag.Title := Trim(txtTitle.Text);
    FileTagger.Tag.Album := Trim(txtAlbum.Text);
    FileTagger.Tag.Comment := Trim(txtComment.Text);

    if FileTagger.Write(FFilename) then
      Close
    else
      MsgBox(Self.Handle, _('The file could not be saved. Please make sure it is not in use and try saving again.'), _('Error'), MB_ICONERROR);
  finally
    FileTagger.Free;
  end;
end;

constructor TfrmEditTags.Create(AOwner: TComponent);
begin
  inherited;

  Language.Translate(Self);
end;

destructor TfrmEditTags.Destroy;
begin

  inherited;
end;

function TfrmEditTags.EditFile(Filename: string): Boolean;
var
  FS: TFileStream;
  FileTagger: TFileTagger;
begin
  FFilename := Filename;
  Result := False;
  FAddon := AppGlobals.AddonManager.Find(TAddonAudioGenie) as TAddonAudioGenie;

  if not FAddon.FilesExtracted then
  begin
    if not AppGlobals.AddonManager.EnableAddon(Owner as TCustomForm, FAddon, True) then
      Exit;
  end;

  try
    FS := TFileStream.Create(Filename, fmOpenRead or fmShareExclusive);
  except
    Exit;
  end;
  FS.Free;

  FileTagger := TFileTagger.Create;
  try
    if not FileTagger.Read(Filename) then
      Exit;

    txtArtist.Text := FileTagger.Tag.Artist;
    txtTitle.Text := FileTagger.Tag.Title;
    txtAlbum.Text := FileTagger.Tag.Album;
    txtComment.Text := FileTagger.Tag.Comment;
  finally
    FileTagger.Free;
  end;

  Result := True;
end;

procedure TfrmEditTags.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

end.
