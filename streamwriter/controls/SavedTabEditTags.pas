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
  PluginAudioGenie, FileTagger, Functions;

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
    FPlugin: TPluginAudioGenie;
    FTagger: TFileTagger;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function EditFile(Filename: string): Boolean;
  end;

implementation

{$R *.dfm}

procedure TfrmEditTags.btnCloseClick(Sender: TObject);
begin
  FTagger.Artist := Trim(txtArtist.Text);
  FTagger.Title := Trim(txtTitle.Text);
  FTagger.Album := Trim(txtAlbum.Text);
  FTagger.Comment := Trim(txtComment.Text);

  if FTagger.Write(FTagger.Filename) then
    Close
  else
    MsgBox(Self.Handle, _('The file could not be saved. Please make sure it is not in use and try saving again.'), _('Error'), MB_ICONERROR);
end;

constructor TfrmEditTags.Create(AOwner: TComponent);
begin
  inherited;

  Language.Translate(Self);
end;

destructor TfrmEditTags.Destroy;
begin
  if Assigned(FTagger) then
    FTagger.Free;

  inherited;
end;

function TfrmEditTags.EditFile(Filename: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  FPlugin := AppGlobals.PluginManager.Find(TPluginAudioGenie) as TPluginAudioGenie;

  if not FPlugin.FilesExtracted then
  begin
    if not AppGlobals.PluginManager.EnablePlugin(Owner as TCustomForm, FPlugin, True) then
      Exit;
  end;

  try
    FS := TFileStream.Create(Filename, fmOpenRead or fmShareExclusive);
    Result := True;
  except
    Exit;
  end;
  FS.Free;

  FTagger := TFileTagger.Create;
  if not FTagger.Read(Filename) then
    Exit(False);

  txtArtist.Text := FTagger.Artist;
  txtTitle.Text := FTagger.Title;
  txtAlbum.Text := FTagger.Album;
  txtComment.Text := FTagger.Comment;
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
