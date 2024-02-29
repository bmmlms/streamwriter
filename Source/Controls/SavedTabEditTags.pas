{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2024 Alexander Nottelmann

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
  AddonAudioGenie,
  AppData,
  Buttons,
  Classes,
  Controls,
  DataManager,
  Images,
  ExtCtrls,
  FileTagger,
  Forms,
  Functions,
  LanguageObjects,
  MLabeledEdit,
  SharedData,
  StdCtrls,
  SysUtils,
  Windows;

const
  WM_AFTERSHOWN = WM_USER + 678;

type

  { TfrmEditTags }

  TfrmEditTags = class(TForm)
    grpTags: TGroupBox;
    Label1: TLabel;
    txtStreamname: TMLabeledEdit;
    txtAlbum: TMLabeledEdit;
    txtArtist: TMLabeledEdit;
    txtComment: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    grpData: TGroupBox;
    txtGenre: TMLabeledEdit;
    txtTitle: TMLabeledEdit;
    procedure btnCloseClick(Sender: TObject);
  private
    FAddon: TAddonAudioGenie;
    FTracks: TTrackInfoArray;
    FTagsRead: Boolean;

    procedure DisableTags;
    procedure AfterShown(var Msg: TMessage); message WM_AFTERSHOWN;
  protected
    procedure DoShow; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    function ShowModal(Tracks: TTrackInfoArray): Integer; reintroduce;

    property Tracks: TTrackInfoArray read FTracks;
  end;

implementation

{$R *.lfm}

procedure TfrmEditTags.AfterShown(var Msg: TMessage);
var
  FS: TFileStream;
  FileTagger: TFileTagger;
begin
  txtStreamname.Control.Text := FTracks[0].Streamname;

  FAddon := AppGlobals.AddonManager.Find(TAddonAudioGenie) as TAddonAudioGenie;

  if Length(FTracks) = 1 then
  begin
    if not FAddon.FilesExtracted then
      if not AppGlobals.AddonManager.EnableAddon(Owner as TCustomForm, FAddon, True) then
      begin
        DisableTags;
        TFunctions.MsgBox(_('Tags cannot be read and written because a needed addon has not been installed.'), _('Info'), MB_ICONINFORMATION);
      end;

    if FAddon.FilesExtracted then
      try
        FS := TFileStream.Create(FTracks[0].Filename, fmOpenRead or fmShareExclusive);
        FS.Free;

        FileTagger := TFileTagger.Create;
        try
          if FileTagger.Read(FTracks[0].Filename) then
          begin
            txtArtist.Control.Text := FileTagger.Tag.Artist;
            txtTitle.Control.Text := FileTagger.Tag.Title;
            txtAlbum.Control.Text := FileTagger.Tag.Album;
            txtGenre.Control.Text := FileTagger.Tag.Genre;
            txtComment.Text := FileTagger.Tag.Comment;

            FTagsRead := True;
          end else
            DisableTags;
        finally
          FileTagger.Free;
        end;
      except
        DisableTags;
        TFunctions.MsgBox(_('Tags could not be read from the file because it is in use.'), _('Info'), MB_ICONINFORMATION);
      end;
  end else
    DisableTags;
end;

procedure TfrmEditTags.DoShow;
begin
  inherited DoShow;

  PostMessage(Handle, WM_AFTERSHOWN, 0, 0);
end;

procedure TfrmEditTags.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;

  inherited;
end;

procedure TfrmEditTags.btnCloseClick(Sender: TObject);
var
  FileTagger: TFileTagger;
  i: Integer;
begin
  if FTagsRead and (Length(FTracks) = 1) and FileExists(FTracks[0].Filename) then
  begin
    FileTagger := TFileTagger.Create;
    try
      FileTagger.Tag.Artist := Trim(txtArtist.Control.Text);
      FileTagger.Tag.Title := Trim(txtTitle.Control.Text);
      FileTagger.Tag.Album := Trim(txtAlbum.Control.Text);
      FileTagger.Tag.Genre := Trim(txtGenre.Control.Text);
      FileTagger.Tag.Comment := Trim(txtComment.Text);

      if not FileTagger.Write(Language.CurrentLanguage.LCID, FTracks[0].Filename) then
      begin
        TFunctions.MsgBox(_('The file could not be saved. Please make sure it is not in use and try saving again.'), _('Error'), MB_ICONERROR);
        Exit;
      end;
    finally
      FileTagger.Free;
    end;
  end;

  for i := 0 to High(FTracks) do
    FTracks[i].Streamname := Trim(txtStreamname.Control.Text);

  Close;
end;

constructor TfrmEditTags.Create(AOwner: TComponent);
begin
  inherited;

  modSharedData.imgImages.GetIcon(TImages.TAG_BLUE_EDIT, Icon);

  Language.Translate(Self);
end;

destructor TfrmEditTags.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FTracks) do
    FTracks[i].Free;

  inherited;
end;

procedure TfrmEditTags.DisableTags;
begin
  txtArtist.Enabled := False;
  txtTitle.Enabled := False;
  txtAlbum.Enabled := False;
  txtGenre.Enabled := False;
  Label1.Enabled := False;
  txtComment.Enabled := False;
  grpTags.Enabled := False;

  txtStreamname.SetFocus;
  txtStreamname.Control.SelLength := 0;
end;

function TfrmEditTags.ShowModal(Tracks: TTrackInfoArray): Integer;
var
  i: Integer;
begin
  SetLength(FTracks, Length(Tracks));
  for i := 0 to High(Tracks) do
    FTracks[i] := Tracks[i].Copy;

  Result := inherited ShowModal;
end;

end.
