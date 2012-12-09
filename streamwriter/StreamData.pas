{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

    Portions created by Ralf Kruse

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
unit StreamData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, PngSpeedButton, GUIFunctions,
  HomeCommunication, Functions, LanguageObjects, PerlRegEx, Logging,
  ComCtrls, AppData, ImgList, PngImageList, HomeCommands;

type
  TfrmStreamData = class(TForm)
    txtTitlePattern: TLabeledEdit;
    optGood: TRadioButton;
    Label1: TLabel;
    optBad: TRadioButton;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    Label2: TLabel;
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    Label3: TLabel;
    btnResetTitlePattern: TPngSpeedButton;
    lstIgnoreTitles: TListView;
    btnAddIgnoreTitlePattern: TButton;
    btnRemoveIgnoreTitlePattern: TButton;
    lblIgnoreTitles: TLabel;
    btnApplyFromStream: TButton;
    PngImageList1: TPngImageList;
    procedure btnResetTitlePatternClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure optGoodClick(Sender: TObject);
    procedure optBadClick(Sender: TObject);
    procedure txtTitlePatternChange(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure btnApplyFromStreamClick(Sender: TObject);
    procedure lstIgnoreTitlesResize(Sender: TObject);
    procedure lstIgnoreTitlesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnRemoveIgnoreTitlePatternClick(Sender: TObject);
    procedure lstIgnoreTitlesEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure btnAddIgnoreTitlePatternClick(Sender: TObject);
  private
    FInitialized: Boolean;

    FSaveSettings: Boolean;
    FStreamSettings: TStreamSettings;
    FID: Integer;
    FName: string;
    FRegEx: string;
    FIsOkay: Boolean;
    FIgnoreList: TStringList;

    FIsOkayChanged: Boolean;
    FRegExChanged: Boolean;
    FIgnoreTracksChanged: Boolean;
  public
    constructor Create(AOwner: TComponent; StreamSettings: TStreamSettings; ID: Integer;
      Name: string; RegEx: string; IsOkay: Boolean; IgnoreTracks: TStringList); reintroduce;
    destructor Destroy; override;

    property SaveSettings: Boolean read FSaveSettings;
    property RecordingOkay: Boolean read FIsOkay;
    property RegEx: string read FRegEx;
    property IgnoreTracks: TStringList read FIgnoreList;
    property IsOkayChanged: Boolean read FIsOkayChanged;
    property RegExChanged: Boolean read FRegExChanged;
    property IgnoreTracksChanged: Boolean read FIgnoreTracksChanged;
  end;

implementation

{$R *.dfm}

procedure TfrmStreamData.btnAddIgnoreTitlePatternClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := lstIgnoreTitles.Items.Add;
  Item.ImageIndex := 0;

  Item.EditCaption;

  FIgnoreTracksChanged := True;
end;

procedure TfrmStreamData.btnApplyFromStreamClick(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
begin
  txtTitlePattern.Text := FStreamSettings.TitlePattern;
  lstIgnoreTitles.Clear;
  for i := 0 to FStreamSettings.IgnoreTrackChangePattern.Count - 1 do
  begin
    Item := lstIgnoreTitles.Items.Add;
    Item.Caption := FStreamSettings.IgnoreTrackChangePattern[i];
    Item.ImageIndex := 0;
  end;

  FIgnoreTracksChanged := True;
end;

procedure TfrmStreamData.btnOKClick(Sender: TObject);
var
  i: Integer;
  R: TPerlRegEx;
  RValid: Boolean;
  ArtistFound, TitleFound: Boolean;
  Send: Boolean;
  Cmd: TCommandSetStreamData;
begin
  if not HomeComm.Connected then
  begin
    MsgBox(Handle, _('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  RValid := False;
  R := TPerlRegEx.Create;
  try
    R.RegEx := txtTitlePattern.Text;
    try
      R.Compile;
      RValid := True;
    except end;
  finally
    R.Free;
  end;

  ArtistFound := (Pos('(?P<a>.*)', txtTitlePattern.Text) > 0) or (Pos('(?P<a>.*?)', txtTitlePattern.Text) > 0);
  TitleFound := (Pos('(?P<t>.*)', txtTitlePattern.Text) > 0) or (Pos('(?P<t>.*?)', txtTitlePattern.Text) > 0);

  if (Trim(txtTitlePattern.Text) = '') or (not RValid) or (not ArtistFound) or (not TitleFound) then
  begin
    MsgBox(Handle, _('Please supply a valid regular expression containing the groups (?P<a>.*)/(?P<a>.*?) and (?P<t>.*)/(?P<t>.*?). If you don''t have a clue about regular expressions, click the button next to the text field to reset the pattern.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  Send := False;
  Cmd := TCommandSetStreamData.Create;
  Cmd.StreamID := FID;

  // Das mit das ListView bei "Enter" das bearbeiten aufhört, dass der Wert
  // uns hier zur Verfügung steht.
  btnOK.SetFocus;
  Application.ProcessMessages;

  if (optGood.Checked <> FIsOkay) and FIsOkayChanged then
  begin
    Cmd.HasRecordingOkay := True;
    Cmd.RecordingOkay := optGood.Checked;
    Send := True;

    FIsOkay := optGood.Checked;
    FSaveSettings := True;
  end else
    FIsOkayChanged := False;

  if (FRegEx <> txtTitlePattern.Text) and (FRegExChanged) then
  begin
    Cmd.TitleRegEx := txtTitlePattern.Text;
    Send := True;

    FRegEx := txtTitlePattern.Text;
    FSaveSettings := True;
  end else
    FRegExChanged := False;

  if FIgnoreTracksChanged then
  begin
    FIgnoreList.Clear;
    for i := 0 to lstIgnoreTitles.Items.Count - 1 do
      if Trim(lstIgnoreTitles.Items[i].Caption) <> '' then
        FIgnoreList.Add(Trim(lstIgnoreTitles.Items[i].Caption));

    Cmd.HasIgnoreTitles := True;
    Cmd.IgnoreTitles := FIgnoreList.Text;
    Send := True;

    FSaveSettings := True;
  end;

  if Send then
  begin
    if not HomeComm.SendCommand(Cmd) then
      Cmd.Free;
  end else
    Cmd.Free;

  Close;
end;

procedure TfrmStreamData.btnRemoveIgnoreTitlePatternClick(Sender: TObject);
begin
  lstIgnoreTitles.Items.Delete(lstIgnoreTitles.Selected.Index);

  FIgnoreTracksChanged := True;
end;

procedure TfrmStreamData.btnResetTitlePatternClick(Sender: TObject);
begin
  txtTitlePattern.Text := '(?P<a>.*) - (?P<t>.*)';
  txtTitlePattern.SetFocus;
end;

constructor TfrmStreamData.Create(AOwner: TComponent; StreamSettings: TStreamSettings;
  ID: Integer; Name, RegEx: string; IsOkay: Boolean; IgnoreTracks: TStringList);
var
  i: Integer;
  Item: TListItem;
begin
  inherited Create(AOwner);

  FStreamSettings := StreamSettings;
  FID := ID;
  FName := Name;
  FRegEx := RegEx;
  FIsOkay := IsOkay;
  FIgnoreList := TStringList.Create;

  optGood.Checked := FIsOkay;
  optBad.Checked := not FIsOkay;
  txtTitlePattern.Text := RegEx;
  if txtTitlePattern.Text = '' then
    txtTitlePattern.Text := '(?P<a>.*) - (?P<t>.*)';

  FInitialized := True;

  Language.Translate(Self);

  btnApplyFromStream.Enabled := FStreamSettings <> nil;

  for i := 0 to IgnoreTracks.Count - 1 do
  begin
    Item := lstIgnoreTitles.Items.Add;
    Item.Caption := IgnoreTracks[i];
    Item.ImageIndex := 0;
  end;
end;

destructor TfrmStreamData.Destroy;
begin
  FIgnoreList.Free;

  inherited;
end;

procedure TfrmStreamData.FormActivate(Sender: TObject);
begin
  lblTop.Caption := TruncateText(lblTop.Caption + ' ' + FName, lblTop.Width, lblTop.Font);
end;

procedure TfrmStreamData.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmStreamData.Label1Click(Sender: TObject);
begin
  optGood.Checked := True;
end;

procedure TfrmStreamData.Label2Click(Sender: TObject);
begin
  optBad.Checked := True;
end;

procedure TfrmStreamData.lstIgnoreTitlesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  btnRemoveIgnoreTitlePattern.Enabled := lstIgnoreTitles.Selected <> nil;
end;

procedure TfrmStreamData.lstIgnoreTitlesEdited(Sender: TObject;
  Item: TListItem; var S: string);
begin
  if Trim(S) = '' then
    S := Item.Caption
  else
    FIgnoreTracksChanged := True;
end;

procedure TfrmStreamData.lstIgnoreTitlesResize(Sender: TObject);
begin
  lstIgnoreTitles.Columns[0].Width := lstIgnoreTitles.ClientWidth - 25;
end;

procedure TfrmStreamData.optBadClick(Sender: TObject);
begin
  if FInitialized then
    FIsOkayChanged := True;
end;

procedure TfrmStreamData.optGoodClick(Sender: TObject);
begin
  if FInitialized then
    FIsOkayChanged := True;
end;

procedure TfrmStreamData.txtTitlePatternChange(Sender: TObject);
begin
  if FInitialized then
    FRegExChanged := True;
end;

end.
