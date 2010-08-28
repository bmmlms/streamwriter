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
unit Settings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, ImgList, ComCtrls, ShellAPI,
  ShlObj, AppData, LanguageObjects, Functions, GUIFunctions, SettingsBase,
  Plugins, StrUtils, DynBASS;

type
  TfrmSettings = class(TfrmSettingsBase)
    pnlStreams: TPanel;
    pnlMain: TPanel;
    chkTrayClose: TCheckBox;
    pnlAdvanced: TPanel;
    txtMaxRetries: TLabeledEdit;
    txtRetryDelay: TLabeledEdit;
    Label1: TLabel;
    txtMinDiskSpace: TLabeledEdit;
    Label7: TLabel;
    pnlPlugins: TPanel;
    lstPlugins: TListView;
    cmdConfigure: TBitBtn;
    chkSubmitStreams: TCheckBox;
    Label6: TLabel;
    lblHelp: TLabel;
    txtDir: TLabeledEdit;
    cmdBrowse: TSpeedButton;
    chkRelay: TCheckBox;
    Label11: TLabel;
    GroupBox2: TGroupBox;
    txtFilePattern: TLabeledEdit;
    txtPreview: TLabeledEdit;
    lblFilePattern: TLabel;
    lstDefaultAction: TComboBox;
    Label3: TLabel;
    pnlCut: TPanel;
    txtSongBuffer: TLabeledEdit;
    txtShortSongSize: TLabeledEdit;
    Label4: TLabel;
    Label5: TLabel;
    chkSkipShort: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    chkSearchSilence: TCheckBox;
    Label2: TLabel;
    Label10: TLabel;
    txtSilenceLevel: TEdit;
    Label12: TLabel;
    txtSilenceLength: TEdit;
    Label13: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdBrowseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Label11Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure lstPluginsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure txtFilePatternChange(Sender: TObject);
    procedure chkSkipShortClick(Sender: TObject);
    procedure chkSearchSilenceClick(Sender: TObject);
  private
    FBrowseDir: Boolean;
    FRelayChanged: Boolean;
    FDefaultActionIdx: Integer;
    function ValidatePattern: string;
  protected
    procedure RegisterPages; override;
    procedure Finish; override;
    function CanFinish: Boolean; override;
    procedure PreTranslate; override;
    procedure PostTranslate; override;
  public
    constructor Create(AOwner: TComponent; BrowseDir: Boolean = False); reintroduce;
    property RelayChanged: Boolean read FRelayChanged;
  end;

implementation

{$R *.dfm}

constructor TfrmSettings.Create(AOwner: TComponent; BrowseDir: Boolean = False);
var
  i: Integer;
  Item: TListItem;
begin
  inherited Create(AOwner);

  FBrowseDir := BrowseDir;

  ClientWidth := 420;
  ClientHeight := 395;

  lstDefaultAction.ItemIndex := Integer(AppGlobals.DefaultAction);

  Language.Translate(Self, PreTranslate, PostTranslate);

  for i := 0 to Self.ControlCount - 1 do begin
    if Self.Controls[i] is TPanel then begin
      if TPanel(Self.Controls[i]) = pnlLeft then
        Continue;
      Self.Controls[i].Left := 96;
      Self.Controls[i].Top := 36;
      TPanel(Self.Controls[i]).BevelOuter := bvNone;
    end;
  end;

  pnlGeneral.BringToFront;

  FRelayChanged := False;

  AppGlobals.Lock;
  txtFilePattern.Text := AppGlobals.FilePattern;
  txtDir.Text := AppGlobals.Dir;

  chkSkipShort.Checked := AppGlobals.SkipShort;
  chkSearchSilence.Checked := AppGlobals.SearchSilence;

  txtSilenceLevel.Enabled := chkSearchSilence.Checked;
  txtSilenceLength.Enabled := chkSearchSilence.Checked;
  txtShortSongSize.Enabled := chkSkipShort.Checked;

  chkTrayClose.Checked := AppGlobals.TrayClose;
  chkRelay.Checked := AppGlobals.Relay;
  chkSubmitStreams.Checked := AppGlobals.SubmitStreams;
  txtShortSongSize.Text := IntToStr(AppGlobals.ShortSize);
  txtSongBuffer.Text := IntToStr(AppGlobals.SongBuffer);
  txtMaxRetries.Text := IntToStr(AppGlobals.MaxRetries);
  txtRetryDelay.Text := IntToStr(AppGlobals.RetryDelay);
  txtMinDiskSpace.Text := IntToStr(AppGlobals.MinDiskSpace);

  AppGlobals.Unlock;

  lblHelp.Caption := '';
  for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
  begin
    Item := lstPlugins.Items.Add;
    Item.GroupID := 0;
    Item.Caption := AppGlobals.PluginManager.Plugins[i].Name;
    Item.Data := AppGlobals.PluginManager.Plugins[i];
    Item.Checked := AppGlobals.PluginManager.Plugins[i].Active;
  end;
  if lstPlugins.Items.Count > 0 then
    lstPlugins.Items[0].Selected := True;

  if not DirectoryExists(txtDir.Text) then
    txtDir.Text := '';

  if not BassLoaded then
  begin
    chkSearchSilence.Enabled := False;
    chkSearchSilence.Checked := False;
    txtSilenceLevel.Enabled := False;
    txtSilenceLength.Enabled := False;
    Label10.Enabled := False;
    Label12.Enabled := False;
    Label13.Enabled := False;
  end;
end;

procedure TfrmSettings.Finish;
var
  i: Integer;
begin
  if AppGlobals.Relay <> chkRelay.Checked then
    FRelayChanged := True;

  AppGlobals.Lock;
  AppGlobals.Dir := txtDir.Text;
  AppGlobals.FilePattern := txtFilePattern.Text;
  AppGlobals.SkipShort := chkSkipShort.Checked;
  AppGlobals.TrayClose := chkTrayClose.Checked;
  AppGlobals.Relay := chkRelay.Checked;
  AppGlobals.SubmitStreams := chkSubmitStreams.Checked;
  AppGlobals.SongBuffer := StrToIntDef(txtSongBuffer.Text, 100);
  AppGlobals.ShortSize := StrToIntDef(txtShortSongSize.Text, 1000);
  AppGlobals.MaxRetries := StrToIntDef(txtMaxRetries.Text, 50);
  AppGlobals.RetryDelay := StrToIntDef(txtRetryDelay.Text, 5);
  AppGlobals.MinDiskSpace := StrToIntDef(txtMinDiskSpace.Text, 5);
  AppGlobals.DefaultAction := TClientActions(lstDefaultAction.ItemIndex);
  if BassLoaded then
    AppGlobals.SearchSilence := chkSearchSilence.Checked;
  AppGlobals.SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 3000000);
  AppGlobals.SilenceLength := StrToIntDef(txtSilenceLength.Text, 200);
  AppGlobals.Unlock;

  for i := 0 to lstPlugins.Items.Count - 1 do
    TPlugin(lstPlugins.Items[i].Data).Active := lstPlugins.Items[i].Checked;

  inherited;
end;

procedure TfrmSettings.FormActivate(Sender: TObject);
begin
  if FBrowseDir then
    cmdBrowse.Click;
end;

procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not DirectoryExists(txtDir.Text) then
  begin
    if MsgBox(Handle, _('The selected folder does not exist.'#13#10'Do you really want to close this window?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDYES then
      CanClose := True
    else
      CanClose := False;
  end;
end;

procedure TfrmSettings.FormResize(Sender: TObject);
begin
  inherited;
  lstPlugins.Columns[0].Width := lstPlugins.ClientWidth - 25;
end;

procedure TfrmSettings.Label11Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When enabled, you can listen to streams you are recording by using the provided context-menu ' +
                   'items or by dragging the stream into your player. This might cause warnings from the firewall, ' +
                   'so it is disabled by default.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.Label6Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When enabled every stream unknown to streamWriter will be submitted to the stream database. This helps populating the stream database so streamWriter''s browser will be kept up to date. No personal information will be sent, only the stream''s url.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.Label8Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When a title was recorded and it''s size is below the set limit, it will not be saved to disk.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.Label9Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('TODO: !!! und volume feld erklären!!! und sagen, dass die felder disabled sind, wenn bass.dll nicht geladen werden konnte!'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.lstPluginsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  inherited;

  cmdConfigure.Enabled := False;
  if Selected then
    lblHelp.Caption := TPlugin(Item.Data).Help;
end;

procedure TfrmSettings.PreTranslate;
begin
  inherited;
  FDefaultActionIdx := lstDefaultAction.ItemIndex;
end;

procedure TfrmSettings.PostTranslate;
var
  i: Integer;
begin
  inherited;
  lstPlugins.Groups[0].Header := _('Post-Processing');
  lblFilePattern.Caption := _('%s = streamname, %a = artist, %t = title, %n = tracknumber'#13#10 +
                              'Backslashes can be used to seperate directories.');
  if lstPlugins.Selected <> nil then
  begin
    AppGlobals.PluginManager.ReInitPlugins;
    lstPluginsSelectItem(lstPlugins, lstPlugins.Selected, True);
  end;

  for i := 0 to lstPlugins.Items.Count - 1 do
    lstPlugins.Items[i].Caption := TPlugin(lstPlugins.Items[i].Data).Name;

  AppGlobals.PluginManager.ReInitPlugins;
  lstDefaultAction.ItemIndex := FDefaultActionIdx;
end;

function TfrmSettings.ValidatePattern: string;
var
  Arr: TPatternReplaceArray;
  i: Integer;
begin
  inherited;

  SetLength(Arr, 4);
  Arr[0].C := 'a';
  Arr[0].Replace := _('Artist');
  Arr[1].C := 't';
  Arr[1].Replace := _('Title');
  Arr[2].C := 's';
  Arr[2].Replace := _('Streamname');
  Arr[3].C := 'n';
  Arr[3].Replace := IntToStr(78);

  Result := PatternReplace(txtFilePattern.Text, Arr);

  // Aneinandergereihte \ entfernen
  i := 1;
  if Length(Result) > 0 then
    while True do
    begin
      if i = Length(Result) then
        Break;
      if Result[i] = '\' then
        if Result[i + 1] = '\' then
        begin
          Result := Copy(Result, 1, i) + Copy(Result, i + 2, Length(Result) - i);
          Continue;
        end;
      Inc(i);
    end;

  // Ungültige Zeichen entfernen
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);
  Result := StringReplace(Result, ':', '', [rfReplaceAll]);
  Result := StringReplace(Result, '*', '', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '', [rfReplaceAll]);

  // Sicherstellen, dass am Anfang/Ende kein \ steht
  if Length(Result) > 0 then
    if Result[1] = '\' then
      Result := Copy(Result, 2, Length(Result) - 1);
  if Length(Result) > 0 then
    if Result[Length(Result)] = '\' then
      Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TfrmSettings.RegisterPages;
begin
  FPageList.Add(TPage.Create('&Settings', pnlMain, 'PROPERTIES'));
  FPageList.Add(TPage.Create('S&treams', pnlStreams, 'START'));
  FPageList.Add(TPage.Create('&Cut', pnlCut, 'CUT'));
  FPageList.Add(TPage.Create('&Plugins', pnlPlugins, 'PLUGINS'));
  FPageList.Add(TPage.Create('&Advanced', pnlAdvanced, 'MISC'));
  inherited;
end;

procedure TfrmSettings.txtFilePatternChange(Sender: TObject);
begin
  inherited;
  txtPreview.Text := ValidatePattern;
end;

function TfrmSettings.CanFinish: Boolean;
begin
  Result := False;

  if not inherited then
    Exit;

  if Trim(txtMinDiskSpace.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter the minumum free space that must be available for recording.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtMinDiskSpace.Parent)));
    txtMinDiskSpace.SetFocus;
    Exit;
  end;

  if Trim(ValidatePattern) = '' then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames, i.e. a preview text must be visible.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtFilePattern.Parent.Parent)));
    txtFilePattern.SetFocus;
    Exit;
  end;

  if not DirectoryExists(txtDir.Text) then
  begin
    MsgBox(Handle, _('The selected folder for saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtDir.Parent)));
    cmdBrowse.Click;
    Exit;
  end;

  if Trim(txtShortSongSize.Text) = '' then
  begin
    if chkSkipShort.Checked then
    begin
      MsgBox(Handle, _('Please enter the maximum size for songs that should be considered as ads.'), _('Info'), MB_ICONINFORMATION);
      SetPage(FPageList.Find(TPanel(txtShortSongSize.Parent)));
      txtShortSongSize.SetFocus;
      Exit;
    end else
      txtShortSongSize.Text := IntToStr(AppGlobals.ShortSize);
  end;

  if Trim(txtSongBuffer.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter the size of the buffer that should be added to every beginning/end of saved titles.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtSongBuffer.Parent)));
    txtSongBuffer.SetFocus;
    Exit;
  end;

  // TODO: Die neuen cutting-felder validieren.

  if Trim(txtMaxRetries.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter the number of maximum connect retries.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtMaxRetries.Parent)));
    txtMaxRetries.SetFocus;
    Exit;
  end;

  if Trim(txtRetryDelay.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter the delay between connect retries.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtRetryDelay.Parent)));
    txtRetryDelay.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TfrmSettings.chkSearchSilenceClick(Sender: TObject);
begin
  inherited;
  txtSilenceLevel.Enabled := chkSearchSilence.Checked;
  txtSilenceLength.Enabled := chkSearchSilence.Checked;
end;

procedure TfrmSettings.chkSkipShortClick(Sender: TObject);
begin
  inherited;
  txtShortSongSize.Enabled := chkSkipShort.Checked;
end;

procedure TfrmSettings.cmdBrowseClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := BrowseDialog(Handle, _('Select folder for saved songs'), BIF_RETURNONLYFSDIRS);

  if Dir = '' then
    Exit;

  if DirectoryExists(Dir) then
    txtDir.Text := IncludeTrailingBackslash(Dir)
  else
    MsgBox(Self.Handle, _('The selected folder does not exist. Please choose another one.'), _('Info'), MB_ICONINFORMATION);
end;

end.
