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
  Plugins, StrUtils, DynBASS, ICEClient, Generics.Collections;

type
  TfrmSettings = class(TfrmSettingsBase)
    pnlStreams: TPanel;
    pnlMain: TPanel;
    chkTray: TCheckBox;
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
    txtDir: TLabeledEdit;
    btnBrowse: TSpeedButton;
    chkRelay: TCheckBox;
    Label11: TLabel;
    GroupBox2: TGroupBox;
    txtFilePattern: TLabeledEdit;
    txtPreview: TLabeledEdit;
    lblFilePattern: TLabel;
    Label3: TLabel;
    pnlCut: TPanel;
    txtSongBuffer: TLabeledEdit;
    txtShortSongSize: TLabeledEdit;
    Label4: TLabel;
    Label5: TLabel;
    chkSkipShort: TCheckBox;
    Label8: TLabel;
    chkSearchSilence: TCheckBox;
    Label2: TLabel;
    Label10: TLabel;
    txtSilenceLevel: TEdit;
    Label12: TLabel;
    txtSilenceLength: TEdit;
    Label13: TLabel;
    lstDefaultAction: TComboBox;
    Label14: TLabel;
    chkDeleteStreams: TCheckBox;
    optClose: TRadioButton;
    optMinimize: TRadioButton;
    chkAddSavedToIgnore: TCheckBox;
    Label15: TLabel;
    lstDefaultFilter: TComboBox;
    dlgOpen: TOpenDialog;
    btnHelp: TSpeedButton;
    btnAddUp: TButton;
    btnRemove: TButton;
    txtApp: TLabeledEdit;
    txtAppParams: TLabeledEdit;
    lblAppParams: TLabel;
    btnBrowseApp: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnMoveDown: TSpeedButton;
    ImageList1: TImageList;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure lstPluginsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure txtFilePatternChange(Sender: TObject);
    procedure chkSkipShortClick(Sender: TObject);
    procedure chkSearchSilenceClick(Sender: TObject);
    procedure chkTrayClick(Sender: TObject);
    procedure btnBrowseAppClick(Sender: TObject);
    procedure btnAddUpClick(Sender: TObject);
    procedure txtAppParamsChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure lstPluginsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure btnBrowseClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    FBrowseDir: Boolean;
    FRelayChanged: Boolean;
    FDefaultActionIdx: Integer;
    FDefaultFilterIdx: Integer;
    FTemporaryPlugins: TList<TExternalPlugin>;
    function ValidatePattern: string;
    function GetNewID: Integer;
  protected
    procedure RegisterPages; override;
    procedure Finish; override;
    function CanFinish: Boolean; override;
    procedure PreTranslate; override;
    procedure PostTranslate; override;
  public
    constructor Create(AOwner: TComponent; BrowseDir: Boolean = False); reintroduce;
    destructor Destroy; override;
    property RelayChanged: Boolean read FRelayChanged;
  end;

implementation

{$R *.dfm}

constructor TfrmSettings.Create(AOwner: TComponent; BrowseDir: Boolean = False);
var
  i: Integer;
  ExtPlugin: TExternalPlugin;
  Item: TListItem;
  B: TBitmap;
begin
  FUseTree := True;

  inherited Create(AOwner);

  FBrowseDir := BrowseDir;

  ClientWidth := 480;
  ClientHeight := 410;

  lstDefaultAction.ItemIndex := Integer(AppGlobals.DefaultAction);
  lstDefaultFilter.ItemIndex := Integer(AppGlobals.DefaultFilter);

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
  chkDeleteStreams.Checked := AppGlobals.DeleteStreams;
  chkAddSavedToIgnore.Checked := AppGlobals.AddSavedToIgnore;

  chkSkipShort.Checked := AppGlobals.SkipShort;
  chkSearchSilence.Checked := AppGlobals.SearchSilence;

  chkSearchSilenceClick(nil);

  chkTray.Checked := AppGlobals.Tray;
  optClose.Checked := not AppGlobals.TrayOnMinimize;
  optMinimize.Checked := AppGlobals.TrayOnMinimize;

  chkTrayClick(nil);

  chkSubmitStreams.Checked := AppGlobals.SubmitStreams;
  txtShortSongSize.Text := IntToStr(AppGlobals.ShortSize);
  txtSongBuffer.Text := IntToStr(AppGlobals.SongBuffer);
  txtMaxRetries.Text := IntToStr(AppGlobals.MaxRetries);
  txtRetryDelay.Text := IntToStr(AppGlobals.RetryDelay);
  txtMinDiskSpace.Text := IntToStr(AppGlobals.MinDiskSpace);

  txtSilenceLevel.Text := IntToStr(AppGlobals.SilenceLevel);
  txtSilenceLength.Text := IntToStr(AppGlobals.SilenceLength);
  AppGlobals.Unlock;

  FTemporaryPlugins := TList<TExternalPlugin>.Create;
  for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
  begin
    Item := lstPlugins.Items.Add;
    Item.Caption := AppGlobals.PluginManager.Plugins[i].Name;
    Item.Checked := AppGlobals.PluginManager.Plugins[i].Active;
    if AppGlobals.PluginManager.Plugins[i] is TPlugin then
    begin
      Item.Data := AppGlobals.PluginManager.Plugins[i];
      Item.ImageIndex := 0;
    end else
    begin
      ExtPlugin := TExternalPlugin(AppGlobals.PluginManager.Plugins[i]);
      ExtPlugin := TExternalPlugin.Create(ExtPlugin.Exe, ExtPlugin.Params,
        ExtPlugin.Active, ExtPlugin.Identifier, ExtPlugin.Order);
      FTemporaryPlugins.Add(ExtPlugin);
      Item.Data := ExtPlugin;
      Item.ImageIndex := 1;
    end;
  end;
  lstPlugins.CustomSort(nil, 0);
  if lstPlugins.Items.Count > 0 then
    lstPlugins.Items[0].Selected := True;

  if not DirectoryExists(txtDir.Text) then
    txtDir.Text := '';

  B := TBitmap.Create;
  try
    GetBitmap('ARROWUP', 2, B);
    btnMoveUp.Glyph := B;
    GetBitmap('ARROWDOWN', 2, B);
    btnMoveDown.Glyph := B;
    GetBitmap('QUESTION', 2, B);
    btnHelp.Glyph := B;
    GetBitmap('BROWSE', 2, B);
    btnBrowse.Glyph := B;
    btnBrowseApp.Glyph := B;
  finally
    B.Free;
  end;

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

destructor TfrmSettings.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTemporaryPlugins.Count - 1 do
    FTemporaryPlugins[i].Free;
  FTemporaryPlugins.Free;
  inherited;
end;

procedure TfrmSettings.Finish;
var
  i, n: Integer;
  EP: TExternalPlugin;
begin
  AppGlobals.Lock;
  AppGlobals.FilePattern := txtFilePattern.Text;
  AppGlobals.Dir := txtDir.Text;
  AppGlobals.DeleteStreams := chkDeleteStreams.Checked;
  AppGlobals.AddSavedToIgnore := chkAddSavedToIgnore.Checked;
  AppGlobals.SkipShort := chkSkipShort.Checked;
  AppGlobals.Tray := chkTray.Checked;
  AppGlobals.TrayOnMinimize := optMinimize.Checked;
  AppGlobals.SubmitStreams := chkSubmitStreams.Checked;
  AppGlobals.SongBuffer := StrToIntDef(txtSongBuffer.Text, 0);
  AppGlobals.ShortSize := StrToIntDef(txtShortSongSize.Text, 1500);
  AppGlobals.MaxRetries := StrToIntDef(txtMaxRetries.Text, 100);
  AppGlobals.RetryDelay := StrToIntDef(txtRetryDelay.Text, 5);
  AppGlobals.MinDiskSpace := StrToIntDef(txtMinDiskSpace.Text, 5);
  AppGlobals.DefaultAction := TClientActions(lstDefaultAction.ItemIndex);
  AppGlobals.DefaultFilter := TUseFilters(lstDefaultFilter.ItemIndex);
  if BassLoaded then
    AppGlobals.SearchSilence := chkSearchSilence.Checked;
  AppGlobals.SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 5);
  AppGlobals.SilenceLength := StrToIntDef(txtSilenceLength.Text, 100);

  // TODO: Pluginmanager locken oder sowas? und an den stellen, wo sich plugins tiggern.
  for i := 0 to FTemporaryPlugins.Count - 1 do
  begin
    EP := AppGlobals.PluginManager.GetID(FTemporaryPlugins[i].Identifier);
    if EP = nil then
    begin
      EP := TExternalPlugin(FTemporaryPlugins[i]);
      AppGlobals.PluginManager.Plugins.Add(TExternalPlugin.Create(EP.Exe, EP.Params, EP.Active, EP.Identifier, EP.Order));
    end else
    begin
      EP.Exe := FTemporaryPlugins[i].Exe;
      EP.Params := FTemporaryPlugins[i].Params;
      EP.Active := FTemporaryPlugins[i].Active;
      EP.Order := FTemporaryPlugins[i].Order;
    end;
  end;
  for i := AppGlobals.PluginManager.Plugins.Count - 1 downto 0 do
  begin
    if AppGlobals.PluginManager.Plugins[i] is TExternalPlugin then
    begin
      EP := nil;
      for n := 0 to FTemporaryPlugins.Count - 1 do
        if FTemporaryPlugins[n].Identifier = TExternalPlugin(AppGlobals.PluginManager.Plugins[i]).Identifier then
        begin
          EP := TExternalPlugin(AppGlobals.PluginManager.Plugins[i]);
          Break;
        end;
      if EP = nil then
      begin
        AppGlobals.PluginManager.Plugins[i].Free;
        AppGlobals.PluginManager.Plugins.Delete(i);
        Continue;
      end;
    end;
  end;

  for i := 0 to lstPlugins.Items.Count - 1 do
  begin
    if TPluginBase(lstPlugins.Items[i].Data) is TPlugin then
    begin
      TPlugin(lstPlugins.Items[i].Data).Order := i;
      TPlugin(lstPlugins.Items[i].Data).Active := lstPlugins.Items[i].Checked;
    end else if TPluginBase(lstPlugins.Items[i].Data) is TExternalPlugin then
    begin
      EP := AppGlobals.PluginManager.GetID(TExternalPlugin(lstPlugins.Items[i].Data).Identifier);
      if EP <> nil then
      begin
        EP.Order := i;
        EP.Active := lstPlugins.Items[i].Checked;
      end;
    end;
  end;

  AppGlobals.Unlock;

  for i := 0 to lstPlugins.Items.Count - 1 do
    TPlugin(lstPlugins.Items[i].Data).Active := lstPlugins.Items[i].Checked;

  inherited;
end;

procedure TfrmSettings.FormActivate(Sender: TObject);
begin
  if FBrowseDir then
  begin
    btnBrowse.Click;
    FBrowseDir := False;
  end;
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

function TfrmSettings.GetNewID: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to lstPlugins.Items.Count - 1 do
  begin
    if TPluginBase(lstPlugins.Items[i].Data) is TExternalPlugin then
      if TExternalPlugin(lstPlugins.Items[i].Data).Identifier = Result then
      begin
        Inc(Result);
        Continue;
      end;
  end;
  for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
  begin
    if TPluginBase(AppGlobals.PluginManager.Plugins[i]) is TExternalPlugin then
      if TExternalPlugin(AppGlobals.PluginManager.Plugins[i]).Identifier = Result then
      begin
        Inc(Result);
        Continue;
      end;
  end;
end;

procedure TfrmSettings.Label2Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When enabled streamWriter will search for silence before saving files, ' +
                   'this will only work for streams that have silence between played tracks and if bass.dll was loaded.'#13#10 +
                   'You can test your settings for detecting silence in the manual cut view by ' +
                   'right-clicking a saved track, selecting ''Cut'' and using the corresponding toolbar button in the opened tab.'), _('Info'), MB_ICONINFORMATION);
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

procedure TfrmSettings.lstPluginsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  P1, P2: TPluginBase;
begin
  inherited;
  P1 := TPluginBase(Item1.Data);
  P2 := TPluginBase(Item2.Data);
  Compare := CmpInt(P1.Order, P2.Order);
end;

procedure TfrmSettings.lstPluginsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  cmdConfigure.Enabled := False;
  btnHelp.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data) is TPlugin) and (TPlugin(Item.Data).Help <> '');
  btnRemove.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data) is TExternalPlugin);

  btnMoveUp.Enabled := (Item <> nil) and Selected and (Item.Index > 0);
  btnMoveDown.Enabled := (Item <> nil) and Selected and (Item.Index < lstPlugins.Items.Count - 1);

  if Selected and (TPluginBase(Item.Data) is TExternalPlugin) then
  begin
    txtApp.Text := TExternalPlugin(Item.Data).Exe;
    txtAppParams.Text := TExternalPlugin(Item.Data).Params;
    txtApp.Enabled := True;
    txtAppParams.Enabled := True;
    btnBrowseApp.Enabled := True;
    btnRemove.Enabled := True;
  end else
  begin
    txtApp.Text := '';
    txtAppParams.Text := '';
    txtApp.Enabled := False;
    txtAppParams.Enabled := False;
    btnBrowseApp.Enabled := False;
    btnRemove.Enabled := False;
  end;
end;

procedure TfrmSettings.PreTranslate;
begin
  inherited;
  FDefaultActionIdx := lstDefaultAction.ItemIndex;
  FDefaultFilterIdx := lstDefaultFilter.ItemIndex;
end;

procedure TfrmSettings.PostTranslate;
var
  i: Integer;
begin
  inherited;
  lblFilePattern.Caption := _('%s = streamname, %a = artist, %t = title, %n = tracknumber,'#13#10'%d = date song was saved, %i = time song was saved'#13#10 +
                              'Backslashes can be used to seperate directories.');
  lblAppParams.Caption := _('%f = filename (should be quoted using ")');
  if lstPlugins.Selected <> nil then
  begin
    AppGlobals.PluginManager.ReInitPlugins;
    lstPluginsSelectItem(lstPlugins, lstPlugins.Selected, True);
  end;

  for i := 0 to lstPlugins.Items.Count - 1 do
    lstPlugins.Items[i].Caption := TPlugin(lstPlugins.Items[i].Data).Name;

  AppGlobals.PluginManager.ReInitPlugins;
  lstDefaultAction.ItemIndex := FDefaultActionIdx;
  lstDefaultFilter.ItemIndex := FDefaultFilterIdx;
end;

function TfrmSettings.ValidatePattern: string;
var
  Arr: TPatternReplaceArray;
  i: Integer;
begin
  inherited;

  SetLength(Arr, 6);
  Arr[0].C := 'a';
  Arr[0].Replace := _('Artist');
  Arr[1].C := 't';
  Arr[1].Replace := _('Title');
  Arr[2].C := 's';
  Arr[2].Replace := _('Streamname');
  Arr[3].C := 'n';
  Arr[3].Replace := IntToStr(78);
  Arr[4].C := 'd';
  Arr[4].Replace := FormatDateTime('dd.mm.yy', Now);
  Arr[5].C := 'i';
  Arr[5].Replace := FormatDateTime('hh.nn.ss', Now);

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
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, ':', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '*', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '_', [rfReplaceAll]);

  // Sicherstellen, dass am Anfang/Ende kein \ steht
  if Length(Result) > 0 then
    if Result[1] = '\' then
      Result := Copy(Result, 2, Length(Result) - 1);
  if Length(Result) > 0 then
    if Result[Length(Result)] = '\' then
      Result := Copy(Result, 1, Length(Result) - 1);
  Result := Result + '.mp3';
end;

procedure TfrmSettings.RegisterPages;
begin
  FPageList.Add(TPage.Create(_('Settings'), pnlMain, 'PROPERTIES'));
  FPageList.Add(TPage.Create(_('Streams'), pnlStreams, 'START'));
  FPageList.Add(TPage.Create(_('Cut'), pnlCut, 'CUT'));
  FPageList.Add(TPage.Create(_('Postprocessing'), pnlPlugins, 'LIGHTNING'));
  FPageList.Add(TPage.Create(_('Advanced'), pnlAdvanced, 'MISC'));
  inherited;
end;

procedure TfrmSettings.txtAppParamsChange(Sender: TObject);
begin
  inherited;
  if (lstPlugins.Selected <> nil) and txtAppParams.Focused then
    TExternalPlugin(lstPlugins.Selected.Data).Params := txtAppParams.Text;
end;

procedure TfrmSettings.txtFilePatternChange(Sender: TObject);
begin
  inherited;
  txtPreview.Text := ValidatePattern;
end;

procedure TfrmSettings.btnAddUpClick(Sender: TObject);
var
  Item: TListItem;
  Plugin: TExternalPlugin;
begin
  inherited;
  if dlgOpen.Execute then
  begin
    if FileExists(dlgOpen.FileName) then
    begin
      Item := lstPlugins.Items.Add;
      Item.Caption := ExtractFileName(dlgOpen.FileName);
      Plugin := TExternalPlugin.Create(dlgOpen.FileName, '"%f"', True, GetNewID, 0);
      FTemporaryPlugins.Add(Plugin);
      Item.Checked := Plugin.Active;
      Item.Data := Plugin;
      Item.ImageIndex := 1;
      Item.Selected := True;
    end;
  end;
end;

procedure TfrmSettings.btnBrowseAppClick(Sender: TObject);
begin
  inherited;
  if dlgOpen.Execute then
  begin
    if FileExists(dlgOpen.FileName) then
    begin
      txtApp.Text := dlgOpen.FileName;
      lstPlugins.Selected.Caption := ExtractFileName(dlgOpen.FileName);
      TExternalPlugin(lstPlugins.Selected.Data).Exe := dlgOpen.FileName;
    end;
  end;
end;

procedure TfrmSettings.btnBrowseClick(Sender: TObject);
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

procedure TfrmSettings.btnHelpClick(Sender: TObject);
begin
  if lstPlugins.Selected <> nil then
    MsgBox(Handle, TPlugin(lstPlugins.Selected.Data).Help, _('Help'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.btnMoveClick(Sender: TObject);
var
  Item: TListItem;
begin
  if lstPlugins.Selected = nil then
    Exit;

  lstPlugins.Items.BeginUpdate;
  if Sender = btnMoveUp then
    Item := lstPlugins.Items.Insert(lstPlugins.Selected.Index - 1)
  else
    Item := lstPlugins.Items.Insert(lstPlugins.Selected.Index + 2);
  Item.Caption := lstPlugins.Selected.Caption;
  Item.Checked := lstPlugins.Selected.Checked;;
  Item.Data := lstPlugins.Selected.Data;
  Item.ImageIndex := lstPlugins.Selected.ImageIndex;
  lstPlugins.DeleteSelected;

  Item.Selected := True;
  lstPlugins.Items.EndUpdate;
end;

procedure TfrmSettings.btnRemoveClick(Sender: TObject);
begin
  if lstPlugins.Selected <> nil then
  begin
    FTemporaryPlugins.Remove(TExternalPlugin(lstPlugins.Selected.Data));
    TExternalPlugin(lstPlugins.Selected.Data).Free;
    lstPlugins.Selected.Delete;
  end;
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
    btnBrowse.Click;
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

  if (StrToIntDef(txtSilenceLevel.Text, -1) > 100) or (StrToIntDef(txtSilenceLevel.Text, -1) < 1) then
  begin
    MsgBox(Handle, _('Please enter the maximum volume level for silence detection as a value ranging from 1 to 100.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtSilenceLevel.Parent)));
    txtSilenceLevel.SetFocus;
    Exit;
  end;

  if StrToIntDef(txtSilenceLength.Text, -1) < 20 then
  begin
    MsgBox(Handle, _('Please enter the minimum length for silence (at least 20 ms).'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtSilenceLength.Parent)));
    txtSilenceLength.SetFocus;
    Exit;
  end;

  if Trim(txtSongBuffer.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter the size of the buffer that should be added to every beginning/end of saved titles.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtSongBuffer.Parent)));
    txtSongBuffer.SetFocus;
    Exit;
  end;

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
  Label10.Enabled := chkSearchSilence.Checked;
  Label12.Enabled := chkSearchSilence.Checked;
  Label13.Enabled := chkSearchSilence.Checked;
end;

procedure TfrmSettings.chkSkipShortClick(Sender: TObject);
begin
  inherited;
  txtShortSongSize.Enabled := chkSkipShort.Checked;
end;

procedure TfrmSettings.chkTrayClick(Sender: TObject);
begin
  inherited;

  optClose.Enabled := chkTray.Checked;
  optMinimize.Enabled := chkTray.Checked;
end;

end.
