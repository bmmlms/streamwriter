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
  ShlObj, AppData, LanguageObjects, Functions, GUIFunctions, SettingsBase;

type
  TfrmSettings = class(TfrmSettingsBase)
    pnlStreams: TPanel;
    pnlMain: TPanel;
    txtShortSongSize: TLabeledEdit;
    txtSongBuffer: TLabeledEdit;
    txtDir: TLabeledEdit;
    cmdBrowse: TSpeedButton;
    chkTrayClose: TCheckBox;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    chkSkipShort: TCheckBox;
    chkSeperateDirs: TCheckBox;
    chkRelay: TCheckBox;
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
    Label3: TLabel;
    lstDefaultAction: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label6: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdBrowseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Label11Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    FBrowseDir: Boolean;
    FRelayChanged: Boolean;
  protected
    procedure RegisterPages; override;
    procedure Finish; override;
    function CanFinish: Boolean; override;
    procedure SetText; override;
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

  ClientWidth := 390;
  ClientHeight := 360;

  Language.Translate(Self, SetText);

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
  txtDir.Text := AppGlobals.Dir;
  //chkMetaOnly.Checked := AppGlobals.MetaOnly;
  chkSeperateDirs.Checked := AppGlobals.SeperateDirs;
  chkSkipShort.Checked := AppGlobals.SkipShort;
  chkTrayClose.Checked := AppGlobals.TrayClose;
  chkRelay.Checked := AppGlobals.Relay;
  chkSubmitStreams.Checked := AppGlobals.SubmitStreams;
  //txtMaxBufSize.Text := IntToStr(AppGlobals.MaxBufSize);
  txtShortSongSize.Text := IntToStr(AppGlobals.ShortSize);
  txtSongBuffer.Text := IntToStr(AppGlobals.SongBuffer);
  txtMaxRetries.Text := IntToStr(AppGlobals.MaxRetries);
  txtRetryDelay.Text := IntToStr(AppGlobals.RetryDelay);
  txtMinDiskSpace.Text := IntToStr(AppGlobals.MinDiskSpace);

  lstDefaultAction.Items.Add(_('Start/stop recording'));
  lstDefaultAction.Items.Add(_('Listen to stream'));
  lstDefaultAction.Items.Add(_('Listen to relay'));
  lstDefaultAction.Items.Add(_('Listen to recorded file'));
  lstDefaultAction.ItemIndex := Integer(AppGlobals.DefaultAction);

  AppGlobals.Unlock;

  for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
  begin
    Item := lstPlugins.Items.Add;
    Item.GroupID := 0;
    Item.Caption := AppGlobals.PluginManager.Plugins[i].Name;
    Item.Data := AppGlobals.PluginManager.Plugins[i];
    Item.Checked := AppGlobals.PluginManager.Plugins[i].Active;
  end;

  if not DirectoryExists(txtDir.Text) then
    txtDir.Text := '';
end;

procedure TfrmSettings.Finish;
begin
  if AppGlobals.Relay <> chkRelay.Checked then
    FRelayChanged := True;

  AppGlobals.Lock;
  AppGlobals.Dir := txtDir.Text;
  AppGlobals.SeperateDirs := chkSeperateDirs.Checked;
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
  AppGlobals.Unlock;

{
  for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
  begin
    Item := lstPlugins.Items.Add;
    Item.GroupID := 0;
    Item.Caption := AppGlobals.PluginManager.Plugins[i].Name;
    Item.Data := AppGlobals.PluginManager.Plugins[i];
    Item.Checked := AppGlobals.PluginManager.Plugins[i].Active;
  end;
}

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
  MsgBox(Handle, _('When enabled every stream unknown to streamWriter will be submitted to the stream database. This helps populating the stream database so streamWriter''s browser will be kept up to date. No personal information will be sent.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.Label8Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When a title was recorded and it''s size is below the set limit, it will not be saved to disk.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.Label9Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When a title is saved the entered amount of bytes of the stream will be added to the beginning and the end of the song so the song will be complete if the server announces the title change too early/late.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.RegisterPages;
begin
  FPageList.Add(TPage.Create('&Settings', pnlMain, 'PROPERTIES'));
  FPageList.Add(TPage.Create('S&treams', pnlStreams, 'START'));
  FPageList.Add(TPage.Create('&Plugins', pnlPlugins, 'PLUGINS'));
  FPageList.Add(TPage.Create('&Advanced', pnlAdvanced, 'MISC'));
  inherited;
end;

procedure TfrmSettings.SetText;
begin
  inherited;
  lstPlugins.Groups[0].Header := _('Post-Processing');
  AppGlobals.PluginManager.ReInitPlugins;
end;

function TfrmSettings.CanFinish: Boolean;
begin
  Result := False;

  if not inherited then
    Exit;

  if not DirectoryExists(txtDir.Text) then
  begin
    MsgBox(Handle, _('The selected folder for saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
    SetPage(pnlMain);
    Exit;
  end;

  if Trim(txtShortSongSize.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter the maximum size for songs that should be consireded as ads.'), _('Info'), MB_ICONINFORMATION);
    SetPage(pnlStreams);
    Exit;
  end;

  if Trim(txtSongBuffer.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter the size of the buffer that should be added to every beginning/end of saved titles.'), _('Info'), MB_ICONINFORMATION);
    SetPage(pnlStreams);
    Exit;
  end;

  Result := True;
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
