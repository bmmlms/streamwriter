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
  Plugins, StrUtils, DynBASS, ICEClient, Generics.Collections, Menus,
  MsgDlg;

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
    lblHelpSubmitStreams: TLabel;
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
    lblDefaultFilter: TLabel;
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
    pnlHotkeys: TPanel;
    lstHotkeys: TListView;
    txtHotkey: THotKey;
    Label9: TLabel;
    txtDir: TLabeledEdit;
    btnBrowse: TSpeedButton;
    chkSeparateTracks: TCheckBox;
    chkSaveStreamsToMemory: TCheckBox;
    chkOnlyIfCut: TCheckBox;
    chkOnlySaveFull: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure lblHelpSubmitStreamsClick(Sender: TObject);
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
    procedure txtHotkeyChange(Sender: TObject);
    procedure lstHotkeysChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure txtShortSongSizeChange(Sender: TObject);
    procedure txtSilenceLevelChange(Sender: TObject);
    procedure txtSilenceLengthChange(Sender: TObject);
    procedure txtSongBufferChange(Sender: TObject);
    procedure txtMaxRetriesChange(Sender: TObject);
    procedure txtRetryDelayChange(Sender: TObject);
    procedure chkDeleteStreamsClick(Sender: TObject);
    procedure chkAddSavedToIgnoreClick(Sender: TObject);
    procedure lstDefaultFilterChange(Sender: TObject);
    procedure chkSeparateTracksClick(Sender: TObject);
    procedure chkSaveStreamsToMemoryClick(Sender: TObject);
    procedure chkOnlyIfCutClick(Sender: TObject);
    procedure chkOnlySaveFullClick(Sender: TObject);
  private
    FInitialized: Boolean;
    FBrowseDir: Boolean;
    FRelayChanged: Boolean;
    FDefaultActionIdx: Integer;
    FDefaultFilterIdx: Integer;
    FTemporaryPlugins: TList<TPluginBase>;
    FStreamSettings: TStreamSettingsArray;
    FIgnoreFieldList: TList;
    function ValidatePattern: string;
    function GetNewID: Integer;
    procedure BuildHotkeys;
    procedure RemoveGray(C: TControl);
  protected
    procedure RegisterPages; override;
    procedure Finish; override;
    function CanFinish: Boolean; override;
    procedure PreTranslate; override;
    procedure PostTranslate; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent; BrowseDir: Boolean = False); reintroduce; overload;
    constructor Create(AOwner: TComponent; StreamSettings: TStreamSettingsArray); overload;
    destructor Destroy; override;
    property RelayChanged: Boolean read FRelayChanged;
    property StreamSettings: TStreamSettingsArray read FStreamSettings;
  end;

implementation

{$R *.dfm}

constructor TfrmSettings.Create(AOwner: TComponent; BrowseDir: Boolean = False);
  procedure AddField(F: TControl);
  begin
    if FIgnoreFieldList.IndexOf(F) = -1 then
      FIgnoreFieldList.Add(F);
  end;

  procedure SetFields;
  var
    i: Integer;
    S: TStreamSettings;
    F, ShowDialog: Boolean;
  begin
    if Length(FStreamSettings) <= 1 then
      Exit;

    ShowDialog := False;
    S := FStreamSettings[0];

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.FilePattern <> FStreamSettings[i].FilePattern then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtFilePattern);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.DeleteStreams <> FStreamSettings[i].DeleteStreams then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkDeleteStreams);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.AddSavedToIgnore <> FStreamSettings[i].AddSavedToIgnore then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkAddSavedToIgnore);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SkipShort <> FStreamSettings[i].SkipShort then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkSkipShort);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SearchSilence <> FStreamSettings[i].SearchSilence then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkSearchSilence);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SilenceLevel <> FStreamSettings[i].SilenceLevel then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtSilenceLevel);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SilenceLength <> FStreamSettings[i].SilenceLength then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtSilenceLength);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.ShortSize <> FStreamSettings[i].ShortSize then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtShortSongSize);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SongBuffer <> FStreamSettings[i].SongBuffer then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtSongBuffer);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.MaxRetries <> FStreamSettings[i].MaxRetries then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtMaxRetries);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.RetryDelay <> FStreamSettings[i].RetryDelay then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtRetryDelay);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.Filter <> FStreamSettings[i].Filter then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(lstDefaultFilter);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SeparateTracks <> FStreamSettings[i].SeparateTracks then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkSeparateTracks);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SaveToMemory <> FStreamSettings[i].SaveToMemory then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkSaveStreamsToMemory);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.OnlySaveFull <> FStreamSettings[i].OnlySaveFull then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkOnlySaveFull);

    // Gegen die Warnung..
    if ShowDialog then
    begin

    end;
  end;

  procedure SetGray;
  var
    i: Integer;
  begin
    if FIgnoreFieldList = nil then
      Exit;

    for i := 0 to FIgnoreFieldList.Count - 1 do
      if (TControl(FIgnoreFieldList[i]) is TEdit) or (TControl(FIgnoreFieldList[i]) is TLabeledEdit) then
      begin
        TEdit(FIgnoreFieldList[i]).Color := clGrayText;
      end else if TControl(FIgnoreFieldList[i]) is TCheckBox then
      begin
        TCheckBox(FIgnoreFieldList[i]).State := cbGrayed;
      end else if TControl(FIgnoreFieldList[i]) is TComboBox then
      begin

      end;
  end;
var
  i: Integer;
  Item: TListItem;
  B: TBitmap;
  Settings: TStreamSettings;
begin
  FUseTree := True;

  if Length(FStreamSettings) = 0 then
  begin
    Settings := AppGlobals.StreamSettings.Copy;
    TfrmMsgDlg.ShowMsg(Self, _('Settings from the categories "Streams", "Cut" and "Advanced" configured in the general settings window are only applied to new streams you add to the list.'#13#10 +
                               'To change those settings for streams in the list, select these streams, then right-click one of them and select "Settings" from the popupmenu.'), 1, 4);
  end else
    Settings := FStreamSettings[0].Copy;

  try
    inherited Create(AOwner, Length(FStreamSettings) = 0);

    FBrowseDir := BrowseDir;

    SetFields;

    ClientWidth := 480;
    ClientHeight := 435;

    lstDefaultAction.ItemIndex := Integer(AppGlobals.DefaultAction);
    lstDefaultFilter.ItemIndex := Integer(Settings.Filter);
    chkSeparateTracks.Checked := Settings.SeparateTracks;
    chkSaveStreamsToMemory.Checked := Settings.SaveToMemory;
    chkOnlySaveFull.Checked := Settings.OnlySaveFull;

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
    txtFilePattern.Text := Settings.FilePattern;
    txtDir.Text := AppGlobals.Dir;
    chkDeleteStreams.Checked := Settings.DeleteStreams;
    chkAddSavedToIgnore.Checked := Settings.AddSavedToIgnore;

    chkSkipShort.Checked := Settings.SkipShort;
    chkSearchSilence.Checked := Settings.SearchSilence;

    chkSearchSilenceClick(nil);

    chkTray.Checked := AppGlobals.Tray;
    optClose.Checked := not AppGlobals.TrayOnMinimize;
    optMinimize.Checked := AppGlobals.TrayOnMinimize;

    chkTrayClick(nil);

    chkSubmitStreams.Checked := AppGlobals.SubmitStreams;
    txtShortSongSize.Text := IntToStr(Settings.ShortSize);
    txtSongBuffer.Text := IntToStr(Settings.SongBuffer);
    txtMaxRetries.Text := IntToStr(Settings.MaxRetries);
    txtRetryDelay.Text := IntToStr(Settings.RetryDelay);
    txtMinDiskSpace.Text := IntToStr(AppGlobals.MinDiskSpace);

    txtSilenceLevel.Text := IntToStr(Settings.SilenceLevel);
    txtSilenceLength.Text := IntToStr(Settings.SilenceLength);
    AppGlobals.Unlock;

    FTemporaryPlugins := TList<TPluginBase>.Create;

    for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
    begin
      Item := lstPlugins.Items.Add;
      Item.Caption := AppGlobals.PluginManager.Plugins[i].Name;
      Item.Checked := AppGlobals.PluginManager.Plugins[i].Active;
      Item.Data := AppGlobals.PluginManager.Plugins[i].Copy;
      FTemporaryPlugins.Add(TPluginBase(Item.Data));
      if AppGlobals.PluginManager.Plugins[i] is TDLLPlugin then
      begin
        Item.ImageIndex := 0;
      end else if AppGlobals.PluginManager.Plugins[i] is TInternalPlugin then
      begin
        Item.ImageIndex := 0;
      end else
      begin
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

    BuildHotkeys;

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

    SetGray;

    if chkSaveStreamsToMemory.Checked then
    begin
      chkSeparateTracks.Enabled := False;
      chkSeparateTracks.Checked := True;
      chkDeleteStreams.Enabled := False;
      chkDeleteStreams.Checked := False;
    end;

    if not chkSeparateTracks.Checked then
    begin
      chkDeleteStreams.Enabled := False;
      chkDeleteStreams.Checked := False;

      chkOnlySaveFull.Enabled := False;
    end;

    EnablePanel(pnlCut, chkSaveStreamsToMemory.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled));

    FInitialized := True;
  finally
    Settings.Free;
  end;
end;

constructor TfrmSettings.Create(AOwner: TComponent;
  StreamSettings: TStreamSettingsArray);
var
  i: Integer;
begin
  FIgnoreFieldList := TList.Create;

  SetLength(FStreamSettings, Length(StreamSettings));
  for i := 0 to Length(StreamSettings) - 1 do
  begin
    FStreamSettings[i] := StreamSettings[i].Copy;
  end;

  Create(AOwner, False);

  lblTop.Caption := _('Stream settings');

  chkSubmitStreams.Visible := False;
  lblHelpSubmitStreams.Visible := False;
end;

destructor TfrmSettings.Destroy;
var
  i: Integer;
begin
  FIgnoreFieldList.Free;

  for i := 0 to FTemporaryPlugins.Count - 1 do
    FTemporaryPlugins[i].Free;
  FTemporaryPlugins.Free;

  for i := 0 to Length(FStreamSettings) - 1 do
    FStreamSettings[i].Free;

  inherited;
end;

procedure TfrmSettings.Finish;
var
  i, n: Integer;
  Plugin: TPluginBase;
  EP: TExternalPlugin;
  Item: TListItem;
begin
  if Length(FStreamSettings) > 0 then
  begin
    for i := 0 to Length(FStreamSettings) - 1 do
    begin
      if FIgnoreFieldList.IndexOf(txtFilePattern) = -1 then
        FStreamSettings[i].FilePattern := txtFilePattern.Text;

      if FIgnoreFieldList.IndexOf(chkDeleteStreams) = -1 then
        FStreamSettings[i].DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;

      if FIgnoreFieldList.IndexOf(chkAddSavedToIgnore) = -1 then
        FStreamSettings[i].AddSavedToIgnore := chkAddSavedToIgnore.Checked;

      if FPageList.Find(pnlCut).Node.Enabled then
      begin
        if FIgnoreFieldList.IndexOf(chkSkipShort) = -1 then
          FStreamSettings[i].SkipShort := chkSkipShort.Checked;

        if FIgnoreFieldList.IndexOf(txtSongBuffer) = -1 then
          FStreamSettings[i].SongBuffer := StrToIntDef(txtSongBuffer.Text, 0);

        if FIgnoreFieldList.IndexOf(txtShortSongSize) = -1 then
          FStreamSettings[i].ShortSize := StrToIntDef(txtShortSongSize.Text, 1500);

        if FIgnoreFieldList.IndexOf(chkSearchSilence) = -1 then
          FStreamSettings[i].SearchSilence := chkSearchSilence.Checked;

        if FIgnoreFieldList.IndexOf(txtSilenceLevel) = -1 then
          FStreamSettings[i].SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 5);

        if FIgnoreFieldList.IndexOf(txtSilenceLength) = -1 then
          FStreamSettings[i].SilenceLength := StrToIntDef(txtSilenceLength.Text, 100);
      end;

      if FIgnoreFieldList.IndexOf(txtMaxRetries) = -1 then
        FStreamSettings[i].MaxRetries := StrToIntDef(txtMaxRetries.Text, 100);

      if FIgnoreFieldList.IndexOf(txtRetryDelay) = -1 then
        FStreamSettings[i].RetryDelay := StrToIntDef(txtRetryDelay.Text, 5);

      if FIgnoreFieldList.IndexOf(lstDefaultFilter) = -1 then
        FStreamSettings[i].Filter := TUseFilters(lstDefaultFilter.ItemIndex);

      if FIgnoreFieldList.IndexOf(chkSeparateTracks) = -1 then
        FStreamSettings[i].SeparateTracks := chkSeparateTracks.Checked and chkSeparateTracks.Enabled;

      if FIgnoreFieldList.IndexOf(chkSaveStreamsToMemory) = -1 then
        FStreamSettings[i].SaveToMemory := chkSaveStreamsToMemory.Checked;

      if FIgnoreFieldList.IndexOf(chkOnlySaveFull) = -1 then
        FStreamSettings[i].OnlySaveFull := chkOnlySaveFull.Checked;
    end;
  end else
  begin
    AppGlobals.Lock;
    AppGlobals.StreamSettings.FilePattern := txtFilePattern.Text;
    AppGlobals.StreamSettings.DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;
    AppGlobals.StreamSettings.AddSavedToIgnore := chkAddSavedToIgnore.Checked;
    AppGlobals.StreamSettings.SkipShort := chkSkipShort.Checked;
    AppGlobals.StreamSettings.SongBuffer := StrToIntDef(txtSongBuffer.Text, 0);
    AppGlobals.StreamSettings.ShortSize := StrToIntDef(txtShortSongSize.Text, 1500);
    AppGlobals.StreamSettings.MaxRetries := StrToIntDef(txtMaxRetries.Text, 100);
    AppGlobals.StreamSettings.RetryDelay := StrToIntDef(txtRetryDelay.Text, 5);
    AppGlobals.StreamSettings.Filter := TUseFilters(lstDefaultFilter.ItemIndex);
    if BassLoaded then
      AppGlobals.StreamSettings.SearchSilence := chkSearchSilence.Checked;
    AppGlobals.StreamSettings.SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 5);
    AppGlobals.StreamSettings.SilenceLength := StrToIntDef(txtSilenceLength.Text, 100);
    AppGlobals.StreamSettings.SeparateTracks := chkSeparateTracks.Checked and chkSeparateTracks.Enabled;
    AppGlobals.StreamSettings.SaveToMemory := chkSaveStreamsToMemory.Checked;
    AppGlobals.StreamSettings.OnlySaveFull := chkOnlySaveFull.Checked;

    AppGlobals.Dir := txtDir.Text;
    AppGlobals.Tray := chkTray.Checked;
    AppGlobals.TrayOnMinimize := optMinimize.Checked;
    AppGlobals.SubmitStreams := chkSubmitStreams.Checked;
    AppGlobals.MinDiskSpace := StrToIntDef(txtMinDiskSpace.Text, 5);
    AppGlobals.DefaultAction := TClientActions(lstDefaultAction.ItemIndex);

    if lstHotkeys.Items[0].SubItems[0] <> '' then
      AppGlobals.ShortcutPlay := TextToShortCut(lstHotkeys.Items[0].SubItems[0]);
    if lstHotkeys.Items[1].SubItems[0] <> '' then
      AppGlobals.ShortcutStop := TextToShortCut(lstHotkeys.Items[1].SubItems[0]);
    if lstHotkeys.Items[2].SubItems[0] <> '' then
      AppGlobals.ShortcutNext := TextToShortCut(lstHotkeys.Items[2].SubItems[0]);
    if lstHotkeys.Items[3].SubItems[0] <> '' then
      AppGlobals.ShortcutPrev := TextToShortCut(lstHotkeys.Items[3].SubItems[0]);

    for i := 0 to FTemporaryPlugins.Count - 1 do
    begin
      Plugin := AppGlobals.PluginManager.Find(FTemporaryPlugins[i]);

      if Plugin = nil then
      begin
        // Ein neues Plugin kann nur TExternalPlugin sein.
        Plugin := FTemporaryPlugins[i].Copy;
        AppGlobals.PluginManager.Plugins.Add(Plugin);
      end;

      Item := nil;
      for n := 0 to lstPlugins.Items.Count - 1 do
        if lstPlugins.Items[n].Data = FTemporaryPlugins[i] then
        begin
          Item := lstPlugins.Items[n];
          Break;
        end;

      Plugin.OnlyIfCut := FTemporaryPlugins[i].OnlyIfCut;
      Plugin.Order := Item.Index;
      Plugin.Active := Item.Checked;

      if Plugin is TExternalPlugin then
      begin
        EP := TExternalPlugin(Plugin);
        EP.Exe := TExternalPlugin(FTemporaryPlugins[i]).Exe;
        EP.Params := TExternalPlugin(FTemporaryPlugins[i]).Params;
      end;
    end;

    // Vom Benutzer entfernte Plugins aus den echten Plugins entfernen..
    for i := AppGlobals.PluginManager.Plugins.Count - 1 downto 0 do
    begin
      if AppGlobals.PluginManager.Plugins[i] is TExternalPlugin then
      begin
        EP := nil;
        for n := 0 to FTemporaryPlugins.Count - 1 do
          if FTemporaryPlugins[n] is TExternalPlugin then
            if TExternalPlugin(FTemporaryPlugins[n]).Identifier = TExternalPlugin(AppGlobals.PluginManager.Plugins[i]).Identifier then
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

    AppGlobals.Unlock;
  end;

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

  lstHotkeys.Columns[0].Width := lstHotkeys.ClientWidth - 130;
  lstHotkeys.Columns[1].Width := lstHotkeys.ClientWidth - lstHotkeys.Columns[0].Width - 25;
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

procedure TfrmSettings.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_F1 then
    ShellExecute(Handle, 'open', PChar(AppGlobals.ProjectHelpLink), '', '', 1);
end;

procedure TfrmSettings.Label2Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When enabled streamWriter will search for silence before saving files, ' +
                   'this will only work for streams that have silence between played tracks and if bass.dll was loaded.'#13#10 +
                   'You can test your settings for detecting silence in the manual cut view by ' +
                   'right-clicking a saved track, selecting ''Cut'' and using the corresponding toolbar button in the opened tab.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.lblHelpSubmitStreamsClick(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When enabled every stream unknown to streamWriter will be submitted to the stream database. This helps populating the stream database so streamWriter''s browser will be kept up to date. No personal information will be sent, only the stream''s url.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.Label8Click(Sender: TObject);
begin
  inherited;
  MsgBox(Handle, _('When a title was recorded and it''s size is below the set limit, it will not be saved to disk.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.lstDefaultFilterChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(lstDefaultFilter);
end;

procedure TfrmSettings.lstHotkeysChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  inherited;
  txtHotkey.Enabled := lstHotkeys.Selected <> nil;
  if txtHotkey.Enabled then
  begin
    txtHotkey.HotKey := TextToShortCut(lstHotkeys.Selected.SubItems[0]);
  end else
    txtHotkey.HotKey := 0;
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
  //btnHelp.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data) is TDLLPlugin) and (TDLLPlugin(Item.Data).Help <> '');
  btnHelp.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data).Help <> '');
  btnRemove.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data) is TExternalPlugin);

  btnMoveUp.Enabled := (Item <> nil) and Selected and (Item.Index > 0);
  btnMoveDown.Enabled := (Item <> nil) and Selected and (Item.Index < lstPlugins.Items.Count - 1);

  chkOnlyIfCut.Checked := (Item <> nil) and Selected and TPluginBase(Item.Data).OnlyIfCut;

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
    lstPlugins.Items[i].Caption := TPluginBase(lstPlugins.Items[i].Data).Name;

  BuildHotkeys;

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
  if FStreamSettings = nil then
  begin
    FPageList.Add(TPage.Create(_('Settings'), pnlMain, 'PROPERTIES'));
    FPageList.Add(TPage.Create(_('Streams'), pnlStreams, 'START'));
    FPageList.Add(TPage.Create(_('Cut'), pnlCut, 'CUT'));
    FPageList.Add(TPage.Create(_('Postprocessing'), pnlPlugins, 'LIGHTNING'));
    FPageList.Add(TPage.Create(_('Hotkeys'), pnlHotkeys, 'KEYBOARD'));
    FPageList.Add(TPage.Create(_('Advanced'), pnlAdvanced, 'MISC'));
  end else
  begin
    FPageList.Add(TPage.Create(_('Streams'), pnlStreams, 'START'));
    FPageList.Add(TPage.Create(_('Cut'), pnlCut, 'CUT'));
    FPageList.Add(TPage.Create(_('Advanced'), pnlAdvanced, 'MISC'));
  end;
  inherited;
end;

procedure TfrmSettings.RemoveGray(C: TControl);
begin
  if FIgnoreFieldList = nil then
    Exit;

  FIgnoreFieldList.Remove(C);

  if (TControl(C) is TEdit) or (TControl(C) is TLabeledEdit) then
  begin
    TEdit(C).Color := clWindow;
  end else if TControl(C) is TCheckBox then
  begin

  end else if TControl(C) is TComboBox then
  begin

  end;
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

  if Trim(RemoveFileExt(txtPreview.Text)) = '' then
    txtPreview.Text := '';

  if FInitialized then
    RemoveGray(txtFilePattern);
end;

procedure TfrmSettings.txtHotkeyChange(Sender: TObject);
begin
  inherited;
  lstHotkeys.Selected.SubItems[0] := ShortCutToText(txtHotkey.HotKey);
end;

procedure TfrmSettings.txtMaxRetriesChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtMaxRetries);
end;

procedure TfrmSettings.txtRetryDelayChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtRetryDelay);
end;

procedure TfrmSettings.txtShortSongSizeChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtShortSongSize);
end;

procedure TfrmSettings.txtSilenceLengthChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtSilenceLength);
end;

procedure TfrmSettings.txtSilenceLevelChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtSilenceLevel);
end;

procedure TfrmSettings.txtSongBufferChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtSongBuffer);
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
      Plugin := TExternalPlugin.Create(dlgOpen.FileName, '"%f"', True, False, GetNewID, 0);
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
    MsgBox(Handle, TDLLPlugin(lstPlugins.Selected.Data).Help, _('Help'), MB_ICONINFORMATION);
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

procedure TfrmSettings.BuildHotkeys;
var
  Item: TListItem;
begin
  if lstHotkeys.Items.Count > 0 then
  begin
    lstHotkeys.Items[0].Caption := _('Play');
    lstHotkeys.Items[1].Caption := _('Stop');
    lstHotkeys.Items[2].Caption := _('Next stream');
    lstHotkeys.Items[3].Caption := _('Previous stream');
  end else
  begin
    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Play');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPlay));
    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Stop');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutStop));
    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Next stream');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutNext));
    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Previous stream');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPrev));
  end;
end;

function TfrmSettings.CanFinish: Boolean;
var
  i, n: Integer;
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

  if Trim(RemoveFileExt(ValidatePattern)) = '' then
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

  if FPageList.Find(pnlCut).Node.Enabled then
  begin
    if Trim(txtShortSongSize.Text) = '' then
    begin
      if chkSkipShort.Checked then
      begin
        MsgBox(Handle, _('Please enter the maximum size for songs that should be considered as ads.'), _('Info'), MB_ICONINFORMATION);
        SetPage(FPageList.Find(TPanel(txtShortSongSize.Parent)));
        txtShortSongSize.SetFocus;
        Exit;
      end else
        txtShortSongSize.Text := IntToStr(AppGlobals.StreamSettings.ShortSize);
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

  // Sonst wird kann es zu lange dauern, Clients zu entfernen, wenn der Thread gerade noch schläft.
  if StrToIntDef(txtRetryDelay.Text, 5) > 10 then
    txtRetryDelay.Text := '5';

  for i := 0 to lstHotkeys.Items.Count - 1 do
    for n := 0 to lstHotkeys.Items.Count - 1 do
    begin
      if (lstHotkeys.Items[i] <> lstHotkeys.Items[n]) and
         (lstHotkeys.Items[i].SubItems[0] <> '') and
         (lstHotkeys.Items[i].SubItems[0] = lstHotkeys.Items[n].SubItems[0]) then
      begin
        MsgBox(Handle, _('A hotkey can be defined only once. Please edit the key mappings.'), _('Info'), MB_ICONINFORMATION);
        SetPage(FPageList.Find(pnlHotkeys));
        Exit;
      end;
    end;

  Result := True;
end;

procedure TfrmSettings.chkAddSavedToIgnoreClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkAddSavedToIgnore);
end;

procedure TfrmSettings.chkDeleteStreamsClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkDeleteStreams);
end;

procedure TfrmSettings.chkOnlyIfCutClick(Sender: TObject);
begin
  inherited;
  if (lstPlugins.Selected <> nil) and chkOnlyIfCut.Focused then
    TPluginBase(lstPlugins.Selected.Data).OnlyIfCut := chkOnlyIfCut.Checked;
end;

procedure TfrmSettings.chkOnlySaveFullClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(chkOnlySaveFull);

    if Length(FStreamSettings) > 0 then
      TfrmMsgDlg.ShowMsg(Self, _('When changing this option for a stream which is recording, stop and start recording again for the new setting to become active.'), 1, 5);
  end;
end;

procedure TfrmSettings.chkSeparateTracksClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(chkSeparateTracks);

    chkDeleteStreams.Enabled := chkSeparateTracks.Checked;
    chkDeleteStreams.Checked := (not chkSaveStreamsToMemory.Checked) and AppGlobals.StreamSettings.DeleteStreams;

    chkOnlySaveFull.Enabled := chkSeparateTracks.Checked;

    pnlCut.Enabled := False;
    EnablePanel(pnlCut, chkSeparateTracks.Checked and chkSeparateTracks.Enabled);
    if (not chkSeparateTracks.Checked) or (chkSaveStreamsToMemory.Checked) then
      chkDeleteStreams.Checked := False;

    if (not chkSeparateTracks.Checked) then
      TfrmMsgDlg.ShowMsg(Self, _('When saving streams without saving separate tracks, keep in mind to change the pattern ' +
                                 'for names of saved files, because the variables for artist, title and tracknumber ' +
                                 '(%a, %t, %n) will only be filled with default values.'), 1, 2);
  end;
end;

procedure TfrmSettings.chkSaveStreamsToMemoryClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(chkSaveStreamsToMemory);

    chkSeparateTracks.Enabled := not chkSaveStreamsToMemory.Checked;
    chkSeparateTracks.Checked := True;

    // Weil das hier drüber die Seite abschaltet, schalten wir sie wieder an..
    EnablePanel(pnlCut, chkSaveStreamsToMemory.Checked);
    chkDeleteStreams.Enabled := (not chkSeparateTracks.Checked) or (not chkSaveStreamsToMemory.Checked);
    chkDeleteStreams.Checked := chkDeleteStreams.Enabled and AppGlobals.StreamSettings.DeleteStreams;

    if Length(FStreamSettings) > 0 then
      TfrmMsgDlg.ShowMsg(Self, _('When changing this option for a stream which is recording, stop and start recording again for the new setting to become active.'), 1, 3);
  end;
end;

procedure TfrmSettings.chkSearchSilenceClick(Sender: TObject);
begin
  inherited;

  txtSilenceLevel.Enabled := chkSearchSilence.Checked;
  txtSilenceLength.Enabled := chkSearchSilence.Checked;
  Label10.Enabled := chkSearchSilence.Checked;
  Label12.Enabled := chkSearchSilence.Checked;
  Label13.Enabled := chkSearchSilence.Checked;

  if FInitialized then
    RemoveGray(chkSearchSilence);
end;

procedure TfrmSettings.chkSkipShortClick(Sender: TObject);
begin
  inherited;
  txtShortSongSize.Enabled := chkSkipShort.State <> cbUnchecked;

  if FInitialized then
    RemoveGray(chkSkipShort);
end;

procedure TfrmSettings.chkTrayClick(Sender: TObject);
begin
  inherited;

  optClose.Enabled := chkTray.Checked;
  optMinimize.Enabled := chkTray.Checked;
end;

end.
