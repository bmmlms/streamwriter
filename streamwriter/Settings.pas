{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
  MsgDlg, PngImageList, PngSpeedButton, pngimage, VirtualTrees, Math,
  DataManager, PngBitBtn, DownloadAddons, Logging;

type
  TBlacklistNodeData = record
    Name: string;
  end;
  PBlacklistNodeData = ^TBlacklistNodeData;

  TBlacklistTree = class(TVirtualStringTree)
  private
    FColTitle: TVirtualTreeColumn;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
  public
    constructor Create(AOwner: TComponent; Streams: TStringList); reintroduce;
    destructor Destroy; override;
    procedure UpdateList(List: TStringList);
    procedure RemoveSelected;
  end;

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
    Label3: TLabel;
    pnlCut: TPanel;
    txtSongBuffer: TLabeledEdit;
    txtShortLengthSeconds: TLabeledEdit;
    Label4: TLabel;
    Label5: TLabel;
    chkSkipShort: TCheckBox;
    chkSearchSilence: TCheckBox;
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
    btnAddUp: TButton;
    btnRemove: TButton;
    txtApp: TLabeledEdit;
    txtAppParams: TLabeledEdit;
    lblAppParams: TLabel;
    btnBrowseApp: TSpeedButton;
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
    lblPanelCut: TLabel;
    chkOverwriteSmaller: TCheckBox;
    Label6: TLabel;
    txtSilenceBufferSeconds: TEdit;
    Label15: TLabel;
    PngImageList1: TPngImageList;
    pnlCommunity: TPanel;
    chkAutoTuneIn: TCheckBox;
    chkSubmitStreamInfo: TCheckBox;
    Label2: TLabel;
    chkSubmitStats: TCheckBox;
    Label8: TLabel;
    lstSoundDevice: TComboBox;
    lblSoundDevice: TLabel;
    Label16: TLabel;
    lstMinBitrate: TComboBox;
    Label17: TLabel;
    lstFormat: TComboBox;
    btnHelp: TPngSpeedButton;
    btnMoveDown: TPngSpeedButton;
    btnMoveUp: TPngSpeedButton;
    chkDiscardSmaller: TCheckBox;
    pnlFilenames: TPanel;
    txtPreview: TLabeledEdit;
    txtFilePattern: TLabeledEdit;
    txtFilePatternDecimals: TLabeledEdit;
    lblFilePattern: TLabel;
    Label18: TLabel;
    lstDefaultActionBrowser: TComboBox;
    pnlCommunityBlacklist: TPanel;
    pnlBlacklist: TPanel;
    btnBlacklistRemove: TButton;
    Label19: TLabel;
    chkSnapMain: TCheckBox;
    pnlStreamsAdvanced: TPanel;
    txtTitlePattern: TLabeledEdit;
    btnResetTitlePattern: TPngSpeedButton;
    btnResetFilePattern: TPngSpeedButton;
    btnConfigure: TButton;
    txtIncompleteFilePattern: TLabeledEdit;
    btnResetIncompleteFilePattern: TPngSpeedButton;
    txtIncompletePreview: TLabeledEdit;
    chkRememberRecordings: TCheckBox;
    chkDisplayPlayNotifications: TCheckBox;
    chkAutoTuneInConsiderIgnore: TCheckBox;
    txtRemoveChars: TLabeledEdit;
    pnlBandwidth: TPanel;
    Label11: TLabel;
    txtMaxSpeed: TLabeledEdit;
    chkLimit: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    procedure txtHotkeyChange(Sender: TObject);
    procedure lstHotkeysChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure txtShortLengthSecondsChange(Sender: TObject);
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
    procedure chkOverwriteSmallerClick(Sender: TObject);
    procedure txtSilenceBufferSecondsChange(Sender: TObject);
    procedure lstPluginsResize(Sender: TObject);
    procedure chkAutoTuneInClick(Sender: TObject);
    procedure chkDiscardSmallerClick(Sender: TObject);
    procedure lstHotkeysResize(Sender: TObject);
    procedure txtFilePatternDecimalsChange(Sender: TObject);
    procedure btnBlacklistRemoveClick(Sender: TObject);
    procedure txtTitlePatternChange(Sender: TObject);
    procedure btnResetTitlePatternClick(Sender: TObject);
    procedure btnResetFilePatternClick(Sender: TObject);
    procedure lstPluginsItemChecked(Sender: TObject; Item: TListItem);
    procedure btnConfigureClick(Sender: TObject);
    procedure txtIncompleteFilePatternChange(Sender: TObject);
    procedure txtRemoveCharsChange(Sender: TObject);
    procedure chkLimitClick(Sender: TObject);
  private
    FInitialized: Boolean;
    FBrowseDir: Boolean;
    FRelayChanged: Boolean;
    FDefaultActionIdx: Integer;
    FDefaultActionBrowserIdx: Integer;
    FDefaultFilterIdx: Integer;
    FTemporaryPlugins: TList<TPluginBase>;
    FStreamSettings: TStreamSettingsArray;
    FIgnoreFieldList: TList;
    FLists: TDataLists;
    lstBlacklist: TBlacklistTree;
    btnReset: TBitBtn;
    function ValidatePattern(Text: string): string;
    function GetNewID: Integer;
    procedure BuildHotkeys;
    procedure RemoveGray(C: TControl);
    procedure EnablePanel(Panel: TPanel; Enable: Boolean);
    procedure FillFields(Settings: TStreamSettings);
    procedure SetGray;

    procedure BlacklistTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure BlacklistTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnResetClick(Sender: TObject);
  protected
    procedure RegisterPages; override;
    procedure Finish; override;
    function CanFinish: Boolean; override;
    procedure PreTranslate; override;
    procedure PostTranslate; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists; BrowseDir: Boolean = False); reintroduce; overload;
    constructor Create(AOwner: TComponent; StreamSettings: TStreamSettingsArray); overload;
    destructor Destroy; override;
    property RelayChanged: Boolean read FRelayChanged;
    property StreamSettings: TStreamSettingsArray read FStreamSettings;
  end;

implementation

{$R *.dfm}

constructor TfrmSettings.Create(AOwner: TComponent; Lists: TDataLists; BrowseDir: Boolean = False);
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
      if S.IncompleteFilePattern <> FStreamSettings[i].IncompleteFilePattern then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtIncompleteFilePattern);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.FilePatternDecimals <> FStreamSettings[i].FilePatternDecimals then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtFilePatternDecimals);

    F := False;
    for i := 0 to Length(FStreamSettings) - 1 do
    begin
      if S.RemoveChars <> FStreamSettings[i].RemoveChars then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtRemoveChars);

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
      if S.OverwriteSmaller <> FStreamSettings[i].OverwriteSmaller then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkOverwriteSmaller);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.DiscardSmaller <> FStreamSettings[i].DiscardSmaller then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkDiscardSmaller);

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
      if S.TitlePattern <> FStreamSettings[i].TitlePattern then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtTitlePattern);

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
      if S.SilenceBufferSeconds <> FStreamSettings[i].SilenceBufferSeconds then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtSilenceBufferSeconds);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.ShortLengthSeconds <> FStreamSettings[i].ShortLengthSeconds then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtShortLengthSeconds);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.SongBufferSeconds <> FStreamSettings[i].SongBufferSeconds then
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
var
  i: Integer;
  Item: TListItem;
  B: TBitmap;
  P: TPngImage;
  Settings: TStreamSettings;
begin
  FLists := Lists;

  if Length(FStreamSettings) = 0 then
  begin
    Settings := AppGlobals.StreamSettings.Copy;

    // Wir geben AOwner mit, so dass das MsgDlg zentriert angezeigt wird.
    // Self ist nämlich noch nicht Visible, haben kein Handle, etc..
    TfrmMsgDlg.ShowMsg(TForm(AOwner), _('Settings from the categories "Streams", "Filenames", "Cut" and "Advanced" configured in the general settings window are only applied to new streams you add to the list.'#13#10 +
                                        'To change those settings for streams in the list, select these streams, then right-click one of them and select "Settings" from the popupmenu.'), 4, btOK);
  end else
  begin
    Settings := FStreamSettings[0].Copy;
  end;

  try
    inherited Create(AOwner, Length(FStreamSettings) = 0);

    if Length(FStreamSettings) > 0 then
    begin
      lstSoundDevice.Visible := False;
      lblSoundDevice.Visible := False;

      btnReset := TBitBtn.Create(Self);
      btnReset.Parent := pnlNav;
      btnReset.Caption := '&Apply general settings';
      btnReset.OnClick := btnResetClick;
    end;

    FBrowseDir := BrowseDir;

    SetFields;

    ClientWidth := 510;
    ClientHeight := 435;

    for i := 0 to Self.ControlCount - 1 do
    begin
      if Self.Controls[i] is TPanel then
      begin
        if TPanel(Self.Controls[i]) = pnlLeft then
          Continue;
        Self.Controls[i].Left := 96;
        Self.Controls[i].Top := 36;
        TPanel(Self.Controls[i]).BevelOuter := bvNone;
      end;
    end;


    FillFields(Settings);

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

    B := TBitmap.Create;
    P := TPngImage.Create;
    try
      P.LoadFromResourceName(HInstance, 'ARROWUP');
      btnMoveUp.PngImage := P;
      P.LoadFromResourceName(HInstance, 'ARROWDOWN');
      btnMoveDown.PngImage := P;
      P.LoadFromResourceName(HInstance, 'QUESTION');
      btnHelp.PngImage := P;
      GetBitmap('BROWSE', 2, B);
      btnBrowse.Glyph := B;
      btnBrowseApp.Glyph := B;
    finally
      B.Free;
      P.Free;
    end;

    BuildHotkeys;

    if (Bass.DeviceAvailable) and (Bass.Devices.Count > 0) then
    begin
      for i := 0 to Bass.Devices.Count - 1 do
        lstSoundDevice.Items.Add(Bass.Devices[i]);
      if lstSoundDevice.Items.Count > 0 then
        lstSoundDevice.ItemIndex := 0;
      try
        lstSoundDevice.ItemIndex := AppGlobals.SoundDevice;
      except end;
    end else
    begin
      lstSoundDevice.Style := csDropDown;
      lstSoundDevice.ItemIndex := -1;
      lstSoundDevice.Enabled := False;
      lstSoundDevice.Text := _('(no devices available)');
    end;

    if FLists <> nil then
    begin
      lstBlacklist := TBlacklistTree.Create(Self, FLists.StreamBlacklist);
      lstBlacklist.OnChange := BlacklistTreeChange;
      lstBlacklist.OnKeyDown := BlacklistTreeKeyDown;
      lstBlacklist.Images := PngImageList1;
      lstBlacklist.Parent := pnlBlacklist;
      lstBlacklist.Align := alClient;
    end;

    lblPanelCut.Caption := _('Settings for cutting are only available'#13#10'if ''Save separated tracks'' is enabled.');

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

  Create(AOwner, nil, False);

  lblTop.Caption := _('Stream settings');
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

procedure TfrmSettings.EnablePanel(Panel: TPanel; Enable: Boolean);
var
  i: Integer;
begin
  for i := 0 to Panel.ControlCount - 1 do
    Panel.Controls[i].Visible := Enable;

  if Enable then
    Panel.Tag := 0
  else
    Panel.Tag := 1;

  if Enable then
    lblPanelCut.Visible := False
  else
    lblPanelCut.Visible := True;
end;

procedure TfrmSettings.FillFields(Settings: TStreamSettings);
begin
  lstDefaultAction.ItemIndex := Integer(AppGlobals.DefaultAction);
  lstDefaultActionBrowser.ItemIndex := Integer(AppGlobals.DefaultActionBrowser);
  lstDefaultFilter.ItemIndex := Integer(Settings.Filter);
  chkSeparateTracks.Checked := Settings.SeparateTracks;
  chkSaveStreamsToMemory.Checked := Settings.SaveToMemory;
  chkOnlySaveFull.Checked := Settings.OnlySaveFull;

  Language.Translate(Self, PreTranslate, PostTranslate);

  pnlGeneral.BringToFront;

  FRelayChanged := False;

  AppGlobals.Lock;
  txtFilePattern.Text := Settings.FilePattern;
  txtIncompleteFilePattern.Text := Settings.IncompleteFilePattern;
  txtFilePatternDecimals.Text := IntToStr(Settings.FilePatternDecimals);
  txtRemoveChars.Text := Settings.RemoveChars;


  //if (Length(AppGlobals.Dir) >= 3) and (Copy(AppGlobals.Dir, 1, 2) <> '\\') and (Copy(AppGlobals.Dir, 2, 2) <> ':\') then
  //begin
  //  txtDir.Text := IncludeTrailingBackslash(ExpandFileName(IncludeTrailingBackslash(AppGlobals.Dir)));
  //end else
  txtDir.Text := AppGlobals.Dir;

  chkDeleteStreams.Checked := Settings.DeleteStreams;
  chkAddSavedToIgnore.Checked := Settings.AddSavedToIgnore;
  chkOverwriteSmaller.Checked := Settings.OverwriteSmaller;
  chkDiscardSmaller.Checked := Settings.DiscardSmaller;
  txtTitlePattern.Text := Settings.TitlePattern;

  chkSkipShort.Checked := Settings.SkipShort;
  chkSearchSilence.Checked := Settings.SearchSilence;

  chkSearchSilenceClick(nil);

  chkTray.Checked := AppGlobals.Tray;
  chkSnapMain.Checked := AppGlobals.SnapMain;
  chkRememberRecordings.Checked := AppGlobals.RememberRecordings;
  chkDisplayPlayNotifications.Checked := AppGlobals.DisplayPlayNotifications;
  optClose.Checked := not AppGlobals.TrayOnMinimize;
  optMinimize.Checked := AppGlobals.TrayOnMinimize;

  chkTrayClick(nil);

  chkAutoTuneIn.Checked := AppGlobals.AutoTuneIn;
  chkAutoTuneInConsiderIgnore.Checked := AppGlobals.AutoTuneInConsiderIgnore;
  lstMinBitrate.ItemIndex := AppGlobals.AutoTuneInMinKbps;
  lstFormat.ItemIndex := AppGlobals.AutoTuneInFormat;
  chkSubmitStreamInfo.Checked := AppGlobals.SubmitStreamInfo;
  chkSubmitStats.Checked := AppGlobals.SubmitStats;
  chkLimit.Checked := AppGlobals.LimitSpeed;
  if AppGlobals.MaxSpeed > 0 then
    txtMaxSpeed.Text := IntToStr(AppGlobals.MaxSpeed);

  txtShortLengthSeconds.Text := IntToStr(Settings.ShortLengthSeconds);
  txtSongBuffer.Text := IntToStr(Settings.SongBufferSeconds);
  txtMaxRetries.Text := IntToStr(Settings.MaxRetries);
  txtRetryDelay.Text := IntToStr(Settings.RetryDelay);
  txtMinDiskSpace.Text := IntToStr(AppGlobals.MinDiskSpace);

  txtSilenceLevel.Text := IntToStr(Settings.SilenceLevel);
  txtSilenceLength.Text := IntToStr(Settings.SilenceLength);
  txtSilenceBufferSeconds.Text := IntToStr(Settings.SilenceBufferSeconds);
  AppGlobals.Unlock;

  if not DirectoryExists(txtDir.Text) then
    txtDir.Text := '';

  if not Bass.BassLoaded then
  begin
    chkSearchSilence.Enabled := False;
    chkSearchSilence.Checked := False;
    txtSilenceLevel.Enabled := False;
    txtSilenceLength.Enabled := False;
    txtSilenceBufferSeconds.Enabled := False;
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

  txtShortLengthSeconds.Enabled := chkSkipShort.State <> cbUnchecked;
  EnablePanel(pnlCut, chkSaveStreamsToMemory.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled));
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

      if FIgnoreFieldList.IndexOf(txtIncompleteFilePattern) = -1 then
        FStreamSettings[i].IncompleteFilePattern := txtIncompleteFilePattern.Text;

      if FIgnoreFieldList.IndexOf(txtFilePatternDecimals) = -1 then
        FStreamSettings[i].FilePatternDecimals := StrToIntDef(txtFilePatternDecimals.Text, 3);

      if FIgnoreFieldList.IndexOf(txtRemoveChars) = -1 then
        FStreamSettings[i].RemoveChars := txtRemoveChars.Text;

      if FIgnoreFieldList.IndexOf(chkDeleteStreams) = -1 then
        FStreamSettings[i].DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;

      if FIgnoreFieldList.IndexOf(chkAddSavedToIgnore) = -1 then
        FStreamSettings[i].AddSavedToIgnore := chkAddSavedToIgnore.Checked;

      if FIgnoreFieldList.IndexOf(chkOverwriteSmaller) = -1 then
        FStreamSettings[i].OverwriteSmaller := chkOverwriteSmaller.Checked;

      if FIgnoreFieldList.IndexOf(chkDiscardSmaller) = -1 then
        FStreamSettings[i].DiscardSmaller := chkDiscardSmaller.Checked;

      if FIgnoreFieldList.IndexOf(txtTitlePattern) = -1 then
        FStreamSettings[i].TitlePattern := txtTitlePattern.Text;

      if pnlCut.Tag = 0 then
      begin
        if FIgnoreFieldList.IndexOf(chkSkipShort) = -1 then
          FStreamSettings[i].SkipShort := chkSkipShort.Checked;

        if FIgnoreFieldList.IndexOf(txtSongBuffer) = -1 then
          FStreamSettings[i].SongBufferSeconds := StrToIntDef(txtSongBuffer.Text, 0);

        if FIgnoreFieldList.IndexOf(txtShortLengthSeconds) = -1 then
          FStreamSettings[i].ShortLengthSeconds := StrToIntDef(txtShortLengthSeconds.Text, 45);

        if FIgnoreFieldList.IndexOf(chkSearchSilence) = -1 then
          FStreamSettings[i].SearchSilence := chkSearchSilence.Checked;

        if FIgnoreFieldList.IndexOf(txtSilenceLevel) = -1 then
          FStreamSettings[i].SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 5);

        if FIgnoreFieldList.IndexOf(txtSilenceLength) = -1 then
          FStreamSettings[i].SilenceLength := StrToIntDef(txtSilenceLength.Text, 100);

        if FIgnoreFieldList.IndexOf(txtSilenceBufferSeconds) = -1 then
          FStreamSettings[i].SilenceBufferSeconds := StrToIntDef(txtSilenceBufferSeconds.Text, 3);
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
    AppGlobals.StreamSettings.IncompleteFilePattern := txtIncompleteFilePattern.Text;
    AppGlobals.StreamSettings.FilePatternDecimals := StrToIntDef(txtFilePatternDecimals.Text, 3);
    AppGlobals.StreamSettings.RemoveChars := txtRemoveChars.Text;
    AppGlobals.StreamSettings.DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;
    AppGlobals.StreamSettings.AddSavedToIgnore := chkAddSavedToIgnore.Checked;
    AppGlobals.StreamSettings.OverwriteSmaller := chkOverwriteSmaller.Checked;
    AppGlobals.StreamSettings.DiscardSmaller := chkDiscardSmaller.Checked;

    if pnlCut.Tag = 0 then
    begin
      AppGlobals.StreamSettings.SkipShort := chkSkipShort.Checked;
      AppGlobals.StreamSettings.SongBufferSeconds := StrToIntDef(txtSongBuffer.Text, 0);
      AppGlobals.StreamSettings.ShortLengthSeconds := StrToIntDef(txtShortLengthSeconds.Text, 45);
      AppGlobals.StreamSettings.SearchSilence := chkSearchSilence.Checked;
      AppGlobals.StreamSettings.SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 5);
      AppGlobals.StreamSettings.SilenceLength := StrToIntDef(txtSilenceLength.Text, 100);
      AppGlobals.StreamSettings.SilenceBufferSeconds := StrToIntDef(txtSilenceBufferSeconds.Text, 3);
    end;

    AppGlobals.StreamSettings.MaxRetries := StrToIntDef(txtMaxRetries.Text, 100);
    AppGlobals.StreamSettings.RetryDelay := StrToIntDef(txtRetryDelay.Text, 5);
    AppGlobals.StreamSettings.Filter := TUseFilters(lstDefaultFilter.ItemIndex);

    AppGlobals.StreamSettings.SeparateTracks := chkSeparateTracks.Checked and chkSeparateTracks.Enabled;
    AppGlobals.StreamSettings.SaveToMemory := chkSaveStreamsToMemory.Checked;
    AppGlobals.StreamSettings.OnlySaveFull := chkOnlySaveFull.Checked;

    if lstSoundDevice.ItemIndex > -1 then
      AppGlobals.SoundDevice := lstSoundDevice.ItemIndex;

    AppGlobals.Dir := txtDir.Text;
    AppGlobals.Tray := chkTray.Checked;
    AppGlobals.SnapMain := chkSnapMain.Checked;
    AppGlobals.RememberRecordings := chkRememberRecordings.Checked;
    AppGlobals.DisplayPlayNotifications := chkDisplayPlayNotifications.Checked;
    AppGlobals.TrayOnMinimize := optMinimize.Checked;

    AppGlobals.AutoTuneIn := chkAutoTuneIn.Checked;
    AppGlobals.AutoTuneInConsiderIgnore := chkAutoTuneInConsiderIgnore.Checked;
    AppGlobals.AutoTuneInMinKbps := lstMinBitrate.ItemIndex;
    AppGlobals.AutoTuneInFormat := lstFormat.ItemIndex;
    AppGlobals.SubmitStreamInfo := chkSubmitStreamInfo.Checked;
    AppGlobals.SubmitStats := chkSubmitStats.Checked;
    AppGlobals.LimitSpeed := chkLimit.Checked;
    if StrToIntDef(txtMaxSpeed.Text, -1) > 0 then
      AppGlobals.MaxSpeed := StrToInt(txtMaxSpeed.Text);

    AppGlobals.MinDiskSpace := StrToIntDef(txtMinDiskSpace.Text, 5);
    AppGlobals.DefaultAction := TClientActions(lstDefaultAction.ItemIndex);
    AppGlobals.DefaultActionBrowser := TBrowserActions(lstDefaultActionBrowser.ItemIndex);

    if lstHotkeys.Items[0].SubItems[0] <> '' then
      AppGlobals.ShortcutPlay := TextToShortCut(lstHotkeys.Items[0].SubItems[0])
    else
      AppGlobals.ShortcutPlay := 0;

    if lstHotkeys.Items[1].SubItems[0] <> '' then
      AppGlobals.ShortcutPause := TextToShortCut(lstHotkeys.Items[1].SubItems[0])
    else
      AppGlobals.ShortcutPause := 0;

    if lstHotkeys.Items[2].SubItems[0] <> '' then
      AppGlobals.ShortcutStop := TextToShortCut(lstHotkeys.Items[2].SubItems[0])
    else
      AppGlobals.ShortcutStop := 0;

    if lstHotkeys.Items[3].SubItems[0] <> '' then
      AppGlobals.ShortcutNext := TextToShortCut(lstHotkeys.Items[3].SubItems[0])
    else
      AppGlobals.ShortcutNext := 0;

    if lstHotkeys.Items[4].SubItems[0] <> '' then
      AppGlobals.ShortcutPrev := TextToShortCut(lstHotkeys.Items[4].SubItems[0])
    else
      AppGlobals.ShortcutPrev := 0;

    if lstHotkeys.Items[5].SubItems[0] <> '' then
      AppGlobals.ShortcutVolUp := TextToShortCut(lstHotkeys.Items[5].SubItems[0])
    else
      AppGlobals.ShortcutVolUp := 0;

    if lstHotkeys.Items[6].SubItems[0] <> '' then
      AppGlobals.ShortcutVolDown := TextToShortCut(lstHotkeys.Items[6].SubItems[0])
    else
      AppGlobals.ShortcutVolDown := 0;

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

      {
      if Plugin is TExternalPlugin then
      begin
        EP := TExternalPlugin(Plugin);
        EP.Exe := TExternalPlugin(FTemporaryPlugins[i]).Exe;
        EP.Params := TExternalPlugin(FTemporaryPlugins[i]).Params;
      end;
      }

      Plugin.Assign(FTemporaryPlugins[i]);
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

    lstBlacklist.UpdateList(FLists.StreamBlacklist);

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

  lblPanelCut.Top := pnlCut.ClientHeight div 2 - lblPanelCut.Height div 2;
  lblPanelCut.Left := pnlCut.ClientWidth div 2 - lblPanelCut.Width div 2;

  txtSilenceBufferSeconds.Left := Label6.Left + Label6.Width + 4;
  Label15.Left := txtSilenceBufferSeconds.Left + txtSilenceBufferSeconds.Width + 4;

  if btnReset <> nil then
  begin
    btnReset.Width := 200;
    btnReset.Height := btnOK.Height;
    btnReset.Left := 4;
    btnReset.Top := btnOK.Top;
  end;
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
    if txtHotkey.CanFocus then
      txtHotkey.SetFocus;
  end else
    txtHotkey.HotKey := 0;
end;

procedure TfrmSettings.lstHotkeysResize(Sender: TObject);
begin
  inherited;

  lstHotkeys.Columns[0].Width := lstHotkeys.ClientWidth div 2;
  lstHotkeys.Columns[1].Width := lstHotkeys.ClientWidth div 2 - 25;
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

procedure TfrmSettings.lstPluginsItemChecked(Sender: TObject;
  Item: TListItem);
var
  Res: Integer;
  DA: TfrmDownloadAddons;
begin
  inherited;

  if TObject(Item.Data) is TInternalPlugin then
  begin
    if Item.Checked and (not TInternalPlugin(Item.Data).FilesInstalled) then
    begin
      Res := MsgBox(Handle, _('The plugin cannot be activated because needed files have not been downloaded.'#13#10'Do you want to download these files now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1);
      if Res = IDYES then
      begin
        DA := TfrmDownloadAddons.Create(Self, TInternalPlugin(Item.Data));
        try
          DA.ShowModal;

          if not DA.Downloaded then
          begin
            if DA.Error then
              MsgBox(Handle, _('An error occured while downloading the file.'), _('Error'), MB_ICONEXCLAMATION);

            lstPlugins.OnItemChecked := nil;
            Item.Checked := False;
            lstPlugins.OnItemChecked := lstPluginsItemChecked;
          end;
        finally
          DA.Free;
        end;
      end else if Res = IDNO then
      begin
        lstPlugins.OnItemChecked := nil;
        Item.Checked := False;
        lstPlugins.OnItemChecked := lstPluginsItemChecked;
      end;
    end;

    // Nochmal initialisieren. Evtl. wurde eben erst die .dll heruntergeladen, dann extrahiert .Initialize() jetzt
    TInternalPlugin(Item.Data).Initialize;

    if Item.Checked and not TInternalPlugin(Item.Data).ReadyForActivate then
    begin
      MsgBox(Handle, _('The plugin is not ready for use. This might happen when it''s files could not be extracted.'), _('Error'), MB_ICONEXCLAMATION);

      lstPlugins.OnItemChecked := nil;
      Item.Checked := False;
      lstPlugins.OnItemChecked := lstPluginsItemChecked;
    end;

    Item.Selected := True;
    btnConfigure.Enabled := Item.Checked and TPluginBase(Item.Data).CanConfigure;
  end;
end;

procedure TfrmSettings.lstPluginsResize(Sender: TObject);
begin
  inherited;

  lstPlugins.Columns[0].Width := lstPlugins.ClientWidth - 25;
end;

procedure TfrmSettings.lstPluginsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnConfigure.Enabled := False;

  btnHelp.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data).Help <> '');
  btnRemove.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data) is TExternalPlugin);

  btnMoveUp.Enabled := (Item <> nil) and Selected and (Item.Index > 0);
  btnMoveDown.Enabled := (Item <> nil) and Selected and (Item.Index < lstPlugins.Items.Count - 1);

  chkOnlyIfCut.Checked := (Item <> nil) and Selected and TPluginBase(Item.Data).OnlyIfCut;
  chkOnlyIfCut.Enabled := (Item <> nil) and Selected;

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

  btnConfigure.Enabled := Item.Checked and TPluginBase(Item.Data).CanConfigure;
end;

procedure TfrmSettings.PreTranslate;
begin
  inherited;
  FDefaultActionIdx := lstDefaultAction.ItemIndex;
  FDefaultActionBrowserIdx := lstDefaultActionBrowser.ItemIndex;
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
  begin
    // Damit Sprache neu gesetzt wird und so..
    TPluginBase(lstPlugins.Items[i].Data).Initialize;
    lstPlugins.Items[i].Caption := TPluginBase(lstPlugins.Items[i].Data).Name;
  end;

  txtPreview.Text := ValidatePattern(txtFilePattern.Text);
  txtIncompletePreview.Text := ValidatePattern(txtIncompleteFilePattern.Text);

  BuildHotkeys;

  AppGlobals.PluginManager.ReInitPlugins;
  lstDefaultAction.ItemIndex := FDefaultActionIdx;
  lstDefaultActionBrowser.ItemIndex := FDefaultActionBrowserIdx;
  lstDefaultFilter.ItemIndex := FDefaultFilterIdx;

  FormResize(Self);
end;

function TfrmSettings.ValidatePattern(Text: string): string;
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
  Arr[3].Replace := Format('%.*d', [StrToIntDef(txtFilePatternDecimals.Text, 3), 78]);
  Arr[4].C := 'd';
  Arr[4].Replace := FormatDateTime('dd.mm.yy', Now);
  Arr[5].C := 'i';
  Arr[5].Replace := FormatDateTime('hh.nn.ss', Now);

  Result := PatternReplace(Text, Arr);

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
    FPageList.Add(TPage.Create('Settings', pnlMain, 'PROPERTIES'));
    FPageList.Add(TPage.Create('Streams', pnlStreams, 'START'));
    FPageList.Add(TPage.Create('Filenames', pnlFilenames, 'FILENAMES'));
    FPageList.Add(TPage.Create('Cut', pnlCut, 'CUT'));
    FPageList.Add(TPage.Create('Postprocessing', pnlPlugins, 'LIGHTNING'));
    FPageList.Add(TPage.Create('Bandwidth', pnlBandwidth, 'BANDWIDTH'));
    FPageList.Add(TPage.Create('Community', pnlCommunity, 'GROUP_PNG'));
    FPageList.Add(TPage.Create('Blacklist', pnlCommunityBlacklist, 'BLACKLIST', FPageList.Find(pnlCommunity)));
    FPageList.Add(TPage.Create('Hotkeys', pnlHotkeys, 'KEYBOARD'));
    FPageList.Add(TPage.Create('Advanced', pnlAdvanced, 'MISC'));
  end else
  begin
    FPageList.Add(TPage.Create('Streams', pnlStreams, 'START'));
    FPageList.Add(TPage.Create('Advanced', pnlStreamsAdvanced, 'MISC', FPageList.Find(pnlStreams)));
    FPageList.Add(TPage.Create('Filenames', pnlFilenames, 'FILENAMES'));
    FPageList.Add(TPage.Create('Cut', pnlCut, 'CUT'));
    FPageList.Add(TPage.Create('Advanced', pnlAdvanced, 'MISC'));
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

procedure TfrmSettings.SetGray;
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

procedure TfrmSettings.txtAppParamsChange(Sender: TObject);
begin
  inherited;
  if (lstPlugins.Selected <> nil) and txtAppParams.Focused then
    TExternalPlugin(lstPlugins.Selected.Data).Params := txtAppParams.Text;
end;

procedure TfrmSettings.txtFilePatternChange(Sender: TObject);
begin
  inherited;
  txtPreview.Text := ValidatePattern(txtFilePattern.Text);

  if Trim(RemoveFileExt(txtPreview.Text)) = '' then
    txtPreview.Text := '';

  if FInitialized then
    RemoveGray(txtFilePattern);
end;

procedure TfrmSettings.txtFilePatternDecimalsChange(Sender: TObject);
begin
  inherited;

  txtPreview.Text := ValidatePattern(txtFilePattern.Text);
  txtIncompletePreview.Text := ValidatePattern(txtIncompleteFilePattern.Text);

  if FInitialized then
    RemoveGray(txtFilePatternDecimals);
end;

procedure TfrmSettings.txtHotkeyChange(Sender: TObject);
begin
  inherited;
  lstHotkeys.Selected.SubItems[0] := ShortCutToText(txtHotkey.HotKey);
end;

procedure TfrmSettings.txtIncompleteFilePatternChange(Sender: TObject);
begin
  inherited;
  txtIncompletePreview.Text := ValidatePattern(txtIncompleteFilePattern.Text);

  if Trim(RemoveFileExt(txtIncompletePreview.Text)) = '' then
    txtIncompletePreview.Text := '';

  if FInitialized then
    RemoveGray(txtIncompleteFilePattern);
end;

procedure TfrmSettings.txtMaxRetriesChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtMaxRetries);
end;

procedure TfrmSettings.txtRemoveCharsChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtRemoveChars);
end;

procedure TfrmSettings.txtRetryDelayChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtRetryDelay);
end;

procedure TfrmSettings.txtShortLengthSecondsChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtShortLengthSeconds);
end;

procedure TfrmSettings.txtSilenceBufferSecondsChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtSilenceBufferSeconds);
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

procedure TfrmSettings.txtTitlePatternChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtTitlePattern);
end;

procedure TfrmSettings.BlacklistTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  btnBlacklistRemove.Enabled := lstBlacklist.SelectedCount > 0;
end;

procedure TfrmSettings.BlacklistTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    btnBlacklistRemoveClick(nil);
  end;
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

procedure TfrmSettings.btnBlacklistRemoveClick(Sender: TObject);
begin
  lstBlacklist.RemoveSelected;
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

procedure TfrmSettings.btnConfigureClick(Sender: TObject);
begin
  inherited;

  if lstPlugins.Selected <> nil then
    TPluginBase(lstPlugins.Selected.Data).Configure(Self, 0, True);
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

procedure TfrmSettings.btnResetClick(Sender: TObject);
begin
  FInitialized := False;
  if FIgnoreFieldList <> nil then
  begin
    while FIgnoreFieldList.Count > 0 do
      RemoveGray(TControl(FIgnoreFieldList[0]));
  end;
  FillFields(AppGlobals.StreamSettings);
  FInitialized := True;
end;

procedure TfrmSettings.btnResetFilePatternClick(Sender: TObject);
begin
  inherited;

  if Sender = btnResetFilePattern then
  begin
    txtFilePattern.Text := '%s\%a - %t';
    RemoveGray(txtFilePattern);
  end else
  begin
    txtIncompleteFilePattern.Text := '%s\%a - %t';
    RemoveGray(txtIncompleteFilePattern);
  end;
end;

procedure TfrmSettings.btnResetTitlePatternClick(Sender: TObject);
begin
  inherited;

  txtTitlePattern.Text := '(?P<a>.*) - (?P<t>.*)';
  RemoveGray(txtTitlePattern);
end;

procedure TfrmSettings.BuildHotkeys;
var
  Item: TListItem;
begin
  if lstHotkeys.Items.Count > 0 then
  begin
    lstHotkeys.Items[0].Caption := _('Play');
    lstHotkeys.Items[1].Caption := _('Pause');
    lstHotkeys.Items[2].Caption := _('Stop');
    lstHotkeys.Items[3].Caption := _('Next stream');
    lstHotkeys.Items[4].Caption := _('Previous stream');
    lstHotkeys.Items[5].Caption := _('Volume up');
    lstHotkeys.Items[6].Caption := _('Volume down');
  end else
  begin
    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Play');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPlay));

    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Pause');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPause));

    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Stop');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutStop));

    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Next stream');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutNext));

    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Previous stream');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPrev));

    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Volume up');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutVolUp));

    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Volume down');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutVolDown));
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

  if Trim(RemoveFileExt(ValidatePattern(txtFilePattern.Text))) = '' then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames, i.e. a preview text must be visible.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtFilePattern.Parent)));
    txtFilePattern.SetFocus;
    Exit;
  end;

  if Trim(RemoveFileExt(ValidatePattern(txtIncompleteFilePattern.Text))) = '' then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for names of incomplete files, i.e. a preview text must be visible.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtIncompleteFilePattern.Parent)));
    txtIncompleteFilePattern.SetFocus;
    Exit;
  end;

  if Trim(txtTitlePattern.Text) = '' then
  begin
    MsgBox(Handle, _('Please enter a regular expression to retrieve artist and title from broadcasted track information.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtTitlePattern.Parent)));
    txtTitlePattern.SetFocus;
    Exit;
  end;

  if (StrToIntDef(txtFilePatternDecimals.Text, -1) > 9) or (StrToIntDef(txtFilePatternDecimals.Text, -1) < 1) then
  begin
    MsgBox(Handle, _('Please enter the minimum count of decimals for tracknumbers in filenames.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtFilePatternDecimals.Parent)));
    txtFilePatternDecimals.SetFocus;
    Exit;
  end;

  if not DirectoryExists(txtDir.Text) then
  begin
    MsgBox(Handle, _('The selected folder for saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtDir.Parent)));
    btnBrowse.Click;
    Exit;
  end;

  if pnlCut.Tag = 0 then
  begin
    if Trim(txtShortLengthSeconds.Text) = '' then
    begin
      if chkSkipShort.Checked then
      begin
        MsgBox(Handle, _('Please enter the maximum length for songs that should be considered as ads.'), _('Info'), MB_ICONINFORMATION);
        SetPage(FPageList.Find(TPanel(txtShortLengthSeconds.Parent)));
        txtShortLengthSeconds.SetFocus;
        Exit;
      end else
        txtShortLengthSeconds.Text := IntToStr(AppGlobals.StreamSettings.ShortLengthSeconds);
    end;

    if (StrToIntDef(txtSilenceLevel.Text, -1) > 100) or (StrToIntDef(txtSilenceLevel.Text, -1) < 1) then
    begin
      if chkSearchSilence.Checked then
      begin
        MsgBox(Handle, _('Please enter the maximum volume level for silence detection as a value ranging from 1 to 100.'), _('Info'), MB_ICONINFORMATION);
        SetPage(FPageList.Find(TPanel(txtSilenceLevel.Parent)));
        txtSilenceLevel.SetFocus;
        Exit;
      end else
        txtSilenceLevel.Text := IntToStr(AppGlobals.StreamSettings.SilenceLevel);
    end;

    if StrToIntDef(txtSilenceLength.Text, -1) < 20 then
    begin
      if chkSearchSilence.Checked then
      begin
        MsgBox(Handle, _('Please enter the minimum length for silence (at least 20 ms).'), _('Info'), MB_ICONINFORMATION);
        SetPage(FPageList.Find(TPanel(txtSilenceLength.Parent)));
        txtSilenceLength.SetFocus;
        Exit;
      end else
        txtSilenceLength.Text := IntToStr(AppGlobals.StreamSettings.SilenceLength);
    end;

    if (StrToIntDef(txtSilenceBufferSeconds.Text, -1) < 1) or (StrToIntDef(txtSilenceBufferSeconds.Text, -1) > 15) then
    begin
      if chkSearchSilence.Checked then
      begin
        MsgBox(Handle, _('Please enter the length in seconds to search for silence at beginning and end of song as a value ranging from 1 to 15.'), _('Info'), MB_ICONINFORMATION);
        SetPage(FPageList.Find(TPanel(txtSilenceBufferSeconds.Parent)));
        txtSilenceBufferSeconds.SetFocus;
        Exit;
      end else
        txtSilenceBufferSeconds.Text := IntToStr(AppGlobals.StreamSettings.SilenceBufferSeconds);
    end;

    if Trim(txtSongBuffer.Text) = '' then
    begin
      MsgBox(Handle, _('Please enter the length of the buffer that should be added to every beginning/end of saved titles if no silence could be found.'), _('Info'), MB_ICONINFORMATION);
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

  if chkLimit.Checked then
    if StrToIntDef(txtMaxSpeed.Text, -1) <= 0 then
    begin
      MsgBox(Handle, _('Please enter the maximum bandwidth in KB/s available to streamWriter.'), _('Info'), MB_ICONINFORMATION);
      SetPage(FPageList.Find(TPanel(txtMaxSpeed.Parent)));
      txtMaxSpeed.SetFocus;
      Exit;
    end;

  // Sonst wird kann es zu lange dauern, Clients zu entfernen, wenn der Thread gerade noch schläft.
  // Deshalb Limit auf 10..
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

procedure TfrmSettings.chkAutoTuneInClick(Sender: TObject);
begin
  inherited;

  lstMinBitrate.Enabled := chkAutoTuneIn.Checked;
  lstFormat.Enabled := chkAutoTuneIn.Checked;
  chkAutoTuneInConsiderIgnore.Enabled := chkAutoTuneIn.Checked;
end;

procedure TfrmSettings.chkOverwriteSmallerClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkOverwriteSmaller);
end;

procedure TfrmSettings.chkDeleteStreamsClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkDeleteStreams);
end;

procedure TfrmSettings.chkDiscardSmallerClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkDiscardSmaller);
end;

procedure TfrmSettings.chkLimitClick(Sender: TObject);
begin
  inherited;

  txtMaxSpeed.Enabled := chkLimit.Checked;
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
      TfrmMsgDlg.ShowMsg(Self, _('When changing this option for a stream which is recording, stop and start recording again for the new setting to become active.'), 5, btOK);
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
    chkOnlySaveFull.Checked := (not chkSeparateTracks.Checked) and chkSeparateTracks.Checked;

    pnlCut.Enabled := False;
    if (not chkSeparateTracks.Checked) or (chkSaveStreamsToMemory.Checked) then
      chkDeleteStreams.Checked := False;

    if (not chkSeparateTracks.Checked) then
      TfrmMsgDlg.ShowMsg(Self, _('When saving streams without saving separate tracks, keep in mind to change the pattern ' +
                                 'for names of saved files, because the variables for artist, title and tracknumber ' +
                                 '(%a, %t, %n) will only be filled with default values.'), 2, btOK);

    Application.ProcessMessages;

    EnablePanel(pnlCut, chkSaveStreamsToMemory.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled));
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
    EnablePanel(pnlCut, chkSaveStreamsToMemory.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled));
    chkDeleteStreams.Enabled := (not chkSeparateTracks.Checked) or (not chkSaveStreamsToMemory.Checked);
    chkDeleteStreams.Checked := chkDeleteStreams.Enabled and AppGlobals.StreamSettings.DeleteStreams;

    if Length(FStreamSettings) > 0 then
      TfrmMsgDlg.ShowMsg(Self, _('When changing this option for a stream which is recording, stop and start recording again for the new setting to become active.'), 3, btOK);
  end;
end;

procedure TfrmSettings.chkSearchSilenceClick(Sender: TObject);
begin
  inherited;

  txtSilenceLevel.Enabled := chkSearchSilence.Checked;
  txtSilenceLength.Enabled := chkSearchSilence.Checked;
  txtSilenceBufferSeconds.Enabled := chkSearchSilence.Checked;
  Label10.Enabled := chkSearchSilence.Checked;
  Label12.Enabled := chkSearchSilence.Checked;
  Label13.Enabled := chkSearchSilence.Checked;

  if FInitialized then
    RemoveGray(chkSearchSilence);
end;

procedure TfrmSettings.chkSkipShortClick(Sender: TObject);
begin
  inherited;
  txtShortLengthSeconds.Enabled := chkSkipShort.State <> cbUnchecked;

  if FInitialized then
    RemoveGray(chkSkipShort);
end;

procedure TfrmSettings.chkTrayClick(Sender: TObject);
begin
  inherited;

  optClose.Enabled := chkTray.Checked;
  optMinimize.Enabled := chkTray.Checked;
end;

{ TBlacklistTree }

constructor TBlacklistTree.Create(AOwner: TComponent; Streams: TStringList);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PBlacklistNodeData;
begin
  inherited Create(AOwner);

  NodeDataSize := SizeOf(TBlacklistNodeData);
  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.Options := Header.Options + [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 0;
  DragMode := dmManual;
  ShowHint := True;
  HintMode := hmTooltip;

  for i := 0 to Streams.Count - 1 do
  begin
    Node := AddChild(nil);
    NodeData := GetNodeData(Node);
    NodeData.Name := Streams[i];
  end;

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Name');

  Sort(nil, 0, Header.SortDirection);

  Header.SortColumn := 0;
  Header.SortDirection := sdAscending;
end;

destructor TBlacklistTree.Destroy;
begin

  inherited;
end;

procedure TBlacklistTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PBlacklistNodeData;
begin
  inherited;

  if TextType = ttNormal then
  begin
    NodeData := GetNodeData(Node);
    case Column of
      0: Text := NodeData.Name;
    end;
  end;
end;

function TBlacklistTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  if Column = 0 then
    Index := 1;
end;

procedure TBlacklistTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if Header.SortColumn <> HitInfo.Column then
    begin
      Header.SortColumn := HitInfo.Column;
      Header.SortDirection := sdAscending;
    end else
    begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end;
    Sort(nil, HitInfo.Column, Header.SortDirection);
  end;
end;

function TBlacklistTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  S: string;
  NodeData: PBlacklistNodeData;
begin
  S := Text;
  NodeData := GetNodeData(Node);
  Result := StrLIComp(PChar(S), PChar(NodeData.Name),
    Min(Length(S), Length(NodeData.Name)));
end;

procedure TBlacklistTree.RemoveSelected;
var
  Node, Node2: PVirtualNode;
begin
  Node := GetLast;
  BeginUpdate;
  while Node <> nil do
  begin
    if Selected[Node] then
    begin
      Node2 := GetPrevious(Node);
      DeleteNode(Node);
      Node := Node2;
    end else
      Node := GetPrevious(Node);
  end;
  EndUpdate;
end;

procedure TBlacklistTree.UpdateList(List: TStringList);
var
  Node: PVirtualNode;
  NodeData: PBlacklistNodeData;
begin
  List.Clear;

  Node := GetLast;
  BeginUpdate;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);
    List.Add(NodeData.Name);
    Node := GetPrevious(Node);
  end;
  EndUpdate;
end;

function TBlacklistTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
  function CmpTime(a, b: TDateTime): Integer;
  begin
    if a > b then
      Result := 1
    else if a < b then
      Result := -1
    else
      Result := 0;
  end;
var
  ND1, ND2: PBlacklistNodeData;
begin
  Result := 0;

  ND1 := GetNodeData(Node1);
  ND2 := GetNodeData(Node2);
  case Column of
    0: Result := CompareText(ND1.Name, ND2.Name);
  end;
end;

procedure TBlacklistTree.DoFreeNode(Node: PVirtualNode);
begin
  Finalize(PBlacklistNodeData(GetNodeData(Node)).Name);

  inherited;
end;

end.
