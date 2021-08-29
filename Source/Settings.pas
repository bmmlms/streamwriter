{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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
  AddonBase,
  AddonManager,
  AppData,
  AudioFunctions,
  Buttons,
  Classes,
  ComCtrls,
  ComboEx,
  ConfigureEncoder,
  Constants,
  Controls,
  DataManager,
  Dialogs,
  DownloadAddons,
  DynBASS,
  EditBtn,
  ExtCtrls,
  ExtendedStream,
  Forms,
  Functions,
  Generics.Collections,
  Generics.Defaults,
  Graphics,
  GUIFunctions,
  Images,
  ImgList,
  LanguageObjects,
  Logging,
  MControls,
  Menus,
  mhotkeyedit,
  MLabeledEdit,
  MsgDlg,
  PostProcess,
  SettingsAddPostProcessor,
  SettingsBase,
  SharedData,
  ShlObj,
  Spin,
  StdCtrls,
  SWFunctions,
  SysUtils,
  TypeDefs,
  Variants,
  VirtualTrees,
  Windows;

type
  TSettingsTypes = (stApp, stAuto, stStream);

  TBlacklistNodeData = record
    Name: string;
  end;
  PBlacklistNodeData = ^TBlacklistNodeData;

  TBlacklistTree = class(TVirtualStringTree)
  private
    FColTitle: TVirtualTreeColumn;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
  public
    constructor Create(AOwner: TComponent; Streams: TStringList); reintroduce;
    destructor Destroy; override;
    procedure UpdateList(List: TStringList);
    procedure RemoveSelected;
  end;

  { TfrmSettings }

  TfrmSettings = class(TfrmSettingsBase)
    Bevel1: TBevel;
    btnAdd: TButton;
    btnAddIgnoreTitlePattern: TButton;
    btnAddRegEx: TButton;
    btnBlacklistRemove: TButton;
    btnConfigure: TButton;
    btnConfigureEncoder: TSpeedButton;
    btnHelpPostProcess: TSpeedButton;
    btnMoveDown: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnRemove: TButton;
    btnRemoveIgnoreTitlePattern: TButton;
    btnRemoveRegEx: TButton;
    btnResetColor: TButton;
    chkAddSavedToIgnore: TCheckBox;
    chkAddSavedToStreamIgnore: TCheckBox;
    chkAdjustTrackOffset: TCheckBox;
    chkAutoRemoveSavedFromWishlist: TCheckBox;
    chkAutostart: TCheckBox;
    chkAutoTuneIn: TCheckBox;
    chkAutoTuneInAddToIgnore: TCheckBox;
    chkAutoTuneInConsiderIgnore: TCheckBox;
    chkCoverPanelAlwaysVisible: TCheckBox;
    chkDeleteStreams: TCheckBox;
    chkDiscardAlways: TCheckBox;
    chkDiscardSmaller: TCheckBox;
    chkDisplayPlayedSong: TCheckBox;
    chkDisplayPlayNotifications: TCheckBox;
    chkLimit: TCheckBox;
    chkManualSilenceLevel: TCheckBox;
    chkMonitorMode: TCheckBox;
    chkNormalizeVariables: TCheckBox;
    chkOnlyIfCut: TCheckBox;
    chkOnlySaveFull: TCheckBox;
    chkOverwriteSmaller: TCheckBox;
    chkRememberPlaying: TCheckBox;
    chkRememberRecordings: TCheckBox;
    chkRemoveSavedFromWishlist: TCheckBox;
    chkSaveStreamsToDisk: TCheckBox;
    chkSearchSilence: TCheckBox;
    chkSeparateTracks: TCheckBox;
    chkShowSplashScreen: TCheckBox;
    chkSkipShort: TCheckBox;
    chkSnapMain: TCheckBox;
    chkSubmitStats: TCheckBox;
    chkSubmitStreamInfo: TCheckBox;
    chkTray: TCheckBox;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    FlowPanel3: TFlowPanel;
    FlowPanel4: TFlowPanel;
    FlowPanel5: TFlowPanel;
    FlowPanel6: TFlowPanel;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblAppParams: TLabel;
    lblFilePattern: TLabel;
    lblIgnoreTitles: TLabel;
    lblPanelCut: TLabel;
    lstDefaultAction: TMLabeledComboBoxEx;
    lstDefaultActionBrowser: TMLabeledComboBoxEx;
    lstDefaultFilter: TMLabeledComboBoxEx;
    lstFormat: TMLabeledComboBoxEx;
    lstHotkeys: TListView;
    lstIgnoreTitles: TListView;
    lstMinQuality: TMLabeledComboBoxEx;
    lstOutputFormat: TMLabeledComboBoxEx;
    lstPostProcess: TListView;
    lstRegExes: TListView;
    lstSoundDevice: TMLabeledComboBoxEx;
    optAdjustBackward: TRadioButton;
    optAdjustForward: TRadioButton;
    optClose: TRadioButton;
    optMinimize: TRadioButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pnlAdjustTrackOffset: TPanel;
    pnlAdvanced: TPanel;
    pnlAppearance: TPanel;
    pnlAutoRecord: TPanel;
    pnlBandwidth: TPanel;
    pnlBlacklist: TPanel;
    pnlCommunity: TPanel;
    pnlCommunityBlacklist: TPanel;
    pnlCut: TPanel;
    pnlFilenames: TPanel;
    pnlFilenamesExt: TPanel;
    pnlHotkeys: TPanel;
    pnlMain: TPanel;
    pnlNodeBackgroundColor: TPanel;
    pnlNodeTextColor: TPanel;
    pnlNodeTextColorSelected: TPanel;
    pnlNodeTextColorSelectedFocused: TPanel;
    pnlPostProcess: TPanel;
    pnlStreams: TPanel;
    pnlStreamsAdvanced: TPanel;
    txtAdjustTrackOffset: TSpinEdit;
    txtApp: TMLabeledEditButton;
    txtAppParams: TMLabeledEdit;
    txtAutomaticFilePattern: TMLabeledEditButton;
    txtDir: TMLabeledEditButton;
    txtFilePattern: TMLabeledEditButton;
    txtFilePatternDecimals: TMLabeledSpinEdit;
    txtHotkey: TMLabeledHotkeyEdit;
    txtIgnoreTitlePattern: TMLabeledEdit;
    txtIncompleteFilePattern: TMLabeledEditButton;
    txtLogFile: TMLabeledEditButton;
    txtMaxRetries: TMLabeledSpinEdit;
    txtMaxSpeed: TMLabeledSpinEdit;
    txtMinDiskSpace: TMLabeledSpinEdit;
    txtMonitorCount: TMLabeledSpinEdit;
    txtPreview: TMLabeledEdit;
    txtRegEx: TMLabeledEditButton;
    dlgOpen: TOpenDialog;
    Label11: TLabel;
    pnlAddons: TPanel;
    lstAddons: TListView;
    dlgSave: TSaveDialog;
    dlgColor: TColorDialog;
    txtRemoveChars: TMLabeledEditButton;
    txtRetryDelay: TMLabeledSpinEdit;
    txtShortLengthSeconds: TMLabeledSpinEdit;
    txtSilenceBufferSeconds: TSpinEdit;
    txtSilenceLength: TSpinEdit;
    txtSilenceLevel: TMLabeledSpinEdit;
    txtSongBuffer: TMLabeledSpinEdit;
    txtStreamFilePattern: TMLabeledEditButton;
    procedure FormActivate(Sender: TObject);
    procedure lstOutputFormatSelect(Sender: TObject);
    procedure lstPostProcessSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure txtFilePatternChange(Sender: TObject);
    procedure chkSkipShortClick(Sender: TObject);
    procedure chkSearchSilenceClick(Sender: TObject);
    procedure chkTrayClick(Sender: TObject);
    procedure btnBrowseAppClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure txtAppParamsChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure txtHotkeyChange(Sender: TObject);
    procedure lstHotkeysChange(Sender: TObject; Item: TListItem; Change: TItemChange);
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
    procedure chkSaveStreamsToDiskClick(Sender: TObject);
    procedure chkOnlyIfCutClick(Sender: TObject);
    procedure chkOnlySaveFullClick(Sender: TObject);
    procedure chkOverwriteSmallerClick(Sender: TObject);
    procedure txtSilenceBufferSecondsChange(Sender: TObject);
    procedure lstPostProcessResize(Sender: TObject);
    procedure chkAutoTuneInClick(Sender: TObject);
    procedure chkDiscardSmallerClick(Sender: TObject);
    procedure txtFilePatternDecimalsChange(Sender: TObject);
    procedure btnBlacklistRemoveClick(Sender: TObject);
    procedure txtRegExChange(Sender: TObject);
    procedure btnResetTitlePatternClick(Sender: TObject);
    procedure btnResetFilePatternClick(Sender: TObject);
    procedure lstPostProcessItemChecked(Sender: TObject; Item: TListItem);
    procedure btnConfigureClick(Sender: TObject);
    procedure txtRemoveCharsChange(Sender: TObject);
    procedure chkLimitClick(Sender: TObject);
    procedure lstIgnoreTitlesResize(Sender: TObject);
    procedure txtIgnoreTitlePatternChange(Sender: TObject);
    procedure lstIgnoreTitlesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure btnAddIgnoreTitlePatternClick(Sender: TObject);
    procedure btnRemoveIgnoreTitlePatternClick(Sender: TObject);
    procedure chkAddSavedToStreamIgnoreClick(Sender: TObject);
    procedure chkAdjustTrackOffsetClick(Sender: TObject);
    procedure txtAdjustTrackOffsetChange(Sender: TObject);
    procedure optAdjustClick(Sender: TObject);
    procedure lstIgnoreTitlesEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure btnResetRemoveCharsClick(Sender: TObject);
    procedure chkRemoveSavedFromWishlistClick(Sender: TObject);
    procedure chkNormalizeVariablesClick(Sender: TObject);
    procedure chkManualSilenceLevelClick(Sender: TObject);
    procedure txtFilePatternEnter(Sender: TObject);
    procedure lstAddonsResize(Sender: TObject);
    procedure lstAddonsItemChecked(Sender: TObject; Item: TListItem);
    procedure btnConfigureEncoderClick(Sender: TObject);
    procedure btnBrowseLogFileClick(Sender: TObject);
    procedure chkMonitorModeClick(Sender: TObject);
    procedure chkSubmitStatsClick(Sender: TObject);
    procedure chkDiscardAlwaysClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label20Click(Sender: TObject);
    procedure chkSubmitStreamInfoClick(Sender: TObject);
    procedure lstRegExesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lstRegExesResize(Sender: TObject);
    procedure btnAddRegExClick(Sender: TObject);
    procedure btnRemoveRegExClick(Sender: TObject);
    procedure lstRegExesEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure pnlNodeColorClick(Sender: TObject);
    procedure btnResetColorClick(Sender: TObject);
  private
    FSettingsType: TSettingsTypes;
    FInitialized: Boolean;
    FOptionChanging: Boolean;
    FBrowseDir: Boolean;
    FDefaultActionIdx: Integer;
    FDefaultActionBrowserIdx: Integer;
    FDefaultFilterIdx: Integer;
    FOutputFormatIdx: Integer;
    FMinQualityIdx: Integer;
    FFormatIdx: Integer;
    FTemporaryPostProcessors: TPostProcessorList;
    FStreamSettings: TStreamSettingsArray;
    FIgnoreFieldList: TList;
    lstBlacklist: TBlacklistTree;
    btnReset: TBitBtn;
    FActivePreviewField: TEditButton;
    OutputFormatLastIndex: Integer;

    procedure CreateApp(AOwner: TComponent; BrowseDir: Boolean);
    procedure CreateAuto(AOwner: TComponent; BrowseDir: Boolean);
    procedure CreateStreams(AOwner: TComponent);
    procedure CreateGeneral;
    procedure SetFields;

    function ValidatePattern(Text, Patterns: string): string;
    function GetNewID: Integer;

    function GetStringListHash(Lst: TStringList): Cardinal;
    procedure BuildHotkeys;
    function RemoveGray(C: TControl; ShowMessage: Boolean = True): Boolean;
    procedure EnablePanel(Panel: TPanel; Enable: Boolean);
    procedure FillFields(Settings: TStreamSettings);
    procedure SetGray;
    procedure RebuildPostProcessingList;
    procedure UpdatePostProcessUpDown;
    procedure ShowEncoderNeededMessage;

    procedure BlacklistTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure BlacklistTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnResetClick(Sender: TObject);
  protected
    procedure RegisterPages; override;
    procedure Finish; override;
    function CanFinish: Boolean; override;
    procedure SetPage(Page: TPage); override;
    procedure PreTranslate; override;
    procedure PostTranslate; override;
    procedure GetExportDataHeader(Stream: TExtendedStream); override;
    procedure GetExportData(Stream: TExtendedStream); override;
    function CheckImportFile(Filename: string): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoCreate; override;
  public
    constructor Create(AOwner: TComponent; SettingsType: TSettingsTypes; StreamSettings: TStreamSettingsArray; BrowseDir: Boolean);

    destructor Destroy; override;
    property StreamSettings: TStreamSettingsArray read FStreamSettings;
  end;

const
  WARNING_STREAMRECORDING = 'When changing this option for a stream which is recording, stop and start recording again for the new setting to become active.';

  LABEL_TRACKPATTERN = 'Valid variables for saved tracks: %artist%, %title%, %album%, %genre%, %streamtitle%, %streamname%, %number%, %day%, %month%, %year%, %hour%, %minute%, %second%';
  LABEL_STREAMPATTERN = 'Valid variables for saved streams: %streamname%, %day%, %month%, %year%, %hour%, %minute%, %second%';
  LABEL_PATTERNSEPARATOR = 'Backslashes can be used to seperate directories.';

implementation

{$R *.lfm}

destructor TfrmSettings.Destroy;
var
  i: Integer;
begin
  FIgnoreFieldList.Free;

  for i := 0 to lstAddons.Items.Count - 1 do
    TAddonBase(lstAddons.Items[i].Data).Free;

  for i := 0 to FTemporaryPostProcessors.Count - 1 do
    FTemporaryPostProcessors[i].Free;
  FTemporaryPostProcessors.Free;

  for i := 0 to Length(FStreamSettings) - 1 do
    FStreamSettings[i].Free;

  inherited;
end;

procedure TfrmSettings.DoCreate;
begin
  inherited;

  // TODO: ..... das auch beim wizard so machen. klappt das?
  Width := 636;
  Height := 509;
end;

procedure TfrmSettings.EnablePanel(Panel: TPanel; Enable: Boolean);
var
  i: Integer;
begin
  for i := 0 to Panel.ControlCount - 1 do
  begin
    Panel.Controls[i].Visible := Enable;

    if (Panel = pnlCut) and (FSettingsType <> stStream) then
    begin
      chkAdjustTrackOffset.Visible := False;
      pnlAdjustTrackOffset.Visible := False;
      optAdjustBackward.Visible := False;
      optAdjustForward.Visible := False;
    end;
  end;

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
var
  i: Integer;
begin
  lstDefaultAction.Control.ItemIndex := Integer(AppGlobals.DefaultAction);
  lstDefaultActionBrowser.Control.ItemIndex := Integer(AppGlobals.DefaultActionBrowser);
  lstDefaultFilter.Control.ItemIndex := Integer(Settings.Filter);
  chkSeparateTracks.Checked := Settings.SeparateTracks;
  chkSaveStreamsToDisk.Checked := not Settings.SaveToMemory;
  chkOnlySaveFull.Checked := Settings.OnlySaveFull;

  Language.Translate(Self, PreTranslate, PostTranslate);

  pnlGeneral.BringToFront;

  AppGlobals.Lock;
  try
    txtFilePattern.Control.Text := Settings.FilePattern;
    txtAutomaticFilePattern.Control.Text := Settings.FilePattern;
    txtIncompleteFilePattern.Control.Text := Settings.IncompleteFilePattern;
    txtStreamFilePattern.Control.Text := Settings.StreamFilePattern;
    txtFilePatternDecimals.Control.Value := Settings.FilePatternDecimals;
    txtRemoveChars.Control.Text := Settings.RemoveChars;
    chkNormalizeVariables.Checked := Settings.NormalizeVariables;

    if FSettingsType = stAuto then
    begin
      txtDir.Caption := _('Folder for automatically saved songs:');
      txtDir.Control.Text := AppGlobals.DirAuto;
    end else
    begin
      txtDir.Caption := _('Folder for saved songs:');
      txtDir.Control.Text := AppGlobals.Dir;
    end;

    chkDeleteStreams.Checked := Settings.DeleteStreams;
    chkAddSavedToIgnore.Checked := Settings.AddSavedToIgnore;
    chkAddSavedToStreamIgnore.Checked := Settings.AddSavedToStreamIgnore;
    chkRemoveSavedFromWishlist.Checked := Settings.RemoveSavedFromWishlist;
    chkOverwriteSmaller.Checked := Settings.OverwriteSmaller;
    chkDiscardSmaller.Checked := Settings.DiscardSmaller;
    chkDiscardAlways.Checked := Settings.DiscardAlways;

    chkSkipShort.Checked := Settings.SkipShort;
    chkSearchSilence.Checked := Settings.SearchSilence;
    chkManualSilenceLevel.Checked := not Settings.AutoDetectSilenceLevel;

    chkSearchSilenceClick(nil);
    chkManualSilenceLevelClick(nil);

    chkAutostart.Checked := FileExists(ConcatPaths([GetShellFolder(CSIDL_STARTUP), AppGlobals.AppName + '.lnk']));
    chkTray.Checked := AppGlobals.Tray;
    chkSnapMain.Checked := AppGlobals.SnapMain;
    chkRememberRecordings.Checked := AppGlobals.RememberRecordings;
    chkRememberPlaying.Checked := AppGlobals.RememberPlaying;
    chkDisplayPlayedSong.Checked := AppGlobals.DisplayPlayedSong;
    chkDisplayPlayNotifications.Checked := AppGlobals.DisplayPlayNotifications;
    chkShowSplashScreen.Checked := AppGlobals.ShowSplashScreen;
    chkCoverPanelAlwaysVisible.Checked := AppGlobals.CoverPanelAlwaysVisible;
    optClose.Checked := not AppGlobals.TrayOnMinimize;
    optMinimize.Checked := AppGlobals.TrayOnMinimize;

    chkTrayClick(nil);

    chkAutoTuneIn.Checked := AppGlobals.AutoTuneIn;
    chkAutoTuneInConsiderIgnore.Checked := AppGlobals.AutoTuneInConsiderIgnore;
    chkAutoTuneInAddToIgnore.Checked := Settings.AddSavedToIgnore;
    chkAutoRemoveSavedFromWishlist.Checked := Settings.RemoveSavedFromWishlist;
    lstMinQuality.Control.ItemIndex := AppGlobals.AutoTuneInMinQuality;
    lstFormat.Control.ItemIndex := AppGlobals.AutoTuneInFormat;
    chkSubmitStreamInfo.Checked := AppGlobals.SubmitStreamInfo;
    chkSubmitStats.Checked := AppGlobals.SubmitStats;
    chkMonitorMode.Checked := AppGlobals.MonitorMode;
    txtMonitorCount.Control.Value := AppGlobals.MonitorCount;
    chkLimit.Checked := AppGlobals.LimitSpeed;
    if AppGlobals.MaxSpeed > 0 then
      txtMaxSpeed.Control.Value := AppGlobals.MaxSpeed;

    chkSubmitStreamInfoClick(nil);
    chkSubmitStatsClick(nil);
    chkMonitorModeClick(nil);

    txtShortLengthSeconds.Control.Value := Settings.ShortLengthSeconds;
    txtSongBuffer.Control.Value := Settings.SongBuffer;
    txtMaxRetries.Control.Value := Settings.MaxRetries;
    txtRetryDelay.Control.Value := Settings.RetryDelay;
    txtMinDiskSpace.Control.Value := AppGlobals.MinDiskSpace;
    txtLogFile.Control.Text := AppGlobals.LogFile;

    txtSilenceLevel.Control.Value := Settings.SilenceLevel;
    txtSilenceLength.Value := Settings.SilenceLength;
    txtSilenceBufferSeconds.Value := Settings.SilenceBufferSecondsStart;

    chkAdjustTrackOffset.Checked := Settings.AdjustTrackOffset;
    txtAdjustTrackOffset.Value := Settings.AdjustTrackOffsetMS;
    if Settings.AdjustTrackOffsetDirection = toForward then
      optAdjustForward.Checked := True
    else
      optAdjustBackward.Checked := True;

    if ((FSettingsType = stAuto) and not DirectoryExists(AppGlobals.DirAuto)) or ((FSettingsType <> stAuto) and not DirectoryExists(AppGlobals.Dir)) then
      txtDir.Control.Text := '';
  finally
    AppGlobals.Unlock;
  end;

  SetGray;

  if not chkSaveStreamsToDisk.Checked then
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

  if chkDiscardAlways.Checked then
  begin
    chkDiscardSmaller.Enabled := False;
    chkDiscardSmaller.Checked := False;
    chkOverwriteSmaller.Enabled := False;
    chkOverwriteSmaller.Checked := False;
  end;

  // ---------------------------------------------------------------------------------------------------------
  if FTemporaryPostProcessors <> nil then
  begin
    for i := 0 to FTemporaryPostProcessors.Count - 1 do
      FTemporaryPostProcessors[i].Free;
    FTemporaryPostProcessors.Free;
  end;
  FTemporaryPostProcessors := TPostProcessorList.Create;

  lstOutputFormat.Control.ItemIndex := Integer(Settings.OutputFormat);
  OutputFormatLastIndex := lstOutputFormat.Control.ItemIndex;

  for i := 0 to Settings.PostProcessors.Count - 1 do
  begin
    if Settings.PostProcessors[i].Hidden then
      Continue;

    FTemporaryPostProcessors.Add(Settings.PostProcessors[i].Copy);
  end;

  {
  FTemporaryPostProcessors.Sort(TComparer<TPostProcessBase>.Construct(
    function (const L, R: TPostProcessBase): integer
    begin
      if L.GroupID <> R.GroupID then
        Result := CmpInt(L.GroupID, R.GroupID)
      else
        Result := CmpInt(L.Order, R.Order);
    end
  ));
  }

  RebuildPostProcessingList;
  if lstPostProcess.Items.Count > 0 then
    lstPostProcess.Items[0].Selected := True;
  // ---------------------------------------------------------------------------------------------------------

  txtShortLengthSeconds.Enabled := chkSkipShort.State <> cbUnchecked;

  EnablePanel(pnlCut, (not chkSaveStreamsToDisk.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled)) or (FSettingsType = stAuto));
  chkSkipShort.Enabled := (not chkSaveStreamsToDisk.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled)) or (FSettingsType = stAuto);
  txtShortLengthSeconds.Enabled := chkSkipShort.Enabled and chkSkipShort.Checked;
end;

procedure TfrmSettings.Finish;
var
  i, k, n: Integer;
  PostProcessor: TPostProcessBase;
  EP: TExternalPostProcess;
  Item: TListItem;
  Tree: TVirtualStringTree;
begin
  if Length(FStreamSettings) > 0 then
  begin
    if FSettingsType = stAuto then
    begin
      AppGlobals.Lock;
      try
        AppGlobals.AutoTuneIn := chkAutoTuneIn.Checked;
        AppGlobals.AutoTuneInConsiderIgnore := chkAutoTuneInConsiderIgnore.Checked;
        AppGlobals.AutoTuneInMinQuality := lstMinQuality.Control.ItemIndex;
        AppGlobals.AutoTuneInFormat := lstFormat.Control.ItemIndex;
        AppGlobals.DirAuto := txtDir.Control.Text;

        lstBlacklist.UpdateList(AppGlobals.Data.StreamBlacklist);
      finally
        AppGlobals.Unlock;
      end;
    end;

    if FSettingsType = stApp then
    begin
      AppGlobals.Lock;
      try
        if lstSoundDevice.Control.ItemIndex > -1 then
          AppGlobals.SoundDevice := TBassDevice(lstSoundDevice.Control.ItemsEx[lstSoundDevice.Control.ItemIndex].Data).ID;

        if chkAutostart.Checked then
          CreateLink(Application.ExeName, PChar(GetShellFolder(CSIDL_STARTUP)), AppGlobals.AppName, '-minimize', False)
        else
          CreateLink(Application.ExeName, PChar(GetShellFolder(CSIDL_STARTUP)), AppGlobals.AppName, '', True);

        AppGlobals.Dir := txtDir.Control.Text;

        AppGlobals.Tray := chkTray.Checked;
        AppGlobals.SnapMain := chkSnapMain.Checked;
        AppGlobals.RememberRecordings := chkRememberRecordings.Checked;
        AppGlobals.RememberPlaying := chkRememberPlaying.Checked;
        AppGlobals.DisplayPlayedSong := chkDisplayPlayedSong.Checked;
        AppGlobals.DisplayPlayNotifications := chkDisplayPlayNotifications.Checked;
        AppGlobals.ShowSplashScreen := chkShowSplashScreen.Checked;
        AppGlobals.CoverPanelAlwaysVisible := chkCoverPanelAlwaysVisible.Checked;
        AppGlobals.TrayOnMinimize := optMinimize.Checked;

        AppGlobals.AutoTuneIn := chkAutoTuneIn.Checked;
        AppGlobals.SubmitStreamInfo := chkSubmitStreamInfo.Checked;
        AppGlobals.SubmitStats := chkSubmitStats.Checked;
        AppGlobals.MonitorMode := chkMonitorMode.Checked;
        AppGlobals.MonitorCount := txtMonitorCount.Control.Value;
        AppGlobals.LimitSpeed := chkLimit.Checked;
        if txtMaxSpeed.Control.Value > 0 then
          AppGlobals.MaxSpeed := txtMaxSpeed.Control.Value;

        AppGlobals.MinDiskSpace := txtMinDiskSpace.Control.Value;
        AppGlobals.LogFile := txtLogFile.Control.Text;
        AppGlobals.DefaultAction := TClientActions(lstDefaultAction.Control.ItemIndex);
        AppGlobals.DefaultActionBrowser := TStreamOpenActions(lstDefaultActionBrowser.Control.ItemIndex);

        AppGlobals.ShortcutPlay := LongWord(lstHotkeys.Items[0].Data);
        AppGlobals.ShortcutPause := LongWord(lstHotkeys.Items[1].Data);
        AppGlobals.ShortcutStop := LongWord(lstHotkeys.Items[2].Data);
        AppGlobals.ShortcutNext := LongWord(lstHotkeys.Items[3].Data);
        AppGlobals.ShortcutPrev := LongWord(lstHotkeys.Items[4].Data);
        AppGlobals.ShortcutVolUp := LongWord(lstHotkeys.Items[5].Data);
        AppGlobals.ShortcutVolDown := LongWord(lstHotkeys.Items[6].Data);
        AppGlobals.ShortcutMute := LongWord(lstHotkeys.Items[7].Data);

        Tree := TVirtualStringTree.Create(Self);
        try
          if (pnlNodeTextColor.Color <> Tree.Colors.NodeFontColor) or (pnlNodeTextColorSelected.Color <> Tree.Colors.NodeFontColor) or (pnlNodeTextColorSelectedFocused.Color <>
            Tree.Colors.SelectionTextColor) or (pnlNodeBackgroundColor.Color <> Tree.Colors.BackGroundColor) then
          begin
            AppGlobals.NodeColorsLoaded := True;
            AppGlobals.NodeTextColor := pnlNodeTextColor.Color;
            AppGlobals.NodeTextColorSelected := pnlNodeTextColorSelected.Color;
            AppGlobals.NodeTextColorSelectedFocused := pnlNodeTextColorSelectedFocused.Color;
            AppGlobals.NodeBackgroundColor := pnlNodeBackgroundColor.Color;
          end else
          begin
            AppGlobals.NodeColorsLoaded := False;
            AppGlobals.NodeTextColor := $7F000000;
            AppGlobals.NodeTextColorSelected := $7F000000;
            AppGlobals.NodeTextColorSelectedFocused := $7F000000;
            AppGlobals.NodeBackgroundColor := $7F000000;
          end;
        finally
          Tree.Free;
        end;
      finally
        AppGlobals.Unlock;
      end;
    end;

    for i := 0 to Length(FStreamSettings) - 1 do
    begin
      if FSettingsType = stAuto then
        FStreamSettings[i].FilePattern := Trim(txtAutomaticFilePattern.Control.Text)
      else if FIgnoreFieldList.IndexOf(txtFilePattern) = -1 then
        FStreamSettings[i].FilePattern := Trim(txtFilePattern.Control.Text);

      if FIgnoreFieldList.IndexOf(txtIncompleteFilePattern) = -1 then
        FStreamSettings[i].IncompleteFilePattern := Trim(txtIncompleteFilePattern.Control.Text);

      if FIgnoreFieldList.IndexOf(txtStreamFilePattern) = -1 then
        FStreamSettings[i].StreamFilePattern := Trim(txtStreamFilePattern.Control.Text);

      if FIgnoreFieldList.IndexOf(txtFilePatternDecimals) = -1 then
        FStreamSettings[i].FilePatternDecimals := txtFilePatternDecimals.Control.Value;

      if FIgnoreFieldList.IndexOf(txtRemoveChars) = -1 then
        FStreamSettings[i].RemoveChars := txtRemoveChars.Control.Text;

      if FIgnoreFieldList.IndexOf(chkNormalizeVariables) = -1 then
        FStreamSettings[i].NormalizeVariables := chkNormalizeVariables.Checked;

      if FIgnoreFieldList.IndexOf(chkDeleteStreams) = -1 then
        FStreamSettings[i].DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;

      if FSettingsType = stAuto then
        FStreamSettings[i].AddSavedToIgnore := chkAutoTuneInAddToIgnore.Checked
      else if FIgnoreFieldList.IndexOf(chkAddSavedToIgnore) = -1 then
        FStreamSettings[i].AddSavedToIgnore := chkAddSavedToIgnore.Checked;

      if FIgnoreFieldList.IndexOf(chkAddSavedToStreamIgnore) = -1 then
        FStreamSettings[i].AddSavedToStreamIgnore := chkAddSavedToStreamIgnore.Checked;

      if FSettingsType = stAuto then
        FStreamSettings[i].RemoveSavedFromWishlist := chkAutoRemoveSavedFromWishlist.Checked
      else if FIgnoreFieldList.IndexOf(chkRemoveSavedFromWishlist) = -1 then
        FStreamSettings[i].RemoveSavedFromWishlist := chkRemoveSavedFromWishlist.Checked;

      if FIgnoreFieldList.IndexOf(chkOverwriteSmaller) = -1 then
        FStreamSettings[i].OverwriteSmaller := chkOverwriteSmaller.Checked;

      if FIgnoreFieldList.IndexOf(chkDiscardSmaller) = -1 then
        FStreamSettings[i].DiscardSmaller := chkDiscardSmaller.Checked;

      if FIgnoreFieldList.IndexOf(chkDiscardAlways) = -1 then
        FStreamSettings[i].DiscardAlways := chkDiscardAlways.Checked;

      if pnlCut.Tag = 0 then
      begin
        if FIgnoreFieldList.IndexOf(chkSkipShort) = -1 then
          FStreamSettings[i].SkipShort := chkSkipShort.Checked;

        if FIgnoreFieldList.IndexOf(txtSongBuffer) = -1 then
          FStreamSettings[i].SongBuffer := txtSongBuffer.Control.Value;

        if FIgnoreFieldList.IndexOf(txtShortLengthSeconds) = -1 then
          FStreamSettings[i].ShortLengthSeconds := txtShortLengthSeconds.Control.Value;

        if FIgnoreFieldList.IndexOf(chkSearchSilence) = -1 then
          FStreamSettings[i].SearchSilence := chkSearchSilence.Checked;

        if FIgnoreFieldList.IndexOf(chkManualSilenceLevel) = -1 then
          FStreamSettings[i].AutoDetectSilenceLevel := not chkManualSilenceLevel.Checked;

        if FIgnoreFieldList.IndexOf(txtSilenceLevel) = -1 then
          FStreamSettings[i].SilenceLevel := txtSilenceLevel.Control.Value;

        if FIgnoreFieldList.IndexOf(txtSilenceLength) = -1 then
          FStreamSettings[i].SilenceLength := txtSilenceLength.Value;

        if FIgnoreFieldList.IndexOf(txtSilenceBufferSeconds) = -1 then
        begin
          FStreamSettings[i].SilenceBufferSecondsStart := txtSilenceBufferSeconds.Value;
          FStreamSettings[i].SilenceBufferSecondsEnd := txtSilenceBufferSeconds.Value;
        end;

        if Length(FStreamSettings) > 0 then
        begin
          if FIgnoreFieldList.IndexOf(chkAdjustTrackOffset) = -1 then
            FStreamSettings[i].AdjustTrackOffset := chkAdjustTrackOffset.Checked;

          if FIgnoreFieldList.IndexOf(txtAdjustTrackOffset) = -1 then
            FStreamSettings[i].AdjustTrackOffsetMS := txtAdjustTrackOffset.Value;

          if FIgnoreFieldList.IndexOf(optAdjustBackward) = -1 then
            if optAdjustBackward.Checked then
              FStreamSettings[i].AdjustTrackOffsetDirection := toBackward
            else
              FStreamSettings[i].AdjustTrackOffsetDirection := toForward;
        end;
      end;

      if FIgnoreFieldList.IndexOf(txtMaxRetries) = -1 then
        FStreamSettings[i].MaxRetries := txtMaxRetries.Control.Value;

      if FIgnoreFieldList.IndexOf(txtRetryDelay) = -1 then
        FStreamSettings[i].RetryDelay := txtRetryDelay.Control.Value;

      if FIgnoreFieldList.IndexOf(lstDefaultFilter) = -1 then
        FStreamSettings[i].Filter := TUseFilters(lstDefaultFilter.Control.ItemIndex);

      if FIgnoreFieldList.IndexOf(chkSeparateTracks) = -1 then
        FStreamSettings[i].SeparateTracks := chkSeparateTracks.Checked and chkSeparateTracks.Enabled;

      if FIgnoreFieldList.IndexOf(chkSaveStreamsToDisk) = -1 then
        FStreamSettings[i].SaveToMemory := not chkSaveStreamsToDisk.Checked;

      if FIgnoreFieldList.IndexOf(chkOnlySaveFull) = -1 then
        FStreamSettings[i].OnlySaveFull := chkOnlySaveFull.Checked;

      if (FIgnoreFieldList.IndexOf(lstRegExes) = -1) and (Length(FStreamSettings) > 0) then
      begin
        FStreamSettings[i].RegExes.Clear;
        for n := 0 to lstRegExes.Items.Count - 1 do
          FStreamSettings[i].RegExes.Add(lstRegExes.Items[n].Caption);
      end;

      if (FIgnoreFieldList.IndexOf(lstIgnoreTitles) = -1) and (Length(FStreamSettings) > 0) then
      begin
        FStreamSettings[i].IgnoreTrackChangePattern.Clear;
        for n := 0 to lstIgnoreTitles.Items.Count - 1 do
          FStreamSettings[i].IgnoreTrackChangePattern.Add(lstIgnoreTitles.Items[n].Caption);
      end;

      if FIgnoreFieldList.IndexOf(lstOutputFormat) = -1 then
        FStreamSettings[i].OutputFormat := TAudioTypes(lstOutputFormat.Control.ItemIndex);

      if FIgnoreFieldList.IndexOf(lstPostProcess) = -1 then
      begin
        // -----------------------------------------------------------
        for k := 0 to FTemporaryPostProcessors.Count - 1 do
        begin
          PostProcessor := FStreamSettings[i].PostProcessors.Find(FTemporaryPostProcessors[k]);

          if (PostProcessor = nil) or (FTemporaryPostProcessors[k].IsNew) then
          begin
            // Ein neuer PostProcessor kann nur TExternalPostProcessor sein.
            PostProcessor := FTemporaryPostProcessors[k].Copy;
            FStreamSettings[i].PostProcessors.Add(PostProcessor);
          end;

          Item := nil;
          for n := 0 to lstPostProcess.Items.Count - 1 do
            if lstPostProcess.Items[n].Data = FTemporaryPostProcessors[k] then
            begin
              Item := lstPostProcess.Items[n];
              Break;
            end;

          PostProcessor.OnlyIfCut := FTemporaryPostProcessors[k].OnlyIfCut;
          PostProcessor.Order := Item.Index;
          PostProcessor.Active := Item.Checked;

          PostProcessor.Assign(FTemporaryPostProcessors[k]);
        end;

        // Vom Benutzer entfernte PostProcessors aus den echten PostProcessors entfernen..
        for k := FStreamSettings[i].PostProcessors.Count - 1 downto 0 do
        begin
          if FStreamSettings[i].PostProcessors[k] is TExternalPostProcess then
          begin
            EP := nil;
            for n := 0 to FTemporaryPostProcessors.Count - 1 do
              if FTemporaryPostProcessors[n] is TExternalPostProcess then
                if TExternalPostProcess(FTemporaryPostProcessors[n]).Identifier = TExternalPostProcess(FStreamSettings[i].PostProcessors[k]).Identifier then
                begin
                  EP := TExternalPostProcess(FStreamSettings[i].PostProcessors[k]);
                  Break;
                end;
            if EP = nil then
            begin
              FStreamSettings[i].PostProcessors[k].Free;
              FStreamSettings[i].PostProcessors.Delete(k);
              Continue;
            end;
          end;
          FStreamSettings[i].PostProcessors[k].IsNew := False;
        end;
        // -----------------------------------------------------------
      end;
    end;
  end else
    raise Exception.Create('not Length(FStreamSettings) > 0');

  inherited;
end;

procedure TfrmSettings.FormActivate(Sender: TObject);
begin
  if FBrowseDir then
  begin
    SetPage(FPageList.Find(TPanel(txtDir.Parent)));
    btnBrowseClick(nil);
  end;
  FBrowseDir := False;
end;

procedure TfrmSettings.GetExportData(Stream: TExtendedStream);
begin
  inherited;

  AppGlobals.Lock;
  try
    AppGlobals.Data.Save(Stream, True);
  finally
    AppGlobals.Unlock;
  end;
end;

procedure TfrmSettings.GetExportDataHeader(Stream: TExtendedStream);
begin
  inherited;

  Stream.Write(EXPORTMAGIC[0], Length(EXPORTMAGIC));
end;

function TfrmSettings.GetNewID: Integer;
var
  i: Integer;
  Exists: Boolean;
begin
  Result := 1;

  while True do
  begin
    Exists := False;
    for i := 0 to lstPostProcess.Items.Count - 1 do
      if TPostProcessBase(lstPostProcess.Items[i].Data) is TExternalPostProcess then
        if TExternalPostProcess(lstPostProcess.Items[i].Data).Identifier = Result then
        begin
          Inc(Result);
          Exists := True;
          Break;
        end;

    if not Exists then
      Break;
  end;
end;

function TfrmSettings.GetStringListHash(Lst: TStringList): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Lst.Count - 1 do
    Result := Result + HashString(Lst[i]);
end;

procedure TfrmSettings.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = VK_F1 then
    case FSettingsType of
      stApp:
        ShellExecuteW(Handle, 'open', PWideChar(UnicodeString(AppGlobals.ProjectHelpLinkSettings)), '', '', 1);
      stAuto:
        ShellExecuteW(Handle, 'open', PWideChar(UnicodeString(AppGlobals.ProjectHelpLinkAutoSettings)), '', '', 1);
      stStream:
        ShellExecuteW(Handle, 'open', PWideChar(UnicodeString(AppGlobals.ProjectHelpLinkStreamSettings)), '', '', 1);
    end;
end;

procedure TfrmSettings.Label20Click(Sender: TObject);
begin
  inherited;

  chkMonitorMode.Checked := not chkMonitorMode.Checked;
end;

procedure TfrmSettings.Label2Click(Sender: TObject);
begin
  inherited;

  chkSubmitStreamInfo.Checked := not chkSubmitStreamInfo.Checked;
end;

procedure TfrmSettings.Label8Click(Sender: TObject);
begin
  inherited;

  chkSubmitStats.Checked := not chkSubmitStats.Checked;
end;

procedure TfrmSettings.lstDefaultFilterChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(lstDefaultFilter);
end;

procedure TfrmSettings.lstHotkeysChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  inherited;

  txtHotkey.Enabled := lstHotkeys.Selected <> nil;
  if txtHotkey.Enabled then
  begin
    txtHotkey.Control.HotKey := TShortCut(lstHotkeys.Selected.Data);
    txtHotkey.Control.ApplyFocus;
  end else
    txtHotkey.Control.HotKey := 0;
end;

procedure TfrmSettings.lstIgnoreTitlesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  btnRemoveIgnoreTitlePattern.Enabled := lstIgnoreTitles.Selected <> nil;
end;

procedure TfrmSettings.lstIgnoreTitlesEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  inherited;

  if Trim(S) = '' then
    S := Item.Caption
  else
    RemoveGray(lstIgnoreTitles);
end;

procedure TfrmSettings.lstIgnoreTitlesResize(Sender: TObject);
begin
  lstIgnoreTitles.Columns[0].Width := lstIgnoreTitles.ClientWidth - 25;
end;

procedure TfrmSettings.lstOutputFormatSelect(Sender: TObject);
var
  i: Integer;
begin
  inherited;

  if not FInitialized then
    Exit;

  RemoveGray(lstOutputFormat);

  if lstOutputFormat.Control.ItemIndex = 0 then
  begin
    btnConfigureEncoder.Enabled := False;
    OutputFormatLastIndex := lstOutputFormat.Control.ItemIndex;
    Exit;
  end;

  if AppGlobals.AddonManager.CanEncode(TAudioTypes(lstOutputFormat.Control.ItemIndex)) <> ceOkay then
    if MsgBox(Handle, _('Additional addons are needed to use the selected output format. Do you want to download these addons now?'), _('Question'), MB_YESNO or MB_DEFBUTTON1 or MB_ICONQUESTION) = IDYES then
      AppGlobals.AddonManager.InstallEncoderFor(Self, TAudioTypes(lstOutputFormat.Control.ItemIndex));

  if AppGlobals.AddonManager.CanEncode(TAudioTypes(lstOutputFormat.Control.ItemIndex)) <> ceOkay then
    lstOutputFormat.Control.ItemIndex := OutputFormatLastIndex
  else
    OutputFormatLastIndex := lstOutputFormat.Control.ItemIndex;

  lstAddons.OnItemChecked := nil;
  for i := 0 to lstAddons.Items.Count - 1 do
    lstAddons.Items[i].Checked := TAddonBase(lstAddons.Items[i].Data).PackageDownloaded;
  lstAddons.OnItemChecked := lstAddonsItemChecked;

  btnConfigureEncoder.Enabled := lstOutputFormat.Control.ItemIndex > 0;
end;

procedure TfrmSettings.lstAddonsItemChecked(Sender: TObject; Item: TListItem);
var
  i: Integer;
begin
  if not FInitialized then
    Exit;

  lstAddons.Selected := Item;

  lstAddons.OnItemChecked := nil;
  Item.Checked := AppGlobals.AddonManager.EnableAddon(Self, TAddonBase(Item.Data), True);
  lstAddons.OnItemChecked := lstAddonsItemChecked;

  // Eventuell wurden Abhängigkeiten mitinstalliert. Also alles mal aktualisieren.
  lstAddons.OnItemChecked := nil;
  for i := 0 to lstAddons.Items.Count - 1 do
    lstAddons.Items[i].Checked := TAddonBase(lstAddons.Items[i].Data).FilesExtracted;
  lstAddons.OnItemChecked := lstAddonsItemChecked;
end;

procedure TfrmSettings.lstAddonsResize(Sender: TObject);
begin
  inherited;

  lstAddons.Columns[0].Width := lstAddons.ClientWidth;
end;

procedure TfrmSettings.lstPostProcessItemChecked(Sender: TObject; Item: TListItem);
var
  i: Integer;
begin
  if not FInitialized then
    Exit;

  RemoveGray(lstPostProcess);

  if Item.Data = nil then
    Exit;

  if TObject(Item.Data) is TInternalPostProcess then
  begin
    lstPostProcess.OnItemChecked := nil;
    Item.Checked := AppGlobals.PostProcessManager.EnablePostProcess(Self, Item.Checked, TInternalPostProcess(Item.Data));
    lstPostProcess.OnItemChecked := lstPostProcessItemChecked;

    btnConfigure.Enabled := Item.Checked and TPostProcessBase(Item.Data).CanConfigure;
  end;

  FTemporaryPostProcessors.Find(TPostProcessBase(Item.Data)).Active := Item.Checked;

  if TPostProcessBase(Item.Data).NeedsWave and Item.Checked then
    ShowEncoderNeededMessage;

  lstAddons.OnItemChecked := nil;
  for i := 0 to lstAddons.Items.Count - 1 do
    lstAddons.Items[i].Checked := TAddonBase(lstAddons.Items[i].Data).PackageDownloaded;
  lstAddons.OnItemChecked := lstAddonsItemChecked;

  lstPostProcess.Selected := Item;
end;

procedure TfrmSettings.lstPostProcessResize(Sender: TObject);
begin
  inherited;

  lstPostProcess.Columns[0].Width := lstPostProcess.ClientWidth - 25;
end;

procedure TfrmSettings.lstPostProcessSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Item.Data = nil then
    Exit;

  btnConfigure.Enabled := False;

  btnHelpPostProcess.Enabled := (Item <> nil) and Selected and (TPostProcessBase(Item.Data).Help <> '');
  btnRemove.Enabled := (Item <> nil) and Selected and (TPostProcessBase(Item.Data) is TExternalPostProcess);

  UpdatePostProcessUpDown;

  chkOnlyIfCut.Checked := (Item <> nil) and Selected and TPostProcessBase(Item.Data).OnlyIfCut;
  chkOnlyIfCut.Enabled := (Item <> nil) and Selected;

  if Selected and (TPostProcessBase(Item.Data) is TExternalPostProcess) then
  begin
    txtApp.Control.Text := TExternalPostProcess(Item.Data).Exe;
    txtAppParams.Control.Text := TExternalPostProcess(Item.Data).Params;
    txtApp.Enabled := True;
    txtAppParams.Enabled := True;
    lblAppParams.Enabled := True;
    btnRemove.Enabled := True;
  end else
  begin
    txtApp.Control.Text := '';
    txtAppParams.Control.Text := '';
    txtApp.Enabled := False;
    txtAppParams.Enabled := False;
    lblAppParams.Enabled := False;
    btnRemove.Enabled := False;
  end;

  btnConfigure.Enabled := (Item <> nil) and Item.Checked and TPostProcessBase(Item.Data).CanConfigure;
end;

procedure TfrmSettings.lstRegExesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  btnRemoveRegEx.Enabled := lstRegExes.Selected <> nil;
end;

procedure TfrmSettings.lstRegExesEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  if not CheckRegExp(Handle, S, lstRegExes, Item) then
  begin
    S := Item.Caption;
    Exit;
  end;

  Item.Caption := S;
end;

procedure TfrmSettings.lstRegExesResize(Sender: TObject);
begin
  inherited;

  lstRegExes.Columns[0].Width := lstRegExes.ClientWidth - 25;
end;

procedure TfrmSettings.optAdjustClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(optAdjustBackward);
    RemoveGray(optAdjustForward);
  end;
end;

procedure TfrmSettings.pnlNodeColorClick(Sender: TObject);
begin
  dlgColor.Color := TPanel(Sender).Color;
  if dlgColor.Execute then
    TPanel(Sender).Color := dlgColor.Color;
end;

procedure TfrmSettings.PreTranslate;
begin
  inherited;

  FDefaultActionIdx := lstDefaultAction.Control.ItemIndex;
  FDefaultActionBrowserIdx := lstDefaultActionBrowser.Control.ItemIndex;
  FDefaultFilterIdx := lstDefaultFilter.Control.ItemIndex;
  FOutputFormatIdx := lstOutputFormat.Control.ItemIndex;
  FMinQualityIdx := lstMinQuality.Control.ItemIndex;
  FFormatIdx := lstFormat.Control.ItemIndex;
end;

procedure TfrmSettings.PostTranslate;
var
  i: Integer;
begin
  inherited;

  if FSettingsType = stAuto then
    lblFilePattern.Caption := _(LABEL_TRACKPATTERN) + #13#10 + _(LABEL_PATTERNSEPARATOR)
  else
    lblFilePattern.Caption := _(LABEL_TRACKPATTERN) + #13#10 + _(LABEL_STREAMPATTERN) + #13#10 + _(LABEL_PATTERNSEPARATOR);

  lblAppParams.Caption := _('Valid variables: %filename%, %artist%, %title%, %album%, %genre%, %streamtitle%, %streamname%, %number%, %day%, %month%, %year%, %hour%, %minute%, %second%'#13#10 +
    'Every parameter should be quoted using ".');

  if lstPostProcess.Selected <> nil then
    lstPostProcessSelectItem(lstPostProcess, lstPostProcess.Selected, True);

  for i := 0 to lstAddons.Items.Count - 1 do
    lstAddons.Items[i].Caption := TAddonBase(lstAddons.Items[i].Data).Name;

  for i := 0 to lstPostProcess.Items.Count - 1 do
    lstPostProcess.Items[i].Caption := TPostProcessBase(lstPostProcess.Items[i].Data).Name; // Damit Sprache neu gesetzt wird und so..

  if (lstSoundDevice.Control.ItemsEx.Count > 0) and (lstSoundDevice.Control.ItemsEx[0].Data <> nil) and (TBassDevice(lstSoundDevice.Control.ItemsEx[0].Data).IsDefault) then
    lstSoundDevice.Control.ItemsEx[0].Caption := _('Default device');

  {
  lstPostProcess.Groups[0].Header := _('Processing when in WAVE-format');
  lstPostProcess.Groups[1].Header := _('Processing after conversion to destination format');
  }

  BuildHotkeys;

  lstDefaultAction.Control.ItemIndex := FDefaultActionIdx;
  lstDefaultActionBrowser.Control.ItemIndex := FDefaultActionBrowserIdx;
  lstDefaultFilter.Control.ItemIndex := FDefaultFilterIdx;
  lstOutputFormat.Control.ItemIndex := FOutputFormatIdx;
  lstMinQuality.Control.ItemIndex := FMinQualityIdx;
  lstFormat.Control.ItemIndex := FFormatIdx;
end;

function TfrmSettings.ValidatePattern(Text, Patterns: string): string;
var
  i: Integer;
  Arr: TPatternReplaceArray;
  PList: TStringList;
begin
  inherited;

  PList := TStringList.Create;
  try
    Explode('|', Patterns, PList);

    SetLength(Arr, PList.Count);

    for i := 0 to PList.Count - 1 do
    begin
      Arr[i].C := PList[i];

      if Arr[i].C = 'artist' then
        Arr[i].Replace := _('Artist')
      else if Arr[i].C = 'title' then
        Arr[i].Replace := _('Title')
      else if Arr[i].C = 'album' then
        Arr[i].Replace := _('Album')
      else if Arr[i].C = 'genre' then
        Arr[i].Replace := _('Genre')
      else if Arr[i].C = 'streamtitle' then
        Arr[i].Replace := _('Title on stream')
      else if Arr[i].C = 'streamname' then
        Arr[i].Replace := _('Streamname')
      else if Arr[i].C = 'number' then
        Arr[i].Replace := Format('%.*d', [txtFilePatternDecimals.Control.Value, 78])
      else if Arr[i].C = 'day' then
        Arr[i].Replace := FormatDateTime('dd', Now)
      else if Arr[i].C = 'month' then
        Arr[i].Replace := FormatDateTime('mm', Now)
      else if Arr[i].C = 'year' then
        Arr[i].Replace := FormatDateTime('yy', Now)
      else if Arr[i].C = 'hour' then
        Arr[i].Replace := FormatDateTime('hh', Now)
      else if Arr[i].C = 'minute' then
        Arr[i].Replace := FormatDateTime('nn', Now)
      else if Arr[i].C = 'second' then
        Arr[i].Replace := FormatDateTime('ss', Now);
    end;
  finally
    PList.Free;
  end;

  Result := PatternReplaceNew(Text, Arr);

  Result := FixPatternFilename(Result);

  Result := FixPathName(Result + '.mp3');
end;

procedure TfrmSettings.RebuildPostProcessingList;
var
  i: Integer;
  Item: TListItem;
begin
  lstPostProcess.Items.BeginUpdate;
  try
    lstPostProcess.Items.Clear;
    for i := 0 to FTemporaryPostProcessors.Count - 1 do
    begin
      Item := lstPostProcess.Items.Add;
      // Item.GroupID := FTemporaryPostProcessors[i].GroupID;
      Item.Caption := FTemporaryPostProcessors[i].Name;
      Item.Checked := FTemporaryPostProcessors[i].Active;
      // Data must be set at last that events (i.e. lstPostProcessItemChecked) do not fire
      Item.Data := FTemporaryPostProcessors[i];

      if FTemporaryPostProcessors[i] is TInternalPostProcess then
        Item.ImageIndex := TImages.LIGHTNING
      else
        Item.ImageIndex := TImages.APPLICATION_XP_TERMINAL;
    end;
  finally
    lstPostProcess.Items.EndUpdate;
  end;
end;

procedure TfrmSettings.RegisterPages;
begin
  case FSettingsType of
    stApp:
    begin
      FPageList.Add(TPage.Create('Settings', pnlMain, TImages.WRENCH_APPLICATION));
      FPageList.Add(TPage.Create('Appearance', pnlAppearance, TImages.PAINT));
      FPageList.Add(TPage.Create('Recordings', pnlStreams, TImages.RECORD_RED));
      FPageList.Add(TPage.Create('Filenames', pnlFilenames, TImages.TEXTFIELD_RENAME));
      FPageList.Add(TPage.Create('Advanced', pnlFilenamesExt, TImages.TEXTFIELD_RENAME_COG, FPageList.Find(pnlFilenames)));
      FPageList.Add(TPage.Create('Cut songs', pnlCut, TImages.CUT));
      FPageList.Add(TPage.Create('Addons', pnlAddons, TImages.PLUGIN));
      FPageList.Add(TPage.Create('Postprocessing', pnlPostProcess, TImages.LIGHTNING));
      FPageList.Add(TPage.Create('Bandwidth', pnlBandwidth, TImages.CONNECT));
      FPageList.Add(TPage.Create('Community', pnlCommunity, TImages.GROUP));
      FPageList.Add(TPage.Create('Hotkeys', pnlHotkeys, TImages.KEYBOARD));
      FPageList.Add(TPage.Create('Advanced', pnlAdvanced, TImages.COG));
    end;
    stAuto:
    begin
      FPageList.Add(TPage.Create('Recordings', pnlAutoRecord, TImages.RECORD_RED));
      FPageList.Add(TPage.Create('Blacklist', pnlCommunityBlacklist, TImages.PAGE_WHITE_TRANSMIT, FPageList.Find(pnlAutoRecord)));
      FPageList.Add(TPage.Create('Filenames', pnlFilenames, TImages.TEXTFIELD_RENAME));
      FPageList.Add(TPage.Create('Cut songs', pnlCut, TImages.CUT));
      FPageList.Add(TPage.Create('Postprocessing', pnlPostProcess, TImages.LIGHTNING));
    end;
    stStream:
    begin
      FPageList.Add(TPage.Create('Recordings', pnlStreams, TImages.RECORD_RED));
      FPageList.Add(TPage.Create('Advanced', pnlStreamsAdvanced, TImages.RECORD_RED_COG, FPageList.Find(pnlStreams)));
      FPageList.Add(TPage.Create('Filenames', pnlFilenames, TImages.TEXTFIELD_RENAME));
      FPageList.Add(TPage.Create('Advanced', pnlFilenamesExt, TImages.TEXTFIELD_RENAME_COG, FPageList.Find(pnlFilenames)));
      FPageList.Add(TPage.Create('Cut songs', pnlCut, TImages.CUT));
      FPageList.Add(TPage.Create('Postprocessing', pnlPostProcess, TImages.LIGHTNING));
      FPageList.Add(TPage.Create('Advanced', pnlAdvanced, TImages.COG));
    end;
  end;

  inherited RegisterGeneralPage(TImages.WRENCH_APPLICATION);
end;

function TfrmSettings.RemoveGray(C: TControl; ShowMessage: Boolean = True): Boolean;
begin
  Result := False;
  if FIgnoreFieldList = nil then
    Exit;

  if ShowMessage and (FIgnoreFieldList.IndexOf(C) > -1) and (not FOptionChanging) then
    TfrmMsgDlg.ShowMsg(Self, _('The setting''s configuration you are about to change differs for the selected streams. The new setting will be applied to every selected stream when saving settings using "OK".'),
      mtInformation, [mbOK], mbOK, 13);

  FIgnoreFieldList.Remove(C);

  if (TControl(C) is TEdit) or (TControl(C) is TLabeledEdit) then
    TEdit(C).Color := clWindow
  else if TControl(C) is TCheckBox then
  else if TControl(C) is TComboBoxEx then
  else if TControl(C) is TListView then
    TListView(C).Color := clWindow;

  Result := True;
end;

procedure TfrmSettings.SetFields;

  procedure AddField(F: TControl);
  begin
    if FIgnoreFieldList.IndexOf(F) = -1 then
      FIgnoreFieldList.Add(F);
  end;

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
    if GetStringListHash(S.IgnoreTrackChangePattern) <> GetStringListHash(FStreamSettings[i].IgnoreTrackChangePattern) then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstIgnoreTitles);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.FilePattern <> FStreamSettings[i].FilePattern then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtFilePattern.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.IncompleteFilePattern <> FStreamSettings[i].IncompleteFilePattern then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtIncompleteFilePattern.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.StreamFilePattern <> FStreamSettings[i].StreamFilePattern then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtStreamFilePattern.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.FilePatternDecimals <> FStreamSettings[i].FilePatternDecimals then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtFilePatternDecimals.Control);

  F := False;
  for i := 0 to Length(FStreamSettings) - 1 do
    if S.RemoveChars <> FStreamSettings[i].RemoveChars then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtRemoveChars.Control);

  F := False;
  for i := 0 to Length(FStreamSettings) - 1 do
    if S.NormalizeVariables <> FStreamSettings[i].NormalizeVariables then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkNormalizeVariables);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.DeleteStreams <> FStreamSettings[i].DeleteStreams then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkDeleteStreams);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.AddSavedToIgnore <> FStreamSettings[i].AddSavedToIgnore then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkAddSavedToIgnore);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.AddSavedToStreamIgnore <> FStreamSettings[i].AddSavedToStreamIgnore then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkAddSavedToStreamIgnore);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.RemoveSavedFromWishlist <> FStreamSettings[i].RemoveSavedFromWishlist then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkRemoveSavedFromWishlist);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.OverwriteSmaller <> FStreamSettings[i].OverwriteSmaller then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkOverwriteSmaller);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.DiscardSmaller <> FStreamSettings[i].DiscardSmaller then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkDiscardSmaller);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.DiscardAlways <> FStreamSettings[i].DiscardAlways then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkDiscardAlways);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SkipShort <> FStreamSettings[i].SkipShort then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSkipShort);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if GetStringListHash(S.RegExes) <> GetStringListHash(FStreamSettings[i].RegExes) then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstRegExes);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SearchSilence <> FStreamSettings[i].SearchSilence then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSearchSilence);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.AutoDetectSilenceLevel <> FStreamSettings[i].AutoDetectSilenceLevel then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkManualSilenceLevel);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SilenceLevel <> FStreamSettings[i].SilenceLevel then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSilenceLevel.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SilenceLength <> FStreamSettings[i].SilenceLength then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSilenceLength);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SilenceBufferSecondsStart <> FStreamSettings[i].SilenceBufferSecondsStart then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSilenceBufferSeconds);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.ShortLengthSeconds <> FStreamSettings[i].ShortLengthSeconds then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtShortLengthSeconds.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SongBuffer <> FStreamSettings[i].SongBuffer then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSongBuffer.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.MaxRetries <> FStreamSettings[i].MaxRetries then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtMaxRetries.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.RetryDelay <> FStreamSettings[i].RetryDelay then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtRetryDelay.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.Filter <> FStreamSettings[i].Filter then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstDefaultFilter);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SeparateTracks <> FStreamSettings[i].SeparateTracks then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSeparateTracks);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.SaveToMemory <> FStreamSettings[i].SaveToMemory then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSaveStreamsToDisk);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.OnlySaveFull <> FStreamSettings[i].OnlySaveFull then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkOnlySaveFull);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.AdjustTrackOffset <> FStreamSettings[i].AdjustTrackOffset then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkAdjustTrackOffset);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.AdjustTrackOffsetMS <> FStreamSettings[i].AdjustTrackOffsetMS then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtAdjustTrackOffset);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.AdjustTrackOffsetDirection <> FStreamSettings[i].AdjustTrackOffsetDirection then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
  begin
    AddField(optAdjustBackward);
    AddField(optAdjustForward);
  end;

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.OutputFormat <> FStreamSettings[i].OutputFormat then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstOutputFormat.Control);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.PostProcessors.Hash <> FStreamSettings[i].PostProcessors.Hash then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstPostProcess);

  F := False;
  for i := 1 to Length(FStreamSettings) - 1 do
    if S.EncoderSettings.Hash <> FStreamSettings[i].EncoderSettings.Hash then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(btnConfigureEncoder);

  // Gegen die Warnung..
  if ShowDialog then
  ;
end;

procedure TfrmSettings.SetGray;
var
  i: Integer;
begin
  if FIgnoreFieldList = nil then
    Exit;

  for i := 0 to FIgnoreFieldList.Count - 1 do
    if (TControl(FIgnoreFieldList[i]) is TEdit) or (TControl(FIgnoreFieldList[i]) is TLabeledEdit) then
      TEdit(FIgnoreFieldList[i]).Color := clGrayText
    else if TControl(FIgnoreFieldList[i]) is TCheckBox then
      TCheckBox(FIgnoreFieldList[i]).State := cbGrayed
    else if TControl(FIgnoreFieldList[i]) is TComboBoxEx then
    else if TControl(FIgnoreFieldList[i]) is TListView then
      TListView(FIgnoreFieldList[i]).Color := clGrayText;
end;

procedure TfrmSettings.SetPage(Page: TPage);
begin
  inherited;

  if Page = FPageList.Find(pnlFilenames) then
    txtPreview.Control.Text := '';
end;

procedure TfrmSettings.ShowEncoderNeededMessage;
begin
  TfrmMsgDlg.ShowMsg(Self, _('You enabled a postprocessor that needs a WAVE-file which will be reencoded after processing. ' +
    'Make sure an encoder for the stream''s format is installed if you did not select another encoder by checking the "Addons" page. ' +
    'To configure the encoder, select it at the top of the "Postprocessing" page and click the button next to it.'),
    mtInformation, [mbOK], mbOK, 14);
end;

procedure TfrmSettings.txtAdjustTrackOffsetChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtAdjustTrackOffset);
end;

procedure TfrmSettings.txtAppParamsChange(Sender: TObject);
begin
  inherited;
  if (lstPostProcess.Selected <> nil) and txtAppParams.Focused then
    TExternalPostProcess(lstPostProcess.Selected.Data).Params := txtAppParams.Control.Text;
end;

procedure TfrmSettings.txtFilePatternChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(Sender as TEditButton);

    FActivePreviewField := Sender as TEditButton;

    if Sender = txtAutomaticFilePattern.Control then
      txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|streamname|day|month|year|hour|minute|second')
    else if Sender = txtStreamFilePattern.Control then
      txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'streamname|day|month|year|hour|minute|second')
    else
      txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second');

    if Trim(RemoveFileExt(txtPreview.Control.Text)) = '' then
      txtPreview.Control.Text := '';
  end;
end;

procedure TfrmSettings.txtFilePatternDecimalsChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(txtFilePatternDecimals);
end;

procedure TfrmSettings.txtHotkeyChange(Sender: TObject);
begin
  if Assigned(lstHotkeys.Selected) then
  begin
    lstHotkeys.Selected.SubItems[0] := ShortCutToText(txtHotkey.Control.HotKey);
    lstHotkeys.Selected.Data := Pointer(txtHotkey.Control.HotKey);
  end;
end;

procedure TfrmSettings.txtIgnoreTitlePatternChange(Sender: TObject);
begin
  btnAddIgnoreTitlePattern.Enabled := Length(Trim(txtIgnoreTitlePattern.Control.Text)) >= 1;
end;

procedure TfrmSettings.txtFilePatternEnter(Sender: TObject);
begin
  inherited;

  FActivePreviewField := Sender as TEditButton;

  if Sender = txtAutomaticFilePattern.Control then
    txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|streamname|day|month|year|hour|minute|second')
  else if Sender = txtStreamFilePattern.Control then
    txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'streamname|day|month|year|hour|minute|second')
  else
    txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second');

  if Trim(RemoveFileExt(txtPreview.Control.Text)) = '' then
    txtPreview.Control.Text := '';
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

procedure TfrmSettings.txtRegExChange(Sender: TObject);
begin
  inherited;

  btnAddRegEx.Enabled := Length(Trim(txtRegEx.Control.Text)) >= 1;
end;

procedure TfrmSettings.UpdatePostProcessUpDown;
begin
  // btnMoveUp.Enabled := (lstPostProcess.Selected <> nil) and (TObject(lstPostProcess.Selected.Data) is TExternalPostProcess) and (not (lstPostProcess.Selected.Index = 0)) and (not (lstPostProcess.Items[lstPostProcess.Selected.Index - 1].GroupID <> lstPostProcess.Selected.GroupID));
  // btnMoveDown.Enabled := (lstPostProcess.Selected <> nil) and (TObject(lstPostProcess.Selected.Data) is TExternalPostProcess) and (not (lstPostProcess.Selected.Index = lstPostProcess.Items.Count - 1)) and (not (lstPostProcess.Items[lstPostProcess.Selected.Index + 1].GroupID <> lstPostProcess.Selected.GroupID));
end;

procedure TfrmSettings.BlacklistTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnBlacklistRemove.Enabled := lstBlacklist.SelectedCount > 0;
end;

procedure TfrmSettings.BlacklistTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    btnBlacklistRemoveClick(nil);
end;

procedure TfrmSettings.btnAddIgnoreTitlePatternClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := lstIgnoreTitles.Items.Add;
  Item.Caption := txtIgnoreTitlePattern.Control.Text;
  Item.ImageIndex := TImages.DECLINE;
  txtIgnoreTitlePattern.Control.Text := '';
  txtIgnoreTitlePattern.Control.ApplyFocus;

  RemoveGray(lstIgnoreTitles);
end;

procedure TfrmSettings.btnAddRegExClick(Sender: TObject);
var
  Item: TListItem;
  RegExp: string;
begin
  RegExp := txtRegEx.Control.Text;
  if not CheckRegExp(Handle, RegExp, lstRegExes, nil) then
    Exit;

  Item := lstRegExes.Items.Add;
  Item.Caption := RegExp;
  Item.ImageIndex := TImages.FONT;
  txtRegEx.Control.Text := '';
  txtRegEx.Control.ApplyFocus;

  RemoveGray(lstRegExes);
end;

procedure TfrmSettings.btnAddClick(Sender: TObject);

  function HighestGroupIndex(GroupID: Integer): Integer;
  var
    i: Integer;
    MaxVal: Integer;
  begin
    MaxVal := -1;
    for i := 0 to FTemporaryPostProcessors.Count - 1 do
      if (FTemporaryPostProcessors[i].GroupID = GroupID) and (i > MaxVal) then
        MaxVal := i;
    Result := MaxVal;
  end;

var
  i: Integer;
  Item: TListItem;
  PostProcessor: TExternalPostProcess;
  AddPostProcessorForm: TfrmSettingsAddPostProcessor;
begin
  inherited;

  if not FInitialized then
    Exit;

  RemoveGray(lstPostProcess);

  AddPostProcessorForm := TfrmSettingsAddPostProcessor.Create(Self);
  try
    AddPostProcessorForm.ShowModal;
    if AddPostProcessorForm.Result <= 1 then
      if dlgOpen.Execute then
        if FileExists(dlgOpen.FileName) then
        begin
          Item := lstPostProcess.Items.Insert(HighestGroupIndex(AddPostProcessorForm.Result) + 1);
          Item.Caption := ExtractFileName(dlgOpen.FileName);
          PostProcessor := TExternalPostProcess.Create(dlgOpen.FileName, '"%filename%"', True, False, GetNewID, 100000, AddPostProcessorForm.Result);
          PostProcessor.IsNew := True;
          FTemporaryPostProcessors.Insert(HighestGroupIndex(AddPostProcessorForm.Result) + 1, PostProcessor);
          // Item.GroupID := PostProcessor.GroupID;
          Item.Checked := PostProcessor.Active;
          Item.Data := PostProcessor;
          Item.ImageIndex := TImages.APPLICATION_XP_TERMINAL;
          Item.Selected := True;

          if TPostProcessBase(Item.Data).NeedsWave then
            ShowEncoderNeededMessage;

          RebuildPostProcessingList;

          for i := 0 to lstPostProcess.Items.Count - 1 do
            if TPostProcessBase(lstPostProcess.Items[i].Data) = PostProcessor then
            begin
              lstPostProcess.Items[i].Selected := True;
              Break;
            end;
        end;
  finally
    AddPostProcessorForm.Free;
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
    if FileExists(dlgOpen.FileName) then
    begin
      txtApp.Control.Text := dlgOpen.FileName;
      lstPostProcess.Selected.Caption := ExtractFileName(dlgOpen.FileName);
      TExternalPostProcess(lstPostProcess.Selected.Data).Exe := dlgOpen.FileName;
    end;
end;

procedure TfrmSettings.btnBrowseClick(Sender: TObject);
var
  Msg: string;
  Dir: string;
begin
  if FSettingsType = stAuto then
    Msg := 'Select folder for automatically saved songs'
  else
    Msg := 'Select folder for saved songs';

  Dir := BrowseDialog(Handle, _(Msg), BIF_RETURNONLYFSDIRS);

  if Dir = '' then
    Exit;

  if DirectoryExists(Dir) then
    txtDir.Control.Text := Dir
  else
    MsgBox(Self.Handle, _('The selected folder does not exist. Please choose another one.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.btnBrowseLogFileClick(Sender: TObject);
begin
  inherited;

  if dlgSave.Execute then
    if dlgSave.FileName <> '' then
    begin
      if ExtractFileExt(LowerCase(dlgSave.FileName)) = '' then
        dlgSave.FileName := dlgSave.FileName + '.txt';
      txtLogFile.Control.Text := dlgSave.FileName;
    end;
end;

procedure TfrmSettings.btnConfigureClick(Sender: TObject);
begin
  inherited;

  if not FInitialized then
    Exit;

  if lstPostProcess.Selected <> nil then
  begin
    RemoveGray(lstPostProcess);

    TPostProcessBase(lstPostProcess.Selected.Data).Configure(Self, 0, True);
  end;
end;

procedure TfrmSettings.btnConfigureEncoderClick(Sender: TObject);
var
  i: Integer;
  F: TfrmConfigureEncoder;
  EncoderSettings: TEncoderSettings;
begin
  inherited;

  if not FInitialized then
    Exit;

  RemoveGray(btnConfigureEncoder);

  EncoderSettings := FStreamSettings[0].EncoderSettings.Find(TAudioTypes(lstOutputFormat.Control.ItemIndex)).Copy;

  F := TfrmConfigureEncoder.Create(Self, EncoderSettings);
  try
    F.ShowModal;

    if F.Save then
      for i := 0 to High(FStreamSettings) do
        FStreamSettings[i].EncoderSettings.Find(TAudioTypes(lstOutputFormat.Control.ItemIndex)).Assign(F.EncoderSettings);
  finally
    EncoderSettings.Free;
    F.Free;
  end;
end;

procedure TfrmSettings.btnHelpClick(Sender: TObject);
var
  PostProcess: TPostProcessBase;
begin
  if lstPostProcess.Selected = nil then
    Exit;
  PostProcess := lstPostProcess.Selected.Data;
  MessageBox(Handle, PChar(PostProcess.Help), 'Info', MB_ICONINFORMATION);
end;

procedure TfrmSettings.btnMoveClick(Sender: TObject);
var
  i: integer;
  Selected: TPostProcessBase;
begin
  if lstPostProcess.Selected = nil then
    Exit;

  Selected := TPostProcessBase(lstPostProcess.Selected.Data);

  for i := 0 to FTemporaryPostProcessors.Count - 1 do
    if FTemporaryPostProcessors[i] = TPostProcessBase(lstPostProcess.Selected.Data) then
    begin
      if Sender = btnMoveUp then
        FTemporaryPostProcessors.Exchange(i, i - 1)
      else
        FTemporaryPostProcessors.Exchange(i, i + 1);
      Break;
    end;

  RebuildPostProcessingList;

  for i := 0 to lstPostProcess.Items.Count - 1 do
    if TPostProcessBase(lstPostProcess.Items[i].Data) = Selected then
    begin
      lstPostProcess.Items[i].Selected := True;
      Break;
    end;

  UpdatePostProcessUpDown;
end;

procedure TfrmSettings.btnRemoveClick(Sender: TObject);
begin
  if not FInitialized then
    Exit;

  if lstPostProcess.Selected <> nil then
  begin
    RemoveGray(lstPostProcess);

    FTemporaryPostProcessors.Remove(TExternalPostProcess(lstPostProcess.Selected.Data));
    TExternalPostProcess(lstPostProcess.Selected.Data).Free;

    RebuildPostProcessingList;
  end;
end;

procedure TfrmSettings.btnRemoveIgnoreTitlePatternClick(Sender: TObject);
begin
  txtIgnoreTitlePattern.Control.Text := lstIgnoreTitles.Selected.Caption;
  lstIgnoreTitles.Items.Delete(lstIgnoreTitles.Selected.Index);

  RemoveGray(lstIgnoreTitles);
end;

procedure TfrmSettings.btnRemoveRegExClick(Sender: TObject);
begin
  txtRegEx.Control.Text := lstRegExes.Selected.Caption;
  lstRegExes.Items.Delete(lstRegExes.Selected.Index);

  RemoveGray(lstRegExes);
end;

procedure TfrmSettings.btnResetClick(Sender: TObject);
var
  i: Integer;
begin
  FInitialized := False;
  if FIgnoreFieldList <> nil then
    while FIgnoreFieldList.Count > 0 do
      RemoveGray(TControl(FIgnoreFieldList[0]), False);
  FillFields(AppGlobals.Data.StreamSettings);

  btnConfigureEncoder.Enabled := TAudioTypes(lstOutputFormat.Control.ItemIndex) <> atNone;

  if TAudioTypes(lstOutputFormat.Control.ItemIndex) <> atNone then
    for i := 0 to High(FStreamSettings) do
      FStreamSettings[i].EncoderSettings.Find(TAudioTypes(lstOutputFormat.Control.ItemIndex)).Assign(AppGlobals.Data.StreamSettings.EncoderSettings.Find(TAudioTypes(lstOutputFormat.Control.ItemIndex)));

  FInitialized := True;
end;

procedure TfrmSettings.btnResetColorClick(Sender: TObject);
var
  Tree: TVirtualStringTree;
begin
  Tree := TVirtualStringTree.Create(Self);
  try
    pnlNodeTextColor.Color := Tree.Colors.NodeFontColor;
    pnlNodeTextColorSelected.Color := Tree.Colors.NodeFontColor;
    pnlNodeTextColorSelectedFocused.Color := Tree.Colors.SelectionTextColor;
    pnlNodeBackgroundColor.Color := Tree.Colors.BackGroundColor;
  finally
    Tree.Free;
  end;
end;

procedure TfrmSettings.btnResetFilePatternClick(Sender: TObject);
begin
  inherited;

  if Sender = txtFilePattern.Control then
  begin
    txtFilePattern.Control.Text := '%streamname%\%artist% - %title%';
    txtFilePattern.Control.ApplyFocus;
    RemoveGray(txtFilePattern.Control);
  end else if Sender = txtIncompleteFilePattern.Control then
  begin
    txtIncompleteFilePattern.Control.Text := '%streamname%\%artist% - %title%';
    txtIncompleteFilePattern.Control.ApplyFocus;
    RemoveGray(txtIncompleteFilePattern.Control);
  end else if Sender = txtAutomaticFilePattern.Control then
  begin
    txtAutomaticFilePattern.Control.Text := '%streamname%\%artist% - %title%';
    txtAutomaticFilePattern.Control.ApplyFocus;
    RemoveGray(txtAutomaticFilePattern.Control);
  end else
  begin
    txtStreamFilePattern.Control.Text := '%streamname%';
    txtStreamFilePattern.Control.ApplyFocus;
  end;
end;

procedure TfrmSettings.btnResetRemoveCharsClick(Sender: TObject);
begin
  inherited;

  txtRemoveChars.Control.Text := '[]{}#$§%~^';
  txtRemoveChars.Control.ApplyFocus;
  RemoveGray(txtRemoveChars.Control);
end;

procedure TfrmSettings.btnResetTitlePatternClick(Sender: TObject);
begin
  inherited;

  txtRegEx.Control.Text := DEFAULT_TITLE_REGEXP;
  txtRegEx.Control.ApplyFocus;
end;

procedure TfrmSettings.BuildHotkeys;
var
  Item: TListItem;
begin
  if lstHotkeys.Items.Count = 0 then
  begin
    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPlay));
    Item.Data := Pointer(AppGlobals.ShortcutPlay);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPause));
    Item.Data := Pointer(AppGlobals.ShortcutPause);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutStop));
    Item.Data := Pointer(AppGlobals.ShortcutStop);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutNext));
    Item.Data := Pointer(AppGlobals.ShortcutNext);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutPrev));
    Item.Data := Pointer(AppGlobals.ShortcutPrev);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutVolUp));
    Item.Data := Pointer(AppGlobals.ShortcutVolUp);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutVolDown));
    Item.Data := Pointer(AppGlobals.ShortcutVolDown);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutMute));
    Item.Data := Pointer(AppGlobals.ShortcutMute);
    Item.ImageIndex := TImages.KEYBOARD;
  end;

  lstHotkeys.Items[0].Caption := _('Play');
  lstHotkeys.Items[1].Caption := _('Pause');
  lstHotkeys.Items[2].Caption := _('Stop');
  lstHotkeys.Items[3].Caption := _('Next stream');
  lstHotkeys.Items[4].Caption := _('Previous stream');
  lstHotkeys.Items[5].Caption := _('Volume up');
  lstHotkeys.Items[6].Caption := _('Volume down');
  lstHotkeys.Items[7].Caption := _('Mute');
end;

function TfrmSettings.CanFinish: Boolean;

  function ControlVisible(C: TControl): Boolean;
  var
    i: Integer;
    P: TControl;
  begin
    if not C.Visible then
      Exit(False);

    for i := 0 to FPageList.Count - 1 do
    begin
      P := C.Parent;
      while not P.InheritsFrom(TForm) do
      begin
        if P = FPageList[i].Panel then
          if P.Visible then
            Exit(True)
          else
            Exit(False);
        P := P.Parent;
      end;
    end;
    Exit(False);
  end;

var
  i, n: Integer;
begin
  Result := inherited;

  if not Result then
    Exit;

  if ControlVisible(txtFilePattern) and (Trim(RemoveFileExt(ValidatePattern(txtFilePattern.Control.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second'))) = '') then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of completely recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtFilePattern.Parent)));
    txtFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtIncompleteFilePattern) and (Trim(RemoveFileExt(ValidatePattern(txtIncompleteFilePattern.Control.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second'))) = '') then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of incompletely recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtIncompleteFilePattern.Parent)));
    txtIncompleteFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtAutomaticFilePattern) and (Trim(RemoveFileExt(ValidatePattern(txtAutomaticFilePattern.Control.Text, 'artist|title|album|genre|streamtitle|streamname|day|month|year|hour|minute|second'))) = '') then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of automatically recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtAutomaticFilePattern.Parent)));
    txtAutomaticFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtStreamFilePattern) and (Trim(RemoveFileExt(ValidatePattern(txtStreamFilePattern.Control.Text, 'streamname|day|month|year|hour|minute|second'))) = '') then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of stream files so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtStreamFilePattern.Parent)));
    txtStreamFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtDir) and (not DirectoryExists(txtDir.Control.Text)) then
  begin
    if FSettingsType = stAuto then
      MsgBox(Handle, _('The selected folder for automatically saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION)
    else
      MsgBox(Handle, _('The selected folder for saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtDir.Parent)));
    btnBrowseClick(nil);
    Exit;
  end;

  if chkMonitorMode.Checked then
    if ControlVisible(txtMonitorCount) and (txtMonitorCount.Control.Value > 50) then
      if TfrmMsgDlg.ShowMsg(GetParentForm(Self), _(
        'You entered a high number for streams to monitor. This affects your bandwidth and resources in general. streamWriter might become slow and unresponsible depending on your system. Are you sure you want to do this?'),
        mtConfirmation, mbOKCancel, mbCancel, 17) = mrCancel then
      begin
        SetPage(FPageList.Find(TPanel(txtMonitorCount.Parent)));
        txtMonitorCount.Control.ApplyFocus;
        Exit;
      end;

  if ControlVisible(lstHotkeys) then
    for i := 0 to lstHotkeys.Items.Count - 1 do
      for n := 0 to lstHotkeys.Items.Count - 1 do
        if (lstHotkeys.Items[i] <> lstHotkeys.Items[n]) and Assigned(lstHotkeys.Items[i].Data) and (lstHotkeys.Items[i].Data = lstHotkeys.Items[n].Data) then
        begin
          MsgBox(Handle, _('A hotkey can be defined only once. Please edit the key mappings.'), _('Info'), MB_ICONINFORMATION);
          SetPage(FPageList.Find(pnlHotkeys));
          Exit;
        end;

  if ControlVisible(txtRetryDelay) and (txtRetryDelay.Control.Value > 999) then
    txtRetryDelay.Control.Value := 999;

  Result := True;
end;

function TfrmSettings.CheckImportFile(Filename: string): Boolean;
var
  S: TExtendedStream;
begin
  Result := inherited;

  try
    S := TExtendedStream.Create;
    try
      S.LoadFromFile(Filename);
      TDataLists.VerifyMagic(S, 10, False);
    finally
      S.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      if E is EUnsupportedFormatException then
        MsgBox(0, _('The file could not be imported because it contains regular saved data and no exported profile.'), _('Error'), MB_ICONERROR)
      else if E is EUnknownFormatException then
        MsgBox(0, _('The file could not be imported because it''s format is unknown.'), _('Error'), MB_ICONERROR)
      else
        MsgBox(0, _('The file could not be imported.'), _('Error'), MB_ICONERROR);
    end;
  end;
end;

procedure TfrmSettings.chkAddSavedToIgnoreClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkAddSavedToIgnore);
end;

procedure TfrmSettings.chkAddSavedToStreamIgnoreClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkAddSavedToStreamIgnore);
end;

procedure TfrmSettings.chkAdjustTrackOffsetClick(Sender: TObject);
begin
  inherited;

  pnlAdjustTrackOffset.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;
  optAdjustBackward.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;
  optAdjustForward.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;

  if FInitialized then
    RemoveGray(chkAdjustTrackOffset);
end;

procedure TfrmSettings.chkManualSilenceLevelClick(Sender: TObject);
begin
  inherited;

  txtSilenceLevel.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);

  if FInitialized then
    RemoveGray(chkManualSilenceLevel);
end;

procedure TfrmSettings.chkMonitorModeClick(Sender: TObject);
begin
  inherited;

  Label20.Enabled := (chkMonitorMode.State <> cbUnchecked) or (chkSubmitStats.State <> cbUnchecked);
  txtMonitorCount.Enabled := (chkMonitorMode.State <> cbUnchecked) and (chkSubmitStats.State <> cbUnchecked);
end;

procedure TfrmSettings.chkAutoTuneInClick(Sender: TObject);
begin
  inherited;

  lstMinQuality.Enabled := chkAutoTuneIn.Checked;
  lstFormat.Enabled := chkAutoTuneIn.Checked;
  chkAutoTuneInConsiderIgnore.Enabled := chkAutoTuneIn.Checked;
  chkAutoTuneInAddToIgnore.Enabled := chkAutoTuneIn.Checked;
  chkAutoRemoveSavedFromWishlist.Enabled := chkAutoTuneIn.Checked;
end;

procedure TfrmSettings.chkOverwriteSmallerClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkOverwriteSmaller);
end;

procedure TfrmSettings.chkRemoveSavedFromWishlistClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkRemoveSavedFromWishlist);
end;

procedure TfrmSettings.chkDeleteStreamsClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkDeleteStreams);
end;

procedure TfrmSettings.chkDiscardAlwaysClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(chkDiscardAlways);

    FOptionChanging := True;

    if chkDiscardAlways.Checked then
    begin
      chkDiscardSmaller.Enabled := False;
      chkDiscardSmaller.Checked := False;
      chkOverwriteSmaller.Enabled := False;
      chkOverwriteSmaller.Checked := False;
    end else
    begin
      chkDiscardSmaller.Enabled := True;
      chkDiscardSmaller.Checked := FStreamSettings[0].DiscardSmaller;
      chkOverwriteSmaller.Enabled := True;
      chkOverwriteSmaller.Checked := FStreamSettings[0].OverwriteSmaller;
    end;

    FOptionChanging := False;
  end;
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

procedure TfrmSettings.chkNormalizeVariablesClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkNormalizeVariables);
end;

procedure TfrmSettings.chkOnlyIfCutClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(lstPostProcess);

  if (lstPostProcess.Selected <> nil) and chkOnlyIfCut.Focused then
    TPostProcessBase(lstPostProcess.Selected.Data).OnlyIfCut := chkOnlyIfCut.Checked;
end;

procedure TfrmSettings.chkOnlySaveFullClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(chkOnlySaveFull);

    if (FSettingsType = stStream) and (Length(FStreamSettings) > 0) and (not FOptionChanging) then
      TfrmMsgDlg.ShowMsg(Self, _(WARNING_STREAMRECORDING),
        mtInformation, [mbOK], mbOK, 5);
  end;
end;

procedure TfrmSettings.chkSeparateTracksClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(chkSeparateTracks);

    FOptionChanging := True;

    chkDeleteStreams.Enabled := chkSeparateTracks.Checked;
    chkDeleteStreams.Checked := chkSaveStreamsToDisk.Checked and FStreamSettings[0].DeleteStreams;

    chkOnlySaveFull.Enabled := chkSeparateTracks.Checked;
    chkOnlySaveFull.Checked := chkSeparateTracks.Checked and FStreamSettings[0].OnlySaveFull;

    pnlCut.Enabled := False;
    if (not chkSeparateTracks.Checked) or (not chkSaveStreamsToDisk.Checked) then
      chkDeleteStreams.Checked := False;

    FOptionChanging := False;

    Application.ProcessMessages;

    EnablePanel(pnlCut, (not chkSaveStreamsToDisk.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled)) or (FSettingsType = stAuto));

    chkSkipShort.Enabled := (not chkSaveStreamsToDisk.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled)) or (FSettingsType = stAuto);
    txtShortLengthSeconds.Enabled := chkSkipShort.Enabled and chkSkipShort.Checked;
    if (not chkSeparateTracks.Checked) or (not chkSaveStreamsToDisk.Checked) then
      chkSkipShort.Checked := False;
  end;
end;

procedure TfrmSettings.chkSaveStreamsToDiskClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(chkSaveStreamsToDisk);

    chkSeparateTracks.Enabled := chkSaveStreamsToDisk.Checked;
    chkSeparateTracks.Checked := True;

    // Weil das hier drüber die Seite abschaltet, schalten wir sie wieder an..
    EnablePanel(pnlCut, (not chkSaveStreamsToDisk.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled)) or (FSettingsType = stAuto));
    chkDeleteStreams.Enabled := (not chkSeparateTracks.Checked) or (chkSaveStreamsToDisk.Checked);
    chkDeleteStreams.Checked := chkDeleteStreams.Enabled and FStreamSettings[0].DeleteStreams;

    chkSkipShort.Enabled := (not chkSaveStreamsToDisk.Checked or (chkSeparateTracks.Checked and chkSeparateTracks.Enabled)) or (FSettingsType = stAuto);
    txtShortLengthSeconds.Enabled := chkSkipShort.Enabled and chkSkipShort.Checked;

    if (FSettingsType = stStream) and (Length(FStreamSettings) > 0) then
      TfrmMsgDlg.ShowMsg(Self, _(WARNING_STREAMRECORDING),
        mtInformation, [mbOK], mbOK, 3);
  end;
end;

procedure TfrmSettings.chkSearchSilenceClick(Sender: TObject);
begin
  inherited;

  txtSilenceBufferSeconds.Enabled := chkSearchSilence.Checked;
  Label12.Enabled := chkSearchSilence.Checked;
  Label13.Enabled := chkSearchSilence.Checked;
  Label6.Enabled := chkSearchSilence.Checked;
  Label15.Enabled := chkSearchSilence.Checked;

  chkManualSilenceLevelClick(chkManualSilenceLevel);

  chkManualSilenceLevel.Enabled := chkSearchSilence.Checked;

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

procedure TfrmSettings.chkSubmitStatsClick(Sender: TObject);
begin
  inherited;

  //Label8.Enabled := chkSubmitStats.State <> cbUnchecked;
  chkMonitorMode.Enabled := chkSubmitStats.State <> cbUnchecked;

  Label20.Enabled := chkMonitorMode.Enabled;
  txtMonitorCount.Enabled := (chkMonitorMode.State <> cbUnchecked) and (chkSubmitStats.State <> cbUnchecked);
end;

procedure TfrmSettings.chkSubmitStreamInfoClick(Sender: TObject);
begin
  inherited;

  //Label2.Enabled := chkSubmitStreamInfo.State <> cbUnchecked;
end;

procedure TfrmSettings.chkTrayClick(Sender: TObject);
begin
  inherited;

  optClose.Enabled := chkTray.Checked;
  optMinimize.Enabled := chkTray.Checked;
end;

constructor TfrmSettings.Create(AOwner: TComponent; SettingsType: TSettingsTypes; StreamSettings: TStreamSettingsArray; BrowseDir: Boolean);
var
  i: Integer;
begin
  FSettingsType := SettingsType;

  FIgnoreFieldList := TList.Create;

  SetLength(FStreamSettings, Length(StreamSettings));
  for i := 0 to Length(StreamSettings) - 1 do
    FStreamSettings[i] := StreamSettings[i].Copy;

  case SettingsType of
    stApp:
      CreateApp(AOwner, BrowseDir);
    stAuto:
      CreateAuto(AOwner, BrowseDir);
    stStream:
      CreateStreams(AOwner);
  end;

  lblPanelCut.Caption := _('Settings for cutting are only available'#13#10'if ''Save separated tracks'' is enabled.');
  lblPanelCut.Align := alClient;
  lblPanelCut.Layout := tlCenter;
  lblPanelCut.Alignment := taCenter;

  FInitialized := True;
end;

procedure TfrmSettings.CreateApp(AOwner: TComponent; BrowseDir: Boolean);
var
  i, Tmp: Integer;
  Item: TListItem;
begin
  FBrowseDir := BrowseDir;

  // Wir geben AOwner mit, so dass das MsgDlg zentriert angezeigt wird.
  // Self ist nämlich noch nicht Visible, haben kein Handle, etc..
  if not BrowseDir then
    TfrmMsgDlg.ShowMsg(TForm(AOwner), _('Settings from the categories "Streams", "Filenames", "Cut", "Postprocessing" and "Advanced" configured in the general settings window are only applied to new streams you add to the list.'#13#10 +
      'To change those settings for streams in the list, select these streams, then right-click one of them and select "Settings" from the popupmenu.'), mtInformation, [mbOK], mbOK, 4);

  inherited Create(AOwner, modSharedData.imgImages, True);

  modSharedData.imgImages.GetIcon(TImages.WRENCH_APPLICATION, Icon);

  FillFields(FStreamSettings[0]);

  // Dateinamen ordentlich machen
  //Tmp := txtStreamFilePattern.Top;
  txtAutomaticFilePattern.Visible := False;
  //btnResetAutomaticFilePattern.Visible := False;
  //txtStreamFilePattern.Top := txtAutomaticFilePattern.Top;
  //btnResetStreamFilePattern.Top := btnResetAutomaticFilePattern.Top;
  //txtPreview.Top := Tmp;
  //lblFilePattern.Top := txtPreview.Top + txtPreview.Height + MulDiv(8, Screen.PixelsPerInch, 96);

  // Offseteinstellungen verstecken
  chkAdjustTrackOffset.Visible := False;
  pnlAdjustTrackOffset.Visible := False;
  optAdjustBackward.Visible := False;
  optAdjustForward.Visible := False;

  for i := 0 to AppGlobals.AddonManager.Addons.Count - 1 do
  begin
    Item := lstAddons.Items.Add;
    Item.Caption := AppGlobals.AddonManager.Addons[i].Name;
    Item.Checked := AppGlobals.AddonManager.Addons[i].FilesExtracted;
    Item.Data := AppGlobals.AddonManager.Addons[i].Copy;

    Item.ImageIndex := TImages.PLUGIN;
  end;
  if lstAddons.Items.Count > 0 then
    lstAddons.Items[0].Selected := True;

  BuildHotkeys;

  if (Bass.DeviceAvailable) and (Bass.Devices.Count > 0) then
  begin
    for i := 0 to Bass.Devices.Count - 1 do
      if Bass.Devices[i].IsDefault then
        lstSoundDevice.Control.ItemsEx.AddItem(_('Default device'), TImages.SOUND, -1, -1, -1, Bass.Devices[i])
      else
        lstSoundDevice.Control.ItemsEx.AddItem(Bass.Devices[i].Name, TImages.SOUND, -1, -1, -1, Bass.Devices[i]);

    if lstSoundDevice.Control.ItemsEx.Count > 0 then
      lstSoundDevice.Control.ItemIndex := 0;

    for i := 0 to lstSoundDevice.Control.ItemsEx.Count - 1 do
      if TBassDevice(lstSoundDevice.Control.ItemsEx[i].Data).ID = AppGlobals.SoundDevice then
      begin
        lstSoundDevice.Control.ItemIndex := i;
        Break;
      end;
  end else
    lstSoundDevice.Control.Enabled := False;

  if AppGlobals.NodeColorsLoaded then
  begin
    pnlNodeTextColor.Color := AppGlobals.NodeTextColor;
    pnlNodeTextColorSelected.Color := AppGlobals.NodeTextColorSelected;
    pnlNodeTextColorSelectedFocused.Color := AppGlobals.NodeTextColorSelectedFocused;
    pnlNodeBackgroundColor.Color := AppGlobals.NodeBackgroundColor;
  end else
    btnResetColorClick(btnReset);

  CreateGeneral;
end;

procedure TfrmSettings.CreateAuto(AOwner: TComponent; BrowseDir: Boolean);
var
  i: Integer;
begin
  FBrowseDir := BrowseDir;

  inherited Create(AOwner, modSharedData.imgImages, False);

  modSharedData.imgImages.GetIcon(TImages.WRENCH_BRICKS, Icon);

  FillFields(FStreamSettings[0]);

  lstBlacklist := TBlacklistTree.Create(Self, AppGlobals.Data.StreamBlacklist);
  lstBlacklist.OnChange := BlacklistTreeChange;
  lstBlacklist.OnKeyDown := BlacklistTreeKeyDown;
  lstBlacklist.Images := modSharedData.imgImages;
  lstBlacklist.Parent := pnlBlacklist;
  lstBlacklist.Align := alClient;

  // Werbung überspringen ausblenden
  chkSkipShort.Visible := False;
  txtShortLengthSeconds.Visible := False;

  // Offseteinstellungen verstecken
  chkAdjustTrackOffset.Visible := False;
  pnlAdjustTrackOffset.Visible := False;
  optAdjustBackward.Visible := False;
  optAdjustForward.Visible := False;

  // Dateinamen ordentlich machen
  for i := 0 to pnlFilenames.ControlCount - 1 do
    if ((pnlFilenames.Controls[i].ClassType = TLabeledEdit) or (pnlFilenames.Controls[i].ClassType = TSpeedButton)) and (pnlFilenames.Controls[i].Top > txtDir.Top) then
      pnlFilenames.Controls[i].Visible := False;

  txtAutomaticFilePattern.Visible := True;
 // txtPreview.Top := txtIncompleteFilePattern.Top;
  txtPreview.Visible := True;
 // lblFilePattern.Top := txtPreview.Top + txtPreview.Height + MulDiv(8, Screen.PixelsPerInch, 96);

  chkAutoTuneInAddToIgnore.Checked := FStreamSettings[0].AddSavedToIgnore;
  chkAutoRemoveSavedFromWishlist.Checked := FStreamSettings[0].RemoveSavedFromWishlist;

  Caption := _('Settings for automatic recordings');
  lblTop.Caption := _('Settings for automatic recordings');

  chkAutoTuneInClick(chkAutoTuneIn);

  CreateGeneral;
end;

procedure TfrmSettings.CreateStreams(AOwner: TComponent);
var
  i: Integer;
  Item: TListItem;
begin
  inherited Create(AOwner, modSharedData.imgImages, False);

  modSharedData.imgImages.GetIcon(TImages.WRENCH_TRANSMIT, Icon);

  SetFields;
  FillFields(FStreamSettings[0]);

  CreateGeneral;

  txtDir.Visible := False;

  // Dateinamen ordentlich machen
  txtAutomaticFilePattern.Visible := False;

  // Erweitert ordentlich machen
  lstSoundDevice.Visible := False;
  txtLogFile.Visible := False;

  btnReset := TBitBtn.Create(Self);
  btnReset.Width := MulDiv(210, Screen.PixelsPerInch, 96);
  btnReset.Align := alLeft;
  btnReset.Parent := pnlNav;
  btnReset.Caption := _('A&pply general settings');
  btnReset.OnClick := btnResetClick;

  for i := 0 to FStreamSettings[0].RegExes.Count - 1 do
  begin
    Item := lstRegExes.Items.Add;
    Item.Caption := FStreamSettings[0].RegExes[i];
    Item.ImageIndex := TImages.FONT;
  end;

  for i := 0 to FStreamSettings[0].IgnoreTrackChangePattern.Count - 1 do
  begin
    Item := lstIgnoreTitles.Items.Add;
    Item.Caption := FStreamSettings[0].IgnoreTrackChangePattern[i];
    Item.ImageIndex := TImages.DECLINE;
  end;

  Caption := _('Stream settings');
  lblTop.Caption := _('Stream settings');
end;

// TODO: was ist das hier?
procedure TfrmSettings.CreateGeneral;
var
  i: Integer;
begin
  for i := 0 to Self.ControlCount - 1 do
    if Self.Controls[i] is TPanel then
    begin
      if TPanel(Self.Controls[i]) = pnlLeft then
        Continue;
      Self.Controls[i].Left := 96;
      Self.Controls[i].Top := 36;
      TPanel(Self.Controls[i]).BevelOuter := bvNone;
    end;

  btnConfigureEncoder.Enabled := lstOutputFormat.Control.ItemIndex > 0;
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

procedure TBlacklistTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String);
var
  NodeData: PBlacklistNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  case Column of
    0: Text := NodeData.Name;
  end;
end;

function TBlacklistTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin
  Result := Images;

  if Column = 0 then
    Index := TImages.DECLINE;
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
    end else if Header.SortDirection = sdAscending then
      Header.SortDirection := sdDescending
    else
      Header.SortDirection := sdAscending;
    Sort(nil, HitInfo.Column, Header.SortDirection);
  end;
end;

function TBlacklistTree.DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer;
var
  S: string;
  NodeData: PBlacklistNodeData;
begin
  S := Text;
  NodeData := GetNodeData(Node);
  Result := StrLIComp(PChar(S), PChar(NodeData.Name), Min(Length(S), Length(NodeData.Name)));
end;

procedure TBlacklistTree.RemoveSelected;
var
  Node, Node2: PVirtualNode;
begin
  Node := GetLast;
  BeginUpdate;
  while Node <> nil do
    if Selected[Node] then
    begin
      Node2 := GetPrevious(Node);
      DeleteNode(Node);
      Node := Node2;
    end else
      Node := GetPrevious(Node);
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

function TBlacklistTree.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;

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
