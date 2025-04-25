{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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
  ComboEx,
  ComCtrls,
  CommCtrl,
  ConfigureEncoder,
  Constants,
  Controls,
  DataManager,
  Dialogs,
  uMetaDarkStyle,
  uDarkStyleSchemes,
  DynBASS,
  EditBtn,
  ExtCtrls,
  Forms,
  Functions,
  Generics.Collections,
  Generics.Defaults,
  Graphics,
  Images,
  ImgList,
  LanguageObjects,
  LazUTF8,
  Logging,
  MControlFocuser,
  MControls,
  MDropdownButton,
  Menus,
  MHotkeyEdit,
  MLabeledEdit,
  uDarkStyleParams,
  MsgDlg,
  MSpeedButton,
  PostProcess,
  SettingsBase,
  SharedData,
  ShlObj,
  Spin,
  StdCtrls,
  StrUtils,
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

    procedure UpdateList(List: TStringList);
    procedure RemoveSelected;
  end;

  { TListViewHelper }

  TListViewHelper = class helper for TListView
  public
    procedure AddGroup(const ID: Integer; const Name: UnicodeString);
    procedure EnableGroupView;
  end;

  { TListItemHelper }

  TListItemHelper = class helper for TListItem
  private
    function FGetGroupID: Integer;
    procedure FSetGroupID(const Value: Integer);
  public
    property GroupID: Integer read FGetGroupID write FSetGroupID;
  end;

  { TfrmSettings }

  TfrmSettings = class(TfrmSettingsBase, IPreTranslatable)
    Bevel1: TBevel;
    Bevel3: TBevel;
    btnAdd: TMDropdownButton;
    btnAddIgnoreTitlePattern: TButton;
    btnAddRegEx: TButton;
    btnBlacklistRemove: TButton;
    btnConfigure: TButton;
    btnConfigureEncoder: TMSpeedButton;
    btnHelpPostProcess: TMSpeedButton;
    btnMoveDown: TMSpeedButton;
    btnMoveUp: TMSpeedButton;
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
    Label10: TLabel;
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
    lstDefaultAction: TMLabeledComboBoxEx;
    lstDefaultActionNewStream: TMLabeledComboBoxEx;
    lstDefaultFilter: TMLabeledComboBoxEx;
    lstFormat: TMLabeledComboBoxEx;
    lstHotkeys: TListView;
    lstIgnoreTitles: TListView;
    lstMinQuality: TMLabeledComboBoxEx;
    lstOutputFormat: TMLabeledComboBoxEx;
    lstPostProcess: TListView;
    lstRegExes: TListView;
    lstSoundDevice: TMLabeledComboBoxEx;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    lstColorMode: TMLabeledComboBoxEx;
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
    pnlCutDisabled: TPanel;
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
    pnlTreeBackgroundColor: TPanel;
    pnlTreeNodeFontColor: TPanel;
    pnlTreeSelectionTextColor: TPanel;
    pnlTreeFocusedSelectionColor: TPanel;
    pnlPostProcess: TPanel;
    pnlStreams: TPanel;
    pnlStreamsAdvanced: TPanel;
    PopupMenuAddPostprocessor: TPopupMenu;
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
    procedure lstHotkeysResize(Sender: TObject);
    procedure lstOutputFormatSelect(Sender: TObject);
    procedure lstPostProcessSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure txtFilePatternChange(Sender: TObject);
    procedure chkSkipShortClick(Sender: TObject);
    procedure chkSearchSilenceClick(Sender: TObject);
    procedure chkTrayClick(Sender: TObject);
    procedure btnBrowseAppClick(Sender: TObject);
    procedure mnuAddPostProcessorClick(Sender: TObject);
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
    FDefaultActionNewStreamIdx: Integer;
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
    procedure RemoveGray(C: TControl; ShowMessage: Boolean = True);
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
    procedure PreTranslate;
    procedure PostTranslate; override;
    procedure GetExportDataHeader(Stream: TMemoryStream); override;
    procedure GetExportData(Stream: TMemoryStream); override;
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

{ TListViewHelper }

procedure TListViewHelper.AddGroup(const ID: Integer; const Name: UnicodeString);
var
  LVG: LVGROUP;
begin
  FillChar(LVG, SizeOf(LVG), $00);
  LVG.cbSize := SizeOf(LVG);
  LVG.mask := LVGF_HEADER or LVGF_GROUPID;
  LVG.cchHeader := Length(Name);
  LVG.pszHeader := PWideChar(Name);
  LVG.iGroupId := ID;

  ListView_InsertGroup(Handle, -1, LPARAM(@LVG));
end;

procedure TListViewHelper.EnableGroupView;
begin
  ListView_EnableGroupView(Handle, WPARAM(True));
end;

{ TListItemHelper }

function TListItemHelper.FGetGroupID: Integer;
var
  LVI: TLVITEMW;
begin
  FillChar(LVI, SizeOf(LVI), $00);
  LVI.mask := LVIF_GROUPID;
  LVI.iItem := Index;
  SendMessage(ListView.Handle, LVM_GETITEM, 0, LPARAM(@LVI));
  Result := LVI.iGroupId;
end;

procedure TListItemHelper.FSetGroupID(const Value: Integer);
var
  LVI: TLVITEMW;
begin
  FillChar(LVI, SizeOf(LVI), $00);
  LVI.mask := LVIF_GROUPID;
  LVI.iItem := Index;
  LVI.iGroupId := Value;
  SendMessage(ListView.Handle, LVM_SETITEM, 0, LPARAM(@LVI));
end;

{ TfrmSettings }

destructor TfrmSettings.Destroy;
var
  ListItem: TListItem;
  Settings: TStreamSettings;
  PostProcess: TPostProcessBase;
begin
  FIgnoreFieldList.Free;

  for ListItem in lstAddons.Items do
    TAddonBase(ListItem.Data).Free;

  for PostProcess in FTemporaryPostProcessors do
    PostProcess.Free;
  FTemporaryPostProcessors.Free;

  for Settings in FStreamSettings do
    Settings.Free;

  inherited;
end;

procedure TfrmSettings.DoCreate;
begin
  inherited;

  Width := Scale96ToFont(640);
  Height := Scale96ToFont(450);

  // Strangely this SpinEdit is not scaled on high DPI
  txtSilenceBufferSeconds.Width := Scale96ToFont(txtSilenceBufferSeconds.Width);
end;

procedure TfrmSettings.EnablePanel(Panel: TPanel; Enable: Boolean);
begin
  if (Panel = pnlCut) and (FSettingsType <> stStream) then
  begin
    chkAdjustTrackOffset.Visible := False;
    pnlAdjustTrackOffset.Visible := False;
    optAdjustBackward.Visible := False;
    optAdjustForward.Visible := False;
  end;

  if Enable then
    Panel.Tag := 0
  else
    Panel.Tag := 1;
end;

function ComparePostProcessors(constref L, R: TPostProcessBase): Integer;
begin
  if L.GroupID <> R.GroupID then
    Result := TFunctions.CmpInt(L.GroupID, R.GroupID)
  else
    Result := TFunctions.CmpInt(L.Order, R.Order);
end;

procedure TfrmSettings.FillFields(Settings: TStreamSettings);
var
  i: Integer;
begin
  lstDefaultAction.Control.ItemIndex := Integer(AppGlobals.DefaultAction);

  case AppGlobals.DefaultActionNewStream of
    oaPlay: lstDefaultActionNewStream.Control.ItemIndex := 1;
    oaAdd: lstDefaultActionNewStream.Control.ItemIndex := 2;
    else
      lstDefaultActionNewStream.Control.ItemIndex := 0;
  end;

  lstDefaultFilter.Control.ItemIndex := Integer(Settings.Filter);
  chkSeparateTracks.Checked := Settings.SeparateTracks;
  chkSaveStreamsToDisk.Checked := not Settings.SaveToMemory;
  chkOnlySaveFull.Checked := Settings.OnlySaveFull;

  Language.Translate(Self);

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
    chkAdjustTrackOffsetClick(nil);

    chkAutostart.Checked := FileExists(ConcatPaths([TFunctions.GetShellFolder(CSIDL_STARTUP), AppGlobals.AppName + '.lnk']));
    chkTray.Checked := AppGlobals.Tray;
    chkRememberRecordings.Checked := AppGlobals.RememberRecordings;
    chkRememberPlaying.Checked := AppGlobals.RememberPlaying;
    chkDisplayPlayedSong.Checked := AppGlobals.DisplayPlayedSong;
    chkDisplayPlayNotifications.Checked := AppGlobals.DisplayPlayNotifications;
    chkShowSplashScreen.Checked := AppGlobals.ShowSplashScreen;
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

    chkSubmitStatsClick(nil);
    chkMonitorModeClick(nil);
    chkLimitClick(nil);

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

  FTemporaryPostProcessors.Sort(TComparer<TPostProcessBase>.Construct(@ComparePostProcessors));

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
  i: Integer;
  PostProcessor, TempPostProcessor: TPostProcessBase;
  EP: TExternalPostProcess;
  S: TStreamSettings;
  Item: TListItem = nil;
  Colors: TVTColors;
begin
  if Length(FStreamSettings) = 0 then
    raise Exception.Create('Length(FStreamSettings) = 0');

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
        TFunctions.CreateLink(Application.ExeName, TFunctions.GetShellFolder(CSIDL_STARTUP), AppGlobals.AppName, '-minimize', False)
      else
        TFunctions.CreateLink(Application.ExeName, TFunctions.GetShellFolder(CSIDL_STARTUP), AppGlobals.AppName, '', True);

      AppGlobals.Dir := txtDir.Control.Text;

      AppGlobals.Tray := chkTray.Checked;
      AppGlobals.RememberRecordings := chkRememberRecordings.Checked;
      AppGlobals.RememberPlaying := chkRememberPlaying.Checked;
      AppGlobals.DisplayPlayedSong := chkDisplayPlayedSong.Checked;
      AppGlobals.DisplayPlayNotifications := chkDisplayPlayNotifications.Checked;
      AppGlobals.ShowSplashScreen := chkShowSplashScreen.Checked;
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

      case lstDefaultActionNewStream.Control.ItemIndex of
        1: AppGlobals.DefaultActionNewStream := oaPlay;
        2: AppGlobals.DefaultActionNewStream := oaAdd;
        else
          AppGlobals.DefaultActionNewStream := oaStart;
      end;

      AppGlobals.ShortcutPlay := LongWord(lstHotkeys.Items[0].Data);
      AppGlobals.ShortcutPause := LongWord(lstHotkeys.Items[1].Data);
      AppGlobals.ShortcutStop := LongWord(lstHotkeys.Items[2].Data);
      AppGlobals.ShortcutNext := LongWord(lstHotkeys.Items[3].Data);
      AppGlobals.ShortcutPrev := LongWord(lstHotkeys.Items[4].Data);
      AppGlobals.ShortcutVolUp := LongWord(lstHotkeys.Items[5].Data);
      AppGlobals.ShortcutVolDown := LongWord(lstHotkeys.Items[6].Data);
      AppGlobals.ShortcutMute := LongWord(lstHotkeys.Items[7].Data);

      AppGlobals.ColorMode := TColorMode(lstColorMode.Control.ItemIndex);

      Colors := TVTColors.Create(nil);
      try
        if (pnlTreeNodeFontColor.Color <> clWindowText) or (pnlTreeSelectionTextColor.Color <> Colors.SelectionTextColor) or
          (pnlTreeFocusedSelectionColor.Color <> Colors.FocusedSelectionColor) or (pnlTreeBackgroundColor.Color <> clWindow) then
        begin
          AppGlobals.TreeColorsLoaded := True;
          AppGlobals.TreeNodeFontColor := pnlTreeNodeFontColor.Color;
          AppGlobals.TreeSelectionTextColor := pnlTreeSelectionTextColor.Color;
          AppGlobals.TreeFocusedSelectionColor := pnlTreeFocusedSelectionColor.Color;
          AppGlobals.TreeBackgroundColor := pnlTreeBackgroundColor.Color;
        end else
        begin
          AppGlobals.TreeColorsLoaded := False;
          AppGlobals.TreeNodeFontColor := $7F000000;
          AppGlobals.TreeSelectionTextColor := $7F000000;
          AppGlobals.TreeFocusedSelectionColor := $7F000000;
          AppGlobals.TreeBackgroundColor := $7F000000;
        end;
      finally
        Colors.Free;
      end;
    finally
      AppGlobals.Unlock;
    end;
  end;

  for S in FStreamSettings do
  begin
    if FSettingsType = stAuto then
      S.FilePattern := Trim(txtAutomaticFilePattern.Control.Text)
    else if FIgnoreFieldList.IndexOf(txtFilePattern) = -1 then
      S.FilePattern := Trim(txtFilePattern.Control.Text);

    if FIgnoreFieldList.IndexOf(txtIncompleteFilePattern) = -1 then
      S.IncompleteFilePattern := Trim(txtIncompleteFilePattern.Control.Text);

    if FIgnoreFieldList.IndexOf(txtStreamFilePattern) = -1 then
      S.StreamFilePattern := Trim(txtStreamFilePattern.Control.Text);

    if FIgnoreFieldList.IndexOf(txtFilePatternDecimals) = -1 then
      S.FilePatternDecimals := txtFilePatternDecimals.Control.Value;

    if FIgnoreFieldList.IndexOf(txtRemoveChars) = -1 then
      S.RemoveChars := txtRemoveChars.Control.Text;

    if FIgnoreFieldList.IndexOf(chkNormalizeVariables) = -1 then
      S.NormalizeVariables := chkNormalizeVariables.Checked;

    if FIgnoreFieldList.IndexOf(chkDeleteStreams) = -1 then
      S.DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;

    if FSettingsType = stAuto then
      S.AddSavedToIgnore := chkAutoTuneInAddToIgnore.Checked
    else if FIgnoreFieldList.IndexOf(chkAddSavedToIgnore) = -1 then
      S.AddSavedToIgnore := chkAddSavedToIgnore.Checked;

    if FIgnoreFieldList.IndexOf(chkAddSavedToStreamIgnore) = -1 then
      S.AddSavedToStreamIgnore := chkAddSavedToStreamIgnore.Checked;

    if FSettingsType = stAuto then
      S.RemoveSavedFromWishlist := chkAutoRemoveSavedFromWishlist.Checked
    else if FIgnoreFieldList.IndexOf(chkRemoveSavedFromWishlist) = -1 then
      S.RemoveSavedFromWishlist := chkRemoveSavedFromWishlist.Checked;

    if FIgnoreFieldList.IndexOf(chkOverwriteSmaller) = -1 then
      S.OverwriteSmaller := chkOverwriteSmaller.Checked;

    if FIgnoreFieldList.IndexOf(chkDiscardSmaller) = -1 then
      S.DiscardSmaller := chkDiscardSmaller.Checked;

    if FIgnoreFieldList.IndexOf(chkDiscardAlways) = -1 then
      S.DiscardAlways := chkDiscardAlways.Checked;

    if pnlCut.Tag = 0 then
    begin
      if FIgnoreFieldList.IndexOf(chkSkipShort) = -1 then
        S.SkipShort := chkSkipShort.Checked;

      if FIgnoreFieldList.IndexOf(txtSongBuffer) = -1 then
        S.SongBuffer := txtSongBuffer.Control.Value;

      if FIgnoreFieldList.IndexOf(txtShortLengthSeconds) = -1 then
        S.ShortLengthSeconds := txtShortLengthSeconds.Control.Value;

      if FIgnoreFieldList.IndexOf(chkSearchSilence) = -1 then
        S.SearchSilence := chkSearchSilence.Checked;

      if FIgnoreFieldList.IndexOf(chkManualSilenceLevel) = -1 then
        S.AutoDetectSilenceLevel := not chkManualSilenceLevel.Checked;

      if FIgnoreFieldList.IndexOf(txtSilenceLevel) = -1 then
        S.SilenceLevel := txtSilenceLevel.Control.Value;

      if FIgnoreFieldList.IndexOf(txtSilenceLength) = -1 then
        S.SilenceLength := txtSilenceLength.Value;

      if FIgnoreFieldList.IndexOf(txtSilenceBufferSeconds) = -1 then
      begin
        S.SilenceBufferSecondsStart := txtSilenceBufferSeconds.Value;
        S.SilenceBufferSecondsEnd := txtSilenceBufferSeconds.Value;
      end;

      if Length(FStreamSettings) > 0 then
      begin
        if FIgnoreFieldList.IndexOf(chkAdjustTrackOffset) = -1 then
          S.AdjustTrackOffset := chkAdjustTrackOffset.Checked;

        if FIgnoreFieldList.IndexOf(txtAdjustTrackOffset) = -1 then
          S.AdjustTrackOffsetMS := txtAdjustTrackOffset.Value;

        if FIgnoreFieldList.IndexOf(optAdjustBackward) = -1 then
          if optAdjustBackward.Checked then
            S.AdjustTrackOffsetDirection := toBackward
          else
            S.AdjustTrackOffsetDirection := toForward;
      end;
    end;

    if FIgnoreFieldList.IndexOf(txtMaxRetries) = -1 then
      S.MaxRetries := txtMaxRetries.Control.Value;

    if FIgnoreFieldList.IndexOf(txtRetryDelay) = -1 then
      S.RetryDelay := txtRetryDelay.Control.Value;

    if FIgnoreFieldList.IndexOf(lstDefaultFilter) = -1 then
      S.Filter := TUseFilters(lstDefaultFilter.Control.ItemIndex);

    if FIgnoreFieldList.IndexOf(chkSeparateTracks) = -1 then
      S.SeparateTracks := chkSeparateTracks.Checked and chkSeparateTracks.Enabled;

    if FIgnoreFieldList.IndexOf(chkSaveStreamsToDisk) = -1 then
      S.SaveToMemory := not chkSaveStreamsToDisk.Checked;

    if FIgnoreFieldList.IndexOf(chkOnlySaveFull) = -1 then
      S.OnlySaveFull := chkOnlySaveFull.Checked;

    if (FIgnoreFieldList.IndexOf(lstRegExes) = -1) and (Length(FStreamSettings) > 0) then
    begin
      S.RegExes.Clear;
      for i := 0 to lstRegExes.Items.Count - 1 do
        S.RegExes.Add(lstRegExes.Items[i].Caption);
    end;

    if (FIgnoreFieldList.IndexOf(lstIgnoreTitles) = -1) and (Length(FStreamSettings) > 0) then
    begin
      S.IgnoreTrackChangePattern.Clear;
      for i := 0 to lstIgnoreTitles.Items.Count - 1 do
        S.IgnoreTrackChangePattern.Add(lstIgnoreTitles.Items[i].Caption);
    end;

    if FIgnoreFieldList.IndexOf(lstOutputFormat) = -1 then
      S.OutputFormat := TAudioTypes(lstOutputFormat.Control.ItemIndex);

    if FIgnoreFieldList.IndexOf(lstPostProcess) = -1 then
    begin
      // -----------------------------------------------------------
      for TempPostProcessor in FTemporaryPostProcessors do
      begin
        PostProcessor := S.PostProcessors.Find(TempPostProcessor);

        if (PostProcessor = nil) or (TempPostProcessor.IsNew) then
        begin
          // Ein neuer PostProcessor kann nur TExternalPostProcessor sein.
          PostProcessor := TempPostProcessor.Copy;
          S.PostProcessors.Add(PostProcessor);
        end;

        for Item in lstPostProcess.Items do
          if Item.Data = TempPostProcessor then
            Break;

        PostProcessor.OnlyIfCut := TempPostProcessor.OnlyIfCut;
        PostProcessor.Order := Item.Index;
        PostProcessor.Active := Item.Checked;

        PostProcessor.Assign(TempPostProcessor);
      end;

      // Vom Benutzer entfernte PostProcessors aus den echten PostProcessors entfernen..
      for i := S.PostProcessors.Count - 1 downto 0 do
      begin
        if S.PostProcessors[i] is TExternalPostProcess then
        begin
          EP := nil;
          for TempPostProcessor in FTemporaryPostProcessors do
            if TempPostProcessor is TExternalPostProcess then
              if TExternalPostProcess(TempPostProcessor).Identifier = TExternalPostProcess(S.PostProcessors[i]).Identifier then
              begin
                EP := TExternalPostProcess(S.PostProcessors[i]);
                Break;
              end;
          if EP = nil then
          begin
            S.PostProcessors[i].Free;
            S.PostProcessors.Delete(i);
            Continue;
          end;
        end;
        S.PostProcessors[i].IsNew := False;
      end;
      // -----------------------------------------------------------
    end;
  end;

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

procedure TfrmSettings.lstHotkeysResize(Sender: TObject);
begin
  lstHotkeys.Column[0].Width := Trunc(lstHotkeys.ClientWidth / 2);
  lstHotkeys.Column[1].Width := Trunc(lstHotkeys.ClientWidth / 2);
end;

procedure TfrmSettings.GetExportData(Stream: TMemoryStream);
begin
  inherited;

  AppGlobals.Lock;
  try
    AppGlobals.Data.Save(Stream, True);
  finally
    AppGlobals.Unlock;
  end;
end;

procedure TfrmSettings.GetExportDataHeader(Stream: TMemoryStream);
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
    Result := Result + TFunctions.HashString(Lst[i]);
end;

procedure TfrmSettings.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = VK_F1 then
    case FSettingsType of
      stApp:
        TFunctions.ShellExecute(Handle, 'open', AppGlobals.ProjectHelpLinkSettings);
      stAuto:
        TFunctions.ShellExecute(Handle, 'open', AppGlobals.ProjectHelpLinkAutoSettings);
      stStream:
        TFunctions.ShellExecute(Handle, 'open', AppGlobals.ProjectHelpLinkStreamSettings);
    end;
end;

procedure TfrmSettings.Label20Click(Sender: TObject);
begin
  chkMonitorMode.Checked := not chkMonitorMode.Checked;
end;

procedure TfrmSettings.Label2Click(Sender: TObject);
begin
  chkSubmitStreamInfo.Checked := not chkSubmitStreamInfo.Checked;
end;

procedure TfrmSettings.Label8Click(Sender: TObject);
begin
  chkSubmitStats.Checked := not chkSubmitStats.Checked;
end;

procedure TfrmSettings.lstDefaultFilterChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(lstDefaultFilter.Control);
end;

procedure TfrmSettings.lstHotkeysChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  txtHotkey.Enabled := lstHotkeys.Selected <> nil;
  if txtHotkey.Enabled then
  begin
    txtHotkey.Control.HotKey := TShortCut(lstHotkeys.Selected.Data);
  end else
    txtHotkey.Control.HotKey := 0;
end;

procedure TfrmSettings.lstIgnoreTitlesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  btnRemoveIgnoreTitlePattern.Enabled := lstIgnoreTitles.Selected <> nil;
end;

procedure TfrmSettings.lstIgnoreTitlesEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  if Trim(S) = '' then
    S := Item.Caption
  else
    RemoveGray(lstIgnoreTitles);
end;

procedure TfrmSettings.lstIgnoreTitlesResize(Sender: TObject);
begin
  lstIgnoreTitles.Columns[0].Width := lstIgnoreTitles.Width - GetSystemMetrics(SM_CXVSCROLL) - GetSystemMetrics(SM_CXEDGE) * 2;
end;

procedure TfrmSettings.lstOutputFormatSelect(Sender: TObject);
var
  i: Integer;
begin
  if not FInitialized then
    Exit;

  RemoveGray(lstOutputFormat.Control);

  if lstOutputFormat.Control.ItemIndex = 0 then
  begin
    btnConfigureEncoder.Enabled := False;
    OutputFormatLastIndex := lstOutputFormat.Control.ItemIndex;
    Exit;
  end;

  if AppGlobals.AddonManager.CanEncode(TAudioTypes(lstOutputFormat.Control.ItemIndex)) = ceAddonNeeded then
    if TFunctions.MsgBox(_('Additional addons are needed to use the selected output format. Do you want to download these addons now?'), _('Question'), MB_YESNO or MB_DEFBUTTON1 or MB_ICONQUESTION) = IDYES then
      AppGlobals.AddonManager.EnableAddon(Self, AppGlobals.AddonManager.Find(TAudioTypes(lstOutputFormat.Control.ItemIndex)), True);

  if AppGlobals.AddonManager.CanEncode(TAudioTypes(lstOutputFormat.Control.ItemIndex)) <> ceOkay then
    lstOutputFormat.Control.ItemIndex := OutputFormatLastIndex
  else
    OutputFormatLastIndex := lstOutputFormat.Control.ItemIndex;

  lstAddons.OnItemChecked := nil;
  for i := 0 to lstAddons.Items.Count - 1 do
    lstAddons.Items[i].Checked := TAddonBase(lstAddons.Items[i].Data).PackageDownloaded and TAddonBase(lstAddons.Items[i].Data).FilesExtracted;
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
    lstAddons.Items[i].Checked := TAddonBase(lstAddons.Items[i].Data).PackageDownloaded and TAddonBase(lstAddons.Items[i].Data).FilesExtracted;
  lstAddons.OnItemChecked := lstAddonsItemChecked;
end;

procedure TfrmSettings.lstAddonsResize(Sender: TObject);
begin
  lstAddons.Columns[0].Width := lstAddons.Width - GetSystemMetrics(SM_CXVSCROLL) - GetSystemMetrics(SM_CXEDGE) * 2;
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

  chkOnlyIfCut.Enabled := (Item <> nil) and Item.Checked and Item.Selected;

  if TObject(Item.Data) is TInternalPostProcess then
  begin
    lstPostProcess.OnItemChecked := nil;
    Item.Checked := AppGlobals.PostProcessManager.EnablePostProcess(Self, Item.Checked, TInternalPostProcess(Item.Data));
    lstPostProcess.OnItemChecked := lstPostProcessItemChecked;

    btnConfigure.Enabled := Item.Checked and Item.Selected and TPostProcessBase(Item.Data).CanConfigure;
  end;

  FTemporaryPostProcessors.Find(TPostProcessBase(Item.Data)).Active := Item.Checked;

  if TPostProcessBase(Item.Data).NeedsWave and Item.Checked then
    ShowEncoderNeededMessage;

  lstAddons.OnItemChecked := nil;
  for i := 0 to lstAddons.Items.Count - 1 do
    lstAddons.Items[i].Checked := TAddonBase(lstAddons.Items[i].Data).PackageDownloaded and TAddonBase(lstAddons.Items[i].Data).FilesExtracted;
  lstAddons.OnItemChecked := lstAddonsItemChecked;

  lstPostProcess.Selected := Item;
end;

procedure TfrmSettings.lstPostProcessResize(Sender: TObject);
begin
  lstPostProcess.Columns[0].Width := lstPostProcess.Width - GetSystemMetrics(SM_CXVSCROLL) - GetSystemMetrics(SM_CXEDGE) * 2;
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
  chkOnlyIfCut.Enabled := (Item <> nil) and Selected and Item.Checked;

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

  btnConfigure.Enabled := (Item <> nil) and Selected and Item.Checked and TPostProcessBase(Item.Data).CanConfigure;
end;

procedure TfrmSettings.lstRegExesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  btnRemoveRegEx.Enabled := lstRegExes.Selected <> nil;
end;

procedure TfrmSettings.lstRegExesEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  if not CheckRegExp(S, lstRegExes, Item) then
  begin
    S := Item.Caption;
    Exit;
  end;

  Item.Caption := S;
end;

procedure TfrmSettings.lstRegExesResize(Sender: TObject);
begin
  lstRegExes.Columns[0].Width := lstRegExes.Width - GetSystemMetrics(SM_CXVSCROLL) - GetSystemMetrics(SM_CXEDGE) * 2;
end;

procedure TfrmSettings.optAdjustClick(Sender: TObject);
begin
  if FInitialized then
  begin
    RemoveGray(optAdjustBackward);
    RemoveGray(optAdjustForward, False);
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
  FDefaultActionIdx := lstDefaultAction.Control.ItemIndex;
  FDefaultActionNewStreamIdx := lstDefaultActionNewStream.Control.ItemIndex;
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

  BuildHotkeys;

  lstDefaultAction.Control.ItemIndex := FDefaultActionIdx;
  lstDefaultActionNewStream.Control.ItemIndex := FDefaultActionNewStreamIdx;
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
  PList := TStringList.Create;
  try
    TFunctions.Explode('|', Patterns, PList);

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

  Result := TFunctions.PatternReplaceNew(Text, Arr);

  Result := FixPatternFilename(Result);

  Result := TFunctions.FixPathName(Result + '.mp3');
end;

procedure TfrmSettings.RebuildPostProcessingList;
var
  i: Integer;
  Item: TListItem;
begin
  lstPostProcess.AddGroup(0, _('Processing when in WAVE-format'));
  lstPostProcess.AddGroup(1, _('Processing after conversion to destination format'));
  lstPostProcess.EnableGroupView;

  lstPostProcess.Items.BeginUpdate;
  try
    lstPostProcess.Items.Clear;
    for i := 0 to FTemporaryPostProcessors.Count - 1 do
    begin
      Item := lstPostProcess.Items.Add;
      Item.Caption := FTemporaryPostProcessors[i].Name;
      Item.Checked := FTemporaryPostProcessors[i].Active;
      // Data must be set at last that events (i.e. lstPostProcessItemChecked) do not fire
      Item.Data := FTemporaryPostProcessors[i];

      if FTemporaryPostProcessors[i] is TInternalPostProcess then
        Item.ImageIndex := TImages.LIGHTNING
      else
        Item.ImageIndex := TImages.APPLICATION_XP_TERMINAL;

      Item.GroupID := FTemporaryPostProcessors[i].GroupID;
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

      inherited RegisterGeneralPage(TImages.WRENCH_APPLICATION);
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
end;

procedure TfrmSettings.RemoveGray(C: TControl; ShowMessage: Boolean = True);
begin
  if FIgnoreFieldList = nil then
    Exit;

  if TControl(C) is TCustomEdit then
    TEdit(C).Color := clWindow
  else if TControl(C) is TEditButton then
    TEditButton(C).Color := clWindow
  else if TControl(C) is TCheckBox then
  else if TControl(C) is TComboBoxEx then
    TComboBoxEx(C).Color := clWindow
  else if TControl(C) is TListView then
    TListView(C).Color := clWindow;

  if ShowMessage and (FIgnoreFieldList.IndexOf(C) > -1) and (not FOptionChanging) then
    TfrmMsgDlg.ShowMsg(Self, _('The setting''s configuration you are about to change differs for the selected streams. The new setting will be applied to every selected stream when saving settings using "OK".'),
      mtInformation, [mbOK], mbOK, 13);

  FIgnoreFieldList.Remove(C);
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
  for i := 1 to High(FStreamSettings) do
    if GetStringListHash(S.IgnoreTrackChangePattern) <> GetStringListHash(FStreamSettings[i].IgnoreTrackChangePattern) then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstIgnoreTitles);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.FilePattern <> FStreamSettings[i].FilePattern then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtFilePattern.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.IncompleteFilePattern <> FStreamSettings[i].IncompleteFilePattern then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtIncompleteFilePattern.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.StreamFilePattern <> FStreamSettings[i].StreamFilePattern then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtStreamFilePattern.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.FilePatternDecimals <> FStreamSettings[i].FilePatternDecimals then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtFilePatternDecimals.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.RemoveChars <> FStreamSettings[i].RemoveChars then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtRemoveChars.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.NormalizeVariables <> FStreamSettings[i].NormalizeVariables then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkNormalizeVariables);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.DeleteStreams <> FStreamSettings[i].DeleteStreams then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkDeleteStreams);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.AddSavedToIgnore <> FStreamSettings[i].AddSavedToIgnore then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkAddSavedToIgnore);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.AddSavedToStreamIgnore <> FStreamSettings[i].AddSavedToStreamIgnore then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkAddSavedToStreamIgnore);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.RemoveSavedFromWishlist <> FStreamSettings[i].RemoveSavedFromWishlist then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkRemoveSavedFromWishlist);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.OverwriteSmaller <> FStreamSettings[i].OverwriteSmaller then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkOverwriteSmaller);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.DiscardSmaller <> FStreamSettings[i].DiscardSmaller then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkDiscardSmaller);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.DiscardAlways <> FStreamSettings[i].DiscardAlways then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkDiscardAlways);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SkipShort <> FStreamSettings[i].SkipShort then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSkipShort);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if GetStringListHash(S.RegExes) <> GetStringListHash(FStreamSettings[i].RegExes) then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstRegExes);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SearchSilence <> FStreamSettings[i].SearchSilence then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSearchSilence);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.AutoDetectSilenceLevel <> FStreamSettings[i].AutoDetectSilenceLevel then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkManualSilenceLevel);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SilenceLevel <> FStreamSettings[i].SilenceLevel then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSilenceLevel.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SilenceLength <> FStreamSettings[i].SilenceLength then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSilenceLength);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SilenceBufferSecondsStart <> FStreamSettings[i].SilenceBufferSecondsStart then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSilenceBufferSeconds);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.ShortLengthSeconds <> FStreamSettings[i].ShortLengthSeconds then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtShortLengthSeconds.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SongBuffer <> FStreamSettings[i].SongBuffer then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtSongBuffer.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.MaxRetries <> FStreamSettings[i].MaxRetries then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtMaxRetries.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.RetryDelay <> FStreamSettings[i].RetryDelay then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtRetryDelay.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.Filter <> FStreamSettings[i].Filter then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstDefaultFilter.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SeparateTracks <> FStreamSettings[i].SeparateTracks then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSeparateTracks);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.SaveToMemory <> FStreamSettings[i].SaveToMemory then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkSaveStreamsToDisk);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.OnlySaveFull <> FStreamSettings[i].OnlySaveFull then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkOnlySaveFull);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.AdjustTrackOffset <> FStreamSettings[i].AdjustTrackOffset then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(chkAdjustTrackOffset);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.AdjustTrackOffsetMS <> FStreamSettings[i].AdjustTrackOffsetMS then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(txtAdjustTrackOffset);

  F := False;
  for i := 1 to High(FStreamSettings) do
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
  for i := 1 to High(FStreamSettings) do
    if S.OutputFormat <> FStreamSettings[i].OutputFormat then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstOutputFormat.Control);

  F := False;
  for i := 1 to High(FStreamSettings) do
    if S.PostProcessors.Hash <> FStreamSettings[i].PostProcessors.Hash then
    begin
      F := True;
      ShowDialog := True;
      Break;
    end;
  if F then
    AddField(lstPostProcess);

  F := False;
  for i := 1 to High(FStreamSettings) do
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
    if TControl(FIgnoreFieldList[i]) is TCustomEdit then
      TEdit(FIgnoreFieldList[i]).Color := clBtnFace
    else if TControl(FIgnoreFieldList[i]) is TEditButton then
      TEditButton(FIgnoreFieldList[i]).Color := clBtnFace
    else if TControl(FIgnoreFieldList[i]) is TCheckBox then
      TCheckBox(FIgnoreFieldList[i]).State := cbGrayed
    else if TControl(FIgnoreFieldList[i]) is TComboBoxEx then
      TComboBoxEx(FIgnoreFieldList[i]).Color := clBtnFace
    else if TControl(FIgnoreFieldList[i]) is TListView then
      TListView(FIgnoreFieldList[i]).Color := clBtnFace;
end;

procedure TfrmSettings.SetPage(Page: TPage);
begin
  inherited;

  if Page.Panel = pnlCut then
  begin
    pnlCut.Visible := pnlCut.Tag = 0;
    pnlCutDisabled.Visible := pnlCut.Tag = 1;
    pnlCutDisabled.BringToFront;
  end;

  if Page = FPageList.Find(pnlFilenames) then
    txtPreview.Control.Text := '';

  // Without this, listview groups won't work
  RebuildPostProcessingList;
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
  if FInitialized then
    RemoveGray(txtAdjustTrackOffset);
end;

procedure TfrmSettings.txtAppParamsChange(Sender: TObject);
begin
  if (lstPostProcess.Selected <> nil) and txtAppParams.Focused then
    TExternalPostProcess(lstPostProcess.Selected.Data).Params := txtAppParams.Control.Text;
end;

procedure TfrmSettings.txtFilePatternChange(Sender: TObject);
begin
  if FInitialized then
  begin
    FActivePreviewField := TEditButton(Sender);

    RemoveGray(FActivePreviewField);

    if Sender = txtAutomaticFilePattern.Control then
      txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|streamname|day|month|year|hour|minute|second')
    else if Sender = txtStreamFilePattern.Control then
      txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'streamname|day|month|year|hour|minute|second')
    else
      txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second');

    if Trim(TFunctions.RemoveFileExt(txtPreview.Control.Text)) = '' then
      txtPreview.Control.Text := '';
  end;
end;

procedure TfrmSettings.txtFilePatternDecimalsChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtFilePatternDecimals.Control);
end;

procedure TfrmSettings.txtHotkeyChange(Sender: TObject);
var
  ListItem: TListItem;
begin
  if not Assigned(lstHotkeys.Selected) then
    Exit;

  for ListItem in lstHotkeys.Items do
    if ListItem.Data = Pointer(txtHotkey.Control.HotKey) then
    begin
      ListItem.SubItems[0] := '';
      ListItem.Data := nil;
    end;

  lstHotkeys.Selected.SubItems[0] := ShortcutToText(txtHotkey.Control.HotKey);
  lstHotkeys.Selected.Data := Pointer(txtHotkey.Control.HotKey);
end;

procedure TfrmSettings.txtIgnoreTitlePatternChange(Sender: TObject);
begin
  btnAddIgnoreTitlePattern.Enabled := UTF8LengthFast(UTF8Trim(txtIgnoreTitlePattern.Control.Text)) >= 1;
end;

procedure TfrmSettings.txtFilePatternEnter(Sender: TObject);
begin
  FActivePreviewField := TEditButton(Sender);

  if Sender = txtAutomaticFilePattern.Control then
    txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|streamname|day|month|year|hour|minute|second')
  else if Sender = txtStreamFilePattern.Control then
    txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'streamname|day|month|year|hour|minute|second')
  else
    txtPreview.Control.Text := ValidatePattern(FActivePreviewField.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second');

  if Trim(TFunctions.RemoveFileExt(txtPreview.Control.Text)) = '' then
    txtPreview.Control.Text := '';
end;

procedure TfrmSettings.txtMaxRetriesChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtMaxRetries.Control);
end;

procedure TfrmSettings.txtRemoveCharsChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtRemoveChars.Control);
end;

procedure TfrmSettings.txtRetryDelayChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtRetryDelay.Control);
end;

procedure TfrmSettings.txtShortLengthSecondsChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtShortLengthSeconds.Control);
end;

procedure TfrmSettings.txtSilenceBufferSecondsChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtSilenceBufferSeconds);
end;

procedure TfrmSettings.txtSilenceLengthChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtSilenceLength);
end;

procedure TfrmSettings.txtSilenceLevelChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtSilenceLevel.Control);
end;

procedure TfrmSettings.txtSongBufferChange(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(txtSongBuffer.Control);
end;

procedure TfrmSettings.txtRegExChange(Sender: TObject);
begin
  btnAddRegEx.Enabled := UTF8LengthFast(UTF8Trim(txtRegEx.Control.Text)) >= 1;
end;

procedure TfrmSettings.UpdatePostProcessUpDown;
var
  i: Integer;
  PrevInGroup: TListItem = nil;
  NextInGroup: TListItem = nil;
begin
  if not Assigned(lstPostProcess.Selected) then
    Exit;

  for i := lstPostProcess.Selected.Index - 1 downto 0 do
    if lstPostProcess.Items[i].GroupID = lstPostProcess.Selected.GroupID then
    begin
      PrevInGroup := lstPostProcess.Items[i];
      Break;
    end;

  for i := lstPostProcess.Selected.Index + 1 to lstPostProcess.Items.Count - 1 do
    if lstPostProcess.Items[i].GroupID = lstPostProcess.Selected.GroupID then
    begin
      NextInGroup := lstPostProcess.Items[i];
      Break;
    end;

  btnMoveUp.Enabled := Assigned(PrevInGroup);
  btnMoveDown.Enabled := Assigned(NextInGroup);
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
  if not CheckRegExp(RegExp, lstRegExes, nil) then
    Exit;

  Item := lstRegExes.Items.Add;
  Item.Caption := RegExp;
  Item.ImageIndex := TImages.FONT;
  txtRegEx.Control.Text := '';
  txtRegEx.Control.ApplyFocus;

  RemoveGray(lstRegExes);
end;

procedure TfrmSettings.mnuAddPostProcessorClick(Sender: TObject);

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
  i, Grp: Integer;
  Item: TListItem;
  PostProcessor: TExternalPostProcess;
begin
  if not FInitialized then
    Exit;

  RemoveGray(lstPostProcess);

  if Sender = MenuItem1 then
    Grp := 0
  else
    Grp := 1;

  if dlgOpen.Execute and FileExists(dlgOpen.FileName) then
  begin
    Item := lstPostProcess.Items.Insert(HighestGroupIndex(Grp) + 1);
    Item.Caption := ExtractFileName(dlgOpen.FileName);
    PostProcessor := TExternalPostProcess.Create(dlgOpen.FileName, '"%filename%"', True, False, GetNewID, 100000, Grp);
    PostProcessor.IsNew := True;
    FTemporaryPostProcessors.Insert(HighestGroupIndex(Grp) + 1, PostProcessor);

    if PostProcessor.NeedsWave then
      ShowEncoderNeededMessage;

    RebuildPostProcessingList;

    for i := 0 to lstPostProcess.Items.Count - 1 do
      if TPostProcessBase(lstPostProcess.Items[i].Data) = PostProcessor then
      begin
        lstPostProcess.Items[i].Selected := True;
        Break;
      end;
  end;
end;

procedure TfrmSettings.btnBlacklistRemoveClick(Sender: TObject);
begin
  lstBlacklist.RemoveSelected;
end;

procedure TfrmSettings.btnBrowseAppClick(Sender: TObject);
begin
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
  Dir, Msg: string;
begin
  if FSettingsType = stAuto then
    Msg := 'Select folder for automatically saved songs'
  else
    Msg := 'Select folder for saved songs';

  if not TFunctions.BrowseDialog(Self, _(Msg), Dir) then
    Exit;

  if DirectoryExists(Dir) then
    txtDir.Control.Text := Dir
  else
    TFunctions.MsgBox(_('The selected folder does not exist. Please choose another one.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.btnBrowseLogFileClick(Sender: TObject);
begin
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
  TFunctions.MsgBox(PostProcess.Help, 'Info', MB_ICONINFORMATION);
end;

procedure TfrmSettings.btnMoveClick(Sender: TObject);
var
  i: Integer;
  Selected: TPostProcessBase;
  PrevInGroup: TListItem = nil;
  NextInGroup: TListItem = nil;
begin
  if lstPostProcess.Selected = nil then
    Exit;

  for i := lstPostProcess.Selected.Index - 1 downto 0 do
    if lstPostProcess.Items[i].GroupID = lstPostProcess.Selected.GroupID then
    begin
      PrevInGroup := lstPostProcess.Items[i];
      Break;
    end;

  for i := lstPostProcess.Selected.Index + 1 to lstPostProcess.Items.Count - 1 do
    if lstPostProcess.Items[i].GroupID = lstPostProcess.Selected.GroupID then
    begin
      NextInGroup := lstPostProcess.Items[i];
      Break;
    end;

  Selected := TPostProcessBase(lstPostProcess.Selected.Data);

  for i := 0 to FTemporaryPostProcessors.Count - 1 do
    if FTemporaryPostProcessors[i] = TPostProcessBase(lstPostProcess.Selected.Data) then
    begin
      if (Sender = btnMoveUp) and Assigned(PrevInGroup) then
        FTemporaryPostProcessors.Exchange(i, PrevInGroup.Index)
      else if (Sender = btnMoveDown) and Assigned(NextInGroup) then
        FTemporaryPostProcessors.Exchange(i, NextInGroup.Index);
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
  Colors: TVTColors;
begin
  Colors := TVTColors.Create(nil);
  try
    pnlTreeNodeFontColor.Color := clWindowText;
    pnlTreeSelectionTextColor.Color := Colors.SelectionTextColor;
    pnlTreeFocusedSelectionColor.Color := Colors.FocusedSelectionColor;
    pnlTreeBackgroundColor.Color := clWindow;
  finally
    Colors.Free;
  end;
end;

procedure TfrmSettings.btnResetFilePatternClick(Sender: TObject);
begin
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
  txtRemoveChars.Control.Text := '[]{}#$§%~^';
  txtRemoveChars.Control.ApplyFocus;
  RemoveGray(txtRemoveChars.Control);
end;

procedure TfrmSettings.btnResetTitlePatternClick(Sender: TObject);
begin
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
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutPlay));
    Item.Data := Pointer(AppGlobals.ShortcutPlay);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutPause));
    Item.Data := Pointer(AppGlobals.ShortcutPause);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutStop));
    Item.Data := Pointer(AppGlobals.ShortcutStop);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutNext));
    Item.Data := Pointer(AppGlobals.ShortcutNext);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutPrev));
    Item.Data := Pointer(AppGlobals.ShortcutPrev);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutVolUp));
    Item.Data := Pointer(AppGlobals.ShortcutVolUp);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutVolDown));
    Item.Data := Pointer(AppGlobals.ShortcutVolDown);
    Item.ImageIndex := TImages.KEYBOARD;

    Item := lstHotkeys.Items.Add;
    Item.SubItems.Add(ShortcutToText(AppGlobals.ShortcutMute));
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
  Result := False;

  if not inherited then
    Exit;

  if ControlVisible(txtFilePattern) and (Trim(TFunctions.RemoveFileExt(ValidatePattern(txtFilePattern.Control.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second'))) = '') then
  begin
    TFunctions.MsgBox(_('Please enter a valid pattern for filenames of completely recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtFilePattern.Parent)));
    txtFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtIncompleteFilePattern) and (Trim(TFunctions.RemoveFileExt(ValidatePattern(txtIncompleteFilePattern.Control.Text, 'artist|title|album|genre|streamtitle|number|streamname|day|month|year|hour|minute|second'))) =
    '') then
  begin
    TFunctions.MsgBox(_('Please enter a valid pattern for filenames of incompletely recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtIncompleteFilePattern.Parent)));
    txtIncompleteFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtAutomaticFilePattern) and (Trim(TFunctions.RemoveFileExt(ValidatePattern(txtAutomaticFilePattern.Control.Text, 'artist|title|album|genre|streamtitle|streamname|day|month|year|hour|minute|second'))) = '') then
  begin
    TFunctions.MsgBox(_('Please enter a valid pattern for filenames of automatically recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtAutomaticFilePattern.Parent)));
    txtAutomaticFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtStreamFilePattern) and (Trim(TFunctions.RemoveFileExt(ValidatePattern(txtStreamFilePattern.Control.Text, 'streamname|day|month|year|hour|minute|second'))) = '') then
  begin
    TFunctions.MsgBox(_('Please enter a valid pattern for filenames of stream files so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtStreamFilePattern.Parent)));
    txtStreamFilePattern.Control.ApplyFocus;
    Exit;
  end;

  if ControlVisible(txtDir) and (not DirectoryExists(txtDir.Control.Text)) then
  begin
    if FSettingsType = stAuto then
      TFunctions.MsgBox(_('The selected folder for automatically saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION)
    else
      TFunctions.MsgBox(_('The selected folder for saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
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
          TFunctions.MsgBox(_('A hotkey can be defined only once. Please edit the key mappings.'), _('Info'), MB_ICONINFORMATION);
          SetPage(FPageList.Find(pnlHotkeys));
          Exit;
        end;

  if ControlVisible(txtRetryDelay) and (txtRetryDelay.Control.Value > 999) then
    txtRetryDelay.Control.Value := 999;

  Result := True;
end;

function TfrmSettings.CheckImportFile(Filename: string): Boolean;
var
  S: TMemoryStream;
begin
  Result := inherited;

  try
    S := TMemoryStream.Create;
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
      if E is EFOpenError then
        TFunctions.MsgBox(_('The file could not be imported because it could not be opened for reading.'), _('Error'), MB_ICONERROR)
      else if E is EUnsupportedFormatException then
        TFunctions.MsgBox(_('The file could not be imported because it contains regular saved data and no exported profile.'), _('Error'), MB_ICONERROR)
      else if E is EUnknownFormatException then
        TFunctions.MsgBox(_('The file could not be imported because it''s format is unknown.'), _('Error'), MB_ICONERROR)
      else
        TFunctions.MsgBox(_('The file could not be imported.'), _('Error'), MB_ICONERROR);
    end;
  end;
end;

procedure TfrmSettings.chkAddSavedToIgnoreClick(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(chkAddSavedToIgnore);
end;

procedure TfrmSettings.chkAddSavedToStreamIgnoreClick(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(chkAddSavedToStreamIgnore);
end;

procedure TfrmSettings.chkAdjustTrackOffsetClick(Sender: TObject);
begin
  pnlAdjustTrackOffset.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;
  optAdjustBackward.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;
  optAdjustForward.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;

  if FInitialized then
    RemoveGray(chkAdjustTrackOffset);
end;

procedure TfrmSettings.chkManualSilenceLevelClick(Sender: TObject);
begin
  txtSilenceLevel.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);

  if FInitialized then
    RemoveGray(chkManualSilenceLevel);
end;

procedure TfrmSettings.chkMonitorModeClick(Sender: TObject);
begin
  txtMonitorCount.Enabled := (chkMonitorMode.State <> cbUnchecked) and (chkSubmitStats.State <> cbUnchecked);
end;

procedure TfrmSettings.chkAutoTuneInClick(Sender: TObject);
begin
  lstMinQuality.Enabled := chkAutoTuneIn.Checked;
  lstFormat.Enabled := chkAutoTuneIn.Checked;
  chkAutoTuneInConsiderIgnore.Enabled := chkAutoTuneIn.Checked;
  chkAutoTuneInAddToIgnore.Enabled := chkAutoTuneIn.Checked;
  chkAutoRemoveSavedFromWishlist.Enabled := chkAutoTuneIn.Checked;
end;

procedure TfrmSettings.chkOverwriteSmallerClick(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(chkOverwriteSmaller);
end;

procedure TfrmSettings.chkRemoveSavedFromWishlistClick(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(chkRemoveSavedFromWishlist);
end;

procedure TfrmSettings.chkDeleteStreamsClick(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(chkDeleteStreams);
end;

procedure TfrmSettings.chkDiscardAlwaysClick(Sender: TObject);
begin
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
  if FInitialized then
    RemoveGray(chkDiscardSmaller);
end;

procedure TfrmSettings.chkLimitClick(Sender: TObject);
begin
  txtMaxSpeed.Enabled := chkLimit.Checked;
end;

procedure TfrmSettings.chkNormalizeVariablesClick(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(chkNormalizeVariables);
end;

procedure TfrmSettings.chkOnlyIfCutClick(Sender: TObject);
begin
  if FInitialized then
    RemoveGray(lstPostProcess);

  if (lstPostProcess.Selected <> nil) and chkOnlyIfCut.Focused then
    TPostProcessBase(lstPostProcess.Selected.Data).OnlyIfCut := chkOnlyIfCut.Checked;
end;

procedure TfrmSettings.chkOnlySaveFullClick(Sender: TObject);
begin
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
  if FInitialized then
  begin
    RemoveGray(chkSeparateTracks);

    FOptionChanging := True;

    chkDeleteStreams.Enabled := chkSeparateTracks.Checked;
    chkDeleteStreams.Checked := chkSaveStreamsToDisk.Checked and FStreamSettings[0].DeleteStreams;

    chkOnlySaveFull.Enabled := chkSeparateTracks.Checked;
    chkOnlySaveFull.Checked := chkSeparateTracks.Checked and FStreamSettings[0].OnlySaveFull;

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
  txtShortLengthSeconds.Enabled := chkSkipShort.State <> cbUnchecked;

  if FInitialized then
    RemoveGray(chkSkipShort);
end;

procedure TfrmSettings.chkSubmitStatsClick(Sender: TObject);
begin
  chkMonitorMode.Enabled := chkSubmitStats.State <> cbUnchecked;

  Label20.Enabled := chkMonitorMode.Enabled;
  txtMonitorCount.Enabled := (chkMonitorMode.State <> cbUnchecked) and (chkSubmitStats.State <> cbUnchecked);
end;

procedure TfrmSettings.chkTrayClick(Sender: TObject);
begin
  optClose.Enabled := chkTray.Checked;
  optMinimize.Enabled := chkTray.Checked;
end;

constructor TfrmSettings.Create(AOwner: TComponent; SettingsType: TSettingsTypes; StreamSettings: TStreamSettingsArray; BrowseDir: Boolean);
var
  S: TStreamSettings;
begin
  FSettingsType := SettingsType;

  FIgnoreFieldList := TList.Create;

  FStreamSettings := [];
  for S in StreamSettings do
    FStreamSettings += [S.Copy];

  case SettingsType of
    stApp:
      CreateApp(AOwner, BrowseDir);
    stAuto:
      CreateAuto(AOwner, BrowseDir);
    stStream:
      CreateStreams(AOwner);
  end;

  pnlCutDisabled.Caption := _('Settings for cutting are only available'#13#10'if ''Save separated tracks'' is enabled.');

  FInitialized := True;
end;

procedure TfrmSettings.CreateApp(AOwner: TComponent; BrowseDir: Boolean);
var
  i: Integer;
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
  txtAutomaticFilePattern.Visible := False;

  // Offseteinstellungen verstecken
  chkAdjustTrackOffset.Visible := False;
  pnlAdjustTrackOffset.Visible := False;
  optAdjustBackward.Visible := False;
  optAdjustForward.Visible := False;

  for i := 0 to AppGlobals.AddonManager.Addons.Count - 1 do
  begin
    Item := lstAddons.Items.Add;
    Item.Caption := AppGlobals.AddonManager.Addons[i].Name;
    Item.Checked := AppGlobals.AddonManager.Addons[i].PackageDownloaded and AppGlobals.AddonManager.Addons[i].FilesExtracted;
    Item.Data := AppGlobals.AddonManager.Addons[i].Copy;

    Item.ImageIndex := TImages.PLUGIN;
  end;
  if lstAddons.Items.Count > 0 then
    lstAddons.Items[0].Selected := True;

  BuildHotkeys;

  Bass.EnumDevices;
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

  lstColorMode.Control.ItemIndex := Integer(AppGlobals.ColorMode);

  if AppGlobals.TreeColorsLoaded then
  begin
    pnlTreeNodeFontColor.Color := AppGlobals.TreeNodeFontColor;
    pnlTreeSelectionTextColor.Color := AppGlobals.TreeSelectionTextColor;
    pnlTreeFocusedSelectionColor.Color := AppGlobals.TreeFocusedSelectionColor;
    pnlTreeBackgroundColor.Color := AppGlobals.TreeBackgroundColor;
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
    if (pnlFilenames.Controls[i].ClassType = TMLabeledEditButton) and (pnlFilenames.Controls[i].Top > txtDir.Top) then
      pnlFilenames.Controls[i].Visible := False;

  txtAutomaticFilePattern.Visible := True;
  txtPreview.Visible := True;

  chkAutoTuneInAddToIgnore.Checked := FStreamSettings[0].AddSavedToIgnore;
  chkAutoRemoveSavedFromWishlist.Checked := FStreamSettings[0].RemoveSavedFromWishlist;

  Caption := _('Settings for automatic recordings');

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
  Bevel1.Visible := False;

  // Dateinamen ordentlich machen
  txtAutomaticFilePattern.Visible := False;

  // Erweitert ordentlich machen
  lstSoundDevice.Visible := False;
  txtLogFile.Visible := False;

  btnReset := TBitBtn.Create(Self);
  btnReset.Width := Scale96ToFont(240);
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
end;

procedure TfrmSettings.CreateGeneral;
var
  i: Integer;
begin
  for i := 0 to Self.ControlCount - 1 do
    if Self.Controls[i] is TPanel then
    begin
      if TPanel(Self.Controls[i]) = pnlLeft then
        Continue;
      Self.Controls[i].Left := Scale96ToFont(96);
      Self.Controls[i].Top := Scale96ToFont(36);
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
  Header.Options := Header.Options + [hoAutoResize] - [hoDrag];
  Header.AutoSizeIndex := 0;
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

procedure TBlacklistTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String);
var
  NodeData: PBlacklistNodeData;
begin
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
  NodeData: PBlacklistNodeData;
begin
  Result := 0;
  NodeData := GetNodeData(Node);
  if not StartsText(Text, NodeData.Name) then
    Result := 1;
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
