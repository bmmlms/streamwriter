{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
  PostProcess, StrUtils, DynBASS, ICEClient, Generics.Collections, Menus,
  MsgDlg, PngImageList, PngSpeedButton, pngimage, VirtualTrees, Math,
  DataManager, PngBitBtn, Logging, ToolWin, ListsTab, DownloadAddons,
  ExtendedStream, PluginManager, PluginBase;

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
    pnlPostProcess: TPanel;
    lstPostProcess: TListView;
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
    btnAdd: TButton;
    btnRemove: TButton;
    txtApp: TLabeledEdit;
    txtAppParams: TLabeledEdit;
    lblAppParams: TLabel;
    btnBrowseApp: TSpeedButton;
    pnlHotkeys: TPanel;
    lstHotkeys: TListView;
    txtHotkey: THotKey;
    Label9: TLabel;
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
    btnHelpPostProcess: TPngSpeedButton;
    btnMoveDown: TPngSpeedButton;
    btnMoveUp: TPngSpeedButton;
    chkDiscardSmaller: TCheckBox;
    pnlFilenames: TPanel;
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
    btnConfigure: TButton;
    chkRememberRecordings: TCheckBox;
    chkDisplayPlayNotifications: TCheckBox;
    chkAutoTuneInConsiderIgnore: TCheckBox;
    pnlBandwidth: TPanel;
    Label11: TLabel;
    txtMaxSpeed: TLabeledEdit;
    chkLimit: TCheckBox;
    txtDir: TLabeledEdit;
    btnBrowse: TSpeedButton;
    txtDirAuto: TLabeledEdit;
    btnBrowseAuto: TSpeedButton;
    lblIgnoreTitles: TLabel;
    lstIgnoreTitles: TListView;
    btnRemoveIgnoreTitlePattern: TButton;
    btnAddIgnoreTitlePattern: TButton;
    txtIgnoreTitlePattern: TLabeledEdit;
    chkAddSavedToStreamIgnore: TCheckBox;
    chkAdjustTrackOffset: TCheckBox;
    txtAdjustTrackOffset: TLabeledEdit;
    optAdjustBackward: TRadioButton;
    optAdjustForward: TRadioButton;
    chkAutoTuneInAddToIgnore: TCheckBox;
    pnlFilenamesExt: TPanel;
    txtRemoveChars: TLabeledEdit;
    txtFilePatternDecimals: TLabeledEdit;
    txtFilePattern: TLabeledEdit;
    btnResetFilePattern: TPngSpeedButton;
    txtPreview: TLabeledEdit;
    txtIncompleteFilePattern: TLabeledEdit;
    btnResetIncompleteFilePattern: TPngSpeedButton;
    txtAutomaticFilePattern: TLabeledEdit;
    btnResetAutomaticFilePattern: TPngSpeedButton;
    btnResetRemoveChars: TPngSpeedButton;
    txtStreamFilePattern: TLabeledEdit;
    btnResetStreamFilePattern: TPngSpeedButton;
    chkAutoRemoveSavedFromWishlist: TCheckBox;
    chkRemoveSavedFromWishlist: TCheckBox;
    chkNormalizeVariables: TCheckBox;
    chkManualSilenceLevel: TCheckBox;
    pnlPlugins: TPanel;
    lstPlugins: TListView;
    btnHelpPlugin: TPngSpeedButton;
    lblOutputFormat: TLabel;
    Label20: TLabel;
    lstOutputFormat: TComboBox;
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lstPostProcessSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
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
    procedure lstPostProcessCompare(Sender: TObject; Item1, Item2: TListItem;
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
    procedure lstPostProcessResize(Sender: TObject);
    procedure chkAutoTuneInClick(Sender: TObject);
    procedure chkDiscardSmallerClick(Sender: TObject);
    procedure lstHotkeysResize(Sender: TObject);
    procedure txtFilePatternDecimalsChange(Sender: TObject);
    procedure btnBlacklistRemoveClick(Sender: TObject);
    procedure txtTitlePatternChange(Sender: TObject);
    procedure btnResetTitlePatternClick(Sender: TObject);
    procedure btnResetFilePatternClick(Sender: TObject);
    procedure lstPostProcessItemChecked(Sender: TObject; Item: TListItem);
    procedure btnConfigureClick(Sender: TObject);
    procedure txtRemoveCharsChange(Sender: TObject);
    procedure chkLimitClick(Sender: TObject);
    procedure lstIgnoreTitlesResize(Sender: TObject);
    procedure txtIgnoreTitlePatternChange(Sender: TObject);
    procedure lstIgnoreTitlesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnAddIgnoreTitlePatternClick(Sender: TObject);
    procedure btnRemoveIgnoreTitlePatternClick(Sender: TObject);
    procedure chkAddSavedToStreamIgnoreClick(Sender: TObject);
    procedure chkAdjustTrackOffsetClick(Sender: TObject);
    procedure txtAdjustTrackOffsetChange(Sender: TObject);
    procedure optAdjustClick(Sender: TObject);
    procedure lstIgnoreTitlesEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure btnResetRemoveCharsClick(Sender: TObject);
    procedure txtStreamFilePatternChange(Sender: TObject);
    procedure txtStreamFilePatternClick(Sender: TObject);
    procedure chkRemoveSavedFromWishlistClick(Sender: TObject);
    procedure chkNormalizeVariablesClick(Sender: TObject);
    procedure chkManualSilenceLevelClick(Sender: TObject);
    procedure txtFilePatternEnter(Sender: TObject);
    procedure lstPluginsResize(Sender: TObject);
    procedure lstPluginsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lstPluginsItemChecked(Sender: TObject; Item: TListItem);
  private
    FInitialized: Boolean;
    FBrowseDir: Boolean;
    FBrowseAutoDir: Boolean;
    FDefaultActionIdx: Integer;
    FDefaultActionBrowserIdx: Integer;
    FDefaultFilterIdx: Integer;
    FTemporaryPostProcesses: TList<TPostProcessBase>;
    FStreamSettings: TStreamSettingsArray;
    FIgnoreFieldList: TList;
    FLists: TDataLists;
    lstBlacklist: TBlacklistTree;
    btnReset: TBitBtn;
    FActivePreviewField: TLabeledEdit;

    function ValidatePattern(Text, Patterns: string): string;
    function GetNewID: Integer;

    function GetStringListHash(Lst: TStringList): Cardinal;
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
    procedure SetPage(Page: TPage); override;
    procedure PreTranslate; override;
    procedure PostTranslate; override;
    procedure GetExportData(Stream: TExtendedStream); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists; BrowseDir, BrowseAutoDir: Boolean); reintroduce; overload;
    constructor Create(AOwner: TComponent; StreamSettings: TStreamSettingsArray); overload;
    destructor Destroy; override;
    property StreamSettings: TStreamSettingsArray read FStreamSettings;
  end;

implementation

{$R *.dfm}

constructor TfrmSettings.Create(AOwner: TComponent; Lists: TDataLists; BrowseDir, BrowseAutoDir: Boolean);
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

    if Length(FStreamSettings) > 1 then
      for i := 1 to Length(FStreamSettings) - 1 do
        if GetStringListHash(S.IgnoreTrackChangePattern) <> GetStringListHash(FStreamSettings[i].IgnoreTrackChangePattern) then
          AddField(lstIgnoreTitles);

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
      if S.StreamFilePattern <> FStreamSettings[i].StreamFilePattern then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtStreamFilePattern);

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
    for i := 0 to Length(FStreamSettings) - 1 do
    begin
      if S.NormalizeVariables <> FStreamSettings[i].NormalizeVariables then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkNormalizeVariables);

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
      if S.AddSavedToStreamIgnore <> FStreamSettings[i].AddSavedToStreamIgnore then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkAddSavedToStreamIgnore);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.RemoveSavedFromWishlist <> FStreamSettings[i].RemoveSavedFromWishlist then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkRemoveSavedFromWishlist);

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
      if S.AutoDetectSilenceLevel <> FStreamSettings[i].AutoDetectSilenceLevel then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkManualSilenceLevel);

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
      if S.SilenceBufferSecondsStart <> FStreamSettings[i].SilenceBufferSecondsStart then
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

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.AdjustTrackOffset <> FStreamSettings[i].AdjustTrackOffset then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(chkAdjustTrackOffset);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.AdjustTrackOffsetMS <> FStreamSettings[i].AdjustTrackOffsetMS then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
      AddField(txtAdjustTrackOffset);

    F := False;
    for i := 1 to Length(FStreamSettings) - 1 do
    begin
      if S.AdjustTrackOffsetDirection <> FStreamSettings[i].AdjustTrackOffsetDirection then
      begin
        F := True;
        ShowDialog := True;
        Break;
      end;
    end;
    if F then
    begin
      AddField(optAdjustBackward);
      AddField(optAdjustForward);
    end;

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

      for i := 0 to Settings.IgnoreTrackChangePattern.Count - 1 do
      begin
        Item := lstIgnoreTitles.Items.Add;
        Item.Caption := Settings.IgnoreTrackChangePattern[i];
        Item.ImageIndex := 1;
      end;

      txtAutomaticFilePattern.Visible := False;
      btnResetAutomaticFilePattern.Visible := False;

      txtStreamFilePattern.Top := txtIncompleteFilePattern.Top + (txtIncompleteFilePattern.Top - txtFilePattern.Top);
      btnResetStreamFilePattern.Top := btnResetIncompleteFilePattern.Top + (btnResetIncompleteFilePattern.Top - btnResetFilePattern.Top);
      txtPreview.Top := txtStreamFilePattern.Top + (txtIncompleteFilePattern.Top - txtFilePattern.Top);
    end else
    begin
      chkAutoTuneInClick(chkAutoTuneIn);
    end;

    FBrowseDir := BrowseDir;
    FBrowseAutoDir := BrowseAutoDir;

    SetFields;

    ClientWidth := 590;
    ClientHeight := 480;

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

    FTemporaryPostProcesses := TList<TPostProcessBase>.Create;

    for i := 0 to AppGlobals.PostProcessManager.PostProcessors.Count - 1 do
    begin
      if AppGlobals.PostProcessManager.PostProcessors[i].Hidden then
        Continue;

      Item := lstPostProcess.Items.Add;
      Item.GroupID := AppGlobals.PostProcessManager.PostProcessors[i].GroupID;
      Item.Caption := AppGlobals.PostProcessManager.PostProcessors[i].Name;
      Item.Checked := AppGlobals.PostProcessManager.PostProcessors[i].Active;
      Item.Data := AppGlobals.PostProcessManager.PostProcessors[i].Copy;
      FTemporaryPostProcesses.Add(TPostProcessBase(Item.Data));

      if AppGlobals.PostProcessManager.PostProcessors[i] is TInternalPostProcess then
      begin
        Item.ImageIndex := 4;
      end else
      begin
        Item.ImageIndex := 5;
      end;
    end;
    lstPostProcess.CustomSort(nil, 0);
    if lstPostProcess.Items.Count > 0 then
      lstPostProcess.Items[0].Selected := True;

    B := TBitmap.Create;
    P := TPngImage.Create;
    try
      P.LoadFromResourceName(HInstance, 'ARROWUP');
      btnMoveUp.PngImage := P;
      P.LoadFromResourceName(HInstance, 'ARROWDOWN');
      btnMoveDown.PngImage := P;
      P.LoadFromResourceName(HInstance, 'QUESTION');
      btnHelpPostProcess.PngImage := P;
      btnHelpPlugin.PngImage := P;
      GetBitmap('BROWSE', 2, B);
      btnBrowse.Glyph := B;
      btnBrowseAuto.Glyph := B;
      btnBrowseApp.Glyph := B;
    finally
      B.Free;
      P.Free;
    end;

    for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
    begin
      Item := lstPlugins.Items.Add;
      Item.Caption := AppGlobals.PluginManager.Plugins[i].Name;
      Item.Checked := AppGlobals.PluginManager.Plugins[i].FilesExtracted;
      Item.Data := AppGlobals.PluginManager.Plugins[i].Copy;

      Item.ImageIndex := 6;
    end;
    if lstPlugins.Items.Count > 0 then
      lstPlugins.Items[0].Selected := True;

    BuildHotkeys;

    if (Bass.DeviceAvailable) and (Bass.Devices.Count > 0) then
    begin
      for i := 0 to Bass.Devices.Count - 1 do
        lstSoundDevice.Items.AddObject(Bass.Devices[i].Name, Bass.Devices[i]);
      if lstSoundDevice.Items.Count > 0 then
        lstSoundDevice.ItemIndex := 0;
      try
        for i := 0 to lstSoundDevice.Items.Count - 1 do
          if TBassDevice(lstSoundDevice.Items.Objects[i]).ID = AppGlobals.SoundDevice then
          begin
            lstSoundDevice.ItemIndex := i;
            Break;
          end;
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

    if Length(FStreamSettings) = 0 then
    begin
      chkAdjustTrackOffset.Visible := False;
      txtAdjustTrackOffset.Visible := False;
      optAdjustBackward.Visible := False;
      optAdjustForward.Visible := False;
    end;

    FInitialized := True;
  finally
    Settings.Free;
  end;
end;

constructor TfrmSettings.Create(AOwner: TComponent;
  StreamSettings: TStreamSettingsArray);
var
  i, Substract: Integer;
begin
  FIgnoreFieldList := TList.Create;

  SetLength(FStreamSettings, Length(StreamSettings));
  for i := 0 to Length(StreamSettings) - 1 do
  begin
    FStreamSettings[i] := StreamSettings[i].Copy;
  end;

  Create(AOwner, nil, False, False);

  txtDir.Visible := False;
  txtDirAuto.Visible := False;
  btnBrowse.Visible := False;
  btnBrowseAuto.Visible := False;

  Substract := chkSaveStreamsToMemory.Top;
  for i := 0 to pnlStreams.ControlCount - 1 do
  begin
    if (pnlStreams.Controls[i].ClassType <> TEdit) and (pnlStreams.Controls[i].ClassType <> TSpeedButton) then
      pnlStreams.Controls[i].Top := pnlStreams.Controls[i].Top - Substract - 1;
  end;

  lblTop.Caption := _('Stream settings');
end;

destructor TfrmSettings.Destroy;
var
  i: Integer;
begin
  FIgnoreFieldList.Free;

  for i := 0 to FTemporaryPostProcesses.Count - 1 do
    FTemporaryPostProcesses[i].Free;
  FTemporaryPostProcesses.Free;

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

  AppGlobals.Lock;
  txtFilePattern.Text := Settings.FilePattern;
  txtIncompleteFilePattern.Text := Settings.IncompleteFilePattern;
  txtAutomaticFilePattern.Text := AppGlobals.AutomaticFilePattern;
  txtStreamFilePattern.Text := Settings.StreamFilePattern;
  txtFilePatternDecimals.Text := IntToStr(Settings.FilePatternDecimals);
  txtRemoveChars.Text := Settings.RemoveChars;
  chkNormalizeVariables.Checked := Settings.NormalizeVariables;

  txtDir.Text := AppGlobals.Dir;
  txtDirAuto.Text := AppGlobals.DirAuto;

  chkDeleteStreams.Checked := Settings.DeleteStreams;
  chkAddSavedToIgnore.Checked := Settings.AddSavedToIgnore;
  chkAddSavedToStreamIgnore.Checked := Settings.AddSavedToStreamIgnore;
  chkRemoveSavedFromWishlist.Checked := Settings.RemoveSavedFromWishlist;
  chkOverwriteSmaller.Checked := Settings.OverwriteSmaller;
  chkDiscardSmaller.Checked := Settings.DiscardSmaller;
  txtTitlePattern.Text := Settings.TitlePattern;

  chkSkipShort.Checked := Settings.SkipShort;
  chkSearchSilence.Checked := Settings.SearchSilence;
  chkManualSilenceLevel.Checked := not Settings.AutoDetectSilenceLevel;

  chkSearchSilenceClick(nil);
  chkManualSilenceLevelClick(nil);

  chkTray.Checked := AppGlobals.Tray;
  chkSnapMain.Checked := AppGlobals.SnapMain;
  chkRememberRecordings.Checked := AppGlobals.RememberRecordings;
  chkDisplayPlayNotifications.Checked := AppGlobals.DisplayPlayNotifications;
  optClose.Checked := not AppGlobals.TrayOnMinimize;
  optMinimize.Checked := AppGlobals.TrayOnMinimize;

  chkTrayClick(nil);

  chkAutoTuneIn.Checked := AppGlobals.AutoTuneIn;
  chkAutoTuneInConsiderIgnore.Checked := AppGlobals.AutoTuneInConsiderIgnore;
  chkAutoTuneInAddToIgnore.Checked := AppGlobals.AutoTuneInAddToIgnore;
  chkAutoRemoveSavedFromWishlist.Checked := AppGlobals.AutoRemoveSavedFromWishlist;
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
  txtSilenceBufferSeconds.Text := IntToStr(Settings.SilenceBufferSecondsStart);

  chkAdjustTrackOffset.Checked := Settings.AdjustTrackOffset;
  txtAdjustTrackOffset.Text := IntToStr(Settings.AdjustTrackOffsetMS);
  if Settings.AdjustTrackOffsetDirection = toForward then
    optAdjustForward.Checked := True
  else
    optAdjustBackward.Checked := True;

  AppGlobals.Unlock;

  if not DirectoryExists(txtDir.Text) then
    txtDir.Text := '';

  if not DirectoryExists(txtDirAuto.Text) then
    txtDirAuto.Text := '';

  if not Bass.BassLoaded then
  begin
    chkSearchSilence.Enabled := False;
    chkSearchSilence.Checked := False;
    chkManualSilenceLevel.Enabled := False;
    chkManualSilenceLevel.Checked := False;
    txtSilenceLevel.Enabled := False;
    txtSilenceLength.Enabled := False;
    txtSilenceBufferSeconds.Enabled := False;
    Label10.Enabled := False;
    Label12.Enabled := False;
    Label13.Enabled := False;

    chkAdjustTrackOffset.Enabled := False;
    txtAdjustTrackOffset.Enabled := False;
    optAdjustBackward.Enabled := False;
    optAdjustForward.Enabled := False;
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
  AdvancedDiffers: Boolean;
  PostProcessor: TPostProcessBase;
  EP: TExternalPostProcess;
  Item: TListItem;
  OldTitlePattern: string;
  OldIgnoreTitles: Cardinal;
begin
  AdvancedDiffers := False;

  if Length(FStreamSettings) > 0 then
  begin
    OldTitlePattern := FStreamSettings[0].TitlePattern;
    OldIgnoreTitles := GetStringListHash(FStreamSettings[0].IgnoreTrackChangePattern);

    for i := 0 to Length(FStreamSettings) - 1 do
    begin
      if FIgnoreFieldList.IndexOf(txtFilePattern) = -1 then
        FStreamSettings[i].FilePattern := Trim(txtFilePattern.Text);

      if FIgnoreFieldList.IndexOf(txtIncompleteFilePattern) = -1 then
        FStreamSettings[i].IncompleteFilePattern := Trim(txtIncompleteFilePattern.Text);

      if FIgnoreFieldList.IndexOf(txtStreamFilePattern) = -1 then
        FStreamSettings[i].StreamFilePattern := Trim(txtStreamFilePattern.Text);

      if FIgnoreFieldList.IndexOf(txtFilePatternDecimals) = -1 then
        FStreamSettings[i].FilePatternDecimals := StrToIntDef(txtFilePatternDecimals.Text, 3);

      if FIgnoreFieldList.IndexOf(txtRemoveChars) = -1 then
        FStreamSettings[i].RemoveChars := txtRemoveChars.Text;

      if FIgnoreFieldList.IndexOf(chkNormalizeVariables) = -1 then
        FStreamSettings[i].NormalizeVariables := chkNormalizeVariables.Checked;

      if FIgnoreFieldList.IndexOf(chkDeleteStreams) = -1 then
        FStreamSettings[i].DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;

      if FIgnoreFieldList.IndexOf(chkAddSavedToIgnore) = -1 then
        FStreamSettings[i].AddSavedToIgnore := chkAddSavedToIgnore.Checked;

      if FIgnoreFieldList.IndexOf(chkAddSavedToStreamIgnore) = -1 then
        FStreamSettings[i].AddSavedToStreamIgnore := chkAddSavedToStreamIgnore.Checked;

      if FIgnoreFieldList.IndexOf(chkRemoveSavedFromWishlist) = -1 then
        FStreamSettings[i].RemoveSavedFromWishlist := chkRemoveSavedFromWishlist.Checked;

      if FIgnoreFieldList.IndexOf(chkOverwriteSmaller) = -1 then
        FStreamSettings[i].OverwriteSmaller := chkOverwriteSmaller.Checked;

      if FIgnoreFieldList.IndexOf(chkDiscardSmaller) = -1 then
        FStreamSettings[i].DiscardSmaller := chkDiscardSmaller.Checked;

      if Length(FStreamSettings) > 0 then
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

        if FIgnoreFieldList.IndexOf(chkManualSilenceLevel) = -1 then
          FStreamSettings[i].AutoDetectSilenceLevel := not chkManualSilenceLevel.Checked;

        if FIgnoreFieldList.IndexOf(txtSilenceLevel) = -1 then
          FStreamSettings[i].SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 5);

        if FIgnoreFieldList.IndexOf(txtSilenceLength) = -1 then
          FStreamSettings[i].SilenceLength := StrToIntDef(txtSilenceLength.Text, 100);

        if FIgnoreFieldList.IndexOf(txtSilenceBufferSeconds) = -1 then
        begin
          FStreamSettings[i].SilenceBufferSecondsStart := StrToIntDef(txtSilenceBufferSeconds.Text, 5);
          FStreamSettings[i].SilenceBufferSecondsEnd := StrToIntDef(txtSilenceBufferSeconds.Text, 5);
        end;

        if Length(FStreamSettings) > 0 then
        begin
          if FIgnoreFieldList.IndexOf(chkAdjustTrackOffset) = -1 then
            FStreamSettings[i].AdjustTrackOffset := chkAdjustTrackOffset.Checked;

          if FIgnoreFieldList.IndexOf(txtAdjustTrackOffset) = -1 then
            FStreamSettings[i].AdjustTrackOffsetMS := StrToInt(txtAdjustTrackOffset.Text);

          if FIgnoreFieldList.IndexOf(optAdjustBackward) = -1 then
          begin
            if optAdjustBackward.Checked then
              FStreamSettings[i].AdjustTrackOffsetDirection := toBackward
            else
              FStreamSettings[i].AdjustTrackOffsetDirection := toForward;
          end;
        end;
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

      if (FIgnoreFieldList.IndexOf(lstIgnoreTitles) = -1) and (Length(FStreamSettings) > 0) then
      begin
        FStreamSettings[i].IgnoreTrackChangePattern.Clear;
        for n := 0 to lstIgnoreTitles.Items.Count - 1 do
          FStreamSettings[i].IgnoreTrackChangePattern.Add(lstIgnoreTitles.Items[n].Caption);
      end;

      if (FIgnoreFieldList.IndexOf(txtTitlePattern) = -1) and (OldTitlePattern <> FStreamSettings[i].TitlePattern) then
        AdvancedDiffers := True;
      if (FIgnoreFieldList.IndexOf(lstIgnoreTitles) = -1) and (GetStringListHash(FStreamSettings[i].IgnoreTrackChangePattern) <> OldIgnoreTitles) then
        AdvancedDiffers := True;
    end;

    if AdvancedDiffers then
      TfrmMsgDlg.ShowMsg(Self, _('You changed some advanced stream specific settings. If they work, please contribute them to the community by selecting ''Set data...'' using the ''Administration'' menu from the stream browser popup menu.'), 10, btOK);
  end else
  begin
    AppGlobals.Lock;
    AppGlobals.StreamSettings.FilePattern := Trim(txtFilePattern.Text);
    AppGlobals.StreamSettings.IncompleteFilePattern := Trim(txtIncompleteFilePattern.Text);
    AppGlobals.StreamSettings.StreamFilePattern := Trim(txtStreamFilePattern.Text);
    AppGlobals.AutomaticFilePattern := Trim(txtAutomaticFilePattern.Text);
    AppGlobals.StreamSettings.FilePatternDecimals := StrToIntDef(txtFilePatternDecimals.Text, 3);
    AppGlobals.StreamSettings.RemoveChars := txtRemoveChars.Text;
    AppGlobals.StreamSettings.NormalizeVariables := chkNormalizeVariables.Checked;
    AppGlobals.StreamSettings.DeleteStreams := chkDeleteStreams.Checked and chkDeleteStreams.Enabled;
    AppGlobals.StreamSettings.AddSavedToIgnore := chkAddSavedToIgnore.Checked;
    AppGlobals.StreamSettings.AddSavedToStreamIgnore := chkAddSavedToStreamIgnore.Checked;
    AppGlobals.StreamSettings.RemoveSavedFromWishlist := chkRemoveSavedFromWishlist.Checked;
    AppGlobals.StreamSettings.OverwriteSmaller := chkOverwriteSmaller.Checked;
    AppGlobals.StreamSettings.DiscardSmaller := chkDiscardSmaller.Checked;

    if pnlCut.Tag = 0 then
    begin
      AppGlobals.StreamSettings.SkipShort := chkSkipShort.Checked;
      AppGlobals.StreamSettings.SongBufferSeconds := StrToIntDef(txtSongBuffer.Text, 0);
      AppGlobals.StreamSettings.ShortLengthSeconds := StrToIntDef(txtShortLengthSeconds.Text, 45);
      AppGlobals.StreamSettings.SearchSilence := chkSearchSilence.Checked;
      AppGlobals.StreamSettings.AutoDetectSilenceLevel := not chkManualSilenceLevel.Checked;
      AppGlobals.StreamSettings.SilenceLevel := StrToIntDef(txtSilenceLevel.Text, 5);
      AppGlobals.StreamSettings.SilenceLength := StrToIntDef(txtSilenceLength.Text, 100);
      AppGlobals.StreamSettings.SilenceBufferSecondsStart := StrToIntDef(txtSilenceBufferSeconds.Text, 5);
      AppGlobals.StreamSettings.SilenceBufferSecondsEnd := StrToIntDef(txtSilenceBufferSeconds.Text, 5);
    end;

    AppGlobals.StreamSettings.MaxRetries := StrToIntDef(txtMaxRetries.Text, 100);
    AppGlobals.StreamSettings.RetryDelay := StrToIntDef(txtRetryDelay.Text, 5);
    AppGlobals.StreamSettings.Filter := TUseFilters(lstDefaultFilter.ItemIndex);

    AppGlobals.StreamSettings.SeparateTracks := chkSeparateTracks.Checked and chkSeparateTracks.Enabled;
    AppGlobals.StreamSettings.SaveToMemory := chkSaveStreamsToMemory.Checked;
    AppGlobals.StreamSettings.OnlySaveFull := chkOnlySaveFull.Checked;

    if lstSoundDevice.ItemIndex > -1 then
      AppGlobals.SoundDevice := TBassDevice(lstSoundDevice.Items.Objects[lstSoundDevice.ItemIndex]).ID;

    AppGlobals.Dir := txtDir.Text;
    AppGlobals.DirAuto := txtDirAuto.Text;
    AppGlobals.Tray := chkTray.Checked;
    AppGlobals.SnapMain := chkSnapMain.Checked;
    AppGlobals.RememberRecordings := chkRememberRecordings.Checked;
    AppGlobals.DisplayPlayNotifications := chkDisplayPlayNotifications.Checked;
    AppGlobals.TrayOnMinimize := optMinimize.Checked;

    AppGlobals.AutoTuneIn := chkAutoTuneIn.Checked;
    AppGlobals.AutoTuneInConsiderIgnore := chkAutoTuneInConsiderIgnore.Checked;
    AppGlobals.AutoTuneInAddToIgnore := chkAutoTuneInAddToIgnore.Checked;
    AppGlobals.AutoRemoveSavedFromWishlist := chkAutoRemoveSavedFromWishlist.Checked;
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

    if lstHotkeys.Items[7].SubItems[0] <> '' then
      AppGlobals.ShortcutMute := TextToShortCut(lstHotkeys.Items[7].SubItems[0])
    else
      AppGlobals.ShortcutMute := 0;

    for i := 0 to FTemporaryPostProcesses.Count - 1 do
    begin
      PostProcessor := AppGlobals.PostProcessManager.Find(FTemporaryPostProcesses[i]);

      if PostProcessor = nil then
      begin
        // Ein neues Plugin kann nur TExternalPlugin sein.
        PostProcessor := FTemporaryPostProcesses[i].Copy;
        AppGlobals.PostProcessManager.PostProcessors.Add(PostProcessor);
      end;

      Item := nil;
      for n := 0 to lstPostProcess.Items.Count - 1 do
        if lstPostProcess.Items[n].Data = FTemporaryPostProcesses[i] then
        begin
          Item := lstPostProcess.Items[n];
          Break;
        end;

      PostProcessor.OnlyIfCut := FTemporaryPostProcesses[i].OnlyIfCut;
      PostProcessor.Order := Item.Index;
      PostProcessor.Active := Item.Checked;

      PostProcessor.Assign(FTemporaryPostProcesses[i]);
    end;

    // Vom Benutzer entfernte Plugins aus den echten Plugins entfernen..
    for i := AppGlobals.PostProcessManager.PostProcessors.Count - 1 downto 0 do
    begin
      if AppGlobals.PostProcessManager.PostProcessors[i] is TExternalPostProcess then
      begin
        EP := nil;
        for n := 0 to FTemporaryPostProcesses.Count - 1 do
          if FTemporaryPostProcesses[n] is TExternalPostProcess then
            if TExternalPostProcess(FTemporaryPostProcesses[n]).Identifier = TExternalPostProcess(AppGlobals.PostProcessManager.PostProcessors[i]).Identifier then
              begin
                EP := TExternalPostProcess(AppGlobals.PostProcessManager.PostProcessors[i]);
                Break;
              end;
        if EP = nil then
        begin
          AppGlobals.PostProcessManager.PostProcessors[i].Free;
          AppGlobals.PostProcessManager.PostProcessors.Delete(i);
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
    SetPage(FPageList.Find(TPanel(txtDir.Parent)));
    btnBrowse.Click;
    FBrowseDir := False;
  end;

  if FBrowseAutoDir then
  begin
    SetPage(FPageList.Find(TPanel(txtDirAuto.Parent)));
    btnBrowseAuto.Click;
    FBrowseAutoDir := False;
  end;
end;

procedure TfrmSettings.FormResize(Sender: TObject);
begin
  inherited;

  lblPanelCut.Top := pnlCut.ClientHeight div 2 - lblPanelCut.Height div 2;
  lblPanelCut.Left := pnlCut.ClientWidth div 2 - lblPanelCut.Width div 2;

  txtSilenceLength.Left := Label12.Left + Label12.Width + 4;
  Label13.Left := txtSilenceLength.Left + txtSilenceLength.Width + 4;

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

procedure TfrmSettings.GetExportData(Stream: TExtendedStream);
begin
  inherited;

  FLists.Save(Stream);
end;

function TfrmSettings.GetNewID: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to lstPostProcess.Items.Count - 1 do
  begin
    if TPostProcessBase(lstPostProcess.Items[i].Data) is TExternalPostProcess then
      if TExternalPostProcess(lstPostProcess.Items[i].Data).Identifier = Result then
      begin
        Inc(Result);
        Continue;
      end;
  end;
  for i := 0 to AppGlobals.PostProcessManager.PostProcessors.Count - 1 do
  begin
    if TPostProcessBase(AppGlobals.PostProcessManager.PostProcessors[i]) is TExternalPostProcess then
      if TExternalPostProcess(AppGlobals.PostProcessManager.PostProcessors[i]).Identifier = Result then
      begin
        Inc(Result);
        Continue;
      end;
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

procedure TfrmSettings.lstIgnoreTitlesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  btnRemoveIgnoreTitlePattern.Enabled := lstIgnoreTitles.Selected <> nil;
end;

procedure TfrmSettings.lstIgnoreTitlesEdited(Sender: TObject;
  Item: TListItem; var S: string);
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

procedure TfrmSettings.lstPluginsItemChecked(Sender: TObject;
  Item: TListItem);
var
  Res: Integer;
  DA: TfrmDownloadAddons;
begin
  if not FInitialized then
    Exit;

  lstPlugins.OnItemChecked := nil;
  Item.Checked := AppGlobals.PluginManager.EnablePlugin(Self, TPluginBase(Item.Data), True);
  lstPlugins.OnItemChecked := lstPluginsItemChecked;

  lstPlugins.Selected := Item;
end;

procedure TfrmSettings.lstPluginsResize(Sender: TObject);
begin
  inherited;

  lstPlugins.Columns[0].Width := lstPlugins.ClientWidth - 25;
end;

procedure TfrmSettings.lstPluginsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnHelpPlugin.Enabled := (Item <> nil) and Selected and (TPluginBase(Item.Data).Help <> '');
end;

procedure TfrmSettings.lstPostProcessCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  P1, P2: TPostProcessBase;
begin
  inherited;
  P1 := TPostProcessBase(Item1.Data);
  P2 := TPostProcessBase(Item2.Data);
  Compare := CmpInt(P1.Order, P2.Order);
end;

procedure TfrmSettings.lstPostProcessItemChecked(Sender: TObject; Item: TListItem);
var
  i: Integer;
begin
  if not FInitialized then
    Exit;

  if Item.Data = nil then
    Exit;

  if TObject(Item.Data) is TInternalPostProcess then
  begin
    lstPostProcess.OnItemChecked := nil;
    Item.Checked := AppGlobals.PostProcessManager.EnablePostProcess(Self, Item.Checked, TInternalPostProcess(Item.Data));
    lstPostProcess.OnItemChecked := lstPostProcessItemChecked;

    btnConfigure.Enabled := Item.Checked and TPostProcessBase(Item.Data).CanConfigure;
  end;

  lstPlugins.OnItemChecked := nil;
  for i := 0 to lstPlugins.Items.Count - 1 do
    lstPlugins.Items[i].Checked := TPluginBase(lstPlugins.Items[i].Data).PackageDownloaded;
  lstPlugins.OnItemChecked := lstPluginsItemChecked;

  lstPostProcess.Selected := Item;
end;

procedure TfrmSettings.lstPostProcessResize(Sender: TObject);
begin
  inherited;

  lstPostProcess.Columns[0].Width := lstPostProcess.ClientWidth - 25;
end;

procedure TfrmSettings.lstPostProcessSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Item.Data = nil then
    Exit;

  btnConfigure.Enabled := False;

  btnHelpPostProcess.Enabled := (Item <> nil) and Selected and (TPostProcessBase(Item.Data).Help <> '');
  btnRemove.Enabled := (Item <> nil) and Selected and (TPostProcessBase(Item.Data) is TExternalPostProcess);

  btnMoveUp.Enabled := (Item <> nil) and Selected and (not (Item.Index = 0)) and (not (lstPostProcess.Items[Item.Index - 1].Data = nil));
  btnMoveDown.Enabled := (Item <> nil) and Selected and (not (Item.Index = lstPostProcess.Items.Count - 1)) and (not (lstPostProcess.Items[Item.Index + 1].Data = nil));

  chkOnlyIfCut.Checked := (Item <> nil) and Selected and TPostProcessBase(Item.Data).OnlyIfCut;
  chkOnlyIfCut.Enabled := (Item <> nil) and Selected;

  if Selected and (TPostProcessBase(Item.Data) is TExternalPostProcess) then
  begin
    txtApp.Text := TExternalPostProcess(Item.Data).Exe;
    txtAppParams.Text := TExternalPostProcess(Item.Data).Params;
    txtApp.Enabled := True;
    txtAppParams.Enabled := True;
    btnBrowseApp.Enabled := True;
    lblAppParams.Enabled := True;
    btnRemove.Enabled := True;
  end else
  begin
    txtApp.Text := '';
    txtAppParams.Text := '';
    txtApp.Enabled := False;
    txtAppParams.Enabled := False;
    btnBrowseApp.Enabled := False;
    lblAppParams.Enabled := False;
    btnRemove.Enabled := False;
  end;

  btnConfigure.Enabled := (Item <> nil) and Item.Checked and TPostProcessBase(Item.Data).CanConfigure;
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
  lblFilePattern.Caption := _('%a = artist, %t = title, %l = album, %u = title on stream, %s = streamname,'#13#10'%n = tracknumber, %d = date song was saved, %i = time song was saved'#13#10 +
                              'Backslashes can be used to seperate directories.');

  lblAppParams.Caption := _('%f = filename, %a = artist, %t = title, %l = album, %u = title on stream, %s = streamname, %n = tracknumber, %d = date song was saved, %i = time song was saved (everything should be quoted using ")');

  if lstPostProcess.Selected <> nil then
  begin
    AppGlobals.PostProcessManager.ReInitPlugins;
    lstPostProcessSelectItem(lstPostProcess, lstPostProcess.Selected, True);
  end;

  for i := 0 to lstPostProcess.Items.Count - 1 do
  begin
    // Damit Sprache neu gesetzt wird und so..
    TPostProcessBase(lstPostProcess.Items[i].Data).Initialize;
    lstPostProcess.Items[i].Caption := TPostProcessBase(lstPostProcess.Items[i].Data).Name;
  end;

  BuildHotkeys;

  AppGlobals.PostProcessManager.ReInitPlugins;
  lstDefaultAction.ItemIndex := FDefaultActionIdx;
  lstDefaultActionBrowser.ItemIndex := FDefaultActionBrowserIdx;
  lstDefaultFilter.ItemIndex := FDefaultFilterIdx;

  FormResize(Self);
end;

function TfrmSettings.ValidatePattern(Text, Patterns: string): string;
var
  i: Integer;
  Arr: TPatternReplaceArray;
begin
  inherited;

  SetLength(Arr, Length(Patterns));
  for i := 0 to Length(Patterns) - 1 do
  begin
    Arr[i].C := Patterns[i + 1];
    case Arr[i].C of
      'a':
        Arr[i].Replace := _('Artist');
      't':
        Arr[i].Replace := _('Title');
      'l':
        Arr[i].Replace := _('Album');
      'u':
        Arr[i].Replace := _('Title on stream');
      's':
        Arr[i].Replace := _('Streamname');
      'n':
        Arr[i].Replace := Format('%.*d', [StrToIntDef(txtFilePatternDecimals.Text, 3), 78]);
      'd':
        Arr[i].Replace := FormatDateTime('dd.mm.yy', Now);
      'i':
        Arr[i].Replace := FormatDateTime('hh.nn.ss', Now);
    end;
  end;

  {
  SetLength(Arr, 7);
  Arr[0].C := 'a';
  Arr[0].Replace := _('Artist');
  Arr[1].C := 't';
  Arr[1].Replace := _('Title');
  Arr[2].C := 'l';
  Arr[2].Replace := _('Album');
  Arr[3].C := 's';
  Arr[3].Replace := _('Streamname');
  Arr[4].C := 'n';
  Arr[4].Replace := Format('%.*d', [StrToIntDef(txtFilePatternDecimals.Text, 3), 78]);
  Arr[5].C := 'd';
  Arr[5].Replace := FormatDateTime('dd.mm.yy', Now);
  Arr[6].C := 'i';
  Arr[6].Replace := FormatDateTime('hh.nn.ss', Now);
  }

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
  Result := FixPathName(Result + '.mp3');
end;

procedure TfrmSettings.RegisterPages;
begin
  if FStreamSettings = nil then
  begin
    FPageList.Add(TPage.Create('Settings', pnlMain, 'PROPERTIES'));
    FPageList.Add(TPage.Create('Streams', pnlStreams, 'APPICON'));
    FPageList.Add(TPage.Create('Filenames', pnlFilenames, 'FILENAMES'));
    FPageList.Add(TPage.Create('Advanced', pnlFilenamesExt, 'FILENAMESEXT', FPageList.Find(pnlFilenames)));
    FPageList.Add(TPage.Create('Cut', pnlCut, 'CUT'));
    FPageList.Add(TPage.Create('Plugins', pnlPlugins, 'PLUGINS_PNG'));
    FPageList.Add(TPage.Create('Postprocessing', pnlPostProcess, 'LIGHTNING'));
    FPageList.Add(TPage.Create('Bandwidth', pnlBandwidth, 'BANDWIDTH'));
    FPageList.Add(TPage.Create('Community', pnlCommunity, 'GROUP_PNG'));
    FPageList.Add(TPage.Create('Blacklist', pnlCommunityBlacklist, 'BLACKLIST', FPageList.Find(pnlCommunity)));
    FPageList.Add(TPage.Create('Hotkeys', pnlHotkeys, 'KEYBOARD'));
    FPageList.Add(TPage.Create('Advanced', pnlAdvanced, 'MISC'));
  end else
  begin
    FPageList.Add(TPage.Create('Streams', pnlStreams, 'APPICON'));
    FPageList.Add(TPage.Create('Advanced', pnlStreamsAdvanced, 'MISC', FPageList.Find(pnlStreams)));
    FPageList.Add(TPage.Create('Filenames', pnlFilenames, 'FILENAMES'));
    FPageList.Add(TPage.Create('Advanced', pnlFilenamesExt, 'FILENAMESEXT', FPageList.Find(pnlFilenames)));
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

  end else if TControl(C) is TListView then
  begin
    TListView(C).Color := clWindow;
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

    end else if TControl(FIgnoreFieldList[i]) is TListView then
      TListView(FIgnoreFieldList[i]).Color := clGrayText;
end;

procedure TfrmSettings.SetPage(Page: TPage);
begin
  inherited;

  if Page = FPageList.Find(pnlFilenames) then
    txtPreview.Text := '';
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
    TExternalPostProcess(lstPostProcess.Selected.Data).Params := txtAppParams.Text;
end;

procedure TfrmSettings.txtFilePatternChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(Sender as TLabeledEdit);

    FActivePreviewField := Sender as TLabeledEdit;

    if Sender = txtAutomaticFilePattern then
      txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'atlusdi')
    else if Sender = txtStreamFilePattern then
      txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'sdi')
    else
      txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'atlusndi');

    if Trim(RemoveFileExt(txtPreview.Text)) = '' then
      txtPreview.Text := '';
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
  inherited;
  lstHotkeys.Selected.SubItems[0] := ShortCutToText(txtHotkey.HotKey);
end;

procedure TfrmSettings.txtIgnoreTitlePatternChange(Sender: TObject);
begin
  btnAddIgnoreTitlePattern.Enabled := Length(Trim(txtIgnoreTitlePattern.Text)) >= 1;
end;

procedure TfrmSettings.txtFilePatternEnter(Sender: TObject);
begin
  inherited;

  FActivePreviewField := Sender as TLabeledEdit;

  if Sender = txtAutomaticFilePattern then
    txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'atlusdi')
  else if Sender = txtStreamFilePattern then
    txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'sdi')
  else
    txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'atlusndi');

  if Trim(RemoveFileExt(txtPreview.Text)) = '' then
    txtPreview.Text := '';
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

procedure TfrmSettings.txtStreamFilePatternChange(Sender: TObject);
begin
  inherited;

  if FInitialized then
  begin
    RemoveGray(Sender as TLabeledEdit);

    FActivePreviewField := Sender as TLabeledEdit;
    txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'sdi');

    if Trim(RemoveFileExt(txtPreview.Text)) = '' then
      txtPreview.Text := '';
  end;
end;

procedure TfrmSettings.txtStreamFilePatternClick(Sender: TObject);
begin
  inherited;

  FActivePreviewField := Sender as TLabeledEdit;
  txtPreview.Text := ValidatePattern(FActivePreviewField.Text, 'sdi');

  if Trim(RemoveFileExt(txtPreview.Text)) = '' then
    txtPreview.Text := '';
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

procedure TfrmSettings.btnAddIgnoreTitlePatternClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := lstIgnoreTitles.Items.Add;
  Item.Caption := txtIgnoreTitlePattern.Text;
  Item.ImageIndex := 1;
  txtIgnoreTitlePattern.Text := '';
  txtIgnoreTitlePattern.SetFocus;

  RemoveGray(lstIgnoreTitles);
end;

procedure TfrmSettings.btnAddClick(Sender: TObject);
var
  Item: TListItem;
  Plugin: TExternalPostProcess;
begin
  inherited;
  if dlgOpen.Execute then
  begin
    if FileExists(dlgOpen.FileName) then
    begin
      Item := lstPostProcess.Items.Add;
      Item.Caption := ExtractFileName(dlgOpen.FileName);
      Plugin := TExternalPostProcess.Create(dlgOpen.FileName, '"%f"', True, False, GetNewID, 0);
      FTemporaryPostProcesses.Add(Plugin);
      Item.GroupID := 0; // TODO: !!!
      Item.Checked := Plugin.Active;
      Item.Data := Plugin;
      Item.ImageIndex := 5;
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
      lstPostProcess.Selected.Caption := ExtractFileName(dlgOpen.FileName);
      TExternalPostProcess(lstPostProcess.Selected.Data).Exe := dlgOpen.FileName;
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
    if Sender = btnBrowse then
      txtDir.Text := IncludeTrailingBackslash(Dir)
    else
      txtDirAuto.Text := IncludeTrailingBackslash(Dir)
  else
    MsgBox(Self.Handle, _('The selected folder does not exist. Please choose another one.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TfrmSettings.btnConfigureClick(Sender: TObject);
var
  i: Integer;
  Plugin: TPostProcessBase;
begin
  inherited;

  if lstPostProcess.Selected <> nil then
    TPostProcessBase(lstPostProcess.Selected.Data).Configure(Self, 0, True);

  for i := 0 to lstPostProcess.Items.Count - 1 do
  begin
    Plugin := lstPostProcess.Items[i].Data;
    if Plugin.InheritsFrom(TInternalPostProcess) or (Plugin.ClassType = TInternalPostProcess) then
      TInternalPostProcess(Plugin).LoadSharedSettings;
  end;
end;

procedure TfrmSettings.btnHelpClick(Sender: TObject);
var
  Plugin: TPluginBase;
  PostProcess: TPostProcessBase;
begin
  if Sender = btnHelpPlugin then
  begin
    if lstPlugins.Selected = nil then
      Exit;
    Plugin := lstPlugins.Selected.Data;
    MessageBox(Handle, PChar(Plugin.Help), 'Info', MB_ICONINFORMATION)
  end else
  begin
    if lstPostProcess.Selected = nil then
      Exit;
    PostProcess := lstPostProcess.Selected.Data;
    MessageBox(Handle, PChar(PostProcess.Help), 'Info', MB_ICONINFORMATION);
  end;
end;

procedure TfrmSettings.btnMoveClick(Sender: TObject);
var
  Item: TListItem;

var
  TmpItem: TListItem;
  i, j: integer;
begin
  if lstPostProcess.Selected = nil then
    Exit;

  // TODO: Bewegen von Einträgen funzt nicht.

  i := TPostProcessBase(lstPostProcess.Selected.Data).Order;
  TPostProcessBase(lstPostProcess.Selected.Data).Order := TPostProcessBase(lstPostProcess.Items[lstPostProcess.Selected.Index + 1].Data).Order;
  TPostProcessBase(lstPostProcess.Items[lstPostProcess.Selected.Index + 1].Data).Order := i;

  lstPostProcess.CustomSort(nil, 0);
end;

procedure TfrmSettings.btnRemoveClick(Sender: TObject);
begin
  if lstPostProcess.Selected <> nil then
  begin
    FTemporaryPostProcesses.Remove(TExternalPostProcess(lstPostProcess.Selected.Data));
    TExternalPostProcess(lstPostProcess.Selected.Data).Free;
    lstPostProcess.Selected.Delete;
  end;
end;

procedure TfrmSettings.btnRemoveIgnoreTitlePatternClick(Sender: TObject);
begin
  lstIgnoreTitles.Items.Delete(lstIgnoreTitles.Selected.Index);

  RemoveGray(lstIgnoreTitles);
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
    txtFilePattern.SetFocus;
    RemoveGray(txtFilePattern);
  end else if Sender = btnResetIncompleteFilePattern then
  begin
    txtIncompleteFilePattern.Text := '%s\%a - %t';
    txtIncompleteFilePattern.SetFocus;
    RemoveGray(txtIncompleteFilePattern);
  end else if Sender = btnResetAutomaticFilePattern then
  begin
    txtAutomaticFilePattern.Text := '%s\%a - %t';
    txtAutomaticFilePattern.SetFocus;
    RemoveGray(txtAutomaticFilePattern);
  end else
  begin
    txtStreamFilePattern.Text := '%s';
    txtStreamFilePattern.SetFocus;
  end;
end;

procedure TfrmSettings.btnResetRemoveCharsClick(Sender: TObject);
begin
  inherited;

  txtRemoveChars.Text := '[]{}#$§%~^';
  txtRemoveChars.SetFocus;
  RemoveGray(txtRemoveChars);
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
    lstHotkeys.Items[7].Caption := _('Mute');
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

    Item := lstHotkeys.Items.Add;
    Item.Caption := _('Mute');
    Item.SubItems.Add(ShortCutToText(AppGlobals.ShortcutMute));
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

  if Trim(RemoveFileExt(ValidatePattern(txtFilePattern.Text, 'atlusndi'))) = '' then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of completely recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtFilePattern.Parent)));
    txtFilePattern.SetFocus;
    Exit;
  end;

  if Trim(RemoveFileExt(ValidatePattern(txtIncompleteFilePattern.Text, 'atlusndi'))) = '' then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of incompletely recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtIncompleteFilePattern.Parent)));
    txtIncompleteFilePattern.SetFocus;
    Exit;
  end;

  if Trim(RemoveFileExt(ValidatePattern(txtAutomaticFilePattern.Text, 'atlusdi'))) = '' then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of automatically recorded tracks so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtAutomaticFilePattern.Parent)));
    txtAutomaticFilePattern.SetFocus;
    Exit;
  end;

  if Trim(RemoveFileExt(ValidatePattern(txtStreamFilePattern.Text, 'sdi'))) = '' then
  begin
    MsgBox(Handle, _('Please enter a valid pattern for filenames of stream files so that a preview is shown.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtStreamFilePattern.Parent)));
    txtStreamFilePattern.SetFocus;
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

  if not DirectoryExists(txtDirAuto.Text) then
  begin
    MsgBox(Handle, _('The selected folder for automatically saved songs does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
    SetPage(FPageList.Find(TPanel(txtDirAuto.Parent)));
    btnBrowseAuto.Click;
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
      if chkSearchSilence.Checked and (chkManualSilenceLevel.Checked) then
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
        txtSilenceBufferSeconds.Text := IntToStr(AppGlobals.StreamSettings.SilenceBufferSecondsStart);
    end;

    if Trim(txtSongBuffer.Text) = '' then
    begin
      MsgBox(Handle, _('Please enter the length of the buffer that should be added to every beginning/end of saved titles if no silence could be found.'), _('Info'), MB_ICONINFORMATION);
      SetPage(FPageList.Find(TPanel(txtSongBuffer.Parent)));
      txtSongBuffer.SetFocus;
      Exit;
    end;

    if Length(FStreamSettings) > 0 then
      if StrToIntDef(txtAdjustTrackOffset.Text, -1) = -1 then
      begin
        if chkAdjustTrackOffset.Checked then
        begin
          MsgBox(Handle, _('Please enter the length in seconds for track change detection adjustment.'), _('Info'), MB_ICONINFORMATION);
          SetPage(FPageList.Find(TPanel(txtAdjustTrackOffset.Parent)));
          txtAdjustTrackOffset.SetFocus;
          Exit;
        end else
          txtAdjustTrackOffset.Text := IntToStr(AppGlobals.StreamSettings.AdjustTrackOffsetMS);
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

procedure TfrmSettings.chkAddSavedToStreamIgnoreClick(Sender: TObject);
begin
  inherited;

  if FInitialized then
    RemoveGray(chkAddSavedToStreamIgnore);
end;

procedure TfrmSettings.chkAdjustTrackOffsetClick(Sender: TObject);
begin
  inherited;

  txtAdjustTrackOffset.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;
  optAdjustBackward.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;
  optAdjustForward.Enabled := chkAdjustTrackOffset.State <> cbUnchecked;

  if FInitialized then
    RemoveGray(chkAdjustTrackOffset);
end;

procedure TfrmSettings.chkManualSilenceLevelClick(Sender: TObject);
begin
  inherited;

  txtSilenceLevel.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);
  txtSilenceLength.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);
  Label10.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);
  Label14.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);
  Label12.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);
  Label13.Enabled := (not (chkManualSilenceLevel.State = cbUnchecked)) and (chkSearchSilence.State <> cbUnchecked);

  if FInitialized then
    RemoveGray(chkManualSilenceLevel);
end;

procedure TfrmSettings.chkAutoTuneInClick(Sender: TObject);
begin
  inherited;

  lstMinBitrate.Enabled := chkAutoTuneIn.Checked;
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
  if (lstPostProcess.Selected <> nil) and chkOnlyIfCut.Focused then
    TPostProcessBase(lstPostProcess.Selected.Data).OnlyIfCut := chkOnlyIfCut.Checked;
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

    // REMARK: Auskommentiert. Ich weiß nicht, welchen Sinn die Meldung macht...
    {
    if (not chkSeparateTracks.Checked) then
      TfrmMsgDlg.ShowMsg(Self, _('When saving streams without saving separate tracks, keep in mind to change the pattern ' +
                                 'for names of saved files, because the variables for artist, title and tracknumber ' +
                                 '(%a, %t, %n) will only be filled with default values.'), 2, btOK);
    }
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
