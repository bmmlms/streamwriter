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

{ This unit contains the TabControl used to show saved songs }
unit SavedTab;

interface

uses
  Windows,
  AppData,
  AppMessages,
  AudioFunctions,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  DataManager,
  Dialogs,
  DragDrop,
  DragDropFile,
  DynBASS,
  ExtCtrls,
  FileTagger,
  FileWatcher,
  Forms,
  Functions,
  Generics.Collections,
  Graphics,
  Images,
  ImgList,
  LanguageObjects,
  Logging,
  Math,
  MControls,
  Menus,
  MessageBus,
  Notifications,
  Player,
  PlayerManager,
  SavedTabEditTags,
  SharedControls,
  SharedData,
  StdCtrls,
  SysUtils,
  Tabs,
  Themes,
  VirtualTrees;

type
  TSavedTree = class;

  TSavedNodeData = record
    IsStreamParent: Boolean;
    IsFileParent: Boolean;
    Track: TTrackInfo;
  end;
  PSavedNodeData = ^TSavedNodeData;

  TTrackActions = (taUndefined, taRefresh, taCutSong, taEditTags, taFinalized, taAddToWishlist, taRemoveFromWishlist,
    taAddToIgnorelist, taRemoveFromIgnorelist, taRemove, taRecycle, taDelete, taShowFile, taProperties,
    taImportFiles, taImportFolder);

  TTrackActionEvent = procedure(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray) of object;
  TAddTitleEvent = procedure(Sender: TObject; Title: string; TitleHash: Cardinal) of object;

  { TImportFilesThread }

  TImportFilesThread = class(TThread)
  private
    FDir: string;
    FFiles: TList<TTrackInfo>;
    FKnownFiles: TStringList;
    FFoundAudioFiles: TStringList;
    FProgress: Integer;
    FCurrentFilename: string;

    FOnProgress: TNotifyEvent;

    procedure Sync;
  protected
    procedure Execute; override;
  public
    constructor Create(Files: TStrings; KnownFiles: TStringList); overload;
    constructor Create(Dir: string; KnownFiles: TStringList); overload;
    destructor Destroy; override;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  TSavedTracksPopup = class(TPopupMenu)
  private
    FItemRefresh: TMenuItem;
    FItemPlay: TMenuItem;
    FItemCutSong: TMenuItem;
    FItemEditTags: TMenuItem;
    FItemFinalized: TMenuItem;
    FItemAddToWishlist: TMenuItem;
    FItemRemoveFromWishlist: TMenuItem;
    FItemAddToIgnorelist: TMenuItem;
    FItemRemoveFromIgnorelist: TMenuItem;
    FItemRename: TMenuItem;
    FItemCut: TMenuItem;
    FItemCopy: TMenuItem;
    FItemRemove: TMenuItem;
    FItemRecycle: TMenuItem;
    FItemDelete: TMenuItem;
    FItemShowFile: TMenuItem;
    FItemProperties: TMenuItem;
    FItemImportFiles: TMenuItem;
    FItemImportFolder: TMenuItem;
  protected
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(const Enable: Boolean);

    property ItemRefresh: TMenuItem read FItemRefresh;
    property ItemPlay: TMenuItem read FItemPlay;
    property ItemCutSong: TMenuItem read FItemCutSong;
    property ItemEditTags: TMenuItem read FItemEditTags;
    property ItemFinalized: TMenuItem read FItemFinalized;
    property ItemAddToWishlist: TMenuItem read FItemAddToWishlist;
    property ItemRemoveFromWishlist: TMenuItem read FItemRemoveFromWishlist;
    property ItemAddToIgnorelist: TMenuItem read FItemAddToIgnorelist;
    property ItemRemoveFromIgnorelist: TMenuItem read FItemRemoveFromIgnorelist;
    property ItemRename: TMenuItem read FItemRename;
    property ItemCut: TMenuItem read FItemCut;
    property ItemCopy: TMenuItem read FItemCopy;
    property ItemRemove: TMenuItem read FItemRemove;
    property ItemRecycle: TMenuItem read FItemRecycle;
    property ItemDelete: TMenuItem read FItemDelete;
    property ItemShowFile: TMenuItem read FItemShowFile;
    property ItemProperties: TMenuItem read FItemProperties;
    property ItemImportFiles: TMenuItem read FItemImportFiles;
    property ItemImportFolder: TMenuItem read FItemImportFolder;
  end;

  TSavedToolBar = class(TToolbarForcedHorizontal)
  private
    FRefresh: TToolButton;
    FCutSong: TToolButton;
    FEditTags: TToolButton;
    FFinalized: TToolButton;
    FAddToWishlist: TToolButton;
    FRemoveFromWishlist: TToolButton;
    FAddToIgnorelist: TToolButton;
    FRemoveFromIgnorelist: TToolButton;
    FCut: TToolButton;
    FCopy: TToolButton;
    FRename: TToolButton;
    FRemove: TToolButton;
    FRecycle: TToolButton;
    FDelete: TToolButton;
    FShowFile: TToolButton;
    FProperties: TToolButton;
    FImportFiles: TToolButton;
    FImportFolder: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable, HashesSelected: Boolean);
  end;

  TPlayToolBar = class(TToolbarForcedHorizontal)
  private
    FPrev: TToolButton;
    FPlay: TToolButton;
    FPause: TToolButton;
    FStop: TToolButton;
    FNext: TToolButton;
    FPlayLastSecs: TToolButton;
    FShuffle: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable, Playing, IsFirst, IsLast: Boolean);
  end;

  TSearchBar = class(TPanel)
  private
    FLabel: TLabel;
    FSearch: TEdit;

    FHashFilterSet: Boolean;

    procedure FSetHashFilterSet(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property HashFilterSet: Boolean read FHashFilterSet write FSetHashFilterSet;
  end;

  TImportPanel = class(TPanel)
  private
    LabelFilename: TLabel;
    ProgressBar: TProgressBar;
    Button: TButton;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure SetData(Progress: Integer; CurrentFilename: string);
  end;

  { TSavedTab }

  TSavedTab = class(TMainTabSheet)
  private
    FPositionTimer: TTimer;

    FTopPanel: TPanel;
    FTopLeftPanel: TPanel;
    FTopRightPanel: TPanel;
    FTopRightLeftPanel: TPanel;
    FTopRightRightPanel: TPanel;

    FPosLabel: TLabel;
    FToolBar: TSavedToolBar;
    FPlayToolbar: TPlayToolBar;
    FVolume: TVolumePanel;
    FSeek: TSeekBar;
    FSearchBar: TSearchBar;
    FSavedTree: TSavedTree;

    FImportPanel: TImportPanel;
    FImportThread: TImportFilesThread;

    FOnCut: TTrackEvent;
    FOnRefresh: TNotifyEvent;
    FOnPlayStarted: TNotifyEvent;
    FOnAddTitleToWishlist: TAddTitleEvent;
    FOnRemoveTitleFromWishlist: TAddTitleEvent;
    FOnAddTitleToIgnorelist: TAddTitleEvent;
    FOnRemoveTitleFromIgnorelist: TAddTitleEvent;

    procedure SavedTreeAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
    procedure ToolBarClick(Sender: TObject);
    procedure SearchTextClick(Sender: TObject);
    procedure SearchTextChange(Sender: TObject);
    procedure VolumeTrackbarChange(Sender: TObject);
    function VolumeGetVolumeBeforeMute(Sender: TObject): Integer;
    procedure SeekChange(Sender: TObject);
    procedure PositionTimer(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);

    procedure ImportThreadProgress(Sender: TObject);
    procedure ImportThreadTerminate(Sender: TObject);

    procedure ImportPanelCancelClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure PausePlay;
    procedure PostTranslate;

    procedure UpdateButtons;
    procedure StopThreads;

    procedure ToggleShuffle;

    property Tree: TSavedTree read FSavedTree;

    property OnCut: TTrackEvent read FOnCut write FOnCut;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnPlayStarted: TNotifyEvent read FOnPlayStarted write FOnPlayStarted;
    property OnAddTitleToWishlist: TAddTitleEvent read FOnAddTitleToWishlist write FOnAddTitleToWishlist;
    property OnRemoveTitleFromWishlist: TAddTitleEvent read FOnRemoveTitleFromWishlist write FOnRemoveTitleFromWishlist;
    property OnAddTitleToIgnorelist: TAddTitleEvent read FOnAddTitleToIgnorelist write FOnAddTitleToIgnorelist;
    property OnRemoveTitleFromIgnorelist: TAddTitleEvent read FOnRemoveTitleFromIgnorelist write FOnRemoveTitleFromIgnorelist;
  end;

  { TSavedTree }

  TSavedTree = class(TVirtualStringTree)
  private
    FPlayer: TPlayer;
    FPlayerList: TStringList;
    FPlayerIndex: Integer;
    FDragSource: TDropFileSource;
    FTab: TSavedTab;
    FTrackList: TTrackList;
    FFileWatcher, FFileWatcherAuto: TFileWatcher;
    FStreamNode: PVirtualNode;
    FFileNode: PVirtualNode;
    FPattern: string;
    FPlayNext: Boolean;

    FOnAction: TTrackActionEvent;

    FPopupMenu: TSavedTracksPopup;

    FColImages: TVirtualTreeColumn;
    FColFilename: TVirtualTreeColumn;
    FColSize: TVirtualTreeColumn;
    FColLength: TVirtualTreeColumn;
    FColStream: TVirtualTreeColumn;
    FColSaved: TVirtualTreeColumn;
    FColBitrate: TVirtualTreeColumn;

    FHeaderDragSourcePosition: Cardinal;

    procedure FitColumns;

    function GetNode(Filename: string): PVirtualNode; overload;
    function GetNode(Track: TTrackInfo): PVirtualNode; overload;
    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    function GetSelected: TTrackInfoArray;

    procedure PopupMenuClick(Sender: TObject);

    procedure PlayerEndReached(Sender: TObject);
    procedure PlayerPlay(Sender: TObject);
    procedure PlayerPause(Sender: TObject);
    procedure PlayerStop(Sender: TObject);

    procedure MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);

    procedure CutCopy(Cut: Boolean);

    procedure FileWatcherEvent(Sender: TObject; Action: TFileWatcherEventActions; Path, PathNew: string);
    procedure FileWatcherTerminate(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);
  protected
    procedure CreateHandle; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoDragging(P: TPoint); override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure Change(Node: PVirtualNode); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoEdit; override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoHeaderDragging(Column: TColumnIndex): Boolean; override;
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); override;
    procedure DoNodeDblClick(const HitInfo: THitInfo); override;
    function DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean; override;
    procedure DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PostTranslate;

    function PrevPlayingTrack(ConsiderRnd: Boolean): TTrackInfo;
    function NextPlayingTrack(ConsiderRnd: Boolean; AddToPlayerList: Boolean = True): TTrackInfo;
    procedure AddTrack(Track: TTrackInfo; AddToInternalList: Boolean; IgnorePattern: Boolean = False);
    procedure RemoveTracks(Tracks: TTrackInfoArray);
    procedure UpdateTracks(Tracks: TTrackInfoArray);
    procedure Filter(S: string); overload;
    procedure Filter(S: string; ServerTitleHashes, ServerArtistHashes: TCardinalArray); overload;
    procedure Sort(Node: PVirtualNode; Column: TColumnIndex; Direction: VirtualTrees.TSortDirection; DoInit: Boolean = True); override;
    procedure SetFileWatcher;
    procedure UpdateList;

    property Player: TPlayer read FPlayer;
    property OnAction: TTrackActionEvent read FOnAction write FOnAction;
  end;

const
  STREAMNODETEXT = 'Stream files';
  FILENODETEXT = 'Recorded songs';

implementation

{ TSavedTracksPopup }

constructor TSavedTracksPopup.Create(AOwner: TComponent);
begin
  inherited;

  FItemRefresh := TMenuItem.Create(Self);
  FItemRefresh.Caption := 'Re&fresh';
  FItemRefresh.ImageIndex := TImages.ARROW_REFRESH;
  Items.Add(FItemRefresh);

  Items.AddSeparator;

  {
  FItemPrev := TMenuItem.Create(Self);;
  FItemPrev.Caption := 'Pre&vious';
  FItemPrev.ImageIndex := TImages.PREVIOUS_BLUE;
  Items.Add(FItemPrev);
  }

  FItemPlay := TMenuItem.Create(Self);
  FItemPlay.Caption := '&Play';
  FItemPlay.ImageIndex := TImages.PLAY_BLUE;
  Items.Add(FItemPlay);

  {
  FItemPause := TMenuItem.Create(Self);
  FItemPause.Caption := 'Pa&use';
  FItemPause.ImageIndex := TImages.PAUSE_BLUE;
  Items.Add(FItemPause);

  FItemStop := TMenuItem.Create(Self);
  FItemStop.Caption := 'St&op';
  FItemStop.ImageIndex := TImages.STOP_BLUE;
  Items.Add(FItemStop);

  FItemNext := TMenuItem.Create(Self);
  FItemNext.Caption := '&Next';
  FItemNext.ImageIndex := TImages.NEXT_BLUE;
  Items.Add(FItemNext);

  FItemPlayLastSecs := TMenuItem.Create(Self);
  FItemPlayLastSecs.Caption := 'P&lay end';
  FItemPlayLastSecs.ImageIndex := TImages.PLAY_END_BLUE;
  Items.Add(FItemPlayLastSecs);
  }

  Items.AddSeparator;

  FItemCutSong := TMenuItem.Create(Self);
  FItemCutSong.Caption := '&Cut song';
  FItemCutSong.ImageIndex := TImages.CUT;
  Items.Add(FItemCutSong);

  FItemEditTags := TMenuItem.Create(Self);
  FItemEditTags.Caption := '&Edit tags and data...';
  FItemEditTags.ImageIndex := TImages.TAG_BLUE_EDIT;
  Items.Add(FItemEditTags);

  FItemFinalized := TMenuItem.Create(Self);
  FItemFinalized.Caption := 'Finali&zed';
  FItemFinalized.ImageIndex := TImages.TICK;
  Items.Add(FItemFinalized);

  Items.AddSeparator;

  FItemAddToWishlist := TMenuItem.Create(Self);
  FItemAddToWishlist.Caption := 'Add to &wishlist';
  FItemAddToWishlist.ImageIndex := TImages.SCRIPT_HEART_ADD;
  Items.Add(FItemAddToWishlist);

  FItemRemoveFromWishlist := TMenuItem.Create(Self);
  FItemRemoveFromWishlist.Caption := 'Remo&ve from wishlist';
  FItemRemoveFromWishlist.ImageIndex := TImages.SCRIPT_HEART_DELETE;
  Items.Add(FItemRemoveFromWishlist);

  FItemAddToIgnorelist := TMenuItem.Create(Self);
  FItemAddToIgnorelist.Caption := 'Add to i&gnorelist';
  FItemAddToIgnorelist.ImageIndex := TImages.SCRIPT_DECLINE_ADD;
  Items.Add(FItemAddToIgnorelist);

  FItemRemoveFromIgnorelist := TMenuItem.Create(Self);
  FItemRemoveFromIgnorelist.Caption := 'Remove from ig&norelist';
  FItemRemoveFromIgnorelist.ImageIndex := TImages.SCRIPT_DECLINE_DELETE;
  Items.Add(FItemRemoveFromIgnorelist);

  Items.AddSeparator;

  FItemCut := TMenuItem.Create(Self);
  FItemCut.Caption := 'C&ut';
  FItemCut.ImageIndex := TImages.CUT;
  Items.Add(FItemCut);

  FItemCopy := TMenuItem.Create(Self);
  FItemCopy.Caption := 'C&opy';
  FItemCopy.ImageIndex := TImages.PAGE_WHITE_COPY;
  Items.Add(FItemCopy);

  FItemRename := TMenuItem.Create(Self);
  FItemRename.Caption := 'Ren&ame';
  FItemRename.ImageIndex := TImages.TEXTFIELD_RENAME;
  Items.Add(FItemRename);

  FItemRemove := TMenuItem.Create(Self);
  FItemRemove.Caption := '&Remove from list';
  FItemRemove.ImageIndex := TImages.CROSS;
  Items.Add(FItemRemove);

  FItemRecycle := TMenuItem.Create(Self);
  FItemRecycle.Caption := 'Rec&ycle files';
  FItemRecycle.ImageIndex := TImages.BIN;
  Items.Add(FItemRecycle);

  FItemDelete := TMenuItem.Create(Self);
  FItemDelete.Caption := '&Delete files';
  FItemDelete.ImageIndex := TImages.Delete;
  Items.Add(FItemDelete);

  Items.AddSeparator;

  FItemShowFile := TMenuItem.Create(Self);
  FItemShowFile.Caption := 'Show in e&xplorer...';
  FItemShowFile.ImageIndex := TImages.FOLDER_GO;
  Items.Add(FItemShowFile);

  FItemProperties := TMenuItem.Create(Self);
  FItemProperties.Caption := 'Proper&ties...';
  FItemProperties.ImageIndex := TImages.MUSIC_INFORMATION;
  Items.Add(FItemProperties);

  Items.AddSeparator;

  FItemImportFiles := TMenuItem.Create(Self);
  FItemImportFiles.Caption := '&Import files...';
  FItemImportFiles.ImageIndex := TImages.MUSIC_IN;
  Items.Add(FItemImportFiles);

  FItemImportFolder := TMenuItem.Create(Self);
  FItemImportFolder.Caption := 'I&mport folder...';
  FItemImportFolder.ImageIndex := TImages.FOLDER_IN;
  Items.Add(FItemImportFolder);
end;

procedure TSavedTracksPopup.EnableItems(const Enable: Boolean);
begin
  FItemPlay.Enabled := Enable;
  FItemCutSong.Enabled := Enable;
  FItemEditTags.Enabled := Enable;
  FItemFinalized.Enabled := Enable;
  FItemAddToWishlist.Enabled := Enable;
  FItemRemoveFromWishlist.Enabled := Enable;
  FItemAddToIgnorelist.Enabled := Enable;
  FItemRemoveFromIgnorelist.Enabled := Enable;
  FItemCut.Enabled := Enable;
  FItemCopy.Enabled := Enable;
  FItemRename.Enabled := Enable;
  FItemRemove.Enabled := Enable;
  FItemRecycle.Enabled := Enable;
  FItemDelete.Enabled := Enable;
  ItemShowFile.Enabled := Enable;
  FItemProperties.Enabled := Enable;
end;

{ TSavedToolBar }

constructor TSavedToolBar.Create(AOwner: TComponent);
var
  Sep: TToolButton;
begin
  inherited;

  FRefresh := TToolButton.Create(Self);
  FRefresh.Parent := Self;
  FRefresh.Hint := 'Refresh';
  FRefresh.ImageIndex := TImages.ARROW_REFRESH;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FCutSong := TToolButton.Create(Self);
  FCutSong.Parent := Self;
  FCutSong.Hint := 'Cut song';
  FCutSong.ImageIndex := TImages.CUT;

  FEditTags := TToolButton.Create(Self);
  FEditTags.Parent := Self;
  FEditTags.Hint := 'Edit tags and data...';
  FEditTags.ImageIndex := TImages.TAG_BLUE_EDIT;

  FFinalized := TToolButton.Create(Self);
  FFinalized.Parent := Self;
  FFinalized.Hint := 'Finalized';
  FFinalized.ImageIndex := TImages.TICK;
  FFinalized.Style := tbsCheck;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FAddToWishlist := TToolButton.Create(Self);
  FAddToWishlist.Parent := Self;
  FAddToWishlist.Hint := 'Add to wishlist';
  FAddToWishlist.ImageIndex := TImages.SCRIPT_HEART;

  FRemoveFromWishlist := TToolButton.Create(Self);
  FRemoveFromWishlist.Parent := Self;
  FRemoveFromWishlist.Hint := 'Remove from wishlist';
  FRemoveFromWishlist.ImageIndex := TImages.SCRIPT_HEART_DELETE;

  FAddToIgnoreList := TToolButton.Create(Self);
  FAddToIgnoreList.Parent := Self;
  FAddToIgnoreList.Hint := 'Add to ignorelist';
  FAddToIgnoreList.ImageIndex := TImages.SCRIPT_DECLINE;

  FRemoveFromIgnorelist := TToolButton.Create(Self);
  FRemoveFromIgnorelist.Parent := Self;
  FRemoveFromIgnorelist.Hint := 'Remove from ignorelist';
  FRemoveFromIgnorelist.ImageIndex := TImages.SCRIPT_DECLINE_DELETE;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FCut := TToolButton.Create(Self);
  FCut.Parent := Self;
  FCut.Hint := 'Cut';
  FCut.ImageIndex := TImages.CUT;

  FCopy := TToolButton.Create(Self);
  FCopy.Parent := Self;
  FCopy.Hint := 'Copy';
  FCopy.ImageIndex := TImages.PAGE_WHITE_COPY;

  FRename := TToolButton.Create(Self);
  FRename.Parent := Self;
  FRename.Hint := 'Rename';
  FRename.ImageIndex := TImages.TEXTFIELD_RENAME;

  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := 'Remove from list';
  FRemove.ImageIndex := TImages.CROSS;

  FRecycle := TToolButton.Create(Self);
  FRecycle.Parent := Self;
  FRecycle.Hint := 'Recycle files';
  FRecycle.ImageIndex := TImages.BIN;

  FDelete := TToolButton.Create(Self);
  FDelete.Parent := Self;
  FDelete.Hint := 'Delete files';
  FDelete.ImageIndex := TImages.Delete;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FShowFile := TToolButton.Create(Self);
  FShowFile.Parent := Self;
  FShowFile.Hint := _('Show in explorer...');
  FShowFile.ImageIndex := TImages.FOLDER_GO;

  FProperties := TToolButton.Create(Self);
  FProperties.Parent := Self;
  FProperties.Hint := _('Properties...');
  FProperties.ImageIndex := TImages.MUSIC_INFORMATION;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FImportFiles := TToolButton.Create(Self);
  FImportFiles.Parent := Self;
  FImportFiles.Hint := _('Import files...');
  FImportFiles.ImageIndex := TImages.MUSIC_IN;

  FImportFolder := TToolButton.Create(Self);
  FImportFolder.Parent := Self;
  FImportFolder.Hint := _('Import folder...');
  FImportFolder.ImageIndex := TImages.FOLDER_IN;
end;

procedure TSavedToolBar.EnableItems(Enable, HashesSelected: Boolean);
begin
  FCutSong.Enabled := Enable;
  FEditTags.Enabled := Enable;
  FFinalized.Enabled := Enable;
  FAddToWishlist.Enabled := Enable;
  FRemoveFromWishlist.Enabled := Enable;
  FAddToIgnorelist.Enabled := Enable;
  FRemoveFromIgnorelist.Enabled := Enable;
  FCut.Enabled := Enable;
  FCopy.Enabled := Enable;
  FRemove.Enabled := Enable;
  FRecycle.Enabled := Enable;
  FDelete.Enabled := Enable;
  FShowFile.Enabled := Enable;
  FProperties.Enabled := Enable;
end;

{ TSavedTab }

constructor TSavedTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MsgBus.AddSubscriber(MessageReceived);

  ShowCloseButton := False;
  ImageIndex := TImages.DRIVE;

  FPositionTimer := TTimer.Create(Self);
  FPositionTimer.Interval := 50;
  FPositionTimer.OnTimer := PositionTimer;

  FSavedTree := TSavedTree.Create(Self);
  FSavedTree.Parent := Self;
  FSavedTree.Align := alClient;
  FSavedTree.OnAction := SavedTreeAction;

  Caption := 'Saved songs';

  FSavedTree.Images := modSharedData.imgImages;
  FSavedTree.StateImages := modSharedData.imgImages;
  if Screen.PixelsPerInch = 96 then
    FSavedTree.FPopupMenu.Images := modSharedData.imgImages;

  // Panel oben komplett
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.AutoSize := True;

  // Panel links
  FTopLeftPanel := TPanel.Create(Self);
  FTopLeftPanel.Parent := FTopPanel;
  FTopLeftPanel.Align := alLeft;
  FTopLeftPanel.Width := MulDiv(460, Screen.PixelsPerInch, 96);
  FTopLeftPanel.BevelOuter := bvNone;
  FTopLeftPanel.AutoSize := True;

  // Panel rechts
  FTopRightPanel := TPanel.Create(Self);
  FTopRightPanel.Parent := FTopPanel;
  FTopRightPanel.Align := alClient;
  FTopRightPanel.BevelOuter := bvNone;
  FTopRightPanel.AutoSize := True;

  FTopRightLeftPanel := TPanel.Create(Self);
  FTopRightLeftPanel.Parent := FTopRightPanel;
  FTopRightLeftPanel.Align := alRight;
  FTopRightLeftPanel.BevelOuter := bvNone;
  FTopRightLeftPanel.AutoSize := True;

  FTopRightRightPanel := TPanel.Create(Self);
  FTopRightRightPanel.Parent := FTopRightPanel;
  FTopRightRightPanel.Align := alRight;
  FTopRightRightPanel.BevelOuter := bvNone;
  FTopRightPanel.AutoSize := True;

  FPlayToolbar := TPlayToolBar.Create(Self);
  FPlayToolbar.Parent := FTopRightLeftPanel;
  FPlayToolbar.Align := alBottom;
  FPlayToolbar.Images := modSharedData.imgImages;

  FPosLabel := TLabel.Create(Self);
  FPosLabel.AutoSize := True;
  FPosLabel.Alignment := taRightJustify;
  FPosLabel.Layout := tlCenter;
  FPosLabel.Caption := '00:00';
  FPosLabel.Parent := FTopRightRightPanel;
  FPosLabel.Align := alClient;

  FSearchBar := TSearchBar.Create(Self);
  FSearchBar.Parent := FTopLeftPanel;
  FSearchBar.Align := alClient;
  FSearchBar.AutoSize := True;
  FSearchBar.FSearch.OnClick := SearchTextClick;
  FSearchBar.FSearch.OnChange := SearchTextChange;

  FToolBar := TSavedToolBar.Create(Self);
  FToolBar.Parent := FTopLeftPanel;
  FToolBar.Align := alTop;
  FToolBar.Images := modSharedData.imgImages;
  FToolBar.FRefresh.OnClick := ToolBarClick;
  FToolBar.FCutSong.OnClick := ToolBarClick;
  FToolBar.FEditTags.OnClick := ToolBarClick;
  FToolBar.FFinalized.OnClick := ToolBarClick;
  FToolBar.FAddToWishlist.OnClick := ToolBarClick;
  FToolBar.FRemoveFromWishlist.OnClick := ToolBarClick;
  FToolBar.FAddToIgnorelist.OnClick := ToolBarClick;
  FToolBar.FRemoveFromIgnorelist.OnClick := ToolBarClick;
  FToolBar.FCut.OnClick := ToolBarClick;
  FToolBar.FCopy.OnClick := ToolBarClick;
  FToolBar.FRename.OnClick := ToolBarClick;
  FToolBar.FRemove.OnClick := ToolBarClick;
  FToolBar.FRecycle.OnClick := ToolBarClick;
  FToolBar.FDelete.OnClick := ToolBarClick;
  FToolBar.FShowFile.OnClick := ToolBarClick;
  FToolBar.FProperties.OnClick := ToolBarClick;
  FToolBar.FImportFiles.OnClick := ToolBarClick;
  FToolBar.FImportFolder.OnClick := ToolBarClick;

  FSeek := TSeekBar.Create(Self);
  FSeek.Parent := FTopRightLeftPanel;
  FSeek.Align := alTop;
  FSeek.OnPositionChanged := SeekChange;

  FVolume := TVolumePanel.Create(Self);
  FVolume.Parent := FTopRightRightPanel;
  FVolume.Align := alTop;
  FVolume.Enabled := Bass.DeviceAvailable;
  FVolume.Volume := Players.Volume;
  FVolume.OnVolumeChange := VolumeTrackbarChange;
  FVolume.OnGetVolumeBeforeMute := VolumeGetVolumeBeforeMute;

  FPlayToolBar.FPrev.OnClick := FSavedTree.PopupMenuClick;
  FPlayToolBar.FPlay.OnClick := ToolBarClick;
  FPlayToolBar.FPause.OnClick := FSavedTree.PopupMenuClick;
  FPlayToolBar.FStop.OnClick := FSavedTree.PopupMenuClick;
  FPlayToolBar.FNext.OnClick := FSavedTree.PopupMenuClick;
  FPlayToolBar.FPlayLastSecs.OnClick := FSavedTree.PopupMenuClick;
  FPlayToolbar.FShuffle.OnClick := ToolBarClick;

  FImportPanel := TImportPanel.Create(Self);
  FImportPanel.Width := 250;
  FImportPanel.Height := 80;
  FImportPanel.Visible := False;
  FImportPanel.Parent := Self;
  FImportPanel.AnchorVerticalCenterTo(Self);
  FImportPanel.AnchorHorizontalCenterTo(Self);
  FImportPanel.Button.OnClick := ImportPanelCancelClick;
end;

destructor TSavedTab.Destroy;
begin
  MsgBus.RemoveSubscriber(MessageReceived);
  FPositionTimer.Enabled := False;

  inherited;
end;

procedure TSavedTab.ImportPanelCancelClick(Sender: TObject);
begin
  if FImportThread <> nil then
    FImportThread.Terminate;
end;

procedure TSavedTab.ImportThreadProgress(Sender: TObject);
begin
  FImportPanel.SetData(FImportThread.FProgress, FImportThread.FCurrentFilename);
end;

procedure TSavedTab.ImportThreadTerminate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FImportThread.FFiles.Count - 1 do
  begin
    AppGlobals.Data.TrackList.Add(FImportThread.FFiles[i]);
    FSavedTree.AddTrack(FImportThread.FFiles[i], True);
  end;
  FImportThread := nil;

  FImportPanel.Visible := False;
  FSavedTree.Enabled := True;
  FSearchBar.FSearch.Enabled := True;

  UpdateButtons;
end;

procedure TSavedTab.MessageReceived(Msg: TMessageBase);
var
  VolMsg: TVolumeChangedMsg absolute Msg;
  SelectSavedSongsMsg: TSelectSavedSongsMsg absolute Msg;
  Tmp: TNotifyEvent;
begin
  if (Msg is TVolumeChangedMsg) and (FVolume <> nil) then
  begin
    if VolMsg.Volume <> FVolume.Volume then
      FVolume.Volume := TVolumeChangedMsg(Msg).Volume;
  end else if Msg is TSelectSavedSongsMsg then
  begin
    FSearchBar.HashFilterSet := True;

    Tmp := FSearchBar.FSearch.OnChange;
    FSearchBar.FSearch.OnChange := nil;
    FSearchBar.FSearch.Text := _('[Selected titles]');
    FSearchBar.FSearch.OnChange := Tmp;
    FSavedTree.Filter('', SelectSavedSongsMsg.TitleHashes, SelectSavedSongsMsg.ArtistHashes);
  end;
end;

procedure TSavedTab.PausePlay;
begin
  if Tree.Player.Playing then
    Tree.Player.Pause;
  UpdateButtons;
end;

procedure TSavedTab.PositionTimer(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;

  FPosLabel.Visible := FSavedTree.Player.Playing or FSavedTree.Player.Paused;
  FSeek.GripperVisible := FSavedTree.Player.Playing or FSavedTree.Player.Paused;

  if FSavedTree.Player.Playing or FSavedTree.Player.Paused then
  begin
    FSeek.Position := Tree.Player.PositionByte;
    FPosLabel.Caption := BuildTime(Tree.Player.PositionTime, False);
  end else
    FPosLabel.Caption := '00:00';
end;

procedure TSavedTab.PostTranslate;
begin
  FSavedTree.PostTranslate;
end;

procedure TSavedTab.SavedTreeAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
var
  i, n: Integer;
  Error, AllFinalized: Boolean;
  LowerDir, Dir: string;
  EditTags: TfrmEditTags;
  KnownFiles: TStringList;
  Dlg: TOpenDialog;
  Tracks2: TTrackInfoArray;
begin
  case Action of
    taRefresh:
      if Assigned(FOnRefresh) then
        FOnRefresh(Self);
    taCutSong:
      if Assigned(FOnCut) then
        for i := 0 to Length(Tracks) - 1 do
          FOnCut(nil, Tracks[i]);
    taEditTags:
    begin
      EditTags := TfrmEditTags.Create(GetParentForm(Self));
      try
        EditTags.ShowModal(Tracks);

        // Es könnte sein, dass inzwischen Files gelöscht wurden oder so. Also nochmal Nodes
        // holen und Änderungen aus Formular anwenden.
        Tracks2 := FSavedTree.GetSelected;
        for i := 0 to High(EditTags.Tracks) do
          for n := 0 to High(Tracks2) do
            if EditTags.Tracks[i].Filename = Tracks2[n].Filename then
              Tracks2[n].Streamname := EditTags.Tracks[i].Streamname;
      finally
        EditTags.Free;
      end;
    end;
    taFinalized:
    begin
      AllFinalized := True;
      for i := 0 to Length(Tracks) - 1 do
        if not Tracks[i].Finalized then
        begin
          AllFinalized := False;
          Break;
        end;
      for i := 0 to Length(Tracks) - 1 do
        Tracks[i].Finalized := not AllFinalized;
    end;
    taAddToWishlist:
      for i := 0 to Length(Tracks) - 1 do
        FOnAddTitleToWishlist(Self, Tracks[i].ParsedTitle, Tracks[i].ServerTitleHash);
    taRemoveFromWishlist:
      for i := 0 to Length(Tracks) - 1 do
        FOnRemoveTitleFromWishlist(Self, Tracks[i].ParsedTitle, Tracks[i].ServerTitleHash);
    taAddToIgnorelist:
      for i := 0 to Length(Tracks) - 1 do
        FOnAddTitleToIgnorelist(Self, Tracks[i].ParsedTitle, 0);
    taRemoveFromIgnorelist:
      for i := 0 to Length(Tracks) - 1 do
        FOnRemoveTitleFromIgnorelist(Self, Tracks[i].ParsedTitle, 0);
    taRemove:
      FSavedTree.RemoveTracks(Tracks);
    taRecycle:
      for i := 0 to Length(Tracks) - 1 do
      begin
        MsgBus.SendMessage(TFileModifyMsg.Create(Tracks[i].Filename));

        if TFunctions.Recycle(Handle, Tracks[i].Filename) then
        begin
          LowerDir := LowerCase(ExtractFileDir(Tracks[i].Filename));
          if (LowerDir <> LowerCase(ExcludeTrailingPathDelimiter(AppGlobals.Dir))) and (LowerDir <> LowerCase(ExcludeTrailingPathDelimiter(AppGlobals.DirAuto))) then
            RemoveDir(ExtractFileDir(Tracks[i].Filename));

          FSavedTree.RemoveTracks([Tracks[i]]);
        end;
      end;
    taDelete:
    begin
      if Length(Tracks) = 1 then
      begin
        if TFunctions.MsgBox(Format(_('Do you really want to delete "%s"?'), [ExtractFileName(Tracks[0].Filename)]), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
          Exit;
      end else if TFunctions.MsgBox(_('Do you really want to delete all selected files?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
        Exit;

      Error := False;
      FSavedTree.BeginUpdate;
      try
        for i := 0 to Length(Tracks) - 1 do
        begin
          MsgBus.SendMessage(TFileModifyMsg.Create(Tracks[i].Filename));

          if FileExists(Tracks[i].Filename) then
            Error := not DeleteFile(Tracks[i].Filename);

          if not Error then
          begin
            LowerDir := LowerCase(ExtractFileDir(Tracks[i].Filename));
            if not ((LowerDir = LowerCase(ExcludeTrailingPathDelimiter(AppGlobals.Dir))) and (LowerDir = LowerCase(ExcludeTrailingPathDelimiter(AppGlobals.DirAuto)))) then
              RemoveDir(ExtractFileDir(Tracks[i].Filename));

            FSavedTree.RemoveTracks([Tracks[i]]);
          end;
        end;
      finally
        FSavedTree.EndUpdate;
      end;
      if Error then
        TFunctions.MsgBox(_('Some files could not be deleted.'#13#10'Please make sure they are not opened in a cut-tab or in use by another application.'), _('Info'), MB_ICONINFORMATION);
    end;
    taShowFile:
      TFunctions.RunProcess('explorer.exe /select,"' + Tracks[0].Filename + '"');
    taProperties:
      TFunctions.PropertiesDialog(Tracks[0].Filename);
    taImportFiles:
    begin
      Dlg := TOpenDialog.Create(GetParentForm(Self));
      try
        Dlg.Options := [ofAllowMultiSelect, ofFileMustExist];
        Dlg.Filter := _('Audio files|*.mp3;*.ogg;*.aac;*.m4a');
        if Dlg.Execute and (Dlg.Files.Count > 0) then
        begin
          for i := Dlg.Files.Count - 1 downto 0 do
            if FilenameToFormat(Dlg.Files[i]) = atNone then
              Dlg.Files.Delete(i);
          if Dlg.Files.Count > 0 then
          begin
            KnownFiles := TStringList.Create;
            for i := 0 to AppGlobals.Data.TrackList.Count - 1 do
              KnownFiles.Add(AppGlobals.Data.TrackList[i].Filename);

            FImportThread := TImportFilesThread.Create(Dlg.Files, KnownFiles);
            FImportThread.OnTerminate := ImportThreadTerminate;
            FImportThread.OnProgress := ImportThreadProgress;
            FImportThread.Start;

            if FSavedTree.Player.Playing then
              FSavedTree.FPlayer.Pause;

            FSavedTree.Enabled := False;
            FSearchBar.FSearch.Enabled := False;
            FImportPanel.Show;

            UpdateButtons;
          end;
        end;
      finally
        Dlg.Free;
      end;
    end;
    taImportFolder:
    begin
      Dir := TFunctions.BrowseDialog(GetParentForm(Self), _('Select folder with files to import'));
      if Dir <> '' then
      begin
        KnownFiles := TStringList.Create;
        for i := 0 to AppGlobals.Data.TrackList.Count - 1 do
          KnownFiles.Add(AppGlobals.Data.TrackList[i].Filename);

        FImportThread := TImportFilesThread.Create(Dir, KnownFiles);
        FImportThread.OnTerminate := ImportThreadTerminate;
        FImportThread.OnProgress := ImportThreadProgress;
        FImportThread.Start;

        if FSavedTree.Player.Playing then
          FSavedTree.FPlayer.Pause;

        FSavedTree.Enabled := False;
        FSearchBar.FSearch.Enabled := False;
        FImportPanel.Show;

        UpdateButtons;
      end;
    end;
  end;

  FSavedTree.Change(nil);
end;

procedure TSavedTab.ToggleShuffle;
begin
  FPlayToolbar.FShuffle.Down := not FPlayToolbar.FShuffle.Down;
  FPlayToolbar.FShuffle.Click;
  UpdateButtons;
end;

procedure TSavedTab.ToolBarClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  if Sender = FToolbar.FRefresh then
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);

  {
  if Sender = FPlayToolbar.FPlayLastSecs then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPlayLastSecs);
  if Sender = FPlayToolbar.FPrev then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPrev);
  }
  if Sender = FPlayToolbar.FPlay then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPlay);
  if Sender = FPlayToolbar.FShuffle then
  begin
    if not FPlayToolbar.FShuffle.Down then
    begin
      FSavedTree.FPlayerList.Clear;
      FSavedTree.FPlayerIndex := 0;
    end else if FSavedTree.Player.Playing then
    begin
      Node := FSavedTree.GetNode(FSavedTree.Player.Filename);
      if Node <> nil then
      begin
        NodeData := FSavedTree.GetNodeData(Node);
        FSavedTree.FPlayerList.Add(NodeData.Track.Filename);
        FSavedTree.FPlayerIndex := FSavedTree.FPlayerList.Count - 1;
      end;
    end;

    AppGlobals.PlayerShuffle := FPlayToolbar.FShuffle.Down;
    UpdateButtons;
  end;
  {
  if Sender = FPlayToolbar.FPause then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPause);
  if Sender = FPlayToolbar.FStop then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemStop);
  if Sender = FPlayToolbar.FNext then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemNext);
  }
  if Sender = FToolbar.FCutSong then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemCutSong);
  if Sender = FToolbar.FEditTags then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemEditTags);
  if Sender = FToolbar.FFinalized then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemFinalized);
  if Sender = FToolbar.FAddToWishlist then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemAddToWishlist);
  if Sender = FToolbar.FRemoveFromWishlist then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemRemoveFromWishlist);
  if Sender = FToolbar.FAddToIgnorelist then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemAddToIgnorelist);
  if Sender = FToolbar.FRemoveFromIgnorelist then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemRemoveFromIgnorelist);
  if Sender = FToolbar.FCut then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemCut);
  if Sender = FToolbar.FCopy then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemCopy);
  if Sender = FToolbar.FRename then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemRename);
  if Sender = FToolbar.FRemove then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemRemove);
  if Sender = FToolbar.FRecycle then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemRecycle);
  if Sender = FToolbar.FDelete then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemDelete);
  if Sender = FToolbar.FShowFile then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemShowFile);
  if Sender = FToolbar.FProperties then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemProperties);
  if Sender = FToolbar.FImportFiles then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemImportFiles);
  if Sender = FToolbar.FImportFolder then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemImportFolder);
end;

procedure TSavedTab.UpdateButtons;
var
  i: Integer;
  AllFinalized, IsFirst, IsLast, HashesSelected: Boolean;
  Tracks: TTrackInfoArray;
begin
  inherited;

  if FImportThread <> nil then
  begin
    for i := 0 to FToolbar.ButtonCount - 1 do
      FToolbar.Buttons[i].Enabled := False;
    for i := 0 to Tree.FPopupMenu.Items.Count - 1 do
      Tree.FPopupMenu.Items[i].Enabled := False;

    FPlayToolbar.EnableItems(False, False, True, True);
    FSavedTree.FPopupMenu.EnableItems(False);

    Exit;
  end;

  Tracks := Tree.GetSelected;

  if Tree.Player.Playing or Tree.Player.Paused then
  begin
    IsFirst := FSavedTree.PrevPlayingTrack(False) = nil;
    IsLast := (FSavedTree.NextPlayingTrack(False) = nil);
    if IsLast and (FSavedTree.FFileNode.ChildCount > 1) then
      IsLast := not FPlayToolbar.FShuffle.Down;
    if IsFirst and (FSavedTree.FFileNode.ChildCount > 1) then
      IsFirst := not FPlayToolbar.FShuffle.Down;
  end else
  begin
    IsFirst := True;
    IsLast := True;
  end;

  HashesSelected := False;
  for i := 0 to High(Tracks) do
    if Tracks[i].ServerTitleHash > 0 then
    begin
      HashesSelected := True;
      Break;
    end;

  FPlayToolbar.FShuffle.Down := AppGlobals.PlayerShuffle;
  FPlayToolbar.FShuffle.Enabled := Bass.DeviceAvailable;

  FPlayToolbar.FPause.Down := Tree.Player.Paused;

  Tree.FPopupMenu.EnableItems(Length(Tracks) > 0);
  FToolbar.EnableItems(Length(Tracks) > 0, HashesSelected);
  FPlayToolbar.EnableItems(Length(Tracks) > 0, Tree.FPlayer.Playing or Tree.FPlayer.Paused, IsFirst, IsLast);

  //Tree.FPopupMenu.ItemPlayLastSecs.Enabled := Bass.DeviceAvailable and ((Length(Tracks) = 1) or Tree.FPlayer.Playing or Tree.Player.Paused);
  FPlayToolbar.FPlayLastSecs.Enabled := Bass.DeviceAvailable and ((Length(Tracks) = 1) or Tree.FPlayer.Playing or Tree.Player.Paused);

  Tree.FPopupMenu.ItemPlay.Enabled := Bass.DeviceAvailable and (Length(Tracks) = 1);
  FPlayToolbar.FPlay.Enabled := Bass.DeviceAvailable and (Length(Tracks) = 1);

  Tree.FPopupMenu.ItemShowFile.Enabled := Length(Tracks) = 1;
  FToolbar.FShowFile.Enabled := Length(Tracks) = 1;
  Tree.FPopupMenu.ItemProperties.Enabled := Length(Tracks) = 1;
  FToolbar.FProperties.Enabled := Length(Tracks) = 1;

  Tree.FPopupMenu.ItemCutSong.Enabled := Length(Tracks) > 0;
  FToolbar.FCutSong.Enabled := Length(Tracks) > 0;

  Tree.FPopupMenu.ItemEditTags.Enabled := Length(Tracks) > 0;
  FToolbar.FEditTags.Enabled := Length(Tracks) > 0;

  Tree.FPopupMenu.ItemRename.Enabled := Length(Tracks) = 1;
  FToolbar.FRename.Enabled := Length(Tracks) = 1;

  Tree.FPopupMenu.ItemImportFiles.Enabled := True;
  FToolbar.FImportFiles.Enabled := True;

  Tree.FPopupMenu.ItemImportFolder.Enabled := True;
  FToolbar.FImportFolder.Enabled := True;

  AllFinalized := True;
  for i := 0 to High(Tracks) do
    if not Tracks[i].Finalized then
    begin
      AllFinalized := False;
      Break;
    end;

  FSavedTree.FPopupMenu.FItemFinalized.Checked := AllFinalized;
  FToolbar.FFinalized.Down := AllFinalized;

  FPlayToolbar.FPause.Enabled := FSavedTree.Player.Playing or FSavedTree.Player.Paused;
  FPlayToolbar.FStop.Enabled := FSavedTree.Player.Playing or FSavedTree.Player.Paused;

  FSeek.GripperVisible := FSavedTree.Player.Playing or FSavedTree.Player.Paused;
  FSavedTree.Invalidate;
end;

function TSavedTab.VolumeGetVolumeBeforeMute(Sender: TObject): Integer;
begin
  Result := Players.VolumeBeforeMute;
end;

procedure TSavedTab.VolumeTrackbarChange(Sender: TObject);
begin
  Players.Volume := FVolume.Volume;
  if FVolume.VolumeBeforeDrag > -1 then
    Players.VolumeBeforeMute := FVolume.VolumeBeforeDrag;
end;

procedure TSavedTab.SearchTextChange(Sender: TObject);
begin
  FSearchBar.HashFilterSet := False;
  FSavedTree.Filter(FSearchBar.FSearch.Text);
end;

procedure TSavedTab.SearchTextClick(Sender: TObject);
begin
  if FSearchBar.HashFilterSet then
    FSearchBar.FSearch.SelectAll;
end;

procedure TSavedTab.SeekChange(Sender: TObject);
begin
  FSavedTree.FPlayer.PositionByte := FSeek.Position;
  PositionTimer(FPositionTimer);
  UpdateButtons;
end;

procedure TSavedTab.StopThreads;
begin
  while FImportThread <> nil do
  begin
    FImportThread.Terminate;
    Application.ProcessMessages;
    Sleep(100);
  end;
end;

{ TSavedTree }

constructor TSavedTree.Create(AOwner: TComponent);
var
  i: Integer;
  NodeData: PSavedNodeData;
begin
  inherited Create(AOwner);

  FPlayerList := TStringList.Create;

  FPattern := '*';

  FPlayer := TPlayer.Create;
  FPlayer.OnEndReached := PlayerEndReached;
  FPlayer.OnPlay := PlayerPlay;
  FPlayer.OnPause := PlayerPause;
  FPlayer.OnStop := PlayerStop;
  Players.AddPlayer(FPlayer);

  FTrackList := TTrackList.Create;

  FTab := TSavedTab(AOwner);

  NodeDataSize := SizeOf(TSavedNodeData);
  IncrementalSearch := isVisibleOnly;
  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  Header.Options := [hoColumnResize, hoDrag, hoAutoResize, hoHotTrack, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.AutoSizeIndex := 1;
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FDragSource := TDropFileSource.Create(Self);
  FDragSource.DragTypes := [dtCopy, dtMove];

  FPopupMenu := TSavedTracksPopup.Create(Self);
  FPopupMenu.ItemRefresh.OnClick := PopupMenuClick;
  FPopupMenu.ItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.ItemCutSong.OnClick := PopupMenuClick;
  FPopupMenu.ItemEditTags.OnClick := PopupMenuClick;
  FPopupMenu.ItemFinalized.OnClick := PopupMenuClick;
  FPopupMenu.ItemAddToWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemoveFromWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemAddToIgnorelist.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemoveFromIgnorelist.OnClick := PopupMenuClick;
  FPopupMenu.ItemCut.OnClick := PopupMenuClick;
  FPopupMenu.ItemCopy.OnClick := PopupMenuClick;
  FPopupMenu.ItemRename.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemove.OnClick := PopupMenuClick;
  FPopupMenu.ItemRecycle.OnClick := PopupMenuClick;
  FPopupMenu.ItemDelete.OnClick := PopupMenuClick;
  FPopupMenu.ItemShowFile.OnClick := PopupMenuClick;
  FPopupMenu.ItemProperties.OnClick := PopupMenuClick;
  FPopupMenu.ItemImportFiles.OnClick := PopupMenuClick;
  FPopupMenu.ItemImportFolder.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;

  Header.Options := Header.Options + [hoAutoResize];

  FStreamNode := AddChild(nil);
  NodeData := GetNodeData(FStreamNode);
  NodeData.IsStreamParent := True;

  FFileNode := AddChild(nil);
  NodeData := GetNodeData(FFileNode);
  NodeData.IsFileParent := True;

  SetFileWatcher;

  MsgBus.AddSubscriber(MessageReceived);

  FColImages := Header.Columns.Add;
  FColImages.Text := _('State');
  FColImages.Options := FColImages.Options - [coResizable];
  FColFilename := Header.Columns.Add;
  FColFilename.Text := _('Filename');
  FColSize := Header.Columns.Add;
  FColSize.Text := _('Size');
  FColLength := Header.Columns.Add;
  FColLength.Alignment := taRightJustify;
  FColLength.Text := _('Length');
  FColBitrate := Header.Columns.Add;
  FColBitrate.Alignment := taRightJustify;
  FColBitrate.Text := _('Bitrate');
  FColStream := Header.Columns.Add;
  FColStream.Text := _('Stream');
  FColSaved := Header.Columns.Add;
  FColSaved.Alignment := taRightJustify;
  FColSaved.Text := _('Date');

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);
  TMTreeColumnPopup(Header.PopupMenu).HideIdx := 1;
  TMTreeColumnPopup(Header.PopupMenu).OnAction := MenuColsAction;

  for i := 1 to Header.Columns.Count - 1 do
    if not ((AppGlobals.SavedCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];

  FitColumns;
end;

procedure TSavedTree.CutCopy(Cut: Boolean);
var
  i: Integer;
  Tracks: TTrackInfoArray;
begin
  FDragSource.Files.Clear;
  Tracks := GetSelected;
  for i := 0 to Length(Tracks) - 1 do
    FDragSource.Files.Add(Tracks[i].Filename);

  if FDragSource.Files.Count > 0 then
    if Cut then
      FDragSource.CutToClipboard
    else
      FDragSource.CopyToClipboard;
end;

destructor TSavedTree.Destroy;
begin
  MsgBus.RemoveSubscriber(MessageReceived);

  FPlayerList.Free;
  FPlayer.Free;
  FTrackList.Free;
  FDragSource.Free;

  if FFileWatcher <> nil then
  begin
    FFileWatcher.OnTerminate := nil;
    FFileWatcher.Terminate;
  end;
  if FFileWatcherAuto <> nil then
  begin
    FFileWatcherAuto.OnTerminate := nil;
    FFileWatcherAuto.Terminate;
  end;

  FFileWatcher := nil;
  FFileWatcherAuto := nil;

  inherited;
end;

function TSavedTree.GetNode(Filename: string): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  Result := nil;
  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);
    if (NodeData.Track <> nil) and (LowerCase(NodeData.Track.Filename) = LowerCase(Filename)) then
    begin
      Result := Node;
      Exit;
    end;
    Node := GetNext(Node);
  end;
end;

function TSavedTree.GetNode(Track: TTrackInfo): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  Result := nil;
  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);
    if (NodeData.Track <> nil) and (NodeData.Track = Track) then
    begin
      Result := Node;
      Exit;
    end;
    Node := GetNext(Node);
  end;
end;

function TSavedTree.GetNodes(SelectedOnly: Boolean): TNodeArray;
var
  i: Integer;
  Node: PVirtualNode;
  Nodes: TNodeArray;
begin
  Result := [];
  if not SelectedOnly then
  begin
    Node := GetFirst;
    while Node <> nil do
    begin
      if PSavedNodeData(GetNodeData(Node)).Track <> nil then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Node;
      end;
      Node := GetNext(Node);
    end;
  end else
  begin
    SetLength(Result, 0);
    Nodes := GetSortedSelection(False);
    for i := 0 to Length(Nodes) - 1 do
      if PSavedNodeData(GetNodeData(Nodes[i])).Track <> nil then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Nodes[i];
      end;
  end;
end;

function TSavedTree.GetSelected: TTrackInfoArray;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
begin
  SetLength(Result, 0);
  Nodes := GetNodes(True);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if NodeData.Track <> nil then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := NodeData.Track;
    end;
  end;
end;

procedure TSavedTree.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  L: Integer;
  NodeData: PSavedNodeData;
begin
  if PaintInfo.Column = 0 then
  begin
    NodeData := GetNodeData(PaintInfo.Node);

    L := PaintInfo.ImageInfo[ImageInfoIndex].XPos;

    if NodeData.Track = nil then
    begin
      Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.FOLDER_MUSIC);
      Exit;
    end;

    if LowerCase(FPlayer.Filename) = LowerCase(NodeData.Track.Filename) then
    begin
      if FPlayer.Playing then
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.PLAY_BLUE)
      else if FPlayer.Paused then
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.PAUSE_BLUE)
      else
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.MUSIC);
    end else
      Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.MUSIC);

    if NodeData.Track.IsAuto then
      if NodeData.Track.RecordBecauseArtist then
        Images.Draw(PaintInfo.Canvas, L + 18, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.USER_GRAY_COOL)
      else
        Images.Draw(PaintInfo.Canvas, L + 18, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.BRICKS);

    if NodeData.Track.Finalized then
      Images.Draw(PaintInfo.Canvas, L + 18 * 2, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.TICK)
    else if NodeData.Track.WasCut then
      Images.Draw(PaintInfo.Canvas, L + 18 * 2, PaintInfo.ImageInfo[ImageInfoIndex].YPos, TImages.CUT);
  end;
end;

procedure TSavedTree.PlayerEndReached(Sender: TObject);
var
  TrackInfo: TTrackInfo;
begin
  FPlayer.Stop(True);

  if (not FPlayNext) then
    Exit;

  TrackInfo := NextPlayingTrack(True);
  if TrackInfo <> nil then
  begin
    try
      FPlayer.Filename := TrackInfo.Filename;
    except
      Exit;
    end;

    FPlayer.Play;

    FTab.FSeek.Max := Player.MaxByte;
    FTab.FSeek.Position := Player.PositionByte;
  end;

  FTab.UpdateButtons;
end;

procedure TSavedTree.PlayerPause(Sender: TObject);
begin
  Players.LastPlayer := FPlayer;
  FTab.UpdateButtons;
  Invalidate;
end;

procedure TSavedTree.PlayerPlay(Sender: TObject);
begin
  if AppGlobals.DisplayPlayNotifications then
    if (FPlayer.Tag <> nil) and (FPlayer.Tag.Artist <> '') and (FPlayer.Tag.Title <> '') then
      TfrmNotification.Display(FPlayer.Tag.Artist + ' - ' + FPlayer.Tag.Title, '')
    else
      TfrmNotification.Display(TFunctions.RemoveFileExt(ExtractFileName(FPlayer.Filename)), '');

  FTab.UpdateButtons;
  Invalidate;
end;

procedure TSavedTree.PlayerStop(Sender: TObject);
begin
  if Players.LastPlayer = Sender then
    Players.LastPlayer := nil;
  FTab.UpdateButtons;
  Invalidate;
end;

function TSavedTree.PrevPlayingTrack(ConsiderRnd: Boolean): TTrackInfo;
var
  i: Integer;
  Nodes: TNodeArray;
  Node: PVirtualNode;
  NodeData, NodeDataPrev: PSavedNodeData;
begin
  Result := nil;

  if FTab.FPlayToolbar.FShuffle.Down and ConsiderRnd then
  begin
    for i := FPlayerIndex - 1 downto 0 do
    begin
      Node := GetNode(FPlayerList[FPlayerIndex - 1]);
      if Node <> nil then
      begin
        NodeData := GetNodeData(Node);
        Result := NodeData.Track;
        FPlayerIndex := i;
        Break;
      end;
    end;

    if Result = nil then
    begin
      Result := NextPlayingTrack(True, False);

      if Result <> nil then
      begin
        FPlayerList.Insert(0, Result.Filename);
        FPlayerIndex := 0;
      end;
    end;
  end else
  begin
    Nodes := GetNodes(False);
    for i := 0 to Length(Nodes) - 1 do
    begin
      NodeData := GetNodeData(Nodes[i]);
      if LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename) then
      begin
        if i > 0 then
        begin
          NodeDataPrev := GetNodeData(Nodes[i - 1]);
          Result := NodeDataPrev.Track;
        end;
        Break;
      end;
    end;
  end;
end;

function TSavedTree.NextPlayingTrack(ConsiderRnd: Boolean; AddToPlayerList: Boolean): TTrackInfo;

  function GetRandom(ExceptFilename: string): TTrackInfo;
  var
    R: Integer;
    Node: PVirtualNode;
    NodeData: PSavedNodeData;
    Nodes: TNodeArray;
  begin
    Result := nil;

    if FFileNode.ChildCount <= 1 then
      Exit;

    SetLength(Nodes, 0);

    Node := GetFirstChild(FFileNode);

    if Node = nil then
      Exit;

    while Node <> nil do
    begin
      NodeData := GetNodeData(Node);
      if (NodeData.Track <> nil) and (NodeData.Track.Filename <> ExceptFilename) then
      begin
        SetLength(Nodes, Length(Nodes) + 1);
        Nodes[High(Nodes)] := Node;
      end;

      Node := GetNextSibling(Node);
    end;

    R := Random(Length(Nodes));

    NodeData := GetNodeData(Nodes[R]);
    if NodeData.Track <> nil then
      Exit(NodeData.Track);
  end;

var
  i: Integer;
  Nodes: TNodeArray;
  Node: PVirtualNode;
  NodeData, NodeDataNext: PSavedNodeData;
begin
  Result := nil;

  if not AddToPlayerList then
  begin
    Result := GetRandom(FPlayer.Filename);
    Exit;
  end;

  if FTab.FPlayToolbar.FShuffle.Down and ConsiderRnd then
  begin
    if FPlayerIndex = FPlayerList.Count - 1 then
    begin
      Result := GetRandom(FPlayer.Filename);

      if Result <> nil then
      begin
        FPlayerList.Add(Result.Filename);
        FPlayerIndex := FPlayerList.Count - 1;
      end;
    end else
    begin
      for i := FPlayerIndex + 1 to FPlayerList.Count - 1 do
      begin
        Node := GetNode(FPlayerList[i]);
        if Node <> nil then
        begin
          NodeData := GetNodeData(Node);
          Result := NodeData.Track;
          FPlayerIndex := i;
          Break;
        end;
      end;

      if Result = nil then
      begin
        Result := GetRandom(FPlayer.Filename);

        if Result <> nil then
        begin
          FPlayerList.Add(Result.Filename);
          FPlayerIndex := FPlayerList.Count - 1;
        end;
      end;
    end;
  end else
  begin
    Nodes := GetNodes(False);
    for i := 0 to Length(Nodes) - 1 do
    begin
      NodeData := GetNodeData(Nodes[i]);
      if LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename) then
      begin
        if i < Length(Nodes) - 1 then
        begin
          NodeDataNext := GetNodeData(Nodes[i + 1]);
          if NodeDataNext.Track <> nil then
            Result := NodeDataNext.Track;
        end;
        Break;
      end;
    end;
  end;
end;

procedure TSavedTree.PopupMenuClick(Sender: TObject);
var
  Action: TTrackActions;
  Tracks: TTrackInfoArray;
  Track: TTrackInfo;
begin
  Action := taUndefined;
  Tracks := GetSelected;

  //if Sender = FPopupMenu.ItemPrev then
  if Sender = FTab.FPlayToolbar.FPrev then
  begin
    try
      FPlayer.Filename := PrevPlayingTrack(True).Filename;
    except
      TFunctions.MsgBox(_('The file could not be openend for playing.'), _('Error'), MB_ICONERROR);
      Exit;
    end;

    FPlayNext := True;
    FTab.FSeek.Max := FPlayer.MaxByte;
    FTab.FSeek.Position := 0;

    if Assigned(FTab.FOnPlayStarted) then
      FTab.FOnPlayStarted(FTab);

    FPlayer.Play;
    Exit;
  end else if Sender = FTab.FPlayToolbar.FPause then
  begin
    if FPlayer.Paused then
    begin
      if Assigned(FTab.FOnPlayStarted) then
        FTab.FOnPlayStarted(FTab);
      FPlayer.Play;
    end else
    begin
      FPlayer.Pause;
    end;
    FTab.UpdateButtons;
    Exit;
  end else if Sender = FTab.FPlayToolbar.FStop then
  begin
    FPlayer.Stop(True);
    FTab.UpdateButtons;
    Exit;
  end else if Sender = FTab.FPlayToolbar.FNext then
  begin
    Track := NextPlayingTrack(True);
    if Track = nil then
      Exit;

    try
      FPlayer.Filename := Track.Filename;
    except
      TFunctions.MsgBox(_('The file could not be openend for playing.'), _('Error'), MB_ICONERROR);
      Exit;
    end;

    FPlayNext := True;
    FTab.FSeek.Max := FPlayer.MaxByte;
    FTab.FSeek.Position := 0;

    if Assigned(FTab.FOnPlayStarted) then
      FTab.FOnPlayStarted(FTab);

    FPlayer.Play;
    Exit;
    //end else if Sender = FPopupMenu.ItemPlayLastSecs then
  end else if Sender = FTab.FPlayToolbar.FPlayLastSecs then
  begin
    if (not FPlayer.Paused) and (not FPlayer.Playing) then
    begin
      try
        FPlayer.Filename := Tracks[0].Filename;
      except
        TFunctions.MsgBox(_('The file could not be openend for playing.'), _('Error'), MB_ICONERROR);
        Exit;
      end;
      FPlayer.Volume := Players.Volume;
      FTab.FSeek.Max := FPlayer.MaxByte;
    end;

    FPlayNext := False;
    FPlayer.PositionTime := FPlayer.MaxTime - 5;
    FTab.FSeek.Position := FPlayer.PositionByte;

    if Assigned(FTab.FOnPlayStarted) then
      FTab.FOnPlayStarted(FTab);

    FPlayer.Play;
    Exit;
  end;

  if (Length(Tracks) = 0) and (Sender <> FPopupMenu.ItemImportFiles) and (Sender <> FPopupMenu.ItemImportFolder) and (Sender <> FPopupMenu.ItemRefresh) then
    Exit;

  if Sender = FPopupMenu.ItemRefresh then
    Action := taRefresh
  else if Sender = FPopupMenu.ItemPlay then
  begin
    FPlayNext := True;
    FPlayer.Volume := Players.Volume;

    if FPlayer.Paused then
    begin
      if Assigned(FTab.FOnPlayStarted) then
        FTab.FOnPlayStarted(FTab);

      FPlayer.Play;
    end else
    begin
      try
        FPlayer.Filename := Tracks[0].Filename;

        if AppGlobals.PlayerShuffle then
        begin
          while FPlayerList.Count - 1 > FPlayerIndex do
            FPlayerList.Delete(FPlayerList.Count - 1);

          if (FPlayerList.Count > 0) and (FPlayerList[FPlayerList.Count - 1] = Tracks[0].Filename) then
            FPlayerList.Delete(FPlayerList.Count - 1);

          FPlayerList.Add(Tracks[0].Filename);
          FPlayerIndex := FPlayerList.Count - 1;
        end;
      except
        TFunctions.MsgBox(_('The file could not be openend for playing.'), _('Error'), MB_ICONERROR);
        Exit;
      end;

      FTab.FSeek.Max := FPlayer.MaxByte;
      if not FPlayer.Paused then
      begin
        FTab.FSeek.Position := 0;
        FPlayer.PositionByte := 0;
      end;

      if Assigned(FTab.FOnPlayStarted) then
        FTab.FOnPlayStarted(FTab);

      FPlayer.Play;
    end;

    FTab.UpdateButtons;
    Invalidate;

    Exit;
  end else if Sender = FPopupMenu.ItemCutSong then
    Action := taCutSong
  else if Sender = FPopupMenu.ItemEditTags then
    Action := taEditTags
  else if Sender = FPopupMenu.ItemFinalized then
    Action := taFinalized
  else if Sender = FPopupMenu.ItemAddToWishlist then
    Action := taAddToWishlist
  else if Sender = FPopupMenu.ItemRemoveFromWishlist then
    Action := taRemoveFromWishlist
  else if Sender = FPopupMenu.ItemAddToIgnorelist then
    Action := taAddToIgnorelist
  else if Sender = FPopupMenu.ItemRemoveFromIgnorelist then
    Action := taRemoveFromIgnorelist
  else if Sender = FPopupMenu.ItemCut then
    CutCopy(True)
  else if Sender = FPopupMenu.ItemCopy then
    CutCopy(False)
  else if Sender = FPopupMenu.ItemRename then
    EditNode(GetNode(Tracks[0]), 0)
  else if Sender = FPopupMenu.ItemRemove then
    Action := taRemove
  else if Sender = FPopupMenu.ItemRecycle then
    Action := taRecycle
  else if Sender = FPopupMenu.ItemDelete then
    Action := taDelete
  else if Sender = FPopupMenu.ItemShowFile then
    Action := taShowFile
  else if Sender = FPopupMenu.ItemProperties then
    Action := taProperties
  else if Sender = FPopupMenu.ItemImportFolder then
    Action := taImportFolder
  else if Sender = FPopupMenu.ItemImportFiles then
    Action := taImportFiles
  else
    raise Exception.Create('');

  if Action <> taUndefined then
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Tracks);
end;

procedure TSavedTree.AddTrack(Track: TTrackInfo; AddToInternalList: Boolean; IgnorePattern: Boolean);
var
  Node, ParentNode: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  if AddToInternalList then
    FTrackList.Add(Track);

  ParentNode := nil;
  if IgnorePattern then
    ParentNode := FFileNode
  else if (FPattern = '*') or (TFunctions.Like(LowerCase(Track.Filename), FPattern)) or (TFunctions.Like(LowerCase(Track.Streamname), FPattern)) then
    if Track.IsStreamFile then
      ParentNode := FStreamNode
    else
      ParentNode := FFileNode;

  if ParentNode <> nil then
  begin
    Node := InsertNode(ParentNode, amAddChildLast);
    NodeData := GetNodeData(Node);
    NodeData.Track := Track;
    if (ParentNode.ChildCount = 1) then
      Expanded[ParentNode] := True;
  end;
end;

procedure TSavedTree.RemoveTracks(Tracks: TTrackInfoArray);
var
  Node: PVirtualNode;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
  Track: TTrackInfo;
begin
  BeginUpdate;
  try
    Nodes := GetNodes(False);

    for Track in Tracks do
    begin
      for Node in Nodes do
      begin
        NodeData := GetNodeData(Node);
        if NodeData.Track = Track then
        begin
          DeleteNode(Node);
          Break;
        end;
      end;
      FTrackList.Remove(Track);
      AppGlobals.Data.TrackList.RemoveTrack(Track);
    end;
  finally
    EndUpdate;
  end;

  Change(nil);
end;

procedure TSavedTree.SetFileWatcher;
begin
  if FFileWatcher <> nil then
  begin
    FFileWatcher.OnTerminate := nil;
    FFileWatcher.Terminate;
  end;
  if FFileWatcherAuto <> nil then
  begin
    FFileWatcherAuto.OnTerminate := nil;
    FFileWatcherAuto.Terminate;
  end;

  FFileWatcher := nil;
  FFileWatcherAuto := nil;

  FFileWatcher := TFileWatcher.Create(AppGlobals.Dir, FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_SIZE);
  FFileWatcher.OnEvent := FileWatcherEvent;
  FFileWatcher.OnTerminate := FileWatcherTerminate;
  FFileWatcher.Start;

  if LowerCase(AppGlobals.Dir) <> LowerCase(AppGlobals.DirAuto) then
  begin
    FFileWatcherAuto := TFileWatcher.Create(AppGlobals.DirAuto, FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_SIZE);
    FFileWatcherAuto.OnEvent := FileWatcherEvent;
    FFileWatcherAuto.OnTerminate := FileWatcherTerminate;
    FFileWatcherAuto.Start;
  end;
end;

procedure TSavedTree.Sort(Node: PVirtualNode; Column: TColumnIndex; Direction: VirtualTrees.TSortDirection; DoInit: Boolean);
begin
  inherited;

  MoveTo(FStreamNode, nil, amAddChildFirst, False);
end;

procedure TSavedTree.PostTranslate;
begin
  FColImages.Text := _('State');
  FColFilename.Text := _('Filename');
  FColSize.Text := _('Size');
  FColLength.Text := _('Length');
  FColStream.Text := _('Stream');
  FColSaved.Text := _('Date');
  FColBitrate.Text := _('Bitrate');
end;

procedure TSavedTree.UpdateList;
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);

    if NodeData.Track <> nil then
      NodeData.Track.Index := Node.Index;

    Node := GetNext(Node);
  end;
end;

procedure TSavedTree.UpdateTracks(Tracks: TTrackInfoArray);
var
  Track: TTrackInfo;
  Node: PVirtualNode;
begin
  for Track in Tracks do
  begin
    Node := GetNode(Track);
    if Node <> nil then
      InvalidateNode(Node);
  end;
end;

procedure TSavedTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String);
var
  NodeData: PSavedNodeData;
begin
  inherited;

  Text := '';

  NodeData := GetNodeData(Node);

  if (NodeData.IsStreamParent) or (NodeData.IsFileParent) then
  begin
    if Column = 1 then
      if NodeData.IsStreamParent then
        Text := _(STREAMNODETEXT) + ' (' + IntToStr(Node.ChildCount) + ')'
      else
        Text := _(FILENODETEXT) + ' (' + IntToStr(Node.ChildCount) + ')';
  end else
    case Column of
      1: Text := ExtractFileName(NodeData.Track.Filename);
      2:
        Text := TFunctions.MakeSize(NodeData.Track.Filesize);
      3:
        Text := BuildTime(NodeData.Track.Length, False);
      4:
        if NodeData.Track.Bitrate > 0 then
        begin
          Text := IntToStr(NodeData.Track.Bitrate);

          if NodeData.Track.VBR then
            Text := Text + ' ' + 'VBR';
        end;
      5:
        Text := NodeData.Track.Streamname;
      6:
        if Trunc(NodeData.Track.Time) = Trunc(Now) then
          Text := TimeToStr(NodeData.Track.Time)
        else
          Text := DateTimeToStr(NodeData.Track.Time);
    end;
end;

function TSavedTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  // Wir müssen irgendeinen Index setzen, damit PaintImage() getriggert wird
  if (Column = 0) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    Index := 0;
end;

procedure TSavedTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if Header.SortColumn <> HitInfo.Column then
    begin
      Header.SortColumn := HitInfo.Column;
      if (HitInfo.Column = 0) or (HitInfo.Column = 2) or (HitInfo.Column = 3) or (HitInfo.Column = 4) or (HitInfo.Column = 5) or (HitInfo.Column = 6) then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end else if Header.SortDirection = sdAscending then
      Header.SortDirection := sdDescending
    else
      Header.SortDirection := sdAscending;
    Sort(FStreamNode, HitInfo.Column, Header.SortDirection);
    Sort(FFileNode, HitInfo.Column, Header.SortDirection);
  end;
end;

procedure TSavedTree.DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition);
begin
  inherited;

  if Header.Columns[Column].Position = 0 then
    Header.Columns[Column].Position := FHeaderDragSourcePosition;
end;

function TSavedTree.DoHeaderDragging(Column: TColumnIndex): Boolean;
begin
  if Column = -1 then
    Exit(False);

  Result := inherited;

  FHeaderDragSourcePosition := Header.Columns[Column].Position;
end;

function TSavedTree.DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer;
var
  CmpTxt: string;
  NodeData: PSavedNodeData;
begin
  NodeData := GetNodeData(Node);
  if NodeData.Track = nil then
    CmpTxt := _(STREAMNODETEXT)
  else
    CmpTxt := ExtractFileName(NodeData.Track.Filename);
  Result := StrLIComp(PChar(Text), PChar(CmpTxt), Min(Length(Text), Length(CmpTxt)));
end;

procedure TSavedTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string);
var
  NodeData: PSavedNodeData;
  NewText: string;
begin
  inherited;

  NodeData := GetNodeData(Node);
  NewText := Text;

  if FilenameToFormat(NewText) = atNone then
    NewText := TFunctions.RemoveFileExt(NewText) + ExtractFileExt(NodeData.Track.Filename);

  if RenameFile(ConcatPaths([ExtractFilePath(NodeData.Track.Filename), ExtractFileName(NodeData.Track.Filename)]), ConcatPaths([ExtractFilePath(NodeData.Track.Filename), NewText])) then
    NodeData.Track.Filename := ConcatPaths([ExtractFilePath(NodeData.Track.Filename), NewText])
  else
    TFunctions.MsgBox(_('The file could not be renamed. Make sure that it is not in use and that no other file with the same name already exists.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TSavedTree.DoNodeDblClick(const HitInfo: THitInfo);
var
  Tracks: TTrackInfoArray;
begin
  inherited;

  if hiOnItemButton in HitInfo.HitPositions then
    Exit;

  if HitInfo.HitNode <> nil then
  begin
    Tracks := GetSelected;
    if Length(Tracks) = 1 then
      FPopupMenu.FItemPlay.Click;
  end;
end;

function TSavedTree.DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean;
begin
  Result := inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Result := True;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(R);
end;

procedure TSavedTree.DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(ItemRect);
end;

procedure TSavedTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  if Focused and Selected[Node] then
    Canvas.Font.Color := AppGlobals.NodeTextColorSelectedFocused
  else if Selected[Node] then
    Canvas.Font.Color := AppGlobals.NodeTextColorSelected
  else
    Canvas.Font.Color := AppGlobals.NodeTextColor;
end;

procedure TSavedTree.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal);
var
  NodeData: PSavedNodeData;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  if NodeData.Track <> nil then
    if FPlayer.Playing or FPlayer.Paused then
      if not Selected[PaintInfo.Node] then
        if (FPlayer.Playing or FPlayer.Paused) and (LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename)) then
          PaintInfo.Canvas.Font.Color := TFunctions.HTML2Color('#0078ff');

  inherited;
end;

procedure TSavedTree.FileWatcherEvent(Sender: TObject; Action: TFileWatcherEventActions; Path, PathNew: string);

  function GetTracks(StartingWith: string): TTrackInfoArray;
  var
    Track: TTrackInfo;
    RemoveList: TList<TTrackInfo>;
  begin
    RemoveList := TList<TTrackInfo>.Create;
    try
      for Track in FTrackList do
        if LowerCase(Track.Filename.Substring(0, Length(IncludeTrailingPathDelimiter(StartingWith)))) = LowerCase(IncludeTrailingPathDelimiter(StartingWith)) then
          RemoveList.Add(Track);

      Result := RemoveList.ToArray;
    finally
      RemoveList.Free;
    end;
  end;

var
  Track: TTrackInfo;
  Tracks: TTrackInfoArray;
begin
  if Action = eaAdded then
    Exit;

  Track := AppGlobals.Data.TrackList.GetTrack(Path);
  if Assigned(Track) then
    Tracks := [Track]
  else
    Tracks := GetTracks(Path);

  case Action of
    eaMoved:
      if Assigned(Track) then
        Track.Filename := PathNew
      else
        for Track in Tracks do
          Track.Filename := ConcatPaths([PathNew, Track.Filename.Remove(0, Path.Length)]);
    eaModified:
      if Assigned(Track) then
        Track.Filesize := TFunctions.GetFileSize(Track.Filename);
  end;

  if Action = eaRemoved then
    RemoveTracks(Tracks)
  else
    UpdateTracks(Tracks);
end;

procedure TSavedTree.FileWatcherTerminate(Sender: TObject);
begin
  if Sender = FFileWatcher then
    FFileWatcher := nil
  else if Sender = FFileWatcherAuto then
    FFileWatcherAuto := nil;
end;

procedure TSavedTree.Filter(S: string);
var
  Tmp: TCardinalArray;
begin
  SetLength(Tmp, 0);
  Filter(S, Tmp, Tmp);
end;

procedure TSavedTree.Filter(S: string; ServerTitleHashes, ServerArtistHashes: TCardinalArray);
var
  i, n, k: Integer;
  StreamsExpanded, FilesExpanded: Boolean;
  NodeData: PSavedNodeData;
  Hash: Cardinal;
  Chars: Integer;
  TitleHashesAdded: TCardinalArray;
  AddedTitles: TList;
  Found: Boolean;
begin
  BeginUpdate;

  StreamsExpanded := Expanded[FStreamNode];
  FilesExpanded := Expanded[FFileNode];
  Clear;

  AddedTitles := TList.Create;
  try
    if (Length(ServerTitleHashes) = 0) and (Length(ServerArtistHashes) = 0) then
    begin
      FPattern := TFunctions.BuildPattern(S, Hash, Chars, True);

      FStreamNode := AddChild(nil);
      NodeData := GetNodeData(FStreamNode);
      NodeData.IsStreamParent := True;
      FFileNode := AddChild(nil);
      NodeData := GetNodeData(FFileNode);
      NodeData.IsFileParent := True;

      for i := 0 to FTrackList.Count - 1 do
        AddTrack(FTrackList[i], False);
    end else
    begin
      StreamsExpanded := False;
      FilesExpanded := True;

      FStreamNode := AddChild(nil);
      NodeData := GetNodeData(FStreamNode);
      NodeData.IsStreamParent := True;
      FFileNode := AddChild(nil);
      NodeData := GetNodeData(FFileNode);
      NodeData.IsFileParent := True;

      SetLength(TitleHashesAdded, 0);

      // Erstmal alles basierend auf Title-Hashes einfügen
      for i := 0 to FTrackList.Count - 1 do
        for n := 0 to High(ServerTitleHashes) do
          if FTrackList[i].ServerTitleHash = ServerTitleHashes[n] then
          begin
            AddTrack(FTrackList[i], False, True);

            SetLength(TitleHashesAdded, Length(TitleHashesAdded) + 1);
            TitleHashesAdded[High(TitleHashesAdded)] := ServerTitleHashes[n];

            AddedTitles.Add(FTrackList[i]);
          end;

      // Jetzt alles basierend auf Artist-Hashes einfügen, wenn noch nicht vorhanden
      for i := 0 to FTrackList.Count - 1 do
        for n := 0 to High(ServerArtistHashes) do
          if FTrackList[i].ServerArtistHash = ServerArtistHashes[n] then
          begin
            Found := False;
            for k := 0 to AddedTitles.Count - 1 do
              if AddedTitles[k] = FTrackList[i] then
              begin
                Found := True;
                Break;
              end;

            if not Found then
              AddTrack(FTrackList[i], False, True);
          end;
    end;
  finally
    AddedTitles.Free;
  end;

  Expanded[FStreamNode] := StreamsExpanded;
  Expanded[FFileNode] := FilesExpanded;

  Sort(FStreamNode, Header.SortColumn, Header.SortDirection);
  Sort(FFileNode, Header.SortColumn, Header.SortDirection);
  EndUpdate;

  Change(nil);
end;

procedure TSavedTree.FitColumns;
var
  i: Integer;
begin
  if (Header.Columns.Count <> Length(AppGlobals.SavedHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.SavedHeaderPosition)) then
    raise Exception.Create('(Header.Columns.Count <> Length(AppGlobals.SavedHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.SavedHeaderPosition))');

  if AppGlobals.SavedHeaderWidthLoaded then
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Width := AppGlobals.SavedHeaderWidth[i]
  else
  begin
    FColSize.Width := TFunctions.GetTextSize('111,11 KB', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColLength.Width := TFunctions.GetTextSize('00:00', Font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColBitrate.Width := TFunctions.GetTextSize('320 VBR', font).cx + MulDiv(20, Screen.PixelsPerInch, 96);
    FColStream.Width := MulDiv(200, Screen.PixelsPerInch, 96);
    FColSaved.Width := MulDiv(130, Screen.PixelsPerInch, 96);
  end;

  FColImages.Width := Max(Indent + Margin * 2 + 16 * 4 + 3 * 2, TFunctions.GetTextSize(FColImages.Text, Font).cx + MulDiv(50, Screen.PixelsPerInch, 96));

  if AppGlobals.SavedHeaderPositionLoaded then
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.SavedHeaderPosition[i];
end;

procedure TSavedTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  inherited;

  Allowed := PSavedNodeData(GetNodeData(Node)).Track <> nil;
end;

procedure TSavedTree.Change(Node: PVirtualNode);
begin
  inherited;

  FTab.UpdateButtons;
end;

function TSavedTree.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;

  function CmpTime(a, b: TDateTime): Integer;
  begin
    if a > b then
      Result := 1
    else if a < b then
      Result := -1
    else
      Result := 0;
  end;

  function CmpC(a, b: Cardinal): Integer;
  begin
    if a > b then
      Result := 1
    else if a < b then
      Result := -1
    else
      Result := 0;
  end;

var
  I1, I2: Integer;
  Data1, Data2: PSavedNodeData;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  if Data1.Track = nil then
    Exit(-1);
  if Data2.Track = nil then
    Exit(-1);

  case Column of
    -1:
      Result := CmpC(Data1.Track.Index, Data2.Track.Index);
    0:
    begin
      I1 := 0;
      I2 := 0;

      if Data1.Track.Finalized then
        I1 := I1 + 4;
      if Data2.Track.Finalized then
        I2 := I2 + 4;

      if Data1.Track.WasCut then
        I1 := I1 + 3;
      if Data2.Track.WasCut then
        I2 := I2 + 3;

      if Data1.Track.IsAuto then
        if Data1.Track.RecordBecauseArtist then
          I1 := I1 + 1
        else
          I1 := I1 + 2;
      if Data2.Track.IsAuto then
        if Data2.Track.RecordBecauseArtist then
          I2 := I2 + 1
        else
          I2 := I2 + 2;

      Result := TFunctions.CmpInt(I1, I2);

      if Result = 0 then
      begin
        Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
        if Header.SortDirection = sdDescending then
          Result := Result * -1;
      end;
    end;
    1: Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
    2:
    begin
      Result := TFunctions.CmpInt(Data1.Track.Filesize, Data2.Track.Filesize);
      if Result = 0 then
      begin
        Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
        if Header.SortDirection = sdDescending then
          Result := Result * -1;
      end;
    end;
    3:
    begin
      Result := TFunctions.CmpInt(Data1.Track.Length, Data2.Track.Length);
      if Result = 0 then
      begin
        Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
        if Header.SortDirection = sdDescending then
          Result := Result * -1;
      end;
    end;
    4:
    begin
      Result := TFunctions.CmpInt(Data1.Track.Bitrate, Data2.Track.Bitrate);
      if Result = 0 then
      begin
        Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
        if Header.SortDirection = sdDescending then
          Result := Result * -1;
      end;
    end;
    5:
    begin
      Result := CompareText(Data1.Track.Streamname, Data2.Track.Streamname);
      if Result = 0 then
      begin
        Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
        if Header.SortDirection = sdDescending then
          Result := Result * -1;
      end;
    end;
    6:
    begin
      Result := CmpTime(Data1.Track.Time, Data2.Track.Time);
      if Result = 0 then
      begin
        Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
        if Header.SortDirection = sdDescending then
          Result := Result * -1;
      end;
    end;
  end;
end;

procedure TSavedTree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = VK_SPACE then
    Key := 0
  else if Key = VK_DELETE then
    FPopupMenu.FItemDelete.Click
  else if ssCtrl in Shift then
    // Die Bedingung hier für ist dreckig. In China ist das bestimmt kein 'C' und eigentlich
    // sollte das über einen Menü-Shortcut laufen. Naja.... hauptsache funzt,
    // dann ich geb kein Fick drauf.
    if Key = Ord('C') then
      CutCopy(False)
    else if Key = Ord('X') then
      CutCopy(True);
end;

procedure TSavedTree.KeyPress(var Key: Char);
var
  Tracks: TTrackInfoArray;
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
  Nodes: TNodeArray;
begin
  if Key = ' ' then
  begin
    Nodes := GetNodes(False);
    for Node in Nodes do
    begin
      NodeData := GetNodeData(Node);
      if (NodeData.Track <> nil) and (FPlayer.Filename = NodeData.Track.Filename) then
      begin
        ClearSelection;
        SelectNodes(Node, Node, False);
        FocusedNode := Node;
        ScrollIntoView(Node, True);
        Exit;
      end;
    end;
  end else if Key = #13 then
  begin
    Key := #0;
    Tracks := GetSelected;
    if Length(Tracks) = 1 then
      FPopupMenu.FItemPlay.Click;
  end else
    inherited;
end;

procedure TSavedTree.MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
var
  Show: Boolean;
begin
  Show := True;
  if coVisible in Header.Columns[Index].Options then
    Show := False;

  if Show then
    Header.Columns[Index].Options := Header.Columns[Index].Options + [coVisible]
  else
    Header.Columns[Index].Options := Header.Columns[Index].Options - [coVisible];

  AppGlobals.SavedCols := AppGlobals.SavedCols xor (1 shl Index);
end;

procedure TSavedTree.MessageReceived(Msg: TMessageBase);
begin
  if Msg is TFileModifyMsg then
    if LowerCase(FPlayer.Filename) = LowerCase(TFileModifyMsg(Msg).Filename) then
      FPlayer.Stop(True, True);
end;

procedure TSavedTree.CreateHandle;
var
  TrackInfo: TTrackInfo;
begin
  inherited CreateHandle;

  for TrackInfo in AppGlobals.Data.TrackList do
    AddTrack(TrackInfo, True);

  SortTree(Header.SortColumn, Header.SortDirection);

  FTab.UpdateButtons;

  Expanded[FStreamNode] := True;
  Expanded[FFileNode] := True;

  if RootNodeCount > 0 then
  begin
    Selected[GetFirst] := True;
    FocusedNode := GetFirst;
  end;
end;

procedure TSavedTree.DoDragging(P: TPoint);
var
  i: Integer;
  Tracks: TTrackInfoArray;
begin
  if FDragSource.DragInProgress then
    Exit;

  FDragSource.Files.Clear;
  Tracks := GetSelected;
  for i := 0 to Length(Tracks) - 1 do
    FDragSource.Files.Add(Tracks[i].Filename);

  if FDragSource.Files.Count = 0 then
    Exit;

  DoStateChange([], [tsOLEDragPending, tsOLEDragging, tsClearPending]);
  FDragSource.Execute(True);
end;

procedure TSavedTree.DoEdit;
var
  Edit: TVTEdit;
begin
  EditColumn := 1;

  inherited;

  if (EditLink <> nil) and (EditLink is TStringEditLink) then
  begin
    Edit := (EditLink as TStringEditLink).Edit;
    Edit.SelStart := 0;
    Edit.SelLength := Length(Edit.Text) - Length(ExtractFileExt(Edit.Text));
  end;
end;

{ TSearchBar }

constructor TSearchBar.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FLabel := TLabel.Create(Self);
  FLabel.Align := alLeft;
  FLabel.Layout := tlCenter;
  FLabel.Parent := Self;
  FLabel.Caption := 'Search:';

  FSearch := TEdit.Create(Self);
  FSearch.Align := alLeft;
  FSearch.Width := 200;
  FSearch.Parent := Self;

  FLabel.Left := -100;
end;

destructor TSearchBar.Destroy;
begin

  inherited;
end;

procedure TSearchBar.FSetHashFilterSet(Value: Boolean);
var
  Tmp: TNotifyEvent;
begin
  FHashFilterSet := Value;
  if Value then
  begin
    Tmp := FSearch.OnChange;
    FSearch.OnChange := nil;
    FSearch.Text := _('');
    FSearch.Color := clBtnFace;
    FSearch.OnChange := Tmp;
  end else
    FSearch.Color := clWindow;
end;

{ TImportFilesThread }

constructor TImportFilesThread.Create(Dir: string; KnownFiles: TStringList);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FDir := Dir;
  FFoundAudioFiles := TStringList.Create;
  FFiles := TList<TTrackInfo>.Create;
  FKnownFiles := KnownFiles;
end;

constructor TImportFilesThread.Create(Files: TStrings; KnownFiles: TStringList);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FDir := '';
  FFoundAudioFiles := TStringList.Create;
  FFoundAudioFiles.Assign(Files);
  FFiles := TList<TTrackInfo>.Create;
  FKnownFiles := KnownFiles;
end;

destructor TImportFilesThread.Destroy;
begin
  FFiles.Free;
  FKnownFiles.Free;
  FFoundAudioFiles.Free;

  inherited;
end;

procedure TImportFilesThread.Sync;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TImportFilesThread.Execute;
var
  i, n: Integer;
  Add: Boolean;
  FoundFiles: TStringList;
  Track: TTrackInfo;
  Info: TAudioInfo;
begin
  FoundFiles := TStringList.Create;
  try
    if (FDir <> '') and (FFoundAudioFiles.Count = 0) then
    begin
      TFunctions.FindFiles(ConcatPaths([FDir, '*.*']), FoundFiles, True, @Terminated);
      for i := 0 to FoundFiles.Count - 1 do
      begin
        if Terminated then
          Exit;

        if FilenameToFormat(FoundFiles[i]) <> atNone then
        begin
          Add := True;
          for n := 0 to FKnownFiles.Count - 1 do
            if LowerCase(FKnownFiles[n]) = LowerCase(FoundFiles[i]) then
            begin
              Add := False;
              Break;
            end;

          if Add then
            FFoundAudioFiles.Add(FoundFiles[i]);
        end;
      end;
    end;

    for i := 0 to FFoundAudioFiles.Count - 1 do
    begin
      if Terminated then
        Exit;

      FCurrentFilename := ExtractFileName(FFoundAudioFiles[i]);
      Synchronize(Sync);

      Info.GetAudioInfo(FFoundAudioFiles[i]);
      if Info.Success then
      begin
        Track := TTrackInfo.Create;
        Track.Time := Now;
        Track.Bitrate := Info.Bitrate;
        Track.Length := Trunc(Info.Length);
        Track.Filename := FFoundAudioFiles[i];
        Track.Filesize := TFunctions.GetFileSize(FFoundAudioFiles[i]);
        Track.VBR := Info.VBR;

        if TFunctions.OccurenceCount('\', FFoundAudioFiles[i]) > 1 then
          Track.Streamname := TFunctions.ExtractLastDirName(ExtractFilePath(FFoundAudioFiles[i]));
        FFiles.Add(Track);
      end;

      FProgress := Trunc((i / FFoundAudioFiles.Count) * 100);
      FCurrentFilename := ExtractFileName(FFoundAudioFiles[i]);
      Synchronize(Sync);
    end;
  finally
    FoundFiles.Free;
  end;
end;

{ TImportPanel }

constructor TImportPanel.Create(AOwner: TComponent);
begin
  inherited;

  LabelFilename := TLabel.Create(Self);
  LabelFilename.Parent := Self;
  LabelFilename.Align := alTop;
  LabelFilename.Alignment := taCenter;
  LabelFilename.Caption := _('Searching files...');
  LabelFilename.Top := -100;

  ProgressBar := TProgressBar.Create(Self);
  ProgressBar.Parent := Self;
  ProgressBar.Align := alTop;
  ProgressBar.Style := pbstMarquee;

  Button := TButton.Create(Self);
  Button.Width := 93;
  Button.Height := 25;
  Button.Anchors := [];
  Button.AnchorParallel(akBottom, 0, Self);
  Button.AnchorParallel(akRight, 0, Self);
  Button.Parent := Self;
  Button.Caption := _('Cancel');
end;

procedure TImportPanel.SetData(Progress: Integer; CurrentFilename: string);
var
  W: Integer;
begin
  if ProgressBar.Style <> pbstNormal then
    ProgressBar.Style := pbstNormal;

  W := TFunctions.GetTextSize('Importing ""', LabelFilename.Font).cx;
  LabelFilename.Caption := Format(_('Importing "%s"'), [TFunctions.TruncateText(CurrentFilename, LabelFilename.Width - W - 20, LabelFilename.Font)]);
  if ProgressBar.Position <> Progress then
  begin
    if Progress < 100 then
      ProgressBar.Position := Progress + 1;
    ProgressBar.Position := Progress;
  end;
end;

{ TPlayToolBar }

constructor TPlayToolBar.Create(AOwner: TComponent);
var
  Sep: TToolButton;
begin
  inherited;

  FPrev := TToolButton.Create(Self);
  FPrev.Parent := Self;
  FPrev.Hint := 'Previous';
  FPrev.ImageIndex := TImages.PREVIOUS_BLUE;

  FPlay := TToolButton.Create(Self);
  FPlay.Parent := Self;
  FPlay.Hint := 'Play';
  FPlay.ImageIndex := TImages.PLAY_BLUE;

  FPause := TToolButton.Create(Self);
  FPause.Parent := Self;
  FPause.Hint := 'Pause';
  FPause.ImageIndex := TImages.PAUSE_BLUE;
  FPause.Style := tbsCheck;

  FStop := TToolButton.Create(Self);
  FStop.Parent := Self;
  FStop.Hint := 'Stop';
  FStop.ImageIndex := TImages.STOP_BLUE;

  FNext := TToolButton.Create(Self);
  FNext.Parent := Self;
  FNext.Hint := 'Next';
  FNext.ImageIndex := TImages.NEXT_BLUE;

  Sep := TToolButton.Create(Self);
  Sep.Style := tbsSeparator;
  Sep.Parent := Self;

  FPlayLastSecs := TToolButton.Create(Self);
  FPlayLastSecs.Parent := Self;
  FPlayLastSecs.Hint := 'Play end';
  FPlayLastSecs.ImageIndex := TImages.PLAY_END_BLUE;

  Sep := TToolButton.Create(Self);
  Sep.Style := tbsSeparator;
  Sep.Parent := Self;

  FShuffle := TToolButton.Create(Self);
  FShuffle.Parent := Self;
  FShuffle.Hint := 'Shuffle';
  FShuffle.ImageIndex := TImages.ARROW_SWITCH_BLUE;
  FShuffle.Down := AppGlobals.PlayerShuffle;
  FShuffle.Style := tbsCheck;
end;

procedure TPlayToolBar.EnableItems(Enable, Playing, IsFirst, IsLast: Boolean);
begin
  FPrev.Enabled := (not IsFirst) and Playing;
  FPlay.Enabled := Enable and Bass.DeviceAvailable;
  FPause.Enabled := Playing and Bass.DeviceAvailable;
  FStop.Enabled := Playing and Bass.DeviceAvailable;
  FNext.Enabled := (not IsLast) and Playing;
end;

end.
