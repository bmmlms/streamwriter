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

{ This unit contains the TabControl used to show saved songs }
unit SavedTab;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, MControls, LanguageObjects, Tabs, VirtualTrees, DataManager,
  ImgList, Functions, DragDropFile, GUIFunctions, StreamInfoView, DynBASS,
  Menus, Math, Forms, Player, SharedControls, AppData, Graphics, Themes,
  PlayerManager, Logging, FileWatcher, MessageBus, AppMessages, ShlObj,
  SavedTabEditTags, Generics.Collections, TypeDefs, AudioFunctions, FileTagger,
  Notifications;

type
  TSavedTree = class;

  TSavedNodeData = record
    Track: TTrackInfo;
  end;
  PSavedNodeData = ^TSavedNodeData;

  TTrackActions = (taUndefined, taRefresh, taCut, taEditTags, taFinalized, taAddToWishlist, taRemove,
                   taRecycle, taDelete, taShowFile, taProperties, taImport);

  TTrackInfoArray = array of TTrackInfo;

  TTrackActionEvent = procedure(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray) of object;

  TImportFilesThread = class(TThread)
  private
    FDir: string;
    FFiles: TList<TTrackInfo>;
    FKnownFiles: TStringList;
    FProgress: Integer;
    FCurrentFilename: string;

    FOnProgress: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(Dir: string; KnownFiles: TStringList);
    destructor Destroy; override;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  TSavedTracksPopup = class(TPopupMenu)
  private
    FItemRefresh: TMenuItem;
    FItemPrev: TMenuItem;
    FItemPlay: TMenuItem;
    FItemPause: TMenuItem;
    FItemStop: TMenuItem;
    FItemNext: TMenuItem;
    FItemCut: TMenuItem;
    FItemEditTags: TMenuItem;
    FItemFinalized: TMenuItem;
    FItemAddToWishlist: TMenuItem;
    FItemRename: TMenuItem;
    FItemRemove: TMenuItem;
    FItemRecycle: TMenuItem;
    FItemDelete: TMenuItem;
    FItemShowFile: TMenuItem;
    FItemProperties: TMenuItem;
    FItemImport: TMenuItem;
  protected
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable, Playing, IsFirst, IsLast: Boolean);

    property ItemRefresh: TMenuItem read FItemRefresh;
    property ItemPrev: TMenuItem read FItemPrev;
    property ItemPlay: TMenuItem read FItemPlay;
    property ItemPause: TMenuItem read FItemPause;
    property ItemStop: TMenuItem read FItemStop;
    property ItemNext: TMenuItem read FItemNext;
    property ItemCut: TMenuItem read FItemCut;
    property ItemEditTags: TMenuItem read FItemEditTags;
    property ItemFinalized: TMenuItem read FItemFinalized;
    property ItemAddToWishlist: TMenuItem read FItemAddToWishlist;
    property ItemRename: TMenuItem read FItemRename;
    property ItemRemove: TMenuItem read FItemRemove;
    property ItemRecycle: TMenuItem read FItemRecycle;
    property ItemDelete: TMenuItem read FItemDelete;
    property ItemShowFile: TMenuItem read FItemShowFile;
    property ItemProperties: TMenuItem read FItemProperties;
    property ItemImport: TMenuItem read FItemImport;
  end;

  TSavedToolBar = class(TToolBar)
  private
    FRefresh: TToolButton;
    FSep1: TToolButton;
    FCut: TToolButton;
    FEditTags: TToolButton;
    FFinalized: TToolButton;
    FAddToWishlist: TToolButton;
    FSep2: TToolButton;
    FRename: TToolButton;
    FRemove: TToolButton;
    FRecycle: TToolButton;
    FDelete: TToolButton;
    FSep3: TToolButton;
    FShowFile: TToolButton;
    FProperties: TToolButton;
    FImport: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable: Boolean);

    procedure Setup;
  end;

  TPlayToolBar = class(TToolBar)
  private
    FPrev: TToolButton;
    FPlay: TToolButton;
    FPause: TToolButton;
    FStop: TToolButton;
    FNext: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable, Playing, IsFirst, IsLast: Boolean);

    procedure Setup;
  end;

  TSearchBar = class(TPanel)
  private
    FLabel: TLabel;
    FSearch: TEdit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Setup;
  end;

  TImportPanel = class(TPanel)
  private
    LabelFilename: TLabel;
    ProgressBar: TProgressBar;
    Button: TButton;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure SetData(Progress: Integer; CurrentFilename: string);
  end;

  TSavedTab = class(TMainTabSheet)
  private
    FPositionTimer: TTimer;

    FTopPanel: TPanel;
    FTopLeftPanel: TPanel;
    FTopRightPanel: TPanel;
    FTopRightTopPanel: TPanel;
    FTopRightBottomPanel: TPanel;
    FCoverPanel: TPanel;
    FCoverBorderPanel: TPanel;
    FCoverImage: TImage;
    FSeekPosPanel: TPanel;
    FPosLabel: TLabel;
    FToolbar: TSavedToolBar;
    FPlayToolbar: TPlayToolBar;
    FVolume: TVolumePanel;
    FSeek: TSeekBar;
    FSearchBar: TSearchBar;
    FSavedTree: TSavedTree;
    FStreams: TDataLists;

    FImportPanel: TImportPanel;
    FImportThread: TImportFilesThread;

    FOnCut: TTrackEvent;
    FOnTrackRemoved: TTrackEvent;
    FOnRefresh: TNotifyEvent;
    FOnPlayStarted: TNotifyEvent;
    FOnAddTitleToWishlist: TStringEvent;

    procedure BuildTree;
    procedure SavedTreeAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);
    procedure ToolBarClick(Sender: TObject);
    procedure SearchTextChange(Sender: TObject);
    procedure VolumeTrackbarChange(Sender: TObject);
    function VolumeGetVolumeBeforeMute(Sender: TObject): Integer;
    procedure SeekChange(Sender: TObject);
    procedure PositionTimer(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);

    procedure ImportThreadProgress(Sender: TObject);
    procedure ImportThreadTerminate(Sender: TObject);

    procedure ImportPanelCancelClick(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Setup(Streams: TDataLists; Images: TImageList);
    procedure Shown;
    procedure PausePlay;

    procedure UpdateButtons;
    procedure StopThreads;

    property Tree: TSavedTree read FSavedTree;

    property OnCut: TTrackEvent read FOnCut write FOnCut;
    property OnTrackRemoved: TTrackEvent read FOnTrackRemoved write FOnTrackRemoved;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnPlayStarted: TNotifyEvent read FOnPlayStarted write FOnPlayStarted;
    property OnAddTitleToWishlist: TStringEvent read FOnAddTitleToWishlist write FOnAddTitleToWishlist;
  end;

  TSavedTree = class(TVirtualStringTree)
  private
    FPlayer: TPlayer;
    FDragSource: TDropFileSource;
    FTab: TSavedTab;
    FTrackList: TTrackList;
    FFileWatcher, FFileWatcherAuto: TFileWatcher;
    FStreamNode: PVirtualNode;

    FOnAction: TTrackActionEvent;

    FPopupMenu: TSavedTracksPopup;

    FColImages: TVirtualTreeColumn;
    FColFilename: TVirtualTreeColumn;
    FColSize: TVirtualTreeColumn;
    FColLength: TVirtualTreeColumn;
    FColStream: TVirtualTreeColumn;
    FColSaved: TVirtualTreeColumn;
    FColBitRate: TVirtualTreeColumn;

    procedure FitColumns;

    function GetNode(Filename: string): PVirtualNode; overload;
    function GetNode(Track: TTrackInfo): PVirtualNode; overload;
    function GetNodes(SelectedOnly: Boolean): TNodeArray;
    function GetSelected: TTrackInfoArray;
    function TrackMatchesPattern(Track: TTrackInfo): Boolean;

    procedure PopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);

    procedure PlayerEndReached(Sender: TObject);
    procedure PlayerPlay(Sender: TObject);
    procedure PlayerPause(Sender: TObject);
    procedure PlayerStop(Sender: TObject);

    procedure MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);

    procedure FileWatcherEvent(Sender: TObject; Action: DWORD; OldName, NewName: string);
    procedure FileWatcherTerminate(Sender: TObject);

    procedure MessageReceived(Msg: TMessageBase);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo); override;
    procedure DoDragging(P: TPoint); override;
    function DoIncrementalSearch(Node: PVirtualNode;
      const Text: string): Integer; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: UnicodeString); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure Change(Node: PVirtualNode); override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo;
      ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
    procedure DoEdit; override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; Text: string;
      CellRect: TRect; DrawFormat: Cardinal); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Translate;

    function PrevPlayingTrack: TTrackInfo;
    function NextPlayingTrack: TTrackInfo;
    procedure AddTrack(Track: TTrackInfo; FromFilter: Boolean);
    procedure RemoveTrack(Track: TTrackInfo); overload;
    procedure DeleteTrack(Track: TTrackInfo);
    procedure UpdateTrack(Track: TTrackInfo);
    procedure Filter(S: string);
    procedure Sort(Node: PVirtualNode; Column: TColumnIndex;
      Direction: VirtualTrees.TSortDirection; DoInit: Boolean = True); override;
    procedure SetFileWatcher;
    procedure UpdateList;

    property Player: TPlayer read FPlayer;
    property OnAction: TTrackActionEvent read FOnAction write FOnAction;
  end;

const
  STREAMNODETEXT = 'Stream files';

implementation

{ TSavedTracksPopup }

constructor TSavedTracksPopup.Create(AOwner: TComponent);
var
  ItemTmp: TMenuItem;
begin
  inherited;

  FItemRefresh := CreateMenuItem;
  FItemRefresh.Caption := 'Re&fresh';
  FItemRefresh.ImageIndex := 23;
  Items.Add(FItemRefresh);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemPrev := CreateMenuItem;
  FItemPrev.Caption := 'Pre&vious';
  FItemPrev.ImageIndex := 79;
  Items.Add(FItemPrev);

  FItemPlay := CreateMenuItem;
  FItemPlay.Caption := '&Play';
  FItemPlay.ImageIndex := 33;
  Items.Add(FItemPlay);

  FItemPause := CreateMenuItem;
  FItemPause.Caption := 'Pa&use';
  FItemPause.ImageIndex := 39;
  Items.Add(FItemPause);

  FItemStop := CreateMenuItem;
  FItemStop.Caption := 'St&op';
  FItemStop.ImageIndex := 1;
  Items.Add(FItemStop);

  FItemNext := CreateMenuItem;
  FItemNext.Caption := '&Next';
  FItemNext.ImageIndex := 78;
  Items.Add(FItemNext);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemCut := CreateMenuItem;
  FItemCut.Caption := '&Cut';
  FItemCut.ImageIndex := 17;
  Items.Add(FItemCut);

  FItemEditTags := CreateMenuItem;
  FItemEditTags.Caption := '&Edit tags...';
  FItemEditTags.ImageIndex := 75;
  Items.Add(FItemEditTags);

  FItemFinalized := CreateMenuItem;
  FItemFinalized.Caption := 'Finali&zed';
  FItemFinalized.ImageIndex := 58;
  Items.Add(FItemFinalized);

  FItemAddToWishlist := CreateMenuItem;
  FItemAddToWishlist.Caption := 'Add to &wishlist';
  FItemAddToWishlist.ImageIndex := 11;
  Items.Add(FItemAddToWishlist);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemRename := CreateMenuItem;
  FItemRename.Caption := 'Ren&ame';
  FItemRename.ImageIndex := 74;
  Items.Add(FItemRename);

  FItemRemove := CreateMenuItem;
  FItemRemove.Caption := '&Remove from list';
  FItemRemove.ImageIndex := 21;
  Items.Add(FItemRemove);

  FItemRecycle := CreateMenuItem;
  FItemRecycle.Caption := 'Rec&ycle files';
  FItemRecycle.ImageIndex := 24;
  Items.Add(FItemRecycle);

  FItemDelete := CreateMenuItem;
  FItemDelete.Caption := '&Delete files';
  FItemDelete.ImageIndex := 2;
  Items.Add(FItemDelete);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemShowFile := CreateMenuItem;
  FItemShowFile.Caption := 'Show in e&xplorer';
  FItemShowFile.ImageIndex := 28;
  Items.Add(FItemShowFile);

  FItemProperties := CreateMenuItem;
  FItemProperties.Caption := 'Proper&ties';
  FItemProperties.ImageIndex := 22;
  Items.Add(FItemProperties);

  ItemTmp := CreateMenuItem;
  ItemTmp.Caption := '-';
  Items.Add(ItemTmp);

  FItemImport := CreateMenuItem;
  FItemImport.Caption := '&Import files...';
  FItemImport.ImageIndex := 36;
  Items.Add(FItemImport);
end;

procedure TSavedTracksPopup.EnableItems(Enable, Playing, IsFirst, IsLast: Boolean);
begin
  FItemPrev.Enabled := (not IsFirst) and Playing;
  FItemPlay.Enabled := Enable;
  FItemPause.Enabled := Playing;
  FItemStop.Enabled := Playing;
  FItemNext.Enabled := (not IsLast) and Playing;
  FItemCut.Enabled := Enable;
  FItemEditTags.Enabled := Enable;
  FItemFinalized.Enabled := Enable;
  FItemAddToWishlist.Enabled := Enable;
  FItemRename.Enabled := Enable;
  FItemRemove.Enabled := Enable;
  FItemRecycle.Enabled := Enable;
  FItemDelete.Enabled := Enable;
  ItemShowFile.Enabled := Enable;
  FItemProperties.Enabled := Enable;
end;

{ TSavedToolBar }

constructor TSavedToolBar.Create(AOwner: TComponent);
begin
  inherited;

  ShowHint := True;
  Transparent := True;
end;

procedure TSavedToolBar.EnableItems(Enable: Boolean);
begin
  FCut.Enabled := Enable;
  FEditTags.Enabled := Enable;
  FFinalized.Enabled := Enable;
  FAddToWishlist.Enabled := Enable;
  FRemove.Enabled := Enable;
  FRecycle.Enabled := Enable;
  FDelete.Enabled := Enable;
  FShowFile.Enabled := Enable;
  FProperties.Enabled := Enable;
end;

procedure TSavedToolBar.Setup;
begin
  FImport := TToolButton.Create(Self);
  FImport.Parent := Self;
  FImport.Hint := _('Import files...');
  FImport.ImageIndex := 36;

  FSep3 := TToolButton.Create(Self);
  FSep3.Parent := Self;
  FSep3.Style := tbsSeparator;
  FSep3.Width := 8;

  FProperties := TToolButton.Create(Self);
  FProperties.Parent := Self;
  FProperties.Hint := _('Properties');
  FProperties.ImageIndex := 22;

  FShowFile := TToolButton.Create(Self);
  FShowFile.Parent := Self;
  FShowFile.Hint := _('Show in explorer');
  FShowFile.ImageIndex := 28;

  FSep2 := TToolButton.Create(Self);
  FSep2.Parent := Self;
  FSep2.Style := tbsSeparator;
  FSep2.Width := 8;

  FDelete := TToolButton.Create(Self);
  FDelete.Parent := Self;
  FDelete.Hint := 'Delete files';
  FDelete.ImageIndex := 2;

  FRecycle := TToolButton.Create(Self);
  FRecycle.Parent := Self;
  FRecycle.Hint := 'Recycle files';
  FRecycle.ImageIndex := 24;

  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := 'Remove from list';
  FRemove.ImageIndex := 21;

  FRename := TToolButton.Create(Self);
  FRename.Parent := Self;
  FRename.Hint := 'Rename';
  FRename.ImageIndex := 74;

  FSep2 := TToolButton.Create(Self);
  FSep2.Parent := Self;
  FSep2.Style := tbsSeparator;
  FSep2.Width := 8;

  FAddToWishlist := TToolButton.Create(Self);
  FAddToWishlist.Parent := Self;
  FAddToWishlist.Hint := 'Add to wishlist';
  FAddToWishlist.ImageIndex := 11;

  FFinalized := TToolButton.Create(Self);
  FFinalized.Parent := Self;
  FFinalized.Hint := 'Finalized';
  FFinalized.ImageIndex := 58;

  FEditTags := TToolButton.Create(Self);
  FEditTags.Parent := Self;
  FEditTags.Hint := 'Edit tags...';
  FEditTags.ImageIndex := 75;

  FCut := TToolButton.Create(Self);
  FCut.Parent := Self;
  FCut.Hint := 'Cut';
  FCut.ImageIndex := 17;

  FSep1 := TToolButton.Create(Self);
  FSep1.Parent := Self;
  FSep1.Style := tbsSeparator;
  FSep1.Width := 8;

  FRefresh := TToolButton.Create(Self);
  FRefresh.Parent := Self;
  FRefresh.Hint := 'Refresh';
  FRefresh.ImageIndex := 23;
end;

{ TSavedTab }

constructor TSavedTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MsgBus.AddSubscriber(MessageReceived);

  ShowCloseButton := False;
  ImageIndex := 14;

  FPositionTimer := TTimer.Create(Self);
  FPositionTimer.Interval := 50;
  FPositionTimer.OnTimer := PositionTimer;
  FPositionTimer.Enabled := False;

  FSavedTree := TSavedTree.Create(Self);
  FSavedTree.Parent := Self;
  FSavedTree.Align := alClient;
  FSavedTree.OnAction := SavedTreeAction;
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
    FStreams.TrackList.Add(FImportThread.FFiles[i]);
    FSavedTree.AddTrack(FImportThread.FFiles[i], False);
  end;
  FImportThread := nil;

  FreeAndNil(FImportPanel);
  FSavedTree.Enabled := True;

  UpdateButtons;
end;

procedure TSavedTab.MessageReceived(Msg: TMessageBase);
var
  VolMsg: TVolumeChangedMsg;
begin
  if Msg is TVolumeChangedMsg then
  begin
    VolMsg := TVolumeChangedMsg(Msg);

    if VolMsg.Volume <> FVolume.Volume then
      FVolume.Volume := TVolumeChangedMsg(Msg).Volume;
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

  if FSavedTree.Player.Playing or FSavedTree.Player.Paused then
  begin
    FSeek.GripperVisible := True;

    // Ich habe das Gefühl, dass das hier eine schwer reproduzierbare Exception verursachen
    // kann, die dank des Timers jede Sekunde ein paar MsgBox() macht, deshalb so komisch hier.
    try
      FSeek.Position := Tree.Player.PositionByte;
      FPosLabel.Caption := BuildTime(Tree.Player.PositionTime, False);
    except
      FPosLabel.Caption := '00:00';
    end;
  end else
  begin
    FSeek.GripperVisible := False;
    FPosLabel.Caption := '00:00';
  end;
end;

procedure TSavedTab.Resize;
begin
  inherited;

  if FImportPanel <> nil then
  begin
    FImportPanel.Left := FSavedTree.ClientWidth div 2 - FImportPanel.Width div 2;
    FImportPanel.Top := FSavedTree.Top + (FSavedTree.Height - FSavedTree.ClientHeight) + FSavedTree.ClientHeight div 2 - FImportPanel.Height div 2;
  end;
end;

procedure TSavedTab.BuildTree;
var
  i: Integer;
begin
  for i := 0 to FStreams.TrackList.Count - 1 do
  begin
    FSavedTree.AddTrack(FStreams.TrackList[i], False);
  end;

  FSavedTree.Expanded[FSavedTree.FStreamNode] := False;

  FSavedTree.Header.SortColumn := -1;

  FSavedTree.SortTree(FSavedTree.Header.SortColumn, FSavedTree.Header.SortDirection);

  FSavedTree.Change(nil);
end;

procedure TSavedTab.SavedTreeAction(Sender: TObject; Action: TTrackActions;
  Tracks: TTrackInfoArray);
var
  i: Integer;
  Error, AllFinalized: Boolean;
  LowerDir, Dir: string;
  EditTags: TfrmEditTags;
  KnownFiles: TStringList;
begin
  case Action of
    taRefresh:
      begin
        if Assigned(FOnRefresh) then
          FOnRefresh(Self);
      end;
    taCut:
      begin
        if Assigned(FOnCut) then
          for i := 0 to Length(Tracks) - 1 do
            FOnCut(nil, Tracks[i]);
      end;
    taEditTags:
      begin
        EditTags := TfrmEditTags.Create(GetParentForm(Self));
        try
          if EditTags.EditFile(Tracks[0].Filename) then
            EditTags.ShowModal
          else
            MsgBox(GetPArentForm(Self).Handle, _('The file cannot be edited because it is in use or a needed addon has not been installed.'), _('Info'), MB_ICONINFORMATION);
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
      begin
        for i := 0 to Length(Tracks) - 1 do
          FOnAddTitleToWishlist(Self, ExtractFileName(RemoveFileExt(Tracks[i].Filename)));
      end;
    taRemove:
      begin
        FSavedTree.BeginUpdate;
        try
          for i := 0 to Length(Tracks) - 1 do
          begin
            FStreams.TrackList.RemoveTrack(Tracks[i]);
            FSavedTree.DeleteTrack(Tracks[i]);
            if Assigned(FOnTrackRemoved) then
              FOnTrackRemoved(nil, Tracks[i]);
          end;
        finally
          FSavedTree.EndUpdate;
        end;
      end;
    taRecycle:
      begin
        for i := 0 to Length(Tracks) - 1 do
        begin
          MsgBus.SendMessage(TFileModifyMsg.Create(Tracks[i].Filename));

          if Recycle(Handle, Tracks[i].Filename) then
          begin
            LowerDir := LowerCase(IncludeTrailingBackslash(ExtractFilePath(Tracks[i].Filename)));
            if (LowerDir <> LowerCase(IncludeTrailingBackslash(AppGlobals.Dir))) and (LowerDir <> LowerCase(IncludeTrailingBackslash(AppGlobals.DirAuto))) then
              Windows.RemoveDirectory(PChar(ExtractFilePath(Tracks[i].Filename)));
            FSavedTree.DeleteTrack(Tracks[i]);
            FStreams.TrackList.RemoveTrack(Tracks[i]);
            if Assigned(FOnTrackRemoved) then
              FOnTrackRemoved(nil, Tracks[i]);
          end;
        end;
      end;
    taDelete:
      begin
        if Length(Tracks) = 1 then
        begin
          if MsgBox(GetParentForm(Self).Handle, Format(_('Do you really want to delete "%s"?'), [ExtractFileName(Tracks[0].Filename)]), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
            Exit;
        end else
        begin
          if MsgBox(GetParentForm(Self).Handle, _('Do you really want to delete all selected files?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
            Exit;
        end;

        Error := False;
        FSavedTree.BeginUpdate;
        try
          for i := 0 to Length(Tracks) - 1 do
          begin
            MsgBus.SendMessage(TFileModifyMsg.Create(Tracks[i].Filename));

            if Windows.DeleteFile(PChar(Tracks[i].Filename)) or (GetLastError = ERROR_FILE_NOT_FOUND) then
            begin
              LowerDir := LowerCase(IncludeTrailingBackslash(ExtractFilePath(Tracks[i].Filename)));
              if not ((LowerDir = LowerCase(IncludeTrailingBackslash(AppGlobals.Dir))) and (LowerDir = LowerCase(IncludeTrailingBackslash(AppGlobals.DirAuto)))) then
                Windows.RemoveDirectory(PChar(ExtractFilePath(Tracks[i].Filename)));
              FSavedTree.DeleteTrack(Tracks[i]);
              FStreams.TrackList.RemoveTrack(Tracks[i]);
              if Assigned(FOnTrackRemoved) then
                FOnTrackRemoved(nil, Tracks[i]);
            end else
              Error := True;
          end;
        finally
          FSavedTree.EndUpdate;
        end;
        if Error then
          MsgBox(GetParentForm(Self).Handle, _('Some files could not be deleted.'#13#10'Please make sure they are not opened in a cut-tab or in use by another application.'), _('Info'), MB_ICONINFORMATION);
      end;
    taShowFile:
      RunProcess('explorer.exe /select,"' + Tracks[0].Filename + '"');
    taProperties:
      PropertiesDialog(Tracks[0].Filename);
    taImport:
      begin
        Dir := BrowseDialog(GetParentForm(Self).Handle, _('Select folder with files to import:'), BIF_RETURNONLYFSDIRS);
        if Dir <> '' then
        begin
          KnownFiles := TStringList.Create;
          for i := 0 to FStreams.TrackList.Count - 1 do
            KnownFiles.Add(FStreams.TrackList[i].Filename);

          FImportPanel := TImportPanel.Create(Self);
          FImportPanel.Width := 250;
          FImportPanel.Height := 80;
          FImportPanel.Parent := Self;
          FImportPanel.Button.OnClick := ImportPanelCancelClick;
          Resize;

          FImportThread := TImportFilesThread.Create(Dir, KnownFiles);
          FImportThread.OnTerminate := ImportThreadTerminate;
          FImportThread.OnProgress := ImportThreadProgress;
          FImportThread.Resume;

          if FSavedTree.Player.Playing then
            FSavedTree.FPlayer.Pause;

          FSavedTree.Enabled := False;

          UpdateButtons;
        end;
      end;
  end;

  FSavedTree.Change(nil);
end;

procedure TSavedTab.ToolBarClick(Sender: TObject);
begin
  if Sender = FToolbar.FRefresh then
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);

  if Sender = FPlayToolbar.FPrev then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPrev);
  if Sender = FPlayToolbar.FPlay then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPlay);
  if Sender = FPlayToolbar.FPause then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemPause);
  if Sender = FPlayToolbar.FStop then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemStop);
  if Sender = FPlayToolbar.FNext then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemNext);

  if Sender = FToolbar.FCut then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemCut);
  if Sender = FToolbar.FEditTags then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemEditTags);
  if Sender = FToolbar.FFinalized then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemFinalized);
  if Sender = FToolbar.FAddToWishlist then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemAddToWishlist);
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
  if Sender = FToolbar.FImport then
    FSavedTree.PopupMenuClick(FSavedTree.FPopupMenu.ItemImport);
end;

procedure TSavedTab.UpdateButtons;
var
  i: Integer;
  AllFinalized, IsFirst, IsLast: Boolean;
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
    FSavedTree.FPopupMenu.EnableItems(False, False, True, True);

    Exit;
  end;

  Tracks := Tree.GetSelected;

  if Tree.Player.Playing or Tree.Player.Paused then
  begin
    IsFirst := FSavedTree.PrevPlayingTrack = nil;
    IsLast := FSavedTree.NextPlayingTrack = nil;
  end else
  begin
    IsFirst := True;
    IsLast := True;
  end;

  FPlayToolbar.FPause.Down := Tree.Player.Paused;

  Tree.FPopupMenu.EnableItems(Length(Tracks) > 0, Tree.FPlayer.Playing or Tree.FPlayer.Paused, IsFirst, IsLast);
  FToolbar.EnableItems(Length(Tracks) > 0);
  FPlayToolbar.EnableItems(Length(Tracks) > 0, Tree.FPlayer.Playing or Tree.FPlayer.Paused, IsFirst, IsLast);

  Tree.FPopupMenu.ItemPlay.Enabled := Bass.DeviceAvailable and (Length(Tracks) = 1);
  FPlayToolbar.FPlay.Enabled := Bass.DeviceAvailable and (Length(Tracks) = 1);

  Tree.FPopupMenu.ItemShowFile.Enabled := Length(Tracks) = 1;
  FToolbar.FShowFile.Enabled := Length(Tracks) = 1;
  Tree.FPopupMenu.ItemProperties.Enabled := Length(Tracks) = 1;
  FToolbar.FProperties.Enabled := Length(Tracks) = 1;

  Tree.FPopupMenu.ItemCut.Enabled := Length(Tracks) > 0;
  FToolbar.FCut.Enabled := Length(Tracks) > 0;

  Tree.FPopupMenu.ItemEditTags.Enabled := Length(Tracks) = 1;
  FToolbar.FEditTags.Enabled := Length(Tracks) = 1;

  Tree.FPopupMenu.ItemRename.Enabled := Length(Tracks) = 1;
  FToolbar.FRename.Enabled := Length(Tracks) = 1;

  Tree.FPopupMenu.ItemImport.Enabled := True;
  FToolbar.FImport.Enabled := True;

  AllFinalized := True;
  for i := 0 to High(Tracks) do
    if not Tracks[i].Finalized then
    begin
      AllFinalized := False;
      Break;
    end;
  // Das muss so, sonst klappt das .Down := AllFinalized nicht, wenn sie
  // vorher Disabled waren, vor dem Enable da oben...
  FToolbar.FFinalized.Down := False;
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
  FSavedTree.Filter(FSearchBar.FSearch.Text);
end;

procedure TSavedTab.SeekChange(Sender: TObject);
begin
  FSavedTree.FPlayer.PositionByte := FSeek.Position;
  PositionTimer(FPositionTimer);
  UpdateButtons;
end;

procedure TSavedTab.Setup(Streams: TDataLists; Images: TImageList);
var
  DummyPanel: TPanel;
begin
  Caption := 'Saved songs';

  FStreams := Streams;

  FSavedTree.Images := Images;
  FSavedTree.StateImages := Images;
  FSavedTree.FPopupMenu.Images := Images;

  // Panel oben komplett
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.ClientHeight := 53;
  FTopPanel.BevelOuter := bvNone;

  // Panel links
  FTopLeftPanel := TPanel.Create(Self);
  FTopLeftPanel.Parent := FTopPanel;
  FTopLeftPanel.Align := alClient;
  FTopLeftPanel.ClientHeight := 52;
  FTopLeftPanel.BevelOuter := bvNone;

  // Panel rechts
  FTopRightPanel := TPanel.Create(Self);
  FTopRightPanel.Parent := FTopPanel;
  FTopRightPanel.Align := alRight;
  FTopRightPanel.ClientHeight := 52;
  FTopRightPanel.ClientWidth := 310;
  FTopRightPanel.BevelOuter := bvNone;

  FCoverPanel := TPanel.Create(Self);
  FCoverPanel.Parent := FTopPanel;
  FCoverPanel.Align := alRight;
  FCoverPanel.BevelOuter := bvNone;
  FCoverPanel.Padding.Bottom := 4;
  FCoverPanel.Padding.Right := 8;
  FCoverPanel.Width := FCoverPanel.Height + 8;
  FCoverPanel.Visible := False;

  FCoverBorderPanel := TPanel.Create(FCoverPanel);
  FCoverBorderPanel.Parent := FCoverPanel;
  FCoverBorderPanel.BevelKind := bkFlat;
  FCoverBorderPanel.BevelOuter := bvNone;
  FCoverBorderPanel.Align := alClient;

  FCoverImage := TImage.Create(Self);
  FCoverImage.Parent := FCoverBorderPanel;
  FCoverImage.Align := alClient;
  FCoverImage.Stretch := False;
  FCoverImage.Center := True;

  // Panel rechts unten für Positionslabel/Playercontrols
  FTopRightBottomPanel := TPanel.Create(Self);
  FTopRightBottomPanel.Parent := FTopRightPanel;
  FTopRightBottomPanel.Align := alTop;
  FTopRightBottomPanel.BevelOuter := bvNone;

  // Panel rechts oben für Position suchen und Lautstärke
  FTopRightTopPanel := TPanel.Create(Self);
  FTopRightTopPanel.Parent := FTopRightPanel;
  FTopRightTopPanel.Align := alTop;
  FTopRightTopPanel.ClientHeight := 24;
  FTopRightTopPanel.BevelOuter := bvNone;

  // Panel für Zeitanzeigen und Playercontrols
  FSeekPosPanel := TPanel.Create(Self);
  FSeekPosPanel.Parent := FTopRightBottomPanel;
  FSeekPosPanel.Align := alClient;
  FSeekPosPanel.BevelOuter := bvNone;

  FPlayToolbar := TPlayToolBar.Create(Self);
  FPlayToolbar.Parent := FSeekPosPanel;
  FPlayToolbar.Align := alLeft;
  FPlayToolbar.Images := Images;
  FPlayToolbar.Width := 120;
  FPlayToolbar.Setup;
  FPlayToolbar.Left := 0;

  FPosLabel := TLabel.Create(Self);
  FPosLabel.Caption := '00:00';
  FPosLabel.Parent := FSeekPosPanel;
  FPosLabel.Left := FPlayToolbar.Left + FPlayToolbar.Width + 4;
  FPosLabel.Top := FPlayToolbar.Top + 4;

  FSearchBar := TSearchBar.Create(Self);
  FSearchBar.Parent := FTopLeftPanel;
  FSearchBar.Align := alTop;
  FSearchBar.Setup;
  FSearchBar.FSearch.OnChange := SearchTextChange;

  DummyPanel := TPanel.Create(Self);
  DummyPanel.Parent := FTopLeftPanel;
  DummyPanel.Align := alTop;
  DummyPanel.ClientHeight := 2;
  DummyPanel.BevelOuter := bvNone;

  FToolBar := TSavedToolBar.Create(Self);
  FToolBar.Parent := FTopLeftPanel;
  FToolBar.Align := alTop;
  FToolBar.AutoSize := True;
  FToolBar.Height := 25;
  FToolbar.Indent := 0;
  FToolBar.Images := Images;
  FToolBar.Setup;

  FSeek := TSeekBar.Create(Self);
  FSeek.Parent := FTopRightTopPanel;
  FSeek.Align := alRight;
  FSeek.Left := FToolbar.Left + FToolbar.Width + 10;
  FSeek.Width := 160;
  FSeek.OnPositionChanged := SeekChange;

  FVolume := TVolumePanel.Create(Self);
  FVolume.Parent := FTopRightTopPanel;
  FVolume.Align := alRight;
  FVolume.Setup;
  FVolume.Width := 150;
  FVolume.Volume := Players.Volume;
  FVolume.OnVolumeChange := VolumeTrackbarChange;
  FVolume.OnGetVolumeBeforeMute := VolumeGetVolumeBeforeMute;
  FVolume.Padding.Left := 10;
  FVolume.Left := High(Integer);

  FToolbar.Top := 0;
  FSearchBar.Top := FToolBar.Height + 20;

  FToolBar.FRefresh.OnClick := ToolBarClick;

  FPlayToolBar.FPrev.OnClick := ToolBarClick;
  FPlayToolBar.FPlay.OnClick := ToolBarClick;
  FPlayToolBar.FPause.OnClick := ToolBarClick;
  FPlayToolBar.FStop.OnClick := ToolBarClick;
  FPlayToolBar.FNext.OnClick := ToolBarClick;

  FToolBar.FCut.OnClick := ToolBarClick;
  FToolBar.FEditTags.OnClick := ToolBarClick;
  FToolBar.FFinalized.OnClick := ToolBarClick;
  FToolBar.FAddToWishlist.OnClick := ToolBarClick;
  FToolBar.FRename.OnClick := ToolBarClick;
  FToolBar.FRemove.OnClick := ToolBarClick;
  FToolBar.FRecycle.OnClick := ToolBarClick;
  FToolBar.FDelete.OnClick := ToolBarClick;
  FToolBar.FShowFile.OnClick := ToolBarClick;
  FToolBar.FProperties.OnClick := ToolBarClick;
  FToolbar.FImport.OnClick := ToolBarClick;

  BuildTree;

  FPositionTimer.Enabled := True;
end;

procedure TSavedTab.Shown;
var
  i: Integer;
begin
  if FSavedTree.RootNodeCount > 0 then
  begin
    FSavedTree.Selected[FSavedTree.GetFirst] := True;
    FSavedTree.FocusedNode := FSavedTree.GetFirst;
  end;

  if AppGlobals.SavedHeadersLoaded then
    for i := 2 to FSavedTree.Header.Columns.Count - 1 do
      FSavedTree.Header.Columns[i].Width := AppGlobals.SavedHeaderWidth[i];
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
begin
  inherited Create(AOwner);

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
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.Options := Header.Options - [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 1;
  DragMode := dmAutomatic;
  ShowHint := True;
  HintMode := hmTooltip;

  FDragSource := TDropFileSource.Create(Self);

  FPopupMenu := TSavedTracksPopup.Create(Self);
  FPopupMenu.ItemRefresh.OnClick := PopupMenuClick;
  FPopupMenu.ItemPrev.OnClick := PopupMenuClick;
  FPopupMenu.ItemPlay.OnClick := PopupMenuClick;
  FPopupMenu.ItemPause.OnClick := PopupMenuClick;
  FPopupMenu.ItemStop.OnClick := PopupMenuClick;
  FPopupMenu.ItemNext.OnClick := PopupMenuClick;
  FPopupMenu.ItemCut.OnClick := PopupMenuClick;
  FPopupMenu.ItemEditTags.OnClick := PopupMenuClick;
  FPopupMenu.ItemFinalized.OnClick := PopupMenuClick;
  FPopupMenu.ItemAddToWishlist.OnClick := PopupMenuClick;
  FPopupMenu.ItemRename.OnClick := PopupMenuClick;
  FPopupMenu.ItemRemove.OnClick := PopupMenuClick;
  FPopupMenu.ItemRecycle.OnClick := PopupMenuClick;
  FPopupMenu.ItemDelete.OnClick := PopupMenuClick;
  FPopupMenu.ItemShowFile.OnClick := PopupMenuClick;
  FPopupMenu.ItemProperties.OnClick := PopupMenuClick;
  FPopupMenu.ItemImport.OnClick := PopupMenuClick;
  FPopupMenu.OnPopup := PopupMenuPopUp;

  PopupMenu := FPopupMenu;

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
  FColBitRate := Header.Columns.Add;
  FColBitRate.Alignment := taRightJustify;
  FColBitRate.Text := _('Bitrate');
  FColStream := Header.Columns.Add;
  FColStream.Text := _('Stream');
  FColSaved := Header.Columns.Add;
  FColSaved.Alignment := taRightJustify;
  FColSaved.Text := _('Time');

  Header.Options := Header.Options + [hoAutoResize];

  FStreamNode := AddChild(nil);

  SetFileWatcher;

  MsgBus.AddSubscriber(MessageReceived);

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);
  TMTreeColumnPopup(Header.PopupMenu).HideIdx := 1;
  TMTreeColumnPopup(Header.PopupMenu).OnAction := MenuColsAction;

  for i := 1 to Header.Columns.Count - 1 do
  begin
    if not ((AppGlobals.SavedCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];
  end;

  FitColumns;
end;

destructor TSavedTree.Destroy;
begin
  FPlayer.Free;
  FTrackList.Free;
  FDragSource.Free;

  if FFileWatcher <> nil then
  begin
    FFileWatcher.OnEvent := nil;
    FFileWatcher.Terminate;
  end;
  if FFileWatcherAuto <> nil then
  begin
    FFileWatcherAuto.OnEvent := nil;
    FFileWatcherAuto.Terminate;
  end;

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
  SetLength(Result, 0);
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
    begin
      if PSavedNodeData(GetNodeData(Nodes[i])).Track <> nil then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Nodes[i];
      end;
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

procedure TSavedTree.DeleteTrack(Track: TTrackInfo);
var
  i: Integer;
  NodeData: PSavedNodeData;
  Nodes: TNodeArray;
begin
  Nodes := GetNodes(False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if Track = NodeData.Track then
    begin
      DeleteNode(Nodes[i]);
      Break;
    end;
  end;
  FTrackList.Remove(Track);
end;

procedure TSavedTree.PaintImage(var PaintInfo: TVTPaintInfo;
  ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
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
      Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 67);
      Exit;
    end;

    if LowerCase(FPlayer.Filename) = LowerCase(NodeData.Track.Filename) then
    begin
      if FPlayer.Playing then
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 33)
      else if FPlayer.Paused then
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 39)
      else
        Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 20);
    end else
      Images.Draw(PaintInfo.Canvas, L, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 20);

    if NodeData.Track.IsStreamFile then
    begin
      if NodeData.Track.WasCut then
        Images.Draw(PaintInfo.Canvas, L + 16, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 17);
      if NodeData.Track.Finalized then
        Images.Draw(PaintInfo.Canvas, L + 32, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 58);
    end else
    begin
      if NodeData.Track.WasCut then
        Images.Draw(PaintInfo.Canvas, L + 32, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 17);
      if NodeData.Track.IsAuto then
        Images.Draw(PaintInfo.Canvas, L + 16, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 77);
      if NodeData.Track.Finalized then
        Images.Draw(PaintInfo.Canvas, L + 48, PaintInfo.ImageInfo[ImageInfoIndex].YPos, 58);
    end;
  end;
end;

procedure TSavedTree.PlayerEndReached(Sender: TObject);
var
  PlayedNode, NextNode: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  FPlayer.Stop(True);

  // Nächsten Track in der Liste suchen, der auch in der Ansicht
  // angezeigt wird. Wenn gefunden, abspielen.
  PlayedNode := GetNode(Player.Filename);
  if PlayedNode <> nil then
  begin
    NextNode := GetNext(PlayedNode);
    if NextNode <> nil then
    begin
      NodeData := GetNodeData(NextNode);
      if NodeData.Track <> nil then
      begin
        try
          FPlayer.Filename := NodeData.Track.Filename;
        except
          Exit;
        end;

        FPlayer.Play;

        FTab.FSeek.Max := Player.MaxByte;
        FTab.FSeek.Position := Player.PositionByte;
      end;
    end;
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
      TfrmNotification.Act(FPlayer.Tag.Artist + ' - ' + FPlayer.Tag.Title, '')
    else
      TfrmNotification.Act(RemoveFileExt(ExtractFileName(FPlayer.Filename)), '');

  if (FPlayer.Tag <> nil) and (FPlayer.Tag.CoverImage <> nil) then
  begin
    FTab.FCoverBorderPanel.BevelKind := bkNone;
    FTab.FCoverPanel.Show;
    FTab.FCoverBorderPanel.BevelKind := bkFlat;
    FTab.FCoverImage.Picture.Assign(ResizeBitmap(FPlayer.Tag.CoverImage, Min(FTab.FCoverImage.Height, FTab.FCoverImage.Width)));
  end else
    FTab.FCoverPanel.Hide;

  FTab.UpdateButtons;
  Invalidate;
end;

procedure TSavedTree.PlayerStop(Sender: TObject);
begin
  if Players.LastPlayer = Sender then
    Players.LastPlayer := nil;
  FTab.UpdateButtons;
  Invalidate;

  FTab.FCoverPanel.Hide;
end;

function TSavedTree.PrevPlayingTrack: TTrackInfo;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData, NodeDataPrev: PSavedNodeData;
begin
  Result := nil;
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

function TSavedTree.NextPlayingTrack: TTrackInfo;
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData, NodeDataNext: PSavedNodeData;
begin
  Result := nil;
  Nodes := GetNodes(False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename) then
    begin
      if i < Length(Nodes) - 1 then
      begin
        NodeDataNext := GetNodeData(Nodes[i + 1]);
        Result := NodeDataNext.Track;
      end;
      Break;
    end;
  end;
end;

procedure TSavedTree.PopupMenuClick(Sender: TObject);
var
  Action: TTrackActions;
  Tracks: TTrackInfoArray;
begin
  Action := taUndefined;
  Tracks := GetSelected;

  if Sender = FPopupMenu.ItemPrev then
  begin
    FPlayer.Stop(False);
    FPlayer.Filename := PrevPlayingTrack.Filename;
    FTab.FSeek.Max := FPlayer.MaxByte;
    FTab.FSeek.Position := 0;

    if Assigned(FTab.FOnPlayStarted) then
      FTab.FOnPlayStarted(FTab);

    FPlayer.Play;
    Exit;
  end else if Sender = FPopupMenu.ItemPause then
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
  end else if Sender = FPopupMenu.ItemStop then
  begin
    FPlayer.Stop(True);
    FTab.UpdateButtons;
    Exit;
  end else if Sender = FPopupMenu.ItemNext then
  begin
    FPlayer.Stop(False);
    FPlayer.Filename := NextPlayingTrack.Filename;
    FTab.FSeek.Max := FPlayer.MaxByte;
    FTab.FSeek.Position := 0;

    if Assigned(FTab.FOnPlayStarted) then
      FTab.FOnPlayStarted(FTab);

    FPlayer.Play;
    Exit;
  end;

  if (Length(Tracks) = 0) and (Sender <> FPopupMenu.ItemImport) and (Sender <> FPopupMenu.ItemRefresh) then
    Exit;

  if Sender = FPopupMenu.ItemRefresh then
    Action := taRefresh
  else if Sender = FPopupMenu.ItemPlay then
  begin
    FPlayer.Volume := Players.Volume;

    if FPlayer.Paused then
    begin
      FPlayer.Play;
    end else
    begin
      if Assigned(FTab.FOnPlayStarted) then
        FTab.FOnPlayStarted(FTab);

      if not FPlayer.Paused then
      begin
        try
          FPlayer.Filename := Tracks[0].Filename;
        except
          MsgBox(GetParentForm(Self).Handle, _('The file could not be openend for playing.'), _('Error'), MB_ICONERROR);
          Exit;
        end;
      end;

      FTab.FSeek.Max := FPlayer.MaxByte;
      if not FPlayer.Paused then
      begin
        FTab.FSeek.Position := 0;
        FPlayer.PositionByte := 0;
      end;
      FPlayer.Play;
    end;

    FTab.UpdateButtons;
    Invalidate;

    Exit;
  end else if Sender = FPopupMenu.ItemCut then
    Action := taCut
  else if Sender = FPopupMenu.ItemEditTags then
  begin
    Action := taEditTags;
  end else if Sender = FPopupMenu.ItemFinalized then
    Action := taFinalized
  else if Sender = FPopupMenu.ItemAddToWishlist then
    Action := taAddToWishlist
  else if Sender = FPopupMenu.ItemRename then
    EditNode(GetNode(Tracks[0]), 0)
  else if Sender = FPopupMenu.ItemRemove then
  begin
    Action := taRemove;
  end else if Sender = FPopupMenu.ItemRecycle then
  begin
    Action := taRecycle;
  end else if Sender = FPopupMenu.ItemDelete then
  begin
    Action := taDelete;
  end else if Sender = FPopupMenu.ItemShowFile then
    Action := taShowFile
  else if Sender = FPopupMenu.ItemProperties then
    Action := taProperties
  else if Sender = FPopupMenu.ItemImport then
    Action := taImport
  else
    raise Exception.Create('');

  if Action <> taUndefined then
    if Assigned(FOnAction) then
      FOnAction(Self, Action, Tracks);
end;

procedure TSavedTree.PopupMenuPopup(Sender: TObject);
begin
  FPopupMenu.FItemPause.Enabled := FPlayer.Playing or FPlayer.Paused;
  FPopupMenu.FItemStop.Enabled := FPlayer.Playing or FPlayer.Paused;
end;

procedure TSavedTree.AddTrack(Track: TTrackInfo; FromFilter: Boolean);
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  if not FromFilter then
    FTrackList.Add(Track)
  else
    if not TrackMatchesPattern(Track) then
      Exit;

  if Track.IsStreamFile then
  begin
    Node := AddChild(FStreamNode);
    if FStreamNode.ChildCount = 1 then
      Expanded[FStreamNode] := True;
  end else
    Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Track := Track;
end;

procedure TSavedTree.RemoveTrack(Track: TTrackInfo);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PSavedNodeData;
begin
  if Track = nil then
    Exit;

  FTrackList.Remove(Track);

  Nodes := GetNodes(False);
  for i := 0 to Length(Nodes) - 1 do
  begin
    NodeData := GetNodeData(Nodes[i]);
    if NodeData.Track = Track then
    begin
      DeleteNode(Nodes[i]);
      Break;
    end;
  end;

  Change(nil);
end;

procedure TSavedTree.SetFileWatcher;
begin
  if FFileWatcher <> nil then
  begin
    FFileWatcher.OnEvent := nil;
    FFileWatcher.Terminate;
  end;
  if FFileWatcherAuto <> nil then
  begin
    FFileWatcherAuto.OnEvent := nil;
    FFileWatcherAuto.Terminate;
  end;

  FFileWatcher := TFileWatcher.Create(AppGlobals.Dir, FILE_NOTIFY_CHANGE_FILE_NAME);
  FFileWatcher.OnEvent := FileWatcherEvent;
  FFileWatcher.OnTerminate := FileWatcherTerminate;
  FFileWatcher.Start;

  FFileWatcherAuto := TFileWatcher.Create(AppGlobals.DirAuto, FILE_NOTIFY_CHANGE_FILE_NAME);
  FFileWatcherAuto.OnEvent := FileWatcherEvent;
  FFileWatcherAuto.OnTerminate := FileWatcherTerminate;
  FFileWatcherAuto.Start;
end;

procedure TSavedTree.Sort(Node: PVirtualNode; Column: TColumnIndex;
  Direction: VirtualTrees.TSortDirection; DoInit: Boolean);
begin
  inherited;

  MoveTo(FStreamNode, nil, amAddChildFirst, False);
end;

function TSavedTree.TrackMatchesPattern(Track: TTrackInfo): Boolean;
var
  Hash: Cardinal;
  Chars: Integer;
  P: string;
begin
  Result := True;
  P := BuildPattern(FTab.FSearchBar.FSearch.Text, Hash, Chars, True);
  if P = '' then
    Exit;
  if (not Like(LowerCase(Track.Filename), LowerCase(P))) and (not Like(LowerCase(Track.Streamname), LowerCase(P))) then
    Result := False;
end;

procedure TSavedTree.Translate;
begin
  FColImages.Text := _('State');
  FColFilename.Text := _('Filename');
  FColSize.Text := _('Size');
  FColLength.Text := _('Length');
  FColStream.Text := _('Stream');
  FColSaved.Text := _('Time');
  FColBitRate.Text := _('Bitrate');
end;

procedure TSavedTree.UpdateList;
var
  Node: PVirtualNode;
  NodeData: PSavedNodeData;
begin
  Node := GetFirst;
  while Node <> nil do
  begin
    if Node <> FStreamNode then
    begin
      NodeData := GetNodeData(Node);
      NodeData.Track.Index := Node.Index;
    end;

    Node := GetNext(Node);
  end;
end;

procedure TSavedTree.UpdateTrack(Track: TTrackInfo);
var
  Node: PVirtualNode;
begin
  Node := GetNode(Track);
  if Node <> nil then
    InvalidateNode(Node);
end;

procedure TSavedTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PSavedNodeData;
begin
  inherited;

  Text := '';

  if TextType = ttNormal then
  begin
    NodeData := GetNodeData(Node);

    if NodeData.Track = nil then
    begin
      if Column = 1 then
        Text := _(STREAMNODETEXT) + ' (' + IntToStr(Node.ChildCount) + ')';
    end else
      case Column of
        1: Text := ExtractFileName(NodeData.Track.Filename);
        2:
          Text := MakeSize(NodeData.Track.Filesize);
        3:
          Text := BuildTime(NodeData.Track.Length, False);
        4:
          if NodeData.Track.BitRate > 0 then
          begin
            Text := IntToStr(NodeData.Track.BitRate);

            if NodeData.Track.VBR then
              Text := Text + ' ' + 'VBR';
          end;
        5:
          Text := NodeData.Track.Streamname;
        6:
          begin
            if Trunc(NodeData.Track.Time) = Trunc(Now) then
              Text := TimeToStr(NodeData.Track.Time)
            else
              Text := DateTimeToStr(NodeData.Track.Time);
          end;
      end;
  end;
end;

function TSavedTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
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
      if (HitInfo.Column = 0) or (HitInfo.Column = 2) or (HitInfo.Column = 3) or (HitInfo.Column = 5) then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end else
    begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end;
    Sort(nil, HitInfo.Column, Header.SortDirection);
    Sort(FStreamNode, HitInfo.Column, Header.SortDirection);
  end;
end;

function TSavedTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
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

procedure TSavedTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  Text: UnicodeString);
var
  NodeData: PSavedNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  if FiletypeToFormat(LowerCase(ExtractFileExt(Text))) = atNone then
    Text := RemoveFileExt(Text) + ExtractFileExt(NodeData.Track.Filename);

  if RenameFile(IncludeTrailingBackslash(ExtractFilePath(NodeData.Track.Filename)) + ExtractFileName(NodeData.Track.Filename),
    IncludeTrailingBackslash(ExtractFilePath(NodeData.Track.Filename)) + Text) then
  begin
    NodeData.Track.Filename := IncludeTrailingBackslash(ExtractFilePath(NodeData.Track.Filename)) + Text;
  end else
    MsgBox(GetParentForm(Self).Handle, _('The file could not be renamed. Make sure that it is not in use.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TSavedTree.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  Text: string; CellRect: TRect; DrawFormat: Cardinal);
var
  NodeData: PSavedNodeData;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  if NodeData.Track <> nil then
    if FPlayer.Playing or FPlayer.Paused then
      if not Selected[PaintInfo.Node] then
        if (FPlayer.Playing or FPlayer.Paused) and (LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename)) then
        begin
          PaintInfo.Canvas.Font.Color := HTML2Color('#0078ff');
        end
      else
        if (FPlayer.Playing or FPlayer.Paused) and (LowerCase(NodeData.Track.Filename) = LowerCase(FPlayer.Filename)) then
        begin
          PaintInfo.Canvas.Font.Color := PaintInfo.Canvas.Font.Color - 100;
        end;

  inherited;
end;

procedure TSavedTree.FileWatcherEvent(Sender: TObject; Action: DWORD;
  OldName, NewName: string);
var
  Track: TTrackInfo;
  Node: PVirtualNode;
begin
  Track := nil;
  if (Action = FILE_ACTION_REMOVED) or (Action = FILE_ACTION_RENAMED_NEW_NAME) then
    if Sender = FFileWatcher then
      Track := FTrackList.GetTrack(AppGlobals.Dir + OldName)
    else
      Track := FTrackList.GetTrack(AppGlobals.DirAuto + OldName);

  if Track = nil then
    Exit;

  case Action of
    FILE_ACTION_REMOVED:
      begin
        RemoveTrack(Track);
        FTab.FStreams.TrackList.RemoveTrack(Track);
      end;
    FILE_ACTION_RENAMED_NEW_NAME:
      begin
        if FiletypeToFormat(LowerCase(ExtractFileExt(NewName))) = atNone then
        begin
          RemoveTrack(Track);
          FTab.FStreams.TrackList.RemoveTrack(Track);
          Exit;
        end;

        if Sender = FFileWatcher then
          Track.Filename := AppGlobals.Dir + NewName
        else
          Track.Filename := AppGlobals.DirAuto + NewName;
        Node := GetNode(Track);
        if Node <> nil then
          InvalidateNode(Node);
      end;
  end;
end;

procedure TSavedTree.FileWatcherTerminate(Sender: TObject);
begin
  if Sender = FFileWatcher then
    FFileWatcher := nil;
  if Sender = FFileWatcherAuto then
    FFileWatcherAuto := nil;
end;

procedure TSavedTree.Filter(S: string);
var
  i: Integer;
begin
  BeginUpdate;
  Clear;

  FStreamNode := AddChild(nil);

  for i := 0 to FTrackList.Count - 1 do
    AddTrack(FTrackList[i], True);

  Sort(nil, Header.SortColumn, Header.SortDirection);

  EndUpdate;

  Change(nil);
end;

procedure TSavedTree.FitColumns;
  function GetTextWidth(Text: string): Integer;
  var
    Canvas: TAccessCanvas;
  begin
    Canvas := TAccessCanvas.Create;
    try
      Canvas.Handle := GetDC(GetDesktopWindow);
      SelectObject(Canvas.Handle, Header.Font.Handle);
      Result := Canvas.TextWidth(Text) + 20;
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    finally
      Canvas.Free;
    end;
  end;
begin
  FColImages.Width := 72;
  FColSize.Width := GetTextWidth('111,11 KB');
  FColLength.Width := GetTextWidth('00:00');
  FColBitRate.Width := GetTextWidth('320 VBR');
  FColStream.Width := 200;
  FColSaved.Width := 130;
end;

procedure TSavedTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  inherited;

  Allowed := PSavedNodeData(GetNodeData(Node)).Track <> nil;
end;

procedure TSavedTree.Change(Node: PVirtualNode);
begin
  inherited;

  FTab.UpdateButtons;
end;

function TSavedTree.DoCompare(Node1, Node2: PVirtualNode;
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
          I1 := I1 + 3;
        if Data2.Track.Finalized then
          I2 := I2 + 3;

        if Data1.Track.WasCut then
          I1 := I1 + 2;
        if Data2.Track.WasCut then
          I2 := I2 + 2;

        if Data1.Track.IsAuto then
          I1 := I1 + 1;
        if Data2.Track.IsAuto then
          I2 := I2 + 1;

        Result := CmpInt(I1, I2);

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
        Result := CmpInt(Data1.Track.Filesize, Data2.Track.Filesize);
        if Result = 0 then
        begin
          Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
          if Header.SortDirection = sdDescending then
            Result := Result * -1;
        end;
      end;
    3:
      begin
        Result := CmpInt(Data1.Track.Length, Data2.Track.Length);
        if Result = 0 then
        begin
          Result := CompareText(ExtractFileName(Data1.Track.Filename), ExtractFileName(Data2.Track.Filename));
          if Header.SortDirection = sdDescending then
            Result := Result * -1;
        end;
      end;
    4:
      begin
        Result := CmpInt(Data1.Track.BitRate, Data2.Track.BitRate);
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

procedure TSavedTree.HandleMouseDblClick(var Message: TWMMouse;
  const HitInfo: THitInfo);
var
  Tracks: TTrackInfoArray;
begin
  inherited;
  if HitInfo.HitNode <> nil then
  begin
    Tracks := GetSelected;
    if Length(Tracks) = 1 then
      FPopupMenu.FItemPlay.Click;
  end;
end;

procedure TSavedTree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = VK_DELETE then
  begin
    FPopupMenu.FItemDelete.Click;
  end;
end;

procedure TSavedTree.KeyPress(var Key: Char);
var
  Tracks: TTrackInfoArray;
begin
  inherited;

  if (Key = #13) or (Key = #32) then
  begin
    Key := #0;
    Tracks := GetSelected;
    if Length(Tracks) = 1 then
    begin
      FPopupMenu.FItemPlay.Click;
    end;
  end;
end;

procedure TSavedTree.MenuColsAction(Sender: TVirtualStringTree;
  Index: Integer; Checked: Boolean);
var
  Show: Boolean;
begin
  Show := True;
  if coVisible in Header.Columns[Index].Options then
    Show := False;

  if Show then
  begin
    Header.Columns[Index].Options := Header.Columns[Index].Options + [coVisible];
  end else
  begin
    Header.Columns[Index].Options := Header.Columns[Index].Options - [coVisible];
  end;

  AppGlobals.SavedCols := AppGlobals.SavedCols xor (1 shl Index);
end;

procedure TSavedTree.MessageReceived(Msg: TMessageBase);
begin
  if Msg is TFileModifyMsg then
  begin
    if LowerCase(FPlayer.Filename) = LowerCase(TFileModifyMsg(Msg).Filename) then
      FPlayer.Stop(True, True);
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
    Edit := TStringEditLink(EditLink).Edit;
    Edit.SelStart := 0;
    Edit.SelLength := Length(Edit.Text) - Length(ExtractFileExt(Edit.Text));
  end;
end;

{ TSearchBar }

constructor TSearchBar.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSearchBar.Destroy;
begin

  inherited;
end;

procedure TSearchBar.Setup;
begin
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Left := 0;
  FLabel.Top := 6;
  FLabel.Caption := _('Search:');

  FSearch := TEdit.Create(Self);
  FSearch.Parent := Self;
  FSearch.Left := FLabel.Left + FLabel.Width + 8;

  FSearch.Top := 3;
  FSearch.Width := 200;
  Height := FSearch.Top + FSearch.Height + FSearch.Top + 3;

  BevelOuter := bvNone;
end;

{ TImportFilesThread }

constructor TImportFilesThread.Create(Dir: string; KnownFiles: TStringList);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FDir := IncludeTrailingBackslash(Dir);
  FFiles := TList<TTrackInfo>.Create;
  FKnownFiles := KnownFiles;
end;

destructor TImportFilesThread.Destroy;
begin
  FFiles.Free;
  FKnownFiles.Free;

  inherited;
end;

procedure TImportFilesThread.Execute;
var
  i, n: Integer;
  Add: Boolean;
  FoundFiles, FoundAudioFiles: TStringList;
  Track: TTrackInfo;
  Info: TAudioFileInfo;
begin
  inherited;

  FoundFiles := TStringList.Create;
  FoundAudioFiles := TStringList.Create;
  try
    FindFiles(FDir + '*.*', FoundFiles, True, @Terminated);

    for i := 0 to FoundFiles.Count - 1 do
    begin
      if Terminated then
        Exit;

      if FiletypeToFormat(FoundFiles[i]) <> atNone then
      begin
        Add := True;
        for n := 0 to FKnownFiles.Count - 1 do
          if LowerCase(FKnownFiles[n]) = LowerCase(FoundFiles[i]) then
          begin
            Add := False;
            Break;
          end;

        if Add then
          FoundAudioFiles.Add(FoundFiles[i]);
      end;
    end;

    for i := 0 to FoundAudioFiles.Count - 1 do
    begin
      if Terminated then
        Exit;

      FCurrentFilename := ExtractFileName(FoundAudioFiles[i]);
      if Assigned(FOnProgress) then
        Synchronize(
          procedure
          begin
            if Assigned(FOnProgress) then
              FOnProgress(Self);
          end);

      Info := GetFileInfo(FoundAudioFiles[i]);

      if Info.Success then
      begin
        Track := TTrackInfo.Create;
        Track.Time := Now;
        Track.BitRate := Info.Bitrate;
        Track.Length := Trunc(Info.Length);
        Track.Filename := FoundAudioFiles[i];
        Track.Filesize := GetFileSize(FoundAudioFiles[i]);
        Track.VBR := Info.VBR;
        FFiles.Add(Track);

        FProgress := Trunc((i / FoundAudioFiles.Count) * 100);
        FCurrentFilename := ExtractFileName(FoundAudioFiles[i]);

        if Assigned(FOnProgress) then
          Synchronize(
            procedure
            begin
              if Assigned(FOnProgress) then
                FOnProgress(Self);
            end);
      end;
    end;
  finally
    FoundFiles.Free;
    FoundAudioFiles.Free;
  end;
end;

{ TImportPanel }

constructor TImportPanel.Create(AOwner: TComponent);
begin
  inherited;

  LabelFilename := TLabel.Create(Self);
  LabelFilename.Parent := Self;
  LabelFilename.AutoSize := False;
  LabelFilename.Alignment := taCenter;
  LabelFilename.Caption := _('Searching files...');

  ProgressBar := TProgressBar.Create(Self);
  ProgressBar.Parent := Self;
  ProgressBar.Style := pbstMarquee;

  Button := TButton.Create(Self);
  Button.Parent := Self;
  Button.Caption := _('Cancel');
end;

procedure TImportPanel.Resize;
begin
  inherited;

  LabelFilename.Width := ClientWidth - 8;
  LabelFilename.Top := 4;
  LabelFilename.Left := 4;

  Button.Width := 93;
  Button.Height := 25;
  Button.Top := ClientHeight - 4 - Button.Height;
  Button.Left := ClientWidth - 4 - Button.Width;

  ProgressBar.Width := ClientWidth - 8;
  ProgressBar.Top := Button.Top - 4 - ProgressBar.Height;
  ProgressBar.Left := 4;
end;

procedure TImportPanel.SetData(Progress: Integer; CurrentFilename: string);
var
  W: Integer;
begin
  if ProgressBar.Style <> pbstNormal then
    ProgressBar.Style := pbstNormal;

  W := GetTextSize('Importing ""', LabelFilename.Font).cx;
  LabelFilename.Caption := Format(_('Importing "%s"'), [TruncateText(CurrentFilename, LabelFilename.Width - W - 20, LabelFilename.Font)]);
  if ProgressBar.Position <> Progress then
  begin
    if Progress < 100 then
      ProgressBar.Position := Progress + 1;
    ProgressBar.Position := Progress;
  end;
end;

{ TPlayToolBar }

constructor TPlayToolBar.Create(AOwner: TComponent);
begin
  inherited;

  ShowHint := True;
  Transparent := True;
end;

procedure TPlayToolBar.EnableItems(Enable, Playing, IsFirst, IsLast: Boolean);
begin
  FPrev.Enabled := (not IsFirst) and Playing;
  FPlay.Enabled := Enable and Bass.DeviceAvailable;
  FPause.Enabled := Playing and Bass.DeviceAvailable;
  FStop.Enabled := Playing and Bass.DeviceAvailable;
  FNext.Enabled := (not IsLast) and Playing;
end;

procedure TPlayToolBar.Setup;
begin
  FNext := TToolButton.Create(Self);
  FNext.Parent := Self;
  FNext.Hint := 'Next';
  FNext.ImageIndex := 78;

  FStop := TToolButton.Create(Self);
  FStop.Parent := Self;
  FStop.Hint := 'Stop';
  FStop.ImageIndex := 1;

  FPause := TToolButton.Create(Self);
  FPause.Parent := Self;
  FPause.Hint := 'Pause';
  FPause.ImageIndex := 39;

  FPlay := TToolButton.Create(Self);
  FPlay.Parent := Self;
  FPlay.Hint := 'Play';
  FPlay.ImageIndex := 33;

  FPrev := TToolButton.Create(Self);
  FPrev.Parent := Self;
  FPrev.Hint := 'Previous';
  FPrev.ImageIndex := 79;
end;

end.
