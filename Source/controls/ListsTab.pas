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

{ This unit contains the TabControl used to show the wishlist/ignorelist }
unit ListsTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, MControls, LanguageObjects, Tabs, VirtualTrees, DataManager,
  ImgList, Functions, Menus, Math, DragDrop, DropComboTarget,
  Dialogs, MsgDlg, Forms, Logging, AppData, HomeCommunication, ICEClient,
  ClientManager, Generics.Collections, TypeDefs, MessageBus, AppMessages,
  Graphics, SharedData, HomeCommands, SharedControls, Images, ComboEx;

type
  TTitleTree = class;

  TNodeType = (ntWishParent, ntIgnoreParent, ntStream, ntWish, ntIgnore);
  TNodeTypes = set of TNodeType;

  TTitleNodeData = record
    Title: TTitleInfo;
    Stream: TICEClient;
    NodeType: TNodeType;
  end;
  PTitleNodeData = ^TTitleNodeData;

  TTitleDataArray = array of PTitleNodeData;

  TImportListEntry = class
    Title: string;
    Hash: Cardinal;
    IsArtist: Boolean;

    constructor Create(Title: string; Hash: Cardinal; IsArtist: Boolean);
  end;

  TTitlePopup = class(TPopupMenu)
  private
    FRemove: TMenuItem;
    FRename: TMenuItem;
    FConvertToAutomatic: TMenuItem;
    FShowSaved: TMenuItem;
    FSelectSaved: TMenuItem;
    FSelectIgnored: TMenuItem;
    FExport: TMenuItem;
    FImport: TMenuItem;
  protected
  public
    constructor Create(AOwner: TComponent); override;

    property Remove: TMenuItem read FRemove;
    property Rename: TMenuItem read FRename;
  end;

  TTitleToolbar = class(TToolbarForcedHorizontal)
  private
    FAdd: TToolButton;
    FRemove: TToolButton;
    FRename: TToolButton;
    FConvertToAutomatic: TToolButton;
    FShowSaved: TToolButton;
    FSelectSaved: TToolButton;
    FSelectIgnored: TToolButton;
    FExport: TToolButton;
    FImport: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TTitlePanel }

  TTitlePanel = class(TPanel)
  private
    FTopPanel: TPanel;
    FSearchPanel: TPanel;
    FSearchLabel: TLabel;
    FSearchText: TEdit;
    FToolbarPanel: TPanel;
    FAddLabel: TLabel;
    FAddEdit: TEdit;
    FAddCombo: TComboBoxEx;
    FToolbar: TTitleToolbar;
    FTree: TTitleTree;

    FClientManager: TClientManager;
    FFilterText: string;

    procedure BuildTree(FromFilter: Boolean);
    procedure UpdateButtons;
    procedure FillClientCombo;

    procedure AddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure RenameClick(Sender: TObject);
    procedure ShowSavedClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure ImportClick(Sender: TObject);
    procedure SelectSavedClick(Sender: TObject);
    procedure SelectIgnoredClick(Sender: TObject);
    procedure ConvertToAutomaticClick(Sender: TObject);
    procedure AddEditKeyPress(Sender: TObject; var Key: Char);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchTextChange(Sender: TObject);
  protected
    procedure ControlsAligned; override;
  public
    constructor Create(AOwner: TComponent; Clients: TClientManager); reintroduce;

    procedure PostTranslate;
    function AddEntry(Text: string; TitleHash: Cardinal; ShowMessages: Boolean; ListType: TListType): Boolean;
    procedure RemoveEntry(Text: string; ServerTitleHash: Cardinal; ListType: TListType);
    procedure ClientAdded(Client: TICEClient);
    procedure ClientRemoved(Client: TICEClient);
    procedure UpdateList;

    property Tree: TTitleTree read FTree;
  end;

  { TListsTab }

  TListsTab = class(TMainTabSheet)
  private
    FListsPanel: TTitlePanel;

    procedure MessageReceived(Msg: TMessageBase);

    procedure HomeCommConvertManualToAutomaticReceived(Sender: TObject; FoundTitles: TConvertManualToAutomaticArray; NotFoundTitles: TStringArray);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; Clients: TClientManager); reintroduce;
    destructor Destroy; override;

    procedure AddTitle(Client: TICEClient; ListType: TListType; Title: TTitleInfo);
    procedure RemoveTitle(Client: TICEClient; ListType: TListType; Title: TTitleInfo);

    procedure AddClient(Client: TICEClient);
    procedure RemoveClient(Client: TICEClient);

    procedure UpdateLists;
    procedure PostTranslate;

    property ListsPanel: TTitlePanel read FListsPanel;
  end;

  TTitleTree = class(TVirtualStringTree)
  private
    FColTitle: TVirtualTreeColumn;
    FColSaved: TVirtualTreeColumn;
    FColAdded: TVirtualTreeColumn;

    FHeaderDragSourcePosition: Cardinal;

    FPanel: TTitlePanel;

    FDropTarget: TDropComboTarget;
    FPopupMenu: TTitlePopup;
    FWishNode: PVirtualNode;
    FIgnoreNode: PVirtualNode;

    function GetNode(Stream: TICEClient): PVirtualNode;

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);

    procedure PopupMenuClick(Sender: TObject);

    procedure FitColumns;
    procedure MenuColsAction(Sender: TVirtualStringTree; Index: Integer; Checked: Boolean);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    function DoHeaderDragging(Column: TColumnIndex): Boolean; override;
    procedure DoHeaderDragged(Column: TColumnIndex; OldPosition: TColumnPosition); override;
    function DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean; override;
    procedure DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    function AddTitle(Title: TTitleInfo; Parent: PVirtualNode; FilterText: string; FromFilter: Boolean): PVirtualNode;
    procedure RemoveTitle(Title: TTitleInfo);
    procedure RemoveClient(Client: TICEClient);
    procedure SortItems;

    procedure PostTranslate;

    function GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
    function NodesToData(Nodes: TNodeArray): TTitleDataArray;
  end;

const
  WISHTEXT = 'Wishlist';
  IGNORETEXT = 'Ignorelist';
  
implementation

{ TListsTab }

constructor TListsTab.Create(AOwner: TComponent; Clients: TClientManager);
begin
  inherited Create(AOwner);

  ShowCloseButton := False;
  ImageIndex := TImages.SCRIPT_EDIT;

  FListsPanel := TTitlePanel.Create(Self, Clients);
  FListsPanel.Parent := Self;
  FListsPanel.Align := alClient;

  MsgBus.AddSubscriber(MessageReceived);

  HomeComm.OnConvertManualToAutomaticReceived := HomeCommConvertManualToAutomaticReceived;

  Caption := 'Lists';
end;

destructor TListsTab.Destroy;
begin
  MsgBus.RemoveSubscriber(MessageReceived);

  inherited;
end;

procedure TListsTab.HomeCommConvertManualToAutomaticReceived(
  Sender: TObject; FoundTitles: TConvertManualToAutomaticArray;
  NotFoundTitles: TStringArray);
var
  i, n: Integer;
  Found: Boolean;
  Title: TTitleInfo;
  Hashes: TSyncWishlistRecordArray;
begin
  SetLength(Hashes, 0);

  for i := 0 to High(FoundTitles) do
  begin
    // If a manual title already exists the manual title needs to be removed
    for n := 0 to AppGlobals.Data.SaveList.Count - 1 do
    begin
      if (LowerCase(FoundTitles[i].Title) = LowerCase(AppGlobals.Data.SaveList[n].Title)) and
         (AppGlobals.Data.SaveList[n].ServerHash = 0) and (AppGlobals.Data.SaveList[n].ServerArtistHash = 0) then
      begin
        FListsPanel.FTree.RemoveTitle(AppGlobals.Data.SaveList[n]);
        AppGlobals.Data.SaveList[n].Free;
        AppGlobals.Data.SaveList.Delete(n);
        Break;
      end;
    end;

    Found := False;
    for n := 0 to AppGlobals.Data.SaveList.Count - 1 do
      if AppGlobals.Data.SaveList[n].ServerHash = FoundTitles[i].Hash then
      begin
        Found := True;
        Break;
      end;

    if Found then
      Continue;

    Title := TTitleInfo.Create(FoundTitles[i].Hash, 0, FoundTitles[i].Title);
    AppGlobals.Data.SaveList.Add(Title);
    AddTitle(nil, ltSave, Title);

    SetLength(Hashes, Length(Hashes) + 1);
    Hashes[High(Hashes)] := TSyncWishlistRecord.Create(FoundTitles[i].Hash, False);
  end;

  if Length(Hashes) > 0 then
  begin
    HomeComm.SendSyncWishlist(swAdd, Hashes);
    HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
    MsgBus.SendMessage(TListsChangedMsg.Create);
  end;

  if Length(NotFoundTitles) = 0 then
    MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llInfo, _('Server'), Format(_('Conversion to automatic wishlist titles succeeded'), [Length(FoundTitles), Length(NotFoundTitles)])))
  else if Length(FoundTitles) = 0 then
    MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llInfo, _('Server'), Format(_('Conversion to automatic wishlist titles failed'), [Length(FoundTitles), Length(NotFoundTitles)])))
  else
    MsgBus.SendMessage(TLogMsg.Create(Self, lsHome, ltGeneral, llInfo, _('Server'), Format(_('Conversion to automatic wishlist titles succeeded for %d title(s), failed for %d title(s)'), [Length(FoundTitles), Length(NotFoundTitles)])));
end;

procedure TListsTab.MessageReceived(Msg: TMessageBase);
var
  i: Integer;
  TitleUpdated, ArtistUpdated: Boolean;
  Nodes: TNodeArray;
  SongSavedMsg: TSongSavedMsg absolute Msg;
begin
  if Msg is TSongSavedMsg then
  begin
    // Der Saved-Counter ist nur für Wunschlisteneinträge mit Hash vorhanden, ansonsten raus hier.
    if SongSavedMsg.ServerTitleHash = 0 then
      Exit;

    TitleUpdated := False;
    ArtistUpdated := False;

    for i := 0 to AppGlobals.Data.SaveList.Count - 1 do
    begin
      if (AppGlobals.Data.SaveList[i].ServerHash > 0) and
         (AppGlobals.Data.SaveList[i].ServerHash = SongSavedMsg.ServerTitleHash) then
      begin
        AppGlobals.Data.SaveList[i].Saved := AppGlobals.Data.SaveList[i].Saved + 1;
        TitleUpdated := True;
      end;

      if (AppGlobals.Data.SaveList[i].ServerArtistHash > 0) and
         (AppGlobals.Data.SaveList[i].ServerHash = 0) and
         (AppGlobals.Data.SaveList[i].ServerArtistHash = SongSavedMsg.ServerArtistHash) then
      begin
        AppGlobals.Data.SaveList[i].Saved := AppGlobals.Data.SaveList[i].Saved + 1;
        ArtistUpdated := True;
      end;

      if TitleUpdated and ArtistUpdated then
        Break;
    end;

    Nodes := FListsPanel.FTree.GetNodes([ntWish], False);
    for i := 0 to High(Nodes) do
      FListsPanel.FTree.InvalidateNode(Nodes[i]);
  end;
end;

procedure TListsTab.PostTranslate;
begin
  FListsPanel.PostTranslate;
end;

procedure TListsTab.Resize;
begin
  inherited;

end;

procedure TListsTab.UpdateLists;
begin
  FListsPanel.UpdateList;
end;

procedure TListsTab.AddClient(Client: TICEClient);
begin
  FListsPanel.ClientAdded(Client);
end;

procedure TListsTab.RemoveClient(Client: TICEClient);
begin
  FListsPanel.ClientRemoved(Client);
end;

procedure TListsTab.RemoveTitle(Client: TICEClient; ListType: TListType;
  Title: TTitleInfo);
begin
  FListsPanel.FTree.RemoveTitle(Title);
end;

procedure TListsTab.AddTitle(Client: TICEClient; ListType: TListType; Title: TTitleInfo);
begin
  if ListType = ltSave then
    FListsPanel.FTree.AddTitle(Title, FListsPanel.FTree.FWishNode, FListsPanel.FFilterText, True)
  else if Client <> nil then
    FListsPanel.FTree.AddTitle(Title, FListsPanel.FTree.GetNode(Client), FListsPanel.FFilterText, True)
  else
    FListsPanel.FTree.AddTitle(Title, FListsPanel.FTree.FIgnoreNode, FListsPanel.FFilterText, True);

  FListsPanel.UpdateButtons;
end;

{ TTitlePanel }

procedure TTitlePanel.AddClick(Sender: TObject);
begin
  if AddEntry(FAddEdit.Text, 0, True, ltAutoDetermine) then
  begin
    FAddEdit.Text := '';

    MsgBus.SendMessage(TListsChangedMsg.Create);
  end;
end;

procedure TTitlePanel.RemoveClick(Sender: TObject);
var
  i: Integer;
  Hashes: TSyncWishlistRecordArray;
  Node, SubNode, DeleteNode: PVirtualNode;
  NodeData: PTitleNodeData;
  DeleteList: TList<PVirtualNode>;
begin
  FTree.BeginUpdate;

  SetLength(Hashes, 0);

  DeleteList := TList<PVirtualNode>.Create;
  try
    Node := FTree.GetFirst;
    while Node <> nil do
    begin
      if FTree.Selected[Node] then
      begin
        NodeData := FTree.GetNodeData(Node);

        if not (NodeData.NodeType in [ntWishParent, ntIgnoreParent])  then
        begin
          SubNode := FTree.GetFirstChild(Node);
          while SubNode <> nil do
          begin
            if not DeleteList.Contains(SubNode) then
              DeleteList.Add(SubNode);
            SubNode := FTree.GetNextSibling(SubNode);
          end;
        end;

        if (not DeleteList.Contains(Node)) and (NodeData.NodeType <> ntWishParent) and (NodeData.NodeType <> ntIgnoreParent) then
          DeleteList.Add(Node);
      end;
      Node := FTree.GetNext(Node);
    end;

    for i := DeleteList.Count - 1 downto 0 do
    begin
      NodeData := FTree.GetNodeData(DeleteList[i]);
      if (FTree.ChildCount[DeleteList[i]] = 0) and (NodeData.Title <> nil) then
      begin
        case NodeData.NodeType of
          ntWish:
            begin
              AppGlobals.Data.SaveList.Remove(NodeData.Title);
              if NodeData.Title.ServerHash > 0 then
              begin
                SetLength(Hashes, Length(Hashes) + 1);
                Hashes[High(Hashes)] := TSyncWishlistRecord.Create(NodeData.Title.ServerHash, False);
              end else if NodeData.Title.ServerArtistHash > 0 then
              begin
                SetLength(Hashes, Length(Hashes) + 1);
                Hashes[High(Hashes)] := TSyncWishlistRecord.Create(NodeData.Title.ServerArtistHash, True);
              end;
            end;
          ntIgnore:
            if NodeData.Stream = nil then
              AppGlobals.Data.IgnoreList.Remove(NodeData.Title)
            else
              NodeData.Stream.Entry.IgnoreList.Remove(NodeData.Title);
        end;

        if NodeData.Title <> nil then
          NodeData.Title.Free;
        FTree.DeleteNode(DeleteList[i]);
        DeleteList.Delete(i);
      end;
    end;

    Node := FTree.GetLast;
    while Node <> nil do
    begin
      DeleteNode := nil;
      NodeData := FTree.GetNodeData(Node);
      if (FTree.GetFirstChild(Node) = nil) and (NodeData.Title = nil) and (Node <> FTree.FWishNode) and (Node <> FTree.FIgnoreNode) then
        DeleteNode := Node;
      Node := FTree.GetPrevious(Node);
      if DeleteNode <> nil then
        FTree.DeleteNode(DeleteNode);
    end;
  finally
    DeleteList.Free;
  end;

  if Length(Hashes) > 0 then
  begin
    HomeComm.SendSyncWishlist(swRemove, Hashes);
    HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
    MsgBus.SendMessage(TListsChangedMsg.Create);
  end;

  FTree.EndUpdate;
end;

procedure TTitlePanel.RemoveEntry(Text: string; ServerTitleHash: Cardinal;
  ListType: TListType);
var
  i: Integer;
  Title: TTitleInfo;
  List: TList<TTitleInfo>;
begin
  case ListType of
    ltSave:
      List := AppGlobals.Data.SaveList;
    ltIgnore:
      List := AppGlobals.Data.IgnoreList;
    else
      Exit;
  end;

  Title := nil;
  Text := LowerCase(Text);

  if ServerTitleHash > 0 then
    for i := 0 to List.Count - 1 do
      if List[i].ServerHash = ServerTitleHash then
      begin
        Title := List[i];
        Break;
      end;

  if Title = nil then
    for i := 0 to List.Count - 1 do
      if LowerCase(List[i].Title) = Text then
      begin
        Title := List[i];
        Break;
      end;

  if Title <> nil then
  begin
    FTree.RemoveTitle(Title);
    List.Remove(Title);

    if (Title.ServerHash > 0) and (ListType = ltSave) then
      HomeComm.SendSyncWishlist(swRemove, Title.ServerHash, False);
  end;
end;

procedure TTitlePanel.RenameClick(Sender: TObject);
begin
  FTree.EditNode(FTree.GetFirstSelected, 0);
end;

procedure TTitlePanel.ExportClick(Sender: TObject);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Dlg: TSaveDialog;
  Lst: TStringList;
  ExportList: TList<TTitleInfo>;
begin
  Node := FTree.GetFirstSelected;

  ExportList := TList<TTitleInfo>.Create;
  try
    if (Node <> nil) then
    begin
      NodeData := FTree.GetNodeData(Node);
      if NodeData.NodeType in [ntWishParent, ntIgnoreParent, ntStream] then
      begin
        // Alle Untereinträge der Kategorie exportieren
        Node := FTree.GetFirstChild(Node);
        while Node <> nil do
        begin
          NodeData := FTree.GetNodeData(Node);
          if NodeData.Title <> nil then
          begin
            ExportList.Add(NodeData.Title);
          end;
          Node := FTree.GetNextSibling(Node);
        end;
      end else
      begin
        while Node <> nil do
        begin
          NodeData := FTree.GetNodeData(Node);
          if FTree.Selected[Node] and (NodeData.Title <> nil) then
          begin
            ExportList.Add(NodeData.Title);
          end;
          Node := FTree.GetNext(Node);
        end;
      end;
    end;

    if ExportList.Count = 0 then
    begin
      MsgBox(GetParentForm(Self).Handle, _('Please select at least one title or a single category containing titles to export.'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

    Dlg := TSaveDialog.Create(Self);
    try
      Dlg.Filter := _('Text files') + ' (*.txt)|*.txt';
      Dlg.Options := Dlg.Options + [ofOverwritePrompt];
      Dlg.DefaultExt := '.txt';
      {
      if Dlg.Execute(Handle) then
      begin
        Lst := TStringList.Create;
        try
          for i := 0 to ExportList.Count - 1 do
          begin
            if ExportList[i].ServerHash > 0 then
              Lst.Add(ExportList[i].Title + '|' + IntToStr(ExportList[i].ServerHash))
            else if ExportList[i].ServerArtistHash > 0 then
              Lst.Add(ExportList[i].Title + '|A' + IntToStr(ExportList[i].ServerArtistHash))
            else
              Lst.Add(ExportList[i].Title);
          end;
          try
            Lst.SaveToFile(Dlg.FileName);
          except
            MsgBox(GetParentForm(Self).Handle, _('The file could not be saved.'), _('Error'), MB_ICONEXCLAMATION);
          end;
        finally
          Lst.Free;
        end;
      end;
      }
    finally
      Dlg.Free;
    end;
  finally
    ExportList.Free;
  end;
end;

procedure TTitlePanel.FillClientCombo;
var
  i: Integer;
  O: TObject;
  Item: TComboExItem;
begin
  O := nil;
  if FAddCombo.ItemIndex > -1 then
    O := FAddCombo.ItemsEx[FAddCombo.ItemIndex].Data;

  FAddCombo.ItemsEx.Clear;

  for i := 0 to FClientManager.Count - 1 do
    if FClientManager[i].Entry.CustomName <> '' then
      FAddCombo.ItemsEx.AddItem(FClientManager[i].Entry.CustomName, TImages.TRANSMIT, -1, -1, 16, FClientManager[i]);

  // TODO: ...
  //FAddCombo.Sorted := True;
  //FAddCombo.Sorted := False;

  Item := FAddCombo.ItemsEx.Insert(0);
  Item.Caption := _('Ignorelist');
  Item.ImageIndex := TImages.SCRIPT_DECLINE;

  Item := FAddCombo.ItemsEx.Insert(0);
  Item.Caption := _('Wishlist');
  Item.ImageIndex := TImages.SCRIPT_HEART;

  if O <> nil then
  begin
    for i := 0 to FAddCombo.ItemsEx.Count - 1 do
      if FAddCombo.ItemsEx[i].Data = O then
      begin
        FAddCombo.ItemIndex := i;
        Break;
      end;
  end else
    FAddCombo.ItemIndex := 0;
end;

procedure TTitlePanel.ImportClick(Sender: TObject);
var
  i, n, P, NumChars, MsgRes: Integer;
  Hash, ServerHash, ServerArtistHash: Cardinal;
  Exists, UseTitleInfo, Deleted, Skip: Boolean;
  Pattern, Ext: string;
  Dlg: TOpenDialog;
  Lst: TStringList;
  Title: TTitleInfo;
  List: TList<TTitleInfo>;
  ParentNode: PVirtualNode;
  ImportData, NewImportData: TList<TImportListEntry>;
  Hashes: TSyncWishlistRecordArray;
  ConversionData: TStringList;
  KeepEntry: TImportListEntry;
  TitleInfo: TTitleInfo;
begin
  if FAddCombo.ItemIndex = 0 then
  begin
    List := AppGlobals.Data.SaveList;
    ParentNode := FTree.FWishNode;
  end else if FAddCombo.ItemIndex = 1 then
  begin
    List := AppGlobals.Data.IgnoreList;
    ParentNode := FTree.FIgnoreNode;
  end else
  begin
    List := TICEClient(FAddCombo.ItemsEx[FAddCombo.ItemIndex].Data).Entry.IgnoreList;
    ParentNode := nil;
  end;

  ImportData := TList<TImportListEntry>.Create;
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Filter := _('All supported types') + ' (*.txt, *.m3u, *.pls)|*.txt;*.m3u;*.pls|' +  _('Text files') + ' (*.txt)|*.txt|' + _('M3U playlists') + ' (*.m3u)|*.m3u|' + _('PLS playlists') + ' (*.pls)|*.pls';
    {
    if Dlg.Execute(Handle) then
    begin
      Lst := TStringList.Create;
      try
        try
          Lst.LoadFromFile(Dlg.FileName);

          UseTitleInfo := False;
          for i := 0 to Lst.Count - 1 do
            if (LowerCase(Copy(Lst[i], 1, 5)) = 'title') and (Pos('=', Lst[i]) > 0) then
            begin
              UseTitleInfo := True;
              Break;
            end;

          for i := 0 to Lst.Count - 1 do
          begin
            if ParentNode = nil then
            begin
              ParentNode := FTree.GetNode(TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]));
            end;

            Lst[i] := Trim(Lst[i]);

            if Lst[i] = '' then
              Continue;

            Ext := LowerCase(ExtractFileExt(Dlg.FileName));

            if Ext = '.m3u' then
              if Lst[i][1] = '#' then
                Continue;

            if Ext = '.pls' then
            begin
              if (UseTitleInfo and (LowerCase(Copy(Lst[i], 1, 5)) = 'title')) or
                 (not UseTitleInfo and (LowerCase(Copy(Lst[i], 1, 4)) = 'file')) then
              begin
                n := Pos('=', Lst[i]);
                if (n > 0) and (Length(Lst[i]) > n) then
                  Lst[i] := Copy(Lst[i], n + 1, Length(Lst[i]) - n);
              end else
                Continue;
            end;

            // Wenn es ein ganzer Pfad sein könnte bearbeiten
            if Length(Lst[i]) > 4 then
              if (Copy(Lst[i], 2, 2) = ':\') and (Pos('\', Lst[i]) > -1) and (Pos('.', Lst[i]) > -1) then
              begin
                Lst[i] := ExtractFileName(Lst[i]);
                Lst[i] := RemoveFileExt(Lst[i]);
              end;

            // Wenn ein Künstler Künstler-Hash hinten dran ist auswerten
            ServerArtistHash := 0;
            P := RPos('|A', Lst[i]);
            if P > -1 then
            begin
              ServerArtistHash := StrToIntDef(Copy(Lst[i], P + 2, Length(Lst[i]) - P), 0);
              Lst[i] := Copy(Lst[i], 1, P - 1);
            end;

            // Wenn ein Hash hinten dran ist auswerten
            ServerHash := 0;
            P := RPos('|', Lst[i]);
            if P > -1 then
            begin
              ServerHash := StrToIntDef(Copy(Lst[i], P + 1, Length(Lst[i]) - P), 0);
              Lst[i] := Copy(Lst[i], 1, P - 1);
            end;

            // Das hier darf nicht sein, könnte aber passieren
            if (ServerHash > 0) and (ServerArtistHash > 0) then
            begin
              ServerHash := 0;
              ServerArtistHash := 0;
            end;

            Pattern := BuildPattern(Lst[i], Hash, NumChars, False);
            if NumChars <= 3 then
              Continue;

            if (ServerHash > 0) or ((ServerHash = 0) and (ServerArtistHash = 0)) then
              ImportData.Add(TImportListEntry.Create(Lst[i], ServerHash, False))
            else
              ImportData.Add(TImportListEntry.Create(Lst[i], ServerArtistHash, True));
          end;
        except
          MsgBox(GetParentForm(Self).Handle, _('The file could not be loaded.'), _('Error'), MB_ICONEXCLAMATION);
          Exit;
        end;
      finally
        Lst.Free;
      end;
    end;
    }

    // When not importing into the wishlist no artists or hashes are allowed, so rebuild the list
    if List <> AppGlobals.Data.SaveList then
    begin
      NewImportData := TList<TImportListEntry>.Create;
      for i := 0 to ImportData.Count - 1 do
      begin
        if (not ImportData[i].IsArtist) then
        begin
          ImportData[i].Hash := 0;
          NewImportData.Add(ImportData[i]);
        end else
          ImportData[i].Free;
      end;

      ImportData.Free;
      ImportData := NewImportData;
    end;

    // Now remove duplicates. Titles with hashes have higher priority than titles without hashes
    NewImportData := TList<TImportListEntry>.Create;
    while ImportData.Count > 0 do
    begin
      KeepEntry := ImportData[0];

      for n := 0 to ImportData.Count - 1 do
      begin
        if KeepEntry = ImportData[n] then
          Continue;

        if LowerCase(KeepEntry.Title) = LowerCase(ImportData[n].Title) then
        begin
          if (KeepEntry.Hash = 0) and (ImportData[n].Hash > 0) then
            KeepEntry := ImportData[n];
        end;
      end;

      for n := 0 to List.Count - 1 do
      begin
        TitleInfo := List[n];

        if List = AppGlobals.Data.SaveList then
        begin
          if ((LowerCase(KeepEntry.Title) = LowerCase(TitleInfo.Title)) and (not KeepEntry.IsArtist) and (KeepEntry.Hash = 0) and (TitleInfo.ServerHash > 0)) or
             ((not KeepEntry.IsArtist) and (KeepEntry.Hash > 0) and (KeepEntry.Hash = TitleInfo.ServerHash)) or
             ((KeepEntry.IsArtist) and (TitleInfo.ServerArtistHash > 0) and (LowerCase(KeepEntry.Title) = LowerCase(TitleInfo.Title))) or
             ((KeepEntry.IsArtist) and (KeepEntry.Hash > 0) and (KeepEntry.Hash = TitleInfo.ServerArtistHash)) then
          begin
            KeepEntry := nil;
            Break;
          end;
        end else
        begin
          if LowerCase(KeepEntry.Title) = LowerCase(TitleInfo.Title) then
          begin
            KeepEntry := nil;
            Break;
          end;
        end;
      end;

      if KeepEntry <> nil then
      begin
        NewImportData.Add(KeepEntry);

        for n := ImportData.Count - 1 downto 0 do
          if (LowerCase(KeepEntry.Title) = LowerCase(ImportData[n].Title)) then
          begin
            if KeepEntry <> ImportData[n] then
              ImportData[n].Free;
            ImportData.Delete(n);
          end;
      end else
      begin
        ImportData[0].Free;
        ImportData.Delete(0);
      end;
    end;
    ImportData.Free;
    ImportData := NewImportData;


    if ImportData.Count > 0 then
    begin
      if List = AppGlobals.Data.SaveList then
      begin
        ConversionData := TStringList.Create;
        try
          // Create a separate list for titles without hashes
          for i := 0 to ImportData.Count - 1 do
            if ImportData[i].Hash = 0 then
              ConversionData.Add(ImportData[i].Title);

          {
          if ConversionData.Count > 0 then
          begin
            // If there are manual titles ask the user if they should be converted to automatic titles
            if not HomeComm.CommunicationEstablished then
            begin
               MsgRes := MsgBox(GetParentForm(Self).Handle, Format(_('You have imported %d title(s) for the manual wishlist. You are not connected to the streamWriter server to convert these titles into titles for the automatic wishlist. Do you want to continue and import these titles as manual titles without conversion?'), [ConversionData.Count]), _('Question'), MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2);
               if MsgRes = ID_NO then
                 Exit;
            end else
            begin
              MsgRes := MsgBox(GetParentForm(Self).Handle, Format(_('You have imported %d title(s) for the manual wishlist. Do you want to convert these titles into titles used by the automatic wishlist?'), [ConversionData.Count]), _('Question'), MB_YESNOCANCEL or MB_ICONQUESTION);
              case MsgRes of
                ID_YES:
                  begin
                    // We need to build a separate list to send to the server for conversion.
                    // The stuff we send to the server will be removed from the list of titles we will add soon.
                    NewImportData := TList<TImportListEntry>.Create;
                    for i := ImportData.Count - 1 downto 0 do
                    begin
                      // If it is an automatic title keep it, otherwise add it to the other list for conversion
                      if ImportData[i].Hash <> 0 then
                        NewImportData.Add(ImportData[i])
                      else
                        ImportData[i].Free;
                    end;

                    ImportData.Free;
                    ImportData := NewImportData;

                    HomeComm.SendConvertManualToAutomatic(ConversionData);
                  end;
                ID_CANCEL:
                  Exit;
              end;
            end;
          end;
          }
        finally
          ConversionData.Free;
        end;
      end;

      SetLength(Hashes, 0);
      for i := 0 to ImportData.Count - 1 do
      begin
        Skip := False;

        // If we are importing an automatic title and a manual title already exists the manual title needs to be removed
        if List = AppGlobals.Data.SaveList then
        begin
          Pattern := BuildPattern(Trim(ImportData[i].Title), Hash, NumChars, False);

          for n := 0 to List.Count - 1 do
          begin
            if (LowerCase(ImportData[i].Title) = LowerCase(List[n].Title)) and (ImportData[i].Hash > 0) and (not ImportData[i].IsArtist) and
               (List[n].ServerHash = 0) and (List[n].ServerArtistHash = 0) then
            begin
              FTree.RemoveTitle(List[n]);
              List[n].Free;
              List.Delete(n);
              Break;
            end;

            // Do not allow duplicate manual wishlist entries
            if (ImportData[i].Hash = 0) and (List[n].Hash = Hash) then
            begin
              Skip := True;
              Break;
            end;
          end;
        end;

        if Skip then
          Continue;

        if ImportData[i].Hash = 0 then
          Title := TTitleInfo.Create(0, 0, ImportData[i].Title)
        else if ImportData[i].IsArtist then
        begin
          Title := TTitleInfo.Create(0, ImportData[i].Hash, ImportData[i].Title);
          SetLength(Hashes, Length(Hashes) + 1);
          Hashes[High(Hashes)] := TSyncWishlistRecord.Create(ImportData[i].Hash, True);
        end else
        begin
          Title := TTitleInfo.Create(ImportData[i].Hash, 0, ImportData[i].Title);
          SetLength(Hashes, Length(Hashes) + 1);
          Hashes[High(Hashes)] := TSyncWishlistRecord.Create(ImportData[i].Hash, False);
        end;

        List.Add(Title);
        FTree.AddTitle(Title, ParentNode, FFilterText, True);
      end;

      if (List = AppGlobals.Data.SaveList) and (Length(Hashes) > 0) then
      begin
        HomeComm.SendSyncWishlist(swAdd, Hashes);
        HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);
        MsgBus.SendMessage(TListsChangedMsg.Create);
      end;
    end;
  finally
    for i := 0 to ImportData.Count - 1 do
      ImportData[i].Free;
    ImportData.Free;
    Dlg.Free;
  end;
end;

procedure TTitlePanel.PostTranslate;
begin
  FTree.PostTranslate;
  FillClientCombo;
end;

procedure TTitlePanel.BuildTree(FromFilter: Boolean);
var
  i, n: Integer;
  Node, ClientNode: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  FTree.BeginUpdate;
  try
    FTree.Clear;

    FTree.FWishNode := FTree.AddChild(nil);
    NodeData := FTree.GetNodeData(FTree.FWishNode);
    NodeData.NodeType := ntWishParent;

    FTree.FIgnoreNode := FTree.AddChild(nil);
    NodeData := FTree.GetNodeData(FTree.FIgnoreNode);
    NodeData.NodeType := ntIgnoreParent;

    for i := 0 to AppGlobals.Data.SaveList.Count - 1 do
      FTree.AddTitle(AppGlobals.Data.SaveList[i], FTree.FWishNode, FFilterText, FromFilter);

    for i := 0 to AppGlobals.Data.IgnoreList.Count - 1 do
      FTree.AddTitle(AppGlobals.Data.IgnoreList[i], FTree.FIgnoreNode, FFilterText, FromFilter);

    for i := 0 to FClientManager.Count - 1 do
    begin
      if FClientManager[i].Entry.IgnoreList.Count > 0 then
      begin
        ClientNode := FTree.GetNode(FClientManager[i]);
        for n := 0 to FClientManager[i].Entry.IgnoreList.Count - 1 do
          FTree.AddTitle(FClientManager[i].Entry.IgnoreList[n], ClientNode, FFilterText, FromFilter);
      end;
    end;

    Node := FTree.GetFirst;
    while Node <> nil do
    begin
      FTree.Expanded[Node] := True;
      Node := FTree.GetNext(Node);
    end;
  finally
    FTree.EndUpdate;
  end;

  if not FromFilter then
    FTree.Header.SortColumn := -1;

  FTree.SortItems;
end;

procedure TTitlePanel.ClientAdded(Client: TICEClient);
var
  i: Integer;
begin
  if (Client.AutoRemove) or (Client.Entry.CustomName = '') then
    Exit;

  for i := 0 to FAddCombo.ItemsEx.Count - 1 do
    if FAddCombo.ItemsEx[i].Data = Client then
      Exit;

  FillClientCombo;
end;

procedure TTitlePanel.ClientRemoved(Client: TICEClient);
var
  i: Integer;
begin
  FTree.RemoveClient(Client);
  for i := 0 to FAddCombo.ItemsEx.Count - 1 do
    if FAddCombo.ItemsEx[i].Data = Client then
    begin
      if FAddCombo.ItemIndex = i then
        FAddCombo.ItemIndex := 0;
      FAddCombo.ItemsEx.Delete(i);
      Exit;
    end;
end;

constructor TTitlePanel.Create(AOwner: TComponent; Clients: TClientManager);
begin
  inherited Create(AOwner);

  FClientManager := Clients;

  BevelOuter := bvNone;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Align := alTop;
  FTopPanel.AutoSize := True;
  FTopPanel.Top := -100;

  FSearchPanel := TPanel.Create(Self);
  FSearchPanel.Parent := Self;
  FSearchPanel.BevelOuter := bvNone;
  FSearchPanel.Align := alTop;
  FSearchPanel.AutoSize := True;

  FSearchLabel := TLabel.Create(Self);
  FSearchLabel.Parent := FSearchPanel;
  FSearchLabel.Align := alLeft;
  FSearchLabel.Layout := tlCenter;
  FSearchLabel.Caption := 'Search:';
  FSearchLabel.Left := -1;

  FSearchText := TEdit.Create(Self);
  FSearchText.Parent := FSearchPanel;
  FSearchText.Align := alLeft;
  FSearchText.Width := 200;
  FSearchText.OnChange := SearchTextChange;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Parent := FTopPanel;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Align := alTop;
  FToolbarPanel.AutoSize := True;

  FAddLabel := TLabel.Create(Self);
  FAddLabel.Align := alLeft;
  FAddLabel.Layout := tlCenter;
  FAddLabel.Parent := FToolbarPanel;
  FAddLabel.Caption := 'Add entry:';
  FAddLabel.Left := -1;

  FAddEdit := TEdit.Create(Self);
  FAddEdit.Align := alLeft;
  FAddEdit.Parent := FToolbarPanel;
  FAddEdit.Width := 200;
  FAddEdit.OnKeyPress := AddEditKeyPress;

  FAddCombo := TComboBoxEx.Create(Self);
  FAddCombo.Parent := FToolbarPanel;
  FAddCombo.Align := alClient;
  FAddCombo.Images := modSharedData.imgImages;

  FToolbar := TTitleToolbar.Create(Self);
  FToolbar.Images := modSharedData.imgImages;
  FToolbar.Align := alRight;
  FToolbar.Parent := FToolbarPanel;
  FToolbar.FAdd.OnClick := AddClick;
  FToolbar.FRemove.OnClick := RemoveClick;
  FToolbar.FExport.OnClick := ExportClick;
  FToolbar.FShowSaved.OnClick := ShowSavedClick;
  FToolbar.FImport.OnClick := ImportClick;
  FToolbar.FSelectSaved.OnClick := SelectSavedClick;
  FToolbar.FSelectIgnored.OnClick := SelectIgnoredClick;
  FToolbar.FRename.OnClick := RenameClick;
  FToolbar.FConvertToAutomatic.OnClick := ConvertToAutomaticClick;

  FTree := TTitleTree.Create(Self);
  FTree.Parent := Self;
  FTree.Align := alClient;
  FTree.OnChange := TreeChange;
  FTree.OnKeyDown := TreeKeyDown;

  BuildTree(False);
  FillClientCombo;

  UpdateButtons;
end;

procedure TTitlePanel.SearchTextChange(Sender: TObject);
var
  Hash: Cardinal;
  NumChars: Integer;
begin
  FFilterText := BuildPattern(FSearchText.Text, Hash, NumChars, False);
  BuildTree(True);
end;

procedure TTitlePanel.ControlsAligned;
begin
  inherited ControlsAligned;

  if FAddLabel.Width > FSearchLabel.Width then
  begin
    FAddLabel.BorderSpacing.Right := 0;
    FSearchLabel.BorderSpacing.Right := FAddLabel.Width - FSearchLabel.Width
  end else
  begin
    FSearchLabel.BorderSpacing.Right := 0;
    FAddLabel.BorderSpacing.Right := FSearchLabel.Width - FAddLabel.Width;
  end;
end;

procedure TTitlePanel.SelectIgnoredClick(Sender: TObject);
var
  WishNode, IgnoreNode: PVirtualNode;
  WishNodeData, IgnoreNodeData: PTitleNodeData;
begin
  WishNode := FTree.GetFirst;
  while WishNode <> nil do
  begin
    FTree.Selected[WishNode] := False;
    WishNode := FTree.GetNext(WishNode);
  end;

  WishNode := FTree.GetFirstChild(FTree.FWishNode);
  while WishNode <> nil do
  begin
    WishNodeData := FTree.GetNodeData(WishNode);
    if WishNodeData.NodeType <> ntWish then
      Break;

    FTree.Selected[WishNode] := False;

    if WishNodeData.NodeType = ntWish then
    begin
      IgnoreNode := FTree.GetFirstChild(FTree.FIgnoreNode);
      while IgnoreNode <> nil do
      begin
        IgnoreNodeData := FTree.GetNodeData(IgnoreNode);

        if IgnoreNodeData.NodeType = ntIgnore then
        begin
          if Like(WishNodeData.Title.Title, IgnoreNodeData.Title.Pattern) then
            FTree.Selected[WishNode] := True
        end;

        IgnoreNode := FTree.GetNext(IgnoreNode);
      end;
    end;

    FTree.ApplyFocus;

    WishNode := FTree.GetNext(WishNode);
  end;
end;

procedure TTitlePanel.ConvertToAutomaticClick(Sender: TObject);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PTitleNodeData;
  ConversionData: TStringList;
begin
  ConversionData := TStringList.Create;
  try
    Nodes := FTree.GetNodes([ntWish], True);
    for i := 0 to High(Nodes) do
    begin
      NodeData := FTree.GetNodeData(Nodes[i]);

      if (NodeData.Title.ServerHash = 0) and (NodeData.Title.ServerArtistHash = 0) then
        ConversionData.Add(NodeData.Title.Title);
    end;

    HomeComm.SendConvertManualToAutomatic(ConversionData);
  finally
    ConversionData.Free;
  end;
end;

procedure TTitlePanel.SelectSavedClick(Sender: TObject);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Node := FTree.GetFirst;
  while Node <> nil do
  begin
    FTree.Selected[Node] := False;
    NodeData := FTree.GetNodeData(Node);

    if NodeData.NodeType <> ntWish then
    begin
      Node := FTree.GetNext(Node);
      Continue;
    end;

    for i := 0 to AppGlobals.Data.TrackList.Count - 1 do
    begin
      if ((NodeData.Title.ServerHash = 0) and (NodeData.Title.ServerArtistHash = 0) and (Like(RemoveFileExt(ExtractFileName(AppGlobals.Data.TrackList[i].Filename)), NodeData.Title.Pattern))) or
         ((NodeData.Title.ServerHash > 0) and (AppGlobals.Data.TrackList[i].ServerTitleHash = NodeData.Title.ServerHash)) then
      begin
        FTree.Selected[Node] := True;
      end;
    end;

    Node := FTree.GetNext(Node);
  end;

  FTree.ApplyFocus;
end;

procedure TTitlePanel.ShowSavedClick(Sender: TObject);
var
  i: Integer;
  Nodes: TNodeArray;
  NodeData: PTitleNodeData;
  TitleHashes: TCardinalArray;
  ArtistHashes: TCardinalArray;
  SearchString: string;
begin
  SearchString := '';
  SetLength(TitleHashes, 0);
  SetLength(ArtistHashes, 0);

  Nodes := FTree.GetNodes([ntWish], True);
  for i := 0 to High(Nodes) do
  begin
    NodeData := FTree.GetNodeData(Nodes[i]);

    if NodeData.Title <> nil then
    begin
      if NodeData.Title.ServerHash = 0 then
      begin
        SetLength(ArtistHashes, Length(ArtistHashes) + 1);
        ArtistHashes[High(ArtistHashes)] := NodeData.Title.ServerArtistHash;
      end else
      begin
        SetLength(TitleHashes, Length(TitleHashes) + 1);
        TitleHashes[High(TitleHashes)] := NodeData.Title.ServerHash;
      end;
    end;
  end;

  if (Length(TitleHashes) > 0) or (Length(ArtistHashes) > 0) then
    MsgBus.SendMessage(TSelectSavedSongsMsg.Create(Self, TitleHashes, ArtistHashes));
end;

procedure TTitlePanel.AddEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    FToolbar.FAdd.Click;
    Key := #0;
  end;
end;

function TTitlePanel.AddEntry(Text: string; TitleHash: Cardinal; ShowMessages: Boolean; ListType: TListType): Boolean;
var
  i, NumChars: Integer;
  Pattern: string;
  Node, Parent: PVirtualNode;
  NodeData: PTitleNodeData;
  Title: TTitleInfo;
  Hash: Cardinal;
  List: TList<TTitleInfo>;
begin
  Result := False;

  if Trim(Text) <> '' then
  begin
    Parent := nil;

    if (ListType = ltSave) or ((ListType = ltAutoDetermine) and (FAddCombo.ItemIndex = 0)) then
    begin
      List := AppGlobals.Data.SaveList;
      Parent := FTree.FWishNode;
    end else if (ListType = ltIgnore) or ((ListType = ltAutoDetermine) and (FAddCombo.ItemIndex = 1)) then
    begin
      List := AppGlobals.Data.IgnoreList;
      Parent := FTree.FIgnoreNode;
    end else
      List := TICEClient(FAddCombo.ItemsEx[FAddCombo.ItemIndex].Data).Entry.IgnoreList;

    // Keine doppelten Auto-Einträge erlauben
    if (TitleHash > 0) and (List = AppGlobals.Data.SaveList) then
      for i := 0 to List.Count - 1 do
        if (List[i].ServerHash > 0) and (List[i].ServerHash = TitleHash) then
          Exit;

    Pattern := BuildPattern(Trim(Text), Hash, NumChars, False);

    // Keine doppelten manuellen Einträge erlauben
    if TitleHash = 0 then
      for i := 0 to List.Count - 1 do
        if (List[i].ServerHash = 0) and (List[i].Hash = Hash) then
        begin
          if ShowMessages then
            MsgBox(GetParentForm(Self).Handle, Format(_('The list already contains an entry matching the pattern "%s".'), [Pattern]), _('Info'), MB_ICONINFORMATION);
          Exit;
        end;

    if (NumChars <= 3) and ShowMessages then
      TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('A short pattern may produce many matches, i.e. using ''a'' records/ignores every song containing an ''a''.'), mtInformation, [mbOK], mbOK, 6);

    if ShowMessages and (List = AppGlobals.Data.SaveList) then
      TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('Titles manually entered into the wishlist (without using the "Title search" tab) will not be considered for automatic recordings. Use the "Title search" tab to add titles for automatic recordings.'), mtInformation, [mbOK], mbOK, 15);

    if Parent = nil then
      Parent := FTree.GetNode(TICEClient(FAddCombo.ItemsEx[FAddCombo.ItemIndex].Data));

    Title := TTitleInfo.Create(TitleHash, 0, Trim(Text));
    Node := FTree.AddTitle(Title, Parent, FFilterText, True);
    if Node <> nil then
    begin
      NodeData := FTree.GetNodeData(Node);
      NodeData.Title := Title;

      if (List <> AppGlobals.Data.SaveList) and (List <> AppGlobals.Data.IgnoreList) then
        NodeData.Stream := TICEClient(FAddCombo.ItemsEx[FAddCombo.ItemIndex].Data);

      if Node.Parent.ChildCount = 1 then
        FTree.Expanded[Node.Parent] := True;
    end;

    List.Add(Title);

    HomeComm.SendSetSettings(AppGlobals.Data.SaveList.AnyAutomatic and AppGlobals.AutoTuneIn);

    if TitleHash > 0 then
      HomeComm.SendSyncWishlist(swAdd, TitleHash, False);

    UpdateButtons;

    Result := True;
  end else
    if ShowMessages then
      MsgBox(GetParentForm(Self).Handle, _('Please enter a pattern to add to the list.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TTitlePanel.TreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  List: TList<TTitleInfo>;
  NodeData: PTitleNodeData;
  i: Integer;
begin
  if Node <> nil then
  begin
    List := nil;
    NodeData := FTree.GetNodeData(Node);

    case NodeData.NodeType of
      ntWishParent, ntWish:
        FAddCombo.ItemIndex := 0;
      ntIgnoreParent:
        FAddCombo.ItemIndex := 1;
      ntIgnore:
        if NodeData.Stream <> nil then
          List := NodeData.Stream.Entry.IgnoreList
        else
          FAddCombo.ItemIndex := 1;
      ntStream:
        List := NodeData.Stream.Entry.IgnoreList;
    end;

    if List <> nil then
      for i := 0 to FAddCombo.ItemsEx.Count - 1 do
        if Assigned(FAddCombo.ItemsEx[i].Data) and (TObject(FAddCombo.ItemsEx[i].Data).ClassType = TICEClient) and (TICEClient(FAddCombo.ItemsEx[i].Data).Entry.IgnoreList = List) then
        begin
          FAddCombo.ItemIndex := i;
          Break;
        end;
  end;

  UpdateButtons;
end;

procedure TTitlePanel.TreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    RemoveClick(nil);
  end;
end;

procedure TTitlePanel.UpdateButtons;
var
  i, n: Integer;
  SingleParentSelected, TitlesSelected, CanRemove, CanRename, CanImport, CanShowSaved, CanConvert: Boolean;
  SelectedNodes, SelectedParents: TNodeArray;
  ChildNodeData: PTitleNodeData;

  function TypeCount(NodeType: TNodeType): Integer;
  var
    i: Integer;
    NodeData: PTitleNodeData;
  begin
    Result := 0;
    for i := 0 to Length(SelectedNodes) - 1 do
    begin
      NodeData := FTree.GetNodeData(SelectedNodes[i]);
      if NodeData.NodeType = NodeType then
        Inc(Result);
    end;
  end;
begin
  SelectedNodes := FTree.GetNodes([ntStream, ntWish, ntIgnore], True);
  SelectedParents := FTree.GetNodes([ntWishParent, ntIgnoreParent, ntStream], True);
  CanShowSaved := False;
  CanConvert := False;

  SingleParentSelected := (FTree.SelectedCount = 1) and (Length(SelectedParents) = 1);
  if SingleParentSelected then
  begin
    // Es muss zum Parent mindestens ein Child geben was drunter liegt und ein Titel ist.
    if SelectedParents[0].ChildCount > 0 then
    begin
      ChildNodeData := FTree.GetNodeData(FTree.GetFirstChild(SelectedParents[0]));
      SingleParentSelected := (ChildNodeData.NodeType = ntWish) or (ChildNodeData.NodeType = ntIgnore);
    end else
      SingleParentSelected := False;
  end;

  for i := 0 to High(SelectedNodes) do
  begin
    ChildNodeData := FTree.GetNodeData(SelectedNodes[i]);
    for n := 0 to AppGlobals.Data.TrackList.Count - 1 do
      if ((ChildNodeData.Title <> nil) and (ChildNodeData.Title.ServerHash = AppGlobals.Data.TrackList[n].ServerTitleHash) and (ChildNodeData.Title.ServerHash > 0))
         or ((ChildNodeData.Title <> nil) and (ChildNodeData.Title.ServerArtistHash = AppGlobals.Data.TrackList[n].ServerArtistHash) and (ChildNodeData.Title.ServerArtistHash > 0)) then
      begin
        CanShowSaved := True;
        Break;
      end;

    if HomeComm.CommunicationEstablished and (ChildNodeData.NodeType = ntWish) and (ChildNodeData.Title.ServerHash = 0) and (ChildNodeData.Title.ServerArtistHash = 0) then
      CanConvert := True;

    if CanShowSaved and (CanConvert or (not CanConvert and not HomeComm.CommunicationEstablished)) then
      Break;
  end;

  TitlesSelected := (TypeCount(ntWish) > 0) or (TypeCount(ntIgnore) > 0);
  CanRemove := TitlesSelected or (TypeCount(ntStream) > 0);
  CanRename := (FTree.SelectedCount = 1) and TitlesSelected and (PTitleNodeData(FTree.GetNodeData(SelectedNodes[0])).Title.ServerHash = 0) and
    (PTitleNodeData(FTree.GetNodeData(SelectedNodes[0])).Title.ServerArtistHash = 0);
  CanImport := True;

  FToolbar.FRemove.Enabled := CanRemove;
  FToolbar.FRename.Enabled := CanRename;
  FToolbar.FShowSaved.Enabled := CanShowSaved;
  FToolbar.FSelectSaved.Enabled := FTree.FWishNode.ChildCount > 0;
  FToolbar.FSelectIgnored.Enabled := FTree.FWishNode.ChildCount > 0;
  FToolbar.FExport.Enabled := (TitlesSelected and (not SingleParentSelected) and (Length(SelectedParents) = 0)) or
                              (SingleParentSelected and (not TitlesSelected));
  FToolbar.FImport.Enabled := CanImport;
  FToolbar.FConvertToAutomatic.Enabled := CanConvert;

  FTree.FPopupMenu.FRemove.Enabled := CanRemove;
  FTree.FPopupMenu.FRename.Enabled := CanRename;
  FTree.FPopupMenu.FShowSaved.Enabled := CanShowSaved;
  FTree.FPopupMenu.FSelectSaved.Enabled := FTree.FWishNode.ChildCount > 0;
  FTree.FPopupMenu.FSelectIgnored.Enabled := FTree.FWishNode.ChildCount > 0;
  FTree.FPopupMenu.FExport.Enabled := FToolbar.FExport.Enabled;
  FTree.FPopupMenu.FImport.Enabled := CanImport;
  FTree.FPopupMenu.FConvertToAutomatic.Enabled := CanConvert;
end;

procedure TTitlePanel.UpdateList;
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Node := FTree.GetFirst;
  while Node <> nil do
  begin
    NodeData := FTree.GetNodeData(Node);

    if NodeData.Title <> nil then
    begin
      NodeData.Title.Index := Node.Index;
    end;

    if (NodeData.Stream <> nil) and (NodeData.Title = nil) then
    begin
      NodeData.Stream.Entry.IgnoreListIndex := Node.Index;
    end;

    Node := FTree.GetNext(Node);
  end;
end;

{ TTitleToolbar }

constructor TTitleToolbar.Create(AOwner: TComponent);
var
  Sep: TToolButton;
begin
  inherited;

  FAdd := TToolButton.Create(Self);
  FAdd.Parent := Self;
  FAdd.Hint := 'Add';
  FAdd.ImageIndex := TImages.ADD;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FRename := TToolButton.Create(Self);
  FRename.Parent := Self;
  FRename.Hint := 'Rename';
  FRename.ImageIndex := TImages.TEXTFIELD_RENAME;

  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := 'Remove';
  FRemove.ImageIndex := TImages.DELETE;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FConvertToAutomatic := TToolButton.Create(Self);
  FConvertToAutomatic.Parent := Self;
  FConvertToAutomatic.Hint := 'Convert to automatic wishlist title';
  FConvertToAutomatic.ImageIndex := TImages.BRICKS_COG;

  FShowSaved := TToolButton.Create(Self);
  FShowSaved.Parent := Self;
  FShowSaved.Hint := 'Show in saved tracks';
  FShowSaved.ImageIndex := TImages.DRIVE_GO;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FSelectSaved := TToolButton.Create(Self);
  FSelectSaved.Parent := Self;
  FSelectSaved.Hint := 'Select saved titles';
  FSelectSaved.ImageIndex := TImages.DRIVE_SELECT;

  FSelectIgnored := TToolButton.Create(Self);
  FSelectIgnored.Parent := Self;
  FSelectIgnored.Hint := 'Select ignored titles';
  FSelectIgnored.ImageIndex := TImages.DECLINE_SELECT;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FExport := TToolButton.Create(Self);
  FExport.Parent := Self;
  FExport.Hint := 'Export...';
  FExport.ImageIndex := TImages.SCRIPT_OUT;

  FImport := TToolButton.Create(Self);
  FImport.Parent := Self;
  FImport.Hint := 'Import...';
  FImport.ImageIndex := TImages.SCRIPT_IN;
end;

{ TTitleTree }

function TTitleTree.AddTitle(Title: TTitleInfo; Parent: PVirtualNode; FilterText: string; FromFilter: Boolean): PVirtualNode;
var
  AttachMode: TVTNodeAttachMode;
  Node, SearchNode, LastFoundChild: PVirtualNode;
  NodeData, SearchNodeData, ParentData: PTitleNodeData;
begin
  Result := nil;

  // Kann eigentlich nicht passieren, Yo24hua hatte aber hier drunter im LowerCase() eine Exception
  // Zugriffsverletzung bei Adresse 0041E790 in Modul 'streamwriter.exe'. Lesen von Adresse FFFFFFFF.
  // Vielleicht hilft das, auch wenn ich es mir bis jetzt noch nicht erklären kann.
  if Title = nil then
    Exit;

  if FromFilter and ((FilterText <> '') and (not Like(LowerCase(Title.Title), FilterText))) then
    Exit;

  Node := AddChild(Parent);
  Result := Node;
  NodeData := GetNodeData(Node);
  NodeData.Title := Title;
  NodeData.Stream := nil;
  if Parent <> nil then
  begin
    ParentData := GetNodeData(Parent);

    // Parents für ein neues Kind aufklappen.
    if ((Parent = FWishNode) or (Parent = FIgnoreNode)) and (Parent.ChildCount = 1) then
      Expanded[Parent] := True;
    if (ParentData.Stream <> nil) and (Parent.ChildCount = 1) and (Parent.Parent.ChildCount = 1) then
    begin
      Expanded[Parent.Parent] := True;
    end;
    
    NodeData.Stream := ParentData.Stream;

    case ParentData.NodeType of
      ntWishParent:
        NodeData.NodeType := ntWish;
      ntIgnoreParent:
        if NodeData.Stream <> nil then
          NodeData.NodeType := ntStream
        else
        begin
          // Die Node ans Ende schieben, aber vor den ersten Stream

          AttachMode := amInsertAfter;
          LastFoundChild := nil;
          
          SearchNode := GetLastChild(Parent);
          while SearchNode <> nil do
          begin         
            if SearchNode = Node then
            begin
              SearchNode := GetPreviousSibling(SearchNode);
              Continue;
            end;
            
            SearchNodeData := GetNodeData(SearchNode);
            if SearchNodeData.Stream = nil then
            begin
              LastFoundChild := SearchNode;
              AttachMode := amInsertAfter;
              Break;
            end else
              AttachMode := amInsertBefore;
            LastFoundChild := SearchNode;
            SearchNode := GetPreviousSibling(SearchNode);
          end;
          if LastFoundChild <> nil then
            MoveTo(Node, LastFoundChild, AttachMode, False);
          
          NodeData.NodeType := ntIgnore;
        end;
      ntStream:
        NodeData.NodeType := ntIgnore;
    end;
  end;

  if (Parent <> nil) and (Parent.ChildCount = 1) then
  begin
    Expanded[Parent] := True;
  end;
end;

constructor TTitleTree.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FPanel := TTitlePanel(AOwner);

  NodeDataSize := SizeOf(TTitleNodeData);
  IncrementalSearch := isVisibleOnly;

  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  Header.Options := [hoColumnResize, hoDrag, hoAutoResize, hoHotTrack, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];

  Header.AutoSizeIndex := 0;
  DragMode := dmManual;
  ShowHint := True;
  HintMode := hmTooltip;

  Self.Images := modSharedData.imgImages;

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Title');
  FColTitle.Options := FColTitle.Options - [coDraggable];
  FColSaved := Header.Columns.Add;
  FColSaved.Text := _('Times saved');
  FColSaved.Alignment := taRightJustify;
  FColAdded := Header.Columns.Add;
  FColAdded.Text := _('Date');
  FColAdded.Alignment := taRightJustify;

  Header.PopupMenu := TMTreeColumnPopup.Create(Self);
  TMTreeColumnPopup(Header.PopupMenu).OnAction := MenuColsAction;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfFile];
  FDropTarget.OnDrop := DropTargetDrop;
  FDropTarget.Register(Self);

  FPopupMenu := TTitlePopup.Create(Self);
  if Screen.PixelsPerInch = 96 then
    FPopupMenu.Images := modSharedData.imgImages;
  FPopupMenu.FRemove.OnClick := PopupMenuClick;
  FPopupMenu.FRename.OnClick := PopupMenuClick;
  FPopupMenu.FShowSaved.OnClick := PopupMenuClick;
  FPopupMenu.FSelectSaved.OnClick := PopupMenuClick;
  FPopupMenu.FSelectIgnored.OnClick := PopupMenuClick;
  FPopupMenu.FExport.OnClick := PopupMenuClick;
  FPopupMenu.FImport.OnClick := PopupMenuClick;
  FPopupMenu.FConvertToAutomatic.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;

  for i := 1 to Header.Columns.Count - 1 do
  begin
    if not ((AppGlobals.ListCols and (1 shl i)) <> 0) then
      Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];
  end;

  FitColumns;
end;

procedure TTitleTree.DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
var
  i, n: Integer;
  Found: Boolean;
  HI: THitInfo;
  Node: PVirtualNode;
  NodeData, ParentNodeData: PTitleNodeData;
  Title: TTitleInfo;
  List: TList<TTitleInfo>;
begin
  List := nil;

  GetHitTestInfoAt(APoint.X, APoint.Y, True, HI);
  if Hi.HitNode <> nil then
  begin
    NodeData := GetNodeData(Hi.HitNode);

    Node := Hi.HitNode;

    case NodeData.NodeType of
      ntWishParent:
        begin
          List := AppGlobals.Data.SaveList;
        end;
      ntIgnoreParent:
        begin
          List := AppGlobals.Data.IgnoreList;
        end;
      ntStream:
        begin
          ParentNodeData := GetNodeData(Hi.HitNode.Parent);
          if ParentNodeData.NodeType = ntWishParent then
            List := NodeData.Stream.Entry.SaveList
          else
            List := NodeData.Stream.Entry.IgnoreList;
        end;
      ntWish:
        begin
          ParentNodeData := GetNodeData(Hi.HitNode.Parent);
          case ParentNodeData.NodeType of
            ntWishParent:
              begin
                Node := FWishNode;
                List := AppGlobals.Data.SaveList;
              end;
            ntStream:
              begin
                Node := Hi.HitNode.Parent;
                ParentNodeData := GetNodeData(Hi.HitNode.Parent);
                List := ParentNodeData.Stream.Entry.SaveList
              end;
          end;
        end;
      ntIgnore:
        begin
          ParentNodeData := GetNodeData(Hi.HitNode.Parent);

          case ParentNodeData.NodeType of
            ntIgnoreParent:
              begin
                Node := FIgnoreNode;
                List := AppGlobals.Data.IgnoreList;
              end;
            ntStream:
              begin
                Node := Hi.HitNode.Parent;
                ParentNodeData := GetNodeData(Hi.HitNode.Parent);
                List := ParentNodeData.Stream.Entry.IgnoreList;
              end;
          end;
        end;
    end;
  end else
    Exit;

  if List = nil then
    Exit;

  for i := 0 to FDropTarget.Files.Count - 1 do
  begin
    Title := TTitleInfo.Create(0, 0, RemoveFileExt(ExtractFileName(FDropTarget.Files[i])));

    Found := False;
    for n := 0 to List.Count - 1 do
      if List[n].Hash = Title.Hash then
      begin
        Found := True;
        Break;
      end;

    if not Found then
    begin
      List.Add(Title);
      AddTitle(Title, Node, FPanel.FFilterText, True);

      if List = AppGlobals.Data.SaveList then
        HomeComm.SendSetSettings(AppGlobals.AutoTuneIn);
    end else
      Title.Free;
  end;
end;

procedure TTitleTree.FitColumns;
var
  i: Integer;
begin
  if (Header.Columns.Count <> Length(AppGlobals.ListHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.ListHeaderPosition)) then
    raise Exception.Create('(Header.Columns.Count <> Length(AppGlobals.ListHeaderWidth)) or (Header.Columns.Count <> Length(AppGlobals.ListHeaderPosition))');

  if AppGlobals.ListHeaderWidthLoaded then
  begin
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Width := AppGlobals.ListHeaderWidth[i];
  end else
  begin
    FColSaved.Width := MulDiv(120, Screen.PixelsPerInch, 96);
    FColAdded.Width := MulDiv(130, Screen.PixelsPerInch, 96);
  end;

  if AppGlobals.ListHeaderPositionLoaded then
  begin
    for i := 1 to Header.Columns.Count - 1 do
      Header.Columns[i].Position := AppGlobals.ListHeaderPosition[i];
  end;
end;

function TTitleTree.GetNode(Stream: TICEClient): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  if Stream = nil then
    Exit(nil);

  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);
    if (NodeData.Stream = Stream) and (NodeData.Title = nil) then
      Exit(Node);
    Node := GetNext(Node);
  end;

  Result := AddChild(FIgnoreNode);
  NodeData := GetNodeData(Result);
  NodeData.Stream := Stream;
  NodeData.Title := nil;
  NodeData.NodeType := ntStream;
end;

function TTitleTree.GetNodes(NodeTypes: TNodeTypes;
  SelectedOnly: Boolean): TNodeArray;
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  SetLength(Result, 0);
  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);

    if SelectedOnly and (not Selected[Node]) then
    begin
      Node := GetNext(Node);
      Continue;
    end;

    if not (NodeData.NodeType in NodeTypes) then
    begin
      Node := GetNext(Node);
      Continue;
    end;

    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := Node;
    Node := GetNext(Node);
  end;
end;

procedure TTitleTree.MenuColsAction(Sender: TVirtualStringTree;
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

  AppGlobals.ListCols := AppGlobals.ListCols xor (1 shl Index);
end;

function TTitleTree.NodesToData(Nodes: TNodeArray): TTitleDataArray;
var
  i: Integer;
  Data: PTitleNodeData;
begin
  SetLength(Result, Length(Nodes));
  for i := 0 to Length(Nodes) - 1 do
  begin
    Data := GetNodeData(Nodes[i]);
    Result[i] := Data;
  end;
end;

procedure TTitleTree.PopupMenuClick(Sender: TObject);
begin
  if Sender = FPopupMenu.FRemove then
    TTitlePanel(Owner).FToolbar.FRemove.Click
  else if Sender = FPopupMenu.FRename then
    TTitlePanel(Owner).FToolbar.FRename.Click
  else if Sender = FPopupMenu.FShowSaved then
    TTitlePanel(Owner).FToolbar.FShowSaved.Click
  else if Sender = FPopupMenu.FSelectSaved then
    TTitlePanel(Owner).FToolbar.FSelectSaved.Click
  else if Sender = FPopupMenu.FSelectIgnored then
    TTitlePanel(Owner).FToolbar.FSelectIgnored.Click
  else if Sender = FPopupMenu.FExport then
    TTitlePanel(Owner).FToolbar.FExport.Click
  else if Sender = FPopupMenu.FImport then
    TTitlePanel(Owner).FToolbar.FImport.Click
  else if Sender = FPopupMenu.FConvertToAutomatic then
    TTitlePanel(Owner).FToolbar.FConvertToAutomatic.Click;
end;

procedure TTitleTree.PostTranslate;
begin
  FColTitle.Text := _('Title');
  FColSaved.Text := _('Times saved');
  FColAdded.Text := _('Date');
end;

procedure TTitleTree.RemoveClient(Client: TICEClient);
var
  Node: PVirtualNode;
begin
  Node := GetNode(Client);
  DeleteNode(Node);
end;

procedure TTitleTree.RemoveTitle(Title: TTitleInfo);
var
  Node, DelNode: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Node := GetFirst;

  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);

    DelNode := nil;

    if NodeData.Title = Title then
      DelNode := Node;

    Node := GetNext(Node);

    if DelNode <> nil then
      Self.DeleteNode(DelNode);
  end;
end;

procedure TTitleTree.SortItems;
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Sort(nil, Header.SortColumn, Header.SortDirection);

  Node := GetFirst;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);
    if ((NodeData.Stream = nil) and (NodeData.Title = nil)) or (NodeData.Stream <> nil) then
      Sort(Node, Header.SortColumn, Header.SortDirection);
    Node := GetNext(Node);
  end;
end;

procedure TTitleTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
var
  ChildCount: Integer;
  NodeData: PTitleNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);
  case Column of
    0:
      begin
        case NodeData.NodeType of
          ntWishParent:
            Text := _(WISHTEXT);
          ntIgnoreParent:
            Text := _(IGNORETEXT);
          ntStream:
            Text := NodeData.Stream.Entry.CustomName;
          ntWish, ntIgnore:
            Text := NodeData.Title.Title;
        end;

        if NodeData.NodeType in [ntWishParent, ntIgnoreParent, ntStream] then
        begin
          ChildCount := 0;
          Node := GetFirstChild(Node);
          while Node <> nil do
          begin
            NodeData := GetNodeData(Node);
            if (NodeData.NodeType = ntWish) or (NodeData.NodeType = ntIgnore) then
              Inc(ChildCount);
            Node := GetNextSibling(Node);
          end;
          Text := Text + ' (' + IntToStr(ChildCount) + ')';
        end;
      end;
    1:
      begin
        if (NodeData.NodeType = ntWish) and (NodeData.Title <> nil) and
           ((NodeData.Title.ServerHash > 0) or (NodeData.Title.ServerArtistHash > 0)) then
        begin
          Text := IntToStr(NodeData.Title.Saved);
        end else
          Text := '';
      end;
    2:
      begin
        if NodeData.Title <> nil then
        begin
          Text := DateToStr(NodeData.Title.Added);
        end else
          Text := '';
      end;
  end;
end;

function TTitleTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
var
  NodeData: PTitleNodeData;
begin
  Result := Images;

  NodeData := GetNodeData(Node);

  if Column = 0 then
    case NodeData.NodeType of
      ntWishParent:
        Index := TImages.SCRIPT_HEART;
      ntWish:
        if NodeData.Title.ServerHash > 0 then
          Index := TImages.BRICKS
        else if NodeData.Title.ServerArtistHash > 0 then
          Index := TImages.USER_GRAY_COOL
        else
          Index := TImages.HEART;
      ntIgnoreParent:
        Index := TImages.SCRIPT_DECLINE;
      ntIgnore:
        Index := TImages.DECLINE;
      ntStream:
        Index := TImages.TRANSMIT;
    end;
end;

procedure TTitleTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if Header.SortColumn <> HitInfo.Column then
    begin
      Header.SortColumn := HitInfo.Column;
      Header.SortDirection := sdAscending;

      if (HitInfo.Column = 1) or (HitInfo.Column = 2) then
        Header.SortDirection := sdDescending;
    end else
    begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end;

    SortItems;
  end;
end;

function TTitleTree.DoPaintBackground(Canvas: TCanvas; const R: TRect): Boolean;
begin
  Result := inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Result := True;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(R);
end;

procedure TTitleTree.DoAfterItemErase(Canvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  inherited;

  if not AppGlobals.NodeColorsLoaded then
    Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AppGlobals.NodeBackgroundColor;

  Canvas.FillRect(ItemRect);
end;

procedure TTitleTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
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

procedure TTitleTree.DoHeaderDragged(Column: TColumnIndex;
  OldPosition: TColumnPosition);
begin
  inherited;

  if Header.Columns[Column].Position = 0 then
    Header.Columns[Column].Position := FHeaderDragSourcePosition;
end;

function TTitleTree.DoHeaderDragging(Column: TColumnIndex): Boolean;
begin
  if Column = -1 then
    Exit(False);

  Result := inherited;

  FHeaderDragSourcePosition := Header.Columns[Column].Position;
end;

function TTitleTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  s: string;
  NodeData: PTitleNodeData;
begin
  // TODO: was ist mit "s"???
  if Node = FWishNode then
    Exit(StrLIComp(PChar(s), PChar(_(WISHTEXT)), Min(Length(s), Length(_(WISHTEXT)))));
  if Node = FIgnoreNode then
    Exit(StrLIComp(PChar(s), PChar(_(IGNORETEXT)), Min(Length(s), Length(_(IGNORETEXT)))));

  Result := 0;
  S := Text;
  NodeData := GetNodeData(Node);
  if NodeData = nil then
    Exit;
  if NodeData.Title <> nil then
    Result := StrLIComp(PChar(s), PChar(NodeData.Title.Title), Min(Length(s), Length(NodeData.Title.Title)))
  else
    Result := StrLIComp(PChar(s), PChar(NodeData.Stream.Entry.CustomName), Min(Length(s), Length(NodeData.Stream.Entry.CustomName)))
end;

procedure TTitleTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex; const Text: string);
var
  NodeData: PTitleNodeData;
begin
  inherited;

  if Trim(Text) = '' then
    Exit;

  NodeData := GetNodeData(Node);

  NodeData.Title.Free;
  NodeData.Title := TTitleInfo.Create(0, 0, Text);
end;

procedure TTitleTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var
  NodeData: PTitleNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  Allowed := (NodeData.Title <> nil) and (NodeData.Title.ServerHash = 0);
end;

function TTitleTree.DoCompare(Node1, Node2: PVirtualNode;
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
  Data1, Data2: PTitleNodeData;
begin
  Result := 0;

  if (Node1 = FWishNode) or (Node2 = FIgnoreNode) then
    Exit(0);

  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  if ((Data1.Stream = nil) and (Data1.Title = nil)) or ((Data2.Stream = nil) and (Data2.Title = nil)) then
    if Header.SortDirection = sdAscending then
      Exit(-1)
    else
      Exit(1);

  case Header.SortColumn of
    -1:
      if (Data1.Stream <> nil) and (Data1.Title = nil) and (Data2.Stream <> nil) and (Data2.Title = nil) then
      begin
        Result := CmpC(Data1.Stream.Entry.IgnoreListIndex, Data2.Stream.Entry.IgnoreListIndex);
      end else if (Data1.Title <> nil) and (Data2.Title <> nil) then
      begin
        Result := CmpC(Data1.Title.Index, Data2.Title.Index);
      end;
    0:
      begin
        if (Data1.Title <> nil) and (Data2.Title <> nil) then
          Result := CompareText(Data1.Title.Title, Data2.Title.Title)
        else if (Data1.NodeType = ntStream) and (Data2.NodeType = ntStream) then
          Result := CompareText(Data1.Stream.Entry.CustomName, Data2.Stream.Entry.CustomName)
        else if (Data1.Title <> nil) and (Data2.Stream = nil) then
          Exit(1)
        else if (Data1.Title = nil) and (Data2.Stream <> nil) then
          Exit(-1);
      end;
    1:
      begin
        if (Data1.Title <> nil) and (Data2.Title <> nil) then
          Result := CmpInt(Data1.Title.Saved, Data2.Title.Saved)
        else
          Result := 0;
      end;
    2:
      if Node1 = FWishNode then                                
        Result := 1
      else if Node1 = FIgnoreNode then
        Result := -1    
      else if (Data1.Title <> nil) and (Data2.Title <> nil) then
        Result := CmpTime(Data1.Title.Added, Data2.Title.Added)
      else
        Result := 0;
  end;
end;

{ TTitlePopup }

constructor TTitlePopup.Create(AOwner: TComponent);
begin
  inherited;

  FRename := TMenuItem.Create(Self);;
  FRename.Caption := 'Ren&ame';
  FRename.ImageIndex := TImages.TEXTFIELD_RENAME;
  Items.Add(FRename);

  FRemove := TMenuItem.Create(Self);;
  FRemove.Caption := '&Remove';
  FRemove.ImageIndex := TImages.DELETE;
  Items.Add(FRemove);

  Items.AddSeparator;

  FConvertToAutomatic := TMenuItem.Create(Self);;
  FConvertToAutomatic.Caption := '&Convert to automatic wishlist title';
  FConvertToAutomatic.ImageIndex := TImages.BRICKS_COG;
  Items.Add(FConvertToAutomatic);

  FShowSaved := TMenuItem.Create(Self);;
  FShowSaved.Caption := 'S&how in saved tracks';
  FShowSaved.ImageIndex := TImages.DRIVE_GO;
  Items.Add(FShowSaved);

  Items.AddSeparator;

  FSelectSaved := TMenuItem.Create(Self);;
  FSelectSaved.Caption := '&Select saved titles';
  FSelectSaved.ImageIndex := TImages.DRIVE_SELECT;
  Items.Add(FSelectSaved);

  FSelectIgnored := TMenuItem.Create(Self);;
  FSelectIgnored.Caption := 'Se&lect ignored titles';
  FSelectIgnored.ImageIndex := TImages.DECLINE_SELECT;
  Items.Add(FSelectIgnored);

  Items.AddSeparator;

  FExport := TMenuItem.Create(Self);;
  FExport.Caption := '&Export...';
  FExport.ImageIndex := TImages.SCRIPT_OUT;
  Items.Add(FExport);

  FImport := TMenuItem.Create(Self);;
  FImport.Caption := '&Import...';
  FImport.ImageIndex := TImages.SCRIPT_IN;
  Items.Add(FImport);
end;

{ TImportListEntry }

constructor TImportListEntry.Create(Title: string; Hash: Cardinal;
  IsArtist: Boolean);
begin
  Self.Title := Title;
  Self.Hash := Hash;
  Self.IsArtist := IsArtist;
end;

end.
