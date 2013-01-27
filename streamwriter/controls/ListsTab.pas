    {
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  Windows, SysUtils, Messages, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, MControls, LanguageObjects, Tabs, VirtualTrees, DataManager,
  ImgList, Functions, GUIFunctions, Menus, Math, DragDrop, DropComboTarget,
  Dialogs, MsgDlg, Forms, Logging, AppData, HomeCommunication, ICEClient,
  ClientManager, Generics.Collections, TypeDefs, MessageBus, AppMessages;

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

  TTitlePopup = class(TPopupMenu)
  private
    FRemove: TMenuItem;
    FRename: TMenuItem;
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

  TTitleToolbar = class(TToolbar)
  private
    FAdd: TToolButton;
    FRename: TToolButton;
    FRemove: TToolButton;
    FSelectSaved: TToolButton;
    FSelectIgnored: TToolButton;
    FExport: TToolButton;
    FImport: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup;
  end;

  TTitlePanel = class(TPanel)
  private
    FTopPanel: TPanel;
    FSearchPanel: TPanel;
    FSearchLabel: TLabel;
    FSearchText: TEdit;
    FToolbarPanel: TPanel;
    FAddLabel: TLabel;
    FAddEdit: TEdit;
    FAddCombo: TComboBox;
    FToolbar: TTitleToolbar;
    FTree: TTitleTree;

    FClients: TClientManager;
    FLists: TDataLists;
    FFilterText: string;

    procedure BuildTree(FromFilter: Boolean);
    procedure UpdateButtons;
    procedure FillClientCombo;

    procedure AddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure ImportClick(Sender: TObject);
    procedure SelectSavedClick(Sender: TObject);
    procedure SelectIgnoredClick(Sender: TObject);
    procedure RenameClick(Sender: TObject);
    procedure AddEditKeyPress(Sender: TObject; var Key: Char);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchTextChange(Sender: TObject);
  protected
    procedure Resize; override;
  public
    procedure PostTranslate;
    function AddEntry(Text: string; ShowMessages: Boolean; ListType: TListType): Boolean;
    procedure ClientAdded(Client: TICEClient);
    procedure ClientRemoved(Client: TICEClient);
    procedure Setup(Clients: TClientManager; Lists: TDataLists; Images: TImageList);
    procedure UpdateList;
  end;

  TListsTab = class(TMainTabSheet)
  private
    FListsPanel: TTitlePanel;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup(Clients: TClientManager; Streams: TDataLists; Images: TImageList);
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
    FColAdded: TVirtualTreeColumn;

    FPanel: TTitlePanel;

    FLists: TDataLists;
    FDropTarget: TDropComboTarget;
    FPopupMenu: TTitlePopup;
    FWishNode: PVirtualNode;
    FIgnoreNode: PVirtualNode;

    function GetNode(Stream: TICEClient): PVirtualNode;

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);

    procedure PopupMenuClick(Sender: TObject);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: UnicodeString); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists; Images: TImageList); reintroduce;
    destructor Destroy; override;

    function AddTitle(Title: TTitleInfo; Parent: PVirtualNode; FilterText: string; FromFilter: Boolean): PVirtualNode;
    procedure RemoveTitle(Title: TTitleInfo);
    procedure RemoveClient(Client: TICEClient);
    procedure SortItems;

    function GetNodes(NodeTypes: TNodeTypes; SelectedOnly: Boolean): TNodeArray;
    function NodesToData(Nodes: TNodeArray): TTitleDataArray;
  end;

const
  WISHTEXT = 'Wishlist';
  IGNORETEXT = 'Ignorelist';
  
implementation

{ TListsTab }

constructor TListsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ShowCloseButton := False;
  ImageIndex := 30;

  FListsPanel := TTitlePanel.Create(Self);
  FListsPanel.Parent := Self;
  FListsPanel.Align := alClient;
end;

procedure TListsTab.PostTranslate;
begin
  FListsPanel.PostTranslate;
end;

procedure TListsTab.Resize;
begin
  inherited;

end;

procedure TListsTab.Setup(Clients: TClientManager; Streams: TDataLists; Images: TImageList);
begin
  Caption := 'Lists';

  FListsPanel.Setup(Clients, Streams, Images);
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
end;

{ TTitlePanel }

procedure TTitlePanel.AddClick(Sender: TObject);
begin
  if AddEntry(FAddEdit.Text, True, ltAutoDetermine) then
  begin
    FAddEdit.Text := '';

    MsgBus.SendMessage(TListsChangedMsg.Create);
  end;
end;

procedure TTitlePanel.RemoveClick(Sender: TObject);
var
  i: Integer;
  Node, SubNode, DeleteNode: PVirtualNode;
  NodeData: PTitleNodeData;
  DeleteList: TList<PVirtualNode>;
begin
  FTree.BeginUpdate;

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
            FLists.SaveList.Remove(NodeData.Title);
          ntIgnore:
            if NodeData.Stream = nil then
              FLists.IgnoreList.Remove(NodeData.Title)
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

  MsgBus.SendMessage(TListsChangedMsg.Create);

  HomeComm.SendSetSettings((FLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);

  FTree.EndUpdate;
end;

procedure TTitlePanel.RenameClick(Sender: TObject);
begin
  FTree.EditNode(FTree.GetFirstSelected, 0);
end;

procedure TTitlePanel.Resize;
begin
  inherited;

  FAddCombo.Width := (FToolbarPanel.ClientWidth - FToolbar.Width - FAddEdit.Width - FAddEdit.Left) - 6;

  FToolbar.Left := ClientWidth + 100;
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
      if Dlg.Execute(Handle) then
      begin
        Lst := TStringList.Create;
        try
          for i := 0 to ExportList.Count - 1 do
          begin
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
begin
  O := nil;
  if FAddCombo.ItemIndex > -1 then
    O := FAddCombo.Items.Objects[FAddCombo.ItemIndex];

  FAddCombo.Clear;

  for i := 0 to FClients.Count - 1 do
    if FClients[i].Entry.Name <> '' then
      FAddCombo.AddItem('  ' + FClients[i].Entry.Name, FClients[i]);
  FAddCombo.Sorted := True;
  FAddCombo.Items.InsertObject(0, _('Ignorelist'), FLists.IgnoreList);
  FAddCombo.Items.InsertObject(0, _('Wishlist'), FLists.SaveList);

  if O <> nil then
  begin
    for i := 0 to FAddCombo.Items.Count - 1 do
      if FAddCombo.Items.Objects[i] = O then
      begin
        FAddCombo.ItemIndex := i;
        Break;
      end;
  end else
    FAddCombo.ItemIndex := 0;
end;

procedure TTitlePanel.ImportClick(Sender: TObject);
var
  i, n: Integer;
  NumChars: Integer;
  Hash: Cardinal;
  Exists, UseTitleInfo: Boolean;
  Pattern, Ext: string;
  Dlg: TOpenDialog;
  Lst: TStringList;
  Title: TTitleInfo;
  List: TTitleList;
  ParentNode: PVirtualNode;
begin
  if FAddCombo.ItemIndex = 0 then
  begin
    List := FLists.SaveList;
    ParentNode := FTree.FWishNode;
  end else if FAddCombo.ItemIndex = 1 then
  begin
    List := FLists.IgnoreList;
    ParentNode := FTree.FIgnoreNode;
  end else
  begin
    List := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]).Entry.IgnoreList;
    ParentNode := nil;
  end;

  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Filter := _('All supported types') + ' (*.txt, *.m3u, *.pls)|*.txt;*.m3u;*.pls|' +  _('Text files') + ' (*.txt)|*.txt|' + _('M3U playlists') + ' (*.m3u)|*.m3u|' + _('PLS playlists') + ' (*.pls)|*.pls';
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

            Pattern := BuildPattern(Lst[i], Hash, NumChars, False);
            if NumChars <= 3 then
              Continue;

            Exists := False;
            for n := 0 to List.Count - 1 do
              if List[n].Hash = Hash then
              begin
                Exists := True;
                Break;
              end;

            if Exists then
              Continue;

            Title := TTitleInfo.Create(Lst[i]);
            List.Add(Title);
            FTree.AddTitle(Title, ParentNode, FFilterText, True);
          end;
        except
          MsgBox(GetParentForm(Self).Handle, _('The file could not be loaded.'), _('Error'), MB_ICONEXCLAMATION);
        end;

        MsgBus.SendMessage(TListsChangedMsg.Create);
      finally
        Lst.Free;
      end;
    end;
  finally
    Dlg.Free;
  end;

  HomeComm.SendSetSettings((FLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
end;

procedure TTitlePanel.PostTranslate;
begin
  FSearchText.Left := Max(FAddLabel.Width, FSearchLabel.Width) + 4;
  FAddEdit.Left := Max(FAddLabel.Width, FSearchLabel.Width) + 4;
  FAddCombo.Left := FAddEdit.Left + FAddEdit.Width + 4;
  FSearchText.Width := FAddEdit.Width;

  // Damit die ComboBox auch wieder passig wird von der Breite her
  Resize;
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

    for i := 0 to FLists.SaveList.Count - 1 do
      FTree.AddTitle(FLists.SaveList[i], FTree.FWishNode, FFilterText, FromFilter);

    for i := 0 to FLists.IgnoreList.Count - 1 do
      FTree.AddTitle(FLists.IgnoreList[i], FTree.FIgnoreNode, FFilterText, FromFilter);

    for i := 0 to FClients.Count - 1 do
    begin
      if FClients[i].Entry.IgnoreList.Count > 0 then
      begin
        ClientNode := FTree.GetNode(FClients[i]);
        for n := 0 to FClients[i].Entry.IgnoreList.Count - 1 do
          FTree.AddTitle(FClients[i].Entry.IgnoreList[n], ClientNode, FFilterText, FromFilter);
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
  if (Client.AutoRemove) or (Client.Entry.Name = '') then
    Exit;

  for i := 0 to FAddCombo.Items.Count - 1 do
    if FAddCombo.Items.Objects[i] = Client then
      Exit;

  FillClientCombo;
end;

procedure TTitlePanel.ClientRemoved(Client: TICEClient);
var
  i: Integer;
begin
  FTree.RemoveClient(Client);
  for i := 0 to FAddCombo.Items.Count - 1 do
    if FAddCombo.Items.Objects[i] = Client then
    begin
      if FAddCombo.ItemIndex = i then
        FAddCombo.ItemIndex := 0;
      FAddCombo.Items.Delete(i);
      Exit;
    end;
end;

procedure TTitlePanel.SearchTextChange(Sender: TObject);
var
  Hash: Cardinal;
  NumChars: Integer;
begin
  FFilterText := BuildPattern(FSearchText.Text, Hash, NumChars, False);
  BuildTree(True);
end;

procedure TTitlePanel.SelectIgnoredClick(Sender: TObject);
var
  i: Integer;
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

    FTree.SetFocus;

    WishNode := FTree.GetNext(WishNode);
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

    for i := 0 to FLists.TrackList.Count - 1 do
    begin
      if Like(RemoveFileExt(ExtractFileName(FLists.TrackList[i].Filename)), NodeData.Title.Pattern) then
      begin
        FTree.Selected[Node] := True;
      end;
    end;

    Node := FTree.GetNext(Node);
  end;
  FTree.SetFocus;
end;

procedure TTitlePanel.Setup(Clients: TClientManager; Lists: TDataLists; Images: TImageList);
begin
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Align := alTop;

  FSearchPanel := TPanel.Create(Self);
  FSearchPanel.Parent := Self;
  FSearchPanel.BevelOuter := bvNone;
  FSearchPanel.Align := alTop;
  FSearchPanel.Height := 24;

  FSearchLabel := TLabel.Create(Self);
  FSearchLabel.Parent := FSearchPanel;
  FSearchLabel.Caption := 'Search:';
  FSearchLabel.Left := 0;
  FSearchLabel.Top := 0;

  FSearchText := TEdit.Create(Self);
  FSearchText.Parent := FSearchPanel;
  FSearchText.Top := 2;
  FSearchText.OnChange := SearchTextChange;

  FSearchLabel.Top := FSearchText.Top + FSearchText.Height div 2 - FSearchLabel.Height div 2;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Parent := FTopPanel;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Align := alTop;
  FToolbarPanel.Height := 25;

  FAddLabel := TLabel.Create(Self);
  FAddLabel.Parent := FToolbarPanel;
  FAddLabel.Left := 0;
  FAddLabel.Caption := _('Add entry:');

  FAddEdit := TEdit.Create(Self);
  FAddEdit.Parent := FToolbarPanel;
  FAddEdit.OnKeyPress := AddEditKeyPress;
  FAddEdit.Top := 1;
  FAddEdit.Width := 250;

  FAddLabel.Top := FAddEdit.Top + FAddEdit.Height div 2 - FAddLabel.Height div 2;

  FAddCombo := TComboBox.Create(Self);
  FAddCombo.Parent := FToolbarPanel;
  FAddCombo.Style := csDropDownList;
  FAddCombo.Top := 1;

  FToolbar := TTitleToolbar.Create(Self);
  FToolbar.Parent := FToolbarPanel;
  FToolbar.Images := Images;
  FToolbar.Align := alRight;
  FToolbar.AutoSize := True;
  FToolbar.Indent := 4;
  FToolbar.Setup;
  FToolbar.FAdd.OnClick := AddClick;
  FToolbar.FRemove.OnClick := RemoveClick;
  FToolbar.FExport.OnClick := ExportClick;
  FToolbar.FImport.OnClick := ImportClick;
  FToolbar.FSelectSaved.OnClick := SelectSavedClick;
  FToolbar.FSelectIgnored.OnClick := SelectIgnoredClick;
  FToolbar.FRename.OnClick := RenameClick;

  FTopPanel.ClientHeight := FToolbarPanel.Height;

  // Das macht Höhen/Breiten von manchen Controls passig
  PostTranslate;
  FSearchPanel.ClientHeight := FSearchText.Top + 5 + FSearchText.Height;

  FTree := TTitleTree.Create(Self, Lists, Images);
  FTree.Parent := Self;
  FTree.Align := alClient;
  FTree.OnChange := TreeChange;
  FTree.OnKeyDown := TreeKeyDown;

  FClients := Clients;
  FLists := Lists;

  BuildTree(False);
  FillClientCombo;

  BevelOuter := bvNone;

  UpdateButtons;

  Resize;
end;

procedure TTitlePanel.AddEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    FToolbar.FAdd.Click;
    Key := #0;
  end;
end;

function TTitlePanel.AddEntry(Text: string; ShowMessages: Boolean; ListType: TListType): Boolean;
var
  i, NumChars: Integer;
  Pattern: string;
  Node, Parent: PVirtualNode;
  NodeData: PTitleNodeData;
  Title: TTitleInfo;
  Hash: Cardinal;
  List: TTitleList;
begin
  Result := False;

  if Trim(Text) <> '' then
  begin
    Parent := nil;

    if (ListType = ltSave) or
       ((ListType = ltAutoDetermine) and (FAddCombo.ItemIndex = 0)) then
    begin
      List := FLists.SaveList;
      Parent := FTree.FWishNode;
    end else if (ListType = ltIgnore) or
       ((ListType = ltAutoDetermine) and (FAddCombo.ItemIndex = 1)) then
    begin
      List := FLists.IgnoreList;
      Parent := FTree.FIgnoreNode;
    end else
    begin
      List := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]).Entry.IgnoreList;
    end;

    Pattern := BuildPattern(Trim(Text), Hash, NumChars, False);

    for i := 0 to List.Count - 1 do
      if List[i].Hash = Hash then
      begin
        if ShowMessages then
          MsgBox(GetParentForm(Self).Handle, Format(_('The list already contains an entry matching the pattern "%s".'), [Pattern]), _('Info'), MB_ICONINFORMATION);
        Exit;
      end;

    if (NumChars <= 3) and ShowMessages then
    begin
      TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('A short pattern may produce many matches, i.e. using ''a'' records/ignores every song containing an ''a''.'), 6, btOK);
    end;

    if Parent = nil then
      Parent := FTree.GetNode(TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]));

    Title := TTitleInfo.Create(Trim(Text));
    Node := FTree.AddTitle(Title, Parent, FFilterText, True);
    if Node <> nil then
    begin
      NodeData := FTree.GetNodeData(Node);
      NodeData.Title := Title;

      if (List <> FLists.SaveList) and (List <> FLists.IgnoreList) then
        NodeData.Stream := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]);

      if Node.Parent.ChildCount = 1 then
        FTree.Expanded[Node.Parent] := True;
    end;

    List.Add(Title);

    HomeComm.SendSetSettings((FLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);

    Result := True;
  end else
    if ShowMessages then
      MsgBox(GetParentForm(Self).Handle, _('Please enter a pattern to add to the list.'), _('Info'), MB_ICONINFORMATION);
end;

procedure TTitlePanel.TreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  List: TTitleList;
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
      for i := 0 to FAddCombo.Items.Count - 1 do
        if (FAddCombo.Items.Objects[i] is TICEClient) and
           (TICEClient(FAddCombo.Items.Objects[i]).Entry.IgnoreList = List) then
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
  SingleParentSelected, TitlesSelected, CanRemove, CanRename, CanImport: Boolean;
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

  TitlesSelected := (TypeCount(ntWish) > 0) or (TypeCount(ntIgnore) > 0);
  CanRemove := TitlesSelected or (TypeCount(ntStream) > 0);
  CanRename := (FTree.SelectedCount = 1) and TitlesSelected;
  CanImport := True; // FTree.SelectedCount = 1;

  FToolbar.FRemove.Enabled := CanRemove;
  FToolbar.FRename.Enabled := CanRename;
  FToolbar.FExport.Enabled := (TitlesSelected and (not SingleParentSelected) and (Length(SelectedParents) = 0)) or
                              (SingleParentSelected and (not TitlesSelected));
  FToolbar.FImport.Enabled := CanImport;

  FTree.FPopupMenu.FRemove.Enabled := CanRemove;
  FTree.FPopupMenu.FRename.Enabled := CanRename;
  FTree.FPopupMenu.FExport.Enabled := TitlesSelected;
  FTree.FPopupMenu.FImport.Enabled := CanImport;
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
begin
  inherited;

  ShowHint := True;
  Transparent := True;
end;

procedure TTitleToolbar.Setup;
var
  Sep: TToolButton;
begin
  FImport := TToolButton.Create(Self);
  FImport.Parent := Self;
  FImport.Hint := 'Import...';
  FImport.ImageIndex := 36;

  FExport := TToolButton.Create(Self);
  FExport.Parent := Self;
  FExport.Hint := 'Export...';
  FExport.ImageIndex := 35;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;
  Sep.Width := 8;

  FSelectIgnored := TToolButton.Create(Self);
  FSelectIgnored.Parent := Self;
  FSelectIgnored.Hint := 'Select ignored titles';
  FSelectIgnored.ImageIndex := 84;

  FSelectSaved := TToolButton.Create(Self);
  FSelectSaved.Parent := Self;
  FSelectSaved.Hint := 'Select saved titles (by pattern)';
  FSelectSaved.ImageIndex := 70;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;
  Sep.Width := 8;

  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := 'Remove';
  FRemove.ImageIndex := 2;

  FRename := TToolButton.Create(Self);
  FRename.Parent := Self;
  FRename.Hint := 'Rename';
  FRename.ImageIndex := 74;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;
  Sep.Width := 8;

  FAdd := TToolButton.Create(Self);
  FAdd.Parent := Self;
  FAdd.Hint := 'Add';
  FAdd.ImageIndex := 11;
end;

{ TTitleTree }

function TTitleTree.AddTitle(Title: TTitleInfo; Parent: PVirtualNode; FilterText: string; FromFilter: Boolean): PVirtualNode;
var
  AttachMode: TVTNodeAttachMode;
  Node, SearchNode, LastFoundChild: PVirtualNode;
  NodeData, SearchNodeData, ParentData: PTitleNodeData;
begin
  Result := nil;

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

constructor TTitleTree.Create(AOwner: TComponent; Lists: TDataLists; Images: TImageList);
begin
  inherited Create(AOwner);

  FPanel := TTitlePanel(AOwner);
  FLists := Lists;

  NodeDataSize := SizeOf(TTitleNodeData);
  IncrementalSearch := isVisibleOnly;
  Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];
  TreeOptions.AutoOptions := [toAutoScrollOnExpand];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect, toShowRoot, toShowButtons];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop];
  Header.Options := Header.Options + [hoAutoResize];
  Header.Options := Header.Options - [hoDrag];
  Header.AutoSizeIndex := 0;
  DragMode := dmManual;
  ShowHint := True;
  HintMode := hmTooltip;

  Self.Images := Images;

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Title');

  FColAdded := Header.Columns.Add;
  FColAdded.Text := _('Date');
  FColAdded.Width := 90;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfFile];
  FDropTarget.OnDrop := DropTargetDrop;
  FDropTarget.Register(Self);

  FPopupMenu := TTitlePopup.Create(Self);
  FPopupMenu.Images := Images;
  FPopupMenu.FRemove.OnClick := PopupMenuClick;
  FPopupMenu.FRename.OnClick := PopupMenuClick;
  FPopupMenu.FSelectSaved.OnClick := PopupMenuClick;
  FPopupMenu.FSelectIgnored.OnClick := PopupMenuClick;
  FPopupMenu.FExport.OnClick := PopupMenuClick;
  FPopupMenu.FImport.OnClick := PopupMenuClick;

  PopupMenu := FPopupMenu;
end;

destructor TTitleTree.Destroy;
begin

  inherited;
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
  List: TTitleList;
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
          List := FLists.SaveList;
        end;
      ntIgnoreParent:
        begin
          List := FLists.IgnoreList;
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
                List := FLists.SaveList;
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
                List := FLists.IgnoreList;
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
    Title := TTitleInfo.Create(RemoveFileExt(ExtractFileName(FDropTarget.Files[i])));

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

      if List = FLists.SaveList then
        HomeComm.SendSetSettings(True);
    end;
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
  else if Sender = FPopupMenu.FSelectSaved then
    TTitlePanel(Owner).FToolbar.FSelectSaved.Click
  else if Sender = FPopupMenu.FSelectIgnored then
    TTitlePanel(Owner).FToolbar.FSelectIgnored.Click
  else if Sender = FPopupMenu.FExport then
    TTitlePanel(Owner).FToolbar.FExport.Click
  else if Sender = FPopupMenu.FImport then
    TTitlePanel(Owner).FToolbar.FImport.Click;
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

procedure TTitleTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PTitleNodeData;
begin
  inherited;

  if TextType = ttNormal then
  begin
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
              Text := NodeData.Stream.Entry.Name;
            ntWish, ntIgnore:
              Text := NodeData.Title.Title;
          end;

          if NodeData.NodeType in [ntWishParent, ntIgnoreParent, ntStream] then
            Text := Text + ' (' + IntToStr(ChildCount[Node]) + ')';
        end;
      1:
        begin
          if NodeData.Title <> nil then
          begin
            Text := DateToStr(NodeData.Title.Added);
          end else
            Text := '';
        end;
    end;
  end;
end;

function TTitleTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
var
  NodeData: PTitleNodeData;
begin
  Result := inherited;

  NodeData := GetNodeData(Node);

  if Column = 0 then
    case NodeData.NodeType of
      ntWishParent, ntWish:
        Index := 31;
      ntIgnoreParent, ntIgnore:
        Index := 65;
      ntStream:
        Index := 16;
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

function TTitleTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: string): Integer;
var
  s: string;
  NodeData: PTitleNodeData;
begin
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
    Result := StrLIComp(PChar(s), PChar(NodeData.Stream.Entry.Name), Min(Length(s), Length(NodeData.Stream.Entry.Name)))
end;

procedure TTitleTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  Text: UnicodeString);
var
  NodeData: PTitleNodeData;
begin
  inherited;

  if Trim(Text) = '' then
    Exit;

  NodeData := GetNodeData(Node);

  NodeData.Title.Free;
  NodeData.Title := TTitleInfo.Create(Text);
end;

procedure TTitleTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var
  NodeData: PTitleNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  Allowed := NodeData.Title <> nil;
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
          Result := CompareText(Data1.Stream.Entry.Name, Data2.Stream.Entry.Name)
        else if (Data1.Title <> nil) and (Data2.Stream = nil) then
          Exit(1)
        else if (Data1.Title = nil) and (Data2.Stream <> nil) then
          Exit(-1);
      end;
    1:
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

procedure TTitleTree.DoFreeNode(Node: PVirtualNode);
begin

  inherited;
end;

{ TTitlePopup }

constructor TTitlePopup.Create(AOwner: TComponent);
var
  Sep: TMenuItem;
begin
  inherited;

  FRename := CreateMenuItem;
  FRename.Caption := 'Ren&ame';
  FRename.ImageIndex := 74;
  Items.Add(FRename);

  FRemove := CreateMenuItem;
  FRemove.Caption := '&Remove';
  FRemove.ImageIndex := 2;
  Items.Add(FRemove);

  Sep := CreateMenuItem;
  Sep.Caption := '-';
  Items.Add(Sep);

  FSelectSaved := CreateMenuItem;
  FSelectSaved.Caption := '&Select saved titles (by pattern)';
  FSelectSaved.ImageIndex := 70;
  Items.Add(FSelectSaved);

  FSelectIgnored := CreateMenuItem;
  FSelectIgnored.Caption := 'Se&lect ignored titles';
  FSelectIgnored.ImageIndex := 84;
  Items.Add(FSelectIgnored);

  Sep := CreateMenuItem;
  Sep.Caption := '-';
  Items.Add(Sep);

  FExport := CreateMenuItem;
  FExport.Caption := '&Export...';
  FExport.ImageIndex := 35;
  Items.Add(FExport);

  FImport := CreateMenuItem;
  FImport.Caption := '&Import...';
  FImport.ImageIndex := 36;
  Items.Add(FImport);
end;

end.
