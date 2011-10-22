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
unit ListsTab;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, MControls, LanguageObjects, Tabs, VirtualTrees, DataManager,
  ImgList, Functions, GUIFunctions, Menus, Math, DragDrop, DropComboTarget,
  Dialogs, MsgDlg, Forms, Logging, AppData, HomeCommunication, ICEClient,
  ClientManager, Generics.Collections, TypeDefs;

type
  TTitleTree = class;

  TTitleNodeData = record
    Title: TTitleInfo;
    Stream: TICEClient;
  end;
  PTitleNodeData = ^TTitleNodeData;

  TTitleToolbar = class(TToolbar)
  private
    FAdd: TToolButton;
    FRemove: TToolButton;
    FSep: TToolButton;
    FExport: TToolButton;
    FImport: TToolButton;
    FSelectSaved: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableItems(Enable: Boolean);

    procedure Setup;
  end;

  TTitlePanel = class(TPanel)
  private
    FLabel: TLabel;
    FTopPanel: TPanel;
    FToolbarPanel: TPanel;
    FAddEdit: TEdit;
    FAddCombo: TComboBox;
    FToolbar: TTitleToolbar;
    FTree: TTitleTree;

    FListType: TListType;
    FList: TTitleList;
    FClients: TClientManager;
    FLists: TDataLists;

    procedure BuildTree;
    procedure UpdateButtons;

    procedure AddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure ImportClick(Sender: TObject);
    procedure SelectSavedClick(Sender: TObject);
    procedure AddEditKeyPress(Sender: TObject; var Key: Char);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Resize; override;
  public
    procedure ClientAdded(Client: TICEClient);
    procedure ClientRemoved(Client: TICEClient);
    procedure Setup(Clients: TClientManager; Lists: TDataLists; T: TListType; Images: TImageList; Title: string);
    procedure UpdateList;
  end;

  TListsTab = class(TMainTabSheet)
  private
    FWishPanel, FIgnorePanel: TTitlePanel;
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
  end;

  TTitleTree = class(TVirtualStringTree)
  private
    FType: TListType;
    FColTitle: TVirtualTreeColumn;
    FColAdded: TVirtualTreeColumn;

    FLists: TDataLists;
    FListType: TListType;
    FDropTarget: TDropComboTarget;

    function GetNode(Stream: TICEClient): PVirtualNode;

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: string): Integer; override;
    procedure DoFreeNode(Node: PVirtualNode); override;
  public
    constructor Create(AOwner: TComponent; Lists: TDataLists; T: TListType);
    destructor Destroy; override;

    procedure AddTitle(Title: TTitleInfo; Parent: PVirtualNode; ParentData: PTitleNodeData);
    procedure RemoveTitle(Title: TTitleInfo);
    procedure RemoveClient(Client: TICEClient);
    procedure SortItems;
  end;

implementation

{ TListsTab }

constructor TListsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ShowCloseButton := False;
  ImageIndex := 30;

  FWishPanel := TTitlePanel.Create(Self);
  FWishPanel.Parent := Self;

  FIgnorePanel := TTitlePanel.Create(Self);
  FIgnorePanel.Parent := Self;
end;

procedure TListsTab.Resize;
begin
  inherited;

  FWishPanel.Top := 0;
  FWishPanel.Left := 0;
  FWishPanel.Height := ClientHeight;
  FWishPanel.Width := ClientWidth div 2 - 2;

  FIgnorePanel.Top := 0;
  FIgnorePanel.Left := FWishPanel.Left + FWishPanel.Width + 4;
  FIgnorePanel.Height := ClientHeight;
  FIgnorePanel.Width := ClientWidth div 2 - 2;
end;

procedure TListsTab.Setup(Clients: TClientManager; Streams: TDataLists; Images: TImageList);
begin
  Caption := 'Lists';

  FWishPanel.Setup(Clients, Streams, ltSave, Images, 'Wishlist');
  FIgnorePanel.Setup(Clients, Streams, ltIgnore, Images, 'Ignorelist');
end;

procedure TListsTab.UpdateLists;
begin
  FWishPanel.UpdateList;
  FIgnorePanel.UpdateList;
end;

procedure TListsTab.AddClient(Client: TICEClient);
begin
  FWishPanel.ClientAdded(Client);
  FIgnorePanel.ClientAdded(Client);
end;

procedure TListsTab.RemoveClient(Client: TICEClient);
begin
  FWishPanel.ClientRemoved(Client);
  FIgnorePanel.ClientRemoved(Client);
end;

procedure TListsTab.RemoveTitle(Client: TICEClient; ListType: TListType;
  Title: TTitleInfo);
begin
  if ListType = ltSave then
  begin
    FWishPanel.FTree.RemoveTitle(Title);
  end;
end;

procedure TListsTab.AddTitle(Client: TICEClient; ListType: TListType; Title: TTitleInfo);
begin
  if ListType = ltSave then
  begin
    FWishPanel.FTree.AddTitle(Title, FWishPanel.FTree.GetNode(Client),
      FWishPanel.FTree.GetNodeData(FWishPanel.FTree.GetNode(Client)));
    //FWishPanel.FTree.SortItems;
  end else
  begin
    FIgnorePanel.FTree.AddTitle(Title, FIgnorePanel.FTree.GetNode(Client),
      FIgnorePanel.FTree.GetNodeData(FIgnorePanel.FTree.GetNode(Client)));
    //FIgnorePanel.FTree.SortItems;
  end;
end;

{ TTitlePanel }

procedure TTitlePanel.AddClick(Sender: TObject);
var
  i, NumChars: Integer;
  Pattern: string;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Title: TTitleInfo;
  Hash: Cardinal;
  List: TTitleList;
begin
  if Trim(FAddEdit.Text) <> '' then
  begin
    Pattern := BuildPattern(FAddEdit.Text, Hash, NumChars, False);

    List := nil;
    if FListType = ltSave then
    begin
      if FAddCombo.Items.Objects[FAddCombo.ItemIndex] <> nil then
        List := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]).Entry.SaveList;
    end else
    begin
      if FAddCombo.Items.Objects[FAddCombo.ItemIndex] <> nil then
        List := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]).Entry.IgnoreList;
    end;

    if List = nil then
      List := FList;

    if NumChars <= 3 then
    begin
      TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('A short pattern may produce many matches, i.e. using ''a'' records/ignores every song containing an ''a''.'), 6, btOK);
    end;

    for i := 0 to List.Count - 1 do
      if List[i].Hash = Hash then
      begin
        MsgBox(GetParentForm(Self).Handle, Format(_('The list already contains an entry matching the pattern "%s".'), [Pattern]), _('Info'), MB_ICONINFORMATION);
        Exit;
      end;

    Node := FTree.AddChild(FTree.GetNode(TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex])));
    NodeData := FTree.GetNodeData(Node);
    Title := TTitleInfo.Create(Trim(FAddEdit.Text));
    NodeData.Title := Title;
    NodeData.Stream := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]);
    List.Add(Title);
    FAddEdit.Text := '';

    if Node.Parent.ChildCount = 1 then
      FTree.Expanded[Node.Parent] := True;

    //FTree.SortItems;

    HomeComm.SetTitleNotifications((FLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
  end else
    MsgBox(GetParentForm(Self).Handle, _('Please enter a pattern to add to list.'), _('Info'), MB_ICONINFORMATION);
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
        SubNode := FTree.GetFirstChild(Node);
        while SubNode <> nil do
        begin
          if not DeleteList.Contains(SubNode) then
            DeleteList.Add(SubNode);
          SubNode := FTree.GetNextSibling(SubNode);
        end;

        if not DeleteList.Contains(Node) then
          DeleteList.Add(Node);
      end;
      Node := FTree.GetNext(Node);
    end;

    for i := DeleteList.Count - 1 downto 0 do
    begin
      NodeData := FTree.GetNodeData(DeleteList[i]);
      if (FTree.ChildCount[DeleteList[i]] = 0) and (NodeData.Title <> nil) then
      begin
        if (NodeData.Stream = nil) and (NodeData.Title <> nil) then
          FList.Remove(NodeData.Title);
        if (NodeData.Stream <> nil) and (NodeData.Title <> nil) then
        begin
          if FListType = ltSave then
            NodeData.Stream.Entry.SaveList.Remove(NodeData.Title)
          else if FListType = ltIgnore then
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
      if (FTree.GetFirstChild(Node) = nil) and (NodeData.Title = nil) then
        DeleteNode := Node;
      Node := FTree.GetPrevious(Node);
      if DeleteNode <> nil then
        FTree.DeleteNode(DeleteNode);
    end;
  finally
    DeleteList.Free;
  end;

  HomeComm.SetTitleNotifications((FLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);

  FTree.EndUpdate;
end;

procedure TTitlePanel.Resize;
begin
  inherited;

  if FAddCombo.Visible then
  begin
    FAddEdit.Width := (FToolbarPanel.ClientWidth - FToolbar.Width) div 2 - 4;
    FAddCombo.Width := (FToolbarPanel.ClientWidth - FToolbar.Width) div 2;
  end else
    FAddEdit.Width := FToolbarPanel.ClientWidth - FToolbar.Width;

  FAddCombo.Left := 10;
  FToolbar.Left := ClientWidth + 100;
end;

procedure TTitlePanel.ExportClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Dlg: TSaveDialog;
  Lst: TStringList;
  Ok: Boolean;
begin
  Ok := False;
  Node := FTree.GetFirst;
  while Node <> nil do
  begin
    NodeData := FTree.GetNodeData(Node);
    if FTree.Selected[Node] and (NodeData.Title <> nil) then
    begin
      Ok := True;
      Break;
    end;
    Node := FTree.GetNext(Node);
  end;

  if not Ok then
  begin
    MsgBox(GetParentForm(Self).Handle, _('Please select at least one title to export.'), _('Info'), MB_ICONINFORMATION);
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
        Node := FTree.GetFirst;
        while Node <> nil do
        begin
          NodeData := FTree.GetNodeData(Node);
          if FTree.Selected[Node] and (NodeData.Title <> nil) then
            Lst.Add(NodeData.Title.Title);
          Node := FTree.GetNext(Node);
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
begin
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

            if FAddCombo.Items.Objects[FAddCombo.ItemIndex] = nil then
              if FListType = ltSave then
                List := FLists.SaveList
              else
                List := FLists.IgnoreList
            else
              if FListType = ltSave then
                List := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]).Entry.SaveList
              else
                List := TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]).Entry.IgnoreList;

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
            FTree.AddTitle(Title, FTree.GetNode(TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex])),
              FTree.GetNodeData(FTree.GetNode(TICEClient(FAddCombo.Items.Objects[FAddCombo.ItemIndex]))));
          end;
        except
          MsgBox(GetParentForm(Self).Handle, _('The file could not be loaded.'), _('Error'), MB_ICONEXCLAMATION);
        end;
      finally
        Lst.Free;
      end;
    end;
  finally
    Dlg.Free;
  end;

  //FTree.SortItems;

  HomeComm.SetTitleNotifications((FLists.SaveList.Count > 0) and AppGlobals.AutoTuneIn);
end;

procedure TTitlePanel.BuildTree;
var
  Ok: Boolean;
  i, n: Integer;
  Node, ClientNode: PVirtualNode;
  ClientNodeData: PTitleNodeData;
begin
  FTree.BeginUpdate;
  try
    if FList.Count > 0 then
    begin
      ClientNode := FTree.GetNode(nil);
      for i := 0 to FList.Count - 1 do
        FTree.AddTitle(FList[i], ClientNode, nil);
    end;

    for i := 0 to FClients.Count - 1 do
    begin
      if FListType = ltSave then
        Ok := FClients[i].Entry.SaveList.Count > 0
      else
        Ok := FClients[i].Entry.IgnoreList.Count > 0;

      if Ok then
      begin
        ClientNode := FTree.GetNode(FClients[i]);
        ClientNodeData := FTree.GetNodeData(ClientNode);
        if FListType = ltSave then
          for n := 0 to FClients[i].Entry.SaveList.Count - 1 do
            FTree.AddTitle(FClients[i].Entry.SaveList[n], ClientNode, ClientNodeData)
        else
          for n := 0 to FClients[i].Entry.IgnoreList.Count - 1 do
            FTree.AddTitle(FClients[i].Entry.IgnoreList[n], ClientNode, ClientNodeData);
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

  FTree.Header.SortColumn := -1;

  FTree.SortItems;
end;

procedure TTitlePanel.ClientAdded(Client: TICEClient);
var
  i: Integer;
  O: TObject;
begin
  if Client.AutoRemove then
    Exit;

  for i := 0 to FAddCombo.Items.Count - 1 do
    if FAddCombo.Items.Objects[i] = Client then
      Exit;

  O := FAddCombo.Items.Objects[FAddCombo.ItemIndex];

  // Unbedingt mit InsertObject. Das normale Add hat das neue Teil teilweise mit Index 0 geadded.
  FAddCombo.Items.InsertObject(FAddCombo.Items.Count, Client.Entry.Name, Client);

  FAddCombo.Items.Delete(0);
  FAddCombo.Sorted := False;
  FAddCombo.Sorted := True;
  FAddCombo.Items.InsertObject(0, _('Global'), nil);

  for i := 0 to FAddCombo.Items.Count - 1 do
    if FAddCombo.Items.Objects[i] = O then
    begin
      FAddCombo.ItemIndex := i;
      Break;
    end;

  if not (((FListType = ltSave) and (Client.Entry.SaveList.Count = 0)) or
         ((FListType = ltIgnore) and (Client.Entry.IgnoreList.Count = 0)))
  then
    FTree.GetNode(Client);
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

procedure TTitlePanel.SelectSavedClick(Sender: TObject);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Dlg: TSaveDialog;
  Lst: TStringList;
  Ok: Boolean;
begin
  Node := FTree.GetFirst;
  while Node <> nil do
  begin
    FTree.Selected[Node] := False;
    NodeData := FTree.GetNodeData(Node);

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

procedure TTitlePanel.Setup(Clients: TClientManager; Lists: TDataLists; T: TListType; Images: TImageList; Title: string);
var
  i: Integer;
begin
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Align := alTop;
  FTopPanel.Padding.Top := 0;
  FTopPanel.Padding.Left := 0;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := FTopPanel;
  FLabel.Caption := Title;
  FLabel.Align := alTop;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Parent := FTopPanel;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Align := alTop;
  FToolbarPanel.Height := 26;
  FToolbarPanel.Padding.Top := 2;

  FAddEdit := TEdit.Create(Self);
  FAddEdit.Parent := FToolbarPanel;
  FAddEdit.OnKeyPress := AddEditKeyPress;
  FAddEdit.Left := 0;
  FAddEdit.Top := 2;

  FAddCombo := TComboBox.Create(Self);
  FAddCombo.Parent := FToolbarPanel;
  FAddCombo.Style := csDropDownList;
  FAddCombo.Align := alRight;

  FToolbar := TTitleToolbar.Create(Self);
  FToolbar.Parent := FToolbarPanel;
  FToolbar.Images := Images;
  FToolbar.Align := alRight;
  FToolbar.Height := 24;
  FToolbar.AutoSize := True;
  FToolbar.Indent := 4;
  FToolbar.Setup;
  FToolbar.FAdd.OnClick := AddClick;
  FToolbar.FRemove.OnClick := RemoveClick;
  FToolbar.FExport.OnClick := ExportClick;
  FToolbar.FImport.OnClick := ImportClick;
  FToolbar.FSelectSaved.OnClick := SelectSavedClick;

  FTopPanel.Height := FLabel.Height + FToolbarPanel.Height + 2;

  FTree := TTitleTree.Create(Self, Lists, T);
  FTree.Parent := Self;
  FTree.Images := Images;
  FTree.Align := alClient;
  FTree.OnChange := TreeChange;
  FTree.OnKeyDown := TreeKeyDown;

  FTree.FType := T;
  if T = ltSave then
    FList := Lists.SaveList
  else
    FList := Lists.IgnoreList;
  FListType := T;
  FClients := Clients;
  FLists := Lists;

  BuildTree;

  for i := 0 to FClients.Count - 1 do
    FAddCombo.AddItem(FClients[i].Entry.Name, FClients[i]);
  FAddCombo.Sorted := True;
  FAddCombo.Items.InsertObject(0, _('Global'), nil);
  FAddCombo.ItemIndex := 0;

  FToolbar.FRemove.Enabled := FTree.SelectedCount > 0;

  BevelOuter := bvNone;

  if FListType = ltSave then
  begin
    FAddCombo.Visible := False;
  end else
  begin
    FToolbar.FSelectSaved.Visible := False;
  end;

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

procedure TTitlePanel.TreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  i: Integer;
  NodeData: PTitleNodeData;
begin
  FToolbar.FRemove.Enabled := FTree.SelectedCount > 0;

  Node := FTree.GetFirstSelected;
  if Node <> nil then
  begin
    NodeData := FTree.GetNodeData(Node);

    if NodeData.Stream = nil then
    begin
      FAddCombo.ItemIndex := 0;
    end else
    begin
      for i := 0 to FAddCombo.Items.Count - 1 do
        if FAddCombo.Items.Objects[i] = NodeData.Stream then
        begin
          FAddCombo.ItemIndex := i;
          Break;
        end;
    end;
  end else
  begin
    FAddCombo.ItemIndex := 0;
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
begin
  FToolbar.FRemove.Enabled := FTree.GetFirstSelected <> nil;
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

procedure TTitleToolbar.EnableItems(Enable: Boolean);
begin

end;

procedure TTitleToolbar.Setup;
begin
  FImport := TToolButton.Create(Self);
  FImport.Parent := Self;
  FImport.Hint := 'Import...';
  FImport.ImageIndex := 36;

  FExport := TToolButton.Create(Self);
  FExport.Parent := Self;
  FExport.Hint := 'Export...';
  FExport.ImageIndex := 35;

  FSep := TToolButton.Create(Self);
  FSep.Parent := Self;
  FSep.Style := tbsSeparator;
  FSep.Width := 8;

  FSelectSaved := TToolButton.Create(Self);
  FSelectSaved.Parent := Self;
  FSelectSaved.Hint := 'Select saved (by pattern)';
  FSelectSaved.ImageIndex := 70;

  FRemove := TToolButton.Create(Self);
  FRemove.Parent := Self;
  FRemove.Hint := 'Remove';
  FRemove.ImageIndex := 2;

  FAdd := TToolButton.Create(Self);
  FAdd.Parent := Self;
  FAdd.Hint := 'Add';
  FAdd.ImageIndex := 11;
end;

{ TTitleTree }

procedure TTitleTree.AddTitle(Title: TTitleInfo; Parent: PVirtualNode; ParentData: PTitleNodeData);
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Node := AddChild(Parent);
  NodeData := GetNodeData(Node);
  NodeData.Title := Title;
  NodeData.Stream := nil;
  if ParentData <> nil then
    NodeData.Stream := ParentData.Stream;

  if (Parent <> nil) and (Parent.ChildCount = 1) then
  begin
    Expanded[Parent] := True;
  end;
end;

constructor TTitleTree.Create(AOwner: TComponent; Lists: TDataLists; T: TListType);
begin
  inherited Create(AOwner);

  FLists := Lists;
  FListType := T;

  NodeDataSize := SizeOf(TTitleNodeData);
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

  FColTitle := Header.Columns.Add;
  FColTitle.Text := _('Title');

  FColAdded := Header.Columns.Add;
  FColAdded.Text := _('Date');
  FColAdded.Width := 90;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfFile];
  FDropTarget.OnDrop := DropTargetDrop;
  FDropTarget.Register(Self);
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
  NodeData: PTitleNodeData;
  Title: TTitleInfo;
  List: TTitleList;
  Stream: TICEClient;
begin
  Stream := nil;

  GetHitTestInfoAt(APoint.X, APoint.Y, True, HI);
  if Hi.HitNode <> nil then
  begin
    if HI.HitNode.Parent <> RootNode then
    begin
      Node := HI.HitNode.Parent;
    end else
      Node := HI.HitNode;

    NodeData := GetNodeData(Node);

    if (NodeData.Stream <> nil) or ((NodeData.Stream = nil) and (NodeData.Title = nil)) then
    begin
      Stream := NodeData.Stream;
    end;
  end;

  if FType = ltSave then
    if Stream = nil then
      List := FLists.SaveList
    else
      List := Stream.Entry.SaveList
  else
    if Stream = nil then
      List := FLists.IgnoreList
    else
      List := Stream.Entry.IgnoreList;

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
      AddTitle(Title, GetNode(Stream), GetNodeData(GetNode(Stream)));
  end;
end;

function TTitleTree.GetNode(Stream: TICEClient): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  if FType = ltIgnore then
  begin
    Node := GetFirst;
    while Node <> nil do
    begin
      NodeData := GetNodeData(Node);
      if (NodeData.Stream = Stream) and (NodeData.Title = nil) then
        Exit(Node);
      Node := GetNext(Node);
    end;

    Result := AddChild(nil);
    NodeData := GetNodeData(Result);
    NodeData.Stream := Stream;
    NodeData.Title := nil;
  end else
    Result := nil;
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
          if (NodeData.Stream = nil) and (NodeData.Title = nil) then
            Text := _('Global')
          else if NodeData.Title <> nil then
            Text := NodeData.Title.Title
          else
            Text := NodeData.Stream.Entry.Name;

          if (Node.Parent = RootNode) and (FListType = ltIgnore) then
            Text := Text + ' (' + IntToStr(ChildCount[Node]) + ')';
        end;
      1:
        begin
          if NodeData.Title <> nil then
          begin
            {if Trunc(NodeData.Title.Added) = Trunc(Now) then
              Text := TimeToStr(NodeData.Title.Added)
            else}
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
    if NodeData.Title <> nil then
      if FType = ltSave then
        Index := 31
      else
        Index := 65
    else
      if NodeData.Stream = nil then
        Index := 3
      else
        Index := 16;
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
  Result := 0;
  S := Text;
  NodeData := GetNodeData(Node);
  if NodeData = nil then
    Exit;
  if NodeData.Title <> nil then
    Result := StrLIComp(PChar(s), PChar(NodeData.Title.Title), Min(Length(s), Length(NodeData.Title.Title)))
  else
    if NodeData.Stream <> nil then
      Result := StrLIComp(PChar(s), PChar(NodeData.Stream.Entry.Name), Min(Length(s), Length(NodeData.Stream.Entry.Name)))
    else
      Result := StrLIComp(PChar(s), PChar(_('Global')), Min(Length(s), Length(_('Global'))));
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
      if (Data1.Title <> nil) and (Data2.Title <> nil) then
        Result := CompareText(Data1.Title.Title, Data2.Title.Title)
      else
        Result := CompareText(Data1.Stream.Entry.Name, Data2.Stream.Entry.Name);
    1:
      if (Data1.Title <> nil) and (Data2.Title <> nil) then
        Result := CmpTime(Data1.Title.Added, Data2.Title.Added)
      else
        Result := 0;
  end;
end;

procedure TTitleTree.DoFreeNode(Node: PVirtualNode);
begin

  inherited;
end;

end.
