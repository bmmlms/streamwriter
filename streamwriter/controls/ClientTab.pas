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
unit ClientTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, ClientView, StreamBrowserView, StreamDebugView, StreamInfoView,
  LanguageObjects, HomeCommunication, StationCombo, Menus, ActnList, ImgList,
  RecentManager, ICEClient, ClientManager, VirtualTrees, Clipbrd, Functions,
  GUIFunctions, AppData, DropTarget, DragDropInternet, DragDropText,
  DragDropFile, Dialogs, ShellAPI, Tabs;

type
  TSidebar = class(TPageControl)
  private
    FPage1, FPage2, FPage3: TTabSheet;

    FBrowserView: TMStreamBrowserView;
    FInfoView: TMStreamInfoView;
    FDebugView: TMStreamDebugView;

    FHomeCommunication: THomeCommunication;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(HomeCommunication: THomeCommunication);

    property BrowserView: TMStreamBrowserView read FBrowserView;
    property InfoView: TMStreamInfoView read FInfoView;
  end;

  TClientAddressBar = class(TPanel)
  private
    FLabel: TLabel;
    FStations: TMStationCombo;
    FStart: TSpeedButton;
    FDropTarget: TDropURLTarget;

    procedure FStationsChange(Sender: TObject);
    procedure FStationsKeyPress(Sender: TObject; var Key: Char);

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Setup;

    property Stations: TMStationCombo read FStations;
  end;

  TCutEvent = procedure(Filename: string) of object;

  TClientTab = class(TMainTabSheet)
  private
    FToolbar: TToolBar;
    FAddressBar: TClientAddressBar;
    FClientView: TMClientView;
    FSplitter: TSplitter;
    FSideBar: TSideBar;

    FClients: TClientManager;
    FStreams: TStreamDataList;
    FHomeCommunication: THomeCommunication;
    FDropTarget: TDropURLTarget;

    FReceived: UInt64;
    FRefreshInfo: Boolean;

    FActionRemove: TAction;
    FActionShowSideBar: TAction;
    FActionTuneInRelay: TAction;
    FActionTuneInFile: TAction;
    FActionTuneInStream: TAction;

    FOnUpdateButtons: TNotifyEvent;
    FOnCut: TCutEvent;

    procedure SavePlaylist(Entries: TPlaylistEntryArray; Open: Boolean);

    procedure ShowInfo;

    procedure ActionStartExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionResetDataExecute(Sender: TObject);
    procedure ActionShowSideBarExecute(Sender: TObject);
    procedure ActionSavePlaylistStream(Sender: TObject);
    procedure ActionSavePlaylistRelay(Sender: TObject);
    procedure ActionSavePlaylistFile(Sender: TObject);
    procedure ActionTuneInStream(Sender: TObject);
    procedure ActionTuneInRelay(Sender: TObject);
    procedure ActionTuneInFile(Sender: TObject);

    procedure ClientManagerDebug(Sender: TObject);
    procedure ClientManagerRefresh(Sender: TObject);
    procedure ClientManagerAddRecent(Sender: TObject);
    procedure ClientManagerClientAdded(Sender: TObject);
    procedure ClientManagerClientRemoved(Sender: TObject);
    procedure ClientManagerSongSaved(Sender: TObject; Filename, Title: string);
    procedure ClientManagerTitleChanged(Sender: TObject; Title: string);
    procedure ClientManagerICYReceived(Sender: TObject; Received: Integer);

    procedure FClientViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FClientViewDblClick(Sender: TObject);
    procedure FClientViewKeyPress(Sender: TObject; var Key: Char);
    procedure FClientViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure StationsStreamChanged(Sender: TObject; Stream: TStreamEntry);

    procedure StreamBrowserAction(Sender: TObject; Action: TOpenActions; Streams: TStreamDataArray);
    procedure StreamInfoAction(Sender: TObject; Action: TTrackActions; Tracks: TTrackInfoArray);

    procedure DebugClear(Sender: TObject);

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Setup(Toolbar: TToolbar; Actions: TActionList; Popup: TPopupMenu; MenuImages,
      ClientImages: TImageList; Clients: TClientManager;
      Streams: TStreamDataList; HomeCommunication: THomeCommunication);
    procedure Shown;
    function StartStreaming(Name, URL: string): Boolean;
    procedure TimerTick;

    property AddressBar: TClientAddressBar read FAddressBar;
    property ClientView: TMClientView read FClientView;
    property SideBar: TSideBar read FSideBar;
    property Received: UInt64 read FReceived;
    property OnUpdateButtons: TNotifyEvent read FOnUpdateButtons write FOnUpdateButtons;
    property OnCut: TCutEvent read FOnCut write FOnCut;
  end;

implementation

{ TClientAddressBar }

constructor TClientAddressBar.Create(AOwner: TComponent);
begin
  inherited;


end;

destructor TClientAddressBar.Destroy;
begin

  inherited;
end;

procedure TClientAddressBar.DropTargetDrop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
begin
  FStations.ItemIndex := -1;
  FStations.Text := string(FDropTarget.URL);
end;

procedure TClientAddressBar.Setup;
begin
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Left := 4;
  FLabel.Top := 4;
  FLabel.Caption := _('Playlist/Stream-URL:');

  FStart := TSpeedButton.Create(Self);
  FStart.Parent := Self;
  FStart.Width := 22;
  FStart.Height := 22;
  FStart.Top := 4;
  FStart.Left := ClientWidth - 4 - FStart.Width;
  FStart.Anchors := [akRight];

  FStations := TMStationCombo.Create(Self);
  FStations.Parent := Self;
  FStations.DropDownCount := 15;
  FStations.Left := FLabel.Left + FLabel.Width + 8;

  FStations.Top := 2;
  FStations.Width := ClientWidth - FStations.Left - FStart.Width - 8;
  FStations.Anchors := [akLeft, akTop, akRight];
  FStations.OnKeyPress := FStationsKeyPress;
  FStations.OnChange := FStationsChange;
  Height := FStations.Top + FStations.Height + FStations.Top;

  FDropTarget := TDropURLTarget.Create(Self);
  FDropTarget.Register(FStations);
  FDropTarget.OnDrop := DropTargetDrop;

  BevelOuter := bvNone;
end;

procedure TClientAddressBar.FStationsChange(Sender: TObject);
begin
  FStart.Enabled := (Length(Trim(FStations.Text)) > 0) or (FStations.ItemIndex > -1);
end;

procedure TClientAddressBar.FStationsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    FStart.Click;
  end else
  begin
    FStations.ItemIndex := -1;
  end;
end;

{ TClientTab }

procedure TClientTab.ActionStartExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
  Entry: TStreamEntry;
begin
  if not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
  begin
    MsgBox(Handle, _('Available disk space is below the set limit, so recording will not start.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  Clients := FClientView.NodesToClients(FClientView.GetNodes(True));
  for Client in Clients do
  begin
    Entry := FStreams.Get(Client);
    if Entry <> nil then
      Entry.LastTouched := Now;
    Client.Connect;
  end;
end;

procedure TClientTab.ActionStopExecute(Sender: TObject);
var
  Clients: TClientArray;
  Client: TICEClient;
begin
  Clients := FClientView.NodesToClients(FClientView.GetNodes(True));
  for Client in Clients do
  begin
    Client.Disconnect;
  end;
end;

procedure TClientTab.ActionRemoveExecute(Sender: TObject);
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(True));
  for Client in Clients do
  begin
    FClients.RemoveClient(Client.Client);
  end;
end;

procedure TClientTab.ActionResetDataExecute(Sender: TObject);
var
  Res: Integer;
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  R: TStreamEntry;
  i: Integer;
begin
  Res := MsgBox(Handle, _('This will reset the saved song and bytes received counters and information about saved songs.'#13#10 +
                          'The tracknumber of new saved titles will be 1 if you specified the tracknumber in the filename pattern, this number will also be set in ID3 tags.'#13#10 +
                          'Do you want to continue?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
  if Res = IDYES then
  begin
    Clients := FClientView.NodesToData(FClientView.GetNodes(True));
    for Client in Clients do
    begin
      R := FStreams.Get(Client.Client);
      R.SongsSaved := 0;
      R.BytesReceived := 0;
      for i := 0 to R.Tracks.Count - 1 do
        R.Tracks[i].Free;
      R.Tracks.Clear;

      Client.Client.SongsSaved := 0;
      Client.Client.Received := 0;

      FClientView.RefreshClient(Client.Client);
    end;
  end;
  ShowInfo;
end;

procedure TClientTab.ActionShowSideBarExecute(Sender: TObject);
begin
  FSideBar.Visible := not FSideBar.Visible;
  FSplitter.Visible := not FSplitter.Visible;
  FActionShowSideBar.Checked := FSideBar.Visible;
end;

procedure TClientTab.ActionSavePlaylistStream(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etStream);
  SavePlaylist(Entries, False);
end;

procedure TClientTab.ActionSavePlaylistRelay(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etRelay);
  SavePlaylist(Entries, False);
end;

procedure TClientTab.ActionSavePlaylistFile(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etFile);
  SavePlaylist(Entries, False);
end;

procedure TClientTab.ActionTuneInStream(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  Entries := FClientView.GetEntries(etStream);
  SavePlaylist(Entries, True);
end;

procedure TClientTab.ActionTuneInRelay(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Self);
  if FActionTuneInRelay.Enabled then
  begin
    Entries := FClientView.GetEntries(etRelay);
    SavePlaylist(Entries, True);
  end;
end;

procedure TClientTab.ActionTuneInFile(Sender: TObject);
var
  Entries: TPlaylistEntryArray;
begin
  if Assigned(FOnUpdateButtons) then
    FOnUpdateButtons(Self);
  if FActionTuneInFile.Enabled then
  begin
    Entries := FClientView.GetEntries(etFile);
    SavePlaylist(Entries, True);
  end;
end;

constructor TClientTab.Create(AOwner: TComponent);
begin
  inherited;

  ShowCloseButton := False;
  ImageIndex := 16;
end;

procedure TClientTab.DebugClear(Sender: TObject);
var
  Clients: TNodeDataArray;
  i: Integer;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(False));
  for i := 0 to Length(Clients) - 1 do
    if Clients[i].Client = FSideBar.FDebugView.DebugView.Client then
    begin
      Clients[i].Client.DebugLog.Clear;
      Break;
    end;
end;

destructor TClientTab.Destroy;
begin

  inherited;
end;

procedure TClientTab.DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
var
  DropURL: string;
begin
  DropURL := string(FDropTarget.URL);
  StartStreaming('', DropURL);
end;

procedure TClientTab.SavePlaylist(Entries: TPlaylistEntryArray; Open: Boolean);
 procedure BuildPLS(Entries: TPlaylistEntryArray; List: TStringList);
  var
    i: Integer;
  begin
    List.Clear;
    List.Add('[playlist]');
    List.Add('numberofentries=' + IntToStr(Length(Entries)));
    for i := 0 to Length(Entries) - 1 do
    begin
      List.Add('File' + IntToStr(i + 1) + '=' + Entries[i].URL);
      List.Add('Title' + IntToStr(i + 1) + '=' + Entries[i].Name);
      List.Add('Length' + IntToStr(i + 1) + '=-1');
    end;
  end;
  procedure BuildM3U(Entries: TPlaylistEntryArray; List: TStringList);
  var
    i: Integer;
  begin
    List.Clear;
    List.Add('#EXTM3U');
    for i := 0 to Length(Entries) - 1 do
    begin
      List.Add('#EXTINF:-1,' + Entries[i].Name);
      List.Add(Entries[i].URL);
    end;
  end;
var
  Res: Integer;
  List: TStringList;
  Dlg: TSaveDialog;
begin
  if Length(Entries) = 0 then
    Exit;
  List := TStringList.Create;
  try
    if not Open then
    begin
      Dlg := TSaveDialog.Create(Self);
      try
        Dlg.FileName := '';
        Dlg.Filter := '.M3U Playlist|*.m3u|.PLS Playlist|*.pls';
        Dlg.Options := Dlg.Options + [ofOverwritePrompt, ofPathMustExist];
        if Dlg.Execute(Handle) then
        begin
          try
            if (LowerCase(ExtractFileExt(Dlg.FileName)) <> '.m3u') and
               (LowerCase(ExtractFileExt(Dlg.FileName)) <> '.pls') then
              if Dlg.FilterIndex = 1 then
                Dlg.FileName := Dlg.FileName + '.m3u'
              else
                Dlg.FileName := Dlg.FileName + '.pls';

            if LowerCase(ExtractFileExt(Dlg.FileName)) = '.m3u' then
              BuildM3U(Entries, List)
            else
              BuildPLS(Entries, List);

            List.SaveToFile(Dlg.FileName);
          except
            MsgBox(Handle, Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [ExtractFilePath(Dlg.FileName)]), _('Error'), MB_ICONEXCLAMATION);
          end;
        end;
      finally
        Dlg.Free;
      end;
    end else
    begin
      try
        BuildM3U(Entries, List);
        List.SaveToFile(AppGlobals.TempDir + 'playlist.m3u');
        Res := ShellExecute(Handle, 'open', PChar(AppGlobals.TempDir + 'playlist.m3u'), nil, nil, 1);
        if Res <= 32 then
        begin
          BuildPLS(Entries, List);
          List.SaveToFile(AppGlobals.TempDir + 'playlist.pls');
          Res := ShellExecute(Handle, 'open', PChar(AppGlobals.TempDir + 'playlist.pls'), nil, nil, 1);
          if Res <= 32 then
            ShellExecute(Handle, nil, 'rundll32.exe', PChar('shell32.dll,OpenAs_RunDLL ' + AppGlobals.TempDir + 'playlist.pls'), nil, 1);
        end;
      except
        MsgBox(Handle, Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [AppGlobals.TempDir]), _('Error'), MB_ICONEXCLAMATION);
      end;
    end;
  finally
    List.Free;
  end;

end;

procedure TClientTab.Setup(Toolbar: TToolbar; Actions: TActionList;
  Popup: TPopupMenu; MenuImages,
  ClientImages: TImageList; Clients: TClientManager; Streams: TStreamDataList;
  HomeCommunication: THomeCommunication);
  function GetAction(Name: string): TAction;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to Actions.ActionCount - 1 do
      if Actions[i].Name = Name then
      begin
        Result := Actions[i] as TAction;
        Break;
      end;
    if Result = nil then
      raise Exception.Create('');
  end;
begin
  FRefreshInfo := False;
  FReceived := 0;

  FClients := Clients;
  FClients.OnClientDebug := ClientManagerDebug;
  FClients.OnClientRefresh := ClientManagerRefresh;
  FClients.OnClientAddRecent := ClientManagerAddRecent;
  FClients.OnClientAdded := ClientManagerClientAdded;
  FClients.OnClientRemoved := ClientManagerClientRemoved;
  FClients.OnClientSongSaved := ClientManagerSongSaved;
  FClients.OnClientTitleChanged := ClientManagerTitleChanged;
  FClients.OnClientICYReceived := ClientManagerICYReceived;

  FStreams := Streams;
  FStreams.OnStreamChanged := StationsStreamChanged;

  FHomeCommunication := HomeCommunication;

  Caption := _('Streams');

  FAddressBar := TClientAddressBar.Create(Self);
  FAddressBar.Parent := Self;
  FAddressBar.Align := alTop;
  FAddressBar.Visible := True;
  FAddressBar.Setup;

  FToolbar := Toolbar;
  FToolbar.Parent := Self;

  GetAction('actStart').OnExecute := ActionStartExecute;
  GetAction('actStop').OnExecute := ActionStopExecute;
  FActionRemove := GetAction('actRemove');
  FActionRemove.OnExecute := ActionRemoveExecute;
  GetAction('actResetData').OnExecute := ActionResetDataExecute;
  FActionShowSideBar := GetAction('actShowSideBar');
  FActionShowSideBar.OnExecute := ActionShowSideBarExecute;
  GetAction('actTuneInStream').OnExecute := ActionTuneInStream;
  GetAction('actTuneInRelay').OnExecute := ActionTuneInRelay;
  GetAction('actTuneInFile').OnExecute := ActionTuneInFile;
  GetAction('actSavePlaylistStream').OnExecute := ActionSavePlaylistStream;
  GetAction('actSavePlaylistRelay').OnExecute := ActionSavePlaylistRelay;
  GetAction('actSavePlaylistFile').OnExecute := ActionSavePlaylistFile;
  FActionTuneInRelay := GetAction('actTuneInRelay');
  FActionTuneInFile := GetAction('actTuneInFile');
  FActionTuneInStream := GetAction('actTuneInStream');

  FClientView := TMClientView.Create(Self, Popup);
  FClientView.Parent := Self;
  FClientView.Align := alClient;
  FClientView.Visible := True;
  FClientView.PopupMenu := Popup;
  FClientView.Images := ClientImages;
  FClientView.OnChange := FClientViewChange;
  FClientView.OnDblClick := FClientViewDblClick;
  FClientView.OnKeyPress := FClientViewKeyPress;
  FClientView.OnKeyDown := FClientViewKeyDown;
  FClientView.Show;

  FDropTarget := TDropURLTarget.Create(Self);
  FDropTarget.Register(FClientView);
  FDropTarget.OnDrop := DropTargetDrop;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := alRight;
  FSplitter.Visible := True;
  FSplitter.Width := 4;
  FSplitter.ResizeStyle := rsUpdate;

  FSideBar := TSidebar.Create(Self);
  FSideBar.Parent := Self;
  FSideBar.Align := alRight;
  FSideBar.Visible := True;
  FSideBar.Init(HomeCommunication);

  FSideBar.FDebugView.DebugView.OnClear := DebugClear;
  FSideBar.FBrowserView.StreamTree.OnAction := StreamBrowserAction;
  FSideBar.FInfoView.InfoView.Tree.OnAction := StreamInfoAction;

  FSplitter.Left := FSideBar.Left - 5;

  FSideBar.Visible := AppGlobals.ShowSidebar;
  FSplitter.Visible := AppGlobals.ShowSidebar;
  FSideBar.Width := AppGlobals.SidebarWidth;
end;

procedure TClientTab.ShowInfo;
var
  Clients: TNodeDataArray;
  Client: PClientNodeData;
  Entry: TStreamEntry;
  Entries: TStreamList;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(True));

  Entries := TStreamList.Create;
  try
    for Client in Clients do
    begin
      Entry := FStreams.Get(Client.Client);
      if Entry <> nil then
      begin
        Entries.Add(Entry)
      end;
    end;

    if Entries.Count > 0 then
      FSideBar.InfoView.ShowInfo(Entries)
    else
      FSideBar.InfoView.ShowInfo(nil);
  finally
    Entries.Free;
  end;
end;

procedure TClientTab.Shown;
var
  i: Integer;
begin
  FSideBar.FBrowserView.Setup;

  if FClientView.RootNodeCount > 0 then
  begin
    FClientView.Selected[FClientView.GetFirst] := True;
    FClientView.FocusedNode := FClientView.GetFirst;
  end;

  for i := 0 to FClientView.Header.Columns.Count - 1 do
    FClientView.Header.Columns[i].Width := AppGlobals.HeaderWidth[i];
end;

procedure TClientTab.ClientManagerAddRecent(Sender: TObject);
var
  Client: TICEClient;
  Entry: TStreamEntry;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Add(Client.StreamName, Client.StartURL, Client.URLs,
    Client.BitRate, Client.Genre, Client.SkipShort, 0);
  Entry.Name := Client.StreamName;
  Entry.RecentIndex := 0;
  Entry.LastTouched := Now;
  if not Entry.Submitted then
  begin
    FHomeCommunication.SubmitStream(Entry.StartURL);
    Entry.Submitted := True;
  end;

  ShowInfo;
end;

procedure TClientTab.ClientManagerDebug(Sender: TObject);
begin
  if FSideBar.FDebugView.DebugView.Client = Sender then
  begin
    FSideBar.FDebugView.ShowDebug(TICEClient(Sender));
  end;
end;

procedure TClientTab.ClientManagerICYReceived(Sender: TObject;
  Received: Integer);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  FReceived := FReceived + Received;
  FStreams.Received := FStreams.Received + Received;

  Entry := FStreams.Get(Client);
  if Entry <> nil then
  begin
    Entry.BytesReceived := Entry.BytesReceived + Received;
  end;

  FRefreshInfo := True;
end;

procedure TClientTab.ClientManagerRefresh(Sender: TObject);
begin
  FClientView.RefreshClient(Sender as TICEClient);
end;

procedure TClientTab.ClientManagerClientAdded(Sender: TObject);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Add(Client.StreamName, Client.StartURL, Client.URLs,
    Client.BitRate, Client.Genre, Client.SkipShort, 0);
  Entry.LastTouched := Now;
  Entry.IsInList := True;
  Client.Received := Entry.BytesReceived;

  FClientView.AddClient(Client);

  FClientView.SortItems;
end;

procedure TClientTab.ClientManagerClientRemoved(Sender: TObject);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Get(Client);
  if Entry <> nil then
    Entry.IsInList := False;

  FClientView.RemoveClient(Client);

  if FSidebar.FDebugView.DebugView.Client = Client then
    FSidebar.FDebugView.ShowDebug(nil);

  ShowInfo;
end;

procedure TClientTab.ClientManagerSongSaved(Sender: TObject;
  Filename, Title: string);
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Client := Sender as TICEClient;

  Entry := FStreams.Get(Client);
  if Entry <> nil then
  begin
    Entry.Tracks.Add(TTrackInfo.Create(Now, Filename));
    Entry.SongsSaved := Entry.SongsSaved + 1;
  end;

  ShowInfo;
end;

procedure TClientTab.ClientManagerTitleChanged(Sender: TObject;
  Title: string);
begin

end;

procedure TClientTab.FClientViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    FActionRemove.Execute;
  end;
end;

procedure TClientTab.FClientViewKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) or (Key = #32) then
  begin
    FClientViewDblClick(FClientView);
    Key := #0;
  end;
end;

procedure TClientTab.FClientViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Clients: TNodeDataArray;
begin
  if Assigned(OnUpdateButtons) then
    OnUpdateButtons(Self);
  ShowInfo;

  if FClientView.SelectedCount = 1 then
  begin
    Clients := FClientView.NodesToData(FClientView.GetNodes(True));
    if Length(Clients) = 1 then
      FSideBar.FDebugView.ShowDebug(Clients[0].Client);
  end else
    FSideBar.FDebugView.ShowDebug(nil);
end;

procedure TClientTab.FClientViewDblClick(Sender: TObject);
var
  Clients: TNodeDataArray;
begin
  Clients := FClientView.NodesToData(FClientView.GetNodes(True));
  if Length(Clients) = 1 then
  begin
    case AppGlobals.DefaultAction of
      caStartStop:
        if Clients[0].Client.Active then
          Clients[0].Client.Disconnect
        else
          Clients[0].Client.Connect;
      caStream:                   // TODO: zweig testen, ob er abspielt dann.
        FActionTuneInStream.Execute;
      caRelay:
        FActionTuneInRelay.Execute;
      caFile:
        FActionTuneInFile.Execute;
    end;
  end;
end;

function TClientTab.StartStreaming(Name, URL: string): Boolean;
var
  Entry: TStreamEntry;
  Client: TICEClient;
begin
  Result := True;

  if not DiskSpaceOkay(AppGlobals.Dir, AppGlobals.MinDiskSpace) then
  begin
    Result := False;
    MsgBox(Handle, _('Available disk space is below the set limit, so recording will not start.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  URL := Trim(URL);
  if URL <> '' then
  begin
    Entry := FStreams.Get(Name, URL, nil);
    if Entry <> nil then
      Entry.LastTouched := Now;

    // Ist der Client schon in der Liste?
    Client := FClients.GetClient(Name, URL, nil);
    if Client <> nil then
    begin
      Client.Connect;
      Exit;
    end else
    begin
      // Ist der Client schon bekannt?
      if Entry <> nil then
      begin
        Client := FClients.AddClient(Entry.Name, Entry.StartURL, Entry.URLs, Entry.SkipShort, Entry.SongsSaved);
        Client.Connect;
      end else
      begin
        if ValidURL(URL) then
        begin
          Client := FClients.AddClient(Name, URL);
          Client.Connect;
        end else
        begin
          Result := False;
          MsgBox(Handle, _('The stream could not be added to the list because the URL is invalid.'), _('Info'), MB_ICONINFORMATION);
        end;
      end;
    end;
  end;
end;

procedure TClientTab.StationsStreamChanged(Sender: TObject;
  Stream: TStreamEntry);
var
  Item: TComboExItem;
  Client: TICEClient;
begin
  if Stream.IsInList then
  begin
    Client := FClients.GetClient(Stream.Name, Stream.StartURL, Stream.URLs);
    if Client = nil then
      FClients.AddClient(Stream.Name, Stream.StartURL, Stream.URLs, Stream.SkipShort, Stream.SongsSaved);
  end;

  Item := FAddressBar.FStations.Get(Stream.Name, Stream.StartURL, Stream.URLs);
  if (Item = nil) and (Stream.RecentIndex > -1) then
  begin
    Item := FAddressBar.FStations.ItemsEx.Add;
    Item.ImageIndex := 0;
    Item.Caption := Stream.Name;
    Item.Data := Stream;
    if FAddressBar.FStations.ItemIndex > -1 then
      FAddressBar.FStations.ItemIndex := FAddressBar.FStations.ItemIndex + 1;
  end else
  begin
    if (Item <> nil) and (Stream.RecentIndex = -1) then
      FAddressBar.FStations.ItemsEx.Delete(Item.Index);
  end;
  FAddressBar.FStations.Sort;
end;

procedure TClientTab.StreamBrowserAction(Sender: TObject; Action: TOpenActions;
  Streams: TStreamDataArray);
var
  i: Integer;
  s: string;
  Entries: TPlaylistEntryArray;
begin
  if Action in [oaListen, oaSave] then
  begin
    SetLength(Entries, 0);
    for i := 0 to Length(Streams) - 1 do
    begin
      SetLength(Entries, Length(Entries) + 1);
      Entries[i].Name := Streams[i].Name;
      Entries[i].URL := Streams[i].URL;
    end;
  end;

  case Action of
    oaStart:
      for i := 0 to Length(Streams) - 1 do
        if not StartStreaming(Streams[i].Name, Streams[i].URL) then
          Break;
    oaListen:
      SavePlaylist(Entries, True);
    oaCopy:
      begin
        s := '';
        Clipboard.Clear;
        for i := 0 to Length(Streams) - 1 do
          s := s + Streams[i].URL + #13#10;
        s := Trim(s);
        Clipboard.SetTextBuf(PChar(s));
      end;
    oaSave:
      SavePlaylist(Entries, False);
  end;
end;

procedure TClientTab.StreamInfoAction(Sender: TObject; Action: TTrackActions;
  Tracks: TTrackInfoArray);
var
  Entries: TPlaylistEntryArray;
  i: Integer;
begin
  case Action of
    taPlay:
      begin
        // Tracks in Playlist konvertieren
        SetLength(Entries, Length(Tracks));
        for i := 0 to Length(Tracks) - 1 do
        begin
          Entries[i].Name := Tracks[i].Filename;
          Entries[i].URL := Tracks[i].Filename;
        end;

        SavePlaylist(Entries, True);
      end;
    taCut:
      begin
        if Assigned(FOnCut) then
          for i := 0 to Length(Tracks) - 1 do
            FOnCut(Tracks[i].Filename);
      end;
    taRemove:
      begin
        for i := 0 to Length(Tracks) - 1 do
          FStreams.RemoveTrack(Tracks[i]);
      end;
    taDelete:
      begin
        for i := 0 to Length(Tracks) - 1 do
        begin
          DeleteFile(Tracks[i].Filename);
          FStreams.RemoveTrack(Tracks[i]);
        end;
      end;
    taProperties:
      PropertiesDialog(Tracks[0].Filename); // todo: testen obs lackt.
  end;
end;

procedure TClientTab.TimerTick;
begin
  if FRefreshInfo then
  begin
    ShowInfo;
    FRefreshInfo := False;
  end;
end;

{ TSidebar }

constructor TSidebar.Create(AOwner: TComponent);
begin
  inherited;


end;

destructor TSidebar.Destroy;
begin

  inherited;
end;

procedure TSidebar.Init(HomeCommunication: THomeCommunication);
begin
  FPage1 := TTabSheet.Create(Self);
  FPage1.PageControl := Self;
  FPage1.Caption := _('Browser');

  FPage2 := TTabSheet.Create(Self);
  FPage2.PageControl := Self;
  FPage2.Caption := _('Info');

  FPage3 := TTabSheet.Create(Self);
  FPage3.PageControl := Self;
  FPage3.Caption := _('Log');

  FBrowserView := TMStreamBrowserView.Create(Self, HomeCommunication);
  FInfoView := TMStreamInfoView.Create(Self);
  FDebugView := TMStreamDebugView.Create(Self);

  FBrowserView.Parent := FPage1;
  FInfoView.Parent := FPage2;
  FDebugView.Parent := FPage3;
end;

end.
