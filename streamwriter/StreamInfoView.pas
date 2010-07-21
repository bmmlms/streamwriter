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
unit StreamInfoView;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ImgList,
  RecentManager, VirtualTrees, LanguageObjects, GUIFunctions,
  Generics.Collections, Graphics, Forms;

type
  TSavedHistoryNodeData = record
    TrackInfo: TTrackInfo;
  end;
  PSavedHistoryNodeData = ^TSavedHistoryNodeData;

  TSavedTracksTree = class(TVirtualStringTree)
  private
    FSortColumn: Integer;
    FSortDirection: TSortDirection;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  end;

  TMStreamInfoView = class(TPanel)
  private
    FResized: Boolean;
    FTopPanel: TPanel;
    FName: TLabel;
    FInfo: TMemo;
    FSavedTracks: TSavedTracksTree;

    procedure ShowTracks(Tracks: TList<TTrackInfo>);
  protected
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure ShowInfo(Entries: TStreamList);

    property Tree: TSavedTracksTree read FSavedTracks;
  end;

implementation

{ TStreamInfoTree }

constructor TSavedTracksTree.Create(AOwner: TComponent);
var
  C1, C2: TVirtualTreeColumn;
begin
  inherited Create(AOwner);

  NodeDataSize := SizeOf(TSavedHistoryNodeData);

  C1 := Header.Columns.Add;
  C1.Text := _('Time');
  C2 := Header.Columns.Add;
  C2.Text := _('Filename');
  C1.Width := 80;
  Indent := 2;

  ShowHint := True;
  HintMode := hmTooltip;

  FSortColumn := 0;
  FSortDirection := sdDescending;

  Header.AutoSizeIndex := 1;
  Header.Options := [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
end;

destructor TSavedTracksTree.Destroy;
begin

  inherited;
end;

function TSavedTracksTree.DoCompare(Node1, Node2: PVirtualNode;
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
  Data1, Data2: PSavedHistoryNodeData;
begin
  Result := 0;
  Data1 := GetNodeData(Node1);
  Data2 := GetNodeData(Node2);

  case Column of
    0: Result := CmpTime(Data1.TrackInfo.Time, Data2.TrackInfo.Time);
    1: Result := CompareText(Data1.TrackInfo.Filename, Data2.TrackInfo.Filename);
  end;
end;

function TSavedTracksTree.DoGetImageIndex(Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;
  if Column = 0 then
    Index := 0;
end;

procedure TSavedTracksTree.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
var
  NodeData: PSavedHistoryNodeData;
begin
  inherited;
  if TextType = ttNormal then
  begin
    NodeData := GetNodeData(Node);
    case Column of
      0:
        begin
          if Trunc(NodeData.TrackInfo.Time) = Trunc(Now) then
            Text := TimeToStr(NodeData.TrackInfo.Time)
          else
            Text := DateTimeToStr(NodeData.TrackInfo.Time);
        end;
      1: Text := ExtractFileName(NodeData.TrackInfo.Filename);
    end;
  end;
end;

procedure TSavedTracksTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
begin
  inherited;
  if HitInfo.Button = mbLeft then
  begin
    if FSortColumn <> HitInfo.Column then
    begin
      FSortColumn := HitInfo.Column;
      if (HitInfo.Column <> 0) and (HitInfo.Column <> 1) then
        FSortDirection := sdDescending
      else
        FSortDirection := sdAscending;
    end else
    begin
      if FSortDirection = sdAscending then
        FSortDirection := sdDescending
      else
        FSortDirection := sdAscending;
    end;
    Sort(nil, HitInfo.Column, FSortDirection);
  end;
end;

{ TStreamInfoView }

constructor TMStreamInfoView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FResized := False;

  BevelOuter := bvNone;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 80;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Visible := False;

  FName := TLabel.Create(Self);
  FName.Parent := FTopPanel;
  FName.Align := alTop;
  FName.Font.Name := 'Tahoma';
  FName.Font.Size := 10;
  FName.Font.Style := [fsBold];
  FName.Visible := True;

  FInfo := TMemo.Create(Self);
  FInfo.Parent := FTopPanel;
  FInfo.Align := alClient;
  FInfo.BorderStyle := bsNone;
  FInfo.Color := clWindow;
  FInfo.ScrollBars := ssVertical;
  FInfo.ReadOnly := True;
  FInfo.Visible := True;

  FSavedTracks := TSavedTracksTree.Create(Self);
  FSavedTracks.Parent := Self;
  FSavedTracks.Align := alClient;;
  FSavedTracks.Visible := False;

  Align := alClient;
end;

destructor TMStreamInfoView.Destroy;
begin
  inherited;
end;

procedure TMStreamInfoView.Resize;
begin
  inherited;

end;

procedure TMStreamInfoView.ShowInfo(Entries: TStreamList);
var
  i: Integer;
  Title, Info, Genres, BitRates: string;
  SongsSaved: Cardinal;
  Entry: TStreamEntry;
  Node: PVirtualNode;
  NodeData: PSavedHistoryNodeData;
  TrackList: TList<TTrackInfo>;
begin
  if Entries <> nil then
  begin
    FTopPanel.Visible := True;
    FSavedTracks.Visible := True;
    Caption := '';

    TrackList := TList<TTrackInfo>.Create;
    try
      Genres := '';
      BitRates := '';
      SongsSaved := 0;
      for Entry in Entries do
      begin
        Title := Title + Entry.Name;
        if Entry.Genre <> '' then
        begin
          if Genres <> '' then
            Genres := Genres + ' / ';
          Genres := Genres + Entry.Genre;
        end;
        if Entry.BitRate > 0 then
        begin
          if BitRates <> '' then
            BitRates := BitRates + ' / ';
          BitRates := BitRates + IntToStr(Entry.BitRate);
        end;
        SongsSaved := SongsSaved + Entry.SongsSaved;

        FSavedTracks.Clear;
        for i := 0 to Entry.Tracks.Count - 1 do
          TrackList.Add(Entry.Tracks[i]);
      end;

      Title := TruncateText(Title, FName.Width, FName.Canvas);
      if Title <> FName.Caption then
        FName.Caption := Title;

      Info := '';
      if Genres <> '' then
        Info := Info + Genres + #13#10;
      if BitRates <> '' then
        Info := Info + Bitrates + 'kbps' + #13#10;
      Info := Info + IntToStr(SongsSaved) + _(' songs saved');
      if Info <> FInfo.Text then
        FInfo.Text := Info;

      ShowTracks(TrackList);
    finally
      TrackList.Free;
    end;
  end else
  begin
    FTopPanel.Visible := False;
    FSavedTracks.Visible := False;
    Caption := _('Please select at least one stream.');
  end;
end;

procedure TMStreamInfoView.ShowTracks(Tracks: TList<TTrackInfo>);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PSavedHistoryNodeData;
begin
  FSavedTracks.BeginUpdate;
  try
    FSavedTracks.Clear;
    for i := Tracks.Count - 1 downto 0 do
    begin
      Node := FSavedTracks.AddChild(nil);
      NodeData := FSavedTracks.GetNodeData(Node);
      NodeData.TrackInfo := Tracks[i];
    end;
  finally
    FSavedTracks.EndUpdate;
  end;
  FSavedTracks.Sort(nil, FSavedTracks.FSortColumn, FSavedTracks.FSortDirection);
end;

end.
