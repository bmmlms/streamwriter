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
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
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

  Header.AutoSizeIndex := 1;
  Header.Options := [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
end;

destructor TSavedTracksTree.Destroy;
begin

  inherited;
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

{ TStreamInfoView }

constructor TMStreamInfoView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FResized := False;

  BorderStyle := bsNone;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 100;
  FTopPanel.BorderStyle := bsNone;

  FName := TLabel.Create(Self);
  FName.Parent := FTopPanel;
  FName.Align := alTop;
  FName.Font.Name := 'Tahoma';
  FName.Font.Size := 10;
  FName.Font.Style := [fsBold];
  FName.Show;

  FInfo := TMemo.Create(Self);
  FInfo.Parent := FTopPanel;
  FInfo.Align := alClient;
  FInfo.BorderStyle := bsNone;
  FInfo.Color := clBtnFace;
  FInfo.ScrollBars := ssVertical;
  FInfo.Show;

  FSavedTracks := TSavedTracksTree.Create(Self);
  FSavedTracks.Parent := Self;
  FSavedTracks.Align := alClient;;
  //FSavedTracks.Anchors := [akLeft, akTop, akRight, akBottom];
  FSavedTracks.Show;

  Align := alClient;
end;

destructor TMStreamInfoView.Destroy;
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
    //  TODO:
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
    begin // TODO: Wenn die Liste voll ist, gescrollt war, und dann so ge-refresh-ed wird - ist das fail für den User?
      Node := FSavedTracks.AddChild(nil);
      NodeData := FSavedTracks.GetNodeData(Node);
      NodeData.TrackInfo := Tracks[i];
    end;
  finally
    FSavedTracks.EndUpdate;
  end;
end;

end.
