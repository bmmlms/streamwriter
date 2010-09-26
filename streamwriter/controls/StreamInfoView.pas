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
  Generics.Collections, Graphics, Forms, Menus, Messages, DragDrop,
  DragDropFile, Functions;

type
  TMStreamInfoViewPanel = class(TPanel)
  private
    FEntries: TStreamList;
    FResized: Boolean;
    FTopPanel: TPanel;
    FSplitter: TSplitter;
    FName: TLabel;
    FInfo: TMemo;

    procedure ShowInfo(Entries: TStreamList; ChangedOverride: Boolean = False);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  end;

  TMStreamInfoView = class(TPanel)
  private
    FInfoView: TMStreamInfoViewPanel;
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure Translate;
    procedure ShowInfo(ChangedOverride: Boolean = False); overload;
    procedure ShowInfo(Entries: TStreamList); overload;

    property InfoView: TMStreamInfoViewPanel read FInfoView;
  end;

implementation

{ TStreamInfoView }

constructor TMStreamInfoViewPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEntries := TStreamList.Create;
  FResized := False;

  BevelOuter := bvNone;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alClient;
  FTopPanel.Height := 110;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Visible := True;

  FName := TLabel.Create(Self);
  FName.Parent := FTopPanel;
  FName.Align := alTop;
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

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := alTop;
  FSplitter.ResizeStyle := rsUpdate;
  FSplitter.Visible := True;
  FSplitter.Top := FTopPanel.Top + FTopPanel.Height;

  Align := alClient;
end;

destructor TMStreamInfoViewPanel.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TMStreamInfoViewPanel.Resize;
begin
  inherited;
  ShowInfo(FEntries);
end;

procedure TMStreamInfoViewPanel.ShowInfo(Entries: TStreamList; ChangedOverride: Boolean = False);
var
  i, n: Integer;
  SongsSaved: Cardinal;
  Received: UInt64;
  EntriesChanged: Boolean;
  Title, Info, Genres, BitRates: string;
  Entry: TStreamEntry;
  EntriesNew: TStreamList;
begin
  if Entries = nil then
  begin
    FEntries.Clear;

  end else
  begin
    EntriesChanged := False;
    for Entry in Entries do
      if not FEntries.Contains(Entry) then
      begin
        EntriesChanged := True;
        Break;
      end;
    for Entry in FEntries do
      if not Entries.Contains(Entry) then
      begin
        EntriesChanged := True;
        Break;
      end;

    EntriesNew := TStreamList.Create;

    Genres := '';
    BitRates := '';
    SongsSaved := 0;
    Received := 0;
    for Entry in Entries do
    begin
      EntriesNew.Add(Entry);

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
      Received := Received + Entry.BytesReceived;
    end;

    Title := TruncateText(Title, FName.Parent.Width, FName.Canvas.Font);
    if Title <> FName.Caption then
      FName.Caption := Title;

    Info := '';
    if Genres <> '' then
      Info := Info + Genres + #13#10;
    if BitRates <> '' then
      Info := Info + Bitrates + 'kbps' + #13#10;
    Info := Info + Format('%d songs saved', [SongsSaved]) + #13#10;
    Info := Info + Format('%s received', [MakeSize(Received)]);
    if Info <> FInfo.Text then
      FInfo.Text := Info;

    FEntries.Free;
    FEntries := EntriesNew;
  end;
end;

{ TMStreamInfoContainer }

constructor TMStreamInfoView.Create(AOwner: TComponent);
begin
  inherited;

  Caption := _('Please select at least one stream.');
  BevelOuter := bvNone;
  Align := alClient;

  FInfoView := TMStreamInfoViewPanel.Create(Self);
  FInfoView.Parent := Self;
  FInfoView.Visible := False;
end;

procedure TMStreamInfoView.ShowInfo(ChangedOverride: Boolean = False);
begin
  FInfoView.ShowInfo(FInfoView.FEntries, ChangedOverride);
end;

procedure TMStreamInfoView.ShowInfo(Entries: TStreamList);
begin
  FInfoView.ShowInfo(Entries);
  FInfoView.Visible := Entries <> nil;
end;

procedure TMStreamInfoView.Translate;
begin

end;

end.

