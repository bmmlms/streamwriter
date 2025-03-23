{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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

{ This unit contains everything needed to display the stream-infos }
unit StreamInfoView;

interface

uses
  AudioFunctions,
  Classes,
  Controls,
  DataManager,
  DragDrop,
  DragDropFile,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  ImgList,
  LanguageObjects,
  Logging,
  Menus,
  MStringFunctions,
  StdCtrls,
  SysUtils,
  VirtualTrees;

type
  TMStreamInfoViewPanel = class(TPanel)
  private
    FEntries: TStreamList;
    FName: TLabel;
    FInfo: TMemo;

    function MakeDuration(Seconds: UInt64): string;

    procedure ShowInfo(Entries: TStreamList; ChangedOverride: Boolean = False);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  end;

  { TMStreamInfoView }

  TMStreamInfoView = class(TPanel)
  private
    FInfoView: TMStreamInfoViewPanel;
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure ShowInfo(ChangedOverride: Boolean = False); overload;
    procedure ShowInfo(Entries: TStreamList); overload;

    property InfoView: TMStreamInfoViewPanel read FInfoView;
  end;

implementation

{ TMStreamInfoViewPanel }

constructor TMStreamInfoViewPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEntries := TStreamList.Create;

  BevelOuter := bvNone;

  FName := TLabel.Create(Self);
  FName.Align := alTop;
  FName.Font.Size := 10;
  FName.Font.Style := [fsBold];
  FName.Parent := Self;

  FInfo := TMemo.Create(Self);
  FInfo.Align := alClient;
  FInfo.BorderStyle := bsNone;
  FInfo.Color := clWindow;
  FInfo.ScrollBars := ssVertical;
  FInfo.ReadOnly := True;
  FInfo.Parent := Self;

  Align := alClient;
end;

destructor TMStreamInfoViewPanel.Destroy;
begin
  FEntries.Free;
  inherited;
end;

function TMStreamInfoViewPanel.MakeDuration(Seconds: UInt64): string;
var
  H, M, S: Integer;
begin
  H := Seconds div 60 div 60;
  M := (Seconds - (H * 60 * 60)) div 60;
  S := (Seconds - (H * 60 * 60) - (M * 60));

  Result := Format('%dh %dm %ds', [H, M, S]);
end;

procedure TMStreamInfoViewPanel.ShowInfo(Entries: TStreamList; ChangedOverride: Boolean = False);
var
  i: Integer;
  SongsSaved: Cardinal;
  Received, SecondsReceived: UInt64;
  Title, Info, Genres, Bitrates, NextTitle: string;
  Entry: TStreamEntry;
  EntriesNew: TStreamList;
begin
  if Entries = nil then
    FEntries.Clear
  else
  begin
    EntriesNew := TStreamList.Create;

    Title := '';
    Genres := '';
    Bitrates := '';
    SongsSaved := 0;
    Received := 0;
    SecondsReceived := 0;
    for Entry in Entries do
    begin
      EntriesNew.Add(Entry);

      NextTitle := Entry.DisplayName;
      if Title <> '' then
        Title := Title + ' / ';
      Title := Title + NextTitle;

      if Entry.Genre <> '' then
      begin
        if Genres <> '' then
          Genres := Genres + ' / ';
        Genres := Genres + Entry.Genre;
      end;
      if Entry.Bitrate > 0 then
      begin
        if Bitrates <> '' then
          Bitrates := Bitrates + ' / ';
        Bitrates := Bitrates + IntToStr(Entry.Bitrate) + 'kbps';
        if Entry.VBR then
          Bitrates := Bitrates + ' VBR';
      end;
      SongsSaved := SongsSaved + Entry.SongsSaved;
      Received := Received + Entry.BytesReceived;
      SecondsReceived := SecondsReceived + Entry.SecondsReceived;
    end;

    // StringReplace, damit aus einem '&' kein Shortcut auf dem Label wird..
    Title := StringReplace(TMStringFunctions.TruncateText(Title, FName.Parent.Width, FName.Canvas.Font), '&', '&&', [rfReplaceAll]);
    if Title <> FName.Caption then
      FName.Caption := Title;

    Info := '';
    if Entries.Count = 1 then
    begin
      if (not Entries[0].Name.IsEmpty) and (Entries[0].Name <> Entries[0].DisplayName) then
        Info := Info + Entries[0].Name + #13#10;
      if Entries[0].StreamURL <> '' then
        Info := Info + Entries[0].StreamURL + #13#10;
      if Genres <> '' then
        Info := Info + Genres + #13#10;
      if Entries[0].AudioType <> atNone then
        Info := Info + FormatToDesc(Entries[0].AudioType) + #13#10;
      if Bitrates <> '' then
        Info := Info + Bitrates + #13#10;
    end;
    Info := Info + Format(_('%d songs saved'), [SongsSaved]) + #13#10;
    Info := Info + Format(_('%s received'), [TFunctions.MakeSize(Received)]);
    if Entries.Count = 1 then
      Info := Info + #13#10 + Format(_('%s connected'), [MakeDuration(SecondsReceived)]);
    if Entries.Count = 1 then
    begin
      Info := Info + #13#10#13#10;
      Info := Info + Format('URL: %s', [Entries[0].StartURL]) + #13#10;
      if Entries[0].URLs.Count > 0 then
      begin
        Info := Info + 'URLs:' + #13#10;
        for i := 0 to Entries[0].URLs.Count - 1 do
          Info := Info + Entries[0].URLs[i] + #13#10;
      end;
    end;
    if Info <> FInfo.Text then
      FInfo.Text := Info;

    FEntries.Free;
    FEntries := EntriesNew;
  end;
end;

{ TMStreamInfoView }

constructor TMStreamInfoView.Create(AOwner: TComponent);
begin
  inherited;

  Caption := 'Please select at least one stream.';
  BevelOuter := bvNone;
  WordWrap := True;
  Align := alClient;

  FInfoView := TMStreamInfoViewPanel.Create(Self);
  FInfoView.Parent := Self;
  FInfoView.Visible := False;
end;

destructor TMStreamInfoView.Destroy;
begin
  inherited Destroy;

  FInfoView := nil;
end;

procedure TMStreamInfoView.ShowInfo(ChangedOverride: Boolean = False);
begin
  if not Assigned(FInfoView) then
    Exit;

  FInfoView.ShowInfo(FInfoView.FEntries, ChangedOverride);
end;

procedure TMStreamInfoView.ShowInfo(Entries: TStreamList);
begin
  if not Assigned(FInfoView) then
    Exit;

  FInfoView.ShowInfo(Entries);
  FInfoView.Visible := Entries <> nil;
end;

end.
