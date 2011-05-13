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
unit StationCombo;

interface

uses
  Windows, SysUtils, Classes, ListActns, ComCtrls, DataManager, Logging;

type
  TMStationCombo = class(TComboBoxEx)
  private
    FList: TRecentList;
    function ItemsCompare(List: TListControlItems; Item1, Item2: TListControlItem): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort;
    procedure ReOrganize(FirstItem: TListControlItem);
    procedure BuildList(List: TRecentList);
    function AddItem(Name, URL: string): TListControlItem; reintroduce; overload;
    function AddItem(Entry: TRecentEntry): TListControlItem; reintroduce; overload;
  end;

implementation

{ TMStationCombo }

procedure TMStationCombo.ReOrganize(FirstItem: TListControlItem);
var
  i: Integer;
  Item: TListControlItem;
  LastSelected: TListControlItem;
begin
  LastSelected := nil;
  if ItemIndex > -1 then
    LastSelected := ItemsEx[ItemIndex];

  for i := 0 to ItemsEx.Count - 1 do
    TRecentEntry(ItemsEx[i].Data).Index := TRecentEntry(ItemsEx[i].Data).Index + 1;

  TRecentEntry(FirstItem.Data).Index := 0;

  Sort;

  while ItemsEx.Count > DropDownCount do
  begin
    Item := ItemsEx[ItemsEx.Count - 1];
    TRecentEntry(Item.Data).Free;
    ItemsEx.Delete(ItemsEx.Count - 1);

    if LastSelected = Item then
      LastSelected := nil;
  end;

  for i := 0 to ItemsEx.Count - 1 do
    TRecentEntry(ItemsEx[i].Data).Index := i;

  if ItemIndex <> -1 then
    if LastSelected <> nil then
    begin
      for i := 0 to ItemsEx.Count - 1 do
        if ItemsEx[i] = LastSelected then
        begin
          ItemIndex := ItemsEx[i].Index;
          Break;
        end;
    end else
      ItemIndex := 0;
end;

function TMStationCombo.AddItem(Name, URL: string): TListControlItem;
var
  i: Integer;
  Entry: TRecentEntry;
begin
  for i := 0 to ItemsEx.Count - 1 do
  begin
    if LowerCase(TRecentEntry(ItemsEx[i].Data).Name) = LowerCase(Name) then
    begin
      TRecentEntry(ItemsEx[i].Data).StartURL := URL;
      ReOrganize(ItemsEx[i]);
      Result := ItemsEx[i];
      Exit;
    end;
    if LowerCase(TRecentEntry(ItemsEx[i].Data).StartURL) = LowerCase(URL) then
    begin
      TRecentEntry(ItemsEx[i].Data).Name := Name;
      ReOrganize(ItemsEx[i]);
      Result := ItemsEx[i];
      Exit;
    end;
  end;

  Entry := TRecentEntry.Create(Name, URL, 0);
  Result := ItemsEx.Add;
  Result.Caption := Name;
  Result.Data := Entry;
  Result.ImageIndex := 16;
  ReOrganize(Result);
end;

function TMStationCombo.AddItem(Entry: TRecentEntry): TListControlItem;
begin
  Result := ItemsEx.Add;

  Result.Caption := Entry.Name;
  Result.Data := Entry.Copy;
  Result.ImageIndex := 16;
end;

procedure TMStationCombo.BuildList(List: TRecentList);
var
  i: Integer;
begin
  FList := List;

  for i := 0 to List.Count - 1 do
  begin
    AddItem(List[i]);
  end;

  Sort;
end;

constructor TMStationCombo.Create(AOwner: TComponent);
begin
  inherited;
  ItemsEx.OnCompare := ItemsCompare;
end;

destructor TMStationCombo.Destroy;
var
  i: Integer;
begin
  for i := 0 to ItemsEx.Count - 1 do
    TRecentEntry(ItemsEx[i].Data).Free;
  inherited;
end;

procedure TMStationCombo.Sort;
var
  s: string;
begin
  ItemsEx.BeginUpdate;
  try
    if ItemIndex > -1 then
      s := TRecentEntry(ItemsEx.Items[ItemIndex].Data).Name
    else
      s := Text;

    ItemsEx.SortType := ListActns.stData;
    ItemsEx.Sort;
  finally
    ItemsEx.EndUpdate;
  end;
end;

function TMStationCombo.ItemsCompare(List: TListControlItems; Item1,
  Item2: TListControlItem): Integer;
var
  E1, E2: TRecentEntry;
begin
  E1 := TRecentEntry(Item1.Data);
  E2 := TRecentEntry(Item2.Data);
  if E1.Index > E2.Index then
    Result := 1
  else if E1.Index < E2.Index then
    Result := -1
  else
    Result := 0;
end;

end.
