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
unit StationCombo;

interface

uses
  Windows, SysUtils, Classes, ListActns, ComCtrls, RecentManager;

type
  TMStationCombo = class(TComboBoxEx)
  private
    function ItemsCompare(List: TListControlItems; Item1, Item2: TListControlItem): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Sort;
    function Get(Name, URL: string; URLs: TStringList): TComboExItem;
  end;

implementation

{ TMStationCombo }

constructor TMStationCombo.Create(AOwner: TComponent);
begin
  inherited;
  ItemsEx.OnCompare := ItemsCompare;
end;

procedure TMStationCombo.Sort;
var
  s: string;
  Item: TComboExItem;
begin
  ItemsEx.BeginUpdate;
  try
    if ItemIndex > -1 then
      s := TStreamEntry(ItemsEx.Items[ItemIndex].Data).Name
    else
      s := Text;

    ItemsEx.SortType := ListActns.stData;
    ItemsEx.Sort;

    Item := Get(s, s, nil);
    if Item <> nil then
    begin
      ItemIndex := Item.Index;
    end else
    begin
      ItemIndex := -1;
    end;
  finally
    ItemsEx.EndUpdate;
  end;
end;

function TMStationCombo.Get(Name, URL: string;
  URLs: TStringList): TComboExItem;
var
  i, n, j: Integer;
  Entry: TStreamEntry;
begin
  Name := Trim(Name);
  URL := Trim(URL);

  Result := nil;
  for i := 0 to ItemsEx.Count - 1 do
  begin
    Entry := TStreamEntry(ItemsEx[i].Data);
    if Name <> '' then
      if LowerCase(Entry.Name) = LowerCase(Name) then
      begin
        Result := TComboExItem(ItemsEx[i]);
        Exit;
      end;

    if URL <> '' then
      if LowerCase(Entry.StartURL) = LowerCase(URL) then
      begin
        Result := TComboExItem(ItemsEx[i]);
        Exit;
      end;

    if URLs <> nil then
      for n := 0 to Entry.URLs.Count - 1 do
      begin
        if URL <> '' then
          if LowerCase(Entry.URLs[n]) = LowerCase(URL) then
          begin
            Result := TComboExItem(ItemsEx[i]);
            Exit;
          end;
        for j := 0 to URLs.Count - 1 do
          if LowerCase(Entry.URLs[n]) = LowerCase(URLs[j]) then
          begin
            Result := TComboExItem(ItemsEx[i]);
            Exit;
          end;
      end;
  end;
end;

function TMStationCombo.ItemsCompare(List: TListControlItems; Item1,
  Item2: TListControlItem): Integer;
var
  E1, E2: TStreamEntry;
begin
  E1 := TStreamEntry(Item1.Data);
  E2 := TStreamEntry(Item2.Data);
  if E1.RecentIndex > E2.RecentIndex then
    Result := 1
  else if E1.RecentIndex < E2.RecentIndex then
    Result := -1
  else
    Result := 0;
end;

end.
