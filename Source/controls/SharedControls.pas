{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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

{ This unit contains controls used in this applications, TSeekBar and TVolumePanel }
unit SharedControls;

interface

uses
  AppData,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  GraphUtil,
  Images,
  LanguageObjects,
  Logging,
  Math,
  MControls,
  Menus,
  MStringFunctions,
  MVirtualTree,
  SharedData,
  SysUtils,
  Themes,
  VirtualTrees,
  Windows;

type
  TMenuColEvent = procedure(Sender: TVirtualStringTree; Index: Integer; Checken: Boolean) of object;

  TMTreeColumnPopup = class(TPopupMenu)
  private
    FFileView: TVirtualStringTree;
    FOnAction: TMenuColEvent;
    FHideIdx: Integer;

    procedure ColItemsClick(Sender: TObject);
  protected
    procedure DoPopup(Sender: TObject); override;
  public
    property OnAction: TMenuColEvent read FOnAction write FOnAction;
    property HideIdx: Integer read FHideIdx write FHideIdx;
  end;

  { TMSWVirtualTree }

  TMSWVirtualTree = class(TMVirtualTree, IPostTranslatable)
  public
    procedure PostTranslate; virtual;
  end;

  { TVirtualTreeColumnHelper }

  TVirtualTreeColumnHelper = class helper for TVirtualTreeColumn
  public
    procedure FitColumn(const LongestContentString: string = ''); overload;
    procedure FitColumn(const IconCount: Integer); overload;
  end;

implementation

{ TMSWVirtualTree }

procedure TMSWVirtualTree.PostTranslate;
begin
  if AppGlobals.TreeColorsLoaded then
  begin
    Font.Color := AppGlobals.TreeNodeFontColor;
    Color := AppGlobals.TreeBackgroundColor;
    Colors.SelectionTextColor := AppGlobals.TreeSelectionTextColor;
    Colors.FocusedSelectionColor := AppGlobals.TreeFocusedSelectionColor;
    Colors.FocusedSelectionBorderColor := AppGlobals.TreeFocusedSelectionColor;
  end else
    ResetColors;
end;

{ TMTreeColumnPopup }

procedure TMTreeColumnPopup.ColItemsClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    FOnAction(nil, TVirtualTreeColumn(TMenuItem(Sender).Tag).Index, True);
end;

procedure TMTreeColumnPopup.DoPopup(Sender: TObject);
var
  i: Integer;
  Tree: TVirtualStringTree;
  Item: TMenuItem;
begin
  inherited;

  Items.Clear;

  Tree := TVirtualStringTree(Owner);
  FFileView := Tree;
  for i := 1 to Tree.Header.Columns.Count - 1 do
  begin
    if i = FHideIdx then
      Continue;
    Item := TMenuItem.Create(Self);
    Item.Caption := Tree.Header.Columns[i].Text;
    Item.OnClick := ColItemsClick;
    Item.Tag := Integer(Tree.Header.Columns[i]);
    Items.Add(Item);
  end;

  for i := 0 to Items.Count - 1 do
    Items[i].Checked := coVisible in TVirtualTreeColumn(Items[i].Tag).Options;
end;

{ TVirtualTreeColumnHelper }

procedure TVirtualTreeColumnHelper.FitColumn(const LongestContentString: string);
begin
  Width := IfThen(Index = 0, TVirtualStringTree(Owner.Header.Treeview).Indent, 0)
    + Margin * 2
    + Spacing * 2
    + Max(TMStringFunctions.GetTextSize(Text, Owner.Header.Font).Width + Spacing + Owner.Header.Treeview.Scale96ToFont(16), TMStringFunctions.GetTextSize(LongestContentString, Owner.Header.Treeview.Font).Width);
end;

procedure TVirtualTreeColumnHelper.FitColumn(const IconCount: Integer);
begin
  Width := IfThen(Index = 0, TVirtualStringTree(Owner.Header.Treeview).Indent, 0)
    + Margin * 2
    + Max(Spacing * 2 + TMStringFunctions.GetTextSize(Text, Owner.Header.Font).Width + Spacing + Owner.Header.Treeview.Scale96ToFont(16), Owner.Header.Treeview.Scale96ToFont(IconCount * 16 + (IconCount - 1) * 2));
end;

end.
