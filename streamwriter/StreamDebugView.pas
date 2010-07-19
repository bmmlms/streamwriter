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
unit StreamDebugView;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ImgList,
  RecentManager, VirtualTrees, LanguageObjects, GUIFunctions,
  Generics.Collections, Graphics, Forms, DebugView, ICEClient;

type
  TMStreamDebugView = class(TPanel)
  private
    FClient: TICEClient;
    FDebug: TDebugView;
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure ShowDebug(Client: TICEClient);

    property Client: TICEClient read FClient;
  end;

implementation

{ TStreamDebugView }

constructor TMStreamDebugView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Align := alClient;
  BorderStyle := bsNone;

  FDebug := TDebugView.Create(Self);
  FDebug.Parent := Self;
  FDebug.Align := alClient;
  FDebug.Show;
end;

destructor TMStreamDebugView.Destroy;
begin

  inherited;
end;

procedure TMStreamDebugView.ShowDebug(Client: TICEClient);
var
  i: Integer;
  Entry: TDebugEntry;
begin
  if Client <> FClient then
    FDebug.Clear;
  FClient := Client;
  if Client <> nil then
  begin
    if Client.Killed then
      Exit;
    for i := FDebug.RootNodeCount to Client.DebugLog.Count - 1 do
    begin
      FDebug.AddData(Client.DebugLog[i].Time, Client.DebugLog[i].Text, Client.DebugLog[i].Data);
    end;
  end;
end;

end.
