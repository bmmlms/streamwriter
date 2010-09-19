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
unit SavedTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, LanguageObjects, Tabs;

type
  TSavedTab = class(TMainTabSheet)
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup;
  end;

implementation

{ TCutTab }

constructor TSavedTab.Create(AOwner: TComponent);
begin
  inherited;

  ShowCloseButton := False;
  ImageIndex := 14;
end;

procedure TSavedTab.Setup;
begin
  Caption := _('Saved songs');

end;

end.
