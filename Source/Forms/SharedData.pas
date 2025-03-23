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

unit SharedData;

interface

uses
  Classes,
  Controls,
  ImgList,
  SysUtils;

type

  { TmodSharedData }

  TmodSharedData = class(TDataModule)
    imgImages: TImageList;
  private
  public
  end;

var
  modSharedData: TmodSharedData;

implementation

{$R *.lfm}

end.
