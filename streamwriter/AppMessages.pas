{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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

unit AppMessages;

interface

uses
  Windows, SysUtils, MessageBus;

type
  TFileModifyMsg = class(TMessageBase)
  private
    FFilename: string;
  public
    constructor Create(Filename: string);
    property Filename: string read FFilename;
  end;

  TVolumeChangedMsg = class(TMessageBase)
  private
    FVolume: Integer;
  public
    constructor Create(Volume: Integer);
    property Volume: Integer read FVolume;
  end;

implementation

{ TDeleteFileMessage }

constructor TFileModifyMsg.Create(Filename: string);
begin
  inherited Create;
  FFilename := Filename;
end;

{ TVolumeChangedMsg }

constructor TVolumeChangedMsg.Create(Volume: Integer);
begin
  inherited Create;
  FVolume := Volume;
end;

end.
