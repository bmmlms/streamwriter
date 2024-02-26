{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2024 Alexander Nottelmann

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

unit AddonFAAC;

interface

uses
  AddonBase,
  AudioFunctions,
  Classes,
  Functions,
  LanguageObjects,
  SysUtils,
  Windows;

type
  TAddonFAAC = class(TAddonBase)
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function Copy: TAddonBase; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;
  end;

implementation

uses
  AppData;

{ TAddonFAAC }

function TAddonFAAC.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atAAC;
end;

function TAddonFAAC.Copy: TAddonBase;
begin
  Result := TAddonFAAC.Create;
  Result.Copied := True;
end;

constructor TAddonFAAC.Create;
begin
  inherited Create('addon_faac', ['faac.exe'], 'faac.exe');
end;

function TAddonFAAC.FGetHelp: string;
begin
  Result := _('This addon adds support for encoding of AAC files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonFAAC.FGetName: string;
begin
  Result := _('Support encoding of AAC using FAAC');
end;

end.
