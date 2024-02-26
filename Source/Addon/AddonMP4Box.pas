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

unit AddonMP4Box;

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
  TAddonMP4Box = class(TAddonBase)
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
  AddonFAAC,
  AppData;

{ TAddonMP4Box }

function TAddonMP4Box.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atM4A;
end;

function TAddonMP4Box.Copy: TAddonBase;
begin
  Result := TAddonMP4Box.Create;
  Result.Copied := True;
end;

constructor TAddonMP4Box.Create;
begin
  inherited Create('addon_mp4box', ['MP4Box.exe'], 'MP4Box.exe', '1.0.0.1');

  FRequiredAddons := [TAddonFAAC];
end;

function TAddonMP4Box.FGetHelp: string;
begin
  Result := _('This addon adds support for converting AAC files to M4A files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonMP4Box.FGetName: string;
begin
  Result := _('Support conversion of AAC files to M4A container');
end;

end.
