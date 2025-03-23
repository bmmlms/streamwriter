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

unit AddonAudioGenie;

interface

uses
  AddonBase,
  Classes,
  LanguageObjects,
  SysUtils;

type
  TAddonAudioGenie = class(TAddonBase)
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function Copy: TAddonBase; override;
  end;

implementation

uses
  AppData;

{ TAddonAudioGenie }

function TAddonAudioGenie.Copy: TAddonBase;
begin
  Result := TAddonAudioGenie.Create;
  Result.Copied := True;
end;

constructor TAddonAudioGenie.Create;
begin
  inherited Create('addon_audiogenie', ['audiogenie3.dll'], 'audiogenie3.dll');
end;

function TAddonAudioGenie.FGetHelp: string;
begin
  Result := _('This addon adds support for reading/writing tags to/from saved songs using AudioGenie.');
end;

function TAddonAudioGenie.FGetName: string;
begin
  Result := _('Support reading/writing of tags using AudioGenie');
end;

end.
