{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2014 Alexander Nottelmann

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
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions;

type
  TAddonAudioGenie = class(TAddonBase)
  private
    FDLLPath: string;
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    property DLLPath: string read FDLLPath;
  end;

implementation

uses
  AppData;

{ TAddonAudioGenie }

procedure TAddonAudioGenie.Assign(Source: TAddonBase);
begin
  inherited;

  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonAudioGenie.Copy: TAddonBase;
begin
  Result := TAddonAudioGenie.Create;

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonAudioGenie.Create;
begin
  inherited;

  FDownloadName := 'addon_audiogenie';
  FDownloadPackage := 'addon_audiogenie.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_audiogenie\';
  FDLLPath := FFilesDir + 'audiogenie3.dll';

  FFilenames.Add('audiogenie3.dll');
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
