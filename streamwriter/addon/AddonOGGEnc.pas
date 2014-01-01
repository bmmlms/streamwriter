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

unit AddonOGGEnc;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions,
  AudioFunctions;

type
  TAddonOGGEnc = class(TAddonBase)
  private
    FEXEPath: string;
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TAddonOGGEnc }

procedure TAddonOGGEnc.Assign(Source: TAddonBase);
begin
  inherited;

  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonOGGEnc.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atOGG;
end;

function TAddonOGGEnc.Copy: TAddonBase;
begin
  Result := TAddonOGGEnc.Create;

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonOGGEnc.Create;
begin
  inherited;

  FDownloadName := 'addon_oggenc';
  FDownloadPackage := 'addon_oggenc.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_oggenc\';
  FEXEPath := FFilesDir + 'oggenc2.exe';

  FFilenames.Add('oggenc2.exe');
end;

function TAddonOGGEnc.FGetHelp: string;
begin
  Result := _('This addon adds support for encoding of OGG files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonOGGEnc.FGetName: string;
begin
  Result := _('Support encoding of OGG using OggEnc');
end;

function TAddonOGGEnc.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

end.
