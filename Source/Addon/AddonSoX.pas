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

unit AddonSoX;

interface

uses
  AddonBase,
  Classes,
  LanguageObjects,
  SysUtils;

type
  TAddonSoX = class(TAddonBase)
  private
    FEXEPath: string;
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TAddonSoX }

procedure TAddonSoX.Assign(Source: TAddonBase);
begin
  inherited;

  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonSoX.Copy: TAddonBase;
begin
  Result := TAddonSoX.Create;

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonSoX.Create;
begin
  inherited;

  FDownloadName := 'addon_sox';
  FDownloadPackage := 'addon_sox.dll';

  FFilesDir := ConcatPaths([AppGlobals.TempDir, 'addon_sox\']);
  FEXEPath := ConcatPaths([FFilesDir, 'sox.exe']);

  FFilenames.Add('sox.exe');
end;

function TAddonSoX.FGetHelp: string;
begin
  Result := _('This addon adds support for applying effects to recorded songs using Sound eXchange (SoX).');
end;

function TAddonSoX.FGetName: string;
begin
  Result := _('Support applying of effects to recorded songs using SoX');
end;

end.
