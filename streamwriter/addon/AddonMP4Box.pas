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
unit AddonMP4Box;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions, TypeDefs;

type
  TAddonMP4Box = class(TAddonBase)
  private
    FMP4BoxEXEPath: string;
  protected
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    procedure Initialize; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;

    property MP4BoxEXEPath: string read FMP4BoxEXEPath;
  end;

implementation

uses
  AppData, AddonFAAC;

{ TAddonMP4Box }

procedure TAddonMP4Box.Assign(Source: TAddonBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;

  FMP4BoxEXEPath := TAddonMP4Box(Source).MP4BoxEXEPath;
end;

function TAddonMP4Box.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atM4A;
end;

function TAddonMP4Box.Copy: TAddonBase;
begin
  Result := TAddonMP4Box.Create;

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonMP4Box.Create;
begin
  inherited;

  FNeededAddons.Add(TAddonFAAC);

  FName := _('Support conversion of AAC files to M4A container');
  FHelp := _('This addon adds support for converting AAC files to M4A files to the application which is useful for postprocessing of recorded songs.');
  FDownloadName := 'addon_mp4box';
  FDownloadPackage := 'addon_mp4box.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_mp4box\';
  FMP4BoxEXEPath := FFilesDir + 'MP4Box.exe';

  FFilenames.Add('js32.dll');
  FFilenames.Add('MP4Box.exe');
  FFilenames.Add('msvcr100.dll');

  FNeededVersion := ParseVersion('1.0.0.1');
end;

procedure TAddonMP4Box.Initialize;
begin
  inherited;

  FName := _('Support conversion of AAC files to M4A container');
  FHelp := _('This addon adds support for converting AAC files to M4A files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonMP4Box.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

end.
