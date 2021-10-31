{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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

unit AddonLAME;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions,
  AudioFunctions;

type
  TAddonLAME = class(TAddonBase)
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

{ TAddonLAME }

procedure TAddonLAME.Assign(Source: TAddonBase);
begin
  inherited;

  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonLAME.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atMPEG;
end;

function TAddonLAME.Copy: TAddonBase;
begin
  Result := TAddonLAME.Create;

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonLAME.Create;
begin
  inherited;

  FDownloadName := 'addon_lame';
  FDownloadPackage := 'addon_lame.dll';
  FHasInitMessage := True;

  FFilesDir := ConcatPaths([AppGlobals.TempDir, 'addon_lame']);
  FEXEPath := ConcatPaths([FFilesDir, 'lame.exe']);

  FFilenames.Add('lame.exe');
end;

function TAddonLAME.FGetHelp: string;
begin
  Result := _('This addon adds support for encoding of MP3 files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonLAME.FGetName: string;
begin
  Result := _('Support encoding of MP3 using LAME');
end;

function TAddonLAME.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := MsgBox(_('WARNING:'#13#10'It may not be allowed in some contries to use the LAME-addon because it contains lame.exe ' +
                             'that makes use of some patented technologies. Please make sure you may use these files in your country. ' +
                             'If you are sure you may use these files, press "Yes" to continue.'), _('Warning'), MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = IDYES;
end;

end.
