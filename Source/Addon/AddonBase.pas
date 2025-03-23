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

unit AddonBase;

interface

uses
  AudioFunctions,
  Classes,
  Functions,
  SysUtils,
  TypeDefs,
  Windows;

type

  { TAddonBase }

  TAddonBase = class
  protected
    FCopied: Boolean;
    FCanConfigure: Boolean;
    FOrder: Integer;
    FOnlyIfCut: Boolean;

    FDownloadName: string;
    FDownloadPackage: string;
    FFilesDir: string;
    FModuleFilePath: string;
    FFilenames: TStringArray;
    FRequiredVersion: TAppVersion;
    FRequiredAddons: TClassArray;

    function FGetFilesExtracted: Boolean; virtual;
    function FGetPackageDownloaded: Boolean; virtual;
    function FGetVersionOkay: Boolean;
    function FGetDependenciesMet: Boolean; virtual;

    function FGetName: string; virtual;
    function FGetHelp: string; virtual;

    procedure DeleteFiles; virtual;
  public
    constructor Create(DownloadName: string; Filenames: TStringArray; ModuleFilename: string; RequiredVersion: string = '1.0.0.0');
    destructor Destroy; override;

    function Copy: TAddonBase; virtual; abstract;

    function ExtractFiles: Boolean; virtual;

    function CanEncode(AudioType: TAudioTypes): Boolean; virtual;

    property Copied: Boolean read FCopied write FCopied;

    property Name: string read FGetName;
    property Help: string read FGetHelp;

    property DownloadName: string read FDownloadName;
    property DownloadPackage: string read FDownloadPackage;
    property ModuleFilePath: string read FModuleFilePath;
    property RequiredAddons: TClassArray read FRequiredAddons;

    property FilesExtracted: Boolean read FGetFilesExtracted;
    property PackageDownloaded: Boolean read FGetPackageDownloaded;
    property VersionOkay: Boolean read FGetVersionOkay;
    property DependenciesMet: Boolean read FGetDependenciesMet;
  end;

implementation

uses
  AppData;

  { TAddonBase }

function TAddonBase.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := False;
end;

constructor TAddonBase.Create(DownloadName: string; Filenames: TStringArray; ModuleFilename: string; RequiredVersion: string = '1.0.0.0');
const
  Arch: string = {$IF defined(CPU64)}'x86_64'{$ELSEIF defined(CPU32)}'i386'{$ELSE}{$ERROR Unknown architecture}{$ENDIF};
begin
  FDownloadName := DownloadName;
  FDownloadPackage := '%s-%s.dll'.Format([FDownloadName, Arch]);
  FFilenames := Filenames;
  FRequiredVersion := TFunctions.ParseVersion(RequiredVersion);

  FFilesDir := ConcatPaths([AppGlobals.TempDir, '%s-%s'.Format([FDownloadName, Arch])]);
  FModuleFilePath := ConcatPaths([FFilesDir, ModuleFilename]);
end;

procedure TAddonBase.DeleteFiles;
var
  Filename: string;
begin
  if FCopied then
    Exit;

  for Filename in FFilenames do
    SysUtils.DeleteFile(ConcatPaths([FFilesDir, Filename]));
  try
    RemoveDir(FFilesDir);
  except
  end;
end;

destructor TAddonBase.Destroy;
begin
  DeleteFiles;

  inherited;
end;

function TAddonBase.ExtractFiles: Boolean;
var
  H: THandle;
  Res: TResourceStream;
  Filename, LibraryPath: string;
begin
  if FilesExtracted then
    Exit(True);

  Result := False;
  LibraryPath := ConcatPaths([AppGlobals.Storage.AddonDir, FDownloadPackage]);

  if not FileExists(LibraryPath) then
    Exit;

  H := LoadLibrary(PChar(LibraryPath));
  if H > 0 then
  begin
    for Filename in FFilenames do
    try
      Res := TResourceStream.Create(H, StringReplace(Filename, '.', '_', [rfReplaceAll]), Windows.RT_RCDATA);
      try
        ForceDirectories(FFilesDir);
        Res.SaveToFile(ConcatPaths([FFilesDir, Filename]));
      finally
        Res.Free;
      end;
    except
    end;
    Result := FilesExtracted;
    FreeLibrary(H);
  end else
    SysUtils.DeleteFile(LibraryPath);
end;

function TAddonBase.FGetDependenciesMet: Boolean;
var
  AddonClass: TClass;
  Addon: TAddonBase;
begin
  for AddonClass in FRequiredAddons do
  begin
    Addon := AppGlobals.AddonManager.Find(AddonClass);
    if (Addon = nil) or (not Addon.FilesExtracted) then
      Exit(False);
  end;
  Exit(True);
end;

function TAddonBase.FGetFilesExtracted: Boolean;
var
  Filename: string;
begin
  Result := True;
  for Filename in FFilenames do
    if not FileExists(ConcatPaths([FFilesDir, Filename])) then
    begin
      Result := False;
      Break;
    end;
end;

function TAddonBase.FGetHelp: string;
begin
  Result := '';
end;

function TAddonBase.FGetName: string;
begin
  Result := '';
end;

function TAddonBase.FGetPackageDownloaded: Boolean;
begin
  Result := FileExists(ConcatPaths([AppGlobals.Storage.AddonDir, FDownloadPackage]));
end;

function TAddonBase.FGetVersionOkay: Boolean;
var
  Ver: TAppVersion;
begin
  Result := True;
  try
    Ver := TFunctions.GetFileVersion(ConcatPaths([AppGlobals.Storage.AddonDir, FDownloadPackage]));
    if TFunctions.IsVersionNewer(Ver, FRequiredVersion) then
      Result := False;
  except
    Result := False;
  end;

  if not Result then
    SysUtils.DeleteFile(ConcatPaths([AppGlobals.Storage.AddonDir, FDownloadPackage]));
end;

end.
