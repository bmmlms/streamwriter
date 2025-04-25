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

unit AddonManager;

interface

uses
  AddonAudioGenie,
  AddonBase,
  AddonFAAC,
  AddonLAME,
  AddonMP4Box,
  AddonOggEnc,
  AddonSoX,
  AudioFunctions,
  Classes,
  Forms,
  Functions,
  Generics.Collections,
  LanguageObjects,
  SysUtils,
  TypeDefs,
  Windows;

type
  TCanEncodeResults = (ceNoAddon, ceAddonNeeded, ceOkay);

  TAddonArray = array of TAddonBase;

  TAddonManager = class
  private
    FShowVersionWarning: Boolean;
    FAddons: TList<TAddonBase>;
  public
    constructor Create;
    destructor Destroy; override;

    function EnableAddon(Owner: TCustomForm; Addon: TAddonBase; ShowErrors: Boolean): Boolean;
    function CanEncode(AudioType: TAudioTypes): TCanEncodeResults; overload;
    function Find(ClassTypes: TClassArray): TAddonArray; overload;
    function Find(ClassType: TClass): TAddonBase; overload;
    function Find(AudioType: TAudioTypes): TAddonBase; overload;

    property ShowVersionWarning: Boolean read FShowVersionWarning;
    property Addons: TList<TAddonBase> read FAddons;
  end;

implementation

uses
  DownloadAddons;

  { TAddonManager }

function TAddonManager.CanEncode(AudioType: TAudioTypes): TCanEncodeResults;
var
  Addon: TAddonBase;
begin
  Result := ceNoAddon;
  for Addon in FAddons do
    if Addon.CanEncode(AudioType) then
      Exit(IfThen<TCanEncodeResults>(Addon.FilesExtracted, ceOkay, ceAddonNeeded));
end;

constructor TAddonManager.Create;
var
  Addon: TAddonBase;
begin
  inherited;

  FAddons := TList<TAddonBase>.Create;

  FAddons.Add(TAddonOggEnc.Create);
  FAddons.Add(TAddonLAME.Create);
  FAddons.Add(TAddonFAAC.Create);
  FAddons.Add(TAddonSoX.Create);
  FAddons.Add(TAddonMP4Box.Create);
  FAddons.Add(TAddonAudioGenie.Create);

  for Addon in FAddons do
    if (Addon.PackageDownloaded) and (not Addon.FilesExtracted) then
    try
      if not Addon.VersionOkay then
      begin
        FShowVersionWarning := True;
        Continue;
      end;
      Addon.ExtractFiles;
    except
    end;
end;

destructor TAddonManager.Destroy;
var
  Addon: TAddonBase;
begin
  for Addon in FAddons do
    Addon.Free;

  FAddons.Free;

  inherited;
end;

function TAddonManager.EnableAddon(Owner: TCustomForm; Addon: TAddonBase; ShowErrors: Boolean): Boolean;
var
  RequiredAddonClass: TClass;
  RequiredAddon: TAddonBase;
  DA: TfrmDownloadAddons;
begin
  if not Addon.DependenciesMet then
  begin
    for RequiredAddonClass in Addon.RequiredAddons do
    begin
      RequiredAddon := Find(RequiredAddonClass);

      if not RequiredAddon.FilesExtracted then
        if not EnableAddon(Owner, RequiredAddon, ShowErrors) then
          Exit(False);
    end;
  end;

  if not Addon.PackageDownloaded then
  begin
    DA := TfrmDownloadAddons.Create(Owner, Addon);
    try
      DA.ShowModal;

      if not DA.Downloaded then
      begin
        if ShowErrors and DA.Error then
          TFunctions.MsgBox(_('An error occured while downloading the file.'), _('Error'), MB_ICONEXCLAMATION);

        Exit(False);
      end;
    finally
      DA.Free;
    end;
  end;

  if not Addon.ExtractFiles then
  begin
    if ShowErrors then
      TFunctions.MsgBox(_('The addon is not ready for use. This might happen when it''s files could not be extracted.'), _('Error'), MB_ICONEXCLAMATION);

    Exit(False);
  end;

  Exit(True);
end;

function TAddonManager.Find(ClassTypes: TClassArray): TAddonArray;
var
  ClassType: TClass;
  Addon: TAddonBase;
begin
  Result := [];
  for ClassType in ClassTypes do
    for Addon in FAddons do
      if Addon.ClassType = ClassType then
        Result += [Addon];
end;

function TAddonManager.Find(ClassType: TClass): TAddonBase;
var
  Addon: TAddonBase;
begin
  Result := nil;
  for Addon in FAddons do
    if Addon.ClassType = ClassType then
      Exit(Addon);
end;

function TAddonManager.Find(AudioType: TAudioTypes): TAddonBase;
var
  Addon: TAddonBase;
begin
  Result := nil;
  for Addon in FAddons do
    if Addon.CanEncode(AudioType) then
      Exit(Addon);
end;

end.
