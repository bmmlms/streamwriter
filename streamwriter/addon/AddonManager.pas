{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2020 Alexander Nottelmann

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
  Windows, SysUtils, Classes, Generics.Collections, AddonBase, AddonLAME,
  Forms, Functions, LanguageObjects, AddonSoX, AddonFAAC, AddonOggEnc,
  AddonMP4Box, AddonAudioGenie, AudioFunctions;

type
  TCanEncodeResults = (ceNoAddon, ceAddonNeeded, ceOkay);

  TAddonManager = class
  private
    FShowVersionWarning: Boolean;
    FAddons: TList<TAddonBase>;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(ClassType: TClass): TAddonBase;
    function EnableAddon(Owner: TCustomForm; Addon: TAddonBase; ShowMessage: Boolean): Boolean;
    function CanEncode(FileExtension: string): TCanEncodeResults; overload;
    function CanEncode(AudioType: TAudioTypes): TCanEncodeResults; overload;
    function InstallEncoderFor(Owner: TCustomForm; AudioType: TAudioTypes): Boolean;

    property ShowVersionWarning: Boolean read FShowVersionWarning;
    property Addons: TList<TAddonBase> read FAddons;
  end;

implementation

uses
  DownloadAddons;

{ TAddonManager }

function TAddonManager.CanEncode(FileExtension: string): TCanEncodeResults;
begin
  Result := CanEncode(FiletypeToFormat(FileExtension));
end;

function TAddonManager.CanEncode(AudioType: TAudioTypes): TCanEncodeResults;
var
  i: Integer;
begin
  Result := ceNoAddon;
  for i := 0 to Addons.Count - 1 do
    if Addons[i].CanEncode(AudioType) then
    begin
      Result := ceAddonNeeded;
      if Addons[i].FilesExtracted then
        Result := ceOkay;
      Exit;
    end;
end;

constructor TAddonManager.Create;
var
  i: Integer;
  PB: TAddonBase;
begin
  inherited Create;

  FAddons := TList<TAddonBase>.Create;

  FAddons.Add(TAddonOggEnc.Create);
  FAddons.Add(TAddonLAME.Create);
  FAddons.Add(TAddonFAAC.Create);
  FAddons.Add(TAddonSoX.Create);
  FAddons.Add(TAddonMP4Box.Create);
  FAddons.Add(TAddonAudioGenie.Create);

  for i := 0 to FAddons.Count - 1 do
    if FAddons[i].ClassType.InheritsFrom(TAddonBase) then
    begin
      PB := TAddonBase(Addons[i]);
      if (PB.PackageDownloaded) and (not PB.FilesExtracted) then
      begin
        try
          if not PB.VersionOkay then
          begin
            FShowVersionWarning := True;
            Continue;
          end;
          PB.ExtractFiles;
        except
        end;
      end;
    end;
end;

destructor TAddonManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FAddons.Count - 1 do
  begin
    FAddons[i].Free;
  end;

  FAddons.Free;

  inherited;
end;

function TAddonManager.EnableAddon(Owner: TCustomForm; Addon: TAddonBase; ShowMessage: Boolean): Boolean;
var
  i: Integer;
  Res: Integer;
  MsgShown: Boolean;
  DA: TfrmDownloadAddons;
begin
  if not Addon.DependenciesMet then
  begin
    MsgShown := False;

    for i := 0 to Addon.NeededAddons.Count - 1 do
      if not Addon.FilesExtracted then
      begin
        if not MsgShown then
        begin
          if MsgBox(Owner.Handle, _('The selected addon has some unmet dependencies.'#13#10'Do you want do download the required addons now?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDNO then
          begin
            Exit(False);
          end;
          MsgShown := True;
        end;
        if not EnableAddon(Owner, Find(Addon.NeededAddons[i]), False) then
          Exit(False);
      end;
  end;

  if not Addon.PackageDownloaded then
  begin
    if ShowMessage then
      Res := MsgBox(Owner.Handle, Format(_('The addon "%s" cannot be activated because needed files have not been downloaded.'#13#10'Do you want to download these files now?'), [Addon.Name]), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1)
    else
      Res := IDYES;

    if Res = IDYES then
    begin
      if not Addon.ShowInitMessage(Owner.Handle) then
      begin
        Exit(False);
      end;

      DA := TfrmDownloadAddons.Create(Owner, Addon);
      try
        DA.ShowModal;

        if not DA.Downloaded then
        begin
          if DA.Error then
            MsgBox(Owner.Handle, _('An error occured while downloading the file.'), _('Error'), MB_ICONEXCLAMATION);

          Exit(False);
        end;
      finally
        DA.Free;
      end;
    end else if Res = IDNO then
    begin
      Exit(False);
    end;
  end;

  if not Addon.ExtractFiles then
  begin
    MsgBox(Owner.Handle, _('The addon is not ready for use. This might happen when it''s files could not be extracted.'), _('Error'), MB_ICONEXCLAMATION);
    Exit(False);
  end;

  Exit(True);
end;

function TAddonManager.Find(ClassType: TClass): TAddonBase;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FAddons.Count - 1 do
    if FAddons[i].ClassType = ClassType then
    begin
      Result := FAddons[i];
      Break;
    end;
end;

function TAddonManager.InstallEncoderFor(Owner: TCustomForm;
  AudioType: TAudioTypes): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FAddons.Count - 1 do
    if FAddons[i].CanEncode(AudioType) then
    begin
      Result := EnableAddon(Owner, FAddons[i], False);
      Exit;
    end;
end;

end.
