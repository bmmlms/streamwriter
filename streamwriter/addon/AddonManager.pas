unit AddonManager;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, AddonBase, AddonLAME,
  Forms, Functions, LanguageObjects, AddonSoX, TypeDefs, AddonFAAC, AddonOggEnc,
  AddonMP4Box, AddonAudioGenie;

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
var
  i: Integer;
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
  DA: TfrmDownloadAddons;
begin
  if not Addon.DependenciesMet then
  begin
    for i := 0 to Addon.NeededAddons.Count - 1 do
      if not EnableAddon(Owner, Find(Addon.NeededAddons[i]), False) then
        Exit(False);
  end;

  if not Addon.PackageDownloaded then
  begin
    if ShowMessage then
      Res := MsgBox(Owner.Handle, _('The addon cannot be activated because needed files have not been downloaded.'#13#10'Do you want to download these files now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1)
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
