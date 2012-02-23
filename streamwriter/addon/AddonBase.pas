unit AddonBase;

interface

uses
  SysUtils, Windows, Classes, Functions, TypeDefs;

type
  TAddonBase = class
  private
  protected
    FCopied: Boolean;
    FFilesDir: string;
    FCanConfigure: Boolean;
    FName: string;
    FHelp: string;
    FOrder: Integer;
    FOnlyIfCut: Boolean;

    FDownloadPackage: string;
    FDownloadName: string;

    FFilenames: TStringList;
    FNeededAddons: TList;
    FNeededVersion: TAppVersion;

    function FGetFilesExtracted: Boolean; virtual;
    function FGetPackageDownloaded: Boolean; virtual;
    function FGetVersionOkay: Boolean;
    function FGetDependenciesMet: Boolean; virtual;

    procedure DeleteFiles; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Copy: TAddonBase; virtual; abstract;
    procedure Assign(Source: TAddonBase); virtual;

    procedure Initialize; virtual;
    function ExtractFiles: Boolean; virtual;

    function ShowInitMessage(Handle: THandle): Boolean; virtual;

    function CanEncode(AudioType: TAudioTypes): Boolean; virtual;

    property Copied: Boolean read FCopied write FCopied;

    property Name: string read FName;
    property Help: string read FHelp;

    property DownloadPackage: string read FDownloadPackage;
    property DownloadName: string read FDownloadName;
    property NeededAddons: TList read FNeededAddons;

    property FilesExtracted: Boolean read FGetFilesExtracted;
    property PackageDownloaded: Boolean read FGetPackageDownloaded;
    property VersionOkay: Boolean read FGetVersionOkay;
    property DependenciesMet: Boolean read FGetDependenciesMet;
  end;

implementation

uses
  AppData;

{ TAddonBase }

procedure TAddonBase.Assign(Source: TAddonBase);
begin
  FFilenames.Assign(Source.FFilenames);
end;

function TAddonBase.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := False;
end;

constructor TAddonBase.Create;
begin
  inherited;

  FFilenames := TStringList.Create;
  FNeededAddons := TList.Create;
  FNeededVersion := ParseVersion('1.0.0.0');
end;

procedure TAddonBase.DeleteFiles;
var
  i: Integer;
begin
  if FCopied then
    Exit;

  for i := 0 to FFilenames.Count - 1 do
    Windows.DeleteFile(PChar(FFilesDir + FFilenames[i]));
  try
    RmDir(FFilesDir);
  except end;
end;

destructor TAddonBase.Destroy;
begin
  DeleteFiles;
  FFilenames.Free;
  FNeededAddons.Free;

  inherited;
end;

function TAddonBase.ExtractFiles: Boolean;
var
  i: Integer;
  H: THandle;
  Res: TResourceStream;
begin
  if FilesExtracted then
    Exit(True);

  SetErrorMode(SEM_FAILCRITICALERRORS);

  Result := False;

  ForceDirectories(FFilesDir);
  if FileExists(AppGlobals.Storage.DataDir + FDownloadPackage) then
  begin
    H := LoadLibrary(PChar(AppGlobals.Storage.DataDir + FDownloadPackage));
    if H > 0 then
    begin
      for i := 0 to FFilenames.Count - 1 do
      begin
        try
          Res := TResourceStream.Create(H, StringReplace(FFilenames[i], '.', '_', [rfReplaceAll]), RT_RCDATA);
          try
            Res.SaveToFile(FFilesDir + FFilenames[i]);
          finally
            Res.Free;
          end;
        except end;
      end;
      Result := True;
      FreeLibrary(H);
    end else
      Windows.DeleteFile(PChar(AppGlobals.Storage.DataDir + FDownloadPackage));
  end;
end;

function TAddonBase.FGetDependenciesMet: Boolean;
var
  i: Integer;
  Addon: TAddonBase;
begin
  for i := 0 to FNeededAddons.Count - 1 do
  begin
    Addon := AppGlobals.AddonManager.Find(FNeededAddons[i]);
    if (Addon = nil) or (not Addon.FilesExtracted) then
      Exit(False);
  end;
  Exit(True);
end;

function TAddonBase.FGetFilesExtracted: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FFilenames.Count - 1 do
    if not FileExists(FFilesDir + FFilenames[i]) then
    begin
      Result := False;
      Break;
    end;
end;

function TAddonBase.FGetPackageDownloaded: Boolean;
begin
  Result := FileExists(AppGlobals.Storage.DataDir + FDownloadPackage);
end;

function TAddonBase.FGetVersionOkay: Boolean;
var
  Ver: TAppVersion;
begin
  Result := True;
  try
    Ver := GetFileVersion(AppGlobals.Storage.DataDir + FDownloadPackage);
    if IsVersionNewer(Ver, FNeededVersion) then
      Result := False;
  except
    Result := False;
  end;

  if not Result then
  begin
    DeleteFile(PChar(AppGlobals.Storage.DataDir + FDownloadPackage));
    Result := False;
  end;
end;

procedure TAddonBase.Initialize;
begin
  inherited;

end;

function TAddonBase.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

end.
