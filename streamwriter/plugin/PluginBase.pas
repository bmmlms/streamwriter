unit PluginBase;

interface

uses
  SysUtils, Windows, Classes, Functions;

type
  TPluginBase = class
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

    function FGetFilesExtracted: Boolean; virtual;
    function FGetPackageDownloaded: Boolean; virtual;
    function FGetVersionOkay: Boolean;

    procedure DeleteFiles; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Copy: TPluginBase; virtual; abstract;
    procedure Assign(Source: TPluginBase); virtual;

    procedure Initialize; virtual;
    function ExtractFiles: Boolean; virtual;

    function ShowInitMessage(Handle: THandle): Boolean; virtual;

    function CanEncode(FileExtension: string): Boolean; virtual;

    property Name: string read FName;
    property Help: string read FHelp;

    property DownloadPackage: string read FDownloadPackage;
    property DownloadName: string read FDownloadName;

    property FilesExtracted: Boolean read FGetFilesExtracted;
    property PackageDownloaded: Boolean read FGetPackageDownloaded;
    property VersionOkay: Boolean read FGetVersionOkay;
  end;

implementation

uses
  AppData;

{ TPluginBase }

procedure TPluginBase.Assign(Source: TPluginBase);
begin
  FFilenames.Assign(Source.FFilenames);
end;

function TPluginBase.CanEncode(FileExtension: string): Boolean;
begin
  Result := False;
end;

constructor TPluginBase.Create;
begin
  inherited;

  FFilenames := TStringList.Create;
end;

procedure TPluginBase.DeleteFiles;
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

destructor TPluginBase.Destroy;
begin
  DeleteFiles;
  FFilenames.Free;

  inherited;
end;

function TPluginBase.ExtractFiles: Boolean;
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

function TPluginBase.FGetFilesExtracted: Boolean;
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

function TPluginBase.FGetPackageDownloaded: Boolean;
begin
  Result := FileExists(AppGlobals.Storage.DataDir + FDownloadPackage);
end;

function TPluginBase.FGetVersionOkay: Boolean;
begin
  Result := True;
  try
    GetFileVersion(AppGlobals.Storage.DataDir + FDownloadPackage);
  except
    DeleteFile(PChar(AppGlobals.Storage.DataDir + FDownloadPackage));
    Exit(False);
  end;
end;

procedure TPluginBase.Initialize;
begin
  inherited;

end;

function TPluginBase.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

end.
