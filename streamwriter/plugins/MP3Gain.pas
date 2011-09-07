{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit MP3Gain;

interface

uses
  Windows, SysUtils, Classes, Plugins, PluginsShared, LanguageObjects,
  Functions, Logging;

type
  TNormalizeThread = class(TProcessThreadBase)
  private
    FMP3GainPath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
  end;

  TNormalizePlugin = class(TInternalPlugin)
  private
    FFilesDir: string;
    FCopied: Boolean;
    FMP3GainExe: string;

    procedure DeleteFiles;
  protected
    function FGetReadyForUse: Boolean; override;
    function FGetFilesInstalled: Boolean; override;
    function FGetReadyForActivate: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;
    procedure Initialize; override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
    procedure Save; override;
    function ExtractFiles: Boolean;
    property MP3GainExe: string read FMP3GainExe;
  end;

implementation

uses
  AppData;

const
  Filenames: array[0..0] of string = ('mp3gain.exe');

{ TNormalizeThread }

constructor TNormalizeThread.Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
begin
  inherited Create(Data, Plugin);

  FMP3GainPath := TNormalizePlugin(Plugin).FFilesDir + 'mp3gain.exe';
end;

procedure TNormalizeThread.Execute;
var
  CmdLine: string;
  Output: AnsiString;
  P: TNormalizePlugin;
  Failed: Boolean;
  EC: DWORD;
begin
  inherited;

  FResult := arFail;

  if LowerCase(ExtractFileExt(FData.Filename)) <> '.mp3' then
  begin
    FResult := arImpossible;
    Exit;
  end;

  CmdLine := '"' + FMP3GainPath + '" "' + FData.Filename + '"';

  P := TNormalizePlugin(Plugin);

  if RunProcess(CmdLine, ExtractFilePath(FMP3GainPath), 120000, Output, EC, @Terminated) = 2 then
  begin
    FResult := arTimeout;
  end else
  begin
    Failed := EC <> 0;

    if not Failed then
    begin
      FData.Filesize := GetFileSize(FData.Filename);
      FResult := arWin;
    end;
  end;
end;

{ TNormalizePlugin }

procedure TNormalizePlugin.Assign(Source: TPluginBase);
begin
  inherited;

end;

function TNormalizePlugin.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
begin
  Result := True;
end;

function TNormalizePlugin.Copy: TPluginBase;
begin
  Result := TNormalizePlugin.Create;

  TNormalizePlugin(Result).FCopied := True;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

constructor TNormalizePlugin.Create;
begin
  inherited;

  FActive := False;
  FOrder := 100;
  FDownloadPackage := 'mp3gain.dll';
  FDownloadName := 'mp3gain';
  FCanConfigure := False;

  FFilesDir := AppGlobals.TempDir + 'mp3gain\';
  FMP3GainExe := FFilesDir + 'mp3gain.exe';

  FName := _('Normalize saved files using mp3gain (MP3)');
  FHelp := _('This normalizes recorded songs using mp3gain.');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 90, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    if not FGetFilesInstalled then
      FActive := False;
  except end;

  if not FGetReadyForActivate then
    ExtractFiles;

  if not FGetReadyForActivate then
    FActive := False;
end;

procedure TNormalizePlugin.DeleteFiles;
var
  i: Integer;
begin
  if FCopied then
    Exit;

  for i := 0 to High(Filenames) do
    DeleteFile(FFilesDir + Filenames[i]);
  try
    RmDir(FFilesDir);
  except end;
end;

destructor TNormalizePlugin.Destroy;
begin
  DeleteFiles;

  inherited;
end;

function TNormalizePlugin.ExtractFiles: Boolean;
var
  i: Integer;
  H: THandle;
  Res: TResourceStream;
begin
  SetErrorMode(SEM_FAILCRITICALERRORS);

  Result := False;

  ForceDirectories(FFilesDir);
  if FileExists(AppGlobals.Storage.DataDir + FDownloadPackage) then
  begin
    H := LoadLibrary(PChar(AppGlobals.Storage.DataDir + FDownloadPackage));
    if H > 0 then
    begin
      for i := 0 to High(Filenames) do
      begin
        try
          Res := TResourceStream.Create(H, StringReplace(Filenames[i], '.', '_', [rfReplaceAll]), RT_RCDATA);
          try
            Res.SaveToFile(FFilesDir + Filenames[i]);
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

function TNormalizePlugin.FGetFilesInstalled: Boolean;
begin
  Result := FileExists(AppGlobals.Storage.DataDir + FDownloadPackage);
end;

function TNormalizePlugin.FGetReadyForActivate: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(Filenames) do
    if not FileExists(FFilesDir + Filenames[i]) then
    begin
      Result := False;
      Break;
    end;
end;

function TNormalizePlugin.FGetReadyForUse: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(Filenames) do
    if not FileExists(FFilesDir + Filenames[i]) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TNormalizePlugin.Initialize;
begin
  inherited;

  if (not FGetReadyForUse) or (not FGetReadyForActivate) then
    ExtractFiles;

  FName := _('Normalize saved files using mp3gain (MP3)');
  FHelp := _('This normalizes recorded songs using mp3gain.');
end;

function TNormalizePlugin.ProcessFile(
  Data: PPluginProcessInformation): TProcessThreadBase;
begin
  Result := TNormalizeThread.Create(Data, Self);
end;

procedure TNormalizePlugin.Save;
begin
  inherited;

end;

function TNormalizePlugin.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

end.
