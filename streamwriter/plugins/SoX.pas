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
unit SoX;

interface

uses
  Windows, SysUtils, Classes, Plugins, PluginsShared, LanguageObjects,
  Functions, ConfigureSoX;

type
  TSoXThread = class(TProcessThreadBase)
  private
    FSoxPath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
  end;

  TSoXPlugin = class(TInternalPlugin)
  private
    FFilesDir: string;
    FCopied: Boolean;
    FSoXExe: string;

    FFadeoutStart: Boolean;
    FFadeoutEnd: Boolean;
    FFadeoutStartLength: Integer;
    FFadeoutEndLength: Integer;
    FSilenceStart: Boolean;
    FSilenceEnd: Boolean;
    FSilenceStartLength: Integer;
    FSilenceEndLength: Integer;

    function DeleteFiles: Boolean;
  protected
    function FGetReadyForUse: Boolean; override;
    function FGetFilesInstalled: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;
    procedure Initialize; override;
    procedure Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean); override;
    procedure Save; override;
    function ExtractFiles: Boolean;
    property SoXExe: string read FSoXExe;
  end;

implementation

uses
  AppData;

const
  Filenames: array[0..5] of string = ('libgomp-1.dll', 'pthreadgc2.dll', 'zlib1.dll', 'sox.exe', 'lame-enc.dll', 'libmad.dll');

{ TSoXThread }

constructor TSoXThread.Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
begin
  inherited Create(Data, Plugin);

  FSoxPath := TSoXPlugin(Plugin).FFilesDir + 'sox.exe';
end;

procedure TSoXThread.Execute;
var
  TempFile, CmdLine, Params: string;
  Output: AnsiString;
  P: TSoXPlugin;
  LoopStarted: Integer;
  Failed: Boolean;
  FS: TFileStream;
begin
  inherited;

  FResult := arFail;

  if LowerCase(ExtractFileExt(FData.Filename)) <> '.mp3' then
  begin
    FResult := arImpossible;
    Exit;
  end;

  TempFile := RemoveFileExt(FData.Filename) + '_soxconvert' + ExtractFileExt(FData.Filename);
  CmdLine := '"' + FSoxPath + '"' + ' --multi-threaded "' + FData.Filename + '" "' + TempFile + '" ';

  P := TSoXPlugin(Plugin);

  Params := '';

  if P.FFadeoutStart and P.FFadeoutEnd then
    Params := 'fade p ' + IntToStr(P.FFadeoutStartLength) + ' ' + IntToStr(FData.Length - P.FFadeoutEndLength) + ' ' + IntToStr(P.FFadeoutEndLength)
  else if P.FFadeoutStart then
    Params := 'fade p ' + IntToStr(P.FFadeoutStartLength)
  else if P.FFadeoutEnd then
    Params := 'fade p 0 ' + IntToStr(FData.Length - P.FFadeoutEndLength) + ' ' + IntToStr(P.FFadeoutEndLength);

  if P.FSilenceStart and P.FSilenceEnd then
    Params := Params + ' ' + 'pad ' + IntToStr(P.FSilenceStartLength) + ' ' + IntToStr(P.FSilenceEndLength)
  else if P.FSilenceStart then
    Params := Params + ' ' + 'pad ' + IntToStr(P.FSilenceStartLength)
  else if P.FSilenceEnd then
    Params := Params + ' ' + 'pad 0 ' + IntToStr(P.FSilenceEndLength);

  if Params <> '' then
  begin
    if RunProcess(CmdLine + Params, ExtractFilePath(FSoxPath), 120000, Output) = 2 then
    begin
      FResult := arTimeout;
    end else
    begin
      Failed := True;
      if FileExists(TempFile) then
      begin
        LoopStarted := GetTickCount;
        while Failed do
        begin
          try
            FS := TFileStream.Create(TempFile, fmOpenRead or fmShareExclusive);
            try
              Failed := False;
              Break;
            finally
              FS.Free;
            end;
          except
            Sleep(50);
            if GetTickCount > LoopStarted + 5000 then
            begin
              Break;
            end;
          end;
        end;

        if not Failed then
          if not DeleteFile(FData.Filename) then
            Failed := True;

        if not Failed then
          if not MoveFile(PChar(TempFile), PChar(FData.Filename)) then
            Failed := True;

        if not Failed then
        begin
          FData.Filesize := GetFileSize(FData.Filename);
          FResult := arWin;
        end;
      end;
    end;
  end;
end;

{ TSoXPlugin }

procedure TSoXPlugin.Assign(Source: TPluginBase);
begin
  inherited;

  FFadeoutStart := TSoXPlugin(Source).FFadeoutStart;
  FFadeoutEnd := TSoXPlugin(Source).FFadeoutEnd;
  FFadeoutStartLength := TSoXPlugin(Source).FFadeoutStartLength;
  FFadeoutEndLength := TSoXPlugin(Source).FFadeoutEndLength;
  FSilenceStart := TSoXPlugin(Source).FSilenceStart;
  FSilenceEnd := TSoXPlugin(Source).FSilenceEnd;
  FSilenceStartLength := TSoXPlugin(Source).FSilenceStartLength;
  FSilenceEndLength := TSoXPlugin(Source).FSilenceEndLength;
end;

procedure TSoXPlugin.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean);
var
  F: TfrmConfigureSoX;
begin
  inherited Configure(AOwner, Handle, ShowMessages);

  try
    F := TfrmConfigureSoX.Create(AOwner, FFadeoutStart, FFadeoutEnd, FFadeoutStartLength, FFadeoutEndLength, FSilenceStart, FSilenceEnd,
      FSilenceStartLength, FSilenceEndLength);

    F.ShowModal;

    if F.SaveData then
    begin
      FFadeoutStart := F.FadeoutStart;
      FFadeoutEnd := F.FadeoutEnd;
      FFadeoutStartLength := F.FadeoutStartLength;
      FFadeoutEndLength := F.FadeoutEndLength;
      FSilenceStart := F.SilenceStart;
      FSilenceEnd := F.SilenceEnd;
      FSilenceStartLength := F.SilenceStartLength;
      FSilenceEndLength := F.SilenceEndLength;
      Save;
    end;
  finally
    F.Free;
  end;
end;

function TSoXPlugin.Copy: TPluginBase;
begin
  Result := TSoXPlugin.Create;

  TSoXPlugin(Result).FCopied := True;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
  {
  TSoXPlugin(Result).FFadeoutStart := FFadeoutStart;
  TSoXPlugin(Result).FFadeoutEnd := FFadeoutEnd;
  TSoXPlugin(Result).FFadeoutStartLength := FFadeoutStartLength;
  TSoXPlugin(Result).FFadeoutEndLength := FFadeoutEndLength;
  }
end;

constructor TSoXPlugin.Create;
begin
  inherited;

  FActive := False;
  FOrder := 100;
  FDownloadPackage := 'sox.dll';
  FDownloadName := 'sox';
  FCanConfigure := True;

  FFilesDir := AppGlobals.TempDir + 'sox\';
  FSoXExe := FFilesDir + 'sox.exe';

  FName := _('Apply effects (using SoX)');
  FHelp := _('This applies effects to recorded songs using SoX (Sound eXchange).');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 90, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    AppGlobals.Storage.Read('FadeoutStart_' + ClassName, FFadeoutStart, False, 'Plugins');
    AppGlobals.Storage.Read('FadeoutEnd_' + ClassName, FFadeoutEnd, False, 'Plugins');
    AppGlobals.Storage.Read('FadeoutStartLength_' + ClassName, FFadeoutStartLength, 5, 'Plugins');
    AppGlobals.Storage.Read('FadeoutEndLength_' + ClassName, FFadeoutEndLength, 5, 'Plugins');

    AppGlobals.Storage.Read('SilenceStart_' + ClassName, FSilenceStart, False, 'Plugins');
    AppGlobals.Storage.Read('SilenceEnd_' + ClassName, FSilenceEnd, False, 'Plugins');
    AppGlobals.Storage.Read('SilenceStartLength_' + ClassName, FSilenceStartLength, 5, 'Plugins');
    AppGlobals.Storage.Read('SilenceEndLength_' + ClassName, FSilenceEndLength, 5, 'Plugins');

    if not FGetFilesInstalled then
      FActive := False;
  except end;

  if not FGetReadyForUse then
    ExtractFiles;

  if not FGetReadyForUse then
    FActive := False;
end;

function TSoXPlugin.DeleteFiles: Boolean;
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

destructor TSoXPlugin.Destroy;
begin
  DeleteFiles;

  inherited;
end;

function TSoXPlugin.ExtractFiles: Boolean;
var
  i: Integer;
  H: THandle;
  Res: TResourceStream;
begin
  Result := False;

  ForceDirectories(FFilesDir);
  if FileExists(AppGlobals.Storage.DataDir + FDownloadPackage) then
  begin
    H := LoadLibrary(PChar(AppGlobals.Storage.DataDir + FDownloadPackage));
    if H > 0 then
    begin
      try
        for i := 0 to High(Filenames) do
        begin
          Res := TResourceStream.Create(H, StringReplace(Filenames[i], '.', '_', [rfReplaceAll]), RT_RCDATA);
          try
            Res.SaveToFile(FFilesDir + Filenames[i]);
          finally
            Res.Free;
          end;
        end;
        Result := True;
      except end;
      FreeLibrary(H);
    end;
  end;
end;

function TSoXPlugin.FGetFilesInstalled: Boolean;
begin
  Result := FileExists(AppGlobals.Storage.DataDir + FDownloadPackage);
end;

function TSoXPlugin.FGetReadyForUse: Boolean;
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

procedure TSoXPlugin.Initialize;
begin
  inherited;

  if not FGetReadyForUse then
    ExtractFiles;

  FName := _('Apply effects to MP3s (using SoX)');
  FHelp := _('This applies effects to recorded songs in MP3-format using SoX (Sound eXchange).');
end;

function TSoXPlugin.ProcessFile(
  Data: PPluginProcessInformation): TProcessThreadBase;
begin
  Result := nil;
  if (not FGetReadyForUse) or ((not FFadeoutStart) and (not FFadeoutEnd) and (not FSilenceStart) and (not FSilenceEnd)) then
    Exit;

  Result := TSoXThread.Create(Data, Self);
end;

procedure TSoXPlugin.Save;
begin
  inherited;

  AppGlobals.Storage.Write('FadeoutStart_' + ClassName, FFadeoutStart, 'Plugins');
  AppGlobals.Storage.Write('FadeoutEnd_' + ClassName, FFadeoutEnd, 'Plugins');
  AppGlobals.Storage.Write('FadeoutStartLength_' + ClassName, FFadeoutStartLength, 'Plugins');
  AppGlobals.Storage.Write('FadeoutEndLength_' + ClassName, FFadeoutEndLength, 'Plugins');

  AppGlobals.Storage.Write('SilenceStart_' + ClassName, FSilenceStart, 'Plugins');
  AppGlobals.Storage.Write('SilenceEnd_' + ClassName, FSilenceEnd, 'Plugins');
  AppGlobals.Storage.Write('SilenceStartLength_' + ClassName, FSilenceStartLength, 'Plugins');
  AppGlobals.Storage.Write('SilenceEndLength_' + ClassName, FSilenceEndLength, 'Plugins');
end;

end.