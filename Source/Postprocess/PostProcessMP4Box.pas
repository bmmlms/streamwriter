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

unit PostProcessMP4Box;

interface

uses
  AddonMP4Box,
  AudioFunctions,
  Classes,
  DataManager,
  Functions,
  Generics.Collections,
  LanguageObjects,
  Logging,
  PostProcess,
  SysUtils;

type
  TPostProcessMP4BoxThread = class(TPostProcessThreadBase)
  protected
    procedure Execute; override;
  end;

  TPostProcessMP4Box = class(TInternalPostProcess)
  private
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostprocessBase>): Boolean; override;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; override;
    function Copy: TPostProcessBase; override;

    function MP4BoxMux(InFile, OutFile: string; TerminateFlag: PByteBool): TActResults;
  end;

implementation

uses
  AppData,
  ConfigureSoX;

{ TPostProcessMP4BoxThread }

procedure TPostProcessMP4BoxThread.Execute;
var
  OutFile, MovedFileName: string;
  FileSize: Int64;
begin
  inherited;

  if LowerCase(ExtractFileExt(FData.Filename)) <> '.aac' then
  begin
    FResult := arImpossible;
    Exit;
  end;

  OutFile := TFunctions.RemoveFileExt(FData.Filename) + '_mux.m4a';
  FResult := TPostProcessMP4Box(PostProcessor).MP4BoxMux(FData.Filename, OutFile, @Terminated);
  case FResult of
    arWin:
    begin
      MovedFileName := TFunctions.RemoveFileExt(FData.Filename) + '.m4a';
      if RenameFile(OutFile, MovedFileName) then
      begin
        DeleteFile(FData.Filename);
        FData.Filename := MovedFileName;
        if TFunctions.GetFileSize(MovedFileName, FileSize) then
          FData.Filesize := FileSize
        else
          FData.Filesize := -1;
      end;
    end;
    arFail: ;
    arTimeOut: ;
    arImpossible: ;
  end;

  DeleteFile(OutFile);
end;

{ TPostProcessMP4Box }

function TPostProcessMP4Box.CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostprocessBase>): Boolean;
var
  OutputFormat: TAudioTypes;
begin
  OutputFormat := TEncoderSettings(Data.EncoderSettings).AudioType;
  if OutputFormat = atNone then
    OutputFormat := FilenameToFormat(Data.Filename);

  Result := (OutputFormat = atAAC) and FGetDependenciesMet;
end;

function TPostProcessMP4Box.Copy: TPostProcessBase;
begin
  Result := TPostProcessMP4Box.Create;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

constructor TPostProcessMP4Box.Create;
begin
  inherited;

  FNeededAddons += [TAddonMP4Box];

  FCanConfigure := False;
  FGroupID := 1;
  FOrder := 1001;

  FPostProcessType := ptMP4Box;
end;

function TPostProcessMP4Box.MP4BoxMux(InFile, OutFile: string; TerminateFlag: PByteBool): TActResults;
var
  CmdLine, MP4BoxPath: string;
  Output: AnsiString;
  EC: DWORD;
begin
  Result := arFail;

  MP4BoxPath := AppGlobals.AddonManager.Find(TAddonMP4Box).ModuleFilePath;

  CmdLine := '"' + MP4BoxPath + '" -add "' + InFile + '" "' + OutFile + '"';

  case TFunctions.RunProcess(CmdLine, ExtractFilePath(MP4BoxPath), 300000, Output, EC, TerminateFlag, True) of
    rpWin:
      if FileExists(OutFile) and (EC = 0) then
        Result := arWin;
    rpTimeout:
      Result := arTimeout;
  end;

  if Result <> arWin then
    DeleteFile(OutFile);
end;

function TPostProcessMP4Box.FGetHelp: string;
begin
  Result := _('This postprocessor converts recorded songs from AAC to M4A.');
end;

function TPostProcessMP4Box.FGetName: string;
begin
  Result := _('Convert AAC to M4A');
end;

function TPostProcessMP4Box.ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase;
begin
  Result := TPostProcessMP4BoxThread.Create(Data, Self);
end;

end.
