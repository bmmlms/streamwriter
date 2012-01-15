{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
  Windows, SysUtils, Classes, PostProcess, LanguageObjects, TypeDefs,
  Functions, Logging, Math, ConfigureSetTags, PluginMP4Box;

type
  TPostProcessMP4BoxThread = class(TPostProcessThreadBase)
  private
    FMP4BoxPath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
  end;

  TPostProcessMP4Box = class(TInternalPostProcess)
  private
    FArtist: string;
    FTitle: string;
    FAlbum: string;
    FComment: string;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;
    function CanProcess(Data: PPluginProcessInformation): Boolean; override;
    function ProcessFile(Data: PPluginProcessInformation): TPostProcessThreadBase; override;
    function Copy: TPostProcessBase; override;
    procedure Assign(Source: TPostProcessBase); override;
    procedure Initialize; override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
    procedure Save; override;
    procedure LoadSharedSettings; override;

    function MP4BoxMux(MP4BoxPath, InFile, OutFile: string; TerminateFlag: PBoolean): TActResults;
    function MP4BoxDemux(MP4BoxPath, InFile, OutFile: string; TerminateFlag: PBoolean): TActResults;
  end;

implementation

uses
  AppData, ConfigureSoX;

{ TPostProcessMP4BoxThread }

constructor TPostProcessMP4BoxThread.Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
begin
  inherited Create(Data, Plugin);

  FMP4BoxPath := TPluginMP4Box(AppGlobals.PluginManager.Find(TPluginMP4Box)).MP4BoxEXEPath;
end;

procedure TPostProcessMP4BoxThread.Execute;
var
  TempFile, CmdLine, Params, OutFile, MovedFileName: string;
  Output: AnsiString;
  P: TPostProcessMP4Box;
  LoopStarted: Cardinal;
  Failed: Boolean;
  FS: TFileStream;
  EC: DWORD;
begin
  inherited;

  if LowerCase(ExtractFileExt(FData.Filename)) <> '.aac' then
  begin
    FResult := arImpossible;
    Exit;
  end;

  OutFile := RemoveFileExt(FData.Filename) + '_mux.m4a';
  FResult := TPostProcessMP4Box(Plugin).MP4BoxMux(FMP4BoxPath, FData.Filename, OutFile, @Terminated);
  case FResult of
    arWin:
      begin
        MovedFileName := RemoveFileExt(FData.Filename) + '.m4a';
        if MoveFile(PChar(OutFile), PChar(MovedFileName)) then
        begin
          DeleteFile(FData.Filename);
          FData.Filename := MovedFileName;
          FData.Filesize := GetFileSize(MovedFileName);
        end;
      end;
    arFail:;
    arTimeOut:;
    arImpossible:;
  end;

  DeleteFile(OutFile);
end;

{ TPostProcessMP4Box }

procedure TPostProcessMP4Box.Assign(Source: TPostProcessBase);
begin
  inherited;

  FArtist := TPostProcessMP4Box(Source).FArtist;
  FTitle := TPostProcessMP4Box(Source).FTitle;
  FAlbum := TPostProcessMP4Box(Source).FAlbum;
  FComment := TPostProcessMP4Box(Source).FComment;
end;

function TPostProcessMP4Box.CanProcess(Data: PPluginProcessInformation): Boolean;
begin
  Result := (Data.OutputFormat = atAAC) and FGetDependenciesMet;
end;

function TPostProcessMP4Box.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
var
  F: TfrmConfigureSetTags;
begin
  Result := True;

  F := TfrmConfigureSetTags.Create(AOwner, Self, FArtist, FTitle, FAlbum, FComment);
  try
    F.ShowModal;

    if F.SaveData then
    begin
      FArtist := F.Artist;
      FTitle := F.Title;
      FAlbum := F.Album;
      FComment := F.Comment;

      Save;
    end;
  finally
    F.Free;
  end;
end;

function TPostProcessMP4Box.Copy: TPostProcessBase;
begin
  Result := TPostProcessMP4Box.Create;

  TPostProcessMP4Box(Result).FCopied := True;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

constructor TPostProcessMP4Box.Create;
begin
  inherited;

  FNeededPlugins.Add(TPluginMP4Box);

  FActive := False;
  FOrder := 100;
  FCanConfigure := True;
  FGroupID := 1;

  FName := _('AAC - Convert to M4A');
  FHelp := _('This postprocessor converts recorded songs from AAC to M4A (AAC only).');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, False, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    LoadSharedSettings;

    if not FGetDependenciesMet then
      FActive := False;
  except end;
end;

function TPostProcessMP4Box.MP4BoxDemux(MP4BoxPath, InFile, OutFile: string; TerminateFlag: PBoolean): TActResults;
var
  CmdLine: string;
  Output: AnsiString;
  EC: DWORD;
begin
  Result := arFail;

  // TODO: Unschön. der sollte das nicht selber bestimmen. ist hier drunter nochmal!
  if MP4BoxPath = '' then
  begin
    MP4BoxPath := AppGlobals.TempDir + 'plugin_mp4box\mp4box.exe';
  end;

  CmdLine := '"' + MP4BoxPath + '" -raw 1 "' + InFile + '" -out "' + OutFile + '"';

  if RunProcess(CmdLine, ExtractFilePath(MP4BoxPath), 120000, Output, EC, TerminateFlag) = 2 then
  begin
    Result := arTimeout;
  end else
  begin
    if FileExists(OutFile) and (EC = 0) then
    begin
      Result := arWin;
    end;
  end;
end;

function TPostProcessMP4Box.MP4BoxMux(MP4BoxPath, InFile, OutFile: string; TerminateFlag: PBoolean): TActResults;
var
  CmdLine: string;
  Output: AnsiString;
  EC: DWORD;
begin
  Result := arFail;

  if MP4BoxPath = '' then
  begin
    MP4BoxPath := AppGlobals.TempDir + 'plugin_mp4box\mp4box.exe';
  end;

  CmdLine := '"' + MP4BoxPath + '" -add "' + InFile + '" "' + OutFile + '"';

  if RunProcess(CmdLine, ExtractFilePath(MP4BoxPath), 120000, Output, EC, TerminateFlag) = 2 then
  begin
    Result := arTimeout;
  end else
  begin
    if FileExists(OutFile) and (EC = 0) then
    begin
      Result := arWin;
    end;
  end;
end;

destructor TPostProcessMP4Box.Destroy;
begin

  inherited;
end;

procedure TPostProcessMP4Box.Initialize;
begin
  inherited;

  FName := _('AAC - Convert to M4A');
  FHelp := _('This postprocessor converts recorded songs from AAC to M4A (AAC only).');
end;

procedure TPostProcessMP4Box.LoadSharedSettings;
begin
  inherited;

  AppGlobals.Storage.Read('Shared_Tags_Artist', FArtist, '%a', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Title', FTitle, '%t', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Album', FAlbum, '%l', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Comment', FComment, '%s / %u / Recorded using streamWriter', 'Plugins');
end;

function TPostProcessMP4Box.ProcessFile(
  Data: PPluginProcessInformation): TPostProcessThreadBase;
begin
  Result := nil;
  if not CanProcess(Data) then
    Exit;

  Result := TPostProcessMP4BoxThread.Create(Data, Self);
end;

procedure TPostProcessMP4Box.Save;
begin
  inherited;

  AppGlobals.Storage.Write('Shared_Tags_Artist', FArtist, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Title', FTitle, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Album', FAlbum, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Comment', FComment, 'Plugins');
end;

function TPostProcessMP4Box.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := inherited;
end;

end.
