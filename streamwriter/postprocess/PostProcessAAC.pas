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
unit PostProcessAAC;

interface

uses
  Windows, SysUtils, Classes, PostProcess, LanguageObjects,
  Functions, Logging, Math, Mp3FileUtils, ConfigureSetTags;

type
  TPostProcessAACThread = class(TPostProcessThreadBase)
  private
    FMP4BoxPath: string;
    FAtomicParsleyPath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
  end;

  TPostProcessAAC = class(TInternalPostProcess)
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
    function MP4AtomicParsley(AtomicParsleyPath, InFile: string; var OutFile: string; TerminateFlag: PBoolean; Data: PPluginProcessInformation): TActResults;
  end;

implementation

uses
  AppData, ConfigureSoX;

{ TPostProcessAACThread }

constructor TPostProcessAACThread.Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
begin
  inherited Create(Data, Plugin);

  //FMP4BoxPath := TPostProcessAAC(Plugin).MP4BoxFilename;
  //FAtomicParsleyPath := TPostProcessAAC(Plugin).AtomicParsleyFilename;
end;

procedure TPostProcessAACThread.Execute;
var
  TempFile, CmdLine, Params, OutFile, OutFile2, MovedFileName: string;
  Output: AnsiString;
  P: TPostProcessAAC;
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
  FResult := TPostProcessAAC(Plugin).MP4BoxMux(FMP4BoxPath, FData.Filename, OutFile, @Terminated);
  case FResult of
    arWin:
      begin
        FResult := TPostProcessAAC(Plugin).MP4AtomicParsley(FAtomicParsleyPath, OutFile, OutFile2, @Terminated, FData);
        case FResult of
          arWin:
            begin
              MovedFileName := RemoveFileExt(FData.Filename) + '.m4a';
              if MoveFile(PChar(OutFile2), PChar(MovedFileName)) then
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
      end;
    arFail:;
    arTimeOut:;
    arImpossible:;
  end;

  DeleteFile(OutFile);
  DeleteFile(OutFile2);
end;

{ TPostProcessAAC }

procedure TPostProcessAAC.Assign(Source: TPostProcessBase);
begin
  inherited;

  FArtist := TPostProcessAAC(Source).FArtist;
  FTitle := TPostProcessAAC(Source).FTitle;
  FAlbum := TPostProcessAAC(Source).FAlbum;
  FComment := TPostProcessAAC(Source).FComment;
end;

function TPostProcessAAC.CanProcess(Data: PPluginProcessInformation): Boolean;
begin

end;

function TPostProcessAAC.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
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

function TPostProcessAAC.Copy: TPostProcessBase;
begin
  Result := TPostProcessAAC.Create;

  TPostProcessAAC(Result).FCopied := True;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

constructor TPostProcessAAC.Create;
begin
  inherited;

  FActive := False;
  FOrder := 100;
  FCanConfigure := True;
  FGroupID := 1;

  FName := _('AAC - Convert to M4A and set tags');
  FHelp := _('This plugin converts recorded songs from AAC to M4A an sets tags (AAC only).');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    LoadSharedSettings;

    if not FGetDependenciesMet then
      FActive := False;
  except end;
end;

function TPostProcessAAC.MP4AtomicParsley(AtomicParsleyPath, InFile: string;
  var OutFile: string; TerminateFlag: PBoolean; Data: PPluginProcessInformation): TActResults;
var
  CmdLine, TempFile, Params: string;
  Artist, Title, Album, Comment: string;
  Output: AnsiString;
  P: TPostProcessAAC;
  LoopStarted: Cardinal;
  Failed: Boolean;
  FS: TFileStream;
  EC: DWORD;
  Files: TStringList;
  Arr: TPatternReplaceArray;
begin
  Result := arFail;

  AppGlobals.Storage.Read('Shared_Tags_Artist', Artist, '%a', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Title', Title, '%t', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Album', Album, '%l', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Comment', Comment, '%s / %u / Recorded using streamWriter', 'Plugins');

  SetLength(Arr, 7);
  Arr[0].C := 'a';
  Arr[0].Replace := Data.Artist;
  Arr[1].C := 't';
  Arr[1].Replace := Data.Title;
  Arr[2].C := 'l';
  Arr[2].Replace := Data.Album;
  Arr[3].C := 's';
  Arr[3].Replace := Trim(Data.Station);
  Arr[4].C := 'u';
  Arr[4].Replace := Trim(Data.StreamTitle);
  Arr[5].C := 'd';
  Arr[5].Replace := FormatDateTime('dd.mm.yy', Now);
  Arr[6].C := 'i';
  Arr[6].Replace := FormatDateTime('hh.nn.ss', Now);

  Artist := PatternReplace(Artist, Arr);
  Title := PatternReplace(Title, Arr);
  Album := PatternReplace(Album, Arr);
  Comment := PatternReplace(Comment, Arr);

  if (Trim(Artist) <> '') and (Trim(Title) <> '') then
  begin
    CmdLine := '"' + AtomicParsleyPath + '" "' + InFile + '" --title "' + Title + '" --artist "' + Artist + '"' +
      ' --album ' + '"' + Album + '"' + ' --tracknum ' + '"' + IntToStr(Data.TrackNumber) + '"' + ' --comment "' + Comment + '"';
  end else
  begin
    CmdLine := '"' + AtomicParsleyPath + '" "' + InFile + '" --title "' + Title + '"' +
      ' --album ' + '"' + Album + '"' + ' --tracknum ' + '"' + IntToStr(Data.TrackNumber) + '"' + ' --comment "' + Comment + '"';
  end;

  if RunProcess(CmdLine, ExtractFilePath(AtomicParsleyPath), 120000, Output, EC, TerminateFlag) = 2 then
  begin
    Result := arTimeout;
  end else
  begin
    Files := TStringList.Create;
    try
      FindFiles(RemoveFileExt(InFile) + '-temp-*.m4a', Files);
      if Files.Count > 0 then
        TempFile := IncludeTrailingBackslash(ExtractFilePath(InFile)) + Files[0]
      else
        Exit;
    finally
      Files.Free;
    end;

    if FileExists(TempFile) and (EC = 0) then
    begin
      Result := arWin;
      OutFile := TempFile;
    end;
  end;
end;

function TPostProcessAAC.MP4BoxDemux(MP4BoxPath, InFile, OutFile: string; TerminateFlag: PBoolean): TActResults;
var
  CmdLine: string;
  Output: AnsiString;
  EC: DWORD;
begin
  Result := arFail;

  if MP4BoxPath = '' then
  begin
    MP4BoxPath := AppGlobals.TempDir + 'aacpostprocess\mp4box.exe';
  end;

  CmdLine := '"' + MP4BoxPath + '" -raw 1 "' + InFile + '" -out "' + OutFile + '"';

  if RunProcess(CmdLine, ExtractFilePath(MP4BoxPath), 120000, Output, EC, TerminateFlag) = 2 then
  begin
    Result := arTimeout;
  end else
  begin
    if FileExists(OutFile) and (EC = 0) then
    begin
      OutFile := OutFile;
      Result := arWin;
    end;
  end;
end;

function TPostProcessAAC.MP4BoxMux(MP4BoxPath, InFile, OutFile: string; TerminateFlag: PBoolean): TActResults;
var
  CmdLine: string;
  Output: AnsiString;
  EC: DWORD;
begin
  Result := arFail;

  if MP4BoxPath = '' then
  begin
    MP4BoxPath := AppGlobals.TempDir + 'aacpostprocess\mp4box.exe';
  end;

  CmdLine := '"' + MP4BoxPath + '" -add "' + InFile + '" "' + OutFile + '"';

  if RunProcess(CmdLine, ExtractFilePath(MP4BoxPath), 120000, Output, EC, TerminateFlag) = 2 then
  begin
    Result := arTimeout;
  end else
  begin
    if FileExists(OutFile) and (EC = 0) then
    begin
      OutFile := OutFile;
      Result := arWin;
    end;
  end;
end;

destructor TPostProcessAAC.Destroy;
begin

  inherited;
end;

procedure TPostProcessAAC.Initialize;
begin
  inherited;

  FName := _('AAC - Convert to M4A and set tags');
  FHelp := _('This plugin converts recorded songs from AAC to M4A an sets tags (AAC only).');
end;

procedure TPostProcessAAC.LoadSharedSettings;
begin
  inherited;

  AppGlobals.Storage.Read('Shared_Tags_Artist', FArtist, '%a', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Title', FTitle, '%t', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Album', FAlbum, '%l', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Comment', FComment, '%s / %u / Recorded using streamWriter', 'Plugins');
end;

function TPostProcessAAC.ProcessFile(
  Data: PPluginProcessInformation): TPostProcessThreadBase;
begin
  Result := nil;
  if not CanProcess(Data) then
    Exit;

  Result := TPostProcessAACThread.Create(Data, Self);
end;

procedure TPostProcessAAC.Save;
begin
  inherited;

  AppGlobals.Storage.Write('Shared_Tags_Artist', FArtist, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Title', FTitle, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Album', FAlbum, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Comment', FComment, 'Plugins');
end;

function TPostProcessAAC.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := inherited;
end;

end.
