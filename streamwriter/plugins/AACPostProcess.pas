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
unit AACPostProcess;

interface

uses
  Windows, SysUtils, Classes, Plugins, PluginsShared, LanguageObjects,
  Functions, Logging, Math, Mp3FileUtils, ConfigureSetTags;

type
  TAACPostProcessThread = class(TProcessThreadBase)
  private
    FMP4BoxPath: string;
    FAtomicParsleyPath: string;

    function ProcessMP4Box(FromFile: string; var OutFile: string): TActResults;
    function ProcessAtomicParsley(FromFile: string; var OutFile: string): TActResults;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
  end;

  TAACPostProcessPlugin = class(TInternalPlugin)
  private
    FFilesDir: string;
    FCopied: Boolean;

    FArtist: string;
    FTitle: string;
    FAlbum: string;
    FComment: string;

    procedure DeleteFiles;
  protected
    function FGetReadyForUse: Boolean; override;
    function FGetFilesInstalled: Boolean; override;
    function FGetReadyForActivate: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;
    function CanProcess(Data: PPluginProcessInformation): Boolean; override;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;
    procedure Initialize; override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
    procedure Save; override;
    function ExtractFiles: Boolean;
    procedure LoadSharedSettings; override;
  end;

implementation

uses
  AppData, ConfigureSoX;

const
  Filenames: array[0..2] of string = ('atomicparsley.exe', 'js32.dll', 'mp4box.exe');

{ TSoXThread }

constructor TAACPostProcessThread.Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
begin
  inherited Create(Data, Plugin);

  FMP4BoxPath := TAACPostProcessPlugin(Plugin).FFilesDir + 'mp4box.exe';
  FAtomicParsleyPath := TAACPostProcessPlugin(Plugin).FFilesDir + 'atomicparsley.exe';
end;

procedure TAACPostProcessThread.Execute;
var
  TempFile, CmdLine, Params, OutFile, OutFile2, MovedFileName: string;
  Output: AnsiString;
  P: TAACPostProcessPlugin;
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

  OutFile := '';
  OutFile2 := '';

  FResult := ProcessMP4Box(FData.Filename, OutFile);
  case FResult of
    arWin:
      begin
        FResult := ProcessAtomicParsley(OutFile, OutFile2);
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

function TAACPostProcessThread.ProcessMP4Box(FromFile: string; var OutFile: string): TActResults;
var
  TempFile, CmdLine: string;
  Output: AnsiString;
  EC: DWORD;
begin
  Result := arFail;

  TempFile := RemoveFileExt(FromFile) + '_aacpostprocess.m4a';

  CmdLine := '"' + FMP4BoxPath + '" -add "' + FData.Filename + '" "' + TempFile + '" ';

  if RunProcess(CmdLine, ExtractFilePath(FMP4BoxPath), 120000, Output, EC, @Terminated) = 2 then
  begin
    Result := arTimeout;
  end else
  begin
    if FileExists(TempFile) and (EC = 0) then
    begin
      OutFile := TempFile;
      Result := arWin;
    end;
  end;
end;

function TAACPostProcessThread.ProcessAtomicParsley(FromFile: string; var OutFile: string): TActResults;
var
  TempFile, CmdLine, Params: string;
  Artist, Title, Album, Comment: string;
  Output: AnsiString;
  P: TAACPostProcessPlugin;
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
  Arr[0].Replace := FData.Artist;
  Arr[1].C := 't';
  Arr[1].Replace := FData.Title;
  Arr[2].C := 'l';
  Arr[2].Replace := FData.Album;
  Arr[3].C := 's';
  Arr[3].Replace := Trim(FData.Station);
  Arr[4].C := 'u';
  Arr[4].Replace := Trim(FData.StreamTitle);
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
    CmdLine := '"' + FAtomicParsleyPath + '" "' + FromFile + '" --title "' + Title + '" --artist "' + Artist + '"' +
      ' --album ' + '"' + Album + '"' + ' --tracknum ' + '"' + IntToStr(FData.TrackNumber) + '"' + ' --comment "' + Comment + '"';
  end else
  begin
    CmdLine := '"' + FAtomicParsleyPath + '" "' + FromFile + '" --title "' + Title + '"' +
      ' --album ' + '"' + Album + '"' + ' --tracknum ' + '"' + IntToStr(FData.TrackNumber) + '"' + ' --comment "' + Comment + '"';
  end;

  if RunProcess(CmdLine, ExtractFilePath(FAtomicParsleyPath), 120000, Output, EC, @Terminated) = 2 then
  begin
    Result := arTimeout;
  end else
  begin
    Files := TStringList.Create;
    try
      FindFiles(RemoveFileExt(FromFile) + '-temp-*.m4a', Files);
      if Files.Count > 0 then
        TempFile := IncludeTrailingBackslash(ExtractFilePath(FromFile)) + Files[0]
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

{ TSoXPlugin }

procedure TAACPostProcessPlugin.Assign(Source: TPluginBase);
begin
  inherited;

  FArtist := TAACPostProcessPlugin(Source).FArtist;
  FTitle := TAACPostProcessPlugin(Source).FTitle;
  FAlbum := TAACPostProcessPlugin(Source).FAlbum;
  FComment := TAACPostProcessPlugin(Source).FComment;
end;

function TAACPostProcessPlugin.CanProcess(Data: PPluginProcessInformation): Boolean;
begin

end;

function TAACPostProcessPlugin.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
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

function TAACPostProcessPlugin.Copy: TPluginBase;
begin
  Result := TAACPostProcessPlugin.Create;

  TAACPostProcessPlugin(Result).FCopied := True;



  Result.Assign(Self);
end;

constructor TAACPostProcessPlugin.Create;
begin
  inherited;

  FActive := False;
  FOrder := 100;
  FDownloadPackage := 'aacpostprocess.dll';
  FDownloadName := 'aacpostprocess';
  FCanConfigure := True;

  FFilesDir := AppGlobals.TempDir + 'aacpostprocess\';

  FName := _('AAC - Convert to M4A and set tags');
  FHelp := _('This plugin converts recorded songs from AAC to M4A an sets tags (AAC only).');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    LoadSharedSettings;

    if not FGetFilesInstalled then
      FActive := False;
  except end;

  if not FGetReadyForActivate then
    ExtractFiles;

  if not FGetReadyForActivate then
    FActive := False;
end;

procedure TAACPostProcessPlugin.DeleteFiles;
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

destructor TAACPostProcessPlugin.Destroy;
begin
  DeleteFiles;

  inherited;
end;

function TAACPostProcessPlugin.ExtractFiles: Boolean;
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
        //if (Filenames[i] <> 'lame-enc.dll') and (Filenames[i] <> 'libmad.dll') then
        //begin
          Res := TResourceStream.Create(H, StringReplace(Filenames[i], '.', '_', [rfReplaceAll]), RT_RCDATA);
          try
            Res.SaveToFile(FFilesDir + Filenames[i]);
          finally
            Res.Free;
          end;
        //end;
        except end;
      end;
      Result := True;
      FreeLibrary(H);
    end else
      Windows.DeleteFile(PChar(AppGlobals.Storage.DataDir + FDownloadPackage));
  end;
end;

function TAACPostProcessPlugin.FGetFilesInstalled: Boolean;
begin
  Result := FileExists(AppGlobals.Storage.DataDir + FDownloadPackage);
end;

function TAACPostProcessPlugin.FGetReadyForActivate: Boolean;
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

function TAACPostProcessPlugin.FGetReadyForUse: Boolean;
begin
  Result := FGetReadyForActivate;
end;

procedure TAACPostProcessPlugin.Initialize;
begin
  inherited;

  if (not FGetReadyForUse) or (not FGetReadyForActivate) then
    ExtractFiles;

  FName := _('AAC - Convert to M4A and set tags');
  FHelp := _('This plugin converts recorded songs from AAC to M4A an sets tags (AAC only).');
end;

procedure TAACPostProcessPlugin.LoadSharedSettings;
begin
  inherited;

  AppGlobals.Storage.Read('Shared_Tags_Artist', FArtist, '%a', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Title', FTitle, '%t', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Album', FAlbum, '%l', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Comment', FComment, '%s / %u / Recorded using streamWriter', 'Plugins');
end;

function TAACPostProcessPlugin.ProcessFile(
  Data: PPluginProcessInformation): TProcessThreadBase;
begin
  Result := nil;
  if not CanProcess(Data) then
    Exit;

  Result := TAACPostProcessThread.Create(Data, Self);
end;

procedure TAACPostProcessPlugin.Save;
begin
  inherited;

  AppGlobals.Storage.Write('Shared_Tags_Artist', FArtist, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Title', FTitle, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Album', FAlbum, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Comment', FComment, 'Plugins');
end;

function TAACPostProcessPlugin.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
  { TODO: !!!
  Result := MsgBox(Handle, _('WARNING:'#13#10'It is not be allowed in some contries to use this plugin because it contains libmad.dll ' +
                             'and lame_enc.dll that make use of some patented technologies. Please make sure you may use these files in your country. ' +
                             'If you are sure you may use these files, press "Yes" to continue.'), _('Warning'), MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = IDYES;
  }
end;

end.
