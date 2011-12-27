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
  Functions, Logging, Math, Mp3FileUtils;

type
  TAACPostProcessThread = class(TProcessThreadBase)
  private
    FMP4BoxPath: string;
    FAtomicParsleyPath: string;

    function ProcessAtomicParsley(FromFile: string; var Outfile: string): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
  end;

  TAACPostProcessPlugin = class(TInternalPlugin)
  private
    FFilesDir: string;
    FCopied: Boolean;

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
            // TODO: streamwriter kommt nicht damit klar, wenn sich die dateierweiterung ändert. er sagt dann "plugin hat datei gelöscht"...
procedure TAACPostProcessThread.Execute;
var
  TempFile, CmdLine, Params, Outfile: string;
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

  FResult := arFail;

  TempFile := RemoveFileExt(FData.Filename) + '_aacpostprocess.m4a';

  P := TAACPostProcessPlugin(Plugin);

  CmdLine := '"' + FMP4BoxPath + '" -add "' + FData.Filename + '" "' + TempFile + '" ';

  Params := '123';

  if Params <> '' then
  begin
    if RunProcess(CmdLine, ExtractFilePath(FMP4BoxPath), 120000, Output, EC, @Terminated) = 2 then
    begin
      FResult := arTimeout;
    end else
    begin
      Failed := True;
      if FileExists(TempFile) and (EC = 0) then
      begin
        LoopStarted := GetTickCount;
        while Failed do
        begin
          if FileExists(TempFile) then
          begin
            // TODO: Was ist result, wenn das hier fehlschlägt??
            Failed := not ProcessAtomicParsley(RemoveFileExt(TempFile) + '.m4a', Outfile);
            if not Failed then
              FData.Filename := Outfile;
          end;
        end;

        if not Failed then
        begin
          FResult := arWin;
        end;
      end;
      DeleteFile(PChar(TempFile));
    end;
  end;
end;

function TAACPostProcessThread.ProcessAtomicParsley(FromFile: string; var Outfile: string): Boolean;
var
  TempFile, CmdLine, Params: string;
  Output: AnsiString;
  P: TAACPostProcessPlugin;
  LoopStarted: Cardinal;
  Failed: Boolean;
  FS: TFileStream;
  EC: DWORD;
  Files: TStringList;
begin
  inherited;

  Outfile := '';

  Result := False;

  CmdLine := '"' + FAtomicParsleyPath + '" "' + FromFile + '" --title "' + FData.Title + '" --artist "' + FData.Artist + '"' +
    ' --album ' + '"' + FData.Album + '"' + ' --tracknum ' + '"' + IntToStr(FData.TrackNumber) + '"' + ' --comment ' + '"TODO: !!!"';

  Params := '123';

  if Params <> '' then
  begin
    if RunProcess(CmdLine, ExtractFilePath(FAtomicParsleyPath), 120000, Output, EC, @Terminated) = 2 then
    begin

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

      Failed := True;
      if FileExists(TempFile) and (EC = 0) then
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
          if not MoveFile(PChar(TempFile), PChar(RemoveFileExt(FData.Filename) + '.m4a')) then
            Failed := True;

        if not Failed then
        begin
          Outfile := RemoveFileExt(FData.Filename) + '.m4a';
          FData.Filesize := GetFileSize(Outfile);
        end;
      end;
      DeleteFile(PChar(TempFile));

      Result := not Failed;
    end;
  end;
end;

{ TSoXPlugin }

procedure TAACPostProcessPlugin.Assign(Source: TPluginBase);
begin
  inherited;

end;

function TAACPostProcessPlugin.CanProcess(Data: PPluginProcessInformation): Boolean;
begin

end;

function TAACPostProcessPlugin.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
begin

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
  FHelp := _('This plugin converts recorded songs from AAC to M4A an sets tags.');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');



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
    if (Filenames[i] <> 'atomicparsley.exe') and (Filenames[i] <> 'js32.dll') and (Filenames[i] <> 'mp4box.exe') then
      if not FileExists(FFilesDir + Filenames[i]) then
      begin
        Result := False;
        Break;
      end;
end;

function TAACPostProcessPlugin.FGetReadyForUse: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(Filenames) do
    //if (Filenames[i] <> 'lame-enc.dll') and (Filenames[i] <> 'libmad.dll') then
    if not FileExists(FFilesDir + Filenames[i]) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TAACPostProcessPlugin.Initialize;
begin
  inherited;

  if (not FGetReadyForUse) or (not FGetReadyForActivate) then
    ExtractFiles;

  FName := _('AAC - Convert to M4A and set tags');
  FHelp := _('This plugin converts recorded songs from AAC to M4A an sets tags.');
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
