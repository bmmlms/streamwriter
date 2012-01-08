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
unit PostProcessSoX;

interface

uses
  Windows, SysUtils, Classes, PostProcess, LanguageObjects,
  Functions, Logging, Math, PluginBase, Mp3FileUtils, PluginSoX;

type
  TPostProcessSoxThread = class(TProcessThreadBase)
  private
    FSoxPath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
  end;

  TPostProcessSoX = class(TInternalPostProcess)
  private
    FSoXExe: string;

    FNormalize: Boolean;
    FFadeoutStart: Boolean;
    FFadeoutEnd: Boolean;
    FFadeoutStartLength: Integer;
    FFadeoutEndLength: Integer;
    FSilenceStart: Boolean;
    FSilenceEnd: Boolean;
    FSilenceStartLength: Integer;
    FSilenceEndLength: Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;
    function CanProcess(Data: PPluginProcessInformation): Boolean; override;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPostProcessBase; override;
    procedure Assign(Source: TPostProcessBase); override;
    procedure Initialize; override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
    procedure Save; override;
    //function EatFiles(LameFile, MADFile: string): Boolean;
    property SoXExe: string read FSoXExe;
  end;

implementation

uses
  AppData, ConfigureSoX, PluginLame;

{ TSoXThread }

constructor TPostProcessSoxThread.Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
begin
  inherited Create(Data, Plugin);

  FSoxPath := TPluginSoX(AppGlobals.PluginManager.Find(TPluginSoX)).EXEPath;
end;

procedure TPostProcessSoxThread.Execute;
var
  TempFile, CmdLine, Params: string;
  Output: AnsiString;
  P: TPostProcessSoX;
  LoopStarted: Cardinal;
  Failed: Boolean;
  FS: TFileStream;
  EC: DWORD;

  ID3V1: TID3v1Tag;
  ID3V2: TID3v2Tag;
begin
  inherited;

  if LowerCase(ExtractFileExt(FData.Filename)) <> '.mp3' then
  begin
    FResult := arImpossible;
    Exit;
  end;

  FResult := arFail;

  TempFile := RemoveFileExt(FData.Filename) + '_soxconvert' + ExtractFileExt(FData.Filename);

  P := TPostProcessSoX(Plugin);

  CmdLine := '"' + FSoxPath + '" --norm "' + FData.Filename + '" "' + TempFile + '" ';

  Params := '';

  if P.FNormalize then
    Params := Params + 'gain -b -n';

  if P.FFadeoutStart and P.FFadeoutEnd then
    Params := ' fade p ' + IntToStr(P.FFadeoutStartLength) + ' ' + IntToStr(FData.Length) + ' ' + IntToStr(P.FFadeoutEndLength)
  else if P.FFadeoutStart then
    Params := ' fade p ' + IntToStr(P.FFadeoutStartLength)
  else if P.FFadeoutEnd then
    Params := ' fade p 0 ' + IntToStr(FData.Length) + ' ' + IntToStr(P.FFadeoutEndLength);

  if P.FSilenceStart and P.FSilenceEnd then
    Params := Params + ' pad ' + IntToStr(P.FSilenceStartLength) + ' ' + IntToStr(P.FSilenceEndLength)
  else if P.FSilenceStart then
    Params := Params + ' pad ' + IntToStr(P.FSilenceStartLength)
  else if P.FSilenceEnd then
    Params := Params + ' pad 0 ' + IntToStr(P.FSilenceEndLength);

  if Params <> '' then
  begin
    ID3V1 := TID3v1Tag.Create;
    ID3V2 := TID3v2Tag.Create;

    try
      ID3V1.ReadFromFile(FData.Filename);
      ID3V2.ReadFromFile(FData.Filename);

      if RunProcess(CmdLine + Params, ExtractFilePath(FSoxPath), 120000, Output, EC, @Terminated) = 2 then
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
              Failed := True
            else
            begin
              try
                ID3V1.WriteToFile(TempFile);
                ID3V2.WriteToFile(TempFile);
              except end;
            end;

          if not Failed then
          begin
            FData.Filesize := GetFileSize(FData.Filename);

            // Okay, das hier ist nicht ordentlich, aber sollte passen...
            if P.FSilenceStart then
              FData.Length := FData.Length + P.FSilenceStartLength;
            if P.FSilenceEnd then
              FData.Length := FData.Length + P.FSilenceEndLength;

            FResult := arWin;
          end;
        end;
        DeleteFile(PChar(TempFile));
      end;
    finally
      ID3V1.Free;
      ID3V2.Free;
    end;
  end;
end;

{ TSoXPlugin }

procedure TPostProcessSoX.Assign(Source: TPostProcessBase);
begin
  inherited;

  FNormalize := TPostProcessSoX(Source).FNormalize;
  FFadeoutStart := TPostProcessSoX(Source).FFadeoutStart;
  FFadeoutEnd := TPostProcessSoX(Source).FFadeoutEnd;
  FFadeoutStartLength := TPostProcessSoX(Source).FFadeoutStartLength;
  FFadeoutEndLength := TPostProcessSoX(Source).FFadeoutEndLength;
  FSilenceStart := TPostProcessSoX(Source).FSilenceStart;
  FSilenceEnd := TPostProcessSoX(Source).FSilenceEnd;
  FSilenceStartLength := TPostProcessSoX(Source).FSilenceStartLength;
  FSilenceEndLength := TPostProcessSoX(Source).FSilenceEndLength;
end;

function TPostProcessSoX.CanProcess(Data: PPluginProcessInformation): Boolean;
begin
  Result := FGetDependenciesMet and (FNormalize or FFadeoutStart or
    FFadeoutEnd or FSilenceStart or FSilenceEnd);
end;

function TPostProcessSoX.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
var
  F: TfrmConfigureSoX;
begin
  Result := True;

  F := TfrmConfigureSoX.Create(AOwner, Self, FNormalize, FFadeoutStart, FFadeoutEnd,
    FFadeoutStartLength, FFadeoutEndLength, FSilenceStart, FSilenceEnd, FSilenceStartLength,
    FSilenceEndLength, 0);
  try
    F.ShowModal;

    if F.SaveData then
    begin
      FNormalize := F.Normalize;
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

function TPostProcessSoX.Copy: TPostProcessBase;
begin
  Result := TPostProcessSoX.Create;

  TPostProcessSoX(Result).FCopied := True;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

constructor TPostProcessSoX.Create;
begin
  inherited;

  FNeededPlugins.Add(TPluginSoX);
  FNeededPlugins.Add(TPluginLAME);
  // FNeededPlugins.Add(TPluginAAC); TODO:

  FActive := False;
  FOrder := 100;
  FCanConfigure := True;

  FName := _('Apply effects using SoX');
  FHelp := _('This plugin applies effects to recorded songs using Sound eXchange (SoX).');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 90, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    AppGlobals.Storage.Read('Normalize_' + ClassName, FNormalize, False, 'Plugins');

    AppGlobals.Storage.Read('FadeoutStart_' + ClassName, FFadeoutStart, False, 'Plugins');
    AppGlobals.Storage.Read('FadeoutEnd_' + ClassName, FFadeoutEnd, False, 'Plugins');
    AppGlobals.Storage.Read('FadeoutStartLength_' + ClassName, FFadeoutStartLength, 5, 'Plugins');
    AppGlobals.Storage.Read('FadeoutEndLength_' + ClassName, FFadeoutEndLength, 5, 'Plugins');

    AppGlobals.Storage.Read('SilenceStart_' + ClassName, FSilenceStart, False, 'Plugins');
    AppGlobals.Storage.Read('SilenceEnd_' + ClassName, FSilenceEnd, False, 'Plugins');
    AppGlobals.Storage.Read('SilenceStartLength_' + ClassName, FSilenceStartLength, 5, 'Plugins');
    AppGlobals.Storage.Read('SilenceEndLength_' + ClassName, FSilenceEndLength, 5, 'Plugins');

    if not FGetDependenciesMet then
      FActive := False;
  except end;
end;

destructor TPostProcessSoX.Destroy;
begin

  inherited;
end;

{
function TPostProcessSox.EatFiles(LameFile, MADFile: string): Boolean;
var
  Lame, MAD: Boolean;
  H: Cardinal;
  MS1, MS2: TMemoryStream;
begin
  Result := False;
  Lame := False;
  MAD := False;

  if FileExists(AppGlobals.Storage.DataDir + FDownloadPackage) then
  begin
    MS1 := TMemoryStream.Create;
    MS2 := TMemoryStream.Create;
    try
      try
        MS1.LoadFromFile(LameFile);
        MS2.LoadFromFile(MADFile);
      except
        Exit;
      end;

      H := BeginUpdateResource(PChar(AppGlobals.Storage.DataDir + FDownloadPackage), False);
      if H > 0 then
      begin
        UpdateResource(H, RT_RCDATA, 'lame-enc_dll',
          MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), MS1.Memory, MS1.Size);
        if EndUpdateResource(H, False) then
          Lame := True;
      end;

      H := BeginUpdateResource(PChar(AppGlobals.Storage.DataDir + FDownloadPackage), False);
      if H > 0 then
      begin
        UpdateResource(H, RT_RCDATA, 'libmad_dll',
          MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), MS2.Memory, MS2.Size);
        if EndUpdateResource(H, False) then
          MAD := True;
      end;
    finally
      MS1.Free;
      MS2.Free;
    end;
  end;

  Result := Lame and MAD;

  if Result then
    ExtractFiles;
end;
}

procedure TPostProcessSoX.Initialize;
begin
  inherited;

  FName := _('Apply effects using SoX');
  FHelp := _('This plugin applies effects to recorded songs using Sound eXchange (SoX).');
end;

function TPostProcessSoX.ProcessFile(
  Data: PPluginProcessInformation): TProcessThreadBase;
begin
  Result := nil;
  if not CanProcess(Data) then
    Exit;

  Result := TPostProcessSoxThread.Create(Data, Self);
end;

procedure TPostProcessSoX.Save;
begin
  inherited;

  AppGlobals.Storage.Write('Normalize_' + ClassName, FNormalize, 'Plugins');

  AppGlobals.Storage.Write('FadeoutStart_' + ClassName, FFadeoutStart, 'Plugins');
  AppGlobals.Storage.Write('FadeoutEnd_' + ClassName, FFadeoutEnd, 'Plugins');
  AppGlobals.Storage.Write('FadeoutStartLength_' + ClassName, FFadeoutStartLength, 'Plugins');
  AppGlobals.Storage.Write('FadeoutEndLength_' + ClassName, FFadeoutEndLength, 'Plugins');

  AppGlobals.Storage.Write('SilenceStart_' + ClassName, FSilenceStart, 'Plugins');
  AppGlobals.Storage.Write('SilenceEnd_' + ClassName, FSilenceEnd, 'Plugins');
  AppGlobals.Storage.Write('SilenceStartLength_' + ClassName, FSilenceStartLength, 'Plugins');
  AppGlobals.Storage.Write('SilenceEndLength_' + ClassName, FSilenceEndLength, 'Plugins');
end;

function TPostProcessSoX.ShowInitMessage(Handle: THandle): Boolean;
begin
  { TODO:
  Result := MsgBox(Handle, _('WARNING:'#13#10'It is not be allowed in some contries to use this plugin because it contains libmad.dll ' +
                             'and lame_enc.dll that make use of some patented technologies. Please make sure you may use these files in your country. ' +
                             'If you are sure you may use these files, press "Yes" to continue.'), _('Warning'), MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = IDYES;
  }
end;

end.
