{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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
  SysUtils, Windows, Classes, PostProcess, LanguageObjects, Generics.Collections,
  Functions, Logging, Math, AddonBase, ExtendedStream, AudioFunctions,
  DataManager;

type
  TPostProcessSoxThread = class(TPostProcessThreadBase)
  private
    FSoxPath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPostProcessInformation; Addon: TPostProcessBase);
  end;

  TPostProcessSoX = class(TInternalPostProcess)
  private
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
    function FGetHash: Cardinal; override;
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;
    function CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostprocessBase>): Boolean; override;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; override;
    function Copy: TPostProcessBase; override;
    procedure Assign(Source: TPostProcessBase); override;
    procedure Load(Stream: TExtendedStream; Version: Integer); override;
    procedure Save(Stream: TExtendedStream); override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
    //function EatFiles(LameFile, MADFile: string): Boolean;
  end;

implementation

uses
  AppData, ConfigureSoX, AddonManager, AddonLAME, AddonSoX;

{ TPostProcessSoxThread }

constructor TPostProcessSoxThread.Create(Data: PPostProcessInformation; Addon: TPostProcessBase);
begin
  inherited Create(Data, Addon);

  FSoxPath := TAddonSoX(AppGlobals.AddonManager.Find(TAddonSoX)).EXEPath;
end;

procedure TPostProcessSoxThread.Execute;
var
  SoXOutFile, CmdLine, Params: string;
  Output: AnsiString;
  P: TPostProcessSoX;
  LoopStarted: Cardinal;
  Failed: Boolean;
  FS: TFileStream;
  EC: DWORD;
begin
  inherited;

  FResult := arFail;

  SoxOutFile := RemoveFileExt(FData.WorkFilename) + '_sox.wav';

  P := TPostProcessSoX(PostProcessor);

  if P.FNormalize then
    CmdLine := '"' + FSoxPath + '" --norm "' + FData.WorkFilename + '" "' + SoxOutFile + '" '
  else
    CmdLine := '"' + FSoxPath + '" "' + FData.WorkFilename + '" "' + SoxOutFile + '" ';

  Params := '';

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

  if (Params <> '') or P.FNormalize then
  begin
    case RunProcess(CmdLine + Params, ExtractFilePath(FSoxPath), 300000, Output, EC, @Terminated, True) of
      rpWin:
        begin
          Failed := True;
          if FileExists(SoxOutFile) and (EC = 0) then
          begin
            LoopStarted := GetTickCount;
            while Failed do
            begin
              try
                FS := TFileStream.Create(SoxOutFile, fmOpenRead or fmShareExclusive);
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
              if not MoveFileEx(PChar(SoXOutFile), PChar(FData.WorkFilename), MOVEFILE_REPLACE_EXISTING) then
                Failed := True;
            if not Failed then
            begin
              if P.FSilenceStart then
                FData.Length := FData.Length + P.FSilenceStartLength;
              if P.FSilenceEnd then
                FData.Length := FData.Length + P.FSilenceEndLength;

              FResult := arWin;
            end;
          end;
        end;
      rpTimeout:
        FResult := arTimeout;
    end;
  end;

  if FResult <> arWin then
    DeleteFile(PChar(SoXOutFile));
end;

{ TPostProcessSoX }

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

function TPostProcessSoX.CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostprocessBase>): Boolean;
var
  OutputFormat: TAudioTypes;
begin
  OutputFormat := TEncoderSettings(Data.EncoderSettings).AudioType;
  if OutputFormat = atNone then
    OutputFormat := FiletypeToFormat(Data.Filename);

  Result := (AppGlobals.AddonManager.CanEncode(OutputFormat) = ceOkay) and FGetDependenciesMet and (FNormalize or FFadeoutStart or
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
    end;
  finally
    F.Free;
  end;
end;

function TPostProcessSoX.Copy: TPostProcessBase;
begin
  Result := TPostProcessSoX.Create;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

constructor TPostProcessSoX.Create;
begin
  inherited;

  FNeededAddons.Add(TAddonSoX);

  FCanConfigure := True;

  FPostProcessType := ptSoX;

  // Ggf. Daten von alter Version importieren, ansonsten Defaults zuweisen.
  // Die echten Daten kommen später über Load().
  // REMARK: Irgendwann kann das hier weg!
  AppGlobals.Storage.Read('Active_' + ClassName, FActive, False, 'Plugins');
  AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
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
end;

destructor TPostProcessSoX.Destroy;
begin

  inherited;
end;

function TPostProcessSoX.FGetHash: Cardinal;
begin
  Result := inherited + HashString(BoolToStr(FNormalize) + BoolToStr(FFadeoutStart) + BoolToStr(FFadeoutEnd) +
    BoolToStr(FSilenceStart) + BoolToStr(FSilenceEnd) + IntToStr(FFadeoutStartLength) +
    IntToStr(FFadeoutEndLength) + IntToStr(FSilenceStartLength) + IntToStr(FSilenceEndLength));
end;

function TPostProcessSoX.FGetHelp: string;
begin
  Result := _('This postprocessor applies effects to recorded songs using Sound eXchange (SoX).');
end;

function TPostProcessSoX.FGetName: string;
begin
  Result := _('Apply effects using SoX');
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

procedure TPostProcessSoX.Load(Stream: TExtendedStream; Version: Integer);
begin
  inherited;

  Stream.Read(FNormalize);

  Stream.Read(FFadeoutStart);
  Stream.Read(FFadeoutEnd);
  Stream.Read(FFadeoutStartLength);
  Stream.Read(FFadeoutEndLength);

  Stream.Read(FSilenceStart);
  Stream.Read(FSilenceEnd);
  Stream.Read(FSilenceStartLength);
  Stream.Read(FSilenceEndLength);
end;

function TPostProcessSoX.ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase;
begin
  Result := TPostProcessSoxThread.Create(Data, Self);
end;

procedure TPostProcessSoX.Save(Stream: TExtendedStream);
begin
  inherited;

  Stream.Write(FNormalize);

  Stream.Write(FFadeoutStart);
  Stream.Write(FFadeoutEnd);
  Stream.Write(FFadeoutStartLength);
  Stream.Write(FFadeoutEndLength);

  Stream.Write(FSilenceStart);
  Stream.Write(FSilenceEnd);
  Stream.Write(FSilenceStartLength);
  Stream.Write(FSilenceEndLength);
end;

function TPostProcessSoX.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := inherited;

  //MsgBox(Handle, _('Additional encoding-addons might be needed to have this postprocessor working, that is an encoder for the desired output format or an encoder for the format of the stream itself.'),
  //  _('Info'), MB_ICONINFORMATION);
end;

end.
