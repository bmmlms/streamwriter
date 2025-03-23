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

unit PostProcessConvert;

interface

uses
  AudioFunctions,
  Classes,
  DataManager,
  LanguageObjects,
  Logging,
  PostProcess,
  SysUtils;

type

  { TPostProcessConvertThread }

  TPostProcessConvertThread = class(TPostProcessThreadBase)
  private
    FFromCutView: Boolean;
    FFromFile: string;
    FToFile: string;
    FEncoderSettings: TObject;
    FProgress: Integer;
    FFileInfo: TAudioInfo;
    FSync: TNotifyEvent;

    FOnProgress: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure FileConvertorProgress(Sender: TObject; Percent: Integer);
    procedure Sync;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPostProcessInformation; Addon: TPostProcessBase);
    destructor Destroy; override;
    procedure Convert(FromFile, ToFile: string; EncoderSettings: TObject);

    property ToFile: string read FToFile;
    property Progress: Integer read FProgress;
    property FileInfo: TAudioInfo read FFileInfo;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

  TPostProcessConvert = class(TInternalPostProcess)
  private
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;
    procedure Load(Stream: TStream; Version: Integer); override;
    procedure Save(Stream: TMemoryStream); override;
    function Copy: TPostProcessBase; override;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; override;
  end;

implementation

uses
  AppData,
  FileConvertor;

{ TPostProcessConvertThread }

procedure TPostProcessConvertThread.Convert(FromFile, ToFile: string; EncoderSettings: TObject);
begin
  FFromCutView := True;
  FFromFile := FromFile;
  FToFile := ToFile;

  if EncoderSettings <> nil then
    FEncoderSettings := TEncoderSettings(EncoderSettings).Copy;
end;

constructor TPostProcessConvertThread.Create(Data: PPostProcessInformation; Addon: TPostProcessBase);
begin
  inherited Create(Data, Addon);

  FFileInfo.Success := False;

  if Data <> nil then
    FEncoderSettings := TEncoderSettings(Data.EncoderSettings).Copy;
end;

destructor TPostProcessConvertThread.Destroy;
begin
  FreeAndNil(FEncoderSettings);

  inherited Destroy;
end;

procedure TPostProcessConvertThread.Execute;
var
  FC: TFileConvertor;
begin
  FResult := arFail;

  FC := TFileConvertor.Create;
  if FEncoderSettings <> nil then // Bei "nach WAV" sind die über.
  begin
    FC.CBRBitrate := TEncoderSettings(FEncoderSettings).CBRBitrate;
    FC.BitrateType := TEncoderSettings(FEncoderSettings).BitrateType;
    FC.VBRQuality := TEncoderSettings(FEncoderSettings).VBRQuality;
  end;

  try
    FC.OnProgress := FileConvertorProgress;

    try
      if FC.Convert(FFromFile, FToFile, @Terminated) then
      begin
        FResult := arWin;

        if LowerCase(ExtractFileExt(FToFile)) <> '.wav' then
        begin
          FFileInfo.GetAudioInfo(FToFile);
          if FFileInfo.Success and (FData <> nil) then
          begin
            FData.Bitrate := FFileInfo.Bitrate;
            FData.VBR := FFileInfo.VBR;
          end;
        end;

        if Assigned(FOnFinish) then
        begin
          FSync := FOnFinish;
          Synchronize(Sync);
        end;
      end else if Assigned(FOnError) then
      begin
        FSync := FOnError;
        Synchronize(Sync);
      end;
    except
      if Assigned(FOnError) then
      begin
        FSync := FOnError;
        Synchronize(Sync);
      end;
    end;
  finally
    FC.Free;
  end;
end;

procedure TPostProcessConvertThread.FileConvertorProgress(Sender: TObject; Percent: Integer);
begin
  if not Assigned(FOnProgress) then
    Exit;

  FProgress := Percent;
  FSync := FOnProgress;
  Synchronize(Sync);
end;

procedure TPostProcessConvertThread.Sync;
begin
  FSync(Self);
end;

{ TPostProcessConvert }

constructor TPostProcessConvert.Create;
begin
  inherited;

  FOrder := -100; // Das muss, weil die Liste manchmal nach Order sortiert wird und er immer Position 0 haben muss.
  FHidden := True;

  FPostProcessType := ptConvert;
end;

function TPostProcessConvert.FGetHelp: string;
begin
  Result := '';
end;

function TPostProcessConvert.FGetName: string;
begin
  Result := _('Convert file');
end;

procedure TPostProcessConvert.Load(Stream: TStream; Version: Integer);
begin

end;

function TPostProcessConvert.Copy: TPostProcessBase;
begin
  Result := TPostProcessConvert.Create;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

function TPostProcessConvert.ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase;
begin
  Result := TPostProcessConvertThread.Create(Data, Self);
end;

procedure TPostProcessConvert.Save(Stream: TMemoryStream);
begin

end;

end.
