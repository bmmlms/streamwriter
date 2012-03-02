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
unit PostProcessConvert;

interface

uses
  Windows, SysUtils, Classes, PostProcess, LanguageObjects, AudioFunctions,
  Functions, Logging, Math, AddonBase, StrUtils, ExtendedStream;

type
  TPostProcessConvertThread = class(TPostProcessThreadBase)
  private
    FFromCutView: Boolean;
    FFromFile: string;
    FToFile: string;
    FEncoderSettings: TObject;
    FProgress: Integer;
    FFileInfo: TAudioFileInfo;

    FOnProgress: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure FileConvertorProgress(Sender: TObject; Percent: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPostProcessInformation; Addon: TPostProcessBase);
    procedure Convert(FromFile, ToFile: string; EncoderSettings: TObject);
    destructor Destroy; override;

    property Progress: Integer read FProgress;
    property FileInfo: TAudioFileInfo read FFileInfo;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

  TPostProcessConvert = class(TInternalPostProcess)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(Stream: TExtendedStream; Version: Integer); override;
    procedure Save(Stream: TExtendedStream); override;
    function Copy: TPostProcessBase; override;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; override;
  end;

implementation

uses
  AppData, FileConvertor;

{ TPostProcessConvertThread }

procedure TPostProcessConvertThread.Convert(FromFile, ToFile: string; EncoderSettings: TObject);
begin
  FFromCutView := True;
  FFromFile := FromFile;
  FToFile := ToFile;
  if EncoderSettings <> nil then
    FEncoderSettings := TEncoderSettings(EncoderSettings);
end;

constructor TPostProcessConvertThread.Create(Data: PPostProcessInformation; Addon: TPostProcessBase);
begin
  inherited Create(Data, Addon);

  FFileInfo.Success := False;

  if Data <> nil then
    FEncoderSettings := TEncoderSettings(Data.EncoderSettings);
end;

destructor TPostProcessConvertThread.Destroy;
begin

  inherited;
end;

procedure TPostProcessConvertThread.Execute;
var
  FC: TFileConvertor;
begin
  inherited;

  FResult := arFail;

  FC := TFileConvertor.Create;
  if FEncoderSettings <> nil then // Bei "nach WAV" sind die über.
  begin
    FC.CBRBitRate := TEncoderSettings(FEncoderSettings).CBRBitrate;
    FC.BitRateType := TEncoderSettings(FEncoderSettings).BitrateType;
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
          FFileInfo := GetFileInfo(FToFile);
          if FFileInfo.Success and (FData <> nil) then
          begin
            FData.BitRate := FFileInfo.Bitrate;
            FData.VBR := FFileInfo.VBR;
          end;
        end;

        Synchronize(
          procedure
          begin
            if Assigned(FOnFinish) then
              FOnFinish(Self)
          end);
      end else
      begin
        Synchronize(
          procedure
          begin
            if Assigned(FOnError) then
              FOnError(Self)
          end);
      end;
    except
      Synchronize(
        procedure
        begin
          if Assigned(FOnError) then
            FOnError(Self)
        end);
    end;
  finally
    FC.Free;
  end;
end;

procedure TPostProcessConvertThread.FileConvertorProgress(Sender: TObject;
  Percent: Integer);
begin
  Synchronize(
    procedure
    begin
      FProgress := Percent;
      if Assigned(FOnProgress) then
        FOnProgress(Sender);
    end);
end;

{ TPostProcessConvert }

constructor TPostProcessConvert.Create;
begin
  inherited;

  FOrder := -100; // Das muss, weil die Liste manchmal nach Order sortiert wird und er immer Position 0 haben muss.
  FHidden := True;
  FName := _('Convert file');

  FPostProcessType := ptConvert;
end;

destructor TPostProcessConvert.Destroy;
begin

  inherited;
end;

procedure TPostProcessConvert.Load(Stream: TExtendedStream;
  Version: Integer);
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

procedure TPostProcessConvert.Save(Stream: TExtendedStream);
begin

end;

end.
