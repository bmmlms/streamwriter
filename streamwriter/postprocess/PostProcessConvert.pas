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
  Windows, SysUtils, Classes, PostProcess, LanguageObjects,
  Functions, Logging, Math, PluginBase, StrUtils;

type
  TPostProcessConvertThread = class(TPostProcessThreadBase)
  private
    FFromFile: string;
    FToFile: string;
    FBitRate: Cardinal;
    FProgress: Integer;

    FOnProgress: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure FileConvertorProgress(Sender: TObject; Percent: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);

    procedure Convert(FromFile, ToFile: string; BitRate: Cardinal);

    property Progress: Integer read FProgress;

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
    function ProcessFile(Data: PPluginProcessInformation): TPostProcessThreadBase; override;
  end;

implementation

uses
  AppData, FileConvertor;

{ TPostProcessConvertThread }

procedure TPostProcessConvertThread.Convert(FromFile, ToFile: string; BitRate: Cardinal);
var
  ToExt: string;
begin
  FFromFile := FromFile;
  FToFile := ToFile;
  FBitRate := BitRate;
end;

constructor TPostProcessConvertThread.Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
begin
  inherited Create(Data, Plugin);
end;

procedure TPostProcessConvertThread.Execute;
var
  FC: TFileConvertor;
begin
  inherited;

  FResult := arFail;

  FC := TFileConvertor.Create;
  FC.BitRate := FBitRate;
  try
    FC.OnProgress := FileConvertorProgress;

    try
      if FC.Convert(FFromFile, FToFile, @Terminated) then
      begin
        FResult := arWin;

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
  FName := 'Convert file';
end;

destructor TPostProcessConvert.Destroy;
begin

  inherited;
end;

function TPostProcessConvert.ProcessFile(
  Data: PPluginProcessInformation): TPostProcessThreadBase;
begin
  Result := nil;
  if not CanProcess(Data) then
    Exit;

  Result := TPostProcessConvertThread.Create(Data, Self);
end;

end.
