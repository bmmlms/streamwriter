{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2019 Alexander Nottelmann

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

unit DownloadAddons;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, PostProcess, DownloadClient,
  Functions, LanguageObjects, AppData, Logging, AddonBase;

type
  TfrmDownloadAddons = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    FDownloader: TDownloadClient;
    FDownloaded: Boolean;
    FError: Boolean;
    FDownloadName: string;
    FDownloadPackage: string;

    procedure DownloaderDownloadProgress(Sender: TObject);
    procedure DownloaderDownloaded(Sender: TObject);
    procedure DownloaderError(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; Addon: TAddonBase); reintroduce;
    property Downloaded: Boolean read FDownloaded;
    property Error: Boolean read FError;
  end;

implementation

{$R *.dfm}

constructor TfrmDownloadAddons.Create(AOwner: TComponent; Addon: TAddonBase);
begin
  inherited Create(AOwner);

  FDownloadName := Addon.DownloadName;
  FDownloadPackage := Addon.DownloadPackage;

  Language.Translate(Self);
end;

procedure TfrmDownloadAddons.DownloaderDownloaded(Sender: TObject);
begin
  FDownloader.Thread.RecvDataStream.Seek(0, soFromBeginning);

  try
    FDownloader.Thread.RecvDataStream.SaveToFile(AppGlobals.Storage.DataDir + FDownloadPackage);
    FDownloaded := True;
  except
    FError := True;
  end;

  Close;
end;

procedure TfrmDownloadAddons.DownloaderDownloadProgress(Sender: TObject);
begin
  if FDownloader.Percent < 100 then
    ProgressBar1.Position := FDownloader.Percent + 1;
  ProgressBar1.Position := FDownloader.Percent;
end;

procedure TfrmDownloadAddons.DownloaderError(Sender: TObject);
begin
  FError := True;
  Close;
end;

procedure TfrmDownloadAddons.FormActivate(Sender: TObject);
var
  URL: string;
begin
  if FDownloader <> nil then
    Exit;

  if (AppGlobals.Language <> '') and (AppGlobals.WebLanguages.IndexOf(LowerCase(AppGlobals.Language)) > -1) then
    URL := AppGlobals.ProjectUpdateLinks[0] + Trim(AppGlobals.Language) + '/downloads/getaddon/' + LowerCase(AppGlobals.AppName) + '/' + AppGlobals.AppVersion.AsString + '/' + LowerCase(FDownloadName) + '/'
  else
    URL := AppGlobals.ProjectUpdateLinks[0] + 'en/downloads/getaddon/' + LowerCase(AppGlobals.AppName) + '/' + AppGlobals.AppVersion.AsString + '/' + LowerCase(FDownloadName) + '/';

  FDownloader := TDownloadClient.Create(URL);
  FDownloader.OnDownloadProgress := DownloaderDownloadProgress;
  FDownloader.OnDownloaded := DownloaderDownloaded;
  FDownloader.OnError := DownloaderError;
  FDownloader.Start;
end;

procedure TfrmDownloadAddons.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FDownloader.Kill;
  FDownloader.Free;
end;

end.
