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

unit DownloadAddons;

interface

uses
  AddonBase,
  AppData,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  DownloadClient,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  Images,
  LanguageObjects,
  Logging,
  SharedData,
  StdCtrls,
  SysUtils,
  Variants;

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

{$R *.lfm}

constructor TfrmDownloadAddons.Create(AOwner: TComponent; Addon: TAddonBase);
begin
  inherited Create(AOwner);

  modSharedData.imgImages.GetIcon(TImages.PACKAGE_DOWN, Icon);

  FDownloadName := Addon.DownloadName;
  FDownloadPackage := Addon.DownloadPackage;

  Language.Translate(Self);

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
end;

procedure TfrmDownloadAddons.DownloaderDownloaded(Sender: TObject);
begin
  FDownloader.Thread.RecvDataStream.Seek(0, soFromBeginning);

  try
    ForceDirectories(AppGlobals.Storage.AddonDir);
    FDownloader.Thread.RecvDataStream.SaveToFile(ConcatPaths([AppGlobals.Storage.AddonDir, FDownloadPackage]));
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
    URL := '%s/%s/downloads/getaddon/%s/%s/%s?architecture=%s'.Format([AppGlobals.ProjectUpdateLinks[0], Trim(AppGlobals.Language), LowerCase(AppGlobals.AppName), AppGlobals.AppVersion.AsString, LowerCase(FDownloadName), AppGlobals.Architecture])
  else
    URL := '%s/en/downloads/getaddon/%s/%s/%s?architecture=%s'.Format([AppGlobals.ProjectUpdateLinks[0], LowerCase(AppGlobals.AppName), AppGlobals.AppVersion.AsString, LowerCase(FDownloadName), AppGlobals.Architecture]);

  FDownloader := TDownloadClient.Create(URL);
  FDownloader.OnDownloadProgress := DownloaderDownloadProgress;
  FDownloader.OnDownloaded := DownloaderDownloaded;
  FDownloader.OnError := DownloaderError;
  FDownloader.Start;
end;

procedure TfrmDownloadAddons.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDownloader.Free;
end;

end.
