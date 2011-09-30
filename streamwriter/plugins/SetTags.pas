{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit SetTags;

interface

uses
  Windows, SysUtils, Classes, Plugins, PluginsShared, LanguageObjects,
  Mp3FileUtils, Functions, Logging;

type
  TSetTagsThread = class(TProcessThreadBase)
  protected
    procedure Execute; override;
  end;

  TSetTagsPlugin = class(TInternalPlugin)
  protected
    function FGetReadyForUse: Boolean; override;
    function FGetFilesInstalled: Boolean; override;
    function FGetReadyForActivate: Boolean; override;
  public
    constructor Create;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPluginBase; override;
    procedure Initialize; override;
  end;

implementation

uses
  AppData;

{ TSetTagsThread }

procedure TSetTagsThread.Execute;
var
  Artist, Title: string;
  ID3V1: TID3v1Tag;
  ID3V2: TID3v2Tag;
begin
  inherited;

  FResult := arFail;

  ID3V1 := TID3v1Tag.Create;
  ID3V2 := TID3v2Tag.Create;
  try
    try
      Artist := FData.Artist;
      Title := FData.Title;

      if (Trim(Artist) <> '') and (Trim(Title) <> '') then
      begin
        ID3V1.Artist := Artist;
        ID3V1.Title := Title;
        ID3V2.Artist := Artist;
        ID3V2.Title := Title;
      end else
      begin
        ID3V1.Title := FData.Title;
        ID3V2.Title := FData.Title;
      end;
      ID3V1.Track := IntToStr(FData.TrackNumber);
      ID3V2.Track := IntToStr(FData.TrackNumber);
      ID3V1.Genre := 'ASDFFDSA'; // Dann setzt Mp3FileUtils das auf "Undefined"
      ID3V1.Comment := FData.Station + ' / ' + FData.StreamTitle;
      ID3V2.Comment := FData.Station + ' / ' + FData.StreamTitle;
      if (ID3V1.WriteToFile(FData.Filename) = MP3ERR_None) and (ID3V2.WriteToFile(FData.Filename) = MP3ERR_None) then
      begin
        FData.Filesize := GetFileSize(FData.Filename);
        FResult := arWin;
      end;
    except
    end;
  finally
    ID3V1.Free;
    ID3V2.Free;
  end;
end;

{ TSetTagsPlugin }

function TSetTagsPlugin.Copy: TPluginBase;
begin
  Result := TSetTagsPlugin.Create;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;
end;

constructor TSetTagsPlugin.Create;
begin
  inherited;
  FActive := True;
  FOrder := 100;

  FName := _('Set ID3-tags');
  FHelp := _('This plugin adds ID3-tags to recorded songs.');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');
  except end;
end;

function TSetTagsPlugin.FGetFilesInstalled: Boolean;
begin
  Result := True;
end;

function TSetTagsPlugin.FGetReadyForActivate: Boolean;
begin
  Result := True;
end;

function TSetTagsPlugin.FGetReadyForUse: Boolean;
begin
  Result := True;
end;

procedure TSetTagsPlugin.Initialize;
begin
  inherited;
  FName := _('Set ID3-tags');
  FHelp := _('This plugin adds ID3-tags to recorded songs.');
end;

function TSetTagsPlugin.ProcessFile(
  Data: PPluginProcessInformation): TProcessThreadBase;
begin
  Result := TSetTagsThread.Create(Data, Self);
end;

end.
