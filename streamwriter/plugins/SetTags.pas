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
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
  end;

  TSetTagsPlugin = class(TInternalPlugin)
  private
    FArtist: string;
    FTitle: string;
    FComment: string;
  protected
    function FGetReadyForUse: Boolean; override;
    function FGetFilesInstalled: Boolean; override;
    function FGetReadyForActivate: Boolean; override;
  public
    constructor Create;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;
    procedure Initialize; override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
    procedure Save; override;
  end;

implementation

uses
  AppData, ConfigureSetTags;

{ TSetTagsThread }

constructor TSetTagsThread.Create(Data: PPluginProcessInformation;
  Plugin: TPluginBase);
begin
  inherited Create(Data, Plugin);
end;

procedure TSetTagsThread.Execute;
var
  Artist, Title, Comment: string;
  P: TSetTagsPlugin;
  ID3V1: TID3v1Tag;
  ID3V2: TID3v2Tag;

  i: Integer;
  Dir, StreamName: string;
  Replaced: string;
  Arr: TPatternReplaceArray;
begin
  inherited;

  // TODO: Was ist, wenn es nicht da ist??? quasi leer??????
  // TODO: Klassenname hier ist scheiße!!!
  // TODO: wenn man die settings ändert im betrieb dann werden sie erst nach neustart vom plugin übernommen
  AppGlobals.Storage.Read('Artist_TSetTagsPlugin', Artist, '%a', 'Plugins');
  AppGlobals.Storage.Read('Title_TSetTagsPlugin', Title, '%t', 'Plugins');
  AppGlobals.Storage.Read('Comment_TSetTagsPlugin', Comment, '%a / %t / %s', 'Plugins');

  FResult := arFail;

  SetLength(Arr, 3);
  Arr[0].C := 'a';
  Arr[0].Replace := FData.Artist;
  Arr[1].C := 't';
  Arr[1].Replace := FData.Title;
  Arr[2].C := 's';
  Arr[2].Replace := Trim(FData.Station);

  P := TSetTagsPlugin(Plugin);

  ID3V1 := TID3v1Tag.Create;
  ID3V2 := TID3v2Tag.Create;
  try
    try
      Artist := PatternReplace(Artist, Arr);
      Title := PatternReplace(Title, Arr);
      Comment := PatternReplace(Comment, Arr);

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
      ID3V1.Genre := ''; // Dann setzt Mp3FileUtils das auf "Undefined"
      ID3V1.Comment := PatternReplace(Comment, Arr) + ' / Recorded using streamWriter';
      ID3V2.Comment := PatternReplace(Comment, Arr) + ' / Recorded using streamWriter';
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

procedure TSetTagsPlugin.Assign(Source: TPluginBase);
begin
  inherited;

  FArtist := TSetTagsPlugin(Source).FArtist;
  FTitle := TSetTagsPlugin(Source).FTitle;
  FComment := TSetTagsPlugin(Source).FComment;
end;

function TSetTagsPlugin.Configure(AOwner: TComponent; Handle: Cardinal;
  ShowMessages: Boolean): Boolean;
var
  F: TfrmConfigureSetTags;
begin
  Result := True;

  F := TfrmConfigureSetTags.Create(AOwner, Self, FArtist, FTitle, FComment);
  try
    F.ShowModal;

    if F.SaveData then
    begin
      FArtist := F.Artist;
      FTitle := F.Title;
      FComment := F.Comment;
      Save;
    end;
  finally
    F.Free;
  end;
end;

function TSetTagsPlugin.Copy: TPluginBase;
begin
  Result := TSetTagsPlugin.Create;
// TODO: !!!
  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;
end;

constructor TSetTagsPlugin.Create;
begin
  inherited;

  FActive := True;
  FOrder := 100;
  FCanConfigure := True;

  FName := _('Set ID3-tags');
  FHelp := _('This plugin adds ID3-tags to recorded songs.');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, True, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    AppGlobals.Storage.Read('Artist_' + ClassName, FArtist, '%a', 'Plugins');
    AppGlobals.Storage.Read('Title_' + ClassName, FTitle, '%t', 'Plugins');
    AppGlobals.Storage.Read('Comment_' + ClassName, FComment, '%a / %t / %s', 'Plugins');

    if not FGetFilesInstalled then
      FActive := False;
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

procedure TSetTagsPlugin.Save;
begin
  inherited;

  AppGlobals.Storage.Write('Artist_' + ClassName, FArtist, 'Plugins');
  AppGlobals.Storage.Write('Title_' + ClassName, FTitle, 'Plugins');
  AppGlobals.Storage.Write('Comment_' + ClassName, FComment, 'Plugins');
end;

end.
